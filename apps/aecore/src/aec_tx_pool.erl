%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Memory pool of unconfirmed transactions.
%%%
%%% Unconfirmed transactions are transactions not included in any
%%% block in the longest chain.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aec_tx_pool).

-behaviour(gen_server).

-define(MEMPOOL, mempool).
-define(MEMPOOL_GC, mempool_gc).
-define(KEY(NegFee, NegGasPrice, Origin, Nonce, TxHash),
           {NegFee, NegGasPrice, Origin, Nonce, TxHash}).
-define(VALUE_POS, 2).
-define(KEY_AS_MATCH_SPEC_RESULT(NegFee, NegGasPrice, Origin, Nonce, TxHash),
        { %% Tuple of arity 1 where the single element is the mempool key tuple.
         ?KEY(NegFee, NegGasPrice, Origin, Nonce, TxHash)
        }).
-define(KEY_NONCE_PATTERN(Sender), {?KEY('_', '_', Sender, '$1', '_'), '_'}).

%% API
-export([ start_link/0
        , stop/0
        ]).

-export([ garbage_collect/0
        , get_candidate/2
        , get_max_nonce/1
        , new_sync_top_target/1
        , peek/1
        , peek/2
        , push/1
        , push/2
        , size/0
        , top_change/2
        ]).

%% exports used by GC (should perhaps be in a common lib module)
-export([ top_height/0
        , pool_db/0
        , pool_db_gc/0]).

-ifdef(TEST).
-export([garbage_collect/1]). %% Only for (Unit-)test
-endif.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([ pool_db/0
             , pool_db_key/0
             , tx_hash/0]).

-import(aeu_debug, [pp/1]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-record(state, { db = pool_db()       :: pool_db()
               , gc_db = pool_db_gc() :: pool_db()
               , sync_top_calc        :: pid() | undefined
                 %% Used at tx insertion and at tx re-insertion (on
                 %% chain fork change) for preventing GCing received
                 %% txs while syncing with a stronger (i.e. with a
                 %% higher cumulative difficulty) and much longer
                 %% fork - typically at bootstrap.
               , gc_height = 0 :: aec_blocks:height() | undefined }).

-record(dbs, {db, gc_db}).

-type negated_fee() :: non_pos_integer().
-type non_pos_integer() :: neg_integer() | 0.
-type tx_hash() :: binary().

-type pool_db_key() :: ?KEY(negated_fee(), aect_contracts:amount(),
                            aec_keys:pubkey(), non_neg_integer(), binary()).
-type pool_db_value() :: aetx_sign:signed_tx().

-type pool_db() :: atom().

-type event() :: tx_created | tx_received.

-ifndef(TEST).
-define(DEFAULT_TX_TTL, 256).
-define(DEFAULT_INVALID_TX_TTL, 5).
-else.
-define(DEFAULT_TX_TTL, 8).
-define(DEFAULT_INVALID_TX_TTL, 2).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    %% implies also clearing the mempool
    gen_server:stop(?SERVER).

-define(PUSH_EVENT(E), Event =:= tx_created; Event =:= tx_received).

%% INFO: Transaction from the same sender with the same nonce and fee
%%       will be overwritten
-spec push(aetx_sign:signed_tx()) -> ok | {error, atom()}.
push(Tx) ->
    push(Tx, tx_created).

-spec push(aetx_sign:signed_tx(), event()) -> ok | {error, atom()}.
push(Tx, Event) when ?PUSH_EVENT(Event) ->
    %% Verify that this is a signed transaction.
    jobs:run(tx_pool_push, fun() -> push_(Tx, Event) end).

push_(Tx, Event) ->
    try aetx_sign:tx(Tx)
    catch _:_ -> error({illegal_transaction, Tx})
    end,
    case check_pool_db_put(Tx) of
        ignore -> ok;
        {error,_} = E -> E;
        {ok, Hash} ->
            gen_server:call(?SERVER, {push, Tx, Hash, Event})
    end.


-spec get_max_nonce(aec_keys:pubkey()) -> {ok, non_neg_integer()} | undefined.
get_max_nonce(Sender) ->
    #dbs{db = Db} = dbs(),
    int_get_max_nonce(Db, Sender).

-spec garbage_collect() -> ok.
garbage_collect() ->
    lager:debug("garbage_collect()", []),
    gen_server:cast(?SERVER, garbage_collect).

-ifdef(TEST).
-spec garbage_collect(Height :: aec_blocks:height()) -> ok.
garbage_collect(Height) ->
    aec_tx_pool_gc:garbage_collect(Height).
-endif.

%% The specified maximum number of transactions avoids requiring
%% building in memory the complete list of all transactions in the
%% pool.
-spec peek(pos_integer() | infinity) -> {ok, [aetx_sign:signed_tx()]}.
peek(MaxN) when is_integer(MaxN), MaxN >= 0; MaxN =:= infinity ->
    gen_server:call(?SERVER, {peek, MaxN, all}).

%% Only return transactions for a specific account public key
-spec peek(pos_integer() | infinity, aec_keys:pubkey()) ->
                                       {ok, [aetx_sign:signed_tx()]}.
peek(MaxN, Account) when is_integer(MaxN), MaxN >= 0; MaxN =:= infinity ->
    gen_server:call(?SERVER, {peek, MaxN, Account}).

-spec get_candidate(pos_integer(), binary()) -> {ok, [aetx_sign:signed_tx()]}.
get_candidate(MaxGas, BlockHash) when is_integer(MaxGas), MaxGas > 0,
                                      is_binary(BlockHash) ->
    int_get_candidate(MaxGas, BlockHash, dbs()).
    %% gen_server:call(?SERVER, {get_candidate, MaxGas, BlockHash}).

%% It assumes that the persisted mempool has been updated.
-spec top_change(binary(), binary()) -> ok.
top_change(OldHash, NewHash) ->
    lager:debug("top_change(...)", []),
    gen_server:call(?SERVER, {top_change, OldHash, NewHash}).

-spec new_sync_top_target(aec_blocks:height()) -> ok.
new_sync_top_target(NewSyncTop) ->
    lager:debug("new_sync_top_target()", []),
    gen_server:cast(?SERVER, {new_sync_top_target, NewSyncTop}).

-spec size() -> non_neg_integer() | undefined.
size() ->
    ets:info(?MEMPOOL, size).

pool_db() -> ?MEMPOOL.
pool_db_gc() -> ?MEMPOOL_GC.

%% This function is primarily used to sync with the aec_tx_pool server,
%% in order to preserve order of operations (e.g. if you push a tx, then
%% peek, you should see the tx you just pushed.)
dbs() ->
    gen_server:call(?SERVER, dbs).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, _MempoolDb} = pool_db_open(pool_db()),
    GCHeight = top_height(),
    Handled  = ets:new(init_tx_pool, [private]),
    InitF  = fun(TxHash, _) ->
                     update_pool_on_tx_hash(TxHash, {pool_db(), pool_db_gc(), GCHeight}, Handled),
                     ok
             end,
    ok = aec_db:ensure_transaction(fun() -> aec_db:fold_mempool(InitF, ok) end),
    ets:delete(Handled),
    lager:debug("init: GCHeight = ~p", [GCHeight]),
    {ok, #state{gc_height = GCHeight}}.

handle_call({get_max_nonce, Sender}, _From, #state{db = Mempool} = State) ->
    {reply, int_get_max_nonce(Mempool, Sender), State};
handle_call({push, Tx, Hash, Event}, _From, State) ->
    {Res, State1} = do_pool_db_put(pool_db_key(Tx), Tx, Hash, Event, State),
    {reply, Res, State1};
handle_call({top_change, OldHash, NewHash}, _From, State) ->
    do_top_change(OldHash, NewHash, State),
    {reply, ok, State};
handle_call({peek, MaxNumberOfTxs, Account}, _From, #state{db = Mempool} = State)
  when is_integer(MaxNumberOfTxs), MaxNumberOfTxs >= 0;
       MaxNumberOfTxs =:= infinity ->
    Txs = pool_db_peek(Mempool, MaxNumberOfTxs, Account),
    {reply, {ok, Txs}, State};
handle_call(dbs, _From, #state{db = Db, gc_db = GCDb} = State) ->
    {reply, #dbs{db = Db, gc_db = GCDb}, State};
handle_call(Request, From, State) ->
    lager:warning("Ignoring unknown call request from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast({new_sync_top_target, NewSyncTop}, State) ->
    {noreply, do_update_sync_top_target(NewSyncTop, State)};
handle_cast(garbage_collect, State) ->
    case State of
        #state{gc_height = undefined, sync_top_calc = P} when is_pid(P) ->
            %% sync_top update will be followed by GC (in handle_info/2 below)
            {noreply, State};
        #state{gc_height = H} when is_integer(H) ->
            State1 = do_update_sync_top_target(H, State),
            {noreply, State1}
    end;
handle_cast(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

handle_info({P, new_gc_height, GCHeight}, #state{sync_top_calc = P} = State) ->
    aec_tx_pool_gc:gc(GCHeight),
    {noreply, State#state{sync_top_calc = undefined, gc_height = GCHeight}};
handle_info({'ETS-TRANSFER', _, _, _}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    lager:warning("Ignoring unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec int_get_max_nonce(pool_db(), aec_keys:pubkey()) -> {ok, non_neg_integer()} | undefined.
int_get_max_nonce(Mempool, Sender) ->
    case ets:select(Mempool, [{ ?KEY_NONCE_PATTERN(Sender), [], ['$1'] }]) of
        [] ->
            undefined;
        Nonces ->
            MaxNonce = lists:max(Nonces),
            {ok, MaxNonce}
    end.

%% Ensure ordering of tx nonces in one account, and for duplicate account nonces
%% we only get the one with higher fee or - if equal fee - higher gas price.
%% int_get_candidate(#state{ db = Db, gc_db = GCDb }, MaxGas, BlockHash) ->

int_get_candidate(MaxGas, BlockHash, #dbs{db = Db} = DBs) ->
    {ok, Trees} = aec_chain:get_block_state(BlockHash),
    {ok, Header} = aec_chain:get_header(BlockHash),
    int_get_candidate(MaxGas, DBs, ets:first(Db),
                      {account_trees, aec_trees:accounts(Trees)},
                      aec_headers:height(Header), gb_trees:empty()).

int_get_candidate(Gas, Dbs = #dbs{db = Db, gc_db = GCDb},
                  ?KEY(_, _, Account, Nonce, TxHash) = Key,
                  AccountsTree, Height, Acc) ->
    Next = ets:next(Db, Key),
    case gb_trees:is_defined({Account, Nonce}, Acc) of
        true ->
            %% The earlier must have had higher fee. Skip this tx.
            int_get_candidate(Gas, Dbs, Next, AccountsTree, Height, Acc);
        false ->
            Tx = ets:lookup_element(Db, Key, ?VALUE_POS),
            Tx1 = aetx_sign:tx(Tx),
            TxTTL = aetx:ttl(Tx1),
            TxGas = aetx:gas(Tx1),
            case Height < TxTTL andalso ok =:= int_check_nonce(Tx, AccountsTree) of
                true ->
                    case Gas - TxGas of
                        RemGas when RemGas >= 0 ->
                            Acc1 = gb_trees:insert({Account, Nonce}, Tx, Acc),
                            int_get_candidate(RemGas, Dbs, Next, AccountsTree, Height, Acc1);
                        _ ->
                            %% Check the rest of txs, maybe some of them fits
                            %% into the gas limit.
                            int_get_candidate(Gas, Dbs, Next, AccountsTree, Height, Acc)
                    end;
                false ->
                    %% This is not valid anymore.
                    enter_tx_gc(GCDb, TxHash, Key, Height + invalid_tx_ttl()),
                    int_get_candidate(Gas, Dbs, Next, AccountsTree, Height, Acc)
            end
    end;
int_get_candidate(_Gas, _Dbs, '$end_of_table', _AccountsTree, _Height, Acc) ->
    {ok, gb_trees:values(Acc)}.

top_height() ->
    case aec_chain:top_header() of
        undefined -> 0;
        Header    -> aec_headers:height(Header)
    end.

get_gc_height(#state{gc_height = undefined,
                  sync_top_calc = P} = State) when is_pid(P) ->
    lager:debug("wait for gc_height ...", []),
    receive
        {P, new_gc_height, H} ->
            {H, State#state{sync_top_calc = undefined, gc_height = H}}
    after 10000 ->
            erlang:error(cannot_get_gc_height)
    end;
get_gc_height(#state{gc_height = H} = State) when is_integer(H) ->
    {H, State}.

do_update_sync_top_target(NewSyncTop, #state{ gc_height = GCHeight
                                            , sync_top_calc = OldP } = State) ->
    case OldP of
        undefined ->
            Me = self(),
            NewP = spawn_link(
                     fun() ->
                             do_update_sync_top(NewSyncTop, GCHeight, Me)
                     end),
            State#state{ sync_top_calc = NewP, gc_height = undefined };
        _ when is_pid(OldP) ->
            unlink(OldP),
            exit(OldP, kill),
            do_update_sync_top_target(
              NewSyncTop, State#state{sync_top_calc = undefined})
    end.

do_update_sync_top(NewSyncTop, GCHeight, Parent) ->
    LocalTop = aec_tx_pool:top_height(),
    NewGCHeight = lists:max([LocalTop, GCHeight, NewSyncTop]),
    case NewGCHeight < GCHeight - 5 of
        true ->
            %% This is a special case, normally the height shouldn't go down
            %% this means that the sync was aborted - in this case we
            %% re-adjust the GC-height for all TXs in order not to carry around
            %% transactions unnecessarily.
            aec_tx_pool_gc:adjust_ttl(GCHeight - NewGCHeight);
        false ->
            ok
    end,
    Parent ! {self(), new_gc_height, NewGCHeight}.

-spec pool_db_key(aetx_sign:signed_tx()) -> pool_db_key().
pool_db_key(SignedTx) ->
    Tx = aetx_sign:tx(SignedTx),
    %% INFO: Sort by fee, then by gas price, then by origin, then by nonce

    %% INFO: * given that nonce is an index of transactions for a user,
    %%         the following key is unique for a transaction
    %%       * negative fee and negative gas price place high profit
    %%         transactions at the beginning
    %%       * ordered_set type enables implicit overwrite of the same txs
    ?KEY(-aetx:fee(Tx), -aetx:gas_price(Tx),
         aetx:origin(Tx), aetx:nonce(Tx), aetx_sign:hash(SignedTx)).

-spec pool_db_open(DbName :: atom()) -> {ok, pool_db()}.
pool_db_open(DbName) ->
    {ok, ets:new(DbName, [ordered_set, public, named_table])}.

-spec pool_db_peek(pool_db(), MaxNumber::pos_integer() | infinity, aec_keys:pubkey() | all) ->
                          [pool_db_value()].
pool_db_peek(_, 0, _) -> [];
pool_db_peek(Mempool, Max, all) ->
    sel_return(
      ets_select(Mempool, [{ {'_', '$1'}, [], ['$1'] }], Max));
pool_db_peek(Mempool, Max, Account) ->
    sel_return(
      ets_select(Mempool, [{ {?KEY('_', '_', Account, '_', '_'), '$1'}, [], ['$1'] }], Max)).


%% If TxHash is not already in GC table insert it.
-spec enter_tx_gc(pool_db(), aec_tx_pool_gc:pool_db_gc_key(), pool_db_key(), aetx:tx_ttl()) -> ok.
enter_tx_gc(MempoolGC, TxHash, Key, TTL) ->
    Res = aec_tx_pool_gc:add_hash(MempoolGC, TxHash, Key, TTL),
    %% If Res == TTL we set the TTL otherwise kept prev. value
    [ lager:debug("Adding ~p for GC at ~p", [pp(TxHash), TTL]) || Res == TTL ],
    ok.

ets_select(T, P, infinity) ->
    ets:select(T, P);
ets_select(T, P, N) when is_integer(N), N >= 1 ->
    ets:select(T, P, N).

sel_return(L) when is_list(L) -> L;
sel_return('$end_of_table' ) -> [];
sel_return({Matches, _Cont}) -> Matches.

do_top_change(OldHash, NewHash, State) ->
    %% Add back transactions to the pool from discarded part of the chain
    %% Mind that we don't need to add those which are incoming in the fork

    %% NG: does this work for common ancestor for micro blocks?
    {ok, Ancestor} = aec_chain:find_common_ancestor(OldHash, NewHash),
    Info = {State#state.db, State#state.gc_db, State#state.gc_height},

    Handled = ets:new(foo, [private, set]),
    update_pool_from_blocks(Ancestor, OldHash, Info, Handled),
    update_pool_from_blocks(Ancestor, NewHash, Info, Handled),
    ets:delete(Handled),
    ok.

update_pool_from_blocks(Hash, Hash,_Info,_Handled) -> ok;
update_pool_from_blocks(Ancestor, Current, Info, Handled) ->
    lists:foreach(fun(TxHash) ->
                          update_pool_on_tx_hash(TxHash, Info, Handled)
                  end,
                  safe_get_tx_hashes(Current)),
    Prev = aec_chain:prev_hash_from_hash(Current),
    update_pool_from_blocks(Ancestor, Prev, Info, Handled).

safe_get_tx_hashes(Hash) ->
    case aec_db:find_block_tx_hashes(Hash) of
        none -> [];
        {value, Hashes} -> Hashes
    end.

update_pool_on_tx_hash(TxHash, {Db, GCDb, GCHeight}, Handled) ->
    case ets:member(Handled, TxHash) of
        true -> ok;
        false ->
            ets:insert(Handled, {TxHash}),
            Tx = aec_db:get_signed_tx(TxHash),
            case aec_db:is_in_tx_pool(TxHash) of
                false ->
                    %% Added to chain
                    ets:delete(Db, pool_db_key(Tx)),
                    aec_tx_pool_gc:delete_hash(GCDb, TxHash);
                true ->
                    pool_db_raw_put(Db, GCDb, GCHeight, pool_db_key(Tx), Tx, TxHash)
            end
    end.

-spec check_pool_db_put(aetx_sign:signed_tx()) ->
                               ignore
                             | {'ok', tx_hash()}
                             | {'error', atom()}.
check_pool_db_put(Tx) ->
    Hash = aetx_sign:hash(Tx),
    case aec_chain:find_tx_location(Hash) of
        BlockHash when is_binary(BlockHash) ->
            lager:debug("Already have tx: ~p in ~p", [Hash, BlockHash]),
            {error, already_accepted};
        mempool ->
            %% lager:debug("Already have tx: ~p in ~p", [Hash, mempool]),
            ignore;
        none ->
            lager:debug("Already have GC:ed tx: ~p", [Hash]),
            ignore;
        not_found ->
            Checks = [ fun check_signature/2
                     , fun check_nonce/2
                     , fun check_minimum_fee/2
                     , fun check_minimum_gas_price/2
                     , fun check_tx_ttl/2
                     ],
            case aeu_validation:run(Checks, [Tx, Hash]) of
                {error, _} = E ->
                    lager:debug("Validation error for tx ~p: ~p", [Hash, E]),
                    E;
                ok ->
                    {ok, Hash}
            end
    end.

do_pool_db_put(Key, Tx, Hash, Event,
               #state{ db = Db, gc_db = GCDb, gc_height = GCHeight } = St0) ->
    {GCHeight, St} = get_gc_height(St0),
    %% TODO: This check is never going to hit? Hash is part of the Key!?
    case ets:member(Db, Key) of
        true ->
            lager:debug("Pool db key already present (~p)", [Key]),
            %% TODO: We should make a decision whether to switch the tx.
            {ok, St};
        false ->
            aec_db:add_tx(Tx),
            aec_events:publish(Event, Tx),
            pool_db_raw_put(Db, GCDb, GCHeight, Key, Tx, Hash),
            {ok, St}
    end.


pool_db_raw_put(Db, GCDb, GCHeight, Key, Tx, TxHash) ->
    ets:insert(Db, {Key, Tx}),
    enter_tx_gc(GCDb, TxHash, Key, GCHeight + tx_ttl()).

check_tx_ttl(STx, _Hash) ->
    Tx = aetx_sign:tx(STx),
    case top_height() > aetx:ttl(Tx) of
        true  -> {error, ttl_expired};
        false -> ok
    end.

check_signature(Tx, Hash) ->
    {ok, Trees} = aec_chain:get_top_state(),
    case aetx_sign:verify(Tx, Trees) of
        {error, _} = E ->
            lager:info("Failed signature check on tx: ~p, ~p\n", [E, Hash]),
            E;
        ok ->
            ok
    end.

check_nonce(Tx,_Hash) ->
  int_check_nonce(Tx, {block_hash, aec_chain:top_block_hash()}).

int_check_nonce(Tx, Source) ->
    %% Check is conservative and only rejects certain cases
    Unsigned = aetx_sign:tx(Tx),
    TxNonce = aetx:nonce(Unsigned),
    case aetx:origin(Unsigned) of
        undefined -> {error, no_origin};
        Pubkey when is_binary(Pubkey) ->
            case TxNonce > 0 of
                false ->
                    {error, illegal_nonce};
                true ->
                    case get_account(Pubkey, Source) of
                        {error, no_state_trees} ->
                            ok;
                        none ->
                            ok;
                        {value, Account} ->
                            case aetx_utils:check_nonce(Account, TxNonce) of
                                ok -> ok;
                                {error, account_nonce_too_low} ->
                                    %% This can be ok in the future
                                    ok;
                                {error, account_nonce_too_high} = E ->
                                    E
                            end
                    end
            end
    end.

get_account(AccountKey, {account_trees, AccountsTrees}) ->
    aec_accounts_trees:lookup(AccountKey, AccountsTrees);
get_account(AccountKey, {block_hash, BlockHash}) ->
    aec_chain:get_account_at_hash(AccountKey, BlockHash).

check_minimum_fee(Tx,_Hash) ->
    case aetx:fee(aetx_sign:tx(Tx)) >= aec_governance:minimum_tx_fee() of
        true  -> ok;
        false -> {error, too_low_fee}
    end.

-dialyzer({no_match, check_minimum_gas_price/2}).
check_minimum_gas_price(Tx, _Hash) ->
    GasPrice = aetx:gas_price(aetx_sign:tx(Tx)),
    MinGasPrice = aec_governance:minimum_gas_price(),
    case GasPrice >= MinGasPrice of
        true  -> ok;
        false -> {error, too_low_gas_price}
    end.

tx_ttl() ->
    aeu_env:user_config_or_env([<<"mempool">>, <<"tx_ttl">>],
                               aecore, mempool_tx_ttl, ?DEFAULT_TX_TTL).

invalid_tx_ttl() ->
    aeu_env:user_config_or_env([<<"mempool">>, <<"invalid_tx_ttl">>],
                               aecore, mempool_invalid_tx_ttl, ?DEFAULT_INVALID_TX_TTL).

