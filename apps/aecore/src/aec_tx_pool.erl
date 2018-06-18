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

%% Placeholder for gas price in mempool key for txs unrelated to contracts.
-define(PSEUDO_GAS_PRICE, 0).

%% API
-export([ start_link/0
        , stop/0
        ]).

-export([ garbage_collect/0
        , get_candidate/2
        , get_max_nonce/1
        , invalid_txs/2
        , peek/1
        , push/1
        , push/2
        , size/0
        , top_change/2
        ]).

-ifdef(TEST).
-export([garbage_collect/1]). %% Only for (Unit-)test
-endif.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(aeu_debug, [pp/1]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-record(state, { db :: pool_db(), gc_db :: pool_db() }).

-type negated_fee() :: non_pos_integer().
-type non_pos_integer() :: neg_integer() | 0.
-type tx_hash() :: binary().

-type pool_db_key() :: ?KEY(negated_fee(), aect_contracts:amount(),
                            aec_keys:pubkey(), non_neg_integer(), binary()).
-type pool_db_value() :: aetx_sign:signed_tx().

-type pool_db_gc_key() :: tx_hash().
-type pool_db_gc_value() :: aetx:tx_ttl().

-type pool_db() :: atom().

-type event() :: tx_created | tx_received.

-ifndef(TEST).
-define(INVALID_TX_TTL, 256).
-define(EXPIRED_TX_TTL, 5).
-else.
-define(INVALID_TX_TTL, 5).
-define(EXPIRED_TX_TTL, 2).
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
-spec push(aetx_sign:signed_tx()) -> ok.
push(Tx) ->
    push(Tx, tx_created).

-spec push(aetx_sign:signed_tx(), event()) -> ok.
push(Tx, Event) when ?PUSH_EVENT(Event) ->
    %% Verify that this is a signed transaction.
    try aetx_sign:tx(Tx)
    catch _:_ -> error({illegal_transaction, Tx})
    end,
    gen_server:call(?SERVER, {push, Tx, Event}).

-spec get_max_nonce(aec_keys:pubkey()) -> {ok, non_neg_integer()} | undefined.
get_max_nonce(Sender) ->
    gen_server:call(?SERVER, {get_max_nonce, Sender}).

-spec invalid_txs([tx_hash()], aec_blocks:height()) -> ok.
invalid_txs(TxHashes, Height) ->
    gen_server:cast(?SERVER, {invalid_txs, TxHashes, Height}).

-spec garbage_collect() -> ok.
garbage_collect() ->
    gen_server:cast(?SERVER, garbage_collect).

-ifdef(TEST).
-spec garbage_collect(Height :: aec_blocks:height()) -> ok.
garbage_collect(Height) ->
    gen_server:cast(?SERVER, {garbage_collect, Height}).
-endif.

%% The specified maximum number of transactions avoids requiring
%% building in memory the complete list of all transactions in the
%% pool.
-spec peek(pos_integer() | infinity) -> {ok, [aetx_sign:signed_tx()]}.
peek(MaxN) when is_integer(MaxN), MaxN >= 0; MaxN =:= infinity ->
    gen_server:call(?SERVER, {peek, MaxN}).

-spec get_candidate(pos_integer(), binary()) -> {ok, [aetx_sign:signed_tx()]}.
get_candidate(MaxN, BlockHash) when is_integer(MaxN), MaxN >= 0,
                                    is_binary(BlockHash) ->
    gen_server:call(?SERVER, {get_candidate, MaxN, BlockHash}).

-spec top_change(binary(), binary()) -> ok.
top_change(OldHash, NewHash) ->
    gen_server:call(?SERVER, {top_change, OldHash, NewHash}).

-spec size() -> non_neg_integer() | undefined.
size() ->
    ets:info(?MEMPOOL, size).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, MempoolDb} = pool_db_open(?MEMPOOL),
    Handled  = ets:new(init_tx_pool, [private]),
    InitF  = fun(TxHash, _) ->
                     update_pool_on_tx_hash(TxHash, MempoolDb, Handled),
                     ok
             end,
    ok = aec_db:ensure_transaction(fun() -> aec_db:fold_mempool(InitF, ok) end),
    ets:delete(Handled),
    {ok, MempoolGCDb} = pool_db_open(?MEMPOOL_GC),
    {ok, #state{db = MempoolDb, gc_db = MempoolGCDb}}.

handle_call({get_max_nonce, Sender}, _From, #state{db = Mempool} = State) ->
    {reply, int_get_max_nonce(Mempool, Sender), State};
handle_call({push, Tx, Event}, _From, #state{db = Mempool} = State) ->
    {reply, pool_db_put(Mempool, pool_db_key(Tx), Tx, Event), State};
handle_call({top_change, OldHash, NewHash}, _From,
            #state{db = Mempool} = State) ->
    do_top_change(OldHash, NewHash, Mempool),
    {reply, ok, State};
handle_call({peek, MaxNumberOfTxs}, _From, #state{db = Mempool} = State)
  when is_integer(MaxNumberOfTxs), MaxNumberOfTxs >= 0;
       MaxNumberOfTxs =:= infinity ->
    Txs = pool_db_peek(Mempool, MaxNumberOfTxs),
    {reply, {ok, Txs}, State};
handle_call({get_candidate, MaxNumberOfTxs, BlockHash}, _From, State) ->
    Txs = int_get_candidate(State, MaxNumberOfTxs, BlockHash),
    {reply, {ok, Txs}, State};
handle_call(Request, From, State) ->
    lager:warning("Ignoring unknown call request from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast({invalid_txs, TxHashes, Height}, State = #state{ gc_db = GCDb }) ->
    [ enter_tx_gc(GCDb, TxHash, Height + ?INVALID_TX_TTL) || TxHash <- TxHashes ],
    {noreply, State};
handle_cast(garbage_collect, State) ->
    do_gc(State),
    {noreply, State};
%% Only for (Unit-)test
handle_cast({garbage_collect, Height}, State) ->
    do_gc(State, Height),
    {noreply, State};
handle_cast(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

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
    case lists:flatten(ets:match(Mempool, ?KEY_NONCE_PATTERN(Sender))) of
        [] ->
            undefined;
        Nonces ->
            MaxNonce = lists:max(Nonces),
            {ok, MaxNonce}
    end.

%% Ensure ordering of tx nonces in one account, and for duplicate account nonces
%% we only get the one with higher fee or - if equal fee - higher gas price.
int_get_candidate(#state{ db = Db, gc_db = GCDb }, MaxNumberOfTxs, BlockHash) ->
    {ok, Trees} = aec_chain:get_block_state(BlockHash),
    {ok, Header} = aec_chain:get_header(BlockHash),
    int_get_candidate(MaxNumberOfTxs, {Db, GCDb}, ets:first(Db),
                      {account_trees, aec_trees:accounts(Trees)},
                      aec_headers:height(Header), gb_trees:empty()).

int_get_candidate(0, _Dbs, _, _AccountsTree, _Height, Acc) ->
    gb_trees:values(Acc);
int_get_candidate(_N, _Dbs, '$end_of_table', _AccountsTree, _Height, Acc) ->
    gb_trees:values(Acc);
int_get_candidate(N, Dbs = {Db, GCDb}, ?KEY(_, _, Account, Nonce, TxHash) = Key,
                  AccountsTree, Height, Acc) ->
    Next = ets:next(Db, Key),
    case gb_trees:is_defined({Account, Nonce}, Acc) of
        true ->
            %% The earlier must have had higher fee. Skip this tx.
            int_get_candidate(N, Dbs, Next, AccountsTree, Height, Acc);
        false ->
            Tx = ets:lookup_element(Db, Key, ?VALUE_POS),
            TTL = aetx:ttl(aetx_sign:tx(Tx)),
            case Height < TTL andalso ok == int_check_nonce(Tx, AccountsTree) of
                true ->
                    NewAcc = gb_trees:insert({Account, Nonce}, Tx, Acc),
                    int_get_candidate(N - 1, Dbs, Next, AccountsTree, Height, NewAcc);
                false ->
                    %% This is not valid anymore.
                    enter_tx_gc(GCDb, TxHash, Height + ?EXPIRED_TX_TTL),
                    int_get_candidate(N, Dbs, Next, AccountsTree, Height, Acc)
            end
    end.

-spec do_gc(#state{}) -> ok.
do_gc(State) ->
    Header = aec_chain:top_header(),
    Height = aec_headers:height(Header),
    do_gc(State, Height).

do_gc(State = #state{ gc_db = GCDb }, Height) ->
    GCTxs = ets:foldl(fun({TxHash, ExpireBy}, Acc) ->
                          case ExpireBy > Height of
                              true  -> Acc;
                              false -> [TxHash | Acc]
                          end
                      end, [], GCDb),
    do_gc_(State, GCTxs).

do_gc_(_S, []) ->
    ok;
do_gc_(S = #state{ db = Db, gc_db = GCDb }, [TxHash | TxHashes]) ->
    case aec_db:gc_tx(TxHash) of
        ok ->
            delete_pool_db_gc(GCDb, TxHash),
            delete_pool_db_by_hash(Db, TxHash),
            lager:debug("Garbage collected ~p", [pp(TxHash)]);
        {error, BlockHash} ->
            lager:info("TX garbage collect failed ~p is present in ~p",
                       [pp(BlockHash), pp(TxHash)]),
            ok
    end,
    do_gc_(S, TxHashes).

-spec pool_db_key(aetx_sign:signed_tx()) -> pool_db_key().
pool_db_key(SignedTx) ->
    Tx = aetx_sign:tx(SignedTx),
    %% INFO: Sort by fee, then by gas price, then by origin, then by nonce

    %% INFO: * given that nonce is an index of transactions for a user,
    %%         the following key is unique for a transaction
    %%       * negative fee and negative gas price place high profit
    %%         transactions at the beginning
    %%       * ordered_set type enables implicit overwrite of the same txs
    ?KEY(-aetx:fee(Tx), -int_gas_price(Tx),
         aetx:origin(Tx), aetx:nonce(Tx), aetx_sign:hash(SignedTx)).

-spec select_pool_db_key_by_hash(pool_db(), binary()) -> {ok, pool_db_key()} | not_in_ets.
select_pool_db_key_by_hash(Mempool, TxHash) ->
    MatchFunction =
        { {?KEY('$1', '$2', '$3', '$4', '$5'), '_'},
          [{'=:=','$5', TxHash}],
          [?KEY_AS_MATCH_SPEC_RESULT('$1', '$2', '$3', '$4', '$5')] },
    case sel_return(ets_select(Mempool, [MatchFunction], infinity)) of
        [Key] -> {ok, Key};
        [] -> not_in_ets
    end.

-spec pool_db_open(DbName :: atom()) -> {ok, pool_db()}.
pool_db_open(DbName) ->
    {ok, ets:new(DbName, [ordered_set, public, named_table])}.

-spec pool_db_peek(pool_db(), MaxNumber::pos_integer() | infinity) ->
                          [pool_db_value()].
pool_db_peek(_, 0) -> [];
pool_db_peek(Mempool, Max) ->
    sel_return(
      ets_select(Mempool, [{ {'_', '$1'}, [], ['$1'] }], Max)).

%% If TxHash is not already in GC table insert it.
-spec enter_tx_gc(pool_db(), pool_db_gc_key(), pool_db_gc_value()) -> ok.
enter_tx_gc(MempoolGC, TxHash, TTL) ->
    %% Use update_counter with a threshold to do the compare and maybe update
    %% efficiently.
    Res = ets:update_counter(MempoolGC, TxHash, {2, 0, TTL, TTL}, {TxHash, TTL}),
    %% If Res == TTL we set the TTL otherwise kept prev. value
    [ lager:debug("Adding ~p for GC at ~p", [pp(TxHash), Res]) || Res == TTL ],
    ok.

-spec delete_pool_db_gc(pool_db(), pool_db_gc_key()) -> true.
delete_pool_db_gc(MempoolGC, TxHash) ->
    ets:delete(MempoolGC, TxHash).

ets_select(T, P, infinity) ->
    ets:select(T, P);
ets_select(T, P, N) when is_integer(N), N >= 1 ->
    ets:select(T, P, N).

sel_return(L) when is_list(L) -> L;
sel_return('$end_of_table' ) -> [];
sel_return({Matches, _Cont}) -> Matches.

do_top_change(OldHash, NewHash, Mempool) ->
    %% Add back transactions to the pool from discarded part of the chain
    %% Mind that we don't need to add those which are incoming in the fork
    {ok, Ancestor} = aec_chain:find_common_ancestor(OldHash, NewHash),
    Handled = ets:new(foo, [private, set]),
    update_pool_from_blocks(Ancestor, OldHash, Mempool, Handled),
    update_pool_from_blocks(Ancestor, NewHash, Mempool, Handled),
    ets:delete(Handled),
    ok.

update_pool_from_blocks(Hash, Hash,_Mempool,_Handled) -> ok;
update_pool_from_blocks(Ancestor, Current, Mempool, Handled) ->
    lists:foreach(fun(TxHash) ->
                          update_pool_on_tx_hash(TxHash, Mempool, Handled)
                  end,
                  aec_db:get_block_tx_hashes(Current)),
    Prev = aec_chain:prev_hash_from_hash(Current),
    update_pool_from_blocks(Ancestor, Prev, Mempool, Handled).

update_pool_on_tx_hash(TxHash, Mempool, Handled) ->
    case ets:member(Handled, TxHash) of
        true -> ok;
        false ->
            ets:insert(Handled, {TxHash}),
            Tx = aec_db:get_signed_tx(TxHash),
            case aec_db:is_in_tx_pool(TxHash) of
                false ->
                    %% Added to chain
                    delete_pool_db_by_hash(Mempool, TxHash);
                true ->
                    ets:insert(Mempool, {pool_db_key(Tx), Tx})
            end
    end.

delete_pool_db_by_hash(Mempool, TxHash) ->
    case select_pool_db_key_by_hash(Mempool, TxHash) of
        {ok, Key} -> ets:delete(Mempool, Key);
        not_in_ets -> pass
    end.


-spec pool_db_put(pool_db(), pool_db_key(), aetx_sign:signed_tx(), event()) ->
                         'ok' | {'error', atom()}.
pool_db_put(Mempool, Key, Tx, Event) ->
    Hash = aetx_sign:hash(Tx),
    case aec_db:find_tx_location(Hash) of
        BlockHash when is_binary(BlockHash) ->
            lager:debug("Already have tx: ~p in ~p", [Hash, BlockHash]),
            {error, already_accepted};
        mempool ->
            %% lager:debug("Already have tx: ~p in ~p", [Hash, mempool]),
            ok;
        none ->
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
                    case ets:member(Mempool, Key) of
                        true ->
                            lager:debug("Pool db key already present (~p)", [Key]),
                            %% TODO: We should make a decision whether to switch the tx.
                            ok;
                        false ->
                            lager:debug("Adding tx: ~p", [Hash]),
                            aec_db:add_tx(Tx),
                            ets:insert(Mempool, {Key, Tx}),
                            aec_events:publish(Event, Tx),
                            ok
                    end
            end
    end.

check_tx_ttl(STx, _Hash) ->
    Header = aec_chain:top_header(),
    Tx = aetx_sign:tx(STx),
    case aec_headers:height(Header) > aetx:ttl(Tx) of
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

check_minimum_gas_price(Tx,_Hash) ->
    case aetx:lookup_gas_price(aetx_sign:tx(Tx)) of
        none -> ok;
        {value, GasPrice} -> int_check_minimum_gas_price(GasPrice)
    end.

-dialyzer({no_match, int_check_minimum_gas_price/1}).
int_check_minimum_gas_price(GasPrice) ->
    case GasPrice >= aec_governance:minimum_gas_price() of
        true  -> ok;
        false -> {error, too_low_gas_price}
    end.

int_gas_price(Tx) ->
    case aetx:lookup_gas_price(Tx) of
        none -> ?PSEUDO_GAS_PRICE;
        {value, GP} when is_integer(GP), GP >= 0 -> GP
    end.
