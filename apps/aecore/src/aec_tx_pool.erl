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
-define(MEMPOOL_VISITED, mempool_visited).
-define(MEMPOOL_NONCE, mempool_nonce).
-define(MEMPOOL_GC, mempool_gc).
-define(ORIGINS_CACHE, origins_cache).
-define(ORIGINS_CACHE_MAX_SIZE, 5000).
-define(KEY(NegFee, NegGasPrice, Origin, Nonce, TxHash),
           {NegFee, NegGasPrice, Origin, Nonce, TxHash}).
-define(VALUE_POS, 2).
-define(KEY_AS_MATCH_SPEC_RESULT(NegFee, NegGasPrice, Origin, Nonce, TxHash),
        { %% Tuple of arity 1 where the single element is the mempool key tuple.
         ?KEY(NegFee, NegGasPrice, Origin, Nonce, TxHash)
        }).
-define(KEY_NONCE_PATTERN(Sender), {?KEY('_', '_', Sender, '$1', '_'), '_'}).
-define(DEFAULT_PUSH_TIMEOUT, 5000).

%% API
-export([ start_link/0
        , stop/0
        ]).

-export([ garbage_collect/0
        , get_candidate/2
        , get_max_nonce/1
        , minimum_miner_gas_price/0
        , maximum_auth_fun_gas/0
        , new_sync_top_target/1
        , peek/1
        , peek/2
        , push/1
        , push/2
        , push/3
        , size/0
        , top_change/3
        , dbs/0
        ]).

%% exports used by GC (should perhaps be in a common lib module)
-export([ dbs_/0
        , gc_db/1
        , origins_cache/0
        , origins_cache_max_size/0
        , pool_db/0
        , pool_db_nonce/0
        , pool_db_key/1
        , pool_db_gc/0
        , pool_db_peek/4
        , raw_delete/2
        ]).

-include("aec_tx_pool.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-ifdef(TEST).
-export([sync_garbage_collect/1]). %% Only for (Unit-)test
-export([restore_mempool/0]).
-export([peek_db/0]).
-export([peek_visited/0]).
-export([peek_nonces/0]).
-endif.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([ dbs/0
             , origins_cache/0
             , pool_db/0
             , pool_db_key/0
             , tx_hash/0]).

-import(aeu_debug, [pp/1]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-define(TC(Expr, Msg), begin {Time, Res} = timer:tc(fun() -> Expr end), lager:debug("[~p] Msg = ~p", [Time, Msg]), Res end).

-record(dbs, {db         = pool_db()         :: pool_db(),
              visited_db = pool_db_visited() :: pool_db(),
              nonce_db   = pool_db_nonce()   :: pool_db(),
              gc_db      = pool_db_gc()      :: pool_db()}).
-type dbs() :: #dbs{}.

-record(state, { dbs = #dbs{}
               , sync_top_calc           :: pid() | undefined
                 %% Used at tx insertion and at tx re-insertion (on
                 %% chain fork change) for preventing GCing received
                 %% txs while syncing with a stronger (i.e. with a
                 %% higher cumulative difficulty) and much longer
                 %% fork - typically at bootstrap.
               , gc_height = 0 :: aec_blocks:height() | undefined
               , origins_cache = origins_cache() :: origins_cache() }).

-type negated_fee() :: non_pos_integer().
-type non_pos_integer() :: neg_integer() | 0.
-type tx_hash() :: binary().

-type pool_db_key() :: ?KEY(negated_fee(), aect_contracts:amount(),
                            aec_keys:pubkey(), non_neg_integer(), binary()).
-type pool_db_value() :: aetx_sign:signed_tx().

-type pool_db() :: atom().

-type origins_cache() :: atom().

-type event() :: tx_created | tx_received.

-type push_timeout() :: non_neg_integer() | infinity.

-ifndef(TEST).
-define(DEFAULT_TX_TTL, 256).
-define(DEFAULT_INVALID_TX_TTL, 5).
-else.
-define(DEFAULT_TX_TTL, 8).
-define(DEFAULT_INVALID_TX_TTL, 5).
-endif.

-define(DEFAULT_NONCE_BASELINE, 1).
-define(DEFAULT_NONCE_OFFSET, 5).
-define(DEFAULT_MIN_MINER_GAS_PRICE, 1000000000).
-define(DEFAULT_MAX_AUTH_FUN_GAS, 50000).

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
push(Tx, Event) ->
    push(Tx, Event, ?DEFAULT_PUSH_TIMEOUT).

-spec push(aetx_sign:signed_tx(), event(), push_timeout()) -> ok | {error, atom()}.
push(Tx, Event = tx_received, Timeout) ->
    TxHash = safe_tx_hash(Tx),
    case aec_tx_gossip_cache:in_cache(TxHash) of
        true -> ok;
        false ->
            aec_jobs_queues:run(tx_pool_push, fun() -> push_(Tx, TxHash, Event, Timeout) end)
    end;
push(Tx, Event = tx_created, Timeout) ->
    push_(Tx, safe_tx_hash(Tx), Event, Timeout).

safe_tx_hash(Tx) ->
    try aetx_sign:hash(Tx)
    catch _:_ ->
            incr([push, illegal]),
            error({illegal_transaction, Tx})
    end.

push_(Tx, TxHash, Event, Timeout) ->
    case check_pool_db_put(Tx, TxHash, Event) of
        ignore ->
            incr([push, ignore]),
            ok;
        {error,_} = E ->
            incr([push, error]),
            E;
        ok ->
            incr([push]),
            gen_server:call(?SERVER, {push, Tx, TxHash, Event}, Timeout)
    end.

incr(Metric) ->
    aec_metrics:try_update([ae,epoch,aecore,tx_pool | Metric], 1).

-spec get_max_nonce(aec_keys:pubkey()) -> {ok, non_neg_integer()} | undefined.
get_max_nonce(Sender) ->
    #dbs{nonce_db = NDb} = dbs(),
    ?TC(int_get_max_nonce(NDb, Sender), {max_nonce, Sender}).

-spec garbage_collect() -> ok.
garbage_collect() ->
    lager:debug("garbage_collect()", []),
    gen_server:cast(?SERVER, garbage_collect).

-ifdef(TEST).
-spec sync_garbage_collect(Height :: aec_blocks:height()) -> ok.
sync_garbage_collect(Height) ->
    aec_tx_pool_gc:gc(Height, dbs()),
    sys:get_status(aec_tx_pool_gc), %% sync point (gc is asynchronous)
    ok.

restore_mempool() ->
    revisit(dbs()).

peek_db() ->
    #dbs{db = Db} = dbs(),
    [Tx || {_, Tx} <- ets:tab2list(Db)].

peek_visited() ->
    #dbs{visited_db = VDb} = dbs(),
    [Tx || {_, Tx} <- ets:tab2list(VDb)].

peek_nonces() ->
    #dbs{nonce_db = NDb} = dbs(),
    [N || {N} <- ets:tab2list(NDb)].
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
    ?TC(int_get_candidate(MaxGas, BlockHash, dbs()),
        {get_candidate, MaxGas, BlockHash}).

%% It assumes that the persisted mempool has been updated.
-spec top_change(key | micro, binary(), binary()) -> ok.
top_change(Type, OldHash, NewHash) when Type==key; Type==micro ->
    gen_server:call(?SERVER, {top_change, Type, OldHash, NewHash}).

-spec new_sync_top_target(aec_blocks:height()) -> ok.
new_sync_top_target(NewSyncTop) ->
    lager:debug("new_sync_top_target()", []),
    gen_server:cast(?SERVER, {new_sync_top_target, NewSyncTop}).

-spec size() -> non_neg_integer() | undefined.
size() ->
    ensure_num(ets:info(?MEMPOOL, size))
        + ensure_num(ets:info(?MEMPOOL_VISITED, size)).

ensure_num(undefined) -> 0;
ensure_num(N) when is_integer(N) -> N.

pool_db() -> ?MEMPOOL.
pool_db_visited() -> ?MEMPOOL_VISITED.
pool_db_nonce() -> ?MEMPOOL_NONCE.
pool_db_gc() -> ?MEMPOOL_GC.

origins_cache() -> ?ORIGINS_CACHE.
origins_cache_max_size() -> ?ORIGINS_CACHE_MAX_SIZE.

%% This function is primarily used to sync with the aec_tx_pool server,
%% in order to preserve order of operations (e.g. if you push a tx, then
%% peek, you should see the tx you just pushed.)
dbs() ->
    gen_server:call(?SERVER, dbs).

raw_delete(#dbs{} = Dbs, Key) ->
    pool_db_raw_delete(Dbs, Key).

-spec dbs_() -> dbs().
dbs_() ->
    #dbs{}.

-spec gc_db(dbs()) -> pool_db().
gc_db(#dbs{gc_db = GcDb}) ->
    GcDb.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, _MempoolDb} = pool_db_open(pool_db()),
    {ok, _VisitedDb} = pool_db_open(pool_db_visited()),
    {ok, _NonceDb} = pool_db_open(pool_db_nonce()),
    %% The gc db should be owned by this process to ensure that the gc state
    %% is consistent with the actual mempool if any of the servers restart.
    {ok, _GCDb} = pool_db_open(pool_db_gc(), [{keypos, #tx.hash}]),
    origins_cache_open(origins_cache()),
    GCHeight = top_height(),
    Handled  = ets:new(init_tx_pool, [private]),
    InitF  = fun(TxHash, _) ->
                     update_pool_on_tx_hash(TxHash, {#dbs{}, origins_cache(), GCHeight}, Handled),
                     ok
             end,
    ok = aec_db:ensure_transaction(fun() -> aec_db:fold_mempool(InitF, ok) end),
    ets:delete(Handled),
    lager:debug("init: GCHeight = ~p", [GCHeight]),
    {ok, #state{gc_height = GCHeight}}.

handle_call(Req, From, St) ->
    ?TC(handle_call_(Req, From, St), Req).

handle_call_({get_max_nonce, Sender}, _From, #state{dbs = #dbs{db = Db}} = State) ->
    {reply, int_get_max_nonce(Db, Sender), State};
handle_call_({push, Tx, Hash, Event}, _From, State) ->
    {Res, State1} = do_pool_db_put(pool_db_key(Tx), Tx, Hash, Event, State),
    {reply, Res, State1};
handle_call_({top_change, Type, OldHash, NewHash}, _From, State) ->
    {_, State1} = do_top_change(Type, OldHash, NewHash, State),
    {reply, ok, State1};
handle_call_({peek, MaxNumberOfTxs, Account}, _From, #state{dbs = Dbs} = State)
  when is_integer(MaxNumberOfTxs), MaxNumberOfTxs >= 0;
       MaxNumberOfTxs =:= infinity ->
    Txs = pool_db_peek(Dbs, MaxNumberOfTxs, Account, all),
    {reply, {ok, Txs}, State};
handle_call_(dbs, _From, #state{dbs = Dbs} = State) ->
    {reply, Dbs, State};
handle_call_(Request, From, State) ->
    lager:warning("Ignoring unknown call request from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, St) ->
    ?TC(handle_cast_(Msg, St), Msg).

handle_cast_({new_sync_top_target, NewSyncTop}, State) ->
    {noreply, do_update_sync_top_target(NewSyncTop, State)};
handle_cast_(garbage_collect, State) ->
    case State of
        #state{gc_height = undefined, sync_top_calc = P} when is_pid(P) ->
            %% sync_top update will be followed by GC (in handle_info/2 below)
            {noreply, State};
        #state{gc_height = H} when is_integer(H) ->
            State1 = do_update_sync_top_target(H, State),
            {noreply, State1}
    end;
handle_cast_(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(Msg, St) ->
    ?TC(handle_info_(Msg, St), Msg).

handle_info_({P, new_gc_height, GCHeight}, #state{sync_top_calc = P} = State) ->
    aec_tx_pool_gc:gc(GCHeight, State#state.dbs),
    {noreply, State#state{sync_top_calc = undefined, gc_height = GCHeight}};
handle_info_({'ETS-TRANSFER', _, _, _}, State) ->
    {noreply, State};
handle_info_(Info, State) ->
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
int_get_max_nonce(NonceDb, Sender) ->
    case ets:prev(NonceDb, {Sender, '$larger_than_any_nonce', <<>>}) of
        {Sender, MaxNonce, _} ->
            {ok, MaxNonce};
        _ ->
            undefined
    end.

%% Ensure ordering of tx nonces in one account, and for duplicate account nonces
%% we only get the one with higher fee or - if equal fee - higher gas price.
%% int_get_candidate(#state{ db = Db, gc_db = GCDb }, MaxGas, BlockHash) ->
%%
%% Once selected into a candidate set, transactions are moved to the 'visited'
%% table until the next key block top_change. This means they won't be
%% considered for another microblock until the next leader cycle.
%% ... Unless no matching txs can be found in the regular mempool.
%%
int_get_candidate(MaxGas, BlockHash, #dbs{db = Db} = DBs) ->
    {ok, Trees} = aec_chain:get_block_state(BlockHash),
    {ok, Header} = aec_chain:get_header(BlockHash),
    lager:debug("size(Db) = ~p", [ets:info(Db, size)]),
    MinMinerGasPrice = aec_tx_pool:minimum_miner_gas_price(),
    MinTxGas = aec_governance:min_tx_gas(),
    Acc0     = #{ tree => gb_trees:empty(), txs => [], bad_txs => [] },
    {ok, RemGas, Acc} = int_get_candidate(Db, MaxGas, MinTxGas, MinMinerGasPrice, Trees,
                                          Header, DBs, Acc0),
    {ok, _, Acc1} = int_get_candidate(
                      DBs#dbs.visited_db, RemGas, MinTxGas, MinMinerGasPrice, Trees, Header,
                      DBs, Acc),
    #{ tree := AccTree, txs := AccTxs, bad_txs := AccBadTxs } = Acc1,

    %% Move Txs to visited *after* revisiting visited!
    AllVisited = gb_trees:values(AccTree) ++ lists:reverse(AccTxs) ++ lists:reverse(AccBadTxs),
    Txs = [ begin
                move_to_visited(DbX, DBs, KeyX, TxX),
                TxX
            end || {DbX, KeyX, TxX} <- AllVisited ],

    {ok, Txs}.

int_get_candidate(Db, Gas, MinTxGas, MinMinerGasPrice, Trees, Header, DBs, Acc)
  when Gas > MinTxGas ->
    Pat = [{ '_', [], ['$_'] }],
    int_get_candidate_fold(Gas, MinTxGas, MinMinerGasPrice, Db, DBs,
                           ets:select(Db, Pat, 20),
                           {account_trees, aec_trees:accounts(Trees)},
                           aec_headers:height(Header), Acc);
int_get_candidate(Gas, _, _, _, _, _, _, Acc) ->
    {ok, Gas, Acc}.

int_get_candidate_fold(Gas, MinTxGas, _MinMinerGasPrice, _Db, _Dbs, _Txs,
                       _ATrees, _Height, Acc) when Gas =< MinTxGas ->
    {ok, Gas, Acc};
int_get_candidate_fold(Gas, MinTxGas, MinMinerGasPrice, Db, Dbs = #dbs{}, {Txs, Cont},
                       AccountsTree, Height, Acc) when Gas > MinTxGas ->
    {RemGas, NewAcc} = fold_txs(Txs, Gas, MinTxGas, MinMinerGasPrice, Db, Dbs,
                                AccountsTree, Height, Acc),
    int_get_candidate_fold(RemGas, MinTxGas, MinMinerGasPrice, Db, Dbs, ets:select(Cont),
                           AccountsTree, Height, NewAcc);
int_get_candidate_fold(RemGas, _GL, _MMGP, _Db, _Dbs, '$end_of_table', _AccountsTree,
                       _Height, Acc) ->
    {ok, RemGas, Acc}.

fold_txs([Tx|Txs], Gas, MinTxGas, MinMinerGasPrice, Db, Dbs, AccountsTree, Height, Acc) ->
    if Gas > MinTxGas ->
            {Gas1, Acc1} =
                int_get_candidate_(
                  Tx, Gas, MinMinerGasPrice, Db, Dbs, AccountsTree, Height, Acc),
            fold_txs(Txs, Gas1, MinTxGas, MinMinerGasPrice, Db, Dbs, AccountsTree, Height, Acc1);
       true ->
            {Gas, Acc}
    end;
fold_txs([], Gas, _, _, _, _, _, _, Acc) ->
    {Gas, Acc}.

int_get_candidate_({?KEY(_, _, Account, Nonce, _) = Key, Tx},
                   Gas, MinMinerGasPrice, Db, Dbs, AccountsTree, Height,
                   Acc = #{ tree := AccTree }) ->
    case gb_trees:is_defined({Account, Nonce}, AccTree) of
        true when Nonce > 0 ->
            %% The earlier non-meta Tx must have had higher fee. Skip this tx.
            {Gas, Acc};
        false ->
            check_candidate(
              Db, Dbs, Key, Tx, AccountsTree, Height, Gas, MinMinerGasPrice, Acc)
    end.

check_candidate(Db, #dbs{gc_db = GCDb} = _Dbs,
                ?KEY(_, _, Account, Nonce, TxHash) = Key, Tx,
                AccountsTree, Height, Gas, MinMinerGasPrice,
                Acc = #{ tree := AccTree, txs := AccTxs }) ->
    Tx1 = aetx_sign:tx(Tx),
    TxTTL = aetx:ttl(Tx1),
    TxGas = aetx:gas_limit(Tx1, Height),
    case Height < TxTTL andalso TxGas > 0
         andalso ok =:= int_check_account(Tx, AccountsTree, check_candidate)
         andalso MinMinerGasPrice =< aetx:min_gas_price(Tx1, Height) of
        true ->
            case Gas - TxGas of
                RemGas when RemGas >= 0 ->
                    case Nonce of
                        0 -> %% Meta tx
                            {RemGas, Acc#{ txs := [{Db, Key, Tx} | AccTxs] }};
                        _N ->
                            {RemGas, Acc#{ tree := gb_trees:insert({Account, Nonce}, {Db, Key, Tx}, AccTree) }}
                    end;
                _ ->
                    %% Check the rest of txs, maybe some of them fits
                    %% into the gas limit.
                    {Gas, Acc}
            end;
        false ->
            %% This is not valid anymore.
            enter_tx_gc(GCDb, TxHash, Key, Height + invalid_tx_ttl()),
            {Gas, Acc#{ bad_txs := [{Db, Key, Tx} | maps:get(bad_txs, Acc)] }}
    end.

move_to_visited(VDb, #dbs{visited_db = VDb}, _, _) ->
    %% already in visited
    ignore;
move_to_visited(Db, #dbs{visited_db = VDb}, Key, Tx) ->
    ets:insert(VDb, {Key, Tx}),
    ets:delete(Db, Key),
    ok.

revisit(#dbs{db = Db, visited_db = VDb}) ->
    [ets:insert_new(Db, V) || V <- ets:tab2list(VDb)],
    ets:delete_all_objects(VDb),
    ok.

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
                                            , sync_top_calc = OldP
                                            , dbs = Dbs } = State) ->
    case OldP of
        undefined ->
            Me = self(),
            NewP = proc_lib:spawn_link(
                     fun() ->
                             do_update_sync_top(NewSyncTop, GCHeight, Me, Dbs)
                     end),
            State#state{ sync_top_calc = NewP, gc_height = undefined };
        _ when is_pid(OldP) ->
            {_, State1} = get_gc_height(State),
            do_update_sync_top_target(NewSyncTop, State1)
    end.

do_update_sync_top(NewSyncTop, GCHeight, Parent, Dbs) ->
    LocalTop = top_height(),
    lager:debug("do_update_sync_top(~p,~p,~p), LocalTop = ~p",
                [NewSyncTop, GCHeight, Parent, LocalTop]),
    NewGCHeight = lists:max([LocalTop, GCHeight, NewSyncTop]),
    Parent ! {self(), new_gc_height, NewGCHeight},
    try NewGCHeight < GCHeight - 5 of
        true ->
            %% This is a special case, normally the height shouldn't go down
            %% this means that the sync was aborted - in this case we
            %% re-adjust the GC-height for all TXs in order not to carry around
            %% transactions unnecessarily.
            aec_tx_pool_gc:adjust_ttl(GCHeight - NewGCHeight, Dbs);
        false ->
            ok
    catch
        error:E ->
            lager:error(
              "do_update_sync_top(~p,~p,~p), LocalTop=~p, NewGCHeight=~p ERROR: ~p/~p",
              [NewSyncTop, GCHeight, Parent, LocalTop, NewGCHeight, E, erlang:get_stacktrace()])
    end.

-spec pool_db_key(aetx_sign:signed_tx()) -> pool_db_key().
pool_db_key(SignedTx) ->
    Tx = aetx_sign:tx(SignedTx),
    %% INFO: Sort by fee, then by gas price, then by origin, then by nonce

    %% INFO: * given that nonce is an index of transactions for a user,
    %%         the following key is unique for a transaction
    %%       * negative fee and negative gas price place high profit
    %%         transactions at the beginning
    %%       * ordered_set type enables implicit overwrite of the same txs
    ?KEY(-aetx:deep_fee(Tx), -int_gas_price(Tx),
         aetx:origin(Tx), aetx:nonce(Tx), aetx_sign:hash(SignedTx)).

-spec pool_db_open(DbName :: atom()) -> {ok, pool_db()}.
pool_db_open(DbName) ->
    pool_db_open(DbName, []).

-spec pool_db_open(DbName :: atom(), list()) -> {ok, pool_db()}.
pool_db_open(DbName, ExtraOpts) ->
    {ok, ets:new(DbName, [ordered_set, public, named_table] ++ ExtraOpts)}.

-spec origins_cache_open(CacheName :: atom()) -> origins_cache().
origins_cache_open(Name) ->
    ets:new(Name, [set, public, named_table]).

-spec pool_db_peek(dbs(), MaxNumber::pos_integer() | infinity,
                   aec_keys:pubkey() | all, non_neg_integer() | all) ->
                          [pool_db_value()].
pool_db_peek(_, 0, _, _) -> [];
pool_db_peek(#dbs{db = Db, visited_db = VDb}, Max, all, _MaxNonce) ->
    Pat = [{ '_', [], ['$_'] }],
    pool_db_peek_(VDb, Db, Pat, Max);
pool_db_peek(#dbs{db = Db, visited_db = VDb}, Max, Account, all) ->
    Pat = [{ {?KEY('_', '_', Account, '_', '_'), '_'}, [], ['$_'] }],
    pool_db_peek_(VDb, Db, Pat, Max);
pool_db_peek(#dbs{db = Db, visited_db = VDb}, Max, Account, MaxNonce) ->
    Pat = [{ {?KEY('_', '_', Account, '$1', '_'), '_'}, [ {'=<', '$1' , MaxNonce} ], ['$_'] }],
    pool_db_peek_(VDb, Db, Pat, Max).

pool_db_peek_(VDb, Db, Pat, Max) ->
    case sel_return(ets_select(VDb, Pat, Max)) of
        [] ->
            [Tx || {_, Tx} <- sel_return(ets_select(Db, Pat, Max))];
        Vs ->
            pool_db_merge(Vs, sel_return(ets_select(Db, Pat, Max)), Max)
    end.

pool_db_merge(L1, L2, infinity) ->
    pool_db_merge(L1, L2, length(L1) + length(L2));
pool_db_merge([{K1,V1}|T1] = L1, [{K2,V2}|T2] = L2, N) when N>0 ->
    if K1 < K2 ->
            [V1 | pool_db_merge(T1, L2, N-1)];
       true ->
            [V2 | pool_db_merge(L1, T2, N-1)]
    end;
pool_db_merge(_, _, 0) -> [];
%% the following clauses work also when L1 == L2 == [], and/or N == 0
pool_db_merge([], L2, N) ->
    [Tx || {_, Tx} <- lists:sublist(L2, N)];
pool_db_merge(L1, [], N) ->
    [Tx || {_, Tx} <- lists:sublist(L1, N)].


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

do_top_change(Type, OldHash, NewHash, State0) ->
    %% Add back transactions to the pool from discarded part of the chain
    %% Mind that we don't need to add those which are incoming in the fork

    %% NG: does this work for common ancestor for micro blocks?
    {ok, Ancestor} = aec_chain:find_common_ancestor(OldHash, NewHash),
    {GCHeight, State} = get_gc_height(State0),
    Info = {State#state.dbs, State#state.origins_cache, GCHeight},

    Handled = ets:new(foo, [private, set]),
    update_pool_from_blocks(Ancestor, OldHash, Info, Handled),
    update_pool_from_blocks(Ancestor, NewHash, Info, Handled),
    ets:delete(Handled),
    Ret = case Type of
              key -> revisit(State#state.dbs);
              micro -> ok
          end,
    {Ret, State}.

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

update_pool_on_tx_hash(TxHash, {#dbs{gc_db = GCDb} = Dbs, OriginsCache, GCHeight}, Handled) ->
    case ets:member(Handled, TxHash) of
        true -> ok;
        false ->
            ets:insert(Handled, {TxHash}),
            Tx = aec_db:get_signed_tx(TxHash),
            case aec_db:is_in_tx_pool(TxHash) of
                false ->
                    %% Added to chain
                    Key = pool_db_key(Tx),
                    pool_db_raw_delete(Dbs, Key),
                    aec_tx_pool_gc:delete_hash(GCDb, TxHash),
                    add_to_origins_cache(OriginsCache, Tx);
                true ->
                    Key = pool_db_key(Tx),
                    pool_db_raw_put(Dbs, GCHeight, Key, Tx, TxHash)
            end
    end.

add_to_origins_cache(OriginsCache, SignedTx) ->
    Tx     = aetx_sign:tx(SignedTx),
    Origin = aetx:origin(Tx),
    Nonce  = aetx:nonce(Tx),
    ok = aec_tx_pool_gc:add_to_origins_cache(OriginsCache, Origin, Nonce).

-spec check_pool_db_put(aetx_sign:signed_tx(), tx_hash(), event()) ->
                               ignore | ok | {error, atom()}.
check_pool_db_put(Tx, TxHash, Event) ->
    case aec_chain:find_tx_location(TxHash) of
        BlockHash when is_binary(BlockHash) ->
            lager:debug("Already have tx: ~p in ~p", [TxHash, BlockHash]),
            {error, already_accepted};
        mempool ->
            %% lager:debug("Already have tx: ~p in ~p", [TxHash, mempool]),
            ignore;
        none ->
            lager:debug("Already have GC:ed tx: ~p", [TxHash]),
            ignore;
        not_found ->
            Checks = [ fun check_valid_at_protocol/4
                     , fun check_signature/4
                     , fun check_account/4
                     , fun check_minimum_fee/4
                     , fun check_minimum_gas_price/4
                     , fun check_minimum_miner_gas_price/4
                     , fun check_tx_ttl/4
                     ],
            Height = top_height(),
            case aeu_validation:run(Checks, [Tx, TxHash, Height, Event]) of
                {error, _} = E ->
                    lager:debug("Validation error for tx ~p: ~p", [TxHash, E]),
                    E;
                ok ->
                    ok
            end
    end.

do_pool_db_put(Key, Tx, Hash, Event,
               #state{ dbs = #dbs{db = Db} = Dbs } = St0) ->
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
            pool_db_raw_put(Dbs, GCHeight, Key, Tx, Hash),
            {ok, St}
    end.

pool_db_raw_delete(#dbs{db = Db, visited_db = VDb, nonce_db = NDb}, Key) ->
    delete_nonce(NDb, Key),
    ets:delete(VDb, Key),
    ets:delete(Db, Key).

pool_db_raw_put(#dbs{db = Db, nonce_db = NDb, gc_db = GCDb},
                GCHeight, Key, Tx, TxHash) ->
    ets:insert(Db, {Key, Tx}),
    insert_nonce(NDb, Key),
    enter_tx_gc(GCDb, TxHash, Key, min(GCHeight + tx_ttl(), aetx:ttl(aetx_sign:tx(Tx)))).

insert_nonce(NDb, ?KEY(_, _, Account, Nonce, TxHash)) ->
    ets:insert(NDb, {{Account, Nonce, TxHash}}).

delete_nonce(NDb, ?KEY(_, _, Account, Nonce, TxHash)) ->
    ets:delete(NDb, {Account, Nonce, TxHash}).

check_tx_ttl(STx, _Hash, Height, _Event) ->
    Tx = aetx_sign:tx(STx),
    case Height > aetx:ttl(Tx) of
        true  -> {error, ttl_expired};
        false -> ok
    end.

check_valid_at_protocol(STx, _Hash, Height, _Event) ->
    aetx:check_protocol_at_height(aetx_sign:tx(STx), Height).

check_signature(Tx, Hash, _Height, _Event) ->
    {ok, Trees} = aec_chain:get_top_state(),
    case aetx_sign:verify(Tx, Trees, top_height()) of
        {error, _} = E ->
            lager:info("Failed signature check on tx: ~p, ~p\n", [E, Hash]),
            E;
        ok ->
            ok
    end.

check_account(Tx, _Hash, _Height, Event) ->
    int_check_account(Tx, {block_hash, aec_chain:top_block_hash()}, Event).


int_check_account(Tx, Source, Event) ->
    CheckNonce = nonce_check_by_event(Event),

    %% Check is conservative and only rejects certain cases
    Unsigned = aetx_sign:tx(Tx),
    TxNonce = aetx:nonce(Unsigned),
    {TxType, MetaTx} = aetx:specialize_type(Unsigned),
    case {aetx:origin(Unsigned), TxType} of
        {undefined, _} ->
            {error, no_origin};
        {Pubkey, ga_meta_tx} when is_binary(Pubkey) ->
            int_check_meta_tx(TxNonce, MetaTx, Pubkey, Source);
        {Pubkey, _} when is_binary(Pubkey) ->
            case TxNonce > 0 of
                false ->
                    {error, illegal_nonce};
                true ->
                    case get_account(Pubkey, Source) of
                        {error, no_state_trees} -> nonce_baseline_check(TxNonce, CheckNonce);
                        none -> nonce_baseline_check(TxNonce, CheckNonce);
                        {value, Account} ->
                            Offset   = nonce_offset(),
                            AccNonce = aec_accounts:nonce(Account),
                            AccType  = aec_accounts:type(Account),
                            if
                                AccType == generalized ->
                                    {error, generalized_account_cant_sign_non_meta_tx};
                                TxNonce =< AccNonce -> {error, nonce_too_low};
                                TxNonce =< (AccNonce + Offset) -> ok;
                                TxNonce >  (AccNonce + Offset) andalso not CheckNonce -> ok;
                                TxNonce >  (AccNonce + Offset) -> {error, nonce_too_high}
                            end
                    end
            end
    end.

int_check_meta_tx(0, MetaTx, Pubkey, Source) ->
    Gas    = aega_meta_tx:gas(MetaTx),
    case Gas =< aec_tx_pool:maximum_auth_fun_gas() of
        true ->
            case get_account(Pubkey, Source) of
                {value, Account} ->
                    TotalAmount = Gas * aega_meta_tx:gas_price(MetaTx) +
                                    aega_meta_tx:fee(MetaTx),
                    case TotalAmount =< aec_accounts:balance(Account) of
                        true  -> ok;
                        false -> {error, insufficient_funds}
                    end;
                _ ->
                    {error, authenticating_account_does_not_exist}
            end;
        false ->
            {error, too_much_gas_for_auth_function}
    end;
int_check_meta_tx(_, _, _, _) ->
    {error, illegal_nonce}.


nonce_check_by_event(tx_created) -> true;
nonce_check_by_event(tx_received) -> false;
nonce_check_by_event(check_candidate) ->
    false.

nonce_baseline_check(_, false) -> ok;
nonce_baseline_check(TxNonce, _) ->
    case TxNonce =< nonce_baseline() of
        true -> ok;
        false -> {error, nonce_too_high}
    end.

get_account(AccountKey, {account_trees, AccountsTrees}) ->
    aec_accounts_trees:lookup(AccountKey, AccountsTrees);
get_account(AccountKey, {block_hash, BlockHash}) ->
    aec_chain:get_account_at_hash(AccountKey, BlockHash).

check_minimum_fee(Tx0, _Hash, Height, _Event) ->
    Tx = aetx_sign:tx(Tx0),
    case aetx:fee(Tx) >= aetx:min_fee(Tx, Height) of
        true  -> ok;
        false -> {error, too_low_fee}
    end.

check_minimum_miner_gas_price(Tx, _Hash, Height, _Event) ->
    MinMinerGasPrice = aec_tx_pool:minimum_miner_gas_price(),
    case aetx:min_gas_price(aetx_sign:tx(Tx), Height) >= MinMinerGasPrice of
        true  -> ok;
        false -> {error, too_low_gas_price_for_miner}
    end.

check_minimum_gas_price(Tx, _Hash, Height, _Event) ->
    case aetx:gas_price(aetx_sign:tx(Tx)) of
        undefined ->
            ok;
        GasPrice when is_integer(GasPrice) ->
            case GasPrice >= aec_governance:minimum_gas_price(Height) of
                true  -> ok;
                false -> {error, too_low_gas_price}
            end
    end.

int_gas_price(Tx) ->
    case aetx:gas_price(Tx) of
        GasPrice when is_integer(GasPrice), GasPrice >= 0 ->
            GasPrice;
        undefined ->
            %% Non-contract txs have 0 gas price.
            0
    end.

tx_ttl() ->
    aeu_env:user_config_or_env([<<"mempool">>, <<"tx_ttl">>],
                               aecore, mempool_tx_ttl, ?DEFAULT_TX_TTL).

invalid_tx_ttl() ->
    aeu_env:user_config_or_env([<<"mempool">>, <<"invalid_tx_ttl">>],
                               aecore, mempool_invalid_tx_ttl, ?DEFAULT_INVALID_TX_TTL).

nonce_baseline() ->
    aeu_env:user_config_or_env([<<"mempool">>, <<"nonce_baseline">>],
                               aecore, mempool_nonce_baseline, ?DEFAULT_NONCE_BASELINE).

nonce_offset() ->
    aeu_env:user_config_or_env([<<"mempool">>, <<"nonce_offset">>],
                               aecore, mempool_nonce_offset, ?DEFAULT_NONCE_OFFSET).

minimum_miner_gas_price() ->
    aeu_env:user_config_or_env([<<"mining">>, <<"min_miner_gas_price">>],
                               aecore, mining_min_miner_gas_price, ?DEFAULT_MIN_MINER_GAS_PRICE).

maximum_auth_fun_gas() ->
    aeu_env:user_config_or_env([<<"mining">>, <<"max_auth_fun_gas">>],
                               aecore, mining_max_auth_fun_gas, ?DEFAULT_MAX_AUTH_FUN_GAS).
