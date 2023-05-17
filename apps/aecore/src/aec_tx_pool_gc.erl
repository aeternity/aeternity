%%%=============================================================================
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%    Garbage collection of mempool transactions
%%% @end
%%%=============================================================================

-module(aec_tx_pool_gc).

-behaviour(gen_server).

%% API
-export([ add_hash/4
        , add_to_origins_cache/3
        , adjust_ttl/2
        , ttl/2
        , delete_hash/2
        , gc/2
        , sync_gc/1
        ]).

-ifdef(TEST).
-export([ origins_cache_gc/0
        , stop/0]).
-endif.

%% gen_server API
-export([ start_link/0]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-include("aec_tx_pool.hrl").

-import(aeu_debug, [pp/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-export_type([pool_db_gc_key/0]).

-type pool_db_gc_key() :: aec_tx_pool:tx_hash().

-define(SERVER, ?MODULE).

%% More cleanup is needed here...
-record(state,
        {origins_cache = aec_tx_pool:origins_cache() :: aec_tx_pool:origins_cache(),
         dbs = aec_tx_pool:dbs_() :: aec_tx_pool:dbs()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec add_hash(aec_tx_pool:pool_db(), pool_db_gc_key(),
               aec_tx_pool:pool_db_key(), aetx:tx_ttl()) -> integer().
add_hash(MempoolGC, TxHash, Key, TTL) ->
    %% Use update_counter with a threshold to do the compare and maybe update
    %% efficiently.
    ets:update_counter(MempoolGC, TxHash, {#gc_tx.ttl, 0, TTL, TTL},
                       #gc_tx{hash = TxHash, ttl = TTL, key = Key}).

-spec adjust_ttl(integer(), aec_tx_pool:dbs()) -> ok.
adjust_ttl(Diff, Dbs) ->
    gen_server:cast(?SERVER, {adjust_ttl, Diff, Dbs}).

-spec ttl(pool_db_gc_key(), aec_tx_pool:dbs()) ->
    {ok, TTLHeight :: non_neg_integer()} | {error, not_found}.
ttl(TxHash, Dbs) ->
    gen_server:call(?SERVER, {ttl, TxHash, Dbs}).

delete_hash(MempoolGC, TxHash) ->
    aec_db:remove_tx_from_mempool(TxHash),
    ets:delete(MempoolGC, TxHash).

-spec gc(aec_blocks:height(), aec_tx_pool:dbs()) -> ok.
gc(Height, Dbs) ->
    gen_server:cast(?SERVER, {garbage_collect, Height, Dbs}).

sync_gc(Height) ->
    case aec_tx_pool:gc_height_and_dbs() of
        undefined ->
            undefined;
        {GcHeight, Dbs} ->
            case Height >= GcHeight of
                true ->
                    do_gc(Height, Dbs);
                false ->
                    0
            end
    end.

-spec add_to_origins_cache(aec_tx_pool:origins_cache(), aec_keys:pubkey(),
                           non_neg_integer()) -> ok.
add_to_origins_cache(OriginsCache, Origin, Nonce) ->
    gen_server:cast(?SERVER, {add_to_origins_cache, OriginsCache, Origin, Nonce}).

-ifdef(TEST).
origins_cache_gc() ->
    gen_server:call(?SERVER, origins_cache_gc).

stop() ->
    gen_server:stop(?SERVER).
-endif.

%%%===================================================================
%%% Gen server API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = start_origins_cache_gc(),
    {ok, #state{}}.

handle_call(origins_cache_gc, _From, S) ->
    ok = origins_cache_gc(S),
    {reply, ok, S};
handle_call({ttl, TxHash, Dbs}, _From, S) ->
    GCDb = aec_tx_pool:gc_db(Dbs),
    Res =
        case ets:lookup(GCDb, TxHash) of
            [#gc_tx{ttl = TTL}] ->
                {ok, TTL};
            [] -> {error, not_found}
        end,
    {reply, Res, S};
handle_call(_Req, _From, S) ->
    {reply, {error, unknown_request}, S}.

handle_cast({adjust_ttl, Diff, Dbs}, S) ->
    GCDb = aec_tx_pool:gc_db(Dbs),
    do_adjust_ttl(GCDb, Diff),
    {noreply, S};
handle_cast({garbage_collect, Height, Dbs}, S) ->
    ok = do_gc(Height, Dbs),
    {noreply, S};
handle_cast({add_to_origins_cache, OriginsCache, Origin, Nonce}, S) ->
    ok = do_add_to_origins_cache(OriginsCache, Origin, Nonce),
    {noreply, S};
handle_cast(Msg, S) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, S}.

handle_info(origins_cache_gc, S) ->
    ok = origins_cache_gc(S),
    erlang:send_after(30000, self(), origins_cache_gc),
    {noreply, S};
handle_info(Info, S) ->
    lager:debug("Ignoring unknown info: ~p", [Info]),
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_adjust_ttl(GCDb, Diff) ->
    do_adjust_ttl(GCDb, ets:first(GCDb), Diff).

do_adjust_ttl(_GCDb, '$end_of_table', _Diff) ->
    ok;
do_adjust_ttl(GCDb, TxHash, Diff) ->
    ets:update_counter(GCDb, TxHash, {#gc_tx.ttl, -Diff}),
    do_adjust_ttl(GCDb, ets:next(GCDb, TxHash), Diff).

do_gc(Height, Dbs) ->
    GCDb = aec_tx_pool:gc_db(Dbs),
    Cands = ets:select(
              GCDb, [{ #gc_tx{hash = '$1', ttl = '$2', key = '$3', _ = '_'},
                       [{'=<', '$2', Height}],
                       [{{'$1', '$3'}}] }]),
    case Cands of
        [_|_] ->
            GCTxs =
                aec_db:ensure_activity(
                  transaction,
                  fun() ->
                          lists:foldr(
                            fun maybe_gc_tx/2, [], Cands)
                  end),
            N = lists:foldl(fun(Tx, Acc) ->
                                    remove_from_ets(Tx, Dbs, GCDb),
                                    Acc + 1
                            end, 0,  GCTxs),
            aec_metrics:try_update([ae,epoch,aecore,tx_pool,gced], N),
            N;
        [] ->
            0
    end.

maybe_gc_tx({TxHash, _} = Tx, Acc) ->
    case aec_db:gc_tx(TxHash) of
        ok ->
            [Tx|Acc];
        {error, _} ->
            Acc
    end.

remove_from_ets({TxHash, Key}, Dbs, GCDb) ->
    aec_tx_pool:raw_delete(Dbs, Key),
    ets:delete(GCDb, TxHash),
    lager:debug("Garbage collected ~p", [pp(TxHash)]).

%%===================================================================
%% Origins cache
%%
%% Origins/accounts cache keeps track of {origin, nonce} pairs which
%% have been added to the chain. This allows earlier
%% garbage collection of potentially hanging transactions from
%% the same origin/account, but with nonce that will block them
%% from ever getting into the chain.
%% E.g. if a transaction from sender X with nonce Y made it into the
%% chain, all pending transactions from X with nonce =< Y can
%% be removed from the mempool.

%% Limit of origins to be processed in a single GC run
-define(OC_ENTRIES_TO_PROCESS_COUNT, 100).
%% Limit of transactions from a single origin to be processed in a single GC run
-define(OC_TXS_PER_ORIGIN_COUNT, 50).

-ifdef(TEST).
start_origins_cache_gc() ->
    ok.
-else.
start_origins_cache_gc() ->
    erlang:send_after(30000 + rand:uniform(10000), self(), origins_cache_gc),
    ok.
-endif.

do_add_to_origins_cache(OriginsCache, Origin, Nonce) ->
    case ets:info(OriginsCache, size) < aec_tx_pool:origins_cache_max_size() of
        true ->
            case ets:lookup(OriginsCache, Origin) of
                [] ->
                    true = ets:insert(OriginsCache, {Origin, Nonce}),
                    ok;
                [{Origin, N}] ->
                    %% Always keep the highest included nonce in cache
                    case Nonce > N of
                        true ->
                            true = ets:insert(OriginsCache, {Origin, Nonce}),
                            ok;
                        false -> ok
                    end
            end;
        false -> ok
    end.

origins_cache_gc(#state{origins_cache = OriginsCache, dbs = Dbs}) ->
    case origins_cache_get(OriginsCache, ?OC_ENTRIES_TO_PROCESS_COUNT) of
        [] -> ok;
        CacheEntries ->
            case aec_db:ensure_activity(
                   async_dirty,
                   fun() ->
                           aec_chain:get_block_state(aec_chain:top_block_hash())
                   end) of
                error ->
                    lager:info("Error trying to get top block state");
                {ok, Trees} ->
                    AccountsTree = aec_trees:accounts(Trees),
                    %% We run as much as we think safe in a dirty context.
                    %% The actual deletions are run in a transaction.
                    TxsForGc =
                        aec_db:ensure_activity(
                          async_dirty,
                          fun() ->
                                  origins_cache_gc(CacheEntries, AccountsTree, OriginsCache, Dbs)
                          end),
                    aec_db:ensure_activity(
                      transaction,
                      fun() ->
                              lists:foreach(
                                fun(Txs) ->
                                        remove_txs_from_dbs(Txs, Dbs)
                                end, TxsForGc)
                      end)
            end
    end.

origins_cache_get(OriginsCache, Size) ->
    Pat = [{ '_', [], ['$_'] }],
    case ets:select(OriginsCache, Pat, Size) of
        '$end_of_table'  -> [];
        {Entries, _Cont} -> Entries
    end.

origins_cache_gc(Entries, Tree, OriginsCache, Dbs) ->
    origins_cache_gc(Entries, Tree, OriginsCache, Dbs, []).

origins_cache_gc([], _AccountsTree, _OriginsCache, _Dbs, Acc) ->
    Acc;
origins_cache_gc([{Origin, Nonce} | Rest], AccountsTree, OriginsCache, Dbs, Acc) ->
    Acc1 = case aec_tx_pool:pool_db_peek(Dbs, ?OC_TXS_PER_ORIGIN_COUNT, Origin, Nonce) of
               [] ->
                   %% No hanging stale transactions, remove Origin from cache
                   true = ets:delete(OriginsCache, Origin),
                   Acc;
               SignedTxs ->
                   %% Origin cache information may be stale.
                   %% Re-check transactions, based on the account nonce from
                   %% accounts state tree.
                   case get_account(Origin, AccountsTree) of
                       none ->
                           Acc;
                       {value, Account} ->
                           AccountNonce = aec_accounts:nonce(Account),
                           SignedTxsForGc = filter_txs_by_account_nonce(SignedTxs, AccountNonce),
                           [SignedTxsForGc | Acc]
                   end
           end,
    origins_cache_gc(Rest, AccountsTree, OriginsCache, Dbs, Acc1).

filter_txs_by_account_nonce(SignedTxs, AccountNonce) ->
    lists:filter(fun(SignedTx) ->
                         Tx = aetx_sign:tx(SignedTx),
                         aetx:nonce(Tx) =< AccountNonce
                 end, SignedTxs).

remove_txs_from_dbs([], _Dbs) ->
    ok;
remove_txs_from_dbs([SignedTx | Rest], Dbs) ->
    TxHash = aetx_sign:hash(SignedTx),
    case aec_db:gc_tx(TxHash) of
        ok ->
            aec_tx_pool:raw_delete(Dbs, aec_tx_pool:pool_db_key(SignedTx)),
            delete_hash(aec_tx_pool:gc_db(Dbs), TxHash),
            aec_metrics:try_update([ae,epoch,aecore,tx_pool,origin_gced], 1);
        {error, Reason} ->
            lager:debug("TX Origin-based garbage collect of ~p failed: ~p",
                        [pp(TxHash), Reason])
    end,
    remove_txs_from_dbs(Rest, Dbs).

get_account(AccountKey, AccountsTrees) ->
    aec_accounts_trees:lookup(AccountKey, AccountsTrees).
