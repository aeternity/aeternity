-module(aec_tx_pool_gc).
-behaviour(gen_server).

-export([
          start_link/0
        , stop/0
        , gc/1
        , delete_hash/2
        , add_hash/4
        , adjust_ttl/1
        ]).

-export([
          init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).


-import(aeu_debug, [pp/1]).

-export_type([pool_db_gc_key/0]).

-type pool_db() :: atom().
-type pool_db_gc_key() :: aec_tx_pool:tx_hash().

-define(SERVER, ?MODULE).

-record(st, { gc_db = aec_tx_pool:pool_db_gc() :: pool_db() }).

-record(tx, { hash :: aec_tx_pool:tx_hash()     | atom()
            , ttl  :: aetx:tx_ttl()             | atom()
            , key  :: aec_tx_pool:pool_db_key() | atom()}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

gc(Height) ->
    gen_server:cast(?SERVER, {garbage_collect, Height}).

adjust_ttl(Diff) ->
    gen_server:cast(?SERVER, {adjust_ttl, Diff}).

open_db(Name) ->
    {ok, ets:new(Name, [ordered_set, public, {keypos, #tx.hash}, named_table])}.

init([]) ->
    {ok, _MempoolGCDb} = open_db(aec_tx_pool:pool_db_gc()),
    {ok, #st{}}.

handle_call(_Req, _From, S) ->
    {reply, {error, unknown_request}, S}.

handle_cast({garbage_collect, Height}, S) ->
    do_gc(Height, S),
    {noreply, S};
handle_cast({adjust_ttl, Diff}, #st{gc_db = GCDb} = S) ->
    do_adjust_ttl(GCDb, Diff),
    {noreply, S};
handle_cast(Msg, S) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, S}.

handle_info(Info, S) ->
    lager:debug("Ignoring unknown info: ~p", [Info]),
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


do_gc(Height, State = #st{ gc_db = GCDb }) ->
    GCTxs = ets:select(
              GCDb, [{ #tx{hash = '$1', ttl = '$2', key = '$3', _ = '_'},
                       [{'=<', '$2', Height}],
                       [{{'$1', '$3'}}] }]),
    do_gc_(GCTxs, aec_tx_pool:dbs(), State).

do_gc_([], _Dbs, S) ->
    S;
do_gc_([{TxHash, Key} | TxHashes], Dbs, S = #st{ gc_db = GCDb }) ->
    case aec_db:gc_tx(TxHash) of
        ok ->
            aec_tx_pool:raw_delete(Dbs, Key),
            ets:delete(GCDb, TxHash),
            lager:debug("Garbage collected ~p", [pp(TxHash)]);
        {error, tx_not_found} ->
            lager:info("TX garbage collect failed ~p not found",
                       [pp(TxHash)]),
            ok;
        {error, BlockHash} ->
            lager:info("TX garbage collect failed ~p is present in ~p",
                       [pp(BlockHash), pp(TxHash)]),
            ok
    end,
    do_gc_(TxHashes, Dbs, S).

-spec delete_hash(pool_db(), pool_db_gc_key()) -> true.
delete_hash(MempoolGC, TxHash) ->
    Res = ets:delete(MempoolGC, TxHash),
    Res.

add_hash(MempoolGC, TxHash, Key, TTL) ->
    %% Use update_counter with a threshold to do the compare and maybe update
    %% efficiently.
    ets:update_counter(MempoolGC, TxHash, {#tx.ttl, 0, TTL, TTL},
                       #tx{hash = TxHash, ttl = TTL, key = Key}).

do_adjust_ttl(GCDb, Diff) ->
    do_adjust_ttl(GCDb, ets:first(GCDb), Diff).

do_adjust_ttl(_GCDb, '$end_of_table', _Diff) ->
    ok;
do_adjust_ttl(GCDb, TxHash, Diff) ->
    ets:update_counter(GCDb, TxHash, {#tx.ttl, -Diff}),
    do_adjust_ttl(GCDb, ets:next(GCDb, TxHash), Diff).
