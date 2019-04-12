%%%=============================================================================
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%     DB garbage collection of transactions that were already removed
%%%     from the system by mempool GC.
%%%
%%%     This server is responsible for cleaning up aec_tx_gc objects from
%%%     the database. aec_tx_gc objects keep hashes of Garbage Collected
%%%     mempool transactions so that they don't circulate in the system forever.
%%%
%%%     By default, mempool transactions are Garbage Collected after 12 hours -
%%%     then aec_tx_gc objects are created in the DB for them. And aec_tx_gc
%%%     objects are kept in the DB for over 48 hours.
%%% @end
%%%=============================================================================

-module(aec_tx_gc).

%% API
-export([ schedule_removal/2
        , gc/1]).

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

-ifdef(TEST).
-export([ sync_gc/1
        , stop/0]).
-endif.

-define(SERVER, ?MODULE).

-define(TX_GC, tx_gc).
-ifndef(TEST).
-define(TTL, 1024). %% Over 48 hours.
-else.
-define(TTL, 4).
-endif.

-record(tx_gc, {hash :: aec_tx_pool:tx_hash() | atom(),
                ttl  :: aetx:tx_ttl() | atom()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec schedule_removal(aec_tx_pool:tx_hash() | atom(), aec_blocks:height()) -> ok.
schedule_removal(TxHash, Height) ->
    add_tx_gc(TxHash, Height + ?TTL),
    ok.

-spec gc(aec_blocks:height()) -> ok.
gc(Height) ->
    gen_server:cast(?SERVER, {tx_gc_cleanup, Height}).

-ifdef(TEST).
sync_gc(Height) ->
    gen_server:call(?SERVER, {tx_gc_cleanup, Height}).

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
    tx_db_open(),
    TopHeight = top_height(),
    InitFun = fun(TxHash) ->
                      add_tx_gc(TxHash, TopHeight + ?TTL)
              end,
    ok = aec_db:ensure_transaction(
           fun() ->
                   aec_db:foreach_tx_gc(InitFun)
           end),
    {ok, []}.

handle_call({tx_gc_cleanup, Height}, _From, S) ->
    ok = tx_gc(Height),
    {reply, ok, S};
handle_call(_Req, _From, S) ->
    {reply, {error, unknown_request}, S}.

handle_cast({tx_gc_cleanup, Height}, S) ->
    ok = tx_gc(Height),
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


%%%===============================================================
%%% Internal functions
%%%===================================================================

tx_db_open() ->
    ets:new(?TX_GC, [ordered_set, public, named_table, {keypos, #tx_gc.hash}]).

add_tx_gc(TxHash, TTL) ->
    ets:update_counter(?TX_GC, TxHash, {#tx_gc.ttl, 0, TTL, TTL},
                       #tx_gc{hash = TxHash, ttl = TTL}).

tx_gc(Height) ->
    GCTxs = ets:select(
              ?TX_GC, [{#tx_gc{hash = '$1', ttl = '$2'},
                        [{'=<', '$2', Height}],
                        [{{'$1'}}]}]),
    tx_gc_(GCTxs).

tx_gc_([]) ->
    ok;
tx_gc_([{TxHash} | Rest]) ->
    aec_db:remove_tx_gc(TxHash),
    ets:delete(?TX_GC, TxHash),
    tx_gc_(Rest).

top_height() ->
    case aec_chain:top_header() of
        undefined -> 0;
        Header    -> aec_headers:height(Header)
    end.
