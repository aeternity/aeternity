-module(aemon_mon_gen_stats).

-behaviour(gen_server).

-export([notify/3]).
-export([start_link/0]).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

notify(Height, Type, Hash) ->
    gen_server:cast(?MODULE, {gen, Height, Type, Hash}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callback

init(_) ->
    aemon_metrics:create(gen_stats),
    PubKey = aemon_config:pubkey(),
    {ok, PubKey}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast({gen, Height, _Type, _Hash}, PubKey) ->
    {ok, #{micro_blocks := Blocks}} = aec_chain:get_generation_by_height(Height, forward),
    aemon_metrics:gen_stats_microblocks(erlang:length(Blocks)),

    {TxCount, TxMonCount} = lists:foldl(fun(MB, {AccTxC, AccTxMC}) ->
        AccTxs = [ aetx_sign:tx(AccTx) || AccTx <- aec_blocks:txs(MB) ],
        AccTxsMon = [ AccTx || AccTx <- AccTxs, aetx:origin(AccTx) == PubKey ],
        {AccTxC  + length(AccTxs),
         AccTxMC + length(AccTxsMon)}
        end, {0,0}, Blocks),

    aemon_metrics:gen_stats_tx(TxCount),
    aemon_metrics:gen_stats_tx_monitoring(TxMonCount),

    {noreply, PubKey};
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Msg, St) ->
    {noreply, St}.
