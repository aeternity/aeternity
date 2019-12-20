%% @doc This module provides a process for calculating generation metrics when new generations are
%% ready.
-module(aemon_mon_gen_stats).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , notify/3
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(st, {pubkey = <<>> :: binary()}).

%% ==================================================================
%% API

notify(Height, Type, Hash) ->
    gen_server:cast(?MODULE, {gen, Height, Type, Hash}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ==================================================================
%% gen_server callbacks

init(_) ->
    aemon_metrics:create(gen_stats),
    PubKey = aemon_config:pubkey(),
    {ok, #st{pubkey = PubKey}}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast({gen, Height, key, _Hash}, #st{pubkey = PubKey} = St) ->
    {ok, #{micro_blocks := Blocks}} = aec_chain:get_generation_by_height(Height, forward),
    {TxCount, TxMonCount} = tx_count_in_generation(Blocks, PubKey),

    aemon_metrics:gen_stats_tx(TxCount),
    aemon_metrics:gen_stats_tx_monitoring(TxMonCount),
    aemon_metrics:gen_stats_microblocks(erlang:length(Blocks)),

    {noreply, St};
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Msg, St) ->
    {noreply, St}.

%% ==================================================================
%% internal functions

tx_count_in_generation(Blocks, PubKey) ->
    lists:foldl(
      fun(MB, {AccTxC, AccTxMC}) ->
              AccTxs = [ aetx_sign:tx(AccTx) || AccTx <- aec_blocks:txs(MB) ],
              AccTxsMon = [ AccTx || AccTx <- AccTxs, aetx:origin(AccTx) == PubKey ],
              {AccTxC  + length(AccTxs), AccTxMC + length(AccTxsMon)}
      end, {0, 0}, Blocks).
