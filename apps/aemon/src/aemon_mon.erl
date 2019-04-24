-module(aemon_mon).
-behaviour(gen_server).

-export([start_link/0]).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(st, {height = 0 :: non_neg_integer() }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% notify monitoring workers

notify(Height) ->
    aemon_mon_on_chain:notify(Height),
    aemon_mon_gen_stats:notify(Height),
    ok.

%% gen_server callback

init(_) ->
    aec_events:subscribe(top_changed),

    {ok, Block} = aec_chain:top_key_block(),
    Height = aec_blocks:height(Block),

    {ok, #st{height = Height}}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({gproc_ps_event, top_changed, #{info :=
                #{block_type := key, height := NewHeight}
            }}, St = #st{ height = Height}) ->
    Gens = lists:seq(Height, NewHeight-1),
    [ notify(Gen) || Gen <- Gens ],
    {noreply, St#st{ height = NewHeight }};
handle_info(_Msg, St) ->
    {noreply, St}.
