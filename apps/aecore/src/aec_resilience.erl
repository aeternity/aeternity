-module(aec_resilience).
-behaviour(gen_server).

-export([ start_link/0 ]).

-export([ fork_resistance_active/0
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(st, {}).


fork_resistance_active() ->
    {Res, _} = get_fork_resistance(),
    Res.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    aec_events:subscribe(chain_sync),
    aec_events:subscribe(update_config),
    {ok, #st{}}.


handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({gproc_ps_event, chain_sync, #{info := {chain_sync_done, _}}}, St) ->
    maybe_enable_fork_resistance(),
    {noreply, St};
handle_info({gproc_ps_event, update_config,
             #{<<"sync">> := #{<<"sync_allowed_height_from_top">> := _}}}, St) ->
    maybe_enable_fork_resistance(),
    {noreply, St};
handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.


maybe_enable_fork_resistance() ->
    case get_fork_resistance() of
        {_, manual} -> ok;
        _ ->
            Res = case aeu_env:find_config([<<"sync">>, <<"sync_allowed_height_from_top">>],
                                           [ user_config, schema_default ]) of
                      {ok, Height} when Height > 0 ->
                          lager:debug("Activating fork resistance for Height = ~p", [Height]),
                          {yes, Height};
                      _ ->
                          lager:debug("No fork resistance", []),
                          no
                  end,
            set_fork_resistance({Res, auto})
    end.

get_fork_resistance() ->
    persistent_term:get({?MODULE, fork_resistance}, {no, auto}).

set_fork_resistance({_, _} = Value) ->
    persistent_term:put({?MODULE, fork_resistance}, Value).
