-module(aec_eper_metrics_probe).
-behavior(exometer_probe).

-export([behaviour/0,
         probe_init/3,
         probe_terminate/1,
         probe_setopts/3,
         probe_update/2,
         probe_get_value/2,
         probe_get_datapoints/1,
         probe_reset/1,
         probe_sample/1,
         probe_handle_msg/2,
         probe_code_change/3]).

-export([ad_hoc_spec/0]).


-record(s,
        {updates  = 0            :: non_neg_integer(),
         stats    = init_stats() :: #{[atom()] := integer()}}).


ad_hoc_spec() ->
    [{module, ?MODULE},
     {type, probe},
     {sample_interval, infinity}].


-spec behaviour() -> exometer:behaviour().

behaviour() -> probe.


%%% Lifecycle

probe_init(Name, Type, Options) ->
    ok = lager:info("probe_init(~p, ~p, ~p)", [Name, Type, Options]),
    _ = erlang:send_after(2000, self(), update),
    Stats = init_stats(),
    ok = ensure_metrics(Stats),
    ok = set_triggers(),
    {ok, #s{stats = Stats}}.

set_triggers() ->
    Triggers =
        [{long_gc, 500},
         {long_schedule, 500},
         {large_heap, 1024 * 1024},
         busy_port,
         busy_dist_port],
    _ = erlang:system_monitor(self(), Triggers),
    ok.


probe_terminate(Reason) ->
    ok = lager:debug("~p terminating: ~p", [?MODULE, Reason]),
    ok.


%%% External interface

probe_get_value(default, State = #s{updates = Count}) ->
    {ok, [{updates, Count}], State};
probe_get_value(Keys, State) ->
    Values = maps:to_list(maps:with(Keys, all_datapoints(State))),
    {ok, Values, State}.


probe_get_datapoints(State) ->
    Datapoints = maps:to_list(all_datapoints(State)),
    {ok, Datapoints}.


probe_update(_, _) ->
    {error, not_supported}.


probe_reset(State) ->
    NewState = State#s{updates = 0, stats = init_stats()},
    {ok, NewState}.


probe_sample(_) ->
    {error, not_supported}.


probe_setopts(_Entry, Opts, State = #s{stats = Stats}) ->
    Keys = proplists:get_value(datapoints, Opts, maps:keys(Stats)),
    NewStats = maps:with(Keys, Stats),
    NewState = State#s{stats = NewStats},
    {ok, NewState}.


probe_handle_msg(update, State) ->
    _ = erlang:send_after(2000, self(), update),
    NewState = do_update(State),
    {ok, NewState};
probe_handle_msg({monitor, PID, Tag, Info}, State) ->
    NewState = handle_monitor(PID, Tag, Info, State),
    {ok, NewState};
probe_handle_msg(Unexpected, State) ->
    ok = lager:debug("Unknown msg: ~p", [Unexpected]),
    {ok, State}.


probe_code_change(_, State, _) ->
    {ok, State}.


do_update(State = #s{updates = Count, stats = Stats}) ->
    NewStats = maps:merge(Stats, stats()),
    NewState = State#s{updates = Count + 1, stats = NewStats},
    Datapoints = maps:to_list(all_datapoints(NewState)),
    NotifyMetrics = fun({N, V}) -> aec_metrics:try_update(N, V) end,
    ok = lists:foreach(NotifyMetrics, Datapoints),
    NewState.


handle_monitor(PID, Trigger, Info, State = #s{stats = Stats}) ->
    Stat = [ae,epoch,system,monitor,Trigger],
    NewStats = maps:update_with(Stat, fun increment/1, Stats),
    ok = lager:warning("PID ~p triggered monitor ~p with ~p", [PID, Trigger, Info]),
    State#s{stats = NewStats}.

increment(Count) -> Count + 1.


ensure_metrics(Stats) ->
    Ensure = fun(Name) -> ok = exometer:ensure(Name, gauge, []) end,
    lists:foreach(Ensure, maps:keys(Stats)).


%%% Stats busywork

stats() ->
    {{input, IoIn}, {output, IoOut}} = erlang:statistics(io),
    Memory = erlang:memory(),
    #{[ae,epoch,system,procs]                  => erlang:system_info(process_count),
      [ae,epoch,system,io,in]                  => IoIn,
      [ae,epoch,system,io,out]                 => IoOut,
      [ae,epoch,system,memory,total]           => proplists:get_value(total, Memory),
      [ae,epoch,system,memory,processes]       => proplists:get_value(processes, Memory),
      [ae,epoch,system,memory,processes,used]  => proplists:get_value(processes_used, Memory),
      [ae,epoch,system,memory,system]          => proplists:get_value(system, Memory),
      [ae,epoch,system,memory,atom]            => proplists:get_value(atom, Memory),
      [ae,epoch,system,memory,atom,used]       => proplists:get_value(atom_used, Memory),
      [ae,epoch,system,memory,binary]          => proplists:get_value(binary, Memory),
      [ae,epoch,system,memory,ets]             => proplists:get_value(ets, Memory)}.

init_stats() ->
    Monitors =
        #{[ae,epoch,system,monitor,long_gc]        => 0,
          [ae,epoch,system,monitor,long_schedule]  => 0,
          [ae,epoch,system,monitor,large_heap]     => 0,
          [ae,epoch,system,monitor,busy_port]      => 0,
          [ae,epoch,system,monitor,busy_dist_port] => 0},
    Stats = stats(),
    maps:merge(Stats, Monitors).


% Adds the updates count to the stats map as if it were another element.

all_datapoints(#s{updates = Count, stats = Stats}) ->
    maps:put(updates, Count, Stats).
