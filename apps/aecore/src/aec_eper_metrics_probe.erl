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
        {updates = 0       :: non_neg_integer(),
         stats   = stats() :: #{keys() := integer()}}).

-type keys() :: procs
              | io_in
              | io_out
              | total
              | processes
              | processes_used
              | system
              | atom
              | atom_used
              | binary
              | code
              | ets.


ad_hoc_spec() ->
    [{module, ?MODULE},
     {type, probe},
     {sample_interval, infinity}].


-spec behaviour() -> exometer:behaviour().

behaviour() -> probe.


%%% Lifecycle

probe_init(Name, Type, Options) ->
    ok = lager:debug("probe_init(~p, ~p, ~p)", [Name, Type, Options]),
    Stats = stats(),
    ok = ensure_metrics(Stats),
    {ok, #s{stats = Stats}}.


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
    NewState = State#s{updates = 0, stats = stats()},
    {ok, NewState}.


probe_sample(_) ->
    {error, not_supported}.


probe_setopts(_Entry, Opts, State = #s{stats = Stats}) ->
    Keys = proplists:get_value(datapoints, Opts, maps:keys(Stats)),
    NewStats = maps:with(Keys, Stats),
    NewState = State#s{stats = NewStats},
    {ok, NewState}.


probe_handle_msg(update, State) ->
    NewState = do_update(State),
    {ok, NewState};
probe_handle_msg(Unexpected, State) ->
    ok = lager:debug("Unknown msg: ~p", [Unexpected]),
    {ok, State}.


probe_code_change(_, S, _) ->
    {ok, S}.


do_update(State = #s{updates = Count}) ->
    NewState = State#s{updates = Count + 1, stats = stats()},
    Datapoints = maps:to_list(all_datapoints(NewState)),
    NotifyMetrics = fun({N, V}) -> aec_metrics:try_update(N, V) end,
    ok = lists:foreach(NotifyMetrics, Datapoints),
    NewState.


ensure_metrics(Stats) ->
    Ensure = fun(Name) -> ok = exometer:ensure(Name, gauge, []) end,
    lists:foreach(Ensure, maps:keys(Stats)).


%%% Stats busywork

stats() ->
    {{input, IoIn}, {output, IoOut}} = erlang:statistics(io),
    Memory = erlang:memory(),
    #{[ae,epoch,system,procs]                 => erlang:system_info(process_count),
      [ae,epoch,system,io,in]                 => IoIn,
      [ae,epoch,system,io,out]                => IoOut,
      [ae,epoch,system,memory,total]          => proplists:get_value(total, Memory),
      [ae,epoch,system,memory,processes]      => proplists:get_value(processes, Memory),
      [ae,epoch,system,memory,processes,used] => proplists:get_value(processes_used, Memory),
      [ae,epoch,system,memory,system]         => proplists:get_value(system, Memory),
      [ae,epoch,system,memory,atom]           => proplists:get_value(atom, Memory),
      [ae,epoch,system,memory,atom,used]      => proplists:get_value(atom_used, Memory),
      [ae,epoch,system,memory,binary]         => proplists:get_value(binary, Memory),
      [ae,epoch,system,memory,ets]            => proplists:get_value(ets, Memory)}.


% This simply adds the updates count to the stats map as if it were another element.

all_datapoints(#s{updates = Count, stats = Stats}) ->
    maps:put(updates, Count, Stats).
