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

-include_lib("exometer_core/include/exometer.hrl").

-define(DATAPOINTS, [updates]).

-record(st, {
          name,
          datapoints = ?DATAPOINTS,
          data = [],
          ttl = 5000
         }).

ad_hoc_spec() ->
    [{module, ?MODULE},
     {type, probe},
     {cache, 5000},
     {sample_interval, infinity}].

-spec behaviour() -> exometer:behaviour().
behaviour() ->
    probe.

probe_init(Name, _, Opts) ->
    lager:debug("probe_init(~p, _, ~p)", [Name, Opts]),
    ensure_metrics(),
    TTL = proplists:get_value(cache, Opts, (#st{})#st.ttl),
    exometer_cache:write([ae,epoch,aecore,eper], updates, 0, TTL),
    watchdog:add_proc_subscriber(self()),
    DP = proplists:get_value(datapoints, Opts, ?DATAPOINTS),
    {ok, #st{datapoints = DP, name = Name, ttl = TTL}}.

probe_terminate(Reason) ->
    lager:debug("eper_metrics probe terminating: ~p", [Reason]),
    ok.

probe_get_value(DPs, #st{data = Data0,
                         datapoints = DPs0} = S) ->
    Data1 = if Data0 =:= undefined ->
                    [];
               true -> Data0
            end,
    DPs1 = if DPs =:= default -> DPs0;
              true -> DPs
           end,
    {ok, probe_get_value_(Data1, DPs1), S#st{data = Data1}}.

probe_get_value_(Data, DPs) ->
    [D || {K,_} = D <- Data,
          lists:member(K, DPs)].

probe_get_datapoints(#st{datapoints = DPs}) ->
    {ok, DPs}.

probe_update(_, _) ->
    {error, not_supported}.

probe_reset(S) ->
    {ok, S#st{data = []}}.

probe_sample(_) ->
    {error, not_supported}.

probe_setopts(_Entry, Opts, S) ->
    DPs = proplists:get_value(datapoints, Opts, S#st.datapoints),
    {ok, S#st{datapoints = DPs}}.

probe_handle_msg({watchdog, _Node, _Ts, _Trigger, TriggerData}, S) ->
    update_metrics(TriggerData),
    {ok, update_counter(updates, 1, S)};
probe_handle_msg(_Msg, S) ->
    lager:debug("Unknown msg: ~p", [_Msg]),
    {ok, S}.

probe_code_change(_, S, _) ->
    {ok, S}.

update_counter(K, Value, #st{data = Data, name = Name, ttl = TTL} = S) ->
    Data1 =
        case lists:keyfind(K, 1, Data) of
            {_, Prev} ->
                New = Prev + Value,
                exometer_cache:write(Name, K, New, TTL),
                lists:keyreplace(K, 1, Data, {K, New});
            false ->
                [{K, Value}|Data]
        end,
    S#st{data = Data1}.

ensure_metrics() ->
    lists:foreach(
      fun({Name, Type, Opts, _}) ->
              ok = exometer:ensure(Name, Type, Opts)
      end, metrics()).

-define(PFX, [ae,epoch,system]).
metrics() ->
    [{?PFX++[procs]                , gauge, [], [prfSys, procs]},
     {?PFX++[io,in]                , gauge, [], [prfSys, io_in]},
     {?PFX++[io,out]               , gauge, [], [prfSys, io_out]},
     {?PFX++[memory,total]         , gauge, [], [prfSys, total]},
     {?PFX++[memory,processes]     , gauge, [], [prfSys, processes]},
     {?PFX++[memory,processes,used], gauge, [], [prfSys, processes_used]},
     {?PFX++[memory,system]        , gauge, [], [prfSys, system]},
     {?PFX++[memory,atom]          , gauge, [], [prfSys, atom]},
     {?PFX++[memory,atom,used]     , gauge, [], [prfSys, atom_used]},
     {?PFX++[memory,binary]        , gauge, [], [prfSys, binary]},
     {?PFX++[memory,ets]           , gauge, [], [prfSys, ets]}].

update_metrics(Data) ->
    Metrics = metrics(),
    lists:foreach(
      fun({K, Vs}) ->
              case [{N, T} || {N, _, _, [K1|T]} <- Metrics, K1 =:= K] of
                  [] -> ok;
                  [_|_] = Ms ->
                      update_metrics_(Vs, Ms)
              end
      end, Data).

update_metrics_(V, Ms) when is_number(V) ->
    [aec_metrics:try_update(N, V) || {N, []} <- Ms];
update_metrics_([_|_] = Vs, Ms) ->
    lists:foreach(
      fun({K, Vs1}) ->
              case [{N, T} || {N, [K1|T]} <- Ms, K1 =:= K] of
                  [] -> ok;
                  [_|_] = Ms1 ->
                       update_metrics_(Vs1, Ms1)
              end
      end, Vs).
