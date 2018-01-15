-module(aec_tx_pool_metrics_probe).
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

-define(DATAPOINTS, [pending_txs]).

-record(st, {
          datapoints = ?DATAPOINTS,
          data = [],
          ref
         }).

ad_hoc_spec() ->
    [{module, ?MODULE},
     {type, probe},
     {sample_interval, 5000}].

-spec behaviour() -> exometer:behaviour().
behaviour() ->
    probe.

probe_init(_, _, Opts) ->
    lager:debug("tx_pool_metrics probe initialized", []),
    DP = proplists:get_value(datapoints, Opts, ?DATAPOINTS),
    {ok, #st{datapoints = DP}}.

probe_terminate(Reason) ->
    lager:debug("tx_pool_metrics probe terminating: ~p", [Reason]),
    ok.

probe_get_value(DPs, #st{data = Data0,
                         datapoints = DPs0} = S) ->
    Data1 = if Data0 =:= undefined ->
                    sample(DPs0);
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

probe_sample(#st{datapoints = DPs} = S) ->
    {_Pid, Ref} = spawn_monitor(
                    fun() ->
                            exit({sample, sample(DPs)})
                    end),
    {ok, S#st{ref = Ref}}.

probe_setopts(_Entry, Opts, S) ->
    DPs = proplists:get_value(datapoints, Opts, S#st.datapoints),
    {ok, S#st{datapoints = DPs}}.

probe_handle_msg({'DOWN', Ref, _, _, SampleRes}, #st{ref = Ref} = S) ->
    case SampleRes of
        {sample, Data} ->
            lager:debug("got sample: ~p", [Data]),
            {ok, S#st{ref = undefined, data = Data}};
        Other ->
            lager:debug("sampler died: ~p", [Other]),
            {ok, S#st{ref = undefined}}
    end;
probe_handle_msg(_Msg, S) ->
    lager:debug("Unknown msg: ~p", [_Msg]),
    {ok, S}.

probe_code_change(_, S, _) ->
    {ok, S}.

sample(DPs) ->
    lists:foldr(fun sample_/2, [], DPs).

sample_(pending_txs = K, Acc) ->
    [{K, pending_txs()}|Acc];
sample_(_, Acc) ->
    Acc.

pending_txs() ->
    case aec_tx_pool:size() of
        S when is_integer(S), S >= 0 -> S;
        undefined -> 0
    end.
