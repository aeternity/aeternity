%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Exometer probe for MPT read-cache ETS size and memory.
%%% @end
%%%=============================================================================
-module(aec_mpt_cache_metrics_probe).
-behavior(exometer_probe).

-export([ behaviour/0
        , probe_init/3
        , probe_terminate/1
        , probe_setopts/3
        , probe_update/2
        , probe_get_value/2
        , probe_get_datapoints/1
        , probe_reset/1
        , probe_sample/1
        , probe_handle_msg/2
        , probe_code_change/3
        ]).

-export([ad_hoc_spec/0]).

-include_lib("exometer_core/include/exometer.hrl").

-define(DATAPOINTS, [size, memory]).

-record(st, { datapoints = ?DATAPOINTS
            , data = []
            }).

ad_hoc_spec() ->
    [{module, ?MODULE},
     {type, probe},
     {sample_interval, 10000}].

-spec behaviour() -> exometer:behaviour().
behaviour() ->
    probe.

probe_init(_, _, Opts) ->
    lager:debug("mpt_cache_metrics probe initialized", []),
    DPs = proplists:get_value(datapoints, Opts, ?DATAPOINTS),
    {ok, #st{datapoints = DPs}}.

probe_terminate(Reason) ->
    lager:debug("mpt_cache_metrics probe terminating: ~p", [Reason]),
    ok.

probe_get_value(DPs, #st{data = Data0, datapoints = DPs0} = S) ->
    Data1 = case Data0 of
                [] -> sample(DPs0);
                _  -> Data0
            end,
    DPs1 = case DPs of
               default -> DPs0;
               _       -> DPs
           end,
    {ok, [D || {K, _} = D <- Data1, lists:member(K, DPs1)], S#st{data = Data1}}.

probe_get_datapoints(#st{datapoints = DPs}) ->
    {ok, DPs}.

probe_update(_, _) ->
    {error, not_supported}.

probe_reset(S) ->
    {ok, S#st{data = []}}.

probe_sample(#st{datapoints = DPs} = S) ->
    {ok, S#st{data = sample(DPs)}}.

probe_setopts(_Entry, Opts, S) ->
    DPs = proplists:get_value(datapoints, Opts, S#st.datapoints),
    {ok, S#st{datapoints = DPs}}.

probe_handle_msg(_Msg, S) ->
    lager:debug("Unknown msg: ~p", [_Msg]),
    {ok, S}.

probe_code_change(_, S, _) ->
    {ok, S}.

sample(DPs) ->
    lists:foldr(fun sample_/2, [], DPs).

sample_(size = K, Acc) ->
    [{K, aec_mpt_cache:size()} | Acc];
sample_(memory = K, Acc) ->
    [{K, aec_mpt_cache:memory()} | Acc];
sample_(_, Acc) ->
    Acc.
