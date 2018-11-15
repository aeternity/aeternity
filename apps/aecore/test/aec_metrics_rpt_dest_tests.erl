-module(aec_metrics_rpt_dest_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-import(aec_metrics_rpt_dest, [get_destinations/2]).

-define(match(A, B), {A,_} = {B,?LINE}).

read_config_test_() ->
    [{foreach,
      fun config_setup/0,
      fun cleanup/1,
      [
       {"basic prefix filtering", fun prefix_filter/0}
     , {"PT #154133896", fun system_log_only/0}
     , {"Filter on metric type", fun send_histograms/0}
     , {"Filter on datapoints", fun datapoint_filter/0}
     , {"Filter new metric", fun filter_new_metric/0}
     , {"Filter new metric on type", fun filter_new_metric_on_type/0}
     , {"Filter non-existing metric", fun filter_non_existing_metric/0}
      ]
     }].

prefix_filter() ->
    set_config([#{<<"name">> => <<"ae.epoch.aecore.**">>,
                  <<"actions">> => <<"log">>},
                #{<<"name">> => <<"ae.epoch.system.**">>,
                  <<"actions">> => <<"default">>},
                #{<<"name">> => <<"**">>,
                  <<"actions">> => <<"none">>}]),
    check_dests(fun prefix_filter_/0).

prefix_filter_() ->
    [log] =
        get_destinations(
          [ae,epoch,aecore,mining,blocks_mined], value),
    ok.

system_log_only() ->
    set_config([#{<<"name">> => <<"ae.epoch.system.**">>,
                  <<"actions">> => <<"log">>},
                #{<<"name">> => <<"ae.epoch.aecore.**">>,
                  <<"actions">> => <<"log,send">>}]),
    check_dests(fun system_log_only_/0).

system_log_only_() ->
    [log,send] =
        get_destinations(
          [ae,epoch,aecore,chain,height], value),
    [log] =
        get_destinations(
          [ae,epoch,system,procs], value),
    ok.

send_histograms() ->
    set_config([#{<<"type">>    => <<"histogram">>,
                  <<"actions">> => <<"send">>}]),
    check_dests(fun send_histograms_/0).

send_histograms_() ->
    [send] =
        get_destinations([ae,epoch,aecore,mining,interval], mean),
    [send] =
        get_destinations([ae,epoch,aecore,chain,top_change,interval], max),
    undefined =
        get_destinations([ae,epoch,system,procs], value),
    ok.

datapoint_filter() ->
    set_config([#{<<"type">>       => <<"histogram">>,
                  <<"datapoints">> => <<"97,99,max">>,
                  <<"actions">>    => <<"send">>},
                #{<<"type">>       => <<"histogram">>,
                  <<"datapoints">> => <<"default">>,
                  <<"actions">>    => <<"log">>}]),
    check_dests(fun datapoint_filter_/0).

datapoint_filter_() ->
    H = [ae,epoch,aecore,mining,interval],
    ?match([send], get_destinations(H, 97)),
    ?match([send], get_destinations(H, 99)),
    ?match([send], get_destinations(H, max)),
    ?match([log] , get_destinations(H, mean)),
    ?match([log] , get_destinations(H, 50)),
    ok.

filter_new_metric() ->
    set_config([#{<<"name">> => <<"ae.epoch.aecore.**">>,
                  <<"actions">> => <<"log">>}]),
    check_dests(fun filter_new_metric_/0).

filter_new_metric_() ->
    N = [ae,epoch,aecore,filter_new_metric],
    exometer:new(N, gauge, []),
    [log] = get_destinations(N, value),
    ok.

filter_new_metric_on_type() ->
    set_config([#{<<"type">> => <<"counter">>,
                  <<"actions">> => <<"log">>}]),
    check_dests(fun filter_new_metric_on_type_/0).

filter_new_metric_on_type_() ->
    N = [ae,epoch,aecore,filter_new_metric,on_type],
    exometer:new(N, counter, []),
    [log] = get_destinations(N, value),
    ok.

filter_non_existing_metric() ->
    set_config([#{<<"name">> => <<"ae.epoch.aecore.**">>,
                  <<"actions">> => <<"log">>}]),
    check_dests(fun filter_non_existing_metric_/0).

filter_non_existing_metric_() ->
    undefined = get_destinations([ae,epoch,aecore,non,existing],value),
    ok.

check_dests(F) ->
    {ok, Dest} = start_clean_dest(),
    try F()
    after
        stop_dest(Dest)
    end.

set_config(Rules) ->
    meck:expect(aeu_env, user_map,
                fun([<<"metrics">>, <<"rules">>]) ->
                        {ok, Rules}
                end).

config_setup() ->
    {ok, Apps} = application:ensure_all_started(exometer_core),
    create_metrics(),
    meck:new(aeu_env, [passthrough]),
    {Apps, [aeu_env]}.

cleanup({StartedApps, Mocks}) ->
    [application:stop(A) || A <- lists:reverse(StartedApps)],
    [meck:unload(M) || M <- Mocks].

create_metrics() ->
    [exometer:new(N, T, O)
     || {N, T, O} <- metrics()].

metrics() ->
    [
     %% basically, some metrics predefined in aecore/priv/...
     {[ae,epoch,aecore,mining,blocks_mined]      , counter     , []},
     {[ae,epoch,aecore,mining,retries]           , counter     , []},
     {[ae,epoch,aecore,mining,interval]          , histogram   , []},
     {[ae,epoch,aecore,chain,height]             , gauge       , []},
     {[ae,epoch,aecore,chain,top_change,interval], histogram   , []},
     %% some system metrics
     {[ae,epoch,system,procs]                    , gauge       , []},
     {[ae,epoch,system,io,in]                    , gauge       , []},
     {[ae,epoch,system,io,out]                   , gauge       , []},
     {[ae,epoch,system,memory,total]             , gauge       , []},
     {[ae,epoch,system,memory,processes,used]    , gauge       , []}
    ].


start_clean_dest() ->
    %% Start a fake supervisor, since it becomes the owner of the ets tab
    Me = self(),
    Sup = spawn_link(
            fun() ->
                    {ok, Pid} = aec_metrics_rpt_dest:start_link(),
                    ok = aec_metrics_rpt_dest:check_config(),
                    Me ! {self(), Pid},
                    receive
                        {'EXIT', Me, Reason} ->  % not trapping exits, though
                            exit(Reason)
                    end
            end),
    receive
        {Sup, Server} ->
            {ok, {Sup, Server}}
    after 1000 ->
            error(timeout)
    end.

stop_dest({Sup, Server}) ->
    SupRef = erlang:monitor(process, Sup),
    SrvRef = erlang:monitor(process, Server),
    unlink(Sup),
    exit(Sup, shutdown),
    receive_downs([SupRef, SrvRef]).

receive_downs([]) ->
    ok;
receive_downs([H|T]) ->
    receive
        {'DOWN', H, _, _, _} ->
            receive_downs(T)
    after 2000 ->
            error({shutdown_timeout, [H|T]})
    end.

-endif.
