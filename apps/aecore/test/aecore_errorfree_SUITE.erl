%% Test that when starting a node no errors are written in the log file

-module(aecore_errorfree_SUITE).

%% common_test exports
-export(
   [
    all/0, 
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [ application_test/1
   , jobs_server_test/1 ]).


-include_lib("common_test/include/ct.hrl").

all() ->
    [ application_test
    , jobs_server_test ].

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    Config1 = [{symlink_name, "latest.errorfree"},
               {top_dir, TopDir},
               {test_module, ?MODULE}] ++ Config,
    aecore_suite_utils:make_shortcut(Config1),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    aecore_suite_utils:create_configs(Config1),
    aecore_suite_utils:make_multi(Config1),
    [{nodes, [aecore_suite_utils:node_tuple(dev1),
              aecore_suite_utils:node_tuple(dev2)]} | Config1].

end_per_suite(Config) ->
    aecore_suite_utils:stop_node(dev1, Config),
    aecore_suite_utils:stop_node(dev2, Config),
    ok.

init_per_testcase(_Case, Config) ->
    ct:log("testcase pid: ~p", [self()]),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

application_test(Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    N1 = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect(N1),
    aecore_suite_utils:start_node(dev2, Config),
    N2 = aecore_suite_utils:node_name(dev2),
    aecore_suite_utils:connect(N2),
    {ok, _Blocks} = aecore_suite_utils:mine_key_blocks(N1, 8),
    aecore_suite_utils:stop_node(dev1, Config),
    aecore_suite_utils:stop_node(dev2, Config),

    [] = aecore_suite_utils:errors_in_logs([dev1, dev2], Config),

    ok.

jobs_server_test(Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    N1 = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect(N1),
    Info0 = rpc:call(N1, jobs, info, [all]),
    aecore_suite_utils:restart_jobs_server(N1),
    Info1 = rpc:call(N1, jobs, info, [all]),
    true = compare_jobs_info(Info0, Info1),
    ok.

compare_jobs_info(I1, I2) ->
    Ip1 = prune_info(I1),
    Ip2 = prune_info(I2),
    {Ip1, Ip2} = {Ip2, Ip1},
    true.

%% Copied from jobs/test/jobs_server_tests.erl
%%
prune_info(Info) ->
    lists:map(fun prune_info_/1, Info).

prune_info_({queues, Qs}) ->
    {queues, [{Q, prune_q_info(Iq)} || {Q, Iq} <- Qs]};
prune_info_(I) ->
    I.

prune_q_info(I) ->
    [X || {K,_} = X <- I,
          not lists:member(K, [check_interval,
                               latest_dispatch, approved, queued,
                               oldest_job, timer, empty, depleted,
                               waiters, stateful, st])].
