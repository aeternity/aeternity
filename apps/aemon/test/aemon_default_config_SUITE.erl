-module(aemon_default_config_SUITE).

%% common_test exports
-export(
   [
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export([ dont_start_by_default/1 ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() -> [ dont_start_by_default ].

init_per_suite(Config) ->
    Config1 = aecore_suite_utils:init_per_suite([dev1], #{}, [{symlink_name, "latest.pof"}, {test_module, ?MODULE}] ++ Config),
    [{nodes, [aecore_suite_utils:node_tuple(dev1)]} | Config1].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    ct:log("testcase pid: ~p", [self()]),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    aecore_suite_utils:stop_node(dev1, Config),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

dont_start_by_default(Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    Node = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect(Node),

    %% Make sure application start but no workers are present
    ok = wait_for_start(Node, 10),
    Children = rpc:call(Node, supervisor, which_children, [aemon_sup]),
    [] = Children,
    ok.

wait_for_start(_, 0) -> timeout;
wait_for_start(Node, N) ->
    case lists:keysearch(aemon, 1, rpc:call(Node, application, which_applications, [])) of
        {value, _} -> ok;
        _ ->
            timer:sleep(1000),
            wait_for_start(Node, N-1)
    end.
