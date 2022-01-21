-module(aecore_gc_status_SUITE).

%% Test cases for verifying GC enabled nodes remain so.
%% 
%% Start node with gc false
%% stop node
%% Start with GC enabled
%% Assert node is running with GC enabled
%% stop node
%% Start with GC enabled
%% Assert node refuses to start
%% Start with GC disabled
%% Assert runnning with GC enabled

%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [
     mode_is_normal/1
   , set_mode_gc/1
   , set_mode_normal/1
   ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, all}
    ].

groups() ->
    [
     {all, [sequence],
      [{group, switch_to_gc}]},
     {switch_to_gc, [sequence],
      [ mode_is_normal
      , set_mode_gc
      , set_mode_gc
      , set_mode_normal
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => true,
                     <<"garbage_collection">> => #{<<"enabled">> => false}}},
    aecore_suite_utils:init_per_suite(
      [dev1],
      DefCfg,
      [{symlink_name, "latest.gc_status"},
       {test_module, ?MODULE}] ++ Config).

init_per_group(switch_to_gc, Config) ->
    init_(false, Config);
init_per_group(_, Config) ->
    Config.

init_(GCMode, Config) when is_boolean(GCMode) ->
    aecore_suite_utils:start_node(
      dev1, Config, [{"AE__CHAIN__GARBAGE_COLLECTION__ENABLED", atom_to_list(GCMode)}]),
    N = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect_wait(N, aecore),
    ct:log("Node started in init_: ~p",[N]),
    [{nodes, [aecore_suite_utils:node_tuple(dev1)]} | Config].

end_per_group(_Group, Config) ->
    ct:log("Node stopped in end_per_group"),
    aecore_suite_utils:stop_node(dev1, Config),
    ok.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

-define(APP_WAIT, 2000).

mode_is_normal(_Config) ->
    false = get_gc_enabled(),
    ok.

set_mode_normal(Config) ->
    N = aecore_suite_utils:node_name(dev1),
    ct:log("Stop Node ~p to set gc disabled", [N]),
    aecore_suite_utils:stop_node(dev1, Config),
    %% The node should refuse to start again
    ct:log("Monitor Node ~p in set_mode_normal", [N]),
    ct:log("Start Node ~p with GC enabled false", [N]),
    aecore_suite_utils:start_node(
        dev1, Config, [{"AE__CHAIN__GARBAGE_COLLECTION__ENABLED", atom_to_list(false)}]),
    monitor_node(N, true),
    receive
        {nodedown, N} -> ct:log("Node refused to start")
    after 30000 ->
        %% Successful runs in CI take about 10 secs for the restart
        ct:fail("GC node started when configured for non GC")
    end,
    ok.

set_mode_gc(Config) ->
    N = aecore_suite_utils:node_name(dev1),
    ct:log("Stop Node ~p to set gc enabled", [N]),
    aecore_suite_utils:stop_node(dev1, Config),
    ct:log("Start Node ~p with GC enabled true", [N]),
    aecore_suite_utils:start_node(
      dev1, Config, [{"AE__CHAIN__GARBAGE_COLLECTION__ENABLED", atom_to_list(true)}]),
    N = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect_wait(N, aecore),

    true = get_gc_enabled(),

    ct:log("Node ~p running with GC enabled", [N]),

    ok.

get_gc_enabled() ->
    rpc_call(aec_chain_state, is_gc_enabled, []).

rpc_call(M, F, As) ->
    N = aecore_suite_utils:node_name(dev1),
    rpc:call(N, M, F, As).
