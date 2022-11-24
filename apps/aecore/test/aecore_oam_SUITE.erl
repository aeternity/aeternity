-module(aecore_oam_SUITE).

%% Test cases for verifying maintenance mode, etc.

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
   , mode_is_maintenance/1
   , mode_is_offline/1
   , set_mode_normal/1
   , set_mode_maintenance/1
   , set_mode_offline/1
   ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, all}
    ].

groups() ->
    [
     {all, [sequence],
      [{group, normal_init},
       {group, maintenance_init}]},
     {normal_init, [sequence],
      [ mode_is_normal
      , set_mode_maintenance
      , set_mode_normal
      , set_mode_maintenance
      , set_mode_normal
      , set_mode_offline
      , set_mode_normal
      ]},
     {maintenance_init, [sequence],
      [ mode_is_maintenance
      , set_mode_normal
      , set_mode_maintenance
      , set_mode_normal
      , set_mode_maintenance
      , set_mode_offline
      , set_mode_maintenance
      ]},
      {offline_init, [sequence],
      [ mode_is_offline
      , set_mode_normal
      , set_mode_offline
      , set_mode_normal
      , set_mode_offline
      , set_mode_maintenance
      , set_mode_offline
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    aecore_suite_utils:init_per_suite(
      [dev1],
      [{symlink_name, "latest.oam"},
       {test_module, ?MODULE}] ++ Config).

init_per_group(normal_init, Config) ->
    init_(false, false, Config);
init_per_group(maintenance_init, Config) ->
    init_(true, false, Config);
init_per_group(offline_init, Config) ->
    init_(false, true, Config);
init_per_group(_, Config) ->
    Config.

init_(MMode, false, Config) when is_boolean(MMode) ->
    aecore_suite_utils:start_node(
      dev1, Config, [{"AE__SYSTEM__MAINTENANCE_MODE", atom_to_list(MMode)}]),
    N = aecore_suite_utils:node_name(dev1),
    App = case MMode of
              true  -> aecore;
              false -> aehttp
          end,
    aecore_suite_utils:connect_wait(N, App),
    [{nodes, [aecore_suite_utils:node_tuple(dev1)]} | Config];
init_(false, OMode, Config) when is_boolean(OMode) ->
    aecore_suite_utils:start_node(
      dev1, Config, [{"AE__SYSTEM__OFFLINE_MODE", atom_to_list(OMode)}]),
    N = aecore_suite_utils:node_name(dev1),
    App = case OMode of
              true  -> aehttp;
              false -> aesync
          end,
    aecore_suite_utils:connect_wait(N, App),
    [{nodes, [aecore_suite_utils:node_tuple(dev1)]} | Config].

end_per_group(_Group, Config) ->
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
    normal = get_mode(),
    ok.

mode_is_maintenance(_Config) ->
    maintenance = get_mode(),
    ok.

mode_is_offline(_Config) ->
    offline = get_mode(),
    ok.

set_mode_normal(_Config) ->
    rpc_call(app_ctrl, set_mode, [normal]),
    await_stable_mode(),
    true = is_running(aesync),
    true = is_running(aehttp),
    true = is_running(aestratum),
    ok.

set_mode_maintenance(_Config) ->
    rpc_call(app_ctrl, set_mode, [maintenance]),
    await_stable_mode(),
    false = is_running(aesync),
    false = is_running(aehttp),
    false = is_running(aestratum),
    ok.

set_mode_offline(_Config) ->
    rpc_call(app_ctrl, set_mode, [offline]),
    await_stable_mode(),
    false = is_running(aesync),
    true = is_running(aehttp),
    false = is_running(aestratum),
    ok.

get_mode() ->
    rpc_call(app_ctrl, get_mode, []).

await_stable_mode() ->
    rpc_call(app_ctrl, await_stable_mode, [5000]).

is_running(App) ->
    Running = rpc_call(application, which_applications, []),
    lists:keymember(App, 1, Running).

rpc_call(M, F, As) ->
    N = aecore_suite_utils:node_name(dev1),
    rpc:call(N, M, F, As).
