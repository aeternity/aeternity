-module(aehttp_restart_SUITE).

-import(aecore_suite_utils, [http_request/4, internal_address/0, external_address/0,
                             rpc/3, rpc/4]).

-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

-export([
         test_restart/1
        ]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-define(NODE, dev1).

all() -> [
          {group, all}
         ].

groups() ->
    [{all, [sequence],
     [
      test_restart
     ]}
    ].

suite() -> [].

init_per_suite(Config) ->
    DefCfg = #{ <<"chain">> => #{<<"persist">> => true},
                <<"fork_management">> =>
                    #{<<"network_id">> => <<"ae_uat">>},
                <<"include_default_peers">> => true
                    },
    {ok, StartedApps} = application:ensure_all_started(gproc),
    Config1 = aecore_suite_utils:init_per_suite([?NODE], DefCfg,
                                                [{instant_mining, false},
                                                 {build_to_connect_to_mainnet, true},
                                                 {use_config_defaults, false},
                                                 {symlink_name, "latest.http_restart"},
                                                 {test_module, ?MODULE}] ++ Config),
    Config2 = [ {nodes, [aecore_suite_utils:node_tuple(?NODE)]}
              , {started_apps, StartedApps} ]  ++ Config1,
    Config2.

end_per_suite(Config) ->
    [application:stop(A) ||
        A <- lists:reverse(
               proplists:get_value(started_apps, Config, []))],
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

test_restart(Config) ->
    lists:foreach(
        fun(Idx) ->
            ct:log("Test try ~p", [Idx]),
            assert_no_running_node_process(),
            start_node_with_db_intensive_sync(Config),
            assert_one_running_node_process(),
            aecore_suite_utils:stop_node(?NODE, Config),
            assert_no_running_node_process()
        end,
        lists:seq(1, 10)),
    ok.

start_node_with_db_intensive_sync(Config) ->
    aecore_suite_utils:start_node(?NODE, Config),
    ok = wait_till_node_starts(),
    %%aecore_suite_utils:start_node(?NODE, Config),
    {ok, 200, #{<<"top_block_height">> := Height0}} =
        aehttp_integration_SUITE:get_status_sut(),
    ct:log("Initial height ~p", [Height0]),
    timer:sleep(5000),
    {ok, 200, #{<<"top_block_height">> := Height1}} =
        aehttp_integration_SUITE:get_status_sut(),
    ct:log("Synced height ~p", [Height1]),
    true = Height0 < Height1,
    ok.

wait_till_node_starts() ->
    wait_till_node_starts(40).

wait_till_node_starts(Attempts) when Attempts < 1 ->
    {error, node_did_not_start};
wait_till_node_starts(Attempts) ->
    case aehttp_integration_SUITE:get_status_sut() of
        {error, _} ->
            timer:sleep(250),
            wait_till_node_starts(Attempts - 1);
        {ok, 200, _} ->
            ok
    end.

assert_no_running_node_process() ->
    RunningNodes = get_number_of_processes(),
    0 = RunningNodes,
    ok.

assert_one_running_node_process() ->
    RunningNodes = get_number_of_processes(),
    1 = RunningNodes,
    ok.

get_number_of_processes() ->
    BaseCommand = "ps -fea | grep aeternity | grep daemon",
    Cmd = BaseCommand ++ " | wc -l",
    ResTmp = os:cmd(BaseCommand),
    ct:log("Currently running processes:\n~s", [ResTmp]),
    Res = os:cmd(Cmd),
    {RunningNodes, _} = string:to_integer(string:trim(Res)),
    ct:log("Currently there are ~p running nodes", [RunningNodes]),
    RunningNodes.
