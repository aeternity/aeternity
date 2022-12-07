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
    aecore_suite_utils:start_node(?NODE, Config2),
    Node = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:connect(Node, []),
    [{node, Node} | Config2].

end_per_suite(Config) ->
    aecore_suite_utils:stop_node(?NODE, Config),
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
    ok.
