-module(aestratum_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

-export([server_startup/1,
         client_startup/1
        ]).

-define(SERVER_NODE, dev1).
-define(CLIENT_NODE, aestratum_client1).

all() ->
    [{group, all}].

groups() ->
    [{all, [sequence],
      [{group, startup}]
     },

     {startup, [sequence],
      [server_startup,
       client_startup
      ]}

    ].

suite() ->
    [].

init_per_suite(Cfg) ->
    CtCfg = [{symlink_name, "latest.aestratum"},
              {test_module, ?MODULE}] ++ Cfg,
    ServerNodeCfg =
        #{<<"mining">> =>
            #{<<"autostart">> => false},
          <<"stratum">> =>
            #{<<"enabled">> => true,
              <<"connection">> =>
                  #{<<"port">> => 9999,
                    <<"max_connections">> => 1024,
                    <<"num_acceptors">> => 100,
                    <<"transport">> => <<"tcp">>},
              <<"session">> =>
                  #{<<"extra_nonce_bytes">> => 4,
                    <<"initial_share_target">> =>
                        115790322390251417039241401711187164934754157181743688420499462401711837020160,
                    <<"max_share_target">> =>
                        115790322390251417039241401711187164934754157181743688420499462401711837020160,
                    <<"desired_solve_time">> => 30,
                    <<"max_solve_time">> => 60,
                    <<"share_target_diff_threshold">> => 5.0,
                    <<"max_jobs">> => 20,
                    <<"max_workers">> => 10,
                    <<"msg_timeout">> => 15},
              <<"reward">> =>
                  #{<<"reward_last_rounds">> => 2,
                    <<"beneficiaries">> =>
                        [<<"ak_2hJJGh3eJA2v9yLz73To7P8LvoHdz3arku3WXvgbCfwQyaL4nK:3.3">>,
                         <<"ak_241xf1kQiexbSvWKfn5uve7ugGASjME93zDbr6SGQzYSCMTeQS:2.2">>],
                    <<"keys">> => #{<<"dir">> => <<"stratum_keys">>}}
             }},

    Client1NodeCfg = #{},
    Cfg1 = aecore_suite_utils:init_per_suite([?SERVER_NODE], ServerNodeCfg, CtCfg),
    Cfg2 = aestratum_client_suite_utils:init_per_suite([?CLIENT_NODE], Cfg1),
    pp("CFG2", Cfg2),
    [{nodes, [aecore_suite_utils:node_tuple(?SERVER_NODE)]}] ++ Cfg2.

pp(Title, Props) ->
    ct:pal("~p", [Title]),
    pps(Props).

pps([Prop | Props]) ->
    ct:pal(">>> ~p", [Prop]),
    pps(Props);
pps([]) ->
    ok.

end_per_suite(_Cfg) ->
    ok.

init_per_group(startup, Cfg) ->
    aecore_suite_utils:start_node(?SERVER_NODE, Cfg),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(?SERVER_NODE)),
    aestratum_client_suite_utils:start_node(?CLIENT_NODE, Cfg),
    Cfg;
init_per_group(_Group, Cfg) ->
    Cfg.

end_per_group(startup, Cfg) ->
    RpcFun = fun(M, F, A) -> rpc(?SERVER_NODE, M, F, A) end,
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(RpcFun),
    aecore_suite_utils:stop_node(?SERVER_NODE, Cfg),
    aecore_suite_utils:delete_node_db_if_persisted(DbCfg),
    ok;
end_per_group(_Group, _Cfg) ->
    ok.

init_per_testcase(_Case, Cfg) ->
    Cfg.

end_per_testcase(_Case, _Cfg) ->
    ok.

server_startup(_Cfg) ->
    %% Wait for all the apps to start.
    timer:sleep(5000),
    Apps = [App || {App, _Descr, _Vsn} <- rpc(?SERVER_NODE, application, which_applications, [])],
    ?assert(lists:member(aestratum, Apps)),
    ?assertMatch(X when is_pid(X), rpc(?SERVER_NODE, erlang, whereis, [aestratum])),
    ok.

client_startup(_Cfg) ->
    ok.

rpc(Node, Mod, Fun, Args) ->
    rpc:call(aecore_suite_utils:node_name(Node), Mod, Fun, Args, 5000).

