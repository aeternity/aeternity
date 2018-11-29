-module(aehttp_spec_SUITE).

%%
%% simple tests to verify functionality of /api endpoint
%%

-include_lib("common_test/include/ct.hrl").
-define(NODE, dev1).

%% common_test exports
-export(
   [
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [ get_api/1 ]).



all() ->
    [
     get_api
    ].

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    Config1 = [{symlink_name, "latest.spec_endpoint"},
               {top_dir, TopDir},
               {test_module, ?MODULE}] ++ Config,
    aecore_suite_utils:make_shortcut(Config1),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),

    aecore_suite_utils:create_config(?NODE, Config1, #{}, []),
    aecore_suite_utils:make_multi(Config1, [?NODE]),
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]}]  ++ Config1.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    aecore_suite_utils:start_node(?NODE, Config),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    aecore_suite_utils:stop_node(?NODE, Config),
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================
get_api(Config) ->
    %% ensure http interface is up and running
    aecore_suite_utils:connect(aecore_suite_utils:node_name(?NODE)),

    SpecFile = filename:join([proplists:get_value(top_dir, Config),
                              "apps/aehttp/priv/swagger.json"]),

    Host = external_address(),
    URL = binary_to_list(iolist_to_binary([Host, "/api"])),
    Repl1 = httpc:request(URL),

    {ok, {{"HTTP/1.1", 200, "OK"}, _, Json}} = Repl1,
    ct:log("~p returned spec: ~p", [?NODE, Json]),
    {ok, Spec} = file:read_file(SpecFile),
    ct:log("read spec file ~s", [SpecFile]),

    JsonObj = jsx:decode(Spec),
    JsonObj = jsx:decode(list_to_binary(Json)),
    Type = "application/json",
    Body = <<"{broken_json">>,

    {ok,{{"HTTP/1.1",405,"Method Not Allowed"},_,_}} = 
         httpc:request(post, {URL, [], Type, Body}, [], []),
    ok.


external_address() ->
    Port = 
        rpc:call(aecore_suite_utils:node_name(?NODE), 
                 aeu_env, user_config_or_env,
                 [ [<<"http">>, <<"external">>, <<"port">>],
                   aehttp, [external, port], 8043]),
    "http://127.0.0.1:" ++ integer_to_list(Port).     % good enough for requests
