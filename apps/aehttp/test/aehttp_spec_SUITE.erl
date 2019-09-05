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
    Config1 = aecore_suite_utils:init_per_suite([?NODE], [{symlink_name, "latest.spec_endpoint"}, {test_module, ?MODULE}] ++ Config),
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
                              "apps/aehttp/priv/swagger.yaml"]),

    Host = aecore_suite_utils:external_address(),
    URL = binary_to_list(iolist_to_binary([Host, "/api"])),
    Repl1 = httpc:request(URL),

    {ok, {{"HTTP/1.1", 200, "OK"}, _, Json}} = Repl1,
    ct:log("~p returned spec: ~p", [?NODE, Json]),
    Yamls = yamerl_constr:file(SpecFile, [str_node_as_binary]),
    Yaml = lists:last(Yamls),
    Spec = jsx:prettify(jsx:encode(Yaml)),
    ct:log("read spec file ~s", [SpecFile]),

    JsonObj = jsx:decode(Spec),
    JsonObj = jsx:decode(list_to_binary(Json)),
    Type = "application/json",
    Body = <<"{broken_json">>,

    {ok,{{"HTTP/1.1",405,"Method Not Allowed"},_,_}} =
         httpc:request(post, {URL, [], Type, Body}, [], []),
    ok.
