-module(aehttp_spec_SUITE).

%%
%% simple tests to verify functionality of /api endpoint
%%

-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
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
   [ get_api/1, validate_api/1 ]).



all() ->
    [
     get_api, validate_api
    ].

init_per_suite(Config) ->
    Config1 = aecore_suite_utils:init_per_suite([?NODE], [{symlink_name, "latest.spec_endpoint"}, {test_module, ?MODULE}] ++ Config),
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]}]  ++ Config1.

end_per_suite(_Config) ->
    ok.

init_per_testcase(validate_api, Config) ->
    case aect_test_utils:latest_protocol_version() of
        ?IRIS_PROTOCOL_VSN    -> Config;
        _ -> {skip, only_test_swagger_in_latest_protocol}
    end;
init_per_testcase(_Case, Config) ->
    aecore_suite_utils:start_node(?NODE, Config),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(validate_api, _Config) ->
    ok;
end_per_testcase(_Case, Config) ->
    aecore_suite_utils:stop_node(?NODE, Config),
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================
get_api(_Config) ->
    %% ensure http interface is up and running
    N1 = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:connect(N1),
    Type = "application/json",
    Body = <<"{broken_json">>,

    Test =
        fun(SpecVsn, Path) ->
            Spec = rpc:call(N1, aehttp_spec, json, [SpecVsn]),

            Host = aecore_suite_utils:external_address(),
            URL = binary_to_list(iolist_to_binary([Host, Path])),
            Repl1 = httpc:request(URL),

            {ok, {{"HTTP/1.1", 200, "OK"}, _, Json}} = Repl1,
            ct:log("~p returned spec: ~p", [?NODE, Json]),
            JsonObj = jsx:decode(Spec),
            JsonObj = jsx:decode(list_to_binary(Json)),

            %% negative test
            {ok,{{"HTTP/1.1",405,"Method Not Allowed"},_,_}} =
                httpc:request(post, {URL, [], Type, Body}, [], [])
        end,
    Test(swagger2, "/api"),
    Test(oas3, "/api?oas3"),
    ok.

validate_api(_Config) ->
    lists:foreach(
        fun(SpecVsn) ->
            Spec = aehttp_spec:json(SpecVsn),

            Url = "https://validator.swagger.io/validator/",
            case httpc:request(post, {Url, [],  "application/json", Spec}, [], []) of
                {ok, {{_, 200, _}, _Headers, Body}} ->
                    %% For manual verification visit https://github.com/swagger-api/validator-badge
                    %% A picture is returned in the body
                    case aec_hash:blake2b_256_hash(list_to_binary(Body)) of
                        <<85,250,111,235,97,122,213,232,25,130,236,210,99,16,245,146,67,
                          186,213,194,192,223,209,154,83,237,158,255,97,67,22,185>> ->
                            %% Valid picture returned
                            ok;
                        _ ->
                            ct:pal("Wrong hash on swagger validation picture"),
                            case httpc:request(post, {Url++"debug", [],  "application/json", Spec}, [], []) of
                                {ok, {{_, 200, "OK"}, _, Msg}} ->
                                    case yamerl:decode(Msg) of
                                        [[{"messages",null},{"schemaValidationMessages",null}]] ->
                                            ct:pal("Yaml correct. Update picture hash to speed up this test"),
                                            {fail, update_hash};
                                        Yml ->
                                            {fail, Yml}
                                    end;
                                Response ->
                                    ct:log("Returned error ~s", [Response]),
                                    {fail, Response}
                            end
                    end;
                Other ->
                    ct:pal("Connection problem: ~p", [Other]),
                    {fail, "cannot connect to swagger validation server"}
            end
        end,
        [swagger2, oas3]).

