-module(aehttp_cors_middleware_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-define(NODE, dev1).
-define(NODE_NAME, aecore_suite_utils:node_name(?NODE)).

%% common_test exports
-export(
   [
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [
    defaults/1,
    set_allow_domains/1,
    set_allow_any_domain/1,
    set_allow_methods/1,
    set_max_age/1
   ]).



all() ->
    [
     defaults,
     set_allow_domains,
     set_allow_any_domain,
     set_allow_methods,
     set_max_age
    ].

init_per_suite(Config) ->
    Config1 = aecore_suite_utils:init_per_suite([?NODE], [{symlink_name, "latest.aehttp_cors_middleware"}, {test_module, ?MODULE}] ++ Config),
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]}]  ++ Config1.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    stop_node(Config),
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================
defaults(Config) ->
    start_node(Config),
    %% not set origin - no cors headers
    assert_no_headers_if_no_origin(),
    %% set origin - cors headers
    Origin = "example.com",
    Headers = headers(Origin),
    "origin" = proplists:get_value("vary", Headers),
    "true" = proplists:get_value("access-control-allow-credentials", Headers),
    "DELETE, GET, HEAD, OPTIONS, PATCH, POST, PUT" = proplists:get_value("access-control-allow-methods", Headers),
    "1800" = proplists:get_value("access-control-max-age", Headers),
    Origin = proplists:get_value("access-control-allow-origin", Headers),
    ok.

set_allow_domains(Config) ->
    Origin1 = "example1.com",
    Origin2 = "example2.com",
    configure_cors(Config, #{<<"allow_domains">> => [list_to_binary(Origin1),
                                                     list_to_binary(Origin2)]}),
    start_node(Config),
    %% not set origin - no cors headers
    assert_no_headers_if_no_origin(),
    %% set origin - cors headers
    Headers = headers(Origin1),
    "origin" = proplists:get_value("vary", Headers),
    "true" = proplists:get_value("access-control-allow-credentials", Headers),
    "DELETE, GET, HEAD, OPTIONS, PATCH, POST, PUT" = proplists:get_value("access-control-allow-methods", Headers),
    "1800" = proplists:get_value("access-control-max-age", Headers),
    Origin1 = proplists:get_value("access-control-allow-origin", Headers),
    %% not authorised origin does not have headers
    assert_no_headers_if_unauthorised_origin(),
    ok.

set_allow_any_domain(Config) ->
    Origin= "example1.com",
    configure_cors(Config, #{<<"allow_domains">> => [<<"*">>]}),
    start_node(Config),
    %% not set origin - no cors headers
    assert_no_headers_if_no_origin(),
    %% set origin - cors headers
    Headers = headers(Origin),
    "origin" = proplists:get_value("vary", Headers),
    "true" = proplists:get_value("access-control-allow-credentials", Headers),
    "DELETE, GET, HEAD, OPTIONS, PATCH, POST, PUT" = proplists:get_value("access-control-allow-methods", Headers),
    "1800" = proplists:get_value("access-control-max-age", Headers),
    Origin = proplists:get_value("access-control-allow-origin", Headers),
    ok.

set_allow_methods(Config) ->
    Origin = "example.com",
    AllowedMethods = ["GET", "POST"],
    configure_cors(Config, #{<<"allow_domains">> => [list_to_binary(Origin)],
                             <<"allow_methods">> => [list_to_binary(M) || M <- AllowedMethods]}),
    start_node(Config),
    %% not set origin - no cors headers
    assert_no_headers_if_no_origin(),
    %% set origin - cors headers
    Headers = headers(Origin),
    "origin" = proplists:get_value("vary", Headers),
    "true" = proplists:get_value("access-control-allow-credentials", Headers),
    "GET, POST" = proplists:get_value("access-control-allow-methods", Headers),
    "1800" = proplists:get_value("access-control-max-age", Headers),
    Origin = proplists:get_value("access-control-allow-origin", Headers),
    %% not authorised origin does not have headers
    assert_no_headers_if_unauthorised_origin(),
    ok.

set_max_age(Config) ->
    Origin = "example.com",
    MaxAge = 1234,
    configure_cors(Config, #{<<"allow_domains">> => [list_to_binary(Origin)],
                             <<"max_age">> => MaxAge}),
    start_node(Config),
    %% not set origin - no cors headers
    assert_no_headers_if_no_origin(),
    %% set origin - cors headers
    Headers = headers(Origin),
    "origin" = proplists:get_value("vary", Headers),
    "true" = proplists:get_value("access-control-allow-credentials", Headers),
    "DELETE, GET, HEAD, OPTIONS, PATCH, POST, PUT" = proplists:get_value("access-control-allow-methods", Headers),
    "1234" = proplists:get_value("access-control-max-age", Headers),
    Origin = proplists:get_value("access-control-allow-origin", Headers),
    %% not authorised origin does not have headers
    assert_no_headers_if_unauthorised_origin(),
    ok.

configure_cors(_Config, Cors) when map_size(Cors) =:= 0 ->
    pass;
configure_cors(Config, Cors) ->
    Cfg = #{<<"http">> => #{<<"cors">> => Cors}},
    aecore_suite_utils:create_config(?NODE, Config, Cfg, []).

start_node(Config) ->
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(?NODE_NAME, []).

stop_node(Config) ->
    aecore_suite_utils:stop_node(?NODE, Config).

headers() ->
    headers_([]).

headers(Origin) ->
    headers_([{"origin", Origin}]).

headers_(ReqH) ->
    Host = aecore_suite_utils:external_address(),
    %% get request
    {ok, {{_, 200, _}, Headers, _}} =
        aecore_suite_utils:httpc_request(get, {Host ++ "/v3/headers/top", ReqH}, [], []),
    %% preflight request
    {ok, {{_, 200, _}, Headers1, _}} =
        aecore_suite_utils:httpc_request(options, {Host ++ "/v3/headers/top", ReqH}, [], []),
    %% error
    {ok, {{_, 404, _}, Headers2, _}} =
        aecore_suite_utils:httpc_request(get, {Host ++ "/v3/names/nonExistentName.chain", ReqH}, [], []),
    InternalHost = aecore_suite_utils:internal_address(),
    %% crash
    {ok, {{_, 500, _}, Headers3, _}} =
        aecore_suite_utils:httpc_request(get, {InternalHost ++ "/v3/debug/crash", ReqH}, [], []),
    H  = trim_not_common_headers(Headers),
    H1 = trim_not_common_headers(Headers1),
    H2 = trim_not_common_headers(Headers2),
    H3 = trim_not_common_headers(Headers3),
    {H, H} = {H, H1},
    {H, H} = {H, H2},
    {H, H} = {H, H3},
    Headers.

assert_no_headers_if_no_origin() ->
    Headers0 = headers(),
    undefined = proplists:get_value("vary", Headers0),
    undefined = proplists:get_value("access-control-allow-credentials", Headers0),
    undefined = proplists:get_value("access-control-allow-methods", Headers0),
    undefined = proplists:get_value("access-control-max-age", Headers0),
    undefined = proplists:get_value("access-control-allow-origin", Headers0),
    ok.

assert_no_headers_if_unauthorised_origin() ->
    NotAuthorisedOrigin = "notauthorisedorigin.com",
    Headers1 = headers(NotAuthorisedOrigin),
    undefined = proplists:get_value("vary", Headers1),
    undefined = proplists:get_value("access-control-allow-credentials", Headers1),
    undefined = proplists:get_value("access-control-allow-methods", Headers1),
    undefined = proplists:get_value("access-control-max-age", Headers1),
    undefined = proplists:get_value("access-control-allow-origin", Headers1),
    ok.

trim_not_common_headers(Headers) ->
    lists:foldl(fun proplists:delete/2,
                Headers,
                ["date", "content-length", "content-type"]).
