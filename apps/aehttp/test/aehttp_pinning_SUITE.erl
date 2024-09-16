-module(aehttp_pinning_SUITE).

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
   [ get_api/1 ]).



all() ->
    [
     get_api
    ].

init_per_suite(Config) ->
    {ok, _State} = aec_pinning_agent:start_link(dev1, aeternity),
    Config1 = aecore_suite_utils:init_per_suite([?NODE], [{symlink_name, "latest.spec_endpoint"}, {test_module, ?MODULE}] ++ Config),
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]}]  ++ Config1.

end_per_suite(_Config) ->
    ok.

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

    Test =
        fun(_SpecVsn, Path) ->
            % Spec = rpc:call(N1, aehttp_spec, json, [SpecVsn]),

            Host = aecore_suite_utils:external_address(),
            URL = binary_to_list(iolist_to_binary([Host, Path])),
            ct:log(URL),
            
            Repl1 = httpc:request(URL),

            {ok, {{"HTTP/1.1", 200, "OK"}, _, _Json}} = Repl1
    
        end,
    % Test(oas3, "/api"), %% for backward compatibility
    % Test(oas3, "/api?oas3"), %% for backward compatibility
    Test(oas3, "/v3/hyperchain/pin-tx"),
    ok.
