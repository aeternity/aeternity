-module(aehttp_rosetta_SUITE).

%%
%% Each test assumes that the chain is at least at the height where the latest
%% consensus protocol applies hence each test reinitializing the chain should
%% take care of that at the end of the test.
%%

-include_lib("stdlib/include/assert.hrl").

-import(aecore_suite_utils, [http_request/4, httpc_request/4, process_http_return/1]).
-import(aecore_suite_utils,
        [internal_address/0, external_address/0, rosetta_address/0, rpc/3, rpc/4]).

%% common_test exports
-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1, init_per_group/2,
         end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
-export([network_status/1, network_options/1, network_list/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(NODE, dev1).
-define(DEFAULT_TESTS_COUNT, 5).
-define(BOGUS_STATE_HASH, <<42:32/unit:8>>).
-define(SPEND_FEE, 20000 * aec_test_utils:min_gas_price()).
-define(MAX_MINED_BLOCKS, 20).

all() ->
    [{group, all}].

groups() ->
    [{all, [sequence], [{group, rosetta}]},
     {rosetta,
      [sequence],
      %% /network/*
      [{group, network_endpoint}]},
     %% /network/*
     {network_endpoint, [], [network_list, network_options, network_status]}].

suite() ->
    [].

init_per_suite(Config) ->
    DefCfg =
        #{<<"chain">> => #{<<"persist">> => false},
          <<"mining">> =>
              #{<<"micro_block_cycle">> => 1,
                <<"name_claim_bid_timeout">> => 0}}, %% NO name auctions
    {ok, StartedApps} = application:ensure_all_started(gproc),
    Config1 =
        aecore_suite_utils:init_per_suite([?NODE],
                                          DefCfg,
                                          [{instant_mining, true},
                                           {symlink_name, "latest.http_endpoints"},
                                           {test_module, ?MODULE}]
                                          ++ Config),
    Config2 =
        [{nodes, [aecore_suite_utils:node_tuple(?NODE)]}, {started_apps, StartedApps}] ++ Config1,
    aecore_suite_utils:start_node(?NODE, Config2),
    Node = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:connect(Node, []),
    [{node, Node} | Config2].

end_per_suite(Config) ->
    aecore_suite_utils:stop_node(?NODE, Config),
    [application:stop(A)
     || A
            <- lists:reverse(
                   proplists:get_value(started_apps, Config, []))],
    ok.

init_per_group(all, Config) ->
    Config;
init_per_group(Group, Config) when Group =:= status_endpoints ->
    Config;
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    init_per_testcase_all(Config).

init_per_testcase_all(Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, 100),
    aecore_suite_utils:use_rosetta(),
    [{tc_start, os:timestamp()} | Config].

end_per_testcase(_Case, Config) ->
    end_per_testcase_all(Config).

end_per_testcase_all(Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:unmock_mempool_nonce_offset(Node),
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p",
           [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
             || {_, N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

%% /network/list
network_list(_Config) ->
    {ok,
     200,
     #{<<"network_identifiers">> := [#{<<"blockchain">> := Chain,
                                      <<"network">> := Network }]}} =
        get_list_sut(),
    ?assertMatch(<<"aeternity">>, Chain),
    ExpectedNwId = aec_governance:get_network_id(),
    ?assertMatch(ExpectedNwId, Network),
    ok.

get_list_sut() ->
    Host = rosetta_address(),
    Body =
        #{metadata => #{}},
    http_request(Host, post, "network/list", Body).


%% /network/options

%% %% Official semver regex https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string
-define(SEMVER_RE, "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$").

network_options(_Config) ->
    {ok,
     200,
     #{<<"version">> := #{<<"rosetta_version">> := RosettaVsn,
                            <<"node_version">> := NodeVsn }}} =
        get_options_sut(),
    ?assertMatch(<<"1.4.10">>, RosettaVsn),
    {ok, Mp} = re:compile(?SEMVER_RE),
    case re:run(NodeVsn, Mp) of
        {match, _} -> ok;
        _ -> ct:fail("Node version is not semver")
    end,
    ok.

get_options_sut() ->
    Host = rosetta_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          metadata => #{}},
    http_request(Host, post, "network/options", Body).

%% /network/status
network_status(_Config) ->
    {ok,
     200,
     #{<<"current_block_identifier">> := #{<<"hash">> := TopKeyBlockHash},
       <<"current_block_timestamp">> := CurrentBlockTimestamp,
       <<"genesis_block_identifier">> := #{<<"hash">> := GenesisKeyBlockHash},
       <<"sync_status">> := #{<<"synced">> := Synced},
       <<"peers">> := PeersFormatted}} =
        get_status_sut(),
    ?assertMatch({ok, _}, aeser_api_encoder:safe_decode(key_block_hash, GenesisKeyBlockHash)),
    ?assertMatch(X when is_boolean(X), Synced),
    ?assertMatch(true, is_integer(CurrentBlockTimestamp)),
    ?assertMatch([], PeersFormatted),
    ?assertMatch({ok, _}, aeser_api_encoder:safe_decode(key_block_hash, TopKeyBlockHash)),
    ok.

get_status_sut() ->
    Host = rosetta_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          metadata => #{}},
    http_request(Host, post, "network/status", Body).
