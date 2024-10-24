-module(hctest_shared_test_steps).

-export([init_per_suite/2, end_per_suite/2, init_per_group/1, end_per_group/1]).

-include_lib("common_test/include/ct.hrl").

-include("../../aecontract/include/hard_forks.hrl").
-include("../include/hc_test.hrl").

init_per_suite(Config0, CCNodes) ->
    case aect_test_utils:require_at_least_protocol(?CERES_PROTOCOL_VSN) of
        {skip, _} = Skip ->
            Skip;
        ok ->
            {ok, _StartedApps} = application:ensure_all_started(gproc),
            Config = [{symlink_name, "latest.hyperchains"}, {test_module, ?MODULE}] ++ Config0,
            Config1 = aecore_suite_utils:init_per_suite(
                CCNodes,
                %% config is rewritten per suite
                #{},
                [],
                Config
            ),
            GenesisProtocol = 1,
            {ok, AccountFileName} =
                aecore_suite_utils:hard_fork_filename(
                    ?PARENT_CHAIN_NODE,
                    Config1,
                    integer_to_list(GenesisProtocol),
                    "accounts_test.json"
                ),
            GenesisProtocolBin = integer_to_binary(GenesisProtocol),
            ParentCfg =
                #{
                    <<"chain">> =>
                        #{
                            <<"persist">> => false,
                            <<"hard_forks">> =>
                                #{
                                    GenesisProtocolBin => #{
                                        <<"height">> => 0, <<"accounts_file">> => AccountFileName
                                    },
                                    integer_to_binary(?CERES_PROTOCOL_VSN) => #{<<"height">> => 1}
                                },
                            <<"consensus">> =>
                                #{<<"0">> => #{<<"type">> => <<"ct_tests">>}}
                        },
                    <<"fork_management">> =>
                        #{<<"network_id">> => ?PARENT_CHAIN_NETWORK_ID},
                    <<"mempool">> => #{<<"nonce_offset">> => 200},
                    <<"mining">> =>
                        #{
                            <<"micro_block_cycle">> => 1,
                            <<"expected_mine_rate">> => 2000,
                            <<"autostart">> => false,
                            <<"beneficiary_reward_delay">> => ?REWARD_DELAY
                        }
                },
            aecore_suite_utils:make_multi(Config1, [?PARENT_CHAIN_NODE]),
            aecore_suite_utils:create_config(?PARENT_CHAIN_NODE, Config1, ParentCfg, []),
            {_ParentPatronPriv, ParentPatronPub} = aecore_suite_utils:sign_keys(?PARENT_CHAIN_NODE),
            ParentPatronPubEnc = aeser_api_encoder:encode(account_pubkey, ParentPatronPub),
            aecore_suite_utils:create_seed_file(
                AccountFileName,
                #{
                    ParentPatronPubEnc =>
                        100000000000000000000000000000000000000000000000000000000000000000000000,
                    hctest_utils:encoded_pubkey(?DWIGHT) => 2100000000000000000000000000,
                    hctest_utils:encoded_pubkey(?EDWIN) => 3100000000000000000000000000
                }
            ),
            StakingContract = hctest_utils:staking_contract_address(),
            ElectionContract = hctest_utils:election_contract_address(),
            {ok, SVBinSrc} = aect_test_utils:read_contract("StakingValidator"),
            {ok, MSBinSrc} = aect_test_utils:read_contract(?MAIN_STAKING_CONTRACT),
            {ok, EBinSrc} = aect_test_utils:read_contract(?HC_CONTRACT),
            [
                {staking_contract, StakingContract},
                {election_contract, ElectionContract},
                {contract_src, #{
                    "StakingValidator" => hctest_utils:create_stub(binary_to_list(SVBinSrc)),
                    ?MAIN_STAKING_CONTRACT => hctest_utils:create_stub(binary_to_list(MSBinSrc)),
                    ?HC_CONTRACT => hctest_utils:create_stub(binary_to_list(EBinSrc))
                }}
                | Config1
            ]
    end.

end_per_suite(Config, CCNodes) ->
    [catch aecore_suite_utils:stop_node(N, Config) || N <- CCNodes],
    catch aecore_suite_utils:stop_node(?PARENT_CHAIN_NODE, Config),
    [
        application:stop(A)
     || A <- lists:reverse(
            proplists:get_value(started_apps, Config, [])
        )
    ],
    ok.

init_per_group(Config0) ->
    VM = fate,
    NetworkId = <<"hc">>,
    GenesisStartTime = aeu_time:now_in_msecs(),
    Config = [
        {network_id, NetworkId},
        {genesis_start_time, GenesisStartTime},
        {consensus, ?CONSENSUS}
        | aect_test_utils:init_per_group(VM, Config0)
    ],

    aecore_suite_utils:start_node(?PARENT_CHAIN_NODE, Config),
    aecore_suite_utils:connect(?PARENT_CHAIN_NODE_NAME, []),
    ParentTopHeight = aecore_suite_utils:rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    StartHeight = max(ParentTopHeight, ?PARENT_EPOCH_LENGTH),
    ct:log("Parent chain top height ~p start at ~p", [ParentTopHeight, StartHeight]),
    %%TODO mine less than necessary parent height and test chain starts when height reached
    {ok, _} = hctest_utils:mine_key_blocks(
        ?PARENT_CHAIN_NODE_NAME,
        (StartHeight - ParentTopHeight) + ?PARENT_FINALITY
    ),
    [{staker_names, [?ALICE, ?BOB, ?LISA]}, {parent_start_height, StartHeight} | Config].

%% Stops all nodes that were started for the test group
end_per_group(Config) ->
    Config1 = hctest_utils:with_saved_keys([nodes], Config),
    [
        aecore_suite_utils:stop_node(Node, Config1)
     || {Node, _, _} <- proplists:get_value(nodes, Config1, [])
    ],
    Config1.
