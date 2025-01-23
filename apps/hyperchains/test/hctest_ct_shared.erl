-module(hctest_ct_shared).

-export([
    child_node_config/2,
    election_contract_address_from_ctconfig/1,
    end_per_suite/2,
    init_per_suite/3,
    staking_contract_address_from_ctconfig/1, init_per_group/3, end_per_group/2
]).

-include_lib("aecontract/include/hard_forks.hrl").

init_per_suite(Config0,
            #{owner_pubkey := OwnerPubkey, 
            parent_chain_node := ParentChainNode,
            parent_chain_network_id := ParentChainNetworkId,
            parent_finality := ParentFinality,
            parent_epoch_length := ParentEpochLength,
            reward_delay := RewardDelay,
            parent_account_seeds := ParentAccountSeeds,
            main_staking_contract := MainStakingContract,
            staking_validator_contract := StakingValidatorContract, 
            hc_contract := HcContract}, #{nodes_list := NodesList}) ->
    case aect_test_utils:require_at_least_protocol(?CERES_PROTOCOL_VSN) of
        {skip, _} = Skip -> Skip;
        ok ->
            {ok, _StartedApps} = application:ensure_all_started(gproc),
            Config = [{symlink_name, "latest.hyperchains"}, {test_module, ?MODULE}] ++ Config0,
            Config1 = aecore_suite_utils:init_per_suite(NodesList,
                                                        #{}, %% config is rewritten per suite
                                                        [],
                                                        Config),
            GenesisProtocol = 1,
            {ok, AccountFileName} =
                aecore_suite_utils:hard_fork_filename(ParentChainNode, Config1, integer_to_list(GenesisProtocol), "accounts_test.json"),
            GenesisProtocolBin = integer_to_binary(GenesisProtocol),
            ParentCfg =
                #{  <<"chain">> =>
                        #{  <<"persist">> => false,
                            <<"hard_forks">> =>
                                #{  GenesisProtocolBin => #{<<"height">> => 0, <<"accounts_file">> => AccountFileName},
                                    integer_to_binary(?CERES_PROTOCOL_VSN) => #{<<"height">> => 1}
                                },
                            <<"consensus">> =>
                                #{<<"0">> => #{<<"type">> => <<"ct_tests">>}}
                         },
                    <<"fork_management">> =>
                        #{<<"network_id">> => ParentChainNetworkId},
                    <<"mempool">> => #{<<"nonce_offset">> => 200},
                    <<"mining">> =>
                        #{<<"micro_block_cycle">> => 1,
                          <<"expected_mine_rate">> => 2000,
                          <<"autostart">> => false,
                          <<"beneficiary_reward_delay">> => RewardDelay }
                },
            aecore_suite_utils:make_multi(Config1, [ParentChainNode]),
            aecore_suite_utils:create_config(ParentChainNode, Config1, ParentCfg, []),
            {_ParentPatronPriv, ParentPatronPub} = aecore_suite_utils:sign_keys(ParentChainNode),
            ParentPatronPubEnc = aeser_api_encoder:encode(account_pubkey, ParentPatronPub),
            Seeds1 = maps:put(ParentPatronPubEnc, 100000000_000000000_000000000_000000000_000000000_000000000_000000000_000000000, ParentAccountSeeds),
            aecore_suite_utils:create_seed_file(AccountFileName, Seeds1),
            StakingContract = staking_contract_address(OwnerPubkey),
            ElectionContract = election_contract_address(OwnerPubkey),
            CtSrcMap = maps:from_list([{C, create_stub(C)}
                                       || C <- [MainStakingContract, StakingValidatorContract, HcContract]]),
            [ {staking_contract, StakingContract}
            , {election_contract, ElectionContract}
            , {contract_src, CtSrcMap} 
            , {owner_pubkey, OwnerPubkey}
            , {parent_chain_node, ParentChainNode}
            , {parent_finality, ParentFinality}
            , {parent_epoch_length, ParentEpochLength}
            | Config1]
    end.

end_per_suite(Config, Nodes) ->
    [catch aecore_suite_utils:stop_node(N, Config) || N <- Nodes],
    [application:stop(A) ||
        A <- lists:reverse(
               proplists:get_value(started_apps, Config, []))],
    ok.

init_per_group(Group, ConfigPre, #{
    consensus := Consensus,
    parent_chain_node := ParentChainNode,
    parent_chain_node_name := ParentChainNodeName,
    parent_epoch_length := ParentEpochLength,
    parent_finality := ParentFinality,
    stakers := Stakers 
}) ->
    Config0 =
        case Group of
            default_pin -> [ {default_pinning_behavior, true} | ConfigPre ];
            _ -> [ {default_pinning_behavior, false} | ConfigPre ]
        end,
    VM = fate,
    NetworkId = <<"hc">>,
    GenesisStartTime = aeu_time:now_in_msecs(),
    Config = [ {network_id, NetworkId}
             , {genesis_start_time, GenesisStartTime}
             , {consensus, Consensus} 
             | aect_test_utils:init_per_group(VM, Config0) ],

    aecore_suite_utils:start_node(ParentChainNode, Config),
    aecore_suite_utils:connect(ParentChainNodeName, []),
    ParentTopHeight = aecore_suite_utils:rpc(ParentChainNode, aec_chain, top_height, []),
    StartHeight = max(ParentTopHeight, ParentEpochLength),
    ct:log("Parent chain top height ~p start at ~p", [ParentTopHeight, StartHeight]),
    %%TODO mine less than necessary parent height and test chain starts when height reached
    {ok, _} = hctest:mine_key_blocks(
        ParentChainNodeName,
            (StartHeight - ParentTopHeight) + ParentFinality),
    [ {staker_names, Stakers}
    , {parent_start_height, StartHeight} 
    | Config].

end_per_group(_Group, Config) ->
    Config1 = with_saved_keys([nodes], Config),
    [ aecore_suite_utils:stop_node(Node, Config1)
      || {Node, _, _, _} <- proplists:get_value(nodes, Config1, []) ],

    aecore_suite_utils:assert_no_errors_in_logs(Config1, ["{handled_abort,parent_chain_not_synced}"]),

    Config1.

staking_contract_address_from_ctconfig(CtConfig) ->
    proplists:get_value(staking_contract, CtConfig).

staking_contract_address(OwnerPubkey) ->
    %% This value is additionally stored in the CT Config in the shared init_per_suite
    aect_contracts:compute_contract_pubkey(OwnerPubkey, 1).

election_contract_address_from_ctconfig(CtConfig) ->
    proplists:get_value(election_contract, CtConfig).
    
election_contract_address(OwnerPubkey) ->
    %% This value is additionally stored in the CT Config in the shared init_per_suite
    aect_contracts:compute_contract_pubkey(OwnerPubkey, 2).
    
create_stub(Contract) ->
    create_stub(Contract, []).

create_stub(Contract, Opts0) ->
    File = aect_test_utils:contract_filename(Contract),
    Opts = Opts0 ++ [{no_code, true}] ++ aect_test_utils:copts({file, File}),
    {ok, SrcBin} = aect_test_utils:read_contract(Contract),
    {ok, Enc}  = aeso_aci:contract_interface(json, binary_to_list(SrcBin), Opts),
    {ok, Stub} = aeso_aci:render_aci_json(Enc),
    binary_to_list(Stub).

child_node_config(_ChildNodeConfig = #{
    node := Node,
    stakeholders := Stakeholders, 
    pinners := Pinners,
    receive_address := ReceiveAddressPub,
    hc_contract := HcContract
}, CTConfig) ->
    ReceiveAddress = hctest:encoded_pubkey(ReceiveAddressPub),
    NodeConfig = node_config(Node, CTConfig, Stakeholders, Pinners, ReceiveAddress),
    build_json_files(HcContract, NodeConfig, CTConfig),
    aecore_suite_utils:create_config(Node, CTConfig, NodeConfig, [{add_peers, true}]).

node_config(Node, CTConfig, PotentialStakers, PotentialPinners, ReceiveAddress) ->
    NetworkId = proplists:get_value(network_id, CTConfig),
    GenesisStartTime = proplists:get_value(genesis_start_time, CTConfig),
    Stakers = lists:map(
                    fun(HCWho) ->
                        HCPriv = list_to_binary(aeu_hex:bin_to_hex( privkey(HCWho))), %% TODO: discuss key management
                        #{ <<"hyper_chain_account">> => #{<<"pub">> => encoded_pubkey(HCWho), <<"priv">> => HCPriv} }
                    end,
                    PotentialStakers),
    Pinners = lists:map(
                    fun({Owner, Pinner}) ->
                        HCPriv = list_to_binary(aeu_hex:bin_to_hex( privkey(Pinner))), %% TODO: discuss key management
                        #{ <<"parent_chain_account">> => #{<<"pub">> => encoded_pubkey(Pinner), <<"priv">> => HCPriv, <<"owner">> => encoded_pubkey(Owner)} }
                    end,
                    PotentialPinners),
    ct:log("Stakers: ~p", [Stakers]),
    ct:log("Pinners: ~p", [Pinners]),
    ConsensusType = <<"hyperchain">>,
    ParentChainNode = proplists:get_value(parent_chain_node, CTConfig),
    ParentFinality = proplists:get_value(parent_finality, CTConfig),
    ParentEpochLength = proplists:get_value(parent_epoch_length, CTConfig),
    Port = aecore_suite_utils:external_api_port(ParentChainNode),
    SpecificConfig =
                #{  <<"parent_chain">> =>
                    #{  <<"start_height">> => proplists:get_value(parent_start_height, CTConfig),
                        <<"finality">> => ParentFinality,
                        <<"parent_epoch_length">> => ParentEpochLength,
                        <<"consensus">> =>
                            #{  <<"type">> => <<"AE2AE">>,
                                <<"network_id">> => ?PARENT_CHAIN_NETWORK_ID,
                                <<"spend_address">> => ReceiveAddress,
                                <<"fee">> => 100000000000000,
                                <<"amount">> => 9700
                            },
                        <<"polling">> =>
                            #{  <<"fetch_interval">> => 100,
                                <<"cache_size">> => 10,
                                <<"nodes">> => [ iolist_to_binary(io_lib:format("http://test:Pass@127.0.0.1:~p", [Port])) ]
                            }
                        },
                    <<"genesis_start_time">> => GenesisStartTime,
                    <<"child_epoch_length">> => ?CHILD_EPOCH_LENGTH,
                    <<"child_block_time">> => ?CHILD_BLOCK_TIME,
                    <<"child_block_production_time">> => ?CHILD_BLOCK_PRODUCTION_TIME
                    },
    Protocol = aect_test_utils:latest_protocol_version(),
    {ok, ContractFileName} = aecore_suite_utils:hard_fork_filename(Node, CTConfig, integer_to_list(Protocol), binary_to_list(NetworkId) ++ "_contracts.json"),
    {ok, AccountFileName} = aecore_suite_utils:hard_fork_filename(Node, CTConfig, integer_to_list(Protocol), binary_to_list(NetworkId) ++ "_accounts.json"),
    #{<<"chain">> =>
            #{  <<"persist">> => false,
                <<"hard_forks">> => #{integer_to_binary(Protocol) => #{<<"height">> => 0,
                                                                        <<"contracts_file">> => ContractFileName,
                                                                        <<"accounts_file">> => AccountFileName}},
                <<"consensus">> =>
                    #{<<"0">> => #{
                        <<"type">> => ConsensusType,
                        <<"config">> => maps:merge(#{
                            <<"election_contract">> => aeser_api_encoder:encode(contract_pubkey, election_contract_address()),
                            <<"rewards_contract">> => aeser_api_encoder:encode(contract_pubkey, staking_contract_address()),
                            <<"staking_contract">> => aeser_api_encoder:encode(contract_pubkey, staking_contract_address()),
                            <<"contract_owner">> => aeser_api_encoder:encode(account_pubkey,?OWNER_PUBKEY),
                            <<"expected_key_block_rate">> => 2000,
                            <<"stakers">> => Stakers,
                            <<"pinners">> => Pinners,
                            <<"pinning_reward_value">> => 4711,
                            <<"fixed_coinbase">> => ?BLOCK_REWARD,
                            <<"default_pinning_behavior">> => proplists:get_value(default_pinning_behavior, CTConfig)
                        }, SpecificConfig)
                    }}
            },
        <<"fork_management">> =>
            #{<<"network_id">> => <<"this_will_be_overwritten_runtime">>},
        <<"logging">> => #{<<"level">> => <<"debug">>},
        <<"sync">> => #{<<"ping_interval">> => 5000},
        <<"http">> => #{<<"endpoints">> => #{<<"hyperchain">> => true}},
        <<"mining">> =>
            #{<<"micro_block_cycle">> => 1,
            <<"autostart">> => false,
            %%<<"autostart">> => ProducingCommitments,
            <<"beneficiary_reward_delay">> => ?REWARD_DELAY
        }}.  %% this relies on certain nonce numbers

build_json_files(ElectionContract, NodeConfig, CTConfig) ->
    Pubkey = proplists:get_value(owner_pubkey, CTConfig),
    {_PatronPriv, PatronPub} = aecore_suite_utils:sign_keys(?NODE1),
    ct:log("Patron is ~p", [aeser_api_encoder:encode(account_pubkey, PatronPub)]),

    %% create staking contract
    MinStakeAmt = integer_to_list(trunc(math:pow(10,18) * 1)), %% 1 AE
    MSSrc = src(?MAIN_STAKING_CONTRACT, CTConfig),
    #{ <<"pubkey">> := StakingContractPubkey
        , <<"owner_pubkey">> := ContractOwner } = SC
        = contract_create_spec(?MAIN_STAKING_CONTRACT, MSSrc, [MinStakeAmt], 0, 1, Pubkey),
    {ok, StakingAddress} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                            StakingContractPubkey),
    %% assert assumption
    StakingAddress = hctest_ct_shared:staking_contract_address_from_ctconfig(CTConfig),

    %% create election contract
    #{ <<"pubkey">> := ElectionContractPubkey
        , <<"owner_pubkey">> := ContractOwner } = EC
        = contract_create_spec(ElectionContract, src(ElectionContract, CTConfig),
                                [binary_to_list(StakingContractPubkey)], 0, 2, Pubkey),
    {ok, ElectionAddress} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                            ElectionContractPubkey),
    %% assert assumption
    ElectionAddress = hctest_ct_shared:election_contract_address_from_ctconfig(CTConfig),
    {ok, SCId} = aeser_api_encoder:safe_decode(contract_pubkey, StakingContractPubkey),

    APub = binary_to_list(aeser_api_encoder:encode(account_pubkey, pubkey(?ALICE))),
    Call1 =
        contract_call_spec(SCId, MSSrc, "new_validator", [APub, APub, "true"],
                            ?INITIAL_STAKE, pubkey(?ALICE), 1),

    BPub = binary_to_list(aeser_api_encoder:encode(account_pubkey, pubkey(?BOB))),
    BPubSign = binary_to_list(aeser_api_encoder:encode(account_pubkey, pubkey(?BOB_SIGN))),
    Call2 =
        contract_call_spec(SCId, MSSrc, "new_validator", [BPub, BPubSign, "true"],
                            ?INITIAL_STAKE, pubkey(?BOB), 1),

    LPub = binary_to_list(aeser_api_encoder:encode(account_pubkey, pubkey(?LISA))),
    Call3 =
        contract_call_spec(SCId, MSSrc, "new_validator", [LPub, LPub, "true"],
                            ?INITIAL_STAKE, pubkey(?LISA), 1),

    AllCalls =  [Call1, Call2, Call3],
    ProtocolBin = integer_to_binary(aect_test_utils:latest_protocol_version()),
    #{<<"chain">> := #{<<"hard_forks">> := #{ProtocolBin := #{<<"contracts_file">> := ContractsFileName,
                                                                <<"accounts_file">> := AccountsFileName}}}} = NodeConfig,
    aecore_suite_utils:create_seed_file(ContractsFileName,
        #{<<"contracts">> => [SC, EC], <<"calls">> => AllCalls}),
    aecore_suite_utils:create_seed_file(AccountsFileName,
        #{  <<"ak_2evAxTKozswMyw9kXkvjJt3MbomCR1nLrf91BduXKdJLrvaaZt">> => 1000000000000000000000000000000000000000000000000,
            encoded_pubkey(?ALICE) => 2100000000000000000000000000,
            encoded_pubkey(?BOB) => 3100000000000000000000000000,
            encoded_pubkey(?BOB_SIGN) => 3100000000000000000000000000,
            encoded_pubkey(?LISA) => 4100000000000000000000000000
            }),
    ok.

contract_create_spec(Name, Src, Args, Amount, Nonce, Owner) ->
    {ok, Code}   = aect_test_utils:compile_contract(aect_test_utils:sophia_version(), Name),
    Pubkey = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    EncodedPubkey   = aeser_api_encoder:encode(contract_pubkey, Pubkey),
    EncodedOwner    = aeser_api_encoder:encode(account_pubkey, Owner),
    EncodedCode     = aeser_api_encoder:encode(contract_bytearray, Code),
    {ok, CallData} = aect_test_utils:encode_call_data(Src, "init", Args),
    EncodedCallData = aeser_api_encoder:encode(contract_bytearray, CallData),
    VM = aect_test_utils:vm_version(),
    ABI = aect_test_utils:abi_version(),
    Spec = #{ <<"amount">> => Amount
            , <<"vm_version">> => VM
            , <<"abi_version">> => ABI
            , <<"nonce">> => Nonce
            , <<"code">> => EncodedCode
            , <<"call_data">> => EncodedCallData
            , <<"pubkey">> => EncodedPubkey
            , <<"owner_pubkey">> => EncodedOwner },
    Spec.
