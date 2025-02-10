-module(hc_btc_doge_SUITE).

-import(aecore_suite_utils, [ http_request/4
                            , external_address/0
                            , rpc/3
                            , rpc/4
                            ]).

-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).


%% Test cases
-export([start_two_child_nodes/1,
         produce_first_epoch/1,
         produce_some_epochs/1,
         check_default_pin/1,
         try_a_pin/1
        ]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aecontract/test/include/aect_sophia_vsn.hrl").
-include("./hctest_defaults.hrl").

-define(PARENT_START_HEIGHT, 133).

all() -> [{group, doge}].

groups() ->
    [
        {doge, [sequence],
            [ start_two_child_nodes
            , produce_first_epoch
            , try_a_pin
            %, produce_some_epochs
            %, check_default_pin
        ]}
    ].

suite() -> [].

init_per_suite(Config0) ->
    case aect_test_utils:require_at_least_protocol(?CERES_PROTOCOL_VSN) of
        _ -> {skip, only_run_manually}; % comment out line if you actually want to run tests here,.
        {skip, _} = Skip -> Skip;
        ok ->
            {ok, _StartedApps} = application:ensure_all_started(gproc),
            Config = [{symlink_name, "latest.hyperchains"}, {test_module, ?MODULE}] ++ Config0,
            Config1 = aecore_suite_utils:init_per_suite([?NODE1, ?NODE2],
                                                        #{}, %% config is rewritten per suite
                                                        [],
                                                        Config),
            % GenesisProtocol = 1,
            % {ok, AccountFileName} =
            %     aecore_suite_utils:hard_fork_filename(?PARENT_CHAIN_NODE, Config1, integer_to_list(GenesisProtocol), "accounts_test.json"),
            % GenesisProtocolBin = integer_to_binary(GenesisProtocol),
            % ParentCfg =
            %     #{  <<"chain">> =>
            %             #{  <<"persist">> => false,
            %                 <<"hard_forks">> =>
            %                     #{  GenesisProtocolBin => #{<<"height">> => 0, <<"accounts_file">> => AccountFileName},
            %                         integer_to_binary(?CERES_PROTOCOL_VSN) => #{<<"height">> => 1}
            %                     },
            %                 <<"consensus">> =>
            %                     #{<<"0">> => #{<<"type">> => <<"ct_tests">>}}
            %              },
            %         <<"fork_management">> =>
            %             #{<<"network_id">> => ?PARENT_CHAIN_NETWORK_ID},
            %         <<"mempool">> => #{<<"nonce_offset">> => 200},
            %         <<"mining">> =>
            %             #{<<"micro_block_cycle">> => 1,
            %               <<"expected_mine_rate">> => 2000,
            %               <<"autostart">> => false,
            %               <<"beneficiary_reward_delay">> => ?REWARD_DELAY }
            %     },
            % aecore_suite_utils:make_multi(Config1, [?PARENT_CHAIN_NODE]),
            % aecore_suite_utils:create_config(?PARENT_CHAIN_NODE, Config1, ParentCfg, []),
            % {_ParentPatronPriv, ParentPatronPub} = aecore_suite_utils:sign_keys(?PARENT_CHAIN_NODE),
            % ParentPatronPubEnc = aeser_api_encoder:encode(account_pubkey, ParentPatronPub),
            % aecore_suite_utils:create_seed_file(AccountFileName,
            %     #{  ParentPatronPubEnc => 100000000000000000000000000000000000000000000000000000000000000000000000
            %         , encoded_pubkey(?DWIGHT) => 2100000000000000000000000000
            %         , encoded_pubkey(?EDWIN) => 3100000000000000000000000000
            %     }),
            StakingContract = staking_contract_address(),
            ElectionContract = election_contract_address(),
            CtSrcMap = maps:from_list([{C, hctest_ct_shared:create_stub(C)}
                                       || C <- [?MAIN_STAKING_CONTRACT, ?STAKING_VALIDATOR_CONTRACT, ?HC_CONTRACT]]),
            [{staking_contract, StakingContract}, {election_contract, ElectionContract}, {contract_src, CtSrcMap} | Config1]
    end.

end_per_suite(Config) ->
    catch aecore_suite_utils:stop_node(?NODE1, Config),
    catch aecore_suite_utils:stop_node(?NODE2, Config),
    catch aecore_suite_utils:stop_node(?NODE3, Config),
    catch aecore_suite_utils:stop_node(?PARENT_CHAIN_NODE, Config),
    [application:stop(A) ||
        A <- lists:reverse(
               proplists:get_value(started_apps, Config, []))],
    ok.

init_per_group(Group, ConfigPre) ->
    Config0 = [ {default_pinning_behavior, false} | ConfigPre ],
        % case Group of
        %     default_pin -> [ {default_pinning_behavior, true} | ConfigPre ];
        %     _ -> [ {default_pinning_behavior, false} | ConfigPre ]
        % end,
    VM = fate,
    NetworkId = <<"hc">>,
    GenesisStartTime = aeu_time:now_in_msecs(),
    Config = [{network_id, NetworkId}, {genesis_start_time, GenesisStartTime},
              {consensus, ?CONSENSUS} |
              aect_test_utils:init_per_group(VM, Config0)],

    % aecore_suite_utils:start_node(?PARENT_CHAIN_NODE, Config),
    % aecore_suite_utils:connect(?PARENT_CHAIN_NODE_NAME, []),
    % ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    % StartHeight = max(ParentTopHeight, ?PARENT_EPOCH_LENGTH),
    % ct:log("Parent chain top height ~p start at ~p", [ParentTopHeight, StartHeight]),
    % %%TODO mine less than necessary parent height and test chain starts when height reached
    % {ok, _} = mine_key_blocks(
    %         ?PARENT_CHAIN_NODE_NAME,
    %         (StartHeight - ParentTopHeight) + ?PARENT_FINALITY),
    [ {staker_names, [?ALICE, ?BOB, ?LISA]}, {parent_start_height, ?PARENT_START_HEIGHT} | Config].

child_node_config(Node, Stakeholders, Pinners, CTConfig) ->
    ReceiveAddress = hctest:encoded_pubkey(?FORD),
    NodeConfig = node_config(Node, CTConfig, Stakeholders, Pinners, ReceiveAddress),
    build_json_files(?HC_CONTRACT, NodeConfig, CTConfig),
    aecore_suite_utils:create_config(Node, CTConfig, NodeConfig, [{add_peers, true}]).

end_per_group(_Group, Config) ->
    Config1 = with_saved_keys([nodes], Config),
    [ aecore_suite_utils:stop_node(Node, Config1)
      || {Node, _, _, _} <- proplists:get_value(nodes, Config1, []) ],

    aecore_suite_utils:assert_no_errors_in_logs(Config1, ["{handled_abort,parent_chain_not_synced}"]),

    Config1.

%% Here we decide which nodes are started/running
init_per_testcase(start_two_child_nodes, Config) ->
    Config1 =
        [{nodes, [{?NODE1, ?NODE1_NAME, [?ALICE, ?LISA], [{?ALICE, ?DWIGHT}, {?LISA, ?EDWIN}]},
                  {?NODE2, ?NODE2_NAME, [?BOB_SIGN], [{?BOB_SIGN, ?EDWIN}]}
                 ]}
         | Config],
    aect_test_utils:setup_testcase(Config1),
    Config1;
init_per_testcase(sync_third_node, Config) ->
    Config1 = with_saved_keys([nodes], Config),
    Nodes = ?config(nodes, Config1),
    Config2 = lists:keyreplace(nodes, 1, Config1,
                               {nodes, Nodes ++ [{?NODE3, ?NODE3_NAME, [], []}]}),
    aect_test_utils:setup_testcase(Config2),
    Config2;
init_per_testcase(_Case, Config) ->
    Config1 = with_saved_keys([nodes], Config),
    aect_test_utils:setup_testcase(Config1),
    Config1.

end_per_testcase(_Case, Config) ->
    {save_config, Config}.

with_saved_keys(Keys, Config) ->
    {_TC, SavedConfig} = ?config(saved_config, Config),
    lists:foldl(fun(Key, Conf) ->
                    case proplists:get_value(Key, SavedConfig) of
                        undefined -> Conf;
                        Val -> [{Key, Val} | Conf]
                    end
                end,
                lists:keydelete(saved_config, 1, Config), Keys).

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

contract_call_spec(ContractPubkey, Src, Fun, Args, Amount, From, Nonce) ->
    {contract_call_tx, CallTx} =
        aetx:specialize_type(contract_call(ContractPubkey, Src, Fun, Args,
                                           Amount, From, Nonce)),
    %% Don't allow named contracts!?
    {contract, ContractPubKey} =
        aeser_id:specialize(aect_call_tx:contract_id(CallTx)),
    Spec =
        #{  <<"caller">>          => aeser_api_encoder:encode(account_pubkey,
                                                              aect_call_tx:caller_pubkey(CallTx))
          , <<"nonce">>           => aect_call_tx:nonce(CallTx)
          , <<"contract_pubkey">> => aeser_api_encoder:encode(contract_pubkey, ContractPubKey)
          , <<"abi_version">>     => aect_call_tx:abi_version(CallTx)
          , <<"fee">>             => aect_call_tx:fee(CallTx)
          , <<"amount">>          => aect_call_tx:amount(CallTx)
          , <<"gas">>             => aect_call_tx:gas(CallTx)
          , <<"gas_price">>       => aect_call_tx:gas_price(CallTx)
          , <<"call_data">>       => aeser_api_encoder:encode(contract_bytearray,
                                                              aect_call_tx:call_data(CallTx))},
    Spec.

contract_call(ContractPubkey, Src, Fun, Args, Amount, From, Nonce) ->
    {ok, CallData} = aect_test_utils:encode_call_data(Src, Fun, Args),
    ABI = aect_test_utils:abi_version(),
    TxSpec =
        #{  caller_id   => aeser_id:create(account, From)
          , nonce       => Nonce
          , contract_id => aeser_id:create(contract, ContractPubkey)
          , abi_version => ABI
          , fee         => 1000000 * ?DEFAULT_GAS_PRICE
          , amount      => Amount
          , gas         => 1000000
          , gas_price   => ?DEFAULT_GAS_PRICE
          , call_data   => CallData},
    {ok, Tx} = aect_call_tx:new(TxSpec),
    Tx.

start_two_child_nodes(Config) ->
    [{Node1, NodeName1, Stakers1, Pinners1}, {Node2, NodeName2, Stakers2, Pinners2} | _] = ?config(nodes, Config),
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(?config(network_id, Config))} ],
    child_node_config(Node1, Stakers1, Pinners1, Config),
    aecore_suite_utils:start_node(Node1, Config, Env),
    aecore_suite_utils:connect(NodeName1, []),
    child_node_config(Node2, Stakers2, Pinners2, Config),
    aecore_suite_utils:start_node(Node2, Config, Env),
    aecore_suite_utils:connect(NodeName2, []),
    ok.

produce_first_epoch(Config) ->
    produce_n_epochs(Config, 1).

produce_some_epochs(Config) ->
    produce_n_epochs(Config, 5).

produce_n_epochs(Config, N) ->
    [{Node1, _, _, _}|_] = ?config(nodes, Config),
    %% produce blocks; TODO: produce to target height instead?
    {ok, Bs} = hctest:produce_cc_blocks(Config, #{count => N * ?CHILD_EPOCH_LENGTH}),
    %% check producers
    Producers = [ aec_blocks:miner(B) || B <- Bs, aec_blocks:is_key_block(B) ],
    ChildTopHeight = rpc(Node1, aec_chain, top_height, []),
    Leaders = hctest:leaders_at_height(Node1, ChildTopHeight, Config),
    ct:log("Bs: ~p  Leaders ~p", [Bs, Leaders]),
    %% Check that all producers are valid leaders
    ?assertEqual([], lists:usort(Producers) -- Leaders),
    %% If we have more than 1 leader, then we should see more than one producer
    %% at least for larger EPOCHs
    ?assert(length(Leaders) > 1, length(Producers) > 1),
    % ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    % {ok, ParentBlocks} = get_generations(?PARENT_CHAIN_NODE, 0, ParentTopHeight),
    % ct:log("Parent chain blocks ~p", [ParentBlocks]),
    {ok, ChildBlocks} = get_generations(Node1, 0, ChildTopHeight),
    ct:log("Child chain blocks ~p", [ChildBlocks]),
    ok.

check_default_pin(Config) ->
    [{Node, NodeName, _, _} | _] = ?config(nodes, Config),

    {ok, _} = hctest:produce_cc_blocks(Config, #{count => 12}),
    {ok, #{last := Last}} = rpc(Node, aec_chain_hc, epoch_info, []),
    {ok, LastLeader} = rpc(Node, aec_consensus_hc, leader_for_height, [Last]),
    ct:log("Last Leader: ~p", [LastLeader]),

    hctest:mine_to_last_block_in_epoch(Node, Config),

    aecore_suite_utils:subscribe(NodeName, pin),

    {ok, _} = hctest:produce_cc_blocks(Config, #{count => 2}),
    %% with current test setup, all validators have a pc account, so pins will always happen(?)
    {ok, #{info := {pin_accepted, _}}} = wait_for_ps(pin),

    aecore_suite_utils:unsubscribe(NodeName, pin),

    %% TODO test when not all validators have PC account, but how ensure
    %% that any given validator will be last leader within the run of the test???

    ok.

try_a_pin(Config) ->
    [{Node, _, _, _} | _] = ?config(nodes, Config),
    Hash = rpc(Node, aec_parent_connector, pin_to_pc, [hctest:pubkey(?ALICE),100000000,1000]),
    Raw = rpc(Node, aec_parent_connector, get_pin_by_tx_hash, [Hash]),
    ct:log("Raw: ~p", [Raw]).



%%% --------- pinning helpers


wait_for_ps(Event) ->
    receive
        {gproc_ps_event, Event, Info} -> {ok, Info};
        Other -> error({wrong_signal, Other})
    end.

%%% --------- helper functions

build_json_files(ElectionContract, NodeConfig, CTConfig) ->
    Pubkey = ?OWNER_PUBKEY,
    {_PatronPriv, PatronPub} = aecore_suite_utils:sign_keys(?NODE1),
    ct:log("Patron is ~p", [aeser_api_encoder:encode(account_pubkey, PatronPub)]),

    %% create staking contract
    MinStakeAmt = integer_to_list(trunc(math:pow(10,18) * 1)), %% 1 AE
    MSSrc = hctest:src(?MAIN_STAKING_CONTRACT, CTConfig),
    #{ <<"pubkey">> := StakingContractPubkey
     , <<"owner_pubkey">> := ContractOwner } = SC
        = contract_create_spec(?MAIN_STAKING_CONTRACT, MSSrc, [MinStakeAmt], 0, 1, Pubkey),
    {ok, StakingAddress} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                         StakingContractPubkey),
    %% assert assumption
    StakingAddress = staking_contract_address(),

    %% create election contract
    #{ <<"pubkey">> := ElectionContractPubkey
     , <<"owner_pubkey">> := ContractOwner } = EC
        = contract_create_spec(ElectionContract, hctest:src(ElectionContract, CTConfig),
                               [binary_to_list(StakingContractPubkey)], 0, 2, Pubkey),
    {ok, ElectionAddress} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                          ElectionContractPubkey),
    %% assert assumption
    ElectionAddress = election_contract_address(),
    {ok, SCId} = aeser_api_encoder:safe_decode(contract_pubkey, StakingContractPubkey),

    APub = binary_to_list(aeser_api_encoder:encode(account_pubkey, hctest:pubkey(?ALICE))),
    Call1 =
        contract_call_spec(SCId, MSSrc, "new_validator", [APub, APub, "true"],
                           ?INITIAL_STAKE, hctest:pubkey(?ALICE), 1),

    BPub = binary_to_list(aeser_api_encoder:encode(account_pubkey, hctest:pubkey(?BOB))),
    BPubSign = binary_to_list(aeser_api_encoder:encode(account_pubkey, hctest:pubkey(?BOB_SIGN))),
    Call2 =
        contract_call_spec(SCId, MSSrc, "new_validator", [BPub, BPubSign, "true"],
                           ?INITIAL_STAKE, hctest:pubkey(?BOB), 1),

    LPub = binary_to_list(aeser_api_encoder:encode(account_pubkey, hctest:pubkey(?LISA))),
    Call3 =
        contract_call_spec(SCId, MSSrc, "new_validator", [LPub, LPub, "true"],
                           ?INITIAL_STAKE, hctest:pubkey(?LISA), 1),

    AllCalls =  [Call1, Call2, Call3],
    ProtocolBin = integer_to_binary(aect_test_utils:latest_protocol_version()),
    #{<<"chain">> := #{<<"hard_forks">> := #{ProtocolBin := #{<<"contracts_file">> := ContractsFileName,
                                                              <<"accounts_file">> := AccountsFileName}}}} = NodeConfig,
    aecore_suite_utils:create_seed_file(ContractsFileName,
        #{<<"contracts">> => [SC, EC], <<"calls">> => AllCalls}),
    aecore_suite_utils:create_seed_file(AccountsFileName,
        #{  <<"ak_2evAxTKozswMyw9kXkvjJt3MbomCR1nLrf91BduXKdJLrvaaZt">> => 1000000000000000000000000000000000000000000000000,
            hctest:encoded_pubkey(?ALICE) => 2100000000000000000000000000,
            hctest:encoded_pubkey(?BOB) => 3100000000000000000000000000,
            hctest:encoded_pubkey(?BOB_SIGN) => 3100000000000000000000000000,
            hctest:encoded_pubkey(?LISA) => 4100000000000000000000000000
         }),
    ok.

node_config(Node, CTConfig, PotentialStakers, PotentialPinners, ReceiveAddress) ->
    NetworkId = ?config(network_id, CTConfig),
    GenesisStartTime = ?config(genesis_start_time, CTConfig),
    Stakers = lists:map(
                    fun(HCWho) ->
                        HCPriv = list_to_binary(aeu_hex:bin_to_hex( hctest:privkey(HCWho))), %% TODO: discuss key management
                        #{ <<"hyper_chain_account">> => #{<<"pub">> => hctest:encoded_pubkey(HCWho), <<"priv">> => HCPriv} }
                    end,
                    PotentialStakers),
    Pinners = lists:map(
                    fun({Owner, Pinner}) ->
                        HCPriv = list_to_binary(aeu_hex:bin_to_hex( hctest:privkey(Pinner))), %% TODO: discuss key management
                        #{ <<"parent_chain_account">> => #{
                           <<"pub">> => hctest:encoded_pubkey(Pinner),
                              <<"priv">> => HCPriv,
                              <<"owner">> => hctest:encoded_pubkey(Owner)
                           }
                        }
                    end,
                    PotentialPinners),
    ct:log("Stakers: ~p", [Stakers]),
    ct:log("Pinners: ~p", [Pinners]),
    ConsensusType = <<"hyperchain">>,
    Port = 44555,
    SpecificConfig =
                #{  <<"parent_chain">> =>
                    #{  <<"start_height">> => ?config(parent_start_height, CTConfig),
                        <<"finality">> => ?PARENT_FINALITY,
                        <<"parent_epoch_length">> => ?PARENT_EPOCH_LENGTH,
                        <<"consensus">> =>
                            #{  <<"type">> => <<"AE2DOGE">>,
                                <<"network_id">> => <<"testnet">>,
                                <<"spend_address">> => ReceiveAddress,
                                <<"fee">> => 100000,
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
                    #{<<"0">> => #{<<"type">> => ConsensusType,
                                <<"config">> =>
                                maps:merge(
                                    #{  <<"election_contract">> => aeser_api_encoder:encode(contract_pubkey, election_contract_address()),
                                        <<"rewards_contract">> => aeser_api_encoder:encode(contract_pubkey, staking_contract_address()),
                                        <<"staking_contract">> => aeser_api_encoder:encode(contract_pubkey, staking_contract_address()),
                                        <<"contract_owner">> => aeser_api_encoder:encode(account_pubkey,?OWNER_PUBKEY),
                                        <<"expected_key_block_rate">> => 2000,
                                        <<"stakers">> => Stakers,
                                        <<"pinners">> => Pinners,
                                        <<"pinning_reward_value">> => 4711,
                                        <<"fixed_coinbase">> => ?BLOCK_REWARD,
                                        <<"default_pinning_behavior">> => ?config(default_pinning_behavior, CTConfig)},
                                    SpecificConfig)
                                    }}},
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

staking_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 1).

election_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 2).

get_generations(Node, FromHeight, ToHeight) ->
    ReversedBlocks =
        lists:foldl(
            fun(Height, Accum) ->
                case rpc(Node, aec_chain, get_generation_by_height, [Height, backward]) of
                    {ok, #{key_block := KB, micro_blocks := MBs}} ->
                        ReversedGeneration = lists:reverse(MBs) ++ [KB],
                        ReversedGeneration ++ Accum;
                    error -> error({failed_to_fetch_generation, Height})
                end
            end,
            [],
            lists:seq(FromHeight, ToHeight)),
    {ok, lists:reverse(ReversedBlocks)}.
