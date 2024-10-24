-module(hctest_utils).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../../aecontract/include/hard_forks.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").
-include("../include/hc_test.hrl").

-export([child_node_config/3, produce_cc_blocks/2, mine_cc_blocks/2, get_generations/3, get_block_producer_name/2]).

%% Retrieves configuration values from saved_config for specified keys and merges them into CT Config
with_saved_keys(Keys, Config) ->
    {_TC, SavedConfig} = ?config(saved_config, Config),
    lists:foldl(
        fun(Key, Conf) ->
            case proplists:get_value(Key, SavedConfig) of
                undefined -> Conf;
                Val -> [{Key, Val} | Conf]
            end
        end,
        lists:keydelete(saved_config, 1, Config),
        Keys
    ).

child_node_config(Node, Stakeholders, CTConfig) ->
    ReceiveAddress = encoded_pubkey(?FORD),
    Pinning = false,
    NodeConfig = node_config(Node, CTConfig, Stakeholders, ReceiveAddress, Pinning),
    build_json_files(?HC_CONTRACT, NodeConfig, CTConfig),
    aecore_suite_utils:create_config(Node, CTConfig, NodeConfig, [{add_peers, true}]).

pubkey({Pubkey, _, _}) -> Pubkey.

privkey({_, Privkey, _}) -> Privkey.

encoded_pubkey(Who) ->
    aeser_api_encoder:encode(account_pubkey, pubkey(Who)).

src(ContractName, Config) ->
    Srcs = ?config(contract_src, Config),
    maps:get(ContractName, Srcs).

node_config(Node, CTConfig, PotentialStakers, ReceiveAddress, ProducingCommitments) ->
    NetworkId = ?config(network_id, CTConfig),
    GenesisStartTime = ?config(genesis_start_time, CTConfig),
    Stakers = lists:map(
        fun(HCWho) ->
            %% TODO: discuss key management
            HCPriv = list_to_binary(aeu_hex:bin_to_hex(privkey(HCWho))),
            #{
                <<"hyper_chain_account">> => #{
                    <<"pub">> => encoded_pubkey(HCWho), <<"priv">> => HCPriv
                }
            }
        end,
        PotentialStakers
    ),
    ConsensusType = <<"hyper_chain">>,
    Port = aecore_suite_utils:external_api_port(?PARENT_CHAIN_NODE),
    SpecificConfig =
        #{
            <<"parent_chain">> =>
                #{
                    <<"start_height">> => ?config(parent_start_height, CTConfig),
                    <<"finality">> => ?PARENT_FINALITY,
                    <<"parent_generation">> => ?PARENT_EPOCH_LENGTH,
                    <<"consensus">> =>
                        #{
                            <<"type">> => <<"AE2AE">>,
                            <<"network_id">> => ?PARENT_CHAIN_NETWORK_ID,
                            <<"spend_address">> => ReceiveAddress,
                            <<"fee">> => 100000000000000,
                            <<"amount">> => 9700
                        },
                    <<"polling">> =>
                        #{
                            <<"fetch_interval">> => 100,
                            <<"cache_size">> => 10,
                            <<"nodes">> => [
                                iolist_to_binary(
                                    io_lib:format("http://test:Pass@127.0.0.1:~p", [Port])
                                )
                            ]
                        },
                    <<"producing_commitments">> => ProducingCommitments
                },
            <<"genesis_start_time">> => GenesisStartTime,
            <<"child_epoch_length">> => ?CHILD_EPOCH_LENGTH,
            <<"child_block_time">> => ?CHILD_BLOCK_TIME
        },
    Protocol = aect_test_utils:latest_protocol_version(),
    {ok, ContractFileName} = aecore_suite_utils:hard_fork_filename(
        Node, CTConfig, integer_to_list(Protocol), binary_to_list(NetworkId) ++ "_contracts.json"
    ),
    {ok, AccountFileName} = aecore_suite_utils:hard_fork_filename(
        Node, CTConfig, integer_to_list(Protocol), binary_to_list(NetworkId) ++ "_accounts.json"
    ),
    #{
        <<"chain">> =>
            #{
                <<"persist">> => false,
                <<"hard_forks">> => #{
                    integer_to_binary(Protocol) => #{
                        <<"height">> => 0,
                        <<"contracts_file">> => ContractFileName,
                        <<"accounts_file">> => AccountFileName
                    }
                },
                <<"consensus">> =>
                    #{
                        <<"0">> => #{
                            <<"type">> => ConsensusType,
                            <<"config">> =>
                                maps:merge(
                                    #{
                                        <<"election_contract">> => aeser_api_encoder:encode(
                                            contract_pubkey, election_contract_address()
                                        ),
                                        <<"rewards_contract">> => aeser_api_encoder:encode(
                                            contract_pubkey, staking_contract_address()
                                        ),
                                        <<"contract_owner">> => aeser_api_encoder:encode(
                                            account_pubkey, ?OWNER_PUBKEY
                                        ),
                                        <<"expected_key_block_rate">> => 2000,
                                        <<"stakers">> => Stakers
                                    },
                                    SpecificConfig
                                )
                        }
                    }
            },
        <<"fork_management">> =>
            #{<<"network_id">> => <<"this_will_be_overwritten_runtime">>},
        <<"logging">> => #{<<"level">> => <<"debug">>},
        <<"sync">> => #{<<"ping_interval">> => 5000},
        <<"http">> => #{<<"endpoints">> => #{<<"hyperchain">> => true}},
        <<"mining">> =>
            #{
                <<"micro_block_cycle">> => 1,
                <<"autostart">> => false,
                %%<<"autostart">> => ProducingCommitments,
                <<"beneficiary_reward_delay">> => ?REWARD_DELAY
                %% this relies on certain nonce numbers
            }
    }.

build_json_files(ElectionContract, NodeConfig, CTConfig) ->
    Pubkey = ?OWNER_PUBKEY,
    {_PatronPriv, PatronPub} = aecore_suite_utils:sign_keys(?NODE1),
    ct:log("Patron is ~p", [aeser_api_encoder:encode(account_pubkey, PatronPub)]),
    EncodePub =
        fun(P) ->
            binary_to_list(aeser_api_encoder:encode(account_pubkey, P))
        end,
    %% create staking contract

    %% 1 mln AE
    MinValidatorAmt = integer_to_list(trunc(math:pow(10, 18) * math:pow(10, 6))),
    %% 1 AE
    MinStakeAmt = integer_to_list(trunc(math:pow(10, 18))),
    MinStakePercent = "30",
    OnlineDelay = "0",
    StakeDelay = "0",
    UnstakeDelay = "0",
    #{<<"pubkey">> := StakingValidatorContract} =
        C0 =
        contract_create_spec(
            "StakingValidator",
            src("StakingValidator", CTConfig),
            [EncodePub(Pubkey), UnstakeDelay],
            0,
            1,
            Pubkey
        ),
    {ok, ValidatorPoolAddress} = aeser_api_encoder:safe_decode(
        contract_pubkey,
        StakingValidatorContract
    ),
    %% assert assumption
    ValidatorPoolAddress = validator_pool_contract_address(),
    MSSrc = src(?MAIN_STAKING_CONTRACT, CTConfig),
    #{
        <<"pubkey">> := StakingContractPubkey,
        <<"owner_pubkey">> := ContractOwner
    } =
        SC =
        contract_create_spec(
            ?MAIN_STAKING_CONTRACT,
            MSSrc,
            [
                binary_to_list(StakingValidatorContract),
                MinValidatorAmt,
                MinStakePercent,
                MinStakeAmt,
                OnlineDelay,
                StakeDelay,
                UnstakeDelay
            ],
            0,
            2,
            Pubkey
        ),
    {ok, StakingAddress} = aeser_api_encoder:safe_decode(
        contract_pubkey,
        StakingContractPubkey
    ),
    %% assert assumption
    StakingAddress = staking_contract_address(),
    %% create election contract
    #{
        <<"pubkey">> := ElectionContractPubkey,
        <<"owner_pubkey">> := ContractOwner
    } =
        EC =
        contract_create_spec(
            ElectionContract,
            src(ElectionContract, CTConfig),
            [binary_to_list(StakingContractPubkey)],
            0,
            3,
            Pubkey
        ),
    {ok, ElectionAddress} = aeser_api_encoder:safe_decode(
        contract_pubkey,
        ElectionContractPubkey
    ),
    %% assert assumption
    ElectionAddress = election_contract_address(),
    {ok, SCId} = aeser_api_encoder:safe_decode(
        contract_pubkey,
        StakingContractPubkey
    ),
    Call1 =
        contract_call_spec(
            SCId,
            MSSrc,
            "new_validator",
            [],
            ?INITIAL_STAKE,
            pubkey(?ALICE),
            1
        ),
    Call2 =
        contract_call_spec(
            SCId,
            MSSrc,
            "new_validator",
            [],
            ?INITIAL_STAKE,
            pubkey(?BOB),
            1
        ),
    Call3 =
        contract_call_spec(
            SCId,
            MSSrc,
            "new_validator",
            [],
            ?INITIAL_STAKE,
            pubkey(?LISA),
            1
        ),
    Call4 =
        contract_call_spec(
            SCId,
            MSSrc,
            "set_online",
            [],
            0,
            pubkey(?ALICE),
            2
        ),
    Call5 =
        contract_call_spec(
            SCId,
            MSSrc,
            "set_online",
            [],
            0,
            pubkey(?BOB),
            2
        ),
    Call6 =
        contract_call_spec(
            SCId,
            MSSrc,
            "set_online",
            [],
            0,
            pubkey(?LISA),
            2
        ),
    Call7 =
        contract_call_spec(
            SCId,
            MSSrc,
            "set_validator_name",
            ["\"Alice\""],
            0,
            pubkey(?ALICE),
            3
        ),
    Call8 =
        contract_call_spec(
            SCId,
            MSSrc,
            "set_validator_name",
            ["\"Bob\""],
            0,
            pubkey(?BOB),
            3
        ),
    Call9 =
        contract_call_spec(
            SCId,
            MSSrc,
            "set_validator_name",
            ["\"Lisa\""],
            0,
            pubkey(?LISA),
            3
        ),
    Call10 =
        contract_call_spec(
            SCId,
            MSSrc,
            "set_validator_description",
            [
                "\"Alice is a really awesome validator and she had set a description of her great service to the work.\""
            ],
            0,
            pubkey(?ALICE),
            4
        ),
    Call11 =
        contract_call_spec(
            SCId,
            MSSrc,
            "set_validator_avatar_url",
            ["\"https://aeternity.com/images/aeternity-logo.svg\""],
            0,
            pubkey(?ALICE),
            5
        ),

    Call12 =
        contract_call_spec(
            ElectionAddress,
            src(ElectionContract, CTConfig),
            "init_epochs",
            [integer_to_list(?CHILD_EPOCH_LENGTH)],
            0,
            ?OWNER_PUBKEY,
            4
        ),
    %% create a BRI validator in the contract so they can receive
    %% rewards as well
    %% TODO: discuss how we want to tackle this:
    %%  A) require the BRI account to be validator
    %%  B) allow pending stake in the contract that is not allocated
    %%  yet
    %%  C) something else
    %% Call12 =
    %%     contract_call_spec(SCId, MSSrc,
    %%                         "new_validator", [],
    %%                         ?INITIAL_STAKE, BRIPub, 1),
    %% Call13 =
    %%     contract_call_spec(SCId, MSSrc,
    %%                         "set_validator_description",
    %%                         ["\"This validator is offline. She can never become a leader. She has no name set. She is receiving the BRI rewards\""],
    %%                         0, BRIPub, 2),
    %% keep the BRI offline
    AllCalls = [
        Call1,
        Call2,
        Call3,
        Call4,
        Call5,
        Call6,
        Call7,
        Call8,
        Call9,
        Call10,
        Call11,
        Call12
    ],
    ProtocolBin = integer_to_binary(aect_test_utils:latest_protocol_version()),
    #{
        <<"chain">> := #{
            <<"hard_forks">> := #{
                ProtocolBin := #{
                    <<"contracts_file">> := ContractsFileName,
                    <<"accounts_file">> := AccountsFileName
                }
            }
        }
    } = NodeConfig,
    aecore_suite_utils:create_seed_file(
        ContractsFileName,
        #{<<"contracts">> => [C0, SC, EC], <<"calls">> => AllCalls}
    ),
    aecore_suite_utils:create_seed_file(
        AccountsFileName,
        #{
            <<"ak_2evAxTKozswMyw9kXkvjJt3MbomCR1nLrf91BduXKdJLrvaaZt">> =>
                1000000000000000000000000000000000000000000000000,
            encoded_pubkey(?ALICE) => 2100000000000000000000000000,
            encoded_pubkey(?BOB) => 3100000000000000000000000000,
            encoded_pubkey(?LISA) => 4100000000000000000000000000
        }
    ),
    ok.

validator_pool_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 1).

staking_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 2).

election_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 3).

contract_create_spec(Name, Src, Args, Amount, Nonce, Owner) ->
    {ok, Code} = aect_test_utils:compile_contract(aect_test_utils:sophia_version(), Name),
    Pubkey = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    EncodedPubkey = aeser_api_encoder:encode(contract_pubkey, Pubkey),
    EncodedOwner = aeser_api_encoder:encode(account_pubkey, Owner),
    EncodedCode = aeser_api_encoder:encode(contract_bytearray, Code),
    {ok, CallData} = aect_test_utils:encode_call_data(Src, "init", Args),
    EncodedCallData = aeser_api_encoder:encode(contract_bytearray, CallData),
    VM = aect_test_utils:vm_version(),
    ABI = aect_test_utils:abi_version(),
    Spec = #{
        <<"amount">> => Amount,
        <<"vm_version">> => VM,
        <<"abi_version">> => ABI,
        <<"nonce">> => Nonce,
        <<"code">> => EncodedCode,
        <<"call_data">> => EncodedCallData,
        <<"pubkey">> => EncodedPubkey,
        <<"owner_pubkey">> => EncodedOwner
    },
    Spec.

contract_call_spec(ContractPubkey, Src, Fun, Args, Amount, From, Nonce) ->
    {contract_call_tx, CallTx} =
        aetx:specialize_type(
            contract_call(
                ContractPubkey,
                Src,
                Fun,
                Args,
                Amount,
                From,
                Nonce
            )
        ),
    %% Don't allow named contracts!?
    {contract, ContractPubKey} =
        aeser_id:specialize(aect_call_tx:contract_id(CallTx)),
    Spec =
        #{
            <<"caller">> => aeser_api_encoder:encode(
                account_pubkey,
                aect_call_tx:caller_pubkey(CallTx)
            ),
            <<"nonce">> => aect_call_tx:nonce(CallTx),
            <<"contract_pubkey">> => aeser_api_encoder:encode(contract_pubkey, ContractPubKey),
            <<"abi_version">> => aect_call_tx:abi_version(CallTx),
            <<"fee">> => aect_call_tx:fee(CallTx),
            <<"amount">> => aect_call_tx:amount(CallTx),
            <<"gas">> => aect_call_tx:gas(CallTx),
            <<"gas_price">> => aect_call_tx:gas_price(CallTx),
            <<"call_data">> => aeser_api_encoder:encode(
                contract_bytearray,
                aect_call_tx:call_data(CallTx)
            )
        },
    Spec.

contract_call(ContractPubkey, Src, Fun, Args, Amount, From) ->
    %% no contract calls support for parent chain
    Nonce = next_nonce(?NODE1, From),
    contract_call(ContractPubkey, Src, Fun, Args, Amount, From, Nonce).

contract_call(ContractPubkey, Src, Fun, Args, Amount, From, Nonce) ->
    {ok, CallData} = aect_test_utils:encode_call_data(Src, Fun, Args),
    ABI = aect_test_utils:abi_version(),
    TxSpec =
        #{
            caller_id => aeser_id:create(account, From),
            nonce => Nonce,
            contract_id => aeser_id:create(contract, ContractPubkey),
            abi_version => ABI,
            fee => 1000000 * ?DEFAULT_GAS_PRICE,
            amount => Amount,
            gas => 1000000,
            gas_price => ?DEFAULT_GAS_PRICE,
            call_data => CallData
        },
    {ok, Tx} = aect_call_tx:new(TxSpec),
    Tx.

decode_consensus_result(Call, Fun, Src) ->
    ReturnType = aect_call:return_type(Call),
    ReturnValue = aect_call:return_value(Call),
    Res = aect_test_utils:decode_call_result(Src, Fun, ReturnType, ReturnValue),
    {ReturnType, Res}.

next_nonce(Node, Pubkey) ->
    case aecore_suite_utils:rpc(Node, aec_next_nonce, pick_for_account, [Pubkey, max]) of
        {ok, NextNonce} -> NextNonce;
        {error, account_not_found} -> 1
    end.

create_stub(ContractFile) ->
    create_stub(ContractFile, []).

create_stub(ContractFile, Opts) ->
    {ok, Enc} = aeso_aci:contract_interface(json, ContractFile, Opts ++ [{no_code, true}]),
    {ok, Stub} = aeso_aci:render_aci_json(Enc),
    binary_to_list(Stub).

%% Increase the child chain with a number of key blocks. Automatically add key blocks on parent chain and
%% if there are Txs, put them in a micro block.
-spec produce_cc_blocks(Config :: ct_config(), BlocksCnt :: pos_integer()) -> ok.
produce_cc_blocks(Config, BlocksCnt) ->
    [{Node, _, _} | _] = ?config(nodes, Config),
    TopHeight = aecore_suite_utils:rpc(Node, aec_chain, top_height, []),
    {ok, #{epoch := Epoch, first := First, last := Last, length := L} = Info} =
        aecore_suite_utils:rpc(Node, aec_chain_hc, epoch_info, [TopHeight]),
    ct:log("EpochInfo ~p", [Info]),

    %% At end of BlocksCnt child epoch approaches approx:
    CBAfterEpoch = BlocksCnt - (Last - TopHeight),
    ScheduleUpto = Epoch + 1 + (CBAfterEpoch div L),
    ParentTopHeight = aecore_suite_utils:rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    ct:log("P@~p C@~p for next ~p child blocks", [ParentTopHeight, TopHeight, BlocksCnt]),

    %% Spread parent blocks over BlocksCnt
    ParentProduce =
        lists:append([
            spread(
                ?PARENT_EPOCH_LENGTH,
                TopHeight,
                [{CH, 0} || CH <- lists:seq(First + E * L, Last + E * L)]
            )
         || E <- lists:seq(0, ScheduleUpto - Epoch)
        ]),
    %% Last parameter steers where in Child epoch parent block is produced
    produce_cc_blocks_(Config, BlocksCnt, ParentProduce).

-spec produce_cc_blocks_(Config :: ct_config(), BlocksCnt :: pos_integer(), ParentProduce :: list()) -> ok.
produce_cc_blocks_(Config, BlocksCnt, ParentProduce) ->
    [{Node1, _, _} | _] = ?config(nodes, Config),

    %% The previous production ended with wait_same_top, so asking first node is sufficient
    TopHeight = aecore_suite_utils:rpc(Node1, aec_chain, top_height, []),

    %% assert that the parent chain is not mining
    %% elp:ignore W0014 (cross_node_eval)
    ?assertEqual(stopped, rpc:call(?PARENT_CHAIN_NODE_NAME, aec_conductor, get_mining_state, [])),
    ct:log("parent produce ~p", [ParentProduce]),

    NewTopHeight = produce_to_cc_height(Config, TopHeight, TopHeight + BlocksCnt, ParentProduce),
    % NewTopHeight = case SpecialBehaviour of
    %     undefined ->
    %         produce_to_cc_height(Config, TopHeight, TopHeight + BlocksCnt, ParentProduce);
    %     late_producing ->
    %         ct:log("Mining ~p blocks 3s late", [])
    %         % TODO: Set aeu_ets_cache:put on the remote node to make the child chain mine 3s later
    %         Nodes = [N || {N, _, _} <- ?config(nodes, Config)],
    %         set_remote_mining_delay(Nodes, 2_000),
    %         NTH = produce_to_cc_height(Config, TopHeight, TopHeight + BlocksCnt, ParentProduce),
    %         set_remote_mining_delay(Nodes, 200),
    %         NTH
    %     end,
    wait_same_top([Node || {Node, _, _} <- ?config(nodes, Config)]),
    get_generations(Node1, TopHeight + 1, NewTopHeight).

set_remote_mining_delay(Nodes, Delay) ->
    lists:foreach(fun({N, _, _}) ->
        %% TODO: Figure out the correct table name and key name
        %% rpc(N, aeu_ets_cache, put, [?ETS_CACHE_TABLE, child_block_time, Delay])
        todo
    end, Nodes).

%% It seems we automatically produce child chain blocks in the background
-spec produce_to_cc_height(
    Config :: ct_config(),
    TopHeight :: non_neg_integer(),
    GoalHeight :: non_neg_integer(),
    ParentProduce :: list()
) -> non_neg_integer().
produce_to_cc_height(Config, TopHeight, GoalHeight, ParentProduce) ->
    NodeNames = [Name || {_, Name, _} <- ?config(nodes, Config)],
    BlocksNeeded = GoalHeight - TopHeight,
    case BlocksNeeded > 0 of
        false ->
            TopHeight;
        true ->
            NewParentProduce =
                case ParentProduce of
                    [{CH, PBs} | PRest] when CH == TopHeight + 1 ->
                        mine_key_blocks(?PARENT_CHAIN_NODE_NAME, PBs),
                        PRest;
                    PP ->
                        PP
                end,
            KeyBlock =
                case rpc:call(hd(NodeNames), aec_tx_pool, peek, [infinity]) of
                    {ok, []} ->
                        {ok, [{N, Block}]} = mine_cc_blocks(NodeNames, 1),
                        ct:log("CC ~p mined block: ~p", [N, Block]),
                        Block;
                    {ok, _Txs} ->
                        {ok, [{N1, KB}, {N2, MB}]} = mine_cc_blocks(NodeNames, 2),
                        ?assertEqual(key, aec_blocks:type(KB)),
                        ?assertEqual(micro, aec_blocks:type(MB)),
                        ct:log("CC ~p mined block: ~p", [N1, KB]),
                        ct:log("CC ~p mined micro block: ~p", [N2, MB]),
                        KB
                end,
            Producer = get_block_producer_name(?config(staker_names, Config), KeyBlock),
            ct:log("~p produced CC block at height ~p", [Producer, aec_blocks:height(KeyBlock)]),
            produce_to_cc_height(Config, TopHeight + 1, GoalHeight, NewParentProduce)
    end.

mine_cc_blocks(NodeNames, N) ->
    aecore_suite_utils:hc_mine_blocks(NodeNames, N).

get_generations(Node, FromHeight, ToHeight) ->
    ReversedBlocks =
        lists:foldl(
            fun(Height, Accum) ->
                case aecore_suite_utils:rpc(Node, aec_chain, get_generation_by_height, [Height, forward]) of
                    {ok, #{key_block := KB, micro_blocks := MBs}} ->
                        ReversedGeneration = lists:reverse(MBs) ++ [KB],
                        ReversedGeneration ++ Accum;
                    error ->
                        error({failed_to_fetch_generation, Height})
                end
            end,
            [],
            lists:seq(FromHeight, ToHeight)
        ),
    {ok, lists:reverse(ReversedBlocks)}.

mine_key_blocks(ParentNodeName, NumParentBlocks) ->
    {ok, _} = aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(ParentNodeName),
    {ok, KBs} = aecore_suite_utils:mine_key_blocks(ParentNodeName, NumParentBlocks),
    ct:log("Parent block mined ~p ~p number: ~p", [KBs, ParentNodeName, NumParentBlocks]),
    {ok, KBs}.

%get_block_producer_name(Parties, Node, Height) ->
%    Producer = get_block_producer(Node, Height),
%    case lists:keyfind(Producer, 1, Parties) of
%        false -> Producer;
%        {_, _, Name} -> Name
%    end.

get_block_producer_name(Parties, Block) ->
    Producer = aec_blocks:miner(Block),
    case lists:keyfind(Producer, 1, Parties) of
        false -> Producer;
        {_, _, Name} -> Name
    end.

spread(_, _, []) ->
    [];
spread(0, TopHeight, Spread) ->
    [{CH, N} || {CH, N} <- Spread, N /= 0, CH > TopHeight];
%spread(N, TopHeight, [{CH, K} | Spread]) when length(Spread) < N ->
%    %% Take speed first (not realistic), then fill rest
%    spread(0, TopHeight, [{CH, K + N - length(Spread)} | [ {CH2, X+1} || {CH2, X} <- Spread]]);
spread(N, TopHeight, Spread) when N rem 2 == 0 ->
    {Left, Right} = lists:split(length(Spread) div 2, Spread),
    spread(N div 2, TopHeight, Left) ++ spread(N div 2, TopHeight, Right);
spread(N, TopHeight, Spread) when N rem 2 == 1 ->
    {Left, [{Middle, K} | Right]} = lists:split(length(Spread) div 2, Spread),
    spread(N div 2, TopHeight, Left) ++ [{Middle, K + 1} || Middle > TopHeight] ++
        spread(N div 2, TopHeight, Right).

wait_same_top(Nodes) ->
    wait_same_top(Nodes, 3).

wait_same_top(_Nodes, Attempts) when Attempts < 1 ->
    {error, run_out_of_attempts};
wait_same_top(Nodes, Attempts) ->
    KBs = [aecore_suite_utils:rpc(Node, aec_chain, top_block, []) || Node <- Nodes],
    case lists:usort(KBs) of
        [KB] ->
            {ok, KB};
        Diffs ->
            ct:log("Nodes differ: ~p", [Diffs]),
            timer:sleep(?CHILD_BLOCK_TIME div 2),
            wait_same_top(Nodes, Attempts - 1)
    end.
