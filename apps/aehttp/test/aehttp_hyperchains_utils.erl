%%% @doc Helps the setup and drives the mining on multiple nodes for hyperchains test suites
%%% @end
-module(aehttp_hyperchains_utils).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include("./aehttp_hyperchains_utils.hrl").

-include_lib("aecontract/include/hard_forks.hrl").

% -import(aecore_suite_utils, [rpc/3, rpc/4]).

-export([
    run/2,
    init_per_suite/2,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    with_saved_keys/2,
    child_node_config/3,
    start_node/2, start_node/3,
    connect/2, connect/1
]).

-type action() ::
    {start_mining, node()}
    | {start_mining, node(), Opts :: map()}
    | {stop_mining, node()}
    %%
    | {epoch, node(), Count :: pos_integer()}
    %% Mines the given type of block on the given node
    | {cc, node(), NumBlocks :: pos_integer()}
    %% Use algorithm from aehttp_hyperchains_SUITE.erl to produce key and microblocks
    | {auto, node(), NumBlocks :: pos_integer()}
    | {kb, node(), NumParentBlocks :: pos_integer()}
    | {mb, node()}
    %% Asserts that the tx mempool is empty
    | {assert_empty_mempool, node()}
    %% Allows the test to block and wait
    | {wait, Milliseconds :: pos_integer()}
    %% Wait for same top block on all nodes
    | {sync, Nodes :: [node()]}.
-type scenario() :: [action()].
-type ct_config() :: proplists:proplist().

%% @doc Runs the steps from the scenario until the end
%% Example: run([{cc, Node, 10}, {wait, 1000}, {cc, Node, 10}])
-spec run(Scenario :: scenario(), CtConfig :: ct_config()) -> ok.
run([], _CtConfig) ->
    ok;
run([Instruction | Rest], CtConfig) ->
    case Instruction of
        {epoch, Node, Count} ->
            produce_n_epochs(Node, CtConfig, Count),
            run(Rest, CtConfig);
        %% hc_mine_blocks returns both key and micro blocks
        {cc, Node, NumBlocks} ->
            % {ok, _Blocks} = aecore_suite_utils:hc_mine_blocks([Node], NumBlocks, #{}),
            mine_cc_blocks([Node], NumBlocks),
            run(Rest, CtConfig);
        {auto, Node, Blocks} ->
            produce_cc_blocks(Node, CtConfig, Blocks),
            run(Rest, CtConfig);
        %% Will this fail if a keyblock is not expected?
        {kb, Node, NumParentBlocks} ->
            % {ok, _} = aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(Node),
            {ok, _KBs} = aecore_suite_utils:mine_key_blocks(Node, NumParentBlocks),
            run(Rest, CtConfig);
        %% Will this fail if a keyblock is expected but we do a microblock?
        {mb, Node} ->
            {ok, _} = aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(Node),
            run(Rest, CtConfig);
        {wait, Time} ->
            timer:sleep(Time),
            run(Rest, CtConfig);
        {assert_empty_mempool, Node} ->
            ?assertEqual({ok, []}, rpc:call(Node, aec_tx_pool, peek, [infinity])),
            run(Rest, CtConfig);
        {start_mining, Node} ->
            aecore_suite_utils:start_mining(Node, #{}),
            run(Rest, CtConfig);
        {start_mining, Node, Opts} ->
            aecore_suite_utils:start_mining(Node, Opts),
            run(Rest, CtConfig);
        {stop_mining, Node} ->
            aecore_suite_utils:stop_mining(Node),
            run(Rest, CtConfig);
        {sync, Nodes} ->
            wait_same_top(Nodes, 3),
            run(Rest, CtConfig)
    end.

%% Copy from aehttp_hyperchains_SUITE.erl
wait_same_top(_Nodes, Attempts) when Attempts < 1 ->
    {error, run_out_of_attempts};
wait_same_top(Nodes, Attempts) ->
    KBs = [rpc(Node, aec_chain, top_block, []) || Node <- Nodes],
    case lists:usort(KBs) of
        [KB] ->
            {ok, KB};
        Diffs ->
            ct:log("wait_same_top: Nodes differ: ~p", [Diffs]),
            timer:sleep(?CHILD_BLOCK_TIME div 2),
            wait_same_top(Nodes, Attempts - 1)
    end.

%% Increase the child chain with a number of key blocks.
%% Automatically add key blocks on parent chain and if there are Txs, put them in a micro block
-spec produce_cc_blocks(Node :: node(), CtConfig :: ct_config(), BlocksCnt :: pos_integer()) -> _.
produce_cc_blocks(Node, CtConfig, BlocksCnt) ->
    % [{Node, _, _} | _] = ?config(nodes, Config),
    TopHeight = rpc(Node, aec_chain, top_height, []),
    {ok, #{epoch := Epoch, first := First, last := Last, length := L} = Info} =
        rpc(Node, aec_chain_hc, epoch_info, [TopHeight]),
    ct:log("produce_cc_blocks: EpochInfo ~p", [Info]),

    %% At end of BlocksCnt child epoch approaches approx:
    CBAfterEpoch = BlocksCnt - (Last - TopHeight),
    ScheduleUpto = Epoch + 1 + (CBAfterEpoch div L),
    ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    ct:log("produce_cc_blocks: P@~p C@~p for next ~p child blocks", [
        ParentTopHeight, TopHeight, BlocksCnt
    ]),

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
    produce_cc_blocks(Node, CtConfig, BlocksCnt, ParentProduce).

produce_cc_blocks(Node, CtConfig, BlocksCnt, ParentProduce) ->
    % [{Node1, _, _} | _] = ?config(nodes, Config),

    %% The previous production ended with wait_same_top, so asking first node is sufficient
    TopHeight = rpc(Node, aec_chain, top_height, []),

    %% assert that the parent chain is not mining
    ?assertEqual(stopped, rpc:call(?PARENT_CHAIN_NODE, aec_conductor, get_mining_state, [])),
    ct:log("produce_cc_blocks: parent produce ~p", [ParentProduce]),
    NewTopHeight = produce_to_cc_height(CtConfig, TopHeight, TopHeight + BlocksCnt, ParentProduce),
    wait_same_top([N || {N, _, _} <- ?config(nodes, CtConfig)], 3),
    get_generations(Node, TopHeight + 1, NewTopHeight).

-spec produce_n_epochs(Node :: node(), CtConfig :: ct_config(), N :: pos_integer()) -> _.
produce_n_epochs(Node, CtConfig, N) ->
    %% [{Node1, _, _}|_] = ?config(nodes, Config),
    %% produce blocks
    {ok, Bs} = produce_cc_blocks(Node, CtConfig, N * ?CHILD_EPOCH_LENGTH),
    %% check producers
    Producers = [aec_blocks:miner(B) || B <- Bs],
    ChildTopHeight = rpc(Node, aec_chain, top_height, []),
    Leaders = leaders_at_height(Node, ChildTopHeight, CtConfig),
    ct:log("produce_n_epochs: Bs: ~p  Leaders ~p", [Bs, Leaders]),

    %% Check that all producers are valid leaders
    ?assertEqual([], lists:usort(Producers) -- Leaders),

    %% If we have more than 1 leader, then we should see more than one producer
    %% at least for larger EPOCHs
    ?assert(length(Leaders) > 1, length(Producers) > 1),
    ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    {ok, ParentBlocks} = get_generations(?PARENT_CHAIN_NODE, 0, ParentTopHeight),
    ct:log("produce_n_epochs: Parent chain blocks ~p", [ParentBlocks]),
    {ok, ChildBlocks} = get_generations(Node, 0, ChildTopHeight),
    ct:log("produce_n_epochs: Child chain blocks ~p", [ChildBlocks]),
    ok.

%% It seems we automatically produce child chain blocks in the background
produce_to_cc_height(CtConfig, TopHeight, GoalHeight, ParentProduce) ->
    NodeNames = [Name || {_, Name, _} <- ?config(nodes, CtConfig)],
    BlocksNeeded = GoalHeight - TopHeight,
    case BlocksNeeded > 0 of
        false ->
            TopHeight;
        true ->
            NewParentProduce =
                case ParentProduce of
                    [{CH, PBs} | PRest] when CH == TopHeight + 1 ->
                        mine_key_blocks(?PARENT_CHAIN_NODE, PBs),
                        PRest;
                    PP ->
                        PP
                end,
            KeyBlock =
                case rpc:call(hd(NodeNames), aec_tx_pool, peek, [infinity]) of
                    {ok, []} ->
                        {ok, [{N, Block}]} = mine_cc_blocks(NodeNames, 1),
                        ct:log("produce_to_cc_height: CC ~p mined block: ~p", [N, Block]),
                        Block;
                    {ok, _Txs} ->
                        {ok, [{N1, KB}, {N2, MB}]} = mine_cc_blocks(NodeNames, 2),
                        ?assertEqual(key, aec_blocks:type(KB)),
                        ?assertEqual(micro, aec_blocks:type(MB)),
                        ct:log("produce_to_cc_height: CC ~p mined block: ~p", [N1, KB]),
                        ct:log("produce_to_cc_height: CC ~p mined micro block: ~p", [N2, MB]),
                        KB
                end,
            Producer = get_block_producer_name(?config(staker_names, CtConfig), KeyBlock),
            ct:log("produce_to_cc_height: ~p produced CC block at height ~p", [
                Producer, aec_blocks:height(KeyBlock)
            ]),
            produce_to_cc_height(CtConfig, TopHeight + 1, GoalHeight, NewParentProduce)
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

get_generations(Node, FromHeight, ToHeight) ->
    ReversedBlocks =
        lists:foldl(
            fun(Height, Accum) ->
                case rpc(Node, aec_chain, get_generation_by_height, [Height, forward]) of
                    {ok, #{key_block := KB, micro_blocks := MBs}} ->
                        ReversedGeneration = lists:reverse(MBs) ++ [KB],
                        ReversedGeneration ++ Accum;
                    error ->
                        erlang:error({failed_to_fetch_generation, Height})
                end
            end,
            [],
            lists:seq(FromHeight, ToHeight)
        ),
    {ok, lists:reverse(ReversedBlocks)}.

leaders_at_height(Node, Height, CtConfig) ->
    {ok, Hash} = rpc(Node, aec_chain_state, get_key_block_hash_at_height, [Height]),
    {ok, Return} = inspect_staking_contract(?ALICE, leaders, CtConfig, Hash),
    [
        begin
            {account_pubkey, K} = aeser_api_encoder:decode(LeaderKey),
            K
        end
     || [LeaderKey, _LeaderStake] <- Return
    ].

% inspect_staking_contract(OriginWho, WhatToInspect, CtConfig) ->
%     TopHash = rpc(?NODE1, aec_chain, top_block_hash, []),
%     inspect_staking_contract(OriginWho, WhatToInspect, CtConfig, TopHash).

inspect_staking_contract(OriginWho, WhatToInspect, CtConfig, TopHash) ->
    {Fun, Args} =
        case WhatToInspect of
            {staking_power, Who} ->
                {"staking_power", [binary_to_list(encoded_pubkey(Who))]};
            {get_validator_state, Who} ->
                {"get_validator_state", [binary_to_list(encoded_pubkey(Who))]};
            get_state ->
                {"get_state", []};
            leaders ->
                {"sorted_validators", []}
        end,
    ContractPubkey = ?config(staking_contract, CtConfig),
    do_contract_call(
        ContractPubkey, src(?MAIN_STAKING_CONTRACT, CtConfig), Fun, Args, OriginWho, TopHash
    ).

mine_key_blocks(ParentNodeName, NumParentBlocks) ->
    {ok, _} = aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(ParentNodeName),
    {ok, KBs} = aecore_suite_utils:mine_key_blocks(ParentNodeName, NumParentBlocks),
    ct:log("Parent block mined ~p ~p number: ~p", [KBs, ParentNodeName, NumParentBlocks]),
    {ok, KBs}.

mine_cc_blocks(NodeNames, N) ->
    aecore_suite_utils:hc_mine_blocks(NodeNames, N).

get_block_producer_name(Parties, Block) ->
    Producer = aec_blocks:miner(Block),
    case lists:keyfind(Producer, 1, Parties) of
        false -> Producer;
        {_, _, Name} -> Name
    end.

encoded_pubkey(Who) ->
    aeser_api_encoder:encode(account_pubkey, pubkey(Who)).

src(ContractName, Config) ->
    Srcs = ?config(contract_src, Config),
    maps:get(ContractName, Srcs).

do_contract_call(CtPubkey, CtSrc, Fun, Args, Who, TopHash) ->
    F = fun() -> do_contract_call_(CtPubkey, CtSrc, Fun, Args, Who, TopHash) end,
    {T, Res} = timer:tc(F),
    ct:log("Calling contract took ~.2f ms", [T / 1000]),
    Res.

do_contract_call_(CtPubkey, CtSrc, Fun, Args, Who, TopHash) ->
    Tx = contract_call(CtPubkey, CtSrc, Fun, Args, 0, pubkey(Who)),
    {ok, Call} = dry_run(TopHash, Tx),
    decode_consensus_result(Call, Fun, CtSrc).

dry_run(TopHash, Tx) ->
    case rpc(?NODE1, aec_dry_run, dry_run, [TopHash, [], [{tx, Tx}]]) of
        {error, _} = Err -> Err;
        {ok, {[{contract_call_tx, {ok, Call}}], _Events}} -> {ok, Call}
    end.

pubkey({Pubkey, _, _}) -> Pubkey.

privkey({_, Privkey, _}) -> Privkey.

% name({_, _, Name}) -> Name.

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
    case rpc(Node, aec_next_nonce, pick_for_account, [Pubkey, max]) of
        {ok, NextNonce} -> NextNonce;
        {error, account_not_found} -> 1
    end.

%% Sets up a CT config with the given list of child nodes, and one parent node
init_per_suite(Config0, Nodes) ->
    {ok, _StartedApps} = application:ensure_all_started(gproc),
    Config = [{symlink_name, "latest.hyperchains"}, {test_module, ?MODULE}] ++ Config0,
    Config1 = aecore_suite_utils:init_per_suite(
        Nodes,
        %% config is rewritten per suite
        #{},
        [],
        Config
    ),
    GenesisProtocol = 1,
    {ok, AccountFileName} =
        aecore_suite_utils:hard_fork_filename(
            ?PARENT_CHAIN_NODE, Config1, integer_to_list(GenesisProtocol), "accounts_test.json"
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
                100000000_000000000_000000000_000000000_000000000_000000000_000000000_000000000,
            encoded_pubkey(?DWIGHT) => 2_100000000_000000000_000000000,
            encoded_pubkey(?EDWIN) => 3_100000000_000000000_000000000
        }
    ),
    StakingContract = staking_contract_address(),
    ElectionContract = election_contract_address(),
    {ok, SVBinSrc} = aect_test_utils:read_contract("StakingValidator"),
    {ok, MSBinSrc} = aect_test_utils:read_contract(?MAIN_STAKING_CONTRACT),
    {ok, EBinSrc} = aect_test_utils:read_contract(?HC_CONTRACT),
    [
        {staking_contract, StakingContract},
        {election_contract, ElectionContract},
        {contract_src, #{
            "StakingValidator" => create_stub(binary_to_list(SVBinSrc)),
            ?MAIN_STAKING_CONTRACT => create_stub(binary_to_list(MSBinSrc)),
            ?HC_CONTRACT => create_stub(binary_to_list(EBinSrc))
        }}
        | Config1
    ].

end_per_suite(Config) ->
    catch aecore_suite_utils:stop_node(?NODE1, Config),
    catch aecore_suite_utils:stop_node(?NODE2, Config),
    catch aecore_suite_utils:stop_node(?NODE3, Config),
    catch aecore_suite_utils:stop_node(?PARENT_CHAIN_NODE, Config),
    [
        application:stop(A)
     || A <- lists:reverse(
            proplists:get_value(started_apps, Config, [])
        )
    ],
    ok.

init_per_group(_, Config0) ->
    VM = fate,
    NetworkId = <<"hc">>,
    GenesisStartTime = aeu_time:now_in_msecs(),
    Config = [
        {network_id, NetworkId},
        {genesis_start_time, GenesisStartTime},
        {consensus, ?CONSENSUS}
        | aect_test_utils:init_per_group(VM, Config0)
    ],

    start_node(?PARENT_CHAIN_NODE, Config),
    connect(?PARENT_CHAIN_NODE, []),
    ParentTopHeight = rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    StartHeight = max(ParentTopHeight, ?PARENT_EPOCH_LENGTH),
    ct:log("Parent chain top height ~p start at ~p", [ParentTopHeight, StartHeight]),
    %%TODO mine less than necessary parent height and test chain starts when height reached
    {ok, _} = mine_key_blocks(
        ?PARENT_CHAIN_NODE,
        (StartHeight - ParentTopHeight) + ?PARENT_FINALITY
    ),
    [{staker_names, [?ALICE, ?BOB, ?LISA]}, {parent_start_height, StartHeight} | Config].

end_per_group(_Group, Config) ->
    Config1 = with_saved_keys([nodes], Config),
    [
        aecore_suite_utils:stop_node(Node, Config1)
     || {Node, _, _} <- proplists:get_value(nodes, Config1, [])
    ],
    Config1.

create_stub(ContractFile) ->
    create_stub(ContractFile, []).

create_stub(ContractFile, Opts) ->
    {ok, Enc} = aeso_aci:contract_interface(json, ContractFile, Opts ++ [{no_code, true}]),
    {ok, Stub} = aeso_aci:render_aci_json(Enc),
    binary_to_list(Stub).

validator_pool_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 1).

staking_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 2).

election_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 3).

with_saved_keys(Keys, Config) ->
    case ?config(saved_config, Config) of
        {_TestCase, SavedConfig} ->
            lists:foldl(
                fun(Key, Conf) ->
                    case proplists:get_value(Key, SavedConfig) of
                        undefined -> Conf;
                        Val -> [{Key, Val} | Conf]
                    end
                end,
                lists:keydelete(saved_config, 1, Config),
                Keys
            );
        _ ->
            Config
    end.

child_node_config(Node, Stakeholders, CTConfig) ->
    ReceiveAddress = encoded_pubkey(?FORD),
    Pinning = false,
    NodeConfig = node_config(Node, CTConfig, Stakeholders, ReceiveAddress, Pinning),
    build_json_files(?HC_CONTRACT, NodeConfig, CTConfig),
    aecore_suite_utils:create_config(Node, CTConfig, NodeConfig, [{add_peers, true}]).

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
    MinStakeAmt = integer_to_list(trunc(math:pow(10, 18) * 1)),
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

-spec start_node(node(), ct_config()) -> {ok, pid(), node()}.
start_node(N, Config) ->
    start_node(N, Config, []).

%% Returns {ok, PeerPid, LongNodeName}, even if short name was supplied
-spec start_node(node(), ct_config(), ExtraEnv :: proplists:proplist()) -> {ok, pid(), node()}.
start_node(Node, Config, ExtraEnv) ->
    MyDir = filename:dirname(code:which(?MODULE)),
    ConfigFilename = proplists:get_value(config_name, Config, "default"),
    Args0 =
        case ?config(build_to_connect_to_mainnet, Config) of
            %% no proxy!
            true -> [];
            _ -> ["-config", "./" ++ ConfigFilename]
        end,
    Args = ["-pa", MyDir, "-sname", atom_to_list(Node) | Args0],
    Env0 =
        case ?config(build_to_connect_to_mainnet, Config) of
            true ->
                [
                    % {"ERL_FLAGS", Args},
                    {"AETERNITY_CONFIG", "data/aeternity.json"},
                    {"RUNNER_LOG_DIR", "log"}
                ];
            _ ->
                [
                    % {"ERL_FLAGS", Args},
                    {"AETERNITY_CONFIG", "data/aeternity.json"},
                    {"RUNNER_LOG_DIR", "log"},
                    {"CODE_LOADING_MODE", "interactive"}
                ]
        end,
    Env = maybe_override(ExtraEnv, Env0),
    %% cmd(?OPS_BIN, node_shortcut(N, Config), "bin", ["daemon"], Env).
    ct:pal("Starting node name=~0p with env=~0p args=~0p~n", [Node, Env, Args]),

    {ok, SavedDir} = file:get_cwd(),
    UseDir = filename:join([
        ?config(priv_dir, Config), atom_to_list(?config(test_module, Config)), atom_to_list(Node)
    ]),
    ok = file:set_cwd(UseDir),
    %% For ?CT_PEER() options see peer:start_options() type
    {ok, _PeerPid, LongNodeName} = ?CT_PEER(#{
        name => Node,
        env => Env,
        args => Args
    }),
    ok = file:set_cwd(SavedDir),
    % boot_node(Node),
    {ok, _PeerPid, LongNodeName}.

-spec maybe_override(Extra :: proplists:proplist(), Env :: proplists:proplist()) ->
    proplists:proplist().
maybe_override([{K, _} = H | T], L0) ->
    [H | maybe_override(T, lists:keydelete(K, 1, L0))];
maybe_override([], L0) ->
    L0.

%% Run a sequence of application:start() RPC calls
boot_node(Node) ->
    RemoteApps = [
        sasl,
        lager,
        gproc,
        jobs,
        kache,
        % crypto,
        % public_key,
        ssl,
        aeserialization,
        aebytecode,
        aevm,
        aecontract,
        aens,
        aeoracle,
        aeprimop,
        aega,
        % ecrecover,
        % emcl,
        aefate,
        % ecli,
        aecli,
        aecore,
        aeapi,
        aechannel,
        aehttp,
        aemon,
        aestratum,
        aedevmode,
        aesync
    ],
    lists:foreach(
        fun(App) -> rpc:call(Node, application, ensure_all_started, [App]) end,
        RemoteApps
    ).

-spec connect(node(), list(atom()) | all_mocks) -> ok.
connect(N, all_mocks) ->
    Mocks = maps:keys(aecore_suite_utils:known_mocks()),
    connect(N, Mocks);
connect(N, Mocks) when is_list(Mocks) ->
    ok = connect(N),
    aecore_suite_utils:start_mocks(N, Mocks),
    ok.

-spec connect(node()) -> ok.
connect(N) ->
    connect_wait(N, aehttp).

connect_(N, Timeout, WaitF) when Timeout < 10000, is_function(WaitF, 0) ->
    timer:sleep(Timeout),
    case net_kernel:hidden_connect_node(N) of
        true ->
            ct:log("hidden_connect_node(~p) -> true", [N]),
            WaitF(),
            true;
        false ->
            ct:log("hidden_connect_node(~p) -> false, retrying ...", [N]),
            connect_(N, Timeout * 2, WaitF)
    end;
connect_(N, _, _) ->
    ct:log("exhausted retries (~p)", [N]),
    erlang:error({could_not_connect, N}).

-spec connect_wait(node(), atom()) -> ok.
connect_wait(Node, WaitForApp) ->
    connect_(Node, 50, fun() -> aecore_suite_utils:await_app(Node, WaitForApp) end),
    aecore_suite_utils:report_node_config(Node),
    ok.

% rpc(Mod, Fun, Args) ->
%     rpc(?DEFAULT_NODE, Mod, Fun, Args).

rpc(Node, Mod, Fun, Args) ->
    case rpc:call(Node, Mod, Fun, Args, 5_000) of
        {badrpc, Reason} -> error({badrpc, Reason});
        R -> R
    end.
