%%% @doc Test setup support code, copy from aehttp_hyperchains_SUITE
%%% @end
-module(hctest_shared).

-import(aecore_suite_utils, [
    http_request/4,
    external_address/0
]).

-export([
    init_per_suite/2,
    end_per_suite/2,
    init_per_group/2,
    end_per_group/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    %%    start_two_child_nodes/1,
    empty_parent_block/1,
    block_difficulty/1,
    check_blocktime/1,
    get_contract_pubkeys/1,
    config_add_node/5,
    config_add_node1/1,
    config_add_node2/1,
    config_add_node3/1,
    start_child_nodes/2,
    get_nodes/2,
    wait_and_sync/1,
    mine_and_sync/1,
    get_cc_height_and_leader/1,
    with_saved_keys/2,
    produce_n_epochs/2,
    produce_cc_blocks/2
]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").
-include_lib("hc_test/include/hc_test.hrl").

init_per_suite(Config0, Nodes) ->
    case aect_test_utils:require_at_least_protocol(?CERES_PROTOCOL_VSN) of
        {skip, _} = Skip ->
            Skip;
        ok ->
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
                        100000000_000000000_000000000_000000000_000000000_000000000_000000000_000000000,
                    encoded_pubkey(?DWIGHT) => 2_100000000_000000000_000000000,
                    encoded_pubkey(?EDWIN) => 3_100000000_000000000_000000000
                }
            ),
            StakingContract = staking_contract_address(),
            ElectionContract = election_contract_address(),
            CtSrcMap = maps:from_list([
                {C, create_stub(C)}
             || C <- [?MAIN_STAKING_CONTRACT, ?STAKING_VALIDATOR_CONTRACT, ?HC_CONTRACT]
            ]),
            [
                {staking_contract, StakingContract},
                {election_contract, ElectionContract},
                {contract_src, CtSrcMap}
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

init_per_group(Group, ConfigPre) ->
    Config0 =
        case Group of
            default_pin -> [{default_pinning_behavior, true} | ConfigPre];
            _ -> [{default_pinning_behavior, false} | ConfigPre]
        end,
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
    {ok, _} = mine_key_blocks(
        ?PARENT_CHAIN_NODE_NAME,
        (StartHeight - ParentTopHeight) + ?PARENT_FINALITY
    ),
    [{staker_names, [?ALICE, ?BOB, ?LISA]}, {parent_start_height, StartHeight} | Config].

child_node_config(Node, Stakeholders, Pinners, CTConfig) ->
    ReceiveAddress = encoded_pubkey(?FORD),
    NodeConfig = node_config(Node, CTConfig, Stakeholders, Pinners, ReceiveAddress),
    build_json_files(?HC_CONTRACT, NodeConfig, CTConfig),
    aecore_suite_utils:create_config(Node, CTConfig, NodeConfig, [{add_peers, true}]).

end_per_group(Config) ->
    Config1 = with_saved_keys([nodes], Config),
    [
        aecore_suite_utils:stop_node(Node, Config1)
     || #ct_node{short_name = Node} <- proplists:get_value(nodes, Config1, [])
    ],
    %%    aecore_suite_utils:assert_no_errors_in_logs(Config1, []),
    aecore_suite_utils:assert_no_errors_in_logs(Config1, ["{handled_abort,parent_chain_not_synced}"]),
    Config1.

%% Here we decide which nodes are started/running
init_per_testcase(start_two_child_nodes, Config) ->
    Config1 =
        [
            {nodes, [
                #ct_node{short_name = ?NODE1, long_name = ?NODE1_NAME, stakers = [?ALICE, ?LISA]},
                #ct_node{short_name = ?NODE2, long_name = ?NODE2_NAME, stakers = [?BOB]}
            ]}
            | Config
        ],
    aect_test_utils:setup_testcase(Config1),
    Config1;
init_per_testcase(sync_third_node, Config) ->
    Config1 = with_saved_keys([nodes], Config),
    Nodes = proplists:get_value(nodes, Config1),
    Config2 = lists:keyreplace(
        nodes,
        1,
        Config1,
        {nodes, Nodes ++ [#ct_node{short_name = ?NODE3, long_name = ?NODE3_NAME}]}
    ),
    aect_test_utils:setup_testcase(Config2),
    Config2;
init_per_testcase(_Case, Config) ->
    Config1 = with_saved_keys([nodes], Config),
    aect_test_utils:setup_testcase(Config1),
    Config1.

end_per_testcase(_Case, Config) ->
    {save_config, Config}.

start_child_nodes(Nodes, Config) ->
    NetworkId = binary_to_list(proplists:get_value(network_id, Config)),
    AllNodeDefinitions = proplists:get_value(nodes, Config, []),
    StartNodeFn = fun(Node) ->
        #ct_node{short_name = Node1, long_name = NodeName1, stakers = Stakers1, pinners = Pinners1} = lists:keyfind(
            Node, #ct_node.short_name, AllNodeDefinitions
        ),
        Env = [{"AE__FORK_MANAGEMENT__NETWORK_ID", NetworkId}],
        child_node_config(Node1, Stakers1, Pinners1, Config),
        aecore_suite_utils:start_node(Node1, Config, Env),
        aecore_suite_utils:connect(NodeName1, [])
    end,
    lists:foreach(StartNodeFn, Nodes).

config_add_node(Config0, ShortName, LongName, Stakers, Pinners) ->
    Nodes0 = proplists:get_value(nodes, Config0, []),
    Nodes1 = [
        #ct_node{short_name = ShortName, long_name = LongName, stakers = Stakers, pinners = Pinners}
        | Nodes0
    ],
    Config1 = proplists:delete(nodes, Config0),
    [{nodes, Nodes1} | Config1].

config_add_node1(Config) ->
    config_add_node(Config, ?NODE1, ?NODE1_NAME, [?ALICE, ?LISA], [?LISA, ?EDWIN]).

config_add_node2(Config) ->
    config_add_node(Config, ?NODE2, ?NODE2_NAME, [?BOB_SIGN], [{?BOB_SIGN, ?EDWIN}]).

config_add_node3(Config) ->
    config_add_node(Config, ?NODE3, ?NODE3_NAME, [?LISA], [{?LISA, ?EDWIN}]).

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

wait_same_top(Nodes) ->
    wait_same_top(Nodes, 3).

wait_same_top(_Nodes, Attempts) when Attempts < 1 ->
    %% {error, run_out_of_attempts};
    throw({error, run_out_of_attempts});
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

check_blocktime(_Config) ->
    {ok, TopBlock} = aecore_suite_utils:rpc(?NODE1, aec_chain, top_key_block, []),
    check_blocktime_(TopBlock).

check_blocktime_(Block) ->
    case aec_blocks:height(Block) >= 1 of
        true ->
            {ok, PrevBlock} = aecore_suite_utils:rpc(?NODE1, aec_chain, get_block, [
                aec_blocks:prev_key_hash(Block)
            ]),
            Time1 = aec_blocks:time_in_msecs(Block),
            Time2 = aec_blocks:time_in_msecs(PrevBlock),
            [
                ct:pal(
                    "Blocktime not respected KB(~p) at ~p and KB(~p) at ~p",
                    [aec_blocks:height(Block), Time1, aec_blocks:height(PrevBlock), Time2]
                )
             || Time1 - Time2 < ?CHILD_BLOCK_TIME
            ],
            ?assertMatch(Diff when Diff >= ?CHILD_BLOCK_TIME, Time1 - Time2),
            check_blocktime_(PrevBlock);
        false ->
            ok
    end.

%%start_two_child_nodes(Config) ->
%%    [
%%        #ct_node{short_name = Node1, long_name = NodeName1, stakers = Stakers1, pinners = Pinners1},
%%        #ct_node{short_name = Node2, long_name = NodeName2, stakers = Stakers2, pinners = Pinners2}
%%        | _
%%    ] = proplists:get_value(nodes, Config),
%%    Env = [{"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(?config(network_id, Config))}],
%%
%%    child_node_config(Node1, Stakers1, Pinners1, Config),
%%    aecore_suite_utils:start_node(Node1, Config, Env),
%%    aecore_suite_utils:connect(NodeName1, []),
%%
%%    child_node_config(Node2, Stakers2, Pinners2, Config),
%%    aecore_suite_utils:start_node(Node2, Config, Env),
%%    aecore_suite_utils:connect(NodeName2, []),
%%    ok.

%%produce_first_epoch(Config) ->
%%    produce_n_epochs(Config, #{count_epochs => 1}).
%%
%%produce_some_epochs(Config) ->
%%    produce_n_epochs(Config, #{count_epochs => 5}).

%% TODO: flag 'unfinished' to produce one fewer blocks
-type produce_n_epochs() :: #{
    count_epochs => pos_integer(), skip_nodes => [node()], unfinished => boolean()
}.
-spec produce_n_epochs(Config :: proplists:proplist(), Options :: produce_n_epochs()) -> atom().
produce_n_epochs(Config, #{count_epochs := N} = Options) ->
    SkipNodes = maps:get(skip_nodes, Options, []),
    Nodes = get_nodes(Config, SkipNodes),
    Node1 = hd(Nodes),
    ct:pal("produce_n_epochs count=~0p skip=~0p node=~0p", [N, SkipNodes, Node1]),

    %% produce blocks
    {ok, Bs} = produce_cc_blocks(Config, #{count => N * ?CHILD_EPOCH_LENGTH}),

    %% check producers
    Producers = [aec_blocks:miner(B) || B <- Bs],
    ChildTopHeight = aecore_suite_utils:rpc(Node1, aec_chain, top_height, []),
    Leaders = leaders_at_height(Node1, ChildTopHeight, Config),
    ct:log("Bs: ~p  Leaders ~p", [Bs, Leaders]),

    %% Check that all producers are valid leaders
    ?assertEqual([], lists:usort(Producers) -- Leaders),

    %% If we have more than 1 leader, then we should see more than one producer
    %% at least for larger EPOCHs
    ?assert(length(Leaders) > 1, length(Producers) > 1),
    ParentTopHeight = aecore_suite_utils:rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    {ok, ParentBlocks} = get_generations(?PARENT_CHAIN_NODE, 0, ParentTopHeight),
    ct:log("Parent chain blocks ~p", [ParentBlocks]),
    {ok, ChildBlocks} = get_generations(Node1, 0, ChildTopHeight),
    ct:log("Child chain blocks ~p", [ChildBlocks]),
    ok.

empty_parent_block(_Config) ->
    case aect_test_utils:latest_protocol_version() < ?CERES_PROTOCOL_VSN of
        true ->
            {skip, lazy_leader_sync_broken_on_iris};
        false ->
            %% empty_parent_block_(Config)
            {skip, todo}
    end.

block_difficulty(Config) ->
    lists:foreach(
        fun(_) ->
            {ok, [KB]} = produce_cc_blocks(Config, #{count => 1}),
            {ok, AddedStakingPower} = inspect_election_contract(
                ?ALICE, current_added_staking_power, Config
            ),
            Target = aec_blocks:target(KB),
            {Target, Target} = {Target, aeminer_pow:integer_to_scientific(AddedStakingPower)}
        end,
        %% test with 20 elections
        lists:seq(1, 20)
    ),
    ok.

%%%=============================================================================
%%% HC Endpoints
%%%=============================================================================

get_contract_pubkeys(Config) ->
    [#ct_node{short_name = Node} | _] = proplists:get_value(nodes, Config),
    %% Verify that endpoint is available
    {ok, IsChildChain} = aecore_suite_utils:rpc(
        Node,
        aeu_env,
        find_config,
        [[<<"http">>, <<"endpoints">>, <<"hyperchain">>], [user_config, schema_default]]
    ),
    ?assert(IsChildChain),
    StakingContractPK = aecore_suite_utils:rpc(Node, aec_consensus_hc, get_contract_pubkey, [
        staking
    ]),
    ElectionContractPK = aecore_suite_utils:rpc(Node, aec_consensus_hc, get_contract_pubkey, [
        election
    ]),
    RewardsContractPK = aecore_suite_utils:rpc(Node, aec_consensus_hc, get_contract_pubkey, [
        rewards
    ]),
    ct:log("Calling hyperchain/contracts at ~p", [aecore_suite_utils:external_address()]),
    {ok, 200, Repl1} = aecore_suite_utils:http_request(
        aecore_suite_utils:external_address(), get, "hyperchain/contracts", []
    ),
    #{
        <<"staking">> := Staking,
        <<"election">> := Election,
        <<"rewards">> := Rewards
    } = Repl1,
    ?assertEqual({ok, StakingContractPK}, aeser_api_encoder:safe_decode(contract_pubkey, Staking)),
    ?assertEqual(
        {ok, ElectionContractPK}, aeser_api_encoder:safe_decode(contract_pubkey, Election)
    ),
    ?assertEqual({ok, RewardsContractPK}, aeser_api_encoder:safe_decode(contract_pubkey, Rewards)),

    ok.

pubkey({Pubkey, _, _}) -> Pubkey.

privkey({_, Privkey, _}) -> Privkey.

encoded_pubkey(Who) ->
    aeser_api_encoder:encode(account_pubkey, pubkey(Who)).

next_nonce(Node, Pubkey) ->
    case aecore_suite_utils:rpc(Node, aec_next_nonce, pick_for_account, [Pubkey, max]) of
        {ok, NextNonce} -> NextNonce;
        {error, account_not_found} -> 1
    end.

inspect_staking_contract(OriginWho, WhatToInspect, Config, TopHash) ->
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
    ContractPubkey = ?config(staking_contract, Config),
    do_contract_call(
        ContractPubkey, src(?MAIN_STAKING_CONTRACT, Config), Fun, Args, OriginWho, TopHash
    ).

inspect_election_contract(OriginWho, WhatToInspect, Config) ->
    TopHash = aecore_suite_utils:rpc(?NODE1, aec_chain, top_block_hash, []),
    inspect_election_contract(OriginWho, WhatToInspect, Config, TopHash).

inspect_election_contract(OriginWho, WhatToInspect, Config, TopHash) ->
    {Fun, Args} =
        case WhatToInspect of
            current_added_staking_power -> {"added_stake", []};
            _ -> {WhatToInspect, []}
        end,
    ContractPubkey = ?config(election_contract, Config),
    do_contract_call(ContractPubkey, src(?HC_CONTRACT, Config), Fun, Args, OriginWho, TopHash).

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
    case aecore_suite_utils:rpc(?NODE1, aec_dry_run, dry_run, [TopHash, [], [{tx, Tx}]]) of
        {error, _} = Err -> Err;
        {ok, {[{contract_call_tx, {ok, Call}}], _Events}} -> {ok, Call}
    end.

decode_consensus_result(Call, Fun, Src) ->
    ReturnType = aect_call:return_type(Call),
    ReturnValue = aect_call:return_value(Call),
    Res = aect_test_utils:decode_call_result(Src, Fun, ReturnType, ReturnValue),
    {ReturnType, Res}.

src(ContractName, Config) ->
    Srcs = ?config(contract_src, Config),
    maps:get(ContractName, Srcs).

build_json_files(ElectionContract, NodeConfig, CTConfig) ->
    Pubkey = ?OWNER_PUBKEY,
    {_PatronPriv, PatronPub} = aecore_suite_utils:sign_keys(?NODE1),
    ct:log("Patron is ~p", [aeser_api_encoder:encode(account_pubkey, PatronPub)]),

    %% create staking contract

    %% 1 AE
    MinStakeAmt = integer_to_list(trunc(math:pow(10, 18) * 1)),
    MSSrc = src(?MAIN_STAKING_CONTRACT, CTConfig),
    #{
        <<"pubkey">> := StakingContractPubkey,
        <<"owner_pubkey">> := ContractOwner
    } =
        SC =
        contract_create_spec(?MAIN_STAKING_CONTRACT, MSSrc, [MinStakeAmt], 0, 1, Pubkey),
    {ok, StakingAddress} = aeser_api_encoder:safe_decode(
        contract_pubkey,
        StakingContractPubkey
    ),
    %% assert assumption
    ?assertEqual(StakingAddress, staking_contract_address()),

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
            2,
            Pubkey
        ),
    {ok, ElectionAddress} = aeser_api_encoder:safe_decode(
        contract_pubkey,
        ElectionContractPubkey
    ),
    %% assert assumption
    ElectionAddress = election_contract_address(),
    {ok, SCId} = aeser_api_encoder:safe_decode(contract_pubkey, StakingContractPubkey),

    APub = binary_to_list(aeser_api_encoder:encode(account_pubkey, pubkey(?ALICE))),
    Call1 =
        contract_call_spec(
            SCId,
            MSSrc,
            "new_validator",
            [APub, APub, "true"],
            ?INITIAL_STAKE,
            pubkey(?ALICE),
            1
        ),

    BPub = binary_to_list(aeser_api_encoder:encode(account_pubkey, pubkey(?BOB))),
    BPubSign = binary_to_list(aeser_api_encoder:encode(account_pubkey, pubkey(?BOB_SIGN))),
    Call2 =
        contract_call_spec(
            SCId,
            MSSrc,
            "new_validator",
            [BPub, BPubSign, "true"],
            ?INITIAL_STAKE,
            pubkey(?BOB),
            1
        ),

    LPub = binary_to_list(aeser_api_encoder:encode(account_pubkey, pubkey(?LISA))),
    Call3 =
        contract_call_spec(
            SCId,
            MSSrc,
            "new_validator",
            [LPub, LPub, "true"],
            ?INITIAL_STAKE,
            pubkey(?LISA),
            1
        ),

    AllCalls = [Call1, Call2, Call3],
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
        #{<<"contracts">> => [SC, EC], <<"calls">> => AllCalls}
    ),
    aecore_suite_utils:create_seed_file(
        AccountsFileName,
        #{
            <<"ak_2evAxTKozswMyw9kXkvjJt3MbomCR1nLrf91BduXKdJLrvaaZt">> =>
                1000000000000000000000000000000000000000000000000,
            encoded_pubkey(?ALICE) => 2100000000000000000000000000,
            encoded_pubkey(?BOB) => 3100000000000000000000000000,
            encoded_pubkey(?BOB_SIGN) => 3100000000000000000000000000,
            encoded_pubkey(?LISA) => 4100000000000000000000000000
        }
    ),
    ok.

node_config(Node, CTConfig, PotentialStakers, PotentialPinners, ReceiveAddress) ->
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
    Pinners = lists:map(
        fun({Owner, Pinner}) ->
            %% TODO: discuss key management
            HCPriv = list_to_binary(aeu_hex:bin_to_hex(privkey(Pinner))),
            #{
                <<"parent_chain_account">> => #{
                    <<"pub">> => encoded_pubkey(Pinner),
                    <<"priv">> => HCPriv,
                    <<"owner">> => encoded_pubkey(Owner)
                }
            }
        end,
        PotentialPinners
    ),
    ct:log("Stakers: ~p", [Stakers]),
    ct:log("Pinners: ~p", [Pinners]),
    ConsensusType = <<"hyperchain">>,
    Port = aecore_suite_utils:external_api_port(?PARENT_CHAIN_NODE),
    SpecificConfig =
        #{
            <<"parent_chain">> =>
                #{
                    <<"start_height">> => ?config(parent_start_height, CTConfig),
                    <<"finality">> => ?PARENT_FINALITY,
                    <<"parent_epoch_length">> => ?PARENT_EPOCH_LENGTH,
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
                        }
                },
            <<"genesis_start_time">> => GenesisStartTime,
            <<"child_epoch_length">> => ?CHILD_EPOCH_LENGTH,
            <<"child_block_time">> => ?CHILD_BLOCK_TIME,
            <<"child_block_production_time">> => ?CHILD_BLOCK_PRODUCTION_TIME
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
                                        <<"staking_contract">> => aeser_api_encoder:encode(
                                            contract_pubkey, staking_contract_address()
                                        ),
                                        <<"contract_owner">> => aeser_api_encoder:encode(
                                            account_pubkey, ?OWNER_PUBKEY
                                        ),
                                        <<"expected_key_block_rate">> => 2000,
                                        <<"stakers">> => Stakers,
                                        <<"pinners">> => Pinners,
                                        <<"pinning_reward_value">> => 4711,
                                        <<"fixed_coinbase">> => ?BLOCK_REWARD,
                                        <<"default_pinning_behavior">> => ?config(
                                            default_pinning_behavior, CTConfig
                                        )
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
            }
    }.
%% ^ This relies on certain nonce numbers

staking_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 1).

election_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 2).

%% Increase the child chain with a number of key blocks
%% Automatically add key blocks on parent chain and
%% if there are Txs, put them in a micro block
-type produce_cc_blocks() :: #{
    count => pos_integer(),
    skip_nodes => [node()]
}.
-spec produce_cc_blocks(Config :: proplists:proplist(), Options :: produce_cc_blocks()) -> any().
produce_cc_blocks(Config, #{count := BlocksCnt} = Options) ->
    SkipNodes = maps:get(skip_nodes, Options, []),
    Nodes = get_nodes(Config, SkipNodes),
    Node = hd(Nodes),
    %%    ct:pal("produce_cc_blocks count=~0p skip=~0p nodes=~0p", [BlocksCnt, SkipNodes, Nodes]),

    TopHeight = aecore_suite_utils:rpc(Node, aec_chain, top_height, []),
    {ok, #{epoch := Epoch, first := First, last := Last, length := L} = Info} =
        aecore_suite_utils:rpc(Node, aec_chain_hc, epoch_info, [TopHeight]),
    %%    ct:log("produce_cc_blocks EpochInfo ~p", [Info]),

    %% At end of BlocksCnt child epoch approaches approx:
    CBAfterEpoch = BlocksCnt - (Last - TopHeight),
    ScheduleUpto = Epoch + 1 + (CBAfterEpoch div L),
    ParentTopHeight = aecore_suite_utils:rpc(?PARENT_CHAIN_NODE, aec_chain, top_height, []),
    %%    ct:log("produce_cc_blocks parent_toph=~p c_toph=~p for next ~p child blocks", [
    %%        ParentTopHeight, TopHeight, BlocksCnt
    %%    ]),

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
    produce_cc_blocks(Config, Options, ParentProduce).

-type parent_produce() :: [{pos_integer(), pos_integer()}].
-spec produce_cc_blocks(
    Config :: proplists:proplist(), Options :: produce_cc_blocks(), parent_produce()
) -> any().
produce_cc_blocks(Config, #{count := BlocksCnt} = Options, ParentProduce) ->
    SkipNodes = maps:get(skip_nodes, Options, []),
    Nodes = get_nodes(Config, SkipNodes),
    Node1 = hd(Nodes),
    ct:pal("produce_cc_blocks count=~0p skip=~0p pp=~0p", [BlocksCnt, SkipNodes, ParentProduce]),

    %% The previous production ended with wait_same_top, so asking first node is sufficient
    TopHeight = aecore_suite_utils:rpc(Node1, aec_chain, top_height, []),

    %% assert that the parent chain is not mining
    ?assertEqual(
        stopped,
        rpc:call(?PARENT_CHAIN_NODE_NAME, aec_conductor, get_mining_state, [])
    ),
    ct:log("parent produce ~p", [ParentProduce]),
    NewTopHeight = produce_to_cc_height(Config, #{
        top => TopHeight,
        goal => TopHeight + BlocksCnt,
        parent_produce => ParentProduce,
        skip_nodes => SkipNodes
    }),
    wait_same_top(Nodes),
    get_generations(Node1, TopHeight + 1, NewTopHeight).

%% It seems we automatically produce child chain blocks in the background
-type produce_to_cc_height() :: #{
    top => pos_integer(),
    goal => pos_integer(),
    parent_produce => parent_produce(),
    skip_nodes => [node()]
}.
-spec produce_to_cc_height(Config :: proplists:proplist(), Options :: produce_to_cc_height()) ->
    any().
produce_to_cc_height(
    Config, #{top := TopHeight, goal := GoalHeight, parent_produce := ParentProduce} = Options
) ->
    % NodeNames = [Name || {_, Name, _} <- ?config(nodes, Config)],
    SkipNodes = maps:get(skip_nodes, Options, []),
    {_Nodes, NodeNames} = get_nodes_and_nodenames(Config, SkipNodes),
    ct:pal("produce_to_cc_height nodenames=~0p skip=~0p top=~0p goal=~0p pp=~0p", [
        NodeNames,
        SkipNodes,
        TopHeight,
        GoalHeight,
        ParentProduce
    ]),
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

            %% TODO: add some assertions when we expect an MB (and not)!
            {ok, _Txs} = rpc:call(hd(NodeNames), aec_tx_pool, peek, [infinity]),

            %% This will mine 1 key-block (and 0 or 1 micro-blocks)
            {ok, Blocks} = mine_cc_blocks(NodeNames, 1),

            {Node, KeyBlock} = lists:last(Blocks),
            case Blocks of
                [{Node, MB}, _] ->
                    ?assertEqual(micro, aec_blocks:type(MB)),
                    ct:log("CC ~p produced micro-block: ~p", [Node, MB]);
                [_] ->
                    ok
            end,
            ?assertEqual(key, aec_blocks:type(KeyBlock)),
            ct:log("CC ~p produced key-block: ~p", [Node, KeyBlock]),

            Producer = get_block_producer_name(?config(staker_names, Config), KeyBlock),
            ct:log("~p produced CC block at height ~p", [Producer, aec_blocks:height(KeyBlock)]),
            produce_to_cc_height(Config, Options#{
                top => TopHeight + 1, parent_produce => NewParentProduce
            })
    end.

mine_cc_blocks(NodeNames, N) ->
    aecore_suite_utils:hc_mine_blocks(NodeNames, N).

get_generations(Node, FromHeight, ToHeight) ->
    ReversedBlocks =
        lists:foldl(
            fun(Height, Accum) ->
                case
                    aecore_suite_utils:rpc(Node, aec_chain, get_generation_by_height, [
                        Height, backward
                    ])
                of
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

get_block_producer(Node, Height) ->
    {ok, KeyHeader} = aecore_suite_utils:rpc(Node, aec_chain, get_key_header_by_height, [Height]),
    aec_headers:miner(KeyHeader).

leaders_at_height(Node, Height, Config) ->
    {ok, Hash} = aecore_suite_utils:rpc(Node, aec_chain_state, get_key_block_hash_at_height, [
        Height
    ]),
    {ok, Return} = inspect_staking_contract(?ALICE, leaders, Config, Hash),
    [
        begin
            {account_pubkey, K} = aeser_api_encoder:decode(LeaderKey),
            K
        end
     || [LeaderKey, _LeaderStake] <- Return
    ].

%%key_reward_provided() ->
%%    TopHeight = rpc(?NODE1, aec_chain, top_height, []),
%%    RewardHeight = TopHeight - ?REWARD_DELAY,
%%    key_reward_provided(RewardHeight).

key_reward_provided(RewardHeight) ->
    {
        get_block_producer(?NODE1, RewardHeight),
        aecore_suite_utils:rpc(?NODE1, aec_governance, block_mine_reward, [RewardHeight])
    }.

create_stub(Contract) ->
    create_stub(Contract, []).

create_stub(Contract, Opts0) ->
    File = aect_test_utils:contract_filename(Contract),
    Opts = Opts0 ++ [{no_code, true}] ++ aect_test_utils:copts({file, File}),
    {ok, SrcBin} = aect_test_utils:read_contract(Contract),
    {ok, Enc} = aeso_aci:contract_interface(json, binary_to_list(SrcBin), Opts),
    {ok, Stub} = aeso_aci:render_aci_json(Enc),
    binary_to_list(Stub).

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

mine_and_sync(Config) ->
    {ok, _KBs} = produce_cc_blocks(Config, #{count => 3}),
    Nodes = get_nodes(Config, []),
    {ok, _KB} = wait_same_top(Nodes, 3),
    ok.

wait_and_sync(Config) ->
    Nodes = get_nodes(Config, []),
    {ok, _KB} = wait_same_top(Nodes, 3),
    ok.

get_nodes(Config, SkipNodes) ->
    [
        Node
     || #ct_node{short_name = Node} <- proplists:get_value(nodes, Config),
        not lists:member(Node, SkipNodes)
    ].

-spec get_nodes_and_nodenames(Config :: proplists:proplist(), SkipNodes :: [node()]) ->
    {[node()], [node()]}.
get_nodes_and_nodenames(Config, SkipNodes) ->
    NN = [
        {Node, NodeName}
     || #ct_node{short_name = Node, long_name = NodeName} <- proplists:get_value(nodes, Config, []),
        not lists:member(Node, SkipNodes),
        not lists:member(NodeName, SkipNodes)
    ],
    lists:unzip(NN).

get_height(Node) ->
    aecore_suite_utils:rpc(Node, aec_chain, top_height, []).

accounts_countains_pubkey(Accounts, Pubkey) ->
    lists:any(fun({Pub, _Priv, _AccountName}) -> Pub =:= Pubkey end, Accounts).

get_node_with_pubkey(Pubkey, Config) ->
    NodeTriples = proplists:get_value(nodes, Config),
    Nodes = [Node || #ct_node{short_name = Node} <- NodeTriples],
    case
        [
            Node
         || #ct_node{short_name = Node, stakers = Accounts} <- NodeTriples,
            accounts_countains_pubkey(Accounts, Pubkey)
        ]
    of
        [] -> ct:fail("Can't find node with pubkey=~0p nodes=~0p", [Pubkey, Nodes]);
        [One] -> One;
        _Many -> ct:fail("Found multiple nodes with pubkey=~0p nodes=~0p", [Pubkey, Nodes])
    end.

get_cc_height_and_leader(Config) ->
    #ct_node{short_name = Node1} = hd(proplists:get_value(nodes, Config)),
    Height = get_height(Node1),
    {ok, LeaderPubkey} = aecore_suite_utils:rpc(Node1, aec_consensus_hc, leader_for_height, [
        Height
    ]),
    Leader = get_node_with_pubkey(LeaderPubkey, Config),
    {Height, Leader}.
