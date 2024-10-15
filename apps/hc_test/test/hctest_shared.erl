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
    init_per_group/1,
    end_per_group/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    start_two_child_nodes/1,
    empty_parent_block/1,
    block_difficulty/1,
    check_blocktime/1,
    get_contract_pubkeys/1,
    config_add_node/4,
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

%%-define(GENESIS_BENFICIARY,
%%    <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
%%        0>>
%%).

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
                    encoded_pubkey(?DWIGHT) => 2100000000000000000000000000,
                    encoded_pubkey(?EDWIN) => 3100000000000000000000000000
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
    {ok, _} = mine_key_blocks(
        ?PARENT_CHAIN_NODE_NAME,
        (StartHeight - ParentTopHeight) + ?PARENT_FINALITY
    ),
    [{staker_names, [?ALICE, ?BOB, ?LISA]}, {parent_start_height, StartHeight} | Config].

child_node_config(Node, Stakeholders, CTConfig) ->
    ReceiveAddress = encoded_pubkey(?FORD),
    Pinning = false,
    NodeConfig = node_config(Node, CTConfig, Stakeholders, ReceiveAddress, Pinning),
    build_json_files(?HC_CONTRACT, NodeConfig, CTConfig),
    aecore_suite_utils:create_config(Node, CTConfig, NodeConfig, [{add_peers, true}]).

end_per_group(Config) ->
    Config1 = with_saved_keys([nodes], Config),
    [
        aecore_suite_utils:stop_node(Node, Config1)
     || {Node, _, _} <- proplists:get_value(nodes, Config1, [])
    ],

    aecore_suite_utils:assert_no_errors_in_logs(Config1, ["{handled_abort,parent_chain_not_synced}"]),

    Config1.

%% Here we decide which nodes are started/running
init_per_testcase(start_two_child_nodes, Config) ->
    Config1 =
        [
            {nodes, [
                {?NODE1, ?NODE1_NAME, [?ALICE, ?LISA]},
                {?NODE2, ?NODE2_NAME, [?BOB]}
            ]}
            | Config
        ],
    aect_test_utils:setup_testcase(Config1),
    Config1;
init_per_testcase(sync_third_node, Config) ->
    Config1 = with_saved_keys([nodes], Config),
    Nodes = ?config(nodes, Config1),
    Config2 = lists:keyreplace(
        nodes,
        1,
        Config1,
        {nodes, Nodes ++ [{?NODE3, ?NODE3_NAME, []}]}
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
    NetworkId = binary_to_list(?config(network_id, Config)),
    AllNodeTriples = proplists:get_value(nodes, Config, []),
    StartNodeFn = fun(Node) ->
        {Node, NodeName, Stakers} = lists:keyfind(Node, 1, AllNodeTriples),
        Env = [{"AE__FORK_MANAGEMENT__NETWORK_ID", NetworkId}],
        child_node_config(Node, Stakers, Config),
        aecore_suite_utils:start_node(Node, Config, Env),
        aecore_suite_utils:connect(NodeName, [])
    end,
    lists:foreach(StartNodeFn, Nodes).

config_add_node(Config0, Node, NodeName, Stakers) ->
    Nodes0 = proplists:get_value(nodes, Config0, []),
    Nodes1 = [{Node, NodeName, Stakers} | Nodes0],
    Config1 = proplists:delete(nodes, Config0),
    [{nodes, Nodes1} | Config1].

config_add_node1(Config) ->
    config_add_node(Config, ?NODE1, ?NODE1_NAME, [?ALICE]).

config_add_node2(Config) ->
    config_add_node(Config, ?NODE2, ?NODE2_NAME, [?BOB]).

config_add_node3(Config) ->
    config_add_node(Config, ?NODE3, ?NODE3_NAME, [?LISA]).

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

start_two_child_nodes(Config) ->
    [{Node1, NodeName1, Stakers1}, {Node2, NodeName2, Stakers2} | _] = ?config(nodes, Config),
    Env = [{"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(?config(network_id, Config))}],
    child_node_config(Node1, Stakers1, Config),
    aecore_suite_utils:start_node(Node1, Config, Env),
    aecore_suite_utils:connect(NodeName1, []),
    child_node_config(Node2, Stakers2, Config),
    aecore_suite_utils:start_node(Node2, Config, Env),
    aecore_suite_utils:connect(NodeName2, []),
    ok.

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
    [{Node, _, _} | _] = ?config(nodes, Config),
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

%%% --------- pinning helpers

%%wait_for_ps(Event) ->
%%    receive
%%        {gproc_ps_event, Event, Info} -> {ok, Info};
%%        Other -> error({wrong_signal, Other})
%%    end.

%%mine_to_last_block_in_epoch(Node, Config) ->
%%    {ok, #{
%%        epoch := _Epoch,
%%        first := _First,
%%        last := Last,
%%        length := _Length
%%    }} = rpc(Node, aec_chain_hc, epoch_info, []),
%%    CH = rpc(Node, aec_chain, top_height, []),
%%    DistToBeforeLast = Last - CH - 1,
%%    {ok, _} = produce_cc_blocks(Config, #{count => DistToBeforeLast}).

% PINREFAC
%%pin_contract_call_tx(Config, Fun, Args, Amount, FromPubKey) ->
%%    ContractPubkey = ?config(election_contract, Config),
%%    Nonce = next_nonce(?NODE1, FromPubKey),
%%    {ok, CallData} = aeb_fate_abi:create_calldata(Fun, Args),
%%    ABI = aect_test_utils:abi_version(),
%%    TxSpec =
%%        #{
%%            caller_id => aeser_id:create(account, FromPubKey),
%%            nonce => Nonce,
%%            contract_id => aeser_id:create(contract, ContractPubkey),
%%            abi_version => ABI,
%%            fee => 1000000 * ?DEFAULT_GAS_PRICE,
%%            amount => Amount,
%%            gas => 1000000,
%%            gas_price => ?DEFAULT_GAS_PRICE,
%%            call_data => CallData
%%        },
%%    {ok, Tx} = aect_call_tx:new(TxSpec),
%%    NetworkId = ?config(network_id, Config),
%%    SignedTx = sign_tx(Tx, privkey(who_by_pubkey(FromPubKey)), NetworkId),
%%    aecore_suite_utils:rpc(?NODE1_NAME, aec_tx_pool, push, [SignedTx, tx_received]),
%%    ok.

% PINREFAC aec_parent_connector??
%%pin_to_parent(Node, PinningData, AccountPK) ->
%%    AccPKEncEnc = aeser_api_encoder:encode(account_pubkey, AccountPK),
%%    % no pending transactions
%%    {ok, []} = rpc(?PARENT_CHAIN_NODE, aec_tx_pool, peek, [infinity]),
%%    PinTx = rpc(Node, aec_parent_connector, create_pin_tx, [
%%        AccPKEncEnc, AccountPK, 1, 30000 * ?DEFAULT_GAS_PRICE, PinningData
%%    ]),
%%    SignedPinTx = sign_tx(PinTx, privkey(?DWIGHT), ?PARENT_CHAIN_NETWORK_ID),
%%    rpc(Node, aec_parent_connector, post_pin_tx, [SignedPinTx]).

% PINREFAC
%%tx_hash_to_child(Node, EncTxHash, SendAccount, Leader, Config) ->
%%    NodeName = aecore_suite_utils:node_name(Node),
%%    NetworkId = ?config(network_id, Config),
%%    Nonce = next_nonce(Node, pubkey(SendAccount)),
%%    Params = #{
%%        sender_id => aeser_id:create(account, pubkey(SendAccount)),
%%        recipient_id => aeser_id:create(account, Leader),
%%        amount => 1,
%%        fee => 30000 * ?DEFAULT_GAS_PRICE,
%%        nonce => Nonce,
%%        payload => EncTxHash
%%    },
%%    ct:log("Preparing a spend tx: ~p", [Params]),
%%    {ok, Tx} = aec_spend_tx:new(Params),
%%    SignedTx = sign_tx(Tx, privkey(SendAccount), NetworkId),
%%    ok = aecore_suite_utils:rpc(NodeName, aec_tx_pool, push, [SignedTx, tx_received]),
%%    Hash = aecore_suite_utils:rpc(NodeName, aetx_sign, hash, [SignedTx]),
%%    Hash.

%%mine_to_next_epoch(Node, Config) ->
%%    Height1 = rpc(Node, aec_chain, top_height, []),
%%    {ok, #{last := Last1, length := _Len}} = rpc(Node, aec_chain_hc, epoch_info, []),
%%    {ok, Bs} = produce_cc_blocks(Config, #{count => Last1 - Height1 + 1}),
%%    ct:log("Block last epoch: ~p", [Bs]).

%%% --------- helper functions

pubkey({Pubkey, _, _}) -> Pubkey.

privkey({_, Privkey, _}) -> Privkey.

%%name({_, _, Name}) -> Name.

%%who_by_pubkey(Pubkey) ->
%%    Alice = pubkey(?ALICE),
%%    Bob = pubkey(?BOB),
%%    Lisa = pubkey(?LISA),
%%    Dwight = pubkey(?DWIGHT),
%%    Edwin = pubkey(?EDWIN),
%%    Genesis = ?GENESIS_BENEFICIARY,
%%    case Pubkey of
%%        Alice -> ?ALICE;
%%        Bob -> ?BOB;
%%        Lisa -> ?LISA;
%%        Dwight -> ?DWIGHT;
%%        Edwin -> ?EDWIN;
%%        Genesis -> genesis;
%%        _ -> error(unknown_beneficiary)
%%    end.

encoded_pubkey(Who) ->
    aeser_api_encoder:encode(account_pubkey, pubkey(Who)).

next_nonce(Node, Pubkey) ->
    case aecore_suite_utils:rpc(Node, aec_next_nonce, pick_for_account, [Pubkey, max]) of
        {ok, NextNonce} -> NextNonce;
        {error, account_not_found} -> 1
    end.

%%sign_and_push(NodeName, Tx, Who, NetworkId) ->
%%    SignedTx = sign_tx(Tx, privkey(Who), NetworkId),
%%    ok = aecore_suite_utils:rpc(NodeName, aec_tx_pool, push, [SignedTx, tx_received]),
%%    SignedTx.

%% usually we would use aec_test_utils:sign_tx/3. This function is being
%% executed in the context of the CT test and uses the corresponding
%% network_id. Since the network_id of the HC node is different, we must sign
%% the tx using the test-specific network_id
%%sign_tx(Tx, Privkey, NetworkId) ->
%%    Bin0 = aetx:serialize_to_binary(Tx),
%%    %% since we are in CERES context, we sign th hash
%%    Bin = aec_hash:hash(signed_tx, Bin0),
%%    BinForNetwork = <<NetworkId/binary, Bin/binary>>,
%%    Signatures = [enacl:sign_detached(BinForNetwork, Privkey)],
%%    aetx_sign:new(Tx, Signatures).

%%seed_account(RecpipientPubkey, Amount, NetworkId) ->
%%    seed_account(?NODE1, RecpipientPubkey, Amount, NetworkId).

%%seed_account(Node, RecipientPubkey, Amount, NetworkId) ->
%%    NodeName = aecore_suite_utils:node_name(Node),
%%    {PatronPriv, PatronPub} = aecore_suite_utils:sign_keys(Node),
%%    Nonce = next_nonce(Node, PatronPub),
%%    Params =
%%        #{
%%            sender_id => aeser_id:create(account, PatronPub),
%%            recipient_id => aeser_id:create(account, RecipientPubkey),
%%            amount => Amount,
%%            fee => 30000 * ?DEFAULT_GAS_PRICE,
%%            nonce => Nonce,
%%            payload => <<>>
%%        },
%%    ct:log("Preparing a spend tx: ~p", [Params]),
%%    {ok, Tx} = aec_spend_tx:new(Params),
%%    SignedTx = sign_tx(Tx, PatronPriv, NetworkId),
%%    ok = aecore_suite_utils:rpc(NodeName, aec_tx_pool, push, [SignedTx, tx_received]),
%%    {ok, SignedTx}.

%%account_balance(Pubkey) ->
%%    case rpc(?NODE1, aec_chain, get_account, [Pubkey]) of
%%        {value, Account} -> aec_accounts:balance(Account);
%%        none -> no_such_account
%%    end.

%%inspect_staking_contract(OriginWho, WhatToInspect, Config) ->
%%    TopHash = rpc(?NODE1, aec_chain, top_block_hash, []),
%%    inspect_staking_contract(OriginWho, WhatToInspect, Config, TopHash).

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

%%call_info(SignedTx) ->
%%    Hash = aetx_sign:hash(SignedTx),
%%    case aecore_suite_utils:rpc(?NODE1_NAME, aec_chain, find_tx_location, [Hash]) of
%%        not_found ->
%%            {error, unknown_tx};
%%        none ->
%%            {error, gced_tx};
%%        mempool ->
%%            {error, tx_in_pool};
%%        MBHash when is_binary(MBHash) ->
%%            case
%%                aecore_suite_utils:rpc(
%%                    ?NODE1_NAME,
%%                    aehttp_helpers,
%%                    get_info_object_signed_tx,
%%                    [MBHash, SignedTx]
%%                )
%%            of
%%                {ok, Call} -> {ok, Call};
%%                {error, Reason} -> {error, Reason}
%%            end
%%    end.

%%create_ae_spend_tx(SenderId, RecipientId, Nonce, Payload) ->
%%    Params = #{
%%        sender_id => aeser_id:create(account, SenderId),
%%        recipient_id => aeser_id:create(account, RecipientId),
%%        amount => 1,
%%        nonce => Nonce,
%%        fee => 40000 * ?DEFAULT_GAS_PRICE,
%%        payload => Payload
%%    },
%%    ct:log("Preparing a spend tx: ~p", [Params]),
%%    aec_spend_tx:new(Params).

%%external_address(Node) ->
%%    {ok, Port} = rpc(
%%        Node,
%%        aeu_env,
%%        user_config_or_env,
%%        [[<<"http">>, <<"external">>, <<"port">>], aehttp, [external, port]]
%%    ),
%%    "http://127.0.0.1:" ++ integer_to_list(Port).

decode_consensus_result(Call, Fun, Src) ->
    ReturnType = aect_call:return_type(Call),
    ReturnValue = aect_call:return_value(Call),
    Res = aect_test_utils:decode_call_result(Src, Fun, ReturnType, ReturnValue),
    {ReturnType, Res}.

%%calc_rewards(RewardForHeight) ->
%%    %% we distribute rewards for the previous
%%    {ok, #{
%%        key_block := PrevKB,
%%        micro_blocks := MBs
%%    }} =
%%        rpc(
%%            ?NODE1,
%%            aec_chain,
%%            get_generation_by_height,
%%            [RewardForHeight, backward]
%%        ),
%%    PrevGenProtocol = aec_blocks:version(PrevKB),
%%    Txs = lists:flatten(
%%        lists:map(
%%            fun(MB) -> aec_blocks:txs(MB) end,
%%            MBs
%%        )
%%    ),
%%    ct:log("Txs: ~p", [Txs]),
%%    {_, KeyReward} = key_reward_provided(RewardForHeight),
%%    GenerationFees =
%%        lists:foldl(
%%            fun(SignTx, Accum) ->
%%                %% TODO: maybe add support for contract calls:
%%                %% * contract create
%%                %% * contract call
%%                %% * force progress
%%                %% * meta tx
%%                Tx = aetx_sign:tx(SignTx),
%%                Fee = aetx:fee(Tx),
%%                Accum + Fee
%%            end,
%%            0,
%%            Txs
%%        ),
%%    ct:log(
%%        "Height ~p, Generation fees: ~p, key reward: ~p",
%%        [RewardForHeight, GenerationFees, KeyReward]
%%    ),
%%    BeneficiaryReward1 = GenerationFees * 4 div 10,
%%    BeneficiaryReward2 = GenerationFees - BeneficiaryReward1 + KeyReward,
%%    %% TODO: verify devrewards
%%    {{AdjustedReward1, AdjustedReward2}, _DevRewards} =
%%        Res =
%%        rpc(
%%            ?NODE1,
%%            aec_dev_reward,
%%            split,
%%            [BeneficiaryReward1, BeneficiaryReward2, PrevGenProtocol]
%%        ),
%%    ct:log(
%%        "AdjustedReward1: ~p, AdjustedReward2: ~p",
%%        [AdjustedReward1, AdjustedReward2]
%%    ),
%%    Res.

src(ContractName, Config) ->
    Srcs = ?config(contract_src, Config),
    maps:get(ContractName, Srcs).

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
        Call11
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
                        },
                    <<"producing_commitments">> => ProducingCommitments
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
                                        <<"pinning_reward_value">> => 4711
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

validator_pool_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 1).

staking_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 2).

election_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 3).

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
    skip_list => [node()]
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

create_stub(ContractFile) ->
    create_stub(ContractFile, []).

create_stub(ContractFile, Opts) ->
    {ok, Enc} = aeso_aci:contract_interface(json, ContractFile, Opts ++ [{no_code, true}]),
    {ok, Stub} = aeso_aci:render_aci_json(Enc),
    binary_to_list(Stub).

spread(_, _, []) ->
    [];
spread(0, TopHeight, Spread) ->
    [{CH, N} || {CH, N} <- Spread, N /= 0, CH > TopHeight];
spread(N, TopHeight, Spread) when N rem 2 == 0 ->
    {Left, Right} = lists:split(length(Spread) div 2, Spread),
    spread(N div 2, TopHeight, Left) ++ spread(N div 2, TopHeight, Right);
spread(N, TopHeight, Spread) when N rem 2 == 1 ->
    {Left, [{Middle, K} | Right]} = lists:split(length(Spread) div 2, Spread),
    spread(N div 2, TopHeight, Left) ++ [{Middle, K + 1} || Middle > TopHeight] ++
        spread(N div 2, TopHeight, Right).

%%get_entropy(Node, Epoch) ->
%%    ParentHeight = rpc(Node, aec_consensus_hc, entropy_height, [Epoch]),
%%    {ok, WPHdr} = rpc(?PARENT_CHAIN_NODE, aec_chain, get_key_header_by_height, [ParentHeight]),
%%    {ok, WPHash0} = aec_headers:hash_header(WPHdr),
%%    {ParentHeight, aeser_api_encoder:encode(key_block_hash, WPHash0)}.

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
    [Node || {Node, _, _} <- proplists:get_value(nodes, Config), not lists:member(Node, SkipNodes)].

-spec get_nodes_and_nodenames(Config :: proplists:proplist(), SkipNodes :: [node()]) ->
    {[node()], [node()]}.
get_nodes_and_nodenames(Config, SkipNodes) ->
    NN = [
        {Node, NodeName}
     || {Node, NodeName, _} <- proplists:get_value(nodes, Config, []),
        not lists:member(Node, SkipNodes)
    ],
    lists:unzip(NN).

get_height(Node) ->
    aecore_suite_utils:rpc(Node, aec_chain, top_height, []).

accounts_countains_pubkey(Accounts, Pubkey) ->
    lists:any(fun({Pub, _Priv, _AccountName}) -> Pub =:= Pubkey end, Accounts).

get_node_with_pubkey(Pubkey, Config) ->
    NodeTriples = ?config(nodes, Config),
    Nodes = [Node || {Node, _, _Accounts} <- NodeTriples],
    case
        [Node || {Node, _, Accounts} <- NodeTriples, accounts_countains_pubkey(Accounts, Pubkey)]
    of
        [] -> ct:fail("Can't find node with pubkey=~0p nodes=~0p", [Pubkey, Nodes]);
        [One] -> One;
        _Many -> ct:fail("Found multiple nodes with pubkey=~0p nodes=~0p", [Pubkey, Nodes])
    end.

get_cc_height_and_leader(Config) ->
    {Node1, _, _} = hd(proplists:get_value(nodes, Config)),
    Height = get_height(Node1),
    {ok, LeaderPubkey} = aecore_suite_utils:rpc(Node1, aec_consensus_hc, leader_for_height, [
        Height
    ]),
    Leader = get_node_with_pubkey(LeaderPubkey, Config),
    {Height, Leader}.
