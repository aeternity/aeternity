-module(aest_channels_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_suite/1]).

% Test cases
-export([
    test_simple_same_node_channel/1,
    test_simple_different_nodes_channel/1,
    test_compat_with_initiator_node_using_latest_stable_version/1,
    test_compat_with_responder_node_using_latest_stable_version/1,
    on_chain_channel/1
]).

%% Helpers
-export([
    create_state_channel_perform_operations_leave/2,
    reestablish_state_channel_perform_operations_close/3
]).

-import(aest_nodes, [
    spec/3,
    setup_nodes/2,
    start_node/2,
    wait_for_value/4,
    wait_for_startup/3,
    post_spend_tx/5
]).

-import(aest_api, [
    sc_open/2,
    sc_withdraw/3,
    sc_close_mutual/2,
    sc_transfer/3,
    sc_deploy_contract/4,
    sc_call_contract/4,
    sc_leave/1,
    sc_reestablish/5,
    sc_wait_close/1
]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(MINING_TIMEOUT,   3000).
-define(SYNC_TIMEOUT,      100).
-define(MIN_DEPTH, 4).

-define(MIKE, #{
    pubkey => <<200,171,93,11,3,93,177,65,197,27,123,127,177,165,
                190,211,20,112,79,108,85,78,88,181,26,207,191,211,
                40,225,138,154>>,
    privkey => <<237,12,20,128,115,166,32,106,220,142,111,97,141,104,201,130,56,
                 100,64,142,139,163,87,166,185,94,4,159,217,243,160,169,200,171,
                 93,11,3,93,177,65,197,27,123,127,177,165,190,211,20,112,79,108,
                 85,78,88,181,26,207,191,211,40,225,138,154>>
}).

-define(ALICE, #{
    pubkey => <<177,181,119,188,211,39,203,57,229,94,108,2,107,214, 167,74,27,
                53,222,108,6,80,196,174,81,239,171,117,158,65,91,102>>,
    privkey => <<145,69,14,254,5,22,194,68,118,57,0,134,66,96,8,20,124,253,238,
                 207,230,147,95,173,161,192,86,195,165,186,115,251,177,181,119,
                 188,211,39,203,57,229,94,108,2,107,214,167,74,27,53,222,108,6,
                 80,196,174,81,239,171,117,158,65,91,102>>
}).

-define(BOB, #{
    pubkey => <<103,28,85,70,70,73,69,117,178,180,148,246,81,104,
                33,113,6,99,216,72,147,205,210,210,54,3,122,84,195,
                62,238,132>>,
    privkey => <<59,130,10,50,47,94,36,188,50,163,253,39,81,120,89,219,72,88,68,
                 154,183,225,78,92,9,216,215,59,108,82,203,25,103,28,85,70,70,
                 73,69,117,178,180,148,246,81,104,33,113,6,99,216,72,147,205,
                 210,210,54,3,122,84,195,62,238,132>>
}).

-define(NODE1, #{
    name    => node1,
    peers   => [],
    backend => aest_docker,
    source  => {pull, "aeternity/aeternity:local"}
}).

-define(NODE2, #{
    name    => node2,
    peers   => [node1],
    backend => aest_docker,
    source  => {pull, "aeternity/aeternity:local"},
    mining => #{autostart => false}
}).

%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
    test_simple_same_node_channel,
    test_simple_different_nodes_channel,
    %% latest stable version expects not pinned block_hash
    %% uncomment when updated
    %%test_compat_with_initiator_node_using_latest_stable_version,
    %%test_compat_with_responder_node_using_latest_stable_version,
    on_chain_channel
].

init_per_suite(Config) ->
    Config1 = [
        {node_startup_time, 20000}, %% Time may take to get the node to respond to http
        {node_shutdown_time, 20000}, %% Time it may take to stop node cleanly
        %% FIXME: Remove this when this is fixed:
        %%   https://www.pivotaltracker.com/n/projects/2124891/stories/159293763
        {verify_logs, false}
    | Config],

    %% Precompile a simple contract for testing
    precompile_identity_contract(Config1).

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_suite(_Config) -> ok.

%=== TEST CASES ================================================================

channel_opts(INode, RNode) ->
    #{ initiator_node => INode
     , initiator_id   => ?BOB
     , initiator_amount => 50000 * aest_nodes:gas_price()
     , responder_node => RNode
     , responder_id => ?ALICE
     , responder_amount => 50000 * aest_nodes:gas_price()
     , push_amount => 2
     , bh_delta_not_newer_than => 0
     , bh_delta_not_older_than => 100
     , bh_delta_pick           => 10
    }.

test_simple_same_node_channel(Cfg) ->
    ChannelOpts = channel_opts(node1, node1),
    simple_channel_test(ChannelOpts, #{}, #{}, Cfg).

test_simple_different_nodes_channel(Cfg) ->
    test_different_nodes_channel_(#{}, #{}, Cfg).

test_compat_with_initiator_node_using_latest_stable_version(Cfg) ->
    test_different_nodes_channel_(set_genesis_accounts(node_base_spec_with_latest_stable_version()),
                                  set_genesis_accounts(#{}),
                                  Cfg).

test_compat_with_responder_node_using_latest_stable_version(Cfg) ->
    test_different_nodes_channel_(set_genesis_accounts(#{}),
                                  set_genesis_accounts(node_base_spec_with_latest_stable_version()),
                                  Cfg).

test_different_nodes_channel_(InitiatorNodeBaseSpec, ResponderNodeBaseSpec, Cfg) ->
    ChannelOpts = channel_opts(node1, node2),
    simple_channel_test(ChannelOpts, InitiatorNodeBaseSpec, ResponderNodeBaseSpec, Cfg).

simple_channel_test(ChannelOpts, InitiatorNodeBaseSpec, ResponderNodeBaseSpec, Cfg) ->
    #{
        initiator_node   := INodeName,
        initiator_id     := IAccount,
        initiator_amount := IAmt,
        responder_node   := RNodeName,
        responder_id     := RAccount,
        responder_amount := RAmt,
        push_amount      := PushAmount,
        bh_delta_not_newer_than := _,
        bh_delta_not_older_than := _,
        bh_delta_pick           := _
    } = ChannelOpts,

    MikePubkey = aeser_api_encoder:encode(account_pubkey, maps:get(pubkey, ?MIKE)),
    NodeConfig = #{ beneficiary => MikePubkey },
    StrictMining = #{strictly_follow_top => true},
    setup([spec(node1, [], InitiatorNodeBaseSpec#{mining => StrictMining}),
           spec(node2, [node1], ResponderNodeBaseSpec#{mining => StrictMining#{autostart => false}})],
          NodeConfig, Cfg),
    NodeNames = [INodeName, RNodeName],
    start_node(node1, Cfg),
    start_node(node2, Cfg),
    wait_for_startup([node1, node2], 4, Cfg),  %% make sure there is some money in accounts
    wait_for_value({balance, maps:get(pubkey, ?MIKE), 1000000}, [node1], 10000, Cfg),

    populate_accounts_with_funds([IAccount, RAccount], NodeNames, Cfg),
    {ok, Chan, OpenFee, WFee1, WFee2} = test_open_and_onchain_operations(ChannelOpts, Cfg),
    test_offchain_operations(Chan, Cfg),

    {ok, LatestState} = sc_leave(Chan),
    {ok, Chan1} = sc_reestablish(Chan, INodeName, RNodeName, LatestState, Cfg),

    test_offchain_operations(Chan1, Cfg),

    ct:log("Testing mutual close"),
    {ok, CloseTxHash, CloseFee, IChange, RChange} = sc_close_mutual(Chan1, initiator),
    wait_for_value({txs_on_chain, [CloseTxHash]}, NodeNames, 5000, Cfg),

    ISplitCloseFee = trunc(math:ceil(CloseFee / 2)),
    RSplitCloseFee = trunc(math:floor(CloseFee / 2)),
    ct:log("IAmt = ~p~n"
           "RAmt = ~p~n"
           "Gas price      = ~p~n"
           "ISplitCloseFee = ~p~n"
           "RSplitCloseFee = ~p~n"
           "PushAmount     = ~p~n"
           "IChange        = ~p~n"
           "RChange        = ~p~n", [IAmt, RAmt, aest_nodes:gas_price(),
                                     ISplitCloseFee, RSplitCloseFee, PushAmount, IChange, RChange]),
    ?assertEqual(IAmt - 20 * aest_nodes:gas_price() - ISplitCloseFee - PushAmount, IChange),
    ?assertEqual(RAmt - 50 * aest_nodes:gas_price() - RSplitCloseFee + PushAmount, RChange),

    wait_for_value({txs_on_chain, [CloseTxHash]}, NodeNames, 5000, Cfg),
    wait_for_value({balance, maps:get(pubkey, IAccount), 200000 - IAmt - OpenFee + 20 - WFee1 + IChange}, NodeNames, 10000, Cfg),
    wait_for_value({balance, maps:get(pubkey, RAccount), 200000 - RAmt + 50 - WFee2 + RChange}, NodeNames, 10000, Cfg),
    ok.


%=== HELPERS ===================================================================

precompile_identity_contract(Config1) ->
    %% Precompile a simple contract for testing
    Config2 = aect_test_utils:init_per_group(aevm, Config1),
    aect_test_utils:setup_testcase(Config2),
    SimpleContractName = "identity",
    {ok, BinSrc} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), SimpleContractName),
    {ok, Code}   = aect_test_utils:compile_contract(aect_test_utils:sophia_version(), SimpleContractName),

    [{simple_contract,
        #{ bytecode => aeser_api_encoder:encode(contract_bytearray, Code),
           vm       => aect_test_utils:vm_version(),
           abi      => aect_test_utils:abi_version(),
           code     => Code,
           src      => binary_to_list(BinSrc)
        }
     } | Config2].

create_state_channel_perform_operations_leave({INodeName, RNodeName}, Config) ->
    Config1 = precompile_identity_contract(Config),
    ChannelOpts = #{
        initiator_node => INodeName,
        initiator_id   => ?ALICE,
        initiator_amount => 50000 * aest_nodes:gas_price(),
        responder_node => RNodeName,
        responder_id => ?BOB,
        responder_amount => 50000 * aest_nodes:gas_price(),
        push_amount => 2,
        bh_delta_not_newer_than => 0,
        bh_delta_not_older_than => 100,
        bh_delta_pick           => 10
    },
    IAccount = maps:get(initiator_id, ChannelOpts),
    RAccount = maps:get(responder_id, ChannelOpts),
    NodeNames = [INodeName, RNodeName],

    populate_accounts_with_funds([IAccount, RAccount], NodeNames, Config1),
    {ok, Chan, _OpenFee, _WFee1, _WFee2} = test_open_and_onchain_operations(ChannelOpts, Config1),
    test_offchain_operations(Chan, Config1),

    {ok, LatestState} = sc_leave(Chan),

    #{config => Config1, latest_state => LatestState, channel => Chan}.

reestablish_state_channel_perform_operations_close({INodeName, RNodeName},
    #{ config := Config
     , latest_state := LatestState
     , channel := Chan
    }, _Config) ->
    NodeNames = [INodeName, RNodeName],

    {ok, Chan1} = sc_reestablish(Chan, INodeName, RNodeName, LatestState, Config),
    test_offchain_operations(Chan1, Config),

    ct:log("Testing mutual close"),
    {ok, CloseTxHash, _CloseFee, _IChange, _RChange} = sc_close_mutual(Chan1, initiator),
    wait_for_value({txs_on_chain, [CloseTxHash]}, NodeNames, 5000, Config),
    %% wait for min depth to be reached so the channels die
    #{height := TopHeight} = aest_nodes:get_top(INodeName),
    KeyBlocksToMine = ?MIN_DEPTH + 2, % min depth is 4
    wait_for_value({height, TopHeight + KeyBlocksToMine}, NodeNames, 10000, Config),
    sc_wait_close(Chan1),
    ok.

%=== INTERNAL FUNCTIONS ========================================================

setup(NodeSpecs, Config, Cfg) ->
    setup_nodes([maps:put(config, Config, N) || N <- NodeSpecs], Cfg).

on_chain_channel(Cfg) ->
    MikePubkey = aeser_api_encoder:encode(account_pubkey, maps:get(pubkey, ?MIKE)),
    NodeConfig = #{ beneficiary => MikePubkey },
    setup([?NODE1], NodeConfig, Cfg),
    NodeNames = [node1],
    start_node(node1, Cfg),
    wait_for_startup([node1], 4, Cfg),  %% make sure ?MIKE has some money
    #{tx_hash := Hash1} = aest_nodes:post_spend_tx(node1, ?MIKE, ?BOB, 1, #{amount => 400000 * aest_nodes:gas_price()}),
    #{tx_hash := Hash2} = aest_nodes:post_spend_tx(node1, ?MIKE, ?ALICE, 2, #{amount => 400000 * aest_nodes:gas_price()}),
    aest_nodes:wait_for_value({txs_on_chain, [Hash1, Hash2]}, NodeNames, 10000, []),
    wait_for_value({balance, maps:get(pubkey, ?BOB), 100}, NodeNames, 5000, []),
    wait_for_value({balance, maps:get(pubkey, ?ALICE), 100}, NodeNames, 5000, []),

    #{tx_hash := CreateHash, channel_id := ChannelId} =
        aest_nodes:post_create_state_channel_tx(node1, ?BOB, ?ALICE, #{ nonce => 1 }),
    aest_nodes:wait_for_value({txs_on_chain, [CreateHash]}, NodeNames, 10000, []),

    #{tx_hash := DepositHash} =
        aest_nodes:post_deposit_state_channel_tx(node1, ?BOB, ?ALICE, ChannelId, #{ nonce => 2, amount => 20 * aest_nodes:gas_price(), round => 2 }),
    aest_nodes:wait_for_value({txs_on_chain, [DepositHash]}, NodeNames, 10000, []),

    #{tx_hash := WithdrawHash} =
        aest_nodes:post_withdraw_state_channel_tx(node1, ?ALICE, ?BOB, ChannelId, #{ nonce => 1, amount => 20 * aest_nodes:gas_price(), round => 3 }),
    aest_nodes:wait_for_value({txs_on_chain, [WithdrawHash]}, NodeNames, 10000, []),


    #{tx_hash := CloseHash} =
        aest_nodes:post_close_mutual_state_channel_tx(node1, ?BOB, ?ALICE, ChannelId,
                                                      #{ nonce => 3, fee => 20000 * aest_nodes:gas_price(),
                                                         initiator_amount_final => 59 * aest_nodes:gas_price(),
                                                         responder_amount_final => 100 * aest_nodes:gas_price() }),
    aest_nodes:wait_for_value({txs_on_chain, [CloseHash]}, NodeNames, 10000, []),

    wait_for_value({balance, maps:get(pubkey, ?BOB), 100}, NodeNames, 5000, []),
    wait_for_value({balance, maps:get(pubkey, ?ALICE), 100}, NodeNames, 5000, []).

node_base_spec_with_latest_stable_version() ->
    #{source => {pull, "aeternity/aeternity:latest"}}.

set_genesis_accounts(Spec) ->
    PatronAddress = aeser_api_encoder:encode(account_pubkey,
                                              maps:get(pubkey, ?MIKE)),
    %% have all nodes share the same accounts_test.json
    GenesisAccounts = [{PatronAddress, 123400000000000000000000000000}],
    Spec#{genesis_accounts => GenesisAccounts}.

encode_calldata(Contract, Fun, Args) ->
    {ok, Calldata} = aect_test_utils:encode_call_data(maps:get(src, Contract), Fun, Args),
    {ok, aeser_api_encoder:encode(contract_bytearray, Calldata)}.

test_open_and_onchain_operations(#{
        initiator_node   := INodeName,
        initiator_id     := IAccount,
        initiator_amount := IAmt,
        responder_node   := RNodeName,
        responder_id     := RAccount,
        responder_amount := RAmt,
        bh_delta_not_newer_than := _,
        bh_delta_not_older_than := _,
        bh_delta_pick           := _
    } = ChannelOpts,  Cfg) ->
    NodeNames = [INodeName, RNodeName],

    ct:log("Opening channel"),
    {ok, Chan, TxHash, OpenFee} = sc_open(ChannelOpts, Cfg),
    wait_for_value({txs_on_chain, [TxHash]}, NodeNames, 5000, Cfg),
    wait_for_value({balance, maps:get(pubkey, IAccount), 200000 * aest_nodes:gas_price() - IAmt - OpenFee}, NodeNames, 10000, Cfg),
    wait_for_value({balance, maps:get(pubkey, RAccount), 200000 * aest_nodes:gas_price() - RAmt}, NodeNames, 10000, Cfg),

    ct:log("Testing withdraws"),
    {ok, TxHash1, WFee1} = sc_withdraw(Chan, initiator, 20 * aest_nodes:gas_price()),
    wait_for_value({txs_on_chain, [TxHash1]}, NodeNames, 5000, Cfg),
    wait_for_value({balance, maps:get(pubkey, IAccount), 200000 * aest_nodes:gas_price() - IAmt - OpenFee + 20 - WFee1}, NodeNames, 10000, Cfg),
    wait_for_value({balance, maps:get(pubkey, RAccount), 200000 * aest_nodes:gas_price() - RAmt}, NodeNames, 10000, Cfg),

    {ok, TxHash2, WFee2} = sc_withdraw(Chan, responder, 50 * aest_nodes:gas_price()),
    wait_for_value({txs_on_chain, [TxHash2]}, NodeNames, 5000, Cfg),
    wait_for_value({balance, maps:get(pubkey, IAccount), 200000 * aest_nodes:gas_price() - IAmt - OpenFee + 20 * aest_nodes:gas_price() - WFee1}, NodeNames, 10000, Cfg),
    wait_for_value({balance, maps:get(pubkey, RAccount), 200000 * aest_nodes:gas_price() - RAmt + 50 * aest_nodes:gas_price() - WFee2}, NodeNames, 10000, Cfg),
    {ok, Chan, OpenFee, WFee1, WFee2}.

test_offchain_operations(Chan, Cfg) ->
    ct:log("Testing offchain transfers"),
    TransferVolleyF = fun() ->
        ok = sc_transfer(Chan, initiator, 1 * aest_nodes:gas_price()),
        ok = sc_transfer(Chan, responder, 1 * aest_nodes:gas_price())
    end,
    [ TransferVolleyF() || _ <- lists:seq(1,4) ],

    ct:log("Testing simple contract deployment and calls"),
    SimpleContractTestF = fun(Who) ->
        SimpleContract = proplists:get_value(simple_contract, Cfg),
        {ok, CallData} = encode_calldata(SimpleContract, "init", []),
        {ok, SimpleContract1} = sc_deploy_contract(Chan, Who, SimpleContract, CallData),
        {ok, CallData1} = encode_calldata(SimpleContract, "main", ["42"]),
        {ok, CallRes} = sc_call_contract(Chan, Who, SimpleContract1, CallData1),
        #{ <<"return_type">>       := <<"ok">>
         , <<"return_value">>      := _} = CallRes %% TODO: check if return value matches
    end,
    [ SimpleContractTestF(Role) || Role <- [initiator, responder]].

populate_accounts_with_funds(Accounts, [Node | _] = NodeNames, Cfg) ->
    lists:foldl(fun(Account, Nonce) ->
        post_spend_tx(Node, ?MIKE, Account, Nonce, #{amount => 200000 * aest_nodes:gas_price()}),
        wait_for_value({balance, maps:get(pubkey, Account), 200000 * aest_nodes:gas_price()}, NodeNames, 10000, Cfg),
        Nonce + 1
    end, 1, Accounts).
