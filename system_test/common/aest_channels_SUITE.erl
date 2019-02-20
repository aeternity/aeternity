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
    on_chain_channel/1
]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    wait_for_value/4,
    wait_for_startup/3,
    post_spend_tx/5
]).

-import(aest_api, [
    sc_open/2,
    sc_withdraw/3,
    sc_close_mutual/2
]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(MINING_TIMEOUT,   3000).
-define(SYNC_TIMEOUT,      100).

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
    source  => {pull, "aeternity/aeternity:local"}
}).

%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
    test_simple_same_node_channel,
    test_simple_different_nodes_channel,
    on_chain_channel
].

init_per_suite(Config) ->
    [
        {node_startup_time, 20000}, %% Time may take to get the node to respond to http
        {node_shutdown_time, 20000}, %% Time it may take to stop node cleanly
        %% FIXME: Remove this when this is fixed:
        %%   https://www.pivotaltracker.com/n/projects/2124891/stories/159293763
        {verify_logs, false}
    | Config].

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_suite(_Config) -> ok.

%=== TEST CASES ================================================================

test_simple_same_node_channel(Cfg) ->
    ChannelOpts = #{
        initiator_node => node1,
        initiator_id => ?BOB,
        initiator_amount => 50000 * aest_nodes:gas_price(),
        responder_node => node1,
        responder_id => ?ALICE,
        responder_amount => 50000 * aest_nodes:gas_price(),
        push_amount => 2 * aest_nodes:gas_price()
    },
    simple_channel_test(ChannelOpts, Cfg).

test_simple_different_nodes_channel(Cfg) ->
    ChannelOpts = #{
        initiator_node => node1,
        initiator_id   => ?BOB,
        initiator_amount => 50000 * aest_nodes:gas_price(),
        responder_node => node2,
        responder_id => ?ALICE,
        responder_amount => 50000 * aest_nodes:gas_price(),
        push_amount => 2
    },
    simple_channel_test(ChannelOpts, Cfg).

simple_channel_test(ChannelOpts, Cfg) ->
    #{
        initiator_id     := IAccount,
        initiator_amount := IAmt,
        responder_id     := RAccount,
        responder_amount := RAmt,
        push_amount      := PushAmount
    } = ChannelOpts,

    MikePubkey = aehttp_api_encoder:encode(account_pubkey, maps:get(pubkey, ?MIKE)),
    NodeConfig = #{ beneficiary => MikePubkey },
    setup([?NODE1, ?NODE2], NodeConfig, Cfg),
    NodeNames = [node1, node2],
    start_node(node1, Cfg),
    start_node(node2, Cfg),
    wait_for_startup([node1, node2], 4, Cfg),  %% make sure there is some money in accounts
    wait_for_value({balance, maps:get(pubkey, ?MIKE), 1000000}, [node1], 10000, Cfg),

    post_spend_tx(node1, ?MIKE, IAccount, 1, #{amount => 200000 * aest_nodes:gas_price()}),
    wait_for_value({balance, maps:get(pubkey, IAccount), 200000 * aest_nodes:gas_price()}, NodeNames, 10000, Cfg),

    post_spend_tx(node1, ?MIKE, RAccount, 2, #{amount => 200000 * aest_nodes:gas_price()}),
    wait_for_value({balance, maps:get(pubkey, RAccount), 200000 * aest_nodes:gas_price()}, NodeNames, 10000, Cfg),

    {ok, Chan, TxHash, OpenFee} = sc_open(ChannelOpts, Cfg),
    wait_for_value({txs_on_chain, [TxHash]}, NodeNames, 5000, Cfg),
    wait_for_value({balance, maps:get(pubkey, IAccount), 200000 * aest_nodes:gas_price() - IAmt - OpenFee}, NodeNames, 10000, Cfg),
    wait_for_value({balance, maps:get(pubkey, RAccount), 200000 * aest_nodes:gas_price() - RAmt}, NodeNames, 10000, Cfg),

    {ok, TxHash1, WFee1} = sc_withdraw(Chan, initiator, 20 * aest_nodes:gas_price()),
    wait_for_value({txs_on_chain, [TxHash1]}, NodeNames, 5000, Cfg),
    wait_for_value({balance, maps:get(pubkey, IAccount), 200000 * aest_nodes:gas_price() - IAmt - OpenFee + 20 - WFee1}, NodeNames, 10000, Cfg),
    wait_for_value({balance, maps:get(pubkey, RAccount), 200000 * aest_nodes:gas_price() - RAmt}, NodeNames, 10000, Cfg),

    {ok, TxHash2, WFee2} = sc_withdraw(Chan, responder, 50 * aest_nodes:gas_price()),
    wait_for_value({txs_on_chain, [TxHash2]}, NodeNames, 5000, Cfg),
    wait_for_value({balance, maps:get(pubkey, IAccount), 200000 * aest_nodes:gas_price() - IAmt - OpenFee + 20 * aest_nodes:gas_price() - WFee1}, NodeNames, 10000, Cfg),
    wait_for_value({balance, maps:get(pubkey, RAccount), 200000 * aest_nodes:gas_price() - RAmt + 50 * aest_nodes:gas_price() - WFee2}, NodeNames, 10000, Cfg),

    {ok, CloseTxHash, IChange, RChange} = sc_close_mutual(Chan, initiator),

    SplitOpenFee = 10000 * aest_nodes:gas_price(),
    ?assertEqual(IAmt - 20 * aest_nodes:gas_price() - SplitOpenFee - PushAmount, IChange),
    ?assertEqual(RAmt - 50 * aest_nodes:gas_price() - SplitOpenFee + PushAmount, RChange),

    wait_for_value({txs_on_chain, [CloseTxHash]}, NodeNames, 5000, Cfg),
    wait_for_value({balance, maps:get(pubkey, IAccount), 200000 - IAmt - OpenFee + 20 - WFee1 + IChange}, NodeNames, 10000, Cfg),
    wait_for_value({balance, maps:get(pubkey, RAccount), 200000 - RAmt + 50 - WFee2 + RChange}, NodeNames, 10000, Cfg),


    ok.

%=== INTERNAL FUNCTIONS ========================================================

setup(NodeSpecs, Config, Cfg) ->
    setup_nodes([maps:put(config, Config, N) || N <- NodeSpecs], Cfg).

on_chain_channel(Cfg) ->
    MikePubkey = aehttp_api_encoder:encode(account_pubkey, maps:get(pubkey, ?MIKE)),
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
