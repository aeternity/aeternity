-module(aest_mempool_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_suite/1]).

% Test cases
-export([
    test_mempool_ttl_cleanup/1,
    test_mempool_bad_nonce_cleanup/1
]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    wait_for_value/4,
    wait_for_startup/3,
    post_spend_tx/5
]).

%=== INCLUDES ==================================================================

%=== MACROS ====================================================================

-define(MINING_TIMEOUT,   3000).

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
    test_mempool_ttl_cleanup,
    test_mempool_bad_nonce_cleanup
].

init_per_suite(Config) ->
    [
        {node_startup_time, 20000}, %% Time may take to get the node to respond to http
        {node_shutdown_time, 20000}, %% Time it may take to stop node cleanly
        {gas_price, aest_nodes:gas_price()}
    | Config].

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_suite(_Config) -> ok.

%=== TEST CASES ================================================================

test_mempool_ttl_cleanup(Cfg) ->
    GasPrice = proplists:get_value(gas_price, Cfg),
    %% Setup nodes
    MikePubKey = maps:get(pubkey, ?MIKE),
    EncMikePubkey = aeser_api_encoder:encode(account_pubkey, MikePubKey),
    NodeConfig = #{ beneficiary => EncMikePubkey },
    NodeNames = [node1, node2],
    setup([?NODE1, ?NODE2], NodeConfig, Cfg),
    start_node(node1, Cfg),
    start_node(node2, Cfg),
    wait_for_startup(NodeNames, 1, Cfg),

    %% Give tokens away
    GiveAwayAmount = 400000 * GasPrice,
    aest_nodes:wait_for_value({balance, MikePubKey, 2*GiveAwayAmount}, [node1], 10000, []),
    #{ tx_hash := PostTxHash1 } = post_spend_tx(node1, ?MIKE, ?ALICE, 1, #{amount => GiveAwayAmount}),
    #{ tx_hash := PostTxHash2 } = post_spend_tx(node1, ?MIKE, ?BOB, 2, #{amount => GiveAwayAmount}),
    aest_nodes:wait_for_value({txs_on_chain, [PostTxHash1, PostTxHash2]}, NodeNames, 10000, []),

    %% Create a channel
    #{tx_hash := CreateHash, channel_id := ChannelId} =
        aest_nodes:post_create_state_channel_tx(node1, ?BOB, ?ALICE, #{ nonce => 1 }),
    aest_nodes:wait_for_value({txs_on_chain, [CreateHash]}, [node1], 10000, []),

    %% Send multiple deposit transaction with the same round
    #{tx_hash := DepositHash1} =
        aest_nodes:post_deposit_state_channel_tx(node1, ?BOB, ?ALICE, ChannelId,
            #{ nonce => 2, amount => 10, round => 2, ttl => 20 }),
    #{tx_hash := DepositHash2} =
        aest_nodes:post_deposit_state_channel_tx(node1, ?BOB, ?ALICE, ChannelId,
            #{ nonce => 3, amount => 11, round => 2, ttl => 20 }),
    #{tx_hash := DepositHash3} =
        aest_nodes:post_deposit_state_channel_tx(node1, ?BOB, ?ALICE, ChannelId,
            #{ nonce => 4, amount => 12, round => 2, ttl => 20 }),
    #{tx_hash := DepositHash4} =
        aest_nodes:post_deposit_state_channel_tx(node1, ?BOB, ?ALICE, ChannelId,
            #{ nonce => 5, amount => 13, round => 2, ttl => 20 }),

    %% Check the first one got on the chain
    aest_nodes:wait_for_value({txs_on_chain, [DepositHash1]},
                              [node1, node2], {blocks_delta, 5}, []),

    %% Check the others are dropped
    aest_nodes:wait_for_value({txs_all_dropped, [DepositHash2, DepositHash3, DepositHash4]},
                              [node1, node2], {blocks_delta, 21}, []),

    ok.

test_mempool_bad_nonce_cleanup(Cfg) ->
    GasPrice = proplists:get_value(gas_price, Cfg),
    %% Setup nodes
    MikePubKey = maps:get(pubkey, ?MIKE),
    EncMikePubkey = aeser_api_encoder:encode(account_pubkey, MikePubKey),
    NodeConfig = #{ beneficiary => EncMikePubkey },
    NodeNames = [node1, node2],
    setup([?NODE1, ?NODE2], NodeConfig, Cfg),
    start_node(node1, Cfg),
    start_node(node2, Cfg),
    wait_for_startup(NodeNames, 1, Cfg),

    %% Give tokens away
    GiveAwayAmount = 600000 * GasPrice,
    aest_nodes:wait_for_value({balance, MikePubKey, 2*GiveAwayAmount}, [node1], 10000, []),
    #{ tx_hash := PostTxHash1 } = post_spend_tx(node1, ?MIKE, ?ALICE, 1, #{amount => GiveAwayAmount}),
    #{ tx_hash := PostTxHash2 } = post_spend_tx(node1, ?MIKE, ?BOB, 2, #{amount => GiveAwayAmount}),
    aest_nodes:wait_for_value({txs_on_chain, [PostTxHash1, PostTxHash2]}, NodeNames, 10000, []),

    %% Create a channel
    #{tx_hash := CreateHash, channel_id := ChannelId} =
        aest_nodes:post_create_state_channel_tx(node1, ?BOB, ?ALICE, #{ nonce => 1 }),
    aest_nodes:wait_for_value({txs_on_chain, [CreateHash]}, [node1], 10000, []),

    %% Post deposit transactions on node1 (mining node)
    #{tx_hash := DepositHash1} =
        aest_nodes:post_deposit_state_channel_tx(node1, ?BOB, ?ALICE, ChannelId,
            #{ nonce => 2, amount => 10, round => 2, ttl => 200 }),
    #{tx_hash := DepositHash2} =
        aest_nodes:post_deposit_state_channel_tx(node1, ?BOB, ?ALICE, ChannelId,
            #{ nonce => 3, amount => 11, round => 3, ttl => 200 }),

    %% Post deposit transactions on node2 with already used nounce
    #{tx_hash := DepositHash3} =
        aest_nodes:post_deposit_state_channel_tx(node2, ?BOB, ?ALICE, ChannelId,
            #{ nonce => 3, amount => 13, round => 4, ttl => 200 }),
    #{tx_hash := DepositHash4} =
        aest_nodes:post_deposit_state_channel_tx(node2, ?BOB, ?ALICE, ChannelId,
            #{ nonce => 4, amount => 14, round => 5, ttl => 200 }),

    %% check the deposit with correct nonce got on chain
    aest_nodes:wait_for_value({txs_on_chain, [DepositHash1, DepositHash4]},
                              [node1, node2], {blocks_delta, 5}, []),

    %% Check the one of the transaction with duplicated nonce is dropped
    %% before its TTL expires
    aest_nodes:wait_for_value({txs_any_dropped, [DepositHash2, DepositHash3]},
                              [node1, node2], {blocks_delta, 10}, []),

    ok.

%=== INTERNAL FUNCTIONS ========================================================

setup(NodeSpecs, Config, Cfg) ->
    setup_nodes([maps:put(config, Config, N) || N <- NodeSpecs], Cfg).
