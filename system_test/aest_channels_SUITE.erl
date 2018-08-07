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
    test_simple_different_nodes_channel/1
]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    wait_for_value/4
]).

-import(aest_api, [
    tx_spend/6,
    tx_wait/5,
    sc_open/2,
    sc_withdraw/3,
    sc_close_mutual/2
]).

%=== INCLUDES ==================================================================

-include_lib("eunit/include/eunit.hrl").

%=== MACROS ====================================================================

-define(MINING_TIMEOUT,   3000).
-define(SYNC_TIMEOUT,      100).

-define(MIKE, #{
    pubkey => <<"ak$2XNq9oKtThxKLNFGWTaxmLBZPgP7ECEGxL3zK7dTSFh6RyRvaG">>,
    privkey => <<237,12,20,128,115,166,32,106,220,142,111,97,141,104,201,130,56,
                 100,64,142,139,163,87,166,185,94,4,159,217,243,160,169,200,171,
                 93,11,3,93,177,65,197,27,123,127,177,165,190,211,20,112,79,108,
                 85,78,88,181,26,207,191,211,40,225,138,154>>
}).

-define(ALICE, #{
    pubkey => <<"ak$2MGLPW2CHTDXJhqFJezqSwYSNwbZokSKkG7wSbGtVmeyjGfHtm">>,
    privkey => <<145,69,14,254,5,22,194,68,118,57,0,134,66,96,8,20,124,253,238,
                 207,230,147,95,173,161,192,86,195,165,186,115,251,177,181,119,
                 188,211,39,203,57,229,94,108,2,107,214,167,74,27,53,222,108,6,
                 80,196,174,81,239,171,117,158,65,91,102>>
}).

-define(BOB, #{
    pubkey => <<"ak$nQpnNuBPQwibGpSJmjAah6r3ktAB7pG9JHuaGWHgLKxaKqEvC">>,
    privkey => <<59,130,10,50,47,94,36,188,50,163,253,39,81,120,89,219,72,88,68,
                 154,183,225,78,92,9,216,215,59,108,82,203,25,103,28,85,70,70,
                 73,69,117,178,180,148,246,81,104,33,113,6,99,216,72,147,205,
                 210,210,54,3,122,84,195,62,238,132>>
}).

-define(NODE1, #{
    name    => node1,
    peers   => [],
    backend => aest_docker,
    source  => {pull, "aeternity/epoch:local"}
}).

-define(NODE2, #{
    name    => node2,
    peers   => [node1],
    backend => aest_docker,
    source  => {pull, "aeternity/epoch:local"}
}).

%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
    test_simple_same_node_channel,
    test_simple_different_nodes_channel
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
        initiator_account => ?BOB,
        initiator_amount => 80,
        responder_node => node1,
        responder_account => ?ALICE,
        responder_amount => 80
    },
    simple_channel_test(ChannelOpts, Cfg).

test_simple_different_nodes_channel(Cfg) ->
    ChannelOpts = #{
        initiator_node => node1,
        initiator_account => ?BOB,
        initiator_amount => 80,
        responder_node => node2,
        responder_account => ?ALICE,
        responder_amount => 80
    },
    simple_channel_test(ChannelOpts, Cfg).

simple_channel_test(ChannelOpts, Cfg) ->
    #{
        initiator_account := IAccount,
        initiator_amount := IAmt,
        responder_account := RAccount,
        responder_amount := RAmt
    } = ChannelOpts,

    NodeConfig = #{ beneficiary => maps:get(pubkey, ?MIKE) },
    setup([?NODE1, ?NODE2], NodeConfig, Cfg),
    NodeNames = [node1, node2],
    start_node(node1, Cfg),
    start_node(node2, Cfg),
    wait_balance(NodeNames, ?MIKE, 1000, 10000, Cfg),

    tx_spend(node1, ?MIKE, IAccount, 200, 1, Cfg),
    wait_balance(NodeNames, IAccount, 200, 5000, Cfg),

    tx_spend(node1, ?MIKE, RAccount, 200, 1, Cfg),
    wait_balance(NodeNames, RAccount, 200, 5000, Cfg),

    {ok, Chan, TxHash, OpenFee} = sc_open(ChannelOpts, Cfg),
    ok = tx_wait(NodeNames, TxHash, chain, 2000, Cfg),
    wait_balance(NodeNames, IAccount, 200 - IAmt - OpenFee, 5000, Cfg),
    wait_balance(NodeNames, RAccount, 200 - RAmt, 5000, Cfg),

    {ok, TxHash1, WFee1} = sc_withdraw(Chan, initiator, 20),
    ok = tx_wait(NodeNames, TxHash1, chain, 2000, Cfg),
    wait_balance(NodeNames, IAccount, 200 - IAmt - OpenFee + 20 - WFee1, 5000, Cfg),
    wait_balance(NodeNames, RAccount, 200 - RAmt, 5000, Cfg),

    {ok, TxHash2, WFee2} = sc_withdraw(Chan, responder, 50),
    ok = tx_wait(NodeNames, TxHash2, chain, 2000, Cfg),
    wait_balance(NodeNames, IAccount, 200 - IAmt - OpenFee + 20 - WFee1, 5000, Cfg),
    wait_balance(NodeNames, RAccount, 200 - RAmt + 50 - WFee2, 5000, Cfg),

    {ok, CloseTxHash, IChange, RChange} = sc_close_mutual(Chan, initiator),

    ?assertEqual(IAmt - 20 - OpenFee, IChange),
    ?assertEqual(RAmt - 50, RChange),

    ok = tx_wait(NodeNames, CloseTxHash, chain, 2000, Cfg),
    wait_balance(NodeNames, IAccount, 200 - IAmt - OpenFee + 20 - WFee1 + IChange, 5000, Cfg),
    wait_balance(NodeNames, RAccount, 200 - RAmt + 50 - WFee2 + RChange, 5000, Cfg),

    ok.

%=== INTERNAL FUNCTIONS ========================================================

setup(NodeSpecs, Config, Cfg) ->
    setup_nodes([maps:put(config, Config, N) || N <- NodeSpecs], Cfg).

wait_balance(NodeNames, #{pubkey := Pubkey}, Amount, Timeout, Cfg) ->
    wait_for_value({balance, Pubkey, Amount}, NodeNames, Timeout, Cfg).

