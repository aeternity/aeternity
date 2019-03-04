-module(aest_oracles_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_suite/1]).

% Test cases
-export([
    test_simple_same_node_query/1,
    test_simple_two_nodes_query/1,
    test_oracle_ttl_extension/1,
    test_pipelined_same_node_query/1,
    test_pipelined_two_nodes_query/1
]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    wait_for_value/4,
    wait_for_startup/3,
    request/3,
    post_spend_tx/5,
    post_oracle_register_tx/3,
    post_oracle_extend_tx/3,
    post_oracle_query_tx/4,
    post_oracle_response_tx/3
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

-define(OLIVIA, #{
    pubkey => <<103,28,85,70,70,73,69,117,178,180,148,246,81,104,
                33,113,6,99,216,72,147,205,210,210,54,3,122,84,195,
                62,238,132>>,
    privkey => <<59,130,10,50,47,94,36,188,50,163,253,39,81,120,89,219,72,88,68,
                 154,183,225,78,92,9,216,215,59,108,82,203,25,103,28,85,70,70,
                 73,69,117,178,180,148,246,81,104,33,113,6,99,216,72,147,205,
                 210,210,54,3,122,84,195,62,238,132>>
}).

-define(ALICE, #{
    pubkey => <<177,181,119,188,211,39,203,57,229,94,108,2,107,214, 167,74,27,
                53,222,108,6,80,196,174,81,239,171,117,158,65,91,102>>,
    privkey => <<145,69,14,254,5,22,194,68,118,57,0,134,66,96,8,20,124,253,238,
                 207,230,147,95,173,161,192,86,195,165,186,115,251,177,181,119,
                 188,211,39,203,57,229,94,108,2,107,214,167,74,27,53,222,108,6,
                 80,196,174,81,239,171,117,158,65,91,102>>
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
    test_simple_same_node_query,
    test_simple_two_nodes_query,
    test_oracle_ttl_extension,
    test_pipelined_same_node_query,
    test_pipelined_two_nodes_query
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

test_simple_same_node_query(Cfg) ->
    Opts = #{
        oracle_node => node1,
        oracle_id => ?OLIVIA,
        querier_node => node1,
        querier_id => ?ALICE
    },
    simple_query_test(Opts, Cfg).

test_simple_two_nodes_query(Cfg) ->
    Opts = #{
        oracle_node => node1,
        oracle_id => ?OLIVIA,
        querier_node => node2,
        querier_id => ?ALICE
    },
    simple_query_test(Opts, Cfg).

simple_query_test(Opts, Cfg) ->
    GasPrice = proplists:get_value(gas_price, Cfg),
    #{
        oracle_node := ONode,
        oracle_id := OAccount,
        querier_node := QNode,
        querier_id := QAccount
    } = Opts,

    MPubKey = maps:get(pubkey, ?MIKE),
    OPubKey = maps:get(pubkey, OAccount),
    QPubKey = maps:get(pubkey, QAccount),
    EncMPubKey = aeser_api_encoder:encode(account_pubkey, MPubKey),
    EncOPubKey = aeser_api_encoder:encode(oracle_pubkey, OPubKey),
    EncQPubKey = aeser_api_encoder:encode(account_pubkey, QPubKey),

    %% Setup nodes
    NodeConfig = #{ beneficiary => EncMPubKey },
    setup([?NODE1, ?NODE2], NodeConfig, Cfg),
    NodeNames = [node1, node2],
    start_node(node1, Cfg),
    start_node(node2, Cfg),
    wait_for_startup([node1, node2], 4, Cfg),

    %% Generate tokens for Mike
    wait_for_value({balance, MPubKey, 2000000 * GasPrice}, [node1], 10000, Cfg),

    %% Give some tokens to the oracle account
    post_spend_tx(node1, ?MIKE, OAccount, 1, #{ amount => 600000 * GasPrice }),
    wait_for_value({balance, OPubKey, 600000 * GasPrice }, NodeNames, 10000, Cfg),

    %% Give some tokens to the querier account
    post_spend_tx(node1, ?MIKE, QAccount, 2, #{ amount => 600000 * GasPrice }),
    wait_for_value({balance, QPubKey, 600000 * GasPrice}, NodeNames, 10000, Cfg),

    %% Register oracle
    #{ tx_hash := RegTxHash } = post_oracle_register_tx(ONode, OAccount, #{
        nonce           => 1,
        query_format    => <<"qspec">>,
        response_format => <<"rspec">>,
        query_fee       => 1,
        fee             => 50000 * GasPrice,
        oracle_ttl      => {block, 2000}
    }),
    aest_nodes:wait_for_value({txs_on_chain, [RegTxHash]}, NodeNames, 10000, []),

    {ok, 200, OracleInfo} =
        request(node1, 'GetOracleByPubkey', #{ pubkey => EncOPubKey }),
    ?assertMatch(#{ id := EncOPubKey }, OracleInfo),

    %% Start an oracle query
    #{ tx_hash := QueryTxHash } = post_oracle_query_tx(QNode, QAccount, OAccount, #{
        nonce        => 1,
        query        => <<"Hidely-Ho">>,
        query_fee    => 2,
        fee          => 50000 * GasPrice,
        query_ttl    => {delta, 100},
        response_ttl => {delta, 100}
    }),
    aest_nodes:wait_for_value({txs_on_chain, [QueryTxHash]}, NodeNames, 10000, []),
    QueryId = aeo_query:id(QPubKey, 1, OPubKey),
    EncQueryId = aeser_api_encoder:encode(oracle_query_id, QueryId),

    {ok, 200, ClosedQueriesInfo} =
        request(node1, 'GetOracleQueriesByPubkey', #{ pubkey => EncOPubKey, type => closed }),
    ?assertMatch(#{ oracle_queries := [] }, ClosedQueriesInfo),
    {ok, 200, AllQueriesInfo} =
        request(node1, 'GetOracleQueriesByPubkey', #{ pubkey => EncOPubKey, type => all }),
    ?assertMatch(#{ oracle_queries := [_] }, AllQueriesInfo),
    [QueryInfo] = maps:get(oracle_queries, AllQueriesInfo),
    ?assertMatch(#{ id := EncQueryId, oracle_id := EncOPubKey, sender_id := EncQPubKey }, QueryInfo),
    ?assertEqual({oracle_query, <<"Hidely-Ho">>}, aeser_api_encoder:decode(maps:get(query, QueryInfo))),
    ?assertEqual({oracle_response, <<>>}, aeser_api_encoder:decode(maps:get(response, QueryInfo))),

    %% Respond to the oracle query
    #{ tx_hash := RespTxHash } = post_oracle_response_tx(ONode, OAccount, #{
        nonce        => 2,
        query_id     => QueryId,
        response     => <<"D'oh!">>,
        response_ttl => {delta, 100},
        fee          => 50000 * GasPrice
    }),
    aest_nodes:wait_for_value({txs_on_chain, [RespTxHash]}, NodeNames, 10000, []),

    {ok, 200, OpenQueriesInfo} =
        request(node1, 'GetOracleQueriesByPubkey', #{ pubkey => EncOPubKey, type => <<"open">> }),
    ?assertMatch(#{ oracle_queries := [] }, OpenQueriesInfo),
    {ok, 200, QueryInfo2} =
        request(node1, 'GetOracleQueryByPubkeyAndQueryId', #{ pubkey => EncOPubKey, 'query-id' => EncQueryId }),
    ?assertMatch(#{ id := EncQueryId, oracle_id := EncOPubKey, sender_id := EncQPubKey }, QueryInfo2),
    ?assertEqual({oracle_response, <<"D'oh!">>}, aeser_api_encoder:decode(maps:get(response, QueryInfo2))),

    ok.

test_oracle_ttl_extension(Cfg) ->
    GasPrice = proplists:get_value(gas_price, Cfg),
    MPubKey = maps:get(pubkey, ?MIKE),
    OPubKey = maps:get(pubkey, ?OLIVIA),
    EncMPubKey = aeser_api_encoder:encode(account_pubkey, MPubKey),
    EncOPubKey = aeser_api_encoder:encode(oracle_pubkey, OPubKey),

    %% Setup nodes
    NodeConfig = #{ beneficiary => EncMPubKey },
    setup([?NODE1], NodeConfig, Cfg),
    start_node(node1, Cfg),
    wait_for_startup([node1], 4, Cfg),

    %% Generate tokens for Mike
    wait_for_value({balance, MPubKey, 400000}, [node1], 10000, Cfg),

    %% Give some tokens to the oracle account
    post_spend_tx(node1, ?MIKE, ?OLIVIA, 1, #{ amount => 200000 * GasPrice }),
    wait_for_value({balance, OPubKey, 200}, [node1], 10000, Cfg),

    %% Register oracle
    #{ tx_hash := RegTxHash } = post_oracle_register_tx(node1, ?OLIVIA, #{
        nonce           => 1,
        query_format    => <<"qspec">>,
        response_format => <<"rspec">>,
        query_fee       => 1,
        fee             => 50000 * GasPrice,
        oracle_ttl      => {block, 200}
    }),
    aest_nodes:wait_for_value({txs_on_chain, [RegTxHash]}, [node1], 10000, []),

    {ok, 200, OracleInfo} =
        request(node1, 'GetOracleByPubkey', #{ pubkey => EncOPubKey }),
    ?assertMatch(#{ id := EncOPubKey, ttl := 200 }, OracleInfo),

    %% Extend oracle's TTL
    #{ tx_hash := ExtTxHash } = post_oracle_extend_tx(node1, ?OLIVIA, #{
        nonce      => 2,
        fee        => 50000 * GasPrice,
        oracle_ttl => {delta, 100}
    }),
    aest_nodes:wait_for_value({txs_on_chain, [ExtTxHash]}, [node1], 10000, []),

    {ok, 200, OracleInfo2} =
        request(node1, 'GetOracleByPubkey', #{ pubkey => EncOPubKey }),
    ?assertMatch(#{ id := EncOPubKey, ttl := 300 }, OracleInfo2),

    ok.

test_pipelined_same_node_query(Cfg) ->
    Opts = #{
        oracle_node => node1,
        oracle_id => ?OLIVIA,
        querier_node => node1,
        querier_id => ?ALICE
    },
    pipelined_query_test(Opts, Cfg).

test_pipelined_two_nodes_query(Cfg) ->
    Opts = #{
        oracle_node => node1,
        oracle_id => ?OLIVIA,
        querier_node => node2,
        querier_id => ?ALICE
    },
    pipelined_query_test(Opts, Cfg).

pipelined_query_test(Opts, Cfg) ->
    GasPrice = proplists:get_value(gas_price, Cfg),
    #{
        oracle_node := ONode,
        oracle_id := OAccount,
        querier_node := QNode,
        querier_id := QAccount
    } = Opts,

    MPubKey = maps:get(pubkey, ?MIKE),
    OPubKey = maps:get(pubkey, OAccount),
    QPubKey = maps:get(pubkey, QAccount),
    EncMPubKey = aeser_api_encoder:encode(account_pubkey, MPubKey),
    EncOPubKey = aeser_api_encoder:encode(oracle_pubkey, OPubKey),
    EncQPubKey = aeser_api_encoder:encode(account_pubkey, QPubKey),

    %% Setup nodes
    NodeConfig = #{ beneficiary => EncMPubKey },
    setup([?NODE1, ?NODE2], NodeConfig, Cfg),
    NodeNames = [node1, node2],
    start_node(node1, Cfg),
    start_node(node2, Cfg),
    wait_for_startup([node1, node2], 4, Cfg),

    %% Give tokens away
    GiveAwayAmount = 600000 * GasPrice,
    aest_nodes:wait_for_value({balance, MPubKey, 2*GiveAwayAmount}, [node1], 10000, []),
    %% Give some tokens to the oracle account/OAccount
    post_spend_tx(node1, ?MIKE, OAccount, 1, #{ amount => GiveAwayAmount }),
    %% Give some tokens to the querier account
    post_spend_tx(node1, ?MIKE, QAccount, 2, #{ amount => GiveAwayAmount }),

    Fee = 50000 * GasPrice,
    aest_nodes:wait_for_value({balance, OPubKey, 2*Fee}, [ONode], 10000, []),
    aest_nodes:wait_for_value({balance, QPubKey, Fee}, [QNode], 10000, []),

    %% Register oracle
    #{ tx_hash := _ } = post_oracle_register_tx(ONode, OAccount, #{
        nonce           => 1,
        query_format    => <<"qspec">>,
        response_format => <<"rspec">>,
        query_fee       => 1,
        fee             => Fee,
        oracle_ttl      => {block, 2000}
    }),

    %% Start an oracle query
    #{ tx_hash := _ } = post_oracle_query_tx(QNode, QAccount, OAccount, #{
        nonce        => 1,
        query        => <<"Hidely-Ho">>,
        query_fee    => 2,
        fee          => Fee,
        query_ttl    => {delta, 100},
        response_ttl => {delta, 100}
    }),
    QueryId = aeo_query:id(QPubKey, 1, OPubKey),
    EncQueryId = aeser_api_encoder:encode(oracle_query_id, QueryId),

    %% Respond to the oracle query
    #{ tx_hash := RespTxHash } = post_oracle_response_tx(ONode, OAccount, #{
        nonce        => 2,
        query_id     => QueryId,
        response     => <<"D'oh!">>,
        response_ttl => {delta, 100},
        fee          => Fee
    }),

    %% Wait for the response transaction to get in the chain
    aest_nodes:wait_for_value({txs_on_chain, [RespTxHash]}, NodeNames, 10000, []),

    {ok, 200, OracleInfo} =
        request(node1, 'GetOracleByPubkey', #{ pubkey => EncOPubKey }),
    ?assertMatch(#{ id := EncOPubKey }, OracleInfo),
    {ok, 200, OpenQueriesInfo} =
        request(node1, 'GetOracleQueriesByPubkey', #{ pubkey => EncOPubKey, type => <<"open">> }),
    ?assertMatch(#{ oracle_queries := [] }, OpenQueriesInfo),
    {ok, 200, QueryInfo} =
        request(node1, 'GetOracleQueryByPubkeyAndQueryId', #{ pubkey => EncOPubKey, 'query-id' => EncQueryId }),
    ?assertMatch(#{ id := EncQueryId, oracle_id := EncOPubKey, sender_id := EncQPubKey }, QueryInfo),
    ?assertEqual({oracle_query, <<"Hidely-Ho">>}, aeser_api_encoder:decode(maps:get(query, QueryInfo))),
    ?assertEqual({oracle_response, <<"D'oh!">>}, aeser_api_encoder:decode(maps:get(response, QueryInfo))),

    ok.

%=== INTERNAL FUNCTIONS ========================================================

setup(NodeSpecs, Config, Cfg) ->
    setup_nodes([maps:put(config, Config, N) || N <- NodeSpecs], Cfg).
