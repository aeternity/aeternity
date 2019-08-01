-module(aest_lima_fork_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_suite/1]).

% Test cases
-export([fork_chain/1]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    stop_node/3,
    wait_for_value/4,
    wait_for_startup/3,
    get_block/2,
    get_top/1
]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(MINING_TIMEOUT,   3000).
-define(SYNC_TIMEOUT,      100).

-define(ROMA_VERSION,    1).
-define(ROMA_HEIGHT,     0).

-define(MINERVA_VERSION, 2).
-define(MINERVA_HEIGHT,  5).

-define(FORTUNA_VERSION, 3).
-define(FORTUNA_HEIGHT, 10).

-define(LIMA_VERSION,    4).
-define(LIMA_HEIGHT,    15).

-define(HARD_FORKS, #{?ROMA_VERSION    => ?ROMA_HEIGHT,
                      ?MINERVA_VERSION => ?MINERVA_HEIGHT,
                      ?FORTUNA_VERSION => ?FORTUNA_HEIGHT,
                      ?LIMA_VERSION    => ?LIMA_HEIGHT}).

-define(MINER_NODE1, #{
    name       => miner_node1,
    peers      => [miner_node2],
    hard_forks => ?HARD_FORKS,
    backend    => aest_docker,
    source     => {pull, "aeternity/aeternity:local"}
}).

-define(MINER_NODE2, #{
    name       => miner_node2,
    peers      => [miner_node1],
    hard_forks => ?HARD_FORKS,
    backend    => aest_docker,
    source     => {pull, "aeternity/aeternity:local"}
}).

%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
    fork_chain
].

init_per_suite(Config) ->
    %% Some parameters depend on the speed and capacity of the docker containers:
    %% timers must be less than gen_server:call timeout.
    [{blocks_per_second, 1},
     {node_startup_time, 20000}, %% Time may take to get the node to respond to http
     {node_shutdown_time, 20000} %% Time it may take to stop node cleanly
     | Config].

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_suite(_Config) -> ok.

%=== TEST CASES ================================================================

fork_chain(Cfg) ->
    setup_nodes([?MINER_NODE1, ?MINER_NODE2], Cfg),

    %% Starts a chain with two nodes
    start_node(miner_node1, Cfg),
    start_node(miner_node2, Cfg),

    wait_for_startup([miner_node1, miner_node2], 1, Cfg),
    #{network_id := <<"ae_system_test">>} = aest_nodes:get_status(miner_node1), %% Check node picked user config
    #{network_id := <<"ae_system_test">>} = aest_nodes:get_status(miner_node2), %% Check node picked user config

    check_version_at_height([miner_node1, miner_node2], 1, ?ROMA_VERSION, 15000, Cfg),
    check_version_at_height([miner_node1, miner_node2], ?MINERVA_HEIGHT, ?MINERVA_VERSION, 15000, Cfg),
    check_version_at_height([miner_node1, miner_node2], ?FORTUNA_HEIGHT, ?FORTUNA_VERSION, 15000, Cfg),
    check_version_at_height([miner_node1, miner_node2], ?LIMA_HEIGHT, ?LIMA_VERSION, 15000, Cfg),

    stop_node(miner_node1, 10000, Cfg),
    stop_node(miner_node2, 10000, Cfg),

    ok.

check_version_at_height(Nodes, Height, Version, Timeout, Cfg) ->
    wait_for_value({height, Height}, Nodes, Timeout, Cfg),
    Blocks = [get_block(Node, Height) || Node <- Nodes],
    compare_blocks(Blocks, Version).

compare_blocks([#{hash := H1, version := V1}, #{hash := H2, version := V2} = X | Rest], Version) ->
    ?assertEqual(H1, H2),
    ?assertEqual(V1, Version),
    ?assertEqual(V2, Version),
    compare_blocks([X | Rest], Version);
compare_blocks([_Block | []], _Version) ->
    ok;
compare_blocks([], _Version) ->
    ok.
