-module(aest_community_fork_SUITE).

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

-define(SIGNALLING_START_HEIGHT, 5).
-define(SIGNALLING_END_HEIGHT, 15).
-define(SIGNALLING_BLOCK_COUNT, 9).
-define(FORK_HEIGHT, 20).
-define(INFO_FIELD, 1234).
-define(VERSION, 5).

-define(FORK,
        #{fork =>
              #{signalling_start_height => ?SIGNALLING_START_HEIGHT,
                signalling_end_height => ?SIGNALLING_END_HEIGHT,
                signalling_block_count => ?SIGNALLING_BLOCK_COUNT,
                fork_height => ?FORK_HEIGHT,
                info_field => ?INFO_FIELD,
                version => ?VERSION}}).

-define(MINER_NODE1,
        #{name            => miner_node1,
          peers           => [miner_node2, miner_node3],
          mining          => #{autostart => true},
          fork_management => ?FORK,
          backend         => aest_docker,
          source          => {pull, "aeternity/aeternity:local"}}).

-define(MINER_NODE2,
        #{name            => miner_node2,
          peers           => [miner_node1, miner_node3],
          mining          => #{autostart => true},
          fork_management => ?FORK,
          backend         => aest_docker,
          source          => {pull, "aeternity/aeternity:local"}}).

%% This node is not configured for fork.
-define(MINER_NODE3,
        #{name            => miner_node3,
          peers           => [miner_node1, miner_node2],
          mining          => #{autostart => false},
          backend         => aest_docker,
          source          => {pull, "aeternity/aeternity:local"}}).

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
    setup_nodes([?MINER_NODE1, ?MINER_NODE2, ?MINER_NODE3], Cfg),

    %% Starts a chain with three nodes.
    start_node(miner_node1, Cfg),
    start_node(miner_node2, Cfg),
    start_node(miner_node3, Cfg),

    wait_for_startup([miner_node1, miner_node2, miner_node3], 1, Cfg),
    #{network_id := <<"ae_system_test">>} = aest_nodes:get_status(miner_node1), %% Check node picked user config
    #{network_id := <<"ae_system_test">>} = aest_nodes:get_status(miner_node2), %% Check node picked user config
    #{network_id := <<"ae_system_test">>} = aest_nodes:get_status(miner_node3), %% Check node picked user config

    ct:log(">>> MR: ~p", [aest_nodes:get_status(miner_node3)]),

    wait_for_value({height, ?FORK_HEIGHT}, [miner_node1, miner_node2, miner_node3],
                   ?FORK_HEIGHT * ?MINING_TIMEOUT, Cfg),

    #{hash := N1Hash, version := N1Version} = get_block(miner_node1, ?FORK_HEIGHT),
    #{hash := N2Hash, version := N2Version} = get_block(miner_node2, ?FORK_HEIGHT),
    #{hash := N3Hash, version := N3Version} = get_block(miner_node3, ?FORK_HEIGHT),

    N1InfoFieldBlockCount = info_field_block_count(miner_node1, ?SIGNALLING_START_HEIGHT, ?SIGNALLING_END_HEIGHT, ?INFO_FIELD),
    N2InfoFieldBlockCount = info_field_block_count(miner_node2, ?SIGNALLING_START_HEIGHT, ?SIGNALLING_END_HEIGHT, ?INFO_FIELD),
    N3InfoFieldBlockCount = info_field_block_count(miner_node3, ?SIGNALLING_START_HEIGHT, ?SIGNALLING_END_HEIGHT, ?INFO_FIELD),

    ?assertEqual(N1InfoFieldBlockCount, N2InfoFieldBlockCount),
    ?assertEqual(N2InfoFieldBlockCount, N3InfoFieldBlockCount),

    %% Ensure there are enough blocks signalling fork.
    ?assertMatch(X when X >= ?SIGNALLING_BLOCK_COUNT, N1InfoFieldBlockCount),

    ?assertEqual(N1Hash, N2Hash),
    ?assertEqual(N1Version, N2Version),
    ?assertEqual(N1Version, ?VERSION),
    ?assertNotEqual(N1Hash, N3Hash),

    ?assertNotEqual(N1Version, N3Version),

    stop_node(miner_node1, 10000, Cfg),
    stop_node(miner_node2, 10000, Cfg),
    stop_node(miner_node3, 10000, Cfg),

    ok.

info_field_block_count(Node, StartHeight, EndHeight, InfoField) ->
    %% TODO: encode/decode info_field
    lists:foldl(
      fun(Height, Count) ->
              #{info := Info} = get_block(Node, Height),
              case Info =:= InfoField of
                  true -> Count + 1;
                  false -> Count
              end
      end, 0, lists:seq(StartHeight, EndHeight)).

%% check_version_at_height(Nodes, Height, Version, Timeout, Cfg) ->
%%     wait_for_value({height, Height}, Nodes, Timeout, Cfg),
%%     Blocks = [get_block(Node, Height) || Node <- Nodes],
%%     compare_blocks(Blocks, Version).

%% compare_blocks([#{hash := H1, version := V1}, #{hash := H2, version := V2} = X | Rest], Version) ->
%%     ?assertEqual(H1, H2),
%%     ?assertEqual(V1, Version),
%%     ?assertEqual(V2, Version),
%%     compare_blocks([X | Rest], Version);
%% compare_blocks([_Block | []], _Version) ->
%%     ok;
%% compare_blocks([], _Version) ->
%%     ok.
