-module(aest_community_fork_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_suite/1]).

% Test cases
-export([fork_chain/1, fork_sync/1]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    stop_node/3,
    wait_for_value/4,
    wait_for_startup/3,
    get_block/2,
    get_top/1,
    get_status/1
]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(MINING_TIMEOUT,   10 * 1000).

-define(SIGNALLING_START_HEIGHT, 5).
-define(SIGNALLING_END_HEIGHT, 15).
-define(SIGNALLING_BLOCK_COUNT, 9).
-define(INFO_FIELD, 1234).
-define(VERSION, 3).

-define(FORK_HEIGHT, ?SIGNALLING_END_HEIGHT).

-define(FORK,
        #{signalling_start_height => ?SIGNALLING_START_HEIGHT,
          signalling_end_height   => ?SIGNALLING_END_HEIGHT,
          signalling_block_count  => ?SIGNALLING_BLOCK_COUNT,
          info_field              => ?INFO_FIELD,
          version                 => ?VERSION}).

-define(FORK_ENABLED, maps:put(enabled, true, ?FORK)).
-define(FORK_DISABLED, maps:put(enabled, false, ?FORK)).

-define(NODE1,
        #{name            => node1,
          peers           => [node2, node3],
          mining          => #{autostart => true},
          fork_management => #{fork => ?FORK_ENABLED},
          backend         => aest_docker,
          source          => {pull, "aeternity/aeternity:local"}}).

-define(NODE2,
        #{name            => node2,
          peers           => [node1, node3],
          mining          => #{autostart => true},
          fork_management => #{fork => ?FORK_ENABLED},
          backend         => aest_docker,
          source          => {pull, "aeternity/aeternity:local"}}).

-define(NODE3,
        #{name            => node3,
          peers           => [node1, node2],
          mining          => #{autostart => false},
          fork_management => #{fork => ?FORK_DISABLED},
          backend         => aest_docker,
          source          => {pull, "aeternity/aeternity:local"}}).

-define(NODE4,
        #{name            => node4,
          peers           => [node3],
          mining          => #{autostart => true},
          fork_management => #{fork => ?FORK_DISABLED},
          backend         => aest_docker,
          source          => {pull, "aeternity/aeternity:local"}}).

%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
    fork_sync,
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
    setup_nodes([?NODE1, ?NODE2, ?NODE3, ?NODE4], Cfg),

    %% Supports new protocol, mining.
    start_node(name(?NODE1), Cfg),
    %% Supports new protocol, mining.
    start_node(name(?NODE2), Cfg),
    %% Doesn't support new protocol, not mining to make sure the blocks in the
    %% signalling interval have the signal supporting the new protocol.
    start_node(name(?NODE3), Cfg),
    %% node4 is started later. It's mining and not supporting the new
    %% protocol. If it was started from the beginning it could mine more blocks
    %% than node1 and node2 and they wouldn't switch to a new protocol.

    %% node 3 is not guaranteed to even sync as far as block 1 from nodes running
    %% the new protocol config (and block 1 is the definition of started here)
    %% if the peer offers blocks after the fork for its sync pool.
    %% Assume it's enough for this test case that nodes 3 and 4 get to the right state in the end.
    wait_for_startup([name(?NODE1), name(?NODE2)], 1, Cfg),
    %% Check node picked user config
    #{network_id := <<"ae_system_test">>} = get_status(name(?NODE1)),
    #{network_id := <<"ae_system_test">>} = get_status(name(?NODE2)),

    %% All the three node have the same chain until the last signalling
    %% block. One block after the signalling block the node3 stays with the
    %% current protocol and node1 and node2 switch to the new protocol.
    LastSigBlockHeight = ?SIGNALLING_END_HEIGHT - 1,
    wait_for_value({height, LastSigBlockHeight}, [name(?NODE1), name(?NODE2)],
                   LastSigBlockHeight * ?MINING_TIMEOUT, Cfg),

    #{hash := LastSigBlockHash1, version := LastSigBlockVsn1} = get_block(name(?NODE1), LastSigBlockHeight),
    #{hash := LastSigBlockHash2, version := LastSigBlockVsn2} = get_block(name(?NODE2), LastSigBlockHeight),

    ?assertEqual(LastSigBlockHash1, LastSigBlockHash2),
    ?assertEqual(LastSigBlockVsn1, LastSigBlockVsn2),

    %% node1 and node2 can switch to the new protocol and continue adding blocks
    %% to the chain. Since node3 is not mining and stays with the old protocol,
    %% it cannot produce the blocks with the old protocol, so node4 is started
    %% (which is mining the old protocol blocks) to verify that node3 can still
    %% add old protocol blocks to the chain.
    start_node(name(?NODE4), Cfg),
    wait_for_startup([name(?NODE3), name(?NODE4)], 1, Cfg),
    %% Check node picked user config
    #{network_id := <<"ae_system_test">>} = get_status(name(?NODE4)),

    wait_for_value({height, LastSigBlockHeight}, [name(?NODE3), name(?NODE4)],
                   LastSigBlockHeight * ?MINING_TIMEOUT, Cfg),

    %% All the nodes are one block before the fork height. node1 and node2 will
    %% upgrade the protocol, node3 and node4 stay with the old protocol.

    AfterForkHeight = ?FORK_HEIGHT + 1,
    wait_for_value({height, AfterForkHeight}, [name(?NODE1), name(?NODE2), name(?NODE3), name(?NODE4)],
                   (AfterForkHeight - LastSigBlockHeight) * ?MINING_TIMEOUT, Cfg),

    #{hash := ForkBlockHash1, version := ForkBlockVsn1} = get_block(name(?NODE1), ?FORK_HEIGHT),
    #{hash := ForkBlockHash2, version := ForkBlockVsn2} = get_block(name(?NODE2), ?FORK_HEIGHT),
    #{hash := ForkBlockHash3, version := ForkBlockVsn3} = get_block(name(?NODE3), ?FORK_HEIGHT),
    #{hash := ForkBlockHash4, version := ForkBlockVsn4} = get_block(name(?NODE4), ?FORK_HEIGHT),

    ?assertEqual(ForkBlockHash1, ForkBlockHash2),
    ?assertEqual(ForkBlockVsn1, ForkBlockVsn2),

    ?assertEqual(ForkBlockHash3, ForkBlockHash4),
    ?assertEqual(ForkBlockVsn3, ForkBlockVsn4),

    ?assertNotEqual(ForkBlockHash1, ForkBlockHash3),
    ?assertNotEqual(ForkBlockVsn1, ForkBlockVsn3),

    ?assert(ForkBlockVsn1 > ForkBlockVsn3),

    ?assert(has_status_new_protocol(name(?NODE1), {?VERSION, ?FORK_HEIGHT})),
    ?assert(has_status_new_protocol(name(?NODE2), {?VERSION, ?FORK_HEIGHT})),
    ?assertNot(has_status_new_protocol(name(?NODE3), {?VERSION, ?FORK_HEIGHT})),
    ?assertNot(has_status_new_protocol(name(?NODE4), {?VERSION, ?FORK_HEIGHT})),

    stop_node(name(?NODE1), 10000, Cfg),
    stop_node(name(?NODE2), 10000, Cfg),
    stop_node(name(?NODE3), 10000, Cfg),
    stop_node(name(?NODE4), 10000, Cfg),

    ok.

%% Test that a second mining node can sync to a node that has already mined
%% past the new protocol
%% This test is not absolutely deterministic, but node1 only needs to mine 5 more
%% blocks before node2 has started and synced. This makes it many times more
%% likely to pass than many of our other tests :)
fork_sync(Cfg) ->
    Node1 = maps:put(peers, [node2], ?NODE1),
    Node2 = maps:put(peers, [node1], ?NODE2),
    setup_nodes([Node1, Node2], Cfg),

    %% Supports new protocol, mining.
    start_node(name(?NODE1), Cfg),
    %% Started, and mined almost up to the end of the signalling block
    wait_for_startup([name(?NODE1)], 10, Cfg),
    %% Check node picked user config
    #{network_id := <<"ae_system_test">>} = get_status(name(?NODE1)),

    %% Supports new protocol, mining. Started
    start_node(name(?NODE2), Cfg),
    wait_for_startup([name(?NODE2)], 1, Cfg),
    #{network_id := <<"ae_system_test">>} = get_status(name(?NODE2)),

    LastSigBlockHeight = ?SIGNALLING_END_HEIGHT - 1,
    wait_for_value({height, LastSigBlockHeight}, [name(?NODE1), name(?NODE2)],
                   LastSigBlockHeight * ?MINING_TIMEOUT, Cfg),

    #{hash := LastSigBlockHash1, version := LastSigBlockVsn1} = get_block(name(?NODE1), LastSigBlockHeight),
    #{hash := LastSigBlockHash2, version := LastSigBlockVsn2} = get_block(name(?NODE2), LastSigBlockHeight),

    ?assertEqual(LastSigBlockHash1, LastSigBlockHash2),

    ?assertEqual(LastSigBlockVsn1, LastSigBlockVsn2),

    %% The nodes are at this point at least up to one block before the fork height and either will
    %% soon or already have upgraded the protocol
    AfterForkHeight = ?FORK_HEIGHT + 1,
    wait_for_value({height, AfterForkHeight}, [name(?NODE1), name(?NODE2)],
                   (AfterForkHeight - LastSigBlockHeight) * ?MINING_TIMEOUT, Cfg),

    #{hash := ForkBlockHash1, version := ForkBlockVsn1} = get_block(name(?NODE1), ?FORK_HEIGHT),
    #{hash := ForkBlockHash2, version := ForkBlockVsn2} = get_block(name(?NODE2), ?FORK_HEIGHT),

    ?assertEqual(ForkBlockHash1, ForkBlockHash2),
    ?assertEqual(ForkBlockVsn1, ForkBlockVsn2),

    ?assert(has_status_new_protocol(name(?NODE1), {?VERSION, ?FORK_HEIGHT})),
    ?assert(has_status_new_protocol(name(?NODE2), {?VERSION, ?FORK_HEIGHT})),

    stop_node(name(?NODE1), 10000, Cfg),
    stop_node(name(?NODE2), 10000, Cfg),

    ok.


has_status_new_protocol(Node, {Protocol, Height}) ->
    #{protocols := Protocols} = get_status(Node),
    lists:member(#{version => Protocol, <<"effective_at_height">> => Height}, Protocols).

name(#{name := Name}) -> Name.
