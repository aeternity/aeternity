-module(aest_sync_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([
    new_node_joins_network/1,
    docker_keeps_data/1,
    stop_and_continue_sync/1,
    net_split_recovery/1,
    quick_start_stop/1
]).

-import(aest_nodes, [
    cluster/2,
    setup_nodes/2,
    start_node/2,
    stop_node/3,
    connect_node/3, disconnect_node/3,
    wait_for_value/4,
    get_block/2,
    request/3
]).

%=== INCLUDES ==================================================================

-include_lib("eunit/include/eunit.hrl").

%=== MACROS ====================================================================

-define(MINING_TIMEOUT,   2000).
-define(SYNC_TIMEOUT,      100).

-define(STANDALONE_NODE, #{
    name    => standalone_node,
    peers   => [],
    backend => aest_docker,
    source  => {pull, "aeternity/epoch:local"}
}).

%% By default, this node only connects to network `net1` even though
%% it has a `net2_node1` as a peer. It means that if it is not connected
%% explicitly to `net2` it will not be able to connect to `net2_node1`.
-define(NET1_NODE1, #{
    name     => net1_node1,
    peers    => [net1_node2, net2_node1],
    backend  => aest_docker,
    source   => {pull, "aeternity/epoch:local"},
    networks => [net1]
}).

%% By default, this node only connects to network `net1` even though
%% it has a `net2_node2` as a peer. It means that if it is not connected
%% explicitly to `net2` it will not be able to connect to `net2_node2`.
-define(NET1_NODE2, #{
    name    => net1_node2,
    peers   => [net1_node1, net2_node2],
    backend => aest_docker,
    source  => {pull, "aeternity/epoch:local"},
    networks => [net1]
}).

%% By default, this node only connects to network `net2` even though
%% it has a `net1_node1` as a peer. It means that if it is not connected
%% explicitly to `net1` it will not be able to connect to `net1_node1`.
-define(NET2_NODE1, #{
    name    => net2_node1,
    peers   => [net1_node1, net2_node2],
    backend => aest_docker,
    source  => {pull, "aeternity/epoch:local"},
    networks => [net2]
}).

%% By default, this node only connects to network `net2` even though
%% it has a `net1_node2` as a peer. It means that if it is not connected
%% explicitly to `net1` it will not be able to connect to `net1_node2`.
-define(NET2_NODE2, #{
    name    => net2_node2,
    peers   => [net1_node2, net2_node1],
    backend => aest_docker,
    source  => {pull, "aeternity/epoch:local"},
    networks => [net2]
}).



%=== COMMON TEST FUNCTIONS =====================================================

% Please note: this module is part of of the smoke-test target. The combined
% runtime should be kept below 10 minutes.
all() -> [
    new_node_joins_network,
    docker_keeps_data,
    stop_and_continue_sync,
    net_split_recovery,
    quick_start_stop
].

init_per_testcase(_TC, Config) ->
    %% Some parameters depend on the speed and capacity of the docker containers:
    %% timers must be less than gen_server:call timeout.
    aest_nodes:ct_setup([ {blocks_per_second, 3},
                          {node_startup_time, 20000}, %% Time may take to get the node to respond to http
                          {node_shutdown_time, 20000}  %% Time it may take to stop node cleanly
                          | Config]).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

%=== TEST CASES ================================================================

%% A few tests that verify that our assumptions are right for docker timings and
%% API.

%% A node with a newer version of the code can join and synchronize
%% to a cluster of older nodes.
new_node_joins_network(Cfg) ->
    Length = 20,
    NodeStartupTime = proplists:get_value(node_startup_time, Cfg),

    Compatible = "aeternity/epoch:local", %% Latest version it should be compatible with
                                          %% Change if comptibility with previous version
                                          %% should be guaranteed
    ct:log("Testing compatiblity of epoch:local with ~p", [Compatible]),

    OldNode1 = #{
      name    => old_node1,
      peers   => [old_node2],
      backend => aest_docker,
      source  => {pull, Compatible}},

    OldNode2 = #{
      name    => old_node2,
      peers   => [old_node1],
      backend => aest_docker,
      source  => {pull, Compatible}},

    NewNode =  #{
      name    => new_node1,
      peers   => [old_node1],
      backend => aest_docker,
      source  => {pull, "aeternity/epoch:local"}},

    setup_nodes([OldNode1, OldNode2, NewNode], Cfg),

    %% Starts a chain with two nodes
    start_node(old_node1, Cfg),
    start_node(old_node2, Cfg),
    wait_for_value({height, 0}, [old_node1, old_node2], NodeStartupTime, Cfg),

    %% Mines for 20 blocks and calculate the average mining time
    StartTime = os:timestamp(),
    wait_for_value({height, Length}, [old_node1, old_node2], Length * ?MINING_TIMEOUT, Cfg),
    EndTime = os:timestamp(),
    %% Average mining time per block plus 50% extra
    MiningTime = round(timer:now_diff(EndTime, StartTime) * 1.5)
                 div (1000 * Length),

    {ok, 200, Top1} = request(old_node1, 'GetTop', #{}),
    ct:log("Node 1 top: ~p", [Top1]),
    {ok, 200, Height1} = request(old_node1, 'GetBlockByHeight', #{height => Length}),
    ct:log("Node 1 at height ~p: ~p", [Length, Height1]),
    {ok, 200, Height2} = request(old_node2, 'GetBlockByHeight', #{height => Length}),
    ct:log("Node 2 at height ~p: ~p", [Length, Height2]),

    %% Checks node 1 and 2 are synchronized
    ?assertEqual(Height1, Height2),

    %% Starts a third node and check it synchronize with the first two
    start_node(new_node1, Cfg),
    wait_for_value({height, 0}, [new_node1], NodeStartupTime, Cfg),
    ct:log("Node 3 ready to go"),

    %% Waits enough for node 3 to sync but not for it to build a new chain
    wait_for_value({height, Length}, [new_node1], MiningTime * 3, Cfg),
    ct:log("Node 3 on same height"),
    {ok, 200, Height3} = request(new_node1, 'GetBlockByHeight', #{height => Length}),
    ct:log("Node 3 at height ~p: ~p", [Length, Height3]),

    %% Checks node 3 is synchronized with nodes 1 and 2
    ?assertEqual(Height1, Height3),
    ok.

%% When we stop and restart a node we will be able to read the blocks
%% that we had in the chain before stopping: data is persistent.
docker_keeps_data(Cfg) ->
    Length = 20,
    NodeStartupTime = proplists:get_value(node_startup_time, Cfg),
    NodeShutdownTime = proplists:get_value(node_shutdown_time, Cfg),

    setup_nodes([?STANDALONE_NODE], Cfg),

    start_node(standalone_node, Cfg),
    wait_for_value({height, 0}, [standalone_node], NodeStartupTime, Cfg),

    %% Mines for 20 blocks and calculate the average mining time
    StartTime = os:timestamp(),
    wait_for_value({height, Length}, [standalone_node], Length * ?MINING_TIMEOUT, Cfg),
    EndTime = os:timestamp(),
    %% Average mining time per block plus 50% extra
    MiningTime = round(timer:now_diff(EndTime, StartTime) * 1.5)
                 div (1000 * Length),

    %% Get all blocks before stopping
    A = [get_block(standalone_node, H) || H <- lists:seq(1, Length)],

    stop_node(standalone_node, NodeShutdownTime, Cfg),
    %% This requires some time

    start_node(standalone_node, Cfg),
    wait_for_value({height, 0}, [standalone_node], NodeStartupTime, Cfg),

    ct:log("Node restarted and ready to go"),

    %% Give it time to read from disk, but not enough to build a new chain of same length
    timer:sleep(MiningTime * 8),

    %% Get all blocks after restarting
    B = [get_block(standalone_node, H) || H <- lists:seq(1, Length)],

    %% Checks all the nodes before restarting are still there
    {_, Diff} = lists:foldl(fun({X, Y}, {H, Acc}) ->
        case X =:= Y of
            true -> {H + 1, Acc};
            false ->
                ct:log("Block ~w changed after restart:~n"
                       "BEFORE:~n~p~nAFTER:~n~p~n", [H, X, Y]),
                {H + 1, [H | Acc]}
        end
    end, {1, []}, lists:zip(A, B)),
    ?assertEqual([], Diff),

    %% Mines 10 more blocks
    wait_for_value({height, Length + 10}, [standalone_node], MiningTime * 10, Cfg),

    %% Get all blocks before stopping
    C = [get_block(standalone_node, H) || H <- lists:seq(1, Length + 10)],

    stop_node(standalone_node, NodeShutdownTime, Cfg),
    start_node(standalone_node, Cfg),
    wait_for_value({height, 0}, [standalone_node], NodeStartupTime, Cfg),

    %% Give it time to read from disk, but not enough to build a new chain of same length
    timer:sleep(MiningTime * 5),

    %% Get all blocks after restarting
    D = [get_block(standalone_node, H) || H <- lists:seq(1, Length + 10)],

    %% Checks all the nodes before restarting are still there
    {_, Diff} = lists:foldl(fun({X, Y}, {H, Acc}) ->
        case X =:= Y of
            true -> {H + 1, Acc};
            false ->
                ct:log("Block ~w changed after second restart:~n"
                       "BEFORE:~n~p~nAFTER:~n~p~n", [H, X, Y]),
                {H + 1, [H | Acc]}
        end
    end, {1, []}, lists:zip(C, D)),
    ?assertEqual([], Diff),

    ok.

%% If Node2 has a sync process that fetches blocks from Node1, then
%% if Node1 is stopped and restarted, sync will be able to recover and catch up
%% with Node1 after its restart.
%% Note that Node1 must be considerably ahead to make sure Node2 does not
%% create a fork with higher difficulty in the time Node1 restarts.
stop_and_continue_sync(Cfg) ->
    BlocksPerSecond = proplists:get_value(blocks_per_second, Cfg),
    NodeStartupTime = proplists:get_value(node_startup_time, Cfg),

    %% Create a chain long enough to need 10 seconds to fetch it
    Length = BlocksPerSecond * 30,

    setup_nodes([#{ name    => node1,
                    peers   => [node2],
                    backend => aest_docker,
                    source  => {pull, "aeternity/epoch:local"}
                  },
                 #{ name    => node2,
                    peers   => [node1],
                    backend => aest_docker,
                    source  => {pull, "aeternity/epoch:local"}
                  }], Cfg),

    start_node(node1, Cfg),
    wait_for_value({height, 0}, [node1], NodeStartupTime, Cfg),

    wait_for_value({height, Length}, [node1], Length * ?MINING_TIMEOUT, Cfg),

    {ok, 200, B1} = request(node1, 'GetBlockByHeight', #{height => Length}),
    ct:log("Node 1 at height ~p: ~p~n", [Length, B1]),

    %% Start fetching the chain
    start_node(node2, Cfg),
    wait_for_value({height, 0}, [node2], NodeStartupTime, Cfg),
    ct:log("Node 2 ready to go"),

    %% we are fetching blocks, abruptly stop node1 now
    stop_node(node1, 8000, Cfg),
    {ok, 200, Top2} = request(node2, 'GetTop', #{}),
    ct:log("Node 2 top: ~p~n", [Top2]),
    Height = maps:get(height, Top2),
    case Height >= Length of
         true -> {skip, already_synced_when_stopped};
         false ->
            start_node(node1, Cfg),
            %% should sync with about 10 blocks per second, hence 100ms per block
            wait_for_value({height, Length}, [node2], (Length - Height) * ?MINING_TIMEOUT, Cfg),
            {ok, 200, B2} = request(node2, 'GetBlockByHeight', #{height => Length}),
            {ok, 200, C1} = request(node1, 'GetBlockByHeight', #{height => Length}),
            ct:log("Node 2 at height ~p: ~p and  Node 1 at same height ~p~n", [Length, B2, C1]),
            if C1 == B1 ->
                ct:log("This test showed that sync can be interrupted");
               C1 =/= B2 ->
                ct:log("Tested non-interesting branch, node2 synced with node1")
                %% skip here?
            end,
            ?assertEqual(C1, B2)
    end.

%% Test that two disconnected clusters of nodes are able to recover and merge
%% there chain when connected back together.
%% It tests both case of the chain being started from scratch in different
%% network partitions, and that the network is partitiioned when the chain
%% is already shared.
net_split_recovery(Cfg) ->
    Length = 10,

    setup_nodes([?NET1_NODE1, ?NET1_NODE2, ?NET2_NODE1, ?NET2_NODE2], Cfg),
    Nodes = [net1_node1, net1_node2, net2_node1, net2_node2],
    start_node(net1_node1, Cfg),
    start_node(net1_node2, Cfg),
    start_node(net2_node1, Cfg),
    start_node(net2_node2, Cfg),

    %% Starts with a net split

    wait_for_value({height, Length}, Nodes, Length * ?MINING_TIMEOUT, Cfg),

    {ok, 200, A1} = request(net1_node1, 'GetBlockByHeight', #{height => Length}),
    {ok, 200, A2} = request(net1_node2, 'GetBlockByHeight', #{height => Length}),
    {ok, 200, A3} = request(net2_node1, 'GetBlockByHeight', #{height => Length}),
    {ok, 200, A4} = request(net2_node2, 'GetBlockByHeight', #{height => Length}),

    %% Check that the chains are different
    ?assertEqual(A1, A2),
    ?assertEqual(A3, A4),
    ?assertNotEqual(A1, A3),

    %% Join all the nodes
    connect_node(net1_node1, net2, Cfg),
    connect_node(net1_node2, net2, Cfg),
    connect_node(net2_node1, net1, Cfg),
    connect_node(net2_node2, net1, Cfg),
    T0 = erlang:system_time(millisecond),

    %% Mine Length blocks, this may take longer than ping interval
    %% if so, the chains should be in sync when it's done.
    wait_for_value({height, Length * 2}, Nodes, Length * ?MINING_TIMEOUT, Cfg),

    %% Wait at least as long as the ping timer can take
    try_until(T0 + 2 * ping_interval(),
            fun() ->

              {ok, 200, B1} = request(net1_node1, 'GetBlockByHeight', #{height => Length * 2}),
              {ok, 200, B2} = request(net1_node2, 'GetBlockByHeight', #{height => Length * 2}),
              {ok, 200, B3} = request(net2_node1, 'GetBlockByHeight', #{height => Length * 2}),
              {ok, 200, B4} = request(net2_node2, 'GetBlockByHeight', #{height => Length * 2}),

              %% Check that the chain merged
              ?assertEqual(B1, B2),
              ?assertEqual(B1, B3),
              ?assertEqual(B1, B4)
            end),

    {ok, 200, #{height := Top2}} = request(net1_node1, 'GetTop', #{}),
    ct:log("Height reached ~p", [Top2]),

    %% Split again the nodes in two cluster of 2 nodes
    disconnect_node(net1_node1, net2, Cfg),
    disconnect_node(net1_node2, net2, Cfg),
    disconnect_node(net2_node1, net1, Cfg),
    disconnect_node(net2_node2, net1, Cfg),

    wait_for_value({height, Top2 + Length}, Nodes, Length * ?MINING_TIMEOUT, Cfg),

    {ok, 200, C1} = request(net1_node1, 'GetBlockByHeight', #{height => Top2 + Length}),
    {ok, 200, C2} = request(net1_node2, 'GetBlockByHeight', #{height => Top2 + Length}),
    {ok, 200, C3} = request(net2_node1, 'GetBlockByHeight', #{height => Top2 + Length}),
    {ok, 200, C4} = request(net2_node2, 'GetBlockByHeight', #{height => Top2 + Length}),

    %% Check the the chains forked
    ?assertEqual(C1, C2),
    ?assertEqual(C3, C4),
    ?assertNotEqual(C1, C3),

    %% Reconnect the nodes together
    connect_node(net1_node1, net2, Cfg),
    connect_node(net1_node2, net2, Cfg),
    connect_node(net2_node1, net1, Cfg),
    connect_node(net2_node2, net1, Cfg),
    T1 = erlang:system_time(millisecond),

    wait_for_value({height, Top2 + Length * 2}, Nodes, Length * 2 * ?MINING_TIMEOUT, Cfg),

    try_until(T1 + 2 * ping_interval(),
            fun() ->
              {ok, 200, D1} = request(net1_node1, 'GetBlockByHeight', #{height => Top2 + Length * 2}),
              {ok, 200, D2} = request(net1_node2, 'GetBlockByHeight', #{height => Top2 + Length * 2}),
              {ok, 200, D3} = request(net2_node1, 'GetBlockByHeight', #{height => Top2 + Length * 2}),
              {ok, 200, D4} = request(net2_node2, 'GetBlockByHeight', #{height => Top2 + Length * 2}),

              %% Check the chain merged again
              ?assertEqual(D1, D2),
              ?assertEqual(D1, D3),
              ?assertEqual(D1, D4)
            end),

    {ok, 200,#{height := Top3}} = request(net1_node1, 'GetTop', #{}),
    ct:log("Top reached ~p", [Top3]),

    ok.

quick_start_stop(Cfg) ->
    setup_nodes(cluster([n1, n2], #{}), Cfg),
    start_node(n2, Cfg),
    start_node(n1, Cfg),
    stop_node(n2, 2000, Cfg),
    timer:sleep(2000),
    start_node(n2, Cfg),
    ok.

%% helper functions

ping_interval() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"ping_interval">>],
                               aecore, ping_interval, 120000).

try_until(MSec, F) ->
    try F()
    catch
      _:Reason ->
        case erlang:system_time(millisecond) > MSec of
          true ->
            error(Reason);
          false ->
            timer:sleep(100),
            try_until(MSec, F)
        end
    end.
