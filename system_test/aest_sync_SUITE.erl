-module(aest_sync_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_suite/1]).

% Test cases
-export([
    new_node_joins_network/1,
    docker_keeps_data/1,
    stop_and_continue_sync/1,
    tx_pool_sync/1,
    net_split_recovery/1,
    net_split_mining_power/1,
    abrupt_stop_new_node/1,
    abrupt_stop_mining_node/1
]).

-import(aest_nodes, [
    cluster/2,
    setup_nodes/2,
    start_node/2,
    stop_node/3,
    connect_node/3, disconnect_node/3,
    wait_for_value/4,
    wait_for_startup/3,
    get_block/2,
    get_top/1,
    request/3,
    assert_in_sync/1
]).

%=== INCLUDES ==================================================================

-include_lib("eunit/include/eunit.hrl").

%=== MACROS ====================================================================

-define(MINING_TIMEOUT,   3000).

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

%% By default, this node only connects to network `net2` even though
%% it has a `net1_node1` as a peer. It means that if it is not connected
%% explicitly to `net1` it will not be able to connect to `net1_node1`.
-define(NET2_NODE3, #{
    name    => net2_node3,
    peers   => [net1_node1, net2_node1],
    backend => aest_docker,
    source  => {pull, "aeternity/epoch:local"},
    networks => [net2]
}).

%% By default, this node only connects to network `net2` even though
%% it has a `net1_node1` as a peer. It means that if it is not connected
%% explicitly to `net1` it will not be able to connect to `net1_node1`.
-define(NET2_NODE4, #{
    name    => net2_node4,
    peers   => [net1_node1, net2_node1],
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
    tx_pool_sync,
    net_split_recovery,
    net_split_mining_power,
    abrupt_stop_new_node,
    abrupt_stop_mining_node
].

init_per_suite(Config) ->
    %% Some parameters depend on the speed and capacity of the docker containers:
    %% timers must be less than gen_server:call timeout.
    [ {blocks_per_second, 1},
      {node_startup_time, 10000}, %% Time may take to get the node to respond to http
      {node_shutdown_time, 20000} %% Time it may take to stop node cleanly
    | Config].

init_per_testcase(quick_start_stop, Config) ->
    aest_nodes:ct_setup([{verify_logs, false}|Config]);
init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_suite(_Config) -> ok.

%=== TEST CASES ================================================================

%% This is keypair of an account set in genesis config file
%% (see https://github.com/aeternity/epoch/blob/master/data/aecore/.genesis/accounts.json),
%% so beneficiary configuration in epoch.yaml (mining > beneficiary param) does not matter.
patron() ->
    #{ pubkey => <<206,167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,73,187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>,
       privkey => <<230,169,29,99,60,119,207,87,113,50,157,51,84,179,188,239,27,197,224,50,196,61,112,182,211,90,249,35,206,30,183,77,206,167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,73,187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>
     }.


%% A few tests that verify that our assumptions are right for docker timings and
%% API.

%% A node with a newer version of the code can join and synchronize
%% to a cluster of older nodes.
new_node_joins_network(Cfg) ->
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
    T0 = erlang:system_time(seconds),
    wait_for_startup([old_node1, old_node2], 0, Cfg),
    StartupTime = erlang:system_time(seconds) - T0,

    Length = max(20, 5 + proplists:get_value(blocks_per_second, Cfg) * StartupTime),

    %% Mines for 20 blocks and calculate the average mining time
    StartTime = os:timestamp(),


    inject_spend_txs(old_node1, patron(), 20, 1, 100),
    inject_spend_txs(old_node2, patron(), 20, 21, 100),

    wait_for_value({height, Length}, [old_node1, old_node2], Length * ?MINING_TIMEOUT, Cfg),
    EndTime = os:timestamp(),
    %% Average mining time per block plus 50% extra
    MiningTime = round(timer:now_diff(EndTime, StartTime) * 1.5)
                 div (1000 * Length),

    Top1 = get_top(old_node1),
    ct:log("Node 1 top: ~p", [Top1]),
    Height1 = get_block(old_node1, Length),
    ct:log("Node 1 at height ~p: ~p", [Length, Height1]),
    Height2 = get_block(old_node2, Length),
    ct:log("Node 2 at height ~p: ~p", [Length, Height2]),

    %% Checks node 1 and 2 are synchronized
    ?assertEqual(Height1, Height2),
    ?assertNotEqual(undefined, Height1),

    %% Starts a third node and check it synchronize with the first two
    start_node(new_node1, Cfg),

    inject_spend_txs(old_node1, patron(), 20, 41, 100), 

    wait_for_startup([new_node1], 0, Cfg),

    %% Starting http interface takes more time than sync, but:
    %% Wait enough for node 3 to sync but not for it to build a new chain
    wait_for_value({height, Length}, [new_node1], MiningTime * 2, Cfg),
    ct:log("Node 3 on same height"),
    Height3 = get_block(new_node1, Length),
    ct:log("Node 3 at height ~p: ~p", [Length, Height3]),

    %% Checks node 3 is synchronized with nodes 1 and 2
    ?assertEqual(Height1, Height3),
    ok.

%% When we stop and restart a node we will be able to read the blocks
%% that we had in the chain before stopping: data is persistent.
docker_keeps_data(Cfg) ->
    Length = 20,
    NodeShutdownTime = proplists:get_value(node_shutdown_time, Cfg),

    setup_nodes([?STANDALONE_NODE], Cfg),

    start_node(standalone_node, Cfg),
    wait_for_startup([standalone_node], 0, Cfg),

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
    wait_for_startup([standalone_node], 0, Cfg),

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
    wait_for_startup([standalone_node], 0, Cfg),

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
    %% Create a chain long enough to need 10 seconds to fetch it
    Length = 150,

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
    wait_for_startup([node1], 0, Cfg),

    inject_spend_txs(node1, patron(), 50, 1, 100),

    wait_for_value({height, Length}, [node1], Length * ?MINING_TIMEOUT, Cfg),

    B1 = get_block(node1, Length),
    ct:log("Node 1 at height ~p: ~p~n", [Length, B1]),
    ?assertNotEqual(undefined, B1),

    %% Start fetching the chain
    start_node(node2, Cfg),

    inject_spend_txs(node1, patron(), 20, 51, 100),

    wait_for_startup([node2], 0, Cfg),
    ct:log("Node 2 ready to go"),

    %% we are fetching blocks, abruptly stop node1 now
    stop_node(node1, 8000, Cfg),
    Top2 = get_top(node2),
    ct:log("Node 2 top: ~p~n", [Top2]),
    Height = maps:get(height, Top2),
    case Height >= Length of
         true -> {skip, already_synced_when_stopped};
         false ->
            start_node(node1, Cfg),
            %% should sync with about 10 blocks per second, hence 100ms per block

            inject_spend_txs(node2, patron(), 20, 71, 100),

            wait_for_value({height, Length + 1}, [node2], (Length - Height) * ?MINING_TIMEOUT, Cfg),
            B2 = get_block(node2, Length),
            C1 = get_block(node1, Length),
            ct:log("Node 2 at height ~p: ~p and  Node 1 at same height ~p~n", [Length, B2, C1]),
            if C1 == B1 ->
                  ct:log("This tests sync can be interrupted, node1 unchanged"),
                  ?assertEqual(C1, B2);
               C1 == B2 ->
                  ?assertNotEqual(undefined, C1),
                  ?assertNotEqual(undefined, B2),
                  ct:log("Tested non-interesting branch, node1 copied node2"),
                  {skip, need_longer_chain};
               true ->
                  ct:log("Nodes not in sync, that's an error"),
                  ?assertEqual(C1, B2)
            end
    end.

tx_pool_sync(Cfg) ->
    NodeStartupTime = proplists:get_value(node_startup_time, Cfg),

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

    %% Let's post a bunch of transactions, preferrably some valid
    %% and some "not yet valid"

    Patron = patron(),

    %% Add 5 valid spend transactions
    ValidTxs = add_spend_txs(node1, Patron, 5, 1),
    %% Add 10 invalid (nonce_too_high) spend transactions
    InvalidTxs = add_spend_txs(node1, Patron, 5, 7),
    add_spend_txs(node1, Patron, 5, 15),


    %% Check that the valid transactions made it to the chain.
    #{ receiver := RecvAccount, amount := Amount } = lists:last(ValidTxs),
    wait_for_value({balance, aec_base58c:encode(account_pubkey, RecvAccount), Amount},
                   [node1], 5 * ?MINING_TIMEOUT, Cfg),

    %% Check that the mempool has the other transactions
    {ok, 200, MempoolTxs1} = request(node1, 'GetTxs', #{}),
    {10, _} = {length(MempoolTxs1), MempoolTxs1},

    %% Start 2nd node and let it sync
    start_node(node2, Cfg),
    wait_for_value({height, 0}, [node2], NodeStartupTime, Cfg),

    %% Give the sync a moment to finish
    #{ height := Height1 } = get_top(node1),
    wait_for_value({height, Height1 + 5}, [node2], 5 * ?MINING_TIMEOUT, Cfg),

    {ok, 200, MempoolTxs2} = request(node2, 'GetTxs', #{}),
    {10, _} = {length(MempoolTxs2), MempoolTxs2},

    %% Stop node1
    stop_node(node1, 8000, Cfg),

    %% Add one more invalid transaction at Node2
    add_spend_txs(node2, Patron, 1, 25),

    %% Start node1 and make sure that Tx is synced.
    %% TODO: Automate check that _only_ this Tx is synced.
    start_node(node1, Cfg),
    wait_for_value({height, 0}, [node1], NodeStartupTime, Cfg),

    %% Give the sync a moment to finish
    #{ height := Height2 } = get_top(node2),
    wait_for_value({height, Height2 + 5}, [node1], 5 * ?MINING_TIMEOUT, Cfg),

    {ok, 200, MempoolTxs1B} = request(node1, 'GetTxs', #{}),
    {11, _} = {length(MempoolTxs1B), MempoolTxs1B},

    %% Now add a Tx that unlocks 5 more...
    add_spend_txs(node2, Patron, 1, 6),

    %% Check that the last of the first batch of invalid transactions made it to the chain.
    #{ receiver := RecvAccount2, amount := Amount2 } = lists:last(InvalidTxs),
    wait_for_value({balance, aec_base58c:encode(account_pubkey, RecvAccount2), Amount2},
                   [node1], 5 * ?MINING_TIMEOUT, Cfg),

    ok.


inject_spend_txs(Node, SenderAcct, N, NonceStart, TimeDelay) ->
    [ begin 
          add_spend_tx(Node, SenderAcct, Nonce),
          timer:sleep(TimeDelay)
      end || Nonce <- lists:seq(NonceStart, NonceStart + N - 1) ].

add_spend_txs(Node, SenderAcct, N, NonceStart) ->
    [ add_spend_tx(Node, SenderAcct, Nonce) || Nonce <- lists:seq(NonceStart, NonceStart + N - 1) ].

add_spend_tx(Node, #{ pubkey := SendPubKey, privkey := SendSecKey }, Nonce) ->
    #{ public := RecvPubKey, secret := RecvSecKey } = enacl:sign_keypair(),
    PayLoad = iolist_to_binary(io_lib:format("~p", [Node])),
    Params = #{ sender => aec_id:create(account, SendPubKey)
              , recipient => aec_id:create(account, RecvPubKey)
              , amount => 10000
              , fee => 100
              , ttl => 10000000
              , nonce => Nonce
              , payload => PayLoad },
    {ok, Tx} = aec_spend_tx:new(Params),
    SignedTx = aec_test_utils:sign_tx(Tx, SendSecKey),
    SerSignTx = aetx_sign:serialize_to_binary(SignedTx),
    {ok, 200, #{ tx_hash := TxHash }} = request(Node, 'PostTx', #{ tx => aec_base58c:encode(transaction, SerSignTx) }),
    #{ receiver => RecvPubKey, receiver_sec => RecvSecKey, amount => 10000, tx_hash => TxHash }.

%% Test that two disconnected clusters of nodes are able to recover and merge
%% there chain when connected back together.
%% It tests both case of the chain being started from scratch in different
%% network partitions, and that the network is partitiioned when the chain
%% is already shared.
net_split_recovery(Cfg) ->
    Length = 20,

    setup_nodes([?NET1_NODE1, ?NET1_NODE2, ?NET2_NODE1, ?NET2_NODE2], Cfg),
    Nodes = [net1_node1, net1_node2, net2_node1, net2_node2],
    start_node(net1_node1, Cfg),
    start_node(net1_node2, Cfg),
    start_node(net2_node1, Cfg),
    start_node(net2_node2, Cfg),

    %% Starts with a net split
    wait_for_value({height, 0}, [net1_node1, net2_node1], proplists:get_value(node_startup_time, Cfg), Cfg),

    inject_spend_txs(net1_node1, patron(), 20, 1, 100),
    inject_spend_txs(net2_node1, patron(), 20, 1, 100),

    TargetHeight1 = Length,
    %% Wait for one extra block for resolving potential fork caused by nodes mining distinct blocks at the same time.
    MinedHeight1 = 1 + TargetHeight1,
    wait_for_value({height, MinedHeight1}, Nodes, (1 + Length) * ?MINING_TIMEOUT, Cfg),

    A1 = get_block(net1_node1, TargetHeight1),
    A2 = get_block(net1_node2, TargetHeight1),
    A3 = get_block(net2_node1, TargetHeight1),
    A4 = get_block(net2_node2, TargetHeight1),

    %% Check that the chains are different
    ?assertEqual(A1, A2),
    ?assertEqual(A3, A4),
    ?assertNotEqual(A1, A3),
    ?assertNotEqual(undefined, A1),
    ?assertNotEqual(undefined, A3),

    %% Join all the nodes
    connect_node(net1_node1, net2, Cfg),
    connect_node(net1_node2, net2, Cfg),
    connect_node(net2_node1, net1, Cfg),
    connect_node(net2_node2, net1, Cfg),
    T0 = erlang:system_time(millisecond),

    inject_spend_txs(net1_node1, patron(), 20, 21, 100),
    inject_spend_txs(net2_node1, patron(), 20, 21, 100),

    %% Mine Length blocks, this may take longer than ping interval
    %% if so, the chains should be in sync when it's done.
    TargetHeight2 = MinedHeight1 + Length,
    %% Wait for one extra block for resolving potential fork caused by nodes mining distinct blocks at the same time.
    wait_for_value({height, 1 + TargetHeight2}, Nodes, (1 + Length) * ?MINING_TIMEOUT, Cfg),

    %% Wait at least as long as the ping timer can take
    try_until(T0 + 2 * ping_interval(),
            fun() ->

              B1 = get_block(net1_node1, TargetHeight2),
              B2 = get_block(net1_node2, TargetHeight2),
              B3 = get_block(net2_node1, TargetHeight2),
              B4 = get_block(net2_node2, TargetHeight2),

              %% Check that the chain merged
              ?assertEqual(B1, B2),
              ?assertEqual(B1, B3),
              ?assertEqual(B1, B4),
              ?assertNotEqual(undefined, B1)
            end),

    {ok, 200, #{height := Top2}} = request(net1_node1, 'GetTop', #{}),
    ct:log("Height reached ~p", [Top2]),

    %% Split again the nodes in two cluster of 2 nodes
    disconnect_node(net1_node1, net2, Cfg),
    disconnect_node(net1_node2, net2, Cfg),
    disconnect_node(net2_node1, net1, Cfg),
    disconnect_node(net2_node2, net1, Cfg),

    TargetHeight3 = Top2 + Length,
    %% Wait for one extra block for resolving potential fork caused by nodes mining distinct blocks at the same time.
    MinedHeight3 = 1 + TargetHeight3,
    wait_for_value({height, MinedHeight3}, Nodes, (1 + Length) * ?MINING_TIMEOUT, Cfg),

    C1 = get_block(net1_node1, TargetHeight3),
    C2 = get_block(net1_node2, TargetHeight3),
    C3 = get_block(net2_node1, TargetHeight3),
    C4 = get_block(net2_node2, TargetHeight3),

    %% Check the the chains forked
    ?assertEqual(C1, C2),
    ?assertEqual(C3, C4),
    ?assertNotEqual(C1, C3),
    ?assertNotEqual(undefined, C1),
    ?assertNotEqual(undefined, C3),


    %% Reconnect the nodes together
    connect_node(net1_node1, net2, Cfg),
    connect_node(net1_node2, net2, Cfg),
    connect_node(net2_node1, net1, Cfg),
    connect_node(net2_node2, net1, Cfg),
    T1 = erlang:system_time(millisecond),

    TargetHeight4 = MinedHeight3 + Length,
    %% Wait for one extra block for resolving potential fork caused by nodes mining distinct blocks at the same time.
    wait_for_value({height, 1 + TargetHeight4}, Nodes, (1 + Length) * ?MINING_TIMEOUT, Cfg),

    try_until(T1 + 2 * ping_interval(),
            fun() ->
              D1 = get_block(net1_node1, TargetHeight4),
              D2 = get_block(net1_node2, TargetHeight4),
              D3 = get_block(net2_node1, TargetHeight4),
              D4 = get_block(net2_node2, TargetHeight4),

              %% Check the chain merged again
              ?assertEqual(D1, D2),
              ?assertEqual(D1, D3),
              ?assertEqual(D1, D4),
              ?assertNotEqual(undefined, D1)
            end),

    {ok, 200,#{height := Top3}} = request(net1_node1, 'GetTop', #{}),
    ct:log("Top reached ~p", [Top3]),

    ok.

net_split_mining_power(Cfg) ->
    SplitLength = 40,
    SyncLength = 20,
    ExtraLength = 3,

    Net1Nodes = [net1_node1, net1_node2],
    Net2Nodes = [net2_node1, net2_node2, net2_node3, net2_node4],
    AllNodes = Net1Nodes ++ Net2Nodes,

    setup_nodes([?NET1_NODE1, ?NET1_NODE2, ?NET2_NODE1,
                 ?NET2_NODE2, ?NET2_NODE3, ?NET2_NODE4], Cfg),

    lists:foreach(fun(N) -> start_node(N, Cfg) end, Net2Nodes),
    lists:foreach(fun(N) -> start_node(N, Cfg) end, Net1Nodes),

    TargetHeight1 = SplitLength,
    %% Wait for one extra block for resolving potential fork caused by nodes mining distinct blocks at the same time.
    MinedHeight1 = ExtraLength + TargetHeight1,
    wait_for_value({height, MinedHeight1}, AllNodes,
                   (ExtraLength + SplitLength) * ?MINING_TIMEOUT, Cfg),

    Net1BlocksA = lists:foldl(fun(N, Acc) ->
        [get_block(N, TargetHeight1) | Acc]
    end, [], Net1Nodes),

    [N1A1 | N1As] = Net1BlocksA,
    ?assertNotEqual(undefined, N1A1),
    lists:foreach(fun(A) -> ?assertEqual(N1A1, A) end, N1As),

    Net2BlocksA = lists:foldl(fun(N, Acc) ->
        [get_block(N, TargetHeight1) | Acc]
    end, [], Net2Nodes),

    [N2A1 | N2As] = Net2BlocksA,
    ?assertNotEqual(undefined, N2A1),
    lists:foreach(fun(A) -> ?assertEqual(N2A1, A) end, N2As),

    %% Check that the chains are different
    ?assertNotEqual(N1A1, N2A1),

    % Check that the 4 node cluster has more mining power.
    Net1MinedBlocks1 = node_mined_retries(Net1Nodes),
    Net2MinedBlocks1 = node_mined_retries(Net2Nodes),
    ?assert(Net1MinedBlocks1 < Net2MinedBlocks1),

    %% Join all the nodes
    lists:foreach(fun(N) -> connect_node(N, net2, Cfg) end, Net1Nodes),
    lists:foreach(fun(N) -> connect_node(N, net1, Cfg) end, Net2Nodes),
    T0 = erlang:system_time(millisecond),

    %% Mine Length blocks, this may take longer than ping interval
    %% if so, the chains should be in sync when it's done.
    TargetHeight2 = MinedHeight1 + SyncLength,
    %% Wait for one extra block for resolving potential fork caused by nodes mining distinct blocks at the same time.
    wait_for_value({height, ExtraLength + TargetHeight2}, AllNodes,
                   (ExtraLength + SyncLength) * ?MINING_TIMEOUT, Cfg),

    %% Wait at least as long as the ping timer can take
    try_until(T0 + 2 * ping_interval(),
            fun() ->
                BlocksB = lists:foldl(fun(N, Acc) ->
                    [get_block(N, TargetHeight2) | Acc]
                end, [], AllNodes),

                [B1 | Bs] = BlocksB,
                ?assertNotEqual(undefined, B1),
                lists:foreach(fun(B) -> ?assertEqual(B1, B) end, Bs)
            end),

    {ok, 200, #{height := Top2}} = request(net1_node1, 'GetTop', #{}),
    ct:log("Height reached ~p", [Top2]),

    % Check that the 4 node cluster has still more mining power.
    Net1MinedBlocks2 = node_mined_retries(Net1Nodes),
    Net2MinedBlocks2 = node_mined_retries(Net2Nodes),
    ?assert(Net1MinedBlocks2 < Net2MinedBlocks2),

    % Check the cluster with more mining power win.

    lists:foreach(fun(N) ->
        ?assertEqual(N2A1, get_block(N, TargetHeight1))
    end, AllNodes),

    ok.

abrupt_stop_new_node(Cfg) ->
    RepairTimeout = 30000, % Time allowed for node to repair DB and finish sync
    Nodes = [n1, n2],
    setup_nodes(cluster(Nodes, #{}), Cfg),
    % Start both nodes
    [start_node(N, Cfg) || N <- Nodes],
    % Stop node 2 abruptly
    stop_node(n2, 2000, Cfg),
    % Restart node 2
    start_node(n2, Cfg),
    % Check that they synchronize
    Blocks = wait_for_value({height, 5}, Nodes, RepairTimeout, Cfg),
    assert_in_sync(Blocks).

abrupt_stop_mining_node(Cfg) ->
    RepairTimeout = 30000, % Time allowed for node to repair DB and finish sync
    NodeShutdownTime = proplists:get_value(node_shutdown_time, Cfg),
    Nodes = [n1, n2],
    setup_nodes(cluster(Nodes, #{}), Cfg),
    % Start both nodes
    [start_node(N, Cfg) || N <- Nodes],
    % Let them build some chain
    Blocks = wait_for_value({height, 5}, Nodes, 5 * ?MINING_TIMEOUT, Cfg),
    assert_in_sync(Blocks),
    % Stop node 2
    stop_node(n2, NodeShutdownTime, Cfg),
    % Start node 2
    start_node(n2, Cfg),
    % Stop node 2 abruptly
    stop_node(n2, 2000, Cfg),
    % Restart node 2
    start_node(n2, Cfg),
    % Check that they synchronize
    NewBlocks = wait_for_value({height, 5}, Nodes, RepairTimeout, Cfg),
    ?assertEqual(Blocks, NewBlocks),
    % Check that they continue mining
    LatestBlocks = wait_for_value({height, 10}, Nodes, 5 * ?MINING_TIMEOUT, Cfg),
    assert_in_sync(LatestBlocks).

%% helper functions

node_mined_retries(Nodes) ->
    Metric = "ae.epoch.aecore.mining.retries.value",
    lists:foldl(fun(N, Acc) ->
        case aest_nodes:read_last_metric(N, Metric) of
            undefined -> Acc;
            Num -> Acc + Num
        end
    end, 0, Nodes).

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
