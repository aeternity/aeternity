-module(aest_peers_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_suite/1]).

% Test cases
-export([
    test_peer_discovery/1,
    test_inbound_limitation/1
]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    stop_node/3,
    wait_for_value/4,
    get_block/2,
    get_top/1,
    request/3
]).

%=== INCLUDES ==================================================================

-include_lib("eunit/include/eunit.hrl").

%=== MACROS ====================================================================

-define(MINING_TIMEOUT,   3000).
-define(SYNC_TIMEOUT,      100).

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

-define(NODE3, #{
    name    => node3,
    peers   => [node1],
    backend => aest_docker,
    source  => {pull, "aeternity/epoch:local"}
}).

-define(NODE4, #{
    name    => node4,
    peers   => [node1],
    backend => aest_docker,
    source  => {pull, "aeternity/epoch:local"}
}).

-define(NODE5, #{
    name    => node5,
    peers   => [node1],
    backend => aest_docker,
    source  => {pull, "aeternity/epoch:local"}
}).


%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
    test_peer_discovery,
    test_inbound_limitation
].

init_per_suite(Config) ->
    [ {node_startup_time, 20000}, %% Time may take to get the node to respond to http
      {node_shutdown_time, 20000} %% Time it may take to stop node cleanly
    | Config].

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_suite(_Config) -> ok.

%=== TEST CASES ================================================================

test_peer_discovery(Cfg) ->
    NodeConfig = #{
        ping_interval => 5000,
        max_inbound => 4
    },
    StartupTimeout = proplists:get_value(node_startup_time, Cfg),
    setup([?NODE1, ?NODE2, ?NODE3, ?NODE4, ?NODE5], NodeConfig, Cfg),
    start_node(node1, Cfg),
    start_node(node2, Cfg),
    start_node(node3, Cfg),
    start_node(node4, Cfg),
    start_node(node5, Cfg),
    wait_for_internal_api([node1, node2, node3, node4, node5], StartupTimeout),

    % Wait for two gossip ping messages.
    timer:sleep(ping_interval() * 2 + 500),

    lists:foreach(fun(N) ->
        {ok, 200, Peers} = aehttp_client:request('GetPeers', #{}, [
                {int_http, aest_nodes_mgr:get_service_address(N, int_http)},
                {ct_log, true}
        ]),
        #{inbound := InboundPeers, outbound := OutboundPeers} = Peers,
        ?assertEqual(4, length(InboundPeers) + length(OutboundPeers))
    end, [node1, node2, node3, node4, node5]),
    ok.

test_inbound_limitation(Cfg) ->
    Length = 50,
    StartupTimeout = proplists:get_value(node_startup_time, Cfg),
    NodeConfig = #{
        ping_interval => 5000,
        max_inbound => 2
    },
    setup([?NODE1, ?NODE2, ?NODE3, ?NODE4], NodeConfig, Cfg),
    start_node(node1, Cfg),
    start_node(node2, Cfg),
    wait_for_internal_api([node1, node2], StartupTimeout),

    % Retrieve node1 peer address.
    #{outbound := [Node1PeerAddr]} = get_peers(node2),

    start_node(node3, Cfg),
    wait_for_internal_api([node3], StartupTimeout),

    T1 = erlang:system_time(millisecond),
    wait_for_value({height, Length + 1}, [node1, node2, node3], ?MINING_TIMEOUT * Length, Cfg),

    try_until(T1 + 3 * ping_interval(),
            fun() ->
                B1a = get_block(node1, Length),
                B2a = get_block(node2, Length),
                B3a = get_block(node3, Length),
                ?assertEqual(B1a, B2a),
                ?assertEqual(B1a, B3a),
                ?assertNotEqual(undefined, B1a)
            end),

    % Ensure a ping between start node3 and starting next.
    TimeForPing1 = max(0, ping_interval() - (erlang:system_time(millisecond) - T1)),
    timer:sleep(TimeForPing1),

    % Start 4th node that should get disconnected from node1 and connect to another one.
    start_node(node4, Cfg),
    wait_for_value({height, 0}, [node4], StartupTimeout, Cfg),
    wait_for_internal_api([node4], StartupTimeout),

    T2 = erlang:system_time(millisecond),
    wait_for_value({height, Length * 2 + 1}, [node1, node2, node3, node4], ?MINING_TIMEOUT * Length, Cfg),

    try_until(T2 + 3 * ping_interval(),
            fun() ->
                B1b = get_block(node1, Length * 2),
                B2b = get_block(node2, Length * 2),
                B3b = get_block(node3, Length * 2),
                B4b = get_block(node4, Length * 2),
                ?assertEqual(B1b, B2b),
                ?assertEqual(B1b, B3b),
                ?assertEqual(B1b, B4b),
                ?assertNotEqual(undefined, B1b)
            end),

    % Ensure a ping between start node4 and checking status.
    TimeForPing2 = max(0, ping_interval() - (erlang:system_time(millisecond) - T2)),
    timer:sleep(TimeForPing2),

    % Check node4 do not have an outbound connection to node1 anymore.
    #{outbound := OutboundPeers, inbound := InboundPeers} = get_peers(node4),
    ?assertNot(lists:member(Node1PeerAddr, OutboundPeers)),
    ?assert(lists:member(Node1PeerAddr, InboundPeers)),
    ok.

%=== INTERNAL FUNCTIONS ========================================================

setup(Nodes, Config, Cfg) ->
    setup_nodes([maps:put(config, Config, N) || N <- Nodes], Cfg).

wait_for_internal_api(Nodes, Timeout) ->
    TimerRef = erlang:start_timer(Timeout, self(), undefined),
    wait_for_internal_api(Nodes, TimerRef, Nodes),
    erlang:cancel_timer(TimerRef).

wait_for_internal_api(_Nodes, _TimerRef, []) -> ok;
wait_for_internal_api(AllNodes, TimerRef, [Node | Rest]) ->
    Res = aehttp_client:request('GetPeers', #{}, [
                {int_http, aest_nodes_mgr:get_service_address(Node, int_http)},
                {ct_log, true}
    ]),
    case Res of
        {ok, _, _} -> wait_for_internal_api(AllNodes, TimerRef, Rest);
        _Error ->
            receive
                {timeout, TimerRef, _} ->
                    ?assert(timeout_waiting_for_internal_api)
            after 200 ->
                wait_for_internal_api(AllNodes, TimerRef, AllNodes)
            end
    end.

get_peers(Node) ->
    {ok, 200, Peers} = aehttp_client:request('GetPeers', #{}, [
                {int_http, aest_nodes_mgr:get_service_address(Node, int_http)},
                {ct_log, true}
    ]),
    Peers.

ping_interval() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"ping_interval">>],
                               aecore, ping_interval, 5000).

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
