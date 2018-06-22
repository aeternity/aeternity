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
    test_inbound_limitation
].

init_per_suite(Config) ->
    [
        {node_startup_time, 20000}, %% Time may take to get the node to respond to http
        {node_shutdown_time, 20000} %% Time it may take to stop node cleanly
    |Config].

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_suite(_Config) -> ok.

%=== TEST CASES ================================================================

test_inbound_limitation(Cfg) ->
    Length = 20,
    StartupTimeout = proplists:get_value(node_startup_time, Cfg),
    setup_nodes([?NODE1, ?NODE2, ?NODE3, ?NODE4], Cfg),
    start_node(node1, Cfg),
    start_node(node2, Cfg),
    start_node(node3, Cfg),
    wait_for_value({height, 0}, [node1, node2, node3], StartupTimeout, Cfg),

    T1 = erlang:system_time(millisecond),
    wait_for_value({height, Length}, [node1, node2, node3], ?MINING_TIMEOUT * Length, Cfg),

    try_until(T1 + 2 * ping_interval(),
            fun() ->
                {ok, 200, B1a} = request(node1, 'GetBlockByHeight', #{height => Length}),
                {ok, 200, B2a} = request(node2, 'GetBlockByHeight', #{height => Length}),
                {ok, 200, B3a} = request(node3, 'GetBlockByHeight', #{height => Length}),
                ?assertEqual(B1a, B2a),
                ?assertEqual(B1a, B3a)
            end),

    % Start 4th node that should get disconnected from node1 and connect to another one.
    start_node(node4, Cfg),
    wait_for_value({height, 0}, [node4], StartupTimeout, Cfg),

    T2 = erlang:system_time(millisecond),
    wait_for_value({height, Length * 2}, [node1, node2, node3, node4], ?MINING_TIMEOUT * Length, Cfg),

    try_until(T2 + 2 * ping_interval(),
            fun() ->
                {ok, 200, B1b} = request(node1, 'GetBlockByHeight', #{height => Length * 2}),
                {ok, 200, B2b} = request(node2, 'GetBlockByHeight', #{height => Length * 2}),
                {ok, 200, B3b} = request(node3, 'GetBlockByHeight', #{height => Length * 2}),
                {ok, 200, B4b} = request(node3, 'GetBlockByHeight', #{height => Length * 2}),
                ?assertEqual(B1b, B2b),
                ?assertEqual(B1b, B3b),
                ?assertEqual(B1b, B4b)
            end),
    ok.

%=== INTERNAL FUNCTIONS ========================================================

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
