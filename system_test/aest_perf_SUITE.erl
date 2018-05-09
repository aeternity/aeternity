-module(aest_perf_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([suite/0]).
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([init_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_group/2]).
-export([end_per_suite/1]).

% Test cases
-export([
    startup_speed/1,
    sync_speed/1
]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    stop_node/2, stop_node/3,
    kill_node/2,
    connect_node/3, disconnect_node/3,
    http_get/5,
    request/4,
    wait_for_value/4,
    assert_synchronized/2
]).

%=== INCLUDES ==================================================================

-include_lib("eunit/include/eunit.hrl").

%=== MACROS ====================================================================

-define(BASE, #{
    backend => aest_docker,
    source  => {pull, "aeternity/epoch:local"}
}).

%=== COMMON TEST FUNCTIONS =====================================================

suite() -> [{timetrap, {minutes, 90}}].

all() -> [
    {group, long_chain}
].

groups() ->
    [{long_chain, [], [
        startup_speed,
        sync_speed
    ]}].

init_per_suite(Cfg) ->
    MineRate = 100,
    Cfg ++ [
        {height, 10000},              % Length of chain to test
        {mine_rate, MineRate},        % Mine rate configuration for nodes
        {mine_timeout, MineRate * 2}, % Per block
        {startup_timeout, 10000},     % Timeout until HTTP API responds
        {sync_timeout, 100}           % Per block
    ].

init_per_group(long_chain, InitCfg) ->
    %% Some parameters depend on the speed and capacity of the docker containers:
    Cfg = aest_nodes:ct_setup(InitCfg),

    Nodes = [n1, n2, n3],
    Height = proplists:get_value(height, Cfg),
    MineRate = proplists:get_value(mine_rate, Cfg),
    MineTimeout = proplists:get_value(mine_timeout, Cfg),

    setup_nodes(cluster(Nodes, #{mine_rate => MineRate}), Cfg),
    [start_node(N, Cfg) || N <- Nodes],
    wait_for_startup(Nodes, 0, Cfg),
    wait_for_value({height, Height}, Nodes, MineTimeout * Height, Cfg),
    Tag = io_lib:format("local-h~b", [Height]),
    Ref = aest_nodes:export(n1, Tag, Cfg),
    [kill_node(N, Cfg) || N <- Nodes],
    aest_nodes:ct_cleanup(Cfg),
    [{source, Ref}, {height, Height}, {mine_rate, MineRate}|Cfg].

end_per_group(long_chain, _Cfg) -> ok.

init_per_testcase(_TC, Cfg) -> aest_nodes:ct_setup(Cfg).

end_per_testcase(_TC, Cfg) -> aest_nodes:ct_cleanup(Cfg).

end_per_suite(_Cfg) -> ok.

%=== TEST CASES ================================================================

startup_speed(Cfg) ->
    Height = proplists:get_value(height, Cfg),
    Source = proplists:get_value(source, Cfg),
    StartupTimeout = proplists:get_value(startup_timeout, Cfg),

    setup_nodes([spec(node, [], #{source => Source})], Cfg),
    start_node(node, Cfg),
    wait_for_startup([node], Height, Cfg),
    wait_for_value({height, Height}, [node], StartupTimeout, Cfg).

sync_speed(Cfg) ->
    Height = proplists:get_value(height, Cfg),
    Source = proplists:get_value(source, Cfg),
    MineRate = proplists:get_value(mine_rate, Cfg),

    InitialNodes = [n1, n2, n3, n4],

    InitialNodeSpecs = cluster(InitialNodes, #{
        mine_rate => MineRate,
        source => Source
    }),
    NewNodesSpec = [
        spec(N, InitialNodes, #{mine_rate => MineRate})
        || N <- [n5, n6]
    ],

    setup_nodes(InitialNodeSpecs ++ NewNodesSpec, Cfg),
    [start_node(N, Cfg) || N <- InitialNodes],
    wait_for_startup(InitialNodes, Height, Cfg),

    InitialBlocks = [get_block(N, Height, Cfg) || N <- InitialNodes],

    start_node(n5, Cfg),
    wait_for_startup([n5], 0, Cfg),
    wait_for_sync([n5], Height, Cfg),
    N5Block = get_block(n5, Height, Cfg),

    start_node(n6, Cfg),
    wait_for_startup([n6], 0, Cfg),
    wait_for_sync([n6], Height, Cfg),
    N6Block = get_block(n6, Height, Cfg),

    AllBlocks = InitialBlocks ++ [N5Block, N6Block],

    [?assertEqual(A, B) || A <- AllBlocks, B <- AllBlocks, A =/= B].

%=== INTERNAL FUNCTIONS ========================================================

cluster(Names, Spec) -> [spec(N, Names -- [N], Spec) || N <- Names].

spec(Name, Peers, Spec) ->
    maps:merge(maps:merge(?BASE, Spec), #{name => Name, peers => Peers}).

wait_for_startup(Nodes, Height, Cfg) ->
    StartupTimeout = proplists:get_value(startup_timeout, Cfg),
    wait_for_value({height, Height}, Nodes, StartupTimeout, Cfg).

wait_for_sync(Nodes, Height, Cfg) ->
    SyncTimeout = proplists:get_value(sync_timeout, Cfg),
    wait_for_value({height, Height}, Nodes, SyncTimeout * Height, Cfg).

get_block(NodeName, Height, Cfg) ->
    request(NodeName, [v2, 'block-by-height'], #{height => Height}, Cfg).
