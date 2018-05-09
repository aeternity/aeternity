-module(aest_perf_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
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
    sync_speed/1,
    stay_in_sync/1
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

-define(cfg(Keys), cfg(Keys, Cfg)).

%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
    {group, long_chain}
].

groups() ->
    [{long_chain, [], [
        startup_speed,
        sync_speed,
        stay_in_sync
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
    Height = ?cfg(height),

    setup_nodes(cluster(Nodes, #{mine_rate => ?cfg(mine_rate)}), Cfg),
    [start_node(N, Cfg) || N <- Nodes],
    wait_for_startup(Nodes, 0, Cfg),
    wait_for_value({height, Height}, Nodes, ?cfg(mine_timeout) * Height, Cfg),
    Tag = io_lib:format("local-h~b", [Height]),
    Ref = aest_nodes:export(n1, Tag, Cfg),
    [kill_node(N, Cfg) || N <- Nodes],
    aest_nodes:ct_cleanup(Cfg),
    [{source, Ref}|Cfg].

end_per_group(long_chain, _Cfg) -> ok.

init_per_testcase(_TC, Cfg) -> aest_nodes:ct_setup(Cfg).

end_per_testcase(_TC, Cfg) -> aest_nodes:ct_cleanup(Cfg).

end_per_suite(_Cfg) -> ok.

%=== TEST CASES ================================================================

startup_speed(Cfg) ->
    Height = ?cfg(height),

    setup_nodes([spec(node, [], #{source => ?cfg(source)})], Cfg),
    start_node(node, Cfg),
    wait_for_startup([node], Height, Cfg),
    wait_for_value({height, Height}, [node], ?cfg(startup_timeout), Cfg).

sync_speed(Cfg) ->
    [Height, MineRate] = ?cfg([height, mine_rate]),

    InitialNodes = [n1, n2, n3, n4],

    InitialNodeSpecs = cluster(InitialNodes, #{
        mine_rate => MineRate,
        source => ?cfg(source)
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

stay_in_sync(Cfg) ->
    Height = ?cfg(height),
    Nodes = [n1, n2, n3, n4, n5, n6],
    setup_nodes(cluster(Nodes, #{
        mine_rate => ?cfg(mine_rate),
        source => ?cfg(source)
    }), Cfg),
    [start_node(N, Cfg) || N <- Nodes],
    wait_for_startup(Nodes, Height, Cfg),
    wait_for_value({height, Height + 500}, Nodes, ?cfg(mine_timeout) * 500, Cfg),

    Blocks = [{N, get_block(N, Height + 500, Cfg)} || N <- Nodes],
    [?assertEqual(AB, BB) || {AN, AB} <- Blocks, {BN, BB} <- Blocks, AN =/= BN].

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

cfg(Keys, Cfg) when is_list(Keys) ->
    [cfg(K, Cfg) || K <- Keys];
cfg(Key, Cfg) ->
    case proplists:get_value(Key, Cfg) of
        undefined -> exit({key_not_found, Key});
        Value     -> Value
    end.
