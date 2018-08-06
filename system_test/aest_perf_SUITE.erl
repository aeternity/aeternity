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
    startup_speed_mining/1,
    sync_speed/1,
    stay_in_sync/1
]).

-import(aest_nodes, [
    cluster/2,
    spec/3,
    setup_nodes/2,
    start_node/2,
    stop_node/3,
    kill_node/2,
    request/3,
    wait_for_value/4,
    wait_for_time/4,
    get_block/2,
    wait_for_startup/3,
    time_to_ms/1
]).

%=== INCLUDES ==================================================================

-include_lib("eunit/include/eunit.hrl").

%=== MACROS ====================================================================

-define(cfg(Keys), cfg(Keys, Cfg)).

%=== COMMON TEST FUNCTIONS =====================================================

suite() -> [{timetrap, {minutes, 90}}].

all() -> [
    {group, long_chain}
].

groups() ->
    [{long_chain, [], [
        startup_speed,
        startup_speed_mining,
        sync_speed,
        stay_in_sync
    ]}].

init_per_suite(Cfg) ->
    MineRate = 100,
    Cfg ++ [
        {mine_time, {minutes, 20}},     % Time to mine for
        {mine_interval, {seconds, 30}}, % Interval to check mining height
        {mine_rate, MineRate},          % Mine rate configuration for nodes
        {mine_timeout, MineRate * 2},   % Per block
        {sync_timeout, 1000}            % Per block
    ].

init_per_group(long_chain, InitCfg) ->
    %% Some parameters depend on the speed and capacity of the docker containers:
    Cfg = aest_nodes:ct_setup(InitCfg),

    Nodes = [n1, n2, n3],

    setup_nodes(cluster(Nodes, #{mine_rate => ?cfg(mine_rate)}), Cfg),
    [start_node(N, Cfg) || N <- Nodes],
    wait_for_startup(Nodes, 0, Cfg),
    % wait_for_value({height, Height}, Nodes, ?cfg(mine_timeout) * Height, Cfg),
    Height = wait_for_time(height, Nodes, ?cfg(mine_time), #{
        interval => ?cfg(mine_interval)
    }),
    Tag = io_lib:format("local-h~b", [Height]),
    stop_node(n1, 60000, Cfg), %% Ensure DB synced to disk.
    Ref = aest_nodes:export(n1, Tag, Cfg),
    [kill_node(N, Cfg) || N <- Nodes],

    %% Match "ok =" when we want to find errors in this stage
    %% ok = 
    aest_nodes:ct_cleanup(Cfg),
 
    [{height, Height}, {source, Ref}|Cfg].

end_per_group(long_chain, _Cfg) -> ok.

init_per_testcase(_TC, Cfg) -> aest_nodes:ct_setup(Cfg).

end_per_testcase(_TC, Cfg) ->
  aest_nodes:ct_cleanup(Cfg).

end_per_suite(_Cfg) -> ok.

%=== TEST CASES ================================================================

startup_speed(Cfg) ->
    % This test verifies that startup does not take longer than mining
    % and logs the time taken.  The node does not start to mine
    % automatically.
    int_startup_speed(false, Cfg).

startup_speed_mining(Cfg) ->
    % This test verifies that startup does not take longer than mining
    % and logs the time taken.  The node starts to mine automatically.
    int_startup_speed(true, Cfg).

sync_speed(Cfg) ->
    % This tests starts 4 nodes at the pre-mined height, then adds and syncs two
    % more one at a time. Syncing is verified to not take longer than mining and
    % then the measured time is logged.
    [Height, MineRate] = ?cfg([height, mine_rate]),

    InitialNodes = [n1, n2, n3, n4],

    InitialNodeSpecs = [
        spec(N, InitialNodes -- [N], #{
            mine_rate => MineRate,
            source => ?cfg(source),
            mining => #{autostart => false}
        })
        || N <- InitialNodes
    ],
    NewNodesSpec = [
        spec(N, InitialNodes, #{
            mine_rate => MineRate,
            mining => #{autostart => false}
        }) || N <- [n5, n6]
    ],

    setup_nodes(InitialNodeSpecs ++ NewNodesSpec, Cfg),
    [start_node(N, Cfg) || N <- InitialNodes],
    wait_for_startup(InitialNodes, Height, Cfg),

    InitialBlocks = [{N, get_block(N, Height)} || N <- InitialNodes],

    N5Block = sync_node(n5, Height, Cfg),
    N6Block = sync_node(n6, Height, Cfg),

    Blocks = InitialBlocks ++ [{n5, N5Block}, {n6, N6Block}],

    [?assertEqual(AB, BB) || {AN, AB} <- Blocks, {BN, BB} <- Blocks, AN =/= BN].

stay_in_sync(Cfg) ->
    % This test starts 6 nodes at the pre-mined height and then mines for an
    % additional 20% of the original mining time. It asserts that the network
    % does not fork.
    Height = ?cfg(height),
    Nodes = [n1, n2, n3, n4, n5, n6],
    setup_nodes(cluster(Nodes, #{
        mine_rate => ?cfg(mine_rate),
        source => ?cfg(source)
    }), Cfg),
    [start_node(N, Cfg) || N <- Nodes],
    wait_for_startup(Nodes, Height, Cfg),
    Fifth = floor(time_to_ms(?cfg(mine_time)) / 5),
    Reached = wait_for_time(height, Nodes, Fifth, #{
        interval => ?cfg(mine_interval)
    }),

    Blocks = [{N, get_block(N, Reached)} || N <- Nodes],
    [?assertEqual(AB, BB) || {AN, AB} <- Blocks, {BN, BB} <- Blocks, AN =/= BN].

%=== INTERNAL FUNCTIONS ========================================================

int_startup_speed(Autostart, Cfg) ->
    Height = ?cfg(height),
    setup_nodes([spec(node, [], #{source => ?cfg(source),
                                  mining => #{autostart => Autostart}})], Cfg),
    sync_node(node, Height, Cfg).

sync_node(Node, Height, Cfg) ->
    start_node(Node, Cfg),
    Start = erlang:system_time(millisecond),
    wait_for_startup([Node], 0, Cfg),
    wait_for_sync([Node], Height, Cfg),
    End = erlang:system_time(millisecond),
    Block = get_block(Node, Height),
    % Sanity check that syncing does not take longer than mining
    ?assert(End - Start =< time_to_ms(?cfg(mine_time))),
    Block.

wait_for_sync(Nodes, Height, Cfg) ->
    SyncTimeout = proplists:get_value(sync_timeout, Cfg),
    wait_for_value({height, Height}, Nodes, SyncTimeout * Height, Cfg).

cfg(Keys, Cfg) when is_list(Keys) ->
    [cfg(K, Cfg) || K <- Keys];
cfg(Key, Cfg) ->
    case proplists:get_value(Key, Cfg) of
        undefined -> exit({key_not_found, Key});
        Value     -> Value
    end.
