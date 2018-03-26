-module(aest_fork_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([
         node_persisting_chain_and_not_mining_has_genesis/1
        ]).

%=== INCLUDES ==================================================================

%=== MACROS ====================================================================

-define(OLD_NODE1, #{
          name    => old_node1,
          peers   => [<<"http://unexistent_node:42/">>],
          backend => aest_docker,
          source  => {pull, "aeternity/epoch:v0.9.0"}
         }).

%=== COMMON TEST FUNCTIONS =====================================================

all() ->
    [
     node_persisting_chain_and_not_mining_has_genesis
     %% restore_db_backup_on_same_release
    ].

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

%=== TEST CASES ================================================================

node_persisting_chain_and_not_mining_has_genesis(Cfg) ->
    aest_nodes:setup_nodes([?OLD_NODE1], Cfg),
    start_node(old_node1, Cfg),
    _ = aest_nodes:get_block(old_node1, 0, Cfg),
    ok.

%=== INTERNAL FUNCTIONS ========================================================

start_node(NodeName, Cfg) ->
    aest_nodes:start_node(NodeName, Cfg),
    F = fun() -> try aest_nodes:get_block(NodeName, 0, Cfg), true catch _:_ -> false end end,
    aec_test_utils:wait_for_it(F, true).
