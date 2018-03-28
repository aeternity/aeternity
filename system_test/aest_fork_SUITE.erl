-module(aest_fork_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([
         node_persisting_chain_and_not_mining_has_genesis/1,
         restore_db_backup_on_same_release/1
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
     node_persisting_chain_and_not_mining_has_genesis,
     restore_db_backup_on_same_release
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

restore_db_backup_on_same_release(Cfg) ->
    aest_nodes:setup_nodes([?OLD_NODE1], Cfg),
    start_node(old_node1, Cfg),
    _ = aest_nodes:get_block(old_node1, 0, Cfg),
    {_, DataDir} = proplists:lookup(data_dir, Cfg),
    DbBackupFile = filename:join(DataDir, "mnesia_ae-uat-epoch_backup.tar"),
    {ok, DbBackupBin} = file:read_file(DbBackupFile),
    _ = aest_nodes:run_cmd_in_node_dir(old_node1, ["mkdir", "/tmp/mnesia_backup/"], Cfg),
    ok = aest_nodes:extract_archive(old_node1, "/tmp/mnesia_backup", DbBackupBin, Cfg),
    ct:log("~p", [aest_nodes:run_cmd_in_node_dir(old_node1, ["ls", "-lahrt", "/tmp/mnesia_backup/"], Cfg)]),
    "ok" = aest_nodes:run_cmd_in_node_dir(old_node1, ["bin/epoch", "eval", "'{atomic, [_|_] = Tabs} = mnesia:restore(\"/tmp/mnesia_backup/mnesia_ae-uat-epoch_backup\", []), ok.'"], Cfg),
    ct:log("~p", [aest_nodes:get_top(old_node1, Cfg)]), %% TODO Extract height at top.
    _ = aest_nodes:get_block(old_node1, 100, Cfg), %% TODO Change to non-zero height at top.
    ok.

%=== INTERNAL FUNCTIONS ========================================================

start_node(NodeName, Cfg) ->
    aest_nodes:start_node(NodeName, Cfg),
    F = fun() -> try aest_nodes:get_block(NodeName, 0, Cfg), true catch _:_ -> false end end,
    aec_test_utils:wait_for_it(F, true).
