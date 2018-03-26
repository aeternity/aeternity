-module(aest_fork_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([
         node_persisting_chain_and_not_mining_has_genesis/1,
         restore_db_backup_on_same_release/1,
         new_node_can_receive_short_chain_from_old_node/1
        ]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(DB_BACKUP_TAR, "mnesia_ae-uat-epoch_backup.tar").
-define(DB_BACKUP_CONTENT, "mnesia_ae-uat-epoch_backup").
-define(DB_BACKUP_DEST_DIR, "/tmp/mnesia_backup").

-define(OLD_NODE1, #{
          name    => old_node1,
          peers   => [<<"http://unexistent_node:42/">>],
          backend => aest_docker,
          source  => {pull, "aeternity/epoch:v0.9.0"}
         }).

-define(NEW_NODE1, #{
          name    => new_node1,
          peers   => [old_node1],
          backend => aest_docker,
          source  => {pull, "aeternity/epoch:v0.9.0"}
         }).

%=== COMMON TEST FUNCTIONS =====================================================

all() ->
    [
     %% Validation of test assumptions.
     node_persisting_chain_and_not_mining_has_genesis,
     %% ----
     %% Sanity checks on software version supporting only old
     %% protocol:
     %%
     %% * Old node can restore DB backup of testnet.
     restore_db_backup_on_same_release,
     %%
     %% * (Slow test execution time.) Old node can mine after having restored DB backup.
                                                % TODO (Low priority.)
     %% ----
     %% Checks on capability of software version supporting both old
     %% and new version of the protocol to receive chain from software
     %% version supporting only old protocol:
     %%
     %% * New node can receive short chain from old node.
     new_node_can_receive_short_chain_from_old_node
     %%
     %% * New node accepts long chain from old node only up to configured height for new protocol.
                                                % TODO (Medium priority.)
     %% ----
     %% Checks on capability of software version supporting both old
     %% and new version of the protocol to mine using old protocol:
     %%
     %% * (Slow test execution time.) New node can mine using old protocol (strictly only coinbase tx) on short chain received from old node.
                                                % TODO (High priority.)
     %%
     %% * (Slow test execution time.) New node can mine using old protocol (smoke test with spend tx) on short chain received from old node.
                                                % TODO (Low priority.)
     %% * (Slow test execution time.) New node resolve forks on portion of chain using old protocol in favour of fork with highest total difficulty mined by old nodes.
                                                % TODO (Low priority.)
     %% ----
     %% Checks on capability of software version supporting both old
     %% and new version of the protocol to mine using new protocol:
     %%
     %% * (Slow test execution time.) New node can mine using old protocol (strictly only coinbase tx) on short chain received from old node then using new protocol (strictly only coinbase tx).
                                                % TODO (High priority.)
     %%
     %% * (Slow test execution time.) New node can mine using old protocol (strictly only coinbase tx) on short chain received from old node then using new protocol (smoke test with spend tx).
                                                % TODO (Medium priority.)
    ].

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

%=== TEST CASES ================================================================

node_persisting_chain_and_not_mining_has_genesis(Cfg) ->
    aest_nodes:setup_nodes([?OLD_NODE1], Cfg),
    start_node(old_node1, Cfg),
    _ = get_block_by_height(old_node1, 0, Cfg),
    ok.

restore_db_backup_on_same_release(Cfg) ->
    aest_nodes:setup_nodes([?OLD_NODE1], Cfg),
    start_node(old_node1, Cfg),
    {ok, {TopHash, TopHeight}} = restore_db_backup_on_node(old_node1, Cfg),
    ?assertMatch(X when is_integer(X) andalso X > 0, TopHeight),
    B = get_block_by_height(old_node1, TopHeight, Cfg),
    ?assertEqual(TopHash, maps:get(hash, B)),
    ok.

new_node_can_receive_short_chain_from_old_node(Cfg) ->
    aest_nodes:setup_nodes([?OLD_NODE1, ?NEW_NODE1], Cfg),
    start_node(old_node1, Cfg),
    {ok, {TopHash, TopHeight}} = restore_db_backup_on_node(old_node1, Cfg),
    start_node(new_node1, Cfg),
    aest_nodes:wait_for_height(TopHeight, [old_node1], 5000, Cfg),
    wait_for_height_syncing(TopHeight, [new_node1], {{45000, ms}, {100, blocks}}, Cfg),
    B = get_block_by_height(old_node1, TopHeight, Cfg),
    ?assertEqual(TopHash, maps:get(hash, B)),
    ok.

%=== INTERNAL FUNCTIONS ========================================================

get_block_by_height(NodeName, Height, Cfg) ->
    {ok, 200, B} = aest_nodes:http_get(NodeName, int_http, [v2, block, height, Height], #{}, Cfg),
    B.

wait_for_height_syncing(MinHeight, NodeNames, {{Timeout, ms}, {Blocks, blocks}}, Cfg) ->
    %% TODO Expecting syncing to happen at `Blocks` blocks per `Timeout` ms logging progress update - rather than a single wait without progress log.
    aest_nodes:wait_for_height(MinHeight, NodeNames, Timeout, Cfg).

start_node(NodeName, Cfg) ->
    aest_nodes:start_node(NodeName, Cfg),
    F = fun() -> try get_block_by_height(NodeName, 0, Cfg), true catch _:_ -> false end end,
    aec_test_utils:wait_for_it(F, true).

restore_db_backup_on_node(NodeName, Cfg) ->
    restore_db_backup_on_node(
      NodeName,
      db_backup_tar(Cfg), ?DB_BACKUP_CONTENT,
      ?DB_BACKUP_DEST_DIR,
      Cfg).

restore_db_backup_on_node(NodeName, Tar, Content, DestDir, Cfg) ->
    ct:log("Restoring DB backup ~s on node ~s", [Tar, NodeName]),
    {ok, TarBin} = file:read_file(Tar),
    _ = aest_nodes:run_cmd_in_node_dir(NodeName, ["mkdir", DestDir], Cfg),
    "" = aest_nodes:run_cmd_in_node_dir(NodeName, ["ls", DestDir], Cfg),
    ok = aest_nodes:extract_archive(NodeName, DestDir, TarBin, Cfg),
    Content = aest_nodes:run_cmd_in_node_dir(NodeName, ["ls", DestDir], Cfg),
    Dest = DestDir ++ "/" ++ Content,
    RestoreCmd =
        ["bin/epoch",
         "eval",
         "'{atomic, [_|_] = Tabs} = mnesia:restore(\"" ++ Dest ++ "\", []), "
         "ok.'"],
    "ok" = aest_nodes:run_cmd_in_node_dir(NodeName, RestoreCmd, Cfg),
    Top = #{height := TopHeight,
            hash := TopHash} = aest_nodes:get_top(NodeName, Cfg),
    ct:log("Restored DB backup ~s on node ~s, whose top is now~n~p",
           [Tar, NodeName, Top]),
    {ok, {TopHash, TopHeight}}.

db_backup_tar(Cfg) ->
    {_, DataDir} = proplists:lookup(data_dir, Cfg),
    filename:join(DataDir, ?DB_BACKUP_TAR).
