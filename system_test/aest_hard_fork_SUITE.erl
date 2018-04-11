-module(aest_hard_fork_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([groups/0]).
-export([suite/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([
         old_node_persisting_chain_and_not_mining_has_genesis_as_top/1,
         restore_db_backup_on_old_node/1,
         old_node_can_receive_chain_from_other_old_node/1,
         restore_db_backup_with_short_chain_on_new_node/1,
         new_node_can_receive_short_old_chain_from_other_new_node/1,
         new_node_accepts_long_old_chain_from_other_new_node_up_to_height_of_new_protocol/1
        ]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(DB_BACKUP_DEST_DIR, "/tmp/mnesia_backup").

-define(PROTOCOLS(H), #{9 => 0, 10 => H}).

-define(PUBKEY1, <<37,195,115,246,90,69,150,234,253,209,246,49,199,88,5,116,191,57,106,189,48,134,209,227,116,85,44,59,51,41,245,55>>).
-define(PUBKEY2, <<149,164,91,254,32,218,238,174,159,207,156,5,246,182,63,10,57,70,109,226,193,2,33,168,116,32,244,228,169,122,154,94>>).

-define(OLD_NODE1, #{
          name    => old_node1,
          pubkey  => ?PUBKEY1,
          peers   => [old_node2], %% Node version v0.10.1 does not support configuring empty set of initial peers.
          backend => aest_docker,
          source  => {pull, "aeternity/epoch:v0.10.1"},
          mine_rate => default
         }).

-define(OLD_NODE2, #{
          name    => old_node2,
          pubkey  => ?PUBKEY2,
          peers   => [old_node1],
          backend => aest_docker,
          source  => {pull, "aeternity/epoch:v0.10.1"},
          mine_rate => default
}).

-define(NEW_NODE1(H), #{
          name    => new_node1,
          pubkey  => ?PUBKEY1,
          peers   => [],
          backend => aest_docker,
          source  => {pull, "aeternity/epoch:local"},
          mine_rate => default,
          hard_forks => ?PROTOCOLS(H)
         }).

-define(NEW_NODE2(H), #{
          name    => new_node2,
          pubkey  => ?PUBKEY2,
          peers   => [new_node1],
          backend => aest_docker,
          source  => {pull, "aeternity/epoch:local"},
          mine_rate => default,
          hard_forks => ?PROTOCOLS(H)
         }).

%=== COMMON TEST FUNCTIONS =====================================================

all() ->
    [
     {group, assumptions},
     {group, hard_fork}
    ].

groups() ->
    [
     {assumptions,
      [
       old_node_persisting_chain_and_not_mining_has_genesis_as_top
      ]},
     {hard_fork, [sequence],
      [
       %% Old node can restore DB backup of testnet.  This test also
       %% determines info of top of chain in DB backup.
       restore_db_backup_on_old_node,
       {group, hard_fork_all}
      ]},
     {hard_fork_all,
      [
       %% Sanity checks on software version supporting only old
       %% protocol:
       %%
       %% * Old node can receive (sync) chain from other old node that restored DB from backup.
       old_node_can_receive_chain_from_other_old_node,
       %% ----
       %% Checks on capability of software version supporting both old
       %% and new version of the protocol to restore DB backup of
       %% testnet (old chain):
       %%
       %% * New node can restore DB backup of testnet - case old chain of height lower than height at which new protocol is effective.
       restore_db_backup_with_short_chain_on_new_node,
       %%
       %% * New node can receive (sync) old chain from other new node that restored DB from backup - case old chain of height lower than height of new protocol.
       new_node_can_receive_short_old_chain_from_other_new_node,
       %%
       %% * New node accepts (sync) from other new node that restored DB from backup old chain only up to configured height for new protocol.
       new_node_accepts_long_old_chain_from_other_new_node_up_to_height_of_new_protocol
       %% ----
       %% Checks on capability of software version supporting both old
       %% and new version of the protocol to mine using old protocol:
                                                % TODO
       %% ----
       %% Checks on capability of software version supporting both old
       %% and new version of the protocol to mine using new protocol:
                                                % TODO
      ]}
    ].

suite() ->
    [
     {require, db_backup_tar},
     {require, db_backup_content}
    ].

init_per_suite(Config) ->
    %% Skip gracefully if DB backup absent.
    Tar = db_backup_tar(Config),
    ct:log("Attempting to read DB backup ~s", [Tar]),
    case file:read_file(Tar) of
        {error, enoent} -> {skip, {missing_db_backup, Tar}};
        {ok, _TarBin} -> Config
    end.

end_per_suite(_Config) ->
    ok.

init_per_group(hard_fork_all, Config) ->
    {_, {restore_db_backup_on_old_node, SavedCfg}} =
        proplists:lookup(saved_config, Config),
    [{_, _} = proplists:lookup(db_backup_top_height, SavedCfg),
     {_, _} = proplists:lookup(db_backup_top_hash, SavedCfg)
     | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

%=== TEST CASES ================================================================

old_node_persisting_chain_and_not_mining_has_genesis_as_top(Cfg) ->
    aest_nodes:setup_nodes([?OLD_NODE1, ?OLD_NODE2], Cfg),
    start_node(old_node1, Cfg),
    #{height := 0} = get_block_by_height(old_node1, 0, Cfg),
    #{height := 0} = aest_nodes:get_top(old_node1, Cfg),
    ok.

restore_db_backup_on_old_node(Cfg) ->
    aest_nodes:setup_nodes([?OLD_NODE1, ?OLD_NODE2], Cfg),
    start_node(old_node1, Cfg),
    {ok, {TopHash, TopHeight}} = restore_db_backup_on_node(old_node1, Cfg),
    ?assertMatch(X when is_integer(X) andalso X > 0, TopHeight),
    B = get_block_by_height(old_node1, TopHeight, Cfg),
    ?assertEqual(TopHash, maps:get(hash, B)),
    {save_config,
     [{db_backup_top_height, TopHeight},
      {db_backup_top_hash, TopHash}]}.

old_node_can_receive_chain_from_other_old_node(Cfg) ->
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Cfg),
    {_, TopHash} = proplists:lookup(db_backup_top_hash, Cfg),
    aest_nodes:setup_nodes([?OLD_NODE1, ?OLD_NODE2], Cfg),
    start_node(old_node1, Cfg),
    {ok, {TopHash, TopHeight}} = restore_db_backup_on_node(old_node1, Cfg),
    start_node(old_node2, Cfg),
    aest_nodes:wait_for_height(TopHeight, [old_node1], 5000, Cfg),
    wait_for_height_syncing(TopHeight, [old_node2], {{45000, ms}, {200, blocks}}, Cfg),
    B = get_block_by_height(old_node2, TopHeight, Cfg),
    ?assertEqual(TopHash, maps:get(hash, B)),
    ok.

restore_db_backup_with_short_chain_on_new_node(Cfg) ->
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Cfg),
    {_, TopHash} = proplists:lookup(db_backup_top_hash, Cfg),
    aest_nodes:setup_nodes([?NEW_NODE1(3 + TopHeight)], Cfg),
    start_node(new_node1, Cfg),
    ?assertEqual({ok, {TopHash, TopHeight}},
                 restore_db_backup_on_node(new_node1, Cfg)),
    B = get_block_by_height(new_node1, TopHeight, Cfg),
    ?assertEqual(TopHash, maps:get(hash, B)),
    ok.

new_node_can_receive_short_old_chain_from_other_new_node(Cfg) ->
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Cfg),
    {_, TopHash} = proplists:lookup(db_backup_top_hash, Cfg),
    aest_nodes:setup_nodes([?NEW_NODE1(3 + TopHeight), ?NEW_NODE2(3 + TopHeight)], Cfg),
    start_node(new_node1, Cfg),
    {ok, {TopHash, TopHeight}} = restore_db_backup_on_node(new_node1, Cfg),
    start_node(new_node2, Cfg),
    aest_nodes:wait_for_height(TopHeight, [new_node1], 5000, Cfg),
    wait_for_height_syncing(TopHeight, [new_node2], {{45000, ms}, {200, blocks}}, Cfg),
    B = get_block_by_height(new_node2, TopHeight, Cfg),
    ?assertEqual(TopHash, maps:get(hash, B)),
    ok.

new_node_accepts_long_old_chain_from_other_new_node_up_to_height_of_new_protocol(Cfg) ->
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Cfg),
    {_, TopHash} = proplists:lookup(db_backup_top_hash, Cfg),
    %% If:
    %% 1. All new nodes - 1 and 2 - followed same consensus rules
    %%    (specifically switching to new protocol at same height); and
    %% 2. New node 1 pruned from the old DB backup the top portion of
    %%    a too long old chain
    %% then new nodes shall not exchange old-protocol blocks at height
    %% at which new protocol shall be effective.  So how to test this
    %% case?
    %%
    %% Actually condition 2 does not hold - i.e. new node 1 does not
    %% prune top portion of too long old chain from DB backup.  Though
    %% do not exploit this as tricky.
    %%
    %% Rather, configure nodes with different heights at which new
    %% protocol enters into effect - so to make new node 1 supply
    %% blocks to new node 2 that are too high old blocks for node 2.
    HeightOfNewProtocolForReadingDB = 3 + TopHeight,
    HeightOfNewProtocolForValidatingBlocks = - 3 + TopHeight,
    aest_nodes:setup_nodes(
      [?NEW_NODE1(HeightOfNewProtocolForReadingDB),
       ?NEW_NODE2(HeightOfNewProtocolForValidatingBlocks)], Cfg),
    start_node(new_node1, Cfg),
    {ok, {TopHash, TopHeight}} = restore_db_backup_on_node(new_node1, Cfg),
    start_node(new_node2, Cfg),
    aest_nodes:wait_for_height(TopHeight, [new_node1], 5000, Cfg),
    LastSyncedOldBlock = - 1 + HeightOfNewProtocolForValidatingBlocks,
    wait_for_height_syncing(LastSyncedOldBlock, [new_node2], {{45000, ms}, {200, blocks}}, Cfg),
    B1 = get_block_by_height(new_node1, LastSyncedOldBlock, Cfg),
    %% Node 2 does not mine.
    B2 = get_block_by_height(new_node2, LastSyncedOldBlock, Cfg),
    ?assertEqual(maps:get(hash, B1), maps:get(hash, B2)),
    ?assertMatch(
       {ok, 404, _},
       aest_nodes:http_get(new_node2, int_http, [v2, block, height, TopHeight], #{}, Cfg)),
    ?assertMatch(
        {ok, 404, _},
        aest_nodes:http_get(new_node2, int_http, [v2, block, height, HeightOfNewProtocolForValidatingBlocks], #{}, Cfg)),
    ok.

%=== INTERNAL FUNCTIONS ========================================================

db_backup_tar(Cfg) ->
    {_, DataDir} = proplists:lookup(data_dir, Cfg),
    filename:join(DataDir, ct:get_config(db_backup_tar)).

start_node(NodeName, Cfg) ->
    aest_nodes:start_node(NodeName, Cfg),
    F = fun() -> try get_block_by_height(NodeName, 0, Cfg), true catch _:_ -> false end end,
    aec_test_utils:wait_for_it(F, true),
    ct:log("Node ~p started as version~n~p", [NodeName, get_version(NodeName, Cfg)]),
    ok.

restore_db_backup_on_node(NodeName, Cfg) ->
    {ok, TarBin} = file:read_file(db_backup_tar(Cfg)),
    restore_db_backup_on_node(
      NodeName,
      TarBin, ct:get_config(db_backup_content),
      ?DB_BACKUP_DEST_DIR,
      Cfg).

restore_db_backup_on_node(NodeName, TarBin, Content, DestDir, Cfg) ->
    ct:log("Restoring DB backup of byte size ~p on node ~s",
           [byte_size(TarBin), NodeName]),
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
    ct:log("Restored DB backup on node ~s, whose top is now~n~p",
           [NodeName, Top]),
    {ok, {TopHash, TopHeight}}.

get_version(NodeName, Cfg) ->
    {ok, 200, B} = aest_nodes:http_get(NodeName, ext_http, [v2, version], #{}, Cfg),
    B.

get_block_by_height(NodeName, Height, Cfg) ->
    {ok, 200, B} = aest_nodes:http_get(NodeName, int_http, [v2, block, height, Height], #{}, Cfg),
    B.

wait_for_height_syncing(MinHeight, NodeNames, {{Timeout, ms}, {Blocks, blocks}}, Cfg) ->
    WaitF =
        fun(H) ->
                ct:log("Waiting for height ~p for ~p ms on nodes ~p...", [H, Timeout, NodeNames]),
                aest_nodes:wait_for_height(H, NodeNames, Timeout, Cfg),
                ct:log("Reached height ~p on nodes ~p ...", [H, NodeNames]),
                ok
        end,
    wait_step_for_height(WaitF, MinHeight, Blocks).

wait_step_for_height(WaitF, MinHeight, StepMaxBlocks)
  when is_integer(MinHeight), MinHeight >= 0,
       is_integer(StepMaxBlocks), StepMaxBlocks > 0 ->
    ok = WaitF(0),
    wait_step_for_height(WaitF, MinHeight, StepMaxBlocks, 0).

wait_step_for_height(_, MinHeight, _, MinHeight) ->
    ok;
wait_step_for_height(WaitF, MinHeight, StepMaxBlocks, ReachedHeight) ->
    H = min(MinHeight, ReachedHeight + StepMaxBlocks),
    ok = WaitF(H),
    wait_step_for_height(WaitF, MinHeight, StepMaxBlocks, H).
