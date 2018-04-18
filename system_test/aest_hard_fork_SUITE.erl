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
         new_node_persisting_chain_and_not_mining_has_same_old_genesis_as_top/1,
         new_nodes_can_mine_and_sync_fast_minimal_chain_with_pow/1,
         restore_db_backup_on_old_node/1,
         old_node_can_receive_chain_from_other_old_node/1,
         restore_db_backup_with_short_chain_on_new_node/1,
         new_node_accepts_long_old_chain_from_other_new_node_up_to_height_of_new_protocol/1,
         new_node_can_receive_short_old_chain_from_other_new_node/1,
         new_node_can_mine_on_old_chain_using_old_protocol/1,
         new_node_can_mine_on_old_chain_using_new_protocol/1,
         new_node_can_mine_spend_tx_on_old_chain_using_old_protocol/1,
         new_node_can_mine_spend_tx_on_old_chain_using_new_protocol/1
        ]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(TESTED_DOCKER_IMAGE, "aeternity/epoch:v0.11.0").

-define(DB_BACKUP_DEST_DIR, "/tmp/mnesia_backup").

-define(GENESIS_PROTOCOL_VERSION, 9).
-define(NEW_PROTOCOL_VERSION, 10).
-define(PROTOCOLS(H), #{?GENESIS_PROTOCOL_VERSION => 0,
                        ?NEW_PROTOCOL_VERSION => H}).

-define(HEIGHT_OF_NEW_PROTOCOL(OldChainHeight),
        (2 + OldChainHeight)
       ).
-define(HEIGHT_OF_NEW_PROTOCOL_FOR_VALIDATING_BLOCKS(OldChainHeight),
        (- 3 + OldChainHeight)
       ).

-define(HEIGHT_OF_NEW_PROTOCOL_UNREACHABLE(OldChainHeight),
        (1000000000 + OldChainHeight)
       ).

-define(CUCKOO_MINER(N),
        #{ex => list_to_binary("mean" ++ integer_to_list(N) ++ "s-generic"),
          args => <<"-t 5">>,
          bits => N}
       ).

-define(OLD_NODE1, #{
          name    => old_node1,
          peers   => [old_node2], %% Node version v0.10.1 does not support configuring empty set of initial peers.
          backend => aest_docker,
          source  => {pull, "aeternity/epoch:v0.10.1"},
          mine_rate => default
         }).

-define(OLD_NODE2, #{
          name    => old_node2,
          peers   => [old_node1],
          backend => aest_docker,
          source  => {pull, "aeternity/epoch:v0.10.1"},
          mine_rate => default
}).

-define(NEW_NODE1(H), #{
          name    => new_node1,
          peers   => [],
          backend => aest_docker,
          source  => {pull, ?TESTED_DOCKER_IMAGE},
          mine_rate => default,
          hard_forks => ?PROTOCOLS(H)
         }).

-define(NEW_NODE2(H), #{
          name    => new_node2,
          peers   => [new_node1],
          backend => aest_docker,
          source  => {pull, ?TESTED_DOCKER_IMAGE},
          mine_rate => default,
          hard_forks => ?PROTOCOLS(H)
         }).

-define(NEW_NODE3(H), #{
          name    => new_node3,
          peers   => [new_node1],
          backend => aest_docker,
          source  => {pull, ?TESTED_DOCKER_IMAGE},
          mine_rate => default,
          hard_forks => ?PROTOCOLS(H)
         }).

-define(NEW_NODE4(H), #{
          name    => new_node4,
          peers   => [new_node3],
          backend => aest_docker,
          source  => {pull, ?TESTED_DOCKER_IMAGE},
          mine_rate => default,
          hard_forks => ?PROTOCOLS(H)
         }).

-define(FAST_NEW_NODE1(H), #{
          name    => fast_new_node1,
          peers   => [],
          backend => aest_docker,
          source  => {pull, ?TESTED_DOCKER_IMAGE},
          mine_rate => 1000,
          cuckoo_miner => ?CUCKOO_MINER(16),
          hard_forks => ?PROTOCOLS(H)
         }).

-define(FAST_NEW_NODE2(H), #{
          name    => fast_new_node2,
          peers   => [fast_new_node1],
          backend => aest_docker,
          source  => {pull, ?TESTED_DOCKER_IMAGE},
          mine_rate => 1000,
          cuckoo_miner => ?CUCKOO_MINER(16),
          hard_forks => ?PROTOCOLS(H)
         }).

%=== COMMON TEST FUNCTIONS =====================================================

all() ->
    [
     {group, assumptions},
     {group, hard_fork},
     {group, hard_fork_with_tx}
    ].

groups() ->
    [
     {assumptions,
      [
       {genesis,
        [sequence], %% Hard deps among tests.
        [
         old_node_persisting_chain_and_not_mining_has_genesis_as_top,
         new_node_persisting_chain_and_not_mining_has_same_old_genesis_as_top
        ]},
       new_nodes_can_mine_and_sync_fast_minimal_chain_with_pow
      ]},
     {hard_fork,
      [sequence], %% Hard deps among tests/groups.
      [
       restore_db_backup_on_old_node, %% Determines info of top of chain in DB backup.
       {group, hard_fork_all}
      ]},
     {hard_fork_all,
      [sequence], %% Soft deps among tests/groups: if a test/group fails better skipping the rest.
      [
       old_node_can_receive_chain_from_other_old_node,
       {group, upgrade_flow_smoke_test}
      ]},
     {upgrade_flow_smoke_test,
      [sequence], %% Hard deps among tests/groups.
      [
       restore_db_backup_with_short_chain_on_new_node,
       new_node_accepts_long_old_chain_from_other_new_node_up_to_height_of_new_protocol,
       new_node_can_receive_short_old_chain_from_other_new_node,
       new_node_can_mine_on_old_chain_using_old_protocol,
       new_node_can_mine_on_old_chain_using_new_protocol
      ]},
     {hard_fork_with_tx,
      [sequence],
      [
       restore_db_backup_on_old_node, %% Determines info of top of chain in DB backup.
       {group, hard_fork_all_with_tx}
      ]},
     {hard_fork_all_with_tx,
      [sequence],
      [
       {group, hard_fork_old_chain_with_tx},
       {group, hard_fork_new_chain_with_tx}
      ]},
     {hard_fork_old_chain_with_tx,
      [sequence],
      [
       restore_db_backup_with_short_chain_on_new_node,
       new_node_can_mine_spend_tx_on_old_chain_using_old_protocol
      ]},
     {hard_fork_new_chain_with_tx,
      [sequence],
      [
       restore_db_backup_with_short_chain_on_new_node,
       new_node_can_mine_spend_tx_on_old_chain_using_new_protocol
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

init_per_group(assumptions, Config) -> Config;
init_per_group(genesis, Config) -> Config;
init_per_group(hard_fork, Config) -> Config;
init_per_group(hard_fork_with_tx, Config) -> Config;
init_per_group(Group, Config)
  when Group == hard_fork_all; Group == hard_fork_all_with_tx ->
    {_, {restore_db_backup_on_old_node, SavedCfg}} =
        proplists:lookup(saved_config, Config),
    [{_, _} = proplists:lookup(db_backup_top_height, SavedCfg),
     {_, _} = proplists:lookup(db_backup_top_hash, SavedCfg)
     | Config];
init_per_group(upgrade_flow_smoke_test, Config) ->
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Config),
    NewConfig = aest_nodes:ct_setup(Config),
    aest_nodes:setup_nodes(
      [?NEW_NODE1(?HEIGHT_OF_NEW_PROTOCOL(TopHeight)),
       ?NEW_NODE2(?HEIGHT_OF_NEW_PROTOCOL_FOR_VALIDATING_BLOCKS(TopHeight)),
       ?NEW_NODE3(?HEIGHT_OF_NEW_PROTOCOL(TopHeight)),
       ?NEW_NODE4(?HEIGHT_OF_NEW_PROTOCOL(TopHeight))], NewConfig),
    NewConfig;
init_per_group(hard_fork_old_chain_with_tx, Config) ->
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Config),
    NewConfig = aest_nodes:ct_setup(Config),
    aest_nodes:setup_nodes(
      [?NEW_NODE1(?HEIGHT_OF_NEW_PROTOCOL_UNREACHABLE(TopHeight)),
       ?NEW_NODE3(?HEIGHT_OF_NEW_PROTOCOL_UNREACHABLE(TopHeight)),
       ?NEW_NODE4(?HEIGHT_OF_NEW_PROTOCOL_UNREACHABLE(TopHeight))], NewConfig),
    NewConfig;
init_per_group(hard_fork_new_chain_with_tx, Config) ->
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Config),
    NewConfig = aest_nodes:ct_setup(Config),
    aest_nodes:setup_nodes(
      [?NEW_NODE1(?HEIGHT_OF_NEW_PROTOCOL(TopHeight)),
       ?NEW_NODE3(?HEIGHT_OF_NEW_PROTOCOL(TopHeight)),
       ?NEW_NODE4(?HEIGHT_OF_NEW_PROTOCOL(TopHeight))], NewConfig),
    NewConfig.

end_per_group(assumptions, _) -> ok;
end_per_group(genesis, _) -> ok;
end_per_group(hard_fork, _) -> ok;
end_per_group(hard_fork_all, _) -> ok;
end_per_group(hard_fork_with_tx, _) -> ok;
end_per_group(hard_fork_all_with_tx, _) -> ok;
end_per_group(upgrade_flow_smoke_test, Config) ->
    aest_nodes:ct_cleanup(Config),
    ok;
end_per_group(hard_fork_old_chain_with_tx, Config) ->
    aest_nodes:ct_cleanup(Config),
    ok;
end_per_group(hard_fork_new_chain_with_tx, Config) ->
    aest_nodes:ct_cleanup(Config),
    ok.

init_per_testcase(old_node_persisting_chain_and_not_mining_has_genesis_as_top, Config) ->
    aest_nodes:ct_setup(Config);
init_per_testcase(new_node_persisting_chain_and_not_mining_has_same_old_genesis_as_top, Config) ->
    aest_nodes:ct_setup(Config);
init_per_testcase(new_nodes_can_mine_and_sync_fast_minimal_chain_with_pow, Config) ->
    aest_nodes:ct_setup(Config);
init_per_testcase(restore_db_backup_on_old_node, Config) ->
    aest_nodes:ct_setup(Config);
init_per_testcase(old_node_can_receive_chain_from_other_old_node, Config) ->
    aest_nodes:ct_setup(Config);
init_per_testcase(restore_db_backup_with_short_chain_on_new_node, Config) -> Config;
init_per_testcase(new_node_accepts_long_old_chain_from_other_new_node_up_to_height_of_new_protocol, Config) -> Config;
init_per_testcase(new_node_can_receive_short_old_chain_from_other_new_node, Config) -> Config;
init_per_testcase(new_node_can_mine_on_old_chain_using_old_protocol, Config) -> Config;
init_per_testcase(new_node_can_mine_on_old_chain_using_new_protocol, Config) -> Config;
init_per_testcase(new_node_can_mine_spend_tx_on_old_chain_using_old_protocol, Config) -> Config;
init_per_testcase(new_node_can_mine_spend_tx_on_old_chain_using_new_protocol, Config) -> Config.

end_per_testcase(old_node_persisting_chain_and_not_mining_has_genesis_as_top, Config) ->
    aest_nodes:ct_cleanup(Config);
end_per_testcase(new_node_persisting_chain_and_not_mining_has_same_old_genesis_as_top, Config) ->
    aest_nodes:ct_cleanup(Config);
end_per_testcase(new_nodes_can_mine_and_sync_fast_minimal_chain_with_pow, Config) ->
    aest_nodes:ct_cleanup(Config);
end_per_testcase(restore_db_backup_on_old_node, Config) ->
    aest_nodes:ct_cleanup(Config);
end_per_testcase(old_node_can_receive_chain_from_other_old_node, Config) ->
    aest_nodes:ct_cleanup(Config);
end_per_testcase(restore_db_backup_with_short_chain_on_new_node, _) -> ok;
end_per_testcase(new_node_accepts_long_old_chain_from_other_new_node_up_to_height_of_new_protocol, _) -> ok;
end_per_testcase(new_node_can_receive_short_old_chain_from_other_new_node, _) -> ok;
end_per_testcase(new_node_can_mine_on_old_chain_using_old_protocol, _) -> ok;
end_per_testcase(new_node_can_mine_on_old_chain_using_new_protocol, _) -> ok;
end_per_testcase(new_node_can_mine_spend_tx_on_old_chain_using_old_protocol, _) -> ok;
end_per_testcase(new_node_can_mine_spend_tx_on_old_chain_using_new_protocol, _) -> ok.

%=== TEST CASES ================================================================

old_node_persisting_chain_and_not_mining_has_genesis_as_top(Cfg) ->
    aest_nodes:setup_nodes([?OLD_NODE1, ?OLD_NODE2], Cfg),
    start_node(old_node1, Cfg),
    #{height := 0} = get_block_by_height(old_node1, 0, Cfg),
    #{height := 0, hash := Hash} = aest_nodes:get_top(old_node1, Cfg),
    aest_nodes:kill_node(old_node1, Cfg),
    {save_config,
     [{genesis_hash, Hash}]}.

new_node_persisting_chain_and_not_mining_has_same_old_genesis_as_top(Cfg) ->
    {_, {_Saver, SavedCfg}} = proplists:lookup(saved_config, Cfg),
    {_, Hash} = proplists:lookup(genesis_hash, SavedCfg),
    aest_nodes:setup_nodes([?NEW_NODE1(42)], Cfg),
    start_node(new_node1, Cfg),
    #{height := 0} = get_block_by_height(new_node1, 0, Cfg),
    #{height := 0, hash := NewHash} = aest_nodes:get_top(new_node1, Cfg),
    ?assertEqual(Hash, NewHash),
    aest_nodes:kill_node(new_node1, Cfg),
    ok.

new_nodes_can_mine_and_sync_fast_minimal_chain_with_pow(Cfg) ->
    aest_nodes:setup_nodes([?FAST_NEW_NODE1(25), ?FAST_NEW_NODE2(25)], Cfg),
    start_node(fast_new_node1, Cfg),
    run_erl_cmd_on_node(fast_new_node1, "aec_conductor:start_mining().", "ok", Cfg), %% It would be better to configure node to autostart mining in the first place.
    wait_for_height_syncing(20, [fast_new_node1], {{45000, ms}, {5, blocks}}, Cfg),
    start_node(fast_new_node2, Cfg),
    #{hash := HashMined} = get_block_by_height(fast_new_node1, 20, Cfg),
    wait_for_height_syncing(20, [fast_new_node2], {{45000, ms}, {5, blocks}}, Cfg),
    ?assertEqual(HashMined, maps:get(hash, get_block_by_height(fast_new_node2, 20, Cfg))),
    ok.

%% Old node can restore DB backup of testnet.
restore_db_backup_on_old_node(Cfg) ->
    aest_nodes:setup_nodes([?OLD_NODE1, ?OLD_NODE2], Cfg),
    start_node(old_node1, Cfg),
    {ok, {TopHash, TopHeight}} = restore_db_backup_on_node(old_node1, Cfg),
    ?assertMatch(X when is_integer(X) andalso X > 0, TopHeight),
    B = get_block_by_height(old_node1, TopHeight, Cfg),
    ?assertEqual(TopHash, maps:get(hash, B)),
    aest_nodes:kill_node(old_node1, Cfg),
    {save_config,
     [{db_backup_top_height, TopHeight},
      {db_backup_top_hash, TopHash}]}.

%% Sanity check on software version supporting only old protocol:
%%
%% Old node can receive (sync) chain from other old node that restored
%% DB from backup.
old_node_can_receive_chain_from_other_old_node(Cfg) ->
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Cfg),
    {_, TopHash} = proplists:lookup(db_backup_top_hash, Cfg),
    aest_nodes:setup_nodes([?OLD_NODE1, ?OLD_NODE2], Cfg),
    start_node(old_node1, Cfg),
    {ok, {TopHash, TopHeight}} = restore_db_backup_on_node(old_node1, Cfg),
    start_node(old_node2, Cfg),
    aest_nodes:wait_for_value({height, TopHeight}, [old_node1], 5000, Cfg),
    wait_for_height_syncing(TopHeight, [old_node2], {{45000, ms}, {200, blocks}}, Cfg),
    B = get_block_by_height(old_node2, TopHeight, Cfg),
    ?assertEqual(TopHash, maps:get(hash, B)),
    aest_nodes:kill_node(old_node2, Cfg),
    aest_nodes:kill_node(old_node1, Cfg),
    ok.

%% Check on capability of software version supporting both old and new
%% version of the protocol to restore DB backup of testnet (old
%% chain):
%%
%% New node can restore DB backup of testnet - case old chain of
%% height lower than height at which new protocol is effective.
restore_db_backup_with_short_chain_on_new_node(Cfg) ->
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Cfg),
    {_, TopHash} = proplists:lookup(db_backup_top_hash, Cfg),
    start_node(new_node1, Cfg),
    ?assertEqual({ok, {TopHash, TopHeight}},
                 restore_db_backup_on_node(new_node1, Cfg)),
    B = get_block_by_height(new_node1, TopHeight, Cfg),
    ?assertEqual(TopHash, maps:get(hash, B)),
    {save_config,
     [{new_node_left_running_with_old_chain, new_node1}]}.

%% Check on capability of software version supporting both old and new
%% version of the protocol to restore DB backup of testnet (old
%% chain):
%%
%% New node accepts (sync) from other new node that restored DB from
%% backup old chain only up to configured height for new protocol.
new_node_accepts_long_old_chain_from_other_new_node_up_to_height_of_new_protocol(Cfg) ->
    {_, {_Saver, SavedCfg}} = proplists:lookup(saved_config, Cfg),
    {_, new_node1} = proplists:lookup(new_node_left_running_with_old_chain, SavedCfg),
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Cfg),
    {_, _TopHash} = proplists:lookup(db_backup_top_hash, Cfg),
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
    aest_nodes:wait_for_value({height, TopHeight}, [new_node1], 5000, Cfg),
    HeightOfNewProtocolForValidatingBlocks =
        ?HEIGHT_OF_NEW_PROTOCOL_FOR_VALIDATING_BLOCKS(TopHeight),
    start_node(new_node2, Cfg),
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
    aest_nodes:kill_node(new_node2, Cfg),
    {save_config,
     [{new_node_left_running_with_old_chain, new_node1}]}.

%% Check on capability of software version supporting both old and new
%% version of the protocol to restore DB backup of testnet (old
%% chain):
%%
%% New node can receive (sync) old chain from other new node that
%% restored DB from backup - case old chain of height lower than
%% height of new protocol.
new_node_can_receive_short_old_chain_from_other_new_node(Cfg) ->
    {_, {_Saver, SavedCfg}} = proplists:lookup(saved_config, Cfg),
    {_, new_node1} = proplists:lookup(new_node_left_running_with_old_chain, SavedCfg),
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Cfg),
    {_, TopHash} = proplists:lookup(db_backup_top_hash, Cfg),
    aest_nodes:wait_for_value({height, TopHeight}, [new_node1], 5000, Cfg),
    start_node(new_node3, Cfg),
    wait_for_height_syncing(TopHeight, [new_node3], {{45000, ms}, {200, blocks}}, Cfg),
    B = get_block_by_height(new_node3, TopHeight, Cfg),
    ?assertEqual(TopHash, maps:get(hash, B)),
    aest_nodes:kill_node(new_node1, Cfg),
    {save_config,
     [{new_node_left_running_with_synced_old_chain, new_node3}]}.

%% Check on capability of software version supporting both old and new
%% version of the protocol to mine using old protocol:
%%
%% New node can mine on top of short old chain up to max effective
%% height of old protocol.
new_node_can_mine_on_old_chain_using_old_protocol(Cfg) ->
    {_, {_Saver, SavedCfg}} = proplists:lookup(saved_config, Cfg),
    {_, new_node3} = proplists:lookup(new_node_left_running_with_synced_old_chain, SavedCfg),
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Cfg),
    {_, _TopHash} = proplists:lookup(db_backup_top_hash, Cfg),
    HeightOfNewProtocol = ?HEIGHT_OF_NEW_PROTOCOL(TopHeight),
    #{version := ?GENESIS_PROTOCOL_VERSION,
      height := TopHeight} = aest_nodes:get_top(new_node3, Cfg),
    HeightToBeMinedWithOldProtocol = - 1 + HeightOfNewProtocol,
    {true, _} = {HeightToBeMinedWithOldProtocol > TopHeight,
                 {check_at_least_a_block_to_mine_using_old_protocol,
                  HeightToBeMinedWithOldProtocol}},
    {ok, 404, _} = aest_nodes:http_get(new_node3, int_http, [v2, block, height, HeightToBeMinedWithOldProtocol], #{}, Cfg),
    start_node(new_node4, Cfg),
    wait_for_height_syncing(TopHeight, [new_node4], {{45000, ms}, {200, blocks}}, Cfg),
    ok = mock_pow_on_node(new_node3, Cfg), %% TODO Make configurable.
    ok = mock_pow_on_node(new_node4, Cfg), %% TODO Make configurable.
    run_erl_cmd_on_node(new_node3, "aec_conductor:start_mining().", "ok", Cfg), %% It would be better to: stop container, reinstantiate config template, start container.
    wait_for_height_syncing(HeightToBeMinedWithOldProtocol, [new_node3], {{10000, ms}, {1000, blocks}}, Cfg),
    #{version := ?GENESIS_PROTOCOL_VERSION,
      hash := HashMined} = get_block_by_height(new_node3, HeightToBeMinedWithOldProtocol, Cfg),
    %% Ensure distinct non-mining node can sync mined block(s).
    wait_for_height_syncing(HeightToBeMinedWithOldProtocol, [new_node4], {{45000, ms}, {200, blocks}}, Cfg),
    ?assertEqual(HashMined, maps:get(hash, get_block_by_height(new_node4, HeightToBeMinedWithOldProtocol, Cfg))),
    {save_config,
     [{new_node_left_mining_with_mined_old_protocol, new_node3},
      {new_node_left_running_with_mined_old_protocol, new_node4}]}.

%% Check on capability of software version supporting both old and new
%% version of the protocol to mine using new protocol:
%%
%% New node can mine on top of old chain further to max effective
%% height of old protocol.
new_node_can_mine_on_old_chain_using_new_protocol(Cfg) ->
    {_, {_Saver, SavedCfg}} = proplists:lookup(saved_config, Cfg),
    {_, new_node3} = proplists:lookup(new_node_left_mining_with_mined_old_protocol, SavedCfg),
    {_, new_node4} = proplists:lookup(new_node_left_running_with_mined_old_protocol, SavedCfg),
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Cfg),
    {_, _TopHash} = proplists:lookup(db_backup_top_hash, Cfg),
    HeightOfNewProtocol = ?HEIGHT_OF_NEW_PROTOCOL(TopHeight),
    HeightToBeMinedWithNewProtocol = 1 + HeightOfNewProtocol, %% I.e. two blocks with new protocol.
    wait_for_height_syncing(HeightToBeMinedWithNewProtocol, [new_node3], {{10000, ms}, {1000, blocks}}, Cfg),
    #{version := ?NEW_PROTOCOL_VERSION,
      hash := HashMined} = get_block_by_height(new_node3, HeightToBeMinedWithNewProtocol, Cfg),
    %% Ensure distinct non-mining node can sync mined block(s).
    wait_for_height_syncing(HeightToBeMinedWithNewProtocol, [new_node4], {{45000, ms}, {1000, blocks}}, Cfg),
    ?assertEqual(HashMined, maps:get(hash, get_block_by_height(new_node4, HeightToBeMinedWithNewProtocol, Cfg))),
    aest_nodes:kill_node(new_node4, Cfg),
    aest_nodes:kill_node(new_node3, Cfg),
    ok.

%% New node can sync the old chain from other new node and can start mining
%% on the top of the old chain. The new node can mine blocks using the old
%% protocol and include spend transaction in the blocks. The block with the
%% spend transction can be synced among other new nodes.
new_node_can_mine_spend_tx_on_old_chain_using_old_protocol(Cfg) ->
    {_, {_Saver, SavedCfg}} = proplists:lookup(saved_config, Cfg),
    {_, new_node1} = proplists:lookup(new_node_left_running_with_old_chain, SavedCfg),
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Cfg),
    {_, TopHash} = proplists:lookup(db_backup_top_hash, Cfg),
    %% Sync the chain on new_node3 from new_node1.
    start_node(new_node3, Cfg),
    wait_for_height_syncing(TopHeight, [new_node3], {{45000, ms}, {200, blocks}}, Cfg),
    #{hash := TopHash} = get_block_by_height(new_node3, TopHeight, Cfg),
    %% Get public key of new_node1 and make sure its balance is 0.
    PubKey1 = get_public_key(new_node1, Cfg),
    Balance1 = get_balance(new_node1, PubKey1, Cfg),
    ?assertEqual(0, Balance1),
    ct:log("Balance of node ~p with public key ~p is ~p", [new_node1, PubKey1, Balance1]),
    %% Kill new_node1, it was used just to sync the old chain.
    aest_nodes:kill_node(new_node1, Cfg),
    %% Get public key of new_node3 and make sure its balance is 0.
    PubKey3 = get_public_key(new_node3, Cfg),
    Balance3 = get_balance(new_node3, PubKey3, Cfg),
    ?assertEqual(0, Balance3),
    ct:log("Balance of node ~p with public key ~p is ~p", [new_node3, PubKey3, Balance3]),
    %% Start mining on new_node3.
    ok = mock_pow_on_node(new_node3, Cfg),
    run_erl_cmd_on_node(new_node3, "aec_conductor:start_mining().", "ok", Cfg),
    %% Check that new_node3 got reward for mining.
    MinedReward = 100,
    aest_nodes:wait_for_value({balance, PubKey3, MinedReward}, [new_node3], 10000, Cfg),
    MinedBalance = get_balance(new_node3, PubKey3, Cfg),
    ?assert(MinedBalance >= MinedReward),
    ct:log("Mined balance on ~p with public key ~p is ~p", [new_node3, PubKey3, MinedBalance]),
    %% Send spend transaction from new_node3 to new_node1.
    Fee = 5,
    BalanceToSpend = MinedBalance - Fee,
    ok = post_spend_tx(new_node3, PubKey1, BalanceToSpend, Fee, Cfg),
    ct:log("Sent spend tx with balance of ~p from node ~p with public key ~p to node ~p with public key ~p",
           [BalanceToSpend, new_node3, PubKey3, new_node1, PubKey1]),
    %% Make sure new_node1 received the balance.
    aest_nodes:wait_for_value({balance, PubKey1, BalanceToSpend}, [new_node3], 20000, Cfg),
    ReceivedBalance = get_balance(new_node3, PubKey1, Cfg),
    ct:log("Balance of node ~p with public key ~p is ~p", [new_node1, PubKey1, ReceivedBalance]),
    ?assertEqual(BalanceToSpend, ReceivedBalance),
    %% new_node1 has one spend transaction.
    [#{block_hash := SpendTxBlockHash, block_height := SpendTxHeight, tx := TxInfo}] =
        get_account_txs(new_node3, PubKey1, Cfg),
    ?assertEqual(ReceivedBalance, maps:get(amount, TxInfo)),
    ?assertEqual(Fee, maps:get(fee, TxInfo)),
    ?assertEqual(PubKey3, maps:get(sender, TxInfo)),
    ?assertEqual(PubKey1, maps:get(recipient, TxInfo)),
    %% Block with spend transaction has genesis version.
    ?assertEqual(9, maps:get(version, get_block_by_hash(new_node3, SpendTxBlockHash, Cfg))),
    %% Sync the chain with the block that includes spend tx on new_node4.
    start_node(new_node4, Cfg),
    ok = mock_pow_on_node(new_node4, Cfg),
    wait_for_height_syncing(SpendTxHeight, [new_node4], {{100000, ms}, {1000, blocks}}, Cfg),
    ?assertEqual(SpendTxBlockHash, maps:get(hash, get_block_by_height(new_node4, SpendTxHeight, Cfg))),
    ok.

%% New node can sync the old chain from other new node and can start mining
%% on the top of the old chain until a certain height where it switches to the
%% new protocol. The new node can mine blocks using the new protocol and include
%% spend transaction in the blocks. The block with spend transaction can be
%% synced among other new nodes.
new_node_can_mine_spend_tx_on_old_chain_using_new_protocol(Cfg) ->
    {_, {_Saver, SavedCfg}} = proplists:lookup(saved_config, Cfg),
    {_, new_node1} = proplists:lookup(new_node_left_running_with_old_chain, SavedCfg),
    {_, TopHeight} = proplists:lookup(db_backup_top_height, Cfg),
    {_, TopHash} = proplists:lookup(db_backup_top_hash, Cfg),
    HeightOfNewProtocol = ?HEIGHT_OF_NEW_PROTOCOL(TopHeight),
    LastHeightOfOldProtocol = HeightOfNewProtocol - 1,
    %% Sync the chain on new_node3 from new_node1.
    start_node(new_node3, Cfg),
    wait_for_height_syncing(TopHeight, [new_node3], {{45000, ms}, {200, blocks}}, Cfg),
    #{hash := TopHash} = get_block_by_height(new_node3, TopHeight, Cfg),
    %% Get public key of new_node1 and make sure its balance is 0.
    PubKey1 = get_public_key(new_node1, Cfg),
    Balance1 = get_balance(new_node1, PubKey1, Cfg),
    ?assertEqual(0, Balance1),
    ct:log("Balance of node ~p with public key ~p is ~p", [new_node1, PubKey1, Balance1]),
    %% Kill new_node1, it was used just to sync the old chain.
    aest_nodes:kill_node(new_node1, Cfg),
    %% Get public key of new_node3 and make sure its balance is 0.
    PubKey3 = get_public_key(new_node3, Cfg),
    Balance3 = get_balance(new_node3, PubKey3, Cfg),
    ?assertEqual(0, Balance3),
    ct:log("Balance of node ~p with public key ~p is ~p", [new_node3, PubKey3, Balance3]),
    %% Start mining on new_node3.
    ok = mock_pow_on_node(new_node3, Cfg),
    run_erl_cmd_on_node(new_node3, "aec_conductor:start_mining().", "ok", Cfg),
%    %% Check the last block of old protocol has genesis version.
    wait_for_height_syncing(LastHeightOfOldProtocol, [new_node3], {{10000, ms}, {1000, blocks}}, Cfg),
    B3OldProtocol = get_block_by_height(new_node3, LastHeightOfOldProtocol, Cfg),
    ?assertEqual(9, maps:get(version, B3OldProtocol)),
    %% Check the first block of new protocol has version of the new protocol.
    wait_for_height_syncing(HeightOfNewProtocol, [new_node3], {{10000, ms}, {1000, blocks}}, Cfg),
    B3NewProtocol = get_block_by_height(new_node3, HeightOfNewProtocol, Cfg),
    ?assertEqual(10, maps:get(version, B3NewProtocol)),
    %% Check that new_node3 got reward for mining.
    MinedReward = 100,
    aest_nodes:wait_for_value({balance, PubKey3, MinedReward}, [new_node3], 10000, Cfg),
    MinedBalance = get_balance(new_node3, PubKey3, Cfg),
    ?assert(MinedBalance >= MinedReward),
    ct:log("Mined balance on ~p with public key ~p is ~p", [new_node3, PubKey3, MinedBalance]),
    %% Send spend transaction from new_node3 to new_node1.
    Fee = 5,
    BalanceToSpend = MinedBalance - Fee,
    ok = post_spend_tx(new_node3, PubKey1, BalanceToSpend, Fee, Cfg),
    ct:log("Sent spend tx with balance of ~p from node ~p with public key ~p to node ~p with public key ~p",
           [BalanceToSpend, new_node3, PubKey3, new_node1, PubKey1]),
    %% Make sure new_node1 received the balance.
    aest_nodes:wait_for_value({balance, PubKey1, BalanceToSpend}, [new_node3], 20000, Cfg),
    ReceivedBalance = get_balance(new_node3, PubKey1, Cfg),
    ct:log("Balance of node ~p with public key ~p is ~p", [new_node1, PubKey1, ReceivedBalance]),
    ?assertEqual(BalanceToSpend, ReceivedBalance),
    %% new_node1 has one spend transaction.
    [#{block_hash := SpendTxBlockHash, block_height := SpendTxHeight, tx := TxInfo}] =
        get_account_txs(new_node3, PubKey1, Cfg),
    ?assertEqual(ReceivedBalance, maps:get(amount, TxInfo)),
    ?assertEqual(Fee, maps:get(fee, TxInfo)),
    ?assertEqual(PubKey3, maps:get(sender, TxInfo)),
    ?assertEqual(PubKey1, maps:get(recipient, TxInfo)),
    %% Block with spend transaction has new protocol version.
    ?assertEqual(10, maps:get(version, get_block_by_hash(new_node3, SpendTxBlockHash, Cfg))),
    %% Sync the chain with the block that includes spend tx on new_node4.
    start_node(new_node4, Cfg),
    ok = mock_pow_on_node(new_node4, Cfg),
    wait_for_height_syncing(SpendTxHeight, [new_node4], {{100000, ms}, {1000, blocks}}, Cfg),
    ?assertEqual(SpendTxBlockHash, maps:get(hash, get_block_by_height(new_node4, SpendTxHeight, Cfg))),
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
    ErlCmd =
        "{atomic, [_|_] = Tabs} = mnesia:restore(\"" ++ Dest ++ "\", []), ok.",
    run_erl_cmd_on_node(NodeName, ErlCmd, "ok", Cfg),
    Top = #{height := TopHeight,
            hash := TopHash} = aest_nodes:get_top(NodeName, Cfg),
    ct:log("Restored DB backup on node ~s, whose top is now~n~p",
           [NodeName, Top]),
    {ok, {TopHash, TopHeight}}.

mock_pow_on_node(NodeName, Cfg) ->
    S =
        "-module(aec_pow_cuckoo). "
        "-export([generate/3, verify/4]). "
        "generate(_, _, Nonce) -> Evd = lists:duplicate(42, 0), {ok, {Nonce, Evd}}. "
        "verify(_,_,_,_) -> true.",
    load_module_on_node(NodeName, aec_pow_cuckoo, S, Cfg).

load_module_on_node(NodeName, Module, String, Cfg) ->
    ct:log("Module ~s:~n~s", [Module, String]),
    Tokens = dot_ending_token_lists(String),
    ct:log("Tokens:~n~p", [Tokens]),
    Forms = to_forms(Tokens),
    ct:log("Forms:~n~p", [Forms]),
    {ok, Module, Binary, []} = compile:forms(Forms, [return_errors,
                                                     return_warnings]),
    ErlCmd =
        lists:flatten(
          io_lib:format(
            "{module, _} = code:load_binary(~s, \"Dummy Filename\", ~w), ok.",
            [Module, Binary])),
    run_erl_cmd_on_node(NodeName, ErlCmd, "ok", Cfg),
    ok.

dot_ending_token_lists(Chars) ->
    (fun
         F(ContinuationIn, LeftOverCharsIn, TokenListsIn) ->
             case erl_scan:tokens(ContinuationIn, LeftOverCharsIn, 0) of
                 {done, {eof, _EndLocation}, _} ->
                     TokenListsIn;
                 {done, {ok, Tokens, _EndLocation}, LeftOverCharsOut} ->
                     F([], LeftOverCharsOut, TokenListsIn ++ [Tokens]);
                 {more, ContinuationOut} ->
                     F(ContinuationOut, eof, TokenListsIn)
             end
     end)([], Chars, []).

to_forms(DotEndingTokenLists) ->
    lists:map(fun(Ts) -> {ok, F} = erl_parse:parse_form(Ts), F end,
              DotEndingTokenLists).

run_erl_cmd_on_node(NodeName, ErlCmd, ExpectedOutput, Cfg) ->
    ct:log("Running Erlang command on node ~s:~n~s~nExpecting: ~s",
           [NodeName, ErlCmd, ExpectedOutput]),
    Cmd = ["bin/epoch", "eval", "'" ++ ErlCmd ++ "'"],
    ExpectedOutput = aest_nodes:run_cmd_in_node_dir(NodeName, Cmd, Cfg),
    ct:log("Run Erlang command on node ~s", [NodeName]),
    ok.

get_version(NodeName, Cfg) ->
    {ok, 200, B} = aest_nodes:http_get(NodeName, ext_http, [v2, version], #{}, Cfg),
    B.

get_block_by_height(NodeName, Height, Cfg) ->
    {ok, 200, B} = aest_nodes:http_get(NodeName, int_http, [v2, block, height, Height], #{}, Cfg),
    B.

get_block_by_hash(NodeName, Hash, Cfg) ->
    {ok, 200, B} = aest_nodes:http_get(NodeName, int_http, [v2, block, hash, Hash], #{}, Cfg),
    B.

get_public_key(NodeName, Cfg) ->
    {ok, 200, #{pub_key := PubKey}} = aest_nodes:http_get(NodeName, int_http, [v2, account, 'pub-key'], #{}, Cfg),
    PubKey.

get_balance(NodeName, PubKey, Cfg) ->
    case aest_nodes:http_get(NodeName, ext_http, [v2, account, balance, PubKey], #{}, Cfg) of
        {ok, 404, #{reason := <<"Account not found">>}} -> 0;
        {ok, 200, #{balance := Balance}} -> Balance
    end.

get_account_txs(NodeName, PubKey, Cfg) ->
    Params = #{tx_types => spend_tx, tx_encoding => json},
    {ok, 200, #{transactions := Txs}} =
        aest_nodes:http_get(NodeName, ext_http, [v2, account, txs, PubKey], Params, Cfg),
    Txs.

post_spend_tx(NodeName, Recipient, Amount, Fee, Cfg) ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Body = jsx:encode(#{
            recipient_pubkey => Recipient,
            amount => Amount,
            fee => Fee}),
    {ok, 200, #{}} = aest_nodes:http_post(NodeName, int_http, [v2, 'spend-tx'], #{}, Headers, Body, Cfg),
    ok.

wait_for_height_syncing(MinHeight, NodeNames, {{Timeout, ms}, {Blocks, blocks}}, Cfg) ->
    WaitF =
        fun(H) ->
                ct:log("Waiting for height ~p for ~p ms on nodes ~p...", [H, Timeout, NodeNames]),
                aest_nodes:wait_for_value({height, H}, NodeNames, Timeout, Cfg),
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
