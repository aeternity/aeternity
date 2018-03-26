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
         new_node_accepts_long_old_chain_from_other_new_node_up_to_height_of_new_protocol/1,
         new_node_can_receive_short_old_chain_from_other_new_node/1,
         new_node_can_mine_on_old_chain_using_old_protocol/1,
         new_node_can_mine_on_old_chain_using_new_protocol/1
        ]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(DB_BACKUP_DEST_DIR, "/tmp/mnesia_backup").

-define(PROTOCOLS(H), #{9 => 0, 10 => H}).

-define(HEIGHT_OF_NEW_PROTOCOL(OldChainHeight),
        (2 + OldChainHeight)
       ).
-define(HEIGHT_OF_NEW_PROTOCOL_FOR_VALIDATING_BLOCKS(OldChainHeight),
        (- 3 + OldChainHeight)
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
          source  => {pull, "aeternity/epoch:local"},
          mine_rate => default,
          hard_forks => ?PROTOCOLS(H)
         }).

-define(NEW_NODE2(H), #{
          name    => new_node2,
          peers   => [new_node1],
          backend => aest_docker,
          source  => {pull, "aeternity/epoch:local"},
          mine_rate => default,
          hard_forks => ?PROTOCOLS(H)
         }).

-define(NEW_NODE3(H), #{
          name    => new_node3,
          peers   => [new_node1],
          backend => aest_docker,
          source  => {pull, "aeternity/epoch:local"},
          mine_rate => default,
          hard_forks => ?PROTOCOLS(H)
         }).

-define(NEW_NODE4(H), #{
          name    => new_node4,
          peers   => [new_node3],
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
init_per_group(hard_fork, Config) -> Config;
init_per_group(hard_fork_all, Config) ->
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
    NewConfig.

end_per_group(assumptions, _) -> ok;
end_per_group(hard_fork, _) -> ok;
end_per_group(hard_fork_all, _) -> ok;
end_per_group(upgrade_flow_smoke_test, Config) ->
    aest_nodes:ct_cleanup(Config),
    ok.

init_per_testcase(old_node_persisting_chain_and_not_mining_has_genesis_as_top, Config) ->
    aest_nodes:ct_setup(Config);
init_per_testcase(restore_db_backup_on_old_node, Config) ->
    aest_nodes:ct_setup(Config);
init_per_testcase(old_node_can_receive_chain_from_other_old_node, Config) ->
    aest_nodes:ct_setup(Config);
init_per_testcase(restore_db_backup_with_short_chain_on_new_node, Config) -> Config;
init_per_testcase(new_node_accepts_long_old_chain_from_other_new_node_up_to_height_of_new_protocol, Config) -> Config;
init_per_testcase(new_node_can_receive_short_old_chain_from_other_new_node, Config) -> Config;
init_per_testcase(new_node_can_mine_on_old_chain_using_old_protocol, Config) -> Config;
init_per_testcase(new_node_can_mine_on_old_chain_using_new_protocol, Config) -> Config.

end_per_testcase(old_node_persisting_chain_and_not_mining_has_genesis_as_top, Config) ->
    aest_nodes:ct_cleanup(Config);
end_per_testcase(restore_db_backup_on_old_node, Config) ->
    aest_nodes:ct_cleanup(Config);
end_per_testcase(old_node_can_receive_chain_from_other_old_node, Config) ->
    aest_nodes:ct_cleanup(Config);
end_per_testcase(restore_db_backup_with_short_chain_on_new_node, _) -> ok;
end_per_testcase(new_node_accepts_long_old_chain_from_other_new_node_up_to_height_of_new_protocol, _) -> ok;
end_per_testcase(new_node_can_receive_short_old_chain_from_other_new_node, _) -> ok;
end_per_testcase(new_node_can_mine_on_old_chain_using_old_protocol, _) -> ok;
end_per_testcase(new_node_can_mine_on_old_chain_using_new_protocol, _) -> ok.

%=== TEST CASES ================================================================

old_node_persisting_chain_and_not_mining_has_genesis_as_top(Cfg) ->
    aest_nodes:setup_nodes([?OLD_NODE1, ?OLD_NODE2], Cfg),
    start_node(old_node1, Cfg),
    #{height := 0} = get_block_by_height(old_node1, 0, Cfg),
    #{height := 0} = aest_nodes:get_top(old_node1, Cfg),
    aest_nodes:kill_node(old_node1, Cfg),
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
    aest_nodes:wait_for_height(TopHeight, [old_node1], 5000, Cfg),
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
    aest_nodes:wait_for_height(TopHeight, [new_node1], 5000, Cfg),
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
    aest_nodes:wait_for_height(TopHeight, [new_node1], 5000, Cfg),
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
    #{height := TopHeight} = aest_nodes:get_top(new_node3, Cfg),
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
    #{hash := HashMined} = get_block_by_height(new_node3, HeightToBeMinedWithOldProtocol, Cfg),
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
    #{hash := HashMined} = get_block_by_height(new_node3, HeightToBeMinedWithNewProtocol, Cfg),
    %% Ensure distinct non-mining node can sync mined block(s).
    wait_for_height_syncing(HeightToBeMinedWithNewProtocol, [new_node4], {{45000, ms}, {1000, blocks}}, Cfg),
    ?assertEqual(HashMined, maps:get(hash, get_block_by_height(new_node4, HeightToBeMinedWithNewProtocol, Cfg))),
    aest_nodes:kill_node(new_node4, Cfg),
    aest_nodes:kill_node(new_node3, Cfg),
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
