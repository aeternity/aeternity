-module(aecore_forking_SUITE).

%% This code is brutaly copied form aecore_sync_SUITE and should use joined code base.

%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [
    create_dev1_chain/1,
    create_dev2_chain/1,
    sync_fork_in_wrong_order/1,
    add_dev3_node/1,
    dev3_failed_attack/1,
    dev3_syncs_to_community/1
   ]).

%% tr_ttb behavior callbacks
-export([ flags/0
        , patterns/0 ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(MINE_RATE, 100).

all() ->
    [
     {group, all_nodes}
    ].

groups() ->
    [
     {all_nodes, [sequence], [{group, two_nodes}, {group, three_nodes}]},
     {two_nodes, [sequence],
      [create_dev1_chain,
       create_dev2_chain,
       sync_fork_in_wrong_order]},
     {three_nodes, [sequence],
      [add_dev3_node,
       dev3_failed_attack,
       dev3_syncs_to_community]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    %% Do not use 'instant_mining', as it short-cuts header validation/whitelist tests
    aecore_suite_utils:init_per_suite([dev1, dev2, dev3],
                                      #{ <<"sync">> =>
                                             #{<<"sync_allowed_height_from_top">> => 0}
                                       , <<"mining">> =>
                                             #{ <<"expected_mine_rate">> => ?MINE_RATE } },
                                      [{add_peers, true}],
                                      [{symlink_name, "latest.fork"},
                                       {test_module, ?MODULE}]
                                      ++ Config).

end_per_suite(Config) ->
    [aecore_suite_utils:stop_node(D, Config) || D <- [dev1, dev2, dev3]],
    ok.

init_per_group(two_nodes, Config) ->
    [{nodes, [aecore_suite_utils:node_tuple(dev1),
              aecore_suite_utils:node_tuple(dev2)]} | Config];
init_per_group(three_nodes, Config) ->
    [{nodes, [aecore_suite_utils:node_tuple(D) ||
                 D <- [dev1, dev2, dev3]]} | Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(dev3_syncs_to_community, _Config) ->
    {skip, "Not yet implemented"};
init_per_testcase(_Case, Config) ->
    ct:log("testcase pid: ~p", [self()]),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

create_dev1_chain(Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    N1 = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect(N1),
    {ok, Blocks} = mine_key_blocks(N1, 20),
    true = (length(lists:usort(Blocks)) >= 20),
    N1Top = rpc:call(N1, aec_chain, top_block, [], 5000),
    ct:log("top of chain dev1: ~p (mined ~p)", [N1Top, lists:last(Blocks)]),
    N1Top = lists:last(Blocks),
    ok = stop_and_check([dev1], Config),    %% make sure we do not sync with dev2.
    ok.

create_dev2_chain(Config) ->
    aecore_suite_utils:start_node(dev2, Config),
    N2 = aecore_suite_utils:node_name(dev2),
    aecore_suite_utils:connect(N2),
    mine_key_blocks(N2, 1),
    ForkTop = rpc:call(N2, aec_chain, top_block, [], 5000),
    ct:log("top of fork dev2: ~p", [ ForkTop ]),
    ok = stop_and_check([dev2], Config),
    ok.

sync_fork_in_wrong_order(Config) ->
    aecore_suite_utils:start_node(dev1, Config),
    N1 = aecore_suite_utils:node_name(dev1),
    aecore_suite_utils:connect(N1),
    N1Top = rpc:call(N1, aec_chain, top_block, [], 5000),
    ct:log("top of chain dev1: ~p", [ N1Top ]),
    aecore_suite_utils:stop_node(dev1, Config),

    aecore_suite_utils:start_node(dev2, Config),
    N2 = aecore_suite_utils:node_name(dev2),
    aecore_suite_utils:connect(N2),
    ForkTop = rpc:call(N2, aec_chain, top_block, [], 5000),
    ct:log("top of chain dev2: ~p", [ ForkTop ]),

    false = (ForkTop == N1Top),
    timer:sleep(100),
    %% unexepctedly last block of dev1 arrives before rest of the chain
    %% This is no longer allowed, so it should fail.
    ?assertMatch({error, {illegal_orphan, _}},
                  rpc:call(N2, aec_conductor, post_block, [N1Top], 5000)),

    T0 = os:timestamp(),
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:connect(N1),
    done = aecore_suite_utils:await_sync_complete(T0, [N2]),
    aec_test_utils:wait_for_it(
      fun() -> rpc:call(N2, aec_chain, top_block, [], 5000) end,
      N1Top),
    ok = stop_and_check([dev1, dev2], Config).

add_dev3_node(Config) ->
    %% dev1 and dev2 are in sync
    [N1, N2, N3] = [aecore_suite_utils:node_name(D) || D <- [dev1, dev2, dev3]],
    T0 = os:timestamp(),
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:connect(N1),
    aecore_suite_utils:start_node(dev2, Config),
    aecore_suite_utils:connect(N2),
    %% N1 and N2 should already be in sync
    done = aecore_suite_utils:await_sync_complete(T0, [N1, N2]),
    Top = rpc:call(N1, aec_chain, top_block, [], 5000),
    Top = rpc:call(N2, aec_chain, top_block, [], 5000),
    T1 = os:timestamp(),
    aecore_suite_utils:start_node(dev3, Config),
    aecore_suite_utils:connect(N3),
    [{N3, true, Ts1}] = aecore_suite_utils:await_is_syncing(T1, [N3]),
    [{N3, false, _}] = aecore_suite_utils:await_is_syncing(Ts1, [N3]),
    %% ok = await_relayed_sync_toggle(false, Sub3),
    %% ok = stop_subscriber(Sub3),
    Top = rpc:call(N3, aec_chain, top_block, [], 5000),
    %% Nodes are all in sync
    ok = stop_and_check([dev1, dev2, dev3], Config),
    ok.

dev3_failed_attack(Config) ->
    [N1, N2, N3] = [aecore_suite_utils:node_name(D) || D <- [dev1, dev2, dev3]],
    %%
    %% Start node N3 (dev3), mine a malicious fork, then stop the node
    %%
    aecore_suite_utils:start_node(dev3, Config),
    aecore_suite_utils:connect(N3),
    %% Mine a fork on N3
    {ok, BlocksN3} = mine_key_blocks(N3, 20),
    N3Top = lists:last(BlocksN3),
    ct:log("top of fork dev3 (N3Top): ~p", [ N3Top ]),
    ok = stop_and_check([dev3], Config),
    %%
    %% restart N1 (dev1), N2 (dev2), sync, mine some blocks and set fork resilience
    %%
    T2 = os:timestamp(),
    aecore_suite_utils:start_node(dev1, Config),
    aecore_suite_utils:connect(N1),
    aecore_suite_utils:start_node(dev2, Config),
    aecore_suite_utils:connect(N2),
    done = aecore_suite_utils:await_sync_complete(T2, [N1, N2]),
    {ok, BlocksN1N2} = mine_key_blocks(N1, 10),  %% fewer than N3
    NewTop = rpc:call(N1, aec_chain, top_block, [], 5000),
    NewTopHeight = aec_blocks:height(NewTop),
    ct:log("top of fork dev1 (NewTop): ~p", [ NewTop ]),
    aec_test_utils:wait_for_it(
      fun() -> rpc:call(N2, aec_chain, top_block, [], 5000) end,
      NewTop),
    SetCfg = #{<<"sync">> => #{<<"sync_allowed_height_from_top">> => 5}},
    NewTop = lists:last(BlocksN1N2),
    [ok = rpc:call(N, aeu_env, update_config, [SetCfg]) || N <- [N1, N2]],
    FHeight = NewTopHeight - 5 - 1,
    {ok, _} = aec_test_utils:wait_for_it_or_timeout(
                fun() -> rpc:call(N1, aec_db, get_finalized_height, [], 1000) end,
                FHeight, 5000),
    {ok, _} = aec_test_utils:wait_for_it_or_timeout(
               fun() -> rpc:call(N2, aec_db, get_finalized_height, [], 1000) end,
               FHeight, 5000),
    ct:log("finalized height (~p) set on N1 and N2", [FHeight]),
    %%
    %% Now, stop dev2 before trying to sync N3 against N1.
    %% What should happen is that N1 rejects N3's fork, despite its greater
    %% difficulty. This rejection should be based on the
    %% `sync_allow_height_from_top` setting, as N1 is up and running.
    %%
    ok = stop_and_check([dev2], Config),
    T3 = os:timestamp(),
    aecore_suite_utils:start_node(dev3, Config),
    aecore_suite_utils:connect(N3),
    [{N1,true,Ts1}] = aecore_suite_utils:await_is_syncing(T3, [N1]),
    ct:log("Sync started on dev1", []),
    [{N1,false,_}] = aecore_suite_utils:await_is_syncing(Ts1, [N1]),
    %% [{N2,false,_}] = aecore_suite_utils:await_is_syncing(Ts2, [N2]),
    ct:log("Sync stopped on dev1", []),
    %% Ensure that dev1 still has the same top.
    NewTop = rpc:call(N1, aec_chain, top_block, [], 5000),
    %% NewTop = rpc:call(N2, aec_chain, top_block, [], 5000),
    false = (N3Top == NewTop),
    %%
    %% Now, start dev2. This checks that N3 can be resisted even by a restarting
    %% node. This rejection is based on the persisted finalized_depth.
    %% Since the YAML config has fork resistance turned off, we pass a config
    %% as an OS env variable.
    %%
    T4 = os:timestamp(),
    Env = [{"AE__SYNC__SYNC_ALLOWED_HEIGHT_FROM_TOP", "5"}],
    aecore_suite_utils:start_node(dev2, Config, Env),
    aecore_suite_utils:connect(N2),
    [{N2,true,Ts2}] = aecore_suite_utils:await_is_syncing(T4, [N2]),
    ct:log("Sync started on dev2", []),
    [{N2,false,_}] = aecore_suite_utils:await_is_syncing(Ts2, [N2]),
    ct:log("Sync stopped on dev2", []),
    NewTop = rpc:call(N2, aec_chain, top_block, [], 5000),
    ct:log("dev2 synced against N1 - not N3", []),
    %%
    %% Stop and restart dev2, but this time without fork resistance.
    %% The finalized depth entry will still be in the DB, but not acted on,
    %% since FR is configured to be disabled. N2 should sync against N3.
    %%
    ok = stop_and_check([dev2], Config),
    T5 = os:timestamp(),
    aecore_suite_utils:start_node(dev2, Config),
    aecore_suite_utils:connect(N2),
    [{N2,true,Ts2_1}] = aecore_suite_utils:await_is_syncing(T5, [N2]),
    ct:log("Sync started on dev2", []),
    [{N2,false,_}] = aecore_suite_utils:await_is_syncing(Ts2_1, [N2]),
    N3Top = rpc:call(N2, aec_chain, top_block, [], 5000),
    ct:log("Without fork resistance, N2 synced against N3", []),
    ok = stop_and_check([dev1, dev2, dev3], Config).


dev3_syncs_to_community(_Config) ->
    %% This has not yet been implemented
    ok.

stop_and_check(Ns, Config) ->
    lists:foreach(
      fun(N) ->
              aecore_suite_utils:stop_node(N, Config)
      end, Ns),
    ok = aecore_suite_utils:check_for_logs(Ns, Config).

mine_key_blocks(Node, N) ->
    aecore_suite_utils:mine_blocks(Node, N, ?MINE_RATE, key, #{}).

with_trace(F, Config, File) ->
    aecore_suite_ttb:with_trace(F, Config, File, ?MODULE).

patterns() ->
    [ {aec_sync, '_', '_', []}
    , {aec_resilience, '_', '_', []}
    | [ {aec_chain_state, F, A, []} || {F, A} <- aec_chain_state:module_info(exports) ]]
    ++ [{aec_chain, F, A, []} || {F, A} <- aec_chain:module_info(exports) ].

flags() ->
    {all, call}.
