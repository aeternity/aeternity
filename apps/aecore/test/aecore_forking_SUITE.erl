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
    add_dev3_node/1,
    dev3_failed_attack/1,
    dev3_syncs_to_community/1,
    start_dev1/1,
    start_dev2/1,
    stop_dev1/1,
    stop_dev2/1,
    mine_a_key_block_on_dev1/1,
    mine_a_key_block_on_dev2/1,
    spend_on_dev1/1,
    spend_on_dev2/1,
    mine_a_micro_block_on_dev1/1,
    mine_a_micro_block_on_dev2/1,
    start_nodes_and_wait_sync_dev1_chain_wins/1,
    start_nodes_and_wait_sync_dev2_chain_wins/1,
    assert_orphaned_tx_in_pool_dev1_receives/1,
    assert_orphaned_tx_in_pool_dev2_receives/1
   ]).

%% tr_ttb behavior callbacks
-export([ flags/0
        , patterns/0 ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(MINE_RATE, 100).
-define(SPEND_FEE, 20000 * aec_test_utils:min_gas_price()).

all() ->
    [
     {group, all_nodes}
    ].

groups() ->
    [
     {all_nodes, [sequence],
      [{group, two_nodes},
       {group, three_nodes},
       {group, orphaned_txs_get_included}]},
     {two_nodes, [sequence],
      [start_dev1,
       mine_a_key_block_on_dev1,
       mine_a_key_block_on_dev1,
       stop_dev1,
       start_dev2,
       mine_a_key_block_on_dev2,
       stop_dev2,
       start_nodes_and_wait_sync_dev1_chain_wins,
       stop_dev1,
       stop_dev2]},
     {three_nodes, [sequence],
      [add_dev3_node,
       dev3_failed_attack,
       dev3_syncs_to_community]},
     {orphaned_txs_get_included, [sequence],
      [{group, on_micro_block},
       {group, on_key_block}
      ]},
     {on_micro_block, [sequence],
      [start_dev1,
       start_dev2,
       %% mine more than 2 blocks so the beneficiary has some tokens to spend
       mine_a_key_block_on_dev1,
       mine_a_key_block_on_dev1,
       mine_a_key_block_on_dev1,
       stop_dev2,
       mine_a_key_block_on_dev1,
       spend_on_dev1,
       mine_a_micro_block_on_dev1,
       mine_a_key_block_on_dev1,
       stop_dev1,
       start_dev2,
       mine_a_key_block_on_dev2,
       mine_a_key_block_on_dev2,
       mine_a_key_block_on_dev2,
       mine_a_key_block_on_dev2,
       mine_a_key_block_on_dev2,
       mine_a_key_block_on_dev2,
       mine_a_key_block_on_dev2,
       stop_dev2,
       start_nodes_and_wait_sync_dev2_chain_wins,
       assert_orphaned_tx_in_pool_dev2_receives,
       mine_a_micro_block_on_dev2,
       stop_dev1,
       stop_dev2
       ]},
     {on_key_block, [sequence],
      [start_dev1,
       start_dev2,
       %% mine more than 2 blocks so the beneficiary has some tokens to spend
       mine_a_key_block_on_dev1,
       mine_a_key_block_on_dev1,
       mine_a_key_block_on_dev1,
       stop_dev2,
       mine_a_key_block_on_dev1,
       spend_on_dev1,
       mine_a_micro_block_on_dev1,
       stop_dev1,
       start_dev2,
       mine_a_key_block_on_dev2,
       mine_a_key_block_on_dev2,
       mine_a_key_block_on_dev2,
       mine_a_key_block_on_dev2,
       mine_a_key_block_on_dev2,
       mine_a_key_block_on_dev2,
       stop_dev2,
       start_nodes_and_wait_sync_dev2_chain_wins,
       assert_orphaned_tx_in_pool_dev2_receives,
       mine_a_micro_block_on_dev2,
       stop_dev1,
       stop_dev2
       ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    %% Do not use 'instant_mining', as it short-cuts header validation/whitelist tests
    aecore_suite_utils:init_per_suite([dev1, dev2, dev3],
                                      #{ <<"sync">> =>
                                             #{<<"sync_allowed_height_from_top">> => 0}
                                       , <<"mempool">> =>
                                             #{ <<"sync_interval">> => 1000} %% every second
                                       , <<"mining">> =>
                                             #{ <<"expected_mine_rate">> => ?MINE_RATE,
                                                %% this is important so beneficiary can spend
                                                <<"beneficiary_reward_delay">> => 2}},
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
init_per_group(orphaned_txs_get_included, Config) ->
    [{nodes, [aecore_suite_utils:node_tuple(D) ||
                 D <- [dev1, dev2]]} | Config];
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

patterns() ->
    [ {aec_sync, '_', '_', []}
    , {aec_resilience, '_', '_', []}
    | [ {aec_chain_state, F, A, []} || {F, A} <- aec_chain_state:module_info(exports) ]]
    ++ [{aec_chain, F, A, []} || {F, A} <- aec_chain:module_info(exports) ].

flags() ->
    {all, call}.

start_node(Node, Config) ->
    aecore_suite_utils:start_node(Node, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(Node)),
    ok = aecore_suite_utils:check_for_logs([Node], Config),
    ok.

start_dev1(Config) -> start_node(dev1, Config).

start_dev2(Config) -> start_node(dev2, Config).

stop_dev1(Config) -> stop_and_check([dev1], Config).

stop_dev2(Config) -> stop_and_check([dev2], Config).

mine_a_key_block(Node) ->
    NName= aecore_suite_utils:node_name(Node),
    {ok, [Block]} = mine_key_blocks(NName, 1),
    Top = rpc:call(NName, aec_chain, top_block, [], 5000),
    ct:log("top of chain ~p: ~p (mined ~p)", [Node, Top, Block]),
    {Top, Top} = {Top, Block},
    Top.

mine_a_key_block_on_dev1(_Config) -> mine_a_key_block(dev1).

mine_a_key_block_on_dev2(_Config) -> mine_a_key_block(dev2).

mine_a_micro_block_on_dev1(_Config) -> mine_a_micro_block(dev1).
mine_a_micro_block_on_dev2(_Config) -> mine_a_micro_block(dev2).

mine_a_micro_block(Node) -> 
    NName = aecore_suite_utils:node_name(Node),
    aecore_suite_utils:mine_blocks(NName, 1, ?MINE_RATE, micro, #{}).

spend(Node) ->
    {_, Pub} = aecore_suite_utils:sign_keys(Node),
    NName = aecore_suite_utils:node_name(Node),
    {ok, Tx} = aecore_suite_utils:spend(NName, Pub, Pub, 1, ?SPEND_FEE),
    {ok, [Tx]} = rpc:call(NName, aec_tx_pool, peek, [infinity]),
    ct:log("Spend tx ~p", [Tx]),
    Tx.

spend_on_dev1(_Config) -> spend(dev1).
spend_on_dev2(_Config) -> spend(dev2).

wait_nodes_to_sync(ExpectedTop, WrongForkNode, T0) ->
    WFNName = aecore_suite_utils:node_name(WrongForkNode),
    done = aecore_suite_utils:await_sync_complete(T0, [WFNName]),
    aec_test_utils:wait_for_it(
        fun() -> rpc:call(WFNName, aec_chain, top_block, [], 5000) end,
        ExpectedTop),
    ok.

start_nodes_and_wait_sync_dev1_chain_wins(Config) ->
    start_nodes_and_wait_sync(dev1, dev2, Config).

start_nodes_and_wait_sync_dev2_chain_wins(Config) ->
    start_nodes_and_wait_sync(dev2, dev1, Config).

start_nodes_and_wait_sync(CorrectForkNode, OtherNode, Config) ->
    start_node(CorrectForkNode, Config), 
    CFNName = aecore_suite_utils:node_name(CorrectForkNode),
    CFTop = rpc:call(CFNName, aec_chain, top_block, [], 5000),
    ct:log("top of chain ~p: ~p", [ CorrectForkNode, CFTop ]),
    stop_and_check([CorrectForkNode], Config), 

    start_node(OtherNode, Config), 
    ForkNName = aecore_suite_utils:node_name(OtherNode),
    ForkTop = rpc:call(ForkNName, aec_chain, top_block, [], 5000),
    ct:log("top of chain ~p: ~p", [ OtherNode, ForkTop ]),

    false = (ForkTop == CFTop),
    timer:sleep(100),
    %% unexepctedly last block of dev1 arrives before rest of the chain
    %% This is no longer allowed, so it should fail.
    ?assertMatch({error, {illegal_orphan, _}},
                  rpc:call(ForkNName, aec_conductor, post_block, [CFTop], 5000)),

    T0 = os:timestamp(),
    start_node(CorrectForkNode, Config), 
    wait_nodes_to_sync(CFTop, OtherNode, T0),
    ok.

assert_orphaned_tx_in_pool_dev1_receives(Config) ->
    assert_orphaned_tx_in_pool(dev1, dev2, Config).

assert_orphaned_tx_in_pool_dev2_receives(Config) ->
    assert_orphaned_tx_in_pool(dev2, dev1, Config).

assert_orphaned_tx_in_pool(CorrectForkNode, OtherNode, _Config) ->
    NName = aecore_suite_utils:node_name(OtherNode),
    {ok, [SignedTx]} = rpc:call(NName, aec_tx_pool, peek, [infinity], 5000),
    TxHash = aetx_sign:hash(SignedTx),

    timer:sleep(100),
    N2Name = aecore_suite_utils:node_name(CorrectForkNode),
    TxLocation = rpc:call(N2Name, aec_chain, find_tx_location, [TxHash], 5000),
    {true, _} = {(TxLocation =/= none), none}, %% tx had been GCed
    {true, _} = {(TxLocation =/= not_found), not_found},%% tx had not been seen on the node yet
    {ok, [SignedTx]} = rpc:call(N2Name, aec_tx_pool, peek, [infinity], 5000),
    ok.
