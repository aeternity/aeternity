-module(hctest_producing_SUITE).

%% Test cases
-export([
    produce_1_cc_block_dev2/1,
    stop_dev1/1,
    produce_1_cc_block/1,
    produce_many_cc_blocks/1,
    produce_epoch/1,
    start_dev1_dev2/1,
    start_dev1_dev2_dev3/1,
    sync_dev3/1,
    verify_non_leader_produced_hole/1,
    verify_only_one_block_accepted/1,
    start_dev1/1,
    sleep_1_block/1,
    verify_chain_height/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/hc_test.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-export([
    all/0,
    groups/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

suite() -> [].

all() ->
    [
        {group, normal_production}
        %% {group, hole_production}
        %% {group, producing_two_sequential_blocks}
    ].

groups() ->
    [
        {normal_production, [sequence], [
            start_dev1_dev2,
            produce_epoch,
            produce_epoch,
            produce_epoch,
            verify_chain_height
        ]},
        {hole_production, [sequence], [
            start_dev1_dev2,
            produce_epoch,
            produce_epoch,
            produce_epoch,
            produce_1_cc_block,
            sleep_1_block,
            stop_dev1,
            produce_1_cc_block_dev2,
            produce_1_cc_block_dev2,
            produce_1_cc_block_dev2,
            produce_1_cc_block_dev2,
            produce_1_cc_block_dev2,
            produce_1_cc_block_dev2,
            produce_1_cc_block_dev2,
            produce_1_cc_block_dev2,
            start_dev1,
            produce_1_cc_block,
            produce_epoch
            % verify_non_leader_produced_hole
        ]},
        %% TODO: Test late producing in epoch 1
        %% TODO: Test late producing end of epoch?
        %% TODO: Mine for longer time with only 1 node, other stopped

        {producing_two_sequential_blocks, [sequence], [
            %% Mining two or more valid consecutive blocks should be rejected by the other nodes
            %% Expected behaviour: only the first valid block at the allowed height is accepted
            start_dev1_dev2_dev3,
            produce_epoch,
            %%            produce_many_cc_blocks,
            %%            wait_and_sync,
            verify_only_one_block_accepted
        ]},
        %% can this test be property based?
        {bad_block, [sequence], [
            %% - Producing a bad block that does not pass the checks
            %% What are the things we're checking for in the block?
            %% Block can have timestamp not before previous. And height.
            %% Block can be received (gossiped) only within its time window
        ]}
    ].

init_per_suite(Config0) ->
    case aect_test_utils:latest_protocol_version() < ?CERES_PROTOCOL_VSN of
        true ->
            {skip, hyperchains_not_supported};
        false ->
            hctest_shared:init_per_suite(Config0, [?NODE1, ?NODE2, ?NODE3])
    end.

end_per_suite(Config) ->
    hctest_shared:end_per_suite(Config, [?NODE1, ?NODE2, ?NODE3]).

init_per_group(GroupName, Config0) ->
    hctest_shared:init_per_group(GroupName, Config0).

end_per_group(_Group, Config) ->
    hctest_shared:end_per_group(Config).

%% Here we decide which nodes are started/running
init_per_testcase(start_dev1_dev2, Config) ->
    Config1 = hctest_shared:config_add_node(Config, ?NODE1, ?NODE1_NAME, [?ALICE, ?LISA], [
        {?ALICE, ?DWIGHT}, {?LISA, ?EDWIN}
    ]),
    Config2 = hctest_shared:config_add_node(Config1, ?NODE2, ?NODE2_NAME, [?BOB_SIGN], [
        {?BOB_SIGN, ?EDWIN}
    ]),
    aect_test_utils:setup_testcase(Config2),
    Config2;
init_per_testcase(start_dev1_dev2_dev3, Config) ->
    Config1 = hctest_shared:config_add_node(Config, ?NODE1, ?NODE1_NAME, [?ALICE], [
        {?ALICE, ?DWIGHT}
    ]),
    Config2 = hctest_shared:config_add_node(Config1, ?NODE2, ?NODE2_NAME, [?BOB_SIGN], [
        {?BOB_SIGN, ?EDWIN}
    ]),
    Config3 = hctest_shared:config_add_node(Config2, ?NODE3, ?NODE3_NAME, [?LISA], [{?LISA, ?EDWIN}]),
    aect_test_utils:setup_testcase(Config3),
    Config3;
init_per_testcase(sync_dev3, Config) ->
    Config1 = hctest_shared:with_saved_keys([nodes], Config),
    Config2 = hctest_shared:config_add_node(Config1, ?NODE3, ?NODE3_NAME, [?EDWIN, ?DWIGHT], [
        {?EDWIN, ?DWIGHT}, {?DWIGHT, ?EDWIN}
    ]),
    aect_test_utils:setup_testcase(Config2),
    Config2;
init_per_testcase(_Case, Config) ->
    Config1 = hctest_shared:with_saved_keys([nodes, last_produced_blocks, excluded_leader], Config),
    aect_test_utils:setup_testcase(Config1),
    Config1.

end_per_testcase(_Case, Config) ->
    {save_config, Config}.

start_dev1_dev2(Config) ->
    hctest_shared:start_child_nodes([?NODE1, ?NODE2], Config).

start_dev1_dev2_dev3(Config) ->
    hctest_shared:start_child_nodes([?NODE1, ?NODE2, ?NODE3], Config).

sync_dev3(Config) ->
    hctest_shared:start_child_nodes([?NODE3], Config).

sleep_1_block(_Config) ->
    timer:sleep(?CHILD_BLOCK_TIME).

%% test step
produce_epoch(Config) ->
    hctest_shared:produce_n_epochs(Config, #{count_epochs => 1}).

%% test step
produce_1_cc_block(Config) ->
    {ok, [B]} = hctest_shared:produce_cc_blocks(Config, #{count => 1}),
    ct:pal("produce_1: ~s", [hctest:pp(B)]).

stop_dev1(Config) ->
    ct:pal("stopping dev1", []),
    catch aecore_suite_utils:stop_node(?NODE1, Config).

start_dev1(Config) ->
    ct:pal("starting dev1", []),
    hctest_shared:start_child_nodes([?NODE1], Config).

produce_1_cc_block_dev2(Config) ->
    %% At this point we should expect a 'timeout_waiting_for_block' error,
    %% but non-leader nodes should produce a hole and we will try to detect that hole
    ProduceResult =
        catch hctest_shared:produce_cc_blocks(Config, #{count => 1, skip_nodes => [?NODE1]}),
    ct:pal("produced ~s", [hctest:pp(ProduceResult)]),
    %%    ?assertMatch({error, timeout_waiting_for_block}, ProduceResult),

    timer:sleep(?CHILD_BLOCK_TIME).
%%    hctest_shared:start_child_nodes([Leader], Config).
%%    {save_config, [{excluded_leader, Leader} | Config]}.

%% All the non-leader nodes must have hole blocks in place
%% TODO: Ensure the top block is actually produced and we aren't too early (subscribe to gproc?)
verify_non_leader_produced_hole(Config) ->
    AllNodes = hctest_shared:get_nodes(Config, []),
    ExcludedLeader = proplists:get_value(excluded_leader, Config),
    {HeightAtLeader, _CurrentLeader} = hctest_shared:get_cc_height_and_leader(Config),
    lists:foreach(
        fun(N) ->
            Height = aecore_suite_utils:rpc(N, aec_chain, top_height, []),
            ct:pal("Node ~s: height ~p", [N, Height]),
            ?assertEqual(HeightAtLeader, HeightAtLeader, "All heights on all nodes must be equal")
        end,
        AllNodes
    ),

    %% Check that the block is hole, and even the "late leader" may have synced to have it
    lists:foreach(
        fun(Node) ->
            %% The top block will (except for very short amounts of time) be a key-block.
            %% aec_blocks:prev_hash/1 will give you the hash, that might or might not be a microblock.
            TopBlock = aecore_suite_utils:rpc(Node, aec_chain, top_block, []),
            ?assertEqual('key', aec_blocks:type(TopBlock), "Top block must be a 'key' block"),
            %%            aec_chain:get_header()

            PrevHash = aecore_suite_utils:rpc(Node, aec_blocks, prev_hash, [TopBlock]),
            {ok, PrevBlock} = aecore_suite_utils:rpc(Node, aec_chain, get_block, [PrevHash]),
            ct:pal("top=~s prev=~s", [
                hctest:pp(aecore_suite_utils:rpc(Node, aec_chain, top_block_hash, [])),
                hctest:pp(PrevHash)
            ]),
            ?assertEqual(
                'micro',
                aec_blocks:type(PrevBlock),
                "Top block of 'key' type must be preceded by a 'micro' block"
            ),

            ct:pal("Top block prev: ~0p", [PrevBlock])
        %% TODO: Check that the microblock is a hole
        % ?assertEqual([], aec_blocks:txs(TopBlock))
        end,
        AllNodes -- [ExcludedLeader]
    ),
    ok.

%% Produce a chain of valid blocks, of which only the first is legal and should be accepted,
%% the rest should be dropped
produce_many_cc_blocks(Config) ->
    {HeightBefore, Leader} = hctest_shared:get_cc_height_and_leader(Config),
    ct:pal("Produce many blocks: starting at height ~p", [HeightBefore]),
    {ok, Blocks} = hctest_shared:produce_cc_blocks(Config, #{count => 3}, Leader),

    HeightAfter = hctest_shared:get_height(Leader),
    ct:pal("Produce many blocks: new current height ~p", [HeightAfter]),

    Config1 = [{last_produced_blocks, Blocks} | Config],
    {save_config, Config1}.

verify_only_one_block_accepted(Config) ->
    {save_config, Config1} = produce_many_cc_blocks(Config),
    hctest_shared:wait_and_sync(Config1),
    LastProducedBlocks = ?config(last_produced_blocks, Config1),
    ct:pal("Verify: last produced: ~p", [LastProducedBlocks]),
    lists:foreach(
        fun(Node) ->
            %% Blocks 12 and 13 should be illegal and rejected (assuming new blocks were not yet produced)
            %%            ?assertEqual(11, get_height(Node))

            TopBlock = aecore_suite_utils:rpc(Node, aec_chain, top_block, []),
            ct:pal("Verify: Top at ~s: ~p", [Node, TopBlock]),
            ?assertEqual(
                hd(LastProducedBlocks),
                TopBlock,
                "Of 3 produced blocks only the 1st block must be on chain, other 2 must be rejected"
            )
        end,
        hctest_shared:get_nodes(Config1, [])
    ).

verify_chain_height(Config) ->
    Nodes = hctest_shared:get_nodes(Config, []),
    HashesHeights = lists:map(
        fun(Node) ->
            Hash = aecore_suite_utils:rpc(Node, aec_chain, top_block_hash, []),
            Height = aecore_suite_utils:rpc(Node, aec_chain, top_height, []),
            ?assert(
                30 =< Height,
                "With 3 epochs passing chain height must have reached 30"
            ),
            {Node, Height, Hash}
        end,
        Nodes
    ),
    {Node1, _Height1, SampleHash} = hd(HashesHeights),
    lists:foreach(
        fun({Node2, _Height2, Hash}) ->
            ?assertEqual(Hash, SampleHash, io_lib:format("Nodes ~w and ~w must have the same top block hash", [Node1, Node2]))
        end,
        tl(HashesHeights)).
