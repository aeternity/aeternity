%%% @doc Test simple production with some special features (like holes).
%%% @end
-module(hc_production_SUITE).

%% Test cases
-export([
    produce_1_cc_block_dev2/1,
    produce_1_cc_block/1,
    produce_epoch/1,
    produce_3_epochs/1,
    produce_till_dev1_is_leader/1,
    sleep_1_block/1,
    start_dev1_dev2_dev3/1,
    start_dev1_dev2/1,
    start_dev1/1,
    stop_dev1/1,
    sync_dev3/1,
    verify_chain_height/1,
    verify_non_leader_produced_hole/1
]).

-include_lib("stdlib/include/assert.hrl").
-include("./hctest_defaults.hrl").

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
        {group, normal_production},
        {group, hole_production}
    ].

groups() ->
    [
        %% Produce 3 epochs (30 blocks worth) and see that the nodes have converged correctly
        {normal_production, [sequence], [
            start_dev1_dev2,
            produce_3_epochs,
            verify_chain_height
        ]},
        %% Produce 2 epochs (20 blocks worth), shut down one node, produce some more and see that holes have appeared
        {hole_production, [sequence], [
            start_dev1_dev2,
            produce_epoch,
            produce_epoch,
            produce_till_dev1_is_leader,
            stop_dev1,
            produce_1_cc_block_dev2,
            produce_1_cc_block_dev2,
            % produce_1_cc_block_dev2,
            % produce_1_cc_block_dev2,
            % produce_1_cc_block_dev2,
            % produce_1_cc_block_dev2,
            % produce_1_cc_block_dev2,
            % produce_1_cc_block_dev2,
            % start_dev1,
            % produce_1_cc_block,
            % produce_epoch
            verify_non_leader_produced_hole
        ]}
        %% TODO: Test late producing in epoch 1
        %% TODO: Test late producing end of epoch?
        %% TODO: Mine for longer time with only 1 node, other stopped

        % {producing_two_sequential_blocks, [sequence], [
        %     %% Mining two or more valid consecutive blocks should be rejected by the other nodes
        %     %% Expected behaviour: only the first valid block at the allowed height is accepted
        %     start_dev1_dev2_dev3,
        %     produce_epoch,
        %     %%            produce_many_cc_blocks,
        %     %%            wait_and_sync,
        %     verify_only_one_block_accepted
        % ]},
        % %% can this test be property based?
        % {bad_block, [sequence], [
        %     %% - Producing a bad block that does not pass the checks
        %     %% What are the things we're checking for in the block?
        %     %% Block can have timestamp not before previous. And height.
        %     %% Block can be received (gossiped) only within its time window
        % ]}
    ].

init_per_suite(Config0) ->
    hctest_ct_shared:init_per_suite(Config0, #{
        owner_pubkey => ?OWNER_PUBKEY,
        parent_chain_node => ?PARENT_CHAIN_NODE,
        parent_chain_network_id => ?PARENT_CHAIN_NETWORK_ID,
        parent_finality => ?PARENT_FINALITY,
        parent_epoch_length => ?PARENT_EPOCH_LENGTH,
        reward_delay => ?REWARD_DELAY,
        parent_account_seeds => #{ % Parent patron pubkey is added in the shared implementation
            hctest:encoded_pubkey(?DWIGHT) => 2_100000000_000000000_000000000,
            hctest:encoded_pubkey(?EDWIN) => 3_100000000_000000000_000000000
        },
        main_staking_contract => ?MAIN_STAKING_CONTRACT,
        staking_validator_contract => ?STAKING_VALIDATOR_CONTRACT, 
        hc_contract => ?HC_CONTRACT,
        nodes_list => [?NODE1, ?NODE2]
    }).

end_per_suite(Config) ->
    hctest_ct_shared:end_per_suite(Config, [?NODE1, ?NODE2, ?NODE3, ?PARENT_CHAIN_NODE]).

init_per_group(Group, Config) ->
    hctest_ct_shared:init_per_group(Group, Config, #{
        consensus => ?CONSENSUS,
        parent_chain_node => ?PARENT_CHAIN_NODE,
        parent_chain_node_name => ?PARENT_CHAIN_NODE_NAME,
        parent_epoch_length => ?PARENT_EPOCH_LENGTH,
        parent_finality => ?PARENT_FINALITY,
        stakers => [?ALICE, ?BOB, ?LISA]
    }).

end_per_group(Group, Config) ->
    hctest_ct_shared:end_per_group(Group, Config).

%% Here we decide which nodes are started/running
init_per_testcase(start_dev1_dev2, Config) ->
    Nodes = [
        {?NODE1, ?NODE1_NAME, [?ALICE, ?LISA], [{?ALICE, ?DWIGHT}, {?LISA, ?EDWIN}]},
        {?NODE2, ?NODE2_NAME, [?BOB_SIGN], [{?BOB_SIGN, ?EDWIN}]}
    ],
    Config1 = [{nodes, Nodes} | Config],
    aect_test_utils:setup_testcase(Config1),
    Config1;
init_per_testcase(start_dev1_dev2_dev3, Config) ->
    Nodes = [
        {?NODE1, ?NODE1_NAME, [?ALICE, ?LISA], [{?ALICE, ?DWIGHT}, {?LISA, ?EDWIN}]},
        {?NODE2, ?NODE2_NAME, [?BOB_SIGN], [{?BOB_SIGN, ?EDWIN}]},
        {?NODE3, ?NODE3_NAME, [?LISA], [{?LISA, ?EDWIN}]}
    ],
    Config1 = [{nodes, Nodes} | Config],

    aect_test_utils:setup_testcase(Config1),
    Config1;
init_per_testcase(sync_dev3, Config) ->
    Config1 = hctest:with_saved_keys([nodes], Config),
    Nodes = [
        {?NODE3, ?NODE3_NAME, [?EDWIN, ?DWIGHT], [{?EDWIN, ?DWIGHT}, {?DWIGHT, ?EDWIN}]}
    ],
    Config2 = [{nodes, Nodes} | Config1],
    aect_test_utils:setup_testcase(Config2),
    Config2;
init_per_testcase(_Case, Config) ->
    Config1 = hctest:with_saved_keys([nodes], Config),
    aect_test_utils:setup_testcase(Config1),
    Config1.

end_per_testcase(_Case, Config) ->
    {save_config, Config}.

base_child_config() ->
    #{
        receive_address_pub => ?FORD,
        hc_contract => ?HC_CONTRACT,
        child_epoch_length => ?CHILD_EPOCH_LENGTH,
        child_block_time => ?CHILD_BLOCK_TIME,
        child_block_production_time => ?CHILD_BLOCK_PRODUCTION_TIME,
        block_reward => ?BLOCK_REWARD,
        reward_delay => ?REWARD_DELAY
    }.

start_dev1_dev2(Config) ->
    hctest_ct_shared:start_child_nodes([?NODE1, ?NODE2], base_child_config(), Config).

start_dev1_dev2_dev3(Config) ->
    hctest_ct_shared:start_child_nodes([?NODE1, ?NODE2, ?NODE3], base_child_config(), Config).

sync_dev3(Config) ->
    hctest_ct_shared:start_child_nodes([?NODE3], base_child_config(), Config).

sleep_1_block(_Config) ->
    timer:sleep(?CHILD_BLOCK_TIME).

produce_3_epochs(Config) ->
    hctest:produce_n_epochs(Config, 3).

produce_epoch(Config) ->
    hctest:produce_n_epochs(Config, 1).

produce_1_cc_block(Config) ->
    {ok, [B]} = hctest:produce_cc_blocks(Config, #{count => 1}),
    ct:pal("produce_1: ~w", [B]).

%% Continue stepping through blocks till dev1 becomes the leader
produce_till_dev1_is_leader(Config) ->
    {ok, Blocks0} = hctest:produce_cc_blocks(Config, #{count => 1}),
    Blocks = lists:filter( fun(B) -> aec_blocks:type(B) =:= 'key' end, Blocks0 ),
    %% Node dev1 is started with [Alice, Lisa] keys
    {AlicePubkey, _, _} = ?ALICE, 
    {LisaPubkey, _, _} = ?LISA, 
    %% When mined by other node than dev1, assume that dev1 will become the leader
    case lists:any(
        fun(B) -> % all blocks in Blocks are key blocks (filtered earlier)
            Producer = aec_blocks:miner(B),
            Producer /= AlicePubkey andalso Producer /= LisaPubkey 
        end, Blocks) of
        true -> ok;
        false -> produce_till_dev1_is_leader(Config)
    end.

stop_dev1(Config) ->
    catch aecore_suite_utils:stop_node(?NODE1, Config).

start_dev1(Config) ->
    hctest_ct_shared:start_child_nodes([?NODE1], base_child_config(), Config).

produce_1_cc_block_dev2(Config) ->
    %% At this point we should expect a 'timeout_waiting_for_block' error,
    %% but non-leader nodes should produce a hole and we will try to detect that hole

    PrevHeight = hctest:get_height(?NODE2),
    
    %% The hole is created when a node becomes a leader and attempts to produce 
    aecore_suite_utils:rpc(?NODE2, aec_conductor, start_mining, []),
    case wait_till_dev2_chain_has_moved(Config, PrevHeight, 20, 250) of
        timeout ->
            ?assert(false, "The chain on dev2 did not advance in 5000 msec");
        ok ->
            ok
    end.

wait_till_dev2_chain_has_moved(CTConfig, PrevHeight, 0, SleepMsec) ->
    timeout;
wait_till_dev2_chain_has_moved(CTConfig, PrevHeight, Attempts, SleepMsec) ->
    Height = hctest:get_height(?NODE2),
    case Height > PrevHeight of
        true ->
            ok;
        false -> 
            timer:sleep(SleepMsec),
            wait_till_dev2_chain_has_moved(CTConfig, PrevHeight, Attempts - 1, SleepMsec)
    end.

%% All the non-leader nodes must have hole blocks in place
%% TODO: Ensure the top block is actually produced and we aren't too early (subscribe to gproc?)
verify_non_leader_produced_hole(Config) ->
    AllNodes = hctest:get_nodes(Config),
    ReadCount = 5,
    LastBlocks = hctest:read_last_blocks(
        ?NODE2, aecore_suite_utils:rpc(?NODE2, aec_chain, top_block_hash, []), ReadCount, []),
    ?assert(lists:any(
        fun(Block) -> aec_headers:is_hole(aec_blocks:to_header(Block)) end,
        LastBlocks
    ), hctest:format("A hole is expected within last ~w blocks", [ReadCount])).

%% Produce a chain of valid blocks, of which only the first is legal and should be accepted,
%% the rest should be dropped
% produce_many_cc_blocks(Config) ->
%     {HeightBefore, Leader} = hctest_shared:get_cc_height_and_leader(Config),
%     ct:pal("Produce many blocks: starting at height ~p", [HeightBefore]),
%     {ok, Blocks} = hctest_shared:produce_cc_blocks(Config, #{count => 3}, Leader),

%     HeightAfter = hctest_shared:get_height(Leader),
%     ct:pal("Produce many blocks: new current height ~p", [HeightAfter]),

%     Config1 = [{last_produced_blocks, Blocks} | Config],
%     {save_config, Config1}.

% verify_only_one_block_accepted(Config) ->
%     {save_config, Config1} = produce_many_cc_blocks(Config),
%     hctest:wait_and_sync(Config1),
%     LastProducedBlocks = ?config(last_produced_blocks, Config1),
%     ct:pal("Verify: last produced: ~p", [LastProducedBlocks]),
%     lists:foreach(
%         fun(Node) ->
%             %% Blocks 12 and 13 should be illegal and rejected (assuming new blocks were not yet produced)
%             %%            ?assertEqual(11, get_height(Node))

%             TopBlock = aecore_suite_utils:rpc(Node, aec_chain, top_block, []),
%             ct:pal("Verify: Top at ~s: ~p", [Node, TopBlock]),
%             ?assertEqual(
%                 hd(LastProducedBlocks),
%                 TopBlock,
%                 "Of 3 produced blocks only the 1st block must be on chain, other 2 must be rejected"
%             )
%         end,
%         hctest:get_nodes(Config1)
%     ).

verify_chain_height(Config) ->
    Nodes = hctest:get_nodes(Config),
    HashesHeights = lists:map(
        fun({Node, _, _, _}) ->
            Hash = aecore_suite_utils:rpc(Node, aec_chain, top_block_hash, []),
            Height = hctest:get_height(Node),
            ?assert(
                20 =< Height,
                "With 2 epochs passing chain height must have reached 20"
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
