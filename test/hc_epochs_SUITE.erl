-module(hc_epochs_SUITE).

-import(aecore_suite_utils, [http_request/4, external_address/0, rpc/3, rpc/4]).

-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).


%% Test cases
-export([
    epochs_with_fast_parent/1,
    epochs_with_slow_parent/1,
    first_leader_next_epoch/1,
    start_two_child_nodes/1
]).

-include_lib("stdlib/include/assert.hrl").
-include("./hctest_defaults.hrl").

all() -> [{group, epochs}].

groups() -> [
    {epochs, [sequence], [
        start_two_child_nodes
        , first_leader_next_epoch
        , epochs_with_slow_parent
        , epochs_with_fast_parent
    ]}].

suite() -> [].

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
init_per_testcase(start_two_child_nodes, Config) ->
    Config1 =
        [{nodes, [{?NODE1, ?NODE1_NAME, [?ALICE, ?LISA], [{?ALICE, ?DWIGHT}, {?LISA, ?EDWIN}]},
                  {?NODE2, ?NODE2_NAME, [?BOB_SIGN], [{?BOB_SIGN, ?EDWIN}]}
                 ]}
         | Config],
    aect_test_utils:setup_testcase(Config1),
    Config1;
init_per_testcase(_Case, Config) ->
    Config1 = hctest:with_saved_keys([nodes], Config),
    aect_test_utils:setup_testcase(Config1),
    Config1.

end_per_testcase(_Case, Config) ->
    {save_config, Config}.

-define(EPOCHSUITE_CHILD_BLOCK_TIME, 200).
-define(EPOCHSUITE_CHILD_BLOCK_PRODUCTION_TIME, (?EPOCHSUITE_CHILD_BLOCK_TIME * 2 div 5)). % 40% of block time

default_child_config() ->
    #{
        receive_address_pub => ?FORD,
        hc_contract => ?HC_CONTRACT,
        child_epoch_length => ?CHILD_EPOCH_LENGTH,
        child_block_time => ?EPOCHSUITE_CHILD_BLOCK_TIME,
        child_block_production_time => ?EPOCHSUITE_CHILD_BLOCK_PRODUCTION_TIME,
        block_reward => ?BLOCK_REWARD,
        reward_delay => ?REWARD_DELAY
    }.

start_two_child_nodes(CTConfig) ->
    [{Node1, NodeName1, Stakers1, Pinners1}, {Node2, NodeName2, Stakers2, Pinners2} | _] = hctest:get_nodes(CTConfig),
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(hctest:config(network_id, CTConfig))} ],

    DefaultConfig = default_child_config(),
    hctest_ct_shared:child_node_config(DefaultConfig#{
        node => Node1,
        stakeholders => Stakers1,
        pinners => Pinners1
    }, CTConfig),
    aecore_suite_utils:start_node(Node1, CTConfig, Env),
    aecore_suite_utils:connect(NodeName1, []),
    rpc(Node1, aec_conductor, start_mining, []),

    hctest_ct_shared:child_node_config(DefaultConfig#{
        node => Node2,
        stakeholders => Stakers2,
        pinners => Pinners2
    }, CTConfig),
    aecore_suite_utils:start_node(Node2, CTConfig, Env),
    aecore_suite_utils:connect(NodeName2, []),
    rpc(Node2, aec_conductor, start_mining, []),
    ok.

first_leader_next_epoch(Config) ->
    [{Node, _, _, _} | _] = hctest:get_nodes(Config),
    hctest:produce_cc_blocks(Config, #{count => 1}),
    StartHeight = hctest:get_height(Node),
    {ok, #{last := Last, epoch := Epoch}} = hctest:epoch_info(Node, StartHeight),
    ct:log("Checking leader for first block next epoch ~p (height ~p)", [Epoch+1, Last+1]),
    ?assertMatch({ok, _}, rpc(Node, aec_consensus_hc, leader_for_height, [Last + 1])).

%% Modify the child block time to be faster or slower
set_child_nodes_block_time(Time) ->
    rpc(?NODE1, aeu_ets_cache, put, [aec_consensus_hc, child_block_time, Time]),
    rpc(?NODE2, aeu_ets_cache, put, [aec_consensus_hc, child_block_time, Time]).

%% Demonstrate that child chain start signalling epoch length adjustment downward
%% When parent blocks are produced too quickly, we need to shorten child epoch
epochs_with_fast_parent(Config) ->
    % set_child_nodes_block_time(?CHILD_BLOCK_TIME),
    set_child_nodes_block_time(?EPOCHSUITE_CHILD_BLOCK_TIME * 5 div 4),

    [{Node, _, _, _} | _] = hctest:get_nodes(Config),

    %% ensure start at a new epoch boundary
    StartHeight = hctest:get_height(Node),
    {ok, #{last := Last}} = hctest:epoch_info(Node),
    %% some block production including parent blocks
    {ok, _} = hctest:produce_cc_blocks(Config, #{target_height => Last}),

    {ok, #{length := Len1} = EpochInfo1} = hctest:epoch_info(Node),
    ct:log("Info ~p", [EpochInfo1]),

    %% Produce twice as many parent blocks as needed in an epoch
    begin
        Height0 = hctest:get_height(Node),
        ParentTopHeight0 = hctest:get_height(?PARENT_CHAIN_NODE),
        {ok, #{length := Len1}} = hctest:epoch_info(Node),

        {ok, _} = hctest:produce_cc_blocks(Config, #{
            target_height => Height0 + Len1,
            parent_produce => hctest:spread(2*?PARENT_EPOCH_LENGTH, Height0,
                [ {CH, 0} || CH <- lists:seq(Height0 + 1, Height0 + Len1)])
        })
    end,

    begin
        ParentTopHeight1 = hctest:get_height(?PARENT_CHAIN_NODE),
        Height1 = hctest:get_height(Node),
        {ok, #{length := Len2} = EpochInfo2} = hctest:epoch_info(Node),
        ct:log("Parent at height ~p and child at height ~p in child epoch ~p",
            [ParentTopHeight1, Height1, EpochInfo2]),
        ?assertEqual(2*?PARENT_EPOCH_LENGTH, ParentTopHeight1 - ParentTopHeight0),

        {ok, _} = hctest:produce_cc_blocks(Config, #{
            target_height => Height1 + Len2,
            parent_produce => hctest:spread(2*?PARENT_EPOCH_LENGTH, Height1,
                [ {CH, 0} || CH <- lists:seq(Height1 + 1, Height1 + Len2)]),
            extra_wait_block_intervals => 10 % wait really long
        })
    end,

    begin
        Height2 = hctest:get_height(Node),
        {ok, #{length := Len3, epoch := CurrentEpoch}} = hctest:epoch_info(Node),
        hctest:produce_cc_blocks(Config, #{
            target_height => Height2 + Len3
        })
    end,

    GetFinalizeResult = hctest:try_until(
        hctest:format("Getting finalize_info from the aec_chain_hc on ~p", [Node]),
        fun() -> rpc(Node, aec_chain_hc, finalize_info, []) end,
        fun(Result) -> is_map(Result) end,
        3 * 3, % try for 3 child_block_time's, sleeping 1/3 child_block_time every iteration
        ?CHILD_BLOCK_TIME div 3
    ),
    #{ epoch_length := FinalizeEpochLength, epoch := FinalizeEpoch } = GetFinalizeResult,

    %% Here we should be able to observe signalling that epoch should be shorter
    ct:log("The agreed epoch length is ~p three epochs previous length was ~p", [FinalizeEpochLength, Len1]),
    ?assert(CurrentEpoch == FinalizeEpoch),

    %% Mine till next epoch 2 times
    ChildTopHeight2 = hctest:get_height(Node),
    lists:foreach(
        fun(N) ->
            {ok, #{length := CurrentEpochLen}} = hctest:epoch_info(Node),
            hctest:produce_cc_blocks(Config, #{
                target_height => ChildTopHeight2 + N * CurrentEpochLen
            })
        end,
        lists:seq(1, 2)
    ),

    {ok, #{length := AdjEpochLength} = EpochInfo3} = hctest:epoch_info(Node),
    ?assertEqual(FinalizeEpochLength, AdjEpochLength),
    ?assert(FinalizeEpochLength < Len1,
        hctest:format("New FinalizeEpochLength=~w is expected to be voted shorter than Len1=~w",
        [FinalizeEpochLength, Len1])),

    ct:log("Info ~p", [EpochInfo3]),
    ok.

%% Demonstrate that child chain start signalling epoch length adjustment downward
%% When parent blocks are produced too quickly, we need to shorten child epoch
epochs_with_slow_parent(Config) ->
    %% Modify the child block time to be slower
    % set_child_nodes_block_time(?CHILD_BLOCK_TIME * 6 div 5), % 120% of 'normal' value
    set_child_nodes_block_time(?EPOCHSUITE_CHILD_BLOCK_TIME * 6 div 5),

    [{Node, _, _, _} | _] = hctest:get_nodes(Config),
    ct:log("Parent start height = ~p", [hctest:config(parent_start_height, Config)]),

    begin
        %% ensure start at a new epoch boundary
        StartHeight = hctest:get_height(Node),
        {ok, #{last := Last}} = hctest:epoch_info(Node, StartHeight),
        Boundary = Last + 3 * ?CHILD_EPOCH_LENGTH,
        ct:log("Starting at CC height=~p: producing till height=~p", [StartHeight, Boundary]),
        %% some block production including parent blocks
        hctest:produce_cc_blocks(Config, #{target_height => Boundary})
    end,

    {ParentHeight, EpochDiff} = hctest:try_until(
        "Waiting for parent chain to progress the epoch while child chain stays",
        fun() ->
            ParentHeight1 = hctest:get_height(?PARENT_CHAIN_NODE),
            ct:log("Child continues while parent stuck at: ~p", [ParentHeight1]),
            ParentEpoch = (ParentHeight1 - hctest:config(parent_start_height, Config) + (?PARENT_EPOCH_LENGTH - 1)) div ?PARENT_EPOCH_LENGTH,
            ChildEpoch = hctest:get_height(Node) div ?CHILD_EPOCH_LENGTH,
            ct:log("Child epoch ~p while parent epoch ~p (parent should be in next epoch)", [ChildEpoch, ParentEpoch]),
            {ParentHeight1, ParentEpoch - ChildEpoch}
        end,
        fun(Diff) -> Diff =:= 1 end,
        5,
        ?CHILD_BLOCK_TIME div 5
    ),
    ?assertEqual(1, EpochDiff, "Child epoch should stay while parent epoch had progressed by 1"),

    Resilience = 1, % Child can cope with missing Resilience epochs in parent chain
    ResilienceBoundary = Boundary + ?CHILD_EPOCH_LENGTH * Resilience,
    %% Produce no parent block in the next Resilience child epochs
    %% the child chain should get to a halt or
    %% at least one should be able to observe signalling that the length should be adjusted upward
    {ok, _} = hctest:produce_cc_blocks(Config, #{
        target_height => ResilienceBoundary,
        parent_produce => [] }),

    ct:log("Mined almost ~p additional child epochs without parent progress", [Resilience]),
    ParentTopHeight = hctest:get_height(?PARENT_CHAIN_NODE),
    ?assertEqual(ParentHeight, ParentTopHeight, "Parent chain should not have progressed"),

    ChildTopHeight1 = hctest:get_height(Node),
    {ok, #{epoch := EndEpoch, length := EpochLength}} = hctest:epoch_info(Node, ChildTopHeight1),
    ct:log("Parent at height ~p and child at height ~p in child epoch ~p",
           [ParentTopHeight, ChildTopHeight1, EndEpoch]),

    %% Here we should have observed some signalling for increased child epoch length

    %% Parent hash grabbed in last block child epoch, so here we can start, but not finish next epoch
    {ok, _} = hctest:produce_cc_blocks(Config, #{
        target_height => ResilienceBoundary + EpochLength - 1,
        parent_produce => []
    }),
    %% Fail with timeout because we need parent chain hash but it is not progressing
    ?assertException(error, timeout_waiting_for_block,
        hctest:produce_cc_blocks(Config, #{
            target_height => ResilienceBoundary + EpochLength,
            parent_produce => []
        })),

    ?assertEqual(
        [{ok, (N-1) * ?CHILD_EPOCH_LENGTH + 1} || N <- lists:seq(1, EndEpoch)],
        [rpc(Node, aec_chain_hc, epoch_start_height, [N]) || N <- lists:seq(1, EndEpoch)],
        "Checking epoch start heights"),

    %% Quickly produce parent blocks to be in sync again
    ParentBlocksNeeded = EndEpoch * ?PARENT_EPOCH_LENGTH
        + hctest:config(parent_start_height, Config)
        + ?PARENT_FINALITY - ParentTopHeight,

    {ok, _} = hctest:produce_cc_blocks(Config, #{
        %% we failed with timeout earlier, now should work
        target_height => ResilienceBoundary + EpochLength,
        %% Produce 'ParentBlocksNeeded' on parent, 1 before the child reaches (ResilienceBoundary + EpochLength)
        parent_produce => [{ResilienceBoundary + EpochLength, ParentBlocksNeeded}]
    }),

    %% Result might not be ready right away, try a few times
    GetFinalizeResult = hctest:try_until(
        hctest:format("Getting finalize_info from the aec_chain_hc on ~p", [Node]),
        fun() -> rpc(Node, aec_chain_hc, finalize_info, []) end,
        fun(Result) -> is_map(Result) end,
        3 * 3, % try for 3 child_block_time's, sleeping 1/3 child_block_time every iteration
        ?CHILD_BLOCK_TIME div 3
    ),
    #{ epoch_length := FinalizeEpochLength, epoch := FinalizeEpoch } = GetFinalizeResult,
    ct:log("The agreed epoch length is ~p the current length is ~p for epoch ~p", [FinalizeEpochLength, EpochLength, FinalizeEpoch]),

    ?assert(FinalizeEpochLength > EpochLength,
        hctest:format("New FinalizeEpochLength=~w is expected to be voted longer than EpochLength=~w",
            [FinalizeEpochLength, EpochLength])
    ),

    %% Mine till next epoch 2 times
    ChildTopHeight2 = hctest:get_height(Node),
    lists:foreach(
        fun(N) ->
            {ok, #{length := CurrentEpochLen}} = hctest:epoch_info(Node),
            hctest:produce_cc_blocks(Config, #{
                target_height => ChildTopHeight2 + N * CurrentEpochLen
            })
        end,
        lists:seq(1, 2)
    ),
    {ok, #{length := AdjEpochLength} = EpochInfo} = hctest:epoch_info(Node),
    ct:log("Info ~p", [EpochInfo]),
    ?assertEqual(FinalizeEpochLength, AdjEpochLength),

    ok.
