-module(hctest_producing_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("../../aecontract/include/hard_forks.hrl").
-include("../include/hc_test.hrl").

export([
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

%% Test cases
-export([]).

all() ->
    [
        {group, late_producing},
        {group, producing_two_sequential_blocks},
        {group, bad_block}
    ].

groups() ->
    [
        {late_producing, [sequence], [
            %% Start a parent + two CC nodes. Elect leaders.
            %% Make sure that the current leader does not mine within the time slot
            %% The other node must reject the late block and produce something else valid (blank)
            start_two_child_nodes,
            produce_first_epoch,
            produce_1_cc_block,
            spend_txs_late_producing,
            produce_1_cc_block_late,
            %% Expected behaviour: the other nodes should reject this block and fill in the blank
            verify_consensus_solution_late_block
        ]},
        {producing_two_sequential_blocks, [sequence], [
            %% Mining two valid consecutive blocks should be rejected (because leader is only at a specific height)
            start_two_child_nodes,
            produce_first_epoch
            %%
        ]},
        {bad_block, [sequence], [
            %% - Producing a bad block that does not pass the checks
            %% What are the things we're checking for in the block?
            %% Block can have timestamp not before previous. And height.
            %% Block can be received (gossiped) only within its time window
        ]}
    ].

init_per_suite(Config0) ->
    hctest_shared_test_steps:init_per_suite(Config0, [?NODE1, ?NODE2]).

end_per_suite(Config) ->
    hctest_shared_test_steps:end_per_suite(Config, [?NODE1, ?NODE2, ?NODE3]).

init_per_group(_GroupName, Config0) ->
    hctest_shared_test_steps:init_per_group(Config0).

end_per_group(_Group, Config) ->
    %% TODO: Use new error checking from PR #4444
    Nodes = [Node || {Node, _, _} <- ?config(nodes, Config)],
    [] = aecore_suite_utils:errors_in_logs(Nodes, Config),
    hctest_shared_test_steps:end_per_group(Config).

%% Here we decide which nodes are started/running
init_per_testcase(start_two_child_nodes, Config) ->
    Config1 =
        [
            {nodes, [
                {?NODE1, ?NODE1_NAME, [?ALICE, ?LISA]},
                {?NODE2, ?NODE2_NAME, [?BOB]}
            ]}
            | Config
        ],
    aect_test_utils:setup_testcase(Config1),
    Config1;
init_per_testcase(sync_third_node, Config) ->
    Config1 = hctest_setup:with_saved_keys([nodes], Config),
    Nodes = ?config(nodes, Config1),
    Config2 = lists:keyreplace(
        nodes,
        1,
        Config1,
        {nodes, Nodes ++ [{?NODE3, ?NODE3_NAME, []}]}
    ),
    aect_test_utils:setup_testcase(Config2),
    Config2;
init_per_testcase(_Case, Config) ->
    Config1 = hctest_setup:with_saved_keys([nodes], Config),
    aect_test_utils:setup_testcase(Config1),
    Config1.
