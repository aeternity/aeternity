-module(hctest_eoe_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("../../aecontract/include/hard_forks.hrl").
-include("../include/hc_test.hrl").

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
        % {group, eoe_netsplit},
        % {group, eoe_double_vote}, % covered by slashable SUITE
        % {group, eoe_inactive_producer}, % covered by slashable SUITE
        % {group, eoe_ignored_vote}, % covered by slashable SUITE
        % {group, eoe_finalizing_txn},
        % {group, eoe_restart_node},
    ].

groups() ->
    [
        {eoe_restart_node, [sequence], [
            %% If a node restarts in the middle of an epoch will a start of epoch event be sent when it starts? [by Justin]
        ]},
        {eoe_netsplit, [sequence], [
            %% Create a network split in the end of an epoch (triple length block)
            %% Losing a vote, attempt to penalize a vote and how it is resolved when the netsplit is over, will the penalty be lifted
        ]},
        {eoe_double_vote, [sequence], [
            %% - Ongoing vote can become corrupted/missing votes
            %% - Double vote: two or more voting txns by the same validator on two different forks
            %%      - Reports on the malicious behaviour
            %%      - There should be a penalty (still in discussion)
        ]},
        {eoe_inactive_producer, [sequence], [
            %% - The final producer is not active/does not send the proposal
            %%      - on timeout: the new leader just runs with the preferred fork
            %%      - if the vote fails: ...
        ]},
        {eoe_ignored_vote, [sequence], [
            %% - Ignoring votes?
        ]},
        {eoe_finalizing_txn, [sequence], [
            %% - Ensure that finalizing txn is created on the chain
            %%      - The final decision should include all votes
            start_two_child_nodes,
            produce_first_epoch
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

end_per_testcase(_Case, Config) ->
    {save_config, Config}.
