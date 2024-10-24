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

groups() ->
    [
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
