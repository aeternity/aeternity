-module(hctest_slashable_SUITE).

%% Test cases
-export([
    mine_and_sync/1,
    produce_1_cc_block/1,
    produce_epoch/1,
    start_dev1_dev2/1,
    start_dev1_dev2_dev3/1,
    sync_dev3/1,
    wait_and_sync/1,
    produce_epoch_unfinished/1,
    verify_double_spend_prevented/1,
    verify_double_spend_slash/1,
    mine_double_spend/1
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
        {group, slashable_double_spend}
        % {group, slashable_double_vote}
        % {group, slashable_ignore_vote},
        %% {group, slashable_incorrect_block} % covered by producing_SUITE
        %% {group, slashable_late_block}
        % {group, slashable_ignore_finalize_epoch},
        % {group, slashable_ignore_correctly_pinned}
    ].

groups() ->
    [
        %% Definition: A validator casts multiple votes for different forks or outcomes in a single voting phase. This
        %% action is considered malicious and undermines the voting process.
        %% Penalty: The validator's stake is slashed, and their voting rights are suspended for one or more epochs. The
        %% network may also distribute the slashed amount among honest validators as a reward for maintaining integrity.
        {slashable_double_vote, [sequence], [
            start_dev1_dev2_dev3,
            produce_epoch,
            produce_epoch_unfinished,
            %% Attempt to double vote at the eoe leader.
            %% Verify that the double vote did not pass
            verify_double_vote
        ]},

        %% Definition: A validator produces two different blocks at the same height, effectively attempting a
        %% double-spending attack or creating ambiguity in the chain.
        %% Penalty: The validator's stake is slashed (partially or entirely), and they are barred from participating in
        %% future leader elections for a specified period. The network may also burn a portion of their stake as a
        %% deterrent to others.
        {slashable_double_spend, [sequence], [
            start_dev1_dev2,
            produce_epoch,
            % produce_epoch_unfinished,
            mine_double_spend,
            mine_and_sync,
            verify_double_spend_prevented,
            verify_double_spend_slash
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

init_per_group(_GroupName, Config0) ->
    hctest_shared:init_per_group(Config0).

end_per_group(_Group, Config) ->
    hctest_shared:end_per_group(Config).

%% Here we decide which nodes are started/running
init_per_testcase(start_dev1_dev2, Config) ->
    Config1 = hctest_shared:config_add_node(Config, ?NODE1, ?NODE1_NAME, [?ALICE, ?LISA]),
    Config2 = hctest_shared:config_add_node(Config1, ?NODE2, ?NODE2_NAME, [?BOB]),
    aect_test_utils:setup_testcase(Config2),
    Config2;
init_per_testcase(start_dev1_dev2_dev3, Config) ->
    Config1 = hctest_shared:config_add_node(Config, ?NODE1, ?NODE1_NAME, [?ALICE]),
    Config2 = hctest_shared:config_add_node(Config1, ?NODE2, ?NODE2_NAME, [?BOB]),
    Config3 = hctest_shared:config_add_node(Config2, ?NODE3, ?NODE3_NAME, [?LISA]),
    aect_test_utils:setup_testcase(Config3),
    Config3;
init_per_testcase(sync_dev3, Config) ->
    Config1 = hctest_shared:with_saved_keys([nodes], Config),
    Config2 = hctest_shared:config_add_node(Config1, ?NODE3, ?NODE3_NAME, []),
    aect_test_utils:setup_testcase(Config2),
    Config2;
init_per_testcase(_Case, Config) ->
    Config1 = hctest_shared:with_saved_keys([nodes, last_produced_blocks], Config),
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

%% test step
produce_epoch(Config) ->
    hctest_shared:produce_n_epochs(Config, #{count_epochs => 1}).

%% test step
produce_epoch_unfinished(Config) ->
    hctest_shared:produce_n_epochs(Config, #{count_epochs => 1, unfinished => true}).

%% test step
produce_1_cc_block(Config) ->
    hctest_shared:produce_cc_blocks(Config, #{count => 1}).

mine_and_sync(Config) ->
    {ok, _KBs} = hctest_shared:produce_cc_blocks(Config, #{count => 3}),
    Nodes = hctest_shared:get_nodes(Config, []),
    %%    {_Height, Leader} = get_height_and_leader(Config),
    {ok, _KB} = hctest_shared:wait_same_top(Nodes, 3, #{}),
    ok.

wait_and_sync(Config) ->
    Nodes = hctest_shared:get_nodes(Config, []),
    {ok, _KB} = hctest_shared:wait_same_top(Nodes, 3, #{}),
    ok.

mine_double_spend(Config) ->
    {_Height, Leader} = hctest_shared:get_cc_height_and_leader(Config),
    ct:pal("Before ~p", [hctest_snapshot:cc_snap(#{node => Leader, history_size => 5})]),
    %% FIXME: This halts the production of blocks and nothing happens after
    hctest_shared:double_spend(Leader),
    timer:sleep(3000),
    ok.

verify_double_spend_prevented(Config) ->
    {_Height, Leader} = hctest_shared:get_cc_height_and_leader(Config),
    ct:pal("After ~p", [hctest_snapshot:cc_snap(#{node => Leader, history_size => 5})]),
    ok.

verify_double_spend_slash(Config) ->
    [] = aecore_suite_utils:assert_no_errors_in_logs(Config),
    ok.
