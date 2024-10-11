%% @doc Setting up various pathological situations for the hyperchains
%% and checking how the consensus implementation resolves them.
%% @end
-module(aehttp_hyperchains_forks_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("./aehttp_hyperchains_utils.hrl").

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
-export([start_child_nodes/1]).

%% Groups:
%% - gossip - blocks gossiping late or never (discarded as out of the time slot or never received)
%% - netsplit - nodes visibility is split but they are able to continue mining producing different chains
%% - malicious_miners - nodes which produce invalid blocks or wrong timings
%% - halted_chain - situations when the chain cannot make progress
all() ->
    [
        {group, gossip}
        % {group, netsplit},
        % {group, malicious_miners},
        % {group, halted_chain}
    ].

groups() ->
    [
        {gossip, [sequence], [
            start_child_nodes
        ]},
        {netsplit, [sequence], [
            start_child_nodes
        ]},
        {malicious_miners, [sequence], [
            start_child_nodes
        ]},
        {halted_chain, [sequence], [
            start_child_nodes
        ]}
    ].

suite() -> [].

init_per_suite(Config) ->
    case aect_test_utils:require_at_least_protocol(?CERES_PROTOCOL_VSN) of
        {skip, _} = Skip -> Skip;
        ok -> aehttp_hyperchains_utils:init_per_suite(Config, [?NODE1, ?NODE2])
    end.

end_per_suite(Config) ->
    aehttp_hyperchains_utils:end_per_suite(Config).

init_per_group(Group, Config) ->
    aehttp_hyperchains_utils:init_per_group(Group, Config).

end_per_group(Group, Config) ->
    aehttp_hyperchains_utils:end_per_group(Group, Config).

init_per_testcase(start_child_nodes, Config) ->
    %% TODO: Support more than 2 child nodes
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
% init_per_testcase(sync_third_node, Config) ->
%     Config1 = aehttp_hyperchains_utils:with_saved_keys([nodes], Config),
%     Nodes = ?config(nodes, Config1),
%     Config2 = lists:keyreplace(
%         nodes,
%         1,
%         Config1,
%         {nodes, Nodes ++ [{?NODE3, ?NODE3_NAME, []}]}
%     ),
%     aect_test_utils:setup_testcase(Config2),
%     Config2;
init_per_testcase(_Case, Config) ->
    Config1 = aehttp_hyperchains_utils:with_saved_keys([nodes], Config),
    aect_test_utils:setup_testcase(Config1),
    Config1.

end_per_testcase(_Case, Config) ->
    [catch peer:stop(Node) || {_, Node} <- ?config(nodes, Config)],
    Config.

%%
%%--------------------------------------------------------------------
%%

start_child_nodes(Config) ->
    Env = [{"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(?config(network_id, Config))}],
    [
        begin
            aehttp_hyperchains_utils:child_node_config(Node, Stakers, Config),
            % Use a new implementation of start_node
            {ok, _Pid, LongName} = aehttp_hyperchains_utils:start_node(Node, Config, Env),
            aehttp_hyperchains_utils:connect(LongName, [])
        end
     || {Node, _NodeName, Stakers} <- ?config(nodes, Config)
    ],
    ok.
