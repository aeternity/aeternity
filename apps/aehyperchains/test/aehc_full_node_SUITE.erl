-module(aehc_full_node_SUITE).

-include_lib("common_test/include/ct.hrl").

-define(SIM_VIEW, <<"chain_sim">>).
-define(SIM_CONNECTOR, <<"aehc_chain_sim_connector">>).

%% Test server callbacks
-export([ suite/0
        , all/0
        , groups/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_group/2
        , end_per_group/2
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ simple_full_node/1
        ]).

all() ->
    [{group, full_node}].

groups() ->
    [
        {full_node, [sequence], [simple_full_node]}
    ].

suite() ->
    [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

simple_full_node(_Config) ->
    ct:pal("Running test case simple_full_node/1~n", []),
    application:ensure_started(gproc),
    DefaultGenesisState = aec_block_genesis:genesis_block_with_state(),
    {ok, Pid} = aehc_chain_sim_connector:start_link(#{<<"genesis_state">> => DefaultGenesisState}),
    ok.
