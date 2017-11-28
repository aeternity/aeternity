-module(contract_SUITE).

-export([all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).

-export([eunit_tests/1]).

-include_lib("common_test/include/ct.hrl").
%% -compile({parse_transform, ct_eunit_xform}).

all() ->
    [{group, eunit}].

groups() ->
    [{eunit, [sequence], [eunit_tests]}].

suite() ->
    [].

init_per_suite(Config) ->
    eunit:start(),
    DataDir       = ?config(data_dir, Config),
    {ok, TestDir} = file:get_cwd(),
    [{top_dir, top_dir(DataDir)},
     {test_dir, TestDir} | Config].

%% Split the DataDir path at "_build"
top_dir(DataDir) ->
    [Top, _] = re:split(DataDir, "_build", []),
    Top.

end_per_suite(_Config) ->
    ok.

init_per_group(_Grp, Config) ->
    Config.

end_per_group(_Grp, _Config) ->
    ok.

init_per_testcase(_TC, Config) ->
    %% Hack to run the eunit tests in the project root.
    file:set_cwd(?config(top_dir, Config)),
    Config.

end_per_testcase(_TC, Config) ->
    file:set_cwd(?config(test_dir, Config)).

eunit_tests(_Config) ->
    ok = contract_tests:test().
