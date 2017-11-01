-module(aec_eunit_SUITE).

-export([all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).

-include_lib("common_test/include/ct.hrl").
-compile({parse_transform, ct_eunit_xform}).

all() ->
    [{group, eunit}].

groups() ->
    [].

suite() ->
    [].

init_per_suite(Config) ->
    eunit:start(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Grp, Config) ->
    Config.

end_per_group(_Grp, _Config) ->
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.
