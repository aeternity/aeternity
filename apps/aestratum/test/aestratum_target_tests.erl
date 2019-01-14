-module(aestratum_target_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aestratum_target).
-define(MAX_TARGET, 16#ffff000000000000000000000000000000000000000000000000000000000000).

%recalculate(PrevTargets, DesiredSolveTime, MaxTarget) when

no_prev_targets_test() ->
    L = [],
    ?assertEqual(?MAX_TARGET, ?TEST_MODULE:recalculate(L, 30000, ?MAX_TARGET)).

target_no_change_test() ->
    HighTarget = ?MAX_TARGET,
    SolveTime = 10000,
    L = [{HighTarget, SolveTime}],
    ?assertEqual(HighTarget, ?TEST_MODULE:recalculate(L, SolveTime, HighTarget)).

target_increase_test() ->
    LowTarget = 1,
    L = [{LowTarget, 100000}],
    ?assertMatch(T when T > LowTarget, ?TEST_MODULE:recalculate(L, 15000, ?MAX_TARGET)).

target_decrease_test() ->
   HighTarget = ?MAX_TARGET,
   L = [{HighTarget, 10}],
   ?assertMatch(T when T < HighTarget, ?TEST_MODULE:recalculate(L, 15000, ?MAX_TARGET)).

