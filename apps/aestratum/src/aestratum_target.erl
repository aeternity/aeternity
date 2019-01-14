-module(aestratum_target).

%% TODO: eunit
%% TODO: type spec

%% this should be included from a dependency - aeminer?
-define(MAX_TARGET, 16#ffff000000000000000000000000000000000000000000000000000000000000).

-export([recalculate/3,
         diff/2,
         max/0,
         to_hex/1,
         to_int/1
        ]).

recalculate(PrevTargets, DesiredSolveTime, MaxTarget) when
      PrevTargets =/= [] ->
    N = length(PrevTargets),
    K = MaxTarget * (1 bsl 32),
    {SumKDivTargets, SumSolveTime} =
        lists:foldl(
          fun({Target, SolveTime}, {SumKDivTargets0, SumSolveTime0}) ->
                  {(K div Target) + SumKDivTargets0, SolveTime + SumSolveTime0}
          end, {0, 0}, PrevTargets),
    TemperedTST = (3 * N * DesiredSolveTime) div 4 + (2523 * SumSolveTime) div 10000,
    NewTarget = TemperedTST * K div (DesiredSolveTime * SumKDivTargets),
    min(MaxTarget, NewTarget);
recalculate([], _DesiredSolveTime, MaxTarget) ->
    MaxTarget.

diff(NewTarget, OldTarget) when NewTarget > OldTarget ->
    {increase, (NewTarget - OldTarget) / OldTarget * 100};
diff(NewTarget, OldTarget) when NewTarget < OldTarget ->
    {decrease, (OldTarget - NewTarget) / OldTarget * 100};
diff(Target, Target) ->
    no_change.

max() ->
    ?MAX_TARGET.

to_hex(Target) ->
    iolist_to_binary(io_lib:format("~64.16.0b", [Target])).

to_int(Bin) ->
    binary_to_integer(Bin, 16).

