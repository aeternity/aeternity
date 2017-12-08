%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Simulator for Proof of Work and difficulty adjustments.
%%% @end
%%%=============================================================================
-module(aec_pow_sim).

-compile([export_all, nowarn_export_all]).

-include("common.hrl").
-include("blocks.hrl").

%% -- Block API --------------------------------------------------------------

-define(ADJUST_WINDOW, 10).

genesis_block() ->
    #header{ height = 0, time = 0, target = ?HIGHEST_TARGET_SCI }.

new_block(_Prev = #header{ height = H, time = _T }, Now) ->
    #header{ height = H + 1, time   = Now }.

adjust_target(Top, _) when Top#header.height =< ?ADJUST_WINDOW -> Top;
adjust_target(Top, Chain) ->
    Target = aec_target:recalculate(Top, lists:sublist(Chain, ?ADJUST_WINDOW)),
    Top#header{ target = Target }.

difficulty(Hd) ->
    aec_pow:target_to_difficulty(Hd#header.target).

%% -- Chain simulation -------------------------------------------------------

test_chain(N, PC) ->
    Chain = mine_chain(N, PC),
    [ io:format("~10.2fs (+~8.2fs, rate: ~8.2fs) Block ~3b: difficulty ~9.2f\n",
        [H#header.time / 1000, (H#header.time - HP#header.time) / 1000, rate([H | lists:sublist(Prev, 50)]),
         H#header.height, difficulty(H)])
    || [H | Prev = [HP | _]] <- tails(Chain) ],
    ok.

rate(Xs) ->
    T0 = (lists:last(Xs))#header.time,
    T1 = (hd(Xs))#header.time,
    (T1 - T0) / (length(Xs) - 1) / 1000.

mine_chain(N, PC) -> mine_chain([genesis_block()], N, PC, 24 * 60 * 60 * 1000).

mine_chain(Chain, 0, _, _) -> Chain;
mine_chain(Chain, N, PC, Now) ->
    {Next, Later} = mining_step(Chain, PC, Now),
    mine_chain([Next | Chain], N - 1, PC, Later).

%% PoWCapacity = number of solutions per minute
mining_step(Chain = [Top | _], PoWCapacity, Now) ->
    Next = adjust_target(new_block(Top, Now), Chain),
    MiningTime = mining_time(Next, PoWCapacity),
    {Next, Now + MiningTime}.

%% -- Mining -----------------------------------------------------------------

mining_time(Top, PC) ->
    Attempts = mine(difficulty(Top)),
    round(Attempts / PC * 60 * 1000).

%% Simulate mining a block. Returns number of mining attempts before success.
mine(Difficulty) ->
  mine(Difficulty, 1).

mine(Difficulty, N) ->
    case rand:uniform() * Difficulty < 1.0 of
      true  -> N;
      false -> mine(Difficulty, N + 1)
    end.

%% -- Utils ------------------------------------------------------------------

tails(Xs) -> tails(Xs, []).

tails([], Acc)            -> lists:reverse([[] | Acc]);
tails(Xs = [_ | Tl], Acc) -> tails(Tl, [Xs | Acc]).

