%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Simulator for Proof of Work and difficulty adjustments.
%%% @end
%%%=============================================================================
-module(aec_pow_sim).

-compile([export_all, nowarn_export_all]).

-include_lib("aeminer/include/aeminer.hrl").
-include("blocks.hrl").

-import(aec_headers, [raw_key_header/0]).

%% -- Block API --------------------------------------------------------------

-define(ADJUST_WINDOW, 10).

genesis_block() ->
    Vsn = aec_block_genesis:version(),
    Height = aec_block_genesis:height(),
    H0 = aec_headers:set_version_and_height(raw_key_header(), Vsn, Height),
    H1 = aec_headers:set_time_in_msecs(H0, 0),
    aec_headers:set_target(H1, ?HIGHEST_TARGET_SCI).

new_block(Prev, Now) ->
    H0 = raw_key_header(),
    H1 = aec_headers:set_height(H0, aec_headers:height(Prev) + 1),
    aec_headers:set_time_in_msecs(H1, Now).

adjust_target(Top, Chain) ->
    case aec_headers:height(Top) =< ?ADJUST_WINDOW of
        true  -> Top;
        false ->
            Target = aec_target:recalculate(lists:sublist(Chain, ?ADJUST_WINDOW)),
            aec_headers:set_target(Top, Target)
    end.

difficulty(Hd) ->
    aeminer_pow:target_to_difficulty(aec_headers:target(Hd)).

%% -- Chain simulation -------------------------------------------------------

test_chain(N, PC) ->
    Chain = mine_chain(N, PC),
    [begin
         Time1 = aec_headers:time_in_msecs(H),
         Time2 = aec_headers:time_in_msecs(HP),
         io:format("~10.2fs (+~8.2fs, rate: ~8.2fs) Block ~3b: difficulty ~9.2f\n",
                   [Time1 / 1000, (Time1 - Time2) / 1000, rate([H | lists:sublist(Prev, 50)]),
                    aec_headers:height(H), difficulty(H)])
     end
     || [H | Prev = [HP | _]] <- tails(Chain) ],
    ok.

rate(Xs) ->
    T0 = aec_headers:time_in_msecs(lists:last(Xs)),
    T1 = aec_headers:time_in_msecs(hd(Xs)),
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

