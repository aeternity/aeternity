%%%=============================================================================
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% @end
%%%=============================================================================
-module(aec_target_tests).

-import(aec_test_utils, [running_apps/0, loaded_apps/0, restore_stopped_and_unloaded_apps/2]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("aeminer/include/aeminer.hrl").
-include("blocks.hrl").

-define(MINER_PUBKEY, <<42:?MINER_PUB_BYTES/unit:8>>).
-define(FAKE_TXS_TREE_HASH, <<0:?TXS_HASH_BYTES/unit:8>>).

target_adj_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"With constant PoW capacity the target will stabilize (seed = " ++ integer_to_list(S) ++ ")",
      fun() ->
          Seed = {1, S, S},
          rand:seed(exs1024s, Seed),
          PoWCapacity = 100,
          TargetSpeed = 1 / 3, %% 1 block per 3 minutes
          ExpectedDifficulty = PoWCapacity / TargetSpeed,
          InitBlocks = [aec_test_utils:genesis_block_with_state()],
          Chain = [Top | _] = mine_blocks_only_chain(InitBlocks, 500, PoWCapacity),
          Difficulties = [ hr_difficulty(B) || B <- Chain ],
          %% ?debugFmt("Difficulties: ~p", [Difficulties]),
          Window = 25,
          AvgDiffWindow = lists:sum(lists:sublist(Difficulties, Window)) / Window,
          RateWindow = 60,
          Time1 = aec_blocks:time_in_msecs(Top),
          Time2 = aec_blocks:time_in_msecs(lists:nth(RateWindow + 1, Chain)),
          AvgRate = 60 * 1000 * RateWindow / (Time1 - Time2),

          %% ?debugFmt("Diff: ~.2f Rate: ~.2f", [AvgDiffWindow/ExpectedDifficulty, AvgRate/TargetSpeed]),
          ?assertMatch(N when 0.5 < N andalso N < 2.0, AvgDiffWindow/ExpectedDifficulty),
          ?assertMatch(N when 0.75 < N andalso N < 1.25, AvgRate/TargetSpeed)

      end} || S <- lists:seq(1, 10) ]
     }.

setup() ->
    InitialApps = {running_apps(), loaded_apps()},
    {ok, _} = application:ensure_all_started(aeutils),
    meck:new(aec_txs_trees, [passthrough]),
    meck:new(aec_conductor, [passthrough]),
    meck:expect(aec_txs_trees, from_txs, fun([]) -> fake_txs_tree end),
    meck:expect(aec_txs_trees, root_hash, fun(fake_txs_tree) -> {ok, ?FAKE_TXS_TREE_HASH} end),
    {InitialApps, [aec_txs_trees, aec_conductor]}.

teardown({{OldRunningApps, OldLoadedApps}, Mocks}) ->
    [meck:unload(M) || M <- Mocks],
    ok = restore_stopped_and_unloaded_apps(OldRunningApps, OldLoadedApps).

mine_blocks_only_chain(Chain, N, PC) ->
    aec_test_utils:blocks_only_chain(mine_chain_with_state(Chain, N, PC)).

mine_chain_with_state(Chain, 0, _) -> Chain;
mine_chain_with_state(Chain, N, PC) ->
    {B, S} = mining_step(Chain, PC),
    mine_chain_with_state([{B, S} | Chain], N - 1, PC).

%% PoWCapacity = number of solutions per minute
mining_step(Chain = [{Top, _} | _], PoWCapacity) ->
    {Block, BlockState} = aec_test_utils:create_keyblock_with_state(Chain, ?MINER_PUBKEY),
    MiningTime = mining_time(Chain, PoWCapacity),
    TopTime    = aec_blocks:time_in_msecs(Top),
    Consensus = aec_blocks:consensus_module(Block),
    aec_consensus_bitcoin_ng = Consensus,
    {ok, NewBlock} =
        Consensus:keyblock_create_adjust_target(
          aec_blocks:set_time_in_msecs(Block, TopTime + MiningTime),
          [ aec_blocks:to_header(B) || B <- lists:sublist(aec_test_utils:blocks_only_chain(Chain), 18) ]),
    {NewBlock, BlockState}.

mining_time([_], _) -> 1000000000;
mining_time([{Top, _} | _], PC) ->
    Attempts = mine(hr_difficulty(Top)),
    round(Attempts / PC * 60 * 1000).

mine(Difficulty) ->
  mine(Difficulty, 1).

mine(Difficulty, N) ->
    case rand:uniform() * Difficulty < 1.0 of
      true  -> N;
      false -> mine(Difficulty, N + 1)
    end.

%% Human readable difficulty
hr_difficulty(Block) ->
    aec_blocks:difficulty(Block) / ?DIFFICULTY_INTEGER_FACTOR.
