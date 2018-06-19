%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_pow module
%%% @end
%%%=============================================================================
-module(aec_pow_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aec_pow).

-include("blocks.hrl").

-define(MINER_PUBKEY, <<42:?MINER_PUB_BYTES/unit:8>>).
-define(FAKE_TXS_TREE_HASH, <<0:?TXS_HASH_BYTES/unit:8>>).

conversion_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Integer to scientific conversion",
       fun() ->
               %% 02: shifted up 2 bytes to reach the [0x7fffff, 0x008000] range,
               %%  8: sign as shifted up, 10000: significand
               ?assertEqual(16#01010000, ?TEST_MODULE:integer_to_scientific(1)),
               %% 01: shifted up 1 byte, 8: shifted up, 0ff00: significand
               ?assertEqual(16#0200ff00, ?TEST_MODULE:integer_to_scientific(255)),
               ?assertEqual(16#02010000, ?TEST_MODULE:integer_to_scientific(256)),
               ?assertEqual(16#02010100, ?TEST_MODULE:integer_to_scientific(257)),
               %% iput: 1 more than the largest possible significand:
               %% shifted up 1 byte, the smallest possible significand
               ?assertEqual(16#04008000, ?TEST_MODULE:integer_to_scientific(16#800000)),
               %% same result: underflow
               ?assertEqual(16#04008000, ?TEST_MODULE:integer_to_scientific(16#800001)),
               %% example from BitCoin Wiki:
               %% https://en.bitcoin.it/wiki/Difficulty#How_is_difficulty_calculated.3F_What_is_the_difference_between_bdiff_and_pdiff.3F: (256-bit hash: 64 hex digits)
               ?assertEqual(16#1b0404cb,
                            ?TEST_MODULE:integer_to_scientific(
                               16#00000000000404CB000000000000000000000000000000000000000000000000)),
               %% highest possible target in bitcoin
               ?assertEqual(16#1d00ffff,
                            ?TEST_MODULE:integer_to_scientific(16#00000000FFFF0000000000000000000000000000000000000000000000000000)),
               %% highest expressible number
               ?assertEqual(?HIGHEST_TARGET_SCI,
                            ?TEST_MODULE:integer_to_scientific(?HIGHEST_TARGET_INT))
       end},
      {"Scientific to integer conversion",
       fun() ->               ?assertEqual(1, ?TEST_MODULE:scientific_to_integer(16#01010000)),
                              ?assertEqual(255, ?TEST_MODULE:scientific_to_integer(16#0200ff00)),
                              ?assertEqual(16#800000, ?TEST_MODULE:scientific_to_integer(16#04008000)),
                              ?assertEqual(?HIGHEST_TARGET_INT,
                                           aec_pow:scientific_to_integer(?HIGHEST_TARGET_SCI))
       end},
      {"Integer to scientific and back",
       fun() ->
               %% can be converted w/o losing accuracy
               ?assertEqual(1, ?TEST_MODULE:scientific_to_integer(
                                  ?TEST_MODULE:integer_to_scientific(1))),
               ?assertEqual(255, ?TEST_MODULE:scientific_to_integer(
                                    ?TEST_MODULE:integer_to_scientific(255))),
               ?assertEqual(256, ?TEST_MODULE:scientific_to_integer(
                                    ?TEST_MODULE:integer_to_scientific(256))),
               %% losing accuracy (last digit: 257 = 1 0000 0001_2)
               ?assertEqual(257, ?TEST_MODULE:scientific_to_integer(
                                    ?TEST_MODULE:integer_to_scientific(257))),
               %% can be converted w/o losing accuracy
               ?assertEqual(16#800000, ?TEST_MODULE:scientific_to_integer(
                                          ?TEST_MODULE:integer_to_scientific(16#800000))),
               %% can be converted w/o losing accuracy
               ?assertEqual(16#800000, ?TEST_MODULE:scientific_to_integer(
                                          ?TEST_MODULE:integer_to_scientific(16#800001))),
               %% can be converted w/o losing accuracy
               Num1 = 16#00000000000404CB000000000000000000000000000000000000000000000000,
               ?assertEqual(Num1, ?TEST_MODULE:scientific_to_integer(
                                     ?TEST_MODULE:integer_to_scientific(Num1))),
               Num2 = 16#00000000FFFF0000000000000000000000000000000000000000000000000000,
               %% 0x1230000 = 1 0010 0011 0000 0000 0000_2, we lose the last 1 in conversion
               ?assertEqual(Num2, ?TEST_MODULE:scientific_to_integer(
                                     ?TEST_MODULE:integer_to_scientific(Num2)))
       end},
      {"Testing difficulty",
       fun() ->
               %%----------------------------------------------------------------------
               %% More than 3 nonzero bytes
               %%----------------------------------------------------------------------

               ?assertEqual(true, ?TEST_MODULE:test_target(
                                     <<0,0,0,0,0,16#04,16#04,16#ca,
                                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
                                     16#1b0404cb)),
               ?assertEqual(false, ?TEST_MODULE:test_target(
                                      <<0,0,0,0,0,16#04,16#04,16#cc,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>,
                                      16#1b0404cb)),
               ?assertEqual(false, ?TEST_MODULE:test_target(
                                      <<0,0,1,0,0,16#04,16#04,16#ca,
                                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>,
                                      16#1b0404cb)),
               %%----------------------------------------------------------------------
               %% Less than 3 nonzero bytes
               %%----------------------------------------------------------------------

               %% 0403 < 0404
               ?assertEqual(true, ?TEST_MODULE:test_target(
                                     <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,16#04,16#03>>,
                                     16#020404cb)),
               %% 0404 < 0404 fails
               ?assertEqual(false, ?TEST_MODULE:test_target(
                                     <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,16#04,16#04>>,
                                      16#020404cb)),
               %% 0405 < 0405 fails
               ?assertEqual(false, ?TEST_MODULE:test_target(
                                     <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,16#04,16#05>>,
                                      16#020404cb)),
               %% hide a 1 among zeros
               ?assertEqual(false, ?TEST_MODULE:test_target(
                                     <<0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,16#04,16#03>>,
                                      16#020404cb)),
               %% 03 < 04
               ?assertEqual(true, ?TEST_MODULE:test_target(
                                     <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,16#03>>,
                                      16#010404cb)),
               %% 04 < 04 fails
               ?assertEqual(false, ?TEST_MODULE:test_target(
                                     <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,16#04>>,
                                      16#010404cb)),

               %%----------------------------------------------------------------------
               %% Exp > size of binary
               %%----------------------------------------------------------------------

               %% fffe < ffff
               ?assertEqual(true, ?TEST_MODULE:test_target(
                                     <<255,254,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0>>,
                                      16#2100ffff)),
               %% fffffe < ffff00 fails
               ?assertEqual(false, ?TEST_MODULE:test_target(
                                     <<255,255,254,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0>>,
                                      16#2100ffff)),

               %%----------------------------------------------------------------------
               %% Negative exp marked in the sign og significand
               %%----------------------------------------------------------------------

               ?assertEqual(true, ?TEST_MODULE:test_target(
                                     <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0>>,
                                     16#028404cb)),
               ?assertEqual(false, ?TEST_MODULE:test_target(
                                     <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,16#04,16#03>>,
                                     16#028404cb))
       end},
      {"Threshold to difficulty",
       fun() ->
               %% More than 3 nonzero bytes
               Diff = ?TEST_MODULE:target_to_difficulty(16#1b0404cb),
               ?assert(Diff > 70039839613066.1),
               ?assert(Diff < 70039839613066.2)
       end}
     ]
    }.

target_adj_test_() ->
    {setup,
     fun setup_target/0,
     fun teardown_target/1,
     [{"With constant PoW capacity the target will stabilize (seed = " ++ integer_to_list(S) ++ ")",
      fun() ->
          Seed = {1, 1, S},
          rand:seed(exs1024s, Seed),
          PoWCapacity = 100,
          TargetSpeed = 1 / 5, %% 1 block per 5 minutes
          ExpectedDifficulty = PoWCapacity / TargetSpeed,
          InitBlocks = [aec_test_utils:genesis_block_with_state()],
          Chain = [Top | _] = mine_blocks_only_chain(InitBlocks, 100, PoWCapacity),
          Difficulties = [ aec_blocks:difficulty(B) || B <- Chain ],
          %% ?debugFmt("Difficulties: ~p", [Difficulties]),
          Window = 20,
          AvgDiffWindow = lists:sum(lists:sublist(Difficulties, Window)) / Window,
          RateWindow = 50,
          AvgRate = 60 * 1000 * RateWindow / (Top#block.time - (lists:nth(RateWindow + 1, Chain))#block.time),

          ?assertMatch(N when 0.5 < N andalso N < 2.0, AvgDiffWindow/ExpectedDifficulty),
          ?assertMatch(N when 0.75 < N andalso N < 1.25, AvgRate/TargetSpeed)

      end} || S <- lists:seq(1, 10) ]
     }.

setup_target() ->
    setup(),
    meck:new(aec_txs_trees, [passthrough]),
    meck:new(aec_conductor, [passthrough]),
    meck:expect(aec_txs_trees, from_txs, fun([]) -> fake_txs_tree end),
    meck:expect(aec_txs_trees, root_hash, fun(fake_txs_tree) -> {ok, ?FAKE_TXS_TREE_HASH} end).

teardown_target(X) ->
    meck:unload(aec_txs_trees),
    meck:unload(aec_conductor),
    teardown(X).

mine_blocks_only_chain(Chain, N, PC) ->
    aec_test_utils:blocks_only_chain(mine_chain_with_state(Chain, N, PC)).

mine_chain_with_state(Chain, 0, _) -> Chain;
mine_chain_with_state(Chain, N, PC) ->
    {B, S} = mining_step(Chain, PC),
    mine_chain_with_state([{B, S} | Chain], N - 1, PC).

%% PoWCapacity = number of solutions per minute
mining_step(Chain = [{Top, TopState} | _], PoWCapacity) ->
    FeesInfo = #{txs => 0, gas => 0},
    {Block, BlockState} = aec_block_key_candidate:create_with_state(Top, ?MINER_PUBKEY, TopState, FeesInfo),
    MiningTime = mining_time(Chain, PoWCapacity),
    {ok, NewBlock} =
        aec_block_key_candidate:adjust_target(
          Block#block{ time = Top#block.time + MiningTime },
          [ aec_blocks:to_header(B) || B <- lists:sublist(aec_test_utils:blocks_only_chain(Chain), 10) ]),
    {NewBlock, BlockState}.

mining_time([_], _) -> 1000000000;
mining_time([{Top, _} | _], PC) ->
    Attempts = mine(aec_blocks:difficulty(Top)),
    round(Attempts / PC * 60 * 1000).

mine(Difficulty) ->
  mine(Difficulty, 1).

mine(Difficulty, N) ->
    case rand:uniform() * Difficulty < 1.0 of
      true  -> N;
      false -> mine(Difficulty, N + 1)
    end.

setup() ->
    application:start(crypto).

teardown(_) ->
    application:stop(crypto).

-endif.
