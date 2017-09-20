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

-include("pow.hrl").

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
               ?debugFmt("Diff: ~p~n", [Diff]),
               ?assert(Diff > 70039839613066.1),
               ?assert(Diff < 70039839613066.2)
       end}
     ]
    }.

recalc_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Adjust difficulty",
       fun() ->
               %% block building progressed twice as fast as expected, double difficulty
               D1 = ?TEST_MODULE:integer_to_scientific(16#400000),
               D2 = ?TEST_MODULE:integer_to_scientific(16#800000),
               ?assertEqual(D2, ?TEST_MODULE:recalculate_difficulty(D1, 1000, 500))
       end}
     ]
    }.

setup() ->
    application:start(crypto).

teardown(_) ->
    application:stop(crypto).

-endif.
