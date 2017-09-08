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

conversion_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Base-2 integer logarithm",
       fun() ->
               %% 16^6 = 2^24
               ?assertEqual(24, ?TEST_MODULE:log2(16#1000000))
       end},
      {"Integer to scientific conversion",
       fun() ->
               %% 24*256 + 128
               ?assertEqual(6272, ?TEST_MODULE:integer_to_scientific(16#1000000)),
               %% 24*256 + 145
               ?assertEqual(6289, ?TEST_MODULE:integer_to_scientific(16#1230000))
       end},
      {"Integer to scientific and back",
       fun() ->
               %% can be converted w/o losing accuracy
               ?assertEqual(16#100000, ?TEST_MODULE:scientific_to_integer(
                                          ?TEST_MODULE:integer_to_scientific(16#100000))),
               %% 0x1230000 = 1001 0001 100 000 000 000_2, we lose the last 1 in conversion
               ?assertEqual(16#122000, ?TEST_MODULE:scientific_to_integer(
                                          ?TEST_MODULE:integer_to_scientific(16#123000)))
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
               ?assertEqual(256*5 + 128, ?TEST_MODULE:recalculate_difficulty(256*4 + 128, 1000, 500))
       end}
     ]
    }.

setup() ->
    application:start(crypto).

teardown(_) ->
    application:stop(crypto).

-endif.
