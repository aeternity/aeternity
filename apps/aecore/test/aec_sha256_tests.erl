%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_sha256 module
%%% @end
%%%=============================================================================
-module(aec_sha256_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("sha256.hrl").

-define(TEST_MODULE, aec_sha256).

all_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Hash a binary",
       fun() ->
               ?assertEqual(?HASH_BYTES, size(?TEST_MODULE:hash(<<"hello there!">>)))
       end},
      {"Hash an erlang term",
       fun() ->
               ?assertEqual(?HASH_BYTES, size(?TEST_MODULE:hash({a, b, c})))
       end}
     ]
    }.

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

setup() ->
    application:start(crypto).

teardown(_) ->
    application:stop(crypto).

-endif.
