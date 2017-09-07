%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_pow_sha256 module
%%% @end
%%%=============================================================================
-module(aec_pow_sha256_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("sha256.hrl").

-define(TEST_MODULE, aec_pow_sha256).

pow_test_() ->
    {setup,
     fun setup_pow/0,
     fun teardown_pow/1,
     [{"Fail if retry count is zero",
       fun() ->
               %% succeeds in a single step
               ?assertEqual({error, generation_count_exhausted}, ?TEST_MODULE:generate(<<"hello there">>, 5555, 0))
       end},
      {"Generate with very large difficulty",
       fun() ->
               ?assertEqual(1, ?TEST_MODULE:pick_nonce()),
               %% succeeds in a single step
               BigDiff = 256*256 + 255 + 1,
               ?assertEqual({ok, 1}, ?TEST_MODULE:generate(<<"hello there">>, BigDiff, 1))
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
    crypto:start().

teardown(_) ->
    crypto:stop().

setup_pow() ->
    setup(),
    meck:new(?TEST_MODULE, [passthrough]),
    meck:expect(?TEST_MODULE, pick_nonce, fun() -> 1 end).

teardown_pow(Cfg) ->
    meck:validate(?TEST_MODULE),
    meck:unload(?TEST_MODULE),
    teardown(Cfg).

-endif.
