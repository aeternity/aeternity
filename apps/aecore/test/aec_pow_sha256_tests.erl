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
-include("pow.hrl").

-define(TEST_MODULE, aec_pow_sha256).

pow_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     begin
         [{"Fail if retry count is zero",
           fun() ->
                   %% succeeds in a single step
                   ?assertEqual({error, generation_count_exhausted},
                            ?TEST_MODULE:generate(<<"hello there">>, ?HIGHEST_TARGET_SCI, 0))
           end},
          {"Generate with very high target threshold",
           fun() ->
               ?assertEqual(1, aec_pow:pick_nonce()),
                   %% succeeds in a single step
                   {T1, Res} = timer:tc(?TEST_MODULE, generate, [<<"hello there">>, ?HIGHEST_TARGET_SCI, 1]),
               ?debugFmt("~nReceived result ~p~nin ~p microsecs~n~n", [Res, T1]),
                   ?assertEqual({ok, {1, no_value}}, Res),

                   %% verify
                   {T2, Res2} = timer:tc(?TEST_MODULE, verify,
                                         [<<"hello there">>, 1, no_value, ?HIGHEST_TARGET_SCI]),
                   ?debugFmt("~nVerified in ~p microsecs~n~n", [T2]),
               ?assertEqual(true, Res2)
           end}
         ]
     end
    }.

setup() ->
    crypto:start(),
    meck:new(aec_pow, [passthrough]),
    meck:expect(aec_pow, pick_nonce, fun() -> 1 end).

teardown(_) ->
    meck:validate(aec_pow),
    meck:unload(aec_pow),
    crypto:stop().

-endif.
