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

-define(TEST_BIN, <<"hello there">>).

pow_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Generate with very high target threshold",
       fun() ->
               Target = ?HIGHEST_TARGET_SCI,
               Nonce = 1,
               Soln = no_value,
               {T1, Res} =
                   timer:tc(?TEST_MODULE, generate,
                            [?TEST_BIN, Target, Nonce]),
               ?debugFmt("~nReceived result ~p~nin ~p microsecs~n~n", [Res, T1]),
               ?assertEqual({ok, {Nonce, Soln}}, Res),

               %% verify the nonce
               {T2, Res2} = timer:tc(?TEST_MODULE, verify,
                                     [?TEST_BIN, Nonce, Soln, Target]),
               ?debugFmt("~nVerified in ~p microsecs~n~n", [T2]),
               ?assert(Res2)
       end},
      {"Generate with low target threshold, shall fail",
       fun() ->
               Target = 16#01010000,
               Nonce = 1,
               Res = ?TEST_MODULE:generate(?TEST_BIN, Target, Nonce),
               ?assertEqual({error, no_solution}, Res)
       end}
     ]
    }.

setup() ->
    crypto:start().

teardown(_) ->
    crypto:stop().

-endif.
