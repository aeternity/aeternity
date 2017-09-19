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

setup() ->
    application:start(crypto).

teardown(_) ->
    application:stop(crypto).

-endif.
