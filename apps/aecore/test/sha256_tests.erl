%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the sha256 module
%%% @end
%%%=============================================================================
-module(sha256_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("sha256.hrl").

all_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Hash a binary",
       fun() ->
               ?assertEqual(?HASH_BYTES, size(sha256:hash(<<"hello there!">>)))
       end},
      {"Hash an erlang term",
       fun() ->
               ?assertEqual(?HASH_BYTES, size(sha256:hash({a, b, c})))
       end}
     ]
    }.

setup() ->
    application:start(crypto).

teardown(_) ->
    application:stop(crypto).

-endif.
