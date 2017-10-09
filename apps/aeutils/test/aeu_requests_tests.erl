%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aeu_requests module
%%% @end
%%%=============================================================================
-module(aeu_requests_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     []
    }.

setup() ->
    ok = application:ensure_started(inets),
    {ok, Apps} = application:ensure_all_started(aehttp),
    Apps.

teardown(Apps) ->
    [ok = application:stop(A) || A <- lists:reverse(Apps)],
    ok = application:stop(inets).


-endif.
