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
     [{"Ping the peer",
       fun() ->
               {ok, Peer}=aec_peers:get_random(),
               "http://localhost:8043/"=aec_peers:uri(Peer),
               ?assertEqual({ok, pong}, aeu_requests:ping(Peer))
       end}
     ]
    }.

setup() ->
    inets:start(),
    application:ensure_all_started(aehttp),
    aec_peers:start_link(),
    aec_peers:add("http://localhost:8043/").

teardown(_) ->
    aec_peers:remove("http://localhost:8043/"),
    application:stop(aehttp),
    inets:stop().
-endif.
