%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_peers module
%%% @end
%%%=============================================================================
-module(aec_peers_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Add a peer by Uri",
       fun() ->
               ?assertEqual(ok, aec_peers:add("http://someone.somewhere:1337/baseuri", true))
       end},
      {"Get a random peer (from list of 1)",
       fun() ->
               [Uri] = aec_peers:get_random(1),
               ?assertEqual(<<"http://someone.somewhere:1337">>, Uri)
       end},
      {"Add a peer by object",
       fun() ->
               ?assertEqual(ok, aec_peers:add("http://someonelse.somewhereelse:1337/baseuri/", true))
       end},
      {"All and randomly getting peers",
       fun() ->
               ?assertEqual(2, length(aec_peers:all())),
               [Uri] = aec_peers:get_random(1),
               ?assert(lists:member(Uri, 
                                    [<<"http://someone.somewhere:1337">>,
                                     <<"http://someonelse.somewhereelse:1337">>])),
               ?assertEqual([<<"http://someonelse.somewhereelse:1337">>],
                            aec_peers:get_random(2, [<<"http://someone.somewhere:1337/">>]))
       end},
      {"Remove a peer",
       fun() ->
               %% Note that v1 is unimportant and ignored
               ?assertEqual(ok, aec_peers:remove("http://someone.somewhere:1337/v1")),
               ?assertEqual(1, length(aec_peers:all()))
       end},
      {"Remove all",
       fun do_remove_all/0},
      {"Random peer from nothing",
       fun() ->
               ?assertEqual([], aec_peers:get_random(2))
       end},
      {"Add peer",
       fun() ->
               ok = aec_peers:add("http://localhost:800", false),
               [{<<"http://localhost:800">>, _}] = aec_peers:all()
       end},
      {"Get random N",
       fun() ->
               do_remove_all(),
               Base = "http://localhost:",
               [ok = aec_peers:add(Base ++ integer_to_list(N), false)
                || N <- lists:seq(900,910)],
               L1 = aec_peers:get_random(5),
               5 = length(L1)
       end}

     ]
    }.

do_remove_all() ->
    [aec_peers:remove(P) || {P, _} <- aec_peers:all()],
    [] = aec_peers:all(),
    ok.


setup() ->
    application:ensure_started(crypto),
    application:ensure_started(gproc),
    aec_test_utils:fake_start_aehttp(), %% tricking aec_peers
    aec_peers:start_link(),
    ok.

teardown(_) ->
    gen_server:stop(aec_peers),
    application:stop(gproc),
    crypto:stop().

-endif.
