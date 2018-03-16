%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_peers module
%%% @end
%%%=============================================================================
-module(aec_peers_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(A_KEY, <<32,85,25,13,96,60,236,26,225,111,56,107,78,47,70,220,104,39,95,162,186,6,196,171,235,241,179,126,68,226,208,123>>).

someone() ->
    #{ host => <<"someone.somewhere">>, port => 1337, pubkey => ?A_KEY }.

someoneelse() ->
    #{ host => <<"someoneelse.somewhereelse">>, port => 1337, pubkey => ?A_KEY }.

localhost() ->
    localhost(800).

localhost(Port) ->
    #{ host => <<"localhost">>, port => Port, pubkey => ?A_KEY }.


all_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Add a peer by Uri",
       fun() ->
               ?assertEqual(ok, aec_peers:add(someone()))
       end},
      {"Get a random peer (from list of 1)",
       fun() ->
               [Peer] = aec_peers:get_random(1),
               ?assertEqual(someone(), Peer)
       end},
      {"Add a peer by object",
       fun() ->
               ?assertEqual(ok, aec_peers:add(someoneelse()))
       end},
      {"All and randomly getting peers",
       fun() ->
               ?assertEqual(2, length(aec_peers:all())),
               [Peer] = aec_peers:get_random(1),
               ?assert(lists:member(Peer, [someone(), someoneelse()])),
               ?assertEqual([someoneelse()], aec_peers:get_random(2, [someone()]))
       end},
      {"Remove a peer",
       fun() ->
               %% Note that v1 is unimportant and ignored
               ?assertEqual(ok, aec_peers:remove(someone())),
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
               ok = aec_peers:add(localhost()),
               ?assertEqual([{localhost(), 0}], aec_peers:all())
       end},
      {"Get random N",
       fun() ->
               do_remove_all(),
               [ok = aec_peers:add(localhost(N)) || N <- lists:seq(900, 910)],
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
