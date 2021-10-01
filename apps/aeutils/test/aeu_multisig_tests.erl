%%% File        : aeu_multisig_tests.erl
%%% Author      : Hans Svensson
%%% Description :
%%% Created     : 8 Oct 2021 by Hans Svensson
-module(aeu_multisig_tests).

-compile([export_all, nowarn_export_all]).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

multisig_2_users_test() ->
  AliceSK = crypto:strong_rand_bytes(32),
  BobSK   = crypto:strong_rand_bytes(32),

  Alice = aeu_ed25519_multisig:actor_new(AliceSK),
  Bob   = aeu_ed25519_multisig:actor_new(BobSK),

  aeu_ed25519_multisig:actor_add_user(Alice, Bob),

  timer:sleep(100),

  M = crypto:strong_rand_bytes(32),

  aeu_ed25519_multisig:actor_new_sig(Bob, test_msg, M),

  timer:sleep(100),

  {ok, Sig1} = aeu_ed25519_multisig:actor_get_sig(Alice, test_msg),
  {ok, Sig2} = aeu_ed25519_multisig:actor_get_sig(Bob, test_msg),

  {ok, SigX} = aeu_ed25519_multisig:actor_get_sig(Bob, test_msg),

  aeu_ed25519_multisig:actor_stop(Alice),
  aeu_ed25519_multisig:actor_stop(Bob),

  ?assertEqual(Sig1, Sig2),
  ?assertEqual(not_found, SigX),

  ok.

multisig_3_users_test() ->
  AliceSK = crypto:strong_rand_bytes(32),
  BobSK   = crypto:strong_rand_bytes(32),
  ClausSK = crypto:strong_rand_bytes(32),

  Alice = aeu_ed25519_multisig:actor_new(AliceSK),
  Bob   = aeu_ed25519_multisig:actor_new(BobSK),
  Claus = aeu_ed25519_multisig:actor_new(ClausSK),

  aeu_ed25519_multisig:actor_add_user(Alice, Bob),

  timer:sleep(100),

  aeu_ed25519_multisig:actor_add_user(Alice, Claus),

  timer:sleep(100),

  M = crypto:strong_rand_bytes(32),

  aeu_ed25519_multisig:actor_new_sig(Bob, test_msg, M),

  timer:sleep(100),
  {ok, Sig1} = aeu_ed25519_multisig:actor_get_sig(Alice, test_msg),
  {ok, Sig2} = aeu_ed25519_multisig:actor_get_sig(Bob, test_msg),
  {ok, Sig3} = aeu_ed25519_multisig:actor_get_sig(Claus, test_msg),

  aeu_ed25519_multisig:actor_stop(Alice),
  aeu_ed25519_multisig:actor_stop(Bob),
  aeu_ed25519_multisig:actor_stop(Claus),

  ?assertEqual(Sig1, Sig2),
  ?assertEqual(Sig2, Sig3),

  ok.


-endif.
