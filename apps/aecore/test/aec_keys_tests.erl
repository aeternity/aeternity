%%%-------------------------------------------------------------------
%%% @author sennui
%%% @copyright (C) 2017, Aeternity
%%% @doc
%%%
%%% @end
%%% Created : 04. Sep 2017 23:31
%%%-------------------------------------------------------------------
-module(aec_keys_tests).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpKeysDir) ->
             aec_test_utils:aec_keys_cleanup(TmpKeysDir)
     end,
     [fun(_) ->
              [ {"Promote signing keys candidate (positive case)",
                fun() ->
                        {ok, SPub0} = aec_keys:pubkey(),
                        {ok, CPub0} = aec_keys:candidate_pubkey(),
                        ok = aec_keys:promote_candidate(CPub0),
                        {ok, SPub1} = aec_keys:pubkey(),
                        {ok, CPub1} = aec_keys:candidate_pubkey(),
                        ?assertEqual(CPub0, SPub1),
                        ?assertNotEqual(SPub0, SPub1),
                        ?assertNotEqual(CPub0, CPub1)
                end},
                {"Promote signing keys candidate (negative case)",
                fun() ->
                        {ok, SPub0} = aec_keys:pubkey(),
                        {ok, CPub0} = aec_keys:candidate_pubkey(),
                        ?assertEqual({error, key_not_found}, aec_keys:promote_candidate(SPub0)),
                        ?assertEqual({ok, SPub0}, aec_keys:pubkey()),
                        ?assertEqual({ok, CPub0}, aec_keys:candidate_pubkey())
                end},
               {"Keys validation (positive case)",
                fun() ->
                        #{ public := Pubkey, secret := PrivKey} = enacl:sign_keypair(),
                        ValidPair = aec_keys:check_sign_keys(Pubkey, PrivKey),
                        ?assertEqual(true, ValidPair)
                end},
               {"Keys validation (negative case)",
                fun() ->
                        Pubkey = <<42:32/unit:8>>,
                        Privkey = <<42:64/unit:8>>,
                        ValidPair = aec_keys:check_sign_keys(Pubkey, Privkey),
                        ?assertEqual(false, ValidPair)
                end},
                {"Peer key validation (positive case)",
                fun() ->
                        KeyPair = enoise_keypair:new(dh25519),
                        Pubkey  = enoise_keypair:pubkey(KeyPair),
                        Privkey = enoise_keypair:seckey(KeyPair),
                        ValidPair = aec_keys:check_peer_keys(Pubkey, Privkey),
                        ?assertEqual(true, ValidPair)
                end},
               {"Peer key validation (negative case)",
                fun() ->
                        Pubkey = <<0:(32*8)>>,
                        Privkey = <<0:(32*8)>>,
                        ValidPair = aec_keys:check_peer_keys(Pubkey, Privkey),
                        ?assertEqual(false, ValidPair)
                end}]
      end]}.

start_test_() ->
    TF =
        fun() ->
                ?_test(
                   aec_test_utils:aec_keys_bare_cleanup(
                     aec_test_utils:aec_keys_bare_setup()))
        end,
    {setup,
     fun() -> ok = application:ensure_started(crypto) end,
     fun(_) -> ok = application:stop(crypto) end,
     lazy_gen(100, TF)}.

lazy_gen(N, TF) ->
    {generator,
     fun () ->
             if N > 0 ->
                     [TF()
                      | lazy_gen(N-1, TF)];
                true ->
                     []
             end
     end}.

-endif.
