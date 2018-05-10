%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aetx_sign_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aecore/include/blocks.hrl").

-define(TEST_MODULE, aetx_sign).

sign_txs_test_() ->
    {setup,
     fun() ->
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpKeysDir) ->
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir)
     end,
     [{"Key pair is able to sign and validate transaction",
       fun() ->
               #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
               {ok, CB} = aec_coinbase_tx:new(#{account => Pubkey,
                                                block_height => 1}),
               Signed = ?TEST_MODULE:sign(CB, Privkey),
               ?assertEqual(ok, ?TEST_MODULE:verify(Signed, aec_trees:new())),
               ok
      end},
      {"Mismatched keys do produce invalid signatures",
       fun() ->
               #{ public :=  PubkeyA, secret := _PrivkeyA } = enacl:sign_keypair(),
               #{ public := _PubkeyB, secret :=  PrivkeyB } = enacl:sign_keypair(),
               {ok, CB} = aec_coinbase_tx:new(#{account => PubkeyA,
                                                block_height => 1}),
               Signed = ?TEST_MODULE:sign(CB, PrivkeyB),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify(Signed, aec_trees:new())),
               ok
      end},
      {"Broken pub key does not validate signatures",
       fun() ->
               #{ public := _Pubkey, secret := Privkey } = enacl:sign_keypair(),
               {ok, CB} = aec_coinbase_tx:new(#{account => <<0:42/unit:8>>,
                                                block_height => 1}),
               Signed = ?TEST_MODULE:sign(CB, Privkey),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify(Signed, aec_trees:new())),
               ok
      end},
      {"Broken priv key does not produce signatures",
       fun() ->
               #{ public := Pubkey, secret := _Privkey } = enacl:sign_keypair(),
               {ok, CB} = aec_coinbase_tx:new(#{account => Pubkey,
                                                block_height => 1}),
               ?_assertException(error, invalid_priv_key,
                                 ?TEST_MODULE:sign(CB, <<0:42/unit:8>>)),
               ok
      end}
     ]}.
