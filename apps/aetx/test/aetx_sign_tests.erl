%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aetx_sign_tests).

-include_lib("eunit/include/eunit.hrl").

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
               {ok, SpendTx} = make_spend_tx(Pubkey),
               Signed = ?TEST_MODULE:sign(SpendTx, Privkey),
               ?assertEqual(ok, ?TEST_MODULE:verify(Signed, aec_trees:new())),
               ok
      end},
      {"Mismatched keys do produce invalid signatures",
       fun() ->
               #{ public :=  PubkeyA, secret := _PrivkeyA } = enacl:sign_keypair(),
               #{ public := _PubkeyB, secret :=  PrivkeyB } = enacl:sign_keypair(),
               {ok, SpendTx} = make_spend_tx(PubkeyA),
               Signed = ?TEST_MODULE:sign(SpendTx, PrivkeyB),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify(Signed, aec_trees:new())),
               ok
      end},
      {"Broken pub key does not validate signatures",
       fun() ->
               #{ public := _Pubkey, secret := Privkey } = enacl:sign_keypair(),
               {ok, SpendTx} = make_spend_tx(<<0:42/unit:8>>),
               Signed = ?TEST_MODULE:sign(SpendTx, Privkey),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify(Signed, aec_trees:new())),
               ok
      end},
      {"Broken priv key does not produce signatures",
       fun() ->
               BrokenKey = <<0:42/unit:8>>,
               {ok, SpendTx} = make_spend_tx(BrokenKey),
               ?_assertException(error, {invalid_priv_key, [BrokenKey]},
                                 ?TEST_MODULE:sign(SpendTx, <<0:42/unit:8>>)),
               ok
      end},
      {"Missing channel does not validate",
       fun() ->
               SignedCloseMTx = make_signed_mutual_close(),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify(SignedCloseMTx, aec_trees:new())),
               ok
      end}
     ]}.

make_spend_tx(Sender) ->
    #{ public := OtherPubkey} = enacl:sign_keypair(),
    {ok, _SpendTx} = aec_spend_tx:new(#{sender => Sender,
                                        recipient => OtherPubkey,
                                        amount => 4,
                                        fee => 1,
                                        ttl => 100,
                                        nonce => 1,
                                        payload => <<>>}).
make_signed_mutual_close() ->
    #{ secret:= PrivKey1} = enacl:sign_keypair(),
    #{ secret := PrivKey2} = enacl:sign_keypair(),
    {ok, Tx} = aesc_close_mutual_tx:new(#{channel_id        => <<0:42/unit:8>>,
                                          initiator_amount  => 42,
                                          responder_amount  => 24,
                                          ttl               => 1000,
                                          fee               => 10,
                                          nonce             => 1234}),
    aetx_sign:sign(Tx, [PrivKey1, PrivKey2]).
