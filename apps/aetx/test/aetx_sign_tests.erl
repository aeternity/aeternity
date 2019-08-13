%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aetx_sign_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../../aecore/include/blocks.hrl").
-include("../../aecontract/include/hard_forks.hrl").

-define(TEST_MODULE, aetx_sign).
-define(TEST_HEIGHT, 42).

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
               Signed = aec_test_utils:sign_tx(SpendTx, Privkey),
               ?assertEqual(ok, ?TEST_MODULE:verify(Signed, aec_trees:new(), ?TEST_HEIGHT)),
               ok
       end},
      {"Mismatched keys do produce invalid signatures",
       fun() ->
               #{ public :=  PubkeyA, secret := _PrivkeyA } = enacl:sign_keypair(),
               #{ public := _PubkeyB, secret :=  PrivkeyB } = enacl:sign_keypair(),
               {ok, SpendTx} = make_spend_tx(PubkeyA),
               Signed = aec_test_utils:sign_tx(SpendTx, PrivkeyB),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify(Signed, aec_trees:new(), ?TEST_HEIGHT)),
               ok
       end},
      {"Broken pub key does not validate signatures",
       fun() ->
               #{ public := _Pubkey, secret := Privkey } = enacl:sign_keypair(),
               {ok, SpendTx} = make_spend_tx(<<0:32/unit:8>>),
               Signed = aec_test_utils:sign_tx(SpendTx, Privkey),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify(Signed, aec_trees:new(), ?TEST_HEIGHT)),
               ok
       end},
      {"Broken priv key does not produce signatures",
       fun() ->
               BrokenPrivKey = <<0:65/unit:8>>,
               {ok, SpendTx} = make_spend_tx(<<0:32/unit:8>>),
               ?assertException(error, {invalid_priv_key, [BrokenPrivKey]},
                                aec_test_utils:sign_tx(SpendTx, BrokenPrivKey)),
               ok
       end},
      {"Missing channel does not validate",
       fun() ->
               SignedCloseMTx = make_signed_mutual_close(),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify(SignedCloseMTx, aec_trees:new(), ?TEST_HEIGHT)),
               ok
       end},
      {"Test tx hash signing",
       fun() ->
               #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
               {ok, SpendTx} = make_spend_tx(Pubkey),
               SignedTx = aec_test_utils:sign_tx_hash(SpendTx, Privkey),
               case lists:last(aec_hard_forks:sorted_protocol_versions()) of
                   Vsn when Vsn < ?LIMA_PROTOCOL_VSN ->
                       ?assertEqual({error, signature_check_failed},
                                    ?TEST_MODULE:verify(SignedTx, aec_trees:new(), ?TEST_HEIGHT));
                   _Vsn ->
                       ?assertEqual(ok,
                                    ?TEST_MODULE:verify(SignedTx, aec_trees:new(), ?TEST_HEIGHT))
               end
       end}
     ]}.

make_spend_tx(Sender) ->
    #{ public := OtherPubkey} = enacl:sign_keypair(),
    SenderId = aeser_id:create(account, Sender),
    Recipient = aeser_id:create(account, OtherPubkey),
    {ok, _SpendTx} = aec_spend_tx:new(#{sender_id => SenderId,
                                        recipient_id => Recipient,
                                        amount => 4,
                                        fee => 1,
                                        ttl => 100,
                                        nonce => 1,
                                        payload => <<>>}).
make_signed_mutual_close() ->
    #{ public := PubKey, secret:= PrivKey1} = enacl:sign_keypair(),
    #{ secret := PrivKey2} = enacl:sign_keypair(),
    Channel = aeser_id:create(channel, <<0:32/unit:8>>),
    FromId = aeser_id:create(account, PubKey),
    {ok, Tx} = aesc_close_mutual_tx:new(#{channel_id              => Channel,
                                          from_id                 => FromId,
                                          initiator_amount_final  => 42,
                                          responder_amount_final  => 24,
                                          ttl                     => 1000,
                                          fee                     => 10,
                                          nonce                   => 1234}),
    aec_test_utils:sign_tx(Tx, [PrivKey1, PrivKey2]).
