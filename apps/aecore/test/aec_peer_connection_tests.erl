%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_peer_connection_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aec_peer_connection).

deserialize_tx_test_() ->
    [{"A well-formed serialized signed tx deserializes successfully",
      fun() ->
              #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
              {ok, SpendTx} = make_spend_tx(Pubkey),
              Signed = aec_test_utils:sign_tx(SpendTx, Privkey),
              Bin = aetx_sign:serialize_to_binary(Signed),
              ?assertEqual({true, Signed}, ?TEST_MODULE:deserialize_tx(Bin))
      end},
     {"A malformed binary is dropped instead of raising",
      fun() ->
              ?assertEqual(false, ?TEST_MODULE:deserialize_tx(<<"not a valid signed tx">>))
      end},
     {"One malformed entry does not prevent deserializing the rest of a batch",
      fun() ->
              #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
              {ok, SpendTx} = make_spend_tx(Pubkey),
              Signed = aec_test_utils:sign_tx(SpendTx, Privkey),
              Bin = aetx_sign:serialize_to_binary(Signed),
              Batch = [Bin, <<"garbage">>, Bin],
              ?assertEqual([Signed, Signed], lists:filtermap(fun ?TEST_MODULE:deserialize_tx/1, Batch))
      end}
    ].

make_spend_tx(Sender) ->
    #{ public := OtherPubkey } = enacl:sign_keypair(),
    SenderId = aeser_id:create(account, Sender),
    Recipient = aeser_id:create(account, OtherPubkey),
    aec_spend_tx:new(#{ sender_id    => SenderId
                       , recipient_id => Recipient
                       , amount       => 4
                       , fee          => 1
                       , ttl          => 100
                       , nonce        => 1
                       , payload      => <<>>
                       }).
