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

%% The tx gossip wire message is pre-encoded once and pushed to every peer
%% verbatim, so it must be a complete, self-describing message - the same shape
%% the receive path in handle_info({noise, _, <<Type:16, Payload/binary>>}, _)
%% expects. If this regresses to the inner tx only, gossiped txs stop parsing.
gossip_serialize_tx_test_() ->
    [{"Produces a tagged txs message that round-trips back to the signed tx",
      fun() ->
              #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
              {ok, SpendTx} = make_spend_tx(Pubkey),
              Signed = aec_test_utils:sign_tx(SpendTx, Privkey),

              <<Tag:16, Payload/binary>> = ?TEST_MODULE:gossip_serialize_tx(Signed),
              ?assertEqual(aec_peer_messages:tag(txs), Tag),

              {txs, _Vsn, #{ txs := [SerTx] }} =
                  aec_peer_messages:deserialize(Tag, Payload),
              ?assertEqual({true, Signed}, ?TEST_MODULE:deserialize_tx(SerTx))
      end},
     {"Is byte-identical across calls, so one encoding can serve every peer",
      fun() ->
              #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
              {ok, SpendTx} = make_spend_tx(Pubkey),
              Signed = aec_test_utils:sign_tx(SpendTx, Privkey),
              ?assertEqual(?TEST_MODULE:gossip_serialize_tx(Signed),
                           ?TEST_MODULE:gossip_serialize_tx(Signed))
      end}
    ].

%% Coverage for the ping-version negotiation that only ever takes a real
%% "downgrade" branch when talking to an older peer (aest_peers_SUITE's
%% test_old_peer_discovery is the only place that exercises this against a
%% real historical binary; this covers the same policy without needing one).
use_latest_common_ping_version_test_() ->
    [{"Negotiates down to the only version an older peer advertises",
      fun() ->
              PingObj = #{versions => [#{protocol => <<"ping">>, vsns => [1]}]},
              #{use_ping_vsn := Vsn} = ?TEST_MODULE:use_latest_common_ping_version(#{}, PingObj),
              ?assertEqual(1, Vsn)
      end},
     {"Picks the latest version both sides support",
      fun() ->
              PingObj = #{versions => [#{protocol => <<"ping">>, vsns => [1, 2]}]},
              #{use_ping_vsn := Vsn} = ?TEST_MODULE:use_latest_common_ping_version(#{}, PingObj),
              ?assertEqual(2, Vsn)
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
