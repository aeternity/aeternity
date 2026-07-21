%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aetx_sign_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("aecore/include/blocks.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(TEST_MODULE, aetx_sign).
-define(TEST_HEIGHT, 42).
-define(BLOCK_HEIGHT, 137).
-define(FAKE_TX_HASH, <<7:32/unit:8>>).

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
                                    ?TEST_MODULE:verify(SignedTx, aec_trees:new(), Vsn));
                   Vsn ->
                       ?assertEqual(ok,
                                    ?TEST_MODULE:verify(SignedTx, aec_trees:new(), Vsn))
               end
       end}
     ]}.

serialize_for_client_test_() ->
    {setup,
     fun() ->
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpKeysDir) ->
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir)
     end,
     [{"Pending transaction is hashed as it is handed to the client",
       fun() ->
               SignedTx = signed_spend_tx(),
               Serialized = ?TEST_MODULE:serialize_for_client_pending(SignedTx),
               assert_hash_of_encoded_tx(SignedTx, Serialized),
               assert_signatures(SignedTx, Serialized),
               ?assertMatch(#{<<"type">> := <<"SpendTx">>},
                            maps:get(<<"tx">>, Serialized)),
               ?assertEqual(-1, maps:get(<<"block_height">>, Serialized)),
               ?assertEqual(<<"none">>, maps:get(<<"block_hash">>, Serialized)),
               ok
       end},
      {"Transaction in a micro block is hashed as it is handed to the client",
       fun() ->
               SignedTx = signed_spend_tx(),
               Header = micro_header(?BLOCK_HEIGHT),
               {ok, BlockHash} = aec_headers:hash_header(Header),
               Serialized = ?TEST_MODULE:serialize_for_client(Header, SignedTx),
               assert_hash_of_encoded_tx(SignedTx, Serialized),
               assert_signatures(SignedTx, Serialized),
               ?assertEqual(?BLOCK_HEIGHT, maps:get(<<"block_height">>, Serialized)),
               ?assertEqual(aeser_api_encoder:encode(micro_block_hash, BlockHash),
                            maps:get(<<"block_hash">>, Serialized)),
               ok
       end},
      {"All signatures of a transaction are handed to the client",
       fun() ->
               SignedTx = make_signed_mutual_close(),
               ?assertMatch([_, _], ?TEST_MODULE:signatures(SignedTx)),
               Serialized = ?TEST_MODULE:serialize_for_client_pending(SignedTx),
               assert_hash_of_encoded_tx(SignedTx, Serialized),
               assert_signatures(SignedTx, Serialized),
               ok
       end},
      {"Generalized account meta transaction is hashed as it is handed to the client",
       fun() ->
               SignedTx = signed_meta_tx(),
               Serialized = ?TEST_MODULE:serialize_for_client_pending(SignedTx),
               assert_hash_of_encoded_tx(SignedTx, Serialized),
               %% Signatures make no sense for a generalized account
               ?assertNot(maps:is_key(<<"signatures">>, Serialized)),
               %% The transaction it authenticates does carry its signatures
               #{<<"tx">> := #{<<"tx">> := InnerTx}} = Serialized,
               ?assertMatch(#{<<"signatures">> := [<<"sg_", _/binary>>],
                              <<"tx">> := #{<<"type">> := <<"SpendTx">>}}, InnerTx),
               ok
       end},
      {"Block data and transaction hash are presented as given",
       fun() ->
               SignedTx = signed_spend_tx(),
               Header = micro_header(?BLOCK_HEIGHT),
               {ok, BlockHash} = aec_headers:hash_header(Header),
               ?assertEqual(?TEST_MODULE:serialize_for_client(Header, SignedTx),
                            ?TEST_MODULE:serialize_for_client(SignedTx, ?BLOCK_HEIGHT,
                                                              BlockHash,
                                                              ?TEST_MODULE:hash(SignedTx))),
               %% The caller decides what the transaction is known by, it is
               %% not recomputed from the transaction itself.
               Serialized = ?TEST_MODULE:serialize_for_client(SignedTx, ?BLOCK_HEIGHT + 1,
                                                              BlockHash, ?FAKE_TX_HASH),
               ?assertEqual(aeser_api_encoder:encode(tx_hash, ?FAKE_TX_HASH),
                            maps:get(<<"hash">>, Serialized)),
               ?assertEqual(?BLOCK_HEIGHT + 1, maps:get(<<"block_height">>, Serialized)),
               ok
       end}
     ]}.

%% The client is given the transaction hash and the transaction itself. Both
%% are derived from one single serialization of the signed tx, so assert that
%% the hash is indeed the hash of the very bytes the client is handed.
assert_hash_of_encoded_tx(SignedTx, Serialized) ->
    {ok, TxBin} = aeser_api_encoder:safe_decode(
                    transaction, maps:get(<<"encoded_tx">>, Serialized)),
    ?assertEqual(?TEST_MODULE:serialize_to_binary(SignedTx), TxBin),
    ?assertEqual(SignedTx, ?TEST_MODULE:deserialize_from_binary(TxBin)),
    ?assertEqual(aeser_api_encoder:encode(tx_hash, aec_hash:hash(signed_tx, TxBin)),
                 maps:get(<<"hash">>, Serialized)),
    ?assertEqual(aeser_api_encoder:encode(tx_hash, ?TEST_MODULE:hash(SignedTx)),
                 maps:get(<<"hash">>, Serialized)),
    ?assertEqual(aetx:serialize_for_client(?TEST_MODULE:tx(SignedTx)),
                 maps:get(<<"tx">>, Serialized)).

%% Signatures are presented in the same canonical order they are serialized in.
assert_signatures(SignedTx, Serialized) ->
    ?assertEqual([aeser_api_encoder:encode(signature, S)
                  || S <- ?TEST_MODULE:signatures(SignedTx)],
                 maps:get(<<"signatures">>, Serialized)).

signed_spend_tx() ->
    #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
    {ok, SpendTx} = make_spend_tx(Pubkey),
    aec_test_utils:sign_tx(SpendTx, Privkey).

signed_meta_tx() ->
    #{ public := Pubkey } = enacl:sign_keypair(),
    {ok, MetaTx} = aega_meta_tx:new(
                     #{ga_id       => aeser_id:create(account, Pubkey),
                       auth_data   => <<>>,
                       abi_version => 1,
                       gas         => 1,
                       gas_price   => 1,
                       fee         => 1,
                       tx          => signed_spend_tx()}),
    ?TEST_MODULE:new(MetaTx, []).

micro_header(Height) ->
    aec_headers:new_micro_header(
      Height,
      <<1:?BLOCK_HEADER_HASH_BYTES/unit:8>>, %% prev hash
      <<2:?BLOCK_HEADER_HASH_BYTES/unit:8>>, %% prev key hash
      <<3:?STATE_HASH_BYTES/unit:8>>,        %% root hash
      0,                                     %% time
      <<4:?TXS_HASH_BYTES/unit:8>>,          %% txs hash
      <<>>,                                  %% no fraud
      ?CERES_PROTOCOL_VSN).

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
