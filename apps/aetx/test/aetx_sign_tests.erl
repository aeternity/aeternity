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
-define(INNER_TX_PREFIX, <<"inner_tx">>).

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

%% A signature is checked against the plain payload first and against the
%% hashed one only as a fallback, and every candidate signature is tried
%% against both before moving on. These check that the payloads are still
%% derived correctly when they are built once and reused across the signatures.
verify_multiple_signers_test_() ->
    {setup,
     fun() ->
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpKeysDir) ->
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir)
     end,
     [{"Several signers signing the plain transaction all verify",
       fun() ->
               {Pubkeys, SignedTx} = multi_signed_spend_tx([plain, plain, plain]),
               ?assertEqual(ok, ?TEST_MODULE:verify_half_signed(
                                   Pubkeys, SignedTx, ?CERES_PROTOCOL_VSN))
       end},
      {"Several signers signing the transaction hash all verify",
       fun() ->
               {Pubkeys, SignedTx} = multi_signed_spend_tx([hash, hash, hash]),
               ?assertEqual(ok, ?TEST_MODULE:verify_half_signed(
                                   Pubkeys, SignedTx, ?CERES_PROTOCOL_VSN))
       end},
      {"Signers mixing plain and hash signing all verify",
       fun() ->
               {Pubkeys, SignedTx} = multi_signed_spend_tx([plain, hash, plain]),
               ?assertEqual(ok, ?TEST_MODULE:verify_half_signed(
                                   Pubkeys, SignedTx, ?CERES_PROTOCOL_VSN))
       end},
      {"Hash signing is not accepted before Lima",
       fun() ->
               {Pubkeys, SignedTx} = multi_signed_spend_tx([hash, hash]),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify_half_signed(
                               Pubkeys, SignedTx, ?LIMA_PROTOCOL_VSN - 1))
       end},
      {"A signer without a matching signature does not verify",
       fun() ->
               {Pubkeys, SignedTx} = multi_signed_spend_tx([plain, hash]),
               #{public := Stranger} = enacl:sign_keypair(),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify_half_signed(
                               [Stranger | Pubkeys], SignedTx, ?CERES_PROTOCOL_VSN))
       end},
      {"A signature not belonging to any signer does not verify",
       fun() ->
               {[_Dropped | Pubkeys], SignedTx} =
                   multi_signed_spend_tx([plain, hash, plain]),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify_half_signed(
                               Pubkeys, SignedTx, ?CERES_PROTOCOL_VSN))
       end},
      {"Neither signers nor signatures is nothing to check",
       fun() ->
               {_Pubkey, SignedTx} = unsigned_spend_tx(),
               %% The clause that answers before a payload is built at all -
               %% where a fully GA-authenticated transaction ends up.
               ?assertEqual(ok, ?TEST_MODULE:verify_half_signed(
                                   [], SignedTx, ?CERES_PROTOCOL_VSN))
       end},
      {"An invalid pubkey does not verify next to signers that do",
       fun() ->
               {Pubkeys, SignedTx} = multi_signed_spend_tx([plain, hash]),
               [begin
                    ?assertEqual({error, signature_check_failed},
                                 ?TEST_MODULE:verify_half_signed(
                                    Invalid, SignedTx, ?CERES_PROTOCOL_VSN)),
                    ?assertEqual({error, signature_check_failed},
                                 ?TEST_MODULE:verify_half_signed(
                                    Pubkeys ++ [Invalid], SignedTx,
                                    ?CERES_PROTOCOL_VSN))
                end || Invalid <- invalid_pubkeys()]
       end},
      {"The payloads are built once per transaction, not once per signature",
       fun() ->
               %% The saving itself. Every other test in this file passes just
               %% as happily with the payloads rebuilt per signature.
               {Pubkeys, SignedTx} = multi_signed_spend_tx([hash, hash, hash]),
               meck:new(aec_governance, [passthrough]),
               meck:new(aec_hash, [passthrough]),
               try
                   ?assertEqual(ok, ?TEST_MODULE:verify_half_signed(
                                       Pubkeys, SignedTx, ?CERES_PROTOCOL_VSN)),
                   ?assertEqual(1, meck:num_calls(aec_governance,
                                                  get_network_id, [])),
                   ?assertEqual(1, meck:num_calls(aec_hash,
                                                  hash, [signed_tx, '_']))
               after
                   meck:unload(aec_hash),
                   meck:unload(aec_governance)
               end
       end}
     ]}.

%% verify_one_pubkey/3 is the single signer entry point - the channel FSM asks
%% it whether one participant has signed a transaction the other one has signed
%% too, so unlike the strict path it tolerates signatures it was not asked
%% about. It builds the payloads and maps the outcome itself, so it is checked
%% here directly rather than through the multi signer path it shares them with.
verify_one_pubkey_test_() ->
    {setup,
     fun() ->
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpKeysDir) ->
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir)
     end,
     [{"A plain signature verifies with the other signatures left in place",
       fun() ->
               {[Pubkey | _], SignedTx} =
                   multi_signed_spend_tx([plain, plain, hash]),
               ?assertEqual(ok, ?TEST_MODULE:verify_one_pubkey(
                                   Pubkey, SignedTx, ?CERES_PROTOCOL_VSN)),
               %% The strict path rejects that very transaction, and does so
               %% precisely because of the signatures left unaccounted for.
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify_half_signed(
                               [Pubkey], SignedTx, ?CERES_PROTOCOL_VSN))
       end},
      {"A hashed signature verifies against the hashed payload",
       fun() ->
               {Pubkeys, SignedTx} = multi_signed_spend_tx([plain, plain, hash]),
               ?assertEqual(ok, ?TEST_MODULE:verify_one_pubkey(
                                   lists:last(Pubkeys), SignedTx,
                                   ?CERES_PROTOCOL_VSN))
       end},
      {"Hash signing is not accepted before Lima",
       fun() ->
               {[Pubkey], SignedTx} = multi_signed_spend_tx([hash]),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify_one_pubkey(
                               Pubkey, SignedTx, ?LIMA_PROTOCOL_VSN - 1))
       end},
      {"A signer without a matching signature does not verify",
       fun() ->
               {_Pubkeys, SignedTx} = multi_signed_spend_tx([plain, hash]),
               #{public := Stranger} = enacl:sign_keypair(),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify_one_pubkey(
                               Stranger, SignedTx, ?CERES_PROTOCOL_VSN))
       end},
      {"A transaction carrying no signatures does not verify",
       fun() ->
               {Pubkey, SignedTx} = unsigned_spend_tx(),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify_one_pubkey(
                               Pubkey, SignedTx, ?CERES_PROTOCOL_VSN))
       end},
      {"An invalid pubkey does not verify",
       fun() ->
               {_Pubkeys, SignedTx} = multi_signed_spend_tx([plain]),
               [?assertEqual({error, signature_check_failed},
                             ?TEST_MODULE:verify_one_pubkey(
                                Invalid, SignedTx, ?CERES_PROTOCOL_VSN))
                || Invalid <- invalid_pubkeys()]
       end}
     ]}.

%% The inner-tx prefix is prepended to *both* payloads - to the serialized
%% transaction for a plain signature, and to its hash for a hash signature -
%% before the network id goes in front. Each payload is now built in its own
%% place, so check that both still carry the prefix, and that the prefix is not
%% quietly dropped from either.
verify_prefixed_signature_test_() ->
    {setup,
     fun() ->
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpKeysDir) ->
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir)
     end,
     [{"A plain inner-tx signature verifies against the prefixed payload",
       fun() ->
               {Privkey, SpendTx} = spend_tx_with_keys(),
               SignedTx = aec_test_utils:sign_tx(SpendTx, Privkey, false,
                                                 ?INNER_TX_PREFIX),
               ?assertEqual(ok, verify_inner(SignedTx))
       end},
      {"A hashed inner-tx signature verifies against the prefixed payload",
       fun() ->
               {Privkey, SpendTx} = spend_tx_with_keys(),
               SignedTx = aec_test_utils:sign_pay_for_inner_tx(SpendTx, Privkey),
               ?assertEqual(ok, verify_inner(SignedTx))
       end},
      {"An unprefixed signature does not verify as an inner transaction",
       fun() ->
               {Privkey, SpendTx} = spend_tx_with_keys(),
               Plain = aec_test_utils:sign_tx(SpendTx, Privkey),
               Hashed = aec_test_utils:sign_tx_hash(SpendTx, Privkey),
               ?assertEqual({error, signature_check_failed}, verify_inner(Plain)),
               ?assertEqual({error, signature_check_failed}, verify_inner(Hashed))
       end},
      {"A prefixed signature does not verify as a top level transaction",
       fun() ->
               {Privkey, SpendTx} = spend_tx_with_keys(),
               SignedTx = aec_test_utils:sign_tx(SpendTx, Privkey, false,
                                                 ?INNER_TX_PREFIX),
               ?assertEqual({error, signature_check_failed},
                            ?TEST_MODULE:verify(SignedTx, aec_trees:new(),
                                                ?CERES_PROTOCOL_VSN))
       end}
     ]}.

%% The prefix aetx_sign is handed is the one aec_test_utils signs with, with the
%% separator already in front - see verify_w_env/3 and sign_tx/5.
verify_inner(SignedTx) ->
    ?TEST_MODULE:verify(SignedTx, aec_trees:new(), ?CERES_PROTOCOL_VSN,
                        <<"-", ?INNER_TX_PREFIX/binary>>).

spend_tx_with_keys() ->
    #{public := Pubkey, secret := Privkey} = enacl:sign_keypair(),
    {ok, SpendTx} = make_spend_tx(Pubkey),
    {Privkey, SpendTx}.

%% Builds a spend tx signed by one key per given signing mode.
multi_signed_spend_tx(Modes) ->
    Keypairs = [enacl:sign_keypair() || _ <- Modes],
    #{public := Sender} = hd(Keypairs),
    {ok, SpendTx} = make_spend_tx(Sender),
    Signatures =
        [begin
             Signed = case Mode of
                          plain -> aec_test_utils:sign_tx(SpendTx, Privkey);
                          hash  -> aec_test_utils:sign_tx_hash(SpendTx, Privkey)
                      end,
             [Signature] = ?TEST_MODULE:signatures(Signed),
             Signature
         end || {#{secret := Privkey}, Mode} <- lists:zip(Keypairs, Modes)],
    {[Pubkey || #{public := Pubkey} <- Keypairs],
     ?TEST_MODULE:new(SpendTx, Signatures)}.

unsigned_spend_tx() ->
    #{public := Pubkey} = enacl:sign_keypair(),
    {ok, SpendTx} = make_spend_tx(Pubkey),
    {Pubkey, ?TEST_MODULE:new(SpendTx, [])}.

%% enacl raises on a key that is not 32 bytes long, so the guards on the entry
%% points are the only thing standing between malformed input and a crashing
%% caller. Cut to size from a real key rather than written down as literals, so
%% that the sizes stay out of the type checker's reach at the call sites.
invalid_pubkeys() ->
    #{public := Pubkey} = enacl:sign_keypair(),
    Oversized = <<Pubkey/binary, Pubkey/binary>>,
    [binary:part(Oversized, 0, Size) || Size <- [0, 31, 33]].

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
