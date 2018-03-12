%% @doc Implements a data structure for cryptographically signed transactions.
%% This is the envelope around transactions to make them cryptographically safe.
%% The transactions normally also have keys of the "signers" in the transaction,
%% which are extracted using the signers/1 function in the respective transaction
%% handler.
%%
%% The purpose of this module is to provide an API for cryptograpically signed
%% transactions and hide all implementation details. Therefore, the record
%% #signed_tx{} should be kept private and considered an abstract type.
%%
%% A transaction can be signed by one or several signers. Each transaction can
%% determine its own signers by the transaction callback 'signers'. Since we do not
%% want to depend upon transaction types in this module, the user of
%% {@module} should first obtain the signers of the transaction and then call this
%% {@link sign/2} with these signers. There is a {@link sign/3} function that can sign
%% with respect to a certain block height. This is handy whenever the governance
%% variables on what crypto to use would change.
-module(aetx_sign).

%% API
-export([sign/2,
         new/2,
         hash/1,
         add_signatures/2,
         tx/1,
         verify/2,
         signatures/1]).

%% API that should be avoided to be used
-export([serialize_for_client/3,
         serialize_for_client_pending/2,
         meta_data_from_client_serialized/2,
         serialize_to_binary/1,
         deserialize_from_binary/1]).

-export_type([signed_tx/0,
              binary_signed_tx/0]).

-include_lib("apps/aecore/include/blocks.hrl").

-record(signed_tx, {
          tx                         :: aetx:tx(),
          signatures = ordsets:new() :: ordsets:ordset(binary())}).

-opaque signed_tx() :: #signed_tx{}.
-type binary_signed_tx() :: binary().

-define(VALID_PUBK(K), byte_size(K) =:= 32).
-define(VALID_PRIVK(K), byte_size(K) =:= 64).

-spec new(aetx:tx(), [binary()]) -> signed_tx().
new(Bin, Signatures) when is_binary(Bin) ->
    true = lists:all(fun is_binary/1, Signatures),
    {Bin, lists:usort(Signatures)};
new(Tx, Signatures) ->
    _ = aetx:specialize_type(Tx),
    true = lists:all(fun is_binary/1, Signatures),
    #signed_tx{tx = Tx, signatures = lists:usort(Signatures)}.

%% @doc Given a transaction Tx, a private key or list of keys,
%% return the cryptographically signed transaction using the default crypto
%% parameters.
-spec sign(aetx:tx()|binary(), list(binary()) | binary()) -> signed_tx() | tuple().
sign(Tx, PrivKey) when is_binary(PrivKey) ->
    sign(Tx, [PrivKey]);
sign(Bin, PrivKey) when is_binary(Bin) ->
    Signatures = sign_bin(Bin, PrivKey),
    {Bin, Signatures};
sign(Tx, PrivKeys) when is_list(PrivKeys) ->
    Bin = aetx:serialize_to_binary(Tx),
    case lists:filter(fun(PrivKey) -> not (?VALID_PRIVK(PrivKey)) end, PrivKeys) of
        [_|_]=BrokenKeys -> erlang:error({invalid_priv_key, BrokenKeys});
        [] -> pass
    end,
    Signatures = sign_bin(Bin, PrivKeys),
    #signed_tx{tx = Tx, signatures = Signatures}.

sign_bin(Bin, PrivKeys) ->
    Signatures = [ enacl:sign_detached(Bin, PrivKey) || PrivKey <- PrivKeys ],
    lists:sort(Signatures).

-spec hash(signed_tx()) -> binary().
hash(#signed_tx{} = Tx) ->
    aec_hash:hash(signed_tx, serialize_to_binary(Tx)).

-spec add_signatures(signed_tx(), list(binary())) -> signed_tx().
add_signatures(#signed_tx{signatures = OldSigs} = Tx, NewSigs)
  when is_list(NewSigs) ->
    Tx#signed_tx{signatures = lists:usort(NewSigs ++ OldSigs)}.

-spec tx(signed_tx()) -> aetx:tx().
%% @doc Get the original transaction from a signed transaction.
%% Note that no verification is performed, it just returns the transaction.
%% We have no type yest for any transaction, and spend_tx()
%% seems restricted as type.
tx(#signed_tx{tx = Tx}) ->
    Tx.

%% @doc Get the signatures of a signed transaction.
-spec signatures(signed_tx()) -> list(binary()).
signatures(#signed_tx{signatures = Sigs}) ->
    Sigs.

-spec verify(signed_tx(), aec_trees:trees()) -> ok | {error, signature_check_failed}.
verify(#signed_tx{tx = Tx, signatures = Sigs}, Trees) ->
    Bin     = aetx:serialize_to_binary(Tx),
    case aetx:signers(Tx, Trees) of
        {ok, Signers} ->
            verify_signatures(Signers, Bin, Sigs);
        {error, _Reason} ->
            {error, signature_check_failed}
    end.

verify_signatures([PubKey|Left], Bin, Sigs) ->
    case verify_one_pubkey(Sigs, PubKey, Bin) of
        {ok, SigsLeft} -> verify_signatures(Left, Bin, SigsLeft);
        error -> {error, signature_check_failed}
    end;
verify_signatures([],_Bin, []) ->
    ok;
verify_signatures(PubKeys,_Bin, Sigs) ->
    lager:debug("Signature check failed: ~p ~p", [PubKeys, Sigs]),
    {error, signature_check_failed}.

verify_one_pubkey(Sigs, PubKey, Bin) ->
    verify_one_pubkey(Sigs, PubKey, Bin, []).

verify_one_pubkey([Sig|Left], PubKey, Bin, Acc) when ?VALID_PUBK(PubKey) ->
    case enacl:sign_verify_detached(Sig, Bin, PubKey) of
        {ok, _}    -> {ok, Acc ++ Left};
        {error, _} -> verify_one_pubkey(Left, PubKey, Bin, [Sig|Acc])
    end;
verify_one_pubkey([], _PubKey,_Bin,_Acc) -> % no more signatures
    error;
verify_one_pubkey(_, _PubKey,_Bin,_Acc) -> % invalid pubkey
    error.

-define(SIG_TX_TYPE, signed_tx).
-define(SIG_TX_VSN, 1).

%% deterministic canonical serialization.
-spec serialize_to_binary(signed_tx()) -> binary_signed_tx().
serialize_to_binary(#signed_tx{tx = Tx, signatures = Sigs}) ->
    %% TODO: The original binary should be kept
    %%       around since that is what was signed
    aec_object_serialization:serialize(
      ?SIG_TX_TYPE,
      ?SIG_TX_VSN,
      serialization_template(?SIG_TX_VSN),
      [ {signatures, lists:sort(Sigs)}
      , {transaction, aetx:serialize_to_binary(Tx)}
      ]).

-spec deserialize_from_binary(binary()) -> signed_tx().
deserialize_from_binary(SignedTxBin) when is_binary(SignedTxBin) ->
    [ {signatures, Sigs}
    , {transaction, TxBin}
    ] = aec_object_serialization:deserialize(
          ?SIG_TX_TYPE,
          ?SIG_TX_VSN,
          serialization_template(?SIG_TX_VSN),
          SignedTxBin),
    #signed_tx{ tx = aetx:deserialize_from_binary(TxBin)
              , signatures = Sigs
              }.

serialization_template(?SIG_TX_VSN) ->
    [ {signatures, [binary]}
    , {transaction, binary}
    ].

-spec serialize_for_client(json|message_pack, #header{}, aetx_sign:signed_tx()) ->
                              binary() | map().
serialize_for_client(Encoding, Header, #signed_tx{}=S) ->
    {ok, BlockHash} = aec_headers:hash_header(Header),
    serialize_for_client(Encoding, S, aec_headers:height(Header), BlockHash,
                         hash(S)).

-spec serialize_for_client_pending(json|message_pack, aetx_sign:signed_tx()) ->
                              binary() | map().
serialize_for_client_pending(Encoding, #signed_tx{}=S) ->
    serialize_for_client(Encoding, S, -1, <<>>, hash(S)).

serialize_for_client(message_pack, #signed_tx{}=S, BlockHeight, BlockHash0,
                     TxHash) ->
    BlockHash = case BlockHash0 of
                    <<>> -> <<"none">>;
                    _ -> aec_base58c:encode(block_hash, BlockHash0)
                end,
    MetaData = [#{<<"block_height">> => BlockHeight},
                #{<<"block_hash">> => BlockHash},
                #{<<"hash">> => aec_base58c:encode(tx_hash, TxHash)}],
    TxBin = serialize_to_binary(S),
    Payload = [?SIG_TX_TYPE,
               ?SIG_TX_VSN,
               #{<<"tx">> => aec_base58c:encode(transaction, TxBin)},
               MetaData
              ],
    aec_base58c:encode(transaction, msgpack:pack(Payload));
serialize_for_client(json, #signed_tx{tx = Tx, signatures = Sigs},
                     BlockHeight, BlockHash0, TxHash) ->
    BlockHash = case BlockHash0 of
                    <<>> -> <<"none">>;
                    _ -> aec_base58c:encode(block_hash, BlockHash0)
                end,
    #{<<"tx">> => aetx:serialize_for_client(Tx),
      <<"block_height">> => BlockHeight,
      <<"block_hash">> => BlockHash,
      <<"hash">> => aec_base58c:encode(tx_hash, TxHash),
      <<"signatures">> => lists:map(fun(Sig) -> aec_base58c:encode(signature, Sig) end, Sigs)}.

meta_data_from_client_serialized(message_pack, Bin) ->
    {transaction, MsgPackBin} = aec_base58c:decode(Bin),
    {ok, [_Type, _Version, _TxSer, GenericData]} = msgpack:unpack(MsgPackBin),
    [#{<<"block_height">> := BlockHeight},
     #{<<"block_hash">> := BlockHashEncoded},
     #{<<"hash">> := TxHashEncoded}] = GenericData,
    {block_hash, BlockHash} = aec_base58c:decode(BlockHashEncoded),
    {tx_hash, TxHash} = aec_base58c:decode(TxHashEncoded),
    #{block_height => BlockHeight,
      block_hash => BlockHash,
      hash => TxHash};
meta_data_from_client_serialized(json, Serialized) ->
    #{<<"tx">> := _EncodedTx,
      <<"block_height">> := BlockHeight,
      <<"block_hash">> := BlockHashEncoded,
      <<"hash">> := TxHashEncoded,
      <<"signatures">> := _Sigs} = Serialized,
    {block_hash, BlockHash} = aec_base58c:decode(BlockHashEncoded),
    {tx_hash, TxHash} = aec_base58c:decode(TxHashEncoded),
    #{block_height => BlockHeight,
      block_hash => BlockHash,
      hash => TxHash}.
