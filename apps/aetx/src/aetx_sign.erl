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
         sign/3,
         tx/1,
         signatures/1,
         verify/2,
         is_coinbase/1,
         filter_invalid_signatures/1]).

%% API that should be avoided to be used
-export([verify/1,
         serialize/1,
         serialize_for_client/3,
         serialize_for_client_pending/2,
         meta_data_from_client_serialized/2,
         serialize_to_binary/1,
         deserialize/1,
         deserialize_from_binary/1]).

-export_type([signed_tx/0,
              binary_signed_tx/0]).

-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aecore/include/blocks.hrl").

-record(signed_tx, {
          tx                         :: aetx:tx(),
          signatures = ordsets:new() :: ordsets:ordset(binary())}).

-opaque signed_tx() :: #signed_tx{}.
-type tx() :: aetx:tx().
-type binary_signed_tx() :: binary().

%% @doc Given a transaction Tx, a private key or list of keys,
%% return the cryptographically signed transaction using the default crypto
%% parameters.
-spec sign(tx(), list(binary()) | binary()) -> signed_tx().
sign(Tx, PrivKeys) ->
  sign(Tx, PrivKeys, #{}).

-spec sign(tx(), list(binary()) | binary(), map()) -> signed_tx().
%% @doc Given a transaction Tx, a private key and a crypto map,
%% return the cryptographically signed transaction.
%% A list of signers may be provided instead of one signer key.
sign(Tx, PrivKey, CryptoMap) when is_binary(PrivKey) ->
  sign(Tx, [PrivKey], CryptoMap);
sign(Tx, PrivKeys, CryptoMap) when is_list(PrivKeys) ->
    Bin = aetx:serialize_to_binary(Tx),
    Algo = maps:get(algo, CryptoMap, ecdsa),
    Digest = maps:get(digest, CryptoMap, sha256),
    Curve = maps:get(curve, CryptoMap, secp256k1),
    Signatures =
       [ crypto:sign(Algo, Digest, Bin, [PrivKey, crypto:ec_curve(Curve)]) ||
         PrivKey <- PrivKeys ],
    #signed_tx{tx = Tx,
               signatures = Signatures}.


-spec tx(signed_tx()) -> tx().
%% @doc Get the original transaction from a signed transaction.
%% Note that no verification is performed, it just returns the transaction.
%% We have no type yest for any transaction, and coinbase_tx() | spend_tx()
%% seems restricted as type.
tx(#signed_tx{tx = Tx}) ->
    Tx.

%% @doc Get the signatures of a signed transaction.
-spec signatures(signed_tx()) -> list(binary()).
signatures(#signed_tx{signatures = Sigs}) ->
    Sigs.

%% @doc Verify a signed transaction by checking that the provided keys indeed all
%% have signed this transaction.
-spec verify(signed_tx(), list(binary())) -> ok | {error, signature_check_failed}.
verify(#signed_tx{tx = Tx, signatures = Sigs}, Signers) ->
    %% This works even for Signers being one public key!
    #signed_tx{signatures = NewSigs} = sign(Tx, Signers),
    case {NewSigs -- Sigs, Sigs -- NewSigs} of
        {[], []} ->
          ok;
        {DSigs1, []} ->
          lager:debug("No matching sigs (~p - ~p) additional new signatures", [DSigs1, Sigs]),
          {error, signature_check_failed};
        {_, DSigs2} ->
          lager:debug("No matching sigs (~p - ~p) missing signatures", [DSigs2, Sigs]),
          {error, signature_check_failed}
    end.


%% This should not call aec_keys verify, but aec_keys should call this module!
%% with the keys of the signers.
-spec verify(signed_tx()) -> ok | {error, signature_check_failed}.
verify(#signed_tx{tx = Tx, signatures = Sigs}) ->
    case aec_keys:verify(Sigs, Tx) of
        true -> ok;
        false ->
            lager:debug("No matching sigs (~p)", [Sigs]),
            {error, signature_check_failed}
    end.

-spec filter_invalid_signatures(list(signed_tx())) -> list(signed_tx()).
filter_invalid_signatures(SignedTxs) ->
    lists:filter(fun(SignedTx) -> ok == verify(SignedTx) end, SignedTxs).

-define(SIG_TX_TYPE, <<"sig_tx">>).
-define(SIG_TX_VSN, 1).

version() -> ?SIG_TX_VSN.
type()    -> ?SIG_TX_TYPE.

-spec serialize(signed_tx()) -> list().
serialize(#signed_tx{tx = Tx, signatures = Sigs}) ->
    TxSer = aetx:serialize(Tx),
    [type(), version(), TxSer, Sigs].

-spec deserialize(list()) -> signed_tx().
deserialize([?SIG_TX_TYPE, ?SIG_TX_VSN, TxSer, Sigs]) ->
    Tx = aetx:deserialize(TxSer),
    #signed_tx{tx = Tx, signatures = Sigs}.

%% deterministic canonical serialization.
-spec serialize_to_binary(signed_tx()) -> binary_signed_tx().
serialize_to_binary(#signed_tx{} = SignedTx) ->
    msgpack:pack(serialize(SignedTx)).

-spec deserialize_from_binary(binary()) -> signed_tx().
deserialize_from_binary(SignedTxBin) when is_binary(SignedTxBin) ->
    {ok, Unpacked} = msgpack:unpack(SignedTxBin),
    lager:debug("unpacked Tx: ~p", [Unpacked]),
    deserialize(Unpacked).

-spec serialize_for_client(json|message_pack, #header{}, aetx_sign:signed_tx()) ->
                              binary() | map().
serialize_for_client(Encoding, Header, #signed_tx{tx = Tx}=S) ->
    {ok, BlockHash} = aec_headers:hash_header(Header),
    TxHash = aetx:hash(Tx),
    serialize_for_client(Encoding, S, aec_headers:height(Header), BlockHash,
                         TxHash).

-spec serialize_for_client_pending(json|message_pack, aetx_sign:signed_tx()) ->
                              binary() | map().
serialize_for_client_pending(Encoding, #signed_tx{tx = Tx}=S) ->
    TxHash = aetx:hash(Tx),
    serialize_for_client(Encoding, S, -1, <<>>, TxHash).

serialize_for_client(message_pack, #signed_tx{}=S, BlockHeight, BlockHash0,
                     TxHash) ->
    BlockHash = case BlockHash0 of
                    <<>> -> <<"none">>;
                    _ -> aec_base58c:encode(block_hash, BlockHash0)
                end,
    MetaData = [#{<<"block_height">> => BlockHeight},
                #{<<"block_hash">> => BlockHash},
                #{<<"hash">> => aec_base58c:encode(tx_hash, TxHash)}
               ],
    aec_base58c:encode(transaction, msgpack:pack(serialize(S) ++ [MetaData]));
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
    {ok, [_Type, _Version, _TxSer, _Sigs, GenericData]} = msgpack:unpack(MsgPackBin),
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

-spec is_coinbase(Tx :: signed_tx()) -> boolean().
is_coinbase(#signed_tx{tx = Tx}) ->
    aetx:is_coinbase(Tx).

