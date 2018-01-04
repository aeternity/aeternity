-module(aec_tx_sign).

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

%% API
-export([sign/2,
         sign/3, 
         data/1,
         signatures/1,
         verify/2]).

%% API that should be avoided to be used
-export([verify/1,
         serialize/1,
         serialize_to_binary/1,
         deserialize/1,
         deserialize_from_binary/1]).

-export_type([signed_tx/0,
              binary_signed_tx/0]).

-include("common.hrl").

-record(signed_tx, {
          data                       :: term(),
          signatures = ordsets:new() :: ordsets:ordset(binary())}).

-opaque signed_tx() :: #signed_tx{}.
-type binary_signed_tx() :: binary().

%% @doc Given a transaction Tx, a private key or list of keys, 
%% return the cryptographically signed transaction using the default crypto
%% parameters.
-spec sign(term(), list(binary()) | binary()) -> signed_tx().
sign(Tx, PrivKeys) ->
  sign(Tx, PrivKeys, #{}).

%% @doc Given a transaction Tx, a private key and a crypto map, 
%% return the cryptographically signed transaction.
%% @equiv sign(Tx, [PrivKey], CryptoMap).
sign(Tx, PrivKey, CryptoMap) when is_binary(PrivKey) ->
  sign(Tx, [PrivKey], CryptoMap);
sign(Tx, PrivKeys, CryptoMap) when is_list(PrivKeys) ->
    Bin = aec_tx:serialize_to_binary(Tx),
    Algo = maps:get(algo, CryptoMap, ecdsa),
    Digest = maps:get(digest, CryptoMap, sha256),
    Curve = maps:get(curve, CryptoMap, secp256k1),
    Signatures = 
       [ crypto:sign(Algo, Digest, Bin, [PrivKey, crypto:ec_curve(Curve)]) ||
         PrivKey <- PrivKeys ],
    #signed_tx{data = Tx,
               signatures = Signatures}.

%% @doc Get the original transaction from a signed transaction.
%% Note that no implicit verification is performed, it just returns the data.

-spec data(signed_tx()) -> any().
%% We have no type yest for any transaction, and coinbase_tx() | spend_tx()
%% seems restricted as type.
data(#signed_tx{data = Data}) ->
    Data.

%% @doc Get the signatures of a signed transaction.
-spec signatures(signed_tx()) -> list(binary()).
signatures(#signed_tx{signatures = Sigs}) ->
    Sigs.

%% @doc Verify a signed transaction by checking that the provided keys indeed all
%% have signed this transaction.
-spec verify(signed_tx(), list(binary())) -> ok | {error, signature_check_failed}.
verify(#signed_tx{data = Tx, signatures = Sigs}, Signers) ->
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
verify(#signed_tx{data = Data, signatures = Sigs}) ->
    case aec_keys:verify(Sigs, Data) of
        true -> ok;
        false ->
            lager:debug("No matching sigs (~p - ~p)", [Sigs]),
            {error, signature_check_failed}
    end.

-define(SIG_TX_TYPE, <<"sig_tx">>).
-define(SIG_TX_VSN, 1).

version() -> ?SIG_TX_VSN.
type()    -> ?SIG_TX_TYPE.

-spec serialize(signed_tx()) -> list().
serialize(#signed_tx{data = Tx, signatures = Sigs})  when is_tuple(Tx) ->
    TxSer = aec_tx:serialize(Tx),
    [type(), version(), TxSer, Sigs].

-spec deserialize(list()) -> signed_tx().
deserialize([?SIG_TX_TYPE, ?SIG_TX_VSN, TxSer, Sigs]) ->
    Tx = aec_tx:deserialize(TxSer),
    #signed_tx{data = Tx, signatures = Sigs}.

%% deterministic canonical serialization.
-spec serialize_to_binary(signed_tx()) -> binary_signed_tx().
serialize_to_binary(#signed_tx{} = SignedTx) ->
    msgpack:pack(serialize(SignedTx)).

-spec deserialize_from_binary(binary()) -> signed_tx().
deserialize_from_binary(SignedTxBin) when is_binary(SignedTxBin) ->
    {ok, Unpacked} = msgpack:unpack(SignedTxBin),
    lager:debug("unpacked Tx: ~p", [Unpacked]),
    deserialize(Unpacked).
