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
%% with respect to a certain protocol version. This is handy whenever the governance
%% variables on what crypto to use would change.
-module(aetx_sign).

%% API
-export([new/2,
         hash/1,
         add_signatures/2,
         tx/1,
         innermost_tx/1,
         verify/3,
         verify_w_env/3,
         verify_half_signed/3,
         verify_one_pubkey/3,
         from_db_format/1,
         signatures/1]).

%% API that should be avoided to be used
-export([serialize_for_client/2,
         serialize_for_client_pending/1,
         serialize_for_client_inner/2,
         meta_data_from_client_serialized/1,
         serialize_to_binary/1,
         deserialize_from_binary/1]).

-export([ record_fields/1 ]).

-ifdef(TEST).
-export([set_tx/2]).
-endif.

-export_type([signed_tx/0,
              binary_signed_tx/0]).

-include("../../aecore/include/blocks.hrl").
-include("../../aecontract/include/hard_forks.hrl").

-record(signed_tx,
        { tx                         :: aetx:tx(),
          signatures = ordsets:new() :: ordsets:ordset(binary()) }).

-opaque signed_tx() :: #signed_tx{}.
-type binary_signed_tx() :: binary().

-define(VALID_PUBK(K), byte_size(K) =:= 32).

%% ==================================================================
%% Tracing support

record_fields(signed_tx) -> record_info(fields, signed_tx);
record_fields(_) ->
    {check_mods, [ aetx ]}.

%% ==================================================================

-spec new(aetx:tx(), [binary()]) -> signed_tx().
new(Bin, Signatures) when is_binary(Bin) ->
    true = lists:all(fun is_binary/1, Signatures),
    {Bin, lists:usort(Signatures)};
new(Tx, Signatures) ->
    _ = aetx:specialize_type(Tx),
    true = lists:all(fun is_binary/1, Signatures),
    assert_sigs_size(Signatures),
    #signed_tx{tx = Tx, signatures = lists:usort(Signatures)}.

-spec hash(signed_tx()) -> binary().
hash(#signed_tx{} = Tx) ->
    aec_hash:hash(signed_tx, serialize_to_binary(Tx)).

-spec add_signatures(signed_tx(), list(binary())) -> signed_tx().
add_signatures(#signed_tx{signatures = OldSigs} = Tx, NewSigs)
  when is_list(NewSigs) ->
    assert_sigs_size(NewSigs),
    Tx#signed_tx{signatures = lists:usort(NewSigs ++ OldSigs)}.

-spec tx(signed_tx()) -> aetx:tx().
%% @doc Get the original transaction from a signed transaction.
%% Note that no verification is performed, it just returns the transaction.
%% We have no type yest for any transaction, and spend_tx()
%% seems restricted as type.
tx(#signed_tx{tx = Tx}) ->
    Tx.

-spec innermost_tx(signed_tx()) -> aetx:tx().
innermost_tx(SignedTx) ->
    Aetx = aetx_sign:tx(SignedTx),
    case aetx:specialize_callback(Aetx) of
        {aega_meta_tx, MetaTx} -> innermost_tx(aega_meta_tx:tx(MetaTx));
        {_CallBack, _Tx} -> Aetx
    end.

-spec from_db_format(tuple()) -> signed_tx().
from_db_format(#signed_tx{tx = Tx} = STx) ->
    case aetx:from_db_format(Tx) of
        Tx  -> STx;
        Tx1 -> STx#signed_tx{tx = Tx1}
    end.

%% @doc Get the signatures of a signed transaction.
-spec signatures(signed_tx()) -> list(binary()).
signatures(#signed_tx{signatures = Sigs}) ->
    Sigs.

-spec verify_w_env(signed_tx(), aec_trees:trees(), aetx_env:env()) ->
    ok | {error, signature_verification_failed}.
verify_w_env(#signed_tx{tx = Tx, signatures = Sigs}, Trees, TxEnv) ->
    Bin      = aetx:serialize_to_binary(Tx),
    Protocol = aetx_env:consensus_version(TxEnv),
    case aetx:signers(Tx, Trees) of
        {ok, Signers} ->
            RemainingSigners = Signers -- aetx_env:ga_auth_ids(TxEnv),
            verify_signatures(RemainingSigners, Bin, Sigs, Protocol);
        {error, _Reason} ->
            {error, signature_verification_failed}
    end.

%% this function is strict and does not allow having more signatures that the
%% one being checked
-spec verify(signed_tx(), aec_trees:trees(), aec_hard_forks:protocol_vsn()) ->
    ok | {error, signature_check_failed}.
verify(#signed_tx{tx = Tx, signatures = Sigs}, Trees, Protocol) ->
    Bin = aetx:serialize_to_binary(Tx),
    case aetx:signers(Tx, Trees) of
        {ok, Signers} ->
            verify_signatures(Signers, Bin, Sigs, Protocol);
        {error, _Reason} ->
            {error, signature_check_failed}
    end.


%% this function allows having more signatures that the one being checked
-spec verify_one_pubkey(aec_keys:pubkey(), signed_tx(), aec_hard_forks:protocol_vsn()) ->
    ok | {error, signature_check_failed}.
verify_one_pubkey(Signer, #signed_tx{tx = Tx, signatures = Sigs}, Protocol) ->
    Bin = aetx:serialize_to_binary(Tx),
    case verify_one_pubkey(Sigs, Signer, Bin, Protocol) of
        {ok, _} -> ok;
        error -> {error, signature_check_failed}
    end.

-spec verify_half_signed(aec_keys:pubkey() | [aec_keys:pubkey()],
                         signed_tx(), aec_hard_forks:protocol_vsn()) ->
    ok | {error, signature_check_failed}.
verify_half_signed(Signer, SignedTx, Protocol) when is_binary(Signer) ->
    verify_half_signed([Signer], SignedTx, Protocol);
verify_half_signed(Signers, #signed_tx{tx = Tx, signatures = Sigs}, Protocol) ->
    verify_signatures(Signers, aetx:serialize_to_binary(Tx), Sigs, Protocol).

verify_signatures([], _Bin, [], _Protocol) ->
    ok;
verify_signatures([PubKey|Left], Bin, Sigs, Protocol) ->
    case verify_one_pubkey(Sigs, PubKey, Bin, Protocol) of
        {ok, SigsLeft} -> verify_signatures(Left, Bin, SigsLeft, Protocol);
        error          -> {error, signature_check_failed}
    end;
verify_signatures(PubKeys,_Bin, Sigs, _Protocol) ->
    lager:debug("Signature check failed: ~p ~p", [PubKeys, Sigs]),
    {error, signature_check_failed}.

verify_one_pubkey(Sigs, PubKey, Bin, Protocol) when ?VALID_PUBK(PubKey) ->
    HashSign = Protocol >= ?LIMA_PROTOCOL_VSN,
    verify_one_pubkey(Sigs, PubKey, Bin, HashSign, []);
verify_one_pubkey(_Sigs, _PubKey, _Bin, _Protocol) ->
    error. %% invalid pubkey

verify_one_pubkey([Sig|Left], PubKey, Bin, HashSign, Acc)  ->
    BinForNetwork = aec_governance:add_network_id(Bin),
    case enacl:sign_verify_detached(Sig, BinForNetwork, PubKey) of
        {ok, _} ->
            {ok, Acc ++ Left};
        {error, _} when HashSign ->
            TxHash = aec_hash:hash(signed_tx, Bin),
            BinForNetwork2 = aec_governance:add_network_id(TxHash),
            case enacl:sign_verify_detached(Sig, BinForNetwork2, PubKey) of
                {ok, _}    -> {ok, Acc ++ Left};
                {error, _} -> verify_one_pubkey(Left, PubKey, Bin, HashSign, [Sig | Acc])
            end;
        {error, _} ->
            verify_one_pubkey(Left, PubKey, Bin, HashSign, [Sig|Acc])
    end;
verify_one_pubkey([], _PubKey, _Bin, _HashSign, _Acc) -> % no more signatures
    error.

-define(SIG_TX_TYPE, signed_tx).
-define(SIG_TX_VSN, 1).

%% deterministic canonical serialization.
-spec serialize_to_binary(signed_tx()) -> binary_signed_tx().
serialize_to_binary(#signed_tx{tx = Tx, signatures = Sigs}) ->
    %% TODO: The original binary should be kept
    %%       around since that is what was signed
    aeser_chain_objects:serialize(
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
    ] = aeser_chain_objects:deserialize(
          ?SIG_TX_TYPE,
          ?SIG_TX_VSN,
          serialization_template(?SIG_TX_VSN),
          SignedTxBin),
    assert_sigs_size(Sigs),
    #signed_tx{ tx = aetx:deserialize_from_binary(TxBin)
              , signatures = Sigs
              }.

serialization_template(?SIG_TX_VSN) ->
    [ {signatures, [binary]}
    , {transaction, binary}
    ].

-spec serialize_for_client(aec_headers:header(), aetx_sign:signed_tx()) -> binary() | map().
serialize_for_client(Header, #signed_tx{}=S) ->
    {ok, BlockHash} = aec_headers:hash_header(Header),
    serialize_for_client(S, aec_headers:height(Header), BlockHash, hash(S)).

-spec serialize_for_client_pending(aetx_sign:signed_tx()) -> binary() | map().
serialize_for_client_pending(#signed_tx{}=S) ->
    serialize_for_client(S, -1, <<>>, hash(S)).

-spec serialize_for_client(aetx_sign:signed_tx(), integer(), binary(), binary()) ->
    binary() | map().
serialize_for_client(#signed_tx{} = SigTx, BlockHeight, BlockHash0, TxHash) ->
    BlockHash =
        case BlockHash0 of
            <<>> -> <<"none">>;
            _ -> aeser_api_encoder:encode(micro_block_hash, BlockHash0)
        end,
    MetaData =
        #{
          <<"block_height">> => BlockHeight,
          <<"block_hash">>   => BlockHash,
          <<"hash">>         => aeser_api_encoder:encode(tx_hash, TxHash)},
    serialize_for_client_inner(SigTx, MetaData).

%% For inner transactions we leave out block height etc and for generalized
%% accounts even signatures make no sense.
-spec serialize_for_client_inner(aetx_sign:signed_tx(), map()) -> binary() | map().
serialize_for_client_inner(#signed_tx{tx = Tx, signatures = Sigs}, MetaData) ->
    case aetx:specialize_type(Tx) of
        {ga_meta_tx, _} ->
            MetaData#{<<"tx">> => aetx:serialize_for_client(Tx)};
        _ ->
            MetaData#{<<"tx">> => aetx:serialize_for_client(Tx),
                      <<"signatures">> => [aeser_api_encoder:encode(signature, S) || S <- Sigs]}
    end.

meta_data_from_client_serialized(Serialized) ->
    #{<<"tx">>           := _EncodedTx,
      <<"block_height">> := BlockHeight,
      <<"block_hash">>   := BlockHashEncoded,
      <<"hash">>         := TxHashEncoded,
      <<"signatures">>   := _Sigs} = Serialized,
    {block_hash, BlockHash} = aeser_api_encoder:decode(BlockHashEncoded),
    {tx_hash, TxHash}       = aeser_api_encoder:decode(TxHashEncoded),
    #{block_height => BlockHeight,
      block_hash   => BlockHash,
      hash         => TxHash}.

assert_sigs_size(Sigs) ->
    AllowedByteSize = aeser_api_encoder:byte_size_for_type(signature),
    lists:foreach(
        fun(Sig) -> {AllowedByteSize, _} = {byte_size(Sig), Sig} end,
        Sigs).

-ifdef(TEST).
set_tx(SignedTx, Aetx) ->
    SignedTx#signed_tx{tx = Aetx}.
-endif.
