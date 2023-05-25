%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity
%%% @doc
%%% Manage hyperchain parent chain block representation
%%% @end
%%%-------------------------------------------------------------------

-module(aec_parent_chain_block).
-define(NOT_SET, not_set).
-define(HC_COMMITMENT_BASIC, 1).
-define(HC_COMMITMENT_FULL, 2).
-define(HC_COMMITMENT_BTC, 3).

-opaque hash() :: binary().

%% top's state
-record(block,
    {
        hash = <<>> :: binary(),
        height = 0 :: non_neg_integer(),
        prev_hash = <<>> :: binary(),
        commitments = ?NOT_SET :: ?NOT_SET | list()
    }).

-opaque block() :: #block{}.

-export([new/3,
         hash/1,
         height/1,
         prev_hash/1,
         commitments/1,
         set_commitments/2,
         encode_commitment_basic/3,
         encode_commitment_full/3,
         encode_commitment_btc/3,
         decode_commitment/1]).

-export_type([block/0,
              hash/0]).

%% Functionality:
%% This is a reusable structure to represent parent chain's blocks


%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API

-spec new(binary(), non_neg_integer(), binary()) -> block().
new(Hash, Height, PrevHash) ->
    #block{hash = Hash,
           height = Height,
           prev_hash = PrevHash}.

-spec hash(block()) -> binary().
hash(#block{hash = Hash}) -> Hash.

-spec height(block()) -> non_neg_integer().
height(#block{height = Height}) -> Height.

-spec prev_hash(block()) -> binary().
prev_hash(#block{prev_hash = PrevHash}) -> PrevHash.

-spec set_commitments(block(), list()) -> block().
set_commitments(Block, Commitments) ->
    Block#block{commitments = Commitments}.

-spec commitments(block()) -> {ok, list()} | error.
commitments(#block{commitments = ?NOT_SET}) -> error;
commitments(#block{commitments = Commitments}) -> {ok, Commitments}.

%% Parent chain commitments to bitcoin related chains are restricted to 80 bytes.
%% We need to record the parent chain staking account of the staker and the Parent chain top hash as seen by this node.
%%  - Anonymously
%%  - So we can verify the staking account was the originator of this commitment
%%  - Be sure that it is for the right hyperchain
%% So, we store the signature of the (StakerPubKey, TopHash, NetworkId), signed by the Staker
%% together with enough of the hash of StakerPubKey and TopHash to limit the search for these entries
%% in the smart contract election

-spec encode_commitment_basic(binary(), binary(), binary()) -> binary().
encode_commitment_basic(StakerPubKey, TopHash, _NetworkId) ->
    <<?HC_COMMITMENT_BASIC, StakerPubKey/binary, TopHash/binary>>.

encode_commitment_full(StakerPubKey, TopHash, NetworkId) ->
    Msg = aec_hash:sha256_hash(<<TopHash/binary, NetworkId/binary>>),
    {ok, <<Signature:64/binary>>} = aec_preset_keys:sign_binary(Msg, StakerPubKey),
    <<?HC_COMMITMENT_BTC, Signature/binary, StakerPubKey/binary, TopHash/binary>>.

encode_commitment_btc(StakerPubKey, TopHash, NetworkId) ->
    %% Work around the lack of Address.to_bytes on Sophia pre ceres by storing the hash of the
    %% fate encoded address on the parent chain
    StakerPubKeyFate = aeb_fate_encoding:serialize(aeb_fate_data:make_address(StakerPubKey)),
    <<StakerHash:8/binary, _/binary>> = aec_hash:sha256_hash(StakerPubKeyFate),
    <<TopKeyHash:7/binary, _/binary>> = aec_hash:sha256_hash(TopHash),
    BytesToPad = 15 - byte_size(NetworkId),
    NetworkIdPadded = <<NetworkId/binary, 0:BytesToPad/unit:8>>,
    Msg = aec_hash:sha256_hash(<<TopHash/binary, NetworkIdPadded/binary>>), %% FIXME: Add some Nonce?
    {ok, <<Signature:64/binary>>} = aec_preset_keys:sign_binary(Msg, StakerPubKey),
    <<?HC_COMMITMENT_BTC, Signature/binary, StakerHash/binary, TopKeyHash/binary>>.

-spec decode_commitment(binary()) -> {basic, binary() , binary()} | {full, binary() , binary(), binary()} | {btc, binary() , binary(), binary()}.
decode_commitment(<<?HC_COMMITMENT_BASIC, StakerPubKey:32/binary, TopHash:32/binary>>) ->
    {basic, StakerPubKey, TopHash};
decode_commitment(<<?HC_COMMITMENT_FULL, Signature:64/binary, StakerPubKey:32/binary, TopHash:32/binary>>) ->
    {full, Signature, StakerPubKey, TopHash};
decode_commitment(<<?HC_COMMITMENT_BTC, Signature:64/binary, StakerHash:8/binary, TopKeyHash:7/binary>>) ->
    {btc, Signature, StakerHash, TopKeyHash}.