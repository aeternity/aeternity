%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Abstract parent block header representation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_block).

-export([new_header/3
        , new_header/4
        , header_from_db/1
        , commitment_hashes/1
        , new_block/2
        , prev_hash_block/1
        , commitments_in_block/1
        , hash_block/1
        , height_block/1
        , block_header/1
        , is_hc_parent_block/1]).
-export_type([parent_block_header/0, parent_block/0]).

-include_lib("aehyperchains/include/aehc_types.hrl").

%% TODO: Split up the header and the block to separate modules
-record(hc_parent_block_header, {
    hash = <<>> :: binary(),
    prev_hash = <<>> :: binary(),
    height = 0 :: non_neg_integer(),

    %% For the case of BitcoinNG parent chains those commitments were actually included in the previous generation
    %% In other cases those are commitments which were present in the block
    commitment_hashes = [] :: [commitment_hash()]
}).

-record(hc_parent_block, {
    header :: parent_block_header(),
    commitments :: [aehc_commitment:commitment()]
}).

-type parent_block_header() :: #hc_parent_block_header{}.
-type parent_block() :: #hc_parent_block{}.

-spec new_header(hash(), hash(), non_neg_integer()) -> parent_block_header().
new_header(Hash, PrevHash, Height) ->
    new_header(Hash, PrevHash, Height, []).

-spec new_header(hash(), hash(), non_neg_integer(), [commitment_hash()]) ->
    parent_block_header().
new_header(Hash, PrevHash, Height, CommitmentHashes) ->
    #hc_parent_block_header{
        hash = Hash
        , prev_hash = PrevHash
        , height = Height
        , commitment_hashes = CommitmentHashes
    }.

-spec new_block(parent_block_header(), [aehc_commitment:commitment()]) ->
    parent_block().
new_block(Header, Commitments) ->
    #hc_parent_block{header = Header, commitments = Commitments}.

-spec header_from_db(parent_block_header()) -> parent_block_header().
header_from_db(#hc_parent_block_header{} = Header) ->
    Header.

-spec commitment_hashes(parent_block_header()) -> [commitment_hash()].
commitment_hashes(#hc_parent_block_header{commitment_hashes = Hashes}) ->
    Hashes.

-spec prev_hash_block(parent_block()) -> binary().
prev_hash_block(#hc_parent_block{header = #hc_parent_block_header{prev_hash = PrevH}}) ->
    PrevH.

-spec commitments_in_block(parent_block()) -> [aehc_commitment:commitment()].
commitments_in_block(#hc_parent_block{commitments = Commitments}) ->
    Commitments.

-spec hash_block(parent_block()) -> binary().
hash_block(#hc_parent_block{header = #hc_parent_block_header{hash = Hash}}) ->
    Hash.

-spec height_block(parent_block()) -> non_neg_integer().
height_block(#hc_parent_block{header = #hc_parent_block_header{height = Height}}) ->
    Height.

-spec block_header(parent_block()) -> parent_block_header().
block_header(#hc_parent_block{header = Header}) ->
    Header.

-spec is_hc_parent_block(any()) -> boolean().
is_hc_parent_block(#hc_parent_block{}) -> true;
is_hc_parent_block(_) -> false.
