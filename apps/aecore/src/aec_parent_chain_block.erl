%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity
%%% @doc
%%% Manage hyperchain parent chain block representation
%%% @end
%%%-------------------------------------------------------------------

-module(aec_parent_chain_block).

-opaque hash() :: binary().

%% top's state
-record(block,
    {
        hash = <<>> :: binary(),
        height = 0 :: non_neg_integer(),
        prev_hash = <<>> :: binary(),
        time = 0 :: non_neg_integer()
    }).

-opaque block() :: #block{}.

-export([new/4,
         hash/1,
         height/1,
         prev_hash/1,
         time/1,
         encode_network_id/1
        ]).

-export_type([block/0,
              hash/0]).

%% Functionality:
%% This is a reusable structure to represent parent chain's blocks


%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API

-spec new(binary(), non_neg_integer(), binary(), non_neg_integer()) -> block().
new(Hash, Height, PrevHash, Time) ->
    #block{hash = Hash,
           height = Height,
           prev_hash = PrevHash,
           time = Time}.

-spec hash(block()) -> binary().
hash(#block{hash = Hash}) -> Hash.

-spec height(block()) -> non_neg_integer().
height(#block{height = Height}) -> Height.

-spec prev_hash(block()) -> binary().
prev_hash(#block{prev_hash = PrevHash}) -> PrevHash.

-spec time(block()) -> non_neg_integer().
time(#block{time = Time}) -> Time.

encode_network_id(NetworkId) when is_binary(NetworkId), size(NetworkId) =< 15 ->
    BytesToPad = 15 - byte_size(NetworkId),
    <<NetworkId/binary, 0:BytesToPad/unit:8>>;
encode_network_id(NetworkId) when is_binary(NetworkId) ->
    <<NetworkId:15/binary>> = NetworkId,
    NetworkId;
encode_network_id(_) ->
    <<0:15/unit:8>>.
