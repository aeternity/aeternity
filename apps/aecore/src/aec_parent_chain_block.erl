%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity
%%% @doc
%%% Manage hyperchain parent chain block representation
%%% @end
%%%-------------------------------------------------------------------

-module(aec_parent_chain_block).
%% top's state
-record(block,
    {
        hash = <<>> :: binary(),
        height = 0 :: non_neg_integer(),
        prev_hash = <<>> :: binary()
    }).
-opaque block() :: #block{}.

-export([new/3,
         hash/1,
         height/1,
         prev_hash/1]).

-export_type([block/0]).

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
