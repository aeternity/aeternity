%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity
%%% @doc
%%% Manage hyperchain parent chain block representation
%%% @end
%%%-------------------------------------------------------------------

-module(aec_parent_chain_block).
-define(NOT_SET, not_set).
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
         set_commitments/2]).

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
