%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Data structure used during block insertion - it should be used inside consensus modules
%%% and the consensus agnostic aec_chain_state module
%%% @end
%%%-------------------------------------------------------------------
-module(aec_chain_node).
-author("ra").

-export([new/0, new/3
        , new_header/1, new_type/1
        , header/1, hash/1, type/1, decompose/1]).

-export_type([chain_node/0, header/0, hash/0, type/0]).

-type header() :: aec_headers:header().
-type hash() :: aec_hash:hash().
-type type() :: key | micro.

%% Header along with it's hash
-record(chain_node, {
    header = undefined :: header() | undefined
    , hash = undefined :: hash() | undefined
    , type = undefined :: type() | undefined
}).
-opaque chain_node() :: #chain_node{}. % type 'node' is Erlang internal!


%% API

-spec new() -> chain_node().
new() -> #chain_node{}.

-spec new(header(), hash(), type()) -> chain_node().
new(Header, Hash, Type) ->
    #chain_node{header = Header, hash = Hash, type = Type}.

-spec new_header(header()) -> chain_node().
new_header(Header) -> #chain_node{header = Header}.

-spec new_type(type()) -> chain_node().
new_type(Type) -> #chain_node{type = Type}.

-spec header(chain_node()) -> header().
header(#chain_node{header = Header}) -> Header.

-spec hash(chain_node()) -> hash().
hash(#chain_node{hash = Hash}) -> Hash.

-spec type(chain_node()) -> type().
type(#chain_node{type = Type}) -> Type.

-spec decompose(chain_node()) -> {header(), hash(), type()}.
decompose(#chain_node{header = Header, hash = Hash, type = Type}) ->
    {Header, Hash, Type}.

