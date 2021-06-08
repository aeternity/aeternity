%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Abstract data type for processing validator commitments
%%% agnostic from the parent chain.
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_commitment).

-export([new/1
        , new/2
        , has_pogf/1
        , pogf/1
        , pogf_hash/1
        , header/1
        , hash/1]).
-export_type([commitment/0]).

-include_lib("aehyperchains/include/aehc_types.hrl").

-record(hc_commitment, {
    header :: aehc_commitment_header:commitment_header(),
    pogf :: aehc_pogf:maybe_pogf()
}).
-type commitment() :: #hc_commitment{}.

-spec new(aehc_commitment_header:commitment_header()) -> commitment().
new(Header) ->
    new(Header, no_pogf).

-spec new(aehc_commitment_header:commitment_header(),
    aehc_pogf:maybe_pogf()) -> commitment().
new(Header, PoGF) ->
    #hc_commitment{header = Header, pogf = PoGF}.

-spec has_pogf(commitment()) -> boolean().
has_pogf(#hc_commitment{pogf = no_pogf}) -> false;
has_pogf(#hc_commitment{pogf = _}) -> true.

-spec pogf(commitment()) -> aehc_pogf:maybe_pogf().
pogf(#hc_commitment{pogf = PoGF}) -> PoGF.

-spec pogf_hash(commitment()) -> pogf_hash().
pogf_hash(#hc_commitment{header = Header}) ->
    aehc_commitment_header:hc_pogf_hash(Header).

-spec header(commitment()) -> aehc_commitment_header:commitment_header().
header(#hc_commitment{header = Header}) -> Header.

-spec hash(commitment()) -> commitment_hash().
hash(#hc_commitment{header = Header}) ->
    aehc_commitment_header:hash(Header).
