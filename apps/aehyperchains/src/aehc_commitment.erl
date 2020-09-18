%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Abstract data type for processing validator commitments agnostic from the parent chain
%%% @end
%%%-------------------------------------------------------------------

-module(aehc_commitment).

-export([ new/2
        , has_pogf/1
        , pogf/1
        , pogf_hash/1
        , header/1
        , hash/1
        ]).

-export([ payload_delegate/1
        , payload_keyblock/1
        , payload_pogf/1
        ]).

-include("../../aecore/include/blocks.hrl").
-include("aehc_utils.hrl").

-record(hc_commitment, {
        header :: aehc_commitment_header:commitment_header(),
        pogf :: aehc_pogf:pogf()
    }).
-type commitment() :: #hc_commitment{}.
-export_type([commitment/0]).

-spec new(aehc_commitment_header:commitment_header(), aehc_pogf:pogf()) -> commitment().
new(Header, PoGF) ->
    #hc_commitment{header = Header, pogf = PoGF}.

-spec has_pogf(commitment()) -> boolean().
has_pogf(#hc_commitment{pogf = no_pogf}) -> false;
has_pogf(#hc_commitment{pogf = _}) -> true.

-spec pogf(commitment()) -> aehc_pogf:pogf().
pogf(#hc_commitment{pogf = PoGF}) ->
    PoGF.

-spec pogf_hash(commitment()) -> pogf_hash().
pogf_hash(#hc_commitment{header = Header}) ->
    aehc_commitment_header:hc_pogf_hash(Header).

-spec header(commitment()) -> aehc_commitment_header:commitment_header().
header(#hc_commitment{header = Header}) ->
    Header.

-spec hash(commitment()) -> commitment_hash().
hash(#hc_commitment{header = Header}) ->
    aehc_commitment_header:hash(Header).

%%%===================================================================
%%%  Payload accessors
%%%===================================================================

-spec payload_delegate(commitment_hash()) -> commiter_pubkey().
payload_delegate(<<Delegate:?COMMITER_PUB_BYTES, _:?COMMITMENT_HASH_BYTES, _:?POGF_HASH_BYTES>>) ->
    Delegate.


-spec payload_keyblock(commitment_hash()) -> block_header_hash().
payload_keyblock(<<_:?COMMITER_PUB_BYTES, Keyblock:?COMMITMENT_HASH_BYTES, _:?POGF_HASH_BYTES>>) ->
    Keyblock.

-spec payload_pogf(commitment_hash()) -> pogf_hash().
payload_pogf(<<_:?COMMITER_PUB_BYTES, _:?COMMITMENT_HASH_BYTES, PoGFHash:?POGF_HASH_BYTES>>) ->
    PoGFHash.
