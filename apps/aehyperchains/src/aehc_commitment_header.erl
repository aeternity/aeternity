%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Abstract commitment representation. This data is in fact located on another blockchain
%%% and the exact format used to store this data on the parent chain depends on the used
%%% parent chain connector. The header is (for now) not shared directly via sync over
%%% noise but is retrieved and authenticated by the parent chain connector. Distributing
%%% the headers via sync is problematic as the real source of truth is always located
%%% on the parent chain(multiple parent chains) and it's hard to determine whether some
%%% adversary node lies about the commitment list included on the parent. Creating a MPT
%%% tree from the commitments and storing it in the child block header won't help without
%%% including a proof that the given set of commitments were included in the given parent
%%% block. For now the headers include auth data for the parent chain connector to
%%% authorize this commitment - this will be removed if we decide that distributing commitments
%%% via sync won't be a thing due to security issues(we need to sync the parent chain in order
%%% to validate the chain).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_commitment_header).

-export([new/2
        , new/3
        , from_db/1
        , set_auth_data/2
        , set_pogf_hash/2
        , hc_delegate/1
        , hc_keyblock/1
        , hc_pogf_hash/1
        , hash/1]).
-export_type([commitment_header/0]).

-include_lib("aehyperchains/include/aehc_types.hrl").

-record(hc_commitment_header, {
    %% Delegate who submitted the commitment
    hc_delegate = <<0:?COMMITTER_PUB_BYTES/unit:8>> :: committer_pubkey(),
    %% Hyperchain keyblock to which the delegate committed
    hc_keyblock = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
    %% Hash of PoGF object
    hc_pogf_hash = <<0:?POGF_HASH_BYTES>> :: pogf_hash(),

    %% Connector specific authorization data proving that the given delegate
    %% submitted the commitment. In the friendly case of an AE parent chain
    %% this could include the parent chain tx which includes the above data
    %% TODO: Should this be included in the DB? This would be required if we
    %% TODO: tried to distribute the commitments in a p2p manner
    auth_data = <<>> :: binary()
}).
-opaque commitment_header() :: #hc_commitment_header{}.


%% API

-spec new(committer_pubkey(), block_header_hash()) -> commitment_header().
new(Delegate, KeyblockHash) ->
    new(Delegate, KeyblockHash, aehc_pogf:hash(no_pogf)).

-spec new(committer_pubkey(), block_header_hash(), pogf_hash()) ->
    commitment_header().
new(Delegate, KeyblockHash, PoGFHash) ->
    #hc_commitment_header{
        hc_delegate = Delegate
        , hc_keyblock = KeyblockHash
        , hc_pogf_hash = PoGFHash
    }.

-spec from_db(commitment_header()) -> commitment_header().
from_db(#hc_commitment_header{} = Header) -> Header.

-spec set_auth_data(commitment_header(), binary()) -> commitment_header().
set_auth_data(CommitmentHeader, AuthData) ->
    CommitmentHeader#hc_commitment_header{auth_data = AuthData}.

-spec set_pogf_hash(commitment_header(), pogf_hash()) -> commitment_header().
set_pogf_hash(CommitmentHeader, PoGFHash) ->
    CommitmentHeader#hc_commitment_header{hc_pogf_hash = PoGFHash}.

-spec hc_delegate(commitment_header()) -> committer_pubkey().
hc_delegate(CommitmentHeader) ->
    CommitmentHeader#hc_commitment_header.hc_delegate.

-spec hc_keyblock(commitment_header()) -> block_header_hash().
hc_keyblock(CommitmentHeader) ->
    CommitmentHeader#hc_commitment_header.hc_keyblock.

-spec hc_pogf_hash(commitment_header()) -> pogf_hash().
hc_pogf_hash(CommitmentHeader) ->
    CommitmentHeader#hc_commitment_header.hc_pogf_hash.

-spec hash(commitment_header()) -> commitment_hash().
hash(#hc_commitment_header{
    hc_delegate = D
    , hc_keyblock = K
    , hc_pogf_hash = H}) ->
    aec_hash:hash(hc_commitment, <<D/binary, K/binary, H/binary>>).
