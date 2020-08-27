%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Abstract data type for processing validator commitments agnostic from the parent chain
%%% @end
%%%-------------------------------------------------------------------

-module(aehc_commitment).

-export([]).

-include("../../aecore/include/blocks.hrl").
-include("aehc_utils.hrl").

%% Two different keyheaders with the same prev_key pointer signed by the same leader
-record(hc_pogf, {
        hc_header1 :: aec_headers:key_header(),
        hc_header2 :: aec_headers:key_header()
    }).

-record(hc_commitment_header, {
        %% Delegate who submitted the commitment
        hc_delegate = <<0:?COMMITER_PUB_BYTES/unit:8>> :: commiter_pubkey(),
        %% Hyperchain keyblock to which the delegate commited
        hc_keyblock = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
        %% Hash of PoGF object
        hc_pogf_hash = <<0:?POGF_HASH_BYTES>> :: pogf_hash(),

        %% Connector specific authorization data proving that the given delegate submitted the commitment
        %% In the friendly case of an AE parent chain this could include the parent chain tx which includes the above data
        %% TODO: Should this be included in the DB? This would be required if we tried to distribute the commitments in a p2p manner
        auth_data = <<>> :: binary()
    }).

-record(hc_commitment, {
        header :: #hc_commitment_header{},
        hc_pogf :: no_pogf | #hc_pogf{}
    }).

-record(hc_parent_block, {
        hash = <<>> :: binary(),
        prev_hash = <<>> :: binary(),
        height = 0 :: non_neg_integer(),

        %% For the case of BitcoinNG parent chains those commitments were actually included in the previous generation
        %% In other cases those are commitments which were present in the block
        commitment_header_list = [] :: [#hc_commitment_header{}]
    }).
