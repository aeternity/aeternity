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

-record(hc_commitment, {
        hc_delegate = <<0:?COMMITER_PUB_BYTES/unit:8>> :: commiter_pubkey(),
        hc_keyblock = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
        hc_pof :: binary()
    }).

-record(hc_pogf, {
        hc_header1,
        hc_header2
    }).

-record(hc_parent_block, {

}).
