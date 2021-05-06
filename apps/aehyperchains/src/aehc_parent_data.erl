%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Aeternity Anstalt
%%% @doc
%%% The parent chain based data storage
%%% This module is responsible to provide a format of HC data interchange
%% TODO: To introduce protocol versioning
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_data).

%% API
-export([commitment/1]).
-export([delegate/1]).

-export([is_commitment/1, is_delegate/1]).

-type commitment() :: aehc_commitment:commitment().
-type pubkey() :: aec_keys:pubkey().

-type payload() :: binary().

-spec commitment(commitment()) -> payload().
commitment(Commitment) ->
    Header = aehc_commitment:header(Commitment),
    KeyBlock = aehc_commitment_header:hc_keyblock(Header),
    aeser_api_encoder:encode(key_block_hash, KeyBlock).

-spec delegate(pubkey()) -> payload().
delegate(PubKey) ->
    aeser_api_encoder:encode(account_pubkey, PubKey).

%% TODO Add byte size check
-spec is_commitment(binary()) -> boolean().
is_commitment(<<"kh",_/binary>>) ->
    true;
is_commitment(_) ->
    false.

-spec is_delegate(binary()) -> boolean().
is_delegate(<<"ak",_/binary>>) ->
    true;
is_delegate(_) ->
    false.
