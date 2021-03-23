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
-author("sojourner").

%% API
-export([commitment/1]).
-export([registry/1]).

-export([is_commitment/1, is_registry/1]).

-type commitment() :: aehc_commitment:commitment().

-type payload() :: binary().

-spec commitment(commitment()) -> payload().
commitment(Commitment) ->
    Header = aehc_commitment:header(Commitment),
    KeyBlock = aehc_commitment_header:hc_keyblock(Header),
    aeser_api_encoder:encode(key_block_hash, KeyBlock).

-spec registry(binary()) -> payload().
registry(PubKey) ->
    aeser_api_encoder:encode(account_pubkey, PubKey).

-spec is_commitment(binary()) -> boolean().
is_commitment(<<"kh",_/binary>>) ->
    true;
is_commitment(_) ->
    false.

-spec is_registry(binary()) -> boolean().
is_registry(<<"ak",_/binary>>) ->
    true;
is_registry(_) ->
    false.
