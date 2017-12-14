%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%
%%% @end
%%%=============================================================================

-module(aec_sha256).

-export([hash/1]).

-export_type([hashable/0,
              hash/1]).

-include("sha256.hrl").

-type hashable() :: binary().

%% For enabling code using this module to document types for
%% readability.
-type hash(_DataToBeHashed) :: <<_:(?HASH_BYTES*8)>>.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Calculate the SHA256 hash value of a binary
%%------------------------------------------------------------------------------
-spec hash(hashable()) -> binary().
hash(Data) when is_binary(Data) ->
    <<_:?HASH_BYTES/unit:8>> = crypto:hash(sha256, Data).
