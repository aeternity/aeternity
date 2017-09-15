-module(aec_headers).

%% API
-export([prev_hash/1,
         height/1,
         nonce/1,
         time_in_secs/1,
         linear_difficulty/1,
         serialize_for_network/1,
         deserialize_from_network/1,
         hash_network_serialization/1,
         hash_internal_representation/1]).

-export_type([block_header_serialized_for_network/0]).

-include("common.hrl").
-include("blocks.hrl").

-type block_header_serialized_for_network() :: binary().

prev_hash(Header) ->
    Header#header.prev_hash.

height(Header) ->
    Header#header.height.

nonce(Header) ->
    Header#header.nonce.

time_in_secs(Header) ->
    Time = Header#header.time,
    aeu_time:msecs_to_secs(Time).

%% TODO Clarify linear representation of expected average amount of
%% work for mining block header.
linear_difficulty(Header) ->
    float(max(1.0, Header#header.difficulty)).

-spec serialize_for_network(header()) ->
                                   {ok, block_header_serialized_for_network()}.
serialize_for_network(H = #header{}) ->
    %% TODO: Define serialization format.
    {ok, term_to_binary(H)}.

-spec deserialize_from_network(block_header_serialized_for_network()) ->
                                      {ok, header()}.
deserialize_from_network(H) when is_binary(H) ->
    %% TODO: Define serialization format.
    {ok, #header{} = binary_to_term(H)}.

-spec hash_network_serialization(block_header_serialized_for_network()) ->
                                        {ok, block_header_hash()}.
hash_network_serialization(H) when is_binary(H) ->
    {ok, aec_sha256:hash(H)}.

-spec hash_internal_representation(header()) -> {ok, block_header_hash()}.
hash_internal_representation(H = #header{}) ->
    {ok, HH} = serialize_for_network(H),
    hash_network_serialization(HH).
