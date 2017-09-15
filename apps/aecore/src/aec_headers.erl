-module(aec_headers).

%% API
-export([prev_hash/1,
         height/1,
         time_in_secs/1,
         get_by_height/1,
         get_by_hash/1,
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

time_in_secs(Header) ->
    Time = Header#header.time,
    aeu_time:msecs_to_secs(Time).

get_by_height(_Height) ->
    %% TODO: Return block header by height
    %% This may go to aec_blocks
    {ok, #header{}}.

get_by_hash(_Hash) ->
    %% TODO: Return block header by hash
    %% This may go to aec_blocks
    {ok, #header{}}.

-spec serialize_for_network(header()) ->
                                   {ok, block_header_serialized_for_network()}.
serialize_for_network(H = #header{}) ->
    Serialized =
      #{<<"height">> =>  height(H),
        <<"prev-hash">> => prev_hash(H),
        <<"root-hash">> => H#header.root_hash,
        <<"difficulty">> => H#header.difficulty,
        <<"nonce">> => H#header.nonce,
        <<"time">> => H#header.time,
        <<"version">> => H#header.version
      },
    {ok, Serialized}.

-spec deserialize_from_network(block_header_serialized_for_network()) ->
                                      {ok, header()}.
deserialize_from_network(H) when is_binary(H) ->
    deserialize_from_network(jsx:decode(H));
deserialize_from_network(H = #{}) ->

      #{<<"height">> := Height,
        <<"prev-hash">> := PrevHash,
        <<"root-hash">> := RootHash,
        <<"difficulty">> := Difficulty,
        <<"nonce">> := Nonce,
        <<"time">> := Time,
        <<"version">> := Version 
      } = H,
    {ok, #header{height = Height,
                 prev_hash = PrevHash,
                 root_hash = RootHash,
                 difficulty = Difficulty,
                 nonce = Nonce,
                 time = Time,
                 version = Version}}.

-spec hash_network_serialization(block_header_serialized_for_network()) ->
                                        {ok, block_header_hash()}.
hash_network_serialization(H = #{}) ->
    hash_network_serialization(jsx:encode(H));
hash_network_serialization(H) when is_binary(H) ->
    {ok, aec_sha256:hash(H)}.

-spec hash_internal_representation(header()) -> {ok, block_header_hash()}.
hash_internal_representation(H = #header{}) ->
    {ok, HH} = serialize_for_network(H),
    hash_network_serialization(HH).
