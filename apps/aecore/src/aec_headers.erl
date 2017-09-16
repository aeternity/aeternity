-module(aec_headers).

%% API
-export([prev_hash/1,
         height/1,
         time_in_secs/1,
         serialize_to_map/1,
         deserialize_from_map/1,
         serialize_to_binary/1,
         deserialize_from_binary/1,
         hash_header/1]).

-include("common.hrl").
-include("blocks.hrl").


prev_hash(Header) ->
    Header#header.prev_hash.

height(Header) ->
    Header#header.height.

time_in_secs(Header) ->
    Time = Header#header.time,
    aeu_time:msecs_to_secs(Time).

-spec serialize_to_map(header()) -> {ok, map()}.
serialize_to_map(H = #header{}) ->
    Serialized =
      #{<<"height">> =>  height(H),
        <<"prev-hash">> => base64:encode(prev_hash(H)),
        <<"root-hash">> => base64:encode(H#header.root_hash),
        <<"difficulty">> => H#header.difficulty,
        <<"nonce">> => H#header.nonce,
        <<"time">> => H#header.time,
        <<"version">> => H#header.version
      },
    {ok, Serialized}.

-spec deserialize_from_map(map()) -> {ok, header()}.
deserialize_from_map(H = #{}) ->
      #{<<"height">> := Height,
        <<"prev-hash">> := PrevHash,
        <<"root-hash">> := RootHash,
        <<"difficulty">> := Difficulty,
        <<"nonce">> := Nonce,
        <<"time">> := Time,
        <<"version">> := Version 
      } = H,
    {ok, #header{height = Height,
                 prev_hash = base64:decode(PrevHash),
                 root_hash = base64:decode(RootHash),
                 difficulty = Difficulty,
                 nonce = Nonce,
                 time = Time,
                 version = Version}}.

-spec serialize_to_binary(header()) -> {ok, header_binary()}.
serialize_to_binary(H) ->
    {ok, Map} = serialize_to_map(H),
    {ok, jsx:encode(Map)}.
    %<<(H#header.height):64,
    %  (H#header.prev_hash):(?BLOCK_HEADER_HASH_BYTES*8),
    %  (H#header.root_hash):(?BLOCK_HEADER_HASH_BYTES*8),
    %  (H#header.difficulty):64,
    %  (H#header.nonce):64,
    %  (H#header.time):64,
    %  (H#header.version):16>>.

-spec deserialize_from_binary(header_binary()) -> {ok, header()}.
deserialize_from_binary(B) ->
    deserialize_from_map(jsx:decode(B, [return_maps])).

-spec hash_header(header()) -> {ok, header_hash()}.
hash_header(H) ->
    BinaryH = serialize_to_binary(H),
    {ok, aec_sha256:hash(BinaryH)}.
