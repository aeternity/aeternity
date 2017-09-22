-module(aec_headers).

%% API
-export([prev_hash/1,
         height/1,
         nonce/1,
         target/1,
         difficulty/1,
         time_in_secs/1,
         serialize_to_network/1,
         deserialize_from_network/1,
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

nonce(Header) ->
    Header#header.nonce.

target(Header) ->
    Header#header.target.

difficulty(Header) ->
    aec_pow:target_to_difficulty(target(Header)).

time_in_secs(Header) ->
    Time = Header#header.time,
    aeu_time:msecs_to_secs(Time).

-spec serialize_to_network(header()) -> {ok, binary()}.
serialize_to_network(H = #header{}) ->
    {ok, Map} = serialize_to_map(H),
    {ok, jsx:encode(Map)}.

-spec serialize_to_map(header()) -> {ok, map()}.
serialize_to_map(H = #header{}) ->
    Pow =
        case is_integer(H#header.pow_evidence) of
            true ->
                H#header.pow_evidence;
            false ->
                0
        end,
    Serialized =
      #{<<"height">> =>  height(H),
        <<"prev_hash">> => base64:encode(prev_hash(H)),
        <<"state_hash">> => base64:encode(H#header.root_hash),
        <<"target">> => H#header.target,
        <<"nonce">> => H#header.nonce,
        <<"time">> => H#header.time,
        <<"pow">> => serialize_pow_evidence(Pow),
        <<"version">> => H#header.version,
        <<"txs_hash">> => base64:encode(H#header.txs_hash)
      },
    {ok, Serialized}.

-spec deserialize_from_network(binary()) -> {ok, header()}.
deserialize_from_network(B) when is_binary(B) ->
    deserialize_from_map(jsx:decode(B, [return_maps])).

-spec deserialize_from_map(map()) -> {ok, header()}.
deserialize_from_map(H = #{}) ->
      #{<<"height">> := Height,
        <<"prev_hash">> := PrevHash,
        <<"state_hash">> := RootHash,
        <<"target">> := Target,
        <<"nonce">> := Nonce,
        <<"time">> := Time,
        <<"version">> := Version, 
		    <<"pow">> := PowEvidence0,
        <<"txs_hash">> := TxsHash
      } = H,
    PowEvidence =
        case deserialize_pow_evidence(PowEvidence0) of
            0 ->
                'no_value';
            _ ->
                PowEvidence0
        end,
    {ok, #header{height = Height,
                 prev_hash = base64:decode(PrevHash),
                 root_hash = base64:decode(RootHash),
                 target = Target,
                 nonce = Nonce,
                 time = Time,
                 version = Version,
                 pow_evidence = PowEvidence,
                 txs_hash = base64:decode(TxsHash)}}.

-spec serialize_to_binary(header()) -> {ok, header_binary()}.
serialize_to_binary(H) ->
    {ok, Map} = serialize_to_map(H),
    {ok, jsx:encode(Map)}.
    %<<(H#header.height):64,
    %  (H#header.prev_hash):(?BLOCK_HEADER_HASH_BYTES*8),
    %  (H#header.root_hash):(?BLOCK_HEADER_HASH_BYTES*8),
    %  (H#header.target):64,
    %  (H#header.nonce):64,
    %  (H#header.time):64,
    %  (H#header.version):16>>.

-spec deserialize_from_binary(header_binary()) -> {ok, header()}.
deserialize_from_binary(B) ->
    deserialize_from_map(jsx:decode(B, [return_maps])).

%% TODO: make hash deterministic and based on the canonical serialization of
%% the header.
-spec hash_header(header()) -> {ok, block_header_hash()}.
hash_header(H) ->
    {ok, BinaryH} = serialize_to_binary(H),
    {ok, aec_sha256:hash(BinaryH)}.

serialize_pow_evidence(Ev) ->
    base64:encode(term_to_binary(Ev)).

deserialize_pow_evidence(Bin) ->
    binary_to_term(base64:decode(Bin)).

