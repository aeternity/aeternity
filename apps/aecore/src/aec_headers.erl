-module(aec_headers).

%% API
-export([prev_hash/1,
         height/1,
         nonce/1,
         target/1,
         difficulty/1,
         time_in_secs/1,
         time_in_msecs/1,
         serialize_to_network/1,
         deserialize_from_network/1,
         serialize_to_map/1,
         deserialize_from_map/1,
         serialize_to_binary/1,
         deserialize_from_binary/1,
         hash_header/1,
         serialize_pow_evidence/1,
         deserialize_pow_evidence/1]).

-include("common.hrl").
-include("blocks.hrl").

-define(POW_EV_SIZE, 42).

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

time_in_msecs(Header) ->
    Header#header.time.

-spec serialize_to_network(header()) -> {ok, binary()}.
serialize_to_network(H = #header{}) ->
    {ok, Map} = serialize_to_map(H),
    {ok, jsx:encode(Map)}.

-spec serialize_to_map(header()) -> {ok, map()}.
serialize_to_map(H = #header{}) ->
    Serialized =
      #{<<"height">> =>  height(H),
        <<"prev_hash">> => base64:encode(prev_hash(H)),
        <<"state_hash">> => base64:encode(H#header.root_hash),
        <<"target">> => H#header.target,
        <<"nonce">> => H#header.nonce,
        <<"time">> => H#header.time,
        <<"pow">> => serialize_pow_evidence(H#header.pow_evidence),
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
		    <<"pow">> := PowEvidence,
        <<"txs_hash">> := TxsHash
      } = H,
    {ok, #header{height = Height,
                 prev_hash = base64:decode(PrevHash),
                 root_hash = base64:decode(RootHash),
                 target = Target,
                 nonce = Nonce,
                 time = Time,
                 version = Version,
                 pow_evidence = deserialize_pow_evidence(PowEvidence),
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
    case is_list(Ev) andalso length(Ev) =:= ?POW_EV_SIZE of
        true ->
            Ev;
        false ->
            lists:duplicate(?POW_EV_SIZE, 0)
    end.

deserialize_pow_evidence(L) when is_list(L) ->
    % not trusting the network, filterting out any non-integers or negative
    % numbers
    PowEvidence =
      lists:filter(fun(N) -> is_integer(N) andalso N >=0 end, L),
    NoPow = lists:duplicate(?POW_EV_SIZE, 0),
    case PowEvidence =:= NoPow orelse length(PowEvidence) =/= ?POW_EV_SIZE of
        true -> % broken PoW
            'no_value';
        false ->
            PowEvidence
    end;
deserialize_pow_evidence(_) ->
    'no_value'.


