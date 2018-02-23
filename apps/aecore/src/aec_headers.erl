-module(aec_headers).

%% API
-export([prev_hash/1,
         height/1,
         nonce/1,
         target/1,
         set_target/2,
         difficulty/1,
         time_in_secs/1,
         time_in_msecs/1,
         serialize_for_hash/1,
         serialize_to_map/1,
         deserialize_from_map/1,
         hash_header/1,
         serialize_pow_evidence/1,
         deserialize_pow_evidence/1,
         root_hash/1,
         validate/1]).

-include("common.hrl").
-include("blocks.hrl").

%% header() can't be opaque since it is currently used
%% in a an involved way by aec_blocks - TODO: untangle
-type header() :: #header{}.
-export_type([header/0]).

-define(POW_EV_SIZE, 42).

prev_hash(Header) ->
    Header#header.prev_hash.

height(Header) ->
    Header#header.height.

nonce(Header) ->
    Header#header.nonce.

target(Header) ->
    Header#header.target.

set_target(Header, NewTarget) ->
    Header#header{ target = NewTarget }.

difficulty(Header) ->
    aec_pow:target_to_difficulty(target(Header)).

root_hash(Header) ->
    Header#header.root_hash.

time_in_secs(Header) ->
    Time = Header#header.time,
    aeu_time:msecs_to_secs(Time).

time_in_msecs(Header) ->
    Header#header.time.

-spec serialize_to_map(header()) -> {ok, map()}.
serialize_to_map(H = #header{}) ->
    Serialized =
      #{<<"height">> =>  height(H),
        <<"prev_hash">> => aec_base58c:encode(block_hash, prev_hash(H)),
        <<"state_hash">> => aec_base58c:encode(block_state_hash, H#header.root_hash),
        <<"target">> => H#header.target,
        <<"nonce">> => H#header.nonce,
        <<"time">> => H#header.time,
        <<"pow">> => serialize_pow_evidence(H#header.pow_evidence),
        <<"version">> => H#header.version,
        <<"txs_hash">> => aec_base58c:encode(block_tx_hash, H#header.txs_hash)
      },
    {ok, Serialized}.


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
    try
        {block_hash, DecPrevHash} = aec_base58c:decode(PrevHash),
        {block_state_hash, DecRootHash} = aec_base58c:decode(RootHash),
        {block_tx_hash, DecTxsHash} = aec_base58c:decode(TxsHash),
        {ok, #header{height = Height,
                     prev_hash = DecPrevHash,
                     root_hash = DecRootHash,
                     target = Target,
                     nonce = Nonce,
                     time = Time,
                     version = Version,
                     pow_evidence = deserialize_pow_evidence(PowEvidence),
                     txs_hash = DecTxsHash}}
    catch
        error:_ ->
            {error, deserialize}
    end.

-spec serialize_for_hash(header()) -> deterministic_header_binary().
serialize_for_hash(H) ->
    PowEvidence = serialize_pow_evidence_for_hash(H#header.pow_evidence),
    %% Todo check size of hashes = (?BLOCK_HEADER_HASH_BYTES*8),
    <<(H#header.version):64,
      (H#header.height):64,
      (H#header.prev_hash)/binary,
      (H#header.txs_hash)/binary,
      (H#header.root_hash)/binary,
      (H#header.target):64,
      PowEvidence/binary,
      (H#header.nonce):64,
      (H#header.time):64
    >>.

-spec hash_header(header()) -> {ok, block_header_hash()}.
hash_header(H) ->
    BinaryH = serialize_for_hash(H),
    {ok, aec_hash:hash(header, BinaryH)}.

serialize_pow_evidence_for_hash(Ev) ->
   << <<E:32>> || E <- serialize_pow_evidence(Ev) >>.

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


-spec validate(header()) -> ok | {error, term()}.
validate(Header) ->
    Validators = [fun validate_version/1,
                  fun validate_pow/1,
                  fun validate_time/1],
    aeu_validation:run(Validators, [Header]).

-spec validate_version(header()) -> ok | {error, protocol_version_mismatch}.
validate_version(#header{version = ?PROTOCOL_VERSION}) ->
    ok;
validate_version(_Header) ->
    {error, protocol_version_mismatch}.

-spec validate_pow(header()) -> ok | {error, incorrect_pow}.
validate_pow(#header{nonce = Nonce,
                     pow_evidence = Evd,
                     target = Target} = Header) when Nonce >= 0,
                                                     Nonce =< ?MAX_NONCE ->
    %% Zero nonce and pow_evidence before hashing, as this is how the mined block
    %% got hashed.
    Header1 = Header#header{nonce = 0, pow_evidence = no_value},
    HeaderBinary = serialize_for_hash(Header1),
    case aec_pow_cuckoo:verify(HeaderBinary, Nonce, Evd, Target) of
        true ->
            ok;
        false ->
            {error, incorrect_pow}
    end.

-spec validate_time(header()) -> ok | {error, block_from_the_future}.
validate_time(#header{time = Time}) ->
    MaxAcceptedTime = aeu_time:now_in_msecs() + ?ACCEPTED_FUTURE_BLOCK_TIME_SHIFT,
    case Time < MaxAcceptedTime of
        true ->
            ok;
        false ->
            {error, block_from_the_future}
    end.
