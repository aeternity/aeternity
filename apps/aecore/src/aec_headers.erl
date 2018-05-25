-module(aec_headers).

%% API
-export([prev_hash/1,
         height/1,
         version/1,
         nonce/1,
         target/1,
         set_target/2,
         difficulty/1,
         time_in_secs/1,
         time_in_msecs/1,
         miner/1,
         serialize_to_binary/1,
         serialize_to_map/1,
         deserialize_from_binary/1,
         deserialize_from_map/1,
         hash_header/1,
         serialize_pow_evidence/1,
         deserialize_pow_evidence/1,
         root_hash/1,
         validate_key_block_header/1,
         validate_micro_block_header/2,
         type/1]).

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

version(Header) ->
    Header#header.version.

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

miner(Header) ->
    Header#header.miner.

-spec serialize_to_map(header()) -> {ok, map()}.
serialize_to_map(H = #header{}) ->
    serialize_to_map(type(H), H).

serialize_to_map(key, H) ->
    Serialized =
      #{<<"height">> =>  H#header.height,
        <<"prev_hash">> => H#header.prev_hash,
        <<"state_hash">> => H#header.root_hash,
        <<"miner">> => H#header.miner,
        <<"target">> => H#header.target,
        <<"pow">> => H#header.pow_evidence,
        <<"nonce">> => H#header.nonce,
        <<"time">> => H#header.time,
        <<"version">> => H#header.version
      },
    {ok, Serialized};
serialize_to_map(micro, H) ->
    Serialized =
      #{<<"prev_hash">> => H#header.prev_hash,
        <<"state_hash">> => H#header.root_hash,
        <<"txs_hash">> => H#header.txs_hash,
        <<"miner_hash">> => H#header.miner_hash,
        <<"signature">> => H#header.signature,
        <<"time">> => H#header.time,
        <<"version">> => H#header.version
      },
    {ok, Serialized}.

-spec deserialize_from_map(map()) -> header().
deserialize_from_map(#{<<"height">> := Height,
                       <<"prev_hash">> := PrevHash,
                       <<"state_hash">> := RootHash,
                       <<"miner">> := Miner,
                       <<"target">> := Target,
                       <<"pow">> := PowEvidence,
                       <<"nonce">> := Nonce,
                       <<"time">> := Time,
                       <<"version">> := Version}) ->
    #header{ height = Height,
             prev_hash = PrevHash,
             root_hash = RootHash,
             miner = Miner,
             target = Target,
             pow_evidence = PowEvidence,
             nonce = Nonce,
             time = Time,
             version = Version};
deserialize_from_map(#{<<"prev_hash">> := PrevHash,
                       <<"state_hash">> := RootHash,
                       <<"txs_hash">> := TxsHash,
                       <<"miner_hash">> := MinerHash,
                       <<"signature">> := Signature,
                       <<"time">> := Time,
                       <<"version">> := Version}) ->
    #header{ prev_hash = PrevHash,
             root_hash = RootHash,
             txs_hash = TxsHash,
             miner_hash = MinerHash,
             signature = Signature,
             time = Time,
             version = Version}.

-spec serialize_to_binary(header()) -> deterministic_header_binary().
serialize_to_binary(H) ->
    serialize_to_binary(type(H), H).

serialize_to_binary(key, H) ->
    PowEvidence = serialize_pow_evidence_to_binary(H#header.pow_evidence),
    %% Todo check size of hashes = (?BLOCK_HEADER_HASH_BYTES*8),
    <<(H#header.version):64,
      (H#header.height):64,
      (H#header.prev_hash)/binary,
      (H#header.root_hash)/binary,
      (H#header.miner)/binary,
      (H#header.target):64,
      PowEvidence/binary,
      (H#header.nonce):64,
      (H#header.time):64
    >>;
serialize_to_binary(micro, H) ->
    <<(H#header.version):64,
      (H#header.prev_hash)/binary,
      (H#header.root_hash)/binary,
      (H#header.txs_hash)/binary,
      (H#header.miner_hash)/binary,
      (H#header.signature)/binary,
      (H#header.time):64
    >>.

-spec deserialize_from_binary(deterministic_header_binary()) -> header().
deserialize_from_binary(<<Version:64,
                          Height:64,
                          PrevHash:32/binary,
                          RootHash:32/binary,
                          Miner:32/binary,
                          Target:64,
                          PowEvidenceBin:168/binary,
                          Nonce:64,
                          Time:64 >>) ->
    PowEvidence = deserialize_pow_evidence_from_binary(PowEvidenceBin),
    #header{ height = Height,
             prev_hash = PrevHash,
             root_hash = RootHash,
             miner = Miner,
             target = Target,
             pow_evidence = PowEvidence,
             nonce = Nonce,
             time = Time,
             version = Version};
deserialize_from_binary(<<Version:64,
                          PrevHash:32/binary,
                          RootHash:32/binary,
                          TxsHash:32/binary,
                          MinerHash:32/binary,
                          Signature:64/binary,
                          Time:64>>) ->
    #header{ prev_hash = PrevHash,
             root_hash = RootHash,
             txs_hash = TxsHash,
             miner_hash = MinerHash,
             signature = Signature,
             time = Time,
             version = Version}.

-spec hash_header(header()) -> {ok, block_header_hash()}.
hash_header(H) ->
    BinaryH = serialize_to_binary(H),
    {ok, aec_hash:hash(header, BinaryH)}.

serialize_pow_evidence_to_binary(Ev) ->
   << <<E:32>> || E <- serialize_pow_evidence(Ev) >>.

serialize_pow_evidence(Ev) ->
    case is_list(Ev) andalso length(Ev) =:= ?POW_EV_SIZE of
        true ->
            Ev;
        false ->
            lists:duplicate(?POW_EV_SIZE, 0)
    end.

deserialize_pow_evidence_from_binary(Bin) ->
    deserialize_pow_evidence([ X || <<X:32>> <= Bin ]).

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

validate_key_block_header(Header) ->
    ProtocolVersions = aec_hard_forks:protocols(aec_governance:protocols()),

    Validators = [fun validate_version/1,
                  fun validate_pow/1,
                  fun validate_time/1],
    aeu_validation:run(Validators, [{Header, ProtocolVersions, undefined}]).

validate_micro_block_header(Header, LeaderKey) ->
    ProtocolVersions = aec_hard_forks:protocols(aec_governance:protocols()),

    Validators = [fun validate_version/1,
                  fun validate_signature/1],
    aeu_validation:run(Validators, [{Header, ProtocolVersions, LeaderKey}]).

-spec validate_version({header(), aec_governance:protocols()}) ->
                              ok | {error, Reason} when
      Reason :: unknown_protocol_version
              | {protocol_version_mismatch, ExpectedVersion::non_neg_integer()}.
validate_version({#header{version = V, height = H}, Protocols, _}) ->
    case aec_hard_forks:is_known_protocol(V, Protocols) of
        false -> {error, unknown_protocol_version};
        true ->
            case aec_hard_forks:protocol_effective_at_height(H, Protocols) of
                V -> ok;
                VV -> {error, {protocol_version_mismatch, VV}}
            end
    end.

-spec validate_pow({header(), aec_governance:protocols()}) ->
                          ok | {error, incorrect_pow}.
validate_pow({#header{nonce        = Nonce,
                      pow_evidence = Evd,
                      target       = Target} = Header, _, _})
 when Nonce >= 0, Nonce =< ?MAX_NONCE ->
    %% Zero nonce and pow_evidence before hashing, as this is how the mined block
    %% got hashed.
    Header1 = Header#header{nonce = 0, pow_evidence = no_value},
    HeaderBinary = serialize_to_binary(Header1),
    case aec_pow_cuckoo:verify(HeaderBinary, Nonce, Evd, Target) of
        true ->
            ok;
        false ->
            {error, incorrect_pow}
    end.

-spec validate_time({header(), aec_governance:protocols()}) ->
                           ok | {error, block_from_the_future}.
validate_time({#header{time = Time}, _, _}) ->
    MaxAcceptedTime = aeu_time:now_in_msecs() + ?ACCEPTED_FUTURE_KEY_BLOCK_TIME_SHIFT,
    case Time < MaxAcceptedTime of
        true ->
            ok;
        false ->
            {error, block_from_the_future}
    end.

validate_signature({#header{signature = Sig} = Header, _, LeaderKey}) ->
    {ok, Bin} = hash_header(Header),
    case enacl:sign_verify_detached(Sig, Bin, LeaderKey) of
        {ok, _}    -> ok;
        {error, _} -> {error, signature_verification_failed}
    end.

type(#header{miner = undefined, pow_evidence = no_value, height = H}) when H > 0 -> micro;
type(_) -> key.
