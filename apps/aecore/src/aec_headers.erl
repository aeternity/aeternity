-module(aec_headers).

%% API
-export([beneficiary/1,
         deserialize_from_binary/1,
         deserialize_from_map/1,
         deserialize_pow_evidence/1,
         difficulty/1,
         hash_header/1,
         height/1,
         miner/1,
         new_key_header/10,
         new_micro_header/6,
         nonce/1,
         prev_hash/1,
         root_hash/1,
         serialize_pow_evidence/1,
         serialize_to_binary/1,
         serialize_to_map/1,
         set_target/2,
         set_time_in_msecs/2,
         target/1,
         time_in_msecs/1,
         time_in_secs/1,
         to_key_block/1,
         to_micro_block/3,
         type/1,
         validate_key_block_header/1,
         validate_micro_block_header/1,
         version/1
        ]).

-include("blocks.hrl").

-type height() :: aec_blocks:height().

-record(header, {
          height = 0              :: height(),
          prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          txs_hash = <<0:?TXS_HASH_BYTES/unit:8>> :: txs_hash(),
          root_hash = <<>>        :: state_hash(),
          target = ?HIGHEST_TARGET_SCI :: aec_pow:sci_int(),
          nonce = 0               :: non_neg_integer(),
          time = ?GENESIS_TIME    :: non_neg_integer(),
          version                 :: non_neg_integer(),
          pow_evidence = no_value :: aec_pow:pow_evidence(),
          miner = <<0:?MINER_PUB_BYTES/unit:8>> :: miner_pubkey(), %% TODO: remove default, confusing
          beneficiary = <<0:?BENEFICIARY_PUB_BYTES/unit:8>> :: beneficiary_pubkey()}). %% TODO: separate records for key and micro blocks..

-opaque header() :: #header{}.
-export_type([header/0]).

-define(POW_EV_SIZE, 42).

-ifdef(TEST).

-export([ raw_header/0
        , set_nonce/2
        , set_version/2
        , set_version_and_height/3
        ]).

raw_header() ->
  #header{ root_hash = <<0:32/unit:8>>,
           version = ?PROTOCOL_VERSION }.

set_nonce(Header, Nonce) ->
    Header#header{nonce = Nonce}.

set_version(Header, Version) ->
    Header#header{version = Version}.

set_version_and_height(Header, Version, Height) ->
    Header#header{version = Version
                 , height = Height
                 }.

-endif. %% TEST

-spec new_key_header(height(), block_header_hash(), state_hash(),
                     miner_pubkey(), beneficiary_pubkey(),
                     aec_pow:sci_int(), aec_pow:pow_evidence(),
                     non_neg_integer(), non_neg_integer(), non_neg_integer()
                    ) -> header().
new_key_header(Height, PrevHash, RootHash, Miner, Beneficiary, Target,
               Evd, Nonce, Time, Version) ->
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            miner = Miner,
            beneficiary = Beneficiary,
            target = Target,
            pow_evidence = Evd,
            nonce = Nonce,
            time = Time,
            version = Version}.

-spec new_micro_header(height(), block_header_hash(), state_hash(),
                       non_neg_integer(), txs_hash(), non_neg_integer()
                      ) -> header().
new_micro_header(Height, PrevHash, RootHash, Time, TxsHash, Version) ->
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            txs_hash = TxsHash,
            time = Time,
            version = Version}.

-spec to_key_block(header()) -> aec_blocks:block().
to_key_block(#header{height = Height,
                     prev_hash = PrevHash,
                     root_hash = RootHash,
                     miner = Miner,
                     beneficiary = Beneficiary,
                     target = Target,
                     pow_evidence = Evd,
                     nonce = Nonce,
                     time = Time,
                     version = Version}) ->
    aec_blocks:new_key_with_evidence(Height, PrevHash, RootHash, Target, Nonce,
                                     Time, Version, Miner, Beneficiary, Evd).

-spec to_micro_block(header(), [aetx_sign:signed_tx()], binary()) ->
                            aec_blocks:block().
to_micro_block(#header{height = Height,
                       prev_hash = PrevHash,
                       root_hash = RootHash,
                       txs_hash = TxsHash,
                       time = Time,
                       version = Version}, Txs, Signature) ->
    aec_blocks:new_signed_micro(Height, PrevHash, RootHash, TxsHash, Txs, Time, Version, Signature).

-spec prev_hash(header()) -> block_header_hash().
prev_hash(Header) ->
    Header#header.prev_hash.

-spec height(header()) -> height().
height(Header) ->
    Header#header.height.

-spec version(header()) -> non_neg_integer().
version(Header) ->
    Header#header.version.

-spec nonce(header()) -> non_neg_integer().
nonce(Header) ->
    Header#header.nonce.

-spec target(header()) -> aec_pow:sci_int().
target(Header) ->
    Header#header.target.

-spec set_target(header(), aec_pow:sci_int()) -> header().
set_target(Header, NewTarget) ->
    Header#header{ target = NewTarget }.

-spec difficulty(header()) -> aec_pow:difficulty().
difficulty(Header) ->
    aec_pow:target_to_difficulty(target(Header)).

-spec root_hash(header()) -> state_hash().
root_hash(Header) ->
    Header#header.root_hash.

-spec time_in_secs(header()) -> non_neg_integer().
time_in_secs(Header) ->
    Time = Header#header.time,
    aeu_time:msecs_to_secs(Time).

-spec time_in_msecs(header()) -> non_neg_integer().
time_in_msecs(Header) ->
    Header#header.time.

-spec set_time_in_msecs(header(), non_neg_integer()) -> header().
set_time_in_msecs(Header, Time) ->
    Header#header{time = Time}.

-spec miner(header()) -> miner_pubkey().
miner(Header) ->
    Header#header.miner.

-spec beneficiary(header()) -> beneficiary_pubkey().
beneficiary(Header) ->
    Header#header.beneficiary.

-spec serialize_to_map(header()) -> {ok, map()}.
serialize_to_map(#header{} = Header) ->
    serialize_to_map(type(Header), Header).

-spec serialize_to_map(block_type(), header()) -> {ok, map()}.
serialize_to_map(key, Header) ->
    Serialized =
      #{<<"height">> => Header#header.height,
        <<"prev_hash">> => Header#header.prev_hash,
        <<"state_hash">> => Header#header.root_hash,
        <<"miner">> => Header#header.miner,
        <<"beneficiary">> => Header#header.beneficiary,
        <<"target">> => Header#header.target,
        <<"pow">> => Header#header.pow_evidence,
        <<"nonce">> => Header#header.nonce,
        <<"time">> => Header#header.time,
        <<"version">> => Header#header.version
      },
    {ok, Serialized};
serialize_to_map(micro, Header) ->
    Serialized =
      #{<<"height">> => Header#header.height,
        <<"prev_hash">> => Header#header.prev_hash,
        <<"state_hash">> => Header#header.root_hash,
        <<"txs_hash">> => Header#header.txs_hash,
        <<"time">> => Header#header.time,
        <<"version">> => Header#header.version
      },
    {ok, Serialized}.

-spec deserialize_from_map(map()) -> header().
deserialize_from_map(#{<<"height">> := Height,
                       <<"prev_hash">> := PrevHash,
                       <<"state_hash">> := RootHash,
                       <<"miner">> := Miner,
                       <<"beneficiary">> := Beneficiary,
                       <<"target">> := Target,
                       <<"pow">> := PowEvidence,
                       <<"nonce">> := Nonce,
                       <<"time">> := Time,
                       <<"version">> := Version}) ->
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            miner = Miner,
            beneficiary = Beneficiary,
            target = Target,
            pow_evidence = PowEvidence,
            nonce = Nonce,
            time = Time,
            version = Version};
deserialize_from_map(#{<<"height">> := Height,
                       <<"prev_hash">> := PrevHash,
                       <<"state_hash">> := RootHash,
                       <<"txs_hash">> := TxsHash,
                       <<"time">> := Time,
                       <<"version">> := Version}) ->
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            txs_hash = TxsHash,
            time = Time,
            version = Version}.

-spec serialize_to_binary(header()) -> deterministic_header_binary().
serialize_to_binary(Header) ->
        serialize_to_binary(type(Header), Header).

serialize_to_binary(key, Header) ->
    PowEvidence = serialize_pow_evidence_to_binary(Header#header.pow_evidence),
    %% Todo check size of hashes = (?BLOCK_HEADER_HASH_BYTES*8),
    <<(Header#header.version):64,
      (Header#header.height):64,
      (Header#header.prev_hash)/binary,
      (Header#header.root_hash)/binary,
      (Header#header.miner)/binary,
      (Header#header.beneficiary)/binary,
      (Header#header.target):64,
      PowEvidence/binary,
      (Header#header.nonce):64,
      (Header#header.time):64>>;
serialize_to_binary(micro, Header) ->
    <<(Header#header.version):64,
      (Header#header.height):64,
      (Header#header.prev_hash)/binary,
      (Header#header.root_hash)/binary,
      (Header#header.txs_hash)/binary,
      (Header#header.time):64>>.

-spec deserialize_from_binary(deterministic_header_binary()) -> header().
deserialize_from_binary(<<Version:64,
                          Height:64,
                          PrevHash:32/binary,
                          RootHash:32/binary,
                          Miner:32/binary,
                          Beneficiary:32/binary,
                          Target:64,
                          PowEvidenceBin:168/binary,
                          Nonce:64,
                          Time:64 >>) ->
    PowEvidence = deserialize_pow_evidence_from_binary(PowEvidenceBin),
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            miner = Miner,
            beneficiary = Beneficiary,
            target = Target,
            pow_evidence = PowEvidence,
            nonce = Nonce,
            time = Time,
            version = Version};
deserialize_from_binary(<<Version:64,
                          Height:64,
                          PrevHash:32/binary,
                          RootHash:32/binary,
                          TxsHash:32/binary,
                          Time:64>>) ->
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            txs_hash = TxsHash,
            time = Time,
            version = Version}.

-spec hash_header(header()) -> {ok, aec_blocks:block_header_hash()}.
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
                  fun validate_max_time/1,
                  fun validate_median_time/1],
    aeu_validation:run(Validators, [{Header, ProtocolVersions}]).

validate_micro_block_header(Header) ->
    ProtocolVersions = aec_hard_forks:protocols(aec_governance:protocols()),

    Validators = [fun validate_version/1,
                  fun validate_micro_block_cycle_time/1,
                  fun validate_max_time/1],
    aeu_validation:run(Validators, [{Header, ProtocolVersions}]).

-spec validate_version({header(), aec_governance:protocols()}) ->
                              ok | {error, Reason} when
      Reason :: unknown_protocol_version
              | {protocol_version_mismatch, ExpectedVersion::non_neg_integer()}.
validate_version({#header{version = V, height = H}, Protocols}) ->
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
                      target       = Target} = Header, _})
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

-spec validate_median_time({header(), aec_governance:protocols()}) ->
        ok | {error, block_from_the_past}.
validate_median_time({Header = #header{time = Time}, _}) ->
    case aec_chain_state:median_timestamp(Header) of
        {ok, MTime} when Time > MTime -> ok;
        {ok, _MTime}                  -> {error, block_from_the_past};
        error                         -> ok %% We can't know yet - checked later
    end.

-spec validate_micro_block_cycle_time({header(), aec_governance:protocols()}) ->
        ok | {error, bad_micro_block_interval}.
validate_micro_block_cycle_time({#header{time = Time, prev_hash = PrevHash}, _}) ->
    case aec_chain:get_header(PrevHash) of
        {ok, PrevHeader = #header{ time = PrevTime }} ->
            MinAccepted =
                case aec_headers:type(PrevHeader) of
                    micro -> PrevTime + aec_governance:micro_block_cycle();
                    key   -> PrevTime + 1
                end,
            case Time >= MinAccepted of
                true  -> ok;
                false -> {error, bad_micro_block_interval}
            end;
        error ->
            ok %% We don't know yet - checked later
    end.

-spec validate_max_time({header(), aec_governance:protocols()}) ->
        ok | {error, block_from_the_future}.
validate_max_time({#header{time = Time}, _}) ->
    MaxAcceptedTime = aeu_time:now_in_msecs() + aec_governance:accepted_future_block_time_shift(),
    case Time < MaxAcceptedTime of
        true ->
            ok;
        false ->
            {error, block_from_the_future}
    end.

-spec type(header()) -> block_type().
type(Header = #header{}) ->
    case is_key_header(Header) of
        true  -> key;
        false -> micro
    end.

is_key_header(#header{miner = Miner, height = Height}) ->
    Miner =/= <<0:?MINER_PUB_BYTES/unit:8>> orelse
        (Miner =:= <<0:?MINER_PUB_BYTES/unit:8>> andalso Height =:= aec_block_genesis:height()).
