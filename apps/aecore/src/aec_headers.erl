%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% API for headers
%%% @end
%%%-------------------------------------------------------------------
-module(aec_headers).

%% API
-export([assert_key_header/1,
         assert_micro_header/1,
         beneficiary/1,
         deserialize_from_binary/1,
         deserialize_from_client/2,
         deserialize_from_binary_partial/1,
         deserialize_from_map/1,
         deserialize_key_from_binary/1,
         deserialize_micro_from_binary/1,
         deserialize_pow_evidence/1,
         difficulty/1,
         hash_header/1,
         height/1,
         miner/1,
         new_key_header/11,
         new_micro_header/8,
         nonce/1,
         pof_hash/1,
         pow/1,
         prev_hash/1,
         prev_key_hash/1,
         root_hash/1,
         serialize_pow_evidence/1,
         serialize_to_binary/1,
         serialize_to_map/1,
         serialize_to_signature_binary/1,
         set_height/2,
         set_miner/2,
         set_nonce/2,
         set_nonce_and_pow/3,
         set_pof_hash/2,
         set_prev_hash/2,
         set_prev_key_hash/2,
         set_root_hash/2,
         set_signature/2,
         serialize_for_client/2,
         set_target/2,
         set_time_in_msecs/2,
         signature/1,
         target/1,
         time_in_msecs/1,
         time_in_secs/1,
         txs_hash/1,
         type/1,
         update_micro_candidate/3,
         validate_key_block_header/1,
         validate_micro_block_header/1,
         version/1
        ]).

-include("blocks.hrl").

%%%===================================================================
%%% Records and types
%%%===================================================================

-type height() :: aec_blocks:height().

-record(mic_header, {
          height       = 0                                     :: height(),
          pof_hash     = <<>>                                  :: aec_pof:hash(),
          prev_hash    = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          prev_key     = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          root_hash    = <<0:?STATE_HASH_BYTES/unit:8>>        :: state_hash(),
          signature    = <<0:?BLOCK_SIGNATURE_BYTES/unit:8>>   :: block_signature(),
          txs_hash     = <<0:?TXS_HASH_BYTES/unit:8>>          :: txs_hash(),
          time         = ?GENESIS_TIME                         :: non_neg_integer(),
          version                                              :: non_neg_integer()
         }).

-record(key_header, {
          height       = 0                                     :: height(),
          prev_hash    = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          prev_key     = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          root_hash    = <<0:?STATE_HASH_BYTES/unit:8>>        :: state_hash(),
          target       = ?HIGHEST_TARGET_SCI                   :: aec_pow:sci_int(),
          nonce        = 0                                     :: non_neg_integer(),
          time         = ?GENESIS_TIME                         :: non_neg_integer(),
          version                                              :: non_neg_integer(),
          pow_evidence = no_value                              :: aec_pow:pow_evidence(),
          miner        = <<0:?MINER_PUB_BYTES/unit:8>>         :: miner_pubkey(),
          beneficiary  = <<0:?BENEFICIARY_PUB_BYTES/unit:8>>   :: beneficiary_pubkey()
         }).

-opaque key_header()   :: #key_header{}.
-opaque micro_header() :: #mic_header{}.
-type header()         :: key_header() | micro_header().

-export_type([ header/0
             , key_header/0
             , micro_header/0
             ]).

-define(POW_EV_SIZE, 42).

%%%===================================================================
%%% Test interface
%%%===================================================================

-ifdef(TEST).

-export([ raw_key_header/0
        , raw_micro_header/0
        , set_version/2
        , set_version_and_height/3
        ]).

raw_key_header() ->
  #key_header{ root_hash = <<0:32/unit:8>>,
               version = ?PROTOCOL_VERSION }.

raw_micro_header() ->
  #mic_header{ root_hash = <<0:32/unit:8>>,
               version = ?PROTOCOL_VERSION }.

set_version(#key_header{} = H, Version) -> H#key_header{version = Version};
set_version(#mic_header{} = H, Version) -> H#mic_header{version = Version}.

set_version_and_height(#key_header{} = H, Version, Height) ->
    H#key_header{version = Version, height = Height};
set_version_and_height(#mic_header{} = H, Version, Height) ->
    H#mic_header{version = Version, height = Height}.

-endif. %% TEST

%%%===================================================================
%%% Header structure
%%%===================================================================

-spec assert_key_header(key_header()) -> ok.
assert_key_header(#key_header{}) -> ok;
assert_key_header(Other) -> error({illegal_key_header, Other}).

-spec assert_micro_header(micro_header()) -> ok.
assert_micro_header(#mic_header{}) -> ok;
assert_micro_header(Other) -> error({illegal_key_header, Other}).

-spec type(header()) -> block_type().
type(#key_header{}) -> key;
type(#mic_header{}) -> micro.

%%%===================================================================
%%% Constructors
%%%===================================================================

-spec new_key_header(height(), block_header_hash(), block_header_hash(),
                     state_hash(), miner_pubkey(), beneficiary_pubkey(),
                     aec_pow:sci_int(), aec_pow:pow_evidence(),
                     non_neg_integer(), non_neg_integer(), non_neg_integer()
                    ) -> header().
new_key_header(Height, PrevHash, PrevKeyHash, RootHash, Miner, Beneficiary,
               Target, Evd, Nonce, Time, Version) ->
    #key_header{height       = Height,
                prev_hash    = PrevHash,
                prev_key     = PrevKeyHash,
                root_hash    = RootHash,
                miner        = Miner,
                beneficiary  = Beneficiary,
                target       = Target,
                pow_evidence = Evd,
                nonce        = Nonce,
                time         = Time,
                version      = Version
               }.

-spec new_micro_header(height(), block_header_hash(), block_header_hash(),
                       state_hash(), non_neg_integer(), txs_hash(),
                       aec_pof:hash(), non_neg_integer()
                      ) -> header().
new_micro_header(Height, PrevHash, PrevKey, RootHash, Time, TxsHash, PoFHash, Version) ->
    #mic_header{height    = Height,
                pof_hash  = PoFHash,
                prev_hash = PrevHash,
                prev_key  = PrevKey,
                root_hash = RootHash,
                txs_hash  = TxsHash,
                time      = Time,
                version   = Version
               }.

%%%===================================================================
%%% Header hash
%%%===================================================================

-spec hash_header(header()) -> {ok, aec_blocks:block_header_hash()}.
hash_header(H) ->
    BinaryH = serialize_to_binary(H),
    {ok, aec_hash:hash(header, BinaryH)}.

%%%===================================================================
%%% Getters and setters
%%%===================================================================

-spec beneficiary(key_header()) -> beneficiary_pubkey().
beneficiary(Header) ->
    Header#key_header.beneficiary.

-spec height(header()) -> height().
height(#key_header{height = H}) -> H;
height(#mic_header{height = H}) -> H.

-spec set_height(header(), height()) -> header().
set_height(#key_header{} = H, Height) -> H#key_header{height = Height};
set_height(#mic_header{} = H, Height) -> H#mic_header{height = Height}.

-spec prev_hash(header()) -> block_header_hash().
prev_hash(#key_header{prev_hash = H}) -> H;
prev_hash(#mic_header{prev_hash = H}) -> H.

-spec prev_key_hash(header()) -> block_header_hash().
prev_key_hash(#key_header{prev_key = H}) -> H;
prev_key_hash(#mic_header{prev_key = H}) -> H.

-spec set_prev_key_hash(header(), block_header_hash()) -> header().
set_prev_key_hash(#key_header{} = H, Hash) -> H#key_header{prev_key = Hash};
set_prev_key_hash(#mic_header{} = H, Hash) -> H#mic_header{prev_key = Hash}.

-spec miner(key_header()) -> miner_pubkey().
miner(Header) ->
    Header#key_header.miner.

-spec set_miner(key_header(), aec_keys:pubkey()) -> header().
set_miner(#key_header{} = H, Miner) -> H#key_header{miner = Miner}.

-spec nonce(key_header()) -> non_neg_integer().
nonce(#key_header{nonce = N}) -> N.

-spec set_nonce(key_header(), non_neg_integer()) -> key_header().
set_nonce(Header, Nonce) ->
    Header#key_header{nonce = Nonce}.

-spec set_prev_hash(header(), block_header_hash()) -> header().
set_prev_hash(#key_header{} = H, Hash) -> H#key_header{prev_hash = Hash};
set_prev_hash(#mic_header{} = H, Hash) -> H#mic_header{prev_hash = Hash}.

-spec pof_hash(micro_header()) -> aec_pof:hash().
pof_hash(#mic_header{pof_hash = Hash}) ->
    Hash.

-spec set_pof_hash(micro_header(), aec_pof:hash()) -> micro_header().
set_pof_hash(Header, Hash) when byte_size(Hash) =:= 0;
                                byte_size(Hash) =:= 32 ->
    Header#mic_header{pof_hash = Hash}.

-spec pow(key_header()) -> aec_pow:pow_evidence().
pow(#key_header{pow_evidence = Evd}) ->
    Evd.

-spec root_hash(header()) -> state_hash().
root_hash(#key_header{root_hash = H}) -> H;
root_hash(#mic_header{root_hash = H}) -> H.

-spec set_root_hash(header(), state_hash()) -> header().
set_root_hash(#key_header{} = H, Hash) -> H#key_header{root_hash = Hash};
set_root_hash(#mic_header{} = H, Hash) -> H#mic_header{root_hash = Hash}.

-spec set_nonce_and_pow(key_header(), aec_pow:nonce(), aec_pow:pow_evidence()
                       ) -> key_header().
set_nonce_and_pow(#key_header{} = H, Nonce, Evd) ->
    H#key_header{nonce = Nonce, pow_evidence = Evd}.

-spec difficulty(key_header()) -> aec_pow:difficulty().
difficulty(Header) ->
    aec_pow:target_to_difficulty(target(Header)).

-spec signature(micro_header()) -> block_signature().
signature(Header) ->
    Header#mic_header.signature.

-spec set_signature(micro_header(), block_signature()) -> micro_header().
set_signature(#mic_header{} = Header, Sig) ->
    Header#mic_header{signature = Sig}.

-spec target(key_header()) -> aec_pow:sci_int().
target(Header) ->
    Header#key_header.target.

-spec set_target(key_header(), aec_pow:sci_int()) -> header().
set_target(Header, NewTarget) ->
    Header#key_header{ target = NewTarget }.

-spec txs_hash(micro_header()) -> txs_hash().
txs_hash(#mic_header{txs_hash = Hash}) ->
    Hash.

-spec version(header()) -> non_neg_integer().
version(#key_header{version = V}) -> V;
version(#mic_header{version = V}) -> V.

-spec time_in_secs(header()) -> non_neg_integer().
time_in_secs(#key_header{time = T}) -> aeu_time:msecs_to_secs(T);
time_in_secs(#mic_header{time = T}) -> aeu_time:msecs_to_secs(T).

-spec time_in_msecs(header()) -> non_neg_integer().
time_in_msecs(#key_header{time = T}) -> T;
time_in_msecs(#mic_header{time = T}) -> T.

-spec set_time_in_msecs(header(), non_neg_integer()) -> header().
set_time_in_msecs(#key_header{} = H, Time) -> H#key_header{time = Time};
set_time_in_msecs(#mic_header{} = H, Time) -> H#mic_header{time = Time}.

-spec update_micro_candidate(micro_header(), txs_hash(), state_hash()) ->
                                    micro_header().
update_micro_candidate(#mic_header{} = H, TxsRootHash, RootHash) ->
    H#mic_header{txs_hash = TxsRootHash,
                 root_hash = RootHash
                }.

%%%===================================================================
%%% Serialization
%%%===================================================================

-spec serialize_to_map(header()) -> map().
serialize_to_map(#key_header{} = Header) ->
    #{<<"height">> => Header#key_header.height,
      <<"prev_hash">> => Header#key_header.prev_hash,
      <<"prev_key_hash">> => Header#key_header.prev_key,
      <<"state_hash">> => Header#key_header.root_hash,
      <<"miner">> => Header#key_header.miner,
      <<"beneficiary">> => Header#key_header.beneficiary,
      <<"target">> => Header#key_header.target,
      <<"pow">> => Header#key_header.pow_evidence,
      <<"nonce">> => Header#key_header.nonce,
      <<"time">> => Header#key_header.time,
      <<"version">> => Header#key_header.version};
serialize_to_map(#mic_header{} = Header) ->
    #{<<"height">> => Header#mic_header.height,
      <<"prev_hash">> => Header#mic_header.prev_hash,
      <<"prev_key_hash">> => Header#mic_header.prev_key,
      <<"signature">> => Header#mic_header.signature,
      <<"state_hash">> => Header#mic_header.root_hash,
      <<"txs_hash">> => Header#mic_header.txs_hash,
      <<"time">> => Header#mic_header.time,
      <<"version">> => Header#mic_header.version}.

-spec serialize_for_client(header(), block_type()) -> map().
serialize_for_client(#key_header{} = Header, PrevBlockType) ->
    {ok, Hash} = hash_header(Header),
    Res =
        #{<<"hash">>          => encode_block_hash(key, Hash),
          <<"height">>        => Header#key_header.height,
          <<"prev_hash">>     => encode_block_hash(PrevBlockType, Header#key_header.prev_hash),
          <<"prev_key_hash">> => encode_block_hash(key, Header#key_header.prev_key),
          <<"state_hash">>    => aec_base58c:encode(block_state_hash, Header#key_header.root_hash),
          <<"miner">>         => aec_base58c:encode(account_pubkey, Header#key_header.miner),
          <<"beneficiary">>   => aec_base58c:encode(account_pubkey, Header#key_header.beneficiary),
          <<"target">>        => Header#key_header.target,
          <<"time">>          => Header#key_header.time,
          <<"version">>       => Header#key_header.version},
    case Header#key_header.pow_evidence of
        no_value ->
            Res;
        _ ->
            Res#{<<"pow">>   => serialize_pow_evidence(Header#key_header.pow_evidence),
                 <<"nonce">> => Header#key_header.nonce}
    end;
serialize_for_client(#mic_header{} = Header, PrevBlockType) ->
    {ok, Hash} = hash_header(Header),
    #{<<"hash">>       => encode_block_hash(micro, Hash),
      <<"height">>     => Header#mic_header.height,
      <<"pof_hash">>   => encode_pof_hash(pof_hash(Header)),
      <<"prev_hash">>  => encode_block_hash(PrevBlockType, Header#mic_header.prev_hash),
      <<"prev_key_hash">> => encode_block_hash(key, Header#mic_header.prev_key),
      <<"signature">>  => aec_base58c:encode(signature, Header#mic_header.signature),
      <<"state_hash">> => aec_base58c:encode(block_state_hash, Header#mic_header.root_hash),
      <<"time">>       => Header#mic_header.time,
      <<"txs_hash">>   => aec_base58c:encode(block_tx_hash, Header#mic_header.txs_hash),
      <<"version">>    => Header#mic_header.version
     }.

encode_block_hash(key, Hash) ->
    aec_base58c:encode(key_block_hash, Hash);
encode_block_hash(micro, Hash) ->
    aec_base58c:encode(micro_block_hash, Hash).

encode_pof_hash(<<>>) ->
    <<"no_fraud">>;
encode_pof_hash(PofHash) ->
    aec_base58c:encode(pof_hash, PofHash).

-spec deserialize_from_client(key, map()) -> {ok, header()} | {error, term()}.
deserialize_from_client(key, KeyBlock) ->
    try
        {ok, #key_header{height       = maps:get(<<"height">>, KeyBlock),
                         prev_hash    = decode(block_hash, maps:get(<<"prev_hash">>, KeyBlock)),
                         prev_key     = decode(key_block_hash, maps:get(<<"prev_key_hash">>, KeyBlock)),
                         root_hash    = decode(block_state_hash, maps:get(<<"state_hash">>, KeyBlock)),
                         miner        = decode(account_pubkey, maps:get(<<"miner">>, KeyBlock)),
                         beneficiary  = decode(account_pubkey, maps:get(<<"beneficiary">>, KeyBlock)),
                         target       = maps:get(<<"target">>, KeyBlock),
                         pow_evidence = deserialize_pow_evidence(maps:get(<<"pow">>, KeyBlock)),
                         nonce        = maps:get(<<"nonce">>, KeyBlock),
                         time         = maps:get(<<"time">>, KeyBlock),
                         version      = maps:get(<<"version">>, KeyBlock)}}
    catch
        _:_ -> {error, invalid_header}
    end.

-spec deserialize_from_map(map()) -> {'ok', header()} | {'error', term()}.
deserialize_from_map(#{<<"height">> := Height,
                       <<"prev_hash">> := PrevHash,
                       <<"prev_key_hash">> := PrevKeyHash,
                       <<"state_hash">> := RootHash,
                       <<"miner">> := Miner,
                       <<"beneficiary">> := Beneficiary,
                       <<"target">> := Target,
                       <<"pow">> := PowEvidence,
                       <<"nonce">> := Nonce,
                       <<"time">> := Time,
                       <<"version">> := Version}) ->
    %% Prevent forging a solution without performing actual work by prefixing digits
    %% to a valid nonce (produces valid PoW after truncating to the allowed range)
    case Nonce of
        N when N < 0; N > ?MAX_NONCE ->
            {error, bad_nonce};
        _ ->
            {ok, #key_header{height = Height,
                             prev_hash = PrevHash,
                             prev_key = PrevKeyHash,
                             root_hash = RootHash,
                             miner = Miner,
                             beneficiary = Beneficiary,
                             target = Target,
                             pow_evidence = PowEvidence,
                             nonce = Nonce,
                             time = Time,
                             version = Version}}
    end;
deserialize_from_map(#{<<"height">> := Height,
                       <<"prev_hash">> := PrevHash,
                       <<"prev_key_hash">> := PrevKeyHash,
                       <<"signature">> := Signature,
                       <<"state_hash">> := RootHash,
                       <<"txs_hash">> := TxsHash,
                       <<"time">> := Time,
                       <<"version">> := Version}) ->
    {ok, #mic_header{height = Height,
                     prev_hash = PrevHash,
                     prev_key = PrevKeyHash,
                     root_hash = RootHash,
                     signature = Signature,
                     txs_hash = TxsHash,
                     time = Time,
                     version = Version}}.

-spec serialize_to_signature_binary(micro_header()
                                   ) -> deterministic_header_binary().
serialize_to_signature_binary(#mic_header{signature = Sig} = H) ->
    case Sig of
        <<0:?BLOCK_SIGNATURE_BYTES/unit:8>> ->
            serialize_to_binary(H);
        <<_:?BLOCK_SIGNATURE_BYTES/unit:8>> ->
            Blank  = <<0:?BLOCK_SIGNATURE_BYTES/unit:8>>,
            serialize_to_binary(set_signature(H, Blank))
    end.

-spec serialize_to_binary(header()) -> deterministic_header_binary().
serialize_to_binary(#key_header{} = Header) ->
    PowEvidence = serialize_pow_evidence_to_binary(Header#key_header.pow_evidence),
    Flags = construct_key_flags(Header),
    %% Todo check size of hashes = (?BLOCK_HEADER_HASH_BYTES*8),
    <<(Header#key_header.version):32,
      Flags:32/bits,
      (Header#key_header.height):64,
      (Header#key_header.prev_hash)/binary,
      (Header#key_header.prev_key)/binary,
      (Header#key_header.root_hash)/binary,
      (Header#key_header.miner)/binary,
      (Header#key_header.beneficiary)/binary,
      (Header#key_header.target):64,
      PowEvidence/binary,
      (Header#key_header.nonce):64,
      (Header#key_header.time):64>>;
serialize_to_binary(#mic_header{} = Header) ->
    Flags = construct_micro_flags(Header),
    <<(Header#mic_header.version):32,
      Flags:32/bits,
      (Header#mic_header.height):64,
      (Header#mic_header.prev_hash)/binary,
      (Header#mic_header.prev_key)/binary,
      (Header#mic_header.root_hash)/binary,
      (Header#mic_header.txs_hash)/binary,
      (Header#mic_header.time):64,
      (Header#mic_header.pof_hash)/binary, %% Either 0 or 32 bytes.
      (Header#mic_header.signature)/binary>>.

construct_key_flags(#key_header{}) ->
    <<?KEY_HEADER_TAG:1, 0:31>>.

construct_micro_flags(#mic_header{pof_hash = Bin}) ->
    PoFFlag = min(byte_size(Bin), 1),
    <<?MICRO_HEADER_TAG:1, PoFFlag:1, 0:30>>.

-spec deserialize_from_binary(deterministic_header_binary()) -> header().

deserialize_from_binary(Bin) ->
    case deserialize_from_binary_partial(Bin) of
        {key, Header} -> Header;
        {micro, Header, <<>>} -> Header;
        Other -> error({illegal_header, Other})
    end.

-spec deserialize_from_binary_partial(binary()) ->
                                             {'key', key_header()}
                                           | {'micro', micro_header(), binary()}
                                           | {'error', term()}.
deserialize_from_binary_partial(<<?PROTOCOL_VERSION:32,
                                  ?KEY_HEADER_TAG:1,
                                  _/bits>> = Bin) ->
    case deserialize_key_from_binary(Bin) of
        {ok, Header} -> {key, Header};
        {error, _} = E -> E
    end;
deserialize_from_binary_partial(<<?PROTOCOL_VERSION:32,
                                  ?MICRO_HEADER_TAG:1,
                                  PoFFlag:1,
                                  _/bits>> = Bin) ->
    HeaderSize = PoFFlag * 32 + ?MIC_HEADER_MIN_BYTES,
    case Bin of
        <<HeaderBin:HeaderSize/binary, Rest/binary>> ->
            case deserialize_micro_from_binary(HeaderBin) of
                {ok, Header} -> {micro, Header, Rest};
                {error, _} = E -> E
            end;
        _ ->
            {error, malformed_header}
    end.

-spec deserialize_key_from_binary(deterministic_header_binary()) ->
                                         {'ok', key_header()}
                                       | {'error', term()}.
deserialize_key_from_binary(<<?PROTOCOL_VERSION:32,
                              ?KEY_HEADER_TAG:1,
                              0:31, %% Remaining flags.
                              Height:64,
                              PrevHash:?BLOCK_HEADER_HASH_BYTES/binary,
                              PrevKeyHash:?BLOCK_HEADER_HASH_BYTES/binary,
                              RootHash:?STATE_HASH_BYTES/binary,
                              Miner:32/binary,
                              Beneficiary:32/binary,
                              Target:64,
                              PowEvidenceBin:168/binary,
                              Nonce:64,
                              Time:64 >>) ->
    PowEvidence = deserialize_pow_evidence_from_binary(PowEvidenceBin),
    H = #key_header{height = Height,
                    prev_hash = PrevHash,
                    prev_key = PrevKeyHash,
                    root_hash = RootHash,
                    miner = Miner,
                    beneficiary = Beneficiary,
                    target = Target,
                    pow_evidence = PowEvidence,
                    nonce = Nonce,
                    time = Time,
                    version = ?PROTOCOL_VERSION},
    {ok, H};
deserialize_key_from_binary(_Other) ->
    {error, malformed_header}.


-spec deserialize_micro_from_binary(deterministic_header_binary()) ->
                                           {'ok', micro_header()}
                                         | {'error', term()}.
deserialize_micro_from_binary(<<?PROTOCOL_VERSION:32,
                                ?MICRO_HEADER_TAG:1,
                                PoFTag:1,
                                0:30, %% Remaining flags
                                Height:64,
                                PrevHash:?BLOCK_HEADER_HASH_BYTES/binary,
                                PrevKeyHash:?BLOCK_HEADER_HASH_BYTES/binary,
                                RootHash:?STATE_HASH_BYTES/binary,
                                TxsHash:?TXS_HASH_BYTES/binary,
                                Time:64,
                                Rest/binary
                              >>) ->
    PoFHashSize = PoFTag * 32,
    case Rest of
        <<PoFHash:PoFHashSize/binary,
          Signature:?BLOCK_SIGNATURE_BYTES/binary>> ->
            H = #mic_header{height = Height,
                            pof_hash = PoFHash,
                            prev_hash = PrevHash,
                            prev_key = PrevKeyHash,
                            root_hash = RootHash,
                            signature = Signature,
                            txs_hash = TxsHash,
                            time = Time,
                            version = ?PROTOCOL_VERSION},
            {ok, H};
        _ ->
            {error, malformed_header}
    end;
deserialize_micro_from_binary(_Other) ->
    {error, malformed_header}.

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

%%%===================================================================
%%% Validation
%%%===================================================================

validate_key_block_header(Header) ->
    ProtocolVersions = aec_hard_forks:protocols(aec_governance:protocols()),

    Validators = [fun validate_version/1,
                  fun validate_pow/1,
                  fun validate_max_time/1
                  ],
    aeu_validation:run(Validators, [{Header, ProtocolVersions}]).

validate_micro_block_header(Header) ->
    %% NOTE: The signature is not validated since we don't know the leader key
    %%       This check is performed when adding the header to the chain.
    ProtocolVersions = aec_hard_forks:protocols(aec_governance:protocols()),

    Validators = [fun validate_version/1,
                  fun validate_micro_block_cycle_time/1,
                  fun validate_max_time/1
    ],
    aeu_validation:run(Validators, [{Header, ProtocolVersions}]).

-spec validate_version({header(), aec_governance:protocols()}) ->
                              ok | {error, Reason} when
      Reason :: unknown_protocol_version
              | {protocol_version_mismatch, ExpectedVersion::non_neg_integer()}.
validate_version({Header, Protocols}) ->
    V = version(Header),
    H = height(Header),
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
validate_pow({#key_header{nonce        = Nonce,
                          pow_evidence = Evd,
                          target       = Target} = Header, _})
 when Nonce >= 0, Nonce =< ?MAX_NONCE ->
    %% Zero nonce and pow_evidence before hashing, as this is how the mined block
    %% got hashed.
    Header1 = Header#key_header{nonce = 0, pow_evidence = no_value},
    HeaderBinary = serialize_to_binary(Header1),
    case aec_pow_cuckoo:verify(HeaderBinary, Nonce, Evd, Target) of
        true ->
            ok;
        false ->
            {error, incorrect_pow}
    end.

-spec validate_micro_block_cycle_time({header(), aec_governance:protocols()}) ->
        ok | {error, bad_micro_block_interval}.
validate_micro_block_cycle_time({Header, _}) ->
    Time = time_in_msecs(Header),
    PrevHash = prev_hash(Header),
    case aec_chain:get_header(PrevHash) of
        {ok, PrevHeader} ->
            PrevTime = time_in_msecs(PrevHeader),
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
validate_max_time({Header, _}) ->
    Time = time_in_msecs(Header),
    MaxAcceptedTime = aeu_time:now_in_msecs() + aec_governance:accepted_future_block_time_shift(),
    case Time < MaxAcceptedTime of
        true ->
            ok;
        false ->
            {error, block_from_the_future}
    end.

decode(Type, Enc) ->
    {ok, Val} = aec_base58c:safe_decode(Type, Enc),
    Val.
