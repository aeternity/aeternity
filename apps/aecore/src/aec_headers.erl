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
         deserialize_pow_evidence/1,
         difficulty/1,
         from_db_header/1,
         hash_header/1,
         height/1,
         info/1,
         miner/1,
         new_key_header/12,
         new_micro_header/8,
         nonce/1,
         pof_hash/1,
         pow/1,
         key_seal/1,
         prev_hash/1,
         prev_key_hash/1,
         root_hash/1,
         serialize_pow_evidence/1,
         serialize_to_binary/1,
         serialize_to_signature_binary/1,
         set_height/2,
         set_miner/2,
         set_nonce/2,
         set_nonce_and_pow/3,
         set_nonce_and_key_seal/3,
         set_info/2,
         set_pof_hash/2,
         set_key_seal/2,
         set_prev_hash/2,
         set_prev_key_hash/2,
         set_root_hash/2,
         set_signature/2,
         serialize_for_client/2,
         set_target/2,
         set_time_in_msecs/2,
         set_txs_hash/2,
         signature/1,
         target/1,
         time_in_msecs/1,
         time_in_secs/1,
         txs_hash/1,
         type/1,
         update_micro_candidate/3,
         validate_key_block_header/2,
         validate_micro_block_header/2,
         version/1,
         strip_extra/1,
         set_extra/2,
         extra/1,
         consensus_module/1
        ]).

-include("aec_consensus.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("blocks.hrl").

%%%===================================================================
%%% Records and types
%%%===================================================================

-define(IS_INT_INFO(I), is_integer(I) andalso (I >= 0) andalso (I =< 16#ffffffff)).

-type height() :: aec_blocks:height().
-type bin_info() :: <<>> | <<_:32>>. %% ?OPTIONAL_INFO_BYTES * 8
-type info()     :: aec_blocks:info().

-record(mic_header, {
          height       = 0                                     :: height(),
          pof_hash     = <<>>                                  :: aec_pof:hash(),
          prev_hash    = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          prev_key     = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          root_hash    = <<0:?STATE_HASH_BYTES/unit:8>>        :: state_hash(),
          signature    = <<0:?BLOCK_SIGNATURE_BYTES/unit:8>>   :: block_signature(),
          txs_hash     = <<0:?TXS_HASH_BYTES/unit:8>>          :: txs_hash(),
          time         = 0                                     :: non_neg_integer(),
          version                                              :: non_neg_integer(),
          extra        = #{}                                   :: map()
         }).

-record(key_header, {
          height       = 0                                     :: height(),
          prev_hash    = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          prev_key     = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          root_hash    = <<0:?STATE_HASH_BYTES/unit:8>>        :: state_hash(),
          target                                               :: aec_consensus:key_target(),
          nonce        = 0                                     :: non_neg_integer(),
          time         = 0                                     :: non_neg_integer(),
          version                                              :: non_neg_integer(),
          key_seal     = no_value                              :: aec_consensus:key_seal() | no_value,
          miner        = <<0:?MINER_PUB_BYTES/unit:8>>         :: miner_pubkey(),
          beneficiary  = <<0:?BENEFICIARY_PUB_BYTES/unit:8>>   :: beneficiary_pubkey(),
          info         = <<>>                                  :: bin_info(),
          extra        = #{}                                   :: map()
         }).

%% -record(db_key_header, {
%%           height       = 0                                     :: height(),
%%           prev_hash    = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
%%           prev_key     = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
%%           root_hash    = <<0:?STATE_HASH_BYTES/unit:8>>        :: state_hash(),
%%           target       = ?HIGHEST_TARGET_SCI                   :: aeminer_pow:sci_target(),
%%           nonce        = 0                                     :: non_neg_integer(),
%%           time         = 0                                     :: non_neg_integer(),
%%           version                                              :: non_neg_integer(),
%%           pow_evidence = no_value                              :: aeminer_pow_cuckoo:solution() | no_value,
%%           miner        = <<0:?MINER_PUB_BYTES/unit:8>>         :: miner_pubkey(),
%%           beneficiary  = <<0:?BENEFICIARY_PUB_BYTES/unit:8>>   :: beneficiary_pubkey()
%%          }).

-opaque key_header()   :: #key_header{}.
-opaque micro_header() :: #mic_header{}.
-type header()         :: key_header() | micro_header().

-export_type([ header/0
             , key_header/0
             , micro_header/0
             , block_header_hash/0
             ]).

-deprecated([{pow, 1, eventually}, {set_nonce_and_pow, 3, eventually}]).

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
    Module = aec_consensus:get_genesis_consensus_module(),
    populate_extra(
      #key_header{ root_hash = <<0:32/unit:8>>
                 , version = aec_hard_forks:protocol_effective_at_height(0)
                 , target  = Module:default_target() }
               %%, target  = aec_consensus:default_target_at_height(0) }
    ).

raw_micro_header() ->
    populate_extra(
      #mic_header{ root_hash = <<0:32/unit:8>>
                 , version   = aec_hard_forks:protocol_effective_at_height(0) }
    ).

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


-spec from_db_header(tuple() | header()) -> header().
%% We might have a legacy tuple in the db
from_db_header(Header) -> populate_extra(from_db_header_(Header)).

from_db_header_(#key_header{} = K) -> K;
from_db_header_(#mic_header{} = M) -> M;
from_db_header_({mic_header,
                 Height,
                 Pof_hash,
                 Prev_hash,
                 Prev_key,
                 Root_hash,
                 Signature,
                 Txs_hash,
                 Time,
                 Version}) ->
    #mic_header{
       height    = Height,
       pof_hash  = Pof_hash,
       prev_hash = Prev_hash,
       prev_key  = Prev_key,
       root_hash = Root_hash,
       signature = Signature,
       txs_hash  = Txs_hash,
       time      = Time,
       version   = Version};
from_db_header_({key_header,
                 Height,
                 PrevHash,
                 PrevKey,
                 RootHash,
                 Target,
                 Nonce,
                 Time,
                 Version,
                 KeySeal,
                 Miner,
                 Beneficiary,
                 Info
                }) ->
            #key_header{
               height       = Height,
               prev_hash    = PrevHash,
               prev_key     = PrevKey,
               root_hash    = RootHash,
               target       = Target,
               nonce        = Nonce,
               time         = Time,
               version      = Version,
               key_seal     = KeySeal,
               miner        = Miner,
               beneficiary  = Beneficiary,
               info         = Info
              };
from_db_header_({key_header,
                 Height,
                 PrevHash,
                 PrevKey,
                 RootHash,
                 Target,
                 Nonce,
                 Time,
                 Version,
                 KeySeal,
                 Miner,
                 Beneficiary
                }) ->
            #key_header{
               height       = Height,
               prev_hash    = PrevHash,
               prev_key     = PrevKey,
               root_hash    = RootHash,
               target       = Target,
               nonce        = Nonce,
               time         = Time,
               version      = Version,
               key_seal     = KeySeal,
               miner        = Miner,
               beneficiary  = Beneficiary,
               info         = <<>>
              };
from_db_header_(_) ->
    error(bad_db_header).

%%%===================================================================
%%% Constructors
%%%===================================================================

-spec new_key_header(height(), block_header_hash(), block_header_hash(),
                     state_hash(), miner_pubkey(), beneficiary_pubkey(),
                     aec_consensus:key_target(),
                     aec_consensus:key_seal() | 'no_value',
                     non_neg_integer(), non_neg_integer(), info(),
                     aec_hard_forks:protocol_vsn()
                    ) -> header().
new_key_header(Height, PrevHash, PrevKeyHash, RootHash, Miner, Beneficiary,
               Target, KeySeal, Nonce, Time, Info, Version) ->
    populate_extra(
        #key_header{height       = Height,
                    prev_hash    = PrevHash,
                    prev_key     = PrevKeyHash,
                    root_hash    = RootHash,
                    miner        = Miner,
                    beneficiary  = Beneficiary,
                    target       = Target,
                    key_seal     = KeySeal,
                    nonce        = Nonce,
                    time         = Time,
                    info         = make_info(Version, Info),
                    version      = Version
               }).

make_info(Version, Info) when (Version >= ?MINERVA_PROTOCOL_VSN), ?IS_INT_INFO(Info) ->
    <<Info:?OPTIONAL_INFO_BYTES/unit:8>>;
make_info(Version, default) when Version >= ?MINERVA_PROTOCOL_VSN ->
    PointReleaseInfo = aeu_info:block_info(),
    <<PointReleaseInfo:?OPTIONAL_INFO_BYTES/unit:8>>;
make_info(_Version, default) ->
    <<>>.

-spec new_micro_header(height(), block_header_hash(), block_header_hash(),
                       state_hash(), non_neg_integer(), txs_hash(),
                       aec_pof:hash(), non_neg_integer()
                      ) -> header().
new_micro_header(Height, PrevHash, PrevKey, RootHash, Time, TxsHash, PoFHash, Version) ->
    populate_extra(
        #mic_header{height    = Height,
                    pof_hash  = PoFHash,
                    prev_hash = PrevHash,
                    prev_key  = PrevKey,
                    root_hash = RootHash,
                    txs_hash  = TxsHash,
                    time      = Time,
                    version   = Version
                   }
    ).

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

-spec info(key_header()) -> non_neg_integer() | undefined.
info(#key_header{info = <<>>}) ->
    undefined;
info(#key_header{info = <<I:?OPTIONAL_INFO_BYTES/unit:8>>}) ->
    I.

-spec set_info(key_header(), info()) -> key_header().
set_info(#key_header{version = Vsn} = H, I) ->
    H#key_header{info = make_info(Vsn, I)}.

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

-spec pow(key_header()) -> aec_consensus:key_seal().
%% Deprecated - please use key_seal/1 in new code
pow(H) ->
    key_seal(H).

-spec key_seal(key_header()) -> aec_consensus:key_seal().
key_seal(#key_header{key_seal = Seal}) ->
    Seal.

-spec set_key_seal(key_header(), aec_consensus:key_seal() | no_value) -> key_header().
set_key_seal(#key_header{} = Header, KeySeal) ->
    Header#key_header{key_seal = KeySeal}.

-spec root_hash(header()) -> state_hash().
root_hash(#key_header{root_hash = H}) -> H;
root_hash(#mic_header{root_hash = H}) -> H.

-spec set_root_hash(header(), state_hash()) -> header().
set_root_hash(#key_header{} = H, Hash) -> H#key_header{root_hash = Hash};
set_root_hash(#mic_header{} = H, Hash) -> H#mic_header{root_hash = Hash}.

-spec set_nonce_and_pow(key_header(), aec_consensus:key_nonce(),
                        aec_consensus:key_seal()) -> key_header().
%% Deprecated - please use set_nonce_and_key_seal/3 in new code
set_nonce_and_pow(H, Nonce, KeySeal) ->
    set_nonce_and_key_seal(H, Nonce, KeySeal).

-spec set_nonce_and_key_seal(key_header(), aec_consensus:key_nonce(),
                        aec_consensus:key_seal()) -> key_header().
set_nonce_and_key_seal(#key_header{} = H, Nonce, KeySeal) ->
    H#key_header{nonce = Nonce, key_seal = KeySeal}.

-spec difficulty(key_header()) -> aec_consensus:key_difficulty().
difficulty(Header) ->
    Consensus = consensus_module(Header),
    Consensus:key_header_difficulty(Header).

-spec signature(micro_header()) -> block_signature().
signature(Header) ->
    Header#mic_header.signature.

-spec set_signature(micro_header(), block_signature()) -> micro_header().
set_signature(#mic_header{} = Header, Sig) ->
    Header#mic_header{signature = Sig}.

-spec target(key_header()) -> aec_consensus:key_target().
target(Header) ->
    Header#key_header.target.

-spec set_target(key_header(), aec_consensus:key_target()) -> header().
set_target(Header, NewTarget) when ?MIN_TARGET =< NewTarget, NewTarget =< ?MAX_TARGET ->
    Consensus = consensus_module(Header),
    Consensus:assert_key_target_range(NewTarget),
    Header#key_header{ target = NewTarget }.

-spec txs_hash(micro_header()) -> txs_hash().
txs_hash(#mic_header{txs_hash = Hash}) ->
    Hash.

-spec set_txs_hash(micro_header(), txs_hash()) -> micro_header().
set_txs_hash(#mic_header{} = H, TxsRootHash) ->
    H#mic_header{txs_hash = TxsRootHash
                }.

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

-spec set_extra(header(), map()) -> header().
set_extra(#key_header{} = Header, Extra) ->
    Header#key_header{extra = Extra};
set_extra(#mic_header{} = Header, Extra) ->
    Header#mic_header{extra = Extra}.

-spec extra(header()) -> map().
extra(#key_header{extra = Extra}) -> Extra;
extra(#mic_header{extra = Extra}) -> Extra.

%% The consensus module gets populated in the extra map in the consensus module
-spec consensus_module(header()) -> atom().
consensus_module(Header) ->
    #{consensus := Consensus} = extra(Header),
    Consensus.

%%%===================================================================
%%% Serialization
%%%===================================================================

-spec serialize_for_client(header(), block_type()) -> map().
serialize_for_client(#key_header{} = Header, PrevBlockType) ->
    {ok, Hash} = hash_header(Header),
    Res =
        #{<<"hash">>          => encode_block_hash(key, Hash),
          <<"height">>        => Header#key_header.height,
          <<"prev_hash">>     => encode_block_hash(PrevBlockType, Header#key_header.prev_hash),
          <<"prev_key_hash">> => encode_block_hash(key, Header#key_header.prev_key),
          <<"state_hash">>    => aeser_api_encoder:encode(block_state_hash, Header#key_header.root_hash),
          <<"miner">>         => aeser_api_encoder:encode(account_pubkey, Header#key_header.miner),
          <<"beneficiary">>   => aeser_api_encoder:encode(account_pubkey, Header#key_header.beneficiary),
          <<"target">>        => Header#key_header.target,
          <<"time">>          => Header#key_header.time,
          <<"version">>       => Header#key_header.version,
          <<"info">>          => aeser_api_encoder:encode(contract_bytearray, Header#key_header.info)
         },
    case Header#key_header.key_seal of
        no_value ->
            Res;
        _ ->
            Res#{<<"pow">>   => serialize_pow_evidence(Header#key_header.key_seal),
                 <<"nonce">> => Header#key_header.nonce}
    end;
serialize_for_client(#mic_header{} = Header, PrevBlockType) ->
    {ok, Hash} = hash_header(Header),
    #{<<"hash">>       => encode_block_hash(micro, Hash),
      <<"height">>     => Header#mic_header.height,
      <<"pof_hash">>   => encode_pof_hash(pof_hash(Header)),
      <<"prev_hash">>  => encode_block_hash(PrevBlockType, Header#mic_header.prev_hash),
      <<"prev_key_hash">> => encode_block_hash(key, Header#mic_header.prev_key),
      <<"signature">>  => aeser_api_encoder:encode(signature, Header#mic_header.signature),
      <<"state_hash">> => aeser_api_encoder:encode(block_state_hash, Header#mic_header.root_hash),
      <<"time">>       => Header#mic_header.time,
      <<"txs_hash">>   => aeser_api_encoder:encode(block_tx_hash, Header#mic_header.txs_hash),
      <<"version">>    => Header#mic_header.version
     }.

encode_block_hash(key, Hash) ->
    aeser_api_encoder:encode(key_block_hash, Hash);
encode_block_hash(micro, Hash) ->
    aeser_api_encoder:encode(micro_block_hash, Hash).

encode_pof_hash(<<>>) ->
    <<"no_fraud">>;
encode_pof_hash(PofHash) ->
    aeser_api_encoder:encode(pof_hash, PofHash).

-spec deserialize_from_client(key, map()) -> {ok, header()} | {error, term()}.
deserialize_from_client(key, KeyBlock) ->
    try
        {ok, populate_extra(
             #key_header{height       = maps:get(<<"height">>, KeyBlock),
                         prev_hash    = decode(block_hash, maps:get(<<"prev_hash">>, KeyBlock)),
                         prev_key     = decode(key_block_hash, maps:get(<<"prev_key_hash">>, KeyBlock)),
                         root_hash    = decode(block_state_hash, maps:get(<<"state_hash">>, KeyBlock)),
                         miner        = decode(account_pubkey, maps:get(<<"miner">>, KeyBlock)),
                         beneficiary  = decode(account_pubkey, maps:get(<<"beneficiary">>, KeyBlock)),
                         target       = maps:get(<<"target">>, KeyBlock),
                         key_seal     = deserialize_pow_evidence(maps:get(<<"pow">>, KeyBlock)),
                         nonce        = maps:get(<<"nonce">>, KeyBlock),
                         time         = maps:get(<<"time">>, KeyBlock),
                         version      = maps:get(<<"version">>, KeyBlock),
                         info         = decode(contract_bytearray, maps:get(<<"info">>, KeyBlock))
                        })}
    catch
        _:_ -> {error, invalid_header}
    end.

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
    PowEvidence = serialize_pow_evidence_to_binary(Header#key_header.key_seal),
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
      (Header#key_header.target):32,
      PowEvidence/binary,
      (Header#key_header.nonce):64,
      (Header#key_header.time):64,
      (Header#key_header.info)/binary %% Either 0 or 4 bytes.
    >>;
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

construct_key_flags(#key_header{info = <<>>}) ->
    ContainsInfo = 0,
    <<?KEY_HEADER_TAG:1, ContainsInfo:1, 0:30>>;
construct_key_flags(#key_header{info = <<_:?OPTIONAL_INFO_BYTES/unit:8>>} = H) ->
    [error(illegal_info_field) || version(H) < ?MINERVA_PROTOCOL_VSN],
    ContainsInfo = 1,
    <<?KEY_HEADER_TAG:1, ContainsInfo:1, 0:30>>;
construct_key_flags(#key_header{}) ->
    error(illegal_info_field).

construct_micro_flags(#mic_header{pof_hash = Bin}) ->
    PoFFlag = min(byte_size(Bin), 1),
    <<?MICRO_HEADER_TAG:1, PoFFlag:1, 0:30>>.

-spec deserialize_from_binary(deterministic_header_binary()) -> header().

deserialize_from_binary(Bin) ->
    case deserialize_from_binary_partial(Bin) of
        {key, Header} -> Header;
        {micro, Header, <<>>} -> Header;
        {error, What} -> error(What)
    end.

-spec deserialize_from_binary_partial(binary()) ->
                                             {'key', key_header()}
                                           | {'micro', micro_header(), binary()}
                                           | {'error', term()}.
deserialize_from_binary_partial(<<Version:32,
                                  ?KEY_HEADER_TAG:1,
                                  InfoFlag:1,
                                  _/bits>> = Bin) ->
    HeaderSize = InfoFlag * ?OPTIONAL_INFO_BYTES + ?KEY_HEADER_MIN_BYTES,
    case Bin of
        <<_:HeaderSize/unit:8>> when Version =:= ?ROMA_PROTOCOL_VSN,
                                     InfoFlag =:= 0 ->
            case deserialize_key_from_binary(Bin) of
                {ok, Header} -> {key, Header};
                {error, _} = E -> E
            end;
        <<_:HeaderSize/unit:8>> when Version >= ?MINERVA_PROTOCOL_VSN ->
            case deserialize_key_from_binary(Bin) of
                {ok, Header} -> {key, Header};
                {error, _} = E -> E
            end;
        _ ->
            {error, malformed_header}
    end;
deserialize_from_binary_partial(<<_Version:32,
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

%% The function does not check the validity of the protocol version based on
%% height. It gets the protocol version from the block header. The protocol
%% version check based on height is performed before inserting it into the
%% database (aec_conductor).
-spec deserialize_key_from_binary(deterministic_header_binary()) ->
                                         {'ok', key_header()}
                                       | {'error', term()}.
deserialize_key_from_binary(<<Version:32,
                              ?KEY_HEADER_TAG:1,
                              _ContainsInfoFlag:1,
                              0:30, %% Remaining flags.
                              Height:64,
                              PrevHash:?BLOCK_HEADER_HASH_BYTES/binary,
                              PrevKeyHash:?BLOCK_HEADER_HASH_BYTES/binary,
                              RootHash:?STATE_HASH_BYTES/binary,
                              Miner:32/binary,
                              Beneficiary:32/binary,
                              Target:32,
                              PowEvidenceBin:168/binary,
                              Nonce:64,
                              Time:64,
                              Info/binary
                            >>) ->
    PowEvidence = deserialize_pow_evidence_from_binary(PowEvidenceBin),
    H = #key_header{height = Height,
                    prev_hash = PrevHash,
                    prev_key = PrevKeyHash,
                    root_hash = RootHash,
                    miner = Miner,
                    beneficiary = Beneficiary,
                    target = Target,
                    key_seal = PowEvidence,
                    nonce = Nonce,
                    time = Time,
                    version = Version,
                    info = Info
                   },
    {ok, populate_extra(H)};
deserialize_key_from_binary(_Other) ->
    {error, malformed_header}.


%% The function does not check the validity of the protocol version based on
%% height. It gets the protocol version from the block header. The protocol
%% version check based on height is performed before inserting it into the
%% database (aec_conductor).
-spec deserialize_micro_from_binary(deterministic_header_binary()) ->
                                           {'ok', micro_header()}
                                         | {'error', term()}.
deserialize_micro_from_binary(<<Version:32,
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
                            version = Version},
            {ok, populate_extra(H)};
        _ ->
            {error, malformed_header}
    end;
deserialize_micro_from_binary(_Other) ->
    {error, malformed_header}.

serialize_pow_evidence_to_binary(Ev) ->
   << <<E:32>> || E <- serialize_pow_evidence(Ev) >>.

serialize_pow_evidence(Ev) ->
    case is_list(Ev) andalso length(Ev) =:= ?KEY_SEAL_SIZE of
        true ->
            Ev;
        false ->
            lists:duplicate(?KEY_SEAL_SIZE, 0)
    end.

deserialize_pow_evidence_from_binary(Bin) ->
    deserialize_pow_evidence([ X || <<X:32>> <= Bin ]).

deserialize_pow_evidence(L) when is_list(L) ->
    % not trusting the network, filtering out any non-integers or negative
    % numbers
    PowEvidence =
      lists:filter(fun(N) -> is_integer(N) andalso N >=0 end, L),
    NoPow = lists:duplicate(?KEY_SEAL_SIZE, 0),
    case PowEvidence =:= NoPow orelse length(PowEvidence) =/= ?KEY_SEAL_SIZE of
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

validate_key_block_header(Header, Protocol) ->
    Validators = [fun validate_protocol/2,
                  fun validate_key_header_seal/2,
                  fun validate_max_time/2],
    aeu_validation:run(Validators, [Header, Protocol]).

validate_micro_block_header(Header, Protocol) ->
    %% NOTE: The signature is not validated since we don't know the leader key
    %%       This check is performed when adding the header to the chain.
    Validators = [fun validate_protocol/2,
                  fun validate_micro_block_cycle_time/2,
                  fun validate_max_time/2],
    aeu_validation:run(Validators, [Header, Protocol]).

-spec validate_protocol(header(), aec_hard_forks:protocol_vsn()) ->
                               ok | {error, protocol_version_mismatch}.
validate_protocol(Header, Protocol) ->
    case version(Header) =:= Protocol of
        true  -> ok;
        false -> {error, protocol_version_mismatch}
    end.

-spec validate_key_header_seal(header(), aec_hard_forks:protocol_vsn()) ->
                          ok | {error, incorrect_pow}.
validate_key_header_seal(#key_header{nonce = Nonce} = Header, Protocol)
  when Nonce >= ?MIN_NONCE, Nonce =< ?MAX_NONCE ->
    Consensus = consensus_module(Header),
    Consensus:validate_key_header_seal(Header, Protocol).

-spec validate_micro_block_cycle_time(header(), aec_hard_forks:protocol_vsn()) ->
                                             ok | {error, bad_micro_block_interval}.
validate_micro_block_cycle_time(Header, _Protocol) ->
    Time = time_in_msecs(Header),
    PrevHash = prev_hash(Header),
    case aec_chain:dirty_get_header(PrevHash) of
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

-spec validate_max_time(header(), aec_hard_forks:protocol_vsn()) ->
                               ok | {error, block_from_the_future}.
validate_max_time(Header, _Protocol) ->
    Time = time_in_msecs(Header),
    MaxAcceptedTime = aeu_time:now_in_msecs() + aec_governance:accepted_future_block_time_shift(),
    case Time < MaxAcceptedTime of
        true ->
            ok;
        false ->
            {error, block_from_the_future}
    end.

decode(Type, Enc) ->
    {ok, Val} = aeser_api_encoder:safe_decode(Type, Enc),
    Val.

strip_extra(Header) ->
    set_extra(Header, #{}).

populate_extra(Header1) ->
    Height = height(Header1),
    Consensus = aec_consensus:get_consensus_module_at_height(Height),
    [Consensus:assert_key_target_range(Header1#key_header.target) || aec_headers:type(Header1) =:= key],
    Header2 = set_extra(Header1, Consensus:extra_from_header(Header1)),
    Consensus = consensus_module(Header2),
    Header2.
