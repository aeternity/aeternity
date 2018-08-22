%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_blocks).

%% API
-export([assert_block/1,
         beneficiary/1,
         deserialize_from_binary/1,
         deserialize_from_map/1,
         difficulty/1,
         hash_internal_representation/1,
         height/1,
         is_block/1,
         is_key_block/1,
         miner/1,
         new_key/9,
         new_key_with_evidence/10,
         new_micro/7,
         new_signed_micro/8,
         pow/1,
         prev_hash/1,
         root_hash/1,
         serialize_to_binary/1,
         serialize_to_map/1,
         set_height/2,
         set_miner/2,
         set_nonce/2,
         set_pow/3,
         set_prev_hash/2,
         set_root_hash/2,
         set_signature/2,
         set_target/2,
         set_time_in_msecs/2,
         set_txs/2,
         signature/1,
         target/1,
         time_in_msecs/1,
         to_header/1,
         txs/1,
         txs_hash/1,
         type/1,
         update_micro_candidate/5,
         validate_key_block/1,
         validate_micro_block/2,
         validate_micro_block_no_signature/1,
         version/1
        ]).

-import(aec_hard_forks, [protocol_effective_at_height/1]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("blocks.hrl").

-record(mic_block, {
          height    = 0                                     :: height(),
          prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          root_hash = <<0:?STATE_HASH_BYTES/unit:8>>        :: state_hash(),
          txs_hash  = <<0:?TXS_HASH_BYTES/unit:8>>          :: txs_hash(),
          txs       = []                                    :: [aetx_sign:signed_tx()],
          time      = ?GENESIS_TIME                         :: non_neg_integer(),
          version                                           :: non_neg_integer(),
          signature = undefined                             :: binary() | undefined
         }).

-record(key_block, {
          height       = 0                                     :: height(),
          prev_hash    = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          root_hash    = <<0:?STATE_HASH_BYTES/unit:8>>        :: state_hash(),
          target       = ?HIGHEST_TARGET_SCI                   :: aec_pow:sci_int(),
          nonce        = 0                                     :: non_neg_integer(),
          time         = ?GENESIS_TIME                         :: non_neg_integer(),
          version                                              :: non_neg_integer(),
          pow_evidence = no_value                              :: aec_pow:pow_evidence(),
          miner        = <<0:?MINER_PUB_BYTES/unit:8>>         :: miner_pubkey(),
          beneficiary  = <<0:?BENEFICIARY_PUB_BYTES/unit:8>>   :: beneficiary_pubkey()}).

-opaque key_block()   :: #key_block{}.
-opaque micro_block() :: #mic_block{}.
-type   block()       :: key_block() | micro_block().
-type   height()      :: non_neg_integer().

-export_type([key_block/0,
              micro_block/0,
              block_header_hash/0,
              height/0
             ]).

-ifdef(TEST).

-export([raw_key_block/0,
         raw_micro_block/0
        ]).

raw_key_block() ->
    #key_block{version = ?PROTOCOL_VERSION}.

raw_micro_block() ->
    #mic_block{version = ?PROTOCOL_VERSION}.

-endif. %% TEST

-spec beneficiary(key_block()) -> aec_keys:pubkey().
beneficiary(Block) ->
    Block#key_block.beneficiary.

-spec prev_hash(block()) -> block_header_hash().
prev_hash(#key_block{prev_hash = H}) -> H;
prev_hash(#mic_block{prev_hash = H}) -> H.

-spec set_prev_hash(block(), block_header_hash()) -> block().
set_prev_hash(#key_block{} = B, H) -> B#key_block{prev_hash = H};
set_prev_hash(#mic_block{} = B, H) -> B#mic_block{prev_hash = H}.

-spec height(block()) -> height().
height(#mic_block{height = H}) -> H;
height(#key_block{height = H}) -> H.

-spec set_height(block(), height()) -> block().
set_height(#key_block{} = Block, Height) -> Block#key_block{height = Height};
set_height(#mic_block{} = Block, Height) -> Block#mic_block{height = Height}.

-spec target(key_block()) -> integer().
target(Block) ->
    Block#key_block.target.

-spec difficulty(key_block()) -> float().
difficulty(Block) ->
    aec_pow:target_to_difficulty(target(Block)).

-spec assert_block(block()) -> ok.
assert_block(#key_block{}) -> ok;
assert_block(#mic_block{}) -> ok;
assert_block(Other) -> error({illegal_block, Other}).

-spec is_block(term()) -> boolean().
is_block(#key_block{}) -> true;
is_block(#mic_block{}) -> true;
is_block(_       ) -> false.

-spec is_key_block(block()) -> boolean().
is_key_block(#key_block{}) -> true;
is_key_block(#mic_block{}) -> false.

-spec time_in_msecs(block()) -> non_neg_integer().
time_in_msecs(#mic_block{time = T}) -> T;
time_in_msecs(#key_block{time = T}) -> T.

-spec set_time_in_msecs(block(), non_neg_integer()) -> block().
set_time_in_msecs(#mic_block{} = B, T) -> B#mic_block{time = T};
set_time_in_msecs(#key_block{} = B, T) -> B#key_block{time = T}.

-spec root_hash(block()) -> binary().
root_hash(#mic_block{root_hash = H}) -> H;
root_hash(#key_block{root_hash = H}) -> H.

-spec set_root_hash(block(), binary()) -> block().
set_root_hash(#mic_block{} = B, H) -> B#mic_block{root_hash = H};
set_root_hash(#key_block{} = B, H) -> B#key_block{root_hash = H}.

-spec miner(key_block()) -> aec_keys:pubkey().
miner(#key_block{miner = M}) -> M.

-spec set_miner(key_block(), aec_keys:pubkey()) -> key_block().
set_miner(#key_block{} = B, M) -> B#key_block{miner = M}.

-spec version(block()) -> non_neg_integer().
version(#mic_block{version = V}) -> V;
version(#key_block{version = V}) -> V.

%% Sets the evidence of PoW,too,  for Cuckoo Cycle
-spec set_pow(key_block(), aec_pow:nonce(), aec_pow:pow_evidence()) -> key_block().
set_pow(Block, Nonce, Evd) ->
    Block#key_block{nonce = Nonce,
                    pow_evidence = Evd}.

-spec set_nonce(key_block(), aec_pow:nonce()) -> key_block().
set_nonce(Block, Nonce) ->
    Block#key_block{nonce = Nonce}.

-spec pow(key_block()) -> aec_pow:pow_evidence().
pow(Block) ->
    Block#key_block.pow_evidence.

%% Sets the signature for microblock
-spec set_signature(micro_block(), binary()) -> micro_block().
set_signature(Block, Signature) ->
    Block#mic_block{signature = Signature}.

-spec signature(micro_block()) -> binary() | undefined.
signature(Block) ->
    Block#mic_block.signature.

-spec set_target(key_block(), non_neg_integer()) -> key_block().
set_target(Block, Target) ->
    Block#key_block{target = Target}.

%% TODO: have a spec for list of transactions
-spec txs(micro_block()) -> list(aetx_sign:signed_tx()).
txs(Block) ->
    Block#mic_block.txs.

-spec set_txs(micro_block(), list(aetx_sign:signed_tx())) -> micro_block().
set_txs(Block, Txs) ->
    Block#mic_block{txs = Txs}.

-spec txs_hash(micro_block()) -> binary().
txs_hash(Block) ->
    Block#mic_block.txs_hash.

-spec new_key(height(), block_header_hash(), state_hash(), aec_pow:sci_int(),
              non_neg_integer(), non_neg_integer(), non_neg_integer(),
              miner_pubkey(), beneficiary_pubkey()) -> key_block().
new_key(Height, PrevHash, RootHash, Target, Nonce, Time, Version, Miner, Beneficiary) ->
    new_key_with_evidence(Height, PrevHash, RootHash, Target, Nonce,
                          Time, Version, Miner, Beneficiary, no_value).

-spec new_key_with_evidence(height(), block_header_hash(), state_hash(), aec_pow:sci_int(),
              non_neg_integer(), non_neg_integer(), non_neg_integer(),
              miner_pubkey(), beneficiary_pubkey(), aec_pow:pow_evidence()) -> key_block().
new_key_with_evidence(Height, PrevHash, RootHash, Target, Nonce, Time, Version, Miner, Beneficiary, Evd) ->
    #key_block{ height       = Height
              , prev_hash    = PrevHash
              , root_hash    = RootHash
              , target       = Target
              , nonce        = Nonce
              , pow_evidence = Evd
              , time         = Time
              , version      = Version
              , miner        = Miner
              , beneficiary  = Beneficiary }.

-spec new_micro(height(), block_header_hash(), state_hash(), txs_hash(),
                list(aetx_sign:signed_tx()), non_neg_integer(),
                non_neg_integer()) -> micro_block().
new_micro(Height, PrevHash, RootHash, TxsHash, Txs, Time, Version) ->
    #mic_block{ height    = Height
              , prev_hash = PrevHash
              , root_hash = RootHash
              , txs_hash  = TxsHash
              , txs       = Txs
              , time      = Time
              , version   = Version }.

-spec new_signed_micro(height(), block_header_hash(), state_hash(), txs_hash(),
                list(aetx_sign:signed_tx()), non_neg_integer(), non_neg_integer(),
                binary()) -> micro_block().
new_signed_micro(Height, PrevHash, RootHash, TxsHash, Txs, Time, Version, Signature) ->
    #mic_block{ height    = Height
              , prev_hash = PrevHash
              , root_hash = RootHash
              , signature = Signature
              , txs_hash  = TxsHash
              , txs       = Txs
              , time      = Time
              , version   = Version }.

-spec update_micro_candidate(micro_block(), txs_hash(), state_hash(),
                             [aetx_sign:signed_tx()], non_neg_integer()
                            ) -> micro_block().
update_micro_candidate(#mic_block{} = Block, TxsRootHash, RootHash, Txs, TimeMSecs) ->
    Block#mic_block{ root_hash = RootHash
                   , txs_hash  = TxsRootHash
                   , txs       = Txs
                   , time      = TimeMSecs}.

-spec to_header(block()) -> aec_headers:header().
to_header(#key_block{height = Height,
                     prev_hash = PrevHash,
                     root_hash = RootHash,
                     miner = Miner,
                     beneficiary = Beneficiary,
                     target = Target,
                     pow_evidence = Evd,
                     nonce = Nonce,
                     time = Time,
                     version = Version}) ->
    aec_headers:new_key_header(Height, PrevHash, RootHash, Miner, Beneficiary, Target,
                               Evd, Nonce, Time, Version);
to_header(#mic_block{height = Height,
                     prev_hash = PrevHash,
                     root_hash = RootHash,
                     txs_hash = TxsHash,
                     time = Time,
                     version = Version}) ->
    aec_headers:new_micro_header(Height, PrevHash, RootHash, Time, TxsHash, Version).

-spec serialize_to_binary(block()) -> binary().
serialize_to_binary(#key_block{} = Block) ->
    Hdr = aec_headers:serialize_to_binary(to_header(Block)),
    Vsn = version(Block),
    {ok, Template} = serialization_template(key, Vsn),
    aec_object_serialization:serialize(
      key_block,
      Vsn,
      Template,
      [{header, Hdr}]);
serialize_to_binary(#mic_block{} = Block) ->
    Hdr = aec_headers:serialize_to_binary(to_header(Block)),
    Txs = [ aetx_sign:serialize_to_binary(Tx) || Tx <- txs(Block)],
    Vsn = version(Block),
    {ok, Template} = serialization_template(micro, Vsn),
    aec_object_serialization:serialize(
      micro_block,
      Vsn,
      Template,
      [{header, Hdr}, {txs, Txs}, {signature, signature(Block)}]).

-spec deserialize_from_binary(binary()) -> {'error', term()} | {'ok', block()}.
deserialize_from_binary(Bin) ->
    case aec_object_serialization:deserialize_type_and_vsn(Bin) of
        {key_block, Vsn, _RawFields} ->
            case serialization_template(key, Vsn) of
                {ok, Template} ->
                    [{header, Hdr0}] =
                        aec_object_serialization:deserialize(key_block, Vsn, Template, Bin),
                    Hdr = aec_headers:deserialize_from_binary(Hdr0),
                    {ok, aec_headers:to_key_block(Hdr)};
                Err = {error, _} ->
                    Err
            end;
        {micro_block, Vsn, _RawFields} ->
            case serialization_template(micro, Vsn) of
                {ok, Template} ->
                    [{header, Hdr0}, {txs, Txs0}, {signature, Sig}] =
                        aec_object_serialization:deserialize(micro_block, Vsn, Template, Bin),
                    Hdr = aec_headers:deserialize_from_binary(Hdr0),
                    Txs = [ aetx_sign:deserialize_from_binary(Tx) || Tx <- Txs0 ],
                    {ok, aec_headers:to_micro_block(Hdr, Txs, Sig)};
                Err = {error, _} ->
                    Err
            end
    end.

serialization_template(key, Vsn) when Vsn >= ?GENESIS_VERSION andalso Vsn =< ?PROTOCOL_VERSION ->
    {ok, [{header, binary}]};
serialization_template(micro, Vsn) when Vsn >= ?GENESIS_VERSION andalso Vsn =< ?PROTOCOL_VERSION ->
    {ok, [{header, binary}, {txs, [binary]}, {signature, binary}]};
serialization_template(_BlockType, Vsn) ->
    {error, {bad_block_vsn, Vsn}}.

-spec serialize_to_map(block()) -> map().
serialize_to_map(#key_block{} = Block) ->
    #{<<"height">> => Block#key_block.height,
      <<"prev_hash">> => Block#key_block.prev_hash,
      <<"state_hash">> => Block#key_block.root_hash,
      <<"miner">> => Block#key_block.miner,
      <<"beneficiary">> => Block#key_block.beneficiary,
      <<"target">> => Block#key_block.target,
      <<"pow">> => Block#key_block.pow_evidence,
      <<"nonce">> => Block#key_block.nonce,
      <<"time">> => Block#key_block.time,
      <<"version">> => Block#key_block.version
     };
serialize_to_map(#mic_block{} = Block) ->
    #{<<"height">> => Block#mic_block.height,
      <<"prev_hash">> => Block#mic_block.prev_hash,
      <<"state_hash">> => Block#mic_block.root_hash,
      <<"txs_hash">> => Block#mic_block.txs_hash,
      <<"time">> => Block#mic_block.time,
      <<"version">> => Block#mic_block.version,
      <<"transactions">> => Block#mic_block.txs,
      <<"signature">> => Block#mic_block.signature
     }.

-spec deserialize_from_map(map()) -> {'error', term()} | {'ok', block()}.
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
    case Nonce of
    %% Prevent forging a solution without performing actual work by prefixing digits
    %% to a valid nonce (produces valid PoW after truncating to the allowed range)
        N when N < 0; N > ?MAX_NONCE ->
            {error, bad_nonce};
        _ ->
            {ok, #key_block{
                    height = Height,
                    prev_hash = PrevHash,
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
                       <<"state_hash">> := RootHash,
                       <<"txs_hash">> := TxsHash,
                       <<"time">> := Time,
                       <<"version">> := Version,
                       <<"transactions">> := Txs,
                       <<"signature">> := Signature}) ->
    {ok, #mic_block{
            height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            txs_hash = TxsHash,
            signature = Signature,
            time = Time,
            version = Version,
            txs = Txs}}.

-spec hash_internal_representation(block()) -> {ok, block_header_hash()}.
hash_internal_representation(B) ->
    aec_headers:hash_header(to_header(B)).


-spec validate_key_block(key_block()) -> 'ok' | {'error', {'header', term()}}.
validate_key_block(#key_block{} = Block) ->
    case aec_headers:validate_key_block_header(to_header(Block)) of
        ok -> ok;
        {error, Reason} -> {error, {header, Reason}}
    end.

-spec validate_micro_block_no_signature(micro_block()) ->
                                           'ok' | {'error', {'header' | 'block', term()}}.
validate_micro_block_no_signature(#mic_block{} = Block) ->
    Validators = [fun validate_txs_hash/1],
    validate_micro_block(Block, undefined, Validators).

-spec validate_micro_block(micro_block(), aec_keys:pubkey()) ->
                              'ok' | {'error', {'header' | 'block', term()}}.
validate_micro_block(#mic_block{} = Block, LeaderKey) ->
    Validators = [fun validate_txs_hash/1, fun validate_signature/1],
    validate_micro_block(Block, LeaderKey, Validators).

validate_micro_block(Block, LeaderKey, Validators) ->
    % since trees are required for transaction signature validation, this is
    % performed while applying transactions
    case aec_headers:validate_micro_block_header(to_header(Block)) of
        ok ->
            case aeu_validation:run(Validators, [{Block, LeaderKey}]) of
                ok              -> ok;
                {error, Reason} -> {error, {block, Reason}}
            end;
        {error, Reason} ->
            {error, {header, Reason}}
    end.

-spec validate_signature({block(), binary()}) -> ok | {error, signature_verification_failed}.
validate_signature({#mic_block{signature = Sig} = Block, LeaderKey}) ->
    Bin = aec_headers:serialize_to_binary(to_header(Block)),
    case enacl:sign_verify_detached(Sig, Bin, LeaderKey) of
        {ok, _}    -> ok;
        {error, _} -> {error, signature_verification_failed}
    end.

-spec validate_txs_hash({block(), binary()}) -> ok | {error, malformed_txs_hash}.
validate_txs_hash({#mic_block{txs = Txs, txs_hash = BlockTxsHash}, _}) ->
    case aec_txs_trees:pad_empty(aec_txs_trees:root_hash(aec_txs_trees:from_txs(Txs))) of
        BlockTxsHash ->
            ok;
        _Other ->
            {error, malformed_txs_hash}
    end.

-spec type(block()) -> block_type().
type(#key_block{}) -> 'key';
type(#mic_block{}) -> 'micro'.
