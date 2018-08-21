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
         from_header_txs_and_signature/3,
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

-record(block, {
          height = 0              :: aec_blocks:height(),
          prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> :: block_header_hash(),
          root_hash = <<0:?STATE_HASH_BYTES/unit:8>> :: state_hash(), % Hash of all state Merkle trees
          txs_hash = <<0:?TXS_HASH_BYTES/unit:8>> :: txs_hash(),
          txs = []                :: list(aetx_sign:signed_tx()),
          target = ?HIGHEST_TARGET_SCI :: aec_pow:sci_int(),
          nonce = 0               :: non_neg_integer(),
          time = ?GENESIS_TIME    :: non_neg_integer(),
          version                 :: non_neg_integer(),
          pow_evidence = no_value :: aec_pow:pow_evidence(),
          miner = <<0:?MINER_PUB_BYTES/unit:8>> :: miner_pubkey(),
          signature = undefined   :: binary() | undefined,
          beneficiary = <<0:?BENEFICIARY_PUB_BYTES/unit:8>> :: beneficiary_pubkey()}).

-opaque block() :: #block{}.
-type height() :: non_neg_integer().
-export_type([block/0, block_header_hash/0, height/0]).

-ifdef(TEST).

-export([raw_block/0]).
raw_block() ->
    #block{version = ?PROTOCOL_VERSION}.

-endif. %% TEST


-spec beneficiary(block()) -> aec_keys:pubkey().
beneficiary(Block) ->
    Block#block.beneficiary.

-spec prev_hash(block()) -> block_header_hash().
prev_hash(Block) ->
    Block#block.prev_hash.

-spec set_prev_hash(block(), block_header_hash()) -> block().
set_prev_hash(Block, PrevHash) ->
    Block#block{prev_hash = PrevHash}.

-spec height(block()) -> height().
height(Block) ->
    Block#block.height.

-spec set_height(block(), height()) -> block().
set_height(Block, Height) ->
    Block#block{height = Height}.

-spec target(block()) -> integer().
target(Block) ->
    Block#block.target.

-spec difficulty(block()) -> float().
difficulty(Block) ->
    aec_pow:target_to_difficulty(target(Block)).

-spec assert_block(block()) -> ok.
assert_block(#block{}) -> ok;
assert_block(Other) -> error({illegal_block, Other}).

-spec is_block(term()) -> boolean().
is_block(#block{}) -> true;
is_block(_       ) -> false.

-spec is_key_block(block()) -> boolean().
is_key_block(#block{miner = Miner, height = Height}) ->
    Miner =/= <<0:?MINER_PUB_BYTES/unit:8>>
        orelse (Miner =:= <<0:?MINER_PUB_BYTES/unit:8>> andalso Height =:= aec_block_genesis:height()).

-spec time_in_msecs(block()) -> non_neg_integer().
time_in_msecs(Block) ->
    Block#block.time.

-spec set_time_in_msecs(block(), non_neg_integer()) -> block().
set_time_in_msecs(Block, Time) ->
    Block#block{ time = Time }.

-spec root_hash(block()) -> binary().
root_hash(Block) ->
    Block#block.root_hash.

-spec set_root_hash(block(), binary()) -> block().
set_root_hash(Block, RootHash) ->
    Block#block{root_hash = RootHash}.

-spec miner(block()) -> aec_keys:pubkey().
miner(Block) ->
    Block#block.miner.

-spec set_miner(block(), aec_keys:pubkey()) -> block().
set_miner(Block, Miner) ->
    Block#block{miner = Miner}.

-spec version(block()) -> non_neg_integer().
version(Block) ->
    Block#block.version.

%% Sets the evidence of PoW,too,  for Cuckoo Cycle
-spec set_pow(block(), aec_pow:nonce(), aec_pow:pow_evidence()) -> block().
set_pow(Block, Nonce, Evd) ->
    Block#block{nonce = Nonce,
                pow_evidence = Evd}.

-spec set_nonce(block(), aec_pow:nonce()) -> block().
set_nonce(Block, Nonce) ->
    Block#block{nonce = Nonce}.

-spec pow(block()) -> aec_pow:pow_evidence().
pow(Block) ->
    Block#block.pow_evidence.

%% Sets the signature for microblock
-spec set_signature(block(), binary()) -> block().
set_signature(Block, Signature) ->
    Block#block{signature = Signature}.

-spec signature(block()) -> binary() | undefined.
signature(Block) ->
    Block#block.signature.

-spec set_target(block(), non_neg_integer()) -> block().
set_target(Block, Target) ->
    Block#block{target = Target}.

%% TODO: have a spec for list of transactions
-spec txs(block()) -> list(aetx_sign:signed_tx()).
txs(Block) ->
    Block#block.txs.

-spec set_txs(block(), list(aetx_sign:signed_tx())) -> block().
set_txs(Block, Txs) ->
    Block#block{txs = Txs}.


-spec txs_hash(block()) -> binary().
txs_hash(Block) ->
    Block#block.txs_hash.

-spec new_key(height(), block_header_hash(), state_hash(), aec_pow:sci_int(),
              non_neg_integer(), non_neg_integer(), non_neg_integer(),
              miner_pubkey(), beneficiary_pubkey()) -> block().
new_key(Height, PrevHash, RootHash, Target, Nonce, Time, Version, Miner, Beneficiary) ->
    #block{ height      = Height
          , prev_hash   = PrevHash
          , root_hash   = RootHash
          , target      = Target
          , nonce       = Nonce
          , time        = Time
          , version     = Version
          , miner       = Miner
          , beneficiary = Beneficiary }.

-spec new_key_with_evidence(height(), block_header_hash(), state_hash(), aec_pow:sci_int(),
              non_neg_integer(), non_neg_integer(), non_neg_integer(),
              miner_pubkey(), beneficiary_pubkey(), aec_pow:pow_evidence()) -> block().
new_key_with_evidence(Height, PrevHash, RootHash, Target, Nonce, Time, Version, Miner, Beneficiary, Evd) ->
    #block{ height       = Height
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
                list(aetx_sign:signed_tx()), non_neg_integer(), non_neg_integer()) -> block().
new_micro(Height, PrevHash, RootHash, TxsHash, Txs, Time, Version) ->
    #block{ height    = Height
          , prev_hash = PrevHash
          , root_hash = RootHash
          , txs_hash  = TxsHash
          , txs       = Txs
          , time      = Time
          , version   = Version }.

-spec new_signed_micro(height(), block_header_hash(), state_hash(), txs_hash(),
                list(aetx_sign:signed_tx()), non_neg_integer(), non_neg_integer(),
                binary()) -> block().
new_signed_micro(Height, PrevHash, RootHash, TxsHash, Txs, Time, Version, Signature) ->
    #block{ height    = Height
          , prev_hash = PrevHash
          , root_hash = RootHash
          , signature = Signature
          , txs_hash  = TxsHash
          , txs       = Txs
          , time      = Time
          , version   = Version }.

-spec update_micro_candidate(block(), txs_hash(), state_hash(),
                             [aetx_sign:signed_tx()], non_neg_integer()) -> block().
update_micro_candidate(#block{} = Block, TxsRootHash, RootHash, Txs, TimeMSecs) ->
    Block#block{ root_hash = RootHash
               , txs_hash  = TxsRootHash
               , txs       = Txs
               , time      = TimeMSecs}.

-spec to_header(block()) -> aec_headers:header().
to_header(#block{} = Block) ->
    to_header(type(Block), Block).

-spec to_header(block_type(), block()) -> aec_headers:header().
to_header(key, #block{height = Height,
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
to_header(micro, #block{height = Height,
                        prev_hash = PrevHash,
                        root_hash = RootHash,
                        txs_hash = TxsHash,
                        time = Time,
                        version = Version}) ->
    aec_headers:new_micro_header(Height, PrevHash, RootHash, Time, TxsHash, Version).

from_header_txs_and_signature(Header, Txs, Signature) ->
    from_header_txs_and_signature(aec_headers:type(Header), Header, Txs, Signature).

from_header_txs_and_signature(key, Header,_Txs,_Signature) ->
    aec_headers:to_key_block(Header);
from_header_txs_and_signature(micro, Header, Txs, Signature) ->
    aec_headers:to_micro_block(Header, Txs, Signature).

-spec serialize_to_binary(block()) -> binary().
serialize_to_binary(#block{} = Block) ->
    serialize_to_binary(type(Block), Block).

-spec serialize_to_binary(block_type(), block()) -> binary().
serialize_to_binary(key, Block) ->
    Hdr = aec_headers:serialize_to_binary(to_header(Block)),
    Vsn = Block#block.version,
    {ok, Template} = serialization_template(key, Vsn),
    aec_object_serialization:serialize(
      key_block,
      Vsn,
      Template,
      [{header, Hdr}]);
serialize_to_binary(micro, Block) ->
    Hdr = aec_headers:serialize_to_binary(to_header(Block)),
    Txs = [ aetx_sign:serialize_to_binary(Tx) || Tx <- Block#block.txs ],
    Vsn = Block#block.version,
    {ok, Template} = serialization_template(micro, Vsn),
    aec_object_serialization:serialize(
      micro_block,
      Vsn,
      Template,
      [{header, Hdr}, {txs, Txs}, {signature, Block#block.signature}]).

-spec deserialize_from_binary(binary()) -> {'error', term()} | {'ok', block()}.
deserialize_from_binary(Bin) ->
    case aec_object_serialization:deserialize_type_and_vsn(Bin) of
        {key_block, Vsn, _RawFields} ->
            case serialization_template(key, Vsn) of
                {ok, Template} ->
                    [{header, Hdr0}] =
                        aec_object_serialization:deserialize(key_block, Vsn, Template, Bin),
                    Hdr = aec_headers:deserialize_from_binary(Hdr0),
                    {ok, from_header_txs_and_signature(Hdr, [], <<>>)};
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
                    {ok, from_header_txs_and_signature(Hdr, Txs, Sig)};
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
serialize_to_map(#block{} = Block) ->
    serialize_to_map(type(Block), Block).

-spec serialize_to_map(block_type(), block()) -> map().
serialize_to_map(key, Block) ->
    #{<<"height">> => Block#block.height,
      <<"prev_hash">> => Block#block.prev_hash,
      <<"state_hash">> => Block#block.root_hash,
      <<"miner">> => Block#block.miner,
      <<"beneficiary">> => Block#block.beneficiary,
      <<"target">> => Block#block.target,
      <<"pow">> => Block#block.pow_evidence,
      <<"nonce">> => Block#block.nonce,
      <<"time">> => Block#block.time,
      <<"version">> => Block#block.version
     };
serialize_to_map(micro, Block) ->
    #{<<"height">> => Block#block.height,
      <<"prev_hash">> => Block#block.prev_hash,
      <<"state_hash">> => Block#block.root_hash,
      <<"txs_hash">> => Block#block.txs_hash,
      <<"time">> => Block#block.time,
      <<"version">> => Block#block.version,
      <<"transactions">> => Block#block.txs,
      <<"signature">> => Block#block.signature
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
            {ok, #block{
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
    {ok, #block{
            height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            txs_hash = TxsHash,
            signature = Signature,
            time = Time,
            version = Version,
            txs = Txs}}.

-spec hash_internal_representation(block()) -> {ok, block_header_hash()}.
hash_internal_representation(B = #block{}) ->
    aec_headers:hash_header(to_header(B)).


validate_key_block(Block) ->
    case aec_headers:validate_key_block_header(to_header(Block)) of
        ok ->
            Validators = [fun validate_no_txs/1,
                          fun validate_empty_txs_hash/1],
            case aeu_validation:run(Validators, [Block]) of
                ok              -> ok;
                {error, Reason} -> {error, {block, Reason}}
            end;
        {error, Reason} ->
            {error, {header, Reason}}
    end.

validate_no_txs(#block{txs = Txs}) ->
    case Txs =:= [] of
        true  -> ok;
        false -> {error, txs_in_key_block}
    end.

validate_empty_txs_hash(#block{txs_hash = TxsHash}) ->
    case TxsHash =:= <<0:?TXS_HASH_BYTES/unit:8>> of
        true  -> ok;
        false -> {error, wrong_txs_hash}
    end.

validate_micro_block_no_signature(Block) ->
    Validators = [fun validate_txs_hash/1],
    validate_micro_block(Block, undefined, Validators).

validate_micro_block(Block, LeaderKey) ->
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
validate_signature({#block{signature = Sig} = Block, LeaderKey}) ->
    Bin = aec_headers:serialize_to_binary(to_header(Block)),
    case enacl:sign_verify_detached(Sig, Bin, LeaderKey) of
        {ok, _}    -> ok;
        {error, _} -> {error, signature_verification_failed}
    end.

-spec validate_txs_hash({block(), binary()}) -> ok | {error, malformed_txs_hash}.
validate_txs_hash({#block{txs = Txs, txs_hash = BlockTxsHash}, _}) ->
    case aec_txs_trees:pad_empty(aec_txs_trees:root_hash(aec_txs_trees:from_txs(Txs))) of
        BlockTxsHash ->
            ok;
        _Other ->
            {error, malformed_txs_hash}
    end.

-spec type(block()) -> block_type().
type(Block = #block{}) ->
    case is_key_block(Block) of
        true  -> key;
        false -> micro
    end.
