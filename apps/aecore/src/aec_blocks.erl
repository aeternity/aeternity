%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_blocks).

%% API
-export([beneficiary/1,
         prev_hash/1,
         height/1,
         miner/1,
         target/1,
         txs/1,
         txs_hash/1,
         difficulty/1,
         is_key_block/1,
         time_in_msecs/1,
         pow/1,
         set_pow/3,
         signature/1,
         set_signature/2,
         key_hash/1,
         set_target/2,
         new_key/9,
         new_micro/8,
         from_header_txs_and_signature/3,
         to_header/1,
         serialize_to_binary/1,
         serialize_to_map/1,
         deserialize_from_binary/1,
         deserialize_from_map/1,
         hash_internal_representation/1,
         root_hash/1,
         version/1,
         validate_key_block/1,
         validate_micro_block/2,
         validate_micro_block_no_signature/1,
         type/1]).

-import(aec_hard_forks, [protocol_effective_at_height/1]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("blocks.hrl").

%% block() can't be opaque since aec_block_genesis also needs to
%% be able to handle the raw #block{} record - TODO: change this
-type block() :: #block{}.
-type height() :: non_neg_integer().
-export_type([block/0, block_header_hash/0, height/0]).

-spec beneficiary(block()) -> aec_keys:pubkey().
beneficiary(Block) ->
    Block#block.beneficiary.

-spec prev_hash(block()) -> block_header_hash().
prev_hash(Block) ->
    Block#block.prev_hash.

-spec height(block()) -> height().
height(Block) ->
    Block#block.height.

-spec target(block()) -> integer().
target(Block) ->
    Block#block.target.

-spec difficulty(block()) -> float().
difficulty(Block) ->
    aec_pow:target_to_difficulty(target(Block)).

-spec is_key_block(block()) -> boolean().
is_key_block(#block{miner = Miner, height = Height}) ->
    Miner =/= <<0:?MINER_PUB_BYTES/unit:8>>
        orelse (Miner =:= <<0:?MINER_PUB_BYTES/unit:8>> andalso Height =:= aec_block_genesis:height()).


time_in_msecs(Block) ->
    Block#block.time.

-spec root_hash(block()) -> binary().
root_hash(Block) ->
    Block#block.root_hash.

-spec miner(block()) -> aec_keys:pubkey().
miner(Block) ->
    Block#block.miner.

-spec version(block()) -> non_neg_integer().
version(Block) ->
    Block#block.version.

%% Sets the evidence of PoW,too,  for Cuckoo Cycle
-spec set_pow(block(), aec_pow:nonce(), aec_pow:pow_evidence()) -> block().
set_pow(Block, Nonce, Evd) ->
    Block#block{nonce = Nonce,
                pow_evidence = Evd}.

-spec pow(block()) -> aec_pow:pow_evidence().
pow(Block) ->
    Block#block.pow_evidence.

%% Sets the signature for microblock
-spec set_signature(block(), binary()) -> block().
set_signature(Block, Signature) ->
    Block#block{signature = Signature}.

-spec key_hash(block()) -> binary().
key_hash(Block) ->
    Block#block.key_hash.

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

-spec new_micro(height(), block_header_hash(), block_header_hash(), state_hash(), txs_hash(),
                list(aetx_sign:signed_tx()), non_neg_integer(), non_neg_integer()) -> block().
new_micro(Height, PrevHash, KeyBlockHash, RootHash, TxsHash, Txs, Time, Version) ->
    #block{ height    = Height
          , prev_hash = PrevHash
          , key_hash  = KeyBlockHash
          , root_hash = RootHash
          , txs_hash  = TxsHash
          , txs       = Txs
          , time      = Time
          , version   = Version }.

-spec to_header(block()) -> aec_headers:header().
to_header(#block{} = Block) ->
    to_header(type(Block), Block).

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
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            miner = Miner,
            beneficiary = Beneficiary,
            target = Target,
            pow_evidence = Evd,
            nonce = Nonce,
            time = Time,
            version = Version};
to_header(micro, #block{height = Height,
                        prev_hash = PrevHash,
                        root_hash = RootHash,
                        txs_hash = TxsHash,
                        key_hash = KeyHash,
                        time = Time,
                        version = Version}) ->
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            txs_hash = TxsHash,
            key_hash = KeyHash,
            time = Time,
            version = Version}.

from_header_txs_and_signature(#header{} = Header, Txs, Signature) ->
    from_header_txs_and_signature(aec_headers:type(Header), Header, Txs, Signature).

from_header_txs_and_signature(key, #header{height = Height,
                                           prev_hash = PrevHash,
                                           root_hash = RootHash,
                                           miner = Miner,
                                           beneficiary = Beneficiary,
                                           target = Target,
                                           pow_evidence = Evd,
                                           nonce = Nonce,
                                           time = Time,
                                           version = Version}, _Txs, _Signature) ->
    #block{height = Height,
           prev_hash = PrevHash,
           root_hash = RootHash,
           miner = Miner,
           beneficiary = Beneficiary,
           target = Target,
           pow_evidence = Evd,
           nonce = Nonce,
           time = Time,
           version = Version};
from_header_txs_and_signature(micro, #header{height = Height,
                                             prev_hash = PrevHash,
                                             root_hash = RootHash,
                                             txs_hash = TxsHash,
                                             key_hash = KeyHash,
                                             time = Time,
                                             version = Version}, Txs, Signature) ->
    #block{height = Height,
           prev_hash = PrevHash,
           root_hash = RootHash,
           txs_hash = TxsHash,
           key_hash = KeyHash,
           signature = Signature,
           time = Time,
           version = Version,
           txs = Txs}.

serialize_to_binary(#block{} = Block) ->
    serialize_to_binary(type(Block), Block).

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

serialize_to_map(#block{} = Block) ->
    serialize_to_map(type(Block), Block).

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
      <<"key_hash">> => Block#block.key_hash,
      <<"time">> => Block#block.time,
      <<"version">> => Block#block.version,
      <<"transactions">> => Block#block.txs,
      <<"signature">> => Block#block.signature
     }.

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
                       <<"key_hash">> := KeyHash,
                       <<"time">> := Time,
                       <<"version">> := Version,
                       <<"transactions">> := Txs,
                       <<"signature">> := Signature}) ->
    {ok, #block{
            height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            txs_hash = TxsHash,
            key_hash = KeyHash,
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

type(Block = #block{}) ->
    case is_key_block(Block) of
        true  -> key;
        false -> micro
    end.
