%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_blocks).

%% API
-export([prev_hash/1,
         height/1,
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
         miner_hash/1,
         set_target/2,
         new_key/4,
         new_micro/5,
         new_key_with_state/4,
         new_with_state/5,
         from_header_and_txs/2,
         to_header/1,
         serialize_to_binary/1,
         serialize_to_map/1,
         deserialize_from_binary/1,
         deserialize_from_map/1,
         hash_internal_representation/1,
         root_hash/1,
         validate_key_block/1,
         validate_micro_block/2,
         type/1]).
-import(aec_hard_forks, [protocol_effective_at_height/1]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("common.hrl").
-include("blocks.hrl").

%% block() can't be opaque since aec_block_genesis also needs to
%% be able to handle the raw #block{} record - TODO: change this
-type block() :: #block{}.
-export_type([block/0]).

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
is_key_block(Block) ->
    Block#block.miner =/= undefined.


time_in_msecs(Block) ->
    Block#block.time.

-spec root_hash(block()) -> binary().
root_hash(Block) ->
    Block#block.root_hash.

%% Sets the evidence of PoW,too,  for Cuckoo Cycle
-spec set_pow(block(), aec_pow:nonce(), aec_pow:pow_evidence()) -> block().
set_pow(Block, Nonce, Evd) ->
    Block#block{nonce = Nonce,
                pow_evidence = Evd}.

-spec pow(block()) -> aec_pow:pow_evidence().
pow(Block) ->
    Block#block.pow_evidence.

%% Sets the signature for microblock
-spec set_signature(block(), list()) -> block().
set_signature(Block, Signature) ->
    Block#block{signature = Signature}.

-spec miner_hash(block()) -> binary().
miner_hash(Block) ->
    Block#block.miner_hash.

-spec signature(block()) -> binary().
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


-spec new_micro(block(), block(), pubkey(), list(aetx_sign:signed_tx()), aec_trees:trees()) -> block().
new_micro(LastBlock, CurrentKeyBlock, Miner, Txs, Trees0) ->
    {B, _} = new_with_state(LastBlock, CurrentKeyBlock, Miner, Txs, Trees0),
    B.

-spec new_key(block(), block(), pubkey(), aec_trees:trees()) -> block().
new_key(LastBlock, CurrentKeyBlock, Miner, Trees0) ->
    {B, _} = new_key_with_state(LastBlock, CurrentKeyBlock, Miner, Trees0),
    B.

-spec new_with_state(block(), block(), pubkey(), list(aetx_sign:signed_tx()), aec_trees:trees()) ->
                            {block(), aec_trees:trees()}.
new_with_state(LastBlock, CurrentKeyBlock, Miner, Txs, Trees0) ->
    {ok, LastBlockHeaderHash} = hash_internal_representation(LastBlock),

    LastBlockHeight = height(CurrentKeyBlock),
    Version = protocol_effective_at_height(LastBlockHeight),

    {ok, Txs1, Trees} = aec_trees:apply_signed_txs(Miner, Txs, Trees0, LastBlockHeight, Version),
    {ok, TxsRootHash} = aec_txs_trees:root_hash(aec_txs_trees:from_txs(Txs1)),

    NewBlock =
        #block{height = LastBlockHeight,
               miner_hash = miner_hash(CurrentKeyBlock),
               prev_hash = LastBlockHeaderHash,
               root_hash = aec_trees:hash(Trees),
               txs_hash = TxsRootHash,
               txs = Txs1,
               target = target(LastBlock),
               time = aeu_time:now_in_msecs(),
               version = Version,
               miner = Miner},
    {NewBlock, Trees}.

new_key_with_state(LastBlock, CurrentKeyBlock, Miner, Trees0) ->

    {IncompleteBlock, Trees} = new_with_state(LastBlock, CurrentKeyBlock, Miner, [], Trees0),
    {ok, KeyToClaimLeader} = aec_keys:pubkey(),

    %% Assert correctness of last block protocol version, as minimum
    %% sanity check on previous block and state (mainly for potential
    %% stale state persisted in DB and for development testing).
    ExpectedLastBlockVersion = protocol_effective_at_height(aec_blocks:height(CurrentKeyBlock)),
    {ExpectedLastBlockVersion, _} = {LastBlock#block.version, {expected, ExpectedLastBlockVersion}},

    LastBlockHeight = height(CurrentKeyBlock),
    NewHeight = LastBlockHeight + 1,
    Version = protocol_effective_at_height(NewHeight),

    {IncompleteBlock#block{height = NewHeight, version = Version, miner = KeyToClaimLeader, miner_hash = undefined}, Trees}.

-spec to_header(block()) -> aec_headers:header().
to_header(#block{} = B) ->
    to_header(type(B), B).

to_header(key, #block{height = Height,
                      prev_hash = PrevHash,
                      root_hash = RootHash,
                      miner = Miner,
                      target = Target,
                      pow_evidence = Evd,
                      nonce = Nonce,
                      time = Time,
                      version = Version}) ->
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            miner = Miner,
            target = Target,
            pow_evidence = Evd,
            nonce = Nonce,
            time = Time,
            version = Version};
to_header(micro, #block{height = Height,
                        prev_hash = PrevHash,
                        root_hash = RootHash,
                        txs_hash = TxsHash,
                        miner_hash = MinerHash,
                        signature = Signature,
                        time = Time,
                        version = Version}) ->
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            txs_hash = TxsHash,
            miner_hash = MinerHash,
            signature = Signature,
            time = Time,
            version = Version}.

from_header_and_txs(#header{} = H, Txs) ->
    from_header_and_txs(aec_headers:type(H), H, Txs).

from_header_and_txs(key, #header{height = Height,
                                 prev_hash = PrevHash,
                                 root_hash = RootHash,
                                 miner = Miner,
                                 target = Target,
                                 pow_evidence = Evd,
                                 nonce = Nonce,
                                 time = Time,
                                 version = Version}, _Txs) ->
    #block{height = Height,
           prev_hash = PrevHash,
           root_hash = RootHash,
           miner = Miner,
           target = Target,
           pow_evidence = Evd,
           nonce = Nonce,
           time = Time,
           version = Version};
from_header_and_txs(micro, #header{height = Height,
                                   prev_hash = PrevHash,
                                   root_hash = RootHash,
                                   txs_hash = TxsHash,
                                   miner_hash = MinerHash,
                                   signature = Signature,
                                   time = Time,
                                   version = Version}, Txs) ->
    #block{height = Height,
           prev_hash = PrevHash,
           root_hash = RootHash,
           txs_hash = TxsHash,
           miner_hash = MinerHash,
           signature = Signature,
           time = Time,
           version = Version,
           txs = Txs}.

serialize_to_binary(B = #block{}) ->
    serialize_to_binary(type(B), B).

serialize_to_binary(key, B) ->
    Hdr = aec_headers:serialize_to_binary(to_header(B)),
    Vsn = B#block.version,
    {ok, Template} = serialization_template(key, Vsn),
    aec_object_serialization:serialize(
      key_block,
      Vsn,
      Template,
      [{header, Hdr}]);
serialize_to_binary(micro, B) ->
    Hdr = aec_headers:serialize_to_binary(to_header(B)),
    Txs = [ aetx_sign:serialize_to_binary(Tx) || Tx <- B#block.txs ],
    Vsn = B#block.version,
    {ok, Template} = serialization_template(micro, Vsn),
    aec_object_serialization:serialize(
      micro_block,
      Vsn,
      Template,
      [{header, Hdr}, {txs, Txs}]).

deserialize_from_binary(Bin) ->
    case aec_object_serialization:deserialize_type_and_vsn(Bin) of
        {key_block, Vsn, _RawFields} ->
            case serialization_template(key, Vsn) of
                {ok, Template} ->
                    [{header, Hdr0}] =
                        aec_object_serialization:deserialize(key_block, Vsn, Template, Bin),
                    Hdr = aec_headers:deserialize_from_binary(Hdr0),
                    {ok, from_header_and_txs(Hdr, [])};
                Err = {error, _} ->
                    Err
            end;
        {micro_block, Vsn, _RawFields} ->
            case serialization_template(micro, Vsn) of
                {ok, Template} ->
                    [{header, Hdr0}, {txs, Txs0}] =
                        aec_object_serialization:deserialize(micro_block, Vsn, Template, Bin),
                    Hdr = aec_headers:deserialize_from_binary(Hdr0),
                    Txs = [ aetx_sign:deserialize_from_binary(Tx) || Tx <- Txs0 ],
                    {ok, from_header_and_txs(Hdr, Txs)};
                Err = {error, _} ->
                    Err
            end
    end.

serialization_template(key, Vsn) when Vsn >= ?GENESIS_VERSION andalso Vsn =< ?PROTOCOL_VERSION ->
    {ok, [{header, binary}]};
serialization_template(micro, Vsn) when Vsn >= ?GENESIS_VERSION andalso Vsn =< ?PROTOCOL_VERSION ->
    {ok, [{header, binary}, {txs, [binary]}]};
serialization_template(_BlockType, Vsn) ->
    {error, {bad_block_vsn, Vsn}}.

serialize_to_map(B = #block{}) ->
    serialize_to_map(type(B), B).

serialize_to_map(key, B) ->
    #{<<"height">> => B#block.height,
      <<"prev_hash">> => B#block.prev_hash,
      <<"state_hash">> => B#block.root_hash,
      <<"miner">> => B#block.miner,
      <<"target">> => B#block.target,
      <<"pow">> => B#block.pow_evidence,
      <<"nonce">> => B#block.nonce,
      <<"time">> => B#block.time,
      <<"version">> => B#block.version
     };
serialize_to_map(micro, B) ->
    #{<<"prev_hash">> => B#block.prev_hash,
      <<"state_hash">> => B#block.root_hash,
      <<"txs_hash">> => B#block.txs_hash,
      <<"miner_hash">> => B#block.miner_hash,
      <<"signature">> => B#block.signature,
      <<"time">> => B#block.time,
      <<"version">> => B#block.version,
      <<"transactions">> => B#block.txs
     }.

deserialize_from_map(#{<<"height">> := Height,
                       <<"prev_hash">> := PrevHash,
                       <<"state_hash">> := RootHash,
                       <<"miner">> := Miner,
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
                    target = Target,
                    pow_evidence = PowEvidence,
                    nonce = Nonce,
                    time = Time,
                    version = Version}}
    end;
deserialize_from_map(#{<<"prev_hash">> := PrevHash,
                       <<"state_hash">> := RootHash,
                       <<"txs_hash">> := TxsHash,
                       <<"miner_hash">> := MinerHash,
                       <<"signature">> := Signature,
                       <<"time">> := Time,
                       <<"version">> := Version,
                       <<"transactions">> := Txs}) ->
    {ok, #block{
            prev_hash = PrevHash,
            root_hash = RootHash,
            txs_hash = TxsHash,
            miner_hash = MinerHash,
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

validate_micro_block(Block, LeaderKey) ->
    % since trees are required for transaction signature validation, this is
    % performed while applying transactions
    case aec_headers:validate_micro_block_header(to_header(Block), LeaderKey) of
        ok ->
            Validators = [fun validate_txs_hash/1],
            case aeu_validation:run(Validators, [Block]) of
                ok              -> ok;
                {error, Reason} -> {error, {block, Reason}}
            end;
        {error, Reason} ->
            {error, {header, Reason}}
    end.

-spec validate_txs_hash(block()) -> ok | {error, malformed_txs_hash}.
validate_txs_hash(#block{txs = Txs,
                         txs_hash = BlockTxsHash}) ->
    case aec_txs_trees:root_hash(aec_txs_trees:from_txs(Txs)) of
        BlockTxsHash ->
            ok;
        _Other ->
            {error, malformed_txs_hash}
    end.

type(#block{miner = undefined, pow_evidence = no_value, txs = [], height = H}) when H > 0 -> micro;
type(_) -> key.
