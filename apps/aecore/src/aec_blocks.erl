%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% API for blocks
%%% @end
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
         new_key_from_header/1,
         new_micro/7,
         new_micro_from_header/2,
         pow/1,
         prev_hash/1,
         root_hash/1,
         serialize_to_binary/1,
         serialize_to_map/1,
         set_height/2,
         set_miner/2,
         set_nonce/2,
         set_nonce_and_pow/3,
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
         to_micro_header/1,
         to_key_header/1,
         txs/1,
         txs_hash/1,
         type/1,
         update_micro_candidate/5,
         validate_key_block/1,
         validate_micro_block/1,
         version/1
        ]).

-include("blocks.hrl").

%%%===================================================================
%%% Records and types
%%%===================================================================

-record(mic_block, {
          header    :: aec_headers:micro_header(),
          txs = []  :: tx_list()
         }).

-record(key_block, {
          header    :: aec_headers:key_header()
         }).

-opaque key_block()   :: #key_block{}.
-opaque micro_block() :: #mic_block{}.
-type   block()       :: key_block() | micro_block().
-type   height()      :: non_neg_integer().
-type   tx_list()     :: list(aetx_sign:signed_tx()).

-export_type([block/0,
              block_header_hash/0,
              height/0,
              key_block/0,
              micro_block/0
             ]).

%%%===================================================================
%%% Test interface
%%%===================================================================

-ifdef(TEST).

-export([raw_key_block/0,
         raw_micro_block/0
        ]).

raw_key_block() ->
    #key_block{header = aec_headers:raw_key_header()}.

raw_micro_block() ->
    #mic_block{header = aec_headers:raw_micro_header()}.

-endif. %% TEST

%%%===================================================================
%%% Handling connection to headers
%%%===================================================================

-spec to_header(block()) -> aec_headers:header().
to_header(#mic_block{header = H}) -> H;
to_header(#key_block{header = H}) -> H.

-spec to_key_header(key_block()) -> aec_headers:key_header().
to_key_header(#key_block{header = H}) -> H.

-spec to_micro_header(micro_block()) -> aec_headers:micro_header().
to_micro_header(#mic_block{header = H}) -> H.

%% Internal: DO NOT EXPORT
set_header(#mic_block{} = B, H) ->
    aec_headers:assert_micro_header(H),
    B#mic_block{header = H};
set_header(#key_block{} = B, H) ->
    aec_headers:assert_key_header(H),
    B#key_block{header = H}.

%%%===================================================================
%%% Block structure
%%%===================================================================

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

-spec type(block()) -> block_type().
type(#key_block{}) -> 'key';
type(#mic_block{}) -> 'micro'.

%%%===================================================================
%%% Constructors
%%%===================================================================

-spec new_key(height(), block_header_hash(), state_hash(), aec_pow:sci_int(),
              non_neg_integer(), non_neg_integer(), non_neg_integer(),
              miner_pubkey(), beneficiary_pubkey()
             ) -> key_block().
new_key(Height, PrevHash, RootHash, Target,
        Nonce, Time, Version, Miner, Beneficiary) ->
    H = aec_headers:new_key_header(Height, PrevHash, RootHash,
                                   Miner, Beneficiary, Target,
                                   no_value, Nonce, Time, Version),
    #key_block{header = H}.

-spec new_key_from_header(aec_headers:key_header()) -> key_block().
new_key_from_header(Header) ->
    aec_headers:assert_key_header(Header),
    #key_block{header = Header}.

-spec new_micro(height(), block_header_hash(), state_hash(), txs_hash(),
                tx_list(), non_neg_integer(),
                non_neg_integer()) -> micro_block().
new_micro(Height, PrevHash, RootHash, TxsHash, Txs, Time, Version) ->
    H = aec_headers:new_micro_header(Height, PrevHash, RootHash, Time,
                                     TxsHash, Version),
    #mic_block{header    = H,
               txs       = Txs
              }.

-spec new_micro_from_header(aec_headers:micro_header(), tx_list()
                           )-> micro_block().

new_micro_from_header(Header, Txs) ->
    aec_headers:assert_micro_header(Header),
    #mic_block{header    = Header,
               txs       = Txs
              }.

%%%===================================================================
%%% Block hash
%%%===================================================================

-spec hash_internal_representation(block()) -> {ok, block_header_hash()}.
hash_internal_representation(B) ->
    aec_headers:hash_header(to_header(B)).

%%%===================================================================
%%% Getters and setters
%%%===================================================================

-spec beneficiary(key_block()) -> aec_keys:pubkey().
beneficiary(Block) ->
    aec_headers:beneficiary(to_header(Block)).

-spec prev_hash(block()) -> block_header_hash().
prev_hash(Block) ->
    aec_headers:prev_hash(to_header(Block)).

-spec set_prev_hash(block(), block_header_hash()) -> block().
set_prev_hash(Block, PrevHash) ->
    set_header(Block, aec_headers:set_prev_hash(to_header(Block), PrevHash)).

-spec height(block()) -> height().
height(Block) ->
    aec_headers:height(to_header(Block)).

-spec set_height(block(), height()) -> block().
set_height(Block, Height) ->
    set_header(Block, aec_headers:set_height(to_header(Block), Height)).

-spec difficulty(key_block()) -> float().
difficulty(Block) ->
    aec_pow:target_to_difficulty(target(Block)).

-spec time_in_msecs(block()) -> non_neg_integer().
time_in_msecs(Block) ->
    aec_headers:time_in_msecs(to_header(Block)).

-spec set_time_in_msecs(block(), non_neg_integer()) -> block().
set_time_in_msecs(Block, T) ->
    set_header(Block, aec_headers:set_time_in_msecs(to_header(Block), T)).

-spec root_hash(block()) -> binary().
root_hash(Block) ->
    aec_headers:root_hash(to_header(Block)).

-spec set_root_hash(block(), binary()) -> block().
set_root_hash(Block, H) ->
    set_header(Block, aec_headers:set_root_hash(to_header(Block), H)).

-spec miner(key_block()) -> aec_keys:pubkey().
miner(Block) ->
    aec_headers:miner(to_header(Block)).

-spec set_miner(key_block(), aec_keys:pubkey()) -> key_block().
set_miner(Block, M) ->
    set_header(Block, aec_headers:set_miner(to_key_header(Block), M)).

-spec version(block()) -> non_neg_integer().
version(Block) ->
    aec_headers:version(to_header(Block)).

-spec set_nonce(key_block(), aec_pow:nonce()) -> key_block().
set_nonce(Block, Nonce) ->
    set_header(Block, aec_headers:set_nonce(to_key_header(Block), Nonce)).

-spec pow(key_block()) -> aec_pow:pow_evidence().
pow(Block) ->
    aec_headers:pow(to_key_header(Block)).

-spec set_nonce_and_pow(key_block(), aec_pow:nonce(), aec_pow:pow_evidence()
                       ) -> key_block().
set_nonce_and_pow(Block, Nonce, Evd) ->
    H = aec_headers:set_nonce_and_pow(to_key_header(Block), Nonce, Evd),
    set_header(Block, H).

-spec signature(micro_block()) -> binary() | undefined.
signature(Block) ->
    aec_headers:signature(to_micro_header(Block)).

-spec set_signature(micro_block(), binary()) -> micro_block().
set_signature(Block, Signature) ->
    Header = aec_headers:set_signature(to_micro_header(Block), Signature),
    set_header(Block, Header).

-spec target(key_block()) -> integer().
target(Block) ->
    aec_headers:target(to_key_header(Block)).

-spec set_target(key_block(), non_neg_integer()) -> key_block().
set_target(Block, Target) ->
    set_header(Block, aec_headers:set_target(to_header(Block), Target)).

-spec txs(micro_block()) -> tx_list().
txs(Block) ->
    Block#mic_block.txs.

-spec set_txs(micro_block(), tx_list()) -> micro_block().
set_txs(Block, Txs) ->
    Block#mic_block{txs = Txs}.

-spec txs_hash(micro_block()) -> binary().
txs_hash(Block) ->
    aec_headers:txs_hash(to_micro_header(Block)).

-spec update_micro_candidate(micro_block(), txs_hash(), state_hash(),
                             [aetx_sign:signed_tx()], non_neg_integer()
                            ) -> micro_block().
update_micro_candidate(#mic_block{} = Block, TxsRootHash, RootHash, Txs, TimeMSecs) ->
    H = aec_headers:update_micro_candidate(to_micro_header(Block),
                                           TxsRootHash, RootHash, TimeMSecs),
    Block#mic_block{ header = H
                   , txs    = Txs
                   }.

%%%===================================================================
%%% Serialization
%%%===================================================================

-spec serialize_to_binary(block()) -> binary().
serialize_to_binary(#key_block{} = Block) ->
    Hdr = aec_headers:serialize_to_binary(to_key_header(Block)),
    Vsn = version(Block),
    {ok, Template} = serialization_template(key, Vsn),
    aec_object_serialization:serialize(
      key_block,
      Vsn,
      Template,
      [{header, Hdr}]);
serialize_to_binary(#mic_block{} = Block) ->
    Hdr = aec_headers:serialize_to_binary(to_micro_header(Block)),
    Txs = [ aetx_sign:serialize_to_binary(Tx) || Tx <- txs(Block)],
    Vsn = version(Block),
    {ok, Template} = serialization_template(micro, Vsn),
    aec_object_serialization:serialize(
      micro_block,
      Vsn,
      Template,
      [{header, Hdr}, {txs, Txs}]).

-spec deserialize_from_binary(binary()) -> {'error', term()} | {'ok', block()}.
deserialize_from_binary(Bin) ->
    case aec_object_serialization:deserialize_type_and_vsn(Bin) of
        {key_block,   Vsn, _RawFields} ->
            deserialize_key_block_from_binary(Vsn, Bin);
        {micro_block, Vsn, _RawFields} ->
            deserialize_micro_block_from_binary(Vsn, Bin)
    end.

deserialize_micro_block_from_binary(Vsn, Bin) ->
    case serialization_template(micro, Vsn) of
        {ok, Template} ->
            [{header, Hdr0}, {txs, Txs0}] =
                aec_object_serialization:deserialize(micro_block, Vsn, Template, Bin),
            case aec_headers:deserialize_micro_from_binary(Hdr0, Vsn) of
                {ok, Hdr} ->
                    Txs = [aetx_sign:deserialize_from_binary(Tx)
                           || Tx <- Txs0],
                    {ok, #mic_block{header = Hdr, txs = Txs}};
                Err = {error, _} ->
                    Err
            end;
        Err = {error, _} ->
            Err
    end.

deserialize_key_block_from_binary(Vsn, Bin) ->
    case serialization_template(key, Vsn) of
        {ok, Template} ->
            [{header, Hdr0}] =
                aec_object_serialization:deserialize(key_block, Vsn, Template, Bin),
            case aec_headers:deserialize_key_from_binary(Hdr0, Vsn) of
                {ok, Hdr} ->
                    {ok, #key_block{header = Hdr}};
                {error, _} = E ->
                    E
            end;
        Err = {error, _} ->
            Err
    end.

serialization_template(key, Vsn) when Vsn >= ?GENESIS_VERSION andalso Vsn =< ?PROTOCOL_VERSION ->
    {ok, [{header, binary}]};
serialization_template(micro, Vsn) when Vsn >= ?GENESIS_VERSION andalso Vsn =< ?PROTOCOL_VERSION ->
    {ok, [{header, binary}, {txs, [binary]}]};
serialization_template(_BlockType, Vsn) ->
    {error, {bad_block_vsn, Vsn}}.

-spec serialize_to_map(block()) -> map().
serialize_to_map(#key_block{} = Block) ->
    aec_headers:serialize_to_map(to_key_header(Block));
serialize_to_map(#mic_block{} = Block) ->
    H   = to_micro_header(Block),
    Map = aec_headers:serialize_to_map(H),
    Map#{<<"transactions">> => Block#mic_block.txs
        }.

-spec deserialize_from_map(map()) -> {'error', term()} | {'ok', block()}.
deserialize_from_map(Map) ->
    case aec_headers:deserialize_from_map(Map) of
        {ok, H} ->
            case aec_headers:type(H) of
                micro ->
                    {ok, #mic_block{header    = H,
                                    txs       = maps:get(<<"transactions">>, Map)}};
                key ->
                    {ok, #key_block{header = H}}
            end;
        {error, _} = E -> E
    end.

%%%===================================================================
%%% Validation
%%%===================================================================

-spec validate_key_block(key_block()) -> 'ok' | {'error', {'header', term()}}.
validate_key_block(#key_block{} = Block) ->
    case aec_headers:validate_key_block_header(to_key_header(Block)) of
        ok -> ok;
        {error, Reason} -> {error, {header, Reason}}
    end.

-spec validate_micro_block(micro_block()) -> 'ok' | {'error', {'header' | 'block', term()}}.
validate_micro_block(#mic_block{} = Block) ->
    Validators = [fun validate_txs_hash/1],
    case aec_headers:validate_micro_block_header(to_micro_header(Block)) of
        ok ->
            case aeu_validation:run(Validators, [Block]) of
                ok              -> ok;
                {error, Reason} -> {error, {block, Reason}}
            end;
        {error, Reason} ->
            {error, {header, Reason}}
    end.

-spec validate_txs_hash(block()) -> ok | {error, malformed_txs_hash}.
validate_txs_hash(#mic_block{txs = Txs} = Block) ->
    BlockTxsHash = aec_headers:txs_hash(to_micro_header(Block)),
    case aec_txs_trees:pad_empty(aec_txs_trees:root_hash(aec_txs_trees:from_txs(Txs))) of
        BlockTxsHash ->
            ok;
        _Other ->
            {error, malformed_txs_hash}
    end.
