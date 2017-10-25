-module(aec_blocks).

%% API
-export([prev_hash/1,
         height/1,
         trees/1,
         target/1,
         txs/1,
         difficulty/1,
         set_nonce/3,
         set_trees/2,
         new/3,
         to_header/1,
         serialize_for_network/1,
         deserialize_from_network/1,
         serialize_for_store/1,
         deserialize_from_store/1,
         serialize_to_map/1,
         deserialize_from_map/1,
         hash_internal_representation/1,
         validate/1]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-export_type([block_serialized_for_network/0,
              block_deserialized_from_network/0]).

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").


-define(CURRENT_BLOCK_VERSION, ?GENESIS_VERSION).

-type block_serialized_for_network() :: binary().
-type block_deserialized_from_network() :: #block{trees :: DummyTrees::trees()}.

-spec prev_hash(block()) -> block_header_hash().
prev_hash(Block) ->
    Block#block.prev_hash.

-spec height(block()) -> height().
height(Block) ->
    Block#block.height.

-spec trees(block()) -> trees().
trees(Block) ->
    Block#block.trees.

-spec target(block()) -> integer().
target(Block) ->
    Block#block.target.

-spec difficulty(block()) -> float().
difficulty(Block) ->
    aec_pow:target_to_difficulty(target(Block)).

%% Sets the evidence of PoW,too,  for Cuckoo Cycle
-spec set_nonce(block(), non_neg_integer(), aec_pow:pow_evidence()) -> block().
set_nonce(Block, Nonce, Evd) ->
    Block#block{nonce = Nonce,
                pow_evidence = Evd}.

-spec set_trees(block(), aec_trees:trees()) -> block().
set_trees(Block, Trees) ->
    Block#block{trees = Trees}.

%% TODO: have a spec for list of transactions
-spec txs(block()) -> list(signed_tx()).
txs(Block) ->
    Block#block.txs.

-spec new(block(), list(signed_tx()), trees()) -> {ok, block()} | {error, term()}.
new(LastBlock, Txs, Trees0) ->
    LastBlockHeight = height(LastBlock),
    {ok, LastBlockHeaderHash} = hash_internal_representation(LastBlock),
    Height = LastBlockHeight + 1,
    case aec_tx:apply_signed(Txs, Trees0, Height) of
        {ok, Trees} ->
            {ok, TxsTree} = aec_txs_trees:new(Txs),
            {ok, TxsRootHash} = aec_txs_trees:root_hash(TxsTree),
            {ok, #block{height = Height,
                        prev_hash = LastBlockHeaderHash,
                        root_hash = aec_trees:all_trees_hash(Trees),
                        trees = Trees,
                        txs_hash = TxsRootHash,
                        txs = Txs,
                        target = target(LastBlock),
                        time = aeu_time:now_in_msecs(),
                        version = ?CURRENT_BLOCK_VERSION}};
        {error, _Reason} = Error ->
            Error
    end.

-spec to_header(block()) -> header().
to_header(#block{height = Height,
                 prev_hash = PrevHash,
                 txs_hash = TxsHash,
                 root_hash = RootHash,
                 target = Target,
                 nonce = Nonce,
                 time = Time,
                 version = Version,
                 pow_evidence = Evd}) ->
    #header{height = Height,
            prev_hash = PrevHash,
            txs_hash = TxsHash,
            root_hash = RootHash,
            target = Target,
            nonce = Nonce,
            time = Time,
            pow_evidence = Evd,
            version = Version}.

-spec serialize_for_network(BlockInternalRepresentation) ->
                                   {ok, block_serialized_for_network()} when
      BlockInternalRepresentation :: block()
                                   | block_deserialized_from_network().
serialize_for_network(B = #block{}) ->
    {ok, jsx:encode(serialize_to_map(B))}.

serialize_to_map(B = #block{}) ->
    #{<<"height">> => height(B),
      <<"prev_hash">> => base64:encode(prev_hash(B)),
      <<"state_hash">> => base64:encode(B#block.root_hash),
      <<"txs_hash">> => base64:encode(B#block.txs_hash),
      <<"target">> => B#block.target,
      <<"nonce">> => B#block.nonce,
      <<"time">> => B#block.time,
      <<"version">> => B#block.version,
      <<"pow">> => aec_headers:serialize_pow_evidence(B#block.pow_evidence),
      <<"transactions">> => lists:map(fun aec_tx_sign:serialize/1, B#block.txs)
     }.

-define(STORAGEVERSION, 1).
serialize_for_store(B = #block{}) ->
    term_to_binary({?STORAGEVERSION,
                    height(B),
                    prev_hash(B),
                    B#block.root_hash,
                    B#block.txs_hash,
                    B#block.target,
                    B#block.nonce,
                    B#block.time,
                    B#block.version,
                    B#block.pow_evidence,
                    B#block.txs}, [{compressed,9}]).

deserialize_from_store(Bin) ->
    case binary_to_term(Bin) of
        {?STORAGEVERSION,
         Height,
         PrevHash,
         RootHash,
         TxsHash,
         Target,
         Nonce,
         Time,
         Version,
         PowEvidence,
         Txs} ->
            #block{
               height = Height,
               prev_hash = PrevHash,
               root_hash = RootHash,
               txs_hash = TxsHash,
               target = Target,
               nonce = Nonce,
               time = Time,
               version = Version,
               txs = Txs,
               pow_evidence = PowEvidence};
        T when tuple_size(T) > 0 ->
            case element(1, T) of
               I when is_integer(I), I > ?STORAGEVERSION ->
                    exit({future_storage_version, I, Bin});
                %% Add handler of old version here when upgrading version.
                I when is_integer(I), I < ?STORAGEVERSION ->
                    exit({old_forgotten_storage_version, I, Bin})
            end
    end.

-spec deserialize_from_network(block_serialized_for_network()) ->
                                      {ok, block_deserialized_from_network()}.
deserialize_from_network(B) when is_binary(B) ->
    deserialize_from_map(jsx:decode(B, [return_maps])).

deserialize_from_map(#{<<"height">> := Height,
                       <<"prev_hash">> := PrevHash,
                       <<"state_hash">> := RootHash,
                       <<"txs_hash">> := TxsHash,
                       <<"target">> := Target,
                       <<"nonce">> := Nonce,
                       <<"time">> := Time,
                       <<"version">> := Version,
                       <<"pow">> := PowEvidence,
                       <<"transactions">> := Txs}) ->
    {ok, #block{
            height = Height,
            prev_hash = base64:decode(PrevHash),
            root_hash = base64:decode(RootHash),
            txs_hash = base64:decode(TxsHash),
            target = Target,
            nonce = Nonce,
            time = Time,
            version = Version,
            txs = lists:map(fun aec_tx_sign:deserialize/1, Txs),
            pow_evidence = aec_headers:deserialize_pow_evidence(PowEvidence)}}.

-spec hash_internal_representation(block()) -> {ok, block_header_hash()}.
hash_internal_representation(B = #block{}) ->
    aec_headers:hash_header(to_header(B)).


-spec validate(block()) -> ok | {error, term()}.
validate(Block) ->
    Validators = [fun validate_coinbase_txs_count/1,
                  fun validate_txs_hash/1],
    aeu_validation:run(Validators, Block).

-spec validate_coinbase_txs_count(block()) -> ok | {error, multiple_coinbase_txs}.
validate_coinbase_txs_count(#block{txs = Txs}) ->
    CoinbaseTxsCount = lists:foldl(
                         fun(SignedTx, Count) ->
                                 Tx = aec_tx_sign:data(SignedTx),
                                 Mod = tx_dispatcher:handler(Tx),
                                 case Mod:type() of
                                     <<"coinbase">> ->
                                         Count + 1;
                                     _Other ->
                                         Count
                                 end
                         end, 0, Txs),
    case CoinbaseTxsCount =< 1 of
        true ->
            ok;
        false ->
            {error, multiple_coinbase_txs}
    end.

-spec validate_txs_hash(block()) -> ok | {error, malformed_txs_hash}.
validate_txs_hash(#block{txs = Txs,
                         txs_hash = BlockTxsHash}) ->
    {ok, TxsTree} = aec_txs_trees:new(Txs),
    {ok, TxsRootHash} = aec_txs_trees:root_hash(TxsTree),
    case TxsRootHash of
        BlockTxsHash ->
            ok;
        _Other ->
            {error, malformed_txs_hash}
    end.

