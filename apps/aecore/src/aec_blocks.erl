-module(aec_blocks).

%% API
-export([prev_hash/1,
         height/1,
         trees/1,
         difficulty/1,
         linear_difficulty/1,
         set_nonce/2,
         new/3,
         to_header/1,
         serialize_for_network/1,
         deserialize_from_network/1,
         hash_internal_representation/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

-export_type([block_serialized_for_network/0,
              block_deserialized_from_network/0]).

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").

-define(CURRENT_BLOCK_VERSION, 1).

-type block_serialized_for_network() :: binary().
-type block_deserialized_from_network() :: #block{trees :: DummyTrees::trees()}.

prev_hash(Block) ->
    Block#block.prev_hash.

height(Block) ->
    Block#block.height.

trees(Block) ->
    Block#block.trees.

difficulty(Block) ->
    Block#block.difficulty.

%% TODO Clarify difficulty - see `aec_headers` module.
linear_difficulty(Block) ->
    aec_headers:linear_difficulty(to_header(Block)).

set_nonce(Block, Nonce) ->
    Block#block{nonce = Nonce}.

-spec new(block(), list(signed_tx()), trees()) -> {ok, block()} | {error, term()}.
new(LastBlock, Txs, Trees0) ->
    LastBlockHeight = height(LastBlock),
    {ok, LastBlockHeaderHash} = hash_internal_representation(LastBlock),
    Height = LastBlockHeight + 1,
    case aec_tx:apply_signed(Txs, Trees0, Height) of
        {ok, Trees} ->
            {ok, #block{height = Height,
                        prev_hash = LastBlockHeaderHash,
                        root_hash = aec_trees:all_trees_hash(Trees),
                        trees = Trees,
                        txs = Txs,
                        difficulty = difficulty(LastBlock),
                        time = aeu_time:now_in_msecs(),
                        version = ?CURRENT_BLOCK_VERSION}};
        {error, _Reason} = Error ->
            Error
    end.

-spec to_header(block()) -> header().
to_header(#block{height = Height,
                 prev_hash = PrevHash,
                 root_hash = RootHash,
                 difficulty = Difficulty,
                 nonce = Nonce,
                 time = Time,
                 version = Version}) ->
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            difficulty = Difficulty,
            nonce = Nonce,
            time = Time,
            version = Version}.

-spec serialize_for_network(BlockInternalRepresentation) ->
                                   {ok, block_serialized_for_network()} when
      BlockInternalRepresentation :: block()
                                   | block_deserialized_from_network().
serialize_for_network(B = #block{}) ->
    %% TODO: Define serialization format.
    DummyTrees = #trees{},
    {ok, term_to_binary(B#block{trees = DummyTrees})}.

-spec deserialize_from_network(block_serialized_for_network()) ->
                                      {ok, block_deserialized_from_network()}.
deserialize_from_network(B) when is_binary(B) ->
    %% TODO: Define serialization format.
    DummyTrees = #trees{},
    {ok, #block{trees = DummyTrees} = binary_to_term(B)}.

-spec hash_internal_representation(block()) -> {ok, block_header_hash()}.
hash_internal_representation(B = #block{}) ->
    aec_headers:hash_internal_representation(to_header(B)).
