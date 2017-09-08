-module(aec_blocks).

%% API
-export([height/1,
         trees/1,
         difficulty/1,
         set_nonce/3,
         top/0,
         new/3,
         to_header/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").

-define(CURRENT_BLOCK_VERSION, 1).

height(Block) ->
    Block#block.height.

trees(Block) ->
    Block#block.trees.

difficulty(Block) ->
    Block#block.difficulty.

%% Sets the evidence of PoW,too,  for Cuckoo Cycle
set_nonce(Block, Nonce, no_value) ->
    Block#block{nonce = Nonce};
set_nonce(Block, Nonce, Evd) ->
    Block#block{nonce = Nonce,
                pow_evidence = Evd}.

top() ->
    %% TODO: fetch the most recent block from storage
    %% and transform it to #block{} record
    {ok, #block{}}.

-spec new(block(), list(signed_tx()), trees()) -> {ok, block()} | {error, term()}.
new(LastBlock, Txs, Trees0) ->
    LastBlockHeight = height(LastBlock),
    LastBlockHeader = to_header(LastBlock),
    Height = LastBlockHeight + 1,
    case aec_tx:apply_signed(Txs, Trees0, Height) of
        {ok, Trees} ->
            {ok, #block{height = Height,
                        prev_hash = aec_sha256:hash(LastBlockHeader),
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
                 version = Version,
                 pow_evidence = Evd}) ->
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            difficulty = Difficulty,
            nonce = Nonce,
            time = Time,
            version = Version,
            pow_evidence = Evd}.
