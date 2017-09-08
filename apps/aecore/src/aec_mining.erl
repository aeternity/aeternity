-module(aec_mining).

%% API
-export([mine/0,
         mine/1]).

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").


-define(DEFAULT_MINE_ATTEMPTS_COUNT, 10).

%% API

-spec mine() -> {ok, block()} | {error, term()}.
mine() ->
    mine(?DEFAULT_MINE_ATTEMPTS_COUNT).

-spec mine(non_neg_integer()) -> {ok, block()} | {error, term()}.
mine(Attempts) ->
    {ok, LastBlock} = aec_blocks:top(),
    Trees = aec_blocks:trees(LastBlock),
    Txs = get_txs_to_mine(Trees),
    case aec_blocks:new(LastBlock, Txs, Trees) of
        {ok, Block0} ->
            Block = maybe_recalculate_difficulty(Block0),

            case mine(Block, Attempts) of
                {ok, _Block} = Ok ->
                    Ok;
                {error, _Reason} = Error ->
                    Error
            end;
        {error, _Reason} = Error ->
            Error
    end.


%% Internal functions

-spec mine(block(), non_neg_integer()) -> {ok, block()} | {error, term()}.
mine(Block, Attempts) ->
    Difficulty = aec_blocks:difficulty(Block),
    Mod = aec_pow:pow_module(),
    case Mod:generate(Block, Difficulty, Attempts) of
        {ok, {Nonce, Evd}} ->
            {ok, aec_blocks:set_nonce(Block, Nonce, Evd)};
        {error, generation_count_exhausted} = Error ->
            Error
    end.

-spec get_txs_to_mine(trees()) -> list(signed_tx()).
get_txs_to_mine(Trees) ->
    {ok, Txs0} = aec_tx_pool:all(),
    {ok, CoinbaseTx} = create_coinbase_tx(Trees),
    {ok, SignedCoinbaseTx} = aec_keys:sign(CoinbaseTx),
    [SignedCoinbaseTx | Txs0].

-spec create_coinbase_tx(trees()) -> {ok, coinbase_tx()}.
create_coinbase_tx(Trees) ->
    {ok, Pubkey} = aec_keys:pubkey(),
    {ok, CoinbaseTx} = aec_coinbase_tx:new(#{account => Pubkey}, Trees),
    {ok, CoinbaseTx}.

-spec maybe_recalculate_difficulty(block()) -> block().
maybe_recalculate_difficulty(Block) ->
    Height = aec_blocks:height(Block),
    case should_recalculate_difficulty(Height) of
        true ->
            %% Recalculate difficulty based on mining rate of N last blocks,
            %% where N = recalculate_difficulty_frequency.
            BlocksToCheckCount = aec_governance:recalculate_difficulty_frequency(),
            Difficulty = calculate_difficulty(Block, BlocksToCheckCount),
            Block#block{difficulty = Difficulty};
        false ->
            Block
    end.

-spec should_recalculate_difficulty(non_neg_integer()) -> boolean().
should_recalculate_difficulty(Height) ->
    RecalculateDifficultyFrequency = aec_governance:recalculate_difficulty_frequency(),
    (Height > 10) andalso %% do not change difficulty for the first 10 blocks
                    (Height > RecalculateDifficultyFrequency)
        andalso (0 == (Height rem RecalculateDifficultyFrequency)).

-spec calculate_difficulty(block(), non_neg_integer()) -> non_neg_integer().
calculate_difficulty(NewBlock, BlocksToCheckCount) ->
    CurrentDifficulty = NewBlock#block.difficulty,
    CurrentRate = get_current_rate(NewBlock, BlocksToCheckCount),
    ExpectedRate = aec_governance:expected_block_mine_rate(),
    aec_pow:recalculate_difficulty(CurrentDifficulty, ExpectedRate, CurrentRate).

-spec get_current_rate(block(), non_neg_integer()) -> non_neg_integer().
get_current_rate(Block, BlocksToCheckCount) ->
    BlockHeader = aec_blocks:to_header(Block),
    BlockHeight = aec_blocks:height(Block),

    FirstBlockHeight = BlockHeight - BlocksToCheckCount,
    {ok, FirstBlockHeader} = aec_headers:get_by_height(FirstBlockHeight),

    mining_rate_between_blocks(BlockHeader, FirstBlockHeader, BlocksToCheckCount).

-spec mining_rate_between_blocks(header(), header(), non_neg_integer()) -> non_neg_integer().
mining_rate_between_blocks(Block1Header, Block2Header, BlocksMinedCount) ->
    Time1 = aec_headers:time_in_secs(Block1Header),
    Time2 = aec_headers:time_in_secs(Block2Header),
    TimeDiff = Time1 - Time2,
    TimeDiff div BlocksMinedCount.
