-module(aec_mining).

%% API
-export([mine/0,
         mine/1]).


-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").


%% Cuckoo Cycle has a solution probability of 2.2% so we are
%% likely to succeed in 100 steps (~90%).
-define(DEFAULT_MINE_ATTEMPTS_COUNT, 100).

%% API

-spec mine() -> {ok, block()} | {error, term()}.
mine() ->
    mine(?DEFAULT_MINE_ATTEMPTS_COUNT).

-spec mine(non_neg_integer()) -> {ok, block()} | {error, term()}.
mine(Attempts) ->
    {ok, LastBlock} = aec_chain:top(),
    Trees = aec_blocks:trees(LastBlock),
    case get_txs_to_mine(Trees) of
        {error, _} = Error ->
            Error;
        {ok, Txs} -> 
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
            end
    end.


%% Internal functions

-spec mine(block(), non_neg_integer()) -> {ok, block()} | {error, term()}.
mine(Block, Attempts) ->
    Target = aec_blocks:target(Block),
    {ok, BlockBin} = aec_headers:serialize_to_binary(aec_blocks:to_header(Block)),
    Mod = aec_pow:pow_module(),
    case Mod:generate(BlockBin, Target, Attempts) of
        {ok, {Nonce, Evd}} ->
            {ok, aec_blocks:set_nonce(Block, Nonce, Evd)};
        {error, generation_count_exhausted} = Error ->
            Error
    end.

-spec get_txs_to_mine(trees()) -> {'ok', list(signed_tx())} | {error, term()}.
get_txs_to_mine(Trees) ->
    {ok, Txs0} = aec_tx_pool:all(),
    case create_coinbase_tx(Trees) of
        {ok, CoinbaseTx} ->
            case aec_keys:sign(CoinbaseTx) of
                {ok, SignedCoinbaseTx} ->
                    {ok, [SignedCoinbaseTx | Txs0]};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec create_coinbase_tx(trees()) -> {ok, coinbase_tx()} | {error, term()}.
create_coinbase_tx(Trees) ->
    case aec_keys:pubkey() of
        {ok, Pubkey} ->
            aec_coinbase_tx:new(#{account => Pubkey}, Trees);
        {error, _} = Error ->
            Error
    end.


-spec maybe_recalculate_difficulty(block()) -> block().
maybe_recalculate_difficulty(Block) ->
    Height = aec_blocks:height(Block),
    case should_recalculate_difficulty(Height) of
        true ->
            %% Recalculate difficulty based on mining rate of N last blocks,
            %% where N = recalculate_difficulty_frequency.
            BlocksToCheckCount = aec_governance:recalculate_difficulty_frequency(),
            Target = calculate_difficulty(Block, BlocksToCheckCount),
            Block#block{target = Target};
        false ->
            Block
    end.

-spec should_recalculate_difficulty(height()) -> boolean().
should_recalculate_difficulty(Height) ->
    RecalculateDifficultyFrequency = aec_governance:recalculate_difficulty_frequency(),
    (Height > 10) andalso %% do not change difficulty for the first 10 blocks
                    (Height > RecalculateDifficultyFrequency)
        andalso (0 == (Height rem RecalculateDifficultyFrequency)).

-spec calculate_difficulty(block(), pos_integer()) -> non_neg_integer().
calculate_difficulty(NewBlock, BlocksToCheckCount) ->
    CurrentTarget = NewBlock#block.target,
    CurrentRate = get_current_rate(NewBlock, BlocksToCheckCount),
    ExpectedRate = aec_governance:expected_block_mine_rate(),
    aec_pow:recalculate_difficulty(CurrentTarget, ExpectedRate, CurrentRate).

-spec get_current_rate(block(), pos_integer()) -> non_neg_integer().
get_current_rate(Block, BlocksToCheckCount) ->
    BlockHeader = aec_blocks:to_header(Block),
    BlockHeight = aec_blocks:height(Block),

    FirstBlockHeight = BlockHeight - BlocksToCheckCount,
    {ok, FirstBlockHeader} = aec_chain:get_header_by_height(FirstBlockHeight), %% TODO: Ensure height refers to correct chain when we have support for longest chain.

    mining_rate_between_blocks(BlockHeader, FirstBlockHeader, BlocksToCheckCount).

-spec mining_rate_between_blocks(header(), header(), non_neg_integer()) -> non_neg_integer().
mining_rate_between_blocks(Block1Header, Block2Header, BlocksMinedCount) ->
    Time1 = aec_headers:time_in_secs(Block1Header),
    Time2 = aec_headers:time_in_secs(Block2Header),
    TimeDiff = Time1 - Time2,
    TimeDiff div BlocksMinedCount.
