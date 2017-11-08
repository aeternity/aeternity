-module(aec_governance).

%% API
-export([recalculate_difficulty_frequency/0,
         expected_block_mine_rate/0,
         block_mine_reward/0,
         block_mine_reward_delay/0,
         max_txs_in_block/0,
         minimum_tx_fee/0]).

-define(RECALCULATE_DIFFICULTY_FREQUENCY, 10).
-define(EXPECTED_BLOCK_MINE_RATE, 300). %% 60secs * 5 = 300secs
-define(BLOCK_MINE_REWARD, 10).
-define(BLOCK_MINE_REWARD_DELAY, 1).

recalculate_difficulty_frequency() ->
    %% Between how many blocks difficulty should be calculated.
    %% Returned value is # of blocks.
    ?RECALCULATE_DIFFICULTY_FREQUENCY.

expected_block_mine_rate() ->
    %% Returned in seconds.
    ?EXPECTED_BLOCK_MINE_RATE.

block_mine_reward() ->
    ?BLOCK_MINE_REWARD.

block_mine_reward_delay() ->
    ?BLOCK_MINE_REWARD_DELAY.

max_txs_in_block() ->
    %% TODO Review considering block size and different size of signed transactions.
    10.

minimum_tx_fee() ->
    1.
