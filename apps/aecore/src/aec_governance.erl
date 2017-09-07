-module(aec_governance).

%% API
-export([recalculate_difficulty_frequency/0,
         expected_block_mine_rate/0]).

-define(RECALCULATE_DIFFICULTY_FREQUENCY, 10).
-define(EXPECTED_BLOCK_MINE_RATE, 300). %% 60secs * 5 = 300secs


recalculate_difficulty_frequency() ->
    %% Between how many blocks difficulty should be calculated.
    %% Returned value is # of blocks.
    ?RECALCULATE_DIFFICULTY_FREQUENCY.

expected_block_mine_rate() ->
    %% Returned in seconds.
    ?EXPECTED_BLOCK_MINE_RATE.
