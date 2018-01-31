-module(aec_governance).

%% API
-export([blocks_to_check_difficulty_count/0,
         expected_block_mine_rate/0,
         block_mine_reward/0,
         max_txs_in_block/0,
         minimum_tx_fee/0,
         name_preclaim_expiration/0,
         name_claim_burned_fee/0,
         name_claim_max_expiration/0,
         name_protection_period/0,
         name_claim_preclaim_delta/0,
         name_registrars/0]).

-define(BLOCKS_TO_CHECK_DIFFICULTY_COUNT, 10).
-define(EXPECTED_BLOCK_MINE_RATE, 300000). %% 60secs * 1000ms * 5 = 300000msecs
-define(BLOCK_MINE_REWARD, 10).


blocks_to_check_difficulty_count() ->
    ?BLOCKS_TO_CHECK_DIFFICULTY_COUNT.

expected_block_mine_rate() ->
    application:get_env(aecore, expected_mine_rate,
                        ?EXPECTED_BLOCK_MINE_RATE).

block_mine_reward() ->
    ?BLOCK_MINE_REWARD.

max_txs_in_block() ->
    %% TODO: Consider trade-offs sync latency vs pow time
    %%       Relate to transaction size expressed with gas (when we have channels)
    10946.

minimum_tx_fee() ->
    1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Naming system variables

name_preclaim_expiration() ->
    300.

name_claim_burned_fee() ->
    3.

name_claim_max_expiration() ->
    50000.

name_protection_period() ->
    2016.

name_claim_preclaim_delta() ->
    1.

name_registrars() ->
    [<<"aet">>, <<"test">>].
