-module(aec_governance).

%% API
-export([sorted_protocol_versions/0,
         protocols/0,
         key_blocks_to_check_difficulty_count/0,
         median_timestamp_key_blocks/0,
         expected_block_mine_rate/0,
         block_mine_reward/0,
         block_gas_limit/0,
         tx_gas/0,
         beneficiary_reward_delay/0,
         minimum_tx_fee/0,
         minimum_gas_price/0,
         name_preclaim_expiration/0,
         name_claim_burned_fee/0,
         name_claim_max_expiration/0,
         name_protection_period/0,
         name_claim_preclaim_delta/0,
         name_registrars/0,
         micro_block_cycle/0,
         accepted_future_block_time_shift/0,
         fraud_report_reward/0]).

-export_type([protocols/0]).

-include("blocks.hrl").

-define(SORTED_VERSIONS, lists:sort(maps:keys(?PROTOCOLS))).
-define(PROTOCOLS,
        #{?PROTOCOL_VERSION => ?GENESIS_HEIGHT
         }).

-define(BLOCKS_TO_CHECK_DIFFICULTY_COUNT, 17).
-define(TIMESTAMP_MEDIAN_BLOCKS, 11).
-define(EXPECTED_BLOCK_MINE_RATE, 3 * 60 * 1000). %% 60secs * 1000ms * 3 = 180000msecs
-define(BLOCK_MINE_REWARD, 10000000000000000000).
%% Ethereum's gas limit is 8 000 000 and block time ~15s.
%% For 3s block time it's 1 600 000 (5x less).
-define(BLOCK_GAS_LIMIT, 1600000).
%% Taken from Ethereum - a simple tx to send Eth is about 21000 gas.
-define(TX_GAS, 21000).
-define(POF_REWARD       , 500000000000000000). %% (?BLOCK_MINE_REWARD / 100) * 5
-define(BENEFICIARY_REWARD_DELAY, 180). %% in key blocks / generations
-define(MICRO_BLOCK_CYCLE, 3000). %% in msecs

-define(ACCEPTED_FUTURE_BLOCK_TIME_SHIFT, 10 * 60 * 1000). %% 10 min

%% Maps consensus protocol version to minimum height at which such
%% version is effective.  The height must be strictly increasing with
%% the version.
-type protocols() :: #{Version::non_neg_integer() => aec_blocks:height()}.


sorted_protocol_versions() ->
    ?SORTED_VERSIONS.

protocols() ->
    case aeu_env:user_map([<<"chain">>, <<"hard_forks">>]) of
        undefined -> ?PROTOCOLS;
        {ok, M} ->
            maps:from_list(
              lists:map(
                fun({K, V}) -> {binary_to_integer(K), V} end,
                maps:to_list(M)))
    end.

key_blocks_to_check_difficulty_count() ->
    ?BLOCKS_TO_CHECK_DIFFICULTY_COUNT.

median_timestamp_key_blocks() ->
    ?TIMESTAMP_MEDIAN_BLOCKS.

expected_block_mine_rate() ->
    aeu_env:user_config_or_env([<<"mining">>, <<"expected_mine_rate">>],
                               aecore, expected_mine_rate, ?EXPECTED_BLOCK_MINE_RATE).

%% In Ethereum, block gas limit is changed in every block. The new block gas
%% limit is decided by algorithm and vote by miners.
block_mine_reward() ->
    ?BLOCK_MINE_REWARD.

block_gas_limit() ->
    ?BLOCK_GAS_LIMIT.

tx_gas() ->
    ?TX_GAS.

minimum_tx_fee() ->
    1.

minimum_gas_price() ->
    0.

%% In key blocks / generations
%%
%% NOTE: The delay must cover at least the time frame where fraud can
%% be reported to avoid premature payout.
beneficiary_reward_delay() ->
    Delay = aeu_env:user_config_or_env([<<"mining">>, <<"beneficiary_reward_delay">>],
                                       aecore, beneficiary_reward_delay, ?BENEFICIARY_REWARD_DELAY),
    [error({beneficiary_reward_delay_too_low, Delay})
     || Delay < aec_chain_state:proof_of_fraud_report_delay()],
    Delay.

%% In milliseconds
micro_block_cycle() ->
    aeu_env:user_config_or_env([<<"mining">>, <<"micro_block_cycle">>],
                               aecore, micro_block_cycle, ?MICRO_BLOCK_CYCLE).

accepted_future_block_time_shift() ->
    ?ACCEPTED_FUTURE_BLOCK_TIME_SHIFT.

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

fraud_report_reward() ->
    ?POF_REWARD.
