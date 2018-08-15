-module(aec_governance).

%% API
-export([sorted_protocol_versions/0,
         protocols/0,
         key_blocks_to_check_difficulty_count/0,
         median_timestamp_key_blocks/0,
         expected_block_mine_rate/0,
         block_mine_reward/0,
         max_txs_in_block/0,
         miner_reward_delay/0,
         minimum_tx_fee/0,
         minimum_gas_price/0,
         name_preclaim_expiration/0,
         name_claim_burned_fee/0,
         name_claim_max_expiration/0,
         name_protection_period/0,
         name_claim_preclaim_delta/0,
         name_registrars/0,
         micro_block_cycle/0,
         accepted_future_block_time_shift/0]).

-export_type([protocols/0]).

-include("blocks.hrl").

-define(SORTED_VERSIONS, lists:sort(maps:keys(?PROTOCOLS))).
-define(PROTOCOLS,
        #{?PROTOCOL_VERSION => ?GENESIS_HEIGHT
         }).

-define(BLOCKS_TO_CHECK_DIFFICULTY_COUNT, 10).
-define(TIMESTAMP_MEDIAN_BLOCKS, 11).
-define(EXPECTED_BLOCK_MINE_RATE, 300000). %% 60secs * 1000ms * 5 = 300000msecs
-define(BLOCK_MINE_REWARD, 10000000000000000000).
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

block_mine_reward() ->
    ?BLOCK_MINE_REWARD.

max_txs_in_block() ->
    %% TODO: Consider trade-offs sync latency vs pow time
    %%       Relate to transaction size expressed with gas (when we have channels)
    10946.

minimum_tx_fee() ->
    1.

minimum_gas_price() ->
    0.

miner_reward_delay() ->
    0.

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
