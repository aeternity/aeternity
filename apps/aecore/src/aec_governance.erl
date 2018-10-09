-module(aec_governance).

%% API
-export([sorted_protocol_versions/0,
         protocols/0,
         key_blocks_to_check_difficulty_count/0,
         median_timestamp_key_blocks/0,
         expected_block_mine_rate/0,
         block_mine_reward/0,
         block_gas_limit/0,
         byte_gas/0,
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
         fraud_report_reward/0,
         state_gas_cost_per_block/1,
         primop_base_gas_cost/1]).

-export_type([protocols/0]).

-include("blocks.hrl").
-include_lib("aebytecode/include/aeb_opcodes.hrl").

-define(SORTED_VERSIONS, lists:sort(maps:keys(?PROTOCOLS))).
-define(PROTOCOLS,
        #{?PROTOCOL_VERSION => ?GENESIS_HEIGHT
         }).

-define(BLOCKS_TO_CHECK_DIFFICULTY_COUNT, 17).
-define(TIMESTAMP_MEDIAN_BLOCKS, 11).
-define(EXPECTED_BLOCK_MINE_RATE_MINUTES, 3).
-define(EXPECTED_BLOCK_MINE_RATE, (?EXPECTED_BLOCK_MINE_RATE_MINUTES * 60 * 1000)). %% 60secs * 1000ms * 3 = 180000msecs
-define(EXPECTED_BLOCKS_IN_A_YEAR_FLOOR, (175200 = ((60 * 24 * 365) div ?EXPECTED_BLOCK_MINE_RATE_MINUTES))).
-define(BLOCK_MINE_REWARD, 10000000000000000000).
%% Ethereum's gas limit is 8 000 000 and block time ~15s.
%% For 3s block time it's 1 600 000 (5x less).
-define(BLOCK_GAS_LIMIT, (4*1600000)).
%% Gas for 1 byte of a serialized tx.
-define(BYTE_GAS, 100).
-define(POF_REWARD       , 500000000000000000). %% (?BLOCK_MINE_REWARD / 100) * 5
-define(BENEFICIARY_REWARD_DELAY, 180). %% in key blocks / generations
-define(MICRO_BLOCK_CYCLE, 3000). %% in msecs

-define(ACCEPTED_FUTURE_BLOCK_TIME_SHIFT, (?EXPECTED_BLOCK_MINE_RATE_MINUTES * 3 * 60 * 1000)). %% 9 min

-define(ORACLE_STATE_GAS_COST_PER_YEAR, 32000). %% 32000 as `?GCREATE` in `aevm_gas.hrl` i.e. an oracle-related state object costs per year as much as it costs to indefinitely create an account.

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

byte_gas() ->
    ?BYTE_GAS.

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

%% Reoccurring gas cost, meant to be paid up front.
%%
%% Expressed as a fraction because otherwise it would become too large
%% when multiplied by the number of key blocks.
-spec state_gas_cost_per_block(atom()) -> {Part::pos_integer(), Whole::pos_integer()}.
state_gas_cost_per_block(oracle_registration) -> {?ORACLE_STATE_GAS_COST_PER_YEAR, ?EXPECTED_BLOCKS_IN_A_YEAR_FLOOR};
state_gas_cost_per_block(oracle_extension)    -> state_gas_cost_per_block(oracle_registration);
state_gas_cost_per_block(oracle_query)        -> state_gas_cost_per_block(oracle_registration);
state_gas_cost_per_block(oracle_response)     -> state_gas_cost_per_block(oracle_registration).

%% As primops are not meant to be called from contract call
%% transactions, calling a primop costs (apart from the fees for the
%% calling contract create transaction or contract call transaction)
%% at least the gas cost of the call instruction (e.g. `CALL`) used
%% for calling the primop.
primop_base_gas_cost(?PRIM_CALL_SPEND              ) -> 0;
primop_base_gas_cost(?PRIM_CALL_ORACLE_REGISTER    ) -> 0;
primop_base_gas_cost(?PRIM_CALL_ORACLE_QUERY       ) -> 0;
primop_base_gas_cost(?PRIM_CALL_ORACLE_RESPOND     ) -> 0;
primop_base_gas_cost(?PRIM_CALL_ORACLE_EXTEND      ) -> 0;
primop_base_gas_cost(?PRIM_CALL_ORACLE_GET_ANSWER  ) -> 0;
primop_base_gas_cost(?PRIM_CALL_ORACLE_GET_QUESTION) -> 0;
primop_base_gas_cost(?PRIM_CALL_ORACLE_QUERY_FEE   ) -> 0;
primop_base_gas_cost(?PRIM_CALL_AENS_RESOLVE       ) -> 0;
primop_base_gas_cost(?PRIM_CALL_AENS_PRECLAIM      ) -> 0;
primop_base_gas_cost(?PRIM_CALL_AENS_CLAIM         ) -> 0;
primop_base_gas_cost(?PRIM_CALL_AENS_UPDATE        ) -> 0;
primop_base_gas_cost(?PRIM_CALL_AENS_TRANSFER      ) -> 0;
primop_base_gas_cost(?PRIM_CALL_AENS_REVOKE        ) -> 0;
primop_base_gas_cost(?PRIM_CALL_MAP_EMPTY          ) -> 0;
primop_base_gas_cost(?PRIM_CALL_MAP_GET            ) -> 0;
primop_base_gas_cost(?PRIM_CALL_MAP_PUT            ) -> 0;
primop_base_gas_cost(?PRIM_CALL_MAP_DELETE         ) -> 0;
primop_base_gas_cost(?PRIM_CALL_MAP_SIZE           ) -> 0;
primop_base_gas_cost(?PRIM_CALL_MAP_TOLIST         ) -> 0.

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
