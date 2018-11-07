-module(aec_governance).

%% API
-export([sorted_protocol_versions/0,
         protocols/0,
         key_blocks_to_check_difficulty_count/0,
         median_timestamp_key_blocks/0,
         expected_block_mine_rate/0,
         block_mine_reward/0,
         block_gas_limit/0,
         tx_base_gas/1,
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
         state_gas_per_block/1,
         primop_base_gas/1,
         add_network_id/1,
         get_network_id/0,
         contributors_messages_hash/0]).

-export_type([protocols/0]).

-include("blocks.hrl").
-include_lib("aebytecode/include/aeb_opcodes.hrl").

-define(SORTED_VERSIONS, lists:sort(maps:keys(?PROTOCOLS))).
-define(PROTOCOLS,
        #{?PROTOCOL_VERSION => ?GENESIS_HEIGHT
         }).

-define(NETWORK_ID, <<"ae_mainnet">>).
-define(BLOCKS_TO_CHECK_DIFFICULTY_COUNT, 17).
-define(TIMESTAMP_MEDIAN_BLOCKS, 11).
-define(EXPECTED_BLOCK_MINE_RATE_MINUTES, 3).
-define(EXPECTED_BLOCK_MINE_RATE, (?EXPECTED_BLOCK_MINE_RATE_MINUTES * 60 * 1000)). %% 60secs * 1000ms * 3 = 180000msecs
-define(EXPECTED_BLOCKS_IN_A_YEAR_FLOOR, (175200 = ((60 * 24 * 365) div ?EXPECTED_BLOCK_MINE_RATE_MINUTES))).
-define(BLOCK_MINE_REWARD, 10000000000000000000).
%% Ethereum's gas limit is 8 000 000 and block time ~15s.
%% For 3s block time it's 1 600 000 (5x less).
%% But we can improve and use 6 million (this allows reasonable
%% size contracts to fit in a microblock at all)
-define(BLOCK_GAS_LIMIT, 6000000).
-define(TX_BASE_GAS, 15000).
%% Gas for 1 byte of a serialized tx.
-define(BYTE_GAS, 20).
-define(POF_REWARD       , 500000000000000000). %% (?BLOCK_MINE_REWARD / 100) * 5
-define(BENEFICIARY_REWARD_DELAY, 180). %% in key blocks / generations
-define(MICRO_BLOCK_CYCLE, 3000). %% in msecs

-define(ACCEPTED_FUTURE_BLOCK_TIME_SHIFT, (?EXPECTED_BLOCK_MINE_RATE_MINUTES * 3 * 60 * 1000)). %% 9 min

-define(ORACLE_STATE_GAS_PER_YEAR, 32000). %% 32000 as `?GCREATE` in `aevm_gas.hrl` i.e. an oracle-related state object costs per year as much as it costs to indefinitely create an account.

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
    application:get_env(aecore, block_gas_limit, ?BLOCK_GAS_LIMIT).

tx_base_gas(contract_call_tx) ->  30 * ?TX_BASE_GAS;
tx_base_gas(contract_create_tx) -> 5 * ?TX_BASE_GAS;
tx_base_gas(spend_tx) -> ?TX_BASE_GAS;
tx_base_gas(channel_deposit_tx) -> ?TX_BASE_GAS;
tx_base_gas(channel_close_mutual_tx) -> ?TX_BASE_GAS;
tx_base_gas(channel_close_solo_tx) -> ?TX_BASE_GAS;
tx_base_gas(channel_create_tx) -> ?TX_BASE_GAS;
tx_base_gas(channel_force_progress_tx) -> ?TX_BASE_GAS;
tx_base_gas(channel_slash_tx) -> ?TX_BASE_GAS;
tx_base_gas(channel_settle_tx) -> ?TX_BASE_GAS;
tx_base_gas(channel_snapshot_solo_tx) -> ?TX_BASE_GAS;
tx_base_gas(channel_withdraw_tx) -> ?TX_BASE_GAS;
tx_base_gas(name_preclaim_tx) -> ?TX_BASE_GAS;
tx_base_gas(name_revoke_tx) -> ?TX_BASE_GAS;
tx_base_gas(name_transfer_tx) -> ?TX_BASE_GAS;
tx_base_gas(name_claim_tx) -> ?TX_BASE_GAS;
tx_base_gas(name_update_tx) -> ?TX_BASE_GAS;
tx_base_gas(oracle_extend_tx) -> ?TX_BASE_GAS;
tx_base_gas(oracle_query_tx) -> ?TX_BASE_GAS;
tx_base_gas(oracle_register_tx) -> ?TX_BASE_GAS;
tx_base_gas(oracle_response_tx) -> ?TX_BASE_GAS;
tx_base_gas(_) ->
    %% never accept unknown transaction types
    block_gas_limit().

byte_gas() ->
    ?BYTE_GAS.

minimum_tx_fee() ->
    1.

minimum_gas_price() ->
    1.

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

%% Reoccurring gas, meant to be paid up front.
%%
%% Expressed as a fraction because otherwise it would become too large
%% when multiplied by the number of key blocks.
-spec state_gas_per_block(atom()) -> {Part::pos_integer(), Whole::pos_integer()}.
state_gas_per_block(oracle_registration) -> {?ORACLE_STATE_GAS_PER_YEAR, ?EXPECTED_BLOCKS_IN_A_YEAR_FLOOR};
state_gas_per_block(oracle_extension)    -> state_gas_per_block(oracle_registration);
state_gas_per_block(oracle_query)        -> state_gas_per_block(oracle_registration);
state_gas_per_block(oracle_response)     -> state_gas_per_block(oracle_registration).

%% As primops are not meant to be called from contract call
%% transactions, calling a primop costs (apart from the fees for the
%% calling contract create transaction or contract call transaction)
%% at least the gas of the call instruction (e.g. `CALL`) used
%% for calling the primop.
primop_base_gas(?PRIM_CALL_SPEND              ) -> 12000;
primop_base_gas(?PRIM_CALL_ORACLE_REGISTER    ) -> 12000;
primop_base_gas(?PRIM_CALL_ORACLE_QUERY       ) -> 12000;
primop_base_gas(?PRIM_CALL_ORACLE_RESPOND     ) -> 12000;
primop_base_gas(?PRIM_CALL_ORACLE_EXTEND      ) -> 12000;
primop_base_gas(?PRIM_CALL_ORACLE_GET_ANSWER  ) -> 12000;
primop_base_gas(?PRIM_CALL_ORACLE_GET_QUESTION) -> 12000;
primop_base_gas(?PRIM_CALL_ORACLE_QUERY_FEE   ) -> 12000;
primop_base_gas(?PRIM_CALL_AENS_RESOLVE       ) -> 12000;
primop_base_gas(?PRIM_CALL_AENS_PRECLAIM      ) -> 12000;
primop_base_gas(?PRIM_CALL_AENS_CLAIM         ) -> 12000;
primop_base_gas(?PRIM_CALL_AENS_UPDATE        ) -> 12000;
primop_base_gas(?PRIM_CALL_AENS_TRANSFER      ) -> 12000;
primop_base_gas(?PRIM_CALL_AENS_REVOKE        ) -> 12000;
primop_base_gas(?PRIM_CALL_MAP_EMPTY          ) -> 0;
primop_base_gas(?PRIM_CALL_MAP_GET            ) -> 0;
primop_base_gas(?PRIM_CALL_MAP_PUT            ) -> 0;
primop_base_gas(?PRIM_CALL_MAP_DELETE         ) -> 0;
primop_base_gas(?PRIM_CALL_MAP_SIZE           ) -> 0;
primop_base_gas(?PRIM_CALL_MAP_TOLIST         ) -> 0.

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

-spec name_registrars() -> list(binary()).
name_registrars() ->
    [<<"test">>].

fraud_report_reward() ->
    ?POF_REWARD.

-spec add_network_id(binary()) -> binary().
add_network_id(SerializedTransaction) ->
    NetworkId = get_network_id(),
    <<NetworkId/binary, SerializedTransaction/binary>>.

get_network_id() ->
    aeu_env:user_config_or_env([<<"fork_management">>, <<"network_id">>],
                                aecore, network_id, ?NETWORK_ID).

-spec contributors_messages_hash() -> binary().
contributors_messages_hash() ->
    %% This is generated using messages_hash tool:
    %%  > cd /tmp/node
    %%  > bin/epoch messages_hash
    <<104,78,40,169,217,133,126,163,45,143,180,127,97,10,150,252,190,28,13,188,59,135,7,154,52,86,197,135,46,225,130,136>>.
