-module(aec_governance).

%% API
-export([sorted_protocol_versions/0,
         protocols/0,
         key_blocks_to_check_difficulty_count/0,
         median_timestamp_key_blocks/0,
         expected_block_mine_rate/0,
         block_mine_reward/1,
         block_gas_limit/0,
         tx_base_gas/1,
         byte_gas/0,
         beneficiary_reward_delay/0,
         locked_coins_holder_account/0,
         minimum_gas_price/0,
         name_preclaim_expiration/0,
         name_claim_locked_fee/0,
         name_claim_max_expiration/0,
         name_protection_period/0,
         name_claim_preclaim_delta/0,
         name_registrars/0,
         micro_block_cycle/0,
         accepted_future_block_time_shift/0,
         fraud_report_reward/1,
         state_gas_per_block/1,
         primop_base_gas/1,
         add_network_id/1,
         add_network_id_last/1,
         get_network_id/0,
         contributors_messages_hash/0,
         vm_gas_table/0]).

-export_type([protocols/0]).

-include("blocks.hrl").
-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include_lib("aecore/include/hard_forks.hrl").

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
-define(POF_REWARD_DIVIDER, 20). %% 5% of the coinbase reward
-define(BENEFICIARY_REWARD_DELAY, 180). %% in key blocks / generations
-define(MICRO_BLOCK_CYCLE, 3000). %% in msecs

-define(ACCEPTED_FUTURE_BLOCK_TIME_SHIFT, (?EXPECTED_BLOCK_MINE_RATE_MINUTES * 3 * 60 * 1000)). %% 9 min

-define(ORACLE_STATE_GAS_PER_YEAR, 32000). %% 32000 as `GCREATE` i.e. an oracle-related state object costs per year as much as it costs to indefinitely create an account.

%% Account where locked / burnt coins are sent to.
-define(LOCKED_COINS_ACCOUNT, <<0:32/unit:8>>).

%% Maps consensus protocol version to minimum height at which such
%% version is effective.  The height must be strictly increasing with
%% the version.
-type protocols() :: #{Version::non_neg_integer() => aec_blocks:height()}.

-define(ROMA_PROTOCOL_HEIGHT, 0).
-define(MINERVA_PROTOCOL_HEIGHT, 1).

sorted_protocol_versions() ->
    lists:sort(maps:keys(protocols())).

protocols() ->
    case aeu_env:user_map([<<"chain">>, <<"hard_forks">>]) of
        undefined ->
            #{ ?ROMA_PROTOCOL_VSN     => ?ROMA_PROTOCOL_HEIGHT
             , ?MINERVA_PROTOCOL_VSN  => ?MINERVA_PROTOCOL_HEIGHT
             };
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

block_mine_reward(0) ->
    %% No mining reward for the genesis block
    0;
block_mine_reward(Height) when is_integer(Height), Height > 0 ->
    aec_coinbase:coinbase_at_height(Height).

%% In Ethereum, block gas limit is changed in every block. The new block gas
%% limit is decided by algorithm and vote by miners.
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
state_gas_per_block(oracle_register_tx) -> {?ORACLE_STATE_GAS_PER_YEAR, ?EXPECTED_BLOCKS_IN_A_YEAR_FLOOR};
state_gas_per_block(oracle_extend_tx)   -> state_gas_per_block(oracle_register_tx);
state_gas_per_block(oracle_query_tx)    -> state_gas_per_block(oracle_register_tx);
state_gas_per_block(oracle_response_tx) -> state_gas_per_block(oracle_register_tx).

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
primop_base_gas(?PRIM_CALL_MAP_TOLIST         ) -> 0;
primop_base_gas(?PRIM_CALL_CRYPTO_ECVERIFY    ) -> 1300;    %% 700 for call + 1300 = 2000
primop_base_gas(?PRIM_CALL_CRYPTO_SHA3           ) -> 30;   %% Same as gas cost for SHA3 instruction
primop_base_gas(?PRIM_CALL_CRYPTO_SHA256         ) -> 30;
primop_base_gas(?PRIM_CALL_CRYPTO_BLAKE2B        ) -> 30;
primop_base_gas(?PRIM_CALL_CRYPTO_SHA256_STRING  ) -> 30;
primop_base_gas(?PRIM_CALL_CRYPTO_BLAKE2B_STRING ) -> 30.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Naming system variables

name_preclaim_expiration() ->
    300.

name_claim_locked_fee() ->
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

fraud_report_reward(Height) ->
    Coinbase = block_mine_reward(Height),
    %% Assert that the coinbase is always even dividable.
    0 = (Coinbase rem ?POF_REWARD_DIVIDER),
    Coinbase div ?POF_REWARD_DIVIDER.

-spec add_network_id(binary()) -> binary().
add_network_id(SerializedTransaction) ->
    NetworkId = get_network_id(),
    <<NetworkId/binary, SerializedTransaction/binary>>.

-spec add_network_id_last(binary()) -> binary().
add_network_id_last(Payload) ->
    NetworkId = get_network_id(),
    <<Payload/binary, NetworkId/binary>>.

get_network_id() ->
    aeu_env:user_config_or_env([<<"fork_management">>, <<"network_id">>],
                                aecore, network_id, ?NETWORK_ID).

-spec contributors_messages_hash() -> binary().
contributors_messages_hash() ->
    %% This is generated using messages_hash tool:
    %%  > cd ~/aeternity/node
    %%  > bin/epoch messages_hash
    <<158,79,89,57,49,136,245,229,211,220,44,167,79,151,117,42,149,176,13,160,196,61,156,179,95,106,250,133,59,93,3,170>>.

-spec locked_coins_holder_account() -> aec_keys:pubkey().
locked_coins_holder_account() -> ?LOCKED_COINS_ACCOUNT.

vm_gas_table() ->
    #{ %% Nothing paid for operations of the set Wzero.
       'GZERO' => 0

       %% Amount of gas to pay for operations of the set Wbase.
     , 'GBASE' => 2

       %% Amount of gas to pay for operations of the set Wverylow.
     , 'GVERYLOW' => 3

       %% Amount of gas to pay for operations of the set Wlow.
     , 'GLOW' => 5

       %% Amount of gas to pay for operations of the set Wmid.
     , 'GMID' => 8

       %% Amount of gas to pay for operations of the set Whigh.
     , 'GHIGH' => 10

       %% Amount of gas to pay for a EXTCODESIZE operation.
       %% (Amended name and description compared to yellow paper.)
     , 'GEXTCODESIZE' => 700

       %% Base amount of gas to pay for a EXTCODECOPY operation.
       %% (Amended name and description compared to yellow paper.)
     , 'GEXTCODECOPY' => 700

       %% Amount of gas to pay for a BALANCE operation.
     , 'GBALANCE' => 400

       %% Paid for a SLOAD operation.
     , 'GSLOAD' => 200

       %% Paid for a JUMPDEST operation.
     , 'GJUMPDEST' => 1

       %% Paid for an SSTORE operation when the storage value is set to
       %% non-zero from zero.
     , 'GSSET' => 20000

       %% Paid for an SSTORE operation when the storage value’s zeroness
       %% remains unchanged or is set to zero.
     , 'GSRESET' => 5000

       %% Refund given (added into refund counter) when the storage value is
       %% set to zero from non-zero.
     , 'RSCLEAR' => 15000

       %% Refund given (added into refund counter) for self-destructing an
       %% account.
     , 'RSELFDESTRUCT' => 24000

       %% Amount of gas to pay for a SELFDESTRUCT operation.
     , 'GSELFDESTRUCT' => 5000

       %% Paid for a CREATE operation.
     , 'GCREATE' => 32000

       %% Paid per byte for a CREATE operation to succeed in placing code
       %% into state.
     , 'GCODEDEPOSIT' => 200

       %% Paid for a CALL operation.
     , 'GCALL' => 700

       %% Paid for a non-zero value transfer as part of the CALL operation.
     , 'GCALLVALUE' => 9000

       %% A stipend for the called contract subtracted from Gcallvalue for a
       %% non-zero value transfer.
     , 'GCALLSTIPEND' => 2300

       %% Paid for a CALL or SELFDESTRUCT operation which creates an account.
     , 'GNEWACCOUNT' => 25000

       %% Partial payment for an EXP operation.
     , 'GEXP' => 10

       %% Partial payment when multiplied by dlog256(exponent)e for the EXP
       %% operation.
     , 'GEXPBYTE' => 50

       %% Paid for every additional word when expanding memory.
     , 'GMEMORY' => 3

       %% Paid by all contract-creating transactions after the Homestead
       %% transition.
       %% , 'GTXCREATE' => 32000

       %% Unused. Defined in the yellow paper.
       %% Paid for every zero byte of data or code for a transaction.
       %% , 'GTXDATAZERO' => 4

       %% Unused. Defined in the yellow paper.
       %% Paid for every non-zero byte of data or code for a transaction.
       %% , 'GTXDATANONZERO' => 68

       %% Unused. Defined in the yellow paper.
       %% Paid for every transaction.
       %% , 'GTRANSACTION' => 21000

       %% Partial payment for a LOG operation.
     , 'GLOG' => 375

       %% Paid for each byte in a LOG operation’s data.
     , 'GLOGDATA' => 8

       %% Paid for each topic of a LOG operation.
     , 'GLOGTOPIC' => 375

       %% Paid for each SHA3 operation.
     , 'GSHA3' => 30

       %% Paid for each word (rounded up) for input data to a SHA3 operation.
     , 'GSHA3WORD' => 6

       %% Partial payment for *COPY operations, multiplied by words copied,
       %% rounded up.
     , 'GCOPY' => 3

       %% Payment for BLOCKHASH operation.
     , 'GBLOCKHASH' => 20 %% Called GasExtStep in go implementation.

       %% Unused. Defined in the yellow paper.
       %% The quadratic coefficient of the input sizes of the exponentiation-over-modulo precompiled contract.
       %% , 'GQUADDIVISOR' => 100

       %% --------
       %% From:
       %%
       %% * The [yellow paper](https://github.com/ethereum/yellowpaper/blob/74f7c7e29eead861e724159fa985555b7af0f126/Paper.tex#L1778-L1816) section "Fee Schedule".
       %%
       %% * The go implementation constants applicable to Byzantium phase:
       %%   * https://github.com/ethereum/go-ethereum/blob/8bbe72075e4e16442c4e28d999edee12e294329e/core/vm/gas.go#L27-L36
       %%   * https://github.com/ethereum/go-ethereum/blob/8bbe72075e4e16442c4e28d999edee12e294329e/params/protocol_params.go#L27-L87
       %%   * https://github.com/ethereum/go-ethereum/blob/8bbe72075e4e16442c4e28d999edee12e294329e/params/gas_table.go#L69-L77
       %%
       %%   ```
       %%   GasQuickStep   uint64 = 2
       %%   GasFastestStep uint64 = 3
       %%   GasFastStep    uint64 = 5
       %%   GasMidStep     uint64 = 8
       %%   GasSlowStep    uint64 = 10
       %%   GasExtStep     uint64 = 20
       %%
       %%   GasReturn       uint64 = 0
       %%   GasStop         uint64 = 0
       %%   GasContractByte uint64 = 200
       %%   ```
       %%
       %%   ```
       %%   ExpByteGas            uint64 = 10    // Times ceil(log256(exponent)) for the EXP instruction.
       %%   SloadGas              uint64 = 50    // Multiplied by the number of 32-byte words that are copied (round up) for any *COPY operation and added.
       %%   CallValueTransferGas  uint64 = 9000  // Paid for CALL when the value transfer is non-zero.
       %%   CallNewAccountGas     uint64 = 25000 // Paid for CALL when the destination address didn't exist prior.
       %%   TxGas                 uint64 = 21000 // Per transaction not creating a contract. NOTE: Not payable on data of calls between transactions.
       %%   TxGasContractCreation uint64 = 53000 // Per transaction that creates a contract. NOTE: Not payable on data of calls between transactions.
       %%   TxDataZeroGas         uint64 = 4     // Per byte of data attached to a transaction that equals zero. NOTE: Not payable on data of calls between transactions.
       %%   QuadCoeffDiv          uint64 = 512   // Divisor for the quadratic particle of the memory cost equation.
       %%   LogDataGas            uint64 = 8     // Per byte in a LOG* operation's data.
       %%   CallStipend           uint64 = 2300  // Free gas given at beginning of call.
       %%
       %%   Sha3Gas     uint64 = 30 // Once per SHA3 operation.
       %%   Sha3WordGas uint64 = 6  // Once per word of the SHA3 operation's data.
       %%
       %%   SstoreSetGas    uint64 = 20000 // Once per SLOAD operation.
       %%   SstoreResetGas  uint64 = 5000  // Once per SSTORE operation if the zeroness changes from zero.
       %%   SstoreClearGas  uint64 = 5000  // Once per SSTORE operation if the zeroness doesn't change.
       %%   SstoreRefundGas uint64 = 15000 // Once per SSTORE operation if the zeroness changes to zero.
       %%
       %%   NetSstoreNoopGas  uint64 = 200   // Once per SSTORE operation if the value doesn't change.
       %%   NetSstoreInitGas  uint64 = 20000 // Once per SSTORE operation from clean zero.
       %%   NetSstoreCleanGas uint64 = 5000  // Once per SSTORE operation from clean non-zero.
       %%   NetSstoreDirtyGas uint64 = 200   // Once per SSTORE operation from dirty.
       %%
       %%   NetSstoreClearRefund      uint64 = 15000 // Once per SSTORE operation for clearing an originally existing storage slot
       %%   NetSstoreResetRefund      uint64 = 4800  // Once per SSTORE operation for resetting to the original non-zero value
       %%   NetSstoreResetClearRefund uint64 = 19800 // Once per SSTORE operation for resetting to the original zero value
       %%
       %%   JumpdestGas      uint64 = 1     // Refunded gas, once per SSTORE operation if the zeroness changes to zero.
       %%   ...
       %%   CallGas          uint64 = 40    // Once per CALL operation & message call transaction.
       %%   CreateDataGas    uint64 = 200   //
       %%   ...
       %%   ExpGas           uint64 = 10    // Once per EXP instruction
       %%   LogGas           uint64 = 375   // Per LOG* operation.
       %%   CopyGas          uint64 = 3     //
       %%   ...
       %%   TierStepGas      uint64 = 0     // Once per operation, for a selection of them.
       %%   LogTopicGas      uint64 = 375   // Multiplied by the * of the LOG*, per LOG transaction. e.g. LOG0 incurs 0 * c_txLogTopicGas, LOG4 incurs 4 * c_txLogTopicGas.
       %%   CreateGas        uint64 = 32000 // Once per CREATE operation & contract-creation transaction.
       %%   Create2Gas       uint64 = 32000 // Once per CREATE2 operation
       %%   SuicideRefundGas uint64 = 24000 // Refunded following a suicide operation.
       %%   MemoryGas        uint64 = 3     // Times the address of the (highest referenced byte in memory + 1). NOTE: referencing happens on read, write and in instructions such as RETURN and CALL.
       %%   TxDataNonZeroGas uint64 = 68    // Per byte of data attached to a transaction that is not equal to zero. NOTE: Not payable on data of calls between transactions.
       %%
       %%   ...
       %%
       %%   // Precompiled contract gas prices
       %%
       %%   EcrecoverGas            uint64 = 3000   // Elliptic curve sender recovery gas price
       %%   Sha256BaseGas           uint64 = 60     // Base price for a SHA256 operation
       %%   Sha256PerWordGas        uint64 = 12     // Per-word price for a SHA256 operation
       %%   Ripemd160BaseGas        uint64 = 600    // Base price for a RIPEMD160 operation
       %%   Ripemd160PerWordGas     uint64 = 120    // Per-word price for a RIPEMD160 operation
       %%   IdentityBaseGas         uint64 = 15     // Base price for a data copy operation
       %%   IdentityPerWordGas      uint64 = 3      // Per-work price for a data copy operation
       %%   ModExpQuadCoeffDiv      uint64 = 20     // Divisor for the quadratic particle of the big int modular exponentiation
       %%   Bn256AddGas             uint64 = 500    // Gas needed for an elliptic curve addition
       %%   Bn256ScalarMulGas       uint64 = 40000  // Gas needed for an elliptic curve scalar multiplication
       %%   Bn256PairingBaseGas     uint64 = 100000 // Base price for an elliptic curve pairing check
       %%   Bn256PairingPerPointGas uint64 = 80000  // Per-point price for an elliptic curve pairing check
       %%   ```
       %%
       %%   ```
       %%   // CreateBySuicide occurs when the
       %%   // refunded account is one that does
       %%   // not exist. This logic is similar
       %%   // to call. ...
       %%
       %%   ...
       %%
       %%   GasTableEIP158 = GasTable{
       %%    ExtcodeSize: 700,
       %%    ExtcodeCopy: 700,
       %%    Balance:     400,
       %%    SLoad:       200,
       %%    Calls:       700,
       %%    Suicide:     5000,
       %%    ExpByte:     50,
       %%
       %%    CreateBySuicide: 25000,
       %%   }
       %%   ```
     }.
