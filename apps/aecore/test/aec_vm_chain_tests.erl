%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc EUnit tests for aec_vm_chain
%%% @end
%%%-------------------------------------------------------------------
-module(aec_vm_chain_tests).

-include("hard_forks.hrl").
-include("blocks.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("apps/aecontract/src/aecontract.hrl").

backwards_compatibility_test_() ->
    [{"Oracle fee on unknown oracle", fun test_unknown_oracle_fee/0}
    ].

test_unknown_oracle_fee() ->
    Height       = 42,
    Time         = 4711,
    Beneficiary  = <<123:?BENEFICIARY_PUB_BYTES/unit:8>>,
    Difficulty   = aec_block_genesis:genesis_difficulty(),
    KeyHash      = <<123:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
    TxEnvROMA    = aetx_env:contract_env(Height, ?ROMA_PROTOCOL_VSN, Time,
                                        Beneficiary, Difficulty, KeyHash),
    TxEnvMINERVA = aetx_env:contract_env(Height, ?MINERVA_PROTOCOL_VSN, Time,
                                        Beneficiary, Difficulty, KeyHash),
    Trees        = aec_trees:new_without_backend(),
    ContractAccount = <<321:?BENEFICIARY_PUB_BYTES/unit:8>>,

    StateROMA     = aec_vm_chain:new_state(Trees, TxEnvROMA, ContractAccount,
                                           ?AEVM_01_Sophia_01),
    StateMINERVA1 = aec_vm_chain:new_state(Trees, TxEnvMINERVA, ContractAccount,
                                           ?AEVM_01_Sophia_01),
    StateMINERVA2 = aec_vm_chain:new_state(Trees, TxEnvMINERVA, ContractAccount,
                                           ?AEVM_02_Sophia_01),
    BogusOracle   = Beneficiary,

    ?assertEqual({ok, none},
                 aec_vm_chain:oracle_query_fee(BogusOracle, StateROMA)),
    ?assertEqual({ok, none},
                 aec_vm_chain:oracle_query_fee(BogusOracle, StateMINERVA1)),
    ?assertEqual({error, no_such_oracle},
                 aec_vm_chain:oracle_query_fee(BogusOracle, StateMINERVA2)),

    ok.
