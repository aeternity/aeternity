%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Pins the aefa_engine_state:aefa_stores_for_protocol/1 dispatch
%%%    boundary, and regression-tests that pre-Iris finalize/1 does not
%%%    `undef` (aefa_stores_lima has no terms_to_finalize/1; finalize/1
%%%    must call the live aefa_stores module directly for that step).
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_engine_state_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("apps/aecontract/include/aecontract.hrl").
-include_lib("apps/aecontract/include/hard_forks.hrl").

%%%===================================================================
%%% 1. Dispatch-boundary test
%%%===================================================================

%% Pins the protocol -> store-module dispatch boundary.
aefa_stores_for_protocol_boundary_test_() ->
    Cases =
        [ {?ROMA_PROTOCOL_VSN,    aefa_stores_lima}
        , {?MINERVA_PROTOCOL_VSN, aefa_stores_lima}
        , {?FORTUNA_PROTOCOL_VSN, aefa_stores_lima}
        , {?LIMA_PROTOCOL_VSN,    aefa_stores_lima}   %% last pre-Iris protocol
        , {?IRIS_PROTOCOL_VSN,    aefa_stores_ceres}  %% exact boundary: lima -> ceres
        , {?CERES_PROTOCOL_VSN,   aefa_stores_ceres}
        , {?SALUS_PROTOCOL_VSN,   aefa_stores}        %% exact boundary: ceres -> live
        ],
    [ {case_name(Protocol, Expected),
       fun() -> ?assertEqual(Expected, aefa_engine_state:aefa_stores_for_protocol(Protocol)) end}
      || {Protocol, Expected} <- Cases ].

%% The Salus clause is a `>=` guard, not `==`: future protocols must keep
%% resolving to the live module without a code change.
aefa_stores_for_protocol_forward_compat_test() ->
    ?assertEqual(aefa_stores, aefa_engine_state:aefa_stores_for_protocol(?SALUS_PROTOCOL_VSN + 1)),
    ?assertEqual(aefa_stores, aefa_engine_state:aefa_stores_for_protocol(100)).

case_name(Protocol, Expected) ->
    lists:flatten(io_lib:format("protocol ~p dispatches to ~p", [Protocol, Expected])).

%%%===================================================================
%%% 2. Regression: pre-Iris (Lima) finalize must not `undef`
%%%===================================================================

-define(SR1_OWNER_PUBKEY,    <<16#B0:256>>).
-define(SR1_CONTRACT_PUBKEY, <<16#C0:256>>).
-define(SR1_CALLER_PUBKEY,   <<16#CA:256>>).
-define(SR1_STORE_POS, 1).

%% Must not raise {undef, [{aefa_stores_lima, terms_to_finalize, ...}]}.
sr1_pre_iris_finalize_does_not_crash_test() ->
    ES = lima_engine_state_with_dirty_store(),
    Result = aefa_engine_state:finalize(ES),
    ?assertMatch({ok, _}, Result).

%% finalize/1 also writes the dirty value through unchanged.
sr1_pre_iris_finalize_produces_correct_result_test() ->
    ES = lima_engine_state_with_dirty_store(),
    {ok, ES1} = aefa_engine_state:finalize(ES),
    ChainApi1 = aefa_engine_state:chain_api(ES1),
    {OnChainStore, _ChainApi2} = aefa_chain_api:contract_store(?SR1_CONTRACT_PUBKEY, ChainApi1),
    ReadBack = aefa_stores:put_contract_store(?SR1_CONTRACT_PUBKEY, OnChainStore, aefa_stores:new()),
    %% Reads through the live aefa_stores module directly (bypassing
    %% protocol dispatch), whose find_value/3 returns a 4-tuple.
    ?assertMatch({ok, _, _, _}, aefa_stores:find_value(?SR1_CONTRACT_PUBKEY, ?SR1_STORE_POS, ReadBack)),
    {ok, ReadBackVal, _, _Bytes} = aefa_stores:find_value(?SR1_CONTRACT_PUBKEY, ?SR1_STORE_POS, ReadBack),
    ?assertEqual(sr1_test_value(), ReadBackVal).

%% -- helpers ---------------------------------------------------------

%% Engine state for a pre-Iris (Lima, protocol 4) tx_env, one contract
%% registered in the trees, with one dirty store register.
lima_engine_state_with_dirty_store() ->
    Trees = trees_with_one_contract(),
    TxEnv = aetx_env:tx_env(_Height = 1, ?LIMA_PROTOCOL_VSN),
    ChainApi = aefa_chain_api:new(#{ gas_price => 1
                                   , fee        => 0
                                   , origin     => ?SR1_CALLER_PUBKEY
                                   , trees      => Trees
                                   , tx_env     => TxEnv
                                   }),
    Stores0 = aefa_stores:new(),
    Stores1 = aefa_stores:put_contract_store(?SR1_CONTRACT_PUBKEY,
                                              aefa_stores:initial_contract_store(),
                                              Stores0),
    Stores2 = aefa_stores:put_value(?SR1_CONTRACT_PUBKEY, ?SR1_STORE_POS,
                                     sr1_test_value(), Stores1),
    aefa_engine_state:new(_Gas = 1000000, _Value = 0,
                           #{caller => ?SR1_CALLER_PUBKEY},
                           Stores2, ChainApi, #{}, ?VM_FATE_SOPHIA_2).

sr1_test_value() -> aeb_fate_data:make_integer(424242).

trees_with_one_contract() ->
    CTVersion = #{vm => ?VM_FATE_SOPHIA_2, abi => ?ABI_FATE_SOPHIA_1},
    Contract0 = aect_contracts:new(?SR1_OWNER_PUBKEY, _Nonce = 1, CTVersion,
                                    _Code = <<"unused-in-this-test">>, _Deposit = 0),
    Contract1 = aect_contracts:set_pubkey(?SR1_CONTRACT_PUBKEY, Contract0),
    Account = aec_accounts:new(?SR1_CONTRACT_PUBKEY, 0),
    Trees0 = aec_trees:new_without_backend(),
    Trees1 = aec_trees:set_contracts(Trees0,
                aect_state_tree:insert_contract(Contract1, aec_trees:contracts(Trees0))),
    aec_trees:set_accounts(Trees1,
                aec_accounts_trees:enter(Account, aec_trees:accounts(Trees1))).
