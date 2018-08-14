%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc CT test suite for AE Contracts
%%% @end
%%%-------------------------------------------------------------------
-module(aecontract_SUITE).

%% common_test exports
-export([ all/0
        , groups/0
        ]).

%% test case exports
-export([ call_contract/1
        , call_contract_with_gas_price_zero/1
        , call_contract_error_value/1
        , call_contract_negative_insufficient_funds/1
        , call_contract_negative/1
        , create_contract/1
        , create_contract_with_gas_price_zero/1
        , create_contract_init_error/1
        , create_contract_negative/1
        , state_tree/1
        , sophia_identity/1
        , sophia_state/1
        , sophia_spend/1
        , sophia_oracles/1
        , sophia_oracles_qfee__basic/1
        , sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_is_awarded_to_oracle/1
        , sophia_oracles_qfee__tx_value_above_qfee_in_query_is_awarded_to_oracle/1
        , sophia_oracles_qfee__qfee_in_query_below_qfee_in_oracle_errs/1
        , sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle/1
        , sophia_oracles_qfee__query_tx_value_below_qfee_does_not_take_from_poor_oracle/1
        , sophia_oracles_qfee__query_tx_value_below_qfee_does_not_take_from_rich_oracle_thanks_to_contract_check/1
        , sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_takes_from_rich_oracle/1
        , sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_does_not_take_from_poor_oracle/1
        , sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_does_not_take_from_rich_oracle_thanks_to_contract_check/1
        , sophia_oracles_qfee__error_after_primop/1
        , sophia_oracles_qfee__basic__remote/1
        , sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_is_awarded_to_oracle__remote/1
        , sophia_oracles_qfee__tx_value_above_qfee_in_query_is_awarded_to_oracle__remote/1
        , sophia_oracles_qfee__qfee_in_query_below_qfee_in_oracle_errs__remote/1
        , sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle__remote/1
        , sophia_oracles_qfee__query_tx_value_below_qfee_does_not_take_from_poor_oracle__remote/1
        , sophia_oracles_qfee__query_tx_value_below_qfee_does_not_take_from_rich_oracle_thanks_to_contract_check__remote/1
        , sophia_oracles_qfee__remote_contract_query_value_below_qfee_takes_from_rich_oracle__remote/1
        , sophia_oracles_qfee__remote_contract_query_value_below_qfee_does_not_take_from_poor_oracle__remote/1
        , sophia_oracles_qfee__remote_contract_query_value_below_qfee_does_not_take_from_rich_oracle_thanks_to_contract_check__remote/1
        , sophia_oracles_qfee__inner_error_after_primop__remote/1
        , sophia_oracles_qfee__outer_error_after_primop__remote/1
        , sophia_maps/1
        , sophia_variant_types/1
        , sophia_fundme/1
        , sophia_aens/1
        , create_store/1
        , update_store/1
        , read_store/1
        , store_zero_value/1
        , merge_new_zero_value/1
        , merge_missing_keys/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("apps/aecore/include/blocks.hrl").
-include_lib("apps/aecontract/src/aecontract.hrl").

-define(MINER_PUBKEY, <<12345:?MINER_PUB_BYTES/unit:8>>).

%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, all_tests}
    ].

groups() ->
    [ {all_tests, [sequence], [ {group, transactions}
                              , {group, state_tree}
                              , {group, sophia}
                              , {group, store}
                              ]}
    , {transactions, [], [ create_contract
                         , create_contract_with_gas_price_zero
                         , create_contract_init_error
                         , create_contract_negative
                         , call_contract
                         , call_contract_with_gas_price_zero
                         , call_contract_error_value
                         , call_contract_negative_insufficient_funds
                         , call_contract_negative
                         ]}
    , {state_tree, [sequence], [ state_tree ]}
    , {sophia,     [sequence], [ sophia_identity,
                                 sophia_state,
                                 sophia_spend,
                                 sophia_oracles,
                                 {group, sophia_oracles_query_fee_happy_path},
                                 {group, sophia_oracles_query_fee_happy_path_remote},
                                 {group, sophia_oracles_query_fee_unhappy_path},
                                 {group, sophia_oracles_query_fee_unhappy_path_remote},
                                 sophia_maps,
                                 sophia_variant_types,
                                 sophia_fundme,
                                 sophia_aens ]}
    , {sophia_oracles_query_fee_happy_path, [],
       [ %% Test query fee handling from txs calling contract that calls oracle builtins.
         sophia_oracles_qfee__basic
       , sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_is_awarded_to_oracle
       , sophia_oracles_qfee__tx_value_above_qfee_in_query_is_awarded_to_oracle
       ]}
    , {sophia_oracles_query_fee_happy_path_remote, [],
       [ %% Test query fee handling from txs calling contract that calls contract that calls oracle builtins.
         sophia_oracles_qfee__basic__remote
       , sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_is_awarded_to_oracle__remote
       , sophia_oracles_qfee__tx_value_above_qfee_in_query_is_awarded_to_oracle__remote
       ]}
    , {sophia_oracles_query_fee_unhappy_path, [],
       [ %% Test query fee handling from txs calling contract that calls oracle builtins.
         sophia_oracles_qfee__qfee_in_query_below_qfee_in_oracle_errs
       , sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle
       , sophia_oracles_qfee__query_tx_value_below_qfee_does_not_take_from_poor_oracle
       , sophia_oracles_qfee__query_tx_value_below_qfee_does_not_take_from_rich_oracle_thanks_to_contract_check
       , sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_takes_from_rich_oracle
       , sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_does_not_take_from_poor_oracle
       , sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_does_not_take_from_rich_oracle_thanks_to_contract_check
       , sophia_oracles_qfee__error_after_primop
       ]}
    , {sophia_oracles_query_fee_unhappy_path_remote, [],
       [ %% Test query fee handling from txs calling contract that calls contract that calls oracle builtins.
         sophia_oracles_qfee__qfee_in_query_below_qfee_in_oracle_errs__remote
       , sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle__remote
       , sophia_oracles_qfee__query_tx_value_below_qfee_does_not_take_from_poor_oracle__remote
       , sophia_oracles_qfee__query_tx_value_below_qfee_does_not_take_from_rich_oracle_thanks_to_contract_check__remote
       , sophia_oracles_qfee__remote_contract_query_value_below_qfee_takes_from_rich_oracle__remote
       , sophia_oracles_qfee__remote_contract_query_value_below_qfee_does_not_take_from_poor_oracle__remote
       , sophia_oracles_qfee__remote_contract_query_value_below_qfee_does_not_take_from_rich_oracle_thanks_to_contract_check__remote
       , sophia_oracles_qfee__inner_error_after_primop__remote
       , sophia_oracles_qfee__outer_error_after_primop__remote
       ]}
    , {store, [sequence], [ create_store
                          , update_store
                          , read_store
                          , store_zero_value
                          , merge_new_zero_value
                          , merge_missing_keys
                          ]}
    ].

%%%===================================================================
%%% Create contract
%%%===================================================================

create_contract_negative(_Cfg) ->
    {PubKey, S1} = aect_test_utils:setup_new_account(aect_test_utils:new_state()),
    Trees        = aect_test_utils:trees(S1),
    PrivKey      = aect_test_utils:priv_key(PubKey, S1),
    CurrHeight   = 1,

    %% Test creating a bogus account
    {BadPubKey, BadS} = aect_test_utils:setup_new_account(aect_test_utils:new_state()),
    BadPrivKey        = aect_test_utils:priv_key(BadPubKey, BadS),
    RTx1      = aect_test_utils:create_tx(BadPubKey, S1),
    {error, S1} = sign_and_apply_transaction(RTx1, BadPrivKey, S1),
    {error, account_not_found} = aetx:check(RTx1, Trees, CurrHeight, ?PROTOCOL_VERSION),

    %% Insufficient funds
    S2     = aect_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aect_test_utils:trees(S2),
    RTx2   = aect_test_utils:create_tx(PubKey, S2),
    {error, S2} = sign_and_apply_transaction(RTx2, PrivKey, S2),
    {error, insufficient_funds} = aetx:check(RTx2, Trees2, CurrHeight, ?PROTOCOL_VERSION),

    %% Test too high account nonce
    RTx3 = aect_test_utils:create_tx(PubKey, #{nonce => 0}, S1),
    {error, S1} = sign_and_apply_transaction(RTx3, PrivKey, S1),
    {error, account_nonce_too_high} = aetx:check(RTx3, Trees, CurrHeight, ?PROTOCOL_VERSION),

    ok.

create_contract_init_error(_Cfg) ->
    S  = aect_test_utils:new_state(),
    S0 = aect_test_utils:setup_miner_account(?MINER_PUBKEY, S),
    {PubKey, S1} = aect_test_utils:setup_new_account(S0),
    PrivKey      = aect_test_utils:priv_key(PubKey, S1),

    Overrides = #{ call_data => aeso_abi:create_calldata(<<>>, "init", "()")
                 },
    Tx = aect_test_utils:create_tx(PubKey, Overrides, S1),

    %% Test that the create transaction is accepted
    {ok, S2} = sign_and_apply_transaction(Tx, PrivKey, S1),
    %% Check that the contract is not created
    ContractKey = aect_contracts:compute_contract_pubkey(PubKey, aetx:nonce(Tx)),
    {none, _} = lookup_contract_by_id(ContractKey, S2),
    %% Check that the contract init call is created
    ?assertMatch([_], aect_call_state_tree:to_list(aect_test_utils:calls(S2))),
    InitCallId = aect_call:id(PubKey, aetx:nonce(Tx), ContractKey),
    {value, InitCall} = aect_call_state_tree:lookup_call(ContractKey, InitCallId, aect_test_utils:calls(S2)),
    %% Check that the created init call has the correct details from the contract create tx
    ?assertEqual(PubKey, aect_call:caller_address(InitCall)),
    ?assertEqual(aetx:nonce(Tx), aect_call:caller_nonce(InitCall)),
    ?assertEqual(aect_create_tx:gas_price(aetx:tx(Tx)), aect_call:gas_price(InitCall)),
    %% Check that the created init call has the correct details not from the contract create tx
    ?assertEqual(ContractKey, aect_call:contract_address(InitCall)), %% Contract not created.
    ?assertMatch(X when X > 0, aect_call:gas_used(InitCall)),
    ?assertEqual(error, aect_call:return_type(InitCall)),
    _ = aect_call:return_value(InitCall),

    %% Check that contract create transaction sender got charged correctly.
    %%
    %% In particular, check that amount and deposit (are positive and)
    %% returned to the miner.
    ?assertMatch(D when D > 0, aect_create_tx:deposit(aetx:tx(Tx))), %% Check on test data.
    ?assertMatch(A when A > 0, aect_create_tx:amount(aetx:tx(Tx))), %% Check on test data.
    ?assertEqual(aec_accounts:balance(aect_test_utils:get_account(PubKey, S1))
                 - aect_create_tx:fee(aetx:tx(Tx))
                 - aect_create_tx:gas_price(aetx:tx(Tx)) * aect_call:gas_used(InitCall),
                 aec_accounts:balance(aect_test_utils:get_account(PubKey, S2))),
    ok.

create_contract(_Cfg) ->
    create_contract_(1).

create_contract_with_gas_price_zero(_Cfg) ->
    create_contract_(0).

create_contract_(ContractCreateTxGasPrice) ->
    S  = aect_test_utils:new_state(),
    S0 = aect_test_utils:setup_miner_account(?MINER_PUBKEY, S),
    {PubKey, S1} = aect_test_utils:setup_new_account(S0),
    PrivKey      = aect_test_utils:priv_key(PubKey, S1),

    IdContract   = aect_test_utils:compile_contract("contracts/identity.aes"),
    CallData     = aeso_abi:create_calldata(IdContract, "init", "()"),
    Overrides    = #{ code => IdContract
        , call_data => CallData
        , gas => 10000
        , gas_price => ContractCreateTxGasPrice
    },
    Tx           = aect_test_utils:create_tx(PubKey, Overrides, S1),
    ?assertEqual(ContractCreateTxGasPrice, aect_create_tx:gas_price(aetx:tx(Tx))),

    %% Test that the create transaction is accepted
    {ok, S2} = sign_and_apply_transaction(Tx, PrivKey, S1),
    %% Check that the contract is created
    ContractKey = aect_contracts:compute_contract_pubkey(PubKey, aetx:nonce(Tx)),
    {{value, Contract}, _} = lookup_contract_by_id(ContractKey, S2),
    %% Check that the created contract has the correct details from the contract create tx
    ?assertEqual(PubKey, aect_contracts:owner(Contract)),
    ?assertEqual(aect_create_tx:vm_version(aetx:tx(Tx)), aect_contracts:vm_version(Contract)),
    ?assertEqual(aect_create_tx:code(aetx:tx(Tx)), aect_contracts:code(Contract)),
    ?assertEqual(aect_create_tx:deposit(aetx:tx(Tx)), aect_contracts:deposit(Contract)),
    %% Check that the created contract has the correct details not from the contract create tx
    _ = aect_contracts:log(Contract),
    ?assert(aect_contracts:active(Contract)),
    ?assertEqual([], aect_contracts:referers(Contract)),
    %% Check that the contract init call is created
    ?assertEqual([], aect_call_state_tree:to_list(aect_test_utils:calls(S1))),
    ?assertMatch([_], aect_call_state_tree:to_list(aect_test_utils:calls(S2))),
    InitCallId = aect_call:id(PubKey, aetx:nonce(Tx), ContractKey),
    {value, InitCall} = aect_call_state_tree:lookup_call(ContractKey, InitCallId, aect_test_utils:calls(S2)),
    %% Check that the created init call has the correct details from the contract create tx
    ?assertEqual(PubKey, aect_call:caller_address(InitCall)),
    ?assertEqual(aetx:nonce(Tx), aect_call:caller_nonce(InitCall)),
    ?assertEqual(aect_create_tx:gas_price(aetx:tx(Tx)), aect_call:gas_price(InitCall)),
    %% Check that the created init call has the correct details not from the contract create tx
    ?assertEqual(ContractKey, aect_call:contract_address(InitCall)),
    _ = aect_call:height(InitCall), %% Unclear if this needed.
    ?assertMatch(X when X > 0, aect_call:gas_used(InitCall)),
    ?assertEqual(ok, aect_call:return_type(InitCall)),
    _ = aect_call:return_value(InitCall), %% Value shall be the unit value.

    %% Check that contract create transaction sender got charged correctly.
    ?assertEqual(aec_accounts:balance(aect_test_utils:get_account(PubKey, S1))
                 - aect_create_tx:fee(aetx:tx(Tx))
                 - aect_create_tx:deposit(aetx:tx(Tx))
                 - aect_create_tx:amount(aetx:tx(Tx))
                 - aect_create_tx:gas_price(aetx:tx(Tx)) * aect_call:gas_used(InitCall),
                 aec_accounts:balance(aect_test_utils:get_account(PubKey, S2))),
    %% Check that created contract account got credited correctly.
    ?assertEqual(aect_create_tx:amount(aetx:tx(Tx)),
                 aec_accounts:balance(aect_test_utils:get_account(ContractKey, S2))),

    ok.

sign_and_apply_transaction(Tx, PrivKey, S1) ->
    sign_and_apply_transaction(Tx, PrivKey, S1, 1).

sign_and_apply_transaction(Tx, PrivKey, S1, Height) ->
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    Trees    = aect_test_utils:trees(S1),
    {ok, AcceptedTxs, Trees1} =
        aec_block_micro_candidate:apply_block_txs([SignedTx], Trees, Height, ?PROTOCOL_VERSION),
    S2       = aect_test_utils:set_trees(Trees1, S1),
    case AcceptedTxs of
        [SignedTx] -> {ok, S2};
        []         -> {error, S2}
    end.

sign_and_apply_transaction_strict(Tx, PrivKey, S1) ->
    sign_and_apply_transaction_strict(Tx, PrivKey, S1, 1).

sign_and_apply_transaction_strict(Tx, PrivKey, S1, Height) ->
    SignedTx = aec_test_utils:sign_tx(Tx, PrivKey),
    Trees    = aect_test_utils:trees(S1),
    ConsensusVersion = aec_hard_forks:protocol_effective_at_height(Height),
    {ok, AcceptedTxs, Trees1} =
        aec_block_micro_candidate:apply_block_txs_strict([SignedTx], Trees, Height, ConsensusVersion),
    S2       = aect_test_utils:set_trees(Trees1, S1),
    {SignedTx, AcceptedTxs, S2}.


%%%===================================================================
%%% Call contract
%%%===================================================================

call_contract_negative_insufficient_funds(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = call(fun new_account/2, [1000000]),
    IdC = call(fun create_contract/4, [Acc1, identity, {}]),

    Fee = 1,
    Value = 10,
    Bal = 9 = Fee + Value - 2,
    S = aect_test_utils:set_account_balance(Acc1, Bal, state()),
    Arg = <<"(42)">>,
    CallData = aect_sophia:create_call(IdC, <<"main">>, Arg),
    CallTx = aect_test_utils:call_tx(Acc1, IdC,
                                     #{call_data => CallData,
                                       gas_price => 0,
                                       amount    => Value,
                                       fee       => Fee}, S),
    {error, _} = sign_and_apply_transaction(CallTx, aect_test_utils:priv_key(Acc1, S), S),
    {error, insufficient_funds} = aetx:check(CallTx, aect_test_utils:trees(S), _CurrHeight = 1, ?PROTOCOL_VERSION),
    ok.

call_contract_negative(_Cfg) ->
    %% PLACEHOLDER
    ok.

call_contract(_Cfg) ->
    call_contract_(2).

call_contract_with_gas_price_zero(_Cfg) ->
    call_contract_(0).

call_contract_(ContractCallTxGasPrice) ->
    S  = aect_test_utils:new_state(),
    S0 = aect_test_utils:setup_miner_account(?MINER_PUBKEY, S),

    {Owner,  S1}  = aect_test_utils:setup_new_account(S0),
    {Caller, S2}  = aect_test_utils:setup_new_account(S1),
    OwnerPrivKey  = aect_test_utils:priv_key(Owner, S2),
    CallerPrivKey = aect_test_utils:priv_key(Caller, S2),

    CallerBalance = aec_accounts:balance(aect_test_utils:get_account(Caller, S2)),

    IdContract   = aect_test_utils:compile_contract("contracts/identity.aes"),
    CallDataInit = aeso_abi:create_calldata(IdContract, "init", "()"),
    Overrides    = #{ code => IdContract
		    , call_data => CallDataInit
		    , gas => 10000
                    , gas_price => 1
		    },
    CreateTx     = aect_test_utils:create_tx(Owner, Overrides, S2),
    ?assertEqual(1, aect_create_tx:gas_price(aetx:tx(CreateTx))),

    %% Test that the create transaction is accepted
    {SignedTx, [SignedTx], S3} = sign_and_apply_transaction_strict(CreateTx, OwnerPrivKey, S2),
    ContractKey = aect_contracts:compute_contract_pubkey(Owner, aetx:nonce(CreateTx)),

    %% Now check that we can call it.
    Fee           = 107,
    Value         = 52,
    Arg           = <<"(42)">>,
    CallData = aect_sophia:create_call(IdContract, <<"main">>, Arg),
    CallTx = aect_test_utils:call_tx(Caller, ContractKey,
                                     #{call_data => CallData,
                                       gas_price => ContractCallTxGasPrice,
                                       amount    => Value,
                                       fee       => Fee}, S3),
    ?assertEqual(ContractCallTxGasPrice, aect_call_tx:gas_price(aetx:tx(CallTx))),
    {ok, S4} = sign_and_apply_transaction(CallTx, CallerPrivKey, S3),
    CallId = aect_call:id(Caller, aetx:nonce(CallTx), ContractKey),

    %% Check that it got stored and that we got the right return value
    ?assertMatch([_, _], aect_call_state_tree:to_list(aect_test_utils:calls(S4))), %% Init + Call
    Call = aect_call_state_tree:get_call(ContractKey, CallId, aect_test_utils:calls(S4)),
    ok = aect_call:return_type(Call),
    <<42:256>> = aect_call:return_value(Call),
    %% Check that the stored call has the correct rest of the details
    ?assertEqual(Caller, aect_call:caller_address(Call)),
    ?assertEqual(aetx:nonce(CallTx), aect_call:caller_nonce(Call)),
    _ = aect_call:height(Call), %% Unclear if this needed.
    ?assertEqual(ContractKey, aect_call:contract_address(Call)),
    ?assertEqual(aect_call_tx:gas_price(aetx:tx(CallTx)), aect_call:gas_price(Call)),
    ?assertMatch(X when X > 0, aect_call:gas_used(Call)),

    %% Check that contract call transaction sender got charged the right amount for gas and fee.
    {NewCallerBalance, NewCallerBalance} =
        {aec_accounts:balance(aect_test_utils:get_account(Caller, S4)),
         CallerBalance - Fee - aect_call_tx:gas_price(aetx:tx(CallTx)) * aect_call:gas_used(Call) - Value},
    %% Check that called account got credited correctly.
    ?assertEqual(aec_accounts:balance(aect_test_utils:get_account(ContractKey, S3))
                 + aect_call_tx:amount(aetx:tx(CallTx)),
                 aec_accounts:balance(aect_test_utils:get_account(ContractKey, S4))),

    {ok, S4}.

%% Check behaviour of contract call error - especially re value / amount.
call_contract_error_value(_Cfg) ->
    F = 1,
    DefaultOpts = #{fee => F, gas_price => 0, amount => 0},
    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,
    %% Initialization.
    state(aect_test_utils:new_state()),
    Acc1 = call(fun new_account/2, [1000000]),
    IdC = call(fun create_contract/5, [Acc1, value_on_err, {}, DefaultOpts#{deposit => 0}]),
    RemC = call(fun create_contract/5, [Acc1, remote_value_on_err, {}, DefaultOpts#{deposit => 0}]),
    0 = call(fun account_balance/2, [IdC]),
    0 = call(fun account_balance/2, [RemC]),
    %% Sanity check: value is transferred as expected in calls that do not err.
    S0 = state(),
    {11, S1} = call_contract(Acc1, IdC, ok, word, {}, DefaultOpts#{amount := 3}, S0),
    ?assertEqual(Bal(Acc1, S0) - (F + 3), Bal(Acc1, S1)),
    ?assertEqual(Bal(RemC, S0), Bal(RemC, S1)),
    ?assertEqual(Bal(IdC, S0) + 3, Bal(IdC, S1)),
    {11, S2} = call_contract(Acc1, RemC, callOk, word, {IdC, 10}, DefaultOpts#{amount := 14}, S1),
    ?assertEqual(Bal(Acc1, S1) - (F + 14), Bal(Acc1, S2)),
    ?assertEqual(Bal(RemC, S1) + (14 - 10), Bal(RemC, S2)),
    ?assertEqual(Bal(IdC, S1) + 10, Bal(IdC, S2)),
    %% Check tranfer of value in calls that err.
    {{error, <<"out_of_gas">>}, S3} = call_contract(Acc1, IdC, err, word, {}, DefaultOpts#{amount := 5}, S2),
    ?assertEqual(Bal(Acc1, S2) - (F + 5), Bal(Acc1, S3)),
    ?assertEqual(Bal(RemC, S2), Bal(RemC, S3)),
    ?assertEqual(Bal(IdC, S2) + 5, Bal(IdC, S3)),
    {{error, <<"out_of_gas">>}, S4} = call_contract(Acc1, RemC, callErr, word, {IdC, 7}, DefaultOpts#{amount := 13}, S3),
    ?assertEqual(Bal(Acc1, S3) - (F + 13), Bal(Acc1, S4)),
    ?assertEqual(Bal(RemC, S3) + 13, Bal(RemC, S4)),
    ?assertEqual(Bal(IdC, S3), Bal(IdC, S4)),
    ok.

%%%===================================================================
%%% State trees
%%%===================================================================

make_contract(PubKey, Code, S) ->
    Tx = aect_test_utils:create_tx(PubKey, #{ vm_version => 2,
                                              code => Code }, S),
    {contract_create_tx, CTx} = aetx:specialize_type(Tx),
    aect_contracts:new(CTx).

make_call(PubKey, ContractKey,_Call,_S) ->
    aect_call:new(PubKey, 0, ContractKey, 1, 1).

state()  -> get(the_state).
state(S) -> put(the_state, S).

call(Name, Fun, Xs) ->
    Fmt = string:join(lists:duplicate(length(Xs), "~p"), ", "),
    io:format("~p(" ++ Fmt ++ ") ->\n", [Name | Xs]),
    R = call(Fun, Xs),
    io:format("  ~p\n", [R]),
    R.

call(Fun, Xs) when is_function(Fun, 1 + length(Xs)) ->
    S = state(),
    {R, S1} = try apply(Fun, Xs ++ [S])
              catch _:Reason -> {{'EXIT', Reason, erlang:get_stacktrace()}, S}
              end,
    state(S1),
    R.

-define(call(Fun, X),                call(Fun, fun Fun/2, [X])).
-define(call(Fun, X, Y),             call(Fun, fun Fun/3, [X, Y])).
-define(call(Fun, X, Y, Z),          call(Fun, fun Fun/4, [X, Y, Z])).
-define(call(Fun, X, Y, Z, U),       call(Fun, fun Fun/5, [X, Y, Z, U])).
-define(call(Fun, X, Y, Z, U, V),    call(Fun, fun Fun/6, [X, Y, Z, U, V])).
-define(call(Fun, X, Y, Z, U, V, W), call(Fun, fun Fun/7, [X, Y, Z, U, V, W])).

new_account(Balance, S) ->
    aect_test_utils:setup_new_account(Balance, S).

insert_contract(Account, Code, S) ->
    Contract  = make_contract(Account, Code, S),
    Contracts = aect_state_tree:insert_contract(Contract, aect_test_utils:contracts(S)),
    {Contract, aect_test_utils:set_contracts(Contracts, S)}.

insert_call(Sender, Contract, Fun, S) ->
    ContractId = aect_contracts:id(Contract),
    Call       = make_call(Sender, ContractId, Fun, S),
    CallTree   = aect_call_state_tree:insert_call(Call, aect_test_utils:calls(S)),
    {Call, aect_test_utils:set_calls(CallTree, S)}.

get_contract(Contract0, S) ->
    ContractKey = aect_contracts:id(Contract0),
    Contracts   = aect_test_utils:contracts(S),
    Contract    = aect_state_tree:get_contract(ContractKey, Contracts),
    {Contract, S}.

lookup_contract_by_id(ContractKey, S) ->
    Contracts = aect_test_utils:contracts(S),
    X         = aect_state_tree:lookup_contract(ContractKey, Contracts),
    {X, S}.

get_call(Contract0, Call0, S) ->
    CallId     = aect_call:id(Call0),
    ContractId = aect_contracts:id(Contract0),
    CallTree   = aect_test_utils:calls(S),
    Call       = aect_call_state_tree:get_call(ContractId, CallId, CallTree),
    {Call, S}.

state_tree(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1  = ?call(new_account, 100),
    Ct1   = ?call(insert_contract, Acc1, <<"Code for C1">>),
    Ct1   = ?call(get_contract, Ct1),
    Acc2  = ?call(new_account, 50),
    Acc3  = ?call(new_account, 30),
    Ct2   = ?call(insert_contract, Acc2, <<"Code for C2">>),
    Ct2   = ?call(get_contract, Ct2),
    Ct1   = ?call(get_contract, Ct1),
    Call1 = ?call(insert_call, Acc3, Ct1, <<"Ct1.foo">>),
    Call2 = ?call(insert_call, Acc2, Ct1, <<"Ct1.bar">>),
    Call1 = ?call(get_call, Ct1, Call1),
    Call2 = ?call(get_call, Ct1, Call2),
    Ct1   = ?call(get_contract, Ct1),
    <<"Code for C1">> = aect_contracts:code(Ct1),
    ok.

%%%===================================================================
%%% More elaborate Sophia contracts
%%%===================================================================

create_contract(Owner, Name, Args, S) ->
    create_contract(Owner, Name, Args, #{}, S).

create_contract(Owner, Name, Args, Options, S) ->
    Nonce       = aect_test_utils:next_nonce(Owner, S),
    Code        = aect_test_utils:compile_contract(lists:concat(["contracts/", Name, ".aes"])),
    CallData    = aect_sophia:create_call(Code, <<"init">>, args_to_binary(Args)),
    CreateTx    = aect_test_utils:create_tx(Owner,
                    maps:merge(
                    #{ nonce      => Nonce
                     , vm_version => ?AEVM_01_Sophia_01
                     , code       => Code
                     , call_data  => CallData
                     , fee        => 1
                     , deposit    => 0
                     , amount     => 0
                     , gas        => 10000 }, maps:remove(height, Options)), S),
    Height   = maps:get(height, Options, 1),
    PrivKey  = aect_test_utils:priv_key(Owner, S),
    {ok, S1} = sign_and_apply_transaction(CreateTx, PrivKey, S, Height),
    ContractKey = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    {ContractKey, S1}.

call_contract(Caller, ContractKey, Fun, Type, Args, S) ->
    call_contract(Caller, ContractKey, Fun, Type, Args, #{}, S).

call_contract(Caller, ContractKey, Fun, Type, Args0, Options, S) ->
    Nonce    = aect_test_utils:next_nonce(Caller, S),
    Args     = if is_tuple(Args0) -> Args0; true -> {Args0} end,
    CallData = aeso_data:to_binary({list_to_binary(atom_to_list(Fun)), translate_pubkeys(Args)}),
    CallTx   = aect_test_utils:call_tx(Caller, ContractKey,
                maps:merge(
                #{ nonce      => Nonce
                 , vm_version => ?AEVM_01_Sophia_01
                 , call_data  => CallData
                 , fee        => 1
                 , amount     => 0
                 , gas        => 50000
                 }, maps:remove(height, Options)), S),
    Height   = maps:get(height, Options, 1),
    PrivKey  = aect_test_utils:priv_key(Caller, S),
    {ok, S1} = sign_and_apply_transaction(CallTx, PrivKey, S, Height),
    CallKey  = aect_call:id(Caller, Nonce, ContractKey),
    CallTree = aect_test_utils:calls(S1),
    Call     = aect_call_state_tree:get_call(ContractKey, CallKey, CallTree),
    Result   =
        case aect_call:return_type(Call) of
            ok     -> {ok, Res} = aeso_data:from_binary(Type, aect_call:return_value(Call)),
                      Res;
            error  -> {error, aect_call:return_value(Call)};
            revert -> revert
        end,
    {Result, S1}.

account_balance(PubKey, S) ->
    Account = aect_test_utils:get_account(PubKey, S),
    {aec_accounts:balance(Account), S}.

translate_pubkeys(<<N:256>>) -> N;
translate_pubkeys([H|T]) ->
  [translate_pubkeys(H) | translate_pubkeys(T)];
translate_pubkeys(T) when is_tuple(T) ->
  list_to_tuple(translate_pubkeys(tuple_to_list(T)));
translate_pubkeys(M) when is_map(M) ->
  maps:from_list(translate_pubkeys(maps:to_list(M)));
translate_pubkeys(X) -> X.

args_to_binary(Args) -> list_to_binary(args_to_list(Args)).

commas([]) -> [];
commas([H | T]) ->
    io_lib:format("~s~s", [H, [ [",", X] || X <- T ]]).

args_to_list(<<N:256>>) -> integer_to_list(N);
args_to_list(B) when is_binary(B) ->
    io_lib:format("~10000p", [binary_to_list(B)]);   %% string
args_to_list(N) when is_integer(N) -> integer_to_list(N);
args_to_list(L) when is_list(L) ->
    io_lib:format("[~s]", [commas(lists:map(fun args_to_list/1, L))]);
args_to_list(T) when is_tuple(T) ->
    io_lib:format("(~s)", [commas(lists:map(fun args_to_list/1, tuple_to_list(T)))]);
args_to_list(M) when is_map(M) ->
    Elems = [ io_lib:format("[~s] = ~s", [args_to_list(K), args_to_list(V)]) || {K, V} <- maps:to_list(M) ],
    ["{", string:join(Elems, ","), "}"].

sophia_identity(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1 = ?call(new_account, 1000000),
    %% Remote calling the identity contract
    IdC   = ?call(create_contract, Acc1, identity, {}),
    RemC  = ?call(create_contract, Acc1, remote_call, {}, #{amount => 100}),
    42    = ?call(call_contract,   Acc1, IdC, main, word, 42),
    99    = ?call(call_contract,   Acc1, RemC, call, word, {IdC, 99}),
    RemC2 = ?call(create_contract, Acc1, remote_call, {}, #{amount => 100}),
    77    = ?call(call_contract,   Acc1, RemC2, staged_call, word, {RemC, IdC, 77}),
    ok.

sophia_state(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1         = ?call(new_account, 1000000),
    InitStack    = [<<"top">>, <<"middle">>, <<"bottom">>],
    Stack        = ?call(create_contract, Acc1, stack, InitStack),
    3            = ?call(call_contract,   Acc1, Stack, size, word, {}),
    InitStack    = ?call(call_contract, Acc1, Stack, all, {list, string}, {}),
    4            = ?call(call_contract, Acc1, Stack, push, word, <<"foo">>),
    <<"foo">>    = ?call(call_contract, Acc1, Stack, pop, string, {}),
    <<"top">>    = ?call(call_contract, Acc1, Stack, pop, string, {}),
    <<"middle">> = ?call(call_contract, Acc1, Stack, pop, string, {}),
    <<"bottom">> = ?call(call_contract, Acc1, Stack, pop, string, {}),
    {error, <<"out_of_gas">>} = ?call(call_contract, Acc1, Stack, pop, string, {}),
    ok.

sophia_spend(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1         = ?call(new_account, 1000000),
    Acc2         = ?call(new_account, 2000000),
    Ct1          = ?call(create_contract, Acc1, spend_test, {}, #{amount => 10000}),
    Ct2          = ?call(create_contract, Acc1, spend_test, {}, #{amount => 20000}),
    10000        = ?call(call_contract, Acc1, Ct1, get_balance, word, {}),
    20000        = ?call(call_contract, Acc1, Ct2, get_balance, word, {}),
    5000         = ?call(call_contract, Acc1, Ct2, spend, word, {Acc2, 15000}),
    5000         = ?call(call_contract, Acc1, Ct1, get_balance_of, word, Ct2),
    10000        = ?call(call_contract, Acc1, Ct1, get_balance, word, {}),
    5000         = ?call(call_contract, Acc1, Ct2, get_balance, word, {}),
    2015000      = ?call(call_contract, Acc1, Ct1, get_balance_of, word, Acc2),
    %% Spend in nested call
    2021000      = ?call(call_contract, Acc1, Ct2, spend_from, word, {Ct1, Acc2, 6000}),
    2021000      = ?call(call_contract, Acc1, Ct1, get_balance_of, word, Acc2),
    4000         = ?call(call_contract, Acc1, Ct1, get_balance_of, word, Ct1),
    5000         = ?call(call_contract, Acc1, Ct1, get_balance_of, word, Ct2),
    ok.

%% Oracles tests

%% TODO:
%%  - TTL stuff
%%  - signatures (when oracle is different from contract)
%%  - Handling of fees
%%  - Failing calls
sophia_oracles(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc               = ?call(new_account, 1000000),
    Ct = <<CtId:256>> = ?call(create_contract, Acc, oracles, {}, #{amount => 100000}),
    QueryFee          = 100,
    TTL               = 15,
    CtId              = ?call(call_contract, Acc, Ct, registerOracle, word, {CtId, 0, QueryFee, TTL}),
    Question          = <<"Manchester United vs Brommapojkarna">>,
    QId               = ?call(call_contract, Acc, Ct, createQuery, word, {Ct, Question, QueryFee, 5, 5}, #{amount => QueryFee}),
    Question          = ?call(call_contract, Acc, Ct, getQuestion, string, {CtId, QId}),
    QueryFee          = ?call(call_contract, Acc, Ct, queryFee, word, Ct),
    none              = ?call(call_contract, Acc, Ct, getAnswer, {option, word}, {CtId, QId}),
    {}                = ?call(call_contract, Acc, Ct, respond, {tuple, []}, {CtId, QId, 0, 4001}),
    {some, 4001}      = ?call(call_contract, Acc, Ct, getAnswer, {option, word}, {CtId, QId}),
    {}                = ?call(call_contract, Acc, Ct, extendOracle, {tuple, []}, {Ct, 0, TTL + 10}),

    %% Test complex answers
    Ct1 = ?call(create_contract, Acc, oracles, {}, #{amount => 100000}),
    QuestionType = {variant_t, [{why, [word]}, {how, [string]}]},
    AnswerType   = {variant_t, [{noAnswer, []}, {yesAnswer, [QuestionType, string, word]}]},
    Question1    = {1, <<"birds fly?">>},
    Answer       = {yesAnswer, {how, <<"birds fly?">>}, <<"magic">>, 1337},
    {some, Answer} = ?call(call_contract, Acc, Ct1, complexOracle, {option, AnswerType}, {Question1, 0}),
    ok.

oracle_init_from_contract(OperatorAcc, InitialOracleContractBalance, S) ->
    {OCt, S1} = create_contract(OperatorAcc, oracles, {}, #{amount => InitialOracleContractBalance}, S),
    {{OCt, OCt}, S1}.

oracle_init_from_remote_contract(OperatorAcc, InitialOracleContractBalance, S) ->
    {OCt, S1} = create_contract(OperatorAcc, oracles, {}, #{amount => InitialOracleContractBalance}, S),
    {RCt, S2} = create_contract(OperatorAcc, remote_oracles, {}, #{amount => 0}, S1),
    {{OCt, RCt}, S2}.

oracle_register_from_contract(OperatorAcc, OCt, OCt, Opts, TxOpts, S) ->
    QueryFee = maps:get(qfee, Opts),
    OTtl = maps:get(ottl, Opts, 15),
    <<OCtId:256>> = OCt,
    {OCtId, S1} = call_contract(OperatorAcc, OCt, registerOracle, word, {OCt, 0, QueryFee, OTtl}, TxOpts, S),
    {ok, S1}.

oracle_register_from_remote_contract(OperatorAcc, RCt, OCt, Opts, TxOpts, S) ->
    QueryFee = maps:get(qfee, Opts),
    OTtl = maps:get(ottl, Opts, 15),
    <<OCtId:256>> = OCt,
    {OCtId, S1} = call_contract(OperatorAcc, RCt, callRegisterOracle, word, {OCt, OCt, 0, QueryFee, OTtl}, TxOpts, S),
    {ok, S1}.

oracle_query_from_contract(UserAcc, OCt, OCt, Opts, TxOpts, S) ->
    oracle_query_from_contract_(createQuery, UserAcc, OCt, OCt, Opts, TxOpts, S).
oracle_unsafe_query_from_contract(UserAcc, OCt, OCt, Opts, TxOpts, S) ->
    oracle_query_from_contract_(unsafeCreateQuery, UserAcc, OCt, OCt, Opts, TxOpts, S).
oracle_query_from_contract_(Fun, UserAcc, OCt, OCt, Opts, TxOpts, S) ->
    QueryFee = maps:get(qfee, Opts),
    Question = maps:get(question, Opts, <<"why?">>),
    ?assertMatch(_ when is_binary(Question), Question),
    QTtl = maps:get(qttl, Opts, 5),
    RTtl = maps:get(rttl, Opts, 5),
    call_contract(UserAcc, OCt, Fun, word, {OCt, Question, QueryFee, QTtl, RTtl}, TxOpts, S).

oracle_query_from_remote_contract(UserAcc, RCt, OCt, Opts, TxOpts, S) ->
    oracle_query_from_remote_contract_(callCreateQuery, UserAcc, RCt, OCt, Opts, TxOpts, S).
oracle_unsafe_query_from_remote_contract(UserAcc, RCt, OCt, Opts, TxOpts, S) ->
    oracle_query_from_remote_contract_(callUnsafeCreateQuery, UserAcc, RCt, OCt, Opts, TxOpts, S).
oracle_query_from_remote_contract_(Fun, UserAcc, RCt, OCt, Opts, TxOpts, S) ->
    oracle_query_from_remote_contract_(Fun, UserAcc, RCt, OCt, OCt, Opts, TxOpts, S).
oracle_query_from_remote_contract_(Fun, UserAcc, RCt, OCt, OAcc, Opts, TxOpts, S) ->
    QueryFee = maps:get(qfee, Opts),
    Question = maps:get(question, Opts, <<"why?">>),
    QTtl = maps:get(qttl, Opts, 5),
    RTtl = maps:get(rttl, Opts, 5),
    Value = case maps:find(remote_value, Opts) of
                {ok, V} -> V;
                error -> maps:get(amount, TxOpts)
            end,
    call_contract(UserAcc, RCt, Fun, word, {OCt, Value, OAcc, Question, QueryFee, QTtl, RTtl}, TxOpts, S).

oracle_check_and_respond_from_contract(OperatorAcc, OCt, OCt, QueryId, Opts, TxOpts, S) ->
    ?assertMatch({Question, _} when is_binary(Question), call_contract(OperatorAcc, OCt, getQuestion, string, {OCt, QueryId}, S)),
    Response = maps:get(response, Opts, 4001),
    ?assertMatch(_ when is_integer(Response), Response),
    {{}, S1} = call_contract(OperatorAcc, OCt, respond, {tuple, []}, {OCt, QueryId, 0, Response}, TxOpts, S),
    ?assertMatch({{some, Response}, _}, call_contract(OperatorAcc, OCt, getAnswer, {option, word}, {OCt, QueryId}, S1)),
    {ok, S1}.

oracle_check_and_respond_from_remote_contract(OperatorAcc, RCt, OCt, QueryId, Opts, TxOpts, S) ->
    ?assertMatch({Question, _} when is_binary(Question), call_contract(OperatorAcc, OCt, getQuestion, string, {OCt, QueryId}, S)),
    Response = maps:get(response, Opts, 4001),
    {{}, S1} = call_contract(OperatorAcc, RCt, callRespond, {tuple, []}, {OCt, OCt, QueryId, 0, Response}, TxOpts, S),
    ?assertMatch({{some, Response}, _}, call_contract(OperatorAcc, OCt, getAnswer, {option, word}, {OCt, QueryId}, S1)),
    {ok, S1}.

-record(oracle_cbs, {init, register, query, respond}).
%%
-define(ORACLE_SAFE_CBS,
        #oracle_cbs{
           init = fun oracle_init_from_contract/3,
           register = fun oracle_register_from_contract/6,
           query = fun oracle_query_from_contract/6,
           respond = fun oracle_check_and_respond_from_contract/7
          }).
-define(ORACLE_UNSAFE_CBS, ?ORACLE_SAFE_CBS#oracle_cbs{query = fun oracle_unsafe_query_from_contract/6}).
-define(ORACLE_SAFE_REMOTE_CBS,
        #oracle_cbs{
           init = fun oracle_init_from_remote_contract/3,
           register = fun oracle_register_from_remote_contract/6,
           query = fun oracle_query_from_remote_contract/6,
           respond = fun oracle_check_and_respond_from_remote_contract/7
          }).
-define(ORACLE_UNSAFE_REMOTE_CBS, ?ORACLE_SAFE_REMOTE_CBS#oracle_cbs{query = fun oracle_unsafe_query_from_remote_contract/6}).
%%
closed_oracle_cbs(Cbs,
                  OperatorAcc, UserAcc,
                  InitialOracleCtBalance,
                  RegisterOpts, RegisterTxOpts,
                  QueryOpts   , QueryTxOpts,
                  RespondOpts , RespondTxOpts) ->
    #oracle_cbs{
       init =
           fun(S) -> (Cbs#oracle_cbs.init)(OperatorAcc, InitialOracleCtBalance, S) end,
       register =
           fun(CallingCt, OracleAcc, S) -> (Cbs#oracle_cbs.register)(OperatorAcc, CallingCt, OracleAcc, RegisterOpts, RegisterTxOpts, S) end,
       query =
           fun(CallingCt, OracleAcc, S) -> (Cbs#oracle_cbs.query)(UserAcc, CallingCt, OracleAcc, QueryOpts, QueryTxOpts, S) end,
       respond =
           case {RespondOpts, RespondTxOpts} of
               {no_response, no_response} ->
                   no_respond_cb;
               _ when is_map(RespondOpts), is_map(RespondTxOpts) ->
                   fun(CallingCt, OracleAcc, QId, S) -> (Cbs#oracle_cbs.respond)(OperatorAcc, CallingCt, OracleAcc, QId, RespondOpts, RespondTxOpts, S) end
           end
      }.
%%
sophia_oracles_qfee__init_and_register_and_query_(Init, RegisterOracle, CreateQuery) ->
    {OracleAcc, CallingCt} = call(Init, []),
    S0 = state(),
    ok = call(RegisterOracle, [CallingCt, OracleAcc]),
    S1 = state(),
    R = call(CreateQuery, [CallingCt, OracleAcc]),
    {R,
     {OracleAcc, CallingCt},
     [S0, S1]}.
%%
sophia_oracles_qfee__init_and_register_and_query_and_respond_(Init, RegisterOracle, CreateQuery, RespondQuery) ->
    {QId,
     {OracleAcc, CallingCt},
     [S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__init_and_register_and_query_(Init, RegisterOracle, CreateQuery),
    S2 = state(),
    ok = call(RespondQuery, [CallingCt, OracleAcc, QId]),
    {{OracleAcc, CallingCt},
     [S0, S1, S2]}.

sophia_oracles_qfee__flow_up_to_respond_(Cbs,
                                         TxFee,
                                         InitialOracleCtBalance,
                                         RegisterOpts,
                                         QueryOpts, QueryTxValue,
                                         RespondOpts) ->
    RegisterTxOpts = #{fee => TxFee, gas_price => 0, amount => 0},
    QueryTxOpts = #{fee => TxFee, gas_price => 0, amount => QueryTxValue},
    RespondTxOpts = #{fee => TxFee, gas_price => 0, amount => 0},

    state(aect_test_utils:new_state()),
    OperatorAcc = call(fun new_account/2, [1000000]),
    UserAcc = call(fun new_account/2, [1000000]),
    CCbs = closed_oracle_cbs(Cbs,
                             OperatorAcc, UserAcc,
                             InitialOracleCtBalance,
                             RegisterOpts, RegisterTxOpts,
                             QueryOpts   , QueryTxOpts,
                             RespondOpts , RespondTxOpts),
    {{OracleAcc, CallingCt},
     [S0, %% State before oracle registration.
      S1, %% State after oracle registration.
      S2] %% State after query.
    } = sophia_oracles_qfee__init_and_register_and_query_and_respond_(
          CCbs#oracle_cbs.init,
          CCbs#oracle_cbs.register,
          CCbs#oracle_cbs.query,
          CCbs#oracle_cbs.respond),
    {{OperatorAcc, UserAcc}, {OracleAcc, CallingCt}, [S0, S1, S2]}.
%%
sophia_oracles_qfee__flow_up_to_query_(Cbs,
                                       TxFee,
                                       InitialOracleCtBalance,
                                       RegisterOpts,
                                       QueryOpts, QueryTxValue) ->
    sophia_oracles_qfee__flow_up_to_query_(aect_test_utils:new_state(),
                                           Cbs,
                                           TxFee,
                                           InitialOracleCtBalance,
                                           RegisterOpts,
                                           QueryOpts, QueryTxValue).
sophia_oracles_qfee__flow_up_to_query_(InitialState,
                                       Cbs,
                                       TxFee,
                                       InitialOracleCtBalance,
                                       RegisterOpts,
                                       QueryOpts, QueryTxValue) ->
    RegisterTxOpts = #{fee => TxFee, gas_price => 0, amount => 0},
    QueryTxOpts = #{fee => TxFee, gas_price => 0, amount => QueryTxValue},

    state(InitialState),
    OperatorAcc = call(fun new_account/2, [1000000]),
    UserAcc = call(fun new_account/2, [1000000]),
    CCbs = closed_oracle_cbs(Cbs,
                             OperatorAcc, UserAcc,
                             InitialOracleCtBalance,
                             RegisterOpts, RegisterTxOpts,
                             QueryOpts   , QueryTxOpts,
                             no_response , no_response),
    {{error, <<"out_of_gas">>},
     {OracleAcc, CallingCt},
     [S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__init_and_register_and_query_(
          CCbs#oracle_cbs.init,
          CCbs#oracle_cbs.register,
          CCbs#oracle_cbs.query),
    {{OperatorAcc, UserAcc}, {OracleAcc, CallingCt}, [S0, S1]}.

%% Reference case i.e. all of the following items are equal:
%% * Query fee specified when registering oracle
%% * Query fee specified when creating query
%% * Value specified in call tx creating query
sophia_oracles_qfee__basic__data_() ->
    RegisterTxQFee = 100,
    {_InitialOracleCtBalance = 0,
     RegisterTxQFee,
     _QueryTxValue = RegisterTxQFee,
     _QueryTxQFee = RegisterTxQFee}.
%%
sophia_oracles_qfee__basic(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__basic__data_(),

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    RespondOpts = #{},
    {{OperatorAcc, UserAcc},
     {OracleAcc, OracleAcc = _CallingCt},
     [S0, %% State before oracle registration.
      S1, %% State after oracle registration.
      S2] %% State after query.
    } = sophia_oracles_qfee__flow_up_to_respond_(
          ?ORACLE_UNSAFE_CBS, %% Safe or unsafe query does not matter here.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue,
          RespondOpts),
    S3 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(OperatorAcc, S0) - TxFee, Bal(OperatorAcc, S1)),
    ?assertEqual(Bal(OracleAcc, S0)          , Bal(OracleAcc, S1)),
    ?assertEqual(Bal(UserAcc, S0)            , Bal(UserAcc, S1)),

    ?assertEqual(Bal(OperatorAcc, S1)                       , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1)                         , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(UserAcc, S1) - (TxFee + RegisterTxQFee), Bal(UserAcc, S2)),

    ?assertEqual(Bal(OperatorAcc, S2) - TxFee       , Bal(OperatorAcc, S3)),
    ?assertEqual(Bal(OracleAcc, S2) + RegisterTxQFee, Bal(OracleAcc, S3)),
    ?assertEqual(Bal(UserAcc, S2)                   , Bal(UserAcc, S3)),

    ?assertEqual([], aect_test_utils:get_oracle_queries(OracleAcc, S1)),
    ?assertMatch([_], aect_test_utils:get_oracle_queries(OracleAcc, S2)),
    ?assertMatch([_], aect_test_utils:get_oracle_queries(OracleAcc, S3)).
%%
sophia_oracles_qfee__basic__remote(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__basic__data_(),

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    RespondOpts = #{},
    {{OperatorAcc, UserAcc},
     {OracleAcc, CallingCt},
     [S0, %% State before oracle registration.
      S1, %% State after oracle registration.
      S2] %% State after query.
    } = sophia_oracles_qfee__flow_up_to_respond_(
          ?ORACLE_UNSAFE_REMOTE_CBS, %% Safe or unsafe query does not matter here.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue,
          RespondOpts),
    S3 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(OperatorAcc, S0) - TxFee, Bal(OperatorAcc, S1)),
    ?assertEqual(Bal(OracleAcc, S0)          , Bal(OracleAcc, S1)),
    ?assertEqual(Bal(CallingCt, S0)          , Bal(CallingCt, S1)),
    ?assertEqual(Bal(UserAcc, S0)            , Bal(UserAcc, S1)),

    ?assertEqual(Bal(OperatorAcc, S1)                       , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1)                         , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(CallingCt, S1)                         , Bal(CallingCt, S2)),
    ?assertEqual(Bal(UserAcc, S1) - (TxFee + RegisterTxQFee), Bal(UserAcc, S2)),

    ?assertEqual(Bal(OperatorAcc, S2) - TxFee       , Bal(OperatorAcc, S3)),
    ?assertEqual(Bal(OracleAcc, S2) + RegisterTxQFee, Bal(OracleAcc, S3)),
    ?assertEqual(Bal(CallingCt, S2)                 , Bal(CallingCt, S3)),
    ?assertEqual(Bal(UserAcc, S2)                   , Bal(UserAcc, S3)),

    ?assertEqual([], aect_test_utils:get_oracle_queries(OracleAcc, S1)),
    ?assertMatch([_], aect_test_utils:get_oracle_queries(OracleAcc, S2)),
    ?assertMatch([_], aect_test_utils:get_oracle_queries(OracleAcc, S3)).

%% Excessive query fee (covered by call tx value) is awarded to oracle
%% contract after respond.
sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_is_awarded_to_oracle__data_() ->
    QFeeExcess = 1,
    RegisterTxQFee = 100,
    QueryTxQFee = QFeeExcess + RegisterTxQFee,
    {_InitialOracleCtBalance = 0,
     RegisterTxQFee,
     _QueryTxValue = QueryTxQFee,
     QueryTxQFee}.
%%
sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_is_awarded_to_oracle(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_is_awarded_to_oracle__data_(),

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    RespondOpts = #{},
    {{OperatorAcc, UserAcc},
     {OracleAcc, OracleAcc = _CallingCt},
     [_S0, %% State before oracle registration.
      S1, %% State after oracle registration.
      S2] %% State after query.
    } = sophia_oracles_qfee__flow_up_to_respond_(
          ?ORACLE_UNSAFE_CBS, %% Safe or unsafe query does not matter here.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue,
          RespondOpts),
    S3 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxQFee), Bal(UserAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1)                      , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(OperatorAcc, S1)                    , Bal(OperatorAcc, S2)),

    ?assertEqual(Bal(UserAcc, S2)                , Bal(UserAcc, S3)),
    ?assertEqual(Bal(OracleAcc, S2) + QueryTxQFee, Bal(OracleAcc, S3)),
    ?assertEqual(Bal(OperatorAcc, S2) - TxFee    , Bal(OperatorAcc, S3)).
%%
sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_is_awarded_to_oracle__remote(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_is_awarded_to_oracle__data_(),

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    RespondOpts = #{},
    {{OperatorAcc, UserAcc},
     {OracleAcc, CallingCt},
     [_S0, %% State before oracle registration.
      S1, %% State after oracle registration.
      S2] %% State after query.
    } = sophia_oracles_qfee__flow_up_to_respond_(
          ?ORACLE_UNSAFE_REMOTE_CBS, %% Safe or unsafe query does not matter here.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue,
          RespondOpts),
    S3 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxQFee), Bal(UserAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1)                      , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(CallingCt, S1)                      , Bal(CallingCt, S2)),
    ?assertEqual(Bal(OperatorAcc, S1)                    , Bal(OperatorAcc, S2)),

    ?assertEqual(Bal(UserAcc, S2)                , Bal(UserAcc, S3)),
    ?assertEqual(Bal(OracleAcc, S2) + QueryTxQFee, Bal(OracleAcc, S3)),
    ?assertEqual(Bal(CallingCt, S2)              , Bal(CallingCt, S3)),
    ?assertEqual(Bal(OperatorAcc, S2) - TxFee    , Bal(OperatorAcc, S3)).

%% Call tx value in excess of query fee specified in same query call
%% tx ends up in oracle contract (at query creation).
sophia_oracles_qfee__tx_value_above_qfee_in_query_is_awarded_to_oracle__data_() ->
    QFeeExcess = 1,
    RegisterTxQFee = 100,
    {_InitialOracleCtBalance = 0,
     RegisterTxQFee,
     _QueryTxValue = QFeeExcess + RegisterTxQFee,
     _QueryTxQFee = RegisterTxQFee}.
%%
sophia_oracles_qfee__tx_value_above_qfee_in_query_is_awarded_to_oracle(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__tx_value_above_qfee_in_query_is_awarded_to_oracle__data_(),

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    RespondOpts = #{},
    {{OperatorAcc, UserAcc},
     {OracleAcc, OracleAcc = _CallingCt},
     [_S0, %% State before oracle registration.
      S1, %% State after oracle registration.
      S2] %% State after query.
    } = sophia_oracles_qfee__flow_up_to_respond_(
          ?ORACLE_UNSAFE_CBS, %% Safe or unsafe query does not matter here.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue,
          RespondOpts),
    S3 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue)        , Bal(UserAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1) + (QueryTxValue - QueryTxQFee), Bal(OracleAcc, S2)),
    ?assertEqual(Bal(OperatorAcc, S1)                             , Bal(OperatorAcc, S2)),

    ?assertEqual(Bal(UserAcc, S2)                , Bal(UserAcc, S3)),
    ?assertEqual(Bal(OracleAcc, S2) + QueryTxQFee, Bal(OracleAcc, S3)),
    ?assertEqual(Bal(OperatorAcc, S2) - TxFee    , Bal(OperatorAcc, S3)).
%%
sophia_oracles_qfee__tx_value_above_qfee_in_query_is_awarded_to_oracle__remote(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__tx_value_above_qfee_in_query_is_awarded_to_oracle__data_(),

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    RespondOpts = #{},
    {{OperatorAcc, UserAcc},
     {OracleAcc, CallingCt},
     [_S0, %% State before oracle registration.
      S1, %% State after oracle registration.
      S2] %% State after query.
    } = sophia_oracles_qfee__flow_up_to_respond_(
          ?ORACLE_UNSAFE_REMOTE_CBS, %% Safe or unsafe query does not matter here.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue,
          RespondOpts),
    S3 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue)        , Bal(UserAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1) + (QueryTxValue - QueryTxQFee), Bal(OracleAcc, S2)),
    ?assertEqual(Bal(CallingCt, S1)                               , Bal(CallingCt, S2)),
    ?assertEqual(Bal(OperatorAcc, S1)                             , Bal(OperatorAcc, S2)),

    ?assertEqual(Bal(UserAcc, S2)                , Bal(UserAcc, S3)),
    ?assertEqual(Bal(OracleAcc, S2) + QueryTxQFee, Bal(OracleAcc, S3)),
    ?assertEqual(Bal(CallingCt, S2)              , Bal(CallingCt, S3)),
    ?assertEqual(Bal(OperatorAcc, S2) - TxFee    , Bal(OperatorAcc, S3)).

%% Attempt to create query with query fee smaller than the one
%% requested by the oracle fails.
%%
%% Call tx value is assigned to recipient of (i.e. first contract
%% called from) query call tx.
sophia_oracles_qfee__qfee_in_query_below_qfee_in_oracle_errs__data_() ->
    RegisterTxQFee = 100,
    QueryTxQFee = RegisterTxQFee - 1,
    {_InitialOracleCtBalance = 0,
     RegisterTxQFee,
     _QueryTxValue = QueryTxQFee,
     QueryTxQFee}.
%%
sophia_oracles_qfee__qfee_in_query_below_qfee_in_oracle_errs(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__qfee_in_query_below_qfee_in_oracle_errs__data_(),

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    {{OperatorAcc, UserAcc},
     {OracleAcc, OracleAcc = _CallingCt},
     [_S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__flow_up_to_query_(
          ?ORACLE_UNSAFE_CBS, %% Safe or unsafe query does not matter here.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue),
    S2 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue), Bal(UserAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1) + QueryTxValue        , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(OperatorAcc, S1)                     , Bal(OperatorAcc, S2)).
%%
sophia_oracles_qfee__qfee_in_query_below_qfee_in_oracle_errs__remote(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__qfee_in_query_below_qfee_in_oracle_errs__data_(),

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    {{OperatorAcc, UserAcc},
     {OracleAcc, CallingCt},
     [_S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__flow_up_to_query_(
          ?ORACLE_UNSAFE_REMOTE_CBS, %% Safe or unsafe query does not matter here.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue),
    S2 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue), Bal(UserAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1)                       , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(CallingCt, S1) + QueryTxValue        , Bal(CallingCt, S2)),
    ?assertEqual(Bal(OperatorAcc, S1)                     , Bal(OperatorAcc, S2)).

%% Attempt to create query with call tx value smaller than query fee
%% uses oracle contract balance: oracle contract should implement
%% safety mechanism.
sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle__data_() ->
    ValueLack = 1,
    RegisterTxQFee = 100,
    {_InitialOracleCtBalance = 10 + ValueLack,
     RegisterTxQFee,
     _QueryTxValue = RegisterTxQFee - ValueLack,
     _QueryTxQFee = RegisterTxQFee}.
%%
sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle__data_(),

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    RespondOpts = #{},
    {{OperatorAcc, UserAcc},
     {OracleAcc, OracleAcc = _CallingCt},
     [_S0, %% State before oracle registration.
      S1, %% State after oracle registration.
      S2] %% State after query.
    } = sophia_oracles_qfee__flow_up_to_respond_(
          ?ORACLE_UNSAFE_CBS, %% Unsafe query.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue,
          RespondOpts),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(OperatorAcc, S1)                             , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1) - (QueryTxQFee - QueryTxValue), Bal(OracleAcc, S2)),
    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue)        , Bal(UserAcc, S2)).
%%
sophia_oracles_qfee__query_tx_value_below_qfee_does_not_take_from_poor_oracle(_Cfg) ->
    {_InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle__data_(),
    InitialOracleCtBalance = 0,

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    {{OperatorAcc, UserAcc},
     {OracleAcc, OracleAcc = _CallingCt},
     [_S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__flow_up_to_query_(
          ?ORACLE_UNSAFE_CBS, %% Unsafe query.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue),
    S2 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(OperatorAcc, S1)                     , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1) + QueryTxValue        , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue), Bal(UserAcc, S2)).
%%
sophia_oracles_qfee__query_tx_value_below_qfee_does_not_take_from_rich_oracle_thanks_to_contract_check(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle__data_(),

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    {{OperatorAcc, UserAcc},
     {OracleAcc, OracleAcc = _CallingCt},
     [_S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__flow_up_to_query_(
          ?ORACLE_SAFE_CBS, %% Safe query.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue),
    S2 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(OperatorAcc, S1)                     , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1) + QueryTxValue        , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue), Bal(UserAcc, S2)).
%%
sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle__remote(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle__data_(),

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    RespondOpts = #{},
    {{OperatorAcc, UserAcc},
     {OracleAcc, CallingCt},
     [_S0, %% State before oracle registration.
      S1, %% State after oracle registration.
      S2] %% State after query.
    } = sophia_oracles_qfee__flow_up_to_respond_(
          ?ORACLE_UNSAFE_REMOTE_CBS, %% Unsafe query.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue,
          RespondOpts),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(OperatorAcc, S1)                             , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1) - (QueryTxQFee - QueryTxValue), Bal(OracleAcc, S2)),
    ?assertEqual(Bal(CallingCt, S1)                               , Bal(CallingCt, S2)),
    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue)        , Bal(UserAcc, S2)).
%%
sophia_oracles_qfee__query_tx_value_below_qfee_does_not_take_from_poor_oracle__remote(_Cfg) ->
    {_InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle__data_(),
    InitialOracleCtBalance = 0,

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    {{OperatorAcc, UserAcc},
     {OracleAcc, CallingCt},
     [_S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__flow_up_to_query_(
          ?ORACLE_UNSAFE_REMOTE_CBS, %% Unsafe query.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue),
    S2 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(OperatorAcc, S1)                     , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1)                       , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(CallingCt, S1) + QueryTxValue        , Bal(CallingCt, S2)),
    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue), Bal(UserAcc, S2)).
%%
sophia_oracles_qfee__query_tx_value_below_qfee_does_not_take_from_rich_oracle_thanks_to_contract_check__remote(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle__data_(),

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    {{OperatorAcc, UserAcc},
     {OracleAcc, CallingCt},
     [_S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__flow_up_to_query_(
          ?ORACLE_SAFE_REMOTE_CBS, %% Safe query.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue),
    S2 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(OperatorAcc, S1)                     , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1)                       , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(CallingCt, S1) + QueryTxValue        , Bal(CallingCt, S2)),
    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue), Bal(UserAcc, S2)).
%%
sophia_oracles_qfee__remote_contract_query_value_below_qfee_takes_from_rich_oracle__remote(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue0, QueryTxQFee} =
        sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle__data_(),
    QueryTxValue = RegisterTxQFee,
    QueryRemoteCtValue = QueryTxValue0,

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee, remote_value => QueryRemoteCtValue},
    RespondOpts = #{},
    {{OperatorAcc, UserAcc},
     {OracleAcc, CallingCt},
     [_S0, %% State before oracle registration.
      S1, %% State after oracle registration.
      S2] %% State after query.
    } = sophia_oracles_qfee__flow_up_to_respond_(
          ?ORACLE_UNSAFE_REMOTE_CBS, %% Unsafe query.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue,
          RespondOpts),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(OperatorAcc, S1)                                    , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1) - (QueryTxValue - QueryRemoteCtValue), Bal(OracleAcc, S2)),
    ?assertEqual(Bal(CallingCt, S1) + (QueryTxValue - QueryRemoteCtValue), Bal(CallingCt, S2)),
    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue)               , Bal(UserAcc, S2)).
%%
sophia_oracles_qfee__remote_contract_query_value_below_qfee_does_not_take_from_poor_oracle__remote(_Cfg) ->
    {_InitialOracleCtBalance, RegisterTxQFee, QueryTxValue0, QueryTxQFee} =
        sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle__data_(),
    InitialOracleCtBalance = 0,
    QueryTxValue = RegisterTxQFee,
    QueryRemoteCtValue = QueryTxValue0,

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee, remote_value => QueryRemoteCtValue},
    {{OperatorAcc, UserAcc},
     {OracleAcc, CallingCt},
     [_S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__flow_up_to_query_(
          ?ORACLE_UNSAFE_REMOTE_CBS, %% Unsafe query.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue),
    S2 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(OperatorAcc, S1)                     , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1)                       , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(CallingCt, S1) + QueryTxValue        , Bal(CallingCt, S2)),
    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue), Bal(UserAcc, S2)).
%%
sophia_oracles_qfee__remote_contract_query_value_below_qfee_does_not_take_from_rich_oracle_thanks_to_contract_check__remote(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue0, QueryTxQFee} =
        sophia_oracles_qfee__query_tx_value_below_qfee_takes_from_rich_oracle__data_(),
    QueryTxValue = RegisterTxQFee,
    QueryRemoteCtValue = QueryTxValue0,

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee, remote_value => QueryRemoteCtValue},
    {{OperatorAcc, UserAcc},
     {OracleAcc, CallingCt},
     [_S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__flow_up_to_query_(
          ?ORACLE_SAFE_REMOTE_CBS, %% Safe query.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue),
    S2 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(OperatorAcc, S1)                     , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1)                       , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(CallingCt, S1) + QueryTxValue        , Bal(CallingCt, S2)),
    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue), Bal(UserAcc, S2)).

%% Attempt to create query with query fee larger than the one
%% requested by the oracle but not covered by call tx value uses
%% oracle contract balance: oracle contract should implement safety
%% mechanism.
sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_takes_from_rich_oracle__data_() ->
    QFeeExcess = 1,
    RegisterTxQFee = 100,
    {_InitialOracleCtBalance = 10 + QFeeExcess,
     RegisterTxQFee,
     _QueryTxValue = RegisterTxQFee,
     _QueryTxQFee = QFeeExcess + RegisterTxQFee}.
%%
sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_takes_from_rich_oracle(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_takes_from_rich_oracle__data_(),

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    RespondOpts = #{},
    {{OperatorAcc, UserAcc},
     {OracleAcc, OracleAcc = _CallingCt},
     [_S0, %% State before oracle registration.
      S1, %% State after oracle registration.
      S2] %% State after query.
    } = sophia_oracles_qfee__flow_up_to_respond_(
          ?ORACLE_UNSAFE_CBS, %% Unsafe query.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue,
          RespondOpts),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(OperatorAcc, S1)                               , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1) - (QueryTxQFee - RegisterTxQFee), Bal(OracleAcc, S2)),
    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue)          , Bal(UserAcc, S2)).
%%
sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_does_not_take_from_poor_oracle(_Cfg) ->
    {_InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_takes_from_rich_oracle__data_(),
    InitialOracleCtBalance = 0,

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    {{OperatorAcc, UserAcc},
     {OracleAcc, OracleAcc = _CallingCt},
     [_S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__flow_up_to_query_(
          ?ORACLE_UNSAFE_CBS, %% Unsafe query.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue),
    S2 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(OperatorAcc, S1)                     , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1) + QueryTxValue        , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue), Bal(UserAcc, S2)).
%%
sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_does_not_take_from_rich_oracle_thanks_to_contract_check(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__qfee_in_query_above_qfee_in_oracle_takes_from_rich_oracle__data_(),

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    {{OperatorAcc, UserAcc},
     {OracleAcc, OracleAcc = _CallingCt},
     [_S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__flow_up_to_query_(
          ?ORACLE_SAFE_CBS, %% Safe query.
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue),
    S2 = state(),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(OperatorAcc, S1)                     , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1) + QueryTxValue        , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue), Bal(UserAcc, S2)).

%% Failure after query creation primop succeeds.
sophia_oracles_qfee__error_after_primop(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__basic__data_(),

    Cbs =
        ?ORACLE_UNSAFE_CBS#oracle_cbs{
           query =
               fun(UserAcc, OCt, OCt, Opts, TxOpts, S) ->
                       oracle_query_from_contract_(unsafeCreateQueryThenErr, UserAcc, OCt, OCt, Opts, TxOpts, S)
               end},

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    {{OperatorAcc, UserAcc},
     {OracleAcc, OracleAcc = _CallingCt},
     [_S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__flow_up_to_query_(
          Cbs,
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue),
    S2 = state(),

    ?assertEqual([], aect_test_utils:get_oracle_queries(OracleAcc, S1)),
    ?assertEqual([], aect_test_utils:get_oracle_queries(OracleAcc, S2)),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue), Bal(UserAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1) + QueryTxValue        , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(OperatorAcc, S1)                     , Bal(OperatorAcc, S2)).
%%
sophia_oracles_qfee__inner_error_after_primop__remote(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__basic__data_(),

    InitialState0 = aect_test_utils:new_state(),
    {TmpAcc, InitialState1} = new_account(1000000, InitialState0),
    {OracleErrCt, InitialState} = create_contract(TmpAcc, oracles_err, {}, #{amount => 0}, InitialState1),

    Cbs =
        ?ORACLE_UNSAFE_REMOTE_CBS#oracle_cbs{
           query =
               fun(UserAcc, RCt, OCt, Opts, TxOpts, S) ->
                       oracle_query_from_remote_contract_(callUnsafeCreateQueryThenErr, UserAcc, RCt, OracleErrCt, OCt, Opts, TxOpts, S)
               end},

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    {{OperatorAcc, UserAcc},
     {OracleAcc, CallingCt},
     [_S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__flow_up_to_query_(
          InitialState,
          Cbs,
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue),
    S2 = state(),

    ?assertEqual([], aect_test_utils:get_oracle_queries(OracleAcc, S1)),
    ?assertEqual([], aect_test_utils:get_oracle_queries(OracleAcc, S2)),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue), Bal(UserAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1)                       , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(CallingCt, S1) + QueryTxValue        , Bal(CallingCt, S2)),
    ?assertEqual(Bal(OperatorAcc, S1)                     , Bal(OperatorAcc, S2)),
    ?assertEqual(Bal(TmpAcc, S1)                          , Bal(TmpAcc, S2)).
%%
sophia_oracles_qfee__outer_error_after_primop__remote(_Cfg) ->
    {InitialOracleCtBalance, RegisterTxQFee, QueryTxValue, QueryTxQFee} =
        sophia_oracles_qfee__basic__data_(),

    Cbs =
        ?ORACLE_UNSAFE_REMOTE_CBS#oracle_cbs{
           query =
               fun(UserAcc, RCt, OCt, Opts, TxOpts, S) ->
                       oracle_query_from_remote_contract_(callUnsafeCreateQueryAndThenErr, UserAcc, RCt, OCt, Opts, TxOpts, S)
               end},

    TxFee = 2,
    RegisterOpts = #{qfee => RegisterTxQFee},
    QueryOpts = #{qfee => QueryTxQFee},
    {{OperatorAcc, UserAcc},
     {OracleAcc, CallingCt},
     [_S0, %% State before oracle registration.
      S1] %% State after oracle registration.
    } = sophia_oracles_qfee__flow_up_to_query_(
          Cbs,
          TxFee, InitialOracleCtBalance, RegisterOpts, QueryOpts, QueryTxValue),
    S2 = state(),

    ?assertEqual([], aect_test_utils:get_oracle_queries(OracleAcc, S1)),
    ?assertEqual([], aect_test_utils:get_oracle_queries(OracleAcc, S2)),

    Bal = fun(A, S) -> {B, S} = account_balance(A, S), B end,

    ?assertEqual(Bal(UserAcc, S1) - (TxFee + QueryTxValue), Bal(UserAcc, S2)),
    ?assertEqual(Bal(OracleAcc, S1)                       , Bal(OracleAcc, S2)),
    ?assertEqual(Bal(CallingCt, S1) + QueryTxValue        , Bal(CallingCt, S2)),
    ?assertEqual(Bal(OperatorAcc, S1)                     , Bal(OperatorAcc, S2)).

%% Testing map functions and primitives
sophia_maps(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc = ?call(new_account, 1000000000),
    Ct  = ?call(create_contract, Acc, maps, {}),

    Call = fun(Fn, Type, Args) -> ?call(call_contract, Acc, Ct, Fn, Type, Args) end,

    Pt     = {tuple, [word, word]},
    IntMap = {map, word,   Pt},
    StrMap = {map, string, Pt},
    IntList = {list, {tuple, [word,   Pt]}},
    StrList = {list, {tuple, [string, Pt]}},
    Unit   = {tuple, []},
    State  = {tuple, [IntMap, StrMap]},

    MapI     = #{1 => {1, 2}, 2 => {3, 4}, 3 => {5, 6}},
    MapS     = #{<<"one">> => {1, 2}, <<"two">> => {3, 4}, <<"three">> => {5, 6}},
    EmptyMap = #{},

    MapI = Call(map_i, IntMap, {}),
    MapS = Call(map_s, StrMap, {}),

    {} = Call(map_state_i, Unit, {}),
    {} = Call(map_state_s, Unit, {}),

    {MapI, MapS} = Call(get_state, State, {}),

    MkOption = fun(undefined) -> none; (X) -> {some, X} end,

    OogErr = {error, <<"out_of_gas">>},
    Calls = lists:append(
        %% get
        [ [{Fn,  Pt, {K, Map}, maps:get(K, Map, OogErr)},
           {FnS, Pt, K,        maps:get(K, Map, OogErr)}]
         || {Fn, FnS, Map, Err} <- [{get_i, get_state_i, MapI, 4},
                                    {get_s, get_state_s, MapS, <<"four">>}],
            K <- maps:keys(Map) ++ [Err] ] ++
        %% lookup
        [ [{Fn,  {option, Pt}, {K, Map}, MkOption(maps:get(K, Map, undefined))},
           {FnS, {option, Pt}, K,        MkOption(maps:get(K, Map, undefined))}]
         || {Fn, FnS, Map, Err} <- [{lookup_i, lookup_state_i, MapI, 4},
                                    {lookup_s, lookup_state_s, MapS, <<"four">>}],
            K <- maps:keys(Map) ++ [Err] ] ++
        %% member
        [ [{Fn,  bool, {K, Map}, maps:is_key(K, Map)},
           {FnS, bool, K,        maps:is_key(K, Map)}]
         || {Fn, FnS, Map, Err} <- [{member_i, member_state_i, MapI, 4},
                                    {member_s, member_state_s, MapS, <<"four">>}],
            K <- maps:keys(Map) ++ [Err] ] ++
        %% size
        [ [{Fn,  word, Map, maps:size(Map)},
           {FnS, word, {},  maps:size(Map)}]
         || {Fn, FnS, Map} <- [{size_i, size_state_i, MapI},
                               {size_s, size_state_s, MapS}] ] ++
        %% set (not set_state)
        [ [{Fn, Type, {K, V, Map}, Map#{K => V}}]
         || {Fn, Type, Map, New, V} <- [{set_i, IntMap, MapI, 4, {7, 8}},
                                        {set_s, StrMap, MapS, <<"four">>, {7, 8}}],
            K <- maps:keys(Map) ++ [New] ] ++
        %% setx (not setx_state)
        [ [{Fn, Type, {K, V, Map}, case Map of #{K := {_, Y}} -> Map#{K => {V, Y}}; _ -> OogErr end}]
         || {Fn, Type, Map, New, V} <- [{setx_i, IntMap, MapI, 4, 7},
                                        {setx_s, StrMap, MapS, <<"four">>, 7}],
            K <- maps:keys(Map) ++ [New] ] ++
        %% addx (not addx_state)
        [ [{Fn, Type, {K, V, Map}, case Map of #{K := {X, Y}} -> Map#{K => {X + V, Y}}; _ -> OogErr end}]
         || {Fn, Type, Map, New, V} <- [{addx_i, IntMap, MapI, 4, 7},
                                        {addx_s, StrMap, MapS, <<"four">>, 7}],
            K <- maps:keys(Map) ++ [New] ] ++
        %% delete (not delete_state)
        [ [{Fn, Type, {K, Map}, maps:remove(K, Map)}]
         || {Fn, Type, Map, New} <- [{delete_i, IntMap, MapI, 4},
                                     {delete_s, StrMap, MapS, <<"four">>}],
            K <- maps:keys(Map) ++ [New] ] ++
        %% fromlist (not fromlist_state)
        [ [{Fn, Type, maps:to_list(Map), Map}]
         || {Fn, Type, Map} <- [{fromlist_i, IntMap, MapI},
                                {fromlist_s, StrMap, MapS}] ] ++
        []),

    _ = [ Result = Call(Fn, Type, Args) || {Fn, Type, Args, Result} <- Calls ],

    %% to_list (not tolist_state)
    _ = [ {Xs, Xs} = {lists:keysort(1, Call(Fn, Type, Map)), maps:to_list(Map)}
            || {Fn, Type, Map} <- [{tolist_i, IntList, MapI},
                                   {tolist_s, StrList, MapS}] ],

    %% Reset the state
    Call(fromlist_state_i, Unit, []),
    Call(fromlist_state_s, Unit, []),
    {EmptyMap, EmptyMap} = Call(get_state, State, {}),

    %% fromlist_state
    Call(fromlist_state_i, Unit, maps:to_list(MapI)),
    Call(fromlist_state_s, Unit, maps:to_list(MapS)),
    {MapI, MapS} = Call(get_state, State, {}),

    %% tolist_state
    _ = [ {Xs, Xs} = {lists:keysort(1, Call(Fn, Type, {})), maps:to_list(Map)}
            || {Fn, Type, Map} <- [{tolist_state_i, IntList, MapI},
                                   {tolist_state_s, StrList, MapS}] ],

    %% set_state
    DeltaI1 = #{ 3 => {100, 200}, 4 => {300, 400} },
    DeltaS1 = #{ <<"three">> => {100, 200}, <<"four">> => {300, 400} },
    MapI1 = maps:merge(MapI, DeltaI1),
    MapS1 = maps:merge(MapS, DeltaS1),
    _ = [ {} = Call(Fn, Unit, {K, V})
            || {Fn, Delta} <- [{set_state_i, DeltaI1}, {set_state_s, DeltaS1}],
               {K, V} <- maps:to_list(Delta) ],
    {MapI1, MapS1} = Call(get_state, State, {}),

    %% setx_state/addx_state
    DeltaI2 = [{set, 4, 50}, {set, 5, 300}, {add, 2, 10}, {add, 5, 10}],
    DeltaS2 = [{set, <<"four">>, 50}, {set, <<"five">>, 300}, {add, <<"one">>, 100}, {add, <<"...">>, 1}],
    Upd = fun({Op, K, V}, M) ->
            case maps:get(K, M, undefined) of
                undefined -> M;
                {X, Y}    -> M#{K := {case Op of set -> V; add -> X + V end, Y}} end end,
    MapI2 = lists:foldr(Upd, MapI1, DeltaI2),
    MapS2 = lists:foldr(Upd, MapS1, DeltaS2),
    _ = [ begin
            T   = if is_integer(K) -> i; true -> s end,
            Fn  = list_to_atom(lists:concat([Op, "x_state_", T])),
            Res = case maps:is_key(K, Map) of true -> {}; false -> OogErr end,
            Res = Call(Fn, Unit, {K, V})
          end || {Map, Delta} <- [{MapI1, DeltaI2}, {MapS1, DeltaS2}],
               {Op, K, V} <- Delta ],
    {MapI2, MapS2} = Call(get_state, State, {}),

    %% delete_state
    DeltaI3 = [2, 5],
    DeltaS3 = [<<"four">>, <<"five">>],
    MapI3 = lists:foldr(fun maps:remove/2, MapI2, DeltaI3),
    MapS3 = lists:foldr(fun maps:remove/2, MapS2, DeltaS3),
    _ = [ {} = Call(Fn, Unit, K)
            || {Fn, Ks} <- [{delete_state_i, DeltaI3}, {delete_state_s, DeltaS3}],
               K <- Ks ],
    {MapI3, MapS3} = Call(get_state, State, {}),

    ok.

sophia_variant_types(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc = <<AccId:256>> = ?call(new_account, 1000000),
    Ct  = ?call(create_contract, Acc, variant_types, {}),
    Call = fun(Fn, Type, Args) -> ?call(call_contract, Acc, Ct, Fn, Type, Args) end,
    Color  = {variant_t, [{red, []}, {green, []}, {blue, []}, {grey, [word]}]},
    StateR = {tuple, [word, word, Color]},
    State  = {variant_t, [{started, [StateR]}, {stopped, []}]},
    Unit   = {tuple, []},
    stopped   = Call(get_state, State, {}),
    {}        = Call(start, Unit, {123}),
    {grey, 0} = Call(get_color, Color, {}),
    {}        = Call(set_color, Unit, {{1}}),   %% green has tag 1
    {started, {AccId, 123, green}} = Call(get_state, State, {}),
    ok.


%% The crowd funding example.

-record(fundme_scenario,
    { name
    , goal
    , deadline
    , events }).

run_scenario(#fundme_scenario
             { name     = Scenario
             , goal     = Goal
             , deadline = Deadline
             , events   = Events }) ->

    state(aect_test_utils:new_state()),
    Denomination  = 1000 * 1000,
    StartingFunds = 1000 * 1000 * Denomination,
    InvestorNames = [ Investor || {contribute, Investor, _Amount, _Height} <- Events ],

    %% Set up accounts
    Beneficiary   = ?call(new_account, StartingFunds),
    Organiser     = ?call(new_account, StartingFunds),
    Investors     = maps:from_list([ {Name, ?call(new_account, StartingFunds)} || Name <- InvestorNames ]),

    %% Create the contract
    Contract      = ?call(create_contract, Organiser, fundme, {Beneficiary, Deadline, Goal * Denomination}),

    %% Run the events
    Account = fun(beneficiary) -> Beneficiary; (Name) -> maps:get(Name, Investors) end,
    RunEvent = fun({contribute, Name, Amount, Height}) ->
                    ?call(call_contract, Account(Name), Contract, contribute, bool, {},
                                #{amount => Amount * Denomination, height => Height});
                  ({withdraw, Name, Height, _}) ->
                    ?call(call_contract, Account(Name), Contract, withdraw, {tuple, []}, {},
                          #{height => Height})
               end,

    Results = [ {E, RunEvent(E)} || E <- Events ],

    %% Analyse scenario
    Contributed = fun(By) ->
        lists:sum([ Amount || {contribute, Name, Amount, Height} <- Events,
                              By == any orelse By == Name, Height < Deadline ]) end,

    TotalFunds    = Contributed(any),
    Funded        = TotalFunds >= Goal,

    Withdrawn = fun(By) ->
            [] /= [ w || {withdraw, Name, Height, _} <- Events,
                         Name == By, Height >= Deadline, not Funded ]
        end,

    Contributions = maps:map(fun(Name, _) ->
        case Withdrawn(Name) of
            true  -> 0;
            false -> Contributed(Name)
        end end, Investors),

    GasDelta = 100000,
    Is = fun(_, Expect, Actual) when Expect - GasDelta =< Actual, Actual =< Expect -> true;
            (Tag, Expect, Actual) -> {Scenario, Tag, Actual, is_not, Expect, minus_gas} end,

    BeneficiaryWithdraw = [] /= [ w || {withdraw, beneficiary, Height, _} <- Events,
                                       Funded, Height >= Deadline ],

    io:format("TotalFunds = ~p\n", [TotalFunds]),

    %% Check results
    ExpectedResult =
        fun({withdraw, _, _, ok})       -> {};
           ({withdraw, _, _, error})    -> {error, <<"out_of_gas">>};
           ({contribute, _, _, Height}) -> Height < Deadline end,
    lists:foreach(fun({E, Res}) ->
        Expect = ExpectedResult(E),
        case Expect == Res of
            true -> ok;
            _    -> exit({Scenario, E, expected, Expect, got, Res})
        end end, Results),

    %% Check beneficiary balance
    BalanceB = ?call(account_balance, Beneficiary),
    true = Is(beneficiary,
              if BeneficiaryWithdraw -> TotalFunds * Denomination;
                 true                -> 0 end, BalanceB - StartingFunds),

    %% Check investor balances
    lists:foreach(fun({Name, Acc}) ->
            Bal    = ?call(account_balance, Acc),
            Expect = -maps:get(Name, Contributions),
            true = Is(Name, Expect * Denomination, Bal - StartingFunds)
        end, maps:to_list(Investors)),

    ok.

sophia_fundme(_Cfg) ->
    Funded = #fundme_scenario{
        name     = funded_scenario,
        goal     = 10,
        deadline = 2000,
        events   =
            [{contribute, {investor, I}, I, 1000 + 100 * I} || I <- lists:seq(1, 5)] ++
            [{contribute, {investor, 2}, 5, 1900},
             {withdraw, beneficiary, 2100,   ok},
             {contribute, {investor, 1}, 3, 2150},
             {withdraw, beneficiary, 2200,   error},
             {withdraw, {investor, 5}, 2200, error} ] },

    NotFunded = #fundme_scenario{
        name     = not_funded_scenario,
        goal     = 25,
        deadline = 2000,
        events   =
            [{contribute, {investor, I}, I, 1000 + 100 * I} || I <- lists:seq(1, 5)] ++
            [{contribute, {investor, 2}, 5, 1900},
             {withdraw, beneficiary, 2100, error},
             {contribute, {investor, 2}, 3, 2150}] ++
            [{withdraw, {investor, I}, 2200 + I, ok} || I <- lists:seq(1, 4)] ++
            [{withdraw, {investor, 3}, 2300, error}] },

    run_scenario(Funded),
    run_scenario(NotFunded).

%% AENS tests

aens_preclaim(PubKey, Name, S) ->
    aens_preclaim(PubKey, Name, #{}, S).

aens_preclaim(PubKey, Name, Options, S) ->
    Salt   = rand:uniform(10000),
    Nonce  = aect_test_utils:next_nonce(PubKey, S),
    Height = maps:get(height, Options, 1),
    Fee    = maps:get(fee, Options, 1),
    TTL    = maps:get(ttl, Options, 1000),
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    CHash = aens_hash:commitment_hash(NameAscii, Salt),
    {ok, Tx} = aens_preclaim_tx:new(#{ account => aec_id:create(account, PubKey),
                                       nonce => Nonce,
                                       commitment => aec_id:create(commitment, CHash),
                                       fee => Fee,
                                       ttl => TTL }),
    PrivKey  = aect_test_utils:priv_key(PubKey, S),
    {ok, S1} = sign_and_apply_transaction(Tx, PrivKey, S, Height),
    {Salt, S1}.

aens_claim(PubKey, Name, Salt, S) ->
    aens_claim(PubKey, Name, Salt, #{}, S).

aens_claim(PubKey, Name, Salt, Options, S) ->
    Nonce  = aect_test_utils:next_nonce(PubKey, S),
    Height = maps:get(height, Options, 2),
    Fee    = maps:get(fee, Options, 1),
    TTL    = maps:get(ttl, Options, 1000),
    {ok, NameAscii} = aens_utils:to_ascii(Name),
    NameHash = aens_hash:name_hash(NameAscii),
    {ok, Tx} = aens_claim_tx:new(#{ account   => aec_id:create(account, PubKey),
                                    nonce     => Nonce,
                                    name      => Name,
                                    name_salt => Salt,
                                    fee       => Fee,
                                    ttl       => TTL }),
    PrivKey  = aect_test_utils:priv_key(PubKey, S),
    {ok, S1} = sign_and_apply_transaction(Tx, PrivKey, S, Height),
    {NameHash, S1}.

aens_revoke(PubKey, Hash, S) ->
    aens_revoke(PubKey, Hash, #{}, S).

aens_revoke(PubKey, Hash, Options, S) ->
    Nonce  = aect_test_utils:next_nonce(PubKey, S),
    Height = maps:get(height, Options, 3),
    Fee    = maps:get(fee, Options, 1),
    TTL    = maps:get(ttl, Options, 1000),
    {ok, Tx} = aens_revoke_tx:new(#{ account   => aec_id:create(account, PubKey),
                                     nonce     => Nonce,
                                     name_hash => aec_id:create(name, Hash),
                                     fee       => Fee,
                                     ttl       => TTL }),
    PrivKey  = aect_test_utils:priv_key(PubKey, S),
    {ok, S1} = sign_and_apply_transaction(Tx, PrivKey, S, Height),
    {ok, S1}.

aens_update(PubKey, NameHash, Pointers, S) ->
    aens_update(PubKey, NameHash, Pointers, #{}, S).

aens_update(PubKey, NameHash, Pointers, Options, S) ->
    Nonce     = aect_test_utils:next_nonce(PubKey, S),
    Height    = maps:get(height, Options, 2),
    Fee       = maps:get(fee, Options, 1),
    TTL       = maps:get(ttl, Options, 1000),
    ClientTTL = maps:get(client_ttl, Options, 1000),
    NameTTL   = maps:get(name_ttl, Options, 1000),
    {ok, Tx}  = aens_update_tx:new(#{ account    => aec_id:create(account, PubKey),
                                      nonce      => Nonce,
                                      name_hash  => aec_id:create(name, NameHash),
                                      name_ttl   => NameTTL,
                                      pointers   => Pointers,
                                      client_ttl => ClientTTL,
                                      fee        => Fee,
                                      ttl        => TTL }),
    PrivKey  = aect_test_utils:priv_key(PubKey, S),
    {ok, S1} = sign_and_apply_transaction(Tx, PrivKey, S, Height),
    {ok, S1}.

sophia_aens(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc      = ?call(new_account, 1000000),
    Ct       = ?call(create_contract, Acc, aens, {}, #{ amount => 100000 }),
    Name     = <<"foo.test">>,
    NameKey  = 101101,
    URL      = <<"shadycorp.com">>,
    Pointers = [ {atom_to_binary(Key, utf8), aec_base58c:encode(Key, Val)}
                || {Key, Val} <- [ {account_pubkey, <<NameKey:256>>},
                                   {name, URL} ] ],
    Salt  = ?call(aens_preclaim, Acc, Name),
    Hash  = ?call(aens_claim, Acc, Name, Salt),
    ok    = ?call(aens_update, Acc, Hash, Pointers),

    {some, NameKey} = ?call(call_contract, Acc, Ct, resolve_word,   {option, word},   {Name, <<"account_pubkey">>}),
    none            = ?call(call_contract, Acc, Ct, resolve_word,   {option, word},   {Name, <<"oracle_pubkey">>}),
    {some, URL}     = ?call(call_contract, Acc, Ct, resolve_string, {option, string}, {Name, <<"name">>}),
    ok              = ?call(aens_revoke, Acc, Hash),
    none            = ?call(call_contract, Acc, Ct, resolve_string, {option, string}, {Name, <<"name">>}),

    %% AENS transactions from contract

    Name1           = <<"bla.test">>,
    Salt1           = rand:uniform(10000),
    {ok, NameAscii} = aens_utils:to_ascii(Name1),
    CHash           = aens_hash:commitment_hash(NameAscii, Salt1),
    NHash           = aens_hash:name_hash(NameAscii),
    {} = ?call(call_contract, Acc, Ct, preclaim, {tuple, []}, {Ct, CHash, 0},        #{ height => 10 }),
    {} = ?call(call_contract, Acc, Ct, claim,    {tuple, []}, {Ct, Name1, Salt1, 0}, #{ height => 11 }),
    {} = ?call(call_contract, Acc, Ct, transfer, {tuple, []}, {Ct, Acc, NHash, 0},   #{ height => 12 }),
    ok = ?call(aens_update, Acc, NHash, Pointers),
    {some, URL} = ?call(call_contract, Acc, Ct, resolve_string, {option, string}, {Name1, <<"name">>}),
    {error, <<"out_of_gas">>} = ?call(call_contract, Acc, Ct, revoke, {tuple, []}, {Ct, NHash, 0}, #{ height => 13 }),

    ok.


%%%===================================================================
%%% Store
%%%===================================================================

create_store(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1  = ?call(new_account, 100),
    Ct1   = ?call(insert_contract, Acc1, <<"Code for C1">>),
    Ct1   = ?call(get_contract, Ct1),
    #{}   = aect_contracts:state(Ct1),
    ok.

update_store(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1   = ?call(new_account, 100),
    Ct1    = ?call(insert_contract, Acc1, <<"Code for C1">>),
    Ct1    = ?call(get_contract, Ct1),
    Store1 = #{ <<0>> => <<42>> },
    Ct2    = aect_contracts:set_state(Store1, Ct1),
    Ct2    = ?call(enter_contract, Ct2),
    Ct2    = ?call(get_contract, Ct2),
    ok.

read_store(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1   = ?call(new_account, 100),
    Ct1    = ?call(insert_contract, Acc1, <<"Code for C1">>),
    Ct1    = ?call(get_contract, Ct1),
    Store1 = #{ <<0>> => <<42>> },
    Ct2    = aect_contracts:set_state(Store1, Ct1),
    Ct2    = ?call(enter_contract, Ct2),
    Ct2    = ?call(get_contract, Ct2),
    Store1 = aect_contracts:state(Ct2),
    ok.


store_zero_value(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1   = ?call(new_account, 100),
    Ct1    = ?call(insert_contract, Acc1, <<"Code for C1">>),
    Ct1    = ?call(get_contract, Ct1),
    Store1 = #{ <<0>> => <<42>>
              , <<1>> => <<0>>
              , <<2>> => <<>> },
    Ct2    = aect_contracts:set_state(Store1, Ct1),
    Ct2    = ?call(enter_contract, Ct2),
    %% Empty values are removed in state tree.
    Ct3    = ?call(get_contract, Ct2),
    Store2 = #{ <<0>> => <<42>>
              , <<1>> => <<0>>},
    Store2 = aect_contracts:state(Ct3),
    ok.

merge_new_zero_value(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1   = ?call(new_account, 100),
    Ct1    = ?call(insert_contract, Acc1, <<"Code for C1">>),
    Ct1    = ?call(get_contract, Ct1),
    Store1 = #{ <<0>> => <<42>>
              , <<1>> => <<0>>
              , <<2>> => <<>> },
    Ct2    = aect_contracts:set_state(Store1, Ct1),
    Ct2    = ?call(enter_contract, Ct2),
    %% Empty values are removed in state tree.
    Ct3    = ?call(get_contract, Ct2),
    Store2 = #{ <<0>> => <<0>>
              , <<1>> => <<>>
              , <<2>> => <<42>> },
    Ct4    = aect_contracts:set_state(Store2, Ct3),
    Ct4    = ?call(enter_contract, Ct4),
    Ct5    = ?call(get_contract, Ct4),
    Store3 = #{ <<0>> => <<0>>
              , <<2>> => <<42>>},
    Store3 = aect_contracts:state(Ct5),
    ok.


merge_missing_keys(_Cfg) ->
    state(aect_test_utils:new_state()),
    Acc1   = ?call(new_account, 100),
    Ct1    = ?call(insert_contract, Acc1, <<"Code for C1">>),
    Ct1    = ?call(get_contract, Ct1),
    Store1 = #{ <<0>> => <<42>>
              , <<1>> => <<17>>
              , <<2>> => <<>> },
    Ct2    = aect_contracts:set_state(Store1, Ct1),
    Ct2    = ?call(enter_contract, Ct2),
    %% Empty values are removed in state tree.
    Ct3    = ?call(get_contract, Ct2),
    Store2 = #{ <<3>> => <<1,2,3,4>>
              , <<2>> => <<42>> },
    Ct4    = aect_contracts:set_state(Store2, Ct3),
    Ct4    = ?call(enter_contract, Ct4),
    Ct5    = ?call(get_contract, Ct4),
    Store3 = #{ <<2>> => <<42>>
              , <<3>> => <<1,2,3,4>>},
    Store3 = aect_contracts:state(Ct5),
    ok.


enter_contract(Contract, S) ->
    Contracts = aect_state_tree:enter_contract(Contract, aect_test_utils:contracts(S)),
    {Contract, aect_test_utils:set_contracts(Contracts, S)}.
