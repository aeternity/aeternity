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
        , call_contract_negative/1
        , create_contract/1
        , create_contract_negative/1
        ]).

-include_lib("common_test/include/ct.hrl").

-include_lib("apps/aecontract/include/contract_txs.hrl").

%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, all_tests}
    ].

groups() ->
    [ {all_tests, [sequence], [ {group, transactions}
                              ]}
    , {transactions, [sequence], [ create_contract
                                 , create_contract_negative
                                 , call_contract
                                 , call_contract_negative
                                 ]}
    ].

%%%===================================================================
%%% Register contract
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
    {_, [], S1}  = sign_and_apply_transaction(RTx1, BadPrivKey, S1),
    {error, account_not_found} = aect_create_tx:check(RTx1, Trees, CurrHeight),

    %% Insufficient funds
    S2     = aect_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aect_test_utils:trees(S2),
    RTx2   = aect_test_utils:create_tx(PubKey, S2),
    {_, [], S2}  = sign_and_apply_transaction(RTx2, PrivKey, S2),
    {error, insufficient_funds} =
        aect_create_tx:check(RTx2, Trees2, CurrHeight),

    %% Test too high account nonce
    RTx3 = aect_test_utils:create_tx(PubKey, #{nonce => 0}, S1),
    {_, [], S1}  = sign_and_apply_transaction(RTx3, PrivKey, S1),
    {error, account_nonce_too_high} =
        aect_create_tx:check(RTx3, Trees, CurrHeight),

    ok.

create_contract(_Cfg) ->
    {PubKey, S1} = aect_test_utils:setup_new_account(aect_test_utils:new_state()),
    Tx           = aect_test_utils:create_tx(PubKey, #{}, S1),
    PrivKey      = aect_test_utils:priv_key(PubKey, S1),

    %% Test that the create transaction is accepted
    {SignedTx, [SignedTx], S2} = sign_and_apply_transaction(Tx, PrivKey, S1),
    {PubKey, S2}.

sign_and_apply_transaction(Tx, PrivKey, S1) ->
    SignedTx = aec_tx_sign:sign(Tx, PrivKey),
    Trees    = aect_test_utils:trees(S1),
    Height   = 1,
    {ok, AcceptedTxs, Trees1} = aec_tx:apply_signed([SignedTx], Trees, Height),
    S2       = aect_test_utils:set_trees(Trees1, S1),
    {SignedTx, AcceptedTxs, S2}.

%%%===================================================================
%%% Call contract
%%%===================================================================

call_contract_negative(_Cfg) ->
    %% PLACEHOLDER
    ok.

call_contract(_Cfg) ->
    %% PLACEHOLDER
    ok.

