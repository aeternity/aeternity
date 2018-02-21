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
        , state_tree/1
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
                              , {group, state_tree}
                              ]}
    , {transactions, [sequence], [ create_contract
                                 , create_contract_negative
                                 , call_contract
                                 , call_contract_negative
                                 ]}
    , {state_tree, [sequence], [ state_tree ]}
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
    {_, [], S1}  = sign_and_apply_transaction(RTx1, BadPrivKey, S1),
    {error, account_not_found} = aetx:check(RTx1, Trees, CurrHeight),

    %% Insufficient funds
    S2     = aect_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aect_test_utils:trees(S2),
    RTx2   = aect_test_utils:create_tx(PubKey, S2),
    {_, [], S2}  = sign_and_apply_transaction(RTx2, PrivKey, S2),
    {error, insufficient_funds} = aetx:check(RTx2, Trees2, CurrHeight),

    %% Test too high account nonce
    RTx3 = aect_test_utils:create_tx(PubKey, #{nonce => 0}, S1),
    {_, [], S1}  = sign_and_apply_transaction(RTx3, PrivKey, S1),
    {error, account_nonce_too_high} = aetx:check(RTx3, Trees, CurrHeight),

    ok.

create_contract(_Cfg) ->
    {PubKey, S1} = aect_test_utils:setup_new_account(aect_test_utils:new_state()),
    Tx           = aect_test_utils:create_tx(PubKey, #{}, S1),
    PrivKey      = aect_test_utils:priv_key(PubKey, S1),

    %% Test that the create transaction is accepted
    {SignedTx, [SignedTx], S2} = sign_and_apply_transaction(Tx, PrivKey, S1),
    {PubKey, S2}.

sign_and_apply_transaction(Tx, PrivKey, S1) ->
    SignedTx = aetx_sign:sign(Tx, PrivKey),
    Trees    = aect_test_utils:trees(S1),
    Height   = 1,
    {ok, AcceptedTxs, Trees1} = aec_trees:apply_signed_txs([SignedTx], Trees, Height),
    S2       = aect_test_utils:set_trees(Trees1, S1),
    {SignedTx, AcceptedTxs, S2}.

%%%===================================================================
%%% Call contract
%%%===================================================================

call_contract_negative(_Cfg) ->
    %% PLACEHOLDER
    ok.

call_contract(_Cfg) ->
    S0            = aect_test_utils:new_state(),
    {Owner,  S1}  = aect_test_utils:setup_new_account(S0),
    {Caller, S2}  = aect_test_utils:setup_new_account(S1),
    OwnerPrivKey  = aect_test_utils:priv_key(Owner, S2),
    CallerPrivKey = aect_test_utils:priv_key(Caller, S2),

    CallerBalance = aec_accounts:balance(aect_test_utils:get_account(Caller, S2)),

    IdContract   = aect_test_utils:compile_contract("contracts/identity.aer"),
    CreateTx     = aect_test_utils:create_tx(Owner, #{code => IdContract}, S2),

    %% Test that the create transaction is accepted
    {SignedTx, [SignedTx], S3} = sign_and_apply_transaction(CreateTx, OwnerPrivKey, S2),
    ContractKey = aect_contracts:compute_contract_pubkey(Owner, aetx:nonce(CreateTx)),

    %% Now call check that we can call it.
    Fee           = 107,
    GasPrice      = 2,
    Arg           = <<"42">>,
    CallData = aect_ring:create_call(IdContract, <<"main">>, Arg),
    CallTx = aect_test_utils:call_tx(Caller, ContractKey,
                                     #{call_data => CallData, gas_price => GasPrice, fee => Fee}, S3),
    {SignedTx1, [SignedTx1], S4} = sign_and_apply_transaction(CallTx, CallerPrivKey, S3),
    CallId = aect_call:id(Caller, aetx:nonce(CallTx), ContractKey),

    %% Check that it got stored and that we got the right return value
    Call = aect_state_tree:get_call(ContractKey, CallId, aect_test_utils:contracts(S4)),
    <<42:256>> = aect_call:return_value(Call),

    %% ...and that we got charged the right amount for gas and fee.
    NewCallerBalance = aec_accounts:balance(aect_test_utils:get_account(Caller, S4)),
    NewCallerBalance = CallerBalance - Fee - GasPrice * aect_call:gas_used(Call),

    {ok, S4}.

%%%===================================================================
%%% State trees
%%%===================================================================

make_contract(PubKey = <<_:32, Rest/binary>>, Code, S) ->
    Tx = aect_test_utils:create_tx(PubKey, #{ code => Code }, S),
    ContractKey = <<"CODE", Rest/binary>>,
    {aect_create_tx, CTx} = aetx:specialize_type(Tx),
    aect_contracts:new(ContractKey, CTx, 1).

make_call(PubKey, ContractKey, Call, S) ->
    Tx = aect_test_utils:call_tx(PubKey, ContractKey, #{ call => Call }, S),
    {aect_call_tx, CTx} = aetx:specialize_type(Tx),
    aect_call:new(CTx, 1).

state()  -> get(the_state).
state(S) -> put(the_state, S).

call(Name, Fun, Xs) ->
    S = state(),
    Fmt = string:join(lists:duplicate(length(Xs), "~p"), ", "),
    io:format("~p(" ++ Fmt ++ ") ->\n", [Name | Xs]),
    {R, S1} = try apply(Fun, Xs ++ [S])
              catch _:Reason -> {{'EXIT', Reason, erlang:get_stacktrace()}, S}
              end,
    io:format("  ~p\n", [R]),
    state(S1),
    R.

-define(call(Fun, X),       call(Fun, fun Fun/2, [X])).
-define(call(Fun, X, Y),    call(Fun, fun Fun/3, [X, Y])).
-define(call(Fun, X, Y, Z), call(Fun, fun Fun/4, [X, Y, Z])).

new_account(Balance, S) ->
    aect_test_utils:setup_new_account(Balance, 1, S).

insert_contract(Account, Code, S) ->
    Contract  = make_contract(Account, Code, S),
    Contracts = aect_state_tree:insert_contract(Contract, aect_test_utils:contracts(S)),
    {Contract, aect_test_utils:set_contracts(Contracts, S)}.

insert_call(Sender, Contract, Fun, S) ->
    ContractId = aect_contracts:id(Contract),
    Call       = make_call(Sender, ContractId, Fun, S),
    Contracts  = aect_state_tree:insert_call(Call, aect_test_utils:contracts(S)),
    {Call, aect_test_utils:set_contracts(Contracts, S)}.

get_contract(Contract0, S) ->
    ContractKey = aect_contracts:id(Contract0),
    Contracts   = aect_test_utils:contracts(S),
    Contract    = aect_state_tree:get_contract(ContractKey, Contracts),
    {Contract, S}.

get_call(Contract0, Call0, S) ->
    CallId     = aect_call:id(Call0),
    ContractId = aect_contracts:id(Contract0),
    Contracts  = aect_test_utils:contracts(S),
    Call       = aect_state_tree:get_call(ContractId, CallId, Contracts),
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
