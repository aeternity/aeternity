%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc CT test suite for AEVM/chain interface
%%% @end
%%%-------------------------------------------------------------------
-module(aevm_chain_SUITE).

%% common_test exports
-export([ all/0
        , groups/0
        ]).

%% test case exports
-export([ contracts/1
        , spend/1
        ]).

-include_lib("common_test/include/ct.hrl").

-include_lib("apps/aecore/include/blocks.hrl").

-define(MINER_PUBKEY, <<42:?MINER_PUB_BYTES/unit:8>>).

%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, all_tests}
    ].

groups() ->
    [ {all_tests, [sequence],
        [ {group, spend}
        , {group, contracts}
        ]}
    , {spend, [sequence],
        [ spend ]}
    , {contracts, [sequence],
        [ contracts ]}
    ].

%%%===================================================================
%%% Setup
%%%===================================================================

%% Uses aect_test_utils to set up the chain, but after the setup everything is
%% done through the aevm_chain_api.
setup_chain() ->
    S0              = aect_test_utils:new_state(),
    {Account1, S1}  = aect_test_utils:setup_new_account(S0),
    {Account2, S2}  = aect_test_utils:setup_new_account(S1),
    {Contract1, S3} = create_contract(Account1, S2),
    {Contract2, S4} = create_contract(Account2, S3),
    Trees = aect_test_utils:trees(S4),
    InitS = aec_vm_chain:new_state(Trees, 1, Contract1),
    {[Account1, Account2, Contract1, Contract2], InitS}.

create_contract(Owner, S) ->
    OwnerPrivKey = aect_test_utils:priv_key(Owner, S),
    IdContract   = aect_test_utils:compile_contract("contracts/identity.aes"),
    CallData     = aeso_abi:create_calldata(IdContract, "init", "42"),

    Overrides    = #{ code => IdContract
		    , call_data => CallData
		    , gas => 10000
		    , amount => 2000},
    CreateTx     = aect_test_utils:create_tx(Owner, Overrides, S),
    {SignedTx, [SignedTx], S1} = sign_and_apply_transaction(CreateTx, OwnerPrivKey, S),
    {aect_contracts:compute_contract_pubkey(Owner, aetx:nonce(CreateTx)), S1}.

sign_and_apply_transaction(Tx, PrivKey, S1) ->
    SignedTx = aetx_sign:sign(Tx, PrivKey),
    Trees    = aect_test_utils:trees(S1),
    Height   = 1,
    {ok, AcceptedTxs, Trees1} =
        aec_block_candidate:apply_block_txs([SignedTx], ?MINER_PUBKEY, Trees, Height, ?PROTOCOL_VERSION),
    S2       = aect_test_utils:set_trees(Trees1, S1),
    {SignedTx, AcceptedTxs, S2}.

call_data(Arg) ->
    Code = aect_test_utils:compile_contract("contracts/identity.aes"),
    aect_sophia:create_call(Code, <<"main">>, Arg).

%%%===================================================================
%%% Spend tests
%%%===================================================================

spend(_Cfg) ->
    {[Acc, _Acc2, Contract1, _Contract2], S} = setup_chain(),
    AccBal1  = aec_vm_chain:get_balance(Acc, S),
    Bal1     = aec_vm_chain:get_balance(Contract1, S),
    Amount   = 50,
    {ok, S1} = aec_vm_chain:spend(Acc, Amount, S),
    Bal2     = aec_vm_chain:get_balance(Contract1, S1),
    Bal2     = Bal1 - Amount,
    AccBal2  = aec_vm_chain:get_balance(Acc, S1),
    AccBal2  = AccBal1 + Amount,
    {error, insufficient_funds}
             = aec_vm_chain:spend(Acc, 1000000, S1),
    ok.

%%%===================================================================
%%% Contract tests
%%%===================================================================

contracts(_Cfg) ->
    {[_Acc, _Acc2, Contract1, Contract2], S} = setup_chain(),
    _S1 = lists:foldl(fun({Value, Arg}, S0) -> make_call(Contract1, Contract2, Value, Arg, S0) end,
                      S, [{(I - 3) * 100, I + 100} || I <- lists:seq(1, 10)]),
    ok.

make_call(From, To, Value, Arg, S) ->
    C1Bal1    = aec_vm_chain:get_balance(From, S),
    C2Bal1    = aec_vm_chain:get_balance(To, S),
    CallData  = call_data(integer_to_binary(Arg)),
    Gas       = 10000,
    CallStack = [],
    CallRes   = aec_vm_chain:call_contract(To, Gas, Value, CallData, CallStack, S),
    case C1Bal1 >= Value of
        _ when Value < 0 ->
            {error, negative_amount} = CallRes,
            S;
        true ->
            {ok, Res, S1} = CallRes,
            GasUsed  = aevm_chain_api:gas_spent(Res),
            {ok, <<Arg:256>>} = aevm_chain_api:return_value(Res),
            true     = GasUsed > 0,
            true     = GasUsed =< Gas,
            C1Bal2   = aec_vm_chain:get_balance(From, S1),
            C1Bal2   = C1Bal1 - Value,
            C2Bal2   = aec_vm_chain:get_balance(To, S1),
            C2Bal2   = C2Bal1 + Value,
            S1;
        false ->
            {error, insufficient_funds} = CallRes,
            S
    end.


