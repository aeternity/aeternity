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
        , sophia_identity/1
        , sophia_state/1
        ]).

-include_lib("common_test/include/ct.hrl").

-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aecore/include/blocks.hrl").
-include_lib("apps/aecontract/include/contract_txs.hrl").
-include_lib("apps/aecontract/src/aecontract.hrl").

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
                              ]}
    , {transactions, [sequence], [ create_contract
                                 , create_contract_negative
                                 , call_contract
                                 , call_contract_negative
                                 ]}
    , {state_tree, [sequence], [ state_tree ]}
    , {sophia,     [sequence], [ sophia_identity,
                                 sophia_state ]}
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
    {error, S1}  = sign_and_apply_transaction(RTx1, BadPrivKey, S1),
    {error, account_not_found} = aetx:check(RTx1, Trees, CurrHeight, ?PROTOCOL_VERSION),

    %% Insufficient funds
    S2     = aect_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = aect_test_utils:trees(S2),
    RTx2   = aect_test_utils:create_tx(PubKey, S2),
    {error, S2}  = sign_and_apply_transaction(RTx2, PrivKey, S2),
    {error, insufficient_funds} = aetx:check(RTx2, Trees2, CurrHeight, ?PROTOCOL_VERSION),

    %% Test too high account nonce
    RTx3 = aect_test_utils:create_tx(PubKey, #{nonce => 0}, S1),
    {error, S1}  = sign_and_apply_transaction(RTx3, PrivKey, S1),
    {error, account_nonce_too_high} = aetx:check(RTx3, Trees, CurrHeight, ?PROTOCOL_VERSION),

    ok.

create_contract(_Cfg) ->
    {PubKey, S1} = aect_test_utils:setup_new_account(aect_test_utils:new_state()),
    Tx           = aect_test_utils:create_tx(PubKey, #{}, S1),
    PrivKey      = aect_test_utils:priv_key(PubKey, S1),

    %% Test that the create transaction is accepted
    {ok, S2} = sign_and_apply_transaction(Tx, PrivKey, S1),
    {PubKey, S2}.

sign_and_apply_transaction(Tx, PrivKey, S1) ->
    SignedTx = aetx_sign:sign(Tx, PrivKey),
    Trees    = aect_test_utils:trees(S1),
    Height   = 1,
    {ok, AcceptedTxs, Trees1} = aec_trees:apply_signed_txs([SignedTx], Trees, Height, ?PROTOCOL_VERSION),
    S2       = aect_test_utils:set_trees(Trees1, S1),
    case AcceptedTxs of
        [SignedTx] -> {ok, S2};
        []         -> {error, S2}
    end.

sign_and_apply_transaction_strict(Tx, PrivKey, S1) ->
    SignedTx = aetx_sign:sign(Tx, PrivKey),
    Trees    = aect_test_utils:trees(S1),
    Height   = 1,
    ConsensusVersion = aec_hard_forks:protocol_effective_at_height(Height),
    {ok, AcceptedTxs, Trees1} = aec_trees:apply_signed_txs_strict([SignedTx], Trees, Height, ConsensusVersion),
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

    IdContract   = aect_test_utils:compile_contract("contracts/identity.aes"),
    CallData     = aeso_abi:create_calldata(IdContract, "main", "42"),
    Overrides    = #{ code => IdContract
		    , call_data => CallData
		    , gas => 100000
		    },
    CreateTx     = aect_test_utils:create_tx(Owner, Overrides, S2),

    %% Test that the create transaction is accepted
    {SignedTx, [SignedTx], S3} = sign_and_apply_transaction_strict(CreateTx, OwnerPrivKey, S2),
    ContractKey = aect_contracts:compute_contract_pubkey(Owner, aetx:nonce(CreateTx)),

    %% Now check that we can call it.
    Fee           = 107,
    GasPrice      = 2,
    Value         = 52,
    Arg           = <<"42">>,
    CallData = aect_sophia:create_call(IdContract, <<"main">>, Arg),
    CallTx = aect_test_utils:call_tx(Caller, ContractKey,
                                     #{call_data => CallData,
                                       gas_price => GasPrice,
                                       amount    => Value,
                                       fee       => Fee}, S3),
    {ok, S4} = sign_and_apply_transaction(CallTx, CallerPrivKey, S3),
    CallId = aect_call:id(Caller, aetx:nonce(CallTx), ContractKey),

    %% Check that it got stored and that we got the right return value
    Call = aect_call_state_tree:get_call(ContractKey, CallId, aect_test_utils:calls(S4)),
    <<42:256>> = aect_call:return_value(Call),

    %% ...and that we got charged the right amount for gas and fee.
    {NewCallerBalance, NewCallerBalance} =
        {aec_accounts:balance(aect_test_utils:get_account(Caller, S4)),
         CallerBalance - Fee - GasPrice * aect_call:gas_used(Call) - Value},

    {ok, S4}.

%%%===================================================================
%%% State trees
%%%===================================================================

make_contract(PubKey = <<_:32, Rest/binary>>, Code, S) ->
    Tx = aect_test_utils:create_tx(PubKey, #{ code => Code }, S),
    ContractKey = <<"CODE", Rest/binary>>,
    {contract_create_tx, CTx} = aetx:specialize_type(Tx),
    aect_contracts:new(ContractKey, CTx, 1).

make_call(PubKey, ContractKey,_Call,_S) ->
    aect_call:new(PubKey, 0, ContractKey, 1).

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

-define(call(Fun, X),                call(Fun, fun Fun/2, [X])).
-define(call(Fun, X, Y),             call(Fun, fun Fun/3, [X, Y])).
-define(call(Fun, X, Y, Z),          call(Fun, fun Fun/4, [X, Y, Z])).
-define(call(Fun, X, Y, Z, U),       call(Fun, fun Fun/5, [X, Y, Z, U])).
-define(call(Fun, X, Y, Z, U, V),    call(Fun, fun Fun/6, [X, Y, Z, U, V])).
-define(call(Fun, X, Y, Z, U, V, W), call(Fun, fun Fun/7, [X, Y, Z, U, V, W])).

new_account(Balance, S) ->
    aect_test_utils:setup_new_account(Balance, 1, S).

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
                     , gas        => 10000 }, Options), S),
    PrivKey     = aect_test_utils:priv_key(Owner, S),
    {ok, S1} = sign_and_apply_transaction(CreateTx, PrivKey, S),
    ContractKey = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    {ContractKey, S1}.

call_contract(Caller, ContractKey, Fun, Type, Args, S) ->
    call_contract(Caller, ContractKey, Fun, Type, Args, #{}, S).

call_contract(Caller, ContractKey, Fun, Type, Args, Options, S) ->
    Nonce    = aect_test_utils:next_nonce(Caller, S),
    CallData = aect_sophia:create_call(<<"unused">>, list_to_binary(atom_to_list(Fun)), args_to_binary(Args)),
    CallTx   = aect_test_utils:call_tx(Caller, ContractKey,
                maps:merge(
                #{ nonce      => Nonce
                 , vm_version => ?AEVM_01_Sophia_01
                 , call_data  => CallData
                 , fee        => 1
                 , amount     => 0
                 , gas        => 30000
                 }, Options), S),
    PrivKey  = aect_test_utils:priv_key(Caller, S),
    {ok, S1} = sign_and_apply_transaction(CallTx, PrivKey, S),
    CallKey  = aect_call:id(Caller, Nonce, ContractKey),
    CallTree = aect_test_utils:calls(S1),
    Call     = aect_call_state_tree:get_call(ContractKey, CallKey, CallTree),
    Result   =
        case aect_call:return_type(Call) of
            ok     -> aeso_data:from_binary(Type, aect_call:return_value(Call));
            error  -> error;
            revert -> revert
        end,
    {Result, S1}.

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
    io_lib:format("(~s)", [commas(lists:map(fun args_to_list/1, tuple_to_list(T)))]).

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
    error        = ?call(call_contract, Acc1, Stack, pop, string, {}),
    ok.

