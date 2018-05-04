-module(contract_sophia_bytecode_aevm_SUITE).
-behaviour(aevm_chain_api).

%% common_test exports
-export([all/0]).

%% test case exports
-export(
   [ execute_identity_fun_from_sophia_file/1
   , sophia_remote_call/1
   , sophia_factorial/1
   , simple_multi_argument_call/1
   , remote_multi_argument_call/1
   , spend_tests/1
   , complex_types/1
   , environment/1
   , counter/1
   ]).

%% chain API exports
-export([ spend/3, get_balance/2, call_contract/6, get_store/1, set_store/2 ]).

-include("apps/aecontract/src/aecontract.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [ execute_identity_fun_from_sophia_file,
           sophia_remote_call,
           sophia_factorial,
           simple_multi_argument_call,
           remote_multi_argument_call,
           spend_tests,
           complex_types,
           environment,
           counter ].

compile_contract(Name) ->
    CodeDir           = code:lib_dir(aesophia, test),
    FileName          = filename:join([CodeDir, "contracts", lists:concat([Name, ".aes"])]),
    {ok, ContractBin} = file:read_file(FileName),
    Options           = [],
    %% Options           = [pp_ast, pp_icode, pp_bytecode],
    Code = aeso_compiler:from_string(binary_to_list(ContractBin), Options),
    aeu_hex:hexstring_encode(Code).

%% execute_call(Contract, CallData, ChainState) ->
%%     execute_call(Contract, CallData, ChainState, #{}).

execute_call(Contract, CallData, ChainState, Options) ->
    #{Contract := Code} = ChainState,
    ChainState1 = ChainState#{ running => Contract },
    Trace = false,
    Res = aect_evm:execute_call(
          maps:merge(
          #{ code              => Code,
             address           => Contract,
             caller            => 0,
             data              => CallData,
             gas               => 1000000,
             gasPrice          => 1,
             origin            => 0,
             value             => 0,
             currentCoinbase   => 0,
             currentDifficulty => 0,
             currentGasLimit   => 1000000,
             currentNumber     => 0,
             currentTimestamp  => 0,
             chainState        => ChainState1,
             chainAPI          => ?MODULE,
             vm_version        => ?AEVM_01_Sophia_01}, Options),
          Trace),
    case Res of
        {ok, #{ out := RetVal, chain_state := S }} ->
            {ok, RetVal, S};
        Err = {error, _, _}      -> Err
    end.

make_call(Contract, Fun, Args, Env, Options) ->
    #{ Contract := Code } = Env,
    CallData = aect_sophia:create_call(Code,
                    list_to_binary(atom_to_list(Fun)),
                    list_to_binary(Args)),
    execute_call(Contract, CallData, Env, Options).

create_contract(Address, Code, Args, Env) ->
    Env1 = Env#{Address => Code},
    {ok, InitS, Env2} = make_call(Address, init, Args, Env1, #{}),
    set_store(aevm_eeevm_store:from_sophia_state(InitS), Env2).

successful_call_(Contract, Type, Fun, Args, Env) ->
    {Res, _Env1} = successful_call(Contract, Type, Fun, Args, Env),
    Res.

successful_call_(Contract, Type, Fun, Args, Env, Options) ->
    {Res, _Env1} = successful_call(Contract, Type, Fun, Args, Env, Options),
    Res.

successful_call(Contract, Type, Fun, Args, Env) ->
    successful_call(Contract, Type, Fun, Args, Env, #{}).

successful_call(Contract, Type, Fun, Args, Env, Options) ->
    case make_call(Contract, Fun, Args, Env, Options) of
        {ok, Result, Env1} -> {aeso_data:from_binary(Type, Result), Env1};
        {error, Err, S} ->
            io:format("S =\n  ~p\n", [S]),
            exit({error, Err})
    end.

%% failing_call(Contract, Fun, Args, Env) ->
%%     failing_call(Contract, Fun, Args, Env, #{}).

%% failing_call(Contract, Fun, Args, Env, Options) ->
%%     case make_call(Contract, Fun, Args, Env, Options) of
%%         {ok, Result, _} ->
%%             Words = aeso_test_utils:dump_words(Result),
%%             exit({expected_failure, {ok, Words}});
%%         {error, Err, _} ->
%%             Err
%%     end.

execute_identity_fun_from_sophia_file(_Cfg) ->
    Code = compile_contract(identity),
    Env  = initial_state(#{101 => Code}),
    42   = successful_call_(101, word, main, "42", Env),
    ok.

sophia_remote_call(_Cfg) ->
    IdCode     = compile_contract(identity),
    CallerCode = compile_contract(remote_call),
    Env        = initial_state(#{ 1234 => IdCode, 1 => CallerCode }),
    42         = successful_call_(1, word, call42, "1234", Env),
    ok.

sophia_factorial(_Cfg) ->
    Code    = compile_contract(factorial),
    Env     = initial_state(#{ 999001 => Code, 999002 => Code }),
    3628800 = successful_call_(999001, word, main, "999002", Env),
    ok.

simple_multi_argument_call(_Cfg) ->
    RemoteCode = compile_contract(remote_call),
    Env        = initial_state(#{ 103 => RemoteCode }),
    19911      = successful_call_(103, word, plus, "(9900,10011)", Env),
    ok.

remote_multi_argument_call(_Cfg) ->
    IdCode     = compile_contract(identity),
    RemoteCode = compile_contract(remote_call),
    Env        = initial_state(#{ 101 => IdCode, 102 => RemoteCode, 103 => RemoteCode }),
    42         = successful_call_(102, word, staged_call, "(102,101,42)", Env),
    ok.

spend_tests(_Cfg) ->
    Code = compile_contract(spend_test),
    Env  = initial_state(#{ 101 => Code, 102 => Code },
                         #{ 101 => 1000, 102 => 2000, 1 => 10000 }),
    {900, Env1}  = successful_call(101, word, withdraw, "100", Env, #{caller => 102}),
    {900, Env2}  = successful_call(101, word, get_balance, "()", Env1),
    {2100, Env3} = successful_call(102, word, get_balance, "()", Env2),
    %% The call doesn't fail, but the spend transaction is not executed.
    {-100, Env4} = successful_call(101, signed_word, withdraw, "1000", Env3, #{caller => 102}),
    900          = successful_call_(101, word, get_balance, "()", Env4),
    2100         = successful_call_(102, word, get_balance, "()", Env4),
    %% Spending in nested call
    {900, Env5}  = successful_call(101, word, withdraw_from, "(102,1000)", Env4, #{caller => 1}),
    #{1 := 11000, 101 := 900, 102 := 1100} = maps:get(accounts, Env5),
    ok.

complex_types(_Cfg) ->
    Code = compile_contract(complex_types),
    Env  = initial_state(#{101 => Code}),
    21                      = successful_call_(101, word, sum, "[1,2,3,4,5,6]", Env),
    [1, 2, 3, 4, 5, 6]      = successful_call_(101, {list, word}, up_to, "6", Env),
    [1, 2, 3, 4, 5, 6]      = successful_call_(101, {list, word}, remote_list, "6", Env),
    {<<"answer:">>, 21}     = successful_call_(101, {tuple, [string, word]}, remote_triangle, "(101,6)", Env),
    <<"string">>            = successful_call_(101, string, remote_string, "()", Env),
    {99, <<"luftballons">>} = successful_call_(101, {tuple, [word, string]}, remote_pair, "(99,\"luftballons\")", Env),

    N       = 10,
    Squares = [ {I, I * I} || I <- lists:seq(1, N) ],
    Squares = successful_call_(101, {list, {tuple, [word, word]}}, squares, integer_to_list(N), Env),
    Squares = successful_call_(101, {list, {tuple, [word, word]}}, remote_squares, integer_to_list(N), Env),
    ok.

environment(_Cfg) ->
    Code          = compile_contract(environment),
    Address       = 1001,
    Address2      = 1002,
    Balance       = 9999,
    CallerBalance = 500,
    Caller        = 1,
    Origin        = 11,
    Value         = 100,
    GasPrice      = 3,
    Coinbase      = 22022,
    Timestamp     = 1234,
    BlockHeight   = 701,
    Difficulty    = 18,
    GasLimit      = 1000001,
    ChainEnv      =
              #{ origin            => Origin
               , gasPrice          => GasPrice
               , currentCoinbase   => Coinbase
               , currentTimestamp  => Timestamp
               , currentNumber     => BlockHeight
               , currentDifficulty => Difficulty
               , currentGasLimit   => GasLimit
               },
    State = initial_state(#{Address => Code, Address2 => Code, environment => ChainEnv},
                          #{Address => Balance, Caller => CallerBalance}),
    Options = maps:merge(#{ caller => Caller, value  => Value }, ChainEnv),
    Call1   = fun(Fun, Arg) -> successful_call_(Address, word, Fun, integer_to_list(Arg), State, Options) end,
    Call    = fun(Fun)      -> successful_call_(Address, word, Fun, "()", State, Options) end,

    Address  = Call(contract_address),
    Address2 = Call1(nested_address, Address2),
    Balance  = Call(contract_balance),

    Origin   = Call(call_origin),
    Origin   = Call(nested_origin),
    Caller   = Call(call_caller),
    Address  = Call(nested_caller),
    Value    = Call(call_value),
    99       = Call1(nested_value, 99),
    GasPrice = Call(call_gas_price),

    CallerBalance = Call1(get_balance, Caller),
    0             = Call1(block_hash, BlockHeight), %% TODO: BLOCKHASH not implemented in EVM
    Coinbase      = Call(coinbase),
    Timestamp     = Call(timestamp),
    BlockHeight   = Call(block_height),
    Difficulty    = Call(difficulty),
    GasLimit      = Call(gas_limit),

    ok.

%% State tests

counter(_Cfg) ->
    Code       = compile_contract(counter),
    Env        = initial_state(#{}),
    Env1       = create_contract(101, Code, "5", Env),
    {5,  Env2} = successful_call(101, word, get, "()", Env1),
    {{}, Env3} = successful_call(101, {tuple, []}, tick, "()", Env2),
    {6, _Env4} = successful_call(101, word, get, "()", Env3),
    ok.

%% -- Chain API implementation -----------------------------------------------

initial_state(Contracts) ->
    initial_state(Contracts, #{}).

initial_state(Contracts, Accounts) ->
    maps:merge(#{environment => #{}, store => #{}},
        maps:merge(Contracts, #{accounts => Accounts})).

spend(_To, Amount, _) when Amount < 0 ->
    {error, negative_spend};
spend(To, Amount, S = #{ running := From, accounts := Accounts }) ->
    Balance = get_balance(From, S),
    case Amount =< Balance of
        true -> {ok, S#{ accounts := Accounts#{ From => Balance            - Amount,
                                                To   => get_balance(To, S) + Amount } }};
        false -> {error, insufficient_funds}
    end.

get_balance(Account, #{ accounts := Accounts }) ->
    maps:get(Account, Accounts, 0).

get_store(#{ running := Contract, store := Store }) ->
    Data = maps:get(Contract, Store, undefined),
    case Data of
        undefined -> aevm_eeevm_store:from_sophia_state(<<>>);
        _         -> Data
    end.

set_store(Data, State = #{ running := Contract, store := Store }) ->
    State#{ store => Store#{ Contract => Data } }.

-define(PRIM_CALL_SPEND, 1).

call_contract(0, _Gas, Value, CallData, _, S) ->
    io:format("primitive call with data ~p\n", [aeso_test_utils:dump_words(CallData)]),
    %% TODO: aeso_data:from_binary/2 should take an offset
    case aeso_test_utils:dump_words(CallData) of
        [64, ?PRIM_CALL_SPEND, To] ->
            case spend(To, Value, S) of
                {ok, S1} ->
                    io:format("Spent ~p from ~p to ~p\n", [Value, maps:get(running, S), To]),
                    {ok, aevm_chain_api:call_result(<<>>, 0), S1};
                Err ->
                    io:format("Bad spend ~p from ~p to ~p\n", [Value, maps:get(running, S), To]),
                    Err
            end;
        _ -> {error, {bad_prim_call, aeso_test_utils:dump_words(CallData)}}
    end;
call_contract(Contract, _Gas, Value, CallData, _, S = #{running := Caller}) ->
    io:format("Calling contract ~p with args ~p\n",
              [Contract, aeso_test_utils:dump_words(CallData)]),
    case maps:is_key(Contract, S) of
        true ->
            #{environment := Env0} = S,
            Env = maps:merge(Env0, #{address => Contract, caller => Caller, value => Value}),
            Res = execute_call(Contract, CallData, S, Env),
            case Res of
                {ok, Ret, #{ accounts := Accounts }} ->
                    io:format("  result = ~p\n", [aeso_test_utils:dump_words(Ret)]),
                    {ok, aevm_chain_api:call_result(Ret, 0), S#{ accounts := Accounts }};
                {error, out_of_gas, _} ->
                    io:format("  result = out_of_gas\n"),
                    {ok, aevm_chain_api:call_exception(out_of_gas, 0), S}
            end;
        false ->
            io:format("  oops, no such contract!\n"),
            {error, {no_such_contract, Contract}}
    end.

