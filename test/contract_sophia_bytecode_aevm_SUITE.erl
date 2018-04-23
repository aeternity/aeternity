-module(contract_sophia_bytecode_aevm_SUITE).

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
   ]).

%% chain API exports
-export([ spend/3, get_balance/2, call_contract/6 ]).

-include_lib("common_test/include/ct.hrl").

all() -> [ execute_identity_fun_from_sophia_file,
           sophia_remote_call,
           sophia_factorial,
           simple_multi_argument_call,
           remote_multi_argument_call,
           spend_tests ].

compile_contract(Name) ->
    CodeDir           = code:lib_dir(aesophia, test),
    FileName          = filename:join([CodeDir, "contracts", lists:concat([Name, ".aes"])]),
    {ok, ContractBin} = file:read_file(FileName),
    Options           = <<>>,
    %% Options           = <<"pp_ast pp_icode pp_bytecode">>,
    {ok, Code}        = aect_sophia:compile(ContractBin, Options),
    Code.

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
             chainAPI          => ?MODULE}, Options),
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

successful_call_(Contract, Fun, Args, Env) ->
    {Res, _Env1} = successful_call(Contract, Fun, Args, Env),
    Res.

successful_call(Contract, Fun, Args, Env) ->
    successful_call(Contract, Fun, Args, Env, #{}).

successful_call(Contract, Fun, Args, Env, Options) ->
    case make_call(Contract, Fun, Args, Env, Options) of
        {ok, Result, Env1} -> {aeso_test_utils:dump_words(Result), Env1};
        {error, Err, S} ->
            io:format("S =\n  ~p\n", [S]),
            exit({error, Err})
    end.

failing_call(Contract, Fun, Args, Env) ->
    failing_call(Contract, Fun, Args, Env, #{}).

failing_call(Contract, Fun, Args, Env, Options) ->
    case make_call(Contract, Fun, Args, Env, Options) of
        {ok, Result, _} ->
            Words = aeso_test_utils:dump_words(Result),
            exit({expected_failure, {ok, Words}});
        {error, Err, _} ->
            Err
    end.

execute_identity_fun_from_sophia_file(_Cfg) ->
    Code = compile_contract(identity),
    Env  = initial_state(#{101 => Code}),
    [42] = successful_call_(101, main, "42", Env),
    ok.

sophia_remote_call(_Cfg) ->
    IdCode     = compile_contract(identity),
    CallerCode = compile_contract(remote_call),
    Env        = initial_state(#{ 1234 => IdCode, 1 => CallerCode }),
    [42]       = successful_call_(1, call42, "1234", Env),
    ok.

sophia_factorial(_Cfg) ->
    Code      = compile_contract(factorial),
    Env       = initial_state(#{ 999001 => Code, 999002 => Code }),
    [3628800] = successful_call_(999001, main, "999002", Env),
    ok.

simple_multi_argument_call(_Cfg) ->
    RemoteCode = compile_contract(remote_call),
    Env        = initial_state(#{ 103 => RemoteCode }),
    [19911]    = successful_call_(103, plus, "(9900,10011)", Env),
    ok.

remote_multi_argument_call(_Cfg) ->
    IdCode     = compile_contract(identity),
    RemoteCode = compile_contract(remote_call),
    Env        = initial_state(#{ 101 => IdCode, 102 => RemoteCode, 103 => RemoteCode }),
    [42]       = successful_call_(102, staged_call, "(102,101,42)", Env),
    ok.

spend_tests(_Cfg) ->
    Code = compile_contract(spend_test),
    Env  = initial_state(#{ 101 => Code, 102 => Code },
                         #{ 101 => 1000, 102 => 2000, 1 => 10000 }),
    {[900], Env1}  = successful_call(101, withdraw, "100", Env, #{caller => 102}),
    {[900], Env2}  = successful_call(101, get_balance, "()", Env1),
    {[2100], Env3} = successful_call(102, get_balance, "()", Env2),
    %% The call doesn't fail, but the spend transaction is not executed.
    {[-100], Env4} = successful_call(101, withdraw, "1000", Env3, #{caller => 102}),
    [900]          = successful_call_(101, get_balance, "()", Env4),
    [2100]         = successful_call_(102, get_balance, "()", Env4),
    %% Spending in nested call
    {[900], Env5}  = successful_call(101, withdraw_from, "(102,1000)", Env4, #{caller => 1}),
    #{1 := 11000, 101 := 900, 102 := 1100} = maps:get(accounts, Env5),
    ok.

%% -- Chain API implementation -----------------------------------------------

initial_state(Contracts) ->
    initial_state(Contracts, #{}).

initial_state(Contracts, Accounts) ->
    maps:merge(Contracts, #{accounts => Accounts}).

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

-define(PRIM_CALL_SPEND, 1).

call_contract(0, _Gas, Value, CallData, _, S) ->
    case aeso_test_utils:dump_words(CallData) of
        [?PRIM_CALL_SPEND, To] ->
            case spend(To, Value, S) of
                {ok, S1} ->
                    io:format("Spent ~p from ~p to ~p\n", [Value, maps:get(running, S), To]),
                    {ok, aec_vm_chain_api:call_result(<<>>, 0), S1};
                Err      -> Err
            end;
        Args -> {error, {bad_prim_call, Args}}
    end;
call_contract(Contract, _Gas, _Value, CallData, _, S = #{running := Caller}) ->
    io:format("Calling contract ~p with args ~p\n",
              [Contract, aeso_test_utils:dump_words(CallData)]),
    case maps:is_key(Contract, S) of
        true ->
            Env = #{address => Contract, caller => Caller},
            Res = execute_call(Contract, CallData, S, Env),
            case Res of
                {ok, Ret, #{ accounts := Accounts }} ->
                    io:format("  result = ~p\n", [aeso_test_utils:dump_words(Ret)]),
                    {ok, aec_vm_chain_api:call_result(Ret, 0), S#{ accounts := Accounts }};
                {error, out_of_gas, _} ->
                    io:format("  result = out_of_gas\n"),
                    {ok, aec_vm_chain_api:call_exception(out_of_gas, 0), S}
            end;
        false ->
            io:format("  oops, no such contract!\n"),
            {error, {no_such_contract, Contract}}
    end.

