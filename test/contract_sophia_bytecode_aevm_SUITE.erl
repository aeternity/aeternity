-module(contract_sophia_bytecode_aevm_SUITE).

%% common_test exports
-export(
   [
    all/0
   ]).

%% test case exports
-export(
   [ execute_identity_fun_from_sophia_file/1
   , sophia_remote_call/1
   ]).

%% chain API exports
-export([ spend/3, get_balance/2, call_contract/6 ]).

-include_lib("common_test/include/ct.hrl").

all() -> [ execute_identity_fun_from_sophia_file,
           sophia_remote_call ].

compile_contract(Name) ->
    CodeDir           = code:lib_dir(aesophia, test),
    FileName          = filename:join([CodeDir, "contracts", lists:concat([Name, ".aes"])]),
    {ok, ContractBin} = file:read_file(FileName),
    {ok, Code}        = aect_sophia:compile(ContractBin, <<"pp_bytecode">>),
    Code.

execute_call(Code, CallData, Environment) ->
    Res = aect_evm:execute_call(
          #{ code => Code,
             address => 91210,
             caller => 0,
             data => CallData,
             gas => 1000000,
             gasPrice => 1,
             origin => 0,
             value => 0,
             currentCoinbase => 0,
             currentDifficulty => 0,
             currentGasLimit => 1000000,
             currentNumber => 0,
             currentTimestamp => 0,
             chainState => Environment,
             chainAPI => ?MODULE},
          true),
    case Res of
        {ok, #{ out := RetVal }} -> {ok, RetVal};
        Err = {error, _, _}      -> Err
    end.

execute_identity_fun_from_sophia_file(_Cfg) ->
    Code     = compile_contract(identity),
    CallData = aect_sophia:create_call(Code, <<"main">>, <<"42">>),
    {ok, <<42:256>>} = execute_call(Code, CallData, #{}),
    ok.

sophia_remote_call(_Cfg) ->
    IdCode     = compile_contract(identity),
    CallerCode = compile_contract(remote_call),
    CallData   = aect_sophia:create_call(CallerCode, <<"call42">>, <<"1234">>),
    Env        = #{ 1234 => IdCode },
    case execute_call(CallerCode, CallData, Env) of
        {ok, Result} ->
            [42] = aeso_test_utils:dump_words(Result),
            ok;
        {error, out_of_gas, S} ->
            io:format("S =\n  ~p\n", [S]),
            exit(fail)
    end.

%% -- Chain API implementation -----------------------------------------------

spend(_, _, S) -> S.

get_balance(_, _) -> 0.

call_contract(Contract, _Gas, _Value, CallData, _, S) ->
    io:format("Calling contract ~p with args ~p\n", [Contract, CallData]),
    case S of
        #{ Contract := Code } ->
            Res = execute_call(Code, CallData, S),
            io:format("  result = ~p\n", [Res]),
            case Res of
                {ok, Ret} ->
                    {ok, aec_vm_chain_api:call_result(Ret, 0), S};
                {error, out_of_gas, _} ->
                    {ok, aec_vm_chain_api:call_exception(out_of_gas, 0), S}
            end;
        _ ->
            io:format("  oops, no such contract!\n"),
            {error, {no_such_contract, Contract}}
    end.

