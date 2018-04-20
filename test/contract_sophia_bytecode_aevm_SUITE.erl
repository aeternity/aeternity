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
   ]).

%% chain API exports
-export([ spend/3, get_balance/2, call_contract/6 ]).

-include_lib("common_test/include/ct.hrl").

all() -> [ execute_identity_fun_from_sophia_file,
           sophia_remote_call,
           sophia_factorial,
           simple_multi_argument_call,
           remote_multi_argument_call ].

compile_contract(Name) ->
    CodeDir           = code:lib_dir(aesophia, test),
    FileName          = filename:join([CodeDir, "contracts", lists:concat([Name, ".aes"])]),
    {ok, ContractBin} = file:read_file(FileName),
    {ok, Code}        = aect_sophia:compile(ContractBin, <<"pp_ast pp_icode pp_bytecode">>),
    Code.

%% execute_call(Contract, CallData, ChainState) ->
%%     execute_call(Contract, CallData, ChainState, #{}).

execute_call(Contract, CallData, ChainState, Options) ->
    #{Contract := Code} = ChainState,
    ChainState1 = ChainState#{ running => Contract },
    Res = aect_evm:execute_call(
          maps:merge(
          #{ code              => Code,
             address           => 91210,
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
          true),
    case Res of
        {ok, #{ out := RetVal }} -> {ok, RetVal};
        Err = {error, _, _}      -> Err
    end.

execute_ok(Contract, Call, Env) ->
    execute_ok(Contract, Call, Env, #{}).

execute_ok(Contract, Call, Env, Options) ->
    case execute_call(Contract, Call, Env, Options) of
        {ok, Result} -> aeso_test_utils:dump_words(Result);
        {error, out_of_gas, S} ->
            io:format("S =\n  ~p\n", [S]),
            exit(fail)
    end.

execute_identity_fun_from_sophia_file(_Cfg) ->
    Code     = compile_contract(identity),
    CallData = aect_sophia:create_call(Code, <<"main">>, <<"42">>),
    [42]     = execute_ok(101, CallData, #{101 => Code}),
    ok.

sophia_remote_call(_Cfg) ->
    IdCode     = compile_contract(identity),
    CallerCode = compile_contract(remote_call),
    CallData   = aect_sophia:create_call(CallerCode, <<"call42">>, <<"1234">>),
    Env        = #{ 1234 => IdCode, 1 => CallerCode },
    [42]       = execute_ok(1, CallData, Env),
    ok.

sophia_factorial(_Cfg) ->
    Code      = compile_contract(factorial),
    CallData  = aect_sophia:create_call(Code, <<"main">>, <<"999002">>),
    Env       = #{ 999001 => Code, 999002 => Code },
    [3628800] = execute_ok(999001, CallData, Env),
    ok.

simple_multi_argument_call(_Cfg) ->
    RemoteCode = compile_contract(remote_call),
    PlusCall   = aect_sophia:create_call(RemoteCode, <<"plus">>, <<"(9900,10011)">>),
    Env        = #{ 103 => RemoteCode },
    [19911]    = execute_ok(103, PlusCall, Env),
    ok.

remote_multi_argument_call(_Cfg) ->
    IdCode     = compile_contract(identity),
    RemoteCode = compile_contract(remote_call),
    StagedCall = aect_sophia:create_call(RemoteCode, <<"staged_call">>, <<"(103,101,42)">>),
    Env        = #{ 101 => IdCode, 102 => RemoteCode, 103 => RemoteCode },
    [42]       = execute_ok(102, StagedCall, Env),
    ok.

%% -- Chain API implementation -----------------------------------------------

spend(_, _, S) -> S.

get_balance(_, _) -> 0.

call_contract(Contract, _Gas, _Value, CallData, _, S = #{running := Caller}) ->
    io:format("Calling contract ~p with args ~p\n",
              [Contract, aeso_test_utils:dump_words(CallData)]),
    case maps:is_key(Contract, S) of
        true ->
            Env = #{address => Contract, caller => Caller},
            Res = execute_call(Contract, CallData, S, Env),
            case Res of
                {ok, Ret} ->
                    io:format("  result = ~p\n", [aeso_test_utils:dump_words(Ret)]),
                    {ok, aec_vm_chain_api:call_result(Ret, 0), S};
                {error, out_of_gas, _} ->
                    io:format("  result = out_of_gas\n"),
                    {ok, aec_vm_chain_api:call_exception(out_of_gas, 0), S}
            end;
        false ->
            io:format("  oops, no such contract!\n"),
            {error, {no_such_contract, Contract}}
    end.

