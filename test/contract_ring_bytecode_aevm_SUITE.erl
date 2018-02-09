-module(contract_ring_bytecode_aevm_SUITE).

%% common_test exports
-export(
   [
    all/0
   ]).

%% test case exports
-export(
   [ 
     execute_identity_fun_from_ring_file/1
   ]).

-include_lib("common_test/include/ct.hrl").

all() -> [ execute_identity_fun_from_ring_file ].

execute_identity_fun_from_ring_file(_Cfg) ->
    CodeDir = code:lib_dir(aering, test),
    FileName = filename:join(CodeDir, "contracts/identity.aer"),
    {ok, ContractBin} = file:read_file(FileName),
    {ok, Code} = aect_ring:compile(ContractBin, <<>>),
    CallData = aect_ring:create_call(Code, <<"main">>, <<"42">>),
    #{ out := RetVal} =
        aect_evm:execute_call(
          #{ code => Code,
             address => 0,
             caller => 0,
             data => CallData,
             gas => 1000000,
             gasPrice => 1,
             origin => 0,
             value => 0,
             currentCoinbase => 0,
             currentDifficulty => 0,
             currentGasLimit => 10000,
             currentNumber => 0,
             currentTimestamp => 0},
          true),
    <<42:256>> = RetVal,
    ok.
