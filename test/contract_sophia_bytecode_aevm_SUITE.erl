-module(contract_sophia_bytecode_aevm_SUITE).

%% common_test exports
-export(
   [
    all/0
   ]).

%% test case exports
-export(
   [
     execute_identity_fun_from_sophia_file/1
   ]).

-include_lib("common_test/include/ct.hrl").

all() -> [ execute_identity_fun_from_sophia_file ].

execute_identity_fun_from_sophia_file(_Cfg) ->
    CodeDir = code:lib_dir(aesophia, test),
    FileName = filename:join(CodeDir, "contracts/identity.aes"),
    {ok, ContractBin} = file:read_file(FileName),
    {ok, Code} = aect_sophia:compile(ContractBin, <<>>),
    CallData = aect_sophia:create_call(Code, <<"main">>, <<"42">>),
    {ok, #{ out := RetVal}} =
        aect_evm:execute_call(
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
             currentGasLimit => 10000,
             currentNumber => 0,
             currentTimestamp => 0,
             chainState => aevm_dummy_chain:new_state(),
             chainAPI => aevm_dummy_chain},
          true),
    <<42:256>> = RetVal,
    ok.
