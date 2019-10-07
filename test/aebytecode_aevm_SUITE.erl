-module(aebytecode_aevm_SUITE).

%% common_test exports
-export(
   [
    all/0
   ]).

%% test case exports
-export(
   [ execute_identy_fun_from_file/1
   ]).

-include_lib("aecontract/include/aecontract.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [ execute_identy_fun_from_file ].

execute_identy_fun_from_file(_Cfg) ->
    CodeDir = code:lib_dir(aebytecode, test),
    FileName = filename:join(CodeDir, "asm_code/identity.aesm"),
    Code = aeb_asm:file(FileName, []),
    ChainState = aevm_dummy_chain:new_state(),
    {ok, Res} =
        aevm_eeevm:eval(
          aevm_eeevm_state:init(
            #{ exec => #{ code => Code,
                          store => aect_contracts_store:new(),
                          address => 90120,
                          caller => 0,
                          data => <<0:256, 42:256>>,
                          gas => 1000000,
                          gasPrice => 1,
                          origin => 0,
                          creator => 0,
                          value => 0 },
               env => #{currentCoinbase => 0,
                        currentDifficulty => 0,
                        currentGasLimit => 10000,
                        currentNumber => 0,
                        currentTimestamp => 0,
                        authTxHash => undefined,
                        chainState => ChainState,
                        chainAPI => aevm_dummy_chain,
                        protocol_version => aect_test_utils:latest_protocol_version(),
                        vm_version => ?VM_AEVM_SOLIDITY_1,
                        abi_version => ?ABI_SOLIDITY_1},
               pre => #{}},
            #{trace => false})
         ),
    #{ out := << RetVal:256 >> } = Res,
    42 = RetVal,
    ok.

