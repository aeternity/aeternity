-module(sophia_bytecode_aevm_SUITE).

%% common_test exports
-export(
   [
    all/0
   ]).

%% test case exports
-export(
   [
     execute_identity_fun_from_bytecode/1
     , execute_identity_fun_from_sophia_file/1
   ]).

-include("apps/aecontract/src/aecontract.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
      execute_identity_fun_from_bytecode,
      execute_identity_fun_from_sophia_file ].

execute_identity_fun_from_sophia_file(_Cfg) ->
    CodeDir = code:lib_dir(aesophia, test),
    FileName = filename:join(CodeDir, "contracts/identity.aes"),
    {ok, ContractBin} = file:read_file(FileName),
    Contract = binary_to_list(ContractBin),
    Code = aeso_compiler:from_string(Contract, []),

    %% Create the call data
    CallData = aeso_abi:create_calldata(Code, "main", "42"),

    {ok, Res} =
        aevm_eeevm:eval(
          aevm_eeevm_state:init(
            #{ exec => #{ code => Code,
                          address => 91210,
                          caller => 0,
                          data => CallData,
                          gas => 1000000,
                          gasPrice => 1,
                          origin => 0,
                          value => 0 },
               env => #{currentCoinbase => 0,
                        currentDifficulty => 0,
                        currentGasLimit => 10000,
                        currentNumber => 0,
                        currentTimestamp => 0,
                        chainState => aevm_dummy_chain:new_state(),
                        chainAPI => aevm_dummy_chain,
                        vm_version => ?AEVM_01_Sophia_01},
               pre => #{}},
            #{trace => true})
         ),
    #{ out := RetVal } = Res,
    <<42:256>> = RetVal,
    ok.

execute_identity_fun_from_bytecode(_Cfg) ->
    Code =
        <<96,0,53,128,127,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,20,98,0,0,44,87,0,91,96,32,53,98,0,0,58,144,98,0,0,67,86,91,
          96,0,82,96,32,96,0,243,91,128,144,80,144,86>>,
    {ok, Res} =
        aevm_eeevm:eval(
          aevm_eeevm_state:init(
            #{ exec => #{ code => Code,
                          address => 91210,
                          caller => 0,
                          data => <<0:256, 42:256>>,
                          gas => 1000000,
                          gasPrice => 1,
                          origin => 0,
                      value => 0 },
               env => #{currentCoinbase => 0,
                        currentDifficulty => 0,
                        currentGasLimit => 10000,
                        currentNumber => 0,
                        currentTimestamp => 0,
                        chainState => aevm_dummy_chain:new_state(),
                        chainAPI => aevm_dummy_chain,
                        vm_version => ?AEVM_01_Sophia_01},
               pre => #{}},
            #{trace => true})
         ),
    #{ out := << RetVal:256 >> } = Res,
    42 = RetVal,
    ok.
