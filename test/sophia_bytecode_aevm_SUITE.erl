-module(sophia_bytecode_aevm_SUITE).

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

-include("apps/aecontract/src/aecontract.hrl").
-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
      execute_identity_fun_from_sophia_file ].

execute_identity_fun_from_sophia_file(_Cfg) ->
    CodeDir = code:lib_dir(aesophia, test),
    FileName = filename:join(CodeDir, "contracts/identity.aes"),
    {ok, ContractBin} = file:read_file(FileName),
    Contract = binary_to_list(ContractBin),
    SerializedCode = aeso_compiler:from_string(Contract, [pp_icode, pp_assembler]),
    #{ byte_code := Code,
       type_info := TypeInfo} = aeso_compiler:deserialize(SerializedCode),
    {ok, ArgType} = aeso_compiler:arg_typerep_from_function(<<"main">>, TypeInfo),
    CallDataType = {tuple, [string, ArgType]},

    %% Create the call data
    CallData = aeso_abi:create_calldata(Code, "main", "(42)"),
    {ok, Res} =
        aevm_eeevm:eval(
          aevm_eeevm_state:init(
            #{ exec => #{ code => Code,
                          store => #{},
                          address => 91210,
                          caller => 0,
                          data => CallData,
                          call_data_type => CallDataType,
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
