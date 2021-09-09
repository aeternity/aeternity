-module(sophia_bytecode_aevm_SUITE).

%% common_test exports
-export(
   [ all/0
   , init_per_suite/1
   , end_per_suite/1
   ]).

%% test case exports
-export(
   [
    execute_identity_fun_from_sophia_file/1
   ]).

-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

all() ->
    [
      execute_identity_fun_from_sophia_file ].

init_per_suite(Config) ->
    case aect_test_utils:latest_protocol_version() < ?IRIS_PROTOCOL_VSN of
        true -> Config;
        false -> {skip, aevm_deprecated}
    end.

end_per_suite(_Config) ->
    ok.

execute_identity_fun_from_sophia_file(_Cfg) ->
    {ok, ContractBin} = aect_test_utils:read_contract(identity),
    {ok, Compiled}    = aect_test_utils:compile_contract(identity),
    #{ byte_code := Code,
       type_info := TypeInfo} = aect_sophia:deserialize(Compiled),
    {ok, ArgType} = aeb_aevm_abi:arg_typerep_from_function(<<"main_">>, TypeInfo),
    CallDataType = {tuple, [word, ArgType]},
    OutType = word,

    %% Create the call data
    {ok, CallData} = aect_test_utils:encode_call_data(ContractBin, <<"main_">>, [<<"42">>]),
    ABI = aect_test_utils:latest_sophia_abi_version(),
    VM = aect_test_utils:latest_sophia_vm_version(),
    {ok, Store} = aevm_eeevm_store:from_sophia_state(
                    #{vm => VM, abi => ABI},
                    aeb_heap:to_binary({{tuple, []}, {}})),
    {ok, Res} =
        aevm_eeevm:eval(
          aevm_eeevm_state:init(
            #{ exec => #{ code => Code,
                          store => Store,
                          address => 91210,
                          caller => 0,
                          data => CallData,
                          call_data_type => CallDataType,
                          out_type => OutType,
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
                        chainState => aevm_dummy_chain:new_state(),
                        chainAPI => aevm_dummy_chain,
                        protocol_version => aect_test_utils:latest_protocol_version(),
                        vm_version => VM,
                        abi_version => ABI},
               pre => #{}},
            #{trace => true})
         ),
    #{ out := RetVal } = Res,
    <<42:256>> = RetVal,
    ok.
