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
-include_lib("aebytecode/include/aeb_opcodes.hrl").
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
    Code = aeso_compiler:from_string(Contract, [pp_icode, pp_assembler]),

    %% Create the call data
    CallData = aeso_abi:create_calldata(Code, "main", "(42)"),
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
        %% We enter the VM with the calldata at address 32 and an empty stack, and
        %% should return with a typerep and return value on the stack and 0
        %% written to the state pointer at address 0, to indicate that we
        %% didn't update the state. We haven't initialized any state in this test though,
        %% so the state pointer is already 0.
                        %% Stack    Mem     Comment
                        %%          32:N
        <<?PUSH1, 32,   %% 32       32:N
          ?MLOAD,       %% N        32:N
          ?MSIZE,       %% P N      32:N
          ?PUSH1, 0,    %% 0 P N            The typerep for 'word' is a tuple {0}, i.e. a pointer to 0
          ?MSIZE,       %% P 0 P N
          ?MSTORE,      %% P N      32:N P:0
          ?RETURN>>,
    {ok, Res} =
        aevm_eeevm:eval(
          aevm_eeevm_state:init(
            #{ exec => #{ code => Code,
                          address => 91210,
                          caller => 0,
                          data => <<42:256>>,
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
