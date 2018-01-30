-module(ring_bytecode_aevm_SUITE).

%% common_test exports
-export(
   [
    all/0
   ]).

%% test case exports
-export(
   [ 
     execute_identity_fun_from_bytecode/1
     , execute_identity_fun_from_ring_file/1
   ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [ 
      execute_identity_fun_from_bytecode,
      execute_identity_fun_from_ring_file ].

execute_identity_fun_from_ring_file(_Cfg) ->
    CodeDir = code:lib_dir(aering, test),
    FileName = filename:join(CodeDir, "contracts/identity.aer"),
    {ok, ContractBin} = file:read_file(FileName),
    Contract = binary_to_list(ContractBin),
    Code = aer_compiler:from_string(Contract, []),

    %% Create the call data
    CallData = aer_abi:create_calldata(Code, "main", "42"),

    Res = 
        aevm_eeevm:eval(
          aevm_eeevm_state:init(
            #{ exec => #{ code => Code,
                          address => 0,
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
                        currentTimestamp => 0},
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
    Res = 
        aevm_eeevm:eval(
          aevm_eeevm_state:init(
            #{ exec => #{ code => Code,
                          address => 0,
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
                        currentTimestamp => 0},
               pre => #{}},
            #{trace => true})
         ),
    #{ out := << RetVal:256 >> } = Res,
    42 = RetVal,
    ok.
