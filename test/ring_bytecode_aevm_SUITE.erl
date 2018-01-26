-module(ring_bytecode_aevm_SUITE).

%% common_test exports
-export(
   [
    all/0
   ]).

%% test case exports
-export(
   [ execute_identy_fun_from_ring_file/1
   ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [ execute_identy_fun_from_ring_file ].

execute_identy_fun_from_ring_file(_Cfg) ->
    CodeDir = code:lib_dir(aering, test),
    FileName = filename:join(CodeDir, "contracts/identity.aer"),
    {ok, ContractBin} = file:read_file(FileName),
    Contract = binary_to_list(ContractBin),
    Code = aer_compiler:from_string(Contract,
                                    [pp_icode,
                                     pp_assembler,
                                     pp_bytecode]),
    ok = aeb_disassemble:pp(Code),

    %% Create the call data
    Call = {<<"main">>, 42},
    {0, Data} = aer_data:to_binary(Call),
    Res =
        aevm_eeevm:eval(
          aevm_eeevm_state:init(
            #{ exec => #{ code => Code,
                          address => 0,
                          caller => 0,
                          data => Data,
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
    #{ stack := [RetVal] } = Res,
    42 = RetVal,
    ok.

