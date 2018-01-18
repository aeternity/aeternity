-module(asm_call_SUITE).

%% common_test exports
-export(
   [
    all/0
   ]).

%% test case exports
-export(
   [ execute_identy_fun_from_file/1
   ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [ identity_functions ].


identity_function(_Cfg) ->
    Code = aeb_asm:file("apps/aering/test/contracts/identity.aesm",
                        []),
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
            #{trace => false})
         ),
    #{ out := << RetVal:256 >> } = Res,
    true == (Retval == 42),
    ok.

