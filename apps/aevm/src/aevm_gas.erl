%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Calculate gas cost of operations
%%% @end
%%% Created : 2 Oct 2017
%%%-------------------------------------------------------------------

-module(aevm_gas).

-export([ op_cost/2
        ]).

-include("aevm_eeevm.hrl").
-include("aevm_gas.hrl").

%%====================================================================
%% API
%%====================================================================

op_cost(Op,_State) ->
  op_base_cost(Op).

%% ------------------------------------------------------------------------
%% OPCODES
%% ------------------------------------------------------------------------

%% @doc Opcodes defined as {OPCODE, POPPED, PUSHED, BASE_GAS_COST}

%% op_name(OP) -> element(1, opcode(OP)).
%% op_pop(OP) -> element(2, opcode(OP)).
%% op_push(OP) -> element(3, opcode(OP)).
op_base_cost(OP) -> element(4, opcode(OP)).

opcode(16#00) -> {'STOP'           ,  0,  0, ?GZERO};
opcode(16#01) -> {'ADD'            ,  2,  1, ?GVERYLOW};
opcode(16#02) -> {'MUL'            ,  2,  1, ?GLOW};
opcode(16#03) -> {'SUB'            ,  2,  1, ?GVERYLOW};
opcode(16#04) -> {'DIV'            ,  2,  1, ?GLOW};
opcode(16#05) -> {'SDIV'           ,  2,  1, ?GLOW};
opcode(16#06) -> {'MOD'            ,  2,  1, ?GLOW};
opcode(16#07) -> {'SMOD'           ,  2,  1, ?GLOW};
opcode(16#08) -> {'ADDMOD'         ,  3,  1, ?GMID};
opcode(16#09) -> {'MULMOD'         ,  3,  1, ?GMID};
opcode(16#0a) -> {'EXP'            ,  2,  1, ?GEXP};
opcode(16#0b) -> {'SIGNEXTEND'     ,  2,  1, ?GLOW};
opcode(16#10) -> {'LT'             ,  2,  1, ?GVERYLOW};
opcode(16#11) -> {'GT'             ,  2,  1, ?GVERYLOW};
opcode(16#12) -> {'SLT'            ,  2,  1, ?GVERYLOW};
opcode(16#13) -> {'SGT'            ,  2,  1, ?GVERYLOW};
opcode(16#14) -> {'EQ'             ,  2,  1, ?GVERYLOW};
opcode(16#15) -> {'ISZERO'         ,  1,  1, ?GVERYLOW};
opcode(16#16) -> {'AND'            ,  2,  1, ?GVERYLOW};
opcode(16#17) -> {'OR'             ,  2,  1, ?GVERYLOW};
opcode(16#18) -> {'XOR'            ,  2,  1, ?GVERYLOW};
opcode(16#19) -> {'NOT'            ,  1,  1, ?GVERYLOW};
opcode(16#1a) -> {'BYTE'           ,  2,  1, ?GVERYLOW};
opcode(16#20) -> {'SHA3'           ,  2,  1, ?GSHA3};
opcode(16#30) -> {'ADDRESS'        ,  0,  1, ?GBASE};
opcode(16#31) -> {'BALANCE'        ,  1,  1, ?GBALANCE};
opcode(16#32) -> {'ORIGIN'         ,  0,  1, ?GBASE};
opcode(16#33) -> {'CALLER'         ,  0,  1, ?GBASE};
opcode(16#34) -> {'CALLVALUE'      ,  0,  1, ?GBASE};
opcode(16#35) -> {'CALLDATALOAD'   ,  1,  1, ?GVERYLOW};
opcode(16#36) -> {'CALLDATASIZE'   ,  0,  1, ?GBASE};
opcode(16#37) -> {'CALLDATACOPY'   ,  3,  0, ?GVERYLOW};
opcode(16#38) -> {'CODESIZE'       ,  0,  1, ?GBASE};
opcode(16#39) -> {'CODECOPY'       ,  3,  0, ?GVERYLOW};
opcode(16#3a) -> {'GASPRICE'       ,  0,  1, ?GBASE};
opcode(16#3b) -> {'EXTCODESIZE'    ,  1,  1, ?GEXTCODE};
opcode(16#3c) -> {'EXTCODECOPY'    ,  4,  0, ?GEXTCODE};
opcode(16#3d) -> {'RETURNDATASIZE' ,  0,  1, 2}; %% TODO
opcode(16#3e) -> {'RETURNDATACOPY' ,  3,  0, 3}; %% TODO
opcode(16#40) -> {'BLOCKHASH'      ,  1,  1, ?GBLOCKHASH};
opcode(16#41) -> {'COINBASE'       ,  0,  1, ?GBASE};
opcode(16#42) -> {'TIMESTAMP'      ,  0,  1, ?GBASE};
opcode(16#43) -> {'NUMBER'         ,  0,  1, ?GBASE};
opcode(16#44) -> {'DIFFICULTY'     ,  0,  1, ?GBASE};
opcode(16#45) -> {'GASLIMIT'       ,  0,  1, ?GBASE};
opcode(16#50) -> {'POP'            ,  1,  0, ?GBASE};
opcode(16#51) -> {'MLOAD'          ,  1,  1, ?GVERYLOW};
opcode(16#52) -> {'MSTORE'         ,  2,  0, ?GVERYLOW};
opcode(16#53) -> {'MSTORE8'        ,  2,  0, ?GVERYLOW};
opcode(16#54) -> {'SLOAD'          ,  1,  1, ?GSLOAD};
opcode(16#55) -> {'SSTORE'         ,  2,  0, 0}; %% TODO
opcode(16#56) -> {'JUMP'           ,  1,  0, ?GMID};
opcode(16#57) -> {'JUMPI'          ,  2,  0, ?GHIGH};
opcode(16#58) -> {'PC'             ,  0,  1, ?GBASE};
opcode(16#59) -> {'MSIZE'          ,  0,  1, ?GBASE};
opcode(16#5a) -> {'GAS'            ,  0,  1, ?GBASE};
opcode(16#5b) -> {'JUMPDEST'       ,  0,  0, ?GJUMPDEST};
opcode(16#60) -> {'PUSH1'          ,  1,  0, ?GVERYLOW};
opcode(16#61) -> {'PUSH2'          ,  2,  0, ?GVERYLOW};
opcode(16#62) -> {'PUSH3'          ,  3,  0, ?GVERYLOW};
opcode(16#63) -> {'PUSH4'          ,  4,  0, ?GVERYLOW};
opcode(16#64) -> {'PUSH5'          ,  5,  0, ?GVERYLOW};
opcode(16#65) -> {'PUSH6'          ,  6,  0, ?GVERYLOW};
opcode(16#66) -> {'PUSH7'          ,  7,  0, ?GVERYLOW};
opcode(16#67) -> {'PUSH8'          ,  8,  0, ?GVERYLOW};
opcode(16#68) -> {'PUSH9'          ,  9,  0, ?GVERYLOW};
opcode(16#69) -> {'PUSH10'         , 10,  0, ?GVERYLOW};
opcode(16#6a) -> {'PUSH11'         , 11,  0, ?GVERYLOW};
opcode(16#6b) -> {'PUSH12'         , 12,  0, ?GVERYLOW};
opcode(16#6c) -> {'PUSH13'         , 13,  0, ?GVERYLOW};
opcode(16#6d) -> {'PUSH14'         , 14,  0, ?GVERYLOW};
opcode(16#6e) -> {'PUSH15'         , 15,  0, ?GVERYLOW};
opcode(16#6f) -> {'PUSH16'         , 16,  0, ?GVERYLOW};
opcode(16#70) -> {'PUSH17'         , 17,  0, ?GVERYLOW};
opcode(16#71) -> {'PUSH18'         , 18,  0, ?GVERYLOW};
opcode(16#72) -> {'PUSH19'         , 19,  0, ?GVERYLOW};
opcode(16#73) -> {'PUSH20'         , 20,  0, ?GVERYLOW};
opcode(16#74) -> {'PUSH21'         , 21,  0, ?GVERYLOW};
opcode(16#75) -> {'PUSH22'         , 22,  0, ?GVERYLOW};
opcode(16#76) -> {'PUSH23'         , 23,  0, ?GVERYLOW};
opcode(16#77) -> {'PUSH24'         , 24,  0, ?GVERYLOW};
opcode(16#78) -> {'PUSH25'         , 25,  0, ?GVERYLOW};
opcode(16#79) -> {'PUSH26'         , 26,  0, ?GVERYLOW};
opcode(16#7a) -> {'PUSH27'         , 27,  0, ?GVERYLOW};
opcode(16#7b) -> {'PUSH28'         , 28,  0, ?GVERYLOW};
opcode(16#7c) -> {'PUSH29'         , 29,  0, ?GVERYLOW};
opcode(16#7d) -> {'PUSH30'         , 30,  0, ?GVERYLOW};
opcode(16#7e) -> {'PUSH31'         , 31,  0, ?GVERYLOW};
opcode(16#7f) -> {'PUSH32'         , 32,  0, ?GVERYLOW};
opcode(16#80) -> {'DUP1'           ,  0,  1, ?GVERYLOW};
opcode(16#81) -> {'DUP2'           ,  0,  2, ?GVERYLOW};
opcode(16#82) -> {'DUP3'           ,  0,  3, ?GVERYLOW};
opcode(16#83) -> {'DUP4'           ,  0,  4, ?GVERYLOW};
opcode(16#84) -> {'DUP5'           ,  0,  5, ?GVERYLOW};
opcode(16#85) -> {'DUP6'           ,  0,  6, ?GVERYLOW};
opcode(16#86) -> {'DUP7'           ,  0,  7, ?GVERYLOW};
opcode(16#87) -> {'DUP8'           ,  0,  8, ?GVERYLOW};
opcode(16#88) -> {'DUP9'           ,  0,  9, ?GVERYLOW};
opcode(16#89) -> {'DUP10'          ,  0, 10, ?GVERYLOW};
opcode(16#8a) -> {'DUP11'          ,  0, 11, ?GVERYLOW};
opcode(16#8b) -> {'DUP12'          ,  0, 12, ?GVERYLOW};
opcode(16#8c) -> {'DUP13'          ,  0, 13, ?GVERYLOW};
opcode(16#8d) -> {'DUP14'          ,  0, 14, ?GVERYLOW};
opcode(16#8e) -> {'DUP15'          ,  0, 15, ?GVERYLOW};
opcode(16#8f) -> {'DUP16'          ,  0, 16, ?GVERYLOW};
opcode(16#90) -> {'SWAP1'          ,  0,  2, ?GVERYLOW};
opcode(16#91) -> {'SWAP2'          ,  0,  3, ?GVERYLOW};
opcode(16#92) -> {'SWAP3'          ,  0,  4, ?GVERYLOW};
opcode(16#93) -> {'SWAP4'          ,  0,  5, ?GVERYLOW};
opcode(16#94) -> {'SWAP5'          ,  0,  6, ?GVERYLOW};
opcode(16#95) -> {'SWAP6'          ,  0,  7, ?GVERYLOW};
opcode(16#96) -> {'SWAP7'          ,  0,  8, ?GVERYLOW};
opcode(16#97) -> {'SWAP8'          ,  0,  9, ?GVERYLOW};
opcode(16#98) -> {'SWAP9'          ,  0, 10, ?GVERYLOW};
opcode(16#99) -> {'SWAP10'         ,  0, 11, ?GVERYLOW};
opcode(16#9a) -> {'SWAP11'         ,  0, 12, ?GVERYLOW};
opcode(16#9b) -> {'SWAP12'         ,  0, 13, ?GVERYLOW};
opcode(16#9c) -> {'SWAP13'         ,  0, 14, ?GVERYLOW};
opcode(16#9d) -> {'SWAP14'         ,  0, 15, ?GVERYLOW};
opcode(16#9e) -> {'SWAP15'         ,  0, 16, ?GVERYLOW};
opcode(16#9f) -> {'SWAP16'         ,  0, 17, ?GVERYLOW};
opcode(16#a0) -> {'LOG0'           ,  2,  0, ?GLOG};
opcode(16#a1) -> {'LOG1'           ,  3,  0, ?GLOG + ?GLOGTOPIC};
opcode(16#a2) -> {'LOG2'           ,  4,  0, ?GLOG + 2*?GLOGTOPIC};
opcode(16#a3) -> {'LOG3'           ,  5,  0, ?GLOG + 3*?GLOGTOPIC};
opcode(16#a4) -> {'LOG4'           ,  6,  0, ?GLOG + 4*?GLOGTOPIC};
opcode(16#f0) -> {'CREATE'         ,  3,  1, ?GCREATE};
opcode(16#f1) -> {'CALL'           ,  7,  1, 40}; %% TODO
opcode(16#f2) -> {'CALLCODE'       ,  7,  1, 40}; %% TODO
opcode(16#f3) -> {'RETURN'         ,  2,  0, ?GZERO};
opcode(16#f4) -> {'DELEGATECALL'   ,  6,  1, 40}; %% TODO
opcode(16#f5) -> {'CALLBLACKBOX'   ,  7,  1, 40}; %% TODO
opcode(16#fa) -> {'STATICCALL'     ,  6,  1, 40}; %% TODO
opcode(16#fd) -> {'REVERT'         ,  2,  0, 0};
opcode(16#ff) -> {'SUICIDE'        ,  1,  0, ?GSELFDESTRUCT}. %% TODO: Correct?
