%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Opcodes
%%% @end
%%% Created : 2 Oct 2017
%%%-------------------------------------------------------------------

-module(aevm_opcodes).

-export([ op_base_cost/1
        , op_name/1
        ]).

-include("aevm_eeevm.hrl").
-include("aevm_gas.hrl").


%%====================================================================
%% API
%%====================================================================

op_name(OP) -> element(1, opcode(OP)).
%% op_pop(OP) -> element(2, opcode(OP)).
%% op_push(OP) -> element(3, opcode(OP)).
op_base_cost(OP) -> element(4, opcode(OP)).

%% @doc Opcodes defined as {OPCODE, POPPED, PUSHED, BASE_GAS_COST}

opcode(?STOP)           -> {'STOP'           ,  0,  0, ?GZERO};
opcode(?ADD)            -> {'ADD'            ,  2,  1, ?GVERYLOW};
opcode(?MUL)            -> {'MUL'            ,  2,  1, ?GLOW};
opcode(?SUB)            -> {'SUB'            ,  2,  1, ?GVERYLOW};
opcode(?DIV)            -> {'DIV'            ,  2,  1, ?GLOW};
opcode(?SDIV)           -> {'SDIV'           ,  2,  1, ?GLOW};
opcode(?MOD)            -> {'MOD'            ,  2,  1, ?GLOW};
opcode(?SMOD)           -> {'SMOD'           ,  2,  1, ?GLOW};
opcode(?ADDMOD)         -> {'ADDMOD'         ,  3,  1, ?GMID};
opcode(?MULMOD)         -> {'MULMOD'         ,  3,  1, ?GMID};
opcode(?EXP)            -> {'EXP'            ,  2,  1, ?GEXP};
opcode(?SIGNEXTEND)     -> {'SIGNEXTEND'     ,  2,  1, ?GLOW};
opcode(?LT)             -> {'LT'             ,  2,  1, ?GVERYLOW};
opcode(?GT)             -> {'GT'             ,  2,  1, ?GVERYLOW};
opcode(?SLT)            -> {'SLT'            ,  2,  1, ?GVERYLOW};
opcode(?SGT)            -> {'SGT'            ,  2,  1, ?GVERYLOW};
opcode(?EQ)             -> {'EQ'             ,  2,  1, ?GVERYLOW};
opcode(?ISZERO)         -> {'ISZERO'         ,  1,  1, ?GVERYLOW};
opcode(?AND)            -> {'AND'            ,  2,  1, ?GVERYLOW};
opcode(?OR)             -> {'OR'             ,  2,  1, ?GVERYLOW};
opcode(?XOR)            -> {'XOR'            ,  2,  1, ?GVERYLOW};
opcode(?NOT)            -> {'NOT'            ,  1,  1, ?GVERYLOW};
opcode(?BYTE)           -> {'BYTE'           ,  2,  1, ?GVERYLOW};
opcode(?SHA3)           -> {'SHA3'           ,  2,  1, ?GSHA3};
opcode(?ADDRESS)        -> {'ADDRESS'        ,  0,  1, ?GBASE};
opcode(?BALANCE)        -> {'BALANCE'        ,  1,  1, ?GBALANCE};
opcode(?ORIGIN)         -> {'ORIGIN'         ,  0,  1, ?GBASE};
opcode(?CALLER)         -> {'CALLER'         ,  0,  1, ?GBASE};
opcode(?CALLVALUE)      -> {'CALLVALUE'      ,  0,  1, ?GBASE};
opcode(?CALLDATALOAD)   -> {'CALLDATALOAD'   ,  1,  1, ?GVERYLOW};
opcode(?CALLDATASIZE)   -> {'CALLDATASIZE'   ,  0,  1, ?GBASE};
opcode(?CALLDATACOPY)   -> {'CALLDATACOPY'   ,  3,  0, ?GVERYLOW};
opcode(?CODESIZE)       -> {'CODESIZE'       ,  0,  1, ?GBASE};
opcode(?CODECOPY)       -> {'CODECOPY'       ,  3,  0, ?GVERYLOW};
opcode(?GASPRICE)       -> {'GASPRICE'       ,  0,  1, ?GBASE};
opcode(?EXTCODESIZE)    -> {'EXTCODESIZE'    ,  1,  1, ?GEXTCODE};
opcode(?EXTCODECOPY)    -> {'EXTCODECOPY'    ,  4,  0, ?GEXTCODE};
opcode(?RETURNDATASIZE) -> {'RETURNDATASIZE' ,  0,  1, 2}; %% TODO
opcode(?RETURNDATACOPY) -> {'RETURNDATACOPY' ,  3,  0, 3}; %% TODO
opcode(?BLOCKHASH)      -> {'BLOCKHASH'      ,  1,  1, ?GBLOCKHASH};
opcode(?COINBASE)       -> {'COINBASE'       ,  0,  1, ?GBASE};
opcode(?TIMESTAMP)      -> {'TIMESTAMP'      ,  0,  1, ?GBASE};
opcode(?NUMBER)         -> {'NUMBER'         ,  0,  1, ?GBASE};
opcode(?DIFFICULTY)     -> {'DIFFICULTY'     ,  0,  1, ?GBASE};
opcode(?GASLIMIT)       -> {'GASLIMIT'       ,  0,  1, ?GBASE};
opcode(?POP)            -> {'POP'            ,  1,  0, ?GBASE};
opcode(?MLOAD)          -> {'MLOAD'          ,  1,  1, ?GVERYLOW};
opcode(?MSTORE)         -> {'MSTORE'         ,  2,  0, ?GVERYLOW};
opcode(?MSTORE8)        -> {'MSTORE8'        ,  2,  0, ?GVERYLOW};
opcode(?SLOAD)          -> {'SLOAD'          ,  1,  1, ?GSLOAD};
opcode(?SSTORE)         -> {'SSTORE'         ,  2,  0, 0};
opcode(?JUMP)           -> {'JUMP'           ,  1,  0, ?GMID};
opcode(?JUMPI)          -> {'JUMPI'          ,  2,  0, ?GHIGH};
opcode(?PC)             -> {'PC'             ,  0,  1, ?GBASE};
opcode(?MSIZE)          -> {'MSIZE'          ,  0,  1, ?GBASE};
opcode(?GAS)            -> {'GAS'            ,  0,  1, ?GBASE};
opcode(?JUMPDEST)       -> {'JUMPDEST'       ,  0,  0, ?GJUMPDEST};
opcode(?PUSH1)          -> {'PUSH1'          ,  1,  0, ?GVERYLOW};
opcode(?PUSH2)          -> {'PUSH2'          ,  2,  0, ?GVERYLOW};
opcode(?PUSH3)          -> {'PUSH3'          ,  3,  0, ?GVERYLOW};
opcode(?PUSH4)          -> {'PUSH4'          ,  4,  0, ?GVERYLOW};
opcode(?PUSH5)          -> {'PUSH5'          ,  5,  0, ?GVERYLOW};
opcode(?PUSH6)          -> {'PUSH6'          ,  6,  0, ?GVERYLOW};
opcode(?PUSH7)          -> {'PUSH7'          ,  7,  0, ?GVERYLOW};
opcode(?PUSH8)          -> {'PUSH8'          ,  8,  0, ?GVERYLOW};
opcode(?PUSH9)          -> {'PUSH9'          ,  9,  0, ?GVERYLOW};
opcode(?PUSH10)         -> {'PUSH10'         , 10,  0, ?GVERYLOW};
opcode(?PUSH11)         -> {'PUSH11'         , 11,  0, ?GVERYLOW};
opcode(?PUSH12)         -> {'PUSH12'         , 12,  0, ?GVERYLOW};
opcode(?PUSH13)         -> {'PUSH13'         , 13,  0, ?GVERYLOW};
opcode(?PUSH14)         -> {'PUSH14'         , 14,  0, ?GVERYLOW};
opcode(?PUSH15)         -> {'PUSH15'         , 15,  0, ?GVERYLOW};
opcode(?PUSH16)         -> {'PUSH16'         , 16,  0, ?GVERYLOW};
opcode(?PUSH17)         -> {'PUSH17'         , 17,  0, ?GVERYLOW};
opcode(?PUSH18)         -> {'PUSH18'         , 18,  0, ?GVERYLOW};
opcode(?PUSH19)         -> {'PUSH19'         , 19,  0, ?GVERYLOW};
opcode(?PUSH20)         -> {'PUSH20'         , 20,  0, ?GVERYLOW};
opcode(?PUSH21)         -> {'PUSH21'         , 21,  0, ?GVERYLOW};
opcode(?PUSH22)         -> {'PUSH22'         , 22,  0, ?GVERYLOW};
opcode(?PUSH23)         -> {'PUSH23'         , 23,  0, ?GVERYLOW};
opcode(?PUSH24)         -> {'PUSH24'         , 24,  0, ?GVERYLOW};
opcode(?PUSH25)         -> {'PUSH25'         , 25,  0, ?GVERYLOW};
opcode(?PUSH26)         -> {'PUSH26'         , 26,  0, ?GVERYLOW};
opcode(?PUSH27)         -> {'PUSH27'         , 27,  0, ?GVERYLOW};
opcode(?PUSH28)         -> {'PUSH28'         , 28,  0, ?GVERYLOW};
opcode(?PUSH29)         -> {'PUSH29'         , 29,  0, ?GVERYLOW};
opcode(?PUSH30)         -> {'PUSH30'         , 30,  0, ?GVERYLOW};
opcode(?PUSH31)         -> {'PUSH31'         , 31,  0, ?GVERYLOW};
opcode(?PUSH32)         -> {'PUSH32'         , 32,  0, ?GVERYLOW};
opcode(?DUP1)           -> {'DUP1'           ,  0,  1, ?GVERYLOW};
opcode(?DUP2)           -> {'DUP2'           ,  0,  2, ?GVERYLOW};
opcode(?DUP3)           -> {'DUP3'           ,  0,  3, ?GVERYLOW};
opcode(?DUP4)           -> {'DUP4'           ,  0,  4, ?GVERYLOW};
opcode(?DUP5)           -> {'DUP5'           ,  0,  5, ?GVERYLOW};
opcode(?DUP6)           -> {'DUP6'           ,  0,  6, ?GVERYLOW};
opcode(?DUP7)           -> {'DUP7'           ,  0,  7, ?GVERYLOW};
opcode(?DUP8)           -> {'DUP8'           ,  0,  8, ?GVERYLOW};
opcode(?DUP9)           -> {'DUP9'           ,  0,  9, ?GVERYLOW};
opcode(?DUP10)          -> {'DUP10'          ,  0, 10, ?GVERYLOW};
opcode(?DUP11)          -> {'DUP11'          ,  0, 11, ?GVERYLOW};
opcode(?DUP12)          -> {'DUP12'          ,  0, 12, ?GVERYLOW};
opcode(?DUP13)          -> {'DUP13'          ,  0, 13, ?GVERYLOW};
opcode(?DUP14)          -> {'DUP14'          ,  0, 14, ?GVERYLOW};
opcode(?DUP15)          -> {'DUP15'          ,  0, 15, ?GVERYLOW};
opcode(?DUP16)          -> {'DUP16'          ,  0, 16, ?GVERYLOW};
opcode(?SWAP1)          -> {'SWAP1'          ,  0,  2, ?GVERYLOW};
opcode(?SWAP2)          -> {'SWAP2'          ,  0,  3, ?GVERYLOW};
opcode(?SWAP3)          -> {'SWAP3'          ,  0,  4, ?GVERYLOW};
opcode(?SWAP4)          -> {'SWAP4'          ,  0,  5, ?GVERYLOW};
opcode(?SWAP5)          -> {'SWAP5'          ,  0,  6, ?GVERYLOW};
opcode(?SWAP6)          -> {'SWAP6'          ,  0,  7, ?GVERYLOW};
opcode(?SWAP7)          -> {'SWAP7'          ,  0,  8, ?GVERYLOW};
opcode(?SWAP8)          -> {'SWAP8'          ,  0,  9, ?GVERYLOW};
opcode(?SWAP9)          -> {'SWAP9'          ,  0, 10, ?GVERYLOW};
opcode(?SWAP10)         -> {'SWAP10'         ,  0, 11, ?GVERYLOW};
opcode(?SWAP11)         -> {'SWAP11'         ,  0, 12, ?GVERYLOW};
opcode(?SWAP12)         -> {'SWAP12'         ,  0, 13, ?GVERYLOW};
opcode(?SWAP13)         -> {'SWAP13'         ,  0, 14, ?GVERYLOW};
opcode(?SWAP14)         -> {'SWAP14'         ,  0, 15, ?GVERYLOW};
opcode(?SWAP15)         -> {'SWAP15'         ,  0, 16, ?GVERYLOW};
opcode(?SWAP16)         -> {'SWAP16'         ,  0, 17, ?GVERYLOW};
opcode(?LOG0)           -> {'LOG0'           ,  2,  0, ?GLOG};
opcode(?LOG1)           -> {'LOG1'           ,  3,  0, ?GLOG + ?GLOGTOPIC};
opcode(?LOG2)           -> {'LOG2'           ,  4,  0, ?GLOG + 2*?GLOGTOPIC};
opcode(?LOG3)           -> {'LOG3'           ,  5,  0, ?GLOG + 3*?GLOGTOPIC};
opcode(?LOG4)           -> {'LOG4'           ,  6,  0, ?GLOG + 4*?GLOGTOPIC};
opcode(?CREATE)         -> {'CREATE'         ,  3,  1, ?GCREATE};
opcode(?CALL)           -> {'CALL'           ,  7,  1, 0};
opcode(?CALLCODE)       -> {'CALLCODE'       ,  7,  1, 0};
opcode(?RETURN)         -> {'RETURN'         ,  2,  0, ?GZERO};
opcode(?DELEGATECALL)   -> {'DELEGATECALL'   ,  6,  1, 0};
opcode(?CALLBLACKBOX)   -> {'CALLBLACKBOX'   ,  7,  1, 40}; %% TODO
opcode(?STATICCALL)     -> {'STATICCALL'     ,  6,  1, 40}; %% TODO
opcode(?REVERT)         -> {'REVERT'         ,  2,  0, 0};
opcode(?SUICIDE)        -> {'SUICIDE'        ,  1,  0, ?GSELFDESTRUCT}. %% TODO
