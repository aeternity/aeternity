%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Opcodes
%%% @end
%%% Created : 14 Dec 2017
%%%-------------------------------------------------------------------

-module(aevm_opcodes).

-export([ op_base_cost/1
        , op_name/1
        ]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("aevm_gas.hrl").
%%====================================================================
%% API
%%====================================================================

op_name(OP) -> element(1, opcode(OP)).
%% op_pop(OP) -> element(2, opcode(OP)).
%% op_push(OP) -> element(3, opcode(OP)).
op_base_cost(OP) -> element(4, opcode(OP)).

%% @doc Opcodes defined as {OPCODE, POPPED, PUSHED, BASE_GAS_COST}

opcode(?STOP)           -> {aeb_opcodes:mnemonic(?STOP)           ,  0,  0, ?GZERO};
opcode(?ADD)            -> {aeb_opcodes:mnemonic(?ADD)            ,  2,  1, ?GVERYLOW};
opcode(?MUL)            -> {aeb_opcodes:mnemonic(?MUL)            ,  2,  1, ?GLOW};
opcode(?SUB)            -> {aeb_opcodes:mnemonic(?SUB)            ,  2,  1, ?GVERYLOW};
opcode(?DIV)            -> {aeb_opcodes:mnemonic(?DIV)            ,  2,  1, ?GLOW};
opcode(?SDIV)           -> {aeb_opcodes:mnemonic(?SDIV)           ,  2,  1, ?GLOW};
opcode(?MOD)            -> {aeb_opcodes:mnemonic(?MOD)            ,  2,  1, ?GLOW};
opcode(?SMOD)           -> {aeb_opcodes:mnemonic(?SMOD)           ,  2,  1, ?GLOW};
opcode(?ADDMOD)         -> {aeb_opcodes:mnemonic(?ADDMOD)         ,  3,  1, ?GMID};
opcode(?MULMOD)         -> {aeb_opcodes:mnemonic(?MULMOD)         ,  3,  1, ?GMID};
opcode(?EXP)            -> {aeb_opcodes:mnemonic(?EXP)            ,  2,  1, ?GEXP};
opcode(?SIGNEXTEND)     -> {aeb_opcodes:mnemonic(?SIGNEXTEND)     ,  2,  1, ?GLOW};
opcode(?LT)             -> {aeb_opcodes:mnemonic(?LT)             ,  2,  1, ?GVERYLOW};
opcode(?GT)             -> {aeb_opcodes:mnemonic(?GT)             ,  2,  1, ?GVERYLOW};
opcode(?SLT)            -> {aeb_opcodes:mnemonic(?SLT)            ,  2,  1, ?GVERYLOW};
opcode(?SGT)            -> {aeb_opcodes:mnemonic(?SGT)            ,  2,  1, ?GVERYLOW};
opcode(?EQ)             -> {aeb_opcodes:mnemonic(?EQ)             ,  2,  1, ?GVERYLOW};
opcode(?ISZERO)         -> {aeb_opcodes:mnemonic(?ISZERO)         ,  1,  1, ?GVERYLOW};
opcode(?AND)            -> {aeb_opcodes:mnemonic(?AND)            ,  2,  1, ?GVERYLOW};
opcode(?OR)             -> {aeb_opcodes:mnemonic(?OR)             ,  2,  1, ?GVERYLOW};
opcode(?XOR)            -> {aeb_opcodes:mnemonic(?XOR)            ,  2,  1, ?GVERYLOW};
opcode(?NOT)            -> {aeb_opcodes:mnemonic(?NOT)            ,  1,  1, ?GVERYLOW};
opcode(?BYTE)           -> {aeb_opcodes:mnemonic(?BYTE)           ,  2,  1, ?GVERYLOW};
opcode(?SHA3)           -> {aeb_opcodes:mnemonic(?SHA3)           ,  2,  1, ?GSHA3};
opcode(?ADDRESS)        -> {aeb_opcodes:mnemonic(?ADDRESS)        ,  0,  1, ?GBASE};
opcode(?BALANCE)        -> {aeb_opcodes:mnemonic(?BALANCE)        ,  1,  1, ?GBALANCE};
opcode(?ORIGIN)         -> {aeb_opcodes:mnemonic(?ORIGIN)         ,  0,  1, ?GBASE};
opcode(?CALLER)         -> {aeb_opcodes:mnemonic(?CALLER)         ,  0,  1, ?GBASE};
opcode(?CALLVALUE)      -> {aeb_opcodes:mnemonic(?CALLVALUE)      ,  0,  1, ?GBASE};
opcode(?CALLDATALOAD)   -> {aeb_opcodes:mnemonic(?CALLDATALOAD)   ,  1,  1, ?GVERYLOW};
opcode(?CALLDATASIZE)   -> {aeb_opcodes:mnemonic(?CALLDATASIZE)   ,  0,  1, ?GBASE};
opcode(?CALLDATACOPY)   -> {aeb_opcodes:mnemonic(?CALLDATACOPY)   ,  3,  0, ?GVERYLOW};
opcode(?CODESIZE)       -> {aeb_opcodes:mnemonic(?CODESIZE)       ,  0,  1, ?GBASE};
opcode(?CODECOPY)       -> {aeb_opcodes:mnemonic(?CODECOPY)       ,  3,  0, ?GVERYLOW};
opcode(?GASPRICE)       -> {aeb_opcodes:mnemonic(?GASPRICE)       ,  0,  1, ?GBASE};
opcode(?EXTCODESIZE)    -> {aeb_opcodes:mnemonic(?EXTCODESIZE)    ,  1,  1, ?GEXTCODESIZE};
opcode(?EXTCODECOPY)    -> {aeb_opcodes:mnemonic(?EXTCODECOPY)    ,  4,  0, ?GEXTCODECOPY};
opcode(?RETURNDATASIZE) -> {aeb_opcodes:mnemonic(?RETURNDATASIZE) ,  0,  1, 2}; %% TODO
opcode(?RETURNDATACOPY) -> {aeb_opcodes:mnemonic(?RETURNDATACOPY) ,  3,  0, 3}; %% TODO
opcode(?BLOCKHASH)      -> {aeb_opcodes:mnemonic(?BLOCKHASH)      ,  1,  1, ?GBLOCKHASH};
opcode(?COINBASE)       -> {aeb_opcodes:mnemonic(?COINBASE)       ,  0,  1, ?GBASE};
opcode(?TIMESTAMP)      -> {aeb_opcodes:mnemonic(?TIMESTAMP)      ,  0,  1, ?GBASE};
opcode(?NUMBER)         -> {aeb_opcodes:mnemonic(?NUMBER)         ,  0,  1, ?GBASE};
opcode(?DIFFICULTY)     -> {aeb_opcodes:mnemonic(?DIFFICULTY)     ,  0,  1, ?GBASE};
opcode(?GASLIMIT)       -> {aeb_opcodes:mnemonic(?GASLIMIT)       ,  0,  1, ?GBASE};
opcode(?POP)            -> {aeb_opcodes:mnemonic(?POP)            ,  1,  0, ?GBASE};
opcode(?MLOAD)          -> {aeb_opcodes:mnemonic(?MLOAD)          ,  1,  1, ?GVERYLOW};
opcode(?MSTORE)         -> {aeb_opcodes:mnemonic(?MSTORE)         ,  2,  0, ?GVERYLOW};
opcode(?MSTORE8)        -> {aeb_opcodes:mnemonic(?MSTORE8)        ,  2,  0, ?GVERYLOW};
opcode(?SLOAD)          -> {aeb_opcodes:mnemonic(?SLOAD)          ,  1,  1, ?GSLOAD};
opcode(?SSTORE)         -> {aeb_opcodes:mnemonic(?SSTORE)         ,  2,  0, 0};
opcode(?JUMP)           -> {aeb_opcodes:mnemonic(?JUMP)           ,  1,  0, ?GMID};
opcode(?JUMPI)          -> {aeb_opcodes:mnemonic(?JUMPI)          ,  2,  0, ?GHIGH};
opcode(?PC)             -> {aeb_opcodes:mnemonic(?PC)             ,  0,  1, ?GBASE};
opcode(?MSIZE)          -> {aeb_opcodes:mnemonic(?MSIZE)          ,  0,  1, ?GBASE};
opcode(?GAS)            -> {aeb_opcodes:mnemonic(?GAS)            ,  0,  1, ?GBASE};
opcode(?JUMPDEST)       -> {aeb_opcodes:mnemonic(?JUMPDEST)       ,  0,  0, ?GJUMPDEST};
opcode(?PUSH1)          -> {aeb_opcodes:mnemonic(?PUSH1)          ,  1,  0, ?GVERYLOW};
opcode(?PUSH2)          -> {aeb_opcodes:mnemonic(?PUSH2)          ,  2,  0, ?GVERYLOW};
opcode(?PUSH3)          -> {aeb_opcodes:mnemonic(?PUSH3)          ,  3,  0, ?GVERYLOW};
opcode(?PUSH4)          -> {aeb_opcodes:mnemonic(?PUSH4)          ,  4,  0, ?GVERYLOW};
opcode(?PUSH5)          -> {aeb_opcodes:mnemonic(?PUSH5)          ,  5,  0, ?GVERYLOW};
opcode(?PUSH6)          -> {aeb_opcodes:mnemonic(?PUSH6)          ,  6,  0, ?GVERYLOW};
opcode(?PUSH7)          -> {aeb_opcodes:mnemonic(?PUSH7)          ,  7,  0, ?GVERYLOW};
opcode(?PUSH8)          -> {aeb_opcodes:mnemonic(?PUSH8)          ,  8,  0, ?GVERYLOW};
opcode(?PUSH9)          -> {aeb_opcodes:mnemonic(?PUSH9)          ,  9,  0, ?GVERYLOW};
opcode(?PUSH10)         -> {aeb_opcodes:mnemonic(?PUSH10)         , 10,  0, ?GVERYLOW};
opcode(?PUSH11)         -> {aeb_opcodes:mnemonic(?PUSH11)         , 11,  0, ?GVERYLOW};
opcode(?PUSH12)         -> {aeb_opcodes:mnemonic(?PUSH12)         , 12,  0, ?GVERYLOW};
opcode(?PUSH13)         -> {aeb_opcodes:mnemonic(?PUSH13)         , 13,  0, ?GVERYLOW};
opcode(?PUSH14)         -> {aeb_opcodes:mnemonic(?PUSH14)         , 14,  0, ?GVERYLOW};
opcode(?PUSH15)         -> {aeb_opcodes:mnemonic(?PUSH15)         , 15,  0, ?GVERYLOW};
opcode(?PUSH16)         -> {aeb_opcodes:mnemonic(?PUSH16)         , 16,  0, ?GVERYLOW};
opcode(?PUSH17)         -> {aeb_opcodes:mnemonic(?PUSH17)         , 17,  0, ?GVERYLOW};
opcode(?PUSH18)         -> {aeb_opcodes:mnemonic(?PUSH18)         , 18,  0, ?GVERYLOW};
opcode(?PUSH19)         -> {aeb_opcodes:mnemonic(?PUSH19)         , 19,  0, ?GVERYLOW};
opcode(?PUSH20)         -> {aeb_opcodes:mnemonic(?PUSH20)         , 20,  0, ?GVERYLOW};
opcode(?PUSH21)         -> {aeb_opcodes:mnemonic(?PUSH21)         , 21,  0, ?GVERYLOW};
opcode(?PUSH22)         -> {aeb_opcodes:mnemonic(?PUSH22)         , 22,  0, ?GVERYLOW};
opcode(?PUSH23)         -> {aeb_opcodes:mnemonic(?PUSH23)         , 23,  0, ?GVERYLOW};
opcode(?PUSH24)         -> {aeb_opcodes:mnemonic(?PUSH24)         , 24,  0, ?GVERYLOW};
opcode(?PUSH25)         -> {aeb_opcodes:mnemonic(?PUSH25)         , 25,  0, ?GVERYLOW};
opcode(?PUSH26)         -> {aeb_opcodes:mnemonic(?PUSH26)         , 26,  0, ?GVERYLOW};
opcode(?PUSH27)         -> {aeb_opcodes:mnemonic(?PUSH27)         , 27,  0, ?GVERYLOW};
opcode(?PUSH28)         -> {aeb_opcodes:mnemonic(?PUSH28)         , 28,  0, ?GVERYLOW};
opcode(?PUSH29)         -> {aeb_opcodes:mnemonic(?PUSH29)         , 29,  0, ?GVERYLOW};
opcode(?PUSH30)         -> {aeb_opcodes:mnemonic(?PUSH30)         , 30,  0, ?GVERYLOW};
opcode(?PUSH31)         -> {aeb_opcodes:mnemonic(?PUSH31)         , 31,  0, ?GVERYLOW};
opcode(?PUSH32)         -> {aeb_opcodes:mnemonic(?PUSH32)         , 32,  0, ?GVERYLOW};
opcode(?DUP1)           -> {aeb_opcodes:mnemonic(?DUP1)           ,  0,  1, ?GVERYLOW};
opcode(?DUP2)           -> {aeb_opcodes:mnemonic(?DUP2)           ,  0,  2, ?GVERYLOW};
opcode(?DUP3)           -> {aeb_opcodes:mnemonic(?DUP3)           ,  0,  3, ?GVERYLOW};
opcode(?DUP4)           -> {aeb_opcodes:mnemonic(?DUP4)           ,  0,  4, ?GVERYLOW};
opcode(?DUP5)           -> {aeb_opcodes:mnemonic(?DUP5)           ,  0,  5, ?GVERYLOW};
opcode(?DUP6)           -> {aeb_opcodes:mnemonic(?DUP6)           ,  0,  6, ?GVERYLOW};
opcode(?DUP7)           -> {aeb_opcodes:mnemonic(?DUP7)           ,  0,  7, ?GVERYLOW};
opcode(?DUP8)           -> {aeb_opcodes:mnemonic(?DUP8)           ,  0,  8, ?GVERYLOW};
opcode(?DUP9)           -> {aeb_opcodes:mnemonic(?DUP9)           ,  0,  9, ?GVERYLOW};
opcode(?DUP10)          -> {aeb_opcodes:mnemonic(?DUP10)          ,  0, 10, ?GVERYLOW};
opcode(?DUP11)          -> {aeb_opcodes:mnemonic(?DUP11)          ,  0, 11, ?GVERYLOW};
opcode(?DUP12)          -> {aeb_opcodes:mnemonic(?DUP12)          ,  0, 12, ?GVERYLOW};
opcode(?DUP13)          -> {aeb_opcodes:mnemonic(?DUP13)          ,  0, 13, ?GVERYLOW};
opcode(?DUP14)          -> {aeb_opcodes:mnemonic(?DUP14)          ,  0, 14, ?GVERYLOW};
opcode(?DUP15)          -> {aeb_opcodes:mnemonic(?DUP15)          ,  0, 15, ?GVERYLOW};
opcode(?DUP16)          -> {aeb_opcodes:mnemonic(?DUP16)          ,  0, 16, ?GVERYLOW};
opcode(?SWAP1)          -> {aeb_opcodes:mnemonic(?SWAP1)          ,  0,  2, ?GVERYLOW};
opcode(?SWAP2)          -> {aeb_opcodes:mnemonic(?SWAP2)          ,  0,  3, ?GVERYLOW};
opcode(?SWAP3)          -> {aeb_opcodes:mnemonic(?SWAP3)          ,  0,  4, ?GVERYLOW};
opcode(?SWAP4)          -> {aeb_opcodes:mnemonic(?SWAP4)          ,  0,  5, ?GVERYLOW};
opcode(?SWAP5)          -> {aeb_opcodes:mnemonic(?SWAP5)          ,  0,  6, ?GVERYLOW};
opcode(?SWAP6)          -> {aeb_opcodes:mnemonic(?SWAP6)          ,  0,  7, ?GVERYLOW};
opcode(?SWAP7)          -> {aeb_opcodes:mnemonic(?SWAP7)          ,  0,  8, ?GVERYLOW};
opcode(?SWAP8)          -> {aeb_opcodes:mnemonic(?SWAP8)          ,  0,  9, ?GVERYLOW};
opcode(?SWAP9)          -> {aeb_opcodes:mnemonic(?SWAP9)          ,  0, 10, ?GVERYLOW};
opcode(?SWAP10)         -> {aeb_opcodes:mnemonic(?SWAP10)         ,  0, 11, ?GVERYLOW};
opcode(?SWAP11)         -> {aeb_opcodes:mnemonic(?SWAP11)         ,  0, 12, ?GVERYLOW};
opcode(?SWAP12)         -> {aeb_opcodes:mnemonic(?SWAP12)         ,  0, 13, ?GVERYLOW};
opcode(?SWAP13)         -> {aeb_opcodes:mnemonic(?SWAP13)         ,  0, 14, ?GVERYLOW};
opcode(?SWAP14)         -> {aeb_opcodes:mnemonic(?SWAP14)         ,  0, 15, ?GVERYLOW};
opcode(?SWAP15)         -> {aeb_opcodes:mnemonic(?SWAP15)         ,  0, 16, ?GVERYLOW};
opcode(?SWAP16)         -> {aeb_opcodes:mnemonic(?SWAP16)         ,  0, 17, ?GVERYLOW};
opcode(?LOG0)           -> {aeb_opcodes:mnemonic(?LOG0)           ,  2,  0, ?GLOG};
opcode(?LOG1)           -> {aeb_opcodes:mnemonic(?LOG1)           ,  3,  0, ?GLOG + ?GLOGTOPIC};
opcode(?LOG2)           -> {aeb_opcodes:mnemonic(?LOG2)           ,  4,  0, ?GLOG + 2*?GLOGTOPIC};
opcode(?LOG3)           -> {aeb_opcodes:mnemonic(?LOG3)           ,  5,  0, ?GLOG + 3*?GLOGTOPIC};
opcode(?LOG4)           -> {aeb_opcodes:mnemonic(?LOG4)           ,  6,  0, ?GLOG + 4*?GLOGTOPIC};
opcode(?CREATE)         -> {aeb_opcodes:mnemonic(?CREATE)         ,  3,  1, ?GCREATE};
opcode(?CALL)           -> {aeb_opcodes:mnemonic(?CALL)           ,  7,  1, 0};
opcode(?CALLCODE)       -> {aeb_opcodes:mnemonic(?CALLCODE)       ,  7,  1, 0};
opcode(?RETURN)         -> {aeb_opcodes:mnemonic(?RETURN)         ,  2,  0, ?GZERO};
opcode(?DELEGATECALL)   -> {aeb_opcodes:mnemonic(?DELEGATECALL)   ,  6,  1, 0};
opcode(?CALLBLACKBOX)   -> {aeb_opcodes:mnemonic(?CALLBLACKBOX)   ,  7,  1, 40}; %% TODO
opcode(?STATICCALL)     -> {aeb_opcodes:mnemonic(?STATICCALL)     ,  6,  1, 40}; %% TODO
opcode(?REVERT)         -> {aeb_opcodes:mnemonic(?REVERT)         ,  2,  0, 0};
opcode(?SUICIDE)        -> {aeb_opcodes:mnemonic(?SUICIDE)        ,  1,  0, ?GSELFDESTRUCT}. %% TODO
