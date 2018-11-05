%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Opcodes
%%% @end
%%% Created : 14 Dec 2017
%%%-------------------------------------------------------------------

-module(aevm_opcodes).

-export([ op_base_gas/2
        , op_name/2
        ]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
%%====================================================================
%% API
%%====================================================================

op_name(OP, State) -> element(1, opcode(aevm_eeevm_state:gastable(State), OP)).
%% op_pop(OP) -> element(2, opcode(OP)).
%% op_push(OP) -> element(3, opcode(OP)).
op_base_gas(OP, State) -> element(4, opcode(aevm_eeevm_state:gastable(State), OP)).

%% @doc Opcodes defined as {OPCODE, POPPED, PUSHED, BASE_GAS}

opcode(GasTable, ?STOP)           -> {aeb_opcodes:mnemonic(?STOP)           ,  0,  0, maps:get('GZERO', GasTable)};
opcode(GasTable, ?ADD)            -> {aeb_opcodes:mnemonic(?ADD)            ,  2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?MUL)            -> {aeb_opcodes:mnemonic(?MUL)            ,  2,  1, maps:get('GLOW', GasTable)};
opcode(GasTable, ?SUB)            -> {aeb_opcodes:mnemonic(?SUB)            ,  2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DIV)            -> {aeb_opcodes:mnemonic(?DIV)            ,  2,  1, maps:get('GLOW', GasTable)};
opcode(GasTable, ?SDIV)           -> {aeb_opcodes:mnemonic(?SDIV)           ,  2,  1, maps:get('GLOW', GasTable)};
opcode(GasTable, ?MOD)            -> {aeb_opcodes:mnemonic(?MOD)            ,  2,  1, maps:get('GLOW', GasTable)};
opcode(GasTable, ?SMOD)           -> {aeb_opcodes:mnemonic(?SMOD)           ,  2,  1, maps:get('GLOW', GasTable)};
opcode(GasTable, ?ADDMOD)         -> {aeb_opcodes:mnemonic(?ADDMOD)         ,  3,  1, maps:get('GMID', GasTable)};
opcode(GasTable, ?MULMOD)         -> {aeb_opcodes:mnemonic(?MULMOD)         ,  3,  1, maps:get('GMID', GasTable)};
opcode(GasTable, ?EXP)            -> {aeb_opcodes:mnemonic(?EXP)            ,  2,  1, maps:get('GEXP', GasTable)};
opcode(GasTable, ?SIGNEXTEND)     -> {aeb_opcodes:mnemonic(?SIGNEXTEND)     ,  2,  1, maps:get('GLOW', GasTable)};
opcode(GasTable, ?LT)             -> {aeb_opcodes:mnemonic(?LT)             ,  2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?GT)             -> {aeb_opcodes:mnemonic(?GT)             ,  2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SLT)            -> {aeb_opcodes:mnemonic(?SLT)            ,  2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SGT)            -> {aeb_opcodes:mnemonic(?SGT)            ,  2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?EQ)             -> {aeb_opcodes:mnemonic(?EQ)             ,  2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?ISZERO)         -> {aeb_opcodes:mnemonic(?ISZERO)         ,  1,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?AND)            -> {aeb_opcodes:mnemonic(?AND)            ,  2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?OR)             -> {aeb_opcodes:mnemonic(?OR)             ,  2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?XOR)            -> {aeb_opcodes:mnemonic(?XOR)            ,  2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?NOT)            -> {aeb_opcodes:mnemonic(?NOT)            ,  1,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?BYTE)           -> {aeb_opcodes:mnemonic(?BYTE)           ,  2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SHA3)           -> {aeb_opcodes:mnemonic(?SHA3)           ,  2,  1, maps:get('GSHA3', GasTable)};
opcode(GasTable, ?ADDRESS)        -> {aeb_opcodes:mnemonic(?ADDRESS)        ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?BALANCE)        -> {aeb_opcodes:mnemonic(?BALANCE)        ,  1,  1, maps:get('GBALANCE', GasTable)};
opcode(GasTable, ?ORIGIN)         -> {aeb_opcodes:mnemonic(?ORIGIN)         ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?CALLER)         -> {aeb_opcodes:mnemonic(?CALLER)         ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?CALLVALUE)      -> {aeb_opcodes:mnemonic(?CALLVALUE)      ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?CALLDATALOAD)   -> {aeb_opcodes:mnemonic(?CALLDATALOAD)   ,  1,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?CALLDATASIZE)   -> {aeb_opcodes:mnemonic(?CALLDATASIZE)   ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?CALLDATACOPY)   -> {aeb_opcodes:mnemonic(?CALLDATACOPY)   ,  3,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?CODESIZE)       -> {aeb_opcodes:mnemonic(?CODESIZE)       ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?CODECOPY)       -> {aeb_opcodes:mnemonic(?CODECOPY)       ,  3,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?GASPRICE)       -> {aeb_opcodes:mnemonic(?GASPRICE)       ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?EXTCODESIZE)    -> {aeb_opcodes:mnemonic(?EXTCODESIZE)    ,  1,  1, maps:get('GEXTCODESIZE', GasTable)};
opcode(GasTable, ?EXTCODECOPY)    -> {aeb_opcodes:mnemonic(?EXTCODECOPY)    ,  4,  0, maps:get('GEXTCODECOPY', GasTable)};
opcode(GasTable, ?RETURNDATASIZE) -> {aeb_opcodes:mnemonic(?RETURNDATASIZE) ,  0,  1, 2}; %% TODO
opcode(GasTable, ?RETURNDATACOPY) -> {aeb_opcodes:mnemonic(?RETURNDATACOPY) ,  3,  0, 3}; %% TODO
opcode(GasTable, ?BLOCKHASH)      -> {aeb_opcodes:mnemonic(?BLOCKHASH)      ,  1,  1, maps:get('GBLOCKHASH', GasTable)};
opcode(GasTable, ?COINBASE)       -> {aeb_opcodes:mnemonic(?COINBASE)       ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?TIMESTAMP)      -> {aeb_opcodes:mnemonic(?TIMESTAMP)      ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?NUMBER)         -> {aeb_opcodes:mnemonic(?NUMBER)         ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?DIFFICULTY)     -> {aeb_opcodes:mnemonic(?DIFFICULTY)     ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?GASLIMIT)       -> {aeb_opcodes:mnemonic(?GASLIMIT)       ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?POP)            -> {aeb_opcodes:mnemonic(?POP)            ,  1,  0, maps:get('GBASE', GasTable)};
opcode(GasTable, ?MLOAD)          -> {aeb_opcodes:mnemonic(?MLOAD)          ,  1,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?MSTORE)         -> {aeb_opcodes:mnemonic(?MSTORE)         ,  2,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?MSTORE8)        -> {aeb_opcodes:mnemonic(?MSTORE8)        ,  2,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SLOAD)          -> {aeb_opcodes:mnemonic(?SLOAD)          ,  1,  1, maps:get('GSLOAD', GasTable)};
opcode(GasTable, ?SSTORE)         -> {aeb_opcodes:mnemonic(?SSTORE)         ,  2,  0, 0};
opcode(GasTable, ?JUMP)           -> {aeb_opcodes:mnemonic(?JUMP)           ,  1,  0, maps:get('GMID', GasTable)};
opcode(GasTable, ?JUMPI)          -> {aeb_opcodes:mnemonic(?JUMPI)          ,  2,  0, maps:get('GHIGH', GasTable)};
opcode(GasTable, ?PC)             -> {aeb_opcodes:mnemonic(?PC)             ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?MSIZE)          -> {aeb_opcodes:mnemonic(?MSIZE)          ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?GAS)            -> {aeb_opcodes:mnemonic(?GAS)            ,  0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?JUMPDEST)       -> {aeb_opcodes:mnemonic(?JUMPDEST)       ,  0,  0, maps:get('GJUMPDEST', GasTable)};
opcode(GasTable, ?PUSH1)          -> {aeb_opcodes:mnemonic(?PUSH1)          ,  1,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH2)          -> {aeb_opcodes:mnemonic(?PUSH2)          ,  2,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH3)          -> {aeb_opcodes:mnemonic(?PUSH3)          ,  3,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH4)          -> {aeb_opcodes:mnemonic(?PUSH4)          ,  4,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH5)          -> {aeb_opcodes:mnemonic(?PUSH5)          ,  5,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH6)          -> {aeb_opcodes:mnemonic(?PUSH6)          ,  6,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH7)          -> {aeb_opcodes:mnemonic(?PUSH7)          ,  7,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH8)          -> {aeb_opcodes:mnemonic(?PUSH8)          ,  8,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH9)          -> {aeb_opcodes:mnemonic(?PUSH9)          ,  9,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH10)         -> {aeb_opcodes:mnemonic(?PUSH10)         , 10,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH11)         -> {aeb_opcodes:mnemonic(?PUSH11)         , 11,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH12)         -> {aeb_opcodes:mnemonic(?PUSH12)         , 12,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH13)         -> {aeb_opcodes:mnemonic(?PUSH13)         , 13,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH14)         -> {aeb_opcodes:mnemonic(?PUSH14)         , 14,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH15)         -> {aeb_opcodes:mnemonic(?PUSH15)         , 15,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH16)         -> {aeb_opcodes:mnemonic(?PUSH16)         , 16,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH17)         -> {aeb_opcodes:mnemonic(?PUSH17)         , 17,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH18)         -> {aeb_opcodes:mnemonic(?PUSH18)         , 18,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH19)         -> {aeb_opcodes:mnemonic(?PUSH19)         , 19,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH20)         -> {aeb_opcodes:mnemonic(?PUSH20)         , 20,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH21)         -> {aeb_opcodes:mnemonic(?PUSH21)         , 21,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH22)         -> {aeb_opcodes:mnemonic(?PUSH22)         , 22,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH23)         -> {aeb_opcodes:mnemonic(?PUSH23)         , 23,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH24)         -> {aeb_opcodes:mnemonic(?PUSH24)         , 24,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH25)         -> {aeb_opcodes:mnemonic(?PUSH25)         , 25,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH26)         -> {aeb_opcodes:mnemonic(?PUSH26)         , 26,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH27)         -> {aeb_opcodes:mnemonic(?PUSH27)         , 27,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH28)         -> {aeb_opcodes:mnemonic(?PUSH28)         , 28,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH29)         -> {aeb_opcodes:mnemonic(?PUSH29)         , 29,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH30)         -> {aeb_opcodes:mnemonic(?PUSH30)         , 30,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH31)         -> {aeb_opcodes:mnemonic(?PUSH31)         , 31,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH32)         -> {aeb_opcodes:mnemonic(?PUSH32)         , 32,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP1)           -> {aeb_opcodes:mnemonic(?DUP1)           ,  0,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP2)           -> {aeb_opcodes:mnemonic(?DUP2)           ,  0,  2, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP3)           -> {aeb_opcodes:mnemonic(?DUP3)           ,  0,  3, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP4)           -> {aeb_opcodes:mnemonic(?DUP4)           ,  0,  4, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP5)           -> {aeb_opcodes:mnemonic(?DUP5)           ,  0,  5, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP6)           -> {aeb_opcodes:mnemonic(?DUP6)           ,  0,  6, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP7)           -> {aeb_opcodes:mnemonic(?DUP7)           ,  0,  7, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP8)           -> {aeb_opcodes:mnemonic(?DUP8)           ,  0,  8, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP9)           -> {aeb_opcodes:mnemonic(?DUP9)           ,  0,  9, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP10)          -> {aeb_opcodes:mnemonic(?DUP10)          ,  0, 10, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP11)          -> {aeb_opcodes:mnemonic(?DUP11)          ,  0, 11, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP12)          -> {aeb_opcodes:mnemonic(?DUP12)          ,  0, 12, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP13)          -> {aeb_opcodes:mnemonic(?DUP13)          ,  0, 13, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP14)          -> {aeb_opcodes:mnemonic(?DUP14)          ,  0, 14, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP15)          -> {aeb_opcodes:mnemonic(?DUP15)          ,  0, 15, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP16)          -> {aeb_opcodes:mnemonic(?DUP16)          ,  0, 16, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP1)          -> {aeb_opcodes:mnemonic(?SWAP1)          ,  0,  2, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP2)          -> {aeb_opcodes:mnemonic(?SWAP2)          ,  0,  3, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP3)          -> {aeb_opcodes:mnemonic(?SWAP3)          ,  0,  4, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP4)          -> {aeb_opcodes:mnemonic(?SWAP4)          ,  0,  5, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP5)          -> {aeb_opcodes:mnemonic(?SWAP5)          ,  0,  6, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP6)          -> {aeb_opcodes:mnemonic(?SWAP6)          ,  0,  7, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP7)          -> {aeb_opcodes:mnemonic(?SWAP7)          ,  0,  8, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP8)          -> {aeb_opcodes:mnemonic(?SWAP8)          ,  0,  9, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP9)          -> {aeb_opcodes:mnemonic(?SWAP9)          ,  0, 10, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP10)         -> {aeb_opcodes:mnemonic(?SWAP10)         ,  0, 11, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP11)         -> {aeb_opcodes:mnemonic(?SWAP11)         ,  0, 12, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP12)         -> {aeb_opcodes:mnemonic(?SWAP12)         ,  0, 13, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP13)         -> {aeb_opcodes:mnemonic(?SWAP13)         ,  0, 14, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP14)         -> {aeb_opcodes:mnemonic(?SWAP14)         ,  0, 15, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP15)         -> {aeb_opcodes:mnemonic(?SWAP15)         ,  0, 16, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP16)         -> {aeb_opcodes:mnemonic(?SWAP16)         ,  0, 17, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?LOG0)           -> {aeb_opcodes:mnemonic(?LOG0)           ,  2,  0, maps:get('GLOG', GasTable)};
opcode(GasTable, ?LOG1)           -> {aeb_opcodes:mnemonic(?LOG1)           ,  3,  0, maps:get('GLOG', GasTable) + maps:get('GLOGTOPIC', GasTable)};
opcode(GasTable, ?LOG2)           -> {aeb_opcodes:mnemonic(?LOG2)           ,  4,  0, maps:get('GLOG', GasTable) + 2*maps:get('GLOGTOPIC', GasTable)};
opcode(GasTable, ?LOG3)           -> {aeb_opcodes:mnemonic(?LOG3)           ,  5,  0, maps:get('GLOG', GasTable) + 3*maps:get('GLOGTOPIC', GasTable)};
opcode(GasTable, ?LOG4)           -> {aeb_opcodes:mnemonic(?LOG4)           ,  6,  0, maps:get('GLOG', GasTable) + 4*maps:get('GLOGTOPIC', GasTable)};
opcode(GasTable, ?CREATE)         -> {aeb_opcodes:mnemonic(?CREATE)         ,  3,  1, maps:get('GCREATE', GasTable)};
opcode(GasTable, ?CALL)           -> {aeb_opcodes:mnemonic(?CALL)           ,  7,  1, 0};
opcode(GasTable, ?CALLCODE)       -> {aeb_opcodes:mnemonic(?CALLCODE)       ,  7,  1, 0};
opcode(GasTable, ?RETURN)         -> {aeb_opcodes:mnemonic(?RETURN)         ,  2,  0, maps:get('GZERO', GasTable)};
opcode(GasTable, ?DELEGATECALL)   -> {aeb_opcodes:mnemonic(?DELEGATECALL)   ,  6,  1, 0};
opcode(GasTable, ?CALLBLACKBOX)   -> {aeb_opcodes:mnemonic(?CALLBLACKBOX)   ,  7,  1, 40}; %% TODO
opcode(GasTable, ?STATICCALL)     -> {aeb_opcodes:mnemonic(?STATICCALL)     ,  6,  1, 40}; %% TODO
opcode(GasTable, ?REVERT)         -> {aeb_opcodes:mnemonic(?REVERT)         ,  2,  0, 0};
opcode(GasTable, ?SUICIDE)        -> {aeb_opcodes:mnemonic(?SUICIDE)        ,  1,  0, maps:get('GSELFDESTRUCT', GasTable)}. %% TODO
