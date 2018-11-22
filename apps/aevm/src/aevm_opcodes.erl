%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Opcodes
%%% @end
%%% Created : 14 Dec 2017
%%%-------------------------------------------------------------------

-module(aevm_opcodes).

-export([ op_base_gas/2
        , op_name/1
        ]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
%%====================================================================
%% API
%%====================================================================

op_name(OP) -> aeb_opcodes:mnemonic(OP).
%% op_pop(OP) -> element(1, opcode(OP)).
%% op_push(OP) -> element(2, opcode(OP)).
op_base_gas(OP, State) -> element(3, opcode(aevm_eeevm_state:gastable(State), OP)).

%% @doc Opcodes defined as {POPPED, PUSHED, BASE_GAS}

opcode(GasTable, ?STOP)           -> { 0,  0, maps:get('GZERO', GasTable)};
opcode(GasTable, ?ADD)            -> { 2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?MUL)            -> { 2,  1, maps:get('GLOW', GasTable)};
opcode(GasTable, ?SUB)            -> { 2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DIV)            -> { 2,  1, maps:get('GLOW', GasTable)};
opcode(GasTable, ?SDIV)           -> { 2,  1, maps:get('GLOW', GasTable)};
opcode(GasTable, ?MOD)            -> { 2,  1, maps:get('GLOW', GasTable)};
opcode(GasTable, ?SMOD)           -> { 2,  1, maps:get('GLOW', GasTable)};
opcode(GasTable, ?ADDMOD)         -> { 3,  1, maps:get('GMID', GasTable)};
opcode(GasTable, ?MULMOD)         -> { 3,  1, maps:get('GMID', GasTable)};
opcode(GasTable, ?EXP)            -> { 2,  1, maps:get('GEXP', GasTable)};
opcode(GasTable, ?SIGNEXTEND)     -> { 2,  1, maps:get('GLOW', GasTable)};
opcode(GasTable, ?LT)             -> { 2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?GT)             -> { 2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SLT)            -> { 2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SGT)            -> { 2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?EQ)             -> { 2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?ISZERO)         -> { 1,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?AND)            -> { 2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?OR)             -> { 2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?XOR)            -> { 2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?NOT)            -> { 1,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?BYTE)           -> { 2,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SHA3)           -> { 2,  1, maps:get('GSHA3', GasTable)};
opcode(GasTable, ?ADDRESS)        -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?BALANCE)        -> { 1,  1, maps:get('GBALANCE', GasTable)};
opcode(GasTable, ?ORIGIN)         -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?CALLER)         -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?CALLVALUE)      -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?CALLDATALOAD)   -> { 1,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?CALLDATASIZE)   -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?CALLDATACOPY)   -> { 3,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?CODESIZE)       -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?CODECOPY)       -> { 3,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?GASPRICE)       -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?EXTCODESIZE)    -> { 1,  1, maps:get('GEXTCODESIZE', GasTable)};
opcode(GasTable, ?EXTCODECOPY)    -> { 4,  0, maps:get('GEXTCODECOPY', GasTable)};
opcode(GasTable, ?RETURNDATASIZE) -> { 0,  1, 2}; %% TODO
opcode(GasTable, ?RETURNDATACOPY) -> { 3,  0, 3}; %% TODO
opcode(GasTable, ?BLOCKHASH)      -> { 1,  1, maps:get('GBLOCKHASH', GasTable)};
opcode(GasTable, ?COINBASE)       -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?TIMESTAMP)      -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?NUMBER)         -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?DIFFICULTY)     -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?GASLIMIT)       -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?POP)            -> { 1,  0, maps:get('GBASE', GasTable)};
opcode(GasTable, ?MLOAD)          -> { 1,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?MSTORE)         -> { 2,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?MSTORE8)        -> { 2,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SLOAD)          -> { 1,  1, maps:get('GSLOAD', GasTable)};
opcode(GasTable, ?SSTORE)         -> { 2,  0, 0};
opcode(GasTable, ?JUMP)           -> { 1,  0, maps:get('GMID', GasTable)};
opcode(GasTable, ?JUMPI)          -> { 2,  0, maps:get('GHIGH', GasTable)};
opcode(GasTable, ?PC)             -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?MSIZE)          -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?GAS)            -> { 0,  1, maps:get('GBASE', GasTable)};
opcode(GasTable, ?JUMPDEST)       -> { 0,  0, maps:get('GJUMPDEST', GasTable)};
opcode(GasTable, ?PUSH1)          -> { 1,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH2)          -> { 2,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH3)          -> { 3,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH4)          -> { 4,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH5)          -> { 5,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH6)          -> { 6,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH7)          -> { 7,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH8)          -> { 8,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH9)          -> { 9,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH10)         -> {10,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH11)         -> {11,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH12)         -> {12,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH13)         -> {13,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH14)         -> {14,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH15)         -> {15,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH16)         -> {16,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH17)         -> {17,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH18)         -> {18,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH19)         -> {19,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH20)         -> {20,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH21)         -> {21,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH22)         -> {22,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH23)         -> {23,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH24)         -> {24,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH25)         -> {25,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH26)         -> {26,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH27)         -> {27,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH28)         -> {28,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH29)         -> {29,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH30)         -> {30,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH31)         -> {31,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?PUSH32)         -> {32,  0, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP1)           -> { 0,  1, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP2)           -> { 0,  2, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP3)           -> { 0,  3, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP4)           -> { 0,  4, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP5)           -> { 0,  5, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP6)           -> { 0,  6, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP7)           -> { 0,  7, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP8)           -> { 0,  8, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP9)           -> { 0,  9, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP10)          -> { 0, 10, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP11)          -> { 0, 11, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP12)          -> { 0, 12, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP13)          -> { 0, 13, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP14)          -> { 0, 14, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP15)          -> { 0, 15, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?DUP16)          -> { 0, 16, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP1)          -> { 0,  2, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP2)          -> { 0,  3, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP3)          -> { 0,  4, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP4)          -> { 0,  5, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP5)          -> { 0,  6, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP6)          -> { 0,  7, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP7)          -> { 0,  8, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP8)          -> { 0,  9, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP9)          -> { 0, 10, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP10)         -> { 0, 11, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP11)         -> { 0, 12, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP12)         -> { 0, 13, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP13)         -> { 0, 14, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP14)         -> { 0, 15, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP15)         -> { 0, 16, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?SWAP16)         -> { 0, 17, maps:get('GVERYLOW', GasTable)};
opcode(GasTable, ?LOG0)           -> { 2,  0, maps:get('GLOG', GasTable)};
opcode(GasTable, ?LOG1)           -> { 3,  0, maps:get('GLOG', GasTable) + maps:get('GLOGTOPIC', GasTable)};
opcode(GasTable, ?LOG2)           -> { 4,  0, maps:get('GLOG', GasTable) + 2*maps:get('GLOGTOPIC', GasTable)};
opcode(GasTable, ?LOG3)           -> { 5,  0, maps:get('GLOG', GasTable) + 3*maps:get('GLOGTOPIC', GasTable)};
opcode(GasTable, ?LOG4)           -> { 6,  0, maps:get('GLOG', GasTable) + 4*maps:get('GLOGTOPIC', GasTable)};
opcode(GasTable, ?CREATE)         -> { 3,  1, maps:get('GCREATE', GasTable)};
opcode(GasTable, ?CALL)           -> { 7,  1, 0};
opcode(GasTable, ?CALLCODE)       -> { 7,  1, 0};
opcode(GasTable, ?RETURN)         -> { 2,  0, maps:get('GZERO', GasTable)};
opcode(GasTable, ?DELEGATECALL)   -> { 6,  1, 0};
opcode(GasTable, ?STATICCALL)     -> { 6,  1, 40}; %% TODO
opcode(GasTable, ?REVERT)         -> { 2,  0, 0};
opcode(GasTable, ?SUICIDE)        -> { 1,  0, maps:get('GSELFDESTRUCT', GasTable)}. %% TODO
