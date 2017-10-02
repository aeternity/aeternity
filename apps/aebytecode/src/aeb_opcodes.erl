%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Opcodes
%%% @end
%%% Created : 2 Oct 2017
%%%-------------------------------------------------------------------

-module(aeb_opcodes).

-export([ opcode/1
        , mnemonic/1
        ]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").


%%====================================================================
%% API
%%====================================================================

opcode(?STOP)           -> ?STOP;
opcode(?ADD)            -> ?ADD;
opcode(?MUL)            -> ?MUL;
opcode(?SUB)            -> ?SUB;
opcode(?DIV)            -> ?DIV;
opcode(?SDIV)           -> ?SDIV;
opcode(?MOD)            -> ?MOD;
opcode(?SMOD)           -> ?SMOD;
opcode(?ADDMOD)         -> ?ADDMOD;
opcode(?MULMOD)         -> ?MULMOD;
opcode(?EXP)            -> ?EXP;
opcode(?SIGNEXTEND)     -> ?SIGNEXTEND;
opcode(?LT)             -> ?LT;
opcode(?GT)             -> ?GT;
opcode(?SLT)            -> ?SLT;
opcode(?SGT)            -> ?SGT;
opcode(?EQ)             -> ?EQ;
opcode(?ISZERO)         -> ?ISZERO;
opcode(?AND)            -> ?AND;
opcode(?OR)             -> ?OR;
opcode(?XOR)            -> ?XOR;
opcode(?NOT)            -> ?NOT;
opcode(?BYTE)           -> ?BYTE;
opcode(?SHA3)           -> ?SHA3;
opcode(?ADDRESS)        -> ?ADDRESS;
opcode(?BALANCE)        -> ?BALANCE;
opcode(?ORIGIN)         -> ?ORIGIN;
opcode(?CALLER)         -> ?CALLER;
opcode(?CALLVALUE)      -> ?CALLVALUE;
opcode(?CALLDATALOAD)   -> ?CALLDATALOAD;
opcode(?CALLDATASIZE)   -> ?CALLDATASIZE;
opcode(?CALLDATACOPY)   -> ?CALLDATACOPY;
opcode(?CODESIZE)       -> ?CODESIZE;
opcode(?CODECOPY)       -> ?CODECOPY;
opcode(?GASPRICE)       -> ?GASPRICE;
opcode(?EXTCODESIZE)    -> ?EXTCODESIZE;
opcode(?EXTCODECOPY)    -> ?EXTCODECOPY;
opcode(?RETURNDATASIZE) -> ?RETURNDATASIZE; %% TODO
opcode(?RETURNDATACOPY) -> ?RETURNDATACOPY; %% TODO
opcode(?BLOCKHASH)      -> ?BLOCKHASH;
opcode(?COINBASE)       -> ?COINBASE;
opcode(?TIMESTAMP)      -> ?TIMESTAMP;
opcode(?NUMBER)         -> ?NUMBER;
opcode(?DIFFICULTY)     -> ?DIFFICULTY;
opcode(?GASLIMIT)       -> ?GASLIMIT;
opcode(?POP)            -> ?POP;
opcode(?MLOAD)          -> ?MLOAD;
opcode(?MSTORE)         -> ?MSTORE;
opcode(?MSTORE8)        -> ?MSTORE8;
opcode(?SLOAD)          -> ?SLOAD;
opcode(?SSTORE)         -> ?SSTORE;
opcode(?JUMP)           -> ?JUMP;
opcode(?JUMPI)          -> ?JUMPI;
opcode(?PC)             -> ?PC;
opcode(?MSIZE)          -> ?MSIZE;
opcode(?GAS)            -> ?GAS;
opcode(?JUMPDEST)       -> ?JUMPDEST;
opcode(?PUSH1)          -> ?PUSH1;
opcode(?PUSH2)          -> ?PUSH2;
opcode(?PUSH3)          -> ?PUSH3;
opcode(?PUSH4)          -> ?PUSH4;
opcode(?PUSH5)          -> ?PUSH5;
opcode(?PUSH6)          -> ?PUSH6;
opcode(?PUSH7)          -> ?PUSH7;
opcode(?PUSH8)          -> ?PUSH8;
opcode(?PUSH9)          -> ?PUSH9;
opcode(?PUSH10)         -> ?PUSH10;
opcode(?PUSH11)         -> ?PUSH11;
opcode(?PUSH12)         -> ?PUSH12;
opcode(?PUSH13)         -> ?PUSH13;
opcode(?PUSH14)         -> ?PUSH14;
opcode(?PUSH15)         -> ?PUSH15;
opcode(?PUSH16)         -> ?PUSH16;
opcode(?PUSH17)         -> ?PUSH17;
opcode(?PUSH18)         -> ?PUSH18;
opcode(?PUSH19)         -> ?PUSH19;
opcode(?PUSH20)         -> ?PUSH20;
opcode(?PUSH21)         -> ?PUSH21;
opcode(?PUSH22)         -> ?PUSH22;
opcode(?PUSH23)         -> ?PUSH23;
opcode(?PUSH24)         -> ?PUSH24;
opcode(?PUSH25)         -> ?PUSH25;
opcode(?PUSH26)         -> ?PUSH26;
opcode(?PUSH27)         -> ?PUSH27;
opcode(?PUSH28)         -> ?PUSH28;
opcode(?PUSH29)         -> ?PUSH29;
opcode(?PUSH30)         -> ?PUSH30;
opcode(?PUSH31)         -> ?PUSH31;
opcode(?PUSH32)         -> ?PUSH32;
opcode(?DUP1)           -> ?DUP1;
opcode(?DUP2)           -> ?DUP2;
opcode(?DUP3)           -> ?DUP3;
opcode(?DUP4)           -> ?DUP4;
opcode(?DUP5)           -> ?DUP5;
opcode(?DUP6)           -> ?DUP6;
opcode(?DUP7)           -> ?DUP7;
opcode(?DUP8)           -> ?DUP8;
opcode(?DUP9)           -> ?DUP9;
opcode(?DUP10)          -> ?DUP10;
opcode(?DUP11)          -> ?DUP11;
opcode(?DUP12)          -> ?DUP12;
opcode(?DUP13)          -> ?DUP13;
opcode(?DUP14)          -> ?DUP14;
opcode(?DUP15)          -> ?DUP15;
opcode(?DUP16)          -> ?DUP16;
opcode(?SWAP1)          -> ?SWAP1;
opcode(?SWAP2)          -> ?SWAP2;
opcode(?SWAP3)          -> ?SWAP3;
opcode(?SWAP4)          -> ?SWAP4;
opcode(?SWAP5)          -> ?SWAP5;
opcode(?SWAP6)          -> ?SWAP6;
opcode(?SWAP7)          -> ?SWAP7;
opcode(?SWAP8)          -> ?SWAP8;
opcode(?SWAP9)          -> ?SWAP9;
opcode(?SWAP10)         -> ?SWAP10;
opcode(?SWAP11)         -> ?SWAP11;
opcode(?SWAP12)         -> ?SWAP12;
opcode(?SWAP13)         -> ?SWAP13;
opcode(?SWAP14)         -> ?SWAP14;
opcode(?SWAP15)         -> ?SWAP15;
opcode(?SWAP16)         -> ?SWAP16;
opcode(?LOG0)           -> ?LOG0;
opcode(?LOG1)           -> ?LOG1;
opcode(?LOG2)           -> ?LOG2;
opcode(?LOG3)           -> ?LOG3;
opcode(?LOG4)           -> ?LOG4;
opcode(?CREATE)         -> ?CREATE;
opcode(?CALL)           -> ?CALL;
opcode(?CALLCODE)       -> ?CALLCODE;
opcode(?RETURN)         -> ?RETURN;
opcode(?DELEGATECALL)   -> ?DELEGATECALL;
opcode(?CALLBLACKBOX)   -> ?CALLBLACKBOX; %% TODO
opcode(?STATICCALL)     -> ?STATICCALL; %% TODO
opcode(?REVERT)         -> ?REVERT;
opcode(?SUICIDE)        -> ?SUICIDE.


mnemonic(?STOP)           -> 'STOP'           ;
mnemonic(?ADD)            -> 'ADD'            ;
mnemonic(?MUL)            -> 'MUL'            ;
mnemonic(?SUB)            -> 'SUB'            ;
mnemonic(?DIV)            -> 'DIV'            ;
mnemonic(?SDIV)           -> 'SDIV'           ;
mnemonic(?MOD)            -> 'MOD'            ;
mnemonic(?SMOD)           -> 'SMOD'           ;
mnemonic(?ADDMOD)         -> 'ADDMOD'         ;
mnemonic(?MULMOD)         -> 'MULMOD'         ;
mnemonic(?EXP)            -> 'EXP'            ;
mnemonic(?SIGNEXTEND)     -> 'SIGNEXTEND'     ;
mnemonic(?LT)             -> 'LT'             ;
mnemonic(?GT)             -> 'GT'             ;
mnemonic(?SLT)            -> 'SLT'            ;
mnemonic(?SGT)            -> 'SGT'            ;
mnemonic(?EQ)             -> 'EQ'             ;
mnemonic(?ISZERO)         -> 'ISZERO'         ;
mnemonic(?AND)            -> 'AND'            ;
mnemonic(?OR)             -> 'OR'             ;
mnemonic(?XOR)            -> 'XOR'            ;
mnemonic(?NOT)            -> 'NOT'            ;
mnemonic(?BYTE)           -> 'BYTE'           ;
mnemonic(?SHA3)           -> 'SHA3'           ;
mnemonic(?ADDRESS)        -> 'ADDRESS'        ;
mnemonic(?BALANCE)        -> 'BALANCE'        ;
mnemonic(?ORIGIN)         -> 'ORIGIN'         ;
mnemonic(?CALLER)         -> 'CALLER'         ;
mnemonic(?CALLVALUE)      -> 'CALLVALUE'      ;
mnemonic(?CALLDATALOAD)   -> 'CALLDATALOAD'   ;
mnemonic(?CALLDATASIZE)   -> 'CALLDATASIZE'   ;
mnemonic(?CALLDATACOPY)   -> 'CALLDATACOPY'   ;
mnemonic(?CODESIZE)       -> 'CODESIZE'       ;
mnemonic(?CODECOPY)       -> 'CODECOPY'       ;
mnemonic(?GASPRICE)       -> 'GASPRICE'       ;
mnemonic(?EXTCODESIZE)    -> 'EXTCODESIZE'    ;
mnemonic(?EXTCODECOPY)    -> 'EXTCODECOPY'    ;
mnemonic(?RETURNDATASIZE) -> 'RETURNDATASIZE' ;
mnemonic(?RETURNDATACOPY) -> 'RETURNDATACOPY' ;
mnemonic(?BLOCKHASH)      -> 'BLOCKHASH'      ;
mnemonic(?COINBASE)       -> 'COINBASE'       ;
mnemonic(?TIMESTAMP)      -> 'TIMESTAMP'      ;
mnemonic(?NUMBER)         -> 'NUMBER'         ;
mnemonic(?DIFFICULTY)     -> 'DIFFICULTY'     ;
mnemonic(?GASLIMIT)       -> 'GASLIMIT'       ;
mnemonic(?POP)            -> 'POP'            ;
mnemonic(?MLOAD)          -> 'MLOAD'          ;
mnemonic(?MSTORE)         -> 'MSTORE'         ;
mnemonic(?MSTORE8)        -> 'MSTORE8'        ;
mnemonic(?SLOAD)          -> 'SLOAD'          ;
mnemonic(?SSTORE)         -> 'SSTORE'         ;
mnemonic(?JUMP)           -> 'JUMP'           ;
mnemonic(?JUMPI)          -> 'JUMPI'          ;
mnemonic(?PC)             -> 'PC'             ;
mnemonic(?MSIZE)          -> 'MSIZE'          ;
mnemonic(?GAS)            -> 'GAS'            ;
mnemonic(?JUMPDEST)       -> 'JUMPDEST'       ;
mnemonic(?PUSH1)          -> 'PUSH1'          ;
mnemonic(?PUSH2)          -> 'PUSH2'          ;
mnemonic(?PUSH3)          -> 'PUSH3'          ;
mnemonic(?PUSH4)          -> 'PUSH4'          ;
mnemonic(?PUSH5)          -> 'PUSH5'          ;
mnemonic(?PUSH6)          -> 'PUSH6'          ;
mnemonic(?PUSH7)          -> 'PUSH7'          ;
mnemonic(?PUSH8)          -> 'PUSH8'          ;
mnemonic(?PUSH9)          -> 'PUSH9'          ;
mnemonic(?PUSH10)         -> 'PUSH10'         ;
mnemonic(?PUSH11)         -> 'PUSH11'         ;
mnemonic(?PUSH12)         -> 'PUSH12'         ;
mnemonic(?PUSH13)         -> 'PUSH13'         ;
mnemonic(?PUSH14)         -> 'PUSH14'         ;
mnemonic(?PUSH15)         -> 'PUSH15'         ;
mnemonic(?PUSH16)         -> 'PUSH16'         ;
mnemonic(?PUSH17)         -> 'PUSH17'         ;
mnemonic(?PUSH18)         -> 'PUSH18'         ;
mnemonic(?PUSH19)         -> 'PUSH19'         ;
mnemonic(?PUSH20)         -> 'PUSH20'         ;
mnemonic(?PUSH21)         -> 'PUSH21'         ;
mnemonic(?PUSH22)         -> 'PUSH22'         ;
mnemonic(?PUSH23)         -> 'PUSH23'         ;
mnemonic(?PUSH24)         -> 'PUSH24'         ;
mnemonic(?PUSH25)         -> 'PUSH25'         ;
mnemonic(?PUSH26)         -> 'PUSH26'         ;
mnemonic(?PUSH27)         -> 'PUSH27'         ;
mnemonic(?PUSH28)         -> 'PUSH28'         ;
mnemonic(?PUSH29)         -> 'PUSH29'         ;
mnemonic(?PUSH30)         -> 'PUSH30'         ;
mnemonic(?PUSH31)         -> 'PUSH31'         ;
mnemonic(?PUSH32)         -> 'PUSH32'         ;
mnemonic(?DUP1)           -> 'DUP1'           ;
mnemonic(?DUP2)           -> 'DUP2'           ;
mnemonic(?DUP3)           -> 'DUP3'           ;
mnemonic(?DUP4)           -> 'DUP4'           ;
mnemonic(?DUP5)           -> 'DUP5'           ;
mnemonic(?DUP6)           -> 'DUP6'           ;
mnemonic(?DUP7)           -> 'DUP7'           ;
mnemonic(?DUP8)           -> 'DUP8'           ;
mnemonic(?DUP9)           -> 'DUP9'           ;
mnemonic(?DUP10)          -> 'DUP10'          ;
mnemonic(?DUP11)          -> 'DUP11'          ;
mnemonic(?DUP12)          -> 'DUP12'          ;
mnemonic(?DUP13)          -> 'DUP13'          ;
mnemonic(?DUP14)          -> 'DUP14'          ;
mnemonic(?DUP15)          -> 'DUP15'          ;
mnemonic(?DUP16)          -> 'DUP16'          ;
mnemonic(?SWAP1)          -> 'SWAP1'          ;
mnemonic(?SWAP2)          -> 'SWAP2'          ;
mnemonic(?SWAP3)          -> 'SWAP3'          ;
mnemonic(?SWAP4)          -> 'SWAP4'          ;
mnemonic(?SWAP5)          -> 'SWAP5'          ;
mnemonic(?SWAP6)          -> 'SWAP6'          ;
mnemonic(?SWAP7)          -> 'SWAP7'          ;
mnemonic(?SWAP8)          -> 'SWAP8'          ;
mnemonic(?SWAP9)          -> 'SWAP9'          ;
mnemonic(?SWAP10)         -> 'SWAP10'         ;
mnemonic(?SWAP11)         -> 'SWAP11'         ;
mnemonic(?SWAP12)         -> 'SWAP12'         ;
mnemonic(?SWAP13)         -> 'SWAP13'         ;
mnemonic(?SWAP14)         -> 'SWAP14'         ;
mnemonic(?SWAP15)         -> 'SWAP15'         ;
mnemonic(?SWAP16)         -> 'SWAP16'         ;
mnemonic(?LOG0)           -> 'LOG0'           ;
mnemonic(?LOG1)           -> 'LOG1'           ;
mnemonic(?LOG2)           -> 'LOG2'           ;
mnemonic(?LOG3)           -> 'LOG3'           ;
mnemonic(?LOG4)           -> 'LOG4'           ;
mnemonic(?CREATE)         -> 'CREATE'         ;
mnemonic(?CALL)           -> 'CALL'           ;
mnemonic(?CALLCODE)       -> 'CALLCODE'       ;
mnemonic(?RETURN)         -> 'RETURN'         ;
mnemonic(?DELEGATECALL)   -> 'DELEGATECALL'   ;
mnemonic(?CALLBLACKBOX)   -> 'CALLBLACKBOX'   ;
mnemonic(?STATICCALL)     -> 'STATICCALL'     ;
mnemonic(?REVERT)         -> 'REVERT'         ;
mnemonic(?SUICIDE)        -> 'SUICIDE'        .
