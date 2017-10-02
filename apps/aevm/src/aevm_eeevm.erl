%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Experimental Erlang Etherium Virtual Machine.
%%%     This Etherium Machine is a simple and easy to reason about
%%%     implementation of the Etherium VM written in Erlang to run
%%%     on top of BEAM.
%%%     It should pass the tests in https://github.com/ethereum/tests
%%%
%%%     This implementation is not striving to be efficient.
%%%     This implementation *is* striving to be correct.
%%%
%%%     EEEVM is the basis for building an efficent Aeternity VM (AEVM)
%%%     which is comaptible vith EVM.
%%% @end
%%% Created : 2 Oct 2017
%%%-------------------------------------------------------------------
-module(aevm_eeevm).
-export([run/1]).

%% Exports for tracing. TODO: move to aevm_eevm_code
-export([code_get_op/2]).

-include("aevm_eeevm.hrl").

run(Spec) ->
    State = aevm_eeevm_state:init(Spec),
    eval(State).

%% Main eval loop.
%%
%% 
eval(State) ->
    CP   = aevm_eeevm_state:cp(State),
    Code = aevm_eeevm_state:code(State),
    case CP >= byte_size(Code) of
	false ->
	    OP   = code_get_op(CP, Code),
	    State0 = aevm_eeevm_state:trace_format("~n", [], State),
	    case OP of
		?STOP -> State0;
		?ADD ->
		    {Arg1, State1} = pop(State0),
		    {Arg2, State2} = pop(State1),
		    Val = add(Arg1, Arg2),
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?ADDMOD ->
		    {Arg1, State1} = pop(State0),
		    {Arg2, State2} = pop(State1),
		    {Arg3, State3} = pop(State2),
		    Val = addmod(Arg1, Arg2, Arg3),
		    State4 = push(Val, State3),
		    next_instruction(OP, State4);
		?SSTORE ->
		    {Address, State1} = pop(State0),
		    {Value, State2} = pop(State1),
		    State3 = sstore(Address, Value, State2),
		    next_instruction(OP, State3);
		?PUSH1 ->
		    Arg = code_get_arg(CP+1, 1, Code),
		    State1 = push(Arg, State0),
		    State2 = inc_cp(1, State1),
		    next_instruction(OP, State2);
		?PUSH32 ->
		    Arg = code_get_arg(CP+1, 32, Code),
		    State1 = push(Arg, State0),
		    State2 = inc_cp(32, State1),
		    next_instruction(OP, State2);
		_ ->
		    error({opcode_not_implemented,
			   hd(io_lib:format("~2.16B",[OP]))})
	    end;
	true -> State
    end.

%% ------------------------------------------------------------------------
%% ARITHMETIC
%% ------------------------------------------------------------------------
add(Arg1, Arg2) -> (Arg1 + Arg2) band ?MASK256.

addmod(Arg1, Arg2, Arg3) -> mod((Arg1 + Arg2), Arg3) band ?MASK256.
mod(Arg1, Arg2) -> (Arg1 rem Arg2 + Arg2) rem Arg2.
%% ------------------------------------------------------------------------
%% STACK
%% ------------------------------------------------------------------------
push(Arg, State) ->
    Stack   = aevm_eeevm_state:stack(State),
    aevm_eeevm_state:set_stack([Arg|Stack], State).

pop(State) ->
    [Arg|Stack] = aevm_eeevm_state:stack(State),
    {Arg, aevm_eeevm_state:set_stack(Stack, State)}.

%% ------------------------------------------------------------------------
%% MEMORY
%% ------------------------------------------------------------------------

%% No alignment or size check. Don't use directly.
mem_write(Address, 0, Mem) -> maps:remove(Address, Mem);
mem_write(Address, Value, Mem) -> maps:put(Address, Value, Mem).
    
sstore(Address, Value, State) when is_integer(Value) ->
    case (Address band ?ALIGN256) of
	%% 256-bits-word aligned
	0 -> Mem = aevm_eeevm_state:mem(State),
	     %% Make sure value fits in 256 bits.
	     Value256 = Value band ?MASK256,
	     Mem1 = mem_write(Address, Value256, Mem),
	     aevm_eeevm_state:set_mem(Mem1, State);
	_ -> %% Unligned
	    error({unaligned_sstore_not_handled, Address, Value})
    end.


%% ------------------------------------------------------------------------
%% CODE
%% ------------------------------------------------------------------------
code_get_op(CP, Code) -> binary:at(Code, CP).

code_get_arg(CP, Size, Code) ->
    Pos = CP * 8,
    Length = Size*8,
    <<_:Pos, Arg:Length, _/binary>> = Code,
    Arg.

next_instruction(OP, State) ->
    eval(inc_cp(spend_gas(OP, State))).

inc_cp(State) ->
    CP = aevm_eeevm_state:cp(State),
    aevm_eeevm_state:set_cp(CP + 1, State).

inc_cp(Amount, State) ->
    CP = aevm_eeevm_state:cp(State),
    aevm_eeevm_state:set_cp(CP + Amount, State).

%% ------------------------------------------------------------------------
%% OPCODES
%% ------------------------------------------------------------------------

opcodes() ->
    #{ 16#00 => {'STOP', 0, 0, 0}
     , 16#01 => {'ADD', 2, 1, 3}
     , 16#02 => {'MUL', 2, 1, 5}
     , 16#03 => {'SUB', 2, 1, 3}
     , 16#04 => {'DIV', 2, 1, 5}
     , 16#05 => {'SDIV', 2, 1, 5}
     , 16#06 => {'MOD', 2, 1, 5}
     , 16#07 => {'SMOD', 2, 1, 5}
     , 16#08 => {'ADDMOD', 3, 1, 8}
     , 16#09 => {'MULMOD', 3, 1, 8}
     , 16#0a => {'EXP', 2, 1, 10}
     , 16#0b => {'SIGNEXTEND', 2, 1, 5}
     , 16#10 => {'LT', 2, 1, 3}
     , 16#11 => {'GT', 2, 1, 3}
     , 16#12 => {'SLT', 2, 1, 3}
     , 16#13 => {'SGT', 2, 1, 3}
     , 16#14 => {'EQ', 2, 1, 3}
     , 16#15 => {'ISZERO', 1, 1, 3}
     , 16#16 => {'AND', 2, 1, 3}
     , 16#17 => {'OR', 2, 1, 3}
     , 16#18 => {'XOR', 2, 1, 3}
     , 16#19 => {'NOT', 1, 1, 3}
     , 16#1a => {'BYTE', 2, 1, 3}
     , 16#20 => {'SHA3', 2, 1, 30}
     , 16#30 => {'ADDRESS', 0, 1, 2}
     , 16#31 => {'BALANCE', 1, 1, 20}
     , 16#32 => {'ORIGIN', 0, 1, 2}
     , 16#33 => {'CALLER', 0, 1, 2}
     , 16#34 => {'CALLVALUE', 0, 1, 2}
     , 16#35 => {'CALLDATALOAD', 1, 1, 3}
     , 16#36 => {'CALLDATASIZE', 0, 1, 2}
     , 16#37 => {'CALLDATACOPY', 3, 0, 3}
     , 16#38 => {'CODESIZE', 0, 1, 2}
     , 16#39 => {'CODECOPY', 3, 0, 3}
     , 16#3a => {'GASPRICE', 0, 1, 2}
     , 16#3b => {'EXTCODESIZE', 1, 1, 20}
     , 16#3c => {'EXTCODECOPY', 4, 0, 20}
     , 16#3d => {'RETURNDATASIZE', 0, 1, 2}
     , 16#3e => {'RETURNDATACOPY', 3, 0, 3}
     , 16#40 => {'BLOCKHASH', 1, 1, 20}
     , 16#41 => {'COINBASE', 0, 1, 2}
     , 16#42 => {'TIMESTAMP', 0, 1, 2}
     , 16#43 => {'NUMBER', 0, 1, 2}
     , 16#44 => {'DIFFICULTY', 0, 1, 2}
     , 16#45 => {'GASLIMIT', 0, 1, 2}
     , 16#50 => {'POP', 1, 0, 2}
     , 16#51 => {'MLOAD', 1, 1, 3}
     , 16#52 => {'MSTORE', 2, 0, 3}
     , 16#53 => {'MSTORE8', 2, 0, 3}
     , 16#54 => {'SLOAD', 1, 1, 50}
     , 16#55 => {'SSTORE', 2, 0, 0}
     , 16#56 => {'JUMP', 1, 0, 8}
     , 16#57 => {'JUMPI', 2, 0, 10}
     , 16#58 => {'PC', 0, 1, 2}
     , 16#59 => {'MSIZE', 0, 1, 2}
     , 16#5a => {'GAS', 0, 1, 2}
     , 16#5b => {'JUMPDEST', 0, 0, 1}
     , 16#60 => {'PUSH1',          1,     0,    1}
     , 16#61 => { 'PUSH2',          2,     0,    1}
     , 16#62 => { 'PUSH3',          3,     0,    1}
     , 16#63 => { 'PUSH4',          4,     0,    1}
     , 16#64 => { 'PUSH5',          5,     0,    1}
     , 16#65 => { 'PUSH6',          6,     0,    1}
     , 16#66 => { 'PUSH7',          7,     0,    1}
     , 16#67 => { 'PUSH8',          8,     0,    1}
     , 16#68 => { 'PUSH9',          9,     0,    1}
     , 16#69 => { 'PUSH10',        10,     0,    1}
     , 16#6a => { 'PUSH11',        11,     0,    1}
     , 16#6b => { 'PUSH12',        12,     0,    1}
     , 16#6c => { 'PUSH13',        13,     0,    1}
     , 16#6d => { 'PUSH14',        14,     0,    1}
     , 16#6e => { 'PUSH15',        15,     0,    1}
     , 16#6f => { 'PUSH16',        16,     0,    1}
     , 16#70 => { 'PUSH17',        17,     0,    1}
     , 16#71 => { 'PUSH18',        18,     0,    1}
     , 16#72 => { 'PUSH19',        19,     0,    1}
     , 16#73 => { 'PUSH20',        20,     0,    1}
     , 16#74 => { 'PUSH21',        21,     0,    1}
     , 16#75 => { 'PUSH22',        22,     0,    1}
     , 16#76 => { 'PUSH23',        23,     0,    1}
     , 16#77 => { 'PUSH24',        24,     0,    1}
     , 16#78 => { 'PUSH25',        25,     0,    1}
     , 16#79 => { 'PUSH26',        26,     0,    1}
     , 16#7a => { 'PUSH27',        27,     0,    1}
     , 16#7b => { 'PUSH28',        28,     0,    1}
     , 16#7c => { 'PUSH29',        29,     0,    1}
     , 16#7d => { 'PUSH30',        30,     0,    1}
     , 16#7e => { 'PUSH31',        31,     0,    1}
     , 16#7f => { 'PUSH32',        32,     0,    1}
     , 16#80 => { 'DUP1',           0,     1,    2}
     , 16#81 => { 'DUP2',           0,     2,    3}
     , 16#82 => { 'DUP3',           0,     3,    4}
     , 16#83 => { 'DUP4',           0,     4,    5}
     , 16#84 => { 'DUP5',           0,     5,    6}
     , 16#85 => { 'DUP6',           0,     6,    7}
     , 16#86 => { 'DUP7',           0,     7,    8}
     , 16#87 => { 'DUP8',           0,     8,    9}
     , 16#88 => { 'DUP9',           0,     9,   10}
     , 16#89 => { 'DUP10',          0,    10,   11}
     , 16#8a => { 'DUP11',          0,    11,   12}
     , 16#8b => { 'DUP12',          0,    12,   13}
     , 16#8c => { 'DUP13',          0,    13,   14}
     , 16#8d => { 'DUP14',          0,    14,   15}
     , 16#8e => { 'DUP15',          0,    15,   16}
     , 16#8f => { 'DUP16',          0,    16,   17}
     , 16#90 => { 'SWAP1',          0,     2,    2}
     , 16#91 => { 'SWAP2',          0,     3,    3}
     , 16#92 => { 'SWAP3',          0,     4,    4}
     , 16#93 => { 'SWAP4',          0,     5,    5}
     , 16#94 => { 'SWAP5',          0,     6,    6}
     , 16#95 => { 'SWAP6',          0,     7,    7}
     , 16#96 => { 'SWAP7',          0,     8,    8}
     , 16#97 => { 'SWAP8',          0,     9,    9}
     , 16#98 => { 'SWAP9',          0,    10,   10}
     , 16#99 => { 'SWAP10',         0,    11,   11}
     , 16#9a => { 'SWAP11',         0,    12,   12}
     , 16#9b => { 'SWAP12',         0,    13,   13}
     , 16#9c => { 'SWAP13',         0,    14,   14}
     , 16#9d => { 'SWAP14',         0,    15,   15}
     , 16#9e => { 'SWAP15',         0,    16,   16}
     , 16#9f => { 'SWAP16',         0,    17,   17}
     , 16#a0 => {'LOG0', 2, 0, 375}
     , 16#a1 => {'LOG1', 3, 0, 750}
     , 16#a2 => {'LOG2', 4, 0, 1125}
     , 16#a3 => {'LOG3', 5, 0, 1500}
     , 16#a4 => {'LOG4', 6, 0, 1875}
     , 16#f0 => {'CREATE', 3, 1, 32000}
     , 16#f1 => {'CALL', 7, 1, 40}
     , 16#f2 => {'CALLCODE', 7, 1, 40}
     , 16#f3 => {'RETURN', 2, 0, 0}
     , 16#f4 => {'DELEGATECALL', 6, 1, 40}
     , 16#f5 => {'CALLBLACKBOX', 7, 1, 40}
     , 16#fa => {'STATICCALL', 6, 1, 40}
     , 16#fd => {'REVERT', 2, 0, 0}
     , 16#ff => {'SUICIDE', 1, 0, 0}
     }.

%% ------------------------------------------------------------------------
%% GAS
%% ------------------------------------------------------------------------

op_cost(OpInfo) ->  element(4, OpInfo).

spend_gas(Op, State) ->
    Cost = op_cost(maps:get(Op, opcodes())),
    Gas  = aevm_eeevm_state:gas(State),
    case Gas >= Cost of
	true ->  aevm_eeevm_state:set_gas(Gas - Cost, State);
	false -> exit({out_of_gas, State})
    end.

