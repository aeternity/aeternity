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
-export([eval/1]).

%% Exports for tracing. TODO: move to aevm_eevm_code
-export([code_get_op/2]).

-include("aevm_eeevm.hrl").

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
		?STOP ->
		    %% 0x00 STOP
		    %% Halts execution.
		    State0;
		?ADD ->
		    %% 0x01 ADD δ=2 α=1
		    %% Addition operation.
		    %% µs'[0] ≡ µs[0] + µs[1]
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = add(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?MUL ->
		    %% 0x02 MUL δ=2 α=1
		    %% Multiplication operation.
		    %% µs'[0] ≡ µs[0] * µs[1]
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = mul(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?SUB ->
		    %% 0x03 SUB δ=2 α=1
		    %% Subtraction operation.
		    %% µ's[0] ≡ µs[0] − µs[1]
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = sub(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?DIV ->
		    %% 0x04 DIV δ=2 α=1
		    %% Integer division operation.
		    %% µ's[0] ≡ 0 if µs[1] = 0
		    %%          µs[0] / µs[1] otherwise
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = idiv(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?SDIV ->
		    %% 0x05 SDIV δ=2 α=1
		    %% Signed integer division operation. (truncated)
		    %% µ's[0] ≡ 0      if µs[1] = 0
		    %%          -2^256 if µs[0] = −2^255 ∧ µs[1] = −1
		    %%          sgn(µs[0] ÷ µs[1]) |µs[0] ÷ µs[1]| otherwise
		    %% Where all values are treated as two’s complement
		    %% signed 256-bit integers.
		    %% Note the overflow semantic when −2^255 is negated.
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = sdiv(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?MOD ->
		    %% 0x06 MOD δ=2 α=1
		    %% Modulo remainder operation.
		    %% µ's[0] ≡  0 if µs[1] = 0
		    %%           µs[0] mod µs[1] otherwise
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = mod(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?SMOD ->
		    %% 0x07 SMOD δ=2 α=1
		    %% Signed modulo remainder operation.
		    %% µ's[0] ≡ (0 if µs[1] = 0
		    %%           sgn(µs[0])(|µs[0]| mod |µs[1]|) otherwise
		    %% Where all values are treated as
		    %% two’s complement signed 256-bit integers.
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = smod(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?ADDMOD ->
		    %% 0x08 ADDMOD  δ=3 α=1
		    %% Modulo addition operation.
		    %% µs'[0] ≡ 0 if µs[2] = 0
		    %%          (µs[0] + µs[1]) mod µs[2] otherwise
		    %% All intermediate calculations of this operation
		    %% are not subject to the 2^256 modulo.
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    {Us2, State3} = pop(State2),
		    Val = addmod(Us0, Us1, Us2),
		    State4 = push(Val, State3),
		    next_instruction(OP, State4);
		?MULMOD ->
		    %% 0x09 MULMOD  δ=3 α=1
		    %% Modulo multiplication operation.
		    %% µ's[0] ≡ 0 if µs[2] = 0
		    %%          µs[0] × µs[1]) mod µs[2] otherwise
		    %% All intermediate calculations of this operation are
		    %% not subject to the 2^256 modulo
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    {Us2, State3} = pop(State2),
		    Val = mulmod(Us0, Us1, Us2),
		    State4 = push(Val, State3),
		    next_instruction(OP, State4);
		?EXP ->
		    %% 0x0a EXP δ=2 α=1
		    %% Exponential operation.
		    %% µ's[0] ≡ µs[0] ^ µs[1]
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = exp(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?SIGNEXTEND ->
		    %% 0x0b SIGNEXTEND δ=2 α=1
		    %% Extend length of two’s complement signed integer.
		    %% ∀i ∈ [0..255] : µ's[0]i ≡ µs[1]t if i =< t
		    %%                           where t = 256 − 8*(µs[0] + 1)
		    %%                           µs[1]i otherwise
		    %% µs[x]i gives the ith bit (counting from zero) of µs[x]

		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = signextend(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);

		?LT ->
		    %% 0x10 LT δ=2 α=1
		    %% Less-than comparison.
		    %% µ's[0] ≡ 1 if µs[0] < µs[1]
		    %%          0 otherwise
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = if (Us0 < Us1) -> 1;
			     true -> 0
			  end,
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?GT ->
		    %% 0x11 GT δ=2 α=1
		    %% Greater-than comparison.
		    %% µ's[0] ≡ 1 if µs[0] > µs[1]
		    %%          0 otherwise
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = if (Us0 > Us1) -> 1;
			     true -> 0
			  end,
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?SLT ->
		    %% 0x12 SLT δ=2 α=1
		    %% Signed less-than comparison.
		    %% µ's[0] ≡ 1 if µs[0] < µs[1]
		    %%          0 otherwise
		    %% Where all values are treated as
		    %% two’s complement signed 256-bit integers.
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    SUs0 = signed(Us0),
		    SUs1 = signed(Us1),
		    Val = if (SUs0 < SUs1) -> 1;
			     true -> 0
			  end,
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?SGT ->
		    %% 0x13 SGT δ=2 α=1
		    %% Signed greater-than comparison.
		    %% µ's[0] ≡ 1 if µs[0] > µs[1]
		    %%          0 otherwise
		    %% Where all values are treated as
		    %% two’s complement signed 256-bit integers.
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    SUs0 = signed(Us0),
		    SUs1 = signed(Us1),
		    Val = if (SUs0 > SUs1) -> 1;
			     true -> 0
			  end,
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);


		?EQ ->
		    %% 0x14 EQ δ=2 α=1
		    %% Equality comparison.
		    %% µ's[0] ≡ 1 if µs[0] = µs[1]
		    %%          0 otherwise
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = if (Us0 == Us1) -> 1;
			     true -> 0
			  end,
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?ISZERO ->
		    %% 0x15 ISZERO δ=1 α=1
		    %% Simple not operator.
		    %% µ's[0] ≡ 1 if µs[0] = 0
		    %%          0 otherwise
		    {Us0, State1} = pop(State0),
		    Val = if Us0 =:= 0 -> 1; true -> 0 end,
		    State2 = push(Val, State1),
		    next_instruction(OP, State2);
		?AND ->
		    %% 0x16 AND δ=2 α=1
		    %% Bitwise AND operation.
		    %% ∀i ∈ [0..255] : µ's[0]i ≡ µs[0]i ∧ µs[1]i
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = Us0 band Us1,
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?OR ->
		    %% 0x17 OR δ=2 α=1
		    %% Bitwise OR operation.
		    %% ∀i ∈ [0..255] : µ's[0]i ≡ µs[0]i ∨ µs[1]i
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = Us0 bor Us1,
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?XOR ->
		    %% 0x18 XOR δ=2 α=1
		    %% Bitwise XOR operation.
		    %% ∀i ∈ [0..255] : µ's[0]i ≡ µs[0]i ∨ µs[1]i
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = Us0 bxor Us1,
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?NOT ->
		    %% 0x19 NOT δ=1 α=1
		    %% Bitwise NOT operation.
		    %% ∀i ∈ [0..255] : µ's[0]i ≡ 1 if µs[0]i = 0
		    %%                           0 otherwise
		    {Us0, State1} = pop(State0),
		    Val = (bnot Us0) band ?MASK256,
		    State2 = push(Val, State1),
		    next_instruction(OP, State2);
		?SHA3 ->
		    %% 0x20 SHA3  δ=2 α=1 Compute Keccak-256 hash.
		    %% µ's[0] ≡ Keccak(µm[µs[0] . . .(µs[0] + µs[1] − 1)])
		    %% µi ≡ M(µi, µs[0], µs[1])
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Arg = get_mem_area(Us0, Us0+Us1-1, State2),
		    Hash = sha3:hash(256, Arg),
		    <<Val:256/integer-unsigned>> = Hash,
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?BYTE ->
		    %% 0x1a BYTE δ=2 α=1
		    %% Retrieve single byte from word.
		    %% ∀i ∈ [0..255] : µ's[0]i ≡ (µs[1](i+8µs[0])
                    %%                              if i < 8 ∧ µs[0] < 32
		    %%                           0  otherwise
		    %% For Nth byte, we count from the left
		    %% (i.e. N=0 would be the most significant
		    %% in big endian)
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = byte(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);

		?CALLER ->
		    %% 0x33 CALLER δ=0 α=1
		    %% Get caller address.
		    %% µ's[0] ≡ Is
		    %% This is the address of the account
		    %% that is directly responsible for this execution.
		    Arg = aevm_eeevm_state:caller(State0),
		    State1 = push(Arg, State0),
		    next_instruction(OP, State1);


		?CALLDATALOAD ->
		    %% 0x35 CALLDATALOAD δ=1 α=1
		    %% Get input data of current environment.
		    %% µ's[0] ≡ Id[µs[0] . . .(µs[0] + 31)] with Id[x] = 0 if x >= |Id|
		    %% This pertains to the input data passed with the message
		    %% call instruction or transaction.
		    Bytes = 32,
		    {Us0, State1} = pop(State0),
		    Arg = data_get_val(Us0, Bytes, State1),
		    State2 = push(Arg, State1),
		    next_instruction(OP, State2);

		?MLOAD ->
		    %% 0x51 MLOAD δ=1 α=1
		    %% Load word from memory.
		    %% µ's[0] ≡ µm[µs[0] . . .(µs[0] + 31)]
		    %% µ'i ≡ max(µi, [(µs[0] + 32) ÷ 32])
		    %% The addition in the calculation of µ'i
		    %% is not subject to the 2^256 modulo.
		    {Us0, State1} = pop(State0),
		    Val = mload(Us0, State1),
		    State2 = push(Val, State1),
		    next_instruction(OP, State2);
		?MSTORE ->
		    %% 0x52 MSTORE δ=2 α=0
		    %% Save word to memory.
		    %% µ'm[µs[0] . . .(µs[0] + 31)] ≡ µs[1]
		    %% µ'i ≡ max(µi, [(µs[0] + 32) ÷ 32])
		    %% The addition in the calculation of µ'i
		    %% is not subject to the 2^256 modulo.
		    {Address, State1} = pop(State0),
		    {Value, State2} = pop(State1),
		    State3 = mstore(Address, Value, State2),
		    next_instruction(OP, State3);
		?MSTORE8 ->
		    %% 0x53 MSTORE8 δ=2 α=0
		    %% Save byte to memory.
		    %% µ'm[µs[0]] ≡ µs[1] mod 256
		    %% µ'i ≡ max(µi, [(µs[0] + 32) ÷ 32])
		    %% The addition in the calculation of µ'i
		    %% is not subject to the 2^256 modulo.
		    {Address, State1} = pop(State0),
		    {Value, State2} = pop(State1),
		    State3 = mstore8(Address, Value, State2),
		    next_instruction(OP, State3);
		?SLOAD ->
		    %% 0x54 SLOAD δ=1 α=1
		    %% Load word from storage.
		    %% µ's[0] ≡ σ[Ia]s[µs[0]]
		    {Us0, State1} = pop(State0),
		    Val = sload(Us0, State1),
		    State2 = push(Val, State1),
		    next_instruction(OP, State2);
		?SSTORE ->
		    %% 0x55 SSTORE δ=2 α=0
		    %% Save word to storage.
		    %% σ'[Ia]s[µs[0]] ≡ µs[1]
		    %% CSSTORE(σ, µ) ≡ Gsset if µs[1] =/= 0
                    %%                         ∧ σ[Ia]s[µs[0]] = 0
		    %%                 Gsreset otherwise
		    %% A'r ≡ Ar + Rsclear if µs[1] = 0 
		    %%                      ∧ σ[Ia]s[µs[0]] =/= 0
		    %%       0 otherwise
		    {Address, State1} = pop(State0),
		    {Value, State2} = pop(State1),
		    State3 = sstore(Address, Value, State2),
		    next_instruction(OP, State3);
		?JUMP ->
		    %% 0x56 JUMP  δ=1 α=0
		    %% Alter the program counter.
		    %% JJUMP(µ) ≡ µs[0]
		    %% This has the effect of writing said value to µpc.
		    {Us0, State1} = pop(State0),
		    State2 = set_cp(Us0-1, State1),
		    next_instruction(OP, State2);
		?JUMPI ->
		    %% 0x57 JUMPI δ=2 α=0
		    %% Conditionally alter the program counter.
		    %% JJUMPI(µ) ≡ µs[0] if µs[1] =/= 0
		    %%             µpc + 1 otherwise
		    %% This has the effect of writing said value to µpc.
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    State3 =
			if Us1 =/= 0 -> set_cp(Us0-1, State2);
			   true      -> State2
			end,
		    next_instruction(OP, State3);
		?GAS ->
		    %% 0x5a GAS δ=0 α=1
		    %% Get the amount of available gas,
		    %% including the corresponding reduction
		    %% for the cost of this instruction.
		    %% µ's[0] ≡ µg
		    Arg = aevm_eeevm_state:gas(State0),
		    Cost = op_cost(maps:get(OP, opcodes())),
		    Val = Arg - Cost,
		    State1 = push(Val, State0),
		    next_instruction(OP, State1);


		?PUSH1 ->
		    %% 0x60 PUSH1 δ=0 α=1
		    %% Place 1 byte item on stack.
		    %% µ's[0] ≡ c(µpc + 1)
		    %% where c(x) ≡ (Ib[x] if x < ||Ib||
		    %%               0 otherwise
		    %% The bytes are read in line from the
		    %% program code’s bytes array.
		    %% The function c ensures the bytes
		    %% default to zero if they extend past the limits.
		    %% The byte is right-aligned (takes the lowest
		    %% significant place in big endian).
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH2 ->
		    %% 0x61 PUSH1 δ=0 α=1
		    %% Place 2 byte item on stack.
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH3 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH4 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH5 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH6 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH7 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH8 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH9 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH10 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH11 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH12 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH13 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH14 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH15 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH16 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH17 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH18 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH19 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH20 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH21 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH22 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH23 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH24 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH25 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH26 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH27 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH28 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH29 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH30 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH31 ->
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?PUSH32 ->
		    %% 0x7f PUSH32 δ=0 α=1
		    %% Place 32-byte (full word) item on stack.
		    %% µ's[0] ≡ c(µpc + 1). . .(µpc + 32)
		    %% where c is defined as above.
		    %% The bytes are right-aligned
		    %% (takes the lowest significant place in big endian).
		    next_instruction(OP, push_n_bytes_from_cp(OP-?PUSH1+1, State));
		?DUP1 ->
		    %% 0x80 DUP1  δ=1 α=2
		    %% Duplicate 1nd stack item.
		    %% µ's[0] ≡ µs[0]
		    next_instruction(OP, dup(1,State0));
		?DUP2 ->
		    %% 0x81 DUP2  δ=2 α=3
		    %% Duplicate 2nd stack item.
		    %% µ's[0] ≡ µs[1]
		    next_instruction(OP, dup(2,State0));
		?DUP3 ->
		    %% 0x82 DUP3  δ=3 α=4
		    %% Duplicate 3nd stack item.
		    %% µ's[0] ≡ µs[2]
		    next_instruction(OP, dup(3,State0));
		?DUP4 ->
		    %% 0x83 DUP4  δ=4 α=5
		    %% Duplicate 4th stack item.
		    %% µ's[0] ≡ µs[3]
		    next_instruction(OP, dup(4,State0));
		?DUP5 ->
		    %% 0x84 DUP5  δ=5 α=6
		    %% Duplicate 5th stack item.
		    %% µ's[0] ≡ µs[4]
		    next_instruction(OP, dup(5,State0));
		?DUP6 ->
		    %% 0x85 DUP6  δ=6 α=7
		    %% Duplicate 6th stack item.
		    %% µ's[0] ≡ µs[5]
		    next_instruction(OP, dup(6,State0));
		?DUP7 ->
		    %% 0x86 DUP7  δ=7 α=8
		    %% Duplicate 7th stack item.
		    %% µ's[0] ≡ µs[6]
		    next_instruction(OP, dup(7,State0));
		?DUP8 ->
		    %% 0x87 DUP8  δ=8 α=9
		    %% Duplicate 8th stack item.
		    %% µ's[0] ≡ µs[7]
		    next_instruction(OP, dup(8,State0));
		?DUP9 ->
		    %% 0x88 DUP9  δ=9 α=10
		    %% Duplicate 9th stack item.
		    %% µ's[0] ≡ µs[8]
		    next_instruction(OP, dup(9,State0));
		?DUP10 ->
		    %% 0x89 DUP10  δ=10 α=11
		    %% Duplicate 10th stack item.
		    %% µ's[0] ≡ µs[9]
		    next_instruction(OP, dup(10,State0));
		?DUP11 ->
		    %% 0x8a DUP11  δ=11 α=12
		    %% Duplicate 11th stack item.
		    %% µ's[0] ≡ µs[10]
		    next_instruction(OP, dup(11,State0));
		?DUP12 ->
		    %% 0x8b DUP12  δ=12 α=13
		    %% Duplicate 12th stack item.
		    %% µ's[0] ≡ µs[11]
		    next_instruction(OP, dup(12,State0));
		?DUP13 ->
		    %% 0x8c DUP13  δ=13 α=14
		    %% Duplicate 13th stack item.
		    %% µ's[0] ≡ µs[12]
		    next_instruction(OP, dup(13,State0));
		?DUP14 ->
		    %% 0x8d DUP14  δ=14 α=15
		    %% Duplicate 14th stack item.
		    %% µ's[0] ≡ µs[13]
		    next_instruction(OP, dup(14,State0));
		?DUP15 ->
		    %% 0x8e DUP15  δ=15 α=16
		    %% Duplicate 15th stack item.
		    %% µ's[0] ≡ µs[14]
		    next_instruction(OP, dup(15,State0));
		?DUP16 ->
		    %% 0x8f DUP16  δ=16 α=17
		    %% Duplicate 16th stack item.
		    %% µ's[0] ≡ µs[5]
		    next_instruction(OP, dup(16,State0));

		?SWAP1 ->
		    %% 0x90 SWAP1 δ=2 α=2
		    %% Exchange 1st and 2nd stack items.
		    %% µ's[0] ≡ µs[1]
		    %% µ's[1] ≡ µs[0]
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP2 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP3 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP4 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP5 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP6 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP7 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP8 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP9 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP10 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP11 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP12 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP13 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP14 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP15 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));
		?SWAP16 ->
		    next_instruction(OP, swap(OP-?SWAP1+1, State));


		?CALL ->
		    %% 0xf1 CALL  δ=7 α=1
		    %% Message-call into an account.
		    %% i ≡ µm[µs[3] . . .(µs[3] + µs[4] − 1)]
		    %%                    Θ(σ, Ia, Io, t, t,
		    %% (σ', g', A+, o) ≡    CCALLGAS(µ), Ip, µs[2], µs[2],
		    %%                      i, Ie + 1)
		    %%                      if µs[2] =< σ[Ia]b ∧ Ie < 1024
		    %%                    (σ, g, ∅,()) otherwise  
                    %% n ≡ min({µs[6], |o|})
		    %% µ'm[µs[5] . . .(µs[5] + n − 1)] = o[0 . . .(n − 1)]
		    %% µ'g ≡ µg + g'
		    %% µ's[0] ≡ x
		    %% A' ≡ A U A+
		    %% t ≡ µs[1] mod 2^160
		    %% where
		    %%  x = 0
		    %%   if the code execution for this
		    %%      operation failed due to an exceptional halting
		    %%      Z(σ, µ, I) = T 
                    %%   or
		    %%   if µs[2] > σ[Ia]b (not enough funds)
		    %%   or
		    %%   Ie = 1024 (call depth limit reached); 
		    %%  x = 1
		    %%   otherwise.
		    %% µ'i ≡ M(M(µi, µs[3], µs[4]), µs[5], µs[6])
		    %% Thus the operand order is:
		    %%  gas, to, value, in offset, in size,
		    %%  out offset, out size.
		    %% CCALL(σ, µ) ≡ CGASCAP(σ, µ) + CEXTRA(σ, µ)
		    %% CCALLGAS(σ, µ) ≡ CGASCAP(σ, µ) + Gcallstipend
		    %%                    if µs[2] =/= 0
		    %%                  CGASCAP(σ, µ)
		    %%                    otherwise
		    %% CGASCAP(σ, µ) ≡ min{L(µg − CEXTRA(σ, µ)), µs[0]}
		    %%                    if µg ≥ CEXTRA(σ, µ)
		    %%                 µs[0]
		    %%                    otherwise
		    %% CEXTRA(σ, µ) ≡ Gcall + CXFER(µ) + CNEW(σ, µ)
		    %% CXFER(µ) ≡ Gcallvalue if µs[2] =/= 0
		    %%            0 otherwise
		    %% CNEW(σ, µ) ≡ Gnewaccount if σ[µs[1] mod 2^160] = ∅
		    %%              0 otherwise
		    %%
		    {Gas, State1} = pop(State0),
		    {To, State2} = pop(State1),
		    {Value, State3} = pop(State2),
		    {IOffset, State4} = pop(State3),
		    {ISize, State5} = pop(State4),
		    {OOffset, State6} = pop(State5),
		    {OSize, State7} = pop(State6),
		    Call = #{ gas => Gas
			    , to => To
			    , value => Value
			    , in_offset => IOffset
			    , in_size => ISize
			    , out_offset => OOffset
			    , out_size => OSize},
		    State8 = aevm_eeevm_state:set_call(Call, State7),
		    State8;

		?RETURN ->
		    %% 0xf3 RETURN δ=2 α=0
		    %% Halt execution returning output data.
		    %% HRETURN(µ) ≡ µm[µs[0] . . .(µs[0] + µs[1] − 1)]
		    %% This has the effect of halting the execution
		    %% at this point with output defined.
		    %% µ'i ≡ M(µi, µs[0], µs[1]) TODO: This
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Out = get_mem_area(Us0, Us0+Us1-1, State2),
		    aevm_eeevm_state:set_out(Out, State2);
		?SUICIDE ->
		    %% 0xff SELFDESTRUCT 1 0
		    %% Halt execution and register account for
		    %% later deletion.
		    %% A's ≡ As ∪ {Ia}
		    %% σ'[µs[0] mod 2^160]b ≡ σ[µs[0] mod 2^160]b + σ[Ia]b
		    %% σ'[Ia]b ≡ 0
		    %% A'r ≡ Ar + (Rselfdestruct if Ia -∈ As
		    %%       0 otherwise
		    %% CSELFDESTRUCT(σ, µ) ≡ Gselfdestruct
		    %%                       + Gnewaccount
		    %%                           if σ[µs[0] mod 2^160] = ∅
		    %%                       + 0 otherwise
		    {Us0, State1} = pop(State0),
		    aevm_eeevm_state:set_selfdestruct(Us0, State1);
		_ ->
		    error({opcode_not_implemented,
			   lists:flatten(
			     io_lib:format("~2.16.0B",[OP]))})
	    end;
	true -> State
    end.

%% ------------------------------------------------------------------------
%% ARITHMETIC
%% ------------------------------------------------------------------------
add(Arg1, Arg2) -> (Arg1 + Arg2) band ?MASK256.
mul(Arg1, Arg2) -> (Arg1 * Arg2) band ?MASK256.
sub(Arg1, Arg2) -> (Arg1 - Arg2) band ?MASK256.
exp(Arg1, Arg2) -> pow(Arg1, Arg2) band ?MASK256.
idiv(_Arg1,    0)-> 0;
idiv(Arg1, Arg2)-> (Arg1 div Arg2) band ?MASK256.
sdiv(_Arg1, 0)-> 0;
sdiv(?NEG2TO255, -1) -> ?NEG2TO255;
sdiv(Arg1, Arg2) ->
    <<SArg1:256/integer-signed>> = <<Arg1:256/integer-unsigned>>,
    <<SArg2:256/integer-signed>> = <<Arg2:256/integer-unsigned>>,
    (SArg1 div SArg2) band ?MASK256.

mod(_Arg1,   0) -> 0;
mod(Arg1, Arg2) -> modulo(Arg1, Arg2) band ?MASK256.

smod(_Arg1,   0) -> 0;
smod(Arg1, Arg2) -> smodulo(Arg1, Arg2) band ?MASK256.


addmod(_Arg1,_Arg2,   0) -> 0;
addmod(Arg1, Arg2, Arg3) -> modulo((Arg1 + Arg2), Arg3) band ?MASK256.
modulo(Arg1, Arg2) ->
    Res = (Arg1 rem Arg2 + Arg2) rem Arg2,
    Res.

mulmod(_Arg1,_Arg2,   0) -> 0;
mulmod(Arg1, Arg2, Arg3) -> modulo((Arg1 * Arg2), Arg3) band ?MASK256.

signed(Val) ->
    <<SVal:256/integer-signed>> = <<Val:256/integer-unsigned>>,
    SVal.

smodulo(Arg1, Arg2) ->
    <<SArg1:256/integer-signed>> = <<Arg1:256/integer-unsigned>>,
    <<SArg2:256/integer-signed>> = <<Arg2:256/integer-unsigned>>,
    Res = (SArg1 rem (SArg2 + SArg2)) rem SArg2,
    Res.

pow(X, Y) when is_integer(X), is_integer(Y), Y >= 0 ->
    pow(1, X, Y).

pow(N, _, 0) ->     N;
pow(N, X, 1) -> X * N;
pow(N, X, Y) ->
    Square = (X * X) band ?MASK256,
    Exp = Y bsr 1,
    if (Y band 1) =:= 0 -> pow(    N, Square, Exp);
       true             -> pow(X * N, Square, Exp)
    end.


signextend(Us0, Us1) ->
    ExtendTo =  (256 - 8*((Us0+1) band 255)) band 255,
    <<_:ExtendTo,SignBit:1, TruncVal/bits>> = 
	<<Us1:256/integer-unsigned>>,
    Pad = << <<SignBit:1>> || _ <- lists:seq(1,ExtendTo)>>,
    <<Val:256/integer-unsigned>> =
	<<Pad:ExtendTo/bits, SignBit:1, TruncVal/bits>>,
    Val.


byte(Byte, Arg2) when Byte < 32 ->
    Bitpos = 256 - 8*(Byte+1),
    Mask = 255,
    (Arg2 bsr Bitpos) band Mask;
byte(_,_) -> 0.


    

%% ------------------------------------------------------------------------
%% STACK
%% ------------------------------------------------------------------------
push(Arg, State) ->
    Val = Arg band ?MASK256,
    Stack   = aevm_eeevm_state:stack(State),
    aevm_eeevm_state:set_stack([Val|Stack], State).

push_n_bytes_from_cp(N, State) ->
    CP   = aevm_eeevm_state:cp(State),
    Code = aevm_eeevm_state:code(State),
    Arg = code_get_arg(CP+1, N, Code),
    State1 = push(Arg, State),
    inc_cp(N, State1).


pop(State) ->
    case aevm_eeevm_state:stack(State) of
	[Arg|Stack] ->
	    {Arg, aevm_eeevm_state:set_stack(Stack, State)};
	[] ->
	    throw({error_pop_empty_stack, State})
    end.

dup(N, State) ->
    case aevm_eeevm_state:stack(State) of
	[] ->
	    throw({error_dup_empty_stack, State});
	Stack ->
	    case length(Stack) < N of
		true ->
		    throw({error_dup_too_small_stack, State});
		false ->
		    Val = lists:nth(N, Stack),
		    push(Val, State)
	    end
    end.

swap(N, State) ->
    case aevm_eeevm_state:stack(State) of
	[] ->
    	    throw({error_swap_empty_stack, State});
	[Top|Rest] ->
	    case length(Rest) < N of
		true ->
		    throw({error_swap_too_small_stack, State});
		false ->
		    Nth = lists:nth(N, Rest),
		    Stack = [Nth| set_nth(N, Top, Rest)],
		    aevm_eeevm_state:set_stack(Stack, State)
	    end
    end.

set_nth(1, Val, [_|Rest]) -> [Val|Rest];
set_nth(N, Val, [E|Rest]) -> [E|set_nth(N-1, Val, Rest)].

%% ------------------------------------------------------------------------
%% MEMORY
%% ------------------------------------------------------------------------

%% No alignment or size check. Don't use directly.
m_write(Address,     0, Mem) -> maps:remove(Address, Mem);
m_write(Address, Value, Mem) -> maps:put(Address, Value, Mem).

m_read(Address, 1, Mem) ->
    AlignedAddress = (Address bor ?ALIGN256) - ?ALIGN256,
    WordVal = maps:get(AlignedAddress , Mem, 0),
    ByteOffset = 31 - (Address - AlignedAddress),
    Byte = ((WordVal bsr (ByteOffset*8)) band 255),
    Byte;
m_read(Address, 32, Mem) ->
    AlignedAddress = (Address bor ?ALIGN256) - ?ALIGN256,
    case AlignedAddress =:= Address of
	true -> %% Aligned.
	    maps:get(AlignedAddress , Mem, 0);
	false -> %%
	    error(unaligned_mem_read_not_implemented)
    end.


mload(Address, State) ->
    Mem = aevm_eeevm_state:mem(State),
    Value = m_read(Address, 32, Mem),
    Value.


mstore(Address, Value, State) when is_integer(Value) ->
    case (Address band ?ALIGN256) of
	%% 256-bits-word aligned
	0 -> Mem = aevm_eeevm_state:mem(State),
	     %% Make sure value fits in 256 bits.
	     Value256 = Value band ?MASK256,
	     Mem1 = m_write(Address, Value256, Mem),
	     aevm_eeevm_state:set_mem(Mem1, State);
	_ -> %% Unligned
	    error({unaligned_sstore_not_handled, Address, Value})
    end.

mstore8(Address, Value, State) when is_integer(Value) ->
    Mem = aevm_eeevm_state:mem(State),
    Byte = Value band 255,
    AlignedAddress = (Address bor ?ALIGN256) - ?ALIGN256,
    WordVal = maps:get(AlignedAddress , Mem, 0),
    ByteOffset = Address - AlignedAddress,
    NewWord = (WordVal band (bnot (255 bsl ByteOffset))) bor (Byte bsl ByteOffset),
    Mem1 = m_write(AlignedAddress, NewWord, Mem),
    aevm_eeevm_state:set_mem(Mem1, State).

get_mem_area(From, To, State) ->
    Mem = aevm_eeevm_state:mem(State),
    list_to_binary([m_read(X, 1, Mem) || X <- lists:seq(From, To)]).

%% ------------------------------------------------------------------------
%% STORAGE
%% ------------------------------------------------------------------------
storage_read(Address, Mem) -> maps:get(Address, Mem, 0).

%% No alignment or size check. Don't use directly.
storage_write(Address,     0, Mem) -> maps:remove(Address, Mem);
storage_write(Address, Value, Mem) -> maps:put(Address, Value, Mem).

sload(Address, State) ->
    Store = aevm_eeevm_state:storage(State),
    Value = storage_read(Address, Store),
    Value.


    
sstore(Address, Value, State) when is_integer(Value) ->
    Store = aevm_eeevm_state:storage(State),
    %% Make sure value fits in 256 bits.
    Value256 = Value band ?MASK256,
    Store1 = storage_write(Address, Value256, Store),
    aevm_eeevm_state:set_storage(Store1, State).


%% ------------------------------------------------------------------------
%% DATA
%% ------------------------------------------------------------------------
data_get_val(Address, Size, State) ->
    Data = aevm_eeevm_state:data(State),
    if Address >= byte_size(Data) -> 0;
       true ->
	    Pos = Address * 8,
	    Length = Size*8,
	    <<_:Pos, Arg:Length, _/binary>> = Data,
	    Arg
    end.

					 

%% ------------------------------------------------------------------------
%% CODE
%% ------------------------------------------------------------------------
code_get_op(CP, Code) -> binary:at(Code, CP).

%% The function c ensures the bytes default to zero if they
%% extend past the limits.
%% The byte is right-aligned (takes the lowest significant
%% place in big endian).
code_get_arg(CP,_Size, Code) when CP >= byte_size(Code) -> 0;
code_get_arg(CP, Size, Code) when CP+Size >= byte_size(Code) -> 
    End = byte_size(Code),
    DataSize = (Size - (End - CP))*8,
    Pos = CP * 8,
    Length = Size*8,
    <<_:Pos, Arg:Length, _/binary>> = <<Code/binary, 0:DataSize>>,
    Arg;
code_get_arg(CP, Size, Code) ->
    Pos = CP * 8,
    Length = Size*8,
    <<_:Pos, Arg:Length, _/binary>> = Code,
    Arg.

next_instruction(OP, State) ->
    eval(inc_cp(spend_gas(OP, State))).

set_cp(Address, State) ->
    aevm_eeevm_state:set_cp(Address, State).


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

