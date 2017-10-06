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

%% Exports for tracing. TODO: move to aevm_eeevm_code
-export([code_get_op/2]).

-include("aevm_eeevm.hrl").

%% Main eval loop.
%%
%%
eval(State) ->
    loop(valid_jumpdests(State)).

valid_jumpdests(State) ->
    Code = aevm_eeevm_state:code(State),
    JumpDests = jumpdests(0,Code, #{}),
    aevm_eeevm_state:set_jumpdests(JumpDests, State).

%% Jump Destination Validity. 
%% DJ (c, i) ≡ {} if i > |c|
%%             {i} ∪ DJ (c, N(i, c[i])) if c[i] = JUMPDEST
%%             DJ (c, N(i, c[i])) otherwise
%% where N is the next valid instruction position in the
%% code, skipping the data of a PUSH instruction, if any:
%% 
%% N(i, w) ≡ i + w − PUSH1 + 2 if w ∈ [PUSH1, PUSH32]
%%           i + 1 otherwise
jumpdests(N, Code, ValidDests) when N >= byte_size(Code) ->
    ValidDests;
jumpdests(N, Code, ValidDests) ->
    OP = code_get_op(N, Code),
    case OP of
	?JUMPDEST ->
	    jumpdests(N+1, Code, maps:put(N, true, ValidDests));
	OP when (OP >= ?PUSH1) andalso (OP =< ?PUSH32) ->
	    jumpdests(N+(OP-?PUSH1+2), Code, ValidDests);
	_ -> jumpdests(N+1, Code, ValidDests)
    end.
  
    
    

loop(StateIn) ->
    CP   = aevm_eeevm_state:cp(StateIn),
    Code = aevm_eeevm_state:code(StateIn),
    case CP >= byte_size(Code) of
	false ->
	    OP   = code_get_op(CP, Code),
            State = spend_gas(OP, StateIn),
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
		%% No opcodes 0x0c-0x0f
		16#0c -> throw({illegal_instruction, OP, State});
		16#0d -> throw({illegal_instruction, OP, State});
		16#0e -> throw({illegal_instruction, OP, State});
		16#0f -> throw({illegal_instruction, OP, State});
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
		%% No opcodes 0x1b-0x1f
		16#1b -> throw({illegal_instruction, OP, State});
		16#1c -> throw({illegal_instruction, OP, State});
		16#1d -> throw({illegal_instruction, OP, State});
		16#1e -> throw({illegal_instruction, OP, State});
		16#1f -> throw({illegal_instruction, OP, State});
		?SHA3 ->
		    %% 0x20 SHA3  δ=2 α=1 Compute Keccak-256 hash.
		    %% µ's[0] ≡ Keccak(µm[µs[0] . . .(µs[0] + µs[1] − 1)])
		    %% µi ≡ M(µi, µs[0], µs[1])
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
                    To   = Us0+Us1-1,
		    {Arg, State3} = aevm_eeevm_memory:get_area(Us0, To, State2),
		    Hash = sha3:hash(256, Arg),
		    <<Val:256/integer-unsigned>> = Hash,
		    State4 = push(Val, State3),
		    next_instruction(OP, State4);
		%% No opcodes 0x21-0x2f
		16#21 -> throw({illegal_instruction, OP, State});
		16#22 -> throw({illegal_instruction, OP, State});
		16#23 -> throw({illegal_instruction, OP, State});
		16#24 -> throw({illegal_instruction, OP, State});
		16#25 -> throw({illegal_instruction, OP, State});
		16#26 -> throw({illegal_instruction, OP, State});
		16#27 -> throw({illegal_instruction, OP, State});
		16#28 -> throw({illegal_instruction, OP, State});
		16#29 -> throw({illegal_instruction, OP, State});
		16#2a -> throw({illegal_instruction, OP, State});
		16#2b -> throw({illegal_instruction, OP, State});
		16#2c -> throw({illegal_instruction, OP, State});
		16#2d -> throw({illegal_instruction, OP, State});
		16#2e -> throw({illegal_instruction, OP, State});
		16#2f -> throw({illegal_instruction, OP, State});

		?CALLER ->
		    %% 0x33 CALLER δ=0 α=1
		    %% Get caller address.
		    %% µ's[0] ≡ Is
		    %% This is the address of the account
		    %% that is directly responsible for this execution.
		    Arg = aevm_eeevm_state:caller(State0),
		    State1 = push(Arg, State0),
		    next_instruction(OP, State1);
		?CALLVALUE ->
		   %% 0x34 CALLVALUE δ=0 α=1
		    %% Get deposited value by the instruction/transaction
		    %% responsible for this execution.
		    %% µ's[0] ≡ Iv
		    Val = aevm_eeevm_state:value(State0),
		    State1 = push(Val, State0),
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
		?CALLDATASIZE ->
		    %% 0x36 CALLDATASIZE δ=0 α=1
		    %% Get size of input data in current environment.
		    %% µ's[0] ≡ |Id|
		    %% This pertains to the input data passed with the
		    %% message call instruction or transaction.
		    Val = byte_size(aevm_eeevm_state:data(State0)),
		    State1 = push(Val, State0),
		    next_instruction(OP, State1);
		?CALLDATACOPY ->
		    %% 0x37 CALLDATACOPY 3 0
		    %% Copy input data in current environment to memory.
		    %% ∀i∈{0...µs[2]−1}µ'm[µs[0] + i] ≡ Id[µs[1] + i]
		    %%                                       if µs[1] + i < |Id|
		    %%                                   0 otherwise
		    %% The additions in µs[1] + i are not subject to
		    %% the 2^256 modulo.
		    %% µ'i ≡ M(µi, µs[0], µs[2])
		    %% This pertains to the input data passed with
		    %% the message call instruction or transaction.    
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    {Us2, State3} = pop(State2),
		    CallData = data_get_bytes(Us1, Us2, State3),
		    State4 = aevm_eeevm_memory:write_area(Us0, CallData, State3),
		    next_instruction(OP, State4);
		?CODESIZE ->
		    %% 0x38 CODESIZE  δ=0 α=1
		    %% Get size of code running in current environment.
		    %% µ's[0] ≡ |Ib|
		    State1 = push(byte_size(Code), State0),
		    next_instruction(OP, State1);
		?CODECOPY ->
		    %% 0x39 CODECOPY δ=3 α=0
		    %% Copy code running in current environment to memory.
		    %% ∀i∈{0...µs[2]−1}µ0m[µs[0] + i] ≡ Ib[µs[1] + i]
		    %%                                        if µs[1] + i < |Ib|
		    %%                                   STOP otherwise
		    %% µ'i ≡ M(µi, µs[0], µs[2])
		    %% The additions in µs[1] + i are not subject to
		    %% the 2^256 modulo.
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    {Us2, State3} = pop(State2),
		    CodeArea = code_get_area(Us1, Us2, Code),
		    State4 = aevm_eeevm_memory:write_area(Us0, CodeArea, State3),
		    next_instruction(OP, State4);

		?EXTCODESIZE ->
		    %% 0x3b EXTCODESIZE δ=1 α=1
		    %% Get size of an account’s code.
		    %% µ's[0] ≡ |σ[µs[0] mod 2^160] c|
		    {Us0, State1} = pop(State0),
		    Val = aevm_eeevm_state:extcodesize(Us0, State1),
		    io:format("EXTCODEIZE~p~n",[Val]),
		    State2 = push(Val, State1),
		    next_instruction(OP, State2);
		    
		%% No opcode 0x3f
		16#3f -> throw({illegal_instruction, OP, State});
		?NUMBER ->
		    %% 0x43 NUMBER  δ=0 α=1
		    %% Get the block’s number.
		    %% µ's[0] ≡ IHi
		    Arg = aevm_eeevm_state:number(State0),
		    State1 = push(Arg, State0),
		    next_instruction(OP, State1);
		    
		?POP ->
		    %% 0x50 POP δ=1 α=0
		    %% Remove item from stack.
		    {_, State1} = pop(State0),
		    next_instruction(OP, State1);
		?MLOAD ->
		    %% 0x51 MLOAD δ=1 α=1
		    %% Load word from memory.
		    %% µ's[0] ≡ µm[µs[0] . . .(µs[0] + 31)]
		    %% µ'i ≡ max(µi, [(µs[0] + 32) ÷ 32])
		    %% The addition in the calculation of µ'i
		    %% is not subject to the 2^256 modulo.
		    {Us0, State1} = pop(State0),
		    {Val, State2} = aevm_eeevm_memory:load(Us0, State1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State3);
		?MSTORE ->
		    %% 0x52 MSTORE δ=2 α=0
		    %% Save word to memory.
		    %% µ'm[µs[0] . . .(µs[0] + 31)] ≡ µs[1]
		    %% µ'i ≡ max(µi, [(µs[0] + 32) ÷ 32])
		    %% The addition in the calculation of µ'i
		    %% is not subject to the 2^256 modulo.
		    {Address, State1} = pop(State0),
		    {Value, State2} = pop(State1),
		    State3 = aevm_eeevm_memory:store(Address, Value, State2),
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
		    State3 = aevm_eeevm_memory:store8(Address, Value, State2),
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
		    JumpDests =  aevm_eeevm_state:jumpdests(State1),
		    case maps:get(Us0, JumpDests, false) of
			true -> 
			    State2 = set_cp(Us0-1, State1),
			    next_instruction(OP, State2);
			false -> throw({{invalid_jumpdest, Us0}, State1})
		    end;
		?JUMPI ->
		    %% 0x57 JUMPI δ=2 α=0
		    %% Conditionally alter the program counter.
		    %% JJUMPI(µ) ≡ µs[0] if µs[1] =/= 0
		    %%             µpc + 1 otherwise
		    %% This has the effect of writing said value to µpc.
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    State3 =
			if Us1 =/= 0 ->
				JumpDests =  aevm_eeevm_state:jumpdests(State1),
				case maps:get(Us0, JumpDests, false) of
				    true -> 
					set_cp(Us0-1, State2);
				    false -> 
					throw({{invalid_jumpdest, Us0}, State1})
				end;
			   true      -> State2
			end,
		    next_instruction(OP, State3);
		?PC ->
		    %% 0x58 PC δ=0 α=1
		    %% Get the value of the program counter prior to
		    %% the increment corresponding to this instruction.
		    %% µ's[0] ≡ µpc
		    State1 = push(CP, State0),
		    next_instruction(OP, State1);
		?MSIZE ->
		    %% 0x59 PC δ=0 α=1
		    %% Get the size of active memory in bytes.
		    %% µ's[0] ≡ 32*µi
		    Val =  32 * aevm_eeevm_memory:size_in_words(State),
		    State1 = push(Val, State0),
		    next_instruction(OP, State1);
		?GAS ->
		    %% 0x5a GAS δ=0 α=1
		    %% Get the amount of available gas,
		    %% including the corresponding reduction
		    %% for the cost of this instruction.
		    %% µ's[0] ≡ µg
		    Val = aevm_eeevm_state:gas(State0),
		    State1 = push(Val, State0),
		    next_instruction(OP, State1);
		?JUMPDEST ->
		    %% 0x5b JUMPDEST  δ=0 α=0
		    %% Mark a valid destination for jumps.
		    %% This operation has no effect on machine
		    %% state during execution.
		    next_instruction(OP, State0);
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
                    To = Us0+Us1-1,
		    {Out, State3} = aevm_eeevm_memory:get_area(Us0, To, State2),
		    aevm_eeevm_state:set_out(Out, State3);
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
	true -> StateIn
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
    if length(Stack) < 1024 ->
	    aevm_eeevm_state:set_stack([Val|Stack], State);
       true ->
	    throw({out_of_stack, State})
    end.

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

data_get_bytes(Address, Size, State) ->
    Data = aevm_eeevm_state:data(State),
    Pos = Address * 8,
    if Address+Size >= byte_size(Data) ->
	    End = byte_size(Data),
	    DataSize = (Size - (End - Address))*8,
	    <<_:Pos, Bytes:Size/binary, _/binary>> = <<Data/binary, 0:DataSize>>,
	    Bytes;
       true ->
	    <<_:Pos, Bytes:Size/binary, _/binary>> = Data,
	    Bytes
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

code_get_area(From, Size, Code) when From+Size >= byte_size(Code) ->
    End = byte_size(Code),
    DataSize = (Size - (End -From))*8,
    Pos = From * 8,
     <<_:Pos, Arg:Size/binary, _/binary>> = <<Code/binary, 0:DataSize>>,
    Arg;
code_get_area(From, Size, Code) ->
    Pos = From * 8,
    <<_:Pos, Arg:Size/binary, _/binary>> = Code,
    Arg.   

next_instruction(_OP, State) ->
    loop(inc_cp(State)).

set_cp(Address, State) ->
    aevm_eeevm_state:set_cp(Address, State).


inc_cp(State) ->
    CP = aevm_eeevm_state:cp(State),
    aevm_eeevm_state:set_cp(CP + 1, State).

inc_cp(Amount, State) ->
    CP = aevm_eeevm_state:cp(State),
    aevm_eeevm_state:set_cp(CP + Amount, State).

%% ------------------------------------------------------------------------
%% GAS
%% ------------------------------------------------------------------------

spend_gas(Op, State) ->
    Cost = aevm_gas:op_cost(Op, State),
    Gas  = aevm_eeevm_state:gas(State),
    case Gas >= Cost of
        true ->  aevm_eeevm_state:set_gas(Gas - Cost, State);
	false -> throw({out_of_gas, State})
    end.
