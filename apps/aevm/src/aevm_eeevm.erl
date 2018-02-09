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

-include_lib("aebytecode/include/aeb_opcodes.hrl").
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
            State = spend_op_gas(OP, StateIn),
	    State0 = aevm_eeevm_state:trace_format("~n", [], State),
	    case OP of
		%% =s: Stop and Arithmetic Operations
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
		    next_instruction(OP, State, State3);
		?MUL ->
		    %% 0x02 MUL δ=2 α=1
		    %% Multiplication operation.
		    %% µs'[0] ≡ µs[0] * µs[1]
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = mul(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State, State3);
		?SUB ->
		    %% 0x03 SUB δ=2 α=1
		    %% Subtraction operation.
		    %% µ's[0] ≡ µs[0] − µs[1]
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = sub(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State, State3);
		?DIV ->
		    %% 0x04 DIV δ=2 α=1
		    %% Integer division operation.
		    %% µ's[0] ≡ 0 if µs[1] = 0
		    %%          µs[0] / µs[1] otherwise
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = idiv(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State, State3);
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
		    next_instruction(OP, State, State3);
		?MOD ->
		    %% 0x06 MOD δ=2 α=1
		    %% Modulo remainder operation.
		    %% µ's[0] ≡  0 if µs[1] = 0
		    %%           µs[0] mod µs[1] otherwise
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = mod(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State, State3);
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
		    next_instruction(OP, State, State3);
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
		    next_instruction(OP, State, State4);
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
		    next_instruction(OP, State, State4);
		?EXP ->
		    %% 0x0a EXP δ=2 α=1
		    %% Exponential operation.
		    %% µ's[0] ≡ µs[0] ^ µs[1]
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = exp(Us0, Us1),
		    State3 = push(Val, State2),
		    next_instruction(OP, State, State3);
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
		    next_instruction(OP, State, State3);
		%% No opcodes 0x0c-0x0f
		16#0c -> throw({illegal_instruction, OP, State});
		16#0d -> throw({illegal_instruction, OP, State});
		16#0e -> throw({illegal_instruction, OP, State});
		16#0f -> throw({illegal_instruction, OP, State});
		%% 10s: Comparison & Bitwise Logic Operations
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
		    next_instruction(OP, State, State3);
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
		    next_instruction(OP, State, State3);
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
		    next_instruction(OP, State, State3);
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
		    next_instruction(OP, State, State3);
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
		    next_instruction(OP, State, State3);
		?ISZERO ->
		    %% 0x15 ISZERO δ=1 α=1
		    %% Simple not operator.
		    %% µ's[0] ≡ 1 if µs[0] = 0
		    %%          0 otherwise
		    {Us0, State1} = pop(State0),
		    Val = if Us0 =:= 0 -> 1; true -> 0 end,
		    State2 = push(Val, State1),
		    next_instruction(OP, State, State2);
		?AND ->
		    %% 0x16 AND δ=2 α=1
		    %% Bitwise AND operation.
		    %% ∀i ∈ [0..255] : µ's[0]i ≡ µs[0]i ∧ µs[1]i
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = Us0 band Us1,
		    State3 = push(Val, State2),
		    next_instruction(OP, State, State3);
		?OR ->
		    %% 0x17 OR δ=2 α=1
		    %% Bitwise OR operation.
		    %% ∀i ∈ [0..255] : µ's[0]i ≡ µs[0]i ∨ µs[1]i
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = Us0 bor Us1,
		    State3 = push(Val, State2),
		    next_instruction(OP, State, State3);
		?XOR ->
		    %% 0x18 XOR δ=2 α=1
		    %% Bitwise XOR operation.
		    %% ∀i ∈ [0..255] : µ's[0]i ≡ µs[0]i ∨ µs[1]i
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    Val = Us0 bxor Us1,
		    State3 = push(Val, State2),
		    next_instruction(OP, State, State3);
		?NOT ->
		    %% 0x19 NOT δ=1 α=1
		    %% Bitwise NOT operation.
		    %% ∀i ∈ [0..255] : µ's[0]i ≡ 1 if µs[0]i = 0
		    %%                           0 otherwise
		    {Us0, State1} = pop(State0),
		    Val = (bnot Us0) band ?MASK256,
		    State2 = push(Val, State1),
		    next_instruction(OP, State, State2);
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
		    next_instruction(OP, State, State3);
		%% No opcodes 0x1b-0x1f
		16#1b -> throw({illegal_instruction, OP, State});
		16#1c -> throw({illegal_instruction, OP, State});
		16#1d -> throw({illegal_instruction, OP, State});
		16#1e -> throw({illegal_instruction, OP, State});
		16#1f -> throw({illegal_instruction, OP, State});
		%% 20s: SHA3
		?SHA3 ->
		    %% 0x20 SHA3  δ=2 α=1 Compute Keccak-256 hash.
		    %% µ's[0] ≡ Keccak(µm[µs[0] . . .(µs[0] + µs[1] − 1)])
		    %% µi ≡ M(µi, µs[0], µs[1])
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    {Arg, State3} = aevm_eeevm_memory:get_area(Us0, Us1, State2),
		    Hash = aec_hash:hash(evm, Arg),
		    <<Val:256/integer-unsigned>> = Hash,
		    State4 = push(Val, State3),
		    next_instruction(OP, State, State4);
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
		%% 30s: Environmental Information
		?ADDRESS ->
		    %% 0x30 Address δ=0 α=1
		    %% Get address of currently executing account.
		    %% µ's[0] ≡ Ia
		    Arg = aevm_eeevm_state:address(State0),
		    State1 = push(Arg, State0),
		    next_instruction(OP, State, State1);
		?BALANCE ->
		    %% 0x31 BALANCE δ=1 α=1
		    %%  Get balance of the given account.
		    %%  Get balance of the given account.
		    %% µ's[0] ≡ σ[µs[0]]b if σ[µs[0] mod 2^160] =/= ∅
		    %%          0  otherwise
		    {Us0, State1} = pop(State0),
		    Arg = aevm_eeevm_state:accountbalance(Us0, State1),
		    State2 = push(Arg, State1),
		    next_instruction(OP, State, State2);
		?ORIGIN ->
		    %% 0x32 ORIGIN 0 1
		    %% Get execution origination address.
		    %% µ's[0] ≡ Io
		    %% This is the sender of original transaction;
		    %% it is never an account with non-empty
		    %% associated code.
		    Arg = aevm_eeevm_state:origin(State0),
		    State1 = push(Arg, State0),
		    next_instruction(OP, State, State1);
		?CALLER ->
		    %% 0x33 CALLER δ=0 α=1
		    %% Get caller address.
		    %% µ's[0] ≡ Is
		    %% This is the address of the account
		    %% that is directly responsible for this execution.
		    Arg = aevm_eeevm_state:caller(State0),
		    State1 = push(Arg, State0),
		    next_instruction(OP, State, State1);
		?CALLVALUE ->
		    %% 0x34 CALLVALUE δ=0 α=1
		    %% Get deposited value by the instruction/transaction
		    %% responsible for this execution.
		    %% µ's[0] ≡ Iv
		    Val = aevm_eeevm_state:value(State0),
		    State1 = push(Val, State0),
		    next_instruction(OP, State, State1);
		?CALLDATALOAD ->
		    %% 0x35 CALLDATALOAD δ=1 α=1
		    %% Get input data of current environment.
		    %% µ's[0] ≡ Id[µs[0] . . .(µs[0] + 31)] with Id[x] = 0 if x >= |Id|
		    %% This pertains to the input data passed with the message
		    %% call instruction or transaction.
		    {Us0, State1} = pop(State0),
		    Arg = data_get_val(Us0, State1),
		    State2 = push(Arg, State1),
		    next_instruction(OP, State, State2);
		?CALLDATASIZE ->
		    %% 0x36 CALLDATASIZE δ=0 α=1
		    %% Get size of input data in current environment.
		    %% µ's[0] ≡ |Id|
		    %% This pertains to the input data passed with the
		    %% message call instruction or transaction.
		    Val = byte_size(aevm_eeevm_state:data(State0)),
		    State1 = push(Val, State0),
		    next_instruction(OP, State, State1);
		?CALLDATACOPY ->
		    %% 0x37 CALLDATACOPY δ=3 α=0
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
		    next_instruction(OP, State, State4);
		?CODESIZE ->
		    %% 0x38 CODESIZE  δ=0 α=1
		    %% Get size of code running in current environment.
		    %% µ's[0] ≡ |Ib|
		    State1 = push(byte_size(Code), State0),
		    next_instruction(OP, State, State1);
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
		    next_instruction(OP, State, State4);
		?GASPRICE ->
		    %% 0x3a GASPRICE δ=0 α=1
		    %% Get price of gas in current environment.
		    %% µ's[0] ≡ Ip
		    %%  This is gas price specified by the
		    %% originating transaction.
		    Arg = aevm_eeevm_state:gasprice(State0),
		    State1 = push(Arg, State0),
		    next_instruction(OP, State, State1);
		?EXTCODESIZE ->
		    %% 0x3b EXTCODESIZE δ=1 α=1
		    %% Get size of an account’s code.
		    %% µ's[0] ≡ |σ[µs[0] mod 2^160] c|
		    {Us0, State1} = pop(State0),
		    Val = aevm_eeevm_state:extcodesize(Us0, State1),
		    State2 = push(Val, State1),
		    next_instruction(OP, State, State2);
		?EXTCODECOPY ->
		    %% 0x3c EXTCODECOPY δ=4 α=0
		    %% Copy an account’s code to memory.
		    %% ∀i∈{0...µs[3]−1}µ'm[µs[1] + i] ≡ c[µs[2] + i]
		    %%                                       if µs[2] + i < |c|
		    %%                                   STOP otherwise
		    %% where c ≡ σ[µs[0] mod 2^160]c
		    %% µ'i ≡ M(µi, µs[1], µs[3])
		    %% The additions in µs[2] + i are not
		    %% subject to the 2^256 modulo.
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    {Us2, State3} = pop(State2),
		    {Us3, State4} = pop(State3),
		    CodeArea = aevm_eeevm_state:extcode(Us0, Us2, Us3, State4),
		    State5 = aevm_eeevm_memory:write_area(Us1, CodeArea, State4),
		    next_instruction(OP, State, State5);
		?RETURNDATASIZE ->
		    %% 0x3d RETURNDATASIZE
		    %% Not in yellow paper
		    error({opcode_not_implemented,
			   lists:flatten(
			     io_lib:format("~2.16.0B",[OP]))});
		?RETURNDATACOPY ->
		    %% 0x3e RETURNDATACOPY
		    %% Not in yellow paper
		    error({opcode_not_implemented,
			   lists:flatten(
			     io_lib:format("~2.16.0B",[OP]))});
		%% No opcode 0x3f
		16#3f -> throw({illegal_instruction, OP, State0});
		%% 40s Block Information
		?BLOCKHASH ->
		    %% 0x40 BLOCKHASH δ=1 α=1
		    %% Get the hash of one of the 256 most
		    %% recent complete blocks.
		    %% µ's[0] ≡ P(IHp, µs[0], 0)
		    %% where P is the hash of a block of a particular number,
		    %% up to a maximum age.
		    %% 0 is left on the stack if the looked for block number
		    %% is greater than the current block number
		    %% or more than 256 blocks behind the current block.
		    %%               0 if n > Hi ∨ a = 256 ∨ h = 0
		    %% P(h, n, a) ≡  h if n = Hi
		    %%               P(Hp, n, a + 1) otherwise
		    %% and we assert the header H can be determined as
		    %% its hash is the parent hash
		    %% in the block following it.
		    {Us0, State1} = pop(State0),
		    Hash = aevm_eeevm_state:blockhash(Us0, 0, State1),
		    State2 = push(Hash, State1),
		    next_instruction(OP, State, State2);
		?COINBASE ->
		    %% 0x41 COINBASE δ=0 α=1
		    %% Get the block’s beneficiary address.
		    %% µ's[0] ≡ IHc
		    Arg = aevm_eeevm_state:coinbase(State0),
		    State1 = push(Arg, State0),
		    next_instruction(OP, State, State1);
		?TIMESTAMP ->
		    %% 0x42 TIMESTAMP δ=0 α=1
		    %% Get the block’s timestamp.
		    %% µ's[0] ≡ IHs
		    Arg = aevm_eeevm_state:timestamp(State0),
		    State1 = push(Arg, State0),
		    next_instruction(OP, State, State1);
		?NUMBER ->
		    %% 0x43 NUMBER  δ=0 α=1
		    %% Get the block’s number.
		    %% µ's[0] ≡ IHi
		    Arg = aevm_eeevm_state:number(State0),
		    State1 = push(Arg, State0),
		    next_instruction(OP, State, State1);
		?DIFFICULTY ->
		    %% 0x44 DIFFICULTY δ=0 α=1
		    %% Get the block’s difficulty.
		    %% µ's[0] ≡ IHd
		    Arg = aevm_eeevm_state:difficulty(State0),
		    State1 = push(Arg, State0),
		    next_instruction(OP, State, State1);
		?GASLIMIT ->
		    %% 0x45 GASLIMIT  δ=0 α=1
		    %% Get the block’s number.
		    %% µ's[0] ≡ IHl
		    Arg = aevm_eeevm_state:gaslimit(State0),
		    State1 = push(Arg, State0),
		    next_instruction(OP, State, State1);
		%% No opcode 0x46-0x4f
		16#46 -> throw({illegal_instruction, OP, State0});
		16#47 -> throw({illegal_instruction, OP, State0});
		16#48 -> throw({illegal_instruction, OP, State0});
		16#49 -> throw({illegal_instruction, OP, State0});
		16#4a -> throw({illegal_instruction, OP, State0});
		16#4b -> throw({illegal_instruction, OP, State0});
		16#4c -> throw({illegal_instruction, OP, State0});
		16#4d -> throw({illegal_instruction, OP, State0});
		16#4e -> throw({illegal_instruction, OP, State0});
		16#4f -> throw({illegal_instruction, OP, State0});
		%% 50s: Stack, Memory, Storage and Flow Operations
		?POP ->
		    %% 0x50 POP δ=1 α=0
		    %% Remove item from stack.
		    {_, State1} = pop(State0),
		    next_instruction(OP, State, State1);
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
		    next_instruction(OP, State, State3);
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
		    next_instruction(OP, State, State3);
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
		    next_instruction(OP, State, State3);
		?SLOAD ->
		    %% 0x54 SLOAD δ=1 α=1
		    %% Load word from storage.
		    %% µ's[0] ≡ σ[Ia]s[µs[0]]
		    {Us0, State1} = pop(State0),
		    Val = aevm_eeevm_store:load(Us0, State1),
		    State2 = push(Val, State1),
		    next_instruction(OP, State, State2);
		?SSTORE ->
		    %% 0x55 SSTORE δ=2 α=0
		    %% Save word to storage.
		    %% σ'[Ia]s[µs[0]] ≡ µs[1]
		    %%                   Gsset if µs[1] =/= 0
		    %% CSSTORE(σ, µ)) ≡           ∧ σ[Ia]s[µs[0]] = 0
		    %%                   Gsreset otherwise
		    %% A'r ≡ Ar + Rsclear if µs[1] = 0
		    %%                      ∧ σ[Ia]s[µs[0]] =/= 0
		    %%       0 otherwise
		    {Address, State1} = pop(State0),
		    {Value, State2} = pop(State1),
		    State3 = aevm_eeevm_store:store(Address, Value, State2),
		    next_instruction(OP, State, State3);
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
			    next_instruction(OP, State, State2);
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
		    next_instruction(OP, State, State3);
		?PC ->
		    %% 0x58 PC δ=0 α=1
		    %% Get the value of the program counter prior to
		    %% the increment corresponding to this instruction.
		    %% µ's[0] ≡ µpc
		    State1 = push(CP, State0),
		    next_instruction(OP, State, State1);
		?MSIZE ->
		    %% 0x59 PC δ=0 α=1
		    %% Get the size of active memory in bytes.
		    %% µ's[0] ≡ 32*µi
		    Val =  32 * aevm_eeevm_memory:size_in_words(State),
		    State1 = push(Val, State0),
		    next_instruction(OP, State, State1);
		?GAS ->
		    %% 0x5a GAS δ=0 α=1
		    %% Get the amount of available gas,
		    %% including the corresponding reduction
		    %% for the cost of this instruction.
		    %% µ's[0] ≡ µg
		    Val = aevm_eeevm_state:gas(State0),
		    State1 = push(Val, State0),
		    next_instruction(OP, State, State1);
		?JUMPDEST ->
		    %% 0x5b JUMPDEST  δ=0 α=0
		    %% Mark a valid destination for jumps.
		    %% This operation has no effect on machine
		    %% state during execution.
		    next_instruction(OP, State, State0);
		16#5c -> throw({illegal_instruction, OP, State0});
		16#5d -> throw({illegal_instruction, OP, State0});
		16#5e -> throw({illegal_instruction, OP, State0});
		16#5f -> throw({illegal_instruction, OP, State0});
		%% 60s & 70s Push Operations
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
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH2 ->
		    %% 0x61 PUSH1 δ=0 α=1
		    %% Place 2 byte item on stack.
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH3 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH4 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH5 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH6 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH7 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH8 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH9 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH10 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH11 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH12 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH13 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH14 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH15 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH16 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH17 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH18 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH19 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH20 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH21 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH22 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH23 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH24 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH25 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH26 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH27 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH28 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH29 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH30 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH31 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?PUSH32 ->
                    State1 = push_n_bytes_from_cp(OP-?PUSH1+1, State0),
		    next_instruction(OP, State, State1);
		?DUP1 ->
		    %% 0x80 DUP1  δ=1 α=2
		    %% Duplicate 1nd stack item.
		    %% µ's[0] ≡ µs[0]
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP2 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP3 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP4 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP5 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP6 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP7 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP8 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP9 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP10 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP11 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP12 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP13 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP14 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP15 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?DUP16 ->
                    State1 = dup(OP -?DUP1+1,State0),
		    next_instruction(OP, State, State1);
		?SWAP1 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP2 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP3 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP4 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP5 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP6 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP7 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP8 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP9 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP10 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP11 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP12 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP13 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP14 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP15 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		?SWAP16 ->
                    State1 = swap(OP-?SWAP1+1, State0),
		    next_instruction(OP, State, State1);
		%% For all logging operations,
		%% the state change is to append an additional
		%% log entry on to the substate’s log series:
		%% A'l ≡ Al · (Ia, t, µm[µs[0] . . .(µs[0] + µs[1] − 1)])
		%% and to update the memory consumption counter:
		%% µ'i ≡ M(µi, µs[0], µs[1])
		%% The entry’s topic series, t, differs accordingly:
		?LOG0 ->
		    %% 0xa0 LOG0 δ=2 α=0
		    %% Append log record with no topics.
		    %% t ≡ ()
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    State3 = log({}, Us0, Us1, State2),
		    next_instruction(OP, State, State3);
		?LOG1 ->
		    %% 0xa1 LOG1 δ=3 α=0
		    %% Append log record with one topic.
		    %% t ≡ (µs[2])
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    {Us2, State3} = pop(State2),
		    State4 = log({Us2}, Us0, Us1, State3),
		    next_instruction(OP, State, State4);
		?LOG2 ->
		    %% 0xa2 LOG2 δ=4 α=0
		    %% Append log record with one topic.
		    %% t ≡ (µs[2],(µs[3])
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    {Us2, State3} = pop(State2),
		    {Us3, State4} = pop(State3),
		    State5 = log({Us2, Us3}, Us0, Us1, State4),
		    next_instruction(OP, State, State5);
		?LOG3 ->
		    %% 0xa3 LOG3 δ=4 α=0
		    %% Append log record with one topic.
		    %% t ≡ (µs[2], µs[3], µs[4])
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    {Us2, State3} = pop(State2),
		    {Us3, State4} = pop(State3),
		    {Us4, State5} = pop(State4),
		    State6 = log({Us2, Us3, Us4}, Us0, Us1, State5),
		    next_instruction(OP, State, State6);
		?LOG4 ->
		    %% 0xa4 LOG4 δ=6 α=0
		    %% Append log record with one topic.
		    %% t ≡ (µs[2], µs[3], µs[4], µs[5])
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    {Us2, State3} = pop(State2),
		    {Us3, State4} = pop(State3),
		    {Us4, State5} = pop(State4),
		    {Us5, State6} = pop(State5),
		    State7 = log({Us2, Us3, Us4, Us5}, Us0, Us1, State6),
		    next_instruction(OP, State, State7);
		OP when OP >= 16#a5,
			OP =< 16#ef  ->
		    throw({illegal_instruction, OP, State0});
		%% F0s: System operations
		?CREATE->
		    %% 0xf0 CREATE δ=3 α=1
		    %% Create a new account with associated code.
		    %% i ≡ µm[µs[1] . . .(µs[1] + µs[2] − 1)]
		    %% (σ', µ'g, A+) ≡ (Λ(σ∗, Ia, Io, L(µg), Ip, µs[0], i, Ie + 1)
		    %%                             if µs[0] =< σ[Ia]b ∧ Ie < 1024
		    %%                 (σ, µg, ∅)
		    %%                             otherwise
		    %% σ∗ ≡ σ except σ∗[Ia]n = σ[Ia]n + 1
		    %% A' ≡ A U A+ which implies:   A's ≡ As ∪ A+s
		    %%                            ∧ A'l ≡ Al · A+l
		    %%                            ∧ A'r ≡ Ar + A+r
		    %% µ's[0] ≡ x
		    %% where x = 0 if the code execution for this operation
		    %%                failed due to an exceptional halting
		    %%                Z(σ∗, µ, I) = T or Ie = 1024
		    %%                  (the maximum call depth limit is reached)
		    %%                or µs[0] > σ[Ia]b (balance of the caller is too
		    %%                                   low to fulfil the value transfer);
		    %%        x = A(Ia, σ[Ia]n), the address of the newly created account,
		    %%             otherwise.
		    %% µ'i ≡ M(µi, µs[1], µs[2])
		    %% Thus the operand order is: value, input offset, input size.
		    {Value, State1} = pop(State0),
		    {From, State2} = pop(State1),
		    {Size, State3} = pop(State2),
		    {CodeArea, State4} = aevm_eeevm_memory:get_area(From, Size, State3),
		    {X, State5} = create_account(Value, CodeArea, State4),
		    State6 = push(X, State5),
		    next_instruction(OP, State, State6);
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
		    %% TODO: This is probably completely wrong:
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
		    spend_mem_gas(State, State8);
		?CALLCODE ->
		    %% 0xf2 CALLCODE 7 1
		    %% Message-call into this account with an alternative account’s code.
		    %% Exactly equivalent to CALL except:
		    %%                    Θ(σ∗, Ia, Io, Ia, t, CCALLGAS(µ),
		    %%                    Ip, µs[2], µs[2], i, Ie + 1)
		    %% (σ', g0, A+, o) ≡     if µs[2] =< σ[Ia]b ∧ Ie < 1024
		    %%                    (σ, g, ∅,())
		    %%                       otherwise
		    %% Note the change in the fourth parameter to the call
		    %% Θ from the 2nd stack value µs[1] (as in CALL) to the
		    %% present address Ia. This means that the recipient is
		    %% in fact the same account as at present, simply that
		    %% the code is overwritten.
		    {Gas, State1} = pop(State0),
		    {_To, State2} = pop(State1),
		    {Value, State3} = pop(State2),
		    {IOffset, State4} = pop(State3),
		    {ISize, State5} = pop(State4),
		    {OOffset, State6} = pop(State5),
		    {OSize, State7} = pop(State6),
		    %% TODO: This is most certanly wrong
		    Call = #{ gas => Gas
			    , to => aevm_eeevm_state:address(State7)
			    , value => Value
			    , in_offset => IOffset
			    , in_size => ISize
			    , out_offset => OOffset
			    , out_size => OSize},
		    State8 = aevm_eeevm_state:set_call(Call, State7),
                    spend_mem_gas(State, State8);
		?RETURN ->
		    %% 0xf3 RETURN δ=2 α=0
		    %% Halt execution returning output data.
		    %% HRETURN(µ) ≡ µm[µs[0] . . .(µs[0] + µs[1] − 1)]
		    %% This has the effect of halting the execution
		    %% at this point with output defined.
		    %% µ'i ≡ M(µi, µs[0], µs[1]) TODO: This
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    {Out, State3} = aevm_eeevm_memory:get_area(Us0, Us1, State2),
		    State4 = aevm_eeevm_state:set_out(Out, State3),
                    spend_mem_gas(State, State4);
		?DELEGATECALL ->
		    %% 0xf4 DELEGATECALL  δ=6 α=1
		    %% Message-call into this account with an
		    %% alternative account’s code, but persisting
		    %% the current values for sender and value.
		    %% Compared with CALL, DELEGATECALL takes one fewer arguments.
		    %% The omitted argument is µs[2].
		    %% As a result, µs[3], µs[4], µs[5] and µs[6]
		    %% in the definition of CALL
		    %% should respectively be replaced with µs[2], µs[3], µs[4] and µs[5].
		    %% Otherwise exactly equivalent to CALL except:
		    %%                    Θ(σ∗,Is,Io,Ia,t,µs[0],Ip,0,Iv,i,Ie + 1)
		    %%                      if Iv 6 σ[Ia]b ∧ Ie < 1024
		    %% (σ', g0, A+, o) ≡ 
		    %%                    (σ, g, ∅,())
		    %%                      otherwise
		    %% Note the changes (in addition to that of the fourth parameter)
		    %% to the second and ninth parameters to the call Θ.
		    %% This means that the recipient is in fact the same account as at
		    %% present, simply that the code is overwritten and the context is
		    %% almost entirely identical.
		    %% TODO: This is probably completely wrong:
		    {Gas, State1} = pop(State0),
		    {To, State2} = pop(State1),
		    {IOffset, State3} = pop(State2),
		    {ISize, State4} = pop(State3),
		    {OOffset, State5} = pop(State4),
		    {OSize, State6} = pop(State5),
		    Call = #{ gas => Gas
			    , to => To
			    , in_offset => IOffset
			    , in_size => ISize
			    , out_offset => OOffset
			    , out_size => OSize},
		    State7 = aevm_eeevm_state:set_call(Call, State6),
		    spend_mem_gas(State, State7);
		16#f5 -> throw({illegal_instruction, OP, State0});
		16#f6 -> throw({illegal_instruction, OP, State0});
		16#f7 -> throw({illegal_instruction, OP, State0});
		16#f8 -> throw({illegal_instruction, OP, State0});
		16#f9 -> throw({illegal_instruction, OP, State0});
		16#fa -> throw({illegal_instruction, OP, State0});
		16#fb -> throw({illegal_instruction, OP, State0});
		16#fc -> throw({illegal_instruction, OP, State0});
		?REVERT -> 
		    %% 0xfe REVERT δ=2 α=0
		    %% Halt execution returning output data.
		    %% HREVERT(µ) ≡ µm[µs[0] . . .(µs[0] + µs[1] − 1)]
		    %% This has the effect of halting the execution
		    %% at this point with output defined.
		    %% µ'i ≡ M(µi, µs[0], µs[1]) TODO: Return error code
		    {Us0, State1} = pop(State0),
		    {Us1, State2} = pop(State1),
		    {Out, State3} = aevm_eeevm_memory:get_area(Us0, Us1, State2),
		    State4 = aevm_eeevm_state:set_out(Out, State3),
                    spend_mem_gas(State, State4);
		?INVALID -> 
		    %% 0xfe INVALID δ=∅ α=∅ 
		    %% Designated invalid instruction.
		    throw({the_invalid_instruction, OP, State0});
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
		    State2 = aevm_eeevm_state:set_selfdestruct(Us0, State1),
                    spend_mem_gas(State, State2);
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
    aevm_eeevm_stack:push(Arg, State).

push_n_bytes_from_cp(N, State) ->
    CP   = aevm_eeevm_state:cp(State),
    Code = aevm_eeevm_state:code(State),
    Arg  = code_get_arg(CP+1, N, Code),
    State1 = aevm_eeevm_stack:push(Arg, State),
    inc_cp(N, State1).

pop(State) ->
    aevm_eeevm_stack:pop(State).

dup(N, State) ->
    aevm_eeevm_stack:dup(N, State).

swap(N, State) ->
    aevm_eeevm_stack:swap(N, State).

%% ------------------------------------------------------------------------
%% DATA
%% ------------------------------------------------------------------------

%% Get a 256-bit (32 bytes) word from input data.
data_get_val(Address, State) ->
    Data = aevm_eeevm_state:data(State),
    <<Val:256>> = aevm_eeevm_utils:bin_copy(Address, 32, Data),
    Val.

%% Get a binary of size Size bytes from input data.
data_get_bytes(Address, Size, State) ->
    Data = aevm_eeevm_state:data(State),
    try aevm_eeevm_utils:bin_copy(Address, Size, Data)
    catch error:system_limit ->
	    throw({out_of_memory, State})
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
code_get_arg(CP, Size, Code) when Size < 33 ->
    BitSize = Size * 8,
    <<Arg:BitSize>> = aevm_eeevm_utils:bin_copy(CP, Size, Code),
    Arg.

code_get_area(From, Size, Code) ->
    aevm_eeevm_utils:bin_copy(From, Size, Code).

next_instruction(_OP, StateWithOpGas, StateOut) ->
    loop(inc_cp(spend_mem_gas(StateWithOpGas, StateOut))).

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

spend_op_gas(Op, State) ->
    spend_gas_common(aevm_gas:op_cost(Op, State), State).

spend_mem_gas(StateWithOpGas, StateOut) ->
    spend_gas_common(aevm_gas:mem_cost(StateWithOpGas, StateOut), StateOut).

spend_gas_common(Cost, State) ->
    Gas  = aevm_eeevm_state:gas(State),
    case Gas >= Cost of
	true ->  aevm_eeevm_state:set_gas(Gas - Cost, State);
	false -> throw({out_of_gas, State})
    end.

%% ------------------------------------------------------------------------
%% LOGS
%% ------------------------------------------------------------------------
%%
%% TODO: Should account address be 160 or 256 bits?
%% TODO: Implement log bloom filter/.. q
%%
%% The transaction receipt is a tuple of four items comprising
%% the post-transaction state, Rσ, the cumulative gas
%% used in the block containing the transaction receipt as of
%% immediately after the transaction has happened, Ru, the
%% set of logs created through execution of the transaction, Rl
%% and the Bloom filter composed from information in those
%% logs, Rb:
%% (18) R ≡ (Rσ, Ru, Rb, Rl)
%% The function LR trivially prepares a transaction receipt
%% for being transformed into an RLP-serialised byte array:
%% (19) LR(R) ≡ (TRIE(LS(Rσ)), Ru, Rb, Rl)
%% thus the post-transaction state, Rσ is encoded into a trie
%% structure, the root of which forms the first item.
%% We assert Ru, the cumulative gas used is a positive integer
%% and that the logs Bloom, Rb, is a hash of size 2048
%% bits (256 bytes):
%% (20) Ru ∈ P ∧ Rb ∈ B256
%% The log entries, Rl, is a series of log entries, termed,
%% for example, (O0, O1, ...). A log entry, O, is a tuple of a
%% logger’s address, Oa, a series of 32-bytes log topics, Ot
%% and some number of bytes of data, Od:
%% (21) O ≡ (Oa,(Ot0, Ot1, ...), Od)
%% (22) Oa ∈ B20 ∧ ∀t∈Ot : t ∈ B32 ∧ Od ∈ B
%% We define the Bloom filter function, M, to reduce a log
%% entry into a single 256-byte hash:
%% (23) M(O) ≡ V_(t∈{Oa}∪Ot) (M3:2048(t))
%% where M3:2048 is a specialised Bloom filter that sets
%% three bits out of 2048, given an arbitrary byte sequence.
%% It does this through taking the low-order 11 bits of each
%% of the first three pairs of bytes in a Keccak-256 hash of
%% the byte sequence. Formally:
%% (24) M3:2048(x : x ∈ B) ≡ y : y ∈ B256 where:
%% (25) y = (0, 0, ..., 0) except:
%% (26) ∀i∈{0,2,4} : Bm(x,i)(y) = 1
%% (27) m(x, i) ≡ KEC(x)[i, i + 1] mod 2048
%% where B is the bit reference function such that Bj (x)
%% equals the bit of index j (indexed from 0) in the byte array x.

log(Topics, MemAddress, Length, State) ->
    Logs = aevm_eeevm_state:logs(State),
    AccountAddress = aevm_eeevm_state:address(State),
    Header = log_topics(AccountAddress, Topics),
    {Body, State1} = aevm_eeevm_memory:get_area(
		       MemAddress, Length, State),
    LogEntry = <<Header/binary, Body/binary>>,
    NewLogs = [LogEntry|Logs],
    aevm_eeevm_state:set_logs(NewLogs, State1).


log_topics(AccountAddress, Topics) ->
    Bytes = << << X:256>> || X <- tuple_to_list(Topics) >>,
    <<AccountAddress:256, Bytes/binary >>.


%% ------------------------------------------------------------------------
%% Account Functions
%% ------------------------------------------------------------------------

create_account(_Value, _CodeArea, State) ->
    %% TODO: Do actual account creation
    {16#DEADC0DE, State}.
