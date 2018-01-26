%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Assembler for aevm machine code.
%%%
%%%      Assembler code can be read from a file.
%%%      The assembler has the following format
%%%      Comments start with 2 semicolons and runs till end of line
%%%         ;; This is a comment
%%%      Opcode mnemonics start with an upper case letter.
%%%         DUP1
%%%      Identifiers start with a lower case letter
%%%         an_identifier
%%%      All identifiers has to be decleard with a label.
%%%      A label is an identifier that ends with a colon.
%%%      The label is the byte code address of the next instruction.
%%%         a_label:
%%%      Immediates can be of four types:
%%%       1. integers
%%%          42
%%%       2. hexadecimal integers starting with 0x
%%%          0x0deadbeef0
%%%       3. 256-bit hash strings starting with #
%%%          followed by up to 64 hex chars
%%%          #00000deadbeef
%%%       4. labels as descibed above. 
%%%
%%% @end
%%% Created : 21 Dec 2017
%%%-------------------------------------------------------------------

-module(aeb_asm).

-export([ file/2
        , pp/1
        , to_hexstring/1
        ]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").


pp(Asm) ->
    Listing = format(Asm),
    io:format("~p~n", [Listing]).

format(Asm) -> format(Asm, 0).

format([Mnemonic | Rest], Address) ->
    Op = aeb_opcodes:m_to_op(Mnemonic),
    case (Op >= ?PUSH1) andalso (Op =< ?PUSH32) of
        true ->
            Arity = aeb_opcodes:op_size(Op) - 1,
            {Args, Code} = get_args(Arity, Rest),
            "        " ++ atom_to_list(Mnemonic)
                ++ "        " ++ Args ++"\n" 
                ++ format(Code, Address + Arity + 1);
        false ->
            "        " ++ atom_to_list(Mnemonic)
                ++ format(Rest, Address + 1)
    end;
format([],_) -> [].

%% TODO: Are args encoded as one list element or as a number of bytes...
get_args(1, [Arg|Code]) ->
    {integer_to_list(Arg), Code};
get_args(N, [Arg|Code]) ->
    {Args, Rest} = get_args(N-1, Code),
    {integer_to_list(Arg) ++ ", " ++ Args, Rest}.



file(Filename, Options) ->
    {ok, File} = file:read_file(Filename),
    {ok, Tokens, _} = aeb_asm_scan:scan(binary_to_list(File)),

    case proplists:lookup(pp_tokens, Options) of
        {pp_tokens, true} ->
            io:format("Tokens ~p~n",[Tokens]);
        none ->
            ok
    end,

    ByteList = to_bytecode(Tokens, 0, #{}, [], Options),

    case proplists:lookup(pp_hex_string, Options) of
        {pp_hex_string, true} ->
            io:format("Code: ~s~n",[to_hexstring(ByteList)]);
        none ->
            ok
    end,


    list_to_binary(ByteList).

to_hexstring(ByteList) ->
    "0x" ++ lists:flatten(
              [io_lib:format("~2.16.0b", [X])
               || X <- ByteList]).


to_bytecode([{mnemonic,_line, Op}|Rest], Address, Env, Code, Opts) ->
    OpCode = aeb_opcodes:m_to_op(Op),
    OpSize = aeb_opcodes:op_size(OpCode),
    to_bytecode(Rest, Address + OpSize, Env, [OpCode|Code], Opts);
to_bytecode([{int,_line, Int}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env, [Int|Code], Opts);
to_bytecode([{hash,_line, Hash}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env, [Hash|Code], Opts);
to_bytecode([{id,_line, ID}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env, [{ref, ID}|Code], Opts);
to_bytecode([{label,_line, Label}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env#{Label => Address}, Code, Opts);
to_bytecode([], _Address, Env, Code, Opts) ->
    case proplists:lookup(pp_opcodes, Opts) of
        {pp_opcodes, true} ->
            io:format("opcodes ~p~n", [lists:reverse(Code)]);
        none ->
            ok
    end,

    PatchedCode = resolve_refs(Code, Env, []),
    case proplists:lookup(pp_patched_code, Opts) of
        {pp_patched_code, true} ->
            io:format("Patched Code: ~p~n", [PatchedCode]);
        none ->
            ok
    end,

    expand_args(PatchedCode).

%% Also reverses the code (back to unreversed state).
resolve_refs([{ref, ID} | Rest], Env, Code) ->
    Address = maps:get(ID, Env),
    resolve_refs(Rest, Env, [Address | Code]);
resolve_refs([Op | Rest], Env, Code) ->
    resolve_refs(Rest, Env, [Op | Code]);
resolve_refs([],_Env, Code) -> Code.

expand_args([OP, Arg | Rest]) when OP >= ?PUSH1 andalso OP =< ?PUSH32 ->
    BitSize = (aeb_opcodes:op_size(OP) - 1) * 8,
    Bin = << << X:BitSize>> || X <- [Arg] >>,
    ArgByteList = binary_to_list(Bin),
    [OP | ArgByteList] ++ expand_args(Rest);
expand_args([OP | Rest]) ->
    [OP | expand_args(Rest)];
expand_args([]) -> [].
