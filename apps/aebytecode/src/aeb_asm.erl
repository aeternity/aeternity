%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Assembler for aevm machine code
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

format(Asm) ->
    Asm.


file(Filename,_Opts) ->
    {ok, File} = file:read_file(Filename),
    {ok, Tokens, _} = aeb_asm_scan:scan(binary_to_list(File)),
    io:format("Tokens ~p~n",[Tokens]),
    ByteList = to_bytecode(Tokens, 0, #{}, []),
    list_to_binary(ByteList).

to_hexstring(ByteList) ->
    "0x" ++ lists:flatten(
              [io_lib:format("~2.16.0b", [X])
               || X <- ByteList]).


to_bytecode([{mnemonic,_line, Op}|Rest], Address, Env, Code) ->
    OpCode = aeb_opcodes:m_to_op(Op),
    OpSize = aeb_opcodes:op_size(OpCode),
    to_bytecode(Rest, Address + OpSize, Env, [OpCode|Code]);
to_bytecode([{int,_line, Int}|Rest], Address, Env, Code) ->
    to_bytecode(Rest, Address, Env, [Int|Code]);
to_bytecode([{hash,_line, Hash}|Rest], Address, Env, Code) ->
    to_bytecode(Rest, Address, Env, [Hash|Code]);
to_bytecode([{id,_line, ID}|Rest], Address, Env, Code) ->
    to_bytecode(Rest, Address, Env, [{ref, ID}|Code]);
to_bytecode([{lable,_line, Lable}|Rest], Address, Env, Code) ->
    to_bytecode(Rest, Address, Env#{Lable => Address}, Code);
to_bytecode([], _Address, Env, Code) ->
    io:format("Code ~p~n", [lists:reverse(Code)]),
    PatchedCode = resolve_refs(Code, Env, []),
    io:format("PatchedCode ~p~n", [PatchedCode]),
    expand_args(PatchedCode).

%% Also reverses the code.
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

to_bytecode([Op|Rest], Options) ->
    [aeb_opcodes:m_to_op(Op)|to_bytecode(Rest, Options)];
to_bytecode([], _) -> [].
