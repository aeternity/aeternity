%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Compiler from Aeterinty Ring language to the Aeternity VM, aevm.
%%% @end
%%% Created : 12 Dec 2017
%%% aec_conductor:stop_mining().
%%% aer_compiler:file( identity, [pp_ast,pp_icode,pp_assembler,pp_bytecode, pp_ring_code]).
%%%-------------------------------------------------------------------
-module(aer_compiler).

-export([ file/1
        , file/2
        , from_string/2]).

-export([test/0]).

-type option() :: pp_ring_code | pp_ast | pp_icode | pp_assembler | pp_bytecode.
-type options() :: [option()].

-export_type([ option/0
             , options/0
             ]).

file(Filename) ->
    file(Filename, []).

file(Filename, Options) ->
    C = read_contract(Filename),
    from_string(C, Options).

from_string(ContractString, Options) ->
    ok = pp_ring_code(ContractString, Options),
    Ast = parse(ContractString, Options),
    ok = pp_ast(Ast, Options),
    ICode = to_icode(Ast, Options),
    ok = pp_icode(ICode, Options),
    Assembler =  assemble(ICode, Options),
    ok = pp_assembler(Assembler, Options),
    ByteCodeList = to_bytecode(Assembler, Options),
    ByteCode = << << B:8 >> || B <- ByteCodeList >>,
    ok = pp_bytecode(ByteCode, Options),
    ByteCode.

parse(C,_Options) ->
    parse_string(C).
    
to_icode(Ast, Options) ->
    aer_ast_to_icode:convert(Ast, Options).

assemble(Icode, Options) ->
    aer_icode_to_asm:convert(Icode, Options).


to_bytecode(['COMMENT',_|Rest],_Options) ->
    to_bytecode(Rest,_Options);
to_bytecode([Op|Rest], Options) ->
    [aeb_opcodes:m_to_op(Op)|to_bytecode(Rest, Options)];
to_bytecode([], _) -> [].



pp_ring_code(C, Opts)->  pp(C, Opts, pp_ring_code,
                            fun (X) -> io:format("~s~n",[X]) end).
pp_ast(C, Opts)      ->  pp(C, Opts, pp_ast, fun aer_ast:pp/1).
pp_icode(C, Opts)    ->  pp(C, Opts, pp_icode, fun aer_icode:pp/1).
pp_assembler(C, Opts)->  pp(C, Opts, pp_assembler, fun aeb_asm:pp/1).
pp_bytecode(C, Opts) ->  pp(C, Opts, pp_bytecode, fun aeb_disassemble:pp/1).

pp(Code, Options, Option, PPFun) ->
    case proplists:lookup(Option, Options) of
        {Option, true} ->
            PPFun(Code);
        none ->
            ok
    end.
    

%% -------------------------------------------------------------------
%% TODO: Tempoary parser hook below...

parse_string(Text) ->
    case aer_parser:string(Text) of
        {ok, Contract} -> Contract;
        Err = {error, {Line, aer_scan, Reason}} ->
            io:format("Lexical error at line ~p:\n  ~s\n", [Line, Reason]),
            error(Err);
        Err = {error, {Line, aer_parser, Reason}} ->
            io:format("Parse error at line ~p:\n  ~s\n", [Line, Reason]),
            error(Err)
    end.

read_contract(Name) ->
    {ok, Bin} = file:read_file(filename:join(contract_path(), lists:concat([Name, ".aer"]))),
    binary_to_list(Bin).

contract_path() ->
    "apps/aering/test/contracts".


solidity_id_contract() ->
    binint_to_bin(<< "0x606060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000" >>).

test() ->
    io:format(aeb_disassemble:pp(solidity_id_contract()),[]).

 
binint_to_bin(<<"0x", Bin/binary>>) ->
    << <<(hex_to_int(X)):4>> || <<X:8>> <= Bin>>;
binint_to_bin(<<"0", _/binary>> = Bin) ->
    %% Don't know what to do.
    %% Is this an attempt to pad?
    error({unexpected, Bin});
binint_to_bin(Bin) when is_binary(Bin) ->
    Int = binary_to_integer(Bin),
    binary:encode_unsigned(Int).

hex_to_int(X) when $A =< X, X =< $F -> 10 + X - $A;
hex_to_int(X) when $a =< X, X =< $f -> 10 + X - $a;
hex_to_int(X) when $0 =< X, X =< $9 -> X - $0.
