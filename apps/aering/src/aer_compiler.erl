%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Compiler from Aeterinty Ring language to the Aeternity VM, aevm.
%%% @end
%%% Created : 12 Dec 2017
%%%-------------------------------------------------------------------
-module(aer_compiler).

-export([ file/1
        , file/2]).

file(Filename) ->
    file(Filename, []).

file(Filename, Options) ->
    Ast = parse(Filename, Options),
    ByteCode = compile(Ast, Options),
    ByteCode.
    

parse(_Filename,_Options) ->
     parse_string(read_contract(counter)).
    
compile(Ast,_Options) ->
    Ast.



%% TODO: Tempoary parser hook below...

scan_string(Text) ->
    case aer_scan:string(Text) of
        {ok, Tokens, _} -> Tokens;
        Err = {error, {Line, aer_scan, {user, Reason}}, _} ->
            io:format("Lexical error at line ~p:\n  ~s\n", [Line, Reason]),
            error(Err)
    end.

parse_string(Text) ->
    Tokens = scan_string(Text),
    case aer_parser:parse(Tokens) of
        {ok, Contract} -> Contract;
        Err = {error, {Line, aer_parser, Reason}} ->
            io:format("Parse error at line ~p:\n  ~s\n", [Line, Reason]),
            error(Err)
    end.

parse_expr(Text) ->
    {contract, _, _, _, [{'fun', _, _, _, _, Expr}]} =
        parse_string("contract dummy\nfun expr _ = " ++ Text),
    Expr.

read_contract(Name) ->
    {ok, Bin} = file:read_file(filename:join(contract_path(), lists:concat([Name, ".aer"]))),
    binary_to_list(Bin).

contract_path() ->
    "apps/aering/test/contracts".
