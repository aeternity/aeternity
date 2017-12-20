%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Compiler from Aeterinty Ring language to the Aeternity VM, aevm.
%%% @end
%%% Created : 12 Dec 2017
%%% aec_conductor:stop_mining(),aer_compiler:file(a,c).
%%%-------------------------------------------------------------------
-module(aer_compiler).

-export([ file/1
        , file/2]).

-export([test/0]).

-include("aer_icode.hrl").

file(Filename) ->
    file(Filename, []).

file(Filename, Options) ->
    Ast = parse(Filename, Options),
    ByteCode = compile(Ast, Options),
    ByteCode.


parse(_Filename,_Options) ->
     parse_string(read_contract(identity)).
    
compile(Ast,_Options) ->
    ast_to_icode(Ast, #{functions => [], env => []}).

ast_to_icode([{contract_type,_Attribs, {con, _, Name}, _TypeDecls}|Rest], Icode) ->
    %% TODO: Handle types for future type check.
    ast_to_icode(Rest, set_name(Name, Icode));
ast_to_icode([{contract, Attribs, {con, _, Name}, Code}|Rest], Icode) ->
    try get_name(Icode) of
        Name ->
            NewIcode = ast_code_to_icode(Code, Icode),
            ast_to_icode(Rest, NewIcode);
        _ -> error({contract_name_mismatch, get_line(Attribs)})
    catch
        error:name_not_defined ->
            error({contract_name_not_defined, get_line(Attribs)})
    end;
ast_to_icode([], Icode) -> Icode.

ast_code_to_icode([{type_def,_Attrib, _, _, _}|Rest], Icode) ->
    %% TODO: Handle types
    ast_code_to_icode(Rest, Icode);
ast_code_to_icode([{letfun,_Attrib, Name, Args,_What, Body}|Rest], Icode) ->
    %% TODO: Handle types
    FunName = ast_id(Name),
    %% TODO: push funname to env
    FunArgs = ast_args(Args, []),
    %% TODO: push args to env
    FunBody = ast_body(Body, Icode),
    NewIcode = ast_fun_to_icode(FunName, FunArgs, FunBody, Icode),
    ast_code_to_icode(Rest, NewIcode);
ast_code_to_icode([], Icode) -> Icode;
ast_code_to_icode(Code, Icode) ->
    io:format("Unhandled code ~p~n",[Code]),
    Icode.

ast_id({id, _, Id}) -> Id.

ast_args([{arg, _, Name, Type}|Rest], Acc) ->
    ast_args(Rest, [{ast_id(Name), ast_id(Type)}| Acc]);
ast_args([], Acc) -> lists:reverse(Acc).
                                 

ast_body({id, _, Name},_Icode) ->
    %% TODO Look up id in env
    #var_ref{name = Name}.
    

ast_fun_to_icode(Name, Args, Body, #{functions := Funs} = Icode) ->
    NewFuns = [{Name, Args, Body}| Funs],
    set_functions(NewFuns, Icode).

%% -------------------------------------------------------------------
%% AST
%% -------------------------------------------------------------------
get_line(Attribs) -> %% TODO: use AST primitives.
     proplists:get_value(line, Attribs).
                    

%% -------------------------------------------------------------------
%% Icode
%% -------------------------------------------------------------------
set_name(Name, Icode) ->
    maps:put(contract_name, Name, Icode).

get_name(#{contract_name := Name}) -> Name;
get_name(_) -> error(name_not_defined).
    
set_functions(NewFuns, Icode) ->
    maps:put(functions, NewFuns, Icode).

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

%% parse_expr(Text) ->
%%     [{contract, _, _, [{letval, _, _, _, Expr}]}] =
%%         parse_string("contract Dummy = { let _ = " ++ Text ++ "}"),
%%     Expr.

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
