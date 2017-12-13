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

ast_code_to_icode([{type_def, Attrib, _, _, _}|Rest], Icode) ->
    %% TODO: Handle types
    ast_code_to_icode(Rest, Icode);
ast_code_to_icode([{letfun, Attrib, Name, Args, What, Body}|Rest], Icode) ->
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
                                 

ast_body({id, _, Name}, Icode) ->
    %% TODO Look up id in env
    ['DUP'].
    

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
