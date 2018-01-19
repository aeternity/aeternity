%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Compiler from Aeterinty Ring language to the Aeternity VM, aevm.
%%% @end
%%% Created : 21 Dec 2017
%%% 
%%%-------------------------------------------------------------------
-module(aer_ast_to_icode).

-export([convert/2]).

-include("aer_icode.hrl").

convert(Tree, Options) ->
    code(Tree,  #{ functions => []
                 , env => []
                 , options => Options}).

code([{contract_type,_Attribs, {con, _, Name}, _TypeDecls}|Rest], Icode) ->
    %% TODO: Handle types for future type check.
    code(Rest, set_name(Name, Icode));
code([{contract, Attribs, {con, _, Name}, Code}|Rest], Icode) ->
    try get_name(Icode) of
        Name ->
            NewIcode = contract_to_icode(Code, Icode),
            code(Rest, NewIcode);
        _ -> error({contract_name_mismatch, get_line(Attribs)})
    catch
        error:name_not_defined ->
            error({contract_name_not_defined, get_line(Attribs)})
    end;
code([], Icode) -> Icode.

contract_to_icode([{type_def,_Attrib, _, _, _}|Rest], Icode) ->
    %% TODO: Handle types
    contract_to_icode(Rest, Icode);
contract_to_icode([{letfun,_Attrib, Name, Args,_What, Body}|Rest], Icode) ->
    %% TODO: Handle types
    FunName = ast_id(Name),
    %% TODO: push funname to env
    FunArgs = ast_args(Args, []),
    %% TODO: push args to env
    FunBody = ast_body(Body, Icode),
    NewIcode = ast_fun_to_icode(FunName, FunArgs, FunBody, Icode),
    contract_to_icode(Rest, NewIcode);
contract_to_icode([], Icode) -> Icode;
contract_to_icode(Code, Icode) ->
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
