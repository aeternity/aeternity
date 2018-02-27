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
    TypedTree = aer_ast_infer_types:infer(Tree),
    [io:format("Typed tree:\n  ~p\n",[TypedTree]) || lists:member(pp_typed,Options)],
    code(TypedTree,
%%    code(Tree,
	 #{ functions => []
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
contract_to_icode([{letfun,_Attrib, Name, Args, _What, Body={typed,_,_,T}}|Rest], Icode) ->
    %% TODO: Handle types
    FunName = ast_id(Name),
    %% TODO: push funname to env
    FunArgs = ast_args(Args, []),
    %% TODO: push args to env
    FunBody = ast_body(Body),
    TypeRep = ast_typerep(T),
    NewIcode = ast_fun_to_icode(FunName, FunArgs, FunBody, TypeRep, Icode),
    contract_to_icode(Rest, NewIcode);
contract_to_icode([{letrec,_,Defs}|Rest], Icode) ->
    %% OBS! This code ignores the letrec structure of the source,
    %% because the back end treats ALL declarations as recursive! We
    %% need to decide whether to (a) modify the back end to respect
    %% the letrec structure, or (b) (preferably) modify the front end
    %% just to parse a list of (mutually recursive) definitions.
    contract_to_icode(Defs++Rest, Icode);
contract_to_icode([], Icode) -> Icode;
contract_to_icode(Code, Icode) ->
    io:format("Unhandled code ~p~n",[Code]),
    Icode.

ast_id({id, _, Id}) -> Id.

ast_args([{arg, _, Name, Type}|Rest], Acc) ->
    ast_args(Rest, [{ast_id(Name), ast_type(Type)}| Acc]);
ast_args([], Acc) -> lists:reverse(Acc).

%% ICode is untyped, surely?
ast_type(T) ->
    T.

ast_body({id, _, Name}) ->
    %% TODO Look up id in env
    #var_ref{name = Name};
ast_body({int, _, Value}) ->
    #integer{value = Value};
ast_body({string,_,Bin}) ->
    Cpts = [size(Bin)|aer_data:binary_to_words(Bin)],
    #tuple{cpts = [#integer{value=X} || X <- Cpts]};
ast_body({tuple,_,Args}) ->
    #tuple{cpts = [ast_body(A) || A <- Args]};
ast_body({list,_,Args}) ->
    #list{elems = [ast_body(A) || A <- Args]};
ast_body({app,[_,{format,prefix}],{Op,_},[A]}) ->
    #unop{op = Op, rand = ast_body(A)};
ast_body({app,[_,{format,infix}],{Op,_},[A,B]}) ->
    #binop{op = Op, left = ast_body(A), right = ast_body(B)};
ast_body({app,_,Fun,Args}) ->
    #funcall{function=ast_body(Fun),
	     args=[ast_body(A) || A <- Args]};
ast_body({'if',_,Dec,Then,Else}) ->
    #ifte{decision = ast_body(Dec)
	 ,then     = ast_body(Then)
	 ,else     = ast_body(Else)};
ast_body({switch,_,A,Cases}) ->
    %% let's assume the parser has already ensured that only valid
    %% patterns appear in cases.
    #switch{expr=ast_body(A),
	    cases=[{ast_body(Pat),ast_body(Body)}
		   || {'case',_,Pat,Body} <- Cases]};
ast_body({block,As,[{letval,_,Pat,_,E}|Rest]}) ->
    #switch{expr=ast_body(E),
	    cases=[{ast_body(Pat),ast_body({block,As,Rest})}]};
ast_body({block,_,[]}) ->
    #tuple{cpts=[]};
ast_body({block,_,[E]}) ->
    ast_body(E);
ast_body({block,As,[E|Rest]}) ->
    #switch{expr=ast_body(E),
	    cases=[{#var_ref{name="_"},ast_body({block,As,Rest})}]};
ast_body({lam,_,Args,Body}) ->
    #lambda{args=[{ast_id(P),ast_type(T)} || {arg,_,P,T} <- Args],
	    body=ast_body(Body)};
ast_body({typed,_,{record,Attrs,Fields},{record_t,DefFields}}) ->
    %% Compile as a tuple with the fields in the order they appear in the definition.
    NamedFields = [{Name,E} || {field,_,{id,_,Name},E} <- Fields],
    #tuple{cpts =
	       [case proplists:get_value(Name,NamedFields) of
		    undefined ->
			[{line,Line}] = Attrs,
			#missing_field{format = "Missing field in record: ~s (on line ~p)\n",
				       args = [Name,Line]};
		    E ->
			ast_body(E)
		end
		|| {field_t,_,_,{id,_,Name},_} <- DefFields]};
ast_body({typed,_,{record,Attrs,Fields},T}) ->
    error({record_has_bad_type,Attrs,T});
ast_body({proj,_,{typed,_,Record,{record_t,Fields}},{id,_,FieldName}}) ->
    [Index] = [I
	       || {I,{field_t,_,_,{id,_,Name},_}} <-
		      lists:zip(lists:seq(1,length(Fields)),Fields),
		  Name==FieldName],
    #binop{op = '!', left = #integer{value = 32*(Index-1)}, right = ast_body(Record)};
ast_body({record,Attrs,{typed,_,Record,RecType={record_t,Fields}},Update}) ->
    UpdatedNames = [Name || {field,_,{id,_,Name},_} <- Update],
    #switch{expr=ast_body(Record),
	    cases=[{#var_ref{name = "_record"},
		    ast_body({typed,Attrs,
			      {record,Attrs,
			       Update ++
				   [{field,Attrs,{id,Attrs,Name},
				     {proj,Attrs,
				      {typed,Attrs,{id,Attrs,"_record"},RecType},
				      {id,Attrs,Name}}}
				    || {field_t,_,_,{id,_,Name},_} <- Fields,
				       not lists:member(Name,UpdatedNames)]},
			      RecType})}
		   ]};
ast_body({typed, _, Body, _}) ->
    ast_body(Body).

ast_typerep({id,_,"int"}) ->
    word;
ast_typerep({id,_,"string"}) ->
    string;
ast_typerep({tvar,_,_}) ->
    %% We serialize type variables just as addresses in the originating VM.
    word;
ast_typerep({tuple_t,_,Cpts}) ->
    #tuple{cpts = [ast_typerep(C) || C<-Cpts]};
ast_typerep({record_t,Fields}) ->
    #tuple{cpts = [ast_typerep(T) || {field_t,_,_,_,T} <- Fields]};
ast_typerep({app_t,_,{id,_,"list"},[Elem]}) ->
    #list{elems=[ast_typerep(Elem)]}; %% Reusing value-level lists
ast_typerep({fun_t,_,_,_}) ->
    function.



ast_fun_to_icode(Name, Args, Body, TypeRep, #{functions := Funs} = Icode) ->
    NewFuns = [{Name, Args, Body, TypeRep}| Funs],
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
