-module(aer_ast_infer_types).

-export([infer/1]).

infer([ContractType={contract_type,_,_,_}|Rest]) ->
    %% ignore contract types for now
    [ContractType|infer(Rest)];
infer([{contract,Attribs,ConName,Code}|Rest]) ->
    %% do type inference on each contract independently.
    [{contract,Attribs,ConName,infer_contract(Code)}|infer(Rest)];
infer([]) ->
    [].

infer_contract(Defs) ->
    infer_contract([],Defs).

%% infer_contract takes a proplist mapping global names to types, and
%% a list of definitions.  
infer_contract(Env,[Def={type_def,_,_,_,_}|Rest]) ->
    %% TODO: handle type defs
    [Def|infer_contract(Env,Rest)];
infer_contract(Env,[Fun={letfun,_,_,_,_,_}|Rest]) ->
    {TypeSig,NewFun} = infer_letfun(Env,Fun),
    [NewFun|infer_contract([TypeSig|Env],Rest)];
infer_contract(_,[]) ->
    [].

infer_letfun(Env,{letfun,Attrib,{id,NameAttrib,Name},Args,What,Body}) ->
    ets:new(type_vars,[set,named_table,public]),
    ArgTypes  = [{ArgName,arg_type(T)} || {arg,_,{id,_,ArgName},T} <- Args],
    NewBody={typed,_,_,ResultType} = infer_expr(ArgTypes++Env,Body),
    unify(ResultType,arg_type(What)),
    NewArgs = [{arg,A1,{id,A2,ArgName},instantiate(T)}
	       || {{ArgName,T},{arg,A1,{id,A2,ArgName},_}} <- lists:zip(ArgTypes,Args)],
    NewWhat = instantiate(ResultType),
    IBody = instantiate(NewBody),
    ets:delete(type_vars),
    TypeSig = {type_sig,[T || {arg,_,_,T} <- NewArgs],NewWhat},
    io:format("Inferred ~p : ~s\n",[Name,pp(TypeSig)]),
    {{Name,TypeSig},
     {letfun,Attrib,{id,NameAttrib,Name},NewArgs,NewWhat,IBody}}.

arg_type({id,Attrs,"_"}) ->
    fresh_uvar(Attrs);
arg_type({app_t,Attrs,Name,Args}) ->
    {app_t,Attrs,Name,[arg_type(T) || T <- Args]};
arg_type(T) ->
    T.

infer_expr(_Env,Body={int,As,_}) ->
    {typed,As,Body,{id,As,"int"}};
infer_expr(Env,Body={id,As,Name}) ->
    case proplists:get_value(Name,Env) of
	undefined ->
	    io:format("Unbound variable: ~p\n",[Name]),
	    error({unbound_variable,Name});
	{type_sig,ArgTypes,ReturnType} ->
	    ets:new(freshen_tvars,[set,public,named_table]),
	    Type = freshen({fun_t,As,ArgTypes,ReturnType}),
	    ets:delete(freshen_tvars),
	    {typed,As,Body,Type};
	Type ->
	    {typed,As,Body,Type}
    end;
infer_expr(Env,{app,As=[_,{format,infix}],Op,Args}) ->
    TypedArgs = [infer_expr(Env,A) || A <- Args],
    ArgTypes = [T || {typed,_,_,T} <- TypedArgs],
    {fun_t,_,OperandTypes,ResultType} = infer_infix(Op),
    unify(ArgTypes,OperandTypes),
    {typed,As,{app,As,Op,TypedArgs},ResultType};
%% TODO: prefix operators
infer_expr(Env,{app,As,Fun,Args}) ->
    NewFun={typed,_,_,FunType} = infer_expr(Env,Fun), 
    NewArgs = [infer_expr(Env,A) || A <- Args],
    ArgTypes = [T || {typed,_,_,T} <- NewArgs],
    ResultType = fresh_uvar(As),
    unify(FunType,{fun_t,[],ArgTypes,ResultType}),
    {typed,As,{app,As,NewFun,NewArgs},dereference(ResultType)};
infer_expr(Env,{'if',Attrs,Cond,Then,Else}) ->
    NewCond={typed,_,_,CondType} = infer_expr(Env,Cond),
    unify(CondType,{id,Attrs,"bool"}),
    NewThen={typed,_,_,ThenType} = infer_expr(Env,Then),
    NewElse={typed,_,_,ElseType} = infer_expr(Env,Else),
    unify(ThenType,ElseType),
    {typed,Attrs,{'if',Attrs,NewCond,NewThen,NewElse},ThenType}.


infer_infix({IntOp,As}) 
  when IntOp=='+'; IntOp=='-'; IntOp=='*'; IntOp=='/';
       IntOp=='band'; IntOp=='bor'; IntOp=='bxor' ->
    Int = {id,As,"int"},
    {fun_t,As,[Int,Int],Int};
infer_infix({RelOp,As}) 
  when RelOp=='=='; RelOp=='!='; 
       RelOp=='<'; RelOp=='>'; 
       RelOp=='<='; RelOp=='>=' ->
    Int = {id,As,"int"},
    Bool = {id,As,"bool"},
    {fun_t,As,[Int,Int],Bool}.

unify(T1,T2) ->
  unify1(dereference(T1),dereference(T2)).

unify1({uvar,_,R},{uvar,_,R}) ->
    true;
unify1({uvar,A,R},T) ->
    case occurs_check(R,T) of
        %% TODO:
	%% true ->
	%%     cannot_unify({uvar,A,R},T),
	%%     false;
	false ->
	    ets:insert(type_vars,{R,T}),
	    true
    end;
unify1(T,{uvar,A,R}) ->
    unify1({uvar,A,R},T);
unify1([A|B],[C|D]) ->
    unify(A,C) andalso unify(B,D);
unify1(X,X) ->
    true;
unify1({fun_t,_,Args1,Result1},{fun_t,_,Args2,Result2}) ->
    unify(Args1,Args2) andalso unify(Result1,Result2);
unify1(A,B) ->
    cannot_unify(A,B),
    false.

dereference(T = {uvar,_,R}) ->
    case ets:lookup(type_vars,R) of
	[] ->
	    T;
	[{R,Type}] ->
	    dereference(Type)
    end;
dereference(T) ->
    T.	    

occurs_check(R,T) ->
    %% TODO
    false.

fresh_uvar(Attrs) ->
    {uvar,Attrs,make_ref()}.

freshen({tvar,As,Name}) ->
    NewT = case ets:lookup(freshen_tvars,Name) of
	       [] ->
		   fresh_uvar(As);
	       [{Name,T}] ->
		   T
	   end,
    ets:insert(freshen_tvars,{Name,NewT}),
    NewT;
freshen(T) when is_tuple(T) ->
    list_to_tuple(freshen(tuple_to_list(T)));
freshen([A|B]) ->
    [freshen(A)|freshen(B)];
freshen(X) ->
    X.

%% Dereferences all uvars and replaces the uninstantiated ones with a
%% succession of tvars.
instantiate(E) ->
    instantiate1(dereference(E)).

instantiate1({uvar,Attr,R}) ->
    Next = proplists:get_value(next,ets:lookup(type_vars,next),1),
    TVar = {tvar,Attr,"'"++integer_to_list(Next)},
    ets:insert(type_vars,[{next,Next+1},{R,TVar}]),
    TVar;
instantiate1(T) when is_tuple(T) ->
    list_to_tuple(instantiate1(tuple_to_list(T)));
instantiate1([A|B]) ->
    [instantiate(A)|instantiate(B)];
instantiate1(X) ->
    X.


cannot_unify(A,B) ->
    io:format("Cannot unify ~s (from line ~p)\n"
	      "         and ~s (from line ~p)\n",
	      [pp(A),line_number(A),pp(B),line_number(B)]).

line_number(T) when is_tuple(T) ->
    proplists:get_value(line,element(2,T)).

pp({type_sig,As,B}) ->
    ["(",pp(As),") => ",pp(B)];
pp([T]) ->
    pp(T);
pp({id,_,Name}) ->
    Name;
pp({tvar,_,Name}) ->
    Name.

