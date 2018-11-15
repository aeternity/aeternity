%%%-------------------------------------------------------------------
%%% @author Robert Virding
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%     Parser for Varna.
%%% @end
%%% Created : 2018-04-25
%%%
%%%-------------------------------------------------------------------

%% The Grammar rules here are based on LUA 5.2.

%Expect 2.                               %Suppress shift/reduce waring

Nonterminals
contractdef statedef functiondefs functiondef_list functiondef scope
block stats localdef retstat semi stat functioncall
if_stat if_elseif if_else
explist exp
boolean
varname funcname dottedname args
pars parlist parameter
typedef
fields comma field
binop unop uminus
.

Terminals
NAME NUMBER STRING

'and' 'else' 'elseif' 'end' 'false' 'function' 'funcscope' 'if' 'not' 'null'
'or' 'return' 'then' 'true' 'var' 'type'

'=>'

'+' '-' '*' '/' '==' '!=' '<=' '>=' '<' '>' '='
'(' ')' '{' '}' '[' ']' ';' ':' ',' '.' .


Rootsymbol contractdef.

Left 100 'or'.
Left 200 'and'.
Left 300 '<' '>' '<=' '>=' '!=' '=='.
Left 500 '+' '-'.
Left 600 '*' '/'.
Unary 700 'not' uminus.

%% We have be cunning here as 'contract' both starts the contract
%% definition and is a variable referencing the contract record.

contractdef -> NAME NAME statedef functiondefs 'end' :
                   check_contractdef('$1', '$2', '$3', '$4').

statedef -> '$empty' : undefined .
statedef -> NAME fields 'end' : check_statedef('$1', '$2') .

functiondefs -> '$empty' : [] .
functiondefs -> functiondef_list : '$1' . 

functiondef_list -> functiondef : ['$1'] .
functiondef_list -> functiondef_list functiondef : '$1' ++ ['$2'] .
    
functiondef -> scope function NAME pars typedef block 'end' :
		   check_functiondef('$1', '$2', '$3', '$4', '$5', '$6').

scope -> '$empty' : [] .
scope ->  scope funcscope : '$1' ++ ['$2'] .

block -> stats : '$1'.
block -> stats retstat : '$1' ++ ['$2'].

retstat -> return semi : {return,line('$1'),[]} .
retstat -> return exp semi : {return,line('$1'),['$2']} .

semi -> ';' .                           %semi is never returned
semi -> '$empty' .

stats -> '$empty' : [] .
stats -> stats stat semi : '$1' ++ ['$2'] .

%stat -> ';' : '$1' .                    %This ';' will be included
stat -> localdef : '$1' .
stat -> varname '=' exp : {assign,line('$1'),'$1','$3'} .
stat -> functioncall : '$1' .
stat -> if_stat : '$1'.

localdef -> var NAME typedef '=' exp : {local,line('$1'),'$2','$3','$5'} .

functioncall -> funcname args : {functioncall,line('$1'),'$1','$2'} . 

%% stat ::= if exp then block {elseif exp then block} [else block] end

if_stat -> 'if' exp 'then' block if_elseif if_else 'end' :
               {'if',line('$1'),[{'$2','$4'}|'$5'],'$6'} .

if_elseif -> if_elseif 'elseif' exp 'then' block : '$1' ++ [{'$3','$5'}] .
if_elseif -> '$empty' : [] .

if_else -> 'else' block : '$2' .
if_else -> '$empty' : [] .                      %An empty block

explist -> exp : ['$1'] .
explist -> explist ',' exp : '$1' ++ ['$3'] .

exp -> null : '$1' .
exp -> boolean : '$1' .
exp -> NUMBER : '$1' .
exp -> STRING : '$1' .
exp -> varname : '$1' .
exp -> functioncall : '$1' .
exp -> binop : '$1' .
exp -> unop : '$1' .
exp -> '(' exp ')' : '$1' .

boolean -> true : '$1' .
boolean -> false : '$1' .

%% varname ::= name [ '.' varname ] | varname '[' varname ']'

varname -> dottedname : '$1' .
varname -> dottedname '[' dottedname ']' : {ref,line('$2'),'$1','$3'}.

%% funcname ::= Name [ '.' Name ]

funcname -> dottedname : '$1' .

dottedname -> NAME : '$1'.
dottedname -> NAME '.' NAME : {'.',line('$2'),'$1','$3'} . 

args -> '(' ')' : [] .
args -> '(' explist ')' : '$2' .

pars -> '(' ')' : [] .
pars -> '(' parlist ')' : '$2' .

parlist -> parameter : ['$1'] .
parlist -> parlist ',' parameter : '$1' ++ ['$3'] .

parameter -> NAME typedef : {parameter,line('$1'),'$1','$2'} .

comma -> ',' .                          %comma is never returned
comma -> '$empty' .

fields -> '$empty' : [] .
fields -> fields field comma : '$1' ++ ['$2'].

field -> NAME typedef '=' exp : {field,line('$1'),'$1','$2','$4'} . 

typedef -> ':' type : '$2' .
typedef -> '$empty' : undefined .

%% exp ::= exp binop exp
%% exp ::= unop exp
%% We have to write them these way for the priorities to work.

binop -> exp '+' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '-' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '*' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '/' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '==' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '!=' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '<=' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '>=' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '<' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '>' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp 'and' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp 'or' exp : {op,line('$2'),cat('$2'),'$1','$3'}.

unop -> 'not' exp : {op,line('$1'),cat('$1'),'$2'} .
unop ->  uminus : '$1' .

uminus-> '-' exp : {op,line('$1'),'-','$2'} .

Erlang code.

-export([contract/1]).

%% contract(Tokens) -> Contract | Error.

contract(Ts) ->
    case parse(Ts) of
        {error,_}=Error -> Error;
        {ok,Body} -> {ok,Body} 
    end.

cat(T) -> element(1, T).
line(T) -> element(2, T).
val(T) -> element(3, T).

%% check_contractdef(Name, Fields) -> StateDef.
%%  Check that the Name is 'contract'.

check_contractdef({'NAME',L,contract}, Name, State, Funcs) ->
    {contract,L,val(Name),State,Funcs};
check_contractdef(Other, _Name, _State, _Funcs) ->
    return_error(line(Other),"illegal contract definition").

%% check_statedef(Name, Fields) -> StateDef.
%%  Check that the Name is 'state'.

check_statedef({'NAME',L,state}, Fields) -> {state,L,Fields};
check_statedef(Other, _Fields) ->
    return_error(line(Other),"illegal state definition").

%% check_functiondef(Scope, Func, Name, Pars, Type, Body) -> FunctionDef.
%%  Check the validity of the function definition.

check_functiondef(Scope, Func, Name, Pars, Type, Body) ->
    %% All local defs must come first in the body.
    {_,Sts} = lists:splitwith(fun (F) -> element(1, F) =:= local end, Body),
    case lists:keyfind(local, 1, Sts) of
    	Local when Local =/= false ->
    	    return_error(line(Local), "illegal local definition");
    	false ->
    	    {function,line(Func),Name,Scope,Pars,Type,Body}
    end.
