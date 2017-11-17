%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Ring parser.
%%%
%%% @end
%%%-------------------------------------------------------------------

Nonterminals

'Contract' 'ContractName'
'Exports' 'Export' 'ExportList'
'Decls' 'Decl'
'Modifiers' 'Modifier'
'Args' 'Arg'
'Expr' 'Expr100' 'Expr200' 'Expr300' 'Expr400' 'Expr500' 'Expr600' 'Expr700'
'Expr800' 'Expr900' 'ExprAtom'
'AddOp' 'MulOp'
'FieldAssignments' 'FieldAssignment'
.

Terminals

%% Keywords
contract export 'fun' pure const

%% Identifiers and literals
id con param
int

%% Symbols
',' '=' '(' ')' '_' '{' '}'
'+' '-' '*' '/' mod
.

Rootsymbol 'Contract'.

%% Top-level contract
'Contract' -> contract 'ContractName' 'Exports' 'Decls' :
    {contract, get_line('$1'), '$2', '$3', '$4'}.

'ContractName' -> id  : get_value('$1').
'ContractName' -> con : get_value('$1').

'Exports' -> '$empty'           : [].
'Exports' -> 'Export' 'Exports' : '$1' ++ '$2'.

'Export' -> export 'ExportList' : '$2'.

'ExportList' -> id                  : ['$1'].
'ExportList' -> id ',' 'ExportList' : ['$1' | '$3'].

'Decls' -> '$empty'       : [].
'Decls' -> 'Decl' 'Decls' : ['$1' | '$2'].

'Decl' -> 'Modifiers' 'fun' id 'Args' '=' 'Expr' :
    {'fun', get_line('$2'), '$1', '$3', '$4', '$6'}.

'Modifiers' -> '$empty' : [].
'Modifiers' -> 'Modifier' 'Modifiers' : ['$1' | '$2'].

'Modifier' -> pure  : pure.
'Modifier' -> const : const.

'Args' -> 'Arg'        : ['$1'].
'Args' -> 'Arg' 'Args' : ['$1' | '$2'].

'Arg' -> id      : '$1'.
'Arg' -> '_'     : {'_', get_line('$1')}.
'Arg' -> '(' ')' : {unit, get_line('$1')}.

'Expr' -> 'Expr100' : '$1'.

'Expr100' -> 'Expr200' : '$1'.
'Expr200' -> 'Expr300' : '$1'.
'Expr300' -> 'Expr400' : '$1'.
'Expr400' -> 'Expr500' : '$1'.
'Expr500' -> 'Expr600' : '$1'.

'Expr600' -> 'Expr600' 'AddOp' 'Expr700' : {infix, get_line('$1'), '$1', '$2', '$3'}.
'Expr600' -> 'Expr700' : '$1'.

'Expr700' -> 'Expr700' 'MulOp' 'Expr800' : {infix, get_line('$1'), '$1', '$2', '$3'}.
'Expr700' -> 'Expr800' : '$1'.

'Expr800' -> 'Expr900' : '$1'.

'Expr900' -> 'Expr900' 'ExprAtom' : {app, get_line('$1'), '$1', '$2'}.
'Expr900' -> 'ExprAtom' : '$1'.

'ExprAtom' -> int   : '$1'.
'ExprAtom' -> id    : '$1'.
'ExprAtom' -> param : '$1'.
'ExprAtom' -> '{' 'FieldAssignments' '}' : {record, get_line('$1'), '$2'}.
'ExprAtom' -> '(' 'Expr' ')' : '$2'.

'FieldAssignments' -> 'FieldAssignment' : ['$1'].
'FieldAssignments' -> 'FieldAssignment' 'FieldAssignments' : ['$1' | '$2'].

'FieldAssignment' -> id '=' 'Expr' : {assign, get_line('$1'), '$1', '$3'}.

'AddOp' -> '+' : '+'.
'AddOp' -> '-' : '-'.

'MulOp' -> '*'   : '*'.
'MulOp' -> '/'   : '/'.
'MulOp' -> 'mod' : mod.

Erlang code.
-include("aering_ast.hrl").

-ignore_xref([format_error/1, parse_and_scan/1]).

get_line(Tok) -> element(2, Tok).

get_value({Tok, _Line})       -> Tok;
get_value({_Tok, _Line, Val}) -> Val.

