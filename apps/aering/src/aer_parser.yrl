%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Ring parser.
%%%
%%% @end
%%%-------------------------------------------------------------------

Nonterminals

'File'
'Decls' 'Decls1' 'Decl' 'LetDecl'
'TypeDef' 'FieldTypes' 'FieldType'
'Args' 'Args1' 'Arg'
'Type' 'Type100' 'Type200' 'Type300' 'Type400' 'TypeAtom'
'TypeArgs' 'Constructor' 'Constructors' 'Constructors1'
'Expr' 'Expr100' 'Expr200' 'Expr300' 'Expr400' 'Expr500' 'Expr600' 'Expr700'
'Expr800' 'Expr850' 'Expr900' 'Expr950' 'ExprAtom' 'TypedExprs' 'TypedExprs1' 'TypedExpr'
'BlockStatements' 'Statement' 'FieldAssignment'
'Cases' 'Case'
'AddOp' 'MulOp' 'CmpOp' 'ConsOp' 'AndOp' 'OrOp'
.

Terminals

%% Keywords
contract type 'let' rec switch 'if' else mutable

%% Identifiers and literals
id qid con qcon tvar
int hex bool hash string

%% Symbols
';' ':' ',' '=' '(' ')' '{' '}' '|' '[' ']' '?'
'+' '-' '*' '/' mod '++' '::'
'<' '>' '=<' '>=' '==' '!='
'||' '&&' '!' '.'
'=>'
.

Rootsymbol 'File'.

%% Top-level contract
'File' -> 'Decls' : '$1'.

%% -- Declarations -----------------------------------------------------------

'Decls' -> '$empty' : [].
'Decls' -> 'Decls1' : '$1'.

'Decls1' -> 'Decl'              : ['$1'].
'Decls1' -> 'Decl' ';'          : ['$1'].
'Decls1' -> 'Decl' ';' 'Decls1' : ['$1' | '$3'].

%% Contract types and contracts
'Decl' -> 'contract' 'type' con '=' '{' 'Decls' '}' : {contract_type, get_ann('$1'), '$3', '$6'}.
'Decl' -> 'contract'        con '=' '{' 'Decls' '}' : {contract, get_ann('$1'), '$2', '$5'}.

%% Type declarations
'Decl' -> 'type' id               : {type_decl, get_ann('$1'), '$2'}.
'Decl' -> 'type' id '=' 'TypeDef' : {type_def, get_ann('$1'), '$2', '$4'}.

%% Type definitions
'TypeDef' -> 'Type' : '$1'.
'TypeDef' -> '{' 'FieldTypes' '}' : {record_t, '$2'}.
'TypeDef' -> 'Constructors'       : {variant_t, '$1'}.

'Constructors' ->     'Constructors1' : '$1'.
'Constructors' -> '|' 'Constructors1' : '$2'.

'Constructors1' -> 'Constructor'                     : ['$1'].
'Constructors1' -> 'Constructor' '|' 'Constructors1' : ['$1' | '$3'].

'Constructor' -> con                    : {con, get_ann('$1'), get_value('$1'), []}.
'Constructor' -> con '(' 'TypeArgs' ')' : {con, get_ann('$1'), get_value('$1'), '$3'}.

'FieldTypes' -> 'FieldType' : ['$1'].
'FieldTypes' -> 'FieldType' ',' 'FieldTypes' : ['$1' | '$3'].

'FieldType' -> mutable id ':' 'Type' : {field_t, get_value('$1'), mutable, '$1', '$3'}.
'FieldType' ->         id ':' 'Type' : {field_t, get_value('$1'), immutable, '$1', '$3'}.

%% Function declarations
'Decl' -> 'let' id ':' 'Type' : {fun_decl, get_ann('$1'), '$2', '$4'}.
'Decl' -> 'LetDecl'           : '$1'.

'LetDecl' -> 'let' id ':' 'Type' '=' 'Expr' : {'letval', get_ann('$1'), '$2', '$4', '$6'}.
'LetDecl' -> 'let' id            '=' 'Expr' : {'letval', get_ann('$1'), '$2', type_wildcard(), '$4'}.
'LetDecl' -> 'let' id 'Args' ':' 'Type' '=' 'Expr' : {'letfun', get_ann('$1'), '$2', '$3', '$5', '$7'}.
'LetDecl' -> 'let' id 'Args'            '=' 'Expr' : {'letfun', get_ann('$1'), '$2', '$3', type_wildcard(), '$5'}.
'LetDecl' -> 'let' 'rec' id 'Args' ':' 'Type' '=' 'Expr' : {'letrec', get_ann('$1'), '$3', '$4', '$6', '$8'}.
'LetDecl' -> 'let' 'rec' id 'Args'            '=' 'Expr' : {'letrec', get_ann('$1'), '$3', '$4', type_wildcard(), '$6'}.

%% Argument lists
'Args' -> '(' 'Args1' ')' : ['$2'].
'Args' -> '(' ')'         : [].

'Args1' -> 'Arg'             : ['$1'].
'Args1' -> 'Arg' ',' 'Args1' : ['$1' | '$2'].

'Arg' -> id            : {arg, get_ann('$1'), token('$1'), type_wildcard()}.
'Arg' -> id ':' 'Type' : {arg, get_ann('$1'), token('$1'), '$3'}.

%% -- Types ------------------------------------------------------------------

'Type' -> 'Type100' : '$1'.

'Type100' -> 'Type200' : '$1'.

'Type200' -> 'Type300' '=>' 'Type200' : {fun_t, get_ann('$1'), '$1', '$3'}.
'Type200' -> 'Type300' : '$1'.

'Type300' -> 'Type400' : '$1'.

'Type400' -> 'TypeAtom' '(' 'TypeArgs' ')' : {app_t, get_ann('$1'), '$1', '$3'}.
'Type400' -> 'TypeAtom' : '$1'.

'TypeAtom' -> id   : {id_t, get_ann('$1'), get_value('$1')}.
'TypeAtom' -> tvar : {var_t, get_ann('$1'), get_value('$1')}.
'TypeAtom' -> '(' 'TypeArgs' ')' : tuple_t(get_ann('$1'), '$2').

'TypeArgs' -> 'Type' ',' 'TypeArgs' : ['$1' | '$3'].
'TypeArgs' -> 'Type' : ['$1'].

%% -- Expressions ------------------------------------------------------------

'Expr' -> 'Expr100' : '$1'.

'Expr100' -> 'ExprAtom' '=>' 'Expr100' : {lam, get_ann('$1'), '$1', '$3'}.
'Expr100' -> 'Expr200' '?' 'Expr100' ':' 'Expr100' : set_ann(format, '?:', {'if', get_ann('$1'), '$3', '$5'}).
'Expr100' -> 'Expr200' : '$1'.

'Expr200' -> 'Expr300' 'OrOp' 'Expr200' : infix('$1', '$2', '$3').
'Expr200' -> 'Expr300' : '$1'.

'Expr300' -> 'Expr400' 'AndOp' 'Expr300' : infix('$1', '$2', '$3').
'Expr300' -> 'Expr400' : '$1'.

'Expr400' -> 'Expr500' 'CmpOp' 'Expr500' : infix('$1', '$2', '$3').
'Expr400' -> 'Expr500' : '$1'.

'Expr500' -> 'Expr600' 'ConsOp' 'Expr500' : infix('$1', '$2', '$3').
'Expr500' -> 'Expr600' : '$1'.

'Expr600' -> 'Expr600' 'AddOp' 'Expr700' : infix('$1', '$2', '$3').
'Expr600' -> 'Expr700' : '$1'.

'Expr700' -> 'Expr700' 'MulOp' 'Expr800' : infix('$1', '$2', '$3').
'Expr700' -> 'Expr800' : '$1'.

'Expr800' -> '!' 'Expr800' : prefix('$1', '$2').
'Expr800' -> 'Expr850' : '$1'.

'Expr850' -> switch 'Expr900' '{' 'Cases' '}' : {switch, get_ann('$1'), '$2', '$4'}.
'Expr850' -> 'if' '(' 'Expr' ')' 'ExprAtom' 'else' 'ExprAtom' : {'if', get_ann('$1'), '$3', '$5', '$7'}.
'Expr850' -> 'if' '(' 'Expr' ')' 'ExprAtom'                   : {'if', get_ann('$1'), '$3', '$5', {unit, [{origin, system}]}}.
'Expr850' -> 'Expr900' : '$1'.

'Expr900' -> 'Expr900' '.' id : {project, get_ann('$1'), '$1', token('$2')}.
'Expr900' -> 'Expr950' : '$1'.

'Expr950' -> 'Expr950' '(' 'TypedExprs' ')' : {app, get_ann('$1'), '$1', '$3'}.
'Expr950' -> 'ExprAtom' : '$1'.

'ExprAtom' -> int    : token('$1').
'ExprAtom' -> hex    : set_ann(format, hex, setelement(1, '$1', int)).
'ExprAtom' -> bool   : token('$1').
'ExprAtom' -> id     : token('$1').
'ExprAtom' -> con    : token('$1').
'ExprAtom' -> qid    : token('$1').
'ExprAtom' -> qcon   : token('$1').
'ExprAtom' -> hash   : token('$1').
'ExprAtom' -> string : token('$1').
'ExprAtom' -> '{' 'BlockStatements' '}' : block_e(get_ann('$1'), '$2').
'ExprAtom' -> '(' 'TypedExprs' ')' : tuple_e(get_ann('$1'), '$2').
'ExprAtom' -> '[' 'TypedExprs' ']' : {list, get_ann('$1'), '$2'}.

'TypedExprs' -> '$empty'      : [].
'TypedExprs' -> 'TypedExprs1' : '$1'.

'TypedExprs1' -> 'TypedExpr' : ['$1'].
'TypedExprs1' -> 'TypedExpr' ',' 'TypedExprs' : ['$1' | '$3'].

'TypedExpr' -> 'Expr'            : '$1'.
'TypedExpr' -> 'Expr' ':' 'Type' : {typed, get_ann('$1'), '$1', '$2'}.

'BlockStatements' -> 'Statement' ';'       : {[], ['$1']}.
'BlockStatements' -> 'FieldAssignment' ',' : {[], ['$1']}.
'BlockStatements' -> 'Statement'           : {[], ['$1']}.
'BlockStatements' -> 'FieldAssignment'     : {[], ['$1']}.
'BlockStatements' -> 'FieldAssignment' ',' 'BlockStatements' : begin {Seps, Ss} = '$3', {[',' | Seps], ['$1' | Ss]} end.
'BlockStatements' -> 'Statement'       ';' 'BlockStatements' : begin {Seps, Ss} = '$3', {[';' | Seps], ['$1' | Ss]} end.

'FieldAssignment' -> id ':' 'Expr' : {field, get_ann('$1'), get_value('$1'), '$3'}.

'Statement' -> 'Expr'    : '$1'.
'Statement' -> 'Expr900' '=' 'Expr' : {assign, get_ann('$1'), '$1', '$3'}.
'Statement' -> 'LetDecl' : '$1'.

'Cases' -> '$empty'       : [].
'Cases' -> 'Case' 'Cases' : ['$1' | '$2'].

'Case' -> '|' 'Expr500' '=>' 'BlockStatements' : {'case', get_ann('$1'), '$2', block_e(get_ann(hd(element(2, '$4'))), '$4')}.

'AndOp' -> '&&' : token('$1').
'OrOp'  -> '||' : token('$1').

'ConsOp' -> '::' : token('$1').
'ConsOp' -> '++' : token('$1').

'CmpOp' -> '<'  : token('$1').
'CmpOp' -> '>'  : token('$1').
'CmpOp' -> '=<' : token('$1').
'CmpOp' -> '>=' : token('$1').
'CmpOp' -> '==' : token('$1').
'CmpOp' -> '!=' : token('$1').

'AddOp' -> '+' : token('$1').
'AddOp' -> '-' : token('$1').

'MulOp' -> '*'   : token('$1').
'MulOp' -> '/'   : token('$1').
'MulOp' -> 'mod' : token('$1').

Erlang code.
-include("aering_ast.hrl").

-ignore_xref([format_error/1, parse_and_scan/1]).

-spec line_ann(line()) -> ann().
line_ann(Line) -> [{line, Line}].

-spec get_ann({atom(), line() | ann(), _} | {atom(), line() | ann()}) -> ann().
get_ann(X) ->
  case element(2, X) of
    Line when is_integer(Line) -> line_ann(Line);
    Ann -> Ann
  end.

set_ann(Key, Val, Node) ->
  Ann = get_ann(Node),
  setelement(2, Node, lists:keystore(Key, 1, Ann, {Key, Val})).

get_value({Tok, _Line})       -> Tok;
get_value({_Tok, _Line, Val}) -> Val.

token({Tok, Line}) -> {Tok, line_ann(Line)};
token({Tok, Line, Val}) -> {Tok, line_ann(Line), Val}.

infix(L, Op, R) -> set_ann(format, infix, {app, get_ann(L), Op, [L, R]}).
prefix(Op, E)   -> set_ann(format, prefix, {app, get_ann(Op), Op, [E]}).

type_wildcard() ->
  {'_', [{origin, system}]}.

tuple_t(_Ann, [Type]) -> Type;  %% Not a tuple
tuple_t(Ann, Types) -> {tuple_t, Ann, Types}.

tuple_e(Ann, [])      -> {unit, Ann};
tuple_e(_Ann, [Expr]) -> Expr;  %% Not a tuple
tuple_e(Ann, Exprs)   -> {tuple, Ann, Exprs}.

block_e(Ann, {[], [Fld = {typed, _, _}]}) ->
    {record, Ann, [Fld]};
block_e(_Ann, {[], [Expr]}) -> Expr;
block_e(Ann, {Seps, Exprs}) ->
  case lists:usort(Seps) of
    [','] -> {record, Ann, Exprs};
    [';'] -> {block,  Ann, Exprs};
    _ -> error({parse_error_in_block, Ann})
  end.

