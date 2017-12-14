%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Ring parser.
%%%
%%% @end
%%%-------------------------------------------------------------------

Nonterminals

'File'
'Decls' 'Decls1' 'Decl' 'LetDecl' 'LetDef' 'AndDecls'
'TypeDef' 'FieldTypes' 'FieldType'
'Args' 'Args1' 'Arg' 'TypeVars' 'TypeVars1'
'Type' 'Type100' 'Type200' 'Type300' 'Type400' 'Type500' 'TypeAtom' 'FunDomain'
'TypeArgs' 'Constructor' 'Constructors' 'Constructors1'
'Expr' 'Expr100' 'Expr200' 'Expr300' 'Expr400' 'Expr500' 'Expr600' 'Expr650' 'Expr700'
'Expr800' 'Expr850' 'Expr900' 'ExprAtom' 'TypedExprs' 'TypedExprs1' 'TypedExpr'
'BlockStatements' 'Statement' 'FieldAssignment'
'Cases' 'Case'
'AddOp' 'MulOp' 'CmpOp' 'ConsOp' 'AndOp' 'OrOp'
.

Terminals

%% Keywords
contract type 'let' rec switch 'if' else mutable
'and'

%% Identifiers and literals
id qid con qcon tvar
int hex bool hash string char

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
'Decl' -> 'contract' 'type' con '=' '{' 'Decls' '}' : {contract_type, get_ann('$1'), token('$3'), '$6'}.
'Decl' -> 'contract'        con '=' '{' 'Decls' '}' : {contract, get_ann('$1'), token('$2'), '$5'}.

%% Type declarations
'Decl' -> 'type' id                          : {type_decl, get_ann('$1'), token('$2'), []}.
'Decl' -> 'type' id 'TypeVars'               : {type_decl, get_ann('$1'), token('$2'), '$3'}.
'Decl' -> 'type' id            '=' 'TypeDef' : {type_def,  get_ann('$1'), token('$2'), [], '$4'}.
'Decl' -> 'type' id 'TypeVars' '=' 'TypeDef' : {type_def,  get_ann('$1'), token('$2'), '$3', '$5'}.

'TypeVars' -> '(' 'TypeVars1' ')' : '$2'.
'TypeVars1' -> tvar                 : [token('$1')].
'TypeVars1' -> tvar ',' 'TypeVars1' : [token('$1') | '$3'].

%% Type definitions
'TypeDef' -> 'Type'               : {alias_t, '$1'}.
'TypeDef' -> '{' 'FieldTypes' '}' : {record_t, '$2'}.
'TypeDef' -> 'Constructors'       : {variant_t, '$1'}.

'Constructors' ->     'Constructors1' : '$1'.
'Constructors' -> '|' 'Constructors1' : '$2'.

'Constructors1' -> 'Constructor'                     : ['$1'].
'Constructors1' -> 'Constructor' '|' 'Constructors1' : ['$1' | '$3'].

'Constructor' -> con                    : {constr_t, get_ann('$1'), token('$1'), []}.
'Constructor' -> con '(' 'TypeArgs' ')' : {constr_t, get_ann('$1'), token('$1'), '$3'}.

'FieldTypes' -> 'FieldType' : ['$1'].
'FieldTypes' -> 'FieldType' ',' 'FieldTypes' : ['$1' | '$3'].

'FieldType' -> mutable id ':' 'Type' : {field_t, get_ann('$1'), mutable, token('$2'), '$4'}.
'FieldType' ->         id ':' 'Type' : {field_t, get_ann('$1'), immutable, token('$1'), '$3'}.

%% Function declarations
'Decl' -> 'let' id ':' 'Type' : {fun_decl, get_ann('$1'), token('$2'), '$4'}.
'Decl' -> 'LetDecl'           : '$1'.

'LetDecl' -> 'let' 'LetDef' : set_ann(line, get_ann(line, '$1'), '$2').
'LetDecl' -> 'let' 'rec' 'LetDef' 'AndDecls' : {letrec, get_ann('$1'), ['$3' | '$4']}.

'AndDecls' -> '$empty' : [].
'AndDecls' -> 'and' 'LetDef' 'AndDecls' : ['$2' | '$3'].

'LetDef' -> id ':' 'Type' '=' 'Expr' : {letval, get_ann('$1'), token('$1'), '$3', '$5'}.
'LetDef' -> id            '=' 'Expr' : {letval, get_ann('$1'), token('$1'), type_wildcard(), '$3'}.
'LetDef' -> id 'Args' ':' 'Type' '=' 'Expr' : {letfun, get_ann('$1'), token('$1'), '$2', '$4', '$6'}.
'LetDef' -> id 'Args'            '=' 'Expr' : {letfun, get_ann('$1'), token('$1'), '$2', type_wildcard(), '$4'}.

%% Argument lists
'Args' -> '(' 'Args1' ')' : '$2'.
'Args' -> '(' ')'         : [].

'Args1' -> 'Arg'             : ['$1'].
'Args1' -> 'Arg' ',' 'Args1' : ['$1' | '$3'].

'Arg' -> id            : {arg, get_ann('$1'), token('$1'), type_wildcard()}.
'Arg' -> id ':' 'Type' : {arg, get_ann('$1'), token('$1'), '$3'}.

%% -- Types ------------------------------------------------------------------

'Type' -> 'Type100' : '$1'.

'Type100' -> 'Type200' : '$1'.

'Type200' -> 'FunDomain' '=>' 'Type200' : fun_t(get_ann('$2'), '$1', '$3').
'Type200' -> 'Type300' : '$1'.

'Type300' -> 'Type400' : '$1'.

'Type400' -> 'TypeAtom' '(' 'TypeArgs' ')' : {app_t, get_ann('$1'), '$1', '$3'}.
'Type400' -> 'Type500' : '$1'.

'Type500' -> 'TypeAtom' : '$1'.

'TypeAtom' -> id   : token('$1').
'TypeAtom' -> tvar : token('$1').
'TypeAtom' -> '(' 'TypeArgs' ')' : tuple_t(get_ann('$1'), '$2').

'TypeArgs' -> 'Type' ',' 'TypeArgs' : ['$1' | '$3'].
'TypeArgs' -> 'Type' : ['$1'].

'FunDomain' -> 'Type400' : fun_domain('$1').

%% -- Expressions ------------------------------------------------------------

'Expr' -> 'Expr100' : '$1'.

'Expr100' -> 'ExprAtom' '=>' 'Expr100' : {lam, get_ann('$1'), lam_args('$1'), '$3'}.
'Expr100' -> 'Expr200' '?' 'Expr100' ':' 'Expr100' : set_ann(format, '?:', {'if', get_ann('$1'), '$1', '$3', '$5'}).
'Expr100' -> 'Expr900' '=' 'Expr100' : {assign, get_ann('$1'), parse_lvalue('$1'), '$3'}.
'Expr100' -> 'Expr200' : '$1'.

'Expr200' -> 'Expr300' 'OrOp' 'Expr200' : infix('$1', '$2', '$3').
'Expr200' -> 'Expr300' : '$1'.

'Expr300' -> 'Expr400' 'AndOp' 'Expr300' : infix('$1', '$2', '$3').
'Expr300' -> 'Expr400' : '$1'.

'Expr400' -> 'Expr500' 'CmpOp' 'Expr500' : infix('$1', '$2', '$3').
'Expr400' -> 'Expr500' : '$1'.

'Expr500' -> 'Expr600' 'ConsOp' 'Expr500' : infix('$1', '$2', '$3').
'Expr500' -> 'Expr600' : '$1'.

'Expr600' -> 'Expr600' 'AddOp' 'Expr650' : infix('$1', '$2', '$3').
'Expr600' -> 'Expr650' : '$1'.

'Expr650' -> '-' 'Expr650'               : prefix(token('$1'), '$2').
'Expr650' -> 'Expr700' : '$1'.

'Expr700' -> 'Expr700' 'MulOp' 'Expr800' : infix('$1', '$2', '$3').
'Expr700' -> 'Expr800' : '$1'.

'Expr800' -> '!' 'Expr800' : prefix(token('$1'), '$2').
'Expr800' -> 'Expr850' : '$1'.

'Expr850' -> switch '(' 'Expr' ')' '{' 'Cases' '}'            : {switch, get_ann('$1'), '$3', '$6'}.
'Expr850' -> 'if' '(' 'Expr' ')' 'ExprAtom' 'else' 'ExprAtom' : {'if', get_ann('$1'), '$3', '$5', '$7'}.
'Expr850' -> 'if' '(' 'Expr' ')' 'ExprAtom'                   : {'if', get_ann('$1'), '$3', '$5', {unit, [{origin, system}]}}.
'Expr850' -> 'Expr900' : '$1'.

'Expr900' -> 'Expr900' '.' id                    : {proj, get_ann('$1'), '$1', token('$3')}.
'Expr900' -> 'Expr900' '(' 'TypedExprs' ')'      : {app, get_ann('$1'), '$1', '$3'}.
'Expr900' -> 'ExprAtom' : '$1'.

'ExprAtom' -> int    : token('$1').
'ExprAtom' -> hex    : set_ann(format, hex, setelement(1, '$1', int)).
'ExprAtom' -> bool   : token('$1').
'ExprAtom' -> id     : token('$1').
'ExprAtom' -> con    : token('$1').
'ExprAtom' -> qid    : token('$1').
'ExprAtom' -> qcon   : token('$1').
'ExprAtom' -> hash   : token('$1').
'ExprAtom' -> string : token('$1').
'ExprAtom' -> char   : token('$1').
'ExprAtom' -> '{' 'BlockStatements' '}' : block_e(get_ann('$1'), '$2').
'ExprAtom' -> '(' 'TypedExprs' ')' : tuple_e(get_ann('$1'), '$2').
'ExprAtom' -> '[' 'TypedExprs' ']' : {list, get_ann('$1'), '$2'}.

'TypedExprs' -> '$empty'      : [].
'TypedExprs' -> 'TypedExprs1' : '$1'.

'TypedExprs1' -> 'TypedExpr' : ['$1'].
'TypedExprs1' -> 'TypedExpr' ',' 'TypedExprs' : ['$1' | '$3'].

'TypedExpr' -> 'Expr'            : '$1'.
'TypedExpr' -> 'Expr' ':' 'Type' : {typed, get_ann('$1'), '$1', '$3'}.

'BlockStatements' -> 'Statement' ';'       : {[], ['$1']}.
'BlockStatements' -> 'FieldAssignment' ',' : {[], ['$1']}.
'BlockStatements' -> 'Statement'           : {[], ['$1']}.
'BlockStatements' -> 'FieldAssignment'     : {[], ['$1']}.
'BlockStatements' -> 'FieldAssignment' ',' 'BlockStatements' : begin {Seps, Ss} = '$3', {[',' | Seps], ['$1' | Ss]} end.
'BlockStatements' -> 'Statement'       ';' 'BlockStatements' : begin {Seps, Ss} = '$3', {[';' | Seps], ['$1' | Ss]} end.

'FieldAssignment' -> id ':' 'Expr' : {field, get_ann('$1'), token('$1'), '$3'}.

'Statement' -> 'Expr'    : '$1'.
'Statement' -> 'LetDecl' : '$1'.

'Cases' -> '$empty'       : [].
'Cases' -> 'Case' 'Cases' : ['$1' | '$2'].

'Case' -> '|' 'Expr500' '=>' 'BlockStatements' : {'case', get_ann('$1'), parse_pattern('$2'), block_e(get_ann(hd(element(2, '$4'))), '$4')}.

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

-export([string/1]).

-ignore_xref([format_error/1, parse_and_scan/1]).

string(S) ->
    case aer_scan:string(S) of
        {ok, Toks, _} -> parse(Toks);
        Err           -> Err
    end.

-spec ret_err(integer(), string()) -> no_return().
ret_err(Line, Fmt) -> ret_err(Line, Fmt, []).

-spec ret_err(integer(), string(), [any()]) -> no_return().
ret_err(Line, Fmt, Args) ->
    return_error(Line, lists:flatten(io_lib:format(Fmt, Args))).

-spec ret_doc_err(integer(), prettypr:document()) -> no_return().
ret_doc_err(Line, Doc) ->
    return_error(Line, prettypr:format(Doc)).

-spec bad_expr_err(string(), aer_syntax:expr()) -> no_return().
bad_expr_err(Reason, E) ->
  ret_doc_err(get_ann(line, E),
              prettypr:sep([prettypr:text(Reason ++ ":"),
                            prettypr:nest(2, aer_pretty:expr(E))])).

-type ann()      :: aer_syntax:ann().
-type ann_line() :: aer_syntax:ann_line().

-spec line_ann(ann_line()) -> ann().
line_ann(Line) -> [{line, Line}].

get_ann(Node) ->
    case element(2, Node) of
        Line when is_integer(Line) -> line_ann(Line);
        Ann -> Ann
    end.

get_ann(Key, Node) ->
    proplists:get_value(Key, get_ann(Node)).

set_ann(Key, Val, Node) ->
    Ann = get_ann(Node),
    setelement(2, Node, lists:keystore(Key, 1, Ann, {Key, Val})).

token({Tok, Line}) -> {Tok, line_ann(Line)};
token({Tok, Line, Val}) -> {Tok, line_ann(Line), Val}.

infix(L, Op, R) -> set_ann(format, infix, {app, get_ann(L), Op, [L, R]}).
prefix(Op, E)   -> set_ann(format, prefix, {app, get_ann(Op), Op, [E]}).

type_wildcard() ->
    {id, [{origin, system}], "_"}.

tuple_t(_Ann, [Type]) -> Type;  %% Not a tuple
tuple_t(Ann, Types) -> {tuple_t, Ann, Types}.

fun_t(Ann, Domain, {fun_t, _Ann, Domain1, Result}) ->
  {fun_t, Ann, Domain ++ Domain1, Result};
fun_t(Ann, Domain, Result) -> {fun_t, Ann, Domain, Result}.

tuple_e(Ann, [])      -> {unit, Ann};
tuple_e(_Ann, [Expr]) -> Expr;  %% Not a tuple
tuple_e(Ann, Exprs)   -> {tuple, Ann, Exprs}.

block_e(Ann, {[], [Fld = {field, _, _, _}]}) ->
    {record, Ann, [Fld]};
block_e(_Ann, {[], [Expr]}) -> Expr;
block_e(Ann, {Seps, Exprs}) ->
  case lists:usort(Seps) of
      [','] -> {record, Ann, Exprs};
      [';'] -> {block,  Ann, Exprs};
      _ -> ret_err(proplists:get_value(line, Ann), "Mixed ',' and ';' in block")
  end.

lam_args({tuple, _Ann, Args}) -> [lam_arg(Arg) || Arg <- Args];
lam_args(E)                   -> [lam_arg(E)].

lam_arg({typed, Ann, Id = {id, _, _}, Type}) -> {arg, Ann, Id, Type};
lam_arg(Id = {id, Ann, _})                   -> {arg, Ann, Id, type_wildcard()};
lam_arg(E) ->
  bad_expr_err("Not a valid function parameter", E).

%% TODO: not nice
fun_domain({tuple_t, _, Args}) -> Args;
fun_domain(T)                  -> [T].

-spec parse_pattern(aer_syntax:expr()) -> aer_syntax:pat() | no_return().
parse_pattern({app, Ann, Con = {'::', _}, Es}) ->
    {app, Ann, Con, lists:map(fun parse_pattern/1, Es)};
parse_pattern({app, Ann, Con = {con, _, _}, Es}) ->
    {app, Ann, Con, lists:map(fun parse_pattern/1, Es)};
parse_pattern({tuple, Ann, Es}) ->
    {tuple, Ann, lists:map(fun parse_pattern/1, Es)};
parse_pattern({list, Ann, Es}) ->
    {list, Ann, lists:map(fun parse_pattern/1, Es)};
parse_pattern(E = {con, _, _})    -> E;
parse_pattern(E = {id, _, _})     -> E;
parse_pattern(E = {unit, _})      -> E;
parse_pattern(E = {int, _, _})    -> E;
parse_pattern(E = {bool, _, _})   -> E;
parse_pattern(E = {hash, _, _})   -> E;
parse_pattern(E = {string, _, _}) -> E;
parse_pattern(E = {char, _, _})   -> E;
parse_pattern(E) -> bad_expr_err("Not a valid pattern", E).

-spec parse_lvalue(aer_syntax:expr()) -> aer_syntax:expr() | no_return().
parse_lvalue(E = {proj, _, _, _}) -> E;
parse_lvalue(E) -> bad_expr_err("Not a valid lvalue", E).
