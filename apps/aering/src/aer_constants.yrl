%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Ring constant parser.
%%%
%%% @end
%%%-------------------------------------------------------------------

Nonterminals

'Constant' 'Expr' 'Expr100' 'Expr200' 'Expr300' 'Expr400' 'Expr500' 'Expr600' 'Expr650' 'Expr700'
'Expr800' 'Expr900' 'ExprAtom' 'Exprs' 'Exprs1'
'AddOp' 'MulOp' 'CmpOp' 'ConsOp' 'AndOp' 'OrOp' 'NotOp'
.

Terminals

%% Keywords


%% Identifiers and literals
id qid con qcon 
int hex bool hash string char

%% Symbols
':' ',' '(' ')' '[' ']' '?'
'+' '-' '*' '/' 'mod' '++' '::' '..'
'<' '>' '=<' '>=' '==' '!='
'||' '&&' '!' 'bor' 'band' 'bxor' 'bsl' 'bsr' 'bnot'
.

Rootsymbol 'Constant'.

%% Top-level constant
'Constant' -> 'Expr' : '$1'.

%% -- Expressions ------------------------------------------------------------

'Expr' -> 'Expr100' : '$1'.

'Expr100' -> 'Expr200' '?' 'Expr100' ':' 'Expr100' : set_ann(format, '?:', {'if', get_ann('$1'), '$1', '$3', '$5'}).
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

'Expr800' -> 'NotOp' 'Expr800' : prefix('$1', '$2').
'Expr800' -> 'Expr900' : '$1'.

'Expr900' -> 'Expr900' '(' 'Exprs' ')'      : {tuple, '$2'}.
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
'ExprAtom' -> '[' 'Expr' '..' 'Expr' ']' : infix('$2', token('$3'), '$4').

'Exprs' -> '$empty'      : [].
'Exprs' -> 'Exprs1' : '$1'.

'Exprs1' -> 'Expr' : ['$1'].
'Exprs1' -> 'Expr' ',' 'Exprs' : ['$1' | '$3'].

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

'AddOp' -> '+'    : token('$1').
'AddOp' -> '-'    : token('$1').
'AddOp' -> 'bor'  : token('$1').
'AddOp' -> 'bxor' : token('$1').
'AddOp' -> 'bsr'  : token('$1').
'AddOp' -> 'bsl'  : token('$1').

'MulOp' -> '*'    : token('$1').
'MulOp' -> '/'    : token('$1').
'MulOp' -> 'mod'  : token('$1').
'MulOp' -> 'band' : token('$1').

'NotOp' -> '!'    : token('$1').
'NotOp' -> 'bnot' : token('$1').

Erlang code.

-export([string/1]).

-ignore_xref([format_error/1, parse_and_scan/1]).

string(S) ->
    case aer_scan:scan(S) of
        {ok, Toks, _} -> parse(Toks);
        Err           -> Err
    end.


-type ann()      :: aer_syntax:ann().
-type ann_line() :: aer_syntax:ann_line().

-spec line_ann(ann_line()) -> ann().
line_ann(Line) -> [{line, Line}].


get_ann(Node) ->
    case element(2, Node) of
        Line when is_integer(Line) -> line_ann(Line);
        Ann -> Ann
    end.

set_ann(Key, Val, Node) ->
    Ann = get_ann(Node),
    setelement(2, Node, lists:keystore(Key, 1, Ann, {Key, Val})).


token({Tok, Line}) -> {Tok, line_ann(Line)};
token({Tok, Line, Val}) -> {Tok, line_ann(Line), Val}.

infix(L, Op, R) -> set_ann(format, infix, {app, get_ann(L), Op, [L, R]}).
prefix(Op, E)   -> set_ann(format, prefix, {app, get_ann(Op), Op, [E]}).






