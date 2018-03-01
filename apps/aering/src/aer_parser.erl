%%% File        : aer_parser.erl
%%% Author      : Ulf Norell
%%% Description :
%%% Created     : 1 Mar 2018 by Ulf Norell
-module(aer_parser).

-export([string/1]).

-include("aer_parse_lib.hrl").

-spec string(string()) -> {ok, [aer_syntax:decl()]} | {error, {aer_parse_lib:pos(), atom(), term()}}.
string(String) ->
  parse_and_scan(file(), String).

parse_and_scan(P, S) ->
  case aer_scan:scan(S) of
    {ok, Tokens} -> aer_parse_lib:parse(P, Tokens);
    Error        -> Error
  end.

%% -- Parsing rules ----------------------------------------------------------

file() -> choice([], block(decl())).

decl() ->
    ?LAZY_P(
    choice(
      %% Contract declaration
    [ ?RULE(keyword(contract), con(), tok('='), maybe_block(decl()), {contract, _1, _2, _4})

      %% Type declarations  TODO: format annotation for "type bla" vs "type bla()"
    , ?RULE(keyword(type), id(),                                   {type_decl, _1, _2, []})
    , ?RULE(keyword(type), id(), type_vars(),                      {type_decl, _1, _2, _3})
    , ?RULE(keyword(type), id(),              tok('='), typedef(), {type_def, _1, _2, [], _4})
    , ?RULE(keyword(type), id(), type_vars(), tok('='), typedef(), {type_def, _1, _2, _3, _5})

      %% Function declarations
    , ?RULE(keyword(function), id(), tok(':'), type(),             {fun_decl, _1, _2, _4})
    , ?RULE(keyword(function), fundef(),                           set_pos(get_pos(_1), _2))
    , ?RULE(keyword('let'),    valdef(),                           set_pos(get_pos(_1), _2))
    ])).

%% -- Type declarations ------------------------------------------------------

typedef() ->
    choice(
    [ ?RULE(type(),                   {alias_t, _1})
    , ?RULE(brace_list(field_type()), {record_t, _1})
    , ?RULE(constructors(),           {variant_t, _1})
    ]).

constructors() ->
    sep1(constructor(), tok('|')).

constructor() ->    %% TODO: format for Con() vs Con
    choice(?RULE(con(),              {constr_t, get_ann(_1), _1, []}),
           ?RULE(con(), type_args(), {constr_t, get_ann(_1), _1, _2})).

type_args()  -> paren_list(type()).
field_type() -> ?RULE(id(), tok(':'), type(), {field_t, get_ann(_1), immutable, _1, _3}).

%% -- Let declarations -------------------------------------------------------

letdecl() ->
    choice(
        ?RULE(keyword('let'), letdef(),                             set_pos(get_pos(_1), _2)),
        ?RULE(keyword('let'), tok(rec), sep1(letdef(), tok('and')), {letrec, _1, _3})).

letdef() -> choice(valdef(), fundef()).

valdef() ->
    choice(
        ?RULE(id(),                   tok('='), body(), {letval, [], _1, type_wildcard(), _3}),
        ?RULE(id(), tok(':'), type(), tok('='), body(), {letval, [], _1, _3, _5})).

fundef() ->
    choice(
    [ ?RULE(id(), args(),                   tok('='), body(), {letfun, [], _1, _2, type_wildcard(), _4})
    , ?RULE(id(), args(), tok(':'), type(), tok('='), body(), {letfun, [], _1, _2, _4, _6})
    ]).

args() -> paren_list(arg()).

arg() -> choice(
    ?RULE(id(),                   {arg, get_ann(_1), _1, type_wildcard()}),
    ?RULE(id(), tok(':'), type(), {arg, get_ann(_1), _1, _3})).

%% -- Types ------------------------------------------------------------------

type_vars() -> paren_list(tvar()).

type() -> ?LAZY_P(type100()).

type100() -> type200().

type200() ->
    ?RULE(many({fun_domain(), keyword('=>')}), type300(), fun_t(_1, _2)).

type300() -> type400().

type400() ->
    ?RULE(typeAtom(), optional(type_args()),
          case _2 of
            none       -> _1;
            {ok, Args} -> {app_t, get_ann(_1), _1, Args}
          end).

typeAtom() ->
    ?LAZY_P(choice(
    [ id(), token(qid), tvar()
    , ?RULE(keyword('('), comma_sep(type()), tok(')'), tuple_t(_1, _2))
    ])).

fun_domain() -> ?RULE(?LAZY_P(type300()), fun_domain(_1)).

%% -- Statements -------------------------------------------------------------

body() ->
    ?LET_P(Stmts, maybe_block(stmt()), block_e(Stmts)).

stmt() ->
    ?LAZY_P(choice(
    [ expr()
    , letdecl()
    , {switch, keyword(switch), parens(expr()), maybe_block(branch())}
    , {'if', keyword('if'), parens(expr()), body()}
    , {elif, keyword(elif), parens(expr()), body()}
    , {else, keyword(else), body()}
    ])).

branch() ->
    ?RULE(pattern(), keyword('=>'), body(), {'case', _2, _1, _3}).

pattern() ->
    ?LET_P(E, expr500(), parse_pattern(E)).

%% -- Expressions ------------------------------------------------------------

expr() -> expr100().

expr100() ->
    Expr100 = ?LAZY_P(expr100()),
    Expr200 = ?LAZY_P(expr200()),
    choice(
    [ ?RULE(args(), keyword('=>'), body(), {lam, _2, _1, _3})   %% TODO: better location
    , {'if', keyword('if'), parens(Expr100), Expr200, right(tok(else), Expr100)}
    , ?RULE(Expr200, optional(right(tok(':'), type())),
            case _2 of
                none       -> _1;
                {ok, Type} -> {typed, get_ann(_1), _1, Type}
            end)
    ]).

expr200() -> infixr(expr300(), binop('||')).
expr300() -> infixr(expr400(), binop('&&')).
expr400() -> infix(expr500(),  binop(['<', '>', '=<', '>=', '==', '!='])).
expr500() -> infixr(expr600(), binop(['::', '++'])).
expr600() -> infixl(expr650(), binop(['+', '-', 'bor', 'bxor', 'bsr', 'bsl'])).
expr650() -> ?RULE(many(token('-')), expr700(), prefixes(_1, _2)).
expr700() -> infixl(expr800(), binop(['*', '/', mod, 'band'])).
expr800() -> ?RULE(many(choice(token('!'), token('bnot'))), expr900(), prefixes(_1, _2)).
expr900() -> ?RULE(exprAtom(), many(elim()), elim(_1, _2)).

exprAtom() ->
    ?LAZY_P(begin
        Expr = ?LAZY_P(expr()),
        choice(
        [ id(), con(), token(qid), token(qcon)
        , token(hash), token(string), token(char)
        , token(int)
        , ?RULE(token(hex), set_ann(format, hex, setelement(1, _1, int)))
        , {bool, keyword(true), true}
        , {bool, keyword(false), false}
        , {record, [], brace_list(?LAZY_P(field_assignment()))}
        , {list, [], bracket_list(Expr)}
        , ?RULE(tok('['), Expr, binop('..'), Expr, tok(']'), _3(_2, _4))
        , ?RULE(keyword('('), comma_sep(Expr), tok(')'), tuple_e(_1, _2))
        ])
    end).

elim() ->
    ?LAZY_P(
    choice(
    [ {proj, keyword('.'), id()}
    , ?RULE(paren_list(expr()), {app, get_ann(_1), _1})
    , ?RULE(keyword('{'), comma_sep(field_assignment()), tok('}'), {rec_upd, _1, _2})
    %% TODO: map lookup
    ])).

elim(E, [])                          -> E;
elim(E, [{proj, Ann, P} | Es])       -> elim({proj, Ann, E, P}, Es);
elim(E, [{app, Ann, Args} | Es])     -> elim({app, Ann, E, Args}, Es);
elim(E, [{rec_upd, Ann, Flds} | Es]) -> elim({record, Ann, E, Flds}, Es).


field_assignment() ->
    ?RULE(lvalue(), tok('='), expr(), {field, get_ann(_1), _1, _3}).


lvalue() ->
    ?LET_P(E, expr900(),
    case E of
        E = {proj, _, _, _} -> E;
        E = {id, _, _}      -> E;   %% TODO: map lvalues
        E -> bad_expr_err("Not a valid lvalue", E)
    end).

infix(E, Op) ->
    ?RULE(E, optional({Op, E}),
    case _2 of
        none -> _1;
        {ok, {F, Arg}} -> F(_1, Arg)
    end).

binop(Op) when is_atom(Op) -> binop([Op]);
binop(Ops) ->
    ?RULE(choice([ token(Op) || Op <- Ops ]), fun(A, B) -> infix(A, _1, B) end).

con()      -> token(con).
id()       -> token(id).
tvar()     -> token(tvar).

token(Tag) ->
    ?RULE(tok(Tag),
    case _1 of
        {Tok, {Line, Col}}      -> {Tok, pos_ann(Line, Col)};
        {Tok, {Line, Col}, Val} -> {Tok, pos_ann(Line, Col), Val}
    end).

%% -- Helpers ----------------------------------------------------------------

keyword(K) -> ann(tok(K)).
ann(P)     -> map(fun get_ann/1, P).

block(P) ->
    between(layout(), sep1(P, tok(vsemi)), tok(vclose)).

maybe_block(P) ->
    choice(block(P), [P]).

parens(P)   -> between(tok('('), P, tok(')')).
braces(P)   -> between(tok('{'), P, tok('}')).
brackets(P) -> between(tok('['), P, tok(']')).
comma_sep(P) -> sep(P, tok(',')).

paren_list(P)   -> parens(comma_sep(P)).
brace_list(P)   -> braces(comma_sep(P)).
bracket_list(P) -> brackets(comma_sep(P)).

%% -- Annotations ------------------------------------------------------------

-type ann()      :: aer_syntax:ann().
-type ann_line() :: aer_syntax:ann_line().
-type ann_col()  :: aer_syntax:ann_col().

-spec pos_ann(ann_line(), ann_col()) -> ann().
pos_ann(Line, Col) -> [{line, Line}, {col, Col}].

ann_pos(Ann) ->
    {proplists:get_value(line, Ann),
     proplists:get_value(col, Ann)}.

get_ann(Ann) when is_list(Ann) -> Ann;
get_ann(Node) ->
    case element(2, Node) of
        {Line, Col} when is_integer(Line), is_integer(Col) -> pos_ann(Line, Col);
        Ann -> Ann
    end.

get_ann(Key, Node) ->
    proplists:get_value(Key, get_ann(Node)).

set_ann(Key, Val, Node) ->
    Ann = get_ann(Node),
    setelement(2, Node, lists:keystore(Key, 1, Ann, {Key, Val})).

get_pos(Node) ->
    {get_ann(line, Node), get_ann(col, Node)}.

set_pos({L, C}, Node) ->
    set_ann(line, L, set_ann(col, C, Node)).

infix(L, Op, R) -> set_ann(format, infix,  {app, get_ann(L), Op, [L, R]}).

prefixes(Ops, E) -> lists:foldr(fun prefix/2, E, Ops).
prefix(Op, E)    -> set_ann(format, prefix, {app, get_ann(Op), Op, [E]}).

type_wildcard() ->
    {id, [{origin, system}], "_"}.

block_e(Stmts) ->
    group_ifs(Stmts, []).

group_ifs([], [Stmt]) -> return(Stmt);
group_ifs([], Acc) ->
    Stmts = [Stmt | _] = lists:reverse(Acc),
    {block, get_ann(Stmt), Stmts};
group_ifs([{'if', Ann, Cond, Then} | Stmts], Acc) ->
    {Elses, Rest} = else_branches(Stmts, []),
    group_ifs(Rest, [build_if(Ann, Cond, Then, Elses) | Acc]);
group_ifs([{else, Ann, _} | _], _) ->
    fail({Ann, "No matching 'if' for 'else'"});
group_ifs([{elif, Ann, _, _} | _], _) ->
    fail({Ann, "No matching 'if' for 'elif'"});
group_ifs([Stmt | Stmts], Acc) ->
    group_ifs(Stmts, [Stmt | Acc]).

build_if(Ann, Cond, Then, [{elif, Ann1, Cond1, Then1} | Elses]) ->
    {'if', Ann, Cond, Then,
        set_ann(format, elif, build_if(Ann1, Cond1, Then1, Elses))};
build_if(Ann, Cond, Then, [{else, _Ann, Else}]) ->
    {'if', Ann, Cond, Then, Else};
build_if(Ann, Cond, Then, []) ->
    {'if', Ann, Cond, Then, {unit, [{origin, system}]}}.

else_branches([Elif = {elif, _, _, _} | Stmts], Acc) ->
    else_branches(Stmts, [Elif | Acc]);
else_branches([Else = {else, _, _} | Stmts], Acc) ->
    {lists:reverse([Else | Acc]), Stmts};
else_branches(Stmts, Acc) ->
    {lists:reverse(Acc), Stmts}.

tuple_t(_Ann, [Type]) -> Type;  %% Not a tuple
tuple_t(Ann, Types)   -> {tuple_t, Ann, Types}.

fun_t(Domains, Type) ->
    lists:foldr(fun({Dom, Ann}, T) -> fun_t(Ann, Dom, T) end,
                Type, Domains).

fun_t(Ann, Domain, {fun_t, _Ann, Domain1, Result}) ->
  {fun_t, Ann, Domain ++ Domain1, Result};
fun_t(Ann, Domain, Result) -> {fun_t, Ann, Domain, Result}.

tuple_e(Ann, [])      -> {unit, Ann};
tuple_e(_Ann, [Expr]) -> Expr;  %% Not a tuple
tuple_e(Ann, Exprs)   -> {tuple, Ann, Exprs}.

%% TODO: not nice
fun_domain({tuple_t, _, Args}) -> Args;
fun_domain(T)                  -> [T].

-spec parse_pattern(aer_syntax:expr()) -> aer_parse_lib:parser(aer_syntax:pat()).
parse_pattern({app, Ann, Con = {'::', _}, Es}) ->
    {app, Ann, Con, lists:map(fun parse_pattern/1, Es)};
parse_pattern({app, Ann, Con = {con, _, _}, Es}) ->
    {app, Ann, Con, lists:map(fun parse_pattern/1, Es)};
parse_pattern({tuple, Ann, Es}) ->
    {tuple, Ann, lists:map(fun parse_pattern/1, Es)};
parse_pattern({list, Ann, Es}) ->
    {list, Ann, lists:map(fun parse_pattern/1, Es)};
parse_pattern({record, Ann, Fs}) ->
    {record, Ann, lists:map(fun parse_field_pattern/1, Fs)};
parse_pattern(E = {con, _, _})    -> E;
parse_pattern(E = {id, _, _})     -> E;
parse_pattern(E = {unit, _})      -> E;
parse_pattern(E = {int, _, _})    -> E;
parse_pattern(E = {true, _})      -> E;
parse_pattern(E = {false, _})     -> E;
parse_pattern(E = {hash, _, _})   -> E;
parse_pattern(E = {string, _, _}) -> E;
parse_pattern(E = {char, _, _})   -> E;
parse_pattern(E) -> bad_expr_err("Not a valid pattern", E).

-spec parse_field_pattern(aer_syntax:field(aer_syntax:expr())) -> aer_parse_lib:parser(aer_syntax:field(aer_syntax:pat())).
parse_field_pattern({field, Ann, F, E}) ->
    {field, Ann, F, parse_pattern(E)}.

return_error(Pos, Err) ->
    fail({parse_error, Pos, Err}).

-spec ret_doc_err(ann(), prettypr:document()) -> no_return().
ret_doc_err(Ann, Doc) ->
    return_error(ann_pos(Ann), prettypr:format(Doc)).

-spec bad_expr_err(string(), aer_syntax:expr()) -> no_return().
bad_expr_err(Reason, E) ->
  ret_doc_err(get_ann(E),
              prettypr:sep([prettypr:text(Reason ++ ":"),
                            prettypr:nest(2, aer_pretty:expr(E))])).

