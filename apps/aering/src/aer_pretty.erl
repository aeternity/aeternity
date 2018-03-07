%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Pretty printer for Ring.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aer_pretty).

-import(prettypr, [text/1, sep/1, above/2, beside/2, nest/2, empty/0]).

-export([decls/1, decls/2, decl/1, decl/2, expr/1, expr/2]).

-export_type([options/0]).

-type doc() :: prettypr:document().
-type options() :: [{indent, non_neg_integer()} | show_generated].

%% More options:
%%  Newline before open curly
%%  Space before ':'

%% -- Options ----------------------------------------------------------------

-define(aer_pretty_opts, aer_pretty_opts).

-spec options() -> options().
options() ->
    case get(?aer_pretty_opts) of
        undefined -> [];
        Opts      -> Opts
    end.

-spec option(atom(), any()) -> any().
option(Key, Default) ->
    proplists:get_value(Key, options(), Default).

-spec show_generated() -> boolean().
show_generated() -> option(show_generated, false).

-spec indent() -> non_neg_integer().
indent() -> option(indent, 2).

-spec with_options(options(), fun(() -> A)) -> A.
with_options(Options, Fun) ->
    put(?aer_pretty_opts, Options),
    Res = Fun(),
    erase(?aer_pretty_opts),
    Res.

%% -- Pretty printing helpers ------------------------------------------------

-spec par([doc()]) -> doc().
par(Ds) -> par(Ds, indent()).

-spec par([doc()], non_neg_integer()) -> doc().
par([], _) -> empty();
par(Ds, N) -> prettypr:par(Ds, N).

-spec follow(doc(), doc(), non_neg_integer()) -> doc().
follow(A, B, N) ->
    sep([A, nest(N, B)]).

-spec follow(doc(), doc()) -> doc().
follow(A, B) -> follow(A, B, indent()).

-spec above([doc()]) -> doc().
above([])       -> empty();
above([D])      -> D;
above([D | Ds]) -> lists:foldl(fun(X, Y) -> above(Y, X) end, D, Ds).

-spec beside([doc()]) -> doc().
beside([])       -> empty();
beside([D])      -> D;
beside([D | Ds]) -> lists:foldl(fun(X, Y) -> beside(Y, X) end, D, Ds).

-spec hsep([doc()]) -> doc().
hsep(Ds) -> beside(punctuate(text(" "), [ D || D <- Ds, D /= empty() ])).

-spec hsep(doc(), doc()) -> doc().
hsep(D1, D2) -> hsep([D1, D2]).

-spec punctuate(doc(), [doc()]) -> [doc()].
punctuate(_Sep, [])      -> [];
punctuate(_Sep, [D])     -> [D];
punctuate(Sep, [D | Ds]) -> [beside(D, Sep) | punctuate(Sep, Ds)].

-spec paren(doc()) -> doc().
paren(D) -> beside([text("("), D, text(")")]).

-spec paren(boolean(), doc()) -> doc().
paren(false, D) -> D;
paren(true,  D) -> paren(D).

-spec indent(doc()) -> doc().
indent(D) -> nest(indent(), D).

%% block(Header, Body) ->
%%  Header
%%      Body
-spec block(doc(), doc()) -> doc().
block(Header, Body) ->
    sep([ Header, indent(Body) ]).

-spec comma_brackets(string(), string(), [doc()]) -> doc().
comma_brackets(Open, Close, Ds) ->
    beside([text(Open), par(punctuate(text(","), Ds), 0), text(Close)]).

-spec tuple([doc()]) -> doc().
tuple(Ds) ->
    comma_brackets("(", ")", Ds).

-spec list([doc()]) -> doc().
list(Ds) ->
    comma_brackets("[", "]", Ds).

-spec record([doc()]) -> doc().
record(Ds) ->
    comma_brackets("{", "}", Ds).

%% equals(A, B) -> A = B
-spec equals(doc(), doc()) -> doc().
equals(A, B) -> follow(hsep(A, text("=")), B).

%% typed(A, B) -> A : B.
-spec typed(doc(), aer_syntax:type()) -> doc().
typed(A, Type) ->
    case aer_syntax:get_ann(origin, Type) == system andalso
         not show_generated() of
        true  -> A;
        false -> follow(hsep(A, text(":")), type(Type))
    end.

%% -- Exports ----------------------------------------------------------------

-spec decls([aer_syntax:decl()], options()) -> doc().
decls(Ds, Options) ->
    with_options(Options, fun() -> decls(Ds) end).

-spec decls([aer_syntax:decl()]) -> doc().
decls(Ds) -> above([ decl(D) || D <- Ds ]).

-spec decl(aer_syntax:decl(), options()) -> doc().
decl(D, Options) ->
    with_options(Options, fun() -> decl(D) end).

-spec decl(aer_syntax:decl()) -> doc().
decl({contract, _, C, Ds}) ->
    block(follow(text("contract"), hsep(name(C), text("="))), decls(Ds));
decl({type_decl, _, T, []})   -> hsep(text("type"), name(T));
decl({type_decl, _, T, Vars}) ->
    beside(hsep(text("type"), name(T)),
           tuple(lists:map(fun name/1, Vars)));
decl({type_def, Ann, T, Vars, Def}) ->
    equals(decl({type_decl, Ann, T, Vars}), typedef(Def));
decl({fun_decl, _, F, T}) ->
    hsep(text("function"), typed(name(F), T));
decl(D = {letfun, _, _, _, _, _}) -> letdecl("function", D);
decl(D = {letval, _, _, _, _})    -> letdecl("let", D);
decl(D = {letrec, _, _})          -> letdecl("let", D).

-spec expr(aer_syntax:expr(), options()) -> doc().
expr(E, Options) ->
    with_options(Options, fun() -> expr(E) end).

-spec expr(aer_syntax:expr()) -> doc().
expr(E) -> expr_p(0, E).

%% -- Not exported -----------------------------------------------------------

-spec name(aer_syntax:id() | aer_syntax:con() | aer_syntax:tvar()) -> doc().
name({id, _,   Name})  -> text(Name);
name({con, _,  Name})  -> text(Name);
name({qid, _,  Names}) -> text(string:join(Names, "."));
name({qcon, _, Names}) -> text(string:join(Names, "."));
name({tvar, _, Name})  -> text(Name).

-spec letdecl(string(), aer_syntax:letbind()) -> doc().
letdecl(Let, {letval, _, F, T, E}) ->
    block_expr(0, hsep([text(Let), typed(name(F), T), text("=")]), E);
letdecl(Let, {letfun, _, F, Args, T, E}) ->
    block_expr(0, hsep([text(Let), typed(beside(name(F), args(Args)), T), text("=")]), E);
letdecl(Let, {letrec, _, [D | Ds]}) ->
    hsep(text(Let), above([ letdecl("rec", D) | [ letdecl("and", D1) || D1 <- Ds ] ])).

-spec args([aer_syntax:arg()]) -> doc().
args(Args) ->
    tuple(lists:map(fun arg/1, Args)).

-spec arg(aer_syntax:arg()) -> doc().
arg({arg, _, X, T}) -> typed(name(X), T).

-spec typedef(aer_syntax:typedef()) -> doc().
typedef({alias_t, Type})           -> type(Type);
typedef({record_t, Fields})        ->
    record(lists:map(fun field_t/1, Fields));
typedef({variant_t, Constructors}) ->
    par(punctuate(text(" |"), lists:map(fun constructor_t/1, Constructors))).

-spec constructor_t(aer_syntax:constructor_t()) -> doc().
constructor_t({constr_t, _, C, []}) -> name(C);
constructor_t({constr_t, _, C, Args}) -> beside(name(C), tuple_type(Args)).

-spec field_t(aer_syntax:field_t()) -> doc().
field_t({field_t, _, immutable, Name, Type}) ->
    typed(name(Name), Type).

-spec type(aer_syntax:type()) -> doc().
type({fun_t, _, Args, Ret}) ->
    follow(hsep(tuple_type(Args), text("=>")), type(Ret));
type({app_t, _, Type, Args}) ->
    beside(type(Type), tuple_type(Args));
type({tuple_t, _, Args}) ->
    tuple_type(Args);
type(T = {id, _, _})   -> name(T);
type(T = {tvar, _, _}) -> name(T).

-spec tuple_type([aer_syntax:type()]) -> doc().
tuple_type(Args) ->
    tuple(lists:map(fun type/1, Args)).

-spec expr_p(integer(), aer_syntax:expr()) -> doc().
expr_p(P, {lam, _, Args, E}) ->
    paren(P > 100, follow(hsep(args(Args), text("=>")), expr_p(100, E)));
expr_p(P, If = {'if', Ann, Cond, Then, Else}) ->
    Format   = aer_syntax:get_ann(format, If),
    if  Format == '?:' ->
            paren(P > 100,
                follow(expr_p(200, Cond),
                follow(hsep(text("?"), expr_p(100, Then)),
                   hsep(text(":"), expr_p(100, Else)), 0)));
        true ->
            {Elifs, Else1} = get_elifs(Else),
            above([ stmt_p(Stmt) || Stmt <- [{'if', Ann, Cond, Then} | Elifs] ++ [Else1]])
    end;
expr_p(_P, {switch, _, E, Cases}) ->
    block(beside(text("switch"), paren(expr(E))),
          above(lists:map(fun alt/1, Cases)));
expr_p(_, {tuple, _, Es}) ->
    tuple(lists:map(fun expr/1, Es));
expr_p(_, {list, _, Es}) ->
    list(lists:map(fun expr/1, Es));
expr_p(_, {record, _, Fs}) ->
    record(lists:map(fun field/1, Fs));
expr_p(P, {record, Ann, E, Fs}) ->
    paren(P > 900, hsep(expr_p(900, E), expr({record, Ann, Fs})));
expr_p(_, {block, _, Ss}) ->
    block(empty(), statements(Ss));
expr_p(P, {proj, _, E, X}) ->
    paren(P > 900, beside([expr_p(900, E), text("."), name(X)]));
expr_p(P, {typed, _, E, T}) ->
    paren(P > 0, typed(expr(E), T));
expr_p(P, {assign, _, LV, E}) ->
    paren(P > 0, equals(expr_p(900, LV), expr(E)));
%% -- Operators
expr_p(_, {app, _, {'..', _}, [A, B]}) ->
    list([infix(0, '..', A, B)]);
expr_p(P, E = {app, _, F = {Op, _}, Args}) when is_atom(Op) ->
    case {aer_syntax:get_ann(format, E), Args} of
        {infix, [A, B]} -> infix(P, Op, A, B);
        {prefix, [A]}   -> prefix(P, Op, A);
        _               -> app(P, F, Args)
    end;
expr_p(P, {app, _, F, Args}) ->
    app(P, F, Args);
%% -- Constants
expr_p(_, E = {int, _, N}) ->
    S = case aer_syntax:get_ann(format, E) of
            hex -> "0x" ++ integer_to_list(N, 16);
            _   -> integer_to_list(N)
           end,
    text(S);
expr_p(_, {bool, _, B}) -> text(atom_to_list(B));
expr_p(_, {hash, _, <<N:256>>}) -> text("#" ++ integer_to_list(N, 16));
expr_p(_, {unit, _}) -> text("()");
expr_p(_, {string, _, S}) -> term(binary_to_list(S));
expr_p(_, {char, _, C}) ->
    case C of
        $' -> text("'\\''");
        $" -> text("'\"'");
        _  -> S = lists:flatten(io_lib:format("~p", [[C]])),
              text("'" ++ tl(lists:droplast(S)) ++ "'")
    end;
%% -- Names
expr_p(_, E = {id, _, _})   -> name(E);
expr_p(_, E = {con, _, _})  -> name(E);
expr_p(_, E = {qid, _, _})  -> name(E);
expr_p(_, E = {qcon, _, _}) -> name(E).

stmt_p({'if', _, Cond, Then}) ->
    block_expr(200, beside(text("if"), paren(expr(Cond))), Then);
stmt_p({elif, _, Cond, Then}) ->
    block_expr(200, beside(text("elif"), paren(expr(Cond))), Then);
stmt_p({else, Else}) ->
    HideGenerated = not show_generated(),
    case aer_syntax:get_ann(origin, Else) of
        system when HideGenerated -> empty();
        _ -> block_expr(200, text("else"), Else)
    end.

-spec bin_prec(aer_syntax:bin_op()) -> {integer(), integer(), integer()}.
bin_prec('..')   -> {  0,   0,   0};  %% Always printed inside '[ ]'
bin_prec('||')   -> {200, 300, 200};
bin_prec('&&')   -> {300, 400, 300};
bin_prec('<')    -> {400, 500, 500};
bin_prec('>')    -> {400, 500, 500};
bin_prec('=<')   -> {400, 500, 500};
bin_prec('>=')   -> {400, 500, 500};
bin_prec('==')   -> {400, 500, 500};
bin_prec('!=')   -> {400, 500, 500};
bin_prec('++')   -> {500, 600, 500};
bin_prec('::')   -> {500, 600, 500};
bin_prec('+')    -> {600, 600, 650};
bin_prec('-')    -> {600, 600, 650};
bin_prec('bor')  -> {600, 600, 650};
bin_prec('bxor') -> {600, 600, 650};
bin_prec('bsl')  -> {600, 600, 650};
bin_prec('bsr')  -> {600, 600, 650};
bin_prec('*')    -> {700, 700, 800};
bin_prec('/')    -> {700, 700, 800};
bin_prec(mod)    -> {700, 700, 800};
bin_prec('band') -> {700, 700, 800}.

-spec un_prec(aer_syntax:un_op()) -> {integer(), integer()}.
un_prec('-')    -> {650, 650};
un_prec('!')    -> {800, 800};
un_prec('bnot') -> {800, 800}.

-spec infix(integer(), aer_syntax:bin_op(), aer_syntax:expr(), aer_syntax:expr()) -> doc().
infix(P, Op, A, B) ->
    {Top, L, R} = bin_prec(Op),
    paren(P > Top,
        follow(hsep(expr_p(L, A), text(atom_to_list(Op))),
               expr_p(R, B))).

prefix(P, Op, A) ->
    {Top, Inner} = un_prec(Op),
    paren(P > Top, hsep(text(atom_to_list(Op)), expr_p(Inner, A))).

app(P, F, Args) ->
    paren(P > 900,
    beside(expr_p(900, F),
           tuple(lists:map(fun expr/1, Args)))).

field({field, _, LV, E}) ->
    follow(hsep(expr_p(900, LV), text("=")), expr(E)).

alt({'case', _, Pat, Body}) ->
    block_expr(0, hsep(expr_p(500, Pat), text("=>")), Body).

block_expr(_, Header, {block, _, Ss}) ->
    block(Header, statements(Ss));
block_expr(P, Header, E) ->
    follow(Header, expr_p(P, E)).

statements(Stmts) ->
    above([ statement(S) || S <- Stmts ]).

statement(S = {letval, _, _, _, _})    -> letdecl("let", S);
statement(S = {letfun, _, _, _, _, _}) -> letdecl("let", S);
statement(S = {letrec, _, _})          -> letdecl("let", S);
statement(E) -> expr(E).

get_elifs(Expr) -> get_elifs(Expr, []).

get_elifs(If = {'if', Ann, Cond, Then, Else}, Elifs) ->
    case aer_syntax:get_ann(format, If) of
        elif -> get_elifs(Else, [{elif, Ann, Cond, Then} | Elifs]);
        _    -> {lists:reverse(Elifs), If}
    end;
get_elifs(Else, Elifs) -> {lists:reverse(Elifs), {else, Else}}.

fmt(Fmt, Args) -> text(lists:flatten(io_lib:format(Fmt, Args))).
term(X) -> fmt("~p", [X]).

