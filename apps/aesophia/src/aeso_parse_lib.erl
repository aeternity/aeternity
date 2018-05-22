%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Parser combinators for the Sophia parser. Based on
%%%      Koen Claessen. 2004. Parallel Parsing Processes. J. Functional
%%%      Programming 14, 6 (November 2004)
%%% @end
%%%-------------------------------------------------------------------
-module(aeso_parse_lib).

-export([parse/2,
         return/1, fail/0, fail/1, map/2, bind/2,
         lazy/1, choice/1, choice/2, tok/1, layout/0,
         left/2, right/2, between/3, optional/1,
         many/1, many1/1, sep/2, sep1/2,
         infixl/2, infixr/2]).

%% -- Types ------------------------------------------------------------------

-export_type([parser/1, parser_expr/1, pos/0, token/0, tokens/0]).

-type pos()    :: {integer(), integer()}.
-type token()  :: {atom(), pos(), term()} | {atom(), pos()}.
-type tokens() :: [token()].
-type error()  :: {pos(), string()}.

-define(lazy(F),     {aeso_parse_lazy, F}).
-define(fail(Err),   {aeso_parse_fail, Err}).
-define(choice(Ps),  {aeso_parse_choice, Ps}).
-define(bind(P, F),  {aeso_parse_bind, P, F}).
-define(right(P, Q), {aeso_parse_right, P, Q}).
-define(left(P, Q),  {aeso_parse_left, P, Q}).
-define(map(F, P),   {aeso_parse_map, F, P}).
-define(layout,       aeso_parse_layout).
-define(tok(Atom),   {aeso_parse_tok, Atom}).
-define(return(X),   {aeso_parse_return, X}).

%% Type synonyms since you can't have function types as macro arguments for some reason.
-type delayed(A)         :: fun(() -> A).
-type continuation(A, B) :: fun((A) -> parser(B)).
-type function(A, B)     :: fun((A) -> B).

%% The representation of parsers that the user writes. These get compiled down to a lower-level
%% representation before parsing (parser1/1).
-opaque parser_expr(A)
    :: ?lazy(delayed(parser(A)))
     | ?fail(term())
     | ?choice([parser(A)])
     | ?bind(parser(B), continuation(B, A))
     | ?map(function(B, A), parser(B))
     | ?left(parser(A), parser(A))
     | ?right(parser(A), parser(A)).

%% Lists, tuples and maps of parsers are valid parsers. These are applied in left-to-right order and
%% a list/tuple/map is built out of the results. For maps only the values (and not the keys) can be
%% parsers.
-type parser(A) :: parser_expr(A)
                 | maybe_improper_list(parser(_), parser(_))
                 | tuple()          %% A = tuple()
                 | term().  %% Interpreted as a parser that returns the term without consuming input

%% The low level parser representation. This is what's used when doing the
%% actual parsing (see parse1/2).
-type parser1(A) :: {tok_bind, #{atom() => fun((token()) -> parser1(A))}}
                    %% ^ Consume a token and dispatch on its tag.
                  | {fail, term()}
                    %% ^ Fail with the given error
                  | {return_plus, A, parser1(A)}
                    %% ^ Choice between returning a value and continue parsing
                  | {layout, fun((integer()) -> parser1(A)), parser1(A)}.
                    %% ^ Parse a layout block. If a layout block can be started, it commits to the
                    %%   first argument. I.e. no backtracking to the second argument if the first
                    %%   fails.

%% Apply a parser to its continuation. This compiles a parser to its low-level representation.
-spec apply_p(parser(A), fun((A) -> parser1(B))) -> parser1(B).
apply_p(?lazy(F), K)           -> apply_p(F(), K);
apply_p(?fail(Err), _)         -> {fail, Err};
apply_p(?choice([P | Ps]), K)  -> lists:foldl(fun(Q, R) -> choice1(apply_p(Q, K), R) end,
                                             apply_p(P, K), Ps);
apply_p(?bind(P, F), K)        -> apply_p(P, fun(X) -> apply_p(F(X), K) end);
apply_p(?right(P, Q), K)       -> apply_p(P, fun(_) -> apply_p(Q, K) end);
apply_p(?left(P, Q), K)        -> apply_p(P, fun(X) -> apply_p(Q, fun(_) -> K(X) end) end);
apply_p(?map(F, P), K)         -> apply_p(P, fun(X) -> K(F(X)) end);
apply_p(?layout, K)            -> {layout, K, {fail, {expected, layout_block}}};
apply_p(?tok(Atom), K)         -> {tok_bind, #{Atom => K}};
apply_p(?return(X), K)         -> K(X);
apply_p([P | Q], K)            -> apply_p(P, fun(H) -> apply_p(Q, fun(T) -> K([H | T]) end) end);
apply_p(T, K) when is_tuple(T) -> apply_p(tuple_to_list(T), fun(Xs) -> K(list_to_tuple(Xs)) end);
apply_p(M, K) when is_map(M) ->
    {Keys, Ps} = lists:unzip(maps:to_list(M)),
    apply_p(Ps, fun(Vals) -> K(maps:from_list(lists:zip(Keys, Vals))) end);
apply_p(X, K) -> K(X).

%% -- Primitive combinators --------------------------------------------------

%% @doc Create a delayed parser. Required when building recursive parsers to avoid looping.
-spec lazy(fun(() -> parser(A))) -> parser(A).
lazy(Delayed) -> ?lazy(Delayed).

%% @doc A parser that always fails.
-spec fail(term()) -> parser(none()).
fail(Err) -> ?fail(Err).

%% @doc Fail with no error message.
-spec fail() -> parser(none()).
fail() -> fail(no_error).

%% @doc A choice between two parsers. Succeeds if either parser succeeds.
-spec choice(parser(A), parser(A)) -> parser(A).
choice(?choice(Ps), ?choice(Qs)) -> ?choice(Ps ++ Qs);
choice(?choice(Ps), Q)           -> ?choice([Q | Ps]);
choice(P, ?choice(Qs))           -> ?choice([P | Qs]);
choice(P, Q)                     -> ?choice([P, Q]).

%% @doc A choice between a list of parsers. Applies 'choice/2' repeatedly.
-spec choice([parser(A)]) -> parser(A).
choice([])       -> fail(empty_choice);
choice([P])      -> P;
choice([P | Ps]) -> choice(P, choice(Ps)).

%% @doc Parse a single token with the given tag.
-spec tok(atom()) -> parser(token()).
tok(Atom) -> ?tok(Atom).

%% @doc Apply two parsers in sequence and return the result from the first one.
-spec left(parser(A), parser(_)) -> parser(A).
left(P, Q)  -> ?left(P, Q).

%% @doc Apply two parsers in sequence and return the result from the second one.
-spec right(parser(_), parser(A)) -> parser(A).
right(P, Q) -> ?right(P, Q).

%% @doc A parser that always succeeds with the given value.
-spec return(A) -> parser(A).
return(X) -> ?return(X).

%% @doc Monadic bind. Lets you inspect the result of the first parser before deciding on what to
%% parse next.
-spec bind(parser(A), fun((A) -> parser(B))) -> parser(B).
bind(?return(X), F) -> F(X);
bind(P, F) -> ?bind(P, F).

%% @doc Apply a function to the result of a parser.
-spec map(fun((A) -> B), parser(A)) -> parser(B).
map(Fun, P) -> ?map(Fun, P).

%% @doc Parse the start of a layout block. A layout block can start if the next token is not on the
%%      same line as the previous token and it is indented further than the current layout block (if
%%      any). The result is the column of the new layout block (i.e. the column of the next token).
-spec layout() -> parser(integer()).
layout() -> ?layout.

%% @doc Parse a sequence of tokens using a parser. Fails if the parse is ambiguous.
-spec parse(parser(A), tokens()) -> {ok, A} | {error, term()}.
parse(P, S) ->
  case parse1(apply_p(P, fun(X) -> {return_plus, X, {fail, no_error}} end), S) of
    {[], {Pos, Err}} -> {error, {Pos, parse_error, lists:flatten(Err)}};
    {[A], _}         -> {ok, A};
    {As, _}          -> {error, {{1, 1}, ambiguous_parse, As}}
  end.

%% -- Derived combinators ----------------------------------------------------

%% @doc Parse zero or more A's.
-spec many(parser(A)) -> parser([A]).
many(P)  -> choice([], many1(P)).

-dialyzer({nowarn_function, many1/1}).  %% Silence improper_list warning.
%% @doc Parse one or more A's.
-spec many1(parser(A)) -> parser([A]).
many1(P) -> [P | lazy(fun() -> many(P) end)].

%% @doc Parse zero or more A's, separated by Sep.
-spec sep(parser(A), parser(_)) -> parser([A]).
sep(P, Sep)  -> choice([], sep1(P, Sep)).

-dialyzer({nowarn_function, sep1/2}).   %% Silence improper_list warning.
%% @doc Parse one or more A's, separated by Sep.
-spec sep1(parser(A), parser(_)) -> parser([A]).
sep1(P, Sep) -> [P | many(right(Sep, P))].

%% @doc Parse a left-associative operator. <p>
%%      <tt>infixl(Elem, Op) ::= Elem | infixl(Elem, Op) Op Elem</tt>
%% </p>
-spec infixl(parser(A), parser(fun((A, A) -> A))) -> parser(A).
infixl(Elem, Op) ->
    bind(Elem,             fun(A) ->
    bind(many({Op, Elem}), fun(Ops) ->
    return(build_infixl(A, Ops)) end) end).

%% @doc Parse a right-associative operator. <p>
%%      <tt>infixr(Elem, Op) ::= Elem | Elem Op infixl(Elem, Op)</tt>
%% </p>
-spec infixr(parser(A), parser(fun((A, A) -> A))) -> parser(A).
infixr(Elem, Op) ->
    bind(Elem,             fun(A) ->
    bind(many({Op, Elem}), fun(Ops) ->
    return(build_infixr(A, Ops)) end) end).

build_infixl(A, [])              -> A;
build_infixl(A, [{Op, B} | Ops]) -> build_infixl(Op(A, B), Ops).

build_infixr(A, [])              -> A;
build_infixr(A, [{Op, B} | Ops]) -> Op(A, build_infixr(B, Ops)).

%% @doc Parse an A between two other things (typically brackets of some kind).
-spec between(parser(_), parser(A), parser(_)) -> parser(A).
between(L, P, R) ->
    right(L, left(P, R)).

-spec optional(parser(A)) -> parser(none | {ok, A}).
optional(P) -> choice(none, {ok, P}).

%% -- Internal functions -----------------------------------------------------

-spec tag(token()) -> atom().
tag(T)  when is_tuple(T) -> element(1, T).

-spec pos(token()) -> pos().
pos(T)  when is_tuple(T) -> element(2, T).

-spec line(token()) -> integer().
line(T) when is_tuple(T) -> element(1, pos(T)).

-spec col(token()) -> integer().
col(T)  when is_tuple(T) -> element(2, pos(T)).

%% Choice on low-level parsers.
-spec choice1(parser1(A), parser1(A)) -> parser1(A).

%% If both parsers want the next token we grab it and merge the continuations.
choice1({tok_bind, Map1}, {tok_bind, Map2}) ->
    {tok_bind, merge_with(fun(F, G) -> fun(T) -> choice1(F(T), G(T)) end end, Map1, Map2)};

%% If both parsers fail we combine the error messages. If only one fails we discard it.
choice1({fail, E1}, {fail, E2})             -> {fail, add_error(E1, E2)};
choice1({fail, _}, Q)                       -> Q;
choice1(P, {fail, _})                       -> P;

%% If either side can deliver a value, then so can the choice.
choice1({return_plus, X, P}, Q)             -> {return_plus, X, choice1(P, Q)};
choice1(P, {return_plus, X, Q})             -> {return_plus, X, choice1(P, Q)};

%% If both sides want a layout block we combine them. If only one side wants a layout block we
%% will commit to a layout block is there is one.
choice1({layout, F, P}, {layout, G, Q})     ->
    {layout, fun(N) -> choice1(F(N), G(N)) end, choice1(P, Q)};
choice1({layout, F, P}, Q)                  -> {layout, F, choice1(P, Q)};
choice1(P, {layout, G, Q})                  -> {layout, G, choice1(P, Q)}.

%% Token stream representation. This is the state of the parse function.
-record(ts, {layout   :: [integer()], %% Column numbers of the current layout blocks.
             last     :: token(),     %% The previously consumed token.
             inserted :: tokens(),    %% Inserted layout tokens, consumed before 'tokens'.
             tokens   :: tokens()}).  %% The remaining tokens to be parsed.

%% The initial token stream.
ts(S) ->
    #ts{ layout = [], last = {bof, {0, 0}}, inserted = [], tokens = S }.

%% The parse function. Parses a token stream returning a list of results and an error message in
%% case of failure.
-spec parse1(parser1(A), tokens()) -> {[A], term()}.
parse1(P, S) ->
    parse1(P, ts(S), [], no_error).

%% The main work horse. Returns a list of possible parses and an error message in case parsing
%% fails.
-spec parse1(parser1(A), #ts{}, [A], term()) -> {[A], error()}.
parse1({tok_bind, Map}, Ts, Acc, Err) ->
    case next_token(Ts) of
        {T, Ts1} ->
            case maps:get(tag(T), Map, '$not_found') of
                '$not_found' ->
                    %% Insert a vclose (if required) on unexpected tokens. This lets you have layout
                    %% blocks inside parens without having to put the closing paren on a separate
                    %% line. Example:
                    %%      ((x) =>
                    %%            let y = x + 1
                    %%            y + y)(4)
                    case maps:get(vclose, Map, '$not_found') of
                        '$not_found' ->
                            {Acc, mk_error(Ts, io_lib:format("Unexpected token '~s'.", [tag(T)]))};
                        F ->
                            VClose = {vclose, pos(T)},
                            Ts2    = pop_layout(VClose, Ts#ts{ last = VClose }),
                            parse1(F(VClose), Ts2, Acc, Err)
                    end;
                F -> parse1(F(T), Ts1, Acc, Err)
            end;
        false ->
            {Acc, mk_error(Ts, io_lib:format("Unexpected end of file. Expected one of ~p.",
                                             [maps:keys(Map)]))}
    end;
parse1({layout, F, P}, Ts, Acc, Err) ->
    case start_layout(Ts) of
        {Col, Ts1} -> parse1(F(Col), Ts1, Acc, Err);
        false      -> parse1(P, Ts, Acc, mk_error(Ts, "Expected layout block."))
    end;
parse1({return_plus, X, P}, Ts, Acc,  Err) ->
    case next_token(Ts) of
        false  -> parse1(P, Ts, [X | Acc], Err);
        {T, _} -> parse1(P, Ts, Acc, mk_error(Ts, io_lib:format("Unexpected token ~p", [tag(T)])))
    end;
parse1({fail, Err}, Ts, Acc, Err1) -> {Acc, add_error(mk_error(Ts, Err), Err1)}.

%% Get the current position of the token stream. This is the position of the next token if any, and
%% the line after the last token if at the end of the stream.
-spec current_pos(#ts{}) -> pos().
current_pos(#ts{ inserted = [T | _] }) -> pos(T);
current_pos(#ts{ tokens   = [T | _] }) -> pos(T);
current_pos(#ts{ last     = T })       -> end_pos(pos(T)).

-spec mk_error(#ts{}, term()) -> error().
mk_error(Ts, Err) -> {current_pos(Ts), Err}.

%% Get the next token from a token stream. Inserts layout tokens if necessary.
-spec next_token(#ts{}) -> false | {token(), #ts{}}.
next_token(Ts) ->
    case insert_layout_tokens(Ts) of
        Ts1 = #ts{ inserted = [L | Ls] }  -> {L, pop_layout(L, Ts1#ts{ last = L, inserted = Ls })};
        Ts1 = #ts{ tokens   = [T | S]  }  -> {T, Ts1#ts{ last = T, tokens = S }};
        #ts{ inserted = [], tokens = [] } -> false
    end.

%% Pop a layout block on an inserted 'vclose' token.
-spec pop_layout(token(), #ts{}) -> #ts{}.
pop_layout({vclose, _}, Ts = #ts{ layout = [_ | Layout] }) -> Ts#ts{ layout = Layout };
pop_layout(_, Ts) -> Ts.

%% Attempt to start a new layout block. Requires the next token to be on a new line and indented
%% more than any existing layout block. Sets the previous token to 'vopen'.
-spec start_layout(#ts{}) -> false | {integer(), #ts{}}.
start_layout(#ts{ inserted = [_ | _] }) -> false;    %% Can't start a layout block before consuming all layout tokens
start_layout(#ts{ tokens = [] })        -> false;    %% No more tokens
start_layout(Ts = #ts{ layout = Layout, last = Last, tokens = [T | _] }) ->
    Col = col(T),
    Valid = case Layout of
                []       -> line(Last) < line(T);
                [C1 | _] -> line(Last) < line(T) andalso C1 < Col
            end,
    Valid andalso {Col, Ts#ts{ layout = [Col | Layout], last = {vopen, pos(T)} }}.

%% Insert layout tokens. If the next token is on the same line as the current layout block we insert
%% a 'vsemi' token. If the next token is indented less, we insert a 'vclose' token.
-spec insert_layout_tokens(#ts{}) -> #ts{}.
insert_layout_tokens(Ts = #ts{ inserted = [_ | _] }) ->
    Ts; %% already inserted layout tokens
insert_layout_tokens(Ts = #ts{ layout = Layout, last = Last, tokens = S }) ->
    ToInsert = insert_layout_tokens(Layout, Last, S, []),
    Ts#ts{ inserted = ToInsert }.

%% Compute the layout tokens to be inserted.
-spec insert_layout_tokens([integer()], token(), tokens(), tokens()) -> tokens().
insert_layout_tokens([_ | Layout], Last, [], Acc) ->
    %% End of the file. Insert vclose tokens for all layout blocks.
    Vclose = {vclose, end_pos(pos(Last))},
    insert_layout_tokens(Layout, Last, [], [Vclose | Acc]);
insert_layout_tokens([N | Layout1], Last, S = [T | _], Acc) ->
    Col         = col(T),
    %% Don't insert a vsemi if the previous token was a vopen or a vsemi. The former to avoid a
    %% vsemi for the first token of the block and the latter to avoid inserting infinite vsemis.
    AlreadySemi = lists:member(tag(Last), [vsemi, vopen]) andalso col(Last) == N,
    if Col == N, not AlreadySemi ->
        lists:reverse([{vsemi, pos(T)} | Acc]);
       Col < N ->
        Vclose = {vclose, pos(T)},
        insert_layout_tokens(Layout1, Vclose, S, [Vclose | Acc]);
       true ->
        lists:reverse(Acc)
    end;
insert_layout_tokens([], _Last, _S, Acc) ->
    lists:reverse(Acc).

%% The end-of-file position. Beginning of the line after the last token.
end_pos({L, _}) -> {L + 1, 1}.

%% Combine two error messages. Discard no_error's otherwise pick the first error.
add_error(no_error, Err) -> Err;
add_error(Err, no_error) -> Err;
add_error(Err, _Err1)    -> Err.

%% For some unfathomable reason the maps module does not have a merge_with function.
-spec merge_with(fun((term(), term()) -> term()), map(), map()) -> map().
merge_with(Fun, Map1, Map2) ->
    case maps:size(Map1) > maps:size(Map2) of
        true ->
            lists:foldl(fun({K, R}, M) ->
                            maps:update_with(K, fun(L) -> Fun(L, R) end, R, M)
                        end, Map1, maps:to_list(Map2));
        false ->
            lists:foldl(fun({K, L}, M) ->
                            maps:update_with(K, fun(R) -> Fun(L, R) end, L, M)
                        end, Map2, maps:to_list(Map1))
    end.

