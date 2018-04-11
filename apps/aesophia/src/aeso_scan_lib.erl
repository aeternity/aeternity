%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc A customisable lexer.
%%% @end
%%%-------------------------------------------------------------------
-module(aeso_scan_lib).

-export([compile/1, string/3,
         token/1, token/2, symbol/0, skip/0,
         override/2, push/2, pop/1]).

-export_type([lexer/0, token_spec/0, token_action/0, token/0, pos/0, regex/0]).

%% -- Exported types --

-type regex()     :: iodata() | unicode:charlist().
-type pos()       :: {integer(), integer()}.
-type lex_state() :: atom().
-type token()     :: {atom(), pos(), term()} | {atom(), pos()}.

-type token_spec()     :: {regex(), token_action()}.
-opaque token_action() :: fun((string(), pos()) -> {tok_result(), state_change()}).

-opaque lexer() :: [{lex_state(),
                     fun((string(), pos()) -> {ok, tok_result(), string(), pos()}
                                            | end_of_file | error)}].

%% -- Internal types --
-type tok_result()   :: {token, token()} | skip.
-type state_change() :: none | pop | {push, lex_state()}.

%% @doc Compile a lexer specification. Takes the regexps for each state and
%% combines them into a single big regexp that is then compiled with re:compile/1.
%% Note: contrary to lexer generators like leex, we don't have longest match
%% semantics (since this isn't supported by re). Use override/2 instead.
-spec compile([{lex_state(), [token_spec()]}]) -> lexer().
compile(TokenSpecs) ->
    [{S, compile_spec(Spec)} || {S, Spec} <- TokenSpecs].

compile_spec(TokenSpecs) ->
    WithIxs     = lists:zip(lists:seq(1, length(TokenSpecs)), TokenSpecs),
    {ok, Regex} = re:compile(["^(", name(0), string:join([ ["(", name(I), R, ")"] || {I, {R, _}} <- WithIxs ], "|"),")"]),
    Actions     = [ Fun || {_, Fun} <- TokenSpecs ],
    fun ("", _Pos) -> end_of_file;
        (S, Pos)  ->
            case re:run(S, Regex, [{capture, all_names}]) of
                {match, [{0, N} | Capture]} ->
                    Index        = 1 + length(lists:takewhile(fun({P, _}) -> P == -1 end, Capture)),
                    Action       = lists:nth(Index, Actions),
                    {TokS, Rest} = lists:split(N, S),
                    Tok          = Action(TokS, Pos),
                    {ok, Tok, Rest, next_pos(TokS, Pos)};
                nomatch ->
                    error
            end
    end.

%% @doc Produce a token with the given tag and the matched string as the
%%      value.
-spec token(atom()) -> token_action().
token(Tag) ->
    token(Tag, fun(X) -> X end).

%% @doc Produce a token with the given tag and the value computed from the
%%      matched string using the function.
-spec token(atom(), fun((string()) -> term())) -> token_action().
token(Tag, Fun) ->
    fun(S, P) -> {{token, {Tag, P, Fun(S)}}, none} end.

%% @doc Produce a token with the matched string (converted to an atom) as the
%%      tag and no value.
-spec symbol() -> token_action().
symbol() ->
    fun(S, P) -> {{token, {list_to_atom(S), P}}, none} end.

%% @doc Skip the matched string, producing no token.
-spec skip() -> token_action().
skip() ->
    fun(_, _) -> {skip, none} end.

%% @doc Enter the given state and perform the given action. The argument action
%%      should not change the state.
-spec push(lex_state(), token_action()) -> token_action().
push(State, Action) ->
    fun(S, P) -> {Res, _} = Action(S, P), {Res, {push, State}} end.

%% @doc Exit from the current state and perform the given action. The argument
%%      action should not change the state.
-spec pop(token_action()) -> token_action().
pop(Action) ->
    fun(S, P) -> {Res, _} = Action(S, P), {Res, pop} end.

%% @doc Match using the first spec, but if the second spec also matches use
%%      that one instead. Use this for overlapping tokens (like identifiers and
%%      keywords), since matching does not have longest-match semantics.
-spec override(token_spec(), token_spec()) -> token_spec().
override({Re1, Action1}, {Re2, Action2}) ->
    {ok, Compiled} = re:compile(["^(", Re2, ")$"]),
    {Re1, fun(S, P) ->
        case re:run(S, Compiled, [{capture, none}]) of
            match   -> Action2(S, P);
            nomatch -> Action1(S, P)
        end end}.

%% @doc Run a lexer. Takes the starting state and the string to lex.
-spec string(lexer(), lex_state(), string()) -> {ok, [token()]} | {error, term()}.
string(Lexer, State, String) -> string(Lexer, [State], String, {1, 1}).

string(Lexer, Stack, String, Pos) ->
    Lines = string:split(String, "\n", all),
    string(Lexer, Stack, Lines, Pos, []).

string(_Lexers, [], [Line | _Rest], Pos, _Acc) ->
    {error, {scan_error_no_state, Pos, Line}};
string(_Lexers, _Stack, [], _Pos, Acc) ->
    {ok, lists:reverse(Acc)};
string(Lexers, [State | Stack], [Line | Lines], Pos, Acc) ->
    Lexer = proplists:get_value(State, Lexers, State),
    case Lexer(Line, Pos) of
        {ok, {Res, StateChange}, Line1, Pos1} ->
            Acc1 = case Res of
                       {token, Tok} -> [Tok | Acc];
                       skip         -> Acc
                   end,
            Stack1 = case StateChange of
                         none           -> [State | Stack];
                         pop            -> Stack;
                         {push, State1} -> [State1, State | Stack]
                     end,
            string(Lexers, Stack1, [Line1 | Lines], Pos1, Acc1);
        end_of_file -> string(Lexers, [State | Stack], Lines, next_pos("\n", Pos), Acc);
        error       -> {error, {scan_error, Pos, Line}}
    end.

%% -- Internal functions -----------------------------------------------------

name(I) ->
  io_lib:format("?<A~3.10.0b>", [I]).

-define(TAB_SIZE, 8).

next_pos([], P) -> P;
next_pos([$\n | S], {L, _}) -> next_pos(S, {L + 1, 1});
next_pos([$\t | S], {L, C}) -> next_pos(S, {L, (C + ?TAB_SIZE - 1) div ?TAB_SIZE * ?TAB_SIZE + 1});
next_pos([_   | S], {L, C}) -> next_pos(S, {L, C + 1}).

