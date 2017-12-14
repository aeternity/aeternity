%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Ring lexer.
%%%
%%% @end
%%%-------------------------------------------------------------------

Definitions.
DIGIT    = [0-9]
HEXDIGIT = [0-9a-fA-F]
LOWER    = [a-z_]
UPPER    = [A-Z]
CON      = {UPPER}[a-zA-Z0-9_]*
INT      = {DIGIT}+
HEX      = 0x{HEXDIGIT}+
HASH     = #{HEXDIGIT}+
WS       = [\000-\s]
ID       = {LOWER}[a-zA-Z0-9_']*
TVAR     = '{ID}
QID      = ({CON}\.)+{ID}
QCON     = ({CON}\.)+{CON}

NOT_END_COMMENT = ([^*]|(\*+[^/*]))*

CHARTEXT = ([^\'\\]|(\\.))
STRINGTEXT = ([^\"\\]|(\\.))

Rules.
%% '

%% Symbols
,   : {token, {',', TokenLine}}.
\.  : {token, {'.', TokenLine}}.
;   : {token, {';', TokenLine}}.
\|  : {token, {'|', TokenLine}}.
\:  : {token, {':', TokenLine}}.
\(  : {token, {'(', TokenLine}}.
\)  : {token, {')', TokenLine}}.
\[  : {token, {'[', TokenLine}}.
\]  : {token, {']', TokenLine}}.
{   : {token, {'{', TokenLine}}.
}   : {token, {'}', TokenLine}}.
\?  : {token, {'?', TokenLine}}.

%% Keywords
contract   : {token, {contract, TokenLine}}.
import     : {token, {import, TokenLine}}.
let        : {token, {'let', TokenLine}}.
rec        : {token, {rec, TokenLine}}.
switch     : {token, {switch, TokenLine}}.
type       : {token, {type, TokenLine}}.
if         : {token, {'if', TokenLine}}.
else       : {token, {else, TokenLine}}.
mutable    : {token, {mutable, TokenLine}}.
and        : {token, {'and', TokenLine}}.
true|false : {token, {bool, TokenLine, list_to_atom(TokenChars)}}.

%% Operators
=    : {token, {'=', TokenLine}}.
==   : {token, {'==', TokenLine}}.
!=   : {token, {'!=', TokenLine}}.
>    : {token, {'>', TokenLine}}.
<    : {token, {'<', TokenLine}}.
>=   : {token, {'>=', TokenLine}}.
=<   : {token, {'=<', TokenLine}}.
-    : {token, {'-', TokenLine}}.
\+   : {token, {'+', TokenLine}}.
\+\+ : {token, {'++', TokenLine}}.
\*   : {token, {'*', TokenLine}}.
/    : {token, {'/', TokenLine}}.
mod  : {token, {mod, TokenLine}}.
\:   : {token, {':', TokenLine}}.
\:\: : {token, {'::', TokenLine}}.
->   : {token, {'->', TokenLine}}.
=>   : {token, {'=>', TokenLine}}.
<=   : {token, {'<=', TokenLine}}.
&&   : {token, {'&&', TokenLine}}.
\|\| : {token, {'||', TokenLine}}.
\:\: : {token, {'::', TokenLine}}.
!    : {token, {'!', TokenLine}}.

"{STRINGTEXT}*" : parse_string(TokenLine, TokenChars).
'{CHARTEXT}'    : parse_char(TokenLine, TokenChars).

%% Identifiers and literals
{QID}       : {token, {qid, TokenLine, string:tokens(TokenChars, ".")}}.
{QCON}      : {token, {qcon, TokenLine, string:tokens(TokenChars, ".")}}.
{ID}        : {token, {id, TokenLine, TokenChars}}.
{TVAR}      : {token, {tvar, TokenLine, TokenChars}}.
_           : {token, {'_', TokenLine}}.
{CON}       : {token, {con, TokenLine, TokenChars}}.
@{ID}       : {token, {param, TokenLine, lists:nthtail(1, TokenChars)}}.
{INT}       : {token, {int, TokenLine, list_to_integer(TokenChars)}}.
{HEX}       : {token, {hex, TokenLine, parse_hex(TokenChars)}}.
{HASH}      : {token, {hash, TokenLine, parse_hash(TokenChars)}}.

%% Whitespace ignore
{WS} : skip_token.

%% Comments (TODO: nested comments)
\/\/.*                     : skip_token.
\/\*{NOT_END_COMMENT}\*+\/ : skip_token.
\/\*{NOT_END_COMMENT}\n    : {skip_token, "*/"}.

. : {error, "Unexpected token: " ++ TokenChars}.

Erlang code.

-dialyzer({nowarn_function, yyrev/2}).

-ignore_xref([format_error/1, string/2, token/2, token/3, tokens/2, tokens/3]).

%% TODO: unicode
parse_string(Line, [$" | Chars]) ->
    unescape(Line, Chars).

parse_char(Line, [$', $\\, Code, $']) ->
    Ok = fun(C) -> {token, {char, Line, C}} end,
    case Code of
        $'  -> Ok($');
        $\\ -> Ok($\\);
        $b  -> Ok($\b);
        $e  -> Ok($\e);
        $f  -> Ok($\f);
        $n  -> Ok($\n);
        $r  -> Ok($\r);
        $t  -> Ok($\t);
        $v  -> Ok($\v);
        _   -> {error, "Bad control sequence: \\" ++ [Code]}
    end;
parse_char(Line, [$', C, $']) -> {token, {char, Line, C}}.

unescape(Line, Str) -> unescape(Line, Str, []).

%% TODO: numeric escapes
unescape(Line, [$"], Acc) ->
    {token, {string, Line, list_to_binary(lists:reverse(Acc))}};
unescape(Line, [$\\, Code | Chars], Acc) ->
    Ok = fun(C) -> unescape(Line, Chars, [C | Acc]) end,
    case Code of
        $"  -> Ok($");
        $\\ -> Ok($\\);
        $b  -> Ok($\b);
        $e  -> Ok($\e);
        $f  -> Ok($\f);
        $n  -> Ok($\n);
        $r  -> Ok($\r);
        $t  -> Ok($\t);
        $v  -> Ok($\v);
        _   -> {error, "Bad control sequence: \\" ++ [Code]}
    end;
unescape(Line, [C | Chars], Acc) ->
    unescape(Line, Chars, [C | Acc]).



parse_hex("0x" ++ Chars) -> list_to_integer(Chars, 16).

parse_hash("#" ++ Chars) ->
    N = list_to_integer(Chars, 16),
    <<N:256>>.

