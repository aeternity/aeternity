%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Assembler lexer.
%%%
%%% @end
%%%-------------------------------------------------------------------

Definitions.
DIGIT    = [0-9]
HEXDIGIT = [0-9a-fA-F]
LOWER    = [a-z_]
UPPER    = [A-Z]
MNEMONIC = {UPPER}[A-Z0-9_]*
INT      = {DIGIT}+
HEX      = 0x{HEXDIGIT}+
HASH     = #{HEXDIGIT}+
WS       = [\000-\s]
ID       = {LOWER}[a-zA-Z0-9_]*
LABEL    = {ID}:

Rules.
{LABEL} :
 {token, {lable, TokenLine, TokenChars -- ":"}}.
{MNEMONIC} :
 {token, {mnemonic, TokenLine, list_to_existing_atom(TokenChars)}}.
{ID} :
 {token, {id, TokenLine, TokenChars}}.
{HEX} :
 {token, {int, TokenLine, parse_hex(TokenChars)}}.
{INT} :
 {token, {int, TokenLine, parse_int(TokenChars)}}.
{HASH} :
 {token, {hash, TokenLine, parse_hash(TokenChars)}}.


%% Symbols
,   : {token, {',', TokenLine}}.
\.  : {token, {'.', TokenLine}}.
\(  : {token, {'(', TokenLine}}.
\)  : {token, {')', TokenLine}}.
\[  : {token, {'[', TokenLine}}.
\]  : {token, {']', TokenLine}}.
{   : {token, {'{', TokenLine}}.
}   : {token, {'}', TokenLine}}.


%% Whitespace ignore
{WS} : skip_token.

%% Comments (TODO: nested comments)
;;.*                     : skip_token.

. : {error, "Unexpected token: " ++ TokenChars}.

Erlang code.

-export([scan/1]).

-dialyzer({nowarn_function, yyrev/2}).

-ignore_xref([format_error/1, string/2, token/2, token/3, tokens/2, tokens/3]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").


parse_hex("0x" ++ Chars) -> list_to_integer(Chars, 16).

parse_int(Chars) -> list_to_integer(Chars).

parse_hash("#" ++ Chars) ->
    N = list_to_integer(Chars, 16),
    <<N:256>>.

scan(S) ->
    string(S).

