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
INT      = {DIGIT}+
HEX      = 0x{HEXDIGIT}+
HASH     = #{HEXDIGIT}+
WS       = [\000-\s]
ID       = {LOWER}[a-zA-Z0-9_]*
LABEL    = {ID}:



Rules.
{LABEL} : {token, {label, TokenLine, TokenChars -- ":"}}.
STOP : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
ADD : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
MUL : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SUB : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DIV : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SDIV : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
MOD : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SMOD : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
ADDMOD : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
MULMOD : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
EXP : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SIGNEXTEND : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
LT : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
GT : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SLT : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SGT : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
EQ : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
ISZERO : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
AND : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
OR : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
XOR : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
NOT : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
BYTE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SHA3 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
ADDRESS : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
BALANCE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
ORIGIN : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
CALLER : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
CALLVALUE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
CALLDATALOAD : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
CALLDATASIZE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
CALLDATACOPY : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
CODESIZE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
CODECOPY : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
GASPRICE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
EXTCODESIZE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
EXTCODECOPY : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
RETURNDATASIZE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
RETURNDATACOPY : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
BLOCKHASH : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
COINBASE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
TIMESTAMP : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
NUMBER : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DIFFICULTY : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
GASLIMIT : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
POP : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
MLOAD : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
MSTORE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
MSTORE8 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SLOAD : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SSTORE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
JUMP : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
JUMPI : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PC : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
MSIZE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
GAS : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
JUMPDEST : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH1 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH2 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH3 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH4 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH5 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH6 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH7 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH8 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH9 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH10 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH11 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH12 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH13 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH14 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH15 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH16 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH17 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH18 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH19 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH20 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH21 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH22 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH23 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH24 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH25 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH26 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH27 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH28 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH29 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH30 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH31 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
PUSH32 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP1 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP2 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP3 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP4 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP5 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP6 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP7 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP8 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP9 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP10 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP11 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP12 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP13 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP14 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP15 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DUP16 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP1 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP2 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP3 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP4 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP5 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP6 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP7 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP8 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP9 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP10 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP11 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP12 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP13 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP14 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP15 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SWAP16 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
LOG0 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
LOG1 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
LOG2 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
LOG3 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
LOG4 : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
CREATE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
CALL : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
CALLCODE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
RETURN : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
DELEGATECALL : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
CALLBLACKBOX : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
STATICCALL : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
REVERT : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
INVALID : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
SUICIDE : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
COMMENT : {token, {mnemonic, TokenLine, list_to_atom(TokenChars)}}.
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

