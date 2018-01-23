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
STOP           : {token, {mnemonic, TokenLine, 'STOP'}}.
ADD            : {token, {mnemonic, TokenLine, 'ADD'}}.
MUL            : {token, {mnemonic, TokenLine, 'MUL'}}.
SUB            : {token, {mnemonic, TokenLine, 'SUB'}}.
DIV            : {token, {mnemonic, TokenLine, 'DIV'}}.
SDIV           : {token, {mnemonic, TokenLine, 'SDIV'}}.
MOD            : {token, {mnemonic, TokenLine, 'MOD'}}.
SMOD           : {token, {mnemonic, TokenLine, 'SMOD'}}.
ADDMOD         : {token, {mnemonic, TokenLine, 'ADDMOD'}}.
MULMOD         : {token, {mnemonic, TokenLine, 'MULMOD'}}.
EXP            : {token, {mnemonic, TokenLine, 'EXP'}}.
SIGNEXTEND     : {token, {mnemonic, TokenLine, 'SIGNEXTEND'}}.
LT             : {token, {mnemonic, TokenLine, 'LT'}}.
GT             : {token, {mnemonic, TokenLine, 'GT'}}.
SLT            : {token, {mnemonic, TokenLine, 'SLT'}}.
SGT            : {token, {mnemonic, TokenLine, 'SGT'}}.
EQ             : {token, {mnemonic, TokenLine, 'EQ'}}.
ISZERO         : {token, {mnemonic, TokenLine, 'ISZERO'}}.
AND            : {token, {mnemonic, TokenLine, 'AND'}}.
OR             : {token, {mnemonic, TokenLine, 'OR'}}.
XOR            : {token, {mnemonic, TokenLine, 'XOR'}}.
NOT            : {token, {mnemonic, TokenLine, 'NOT'}}.
BYTE           : {token, {mnemonic, TokenLine, 'BYTE'}}.
SHA3           : {token, {mnemonic, TokenLine, 'SHA3'}}.
ADDRESS        : {token, {mnemonic, TokenLine, 'ADDRESS'}}.
BALANCE        : {token, {mnemonic, TokenLine, 'BALANCE'}}.
ORIGIN         : {token, {mnemonic, TokenLine, 'ORIGIN'}}.
CALLER         : {token, {mnemonic, TokenLine, 'CALLER'}}.
CALLVALUE      : {token, {mnemonic, TokenLine, 'CALLVALUE'}}.
CALLDATALOAD   : {token, {mnemonic, TokenLine, 'CALLDATALOAD'}}.
CALLDATASIZE   : {token, {mnemonic, TokenLine, 'CALLDATASIZE'}}.
CALLDATACOPY   : {token, {mnemonic, TokenLine, 'CALLDATACOPY'}}.
CODESIZE       : {token, {mnemonic, TokenLine, 'CODESIZE'}}.
CODECOPY       : {token, {mnemonic, TokenLine, 'CODECOPY'}}.
GASPRICE       : {token, {mnemonic, TokenLine, 'GASPRICE'}}.
EXTCODESIZE    : {token, {mnemonic, TokenLine, 'EXTCODESIZE'}}.
EXTCODECOPY    : {token, {mnemonic, TokenLine, 'EXTCODECOPY'}}.
RETURNDATASIZE : {token, {mnemonic, TokenLine, 'RETURNDATASIZE'}}.
RETURNDATACOPY : {token, {mnemonic, TokenLine, 'RETURNDATACOPY'}}.
BLOCKHASH      : {token, {mnemonic, TokenLine, 'BLOCKHASH'}}.
COINBASE       : {token, {mnemonic, TokenLine, 'COINBASE'}}.
TIMESTAMP      : {token, {mnemonic, TokenLine, 'TIMESTAMP'}}.
NUMBER         : {token, {mnemonic, TokenLine, 'NUMBER'}}.
DIFFICULTY     : {token, {mnemonic, TokenLine, 'DIFFICULTY'}}.
GASLIMIT       : {token, {mnemonic, TokenLine, 'GASLIMIT'}}.
POP            : {token, {mnemonic, TokenLine, 'POP'}}.
MLOAD          : {token, {mnemonic, TokenLine, 'MLOAD'}}.
MSTORE         : {token, {mnemonic, TokenLine, 'MSTORE'}}.
MSTORE8        : {token, {mnemonic, TokenLine, 'MSTORE8'}}.
SLOAD          : {token, {mnemonic, TokenLine, 'SLOAD'}}.
SSTORE         : {token, {mnemonic, TokenLine, 'SSTORE'}}.
JUMP           : {token, {mnemonic, TokenLine, 'JUMP'}}.
JUMPI          : {token, {mnemonic, TokenLine, 'JUMPI'}}.
PC             : {token, {mnemonic, TokenLine, 'PC'}}.
MSIZE          : {token, {mnemonic, TokenLine, 'MSIZE'}}.
GAS            : {token, {mnemonic, TokenLine, 'GAS'}}.
JUMPDEST       : {token, {mnemonic, TokenLine, 'JUMPDEST'}}.
PUSH1          : {token, {mnemonic, TokenLine, 'PUSH1'}}.
PUSH2          : {token, {mnemonic, TokenLine, 'PUSH2'}}.
PUSH3          : {token, {mnemonic, TokenLine, 'PUSH3'}}.
PUSH4          : {token, {mnemonic, TokenLine, 'PUSH4'}}.
PUSH5          : {token, {mnemonic, TokenLine, 'PUSH5'}}.
PUSH6          : {token, {mnemonic, TokenLine, 'PUSH6'}}.
PUSH7          : {token, {mnemonic, TokenLine, 'PUSH7'}}.
PUSH8          : {token, {mnemonic, TokenLine, 'PUSH8'}}.
PUSH9          : {token, {mnemonic, TokenLine, 'PUSH9'}}.
PUSH10         : {token, {mnemonic, TokenLine, 'PUSH10'}}.
PUSH11         : {token, {mnemonic, TokenLine, 'PUSH11'}}.
PUSH12         : {token, {mnemonic, TokenLine, 'PUSH12'}}.
PUSH13         : {token, {mnemonic, TokenLine, 'PUSH13'}}.
PUSH14         : {token, {mnemonic, TokenLine, 'PUSH14'}}.
PUSH15         : {token, {mnemonic, TokenLine, 'PUSH15'}}.
PUSH16         : {token, {mnemonic, TokenLine, 'PUSH16'}}.
PUSH17         : {token, {mnemonic, TokenLine, 'PUSH17'}}.
PUSH18         : {token, {mnemonic, TokenLine, 'PUSH18'}}.
PUSH19         : {token, {mnemonic, TokenLine, 'PUSH19'}}.
PUSH20         : {token, {mnemonic, TokenLine, 'PUSH20'}}.
PUSH21         : {token, {mnemonic, TokenLine, 'PUSH21'}}.
PUSH22         : {token, {mnemonic, TokenLine, 'PUSH22'}}.
PUSH23         : {token, {mnemonic, TokenLine, 'PUSH23'}}.
PUSH24         : {token, {mnemonic, TokenLine, 'PUSH24'}}.
PUSH25         : {token, {mnemonic, TokenLine, 'PUSH25'}}.
PUSH26         : {token, {mnemonic, TokenLine, 'PUSH26'}}.
PUSH27         : {token, {mnemonic, TokenLine, 'PUSH27'}}.
PUSH28         : {token, {mnemonic, TokenLine, 'PUSH28'}}.
PUSH29         : {token, {mnemonic, TokenLine, 'PUSH29'}}.
PUSH30         : {token, {mnemonic, TokenLine, 'PUSH30'}}.
PUSH31         : {token, {mnemonic, TokenLine, 'PUSH31'}}.
PUSH32         : {token, {mnemonic, TokenLine, 'PUSH32'}}.
DUP1           : {token, {mnemonic, TokenLine, 'DUP1'}}.
DUP2           : {token, {mnemonic, TokenLine, 'DUP2'}}.
DUP3           : {token, {mnemonic, TokenLine, 'DUP3'}}.
DUP4           : {token, {mnemonic, TokenLine, 'DUP4'}}.
DUP5           : {token, {mnemonic, TokenLine, 'DUP5'}}.
DUP6           : {token, {mnemonic, TokenLine, 'DUP6'}}.
DUP7           : {token, {mnemonic, TokenLine, 'DUP7'}}.
DUP8           : {token, {mnemonic, TokenLine, 'DUP8'}}.
DUP9           : {token, {mnemonic, TokenLine, 'DUP9'}}.
DUP10          : {token, {mnemonic, TokenLine, 'DUP10'}}.
DUP11          : {token, {mnemonic, TokenLine, 'DUP11'}}.
DUP12          : {token, {mnemonic, TokenLine, 'DUP12'}}.
DUP13          : {token, {mnemonic, TokenLine, 'DUP13'}}.
DUP14          : {token, {mnemonic, TokenLine, 'DUP14'}}.
DUP15          : {token, {mnemonic, TokenLine, 'DUP15'}}.
DUP16          : {token, {mnemonic, TokenLine, 'DUP16'}}.
SWAP1          : {token, {mnemonic, TokenLine, 'SWAP1'}}.
SWAP2          : {token, {mnemonic, TokenLine, 'SWAP2'}}.
SWAP3          : {token, {mnemonic, TokenLine, 'SWAP3'}}.
SWAP4          : {token, {mnemonic, TokenLine, 'SWAP4'}}.
SWAP5          : {token, {mnemonic, TokenLine, 'SWAP5'}}.
SWAP6          : {token, {mnemonic, TokenLine, 'SWAP6'}}.
SWAP7          : {token, {mnemonic, TokenLine, 'SWAP7'}}.
SWAP8          : {token, {mnemonic, TokenLine, 'SWAP8'}}.
SWAP9          : {token, {mnemonic, TokenLine, 'SWAP9'}}.
SWAP10         : {token, {mnemonic, TokenLine, 'SWAP10'}}.
SWAP11         : {token, {mnemonic, TokenLine, 'SWAP11'}}.
SWAP12         : {token, {mnemonic, TokenLine, 'SWAP12'}}.
SWAP13         : {token, {mnemonic, TokenLine, 'SWAP13'}}.
SWAP14         : {token, {mnemonic, TokenLine, 'SWAP14'}}.
SWAP15         : {token, {mnemonic, TokenLine, 'SWAP15'}}.
SWAP16         : {token, {mnemonic, TokenLine, 'SWAP16'}}.
LOG0           : {token, {mnemonic, TokenLine, 'LOG0'}}.
LOG1           : {token, {mnemonic, TokenLine, 'LOG1'}}.
LOG2           : {token, {mnemonic, TokenLine, 'LOG2'}}.
LOG3           : {token, {mnemonic, TokenLine, 'LOG3'}}.
LOG4           : {token, {mnemonic, TokenLine, 'LOG4'}}.
CREATE         : {token, {mnemonic, TokenLine, 'CREATE'}}.
CALL           : {token, {mnemonic, TokenLine, 'CALL'}}.
CALLCODE       : {token, {mnemonic, TokenLine, 'CALLCODE'}}.
RETURN         : {token, {mnemonic, TokenLine, 'RETURN'}}.
DELEGATECALL   : {token, {mnemonic, TokenLine, 'DELEGATECALL'}}.
CALLBLACKBOX   : {token, {mnemonic, TokenLine, 'CALLBLACKBOX'}}.
STATICCALL     : {token, {mnemonic, TokenLine, 'STATICCALL'}}.
REVERT         : {token, {mnemonic, TokenLine, 'REVERT'}}.
INVALID        : {token, {mnemonic, TokenLine, 'INVALID'}}.
SUICIDE        : {token, {mnemonic, TokenLine, 'SUICIDE'}}.
COMMENT        : {token, {mnemonic, TokenLine, 'COMMENT'}}.
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

