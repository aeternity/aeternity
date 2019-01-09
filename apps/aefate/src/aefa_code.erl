%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, aeternity Anstalt
%%% @doc
%%%     Handling FATE code.
%%% @end
%%% Created : 9 Jan 2019
%%%-------------------------------------------------------------------
-module(aefa_code).
-export([parse/1
        ]).



%%% Assembler syntax:
%%% (in BNF with some confusion about whitespace.
%%%  whitespace is not significant between non-terminals but significant in terminals.)

%%% [non-terminals:]
%%%
%%% <code>         ::= <contractid> <EOL> <functions> | <code> "END" <EOL> <code>
%%%
%%%
%%% <functions>    ::= <function> <functions>
%%% <function>     ::= <header> <EOL> <bbs> "END" <EOL>
%%% <header>       ::= <name> "(" <typelist> ")->" <type> | <name> "()->" <type>
%%%
%%% <typelist>     ::= <type> | <type> ", " <typelist>
%%% <type>         ::= "boolean" | "integer" | "list" <type> | "string" | "address" 
%%%                 | "()" | "(" <typelist> ")" 
%%%                 | "variant" <number>
%%%                 | "map" <type> "->" <type>
%%%
%%% <bbs>          ::= <bb> | <bb> <bbs>
%%% <bb>           ::= <number> ":" <EOL> <intructions>
%%% <instructions> ::= <instruction> <EOL> | <instruction> <EOL> <instructions>
%%% <instruction>  ::= "return"
%%%                 |  "call_local" <name>
%%%                 |  "tailcall_local" <name>
%%%                 |  "call_remote" <contractid> <name>
%%%                 |  "tailcall_remote" <contractid> <name>
%%%                 |  "jump" <number>
%%%                 |  "jumpif_a" <number>
%%%                 |  "push_a_0"
%%%                 |  "inc_a_1_a"
%%%                 |  "add_a_i_a" <number>
%%%                 |  "add_a_a_a"
%%%                 |  "sub_a_a_a"
%%%                 |  "mul_a_a_a"
%%%                 |  "div_a_a_a"
%%%                 |  "mod_a_a_a"
%%%                 |  "pow_a_a_a"
%%%                 |  "lt_a_a_a"
%%%                 |  "gt_a_a_a"
%%%                 |  "elt_a_a_a"
%%%                 |  "egt_a_a_a"
%%%                 |  "eq_a_a_a"
%%%                 |  "neq_a_a_a"
%%%                 |  "push_a_true"
%%%                 |  "push_a_false"
%%%                 |  "and_a_a_a"
%%%                 |  "or_a_a_a"
%%%                 |  "not_a_a"
%%%                 |  "dup"
%%%                 |  "nop"
%%%
%%% <EOL>          ::= "\n" | <comment> <EOL> | <EOL> <EOL>
%%% <comment>      ::= ";" <text>
%%% <text>         ::= <anycharacter> | <anycharacter> <text>

%%% [terminals:]
%%%
%%% <contractid>   ::= <name> | <address>
%%% <name>         ::= <character> | <name>
%%% <address>      ::= "#" <base58string>
%%% <number>       ::= <digit> | <digit> <number>
%%% <character>    ::= <letter> | <digit> | <symbol>
%%% <anycharacter> ::= <letter> | <digit> | "|" | " " | "!" | "#" | "$" | "%"
%%%                 | "&" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":"
%%%                 | ";" | ">" | "=" | "<" | "?" | "@" | "[" | "\" | "]" | "^"
%%%                 | "_" | "`" | "{" | "}" | "~"
%%% <letter>       ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J"
%%%                 | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T"
%%%                 | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d"
%%%                 | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n"
%%%                 | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x"
%%%                 | "y" | "z"
%%% <digit>        ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
%%% <symbol>       ::= "-" | "@" | "_" 
%%% <base58string> ::= <base58char> <base58string>
%%% <base58char>   ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
%%%                 |  "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "J"
%%%                 |  "K" | "L" | "M" | "N" | "P" | "Q" | "R" | "S" | "T"
%%%                 |  "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" 
%%%                 |  "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "m"
%%%                 |  "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v"
%%%                 |  "w" | "x" | "y" | "z"

%%% --------------------------------------------------------------------------
%%% Example:
%%% 

%%% test                    ; contract name.
%%%  id(integer)->integer   ; indentation is not significant but can be used.
%%%   0:                    ; basic block number
%%%     return              ; instruction
%%%  END                    ; End of function
%%%
%%%  jumps()->integer
%%%   0:
%%%     push_a_0
%%%     jump 3
%%%   1:
%%%     nop
%%%   2:
%%%     nop
%%%     return
%%%   3:
%%%      nop
%%%      jump 1
%%%  END
%%%  inc(integer) -> integer
%%%   0:
%%%     inc_a_1_a
%%%     inc_a_1_a
%%%     return
%%%  END
%%%
%%%  call(integer)->integer
%%%   0:
%%%     inc_a_1_a
%%%     call_local inc
%%%   1:
%%%     inc_a_1_a
%%%     return
%%%  END
%%%
%%%  tailcall(integer) -> integer
%%%   0:
%%%     inc_a_1_a
%%%     tailcall_local inc
%%%  END
%%%
%%%  remote_call(integer) -> integer
%%%   0:
%%%     call_remote remote add_five
%%%   1:
%%%     inc_a_1_a
%%%     return
%%%  END
%%%
%%%  remote_tailcall(integer) -> integer
%%%   0:
%%%    tailcall_remote remote add_five
%%%  END
%%% END                   ; end of first contract
%%% remote
%%%  add_five(integer)->integer
%%%   0:
%%%     add_a_i_a 5
%%%     return
%%%  END

parse(_Filename) ->
    [].
