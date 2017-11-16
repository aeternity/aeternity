%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Ring parser.
%%%
%%% @end
%%%-------------------------------------------------------------------

Nonterminals

expr contract_def.

Terminals

contract.

Rootsymbol contract_def.

contract_def -> contract : {contract, get_line('$1'), none, "the_contract", [], []}.

Erlang code.
-include("aering_ast.hrl").

-ignore_xref([format_error/1, parse_and_scan/1]).

get_line(Tok) -> element(2, Tok).

