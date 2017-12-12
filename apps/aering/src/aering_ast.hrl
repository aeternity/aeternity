%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Ring abstract syntax types.
%%%
%%% @end
%%%-------------------------------------------------------------------

-type line() :: integer().
-type ann() :: [{line, line()}].

-type aer_constant()
    :: {int, ann(), integer()}
     | {bool, ann(), true | false}
     | {hash, ann(), binary()}
     | {string, ann(), binary()}.

-type aer_name() :: string().

-type aer_id() :: {id, ann(), aer_name()}
                | {param, ann(), aer_name()}.    %% implicit parameter (like @state or @balance)

-type aer_op()   :: '+' | '-' | '*' | '/' | '%' | '++' | '<' | '>' | '=<' | '>=' | '==' | '!='.
-type aer_unop() :: '!'.

-type aer_modifier() :: pure | const.

-type aer_field_assign()
    :: {assign, ann(), aer_id(), aer_exp()}.

-type aer_exp()
    :: aer_constant()
     | {app, ann(), aer_exp(), aer_exp()}
     | {infix, ann(), aer_exp(), aer_op(), aer_exp()}
     | {unop, ann(), aer_unop(), aer_exp()}
     | {record, ann(), [aer_field_assign()]}
     | {record_sel, ann(), aer_exp(), aer_id()}
     | {list, ann(), [aer_exp()]}
     | {'let', ann(), aer_id(), aer_exp(), aer_exp()}
     | {'if', ann(), aer_exp(), aer_exp(), aer_exp()}.

-type aer_pat()
    :: aer_id()
     | {'_', ann()}
     | {unit, ann()}.

-type aer_dec()
    :: {'fun', ann(), [aer_modifier()], aer_id(), [aer_pat()], aer_exp()}.

-type aer_export() :: [aer_id()].

-type aer_contract()
    :: {contract, ann(), aer_name(), [aer_export()], [aer_dec()]}.

