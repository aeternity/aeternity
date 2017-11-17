%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Ring abstract syntax types.
%%%
%%% @end
%%%-------------------------------------------------------------------

-type line() :: integer().

-type aer_constant()
    :: {int, line(), integer()}
     | {bool, line(), true | false}
     | {hash, line(), binary()}
     | {string, line(), binary()}.

-type aer_name() :: string().

-type aer_id() :: {id, line(), aer_name()}
                | {param, line(), aer_name()}.    %% implicit parameter (like @state or @balance)

-type aer_modifier() :: pure | const.

-type aer_field_assign()
    :: {assign, line(), aer_id(), aer_exp()}.

-type aer_exp()
    :: aer_constant()
     | {app, line(), aer_exp(), aer_exp()}
     | {infix, line(), aer_exp(), aer_id(), aer_exp()}
     | {record, line(), [aer_field_assign()]}
     | {record_sel, line(), aer_exp(), aer_id()}
     | {list, line(), [aer_exp()]}
     | {'let', line(), aer_id(), aer_exp(), aer_exp()}
     | {'if', line(), aer_exp(), aer_exp(), aer_exp()}.

-type aer_pat()
    :: aer_id()
     | {'_', line()}
     | {unit, line()}.

-type aer_dec()
    :: {'fun', line(), [aer_modifier()], aer_id(), [aer_pat()], aer_exp()}.

-type aer_export() :: [aer_id()].

-type aer_contract()
    :: {contract, line(), aer_name(), [aer_export()], [aer_dec()]}.

