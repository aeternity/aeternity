%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Ring abstract syntax types.
%%%
%%% @end
%%%-------------------------------------------------------------------

-type ann_line()   :: integer().
-type ann_origin() :: system | user.
-type ann_format() :: '?:' | hex | infix | prefix.

-type ann() :: [{line, ann_line()} | {format, ann_format()} | {origin, ann_origin()}].

-type aer_name() :: string().
-type aer_id()   :: {id,   ann(), aer_name()}.
-type aer_con()  :: {con,  ann(), aer_name()}.
-type aer_tvar() :: {tvar, ann(), aer_name()}.

-type aer_decl() :: {contract_type, ann(), aer_con(), [aer_decl()]}
                  | {contract, ann(), aer_con(), [aer_decl()]}
                  | {type_decl, ann(), aer_id(), [aer_tvar()]}
                  | {type_def, ann(), aer_id(), [aer_tvar()], aer_typedef()}
                  | {fun_decl, ann(), aer_id(), aer_type()}
                  | aer_letbind().

-type aer_letbind()
    :: {letval, ann(), aer_id(), aer_type(), aer_expr()}
     | {letfun, ann(), aer_id(), [aer_arg()], aer_type(), aer_expr()}
     | {letrec, ann(), [aer_decl()]}.

-type aer_arg() :: {arg, ann(), aer_id(), aer_type()}.

-type aer_typedef()
    :: {alias_t, aer_type()}
     | {record_t, [aer_field_t()]}
     | {variant_t, [aer_constructor_t()]}.

-type aer_field_t() :: {field_t, ann(), aer_mutable(), aer_id(), aer_type()}.
-type aer_mutable() :: mutable | immutable.

-type aer_constructor_t() :: {con, ann(), aer_name(), [aer_type()]}.

-type aer_type() :: {fun_t, ann(), [aer_type()], aer_type()}
                  | {app_t, ann(), aer_type(), [aer_type()]}
                  | {tuple_t, ann(), [aer_type()]}
                  | {con_t, ann(), aer_name()}
                  | {var_t, ann(), aer_name()}.

-type aer_constant()
    :: {int, ann(), integer()}
     | {bool, ann(), true | false}
     | {hash, ann(), binary()}
     | {unit, ann()}
     | {string, ann(), binary()}
     | {char, ann(), integer()}.

-type aer_op() :: '+' | '-' | '*' | '/' | mod
                | '++' | '::' | '<' | '>' | '=<' | '>=' | '==' | '!='
                | '!' | '||' | '&&'.

-type aer_expr()
    :: {lam, ann(), [aer_arg()], aer_expr()}
     | {'if', ann(), aer_expr(), aer_expr(), aer_expr()}
     | {switch, ann(), aer_expr(), [aer_case()]}
     | {app, ann(), aer_expr(), [aer_expr()]}
     | {proj, ann(), aer_expr(), aer_id()}
     | {tuple, ann(), [aer_expr()]}
     | {list, ann(), [aer_expr()]}
     | {typed, ann(), aer_expr(), aer_type()}
     | {record, ann(), [aer_field()]}
     | {block, ann(), [aer_stmt()]}
     | {aer_op(), ann()}
     | aer_constant().

-type aer_field() :: {field, ann(), aer_id(), aer_expr()}.

-type aer_stmt() :: {assign, ann(), aer_lvalue(), aer_expr()}
                  | aer_letbind()
                  | aer_expr().

-type aer_case() :: {'case', ann(), aer_pat(), aer_expr()}.

-type aer_lvalue() :: {proj, ann(), aer_expr(), aer_id()}.

-type aer_pat() :: {app, ann(), aer_con() | aer_op(), [aer_pat()]}
                 | {tuple, ann(), [aer_pat()]}
                 | {list, ann(), [aer_pat()]}
                 | aer_constant()
                 | aer_con()
                 | aer_id().

