%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Ring abstract syntax types.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(aer_syntax).

-export([get_ann/1, get_ann/2, get_ann/3]).

-export_type([ann_line/0, ann_origin/0, ann_format/0, ann/0]).
-export_type([name/0, id/0, con/0, tvar/0, op/0]).
-export_type([bin_op/0, un_op/0]).
-export_type([decl/0, letbind/0, typedef/0]).
-export_type([arg/0, field_t/0, constructor_t/0]).
-export_type([type/0, constant/0, expr/0, field/1, stmt/0, alt/0, lvalue/0, pat/0]).

-type ann_line()   :: integer().
-type ann_origin() :: system | user.
-type ann_format() :: '?:' | hex | infix | prefix.

-type ann() :: [{line, ann_line()} | {format, ann_format()} | {origin, ann_origin()}].

-type name() :: string().
-type id()   :: {id,   ann(), name()}.
-type con()  :: {con,  ann(), name()}.
-type qid()  :: {qid,  ann(), [name()]}.
-type qcon() :: {qcon, ann(), [name()]}.
-type tvar() :: {tvar, ann(), name()}.

-type decl() :: {contract_type, ann(), con(), [decl()]}
              | {contract, ann(), con(), [decl()]}
              | {type_decl, ann(), id(), [tvar()]}
              | {type_def, ann(), id(), [tvar()], typedef()}
              | {fun_decl, ann(), id(), type()}
              | letbind().

-type letbind()
    :: {letval, ann(), id(), type(), expr()}
     | {letfun, ann(), id(), [arg()], type(), expr()}
     | {letrec, ann(), [letbind()]}.

-type arg() :: {arg, ann(), id(), type()}.

-type typedef()
    :: {alias_t, type()}
     | {record_t, [field_t()]}
     | {variant_t, [constructor_t()]}.

-type field_t() :: {field_t, ann(), mutable | immutable, id(), type()}.

-type constructor_t() :: {constr_t, ann(), con(), [type()]}.

-type type() :: {fun_t, ann(), [type()], type()}
              | {app_t, ann(), type(), [type()]}
              | {tuple_t, ann(), [type()]}
              | id() | tvar().

-type constant()
    :: {int, ann(), integer()}
     | {bool, ann(), true | false}
     | {hash, ann(), binary()}
     | {unit, ann()}
     | {string, ann(), binary()}
     | {char, ann(), integer()}.

-type op() :: bin_op() | un_op().

-type bin_op() :: '+' | '-' | '*' | '/' | mod | 'band' | 'bor' | 'bsl' | 'bsr' | 'bxor'
                | '++' | '::' | '<' | '>' | '=<' | '>=' | '==' | '!='
                | '||' | '&&' | '..'.
-type un_op() :: '-' | '!' | 'bnot'.

-type expr()
    :: {lam, ann(), [arg()], expr()}
     | {'if', ann(), expr(), expr(), expr()}
     | {switch, ann(), expr(), [alt()]}
     | {app, ann(), expr(), [expr()]}
     | {proj, ann(), expr(), id()}
     | {tuple, ann(), [expr()]}
     | {list, ann(), [expr()]}
     | {typed, ann(), expr(), type()}
     | {record, ann(), [field(expr())]}
     | {record, ann(), expr(), [field(expr())]} %% record update
     | {block, ann(), [stmt()]}
     | {op(), ann()}
     | id() | qid() | con() | qcon()
     | constant().

%% When lvalue is a projection this is sugar for accessing fields in nested
%% records. For instance,
%%    r { x.y: 5 }
%% is the same as
%%    r { x: r.x { y: 5 } }
-type field(E) :: {field, ann(), lvalue(), E}.

-type stmt() :: {assign, ann(), lvalue(), expr()}
              | letbind()
              | expr().

-type alt() :: {'case', ann(), pat(), expr()}.

-type lvalue() :: {proj, ann(), expr(), id()} | id().

-type pat() :: {app, ann(), con() | op(), [pat()]}
             | {tuple, ann(), [pat()]}
             | {list, ann(), [pat()]}
             | {record, ann(), [field(pat())]}
             | constant()
             | con()
             | id().

get_ann(Node) -> element(2, Node).

get_ann(Key, Node) ->
    proplists:get_value(Key, get_ann(Node)).

get_ann(Key, Node, Default) ->
    proplists:get_value(Key, get_ann(Node), Default).
