%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%     Handle interaction with the aeternity chain
%%%     through calls to AEternity primitive operations at address 0.
%%% @end
%%% Created : 18 Dec 2018
%%%-------------------------------------------------------------------

-module(aeb_primops).
-export([ is_local_primop_op/1
        , op_needs_type_check/1
        ]).

-include("aeb_opcodes.hrl").

is_local_primop_op(Op) when ?PRIM_CALL_IN_MAP_RANGE(Op) -> true;
is_local_primop_op(Op) when is_integer(Op) -> false.

op_needs_type_check(Op) ->
    (not is_local_primop_op(Op)) andalso op_has_dynamic_type(Op).

op_has_dynamic_type(?PRIM_CALL_ORACLE_QUERY) -> true;
op_has_dynamic_type(?PRIM_CALL_ORACLE_RESPOND) -> true;
op_has_dynamic_type(?PRIM_CALL_ORACLE_GET_QUESTION) -> true;
op_has_dynamic_type(?PRIM_CALL_ORACLE_GET_ANSWER) -> true;
op_has_dynamic_type(?PRIM_CALL_MAP_GET) -> true;
op_has_dynamic_type(?PRIM_CALL_MAP_PUT) -> true;
op_has_dynamic_type(?PRIM_CALL_MAP_TOLIST) -> true;
op_has_dynamic_type(?PRIM_CALL_AENS_RESOLVE) -> true;
op_has_dynamic_type(_) -> false.
