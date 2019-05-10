%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% The FATE virtual machine
%%% @end
%%%-------------------------------------------------------------------

-module(aefa_fate).
-export([run/2]).

-export([ gas/1
        , logs/1
        , final_trees/1
        , return_value/1
        , tx_env/1
        ]).

-export([get_trace/1]).

-export([ bind_args_from_signature/2
        , check_return_type/1
        , check_signature/2
        , check_type/2
        , get_function_signature/2
        , push_gas_cap/2
        , push_return_address/1
        , set_function/3
        , set_local_function/2
        , set_remote_function/3
        , type/1
        ]
       ).


%% Memory handling.
-export([ lookup_var/2
        , store_var/3]).

%% Stack handling.
-export([ dup/1
        , dup/2
        , pop_call_stack/1
        , pop_n/2
        , push/2
        ]).

%% Error handling.
-export([ abort/1
        , abort/2
        ]).

%% For unit tests
-ifdef(TEST).
-export([ run_with_cache/3
        ]).
-endif.

-include_lib("aebytecode/include/aeb_fate_data.hrl").

-ifdef(TEST).
-define(trace(I,S), aefa_engine_state:add_trace(I, S)).
-else.
-define(trace(I,S), S).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

-ifdef(TEST).
run_with_cache(What, Spec, Cache) ->
    try execute(setup_engine(What, Spec, Cache)) of
        Res -> {ok, Res}
    catch
        throw:{?MODULE, E, ES} -> {error, E, ES}
    end.
-endif.

run(What, Env) ->
    try execute(setup_engine(What, Env)) of
        Res -> {ok, Res}
    catch
        throw:{?MODULE, E, ES} -> {error, E, ES}
    end.

get_trace(EngineState) ->
    aefa_engine_state:trace(EngineState).

return_value(EngineState) ->
    aefa_engine_state:accumulator(EngineState).

tx_env(EngineState) ->
    aefa_chain_api:tx_env(aefa_engine_state:chain_api(EngineState)).

gas(EngineState) ->
    aefa_engine_state:gas(EngineState).

logs(EngineState) ->
    aefa_engine_state:logs(EngineState).

final_trees(EngineState) ->
    aefa_chain_api:final_trees(aefa_engine_state:chain_api(EngineState)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-define(t(__S,__A,__ES),
        runtime_error(__S,__A,__ES)).

-spec runtime_error(Format :: string(), [term()],
                    aefa_engine_state:state()) -> no_return().
runtime_error(S, A, ES) ->
    Gas = collect_gas_stores(aefa_engine_state:call_stack(ES), 0),
    ES1 = aefa_engine_state:set_gas(Gas, ES),
    throw({?MODULE, iolist_to_binary(io_lib:format(S, A)), ES1}).

%% Runtime error messages for dry run and debugging.
%% Should result on one tyhpe of runtime error and use all gas when
%% executed on chain.
-spec abort(term(), aefa_engine_state:state()) -> no_return().
abort({invalid_tuple_size, Size}, ES) ->
    ?t("Invalid tuple size: ~p", [Size], ES);
abort({element_index_out_of_bounds, Index}, ES) ->
    ?t("Bad index argument to element, Index: ~p", [Index], ES);
abort({bad_arguments_to_element, Index, Tuple}, ES) ->
    ?t("Bad argument to element, Tuple: ~p, Index: ~p", [Tuple, Index], ES);
abort({bad_element_type, Type, Value}, ES) ->
    ?t("Type error in element: ~p is not of type ~p", [Value, Type], ES);
abort({bad_variant_tag, Tag}, ES) ->
    ?t("Type error in switch: tag ~p is larger than switch op", [Tag], ES);
abort({bad_variant_size, Size}, ES) ->
    ?t("Type error in switch: wrong size ~p", [Size], ES);
abort(hd_on_empty_list, ES) ->
    ?t("Head on empty list", [], ES);
abort(tl_on_empty_list, ES) ->
    ?t("Tail on empty list", [], ES);
abort({arithmetic_error, bits_sum_on_infinite_set}, ES) ->
    ?t("Arithmetic error: bits_sum on infinite set", [], ES);
abort(division_by_zero, ES) ->
    ?t("Arithmetic error: division by zero", [], ES);
abort(mod_by_zero, ES) ->
    ?t("Arithmetic error: mod by zero", [], ES);
abort(pow_too_large_exp, ES) ->
    ?t("Arithmetic error: pow with too large exponent", [], ES);
abort(missin_map_key, ES) ->
    ?t("Maps: Key does not exists", [], ES);
abort({type_error, cons, Value, Type}, ES) ->
    ?t("Type error in cons: ~p is not of type ~p", [Value, Type], ES);
abort({cannot_write_to_arg, N}, ES) ->
    ?t("Arguments are read only: ~p", [N], ES);
abort({undefined_var, Var}, ES) ->
    ?t("Undefined var: ~p", [Var], ES);
abort({bad_return_type, Val, Type}, ES) ->
    ?t("Type error on return: ~p is not of type ~p", [Val, Type], ES);
abort({value_does_not_match_type, Val, Type}, ES) ->
    ?t("Type error on call: ~p is not of type ~p", [Val, Type], ES);
abort({trying_to_reach_bb, BB}, ES) ->
    ?t("Trying to jump to non existing bb: ~p", [BB], ES);
abort({trying_to_call_function, Name}, ES) ->
    ?t("Trying to call undefined function: ~p", [Name], ES);
abort({trying_to_call_contract, Pubkey}, ES) ->
    ?t("Trying to call invalid contract: ~p", [Pubkey], ES);
abort(negative_value_in_call, ES) ->
    ?t("Trying to transfer negative value in call", [], ES);
abort({call_error, What}, ES) ->
    ?t("Error in call: ~w", [What], ES);
abort({primop_error, Which, What}, ES) ->
    ?t("Error in ~w: ~w", [Which, What], ES);
abort(out_of_gas, ES) ->
    ?t("Out of gas", [], ES);
abort(bad_byte_code, ES) ->
    ?t("Bad byte code", [], ES).


abort(E) -> throw({add_engine_state, E}).

execute(EngineState) ->
    Instructions = aefa_engine_state:current_bb_instructions(EngineState),
    loop(Instructions, EngineState).

loop(Instructions, EngineState) ->
    case step(Instructions, EngineState) of
        {stop, FinalState} ->
            FinalState;
        {jump, BB, NewState} ->
            {NewInstructions, State2} = jump(BB, NewState),
            loop(NewInstructions, State2)
    end.

step([], EngineState) ->
    %% TODO check BB + 1 exists.
    BB = aefa_engine_state:current_bb(EngineState) + 1,
    {jump, BB, EngineState};
step([I|Is], EngineState0) ->
    ES = ?trace(I, EngineState0),
    case aefa_fate_eval:eval(I, ES) of
        {next, NewState} -> step(Is, NewState);
        {jump,_BB,_NewState} = Res -> Res;
        {stop, _NewState} = Res -> Res

    end.

%% -----------------------------------------------------------

setup_engine(#{ contract := <<_:256>> = ContractPubkey
              , code := ByteCode} = Spec, State) ->
    try aeb_fate_asm:bytecode_to_fate_code(ByteCode, []) of
        Code ->
            Address = aeb_fate_data:make_address(ContractPubkey),
            Cache = #{ Address => Code },
            setup_engine(Spec, State, Cache)
    catch _:_ ->
            abort(bad_bytecode, no_state)
    end.

setup_engine(#{ contract := <<_:256>> = ContractPubkey
              , call := Call
              , gas := Gas
              , value := Value
              },
             Spec, Cache) ->
    {tuple, {Function, {tuple, ArgTuple}}} =
        aeb_fate_encoding:deserialize(Call),
    Arguments = tuple_to_list(ArgTuple),
    Address = aeb_fate_data:make_address(ContractPubkey),
    ES1 = aefa_engine_state:new(Gas, Value, Spec, aefa_chain_api:new(Spec), Cache),
    ES2 = set_function(Address, Function, ES1),
    ES3 = push_arguments(Arguments, ES2),
    Signature = get_function_signature(Function, ES3),
    ok = check_signature(Signature, ES3),
    ES4 = bind_args_from_signature(Signature, ES3),
    aefa_engine_state:set_caller(aeb_fate_data:make_address(maps:get(caller, Spec)), ES4).

set_function(?FATE_ADDRESS(_) = Address, Function, ES) ->
    case aefa_engine_state:current_contract(ES) =:= Address of
        true ->
            case aefa_engine_state:current_function(ES) =:= Function of
                true ->
                    ES;
                false ->
                    set_local_function(Function, ES)
            end;
        false ->
            set_remote_function(Address, Function, ES)
    end.

set_remote_function(?FATE_ADDRESS(Pubkey) = Address, Function, ES) ->
    Contracts = aefa_engine_state:contracts(ES),
    case maps:get(Address, Contracts, void) of
        void ->
            APIState  = aefa_engine_state:chain_api(ES),
            case aefa_chain_api:contract_fate_code(Pubkey, APIState) of
                {ok, ContractCode, APIState1} ->
                    Contracts1 = maps:put(Pubkey, ContractCode, Contracts),
                    ES1 = aefa_engine_state:set_contracts(Contracts1, ES),
                    ES2 = aefa_engine_state:set_chain_api(APIState1, ES1),
                    ES3 = aefa_engine_state:update_for_remote_call(Address, ContractCode, ES2),
                    set_local_function(Function, ES3);
                error ->
                    abort({trying_to_call_contract, Pubkey}, ES)
            end;
        ContractCode ->
            ES1 = aefa_engine_state:update_for_remote_call(Address, ContractCode, ES),
            set_local_function(Function, ES1)
    end.

set_local_function(Function, ES) ->
    BBs = get_function_code(Function, ES),
    ES1 = aefa_engine_state:set_current_function(Function, ES),
    ES2 = aefa_engine_state:set_current_bb(0, ES1),
    aefa_engine_state:set_bbs(BBs, ES2).

get_function_code(Name, ES) ->
    case maps:get(Name, aefa_engine_state:functions(ES), void) of
        void -> abort({trying_to_call_function, Name}, ES);
        {_Signature, Code} -> Code
    end.

get_function_signature(Name, ES) ->
    case maps:get(Name, aefa_engine_state:functions(ES), void) of
        void -> abort({trying_to_call_function, Name}, ES);
        {Signature, _Code} -> Signature
    end.

check_return_type(ES) ->
    Current = aefa_engine_state:current_function(ES),
    {_ArgTypes, RetSignature} = get_function_signature(Current, ES),
    Acc = aefa_engine_state:accumulator(ES),
    case check_type(RetSignature, Acc) of
        true -> ES;
        false -> abort({bad_return_type, Acc, RetSignature}, ES)
    end.

check_signature({ArgTypes, _RetSignature}, ES) ->
    Stack = aefa_engine_state:accumulator_stack(ES),
    Args = [aefa_engine_state:accumulator(ES) | Stack],
    case check_arg_types(ArgTypes, Args) of
        ok ->
            ok;
        {error, T, V}  ->
            abort({value_does_not_match_type, V, T}, ES)
    end.

bind_args_from_signature({ArgTypes, _RetSignature}, ES) ->
    Stack = aefa_engine_state:accumulator_stack(ES),
    Args = [aefa_engine_state:accumulator(ES) | Stack],
    bind_args(0, Args, ArgTypes, #{}, ES).

check_arg_types([], _) -> ok;
check_arg_types([T|Ts], [A|As]) ->
    case check_type(T,A) of
        true -> check_arg_types(Ts, As);
        false -> {error, T, A}
    end.

check_all_types([], []) -> true;
check_all_types([T|Ts], [A|As]) ->
    check_type(T, A) andalso  check_all_types(Ts, As).

check_same_type(_, []) -> true;
check_same_type(T, [A|As]) ->
    check_type(T, A) andalso check_same_type(T, As).

bind_args(N, _, [], Mem, EngineState) ->
    ES1 = drop(N, EngineState),
    aefa_engine_state:push_env(Mem, ES1);
bind_args(N, [Arg|Args], [_Type|Types], Mem, EngineState) ->
    bind_args(N+1, Args, Types, Mem#{{arg, N} => {val, Arg}}, EngineState).

%% TODO: Add types (and tests).
check_type(any, _) -> true;
check_type(_, any) -> true;
check_type(integer, I) when ?IS_FATE_INTEGER(I) -> true;
check_type(boolean, B) when ?IS_FATE_BOOLEAN(B) -> true;
check_type(string, S) when ?IS_FATE_STRING(S) -> true;
check_type(address, A) when ?IS_FATE_ADDRESS(A) -> true;
check_type(bits, B) when ?IS_FATE_BITS(B) -> true;
check_type({list, any}, L) when ?IS_FATE_LIST(L) ->
    true;
check_type({list, ET}, L) when ?IS_FATE_LIST(L) ->
    check_same_type(ET, ?FATE_LIST_VALUE(L));
check_type({tuple, Elements}, T) when ?IS_FATE_TUPLE(T) ->
    check_all_types(Elements, ?FATE_TUPLE_ELEMENTS(T));
check_type({map, Key, Value}, M) when ?IS_FATE_MAP(M) ->
    {Ks, Vs} = lists:unzip(maps:to_list(?FATE_MAP_VALUE(M))),
    check_same_type(Key, Ks) andalso
    check_same_type(Value, Vs);
check_type({variant, Size}, V) when ?IS_FATE_VARIANT(V) ->
    ?FATE_VARIANT(VSize,_Tag,_Values) = V,
    Size =:= VSize;
check_type(_T, _V) -> false.

type(I) when ?IS_FATE_INTEGER(I)  -> integer;
type(B) when ?IS_FATE_BOOLEAN(B)  -> boolean;
type(B) when ?IS_FATE_BITS(B)     -> bits;
type(A) when ?IS_FATE_ADDRESS(A)  -> address;
type(S) when ?IS_FATE_STRING(S)   -> string;
type([E|L]) when ?IS_FATE_LIST(L) -> {list, type(E)};
type(T) when ?IS_FATE_TUPLE(T)    -> {tuple, lists:map(fun type/1, tuple_to_list(element(2, T)))};
type(?FATE_VARIANT(Sizes, _, _))  -> {variant, Sizes};
type([]) -> {list, any}.
%% TODO: handle all types.

jump(BB, ES) ->
    NewES = aefa_engine_state:set_current_bb(BB, ES),
    Instructions = aefa_engine_state:current_bb_instructions(NewES),
    {Instructions, NewES}.

%% ------------------------------------------------------
%% Arguments & Accumulator (-stack)
%% ------------------------------------------------------
push_arguments(Args, ES) ->
    aefa_engine_state:push_arguments(Args, ES).

pop_n(0, ES) -> {[], ES};
pop_n(N, ES) ->
    {Values, ES1} = pop_n(N-1, ES),
    {Value, ES2} = aefa_engine_state:pop_accumulator(ES1),
    {[Value | Values], ES2}.


dup(ES) ->
    aefa_engine_state:dup_accumulator(ES).

dup(N, ES) ->
    aefa_engine_state:dup_accumulator(N, ES).

drop(N, ES) ->
    aefa_engine_state:drop_accumulator(N, ES).

push(V, ES) ->
    aefa_engine_state:push_accumulator(V, ES).

%% ------------------------------------------------------
%% Call stack

push_return_address(ES) ->
    aefa_engine_state:push_return_address(ES).

%% Push a gas cap on the call stack to limit the available gas, but keep the
%% remaining gas around.
push_gas_cap(Gas, ES) when not ?IS_FATE_INTEGER(Gas) ->
    abort({value_does_not_match_type, Gas, integer}, ES);
push_gas_cap(Gas, ES) when ?IS_FATE_INTEGER(Gas) ->
    GasInt = ?FATE_INTEGER_VALUE(Gas),
    case GasInt > 0 of
        false ->
            abort({call_error, bad_gas_cap}, ES);
        true ->
            aefa_engine_state:push_gas_cap(GasInt, ES)
    end.

pop_call_stack(ES) ->
    case aefa_engine_state:call_stack(ES) of
        [] -> {stop, ES};
        [{gas_store, StoredGas}| Rest] ->
            Gas = StoredGas + aefa_engine_state:gas(ES),
            ES1 = aefa_engine_state:set_gas(Gas, ES),
            ES2 = aefa_engine_state:set_call_stack(Rest, ES1),
            pop_call_stack(ES2);
        [{Contract, Function, BB, Mem, Value}| Rest] ->
            ES0 = aefa_engine_state:set_call_value(Value, ES),
            ES1 = aefa_engine_state:set_call_stack(Rest, ES0),
            ES2 = aefa_engine_state:set_memory(Mem, ES1),
            ES3 = set_function(Contract, Function, ES2),
            {jump, BB, ES3}
    end.

collect_gas_stores([{gas_store, Gas}|Left], AccGas) ->
    collect_gas_stores(Left, AccGas + Gas);
collect_gas_stores([{_, _, _, _, _}|Left], AccGas) ->
    collect_gas_stores(Left, AccGas);
collect_gas_stores([], AccGas) ->
    AccGas.


%% ------------------------------------------------------
%% Memory

lookup_var(Var, ES) ->
    case aefa_engine_state:memory(ES) of
        [Env|_Envs] ->
            case maps:get(Var, Env, undefined) of
                {val, Value} ->
                    Value;
                undefined ->
                    abort({undefined_var, Var}, ES)
            end;
        [] ->
            abort({undefined_var, Var}, ES)
    end.


store_var(Var, Val, ES) ->
    [Env|Envs] = aefa_engine_state:memory(ES),
    aefa_engine_state:set_memory([Env#{ Var => {val, Val}} | Envs], ES).

%% ----------------------------
