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
        , check_remote/2
        , check_return_type/1
        , check_signature/2
        , get_function_signature/2
        , push_gas_cap/2
        , push_return_address/1
        , set_local_function/2
        , set_remote_function/3
        , terms_are_of_same_type/2
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
abort(missing_map_key, ES) ->
    ?t("Maps: Key does not exists", [], ES);
abort({type_error, cons}, ES) ->
    ?t("Type error in cons: creating polymorphic list", [], ES);
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
abort(reentrant_call, ES) ->
    ?t("Reentrant call", [], ES);
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
            aefa_engine_state:finalize(FinalState);
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
    try aeb_fate_code:deserialize(ByteCode) of
        Code ->
            Cache = #{ ContractPubkey => Code },
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
    Contract = aeb_fate_data:make_contract(ContractPubkey),
    ES1 = aefa_engine_state:new(Gas, Value, Spec, aefa_chain_api:new(Spec), Cache),
    ES2 = set_remote_function(Contract, Function, ES1),
    ES3 = aefa_engine_state:push_arguments(Arguments, ES2),
    Signature = get_function_signature(Function, ES3),
    ok = check_signature(Signature, ES3),
    ES4 = bind_args_from_signature(Signature, ES3),
    aefa_engine_state:set_caller(aeb_fate_data:make_address(maps:get(caller, Spec)), ES4).

check_remote(Contract, EngineState) when not ?IS_FATE_CONTRACT(Contract) ->
    abort({value_does_not_match_type, Contract, contract}, EngineState);
check_remote(Contract, EngineState) ->
    case aefa_engine_state:check_reentrant_remote(Contract, EngineState) of
        {ok, ES} ->
            ES;
        error ->
            abort(reentrant_call, EngineState)
    end.

set_remote_function(?FATE_CONTRACT(Pubkey), Function, ES) ->
    CodeCache = aefa_engine_state:code_cache(ES),
    case maps:get(Pubkey, CodeCache, void) of
        void ->
            APIState  = aefa_engine_state:chain_api(ES),
            case aefa_chain_api:contract_fate_code(Pubkey, APIState) of
                {ok, ContractCode, APIState1} ->
                    CodeCache1 = maps:put(Pubkey, ContractCode, CodeCache),
                    ES1 = aefa_engine_state:set_code_cache(CodeCache1, ES),
                    ES2 = aefa_engine_state:set_chain_api(APIState1, ES1),
                    ES3 = aefa_engine_state:update_for_remote_call(Pubkey, ContractCode, ES2),
                    set_local_function(Function, ES3);
                error ->
                    abort({trying_to_call_contract, Pubkey}, ES)
            end;
        ContractCode ->
            ES1 = aefa_engine_state:update_for_remote_call(Pubkey, ContractCode, ES),
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
        #{}   -> ES;
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

check_arg_types(Ts, As0) ->
    As = lists:sublist(As0, length(Ts)),
    case check_type({tuple, Ts}, {tuple, list_to_tuple(As)}) of
        #{}   -> ok;
        false -> {error, Ts, As}
    end.

bind_args(N, _, [], Mem, EngineState) ->
    ES1 = drop(N, EngineState),
    aefa_engine_state:set_memory(Mem, ES1);
bind_args(N, [Arg|Args], [_Type|Types], Mem, EngineState) ->
    bind_args(N+1, Args, Types, Mem#{{arg, N} => {val, Arg}}, EngineState).

check_type(T, V) ->
    try
        match_type(T, infer_type(V))
    catch throw:_Err ->
        false
    end.

infer_type(X) when ?IS_FATE_INTEGER(X)   -> integer;
infer_type(X) when ?IS_FATE_BOOLEAN(X)   -> boolean;
infer_type(X) when ?IS_FATE_STRING(X)    -> string;
infer_type(X) when ?IS_FATE_ADDRESS(X)   -> address;
infer_type(X) when ?IS_FATE_HASH(X)      -> hash;
infer_type(X) when ?IS_FATE_SIGNATURE(X) -> signature;
infer_type(X) when ?IS_FATE_CONTRACT(X)  -> contract;
infer_type(X) when ?IS_FATE_ORACLE(X)    -> oracle;
infer_type(X) when ?IS_FATE_NAME(X)      -> name;
infer_type(X) when ?IS_FATE_BITS(X)      -> bits;
infer_type(X) when ?IS_FATE_LIST(X) ->
    {list, infer_element_type(?FATE_LIST_VALUE(X))};
infer_type(X) when ?IS_FATE_MAP(X) ->
    {Ks, Vs} = lists:unzip(maps:to_list(?FATE_MAP_VALUE(X))),
    KeyT = infer_element_type(Ks),
    ValT = infer_element_type(Vs),
    {map, KeyT, ValT};
infer_type(X) when ?IS_FATE_TUPLE(X) ->
    {tuple, [infer_type(Y) || Y <- ?FATE_TUPLE_ELEMENTS(X)]};
infer_type(X) when ?IS_FATE_VARIANT(X) ->
    ?FATE_VARIANT(Arities, Tag, Value) = X,
    N    = length(Arities),
    [ throw({bad_variant_tag, Tag, Arities}) || Tag >= N ],
    Type = infer_type(?FATE_TUPLE(Value)),
    %% Partial variant type (only arities for other constructors)
    {variant, [ if I == Tag -> Type; true -> A end
                || {A, I} <- lists:zip(Arities, lists:seq(0, N - 1))]};
infer_type(X) -> throw({not_a_fate_value, X}).

infer_element_type(Xs) ->
    lists:foldl(fun intersect_types/2, any, [infer_type(X) || X <- Xs]).

intersect_types(T, any) -> T;
intersect_types(any, T) -> T;
intersect_types(T, T) -> T;
intersect_types({list, S}, {list, T}) ->
    {list, intersect_types(S, T)};
intersect_types({tuple, Ss}, {tuple, Ts}) when length(Ss) == length(Ts) ->
    {tuple, lists:zipwith(fun intersect_types/2, Ss, Ts)};
intersect_types({map, SK, SV}, {map, TK, TV}) ->
    {map, intersect_types(SK, TK), intersect_types(SV, TV)};
intersect_types({variant, Ss}, {variant, Ts}) when length(Ss) == length(Ts) ->
    Isect = fun(N, N) when is_integer(N) -> N;
               (N, {tuple, Us}) when length(Us) == N -> {tuple, Us};
               ({tuple, Us}, N) when length(Us) == N -> {tuple, Us};
               (S, T) when is_tuple(S), is_tuple(T) -> intersect_types(S, T);
               (_, _) -> throw({not_compatible, {variant, Ss}, {variant, Ts}})
            end,
    {variant, lists:zipwith(Isect, Ss, Ts)};
intersect_types(S, T) -> throw({not_compatible, S, T}).

match_type(T, T) -> yes_match();
match_type(any, _) -> yes_match();
match_type(_, any) -> yes_match();
match_type({tvar, X}, T) -> #{ X => T };
match_type({list, T}, {list, S}) -> match_type(T, S);
match_type({tuple, Ts}, {tuple, Ss}) when length(Ts) == length(Ss) ->
    merge_match(lists:zipwith(fun match_type/2, Ts, Ss));
match_type({map, TK, TV}, {map, SK, SV}) ->
    merge_match(match_type(TK, SK), match_type(TV, SV));
match_type({variant, Ts}, {variant, Ss}) when length(Ts) == length(Ss) ->
    %% Second argument can be partial variant type
    Match = fun({tuple, Us}, N) when length(Us) == N -> yes_match();
               (T = {tuple, _}, S = {tuple, _})      -> match_type(T, S);
               (_, _)                                -> no_match() end,
    merge_match(lists:zipwith(Match, Ts, Ss));
match_type(_, _) -> no_match().

yes_match() -> #{}.
no_match() -> false.

merge_match(Xs) ->
    lists:foldl(fun merge_match/2, yes_match(), Xs).

merge_match(false, _) -> false;
merge_match(_, false) -> false;
merge_match(Inst1, Inst2) ->
    Ins = fun(_, false)  -> false;
             ({X, T}, M) ->
                case maps:get(X, M, undefined) of
                    undefined -> M#{ X => T };
                    S -> try M#{ X => intersect_types(S, T) }
                         catch throw:_ -> false end
                end end,
    lists:foldl(Ins, Inst2, maps:to_list(Inst1)).

terms_are_of_same_type(X, Y) when ?IS_FATE_INTEGER(X), ?IS_FATE_INTEGER(Y) -> true;
terms_are_of_same_type(X, Y) when ?IS_FATE_BOOLEAN(X), ?IS_FATE_BOOLEAN(Y) -> true;
terms_are_of_same_type(X, Y) when ?IS_FATE_BITS(X), ?IS_FATE_BITS(Y) -> true;
terms_are_of_same_type(X, Y) when ?IS_FATE_ADDRESS(X), ?IS_FATE_ADDRESS(Y) -> true;
terms_are_of_same_type(X, Y) when ?IS_FATE_CONTRACT(X), ?IS_FATE_CONTRACT(Y) -> true;
terms_are_of_same_type(X, Y) when ?IS_FATE_ORACLE(X), ?IS_FATE_ORACLE(Y) -> true;
terms_are_of_same_type(X, Y) when ?IS_FATE_NAME(X), ?IS_FATE_NAME(Y) -> true;
terms_are_of_same_type(X, Y) when ?IS_FATE_STRING(X), ?IS_FATE_STRING(Y) -> true;
terms_are_of_same_type(X, Y) when ?IS_FATE_TUPLE(X), ?IS_FATE_TUPLE(Y) ->
    %% NOTE: This could be more thorough, but it costs too much
    ?FATE_TUPLE(T1) = X,
    ?FATE_TUPLE(T2) = Y,
    tuple_size(T1) =:= tuple_size(T2);
terms_are_of_same_type(X, Y) when ?IS_FATE_VARIANT(X), ?IS_FATE_VARIANT(Y) ->
    %% NOTE: This could be more thorough, but it costs too much
    ?FATE_VARIANT(Arities1,_Tag1,_Value1) = X,
    ?FATE_VARIANT(Arities2,_Tag2,_Value2) = Y,
    Arities1 =:= Arities2;
terms_are_of_same_type(X, Y) when ?IS_FATE_LIST(X), ?IS_FATE_LIST(Y) ->
    L1 = ?FATE_LIST_VALUE(X),
    L2 = ?FATE_LIST_VALUE(Y),
    L1 =:= [] orelse L2 =:= [] orelse terms_are_of_same_type(hd(L1), hd(L2));
terms_are_of_same_type(_X,_Y) ->
    false.

jump(BB, ES) ->
    NewES = aefa_engine_state:set_current_bb(BB, ES),
    Instructions = aefa_engine_state:current_bb_instructions(NewES),
    {Instructions, NewES}.

%% ------------------------------------------------------
%% Arguments & Accumulator (-stack)
%% ------------------------------------------------------

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
    aefa_engine_state:push_call_stack(ES).

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
    case aefa_engine_state:pop_call_stack(ES) of
        empty ->
            {stop, ES};
        {local, Function, BB, ES1} ->
            ES2 = set_local_function(Function, ES1),
            {jump, BB, ES2};
        {remote, Contract, Function, BB, ES1} ->
            ES2 = set_remote_function(Contract, Function, ES1),
            {jump, BB, ES2}
    end.

collect_gas_stores([{gas_store, Gas}|Left], AccGas) ->
    collect_gas_stores(Left, AccGas + Gas);
collect_gas_stores([{_, _, _, _, _}|Left], AccGas) ->
    collect_gas_stores(Left, AccGas);
collect_gas_stores([], AccGas) ->
    AccGas.


%% ------------------------------------------------------
%% Memory

lookup_var({var, N}, ES) when N < 0 ->
    %% These variables represents the state.
    lookup_in_store(-N, ES);
lookup_var(Var, ES) ->
    Env = aefa_engine_state:memory(ES),
    case maps:get(Var, Env, undefined) of
        {val, Value} ->
            {Value, ES};
        undefined ->
            abort({undefined_var, Var}, ES)
    end.

store_var({var, N}, Val, ES) when N < 0 ->
    %% These variables represents the state.
    write_to_store(-N, Val, ES);
store_var(Var, Val, ES) ->
    Env = aefa_engine_state:memory(ES),
    aefa_engine_state:set_memory(Env#{ Var => {val, Val}}, ES).

lookup_in_store(N, ES) ->
    Current = aefa_engine_state:current_contract(ES),
    {Stores, ES1} = ensure_contract_store(Current, ES),
    case aefa_stores:find_value(Current, N, Stores) of
        {ok, Val} ->
            {Val, ES1};
        {ok, Val, Stores1} ->
            ES2 = aefa_engine_state:set_stores(Stores1, ES1),
            {Val, ES2};
        error ->
            abort({undefined_in_store, N}, ES1)
    end.

write_to_store(N, Val, ES) ->
    Current = aefa_engine_state:current_contract(ES),
    {Stores, ES1} = ensure_contract_store(Current, ES),
    Stores1 = aefa_stores:put_value(Current, N, Val, Stores),
    aefa_engine_state:set_stores(Stores1, ES1).

ensure_contract_store(Pubkey, ES) ->
    Stores = aefa_engine_state:stores(ES),
    case aefa_stores:has_contract(Pubkey, Stores) of
        false ->
            APIState  = aefa_engine_state:chain_api(ES),
            {Store, APIState1} = aefa_chain_api:contract_store(Pubkey, APIState),
            Stores1 = aefa_stores:put_contract_store(Pubkey, Store, Stores),
            ES1 = aefa_engine_state:set_chain_api(APIState1, ES),
            {Stores1, aefa_engine_state:set_stores(Stores1, ES1)};
        true ->
            {aefa_engine_state:stores(ES), ES}
    end.


%% ----------------------------
