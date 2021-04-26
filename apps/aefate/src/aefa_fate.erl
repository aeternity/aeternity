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
        , verify_init_calldata/1
        , is_valid_calldata/1
        ]).

-export([get_trace/1]).

-export([ check_remote/2
        , check_return_type/1
        , check_signature/3
        , pop_args_from_signature/2
        , pop_args/2
        , bind_args/2
        , ensure_contract_store/2
        , unfold_store_maps/3
        , unfold_store_maps_in_args/2
        , check_type/2
        , get_function_signature/2
        , push_gas_cap/2
        , push_continuation/2
        , push_return_address/1
        , push_return_type_check/5
        , runtime_exit/2
        , runtime_revert/2
        , set_local_function/3
        , set_remote_function/7
        ]
       ).


%% Memory handling.
-export([ lookup_var/2
        , store_var/3 ]).

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
-include("../../aecontract/include/hard_forks.hrl").

-ifdef(TEST).
-define(trace(I,S), aefa_engine_state:add_trace(I, S)).
-else.
-define(trace(I,S), S).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

-ifdef(TEST).
run_with_cache(Spec, Env, Cache) ->
    try execute(setup_engine(Spec, Env, Cache)) of
        Res -> {ok, Res}
    catch
        throw:{?MODULE, revert, S, ES} -> {revert, S, ES};
        throw:{?MODULE, E, ES} -> {error, E, ES}
    end.
-endif.

run(Spec, Env) ->
    try execute(setup_engine(Spec, Env)) of
        ES -> {ok, ES}
    catch
        throw:{?MODULE, revert, S, ES} -> {revert, S, ES};
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

verify_init_calldata(CallData) ->
    case decode_calldata(CallData) of
        {?FATE_INIT_ID, _Args} -> ok;
        _                      -> error
    end.

is_valid_calldata(CallData) ->
    case decode_calldata(CallData) of
        {_FHash, _Args} -> true;
        false           -> false
    end.

decode_calldata(CallData) ->
    try aeb_fate_encoding:deserialize(CallData) of
        Decoded when ?IS_FATE_TUPLE(Decoded) ->
            case ?FATE_TUPLE_ELEMENTS(Decoded) of
                [FHash, Args] when ?IS_FATE_TUPLE(Args),
                                   ?IS_FATE_STRING(FHash) ->
                    {FHash, Args};
                _ ->
                    false
            end;
        _ ->
            false
    catch _:_ -> false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-define(t(__S,__A,__ES),
        runtime_error(__S,__A,__ES)).

-spec runtime_error(Format :: string(), [term()],
                    aefa_engine_state:state()) -> no_return().
runtime_error(S, A, ES) ->
    Gas = aefa_engine_state:collect_gas_stores_on_error(ES),
    ES1 = aefa_engine_state:set_gas(Gas, ES),
    throw({?MODULE, iolist_to_binary(io_lib:format(S, A)), ES1}).

-spec runtime_exit(string(), aefa_engine_state:state()) -> no_return().
runtime_exit(Value, ES) ->
    Gas = aefa_engine_state:collect_gas_stores_on_exit(ES),
    ES1 = aefa_engine_state:set_gas(Gas, ES),
    throw({?MODULE, Value, ES1}).

-spec runtime_revert(aeb_fate_data:fate_string(), aefa_engine_state:state()) -> no_return().
runtime_revert(Value, ES) when ?IS_FATE_STRING(Value) ->
    Gas = aefa_engine_state:collect_gas_stores_on_revert(ES),
    ES1 = aefa_engine_state:set_gas(Gas, ES),
    throw({?MODULE, revert, Value, ES1}).

%% Runtime error messages for dry run and debugging.
%% Should result in one type of runtime error and use all gas when
%% executed on chain.
-ifdef(TEST).
-define(MAX_TERM_DEPTH, 20).
-else.
-define(MAX_TERM_DEPTH, 6).
-endif.

-spec abort(term(), aefa_engine_state:state()) -> no_return().
abort({invalid_tuple_size, Size}, ES) ->
    ?t("Invalid tuple size: ~P", [Size, ?MAX_TERM_DEPTH], ES);
abort({element_index_out_of_bounds, Index}, ES) ->
    ?t("Bad index argument to element, Index: ~P", [Index, ?MAX_TERM_DEPTH], ES);
abort({bad_variant_tag, Tag}, ES) ->
    ?t("Type error in switch: tag ~P is larger than switch op", [Tag, ?MAX_TERM_DEPTH], ES);
abort({bad_variant_size, Size}, ES) ->
    ?t("Type error in switch: wrong size ~P", [Size, ?MAX_TERM_DEPTH], ES);
abort({type_error, Op, Args}, ES) ->
    ?t("Bad arguments to ~P: ~P", [Op, ?MAX_TERM_DEPTH, Args, ?MAX_TERM_DEPTH], ES);
abort(hd_on_empty_list, ES) ->
    ?t("Head on empty list", [], ES);
abort(tl_on_empty_list, ES) ->
    ?t("Tail on empty list", [], ES);
abort({arithmetic_error, Reason}, ES) ->
    ?t("Arithmetic error: ~P", [Reason, ?MAX_TERM_DEPTH], ES);
abort(division_by_zero, ES) ->
    ?t("Arithmetic error: division by zero", [], ES);
abort(mod_by_zero, ES) ->
    ?t("Arithmetic error: mod by zero", [], ES);
abort(pow_too_large_exp, ES) ->
    ?t("Arithmetic error: pow with too large exponent", [], ES);
abort(missing_map_key, ES) ->
    ?t("Maps: Key does not exist", [], ES);
abort(bad_store_map_id, ES) ->
    ?t("Maps: Map does not exist", [], ES);
abort({undefined_var, Var}, ES) ->
    ?t("Undefined var: ~P", [Var, ?MAX_TERM_DEPTH], ES);
abort({bad_return_type, Val, Type}, ES) ->
    ?t("Type error on return: ~P is not of type ~P", [Val, ?MAX_TERM_DEPTH, Type, ?MAX_TERM_DEPTH], ES);
abort(remote_type_mismatch, ES) ->
    ?t("Type of remote function does not match expected type", [], ES);
abort({function_arity_mismatch, Got, Expected}, ES) ->
    ?t("Expected ~P arguments, got ~P", [Expected, ?MAX_TERM_DEPTH, Got, ?MAX_TERM_DEPTH], ES);
abort({value_does_not_match_type, Val, Type}, ES) ->
    ?t("Type error on call: ~P is not of type ~P", [Val, ?MAX_TERM_DEPTH, Type, ?MAX_TERM_DEPTH], ES);
abort({trying_to_reach_bb, BB}, ES) ->
    ?t("Trying to jump to non existing bb: ~P", [BB, ?MAX_TERM_DEPTH], ES);
abort({trying_to_call_function, Name}, ES) ->
    ?t("Trying to call undefined function: ~W", [Name, ?MAX_TERM_DEPTH], ES);
abort(invalid_init_call, ES) ->
    ?t("Calling init is not allowed in this context", [], ES);
abort({trying_to_call_contract, Pubkey}, ES) ->
    ?t("Trying to call invalid contract: ~w", [Pubkey], ES);
abort({not_allowed_in_auth_context, Op}, ES) ->
    ?t("Operation ~P not allowed in GA Authentication context", [Op, ?MAX_TERM_DEPTH], ES);
abort({not_allowed_offchain, Op}, ES) ->
    ?t("Operation ~P not allowed off chain", [Op, ?MAX_TERM_DEPTH], ES);
abort(negative_value_in_call, ES) ->
    ?t("Trying to transfer negative value in call", [], ES);
abort({call_error, What}, ES) ->
    ?t("Error in call: ~W", [What, ?MAX_TERM_DEPTH], ES);
abort({function_is_not_payable, Fun}, ES) ->
    ?t("Function with hash ~W is not payable", [Fun, ?MAX_TERM_DEPTH], ES);
abort({function_is_private, Fun}, ES) ->
    ?t("Function with hash ~W is private", [Fun, ?MAX_TERM_DEPTH], ES);
abort({primop_error, Which, What}, ES) ->
    ?t("Error in ~W: ~W", [Which, ?MAX_TERM_DEPTH, What, ?MAX_TERM_DEPTH], ES);
abort(reentrant_call, ES) ->
    ?t("Reentrant call", [], ES);
abort({log_illegal_int, N}, ES) ->
    ?t("Illegal integer in log: ~W", [N, ?MAX_TERM_DEPTH], ES);
abort(log_illegal_bits, ES) ->
    ?t("Illegal bits in log", [], ES);
abort(out_of_gas, ES) ->
    ?t("Out of gas", [], ES);
abort(bad_bytecode, ES) ->
    ?t("Bad byte code", [], ES);
abort({disabled_operation, Op}, ES) ->
    ?t("Error: operation ~P is disabled", [Op, ?MAX_TERM_DEPTH], ES);
abort({pop_empty_stack, N}, ES) ->
    ?t("Stack overflow when pop:ing ~P elements", [N, ?MAX_TERM_DEPTH], ES);
abort({bad_map, Op, Map}, ES) ->
    ?t("Operation ~P expected a map as argument, got ~P", [Op, ?MAX_TERM_DEPTH, Map, ?MAX_TERM_DEPTH], ES);
abort({op_not_implemented, Op}, ES) ->
    ?t("Operation ~P is not implemented", [Op, ?MAX_TERM_DEPTH], ES).

abort(E) -> throw({add_engine_state, E}).

execute(EngineState) ->
    Instructions = aefa_engine_state:current_bb_instructions(EngineState),
    loop(Instructions, EngineState).

loop(Instructions, EngineState) ->
    case step(Instructions, EngineState) of
        {stop, FinalState} ->
            case aefa_engine_state:finalize(FinalState) of
                {ok, ES} ->
                    %% We have already unfolded the result, but the result
                    %% is also serialized, so a final gas cost.
                    aefa_engine_state:spend_gas_for_traversal(
                        aefa_engine_state:accumulator(ES), final, ES);
                {error, What} -> abort(What, FinalState)
            end;
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
    try aefa_fate_eval:eval(I, ES) of
        {next, NewState} -> step(Is, NewState);
        {jump,_BB,_NewState} = Res -> Res;
        {stop, _NewState} = Res -> Res
    catch
        throw:{?MODULE, _, _} = Err ->
            catch_protected(Err, EngineState0);
        throw:{?MODULE, revert, _, _} = Err ->
            catch_protected(Err, EngineState0)
    end.

catch_protected(Err, ES) ->
    case aefa_engine_state:pop_call_stack(ES) of
        {empty, _} -> throw(Err);
        {modify, Cont, ES1} -> catch_protected(Err, Cont(ES1));
        {return_check, _, protected, _, Stores, API, ES1} ->
            ES2 = aefa_engine_state:set_accumulator(make_none(),
                  aefa_engine_state:set_stores(Stores,
                  aefa_engine_state:set_chain_api(API, ES1))),
            pop_call_stack(ES2);
        {return_check, _, _, _, _, _, ES1} -> catch_protected(Err, ES1);
        {local, _, _, _, ES1}        -> catch_protected(Err, ES1);
        {remote, _, _, _, _, _, ES1} -> catch_protected(Err, ES1)
    end.

%% -----------------------------------------------------------

setup_engine(#{ contract := <<_:256>> = ContractPubkey
              , code := ByteCode
              , vm_version := VMVersion} = Spec, Env) ->
    try aeb_fate_code:deserialize(ByteCode) of
        Code ->
            Cache = #{ ContractPubkey => { Code, VMVersion } },
            setup_engine(Spec, Env, Cache)
    catch _:_ ->
            abort(bad_bytecode, no_state)
    end.

setup_engine(#{ contract := <<_:256>> = ContractPubkey
              , call := Call
              , gas := Gas
              , value := Value
              , store := Store
              , vm_version := VMVersion
              , allow_init := AllowInit
              }, Env, Cache) ->
    {tuple, {Function, {tuple, ArgTuple}}} =
        aeb_fate_encoding:deserialize(Call),
    Arguments = tuple_to_list(ArgTuple),
    Contract = aeb_fate_data:make_contract(ContractPubkey),
    Stores = aefa_stores:put_contract_store(ContractPubkey, Store, aefa_stores:new()),
    APIState = aefa_chain_api:new(Env),
    ES1 = aefa_engine_state:new(Gas, Value, Env, Stores, APIState, Cache, VMVersion),
    Caller = aeb_fate_data:make_address(maps:get(caller, Env)),
    ES2 = set_remote_function(Caller, Contract, Function, Value > 0, true, AllowInit, ES1),
    Signature = get_function_signature(Function, ES2),
    ES3 = check_signature(Arguments, Signature, ES2),
    bind_args(Arguments, ES3).

check_remote(Contract, EngineState) when not ?IS_FATE_CONTRACT(Contract) ->
    abort({value_does_not_match_type, Contract, contract}, EngineState);
check_remote(Contract, EngineState) ->
    case aefa_engine_state:check_reentrant_remote(Contract, EngineState) of
        {ok, ES} ->
            ES;
        error ->
            abort(reentrant_call, EngineState)
    end.

set_remote_function(Caller, ?FATE_CONTRACT(Pubkey), Function, CheckPayable, CheckPrivate, AllowInit, ES0) ->
    case aefa_engine_state:contract_fate_bytecode(Pubkey, ES0) of
        error ->
            abort({trying_to_call_contract, Pubkey}, ES0);
        {ok, ContractCode, VMV, ES1} ->
            ES2 = aefa_engine_state:update_for_remote_call(Pubkey, ContractCode, VMV, Caller, ES1),
            check_flags_and_set_local_function(CheckPayable, CheckPrivate, Function, AllowInit, ES2)
    end.

check_flags_and_set_local_function(false, false, Function, AllowInit, ES) ->
    set_local_function(Function, AllowInit, ES);
check_flags_and_set_local_function(CheckPayable, CheckPrivate, Function, AllowInit, ES) ->
    case (not CheckPayable) orelse is_function_payable(Function, ES) of
        true  ->
            case CheckPrivate andalso is_function_private(Function, ES) of
                false -> set_local_function(Function, AllowInit, ES);
                true  -> abort({function_is_private, Function}, ES)
            end;
        false -> abort({function_is_not_payable, Function}, ES)
    end.

set_local_function(Function, _AllowInit = true, ES) ->
    set_local_function_(Function, ES);
set_local_function(?FATE_INIT_ID, false, ES) ->
    abort(invalid_init_call, ES);
set_local_function(Function, false, ES) ->
    set_local_function_(Function, ES).

set_local_function_(Function, ES) ->
    BBs = get_function_code(Function, ES),
    ES1 = aefa_engine_state:set_current_function(Function, ES),
    ES2 = aefa_engine_state:set_current_bb(0, ES1),
    aefa_engine_state:set_bbs(BBs, ES2).

get_function_code(Name, ES) ->
    case maps:get(Name, aefa_engine_state:functions(ES), void) of
        void -> abort({trying_to_call_function, Name}, ES);
        {_Attrs, _Signature, Code} -> Code
    end.

get_function_signature(Name, ES) ->
    case maps:get(Name, aefa_engine_state:functions(ES), void) of
        void -> abort({trying_to_call_function, Name}, ES);
        {_Attrs, Signature, _Code} -> Signature
    end.

is_function_payable(Name, ES) ->
    case maps:get(Name, aefa_engine_state:functions(ES), void) of
        void -> abort({trying_to_call_function, Name}, ES);
        {Attrs, _Signature, _Code} -> lists:member(payable, Attrs)
    end.

is_function_private(Name, ES) ->
    case maps:get(Name, aefa_engine_state:functions(ES), void) of
        void -> abort({trying_to_call_function, Name}, ES);
        {Attrs, _Signature, _Code} -> lists:member(private, Attrs)
    end.

check_return_type(ES) ->
    Current = aefa_engine_state:current_function(ES),
    TVars   = aefa_engine_state:current_tvars(ES),
    {_ArgTypes, RetType} = get_function_signature(Current, ES),
    check_return_type(RetType, TVars, ES).

check_return_type(RetType, TVars, ES) ->
    Acc = aefa_engine_state:accumulator(ES),
    ES1 = aefa_engine_state:spend_gas_for_traversal(Acc, simple, ES),
    case check_type(RetType, Acc) of
        false -> abort({bad_return_type, Acc, RetType}, ES1);
        Inst  ->
            case merge_match(Inst, TVars) of
                false -> abort({bad_return_type, Acc, instantiate_type(TVars, RetType)}, ES1);
                #{}   -> ES1
            end
    end.

check_signature(Args, {ArgTypes, _RetSignature}, ES) when length(ArgTypes) /= length(Args) ->
    abort({function_arity_mismatch, length(Args), length(ArgTypes)}, ES);
check_signature(Args, {ArgTypes, _RetSignature}, ES) ->
    case check_arg_types(ArgTypes, Args) of
        {ok, Inst} ->
            aefa_engine_state:set_current_tvars(Inst, ES);
        {error, T, V}  ->
            abort({value_does_not_match_type, V, T}, ES)
    end.

pop_args_from_signature({ArgTypes, _RetSignature}, ES) ->
    pop_args(length(ArgTypes), ES).

pop_args(N, ES) ->
    Tail  = aefa_engine_state:accumulator_stack(ES),
    Stack = [aefa_engine_state:accumulator(ES) | Tail],
    Args  = lists:sublist(Stack, N),
    Protocol = aefa_engine_state:consensus_version(ES),
    case length(Args) == N of
        false when Protocol >= ?IRIS_PROTOCOL_VSN ->
            abort({pop_empty_stack, N}, ES);
        _ ->
            {Args, drop(N, ES)}
    end.

bind_args(Args, ES) ->
    bind_args(0, Args, #{}, ES).

check_arg_types(Ts, As0) ->
    As = lists:sublist(As0, length(Ts)),
    case check_type({tuple, Ts}, {tuple, list_to_tuple(As)}) of
        false -> {error, Ts, As};
        Inst  -> {ok, Inst}
    end.

bind_args(_, [], Mem, EngineState) ->
    aefa_engine_state:set_memory(Mem, EngineState);
bind_args(N, [Arg|Args], Mem, EngineState) ->
    bind_args(N + 1, Args, Mem#{{arg, N} => {val, Arg}}, EngineState).

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
infer_type(X) when ?IS_FATE_BYTES(X)     -> {bytes, byte_size(?FATE_BYTES_VALUE(X))};
infer_type(X) when ?IS_FATE_CONTRACT(X)  -> contract;
infer_type(X) when ?IS_FATE_ORACLE(X)    -> oracle;
infer_type(X) when ?IS_FATE_ORACLE_Q(X)  -> oracle_query;
infer_type(X) when ?IS_FATE_BITS(X)      -> bits;
infer_type(X) when ?IS_FATE_LIST(X) ->
    {list, infer_element_type(?FATE_LIST_VALUE(X))};
infer_type(X) when ?IS_FATE_MAP(X) ->
    {Ks, Vs} = lists:unzip(maps:to_list(?FATE_MAP_VALUE(X))),
    KeyT = infer_element_type(Ks),
    ValT = infer_element_type(Vs),
    {map, KeyT, ValT};
infer_type(?FATE_STORE_MAP(_, _)) ->
    {map, any, any};    %% Should only happen for local calls
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

instantiate_type(TVars, {tvar, X})          -> maps:get(X, TVars, any);
instantiate_type(_TVars, T) when is_atom(T) -> T;
instantiate_type(_TVars, T = {bytes, _})    -> T;
instantiate_type(TVars, {list, T})          -> {list, instantiate_type(TVars, T)};
instantiate_type(TVars, {tuple, Ts})        -> {tuple, [instantiate_type(TVars, T) || T <- Ts]};
instantiate_type(TVars, {map, K, V})        -> {map, instantiate_type(TVars, K), instantiate_type(TVars, V)};
instantiate_type(TVars, {variant, Ts})      -> {variant, [instantiate_type(TVars, T) || T <- Ts]}.

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

push_continuation(Continuation, ES) ->
    aefa_engine_state:push_continuation(Continuation, ES).

push_return_address(ES) ->
    aefa_engine_state:push_call_stack(ES).

push_return_type_check({CalleeArgs, CalleeRet}, {CallerArgs, CallerRet}, CalleeTVars, Protected, ES) ->
    CalleeSig = {tuple, [CalleeRet | CalleeArgs]},
    CallerSig = {tuple, [CallerRet | CallerArgs]},

    %% CallerTVars instantiates the type vars in CalleeSig, so we have to
    %% unify CalleeSig and CallerSig to get the instantiation for the Caller
    %% tvars.
    InstCalleeSig = instantiate_type(CalleeTVars, CalleeSig),
    case match_type(CallerSig, InstCalleeSig) of
        false       -> abort(remote_type_mismatch, ES);
        CallerTVars -> aefa_engine_state:push_return_type_check(CallerRet, CallerTVars, Protected, ES)
    end.

%% Push a gas cap on the call stack to limit the available gas, but keep the
%% remaining gas around.
push_gas_cap(Gas, ES) when not ?IS_FATE_INTEGER(Gas) ->
    abort({value_does_not_match_type, Gas, integer}, ES);
push_gas_cap(Gas, ES) when ?IS_FATE_INTEGER(Gas) ->
    GasInt = ?FATE_INTEGER_VALUE(Gas),
    case GasInt > 0 of
        false ->
            abort({call_error, {bad_gas_cap, GasInt}}, ES);
        true ->
            aefa_engine_state:push_gas_cap(GasInt, ES)
    end.

pop_call_stack(ES) ->
    case aefa_engine_state:pop_call_stack(ES) of
        {empty, ES1} ->
            ES2 = unfold_store_maps(ES1),
            {stop, ES2};
        {modify, Continuation, ES1} ->
            pop_call_stack(Continuation(ES1));
        {return_check, TVars, Protected, RetType, Stores, API, ES1} ->
            ES2 = check_return_type_protected(Protected, RetType, TVars, Stores, API, ES1),
            pop_call_stack(ES2);
        {local, Function, TVars, BB, ES1} ->
            % Allow returning to init
            ES2 = set_local_function(Function, true, ES1),
            ES3 = aefa_engine_state:set_current_tvars(TVars, ES2),
            {jump, BB, ES3};
        {remote, Caller, Contract, Function, TVars, BB, ES1} ->
            %% Popping the callstack resets the current contract to the caller,
            %% but we need to unfold the store maps as the callee.
            Callee = aefa_engine_state:current_contract(ES),
            ES2    = with_current_contract(Callee, ES1, fun unfold_store_maps/1),
            % Allow returning to init
            ES3 = set_remote_function(Caller, Contract, Function, false, false, true, ES2),
            ES4 = aefa_engine_state:set_current_tvars(TVars, ES3),
            {jump, BB, ES4}
    end.

with_current_contract(Pubkey, ES, Fun) ->
    Old = aefa_engine_state:current_contract(ES),
    ES1 = aefa_engine_state:set_current_contract(Pubkey, ES),
    ES2 = Fun(ES1),
    aefa_engine_state:set_current_contract(Old, ES2).

check_return_type_protected(unprotected, RetType, TVars, _Stores, _API, ES) ->
    check_return_type(RetType, TVars, ES);
check_return_type_protected(protected, RetType, TVars, Stores, API, ES) ->
    try check_return_type(RetType, TVars, ES) of
        ES1 ->
            Val = aefa_engine_state:accumulator(ES1),
            aefa_engine_state:set_accumulator(make_some(Val), ES1)
    catch _:_ ->
        %% Rollback side effects from failed call
        ES1 = aefa_engine_state:set_stores(Stores,
              aefa_engine_state:set_chain_api(API, ES)),
        aefa_engine_state:set_accumulator(make_none(), ES1)
    end.

unfold_store_maps(ES) ->
    {Acc, ES1} = unfold_store_maps(aefa_engine_state:accumulator(ES), ES, unfold),
    aefa_engine_state:set_accumulator(Acc, ES1).

unfold_store_maps_in_args(0, ES)     -> ES;
unfold_store_maps_in_args(Arity, ES) ->
    Acc   = aefa_engine_state:accumulator(ES),
    Stack = aefa_engine_state:accumulator_stack(ES),
    {Args, Rest} = lists:split(min(Arity, length(Stack) + 1), [Acc | Stack]),
    {Args1, ES1} = unfold_store_maps(?MAKE_FATE_LIST(Args), ES, unfold),
    [Acc1 | Stack1] = ?FATE_LIST_VALUE(Args1),
    aefa_engine_state:set_accumulator(Acc1,
        aefa_engine_state:set_accumulator_stack(Stack1 ++ Rest, ES1)).

unfold_store_maps(Val, ES, CostModel) ->
    ES1 = aefa_engine_state:spend_gas_for_traversal(Val, simple, ES),
    case aeb_fate_maps:has_store_maps(Val) of
        true ->
            Pubkey = aefa_engine_state:current_contract(ES1),
            {Store, ES2} = ensure_contract_store(Pubkey, ES1),
            Store1 = aefa_stores:cache_map_metadata(Pubkey, Store),
            ES3    = aefa_engine_state:set_stores(Store1, ES2),
            Unfold = fun(Id) ->
                        {List, _Store2} = aefa_stores:store_map_to_list(Pubkey, Id, Store1),
                        maps:from_list(List)
                     end,
            MapSize = fun(Id) ->
                          {Size, _Store2} = aefa_stores:store_map_size(Pubkey, Id, Store1),
                          Size
                      end,
            ES4 = aefa_engine_state:spend_gas_for_traversal(Val, CostModel, {MapSize, Unfold}, ES3),
            {aeb_fate_maps:unfold_store_maps(Unfold, Val), ES4};
        false ->
            {Val, ES1}
    end.

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

make_none() ->
    aeb_fate_data:make_variant([0, 1], 0, {}).

make_some(Val) ->
    aeb_fate_data:make_variant([0, 1], 1, {Val}).
