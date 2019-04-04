%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% The FATE virtual machine
%%% @end
%%%-------------------------------------------------------------------

-module(aefa_fate).
-export([run/2]).

-export([get_trace/1]).

%% Type handling.
-export([ check_return_type/1
        , check_signature_and_bind_args/2
        , check_type/2
        , get_function_signature/2
        , push_return_address/1
        , set_current_function/2
        , set_function/3
        , type/1
        ]
       ).


%% Memory handling.
-export([ lookup_var/3
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


-include_lib("aebytecode/include/aeb_fate_data.hrl").

-ifdef(TEST).
-define(trace(I,S), S#{trace => [{I, erlang:process_info(self(), reductions)} |get_trace(S)]}).
-else.
-define(trace(I,S), S).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

run(What, Chain) ->
    EngineState = setup_engine(What, Chain),
    try execute(EngineState) of
        Res -> {ok, Res}
    catch
        throw:{?MODULE, E, ES} -> {error, E, ES}
    end.

get_trace(#{trace := T}) -> T.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-define(t(__S,__A,__ES), throw({?MODULE, iolist_to_binary(io_lib:format(__S, __A)), __ES})).

%% Runtime error messages for dry run and debugging.
%% Should result on one tyhpe of runtime error and use all gas when
%% executed on chain.
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
    ?t("Trying to call undefined function: ~p", [Name], ES).

abort(E) -> throw({add_engine_state, E}).

execute(EngineState) ->
    Instructions = current_bb(EngineState),
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
    BB = next_bb_index(EngineState),
    {jump, BB, EngineState};
step([I|Is], EngineState0) ->
    ES = ?trace(I, EngineState0),
    case aefa_fate_eval:eval(I, ES) of
        {next, NewState} -> step(Is, NewState);
        {jump,_BB,_NewState} = Res -> Res;
        {stop, _NewState} = Res -> Res

    end.




%% -----------------------------------------------------------



setup_engine(#{ contract := Contract
              , call := Call},
             Chain) ->
    {tuple, {Function, {tuple, ArgTuple}}} =
        aeb_fate_encoding:deserialize(Call),
    Arguments = tuple_to_list(ArgTuple),
    ES1 = new_engine_state(Chain),
    ES2 = set_function(Contract, Function, ES1),
    ES3 = push_arguments(Arguments, ES2),
    Signature = get_function_signature(Function, ES3),
    {ok, ES4} = check_signature_and_bind_args(Signature, ES3),
    ES4.



set_function(Contract, Function, #{ chain := Chain
                                  , contracts := Contracts} = ES) ->
    {ES2, #{functions := Code}} =
        case maps:get(Contract, Contracts, void) of
            void ->
                ContractCode = chain_get_contract(Contract, Chain),
                {ES#{contracts => maps:put(Contract, ContractCode, Contracts)},
                 ContractCode};
            ContractCode ->
                {ES, ContractCode}
        end,
    ES3 = ES2#{current_contract => Contract},
    ES4 = ES3#{functions => Code},
    set_current_function(Function, ES4).

%get_current_contract(#{current_contract := Contract}) ->
%    Contract.


set_current_function(Function, ES) ->
    BBs = get_function_code(Function, ES),
    ES#{current_function => Function
       , current_bb => 0
       , bbs := BBs
       }.


get_function_code(Name, #{functions := Functions} = ES) ->
    case maps:get(Name, Functions, void) of
        void -> abort({trying_to_call_function, Name}, ES);
        {_Signature, Code} -> Code
    end.

get_function_signature(Name,  #{functions := Functions} = ES) ->
    case maps:get(Name, Functions, void) of
        void -> abort({trying_to_call_function, Name}, ES);
        {Signature, _Code} -> Signature
    end.

check_return_type(#{current_function := Function,
                    accumulator := Acc} = ES) ->
    {_ArgTypes, RetSignature} = get_function_signature(Function, ES),
    case check_type(RetSignature, Acc) of
        true -> ES;
        false -> abort({bad_return_type, Acc, RetSignature}, ES)
    end.

check_signature_and_bind_args({ArgTypes, _RetSignature},
                #{ accumulator := Acc
                 , accumulator_stack := Stack}
               = EngineState) ->
    Args = [Acc | Stack],
    case check_arg_types(ArgTypes, Args) of
        ok ->
            {ok, bind_args(0, Args, ArgTypes, #{}, EngineState)};
        {error, T, V}  ->
            abort({value_does_not_match_type, V, T}, EngineState)
    end.

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
    new_env(Mem, drop(N, EngineState));
bind_args(N, [Arg|Args], [Type|Types], Mem, EngineState) ->
    bind_args(N+1, Args, Types, Mem#{{arg, N} => {Arg, Type}}, EngineState).

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
    check_all_types(Elements, aeb_fate_data:tuple_to_list(T));
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
type([]) -> {list, any}.
%% TODO: handle all types.

jump(BB, ES) ->
    NewES = set_current_bb_index(BB, ES),
    Instructions = current_bb(NewES),
    {Instructions, NewES}.



%% ------------------------------------------------------
%% Arguments & Accumulator (-stack)
%% ------------------------------------------------------


push_arguments(Args, #{ accumulator := X
                      , accumulator_stack := XS} = ES) ->
    push_arguments(lists:reverse(Args), X, XS, ES).

push_arguments([], Acc, Stack, ES) ->
    ES#{ accumulator := Acc
       , accumulator_stack := Stack};
push_arguments([A|As], Acc, Stack, ES ) ->
    push_arguments(As, A, [Acc | Stack], ES).


pop(#{ accumulator := X, accumulator_stack := []} = ES) ->
    {X, ES#{accumulator => ?FATE_VOID}};
pop(#{ accumulator := X, accumulator_stack := [V|Stack]} = ES) ->
    {X, ES#{ accumulator => V
           , accumulator_stack := Stack
           }}.

pop_n(0, ES) -> {[], ES};
pop_n(N, ES) ->
    {Values, ES1} = pop_n(N-1, ES),
    {Value, ES2} = pop(ES1),
    {[Value | Values], ES2}.


dup(#{ accumulator := X, accumulator_stack := Stack} = ES) ->
    ES#{ accumulator => X
       , accumulator_stack := [X|Stack]}.

dup(N, #{ accumulator := X, accumulator_stack := Stack} = ES) ->
    {X1, Stack} = get_n(N, [X|Stack]),
    ES#{ accumulator => X1
       , accumulator_stack := Stack}.

get_n(0, [X|XS]) -> {X, [X|XS]};
get_n(N, [X|XS]) ->
    {Y, List} = get_n(N-1, XS),
    {Y, [X|List]}.


drop(0, ES) -> ES;
drop(N, #{ accumulator := _, accumulator_stack := [V|Stack]} = ES) ->
    drop(N-1, ES#{ accumulator => V, accumulator_stack => Stack});
drop(N, #{ accumulator := _, accumulator_stack := []} = ES) ->
    drop(N-1, ES#{ accumulator => ?FATE_VOID, accumulator_stack => []}).

push(V,
     #{ accumulator := ?FATE_VOID
      , accumulator_stack := [] } = ES) ->
    ES#{ accumulator => V
       , accumulator_stack => []};
push(V,
     #{ accumulator := X
      , accumulator_stack := Stack } = ES) ->
    ES#{ accumulator => V
       , accumulator_stack => [X|Stack]}.



%% ------------------------------------------------------
%% BBs
set_current_bb_index(BB, ES) ->
    ES#{ current_bb => BB }.

current_bb(#{ current_bb := BB} = ES) ->
    get_bb(BB, ES).

get_bb(BB, #{bbs := BBS} = ES) ->
    case maps:get(BB, BBS, void) of
        void -> abort({trying_to_reach_bb, BB}, ES);
        Instructions -> Instructions
    end.

next_bb_index(#{ current_bb := BB}) ->
    %% TODO check BB + 1 exists.
    BB + 1.


%% ------------------------------------------------------
%% Call stack

push_return_address(#{ current_bb := BB
                     , current_function := Function
                     , current_contract := Contract
                     , call_stack := Stack
                     , memory := Mem} = ES) ->
    ES#{ call_stack => [{Contract, Function, BB+1, Mem}|Stack]}.

pop_call_stack(#{ call_stack := []} = ES) -> {stop, ES};
pop_call_stack(#{ call_stack := [{Contract, Function, BB, Mem}| Rest]
                , current_contract := CurrentContract
                , current_function := CurrentFunction} = ES) ->
    ES1 = ES#{ call_stack => Rest,
               %% The memory is functional and restored
               %% to the version before the call.
               memory => Mem},
    if CurrentContract =:= Contract -> %% Local return
            if CurrentFunction =:= Function -> % self return
                    {jump, BB, ES1};
               true -> %% Other function same contract
                    ES2 = set_current_function(Function, ES1),
                    {jump, BB, ES2}
            end;
       true -> %% Non local return
            ES2 = set_function(Contract, Function, ES1),
            {jump, BB, ES2}
    end.

%% ------------------------------------------------------
%% Memory
new_env(Mem, #{ memory := Envs} = EngineState) ->
    EngineState#{ memory => [Mem|Envs]}.

lookup_var(Var, [Env|Envs], ES) ->
    case maps:get(Var, Env, undefined) of
        {Value, _Type} ->
            Value;
        undefined ->
            lookup_var(Var, Envs, ES)
    end;
lookup_var(Var, [], ES) ->
    abort({undefined_var, Var}, ES).

store_var(Var, Val, [Env|Envs]) ->
    T = type(Val),
    [Env#{ Var => {Val, T}} | Envs].

%% ------------------------------------------------------
%% New state


new_engine_state(Chain) ->
    #{ current_bb => 0
     , bbs => #{}
     , memory => [] %% Stack of environments (name => val)
     , chain => Chain
     , trace => []
     , accumulator => ?FATE_VOID
     , accumulator_stack => []
     , functions => #{} %% Cache for current contract.
     , contracts => #{} %% Cache for loaded contracts.
     , current_contract => ?FATE_VOID
     , current_function => ?FATE_VOID
     , call_stack => []
     }.

%% ----------------------------

%% TODO: real chain interface
chain_get_contract(ContractAddress, #{ contracts :=  Contracts} = _Chain) ->
    case maps:get(ContractAddress, Contracts, void) of
        void -> throw({error, calling, ContractAddress});
        C -> C
    end.


