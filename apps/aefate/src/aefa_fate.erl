-module(aefa_fate).
-export([run/2]).

-export([get_trace/1]).

-include("aefa_data.hrl").

-ifdef(TEST).
-define(trace(I,S), S#{trace => [{I, erlang:process_info(self(), reductions)} |get_trace(S)]}).
-else.
-define(trace(I,S), S).
-endif.

run(What, Chain) ->
    EngineState = setup_engine(What, Chain),
    execute(EngineState).

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
    case eval(I, ES) of
        {next, NewState} -> step(Is, NewState);
        {jump,_BB,_NewState} = Res -> Res;
        {stop, _NewState} = Res -> Res

    end.

%% ------------------------------------------------------
%% Call/return instructions
%% ------------------------------------------------------
eval(return, EngineState) ->
    ES = check_return_type(EngineState),
    pop_call_stack(ES);
eval({return_r, Name}, EngineState) ->
    ES1 = un_op(get, {{stack, 0}, Name}, EngineState),
    ES2 = check_return_type(ES1),
    pop_call_stack(ES2);
eval({call_local, Function}, EngineState) ->
    Signature = get_function_signature(Function, EngineState),
    {ok, ES2} = check_signature_and_bind_args(Signature, EngineState),
    ES3 = push_return_address(ES2),
    {jump, 0,  set_current_function(Function, ES3)};
eval({tailcall_local, Function}, EngineState) ->
    Signature = get_function_signature(Function, EngineState),
    {ok, ES2} = check_signature_and_bind_args(Signature, EngineState),
    {jump, 0,  set_current_function(Function, ES2)};
eval({call_remote, Contract, Function}, EngineState) ->
    ES1 = push_return_address(EngineState),
    ES2 = set_function(Contract, Function, ES1),
    Signature = get_function_signature(Function, ES2),
    {ok, ES3} = check_signature_and_bind_args(Signature, ES2),
    {jump, 0, ES3};
eval({tailcall_remote, Contract, Function}, EngineState) ->
    ES2 = set_function(Contract, Function, EngineState),
    Signature = get_function_signature(Function, ES2),
    {ok, ES3} = check_signature_and_bind_args(Signature, ES2),
    {jump, 0, ES3};

%% ------------------------------------------------------
%% Control flow instructions
%% ------------------------------------------------------
eval({jump, BB}, EngineState) ->
    {jump, BB, EngineState};

eval({jumpif_a, BB}, EngineState) ->
    case is_true_a(EngineState) of
        {true, ES1} -> {jump, BB, ES1};
        {false, ES1} -> {next, ES1}
    end;

%% ------------------------------------------------------
%% Integer instructions
%% ------------------------------------------------------
eval(push_a_0, EngineState) ->
    {next, push_int(?MAKE_FATE_INTEGER(0), EngineState)};
eval({push, Name}, EngineState) ->
    {next, un_op(get, {{stack, 0}, Name}, EngineState)};

eval(inc_a_1_a, EngineState) ->
    {next, inc_acc(EngineState)};
eval({inc_a_1_r, Name}, EngineState) ->
    {next, un_op(inc, {{stack, 0}, Name}, EngineState)};

eval({add, Dest, Left, Right}, EngineState) ->
    {next, bin_op(add, {Dest, Left, Right}, EngineState)};

eval({sub, Dest, Left, Right}, EngineState) ->
    {next, bin_op(sub, {Dest, Left, Right}, EngineState)};

eval({mul, Dest, Left, Right}, EngineState) ->
    {next, bin_op(mul, {Dest, Left, Right}, EngineState)};

eval({'div', Dest, Left, Right}, EngineState) ->
    {next, bin_op('div', {Dest, Left, Right}, EngineState)};

eval({mod, Dest, Left, Right}, EngineState) ->
    {next, bin_op(mod, {Dest, Left, Right}, EngineState)};

eval({pow, Dest, Left, Right}, EngineState) ->
    {next, bin_op(pow, {Dest, Left, Right}, EngineState)};




%% ------------------------------------------------------
%% Comparison instructions
%% ------------------------------------------------------
eval(lt_a_a_a, EngineState) ->
    {next, bin_comp(lt, {{stack, 0}, {stack, 0}, {stack, 0}}, EngineState)};
eval({lt_a_r_r, Left, Right}, EngineState) ->
    {next, bin_comp(lt, {{stack, 0}, Left, Right}, EngineState)};
eval({lt_a_a_r, Right}, EngineState) ->
    {next, bin_comp(lt, {{stack, 0}, {stack, 0}, Right}, EngineState)};
eval({lt_a_r_a, Left}, EngineState) ->
    {next, bin_comp(lt, {{stack, 0}, Left, {stack, 0}}, EngineState)};

eval(gt_a_a_a, EngineState) ->
    {next, bin_comp(gt, {{stack, 0}, {stack, 0}, {stack, 0}}, EngineState)};
eval({gt_a_r_r, Left, Right}, EngineState) ->
    {next, bin_comp(gt, {{stack, 0}, Left, Right}, EngineState)};
eval({gt_a_a_r, Right}, EngineState) ->
    {next, bin_comp(gt, {{stack, 0}, {stack, 0}, Right}, EngineState)};
eval({gt_a_r_a, Left}, EngineState) ->
    {next, bin_comp(gt, {{stack, 0}, Left, {stack, 0}}, EngineState)};

eval(elt_a_a_a, EngineState) ->
    {next, bin_comp(elt, {{stack, 0}, {stack, 0}, {stack, 0}}, EngineState)};
eval({elt_a_r_r, Left, Right}, EngineState) ->
    {next, bin_comp(elt, {{stack, 0}, Left, Right}, EngineState)};
eval({elt_a_a_r, Right}, EngineState) ->
    {next, bin_comp(elt, {{stack, 0}, {stack, 0}, Right}, EngineState)};
eval({elt_a_r_a, Left}, EngineState) ->
    {next, bin_comp(elt, {{stack, 0}, Left, {stack, 0}}, EngineState)};

eval(egt_a_a_a, EngineState) ->
    {next, bin_comp(egt, {{stack, 0}, {stack, 0}, {stack, 0}}, EngineState)};
eval({egt_a_r_r, Left, Right}, EngineState) ->
    {next, bin_comp(egt, {{stack, 0}, Left, Right}, EngineState)};
eval({egt_a_a_r, Right}, EngineState) ->
    {next, bin_comp(egt, {{stack, 0}, {stack, 0}, Right}, EngineState)};
eval({egt_a_r_a, Left}, EngineState) ->
    {next, bin_comp(egt, {{stack, 0}, Left, {stack, 0}}, EngineState)};

eval(eq_a_a_a, EngineState) ->
    {next, bin_comp(eq, {{stack, 0}, {stack, 0}, {stack, 0}}, EngineState)};
eval({eq_a_r_r, Left, Right}, EngineState) ->
    {next, bin_comp(eq, {{stack, 0}, Left, Right}, EngineState)};
eval({eq_a_a_r, Right}, EngineState) ->
    {next, bin_comp(eq, {{stack, 0}, {stack, 0}, Right}, EngineState)};
eval({eq_a_r_a, Left}, EngineState) ->
    {next, bin_comp(eq, {{stack, 0}, Left, {stack, 0}}, EngineState)};

eval(neq_a_a_a, EngineState) ->
    {next, bin_comp(neq, {{stack, 0}, {stack, 0}, {stack, 0}}, EngineState)};
eval({neq_a_r_r, Left, Right}, EngineState) ->
    {next, bin_comp(neq, {{stack, 0}, Left, Right}, EngineState)};
eval({neq_a_a_r, Right}, EngineState) ->
    {next, bin_comp(lt, {{stack, 0}, {stack, 0}, Right}, EngineState)};
eval({neq_a_r_a, Left}, EngineState) ->
    {next, bin_comp(neq, {{stack, 0}, Left, {stack, 0}}, EngineState)};



%% ------------------------------------------------------
%% Boolean instructions
%% ------------------------------------------------------
eval(push_a_true, EngineState) ->
    {next, push_bool(?FATE_TRUE, EngineState)};
eval(push_a_false, EngineState) ->
    {next, push_bool(?FATE_FALSE, EngineState)};
eval(and_a_a_a, EngineState) ->
    {next, and_aaa(EngineState)};
eval({and_a_r_r, Left, Right}, EngineState) ->
    {next, bin_op('and', {{stack, 0}, Left, Right}, EngineState)};
eval(or_a_a_a, EngineState) ->
    {next, or_aaa(EngineState)};
eval({or_a_r_r, Left, Right}, EngineState) ->
    {next, bin_op('or', {{stack, 0}, Left, Right}, EngineState)};
eval(not_a_a, EngineState) ->
    {next, not_aa(EngineState)};
eval({not_a_r, Name}, EngineState) ->
    {next, un_op('not', {{stack, 0}, Name}, EngineState)};


%% ------------------------------------------------------
%% Tuple instructions
%% ------------------------------------------------------
%% Make tuple only takes a fixed size.
%% NOTE: There is no type checking on the arguments on the stack.
eval({make_tuple, Size}, EngineState) ->
    if is_integer(Size) andalso (Size >= 0) ->
            {next, make_tuple(Size, EngineState)};
       true -> throw({error, {invalid_tuple_size, Size}})
    end;
%% (get) Element takes a type and two named arguments and stores
%% the element at position 'which' of the 'Tuple' in 'Dest'.
eval({element, Type, Dest, Which, Tuple}, EngineState) ->
    {next, tuple_element(Type, Dest, Which, Tuple, EngineState)};

%% ------------------------------------------------------
%% Map instructions
%% ------------------------------------------------------
%% Todo type checking?
eval({map_empty, Dest}, EngineState) ->
    {next, un_op(get, {Dest, {immediate, aefa_data:make_map(#{})}}, EngineState)};
eval({map_lookup, Dest, Map, Key}, EngineState) ->
    {next, bin_op(map_lookup, {Dest, Map, Key}, EngineState)};
eval({map_lookup_default, Dest, Map, Key, Default}, EngineState) ->
    {next, ter_op(map_lookup_default, {Dest, Map, Key, Default}, EngineState)};
eval({map_update, Dest, Map, Key, Value}, EngineState) ->
    {next, ter_op(map_update, {Dest, Map, Key, Value}, EngineState)};
eval({map_member, Dest, Map, Key}, EngineState) ->
    {next, bin_op(map_member, {Dest, Map, Key}, EngineState)};

%% builtin_deps1({map_upd, Type})            -> [{map_get, Type}, map_put];
%% builtin_deps1({map_upd_default, Type})    -> [{map_lookup_default, Type}, map_put];
%% builtin_deps1(map_from_list)              -> [map_put];


%% ------------------------------------------------------
%% List instructions
%% ------------------------------------------------------
%% Todo type checking?
eval({nil, Dest}, EngineState) ->
    {next, un_op(get, {Dest, {immediate, aefa_data:make_list([])}}, EngineState)};
eval({cons, Dest, Hd, Tl}, EngineState) ->
    {next, bin_op(cons, {Dest, Hd, Tl}, EngineState)};
eval({hd, Dest, List}, EngineState) ->
    {next, un_op(hd, {Dest, List}, EngineState)};
eval({tl, Dest, List}, EngineState) ->
    {next, un_op(tl, {Dest, List}, EngineState)};
eval({length, Dest, List}, EngineState) ->
    {next, un_op(length, {Dest, List}, EngineState)};

%% ------------------------------------------------------
%% String instructions
%% ------------------------------------------------------
%% builtin_deps1(str_equal)                  -> [str_equal_p];
%% builtin_deps1(string_concat)              -> [string_concat_inner1, string_copy, string_shift_copy];
%% builtin_deps1(int_to_str)                 -> [{baseX_int, 10}];
%% builtin_deps1(addr_to_str)                -> [{baseX_int, 58}];
%% builtin_deps1({baseX_int, X})             -> [{baseX_int_pad, X}];
%% builtin_deps1({baseX_int_pad, X})         -> [{baseX_int_encode, X}];
%% builtin_deps1({baseX_int_encode, X})      -> [{baseX_int_encode_, X}, {baseX_tab, X}, {baseX_digits, X}];
%% builtin_deps1(string_reverse)             -> [string_reverse_];


%% ------------------------------------------------------
%% Bits instructions
%% ------------------------------------------------------
%% A bit field is represented by an integer.
%% Bit fields "starting" with Bits.all are represented by a
%% negative integer (with infinite set bits to the left.)
%% Take Bits.sum of an infinite set fails with an `arithmetic_error`.

%% Bits.none : bits
%% An empty bit set.
eval(bits_none, EngineState) -> {next, push(?FATE_BITS(0), EngineState)};
eval({bits_none, To}, EngineState) ->
    {next, un_op(get, {To, {immediate, ?FATE_BITS(0)}}, EngineState)};

%% Bits.all : bits
%% A bit field with all (an infinite amount) bits set
eval(bits_all, EngineState) -> {next, push(?FATE_BITS(-1), EngineState)};
eval({bits_all, To}, EngineState) ->
    {next, un_op(get, {To, {immediate, ?FATE_BITS(-1)}}, EngineState)};

%% Bits.all_n : bits
%% A bit field with n bits set
eval({bits_all_n, To, N}, EngineState) ->
    {next, un_op(bits_all, {To, N}, EngineState)};

%% Bits.set(b : bits, i : int) : bits
%% Set bit i
eval({bits_set, To, Bits, Bit}, EngineState) ->
    {next, bin_op(bits_set, {To, Bits, Bit}, EngineState)};

%% Bits.clear(b : bits, i : int) : bits
%% Clear bit i
eval({bits_clear, To, Bits, Bit}, EngineState) ->
    {next, bin_op(bits_clear, {To, Bits, Bit}, EngineState)};

%% Bits.test(b : bits, i : int) : bool
%% Check if bit i is set
eval({bits_test, To, Bits, Bit}, EngineState) ->
    {next, bin_op(bits_test, {To, Bits, Bit}, EngineState)};

%% Bits.sum(b : bits) : int
%% Count the number of set bits.
%% Throws an exception for infinite bit sets (starting from Bits.all)
eval({bits_sum, To, Bits}, EngineState) ->
    {next, un_op(bits_sum, {To, Bits}, EngineState)};

%% Bits.union(a : bits, b : bits) : bits
%% For all i:
%%   Bits.test(Bits.union(a, b), i) == (Bits.test(a, i) || Bits.test(b, i))
eval({bits_union, To, Bits, Bit}, EngineState) ->
    {next, bin_op(bits_union, {To, Bits, Bit}, EngineState)};

%% Bits.intersection(a : bits, b : bits) : bits
%% For all i:
%% Bits.test(Bits.intersection(a, b), i) == (Bits.test(a, i) && Bits.test(b, i))
eval({bits_intersection, To, Bits, Bit}, EngineState) ->
    {next, bin_op(bits_intersection, {To, Bits, Bit}, EngineState)};

%% Bits.difference(a : bits, b : bits) : bits
%% For all i:
%%  Bits.test(Bits.difference(a, b), i) == (Bits.test(a, i) && !Bits.test(b, i))
eval({bits_difference, To, Bits, Bit}, EngineState) ->
    {next, bin_op(bits_difference, {To, Bits, Bit}, EngineState)};



%% ------------------------------------------------------
%% Stack instructions
%% ------------------------------------------------------
eval(dup, EngineState) ->
    {next, dup(EngineState)};

%% ------------------------------------------------------
%% Memory instructions
%% ------------------------------------------------------
eval({store, Var, What}, EngineState) ->
    {next, un_op(get, {Var, What}, EngineState)};



%% ------------------------------------------------------
%% Other Instructions
%% ------------------------------------------------------
eval(nop, EngineState) ->
    {next, EngineState}.




%% -----------------------------------------------------------



setup_engine(#{ contract := Contract
              , function := Function
              , arguments := SerializedArguments}, Chain) ->
    Arguments = aefa_encoding:deserialize(SerializedArguments),
    ES1 = new_engine_state(Chain),
    ES2 = set_function(Contract, Function, ES1),
    ES3 = push_arguments(Arguments, ES2),
    Signature = get_function_signature(Function, ES3),
    {ok, ES4} = check_signature_and_bind_args(Signature, ES3),
    ES4.



set_function(Contract, Function, #{ chain := Chain
                                  , contracts := Contracts} = ES) ->
    {ES2, Code} =
        case maps:get(Contract, Contracts, void) of
            void ->
                ContractCode = chain_get_contract(Contract, Chain),
                {ES#{contracts => maps:put(Contract, ContractCode, Contracts)},
                 ContractCode};
            ContractCode ->
                {ES, ContractCode}
        end,
    ES3 = ES2#{current_contract => Contract},
    ES4 = setup_functions(Code, ES3),
    set_current_function(Function, ES4).

%get_current_contract(#{current_contract := Contract}) ->
%    Contract.

setup_functions(ContractCode, ES) ->
    lists:foldl(
      fun({FunctionName, Signature, BBs}, State) ->
              set_function_code(FunctionName, Signature, BBs, State)
      end,
      ES,
      ContractCode).

set_current_function(Function, ES) ->
    ES1 = ES#{current_function => Function
             , current_bb => 0},
    BBs = get_function_code(Function, ES1),
    lists:foldl(
      fun({BB, Instructions}, State) ->
              set_instructions(BB, Instructions, State)
      end,
      ES1,
      BBs).

set_instructions(BB, Is, #{bbs := BBs} = ES) ->
    NewBBs = maps:put(BB, Is, BBs),
    maps:put(bbs, NewBBs, ES).

set_function_code(Name, Signature, BBs, #{functions := Functions} = ES) ->
    NewFunctions = maps:put(Name, {Signature, BBs}, Functions),
    maps:put(functions, NewFunctions, ES).

get_function_code(Name, #{functions := Functions}) ->
    case maps:get(Name, Functions, void) of
        void ->  throw({error, {trying_to_call_function, Name}});
        {_Signature, Code} -> Code
    end.

get_function_signature(Name,  #{functions := Functions}) ->
    case maps:get(Name, Functions, void) of
        void ->  throw({error, {trying_to_call_function, Name}});
        {Signature, _Code} -> Signature
    end.

check_return_type(#{current_function := Function,
                    accumulator := Acc} = ES) ->
    {_ArgTypes, RetSignature} = get_function_signature(Function, ES),
    case check_type(RetSignature, Acc) of
        true -> ES;
        false -> throw({error, {bad_return_type, Acc, RetSignature}})
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
            throw({error, {value_does_not_match_type, V, T}})
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
check_type(integer, I) when ?IS_FATE_INTEGER(I) -> true;
check_type(boolean, B) when ?IS_FATE_BOOLEAN(B) -> true;
check_type(bits, B) when ?IS_FATE_BITS(B) -> true;
check_type({list, ET}, L) when ?IS_FATE_LIST(L) ->
    check_same_type(ET, ?FATE_LIST_VALUE(L));
check_type({tuple, Elements}, T) when ?IS_FATE_TUPLE(T) ->
    check_all_types(Elements, aefa_data:tuple_to_list(T));
check_type({map, Key, Value}, M) when ?IS_FATE_MAP(M) ->
    {Ks, Vs} = lists:unzip(maps:to_list(?FATE_MAP_VALUE(M))),
    check_same_type(Key, Ks) andalso
    check_same_type(Value, Vs);
check_type(_T, _V) -> false.

type(I) when ?IS_FATE_INTEGER(I) -> integer;
type(B) when ?IS_FATE_BOOLEAN(B) ->  boolean;
type(B) when ?IS_FATE_BITS(B) ->  bits;
type([E|L]) when ?IS_FATE_LIST(L) -> {list, type(E)}.
%% TODO: handle all types.

jump(BB, ES) ->
    NewES = set_current_bb_index(BB, ES),
    Instructions = current_bb(NewES),
    {Instructions, NewES}.

%% ------------------------------------------------------
%% Integer instructions
%% ------------------------------------------------------

un_op(Op, {To, What}, ES) ->
    {Value, ES1} = get_op_arg(What, ES),
    Result = op(Op, Value),
    store(To, Result, ES1).

bin_op(Op, {To, Left, Right}, ES) ->
    {LeftValue, ES1} = get_op_arg(Left, ES),
    {RightValue, ES2} = get_op_arg(Right, ES1),
    Result = op(Op, LeftValue, RightValue),
    store(To, Result, ES2).

ter_op(Op, {To, One, Two, Three}, ES) ->
    {ValueOne, ES1} = get_op_arg(One, ES),
    {ValueTwo, ES2} = get_op_arg(Two, ES1),
    {ValueThree, ES3} = get_op_arg(Three, ES2),
    Result = op(Op, ValueOne, ValueTwo, ValueThree),
    store(To, Result, ES3).

get_op_arg({stack, 0}, #{ accumulator := A
                        , accumulator_stack := [S|Stack] } = ES) ->
    {A, ES#{accumulator => S, accumulator_stack => Stack}};
get_op_arg({stack, 0}, #{ accumulator := A
                        , accumulator_stack := [] } = ES) ->
    {A, ES#{accumulator := ?FATE_VOID}};
get_op_arg({arg,_N} = Var, #{ memory := Mem } = ES) ->
    Value = lookup_var(Var, Mem),
    {Value, ES};
get_op_arg({var,_N} = Var, #{ memory := Mem } = ES) ->
    Value = lookup_var(Var, Mem),
    {Value, ES};
get_op_arg({immediate, X}, ES) -> {X, ES}.

store({stack, 0}, Val, #{ accumulator := ?FATE_VOID} = ES) ->
    ES#{accumulator := Val};
store({stack, 0}, Val, #{ accumulator := A,
                           accumulator_stack := Stack} = ES) ->
             ES#{accumulator => Val,
                 accumulator_stack => [A | Stack]};
store({var, _} = Name,  Val, #{ memory := Mem } = ES) ->
    NewMem = store_var(Name, Val, Mem),
    ES#{ memory => NewMem};
store({arg, N}, _, _ES) ->
     throw({error, {cannot_write_to_arg, N}}).




push_int(I, ES) when ?IS_FATE_INTEGER(I) -> push(I, ES).




make_tuple(Size, ES) ->
    {Elements, ES2} = pop_n(Size, ES),
    Tuple = list_to_tuple(Elements),
    FateTuple = aefa_data:make_tuple(Tuple),
    push(FateTuple, ES2).


tuple_element(Type, To, Which, TupleArg, ES) ->
    {Index, ES1} = get_op_arg(Which, ES),
    {FateTuple, ES2} = get_op_arg(TupleArg, ES1),
    case check_type(integer, Index) andalso ?IS_FATE_TUPLE(FateTuple) of
        false -> throw({error, {bad_arguments_to_element, Index, FateTuple}});
        true ->
            ?FATE_TUPLE(Tuple) = FateTuple,
            case size(Tuple) > Index of
                true ->
                    V = element(Index+1, Tuple),
                    case check_type(Type, V) of
                        true -> store(To, V, ES2);
                        false -> throw({error, {bad_element_type, Type, V}})
                    end;
                false ->
                    throw({error, {element_index_out_of_bounds, Index}})
            end
    end.

%% Unary operations
op(get, A) ->
    A;
op(inc, A) ->
    A + 1;
op('not', A) ->
    not A;
op(hd, A) when ?IS_FATE_LIST(A) ->
    case ?FATE_LIST_VALUE(A) of
        [] -> throw({error, hd_on_empty_list});
        [Hd|_] -> Hd
    end;
op(tl, A) when ?IS_FATE_LIST(A) ->
    case ?FATE_LIST_VALUE(A) of
        [] -> throw({error, hd_on_empty_list});
        [_|Tl] -> Tl
    end;
op(length, A) when ?IS_FATE_LIST(A) ->
    aefa_data:make_integer(length(?FATE_LIST_VALUE(A)));
op(bits_all, N)  when ?IS_FATE_INTEGER(N) ->
    ?FATE_BITS((1 bsl (N)) - 1);
op(bits_sum, A)  when ?IS_FATE_BITS(A) ->
    ?FATE_BITS(Bits) = A,
    if Bits < 0 -> throw({error, {arithmetic_error, bits_sum_on_ifinite_set}});
       true -> bits_sum(Bits, 0)
    end.

%% Binary operations
op(add, A, B)  when ?IS_FATE_INTEGER(A)
                    , ?IS_FATE_INTEGER(B) ->
    A + B;
op(sub, A, B)  when ?IS_FATE_INTEGER(A)
                    , ?IS_FATE_INTEGER(B) ->
    A - B;
op(mul, A, B)  when ?IS_FATE_INTEGER(A)
                    , ?IS_FATE_INTEGER(B) ->
    A * B;
op('div', A, B)  when ?IS_FATE_INTEGER(A)
                    , ?IS_FATE_INTEGER(B) ->
    if B =:= 0 -> throw(division_by_zero);
       true -> A div B
    end;
op(pow, A, B)  when ?IS_FATE_INTEGER(A)
                    , ?IS_FATE_INTEGER(B) ->
    math:pow(A, B);
op(mod, A, B)  when ?IS_FATE_INTEGER(A)
                    , ?IS_FATE_INTEGER(B) ->
    if B =:= 0 -> throw(mod_by_zero);
       true -> A rem B
    end;
op('and', A, B)  when ?IS_FATE_BOOLEAN(A)
                    , ?IS_FATE_BOOLEAN(B) ->
    A and B;
op('or', A, B)  when ?IS_FATE_BOOLEAN(A)
                    , ?IS_FATE_BOOLEAN(B) ->
    A or B;
op(map_lookup, Map, Key) when ?IS_FATE_MAP(Map),
                              not ?IS_FATE_MAP(Key) ->
    case maps:get(Key, ?FATE_MAP_VALUE(Map), void) of
        void -> throw(missing_map_key);
        Res -> Res
    end;
op(map_member, Map, Key) when ?IS_FATE_MAP(Map),
                              not ?IS_FATE_MAP(Key) ->
    aefa_data:make_boolean(maps:is_key(Key, ?FATE_MAP_VALUE(Map)));
op(cons, Hd, Tail) when ?IS_FATE_LIST(Tail) ->
    %% TODO: Check type of Hd and tail.
    aefa_data:make_list([Hd|?FATE_LIST_VALUE(Tail)]);

op(bits_set, A, B)  when ?IS_FATE_BITS(A)
                         , ?IS_FATE_INTEGER(B)
                         , B >= 0 ->
    ?FATE_BITS(Bits) = A,
    ?FATE_BITS(Bits bor (1 bsl B));
op(bits_clear, A, B)  when ?IS_FATE_BITS(A)
                         , ?IS_FATE_INTEGER(B)
                         , B >= 0 ->
    ?FATE_BITS(Bits) = A,
    ?FATE_BITS(Bits band (bnot (1 bsl B)));
op(bits_test, A, B)  when ?IS_FATE_BITS(A)
                         , ?IS_FATE_INTEGER(B)
                         , B >= 0 ->
    ?FATE_BITS(Bits) = A,
    ((Bits band (1 bsl B)) > 0);
op(bits_union, A, B)
  when ?IS_FATE_BITS(A), ?IS_FATE_BITS(B) ->
    ?FATE_BITS(BitsA) = A,
    ?FATE_BITS(BitsB) = B,
    ?FATE_BITS(BitsA bor BitsB);
op(bits_intersection, A, B)
  when ?IS_FATE_BITS(A), ?IS_FATE_BITS(B) ->
    ?FATE_BITS(BitsA) = A,
    ?FATE_BITS(BitsB) = B,
    ?FATE_BITS(BitsA band BitsB);
op(bits_difference, A, B)
  when ?IS_FATE_BITS(A), ?IS_FATE_BITS(B) ->
    ?FATE_BITS(BitsA) = A,
    ?FATE_BITS(BitsB) = B,
    ?FATE_BITS((BitsA band BitsB) bxor BitsA).

%% Terinay operations
op(map_lookup_default, Map, Key, Default) when ?IS_FATE_MAP(Map),
                                               not ?IS_FATE_MAP(Key) ->
    maps:get(Key, ?FATE_MAP_VALUE(Map), Default);
op(map_update, Map, Key, Value) when ?IS_FATE_MAP(Map),
                              not ?IS_FATE_MAP(Key) ->
    Res = maps:put(Key, Value, ?FATE_MAP_VALUE(Map)),
    aefa_data:make_map(Res).




bits_sum(0, Sum) -> Sum;
bits_sum(N, Sum) -> bits_sum(N bsr 1, Sum + (N band 2#1)).


inc_acc(#{accumulator := X} = ES) when ?IS_FATE_INTEGER(X) ->
    ES#{accumulator := ?MAKE_FATE_INTEGER(?FATE_INTEGER_VALUE(X)+1)}.

%% ------------------------------------------------------
%% Comparison instructions
%% ------------------------------------------------------

bin_comp(Comp, {To, Left, Right}, ES) ->
    {LeftValue, ES1} = get_op_arg(Left, ES),
    {RightValue, ES2} = get_op_arg(Right, ES1),
    Result = comp(Comp, LeftValue, RightValue),
    store(To, Result, ES2).

comp( lt, A, B) -> A < B;
comp( gt, A, B) -> A > B;
comp(elt, A, B) -> A =< B;
comp(egt, A, B) -> A >= B;
comp( eq, A, B) -> A =:= B;
comp(neq, A, B) -> A =/= B.


%% ------------------------------------------------------
%% Boolean instructions
%% ------------------------------------------------------
push_bool(B,
         #{ accumulator := A
          , accumulator_stack := Stack } = ES) when ?IS_FATE_BOOLEAN(B) ->
    ES#{ accumulator => B
       , accumulator_stack => [A|Stack]}.

and_aaa(#{ accumulator := A
          , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_BOOLEAN(A)
                                                       , ?IS_FATE_BOOLEAN(B) ->
    ES#{ accumulator => (A and B)
       , accumulator_stack => Stack}.

or_aaa(#{ accumulator := A
          , accumulator_stack := [B|Stack]} = ES) when ?IS_FATE_BOOLEAN(A)
                                                       , ?IS_FATE_BOOLEAN(B) ->
    ES#{ accumulator => (A or B)
       , accumulator_stack => Stack}.

not_aa(#{ accumulator := A} = ES) when ?IS_FATE_BOOLEAN(A) ->
    ES#{ accumulator => (not A)}.

is_true_a(#{ accumulator := A} = ES) when ?IS_FATE_BOOLEAN(A) ->
    pop(ES).


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

get_bb(BB, #{bbs := BBS}) ->
    case maps:get(BB, BBS, void) of
        void -> throw({error, {trying_to_reach_bb, BB}});
        Instrucitions -> Instrucitions
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

get_trace(#{trace := T}) -> T.

%% ------------------------------------------------------
%% Memory
new_env(Mem, #{ memory := Envs} = EngineState) ->
    EngineState#{ memory => [Mem|Envs]}.

lookup_var(Var, [Env|Envs]) ->
    case maps:get(Var, Env, undefined) of
        {Value, _Type} ->
            Value;
        undefined ->
            lookup_var(Var, Envs)
    end;
lookup_var(Var, []) ->
    throw({error, {undefined_var, Var}}).

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

