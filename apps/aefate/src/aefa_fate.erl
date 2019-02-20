-module(aefa_fate).
-export([run/2]).

-export([get_trace/1]).

-include_lib("aebytecode/include/aeb_fate_data.hrl").

-ifdef(TEST).
-define(trace(I,S), S#{trace => [{I, erlang:process_info(self(), reductions)} |get_trace(S)]}).
-else.
-define(trace(I,S), S).
-endif.

run(What, Chain) ->
    EngineState = setup_engine(What, Chain),
    try execute(EngineState) of
        Res -> Res
    catch
        throw:{E, ES} -> throw({E, ES})
    end.


-define(t(__S,__A,__ES), throw({iolist_to_binary(io_lib:format(__S, __A)), __ES})).

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
abort({bad_variant_tag, Tag, Size}, ES) ->
    ?t("Type error in switch: tag ~p is larger than ~p", [Tag, Size], ES);
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
    case eval(I, ES) of
        {next, NewState} -> step(Is, NewState);
        {jump,_BB,_NewState} = Res -> Res;
        {stop, _NewState} = Res -> Res

    end.

%% ------------------------------------------------------
%% Call/return instructions
%% ------------------------------------------------------
eval('RETURN', EngineState) ->
    ES = check_return_type(EngineState),
    pop_call_stack(ES);
eval({'RETURNR', Name}, EngineState) ->
    ES1 = un_op(get, {{stack, 0}, Name}, EngineState),
    ES2 = check_return_type(ES1),
    pop_call_stack(ES2);
eval({'CALL', {immediate, Function}}, EngineState) ->
    Signature = get_function_signature(Function, EngineState),
    {ok, ES2} = check_signature_and_bind_args(Signature, EngineState),
    ES3 = push_return_address(ES2),
    {jump, 0,  set_current_function(Function, ES3)};
eval({'CALL_T', {immediate, Function}}, EngineState) ->
    Signature = get_function_signature(Function, EngineState),
    {ok, ES2} = check_signature_and_bind_args(Signature, EngineState),
    {jump, 0,  set_current_function(Function, ES2)};
eval({'CALL_R', Contract, {immediate, Function}}, EngineState) ->
    ES1 = push_return_address(EngineState),
    {Address, ES2} = get_op_arg(Contract, ES1),
    ES3 = set_function(Address, Function, ES2),
    Signature = get_function_signature(Function, ES3),
    {ok, ES4} = check_signature_and_bind_args(Signature, ES3),
    {jump, 0, ES4};
eval({'CALL_TR', Contract, {immediate, Function}}, EngineState) ->
    {Address, ES1} = get_op_arg(Contract, EngineState),
    ES2 = set_function(Address, Function, ES1),
    Signature = get_function_signature(Function, ES2),
    {ok, ES3} = check_signature_and_bind_args(Signature, ES2),
    {jump, 0, ES3};

%% ------------------------------------------------------
%% Control flow instructions
%% ------------------------------------------------------
eval({'JUMP', {immediate, BB}}, EngineState) ->
    {jump, BB, EngineState};

eval({'JUMPIF', Arg, {immediate, BB}}, EngineState) ->
    {Value, ES1} = get_op_arg(Arg, EngineState),
    case Value of
        true -> {jump, BB, ES1};
        false -> {next, ES1}
    end;

eval({'SWITCH_V2', Variant, {immediate, BB1}, {immediate, BB2}}, ES) ->
    {Value, ES1} = get_op_arg(Variant, ES),
    if ?IS_FATE_VARIANT(Value) ->
            ?FATE_VARIANT(Size, Tag, _T) = Value,
            if Size =:= 2 ->
                    case Tag of
                        0 -> {jump, BB1, ES1};
                        1 -> {jump, BB2, ES1};
                        _ -> abort({bad_variant_tag, Tag, Size}, ES1)
                    end;
               true -> abort({bad_variant_size, Size}, ES1)
            end;
       true -> abort({value_does_not_match_type,Value, variant}, ES1)
    end;
eval({'SWITCH_V3', Variant, {immediate, BB1}, {immediate, BB2}, {immediate, BB3}}, ES) ->
    {Value, ES1} = get_op_arg(Variant, ES),
    if ?IS_FATE_VARIANT(Value) ->
            ?FATE_VARIANT(Size, Tag, _T) = Value,
            if Size =:= 3 ->
                    case Tag of
                        0 -> {jump, BB1, ES1};
                        1 -> {jump, BB2, ES1};
                        2 -> {jump, BB3, ES1};
                        _ -> abort({bad_variant_tag, Tag, Size}, ES1)
                    end;
               true -> abort({bad_variant_size, Size}, ES1)
            end;
       true -> abort({value_does_not_match_type,Value, variant}, ES1)
    end;
eval({'SWITCH_V4', Variant, {immediate, BB1},
      {immediate, BB2}, {immediate, BB3}, {immediate, BB4}}
    , ES) ->
    {Value, ES1} = get_op_arg(Variant, ES),
    if ?IS_FATE_VARIANT(Value) ->
            ?FATE_VARIANT(Size, Tag, _T) = Value,
            if Size =:= 4 ->
                    case Tag of
                        0 -> {jump, BB1, ES1};
                        1 -> {jump, BB2, ES1};
                        2 -> {jump, BB3, ES1};
                        3 -> {jump, BB4, ES1};
                        _ -> abort({bad_variant_tag, Tag, Size}, ES1)
                    end;
               true -> abort({bad_variant_size, Size}, ES1)
            end;
       true -> abort({value_does_not_match_type, Value, variant}, ES1)
    end;
eval({'SWITCH_V5', Variant, {immediate, BB1},
      {immediate, BB2}, {immediate, BB3}, {immediate, BB4}, {immediate, BB5}}
    , ES) ->
    {Value, ES1} = get_op_arg(Variant, ES),
    if ?IS_FATE_VARIANT(Value) ->
            ?FATE_VARIANT(Size, Tag, _T) = Value,
            if Size =:= 5 ->
                    case Tag of
                        0 -> {jump, BB1, ES1};
                        1 -> {jump, BB2, ES1};
                        2 -> {jump, BB3, ES1};
                        3 -> {jump, BB4, ES1};
                        4 -> {jump, BB5, ES1};
                        _ -> abort({bad_variant_tag, Tag, Size}, ES1)
                    end;
               true -> abort({bad_variant_size, Size}, ES1)
            end;
       true -> abort({value_does_not_match_type,Value, variant}, ES1)
    end;
%% Note: More complex variants (size > 5) has to be handled by a
%%       series of 'variant_test' and 'jump_if' instructions.

%% ------------------------------------------------------
%% Integer instructions
%% ------------------------------------------------------
eval({'PUSH', Name}, EngineState) ->
    {next, un_op(get, {{stack, 0}, Name}, EngineState)};

eval('INCA', EngineState) ->
    {next, un_op(inc, {{stack, 0}, {stack, 0}}, EngineState)};

eval({'INC', Name}, EngineState) ->
    {next, un_op(inc, {Name, Name}, EngineState)};

eval({'DEC', Name}, EngineState) ->
    {next, un_op(dec, {Name, Name}, EngineState)};

eval('DECA', EngineState) ->
    {next, un_op(dec, {{stack, 0}, {stack, 0}}, EngineState)};

eval({'ADD', Dest, Left, Right}, EngineState) ->
    {next, bin_op(add, {Dest, Left, Right}, EngineState)};

eval({'SUB', Dest, Left, Right}, EngineState) ->
    {next, bin_op(sub, {Dest, Left, Right}, EngineState)};

eval({'MUL', Dest, Left, Right}, EngineState) ->
    {next, bin_op(mul, {Dest, Left, Right}, EngineState)};

eval({'DIV', Dest, Left, Right}, EngineState) ->
    {next, bin_op('div', {Dest, Left, Right}, EngineState)};

eval({'MOD', Dest, Left, Right}, EngineState) ->
    {next, bin_op(mod, {Dest, Left, Right}, EngineState)};

eval({'POW', Dest, Left, Right}, EngineState) ->
    {next, bin_op(pow, {Dest, Left, Right}, EngineState)};




%% ------------------------------------------------------
%% Comparison instructions
%% ------------------------------------------------------
eval({'LT', Dest,  Left, Right}, EngineState) ->
    {next, bin_comp(lt, {Dest, Left, Right}, EngineState)};
eval({'GT', Dest, Left, Right}, EngineState) ->
    {next, bin_comp(gt, {Dest, Left, Right}, EngineState)};
eval({'ELT', Dest, Left, Right}, EngineState) ->
    {next, bin_comp(elt, {Dest, Left, Right}, EngineState)};
eval({'EGT', Dest, Left, Right}, EngineState) ->
    {next, bin_comp(egt, {Dest, Left, Right}, EngineState)};
eval({'EQ', Dest, Left, Right}, EngineState) ->
    {next, bin_comp(eq, {Dest, Left, Right}, EngineState)};
eval({'NEQ', Dest, Left, Right}, EngineState) ->
    {next, bin_comp(neq, {Dest, Left, Right}, EngineState)};

%% ------------------------------------------------------
%% Boolean instructions
%% ------------------------------------------------------
eval({'AND', Dest, Left, Right}, EngineState) ->
    {next, bin_op('and', {Dest, Left, Right}, EngineState)};
eval({'OR', Dest, Left, Right}, EngineState) ->
    {next, bin_op('or', {Dest, Left, Right}, EngineState)};
eval({'NOT', Dest, Name}, EngineState) ->
    {next, un_op('not', {Dest, Name}, EngineState)};


%% ------------------------------------------------------
%% Tuple instructions
%% ------------------------------------------------------
%% Make tuple only takes a fixed size.
%% NOTE: There is no type checking on the arguments on the stack.
%% TODO: add type checking.
eval({'TUPLE', {immediate, Size}}, EngineState) ->
    if is_integer(Size) andalso (Size >= 0) ->
            {next, make_tuple(Size, EngineState)};
       true -> abort({invalid_tuple_size, Size}, EngineState)
    end;
%% (get) Element takes a type and two named arguments and stores
%% the element at position 'which' of the 'Tuple' in 'Dest'.
eval({'ELEMENT', Type, Dest, Which, Tuple}, EngineState) ->
    {next, tuple_element(Type, Dest, Which, Tuple, EngineState)};

%% ------------------------------------------------------
%% Map instructions
%% ------------------------------------------------------
%% Todo type checking?
eval({'MAP_EMPTY', Dest}, EngineState) ->
    {next, un_op(get, {Dest, {immediate, aeb_fate_data:make_map(#{})}}, EngineState)};
eval({'MAP_LOOKUP', Dest, Map, Key}, EngineState) ->
    {next, bin_op(map_lookup, {Dest, Map, Key}, EngineState)};
eval({'MAP_LOOKUPD', Dest, Map, Key, Default}, EngineState) ->
    {next, ter_op(map_lookup_default, {Dest, Map, Key, Default}, EngineState)};
eval({'MAP_UPDATE', Dest, Map, Key, Value}, EngineState) ->
    {next, ter_op(map_update, {Dest, Map, Key, Value}, EngineState)};
eval({'MAP_MEMBER', Dest, Map, Key}, EngineState) ->
    {next, bin_op(map_member, {Dest, Map, Key}, EngineState)};
eval({'MAP_FROM_LIST', Dest, List}, EngineState) ->
    {next, un_op(map_from_list, {Dest, List}, EngineState)};


%% ------------------------------------------------------
%% List instructions
%% ------------------------------------------------------
%% Todo type checking?
eval({'NIL', Dest}, EngineState) ->
    {next, un_op(get, {Dest, {immediate, aeb_fate_data:make_list([])}}, EngineState)};
eval({'IS_NIL', Dest, List}, EngineState) ->
    {next, un_op(is_nil, {Dest, List}, EngineState)};
eval({'CONS', Dest, Hd, Tl}, EngineState) ->
    {next, bin_op(cons, {Dest, Hd, Tl}, EngineState)};
eval({'HD', Dest, List}, EngineState) ->
    {next, un_op(hd, {Dest, List}, EngineState)};
eval({'TL', Dest, List}, EngineState) ->
    {next, un_op(tl, {Dest, List}, EngineState)};
eval({'LENGTH', Dest, List}, EngineState) ->
    {next, un_op(length, {Dest, List}, EngineState)};

%% ------------------------------------------------------
%% String instructions
%% ------------------------------------------------------
eval({'STR_EQ', Dest, Str1, Str2}, EngineState) ->
    {next, bin_op(str_equal, {Dest, Str1, Str2}, EngineState)};
eval({'STR_JOIN', Dest, Str1, Str2}, EngineState) ->
    {next, bin_op(str_join, {Dest, Str1, Str2}, EngineState)};
eval({'INT_TO_STR', Dest, Str}, EngineState) ->
    {next, un_op(int_to_str, {Dest, Str}, EngineState)};
eval({'ADDR_TO_STR', Dest, Str}, EngineState) ->
    {next, un_op(addr_to_str, {Dest, Str}, EngineState)};
eval({'STR_REVERSE', Dest, Str}, EngineState) ->
    {next, un_op(str_reverse, {Dest, Str}, EngineState)};


eval({'INT_TO_ADDR', Dest, Str}, EngineState) ->
    {next, un_op(int_to_addr, {Dest, Str}, EngineState)};

%% ------------------------------------------------------
%% Variant instructions
%% ------------------------------------------------------
%% A Variant type has a Size (number of different tags).
%% A Variant also has a tag.
%% A Variant has a tuple of values which size and types
%%   are decided by the tag.
%% Note: At the momement the types of the values are not
%%       specified.
%%       Also, tags are only numbers (in Sophia tags will
%%       correspond to names)
%% There are some variant_switch instructions for
%% variants of size 2 to 5.

%% TODO: should this also take a size?
eval({'VARIANT_TEST', Dest, Variant, Tag}, EngineState) ->
    {next, bin_op(variant_test, {Dest, Variant, Tag}, EngineState)};
%% TODO: type test? Size, Tag, and element type?
eval({'VARIANT_ELEMENT', Dest, Variant, Index}, EngineState) ->
    {next, bin_op(variant_element, {Dest, Variant, Index}, EngineState)};
eval({'VARIANT', Dest, SizeA, TagA, ElementsA}, EngineState) ->
    {Size, ES1} = get_op_arg(SizeA, EngineState),
    {Tag, ES2} = get_op_arg(TagA, ES1),
    {N, ES3} = get_op_arg(ElementsA, ES2),
    {Result, ES4} = make_variant(Size, Tag, N, ES3),
    {next, store(Dest, Result, ES4)};



%% ------------------------------------------------------
%% Bits instructions
%% ------------------------------------------------------
%% A bit field is represented by an integer.
%% Bit fields "starting" with Bits.all are represented by a
%% negative integer (with infinite set bits to the left.)
%% Take Bits.sum of an infinite set fails with an `arithmetic_error`.

%% Bits.none : bits
%% An empty bit set.
eval('BITS_NONEA', EngineState) -> {next, push(?FATE_BITS(0), EngineState)};
eval({'BITS_NONE', To}, EngineState) ->
    {next, un_op(get, {To, {immediate, ?FATE_BITS(0)}}, EngineState)};

%% Bits.all : bits
%% A bit field with all (an infinite amount) bits set
eval('BITS_ALLA', EngineState) -> {next, push(?FATE_BITS(-1), EngineState)};
eval({'BITS_ALL', To}, EngineState) ->
    {next, un_op(get, {To, {immediate, ?FATE_BITS(-1)}}, EngineState)};

%% Bits.all_n : bits
%% A bit field with n bits set
eval({'BITS_ALL_N', To, N}, EngineState) ->
    {next, un_op(bits_all, {To, N}, EngineState)};

%% Bits.set(b : bits, i : int) : bits
%% Set bit i
eval({'BITS_SET', To, Bits, Bit}, EngineState) ->
    {next, bin_op(bits_set, {To, Bits, Bit}, EngineState)};

%% Bits.clear(b : bits, i : int) : bits
%% Clear bit i
eval({'BITS_CLEAR', To, Bits, Bit}, EngineState) ->
    {next, bin_op(bits_clear, {To, Bits, Bit}, EngineState)};

%% Bits.test(b : bits, i : int) : bool
%% Check if bit i is set
eval({'BITS_TEST', To, Bits, Bit}, EngineState) ->
    {next, bin_op(bits_test, {To, Bits, Bit}, EngineState)};

%% Bits.sum(b : bits) : int
%% Count the number of set bits.
%% Throws an exception for infinite bit sets (starting from Bits.all)
eval({'BITS_SUM', To, Bits}, EngineState) ->
    {next, un_op(bits_sum, {To, Bits}, EngineState)};

%% Bits.union(a : bits, b : bits) : bits
%% For all i:
%%   Bits.test(Bits.union(a, b), i) == (Bits.test(a, i) || Bits.test(b, i))
eval({'BITS_OR', To, Bits, Bit}, EngineState) ->
    {next, bin_op(bits_union, {To, Bits, Bit}, EngineState)};

%% Bits.intersection(a : bits, b : bits) : bits
%% For all i:
%% Bits.test(Bits.intersection(a, b), i) == (Bits.test(a, i) && Bits.test(b, i))
eval({'BITS_AND', To, Bits, Bit}, EngineState) ->
    {next, bin_op(bits_intersection, {To, Bits, Bit}, EngineState)};

%% Bits.difference(a : bits, b : bits) : bits
%% For all i:
%%  Bits.test(Bits.difference(a, b), i) == (Bits.test(a, i) && !Bits.test(b, i))
eval({'BITS_DIFF', To, Bits, Bit}, EngineState) ->
    {next, bin_op(bits_difference, {To, Bits, Bit}, EngineState)};



%% ------------------------------------------------------
%% Stack instructions
%% ------------------------------------------------------
eval('DUPA', EngineState) ->
    {next, dup(EngineState)};

eval({'DUP', {immediate, N}}, EngineState) ->
    {next, dup(N, EngineState)};

eval({'POP', Dest}, EngineState) ->
    {next, un_op(get, {Dest, {stack, 0}}, EngineState)};

%% ------------------------------------------------------
%% Memory instructions
%% ------------------------------------------------------
eval({'STORE', Var, What}, EngineState) ->
    {next, un_op(get, {Var, What}, EngineState)};



%% ------------------------------------------------------
%% Other Instructions
%% ------------------------------------------------------
eval('NOP', EngineState) ->
    {next, EngineState}.




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

%% Guarded ops
gop(Op, Arg, ES) ->
    try op(Op, Arg) of
        Res -> Res
    catch
        {add_engine_state, E} -> abort(E, ES)
    end.

gop(Op, Arg1, Arg2, ES) ->
    try op(Op, Arg1, Arg2) of
        Res -> Res
    catch
        {add_engine_state, E} -> abort(E, ES)
    end.

gop(Op, Arg1, Arg2, Arg3, ES) ->
    try op(Op, Arg1, Arg2, Arg3) of
        Res -> Res
    catch
        {add_engine_state, E} -> abort(E, ES)
    end.


un_op(Op, {To, What}, ES) ->
    {Value, ES1} = get_op_arg(What, ES),
    Result = gop(Op, Value, ES1),
    store(To, Result, ES1).

bin_op(Op, {To, Left, Right}, ES) ->
    {LeftValue, ES1} = get_op_arg(Left, ES),
    {RightValue, ES2} = get_op_arg(Right, ES1),
    Result = gop(Op, LeftValue, RightValue, ES2),
    store(To, Result, ES2).

ter_op(Op, {To, One, Two, Three}, ES) ->
    {ValueOne, ES1} = get_op_arg(One, ES),
    {ValueTwo, ES2} = get_op_arg(Two, ES1),
    {ValueThree, ES3} = get_op_arg(Three, ES2),
    Result = gop(Op, ValueOne, ValueTwo, ValueThree, ES3),
    store(To, Result, ES3).

get_op_arg({stack, 0}, #{ accumulator := A
                        , accumulator_stack := [S|Stack] } = ES) ->
    {A, ES#{accumulator => S, accumulator_stack => Stack}};
get_op_arg({stack, 0}, #{ accumulator := A
                        , accumulator_stack := [] } = ES) ->
    {A, ES#{accumulator := ?FATE_VOID}};
get_op_arg({arg,_N} = Var, #{ memory := Mem } = ES) ->
    Value = lookup_var(Var, Mem, ES),
    {Value, ES};
get_op_arg({var,_N} = Var, #{ memory := Mem } = ES) ->
    Value = lookup_var(Var, Mem, ES),
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
store({arg, N}, _, ES) ->
     abort({cannot_write_to_arg, N}, ES).


%% ------------------------------------------------------
%% Variant instructions
%% ------------------------------------------------------


make_variant(Size, Tag, NoElements, ES)  when ?IS_FATE_INTEGER(Size)
                                              , ?IS_FATE_INTEGER(Tag)
                                              , ?IS_FATE_INTEGER(NoElements)
                                              , NoElements >= 0
                                              , Size >= 0
                                              , Tag < Size
                                              , Tag >= 0 ->
    {Elements, ES2} = pop_n(NoElements, ES),
    Values = list_to_tuple(Elements),
    {aeb_fate_data:make_variant(Size, Tag, Values), ES2};
make_variant(Size, Tag, NoElements, ES) ->
    abort({bad_arguments_to_make_variant, Size, Tag, NoElements}, ES).

%% ------------------------------------------------------
%% Tuple instructions
%% ------------------------------------------------------


make_tuple(Size, ES) ->
    {Elements, ES2} = pop_n(Size, ES),
    Tuple = list_to_tuple(Elements),
    FateTuple = aeb_fate_data:make_tuple(Tuple),
    push(FateTuple, ES2).


tuple_element(Type, To, Which, TupleArg, ES) ->
    {Index, ES1} = get_op_arg(Which, ES),
    {FateTuple, ES2} = get_op_arg(TupleArg, ES1),
    case check_type(integer, Index) andalso ?IS_FATE_TUPLE(FateTuple) of
        false -> abort({bad_arguments_to_element, Index, FateTuple}, ES);
        true ->
            ?FATE_TUPLE(Tuple) = FateTuple,
            case size(Tuple) > Index of
                true ->
                    V = element(Index+1, Tuple),
                    case check_type(Type, V) of
                        true -> store(To, V, ES2);
                        false -> abort({bad_element_type, Type, V}, ES)
                    end;
                false ->
                    abort({element_index_out_of_bounds, Index}, ES)
            end
    end.

%% Unary operations
op(get, A) ->
    A;
op(inc, A) ->
    A + 1;
op(dec, A) ->
    A - 1;
op('not', A) ->
    not A;
op(map_from_list, A) when ?IS_FATE_LIST(A) ->
    KeyValues = [T || ?FATE_TUPLE(T) <- ?FATE_LIST_VALUE(A)],
    aeb_fate_data:make_map(maps:from_list(KeyValues));
op(hd, A) when ?IS_FATE_LIST(A) ->
    case ?FATE_LIST_VALUE(A) of
        [] -> abort(hd_on_empty_list);
        [Hd|_] -> Hd
    end;
op(is_nil, A) when ?IS_FATE_LIST(A) ->
    aeb_fate_data:make_boolean(?FATE_LIST_VALUE(A) =:= []);
op(tl, A) when ?IS_FATE_LIST(A) ->
    case ?FATE_LIST_VALUE(A) of
        [] -> abort(tl_on_empty_list);
        [_|Tl] -> Tl
    end;
op(length, A) when ?IS_FATE_LIST(A) ->
    aeb_fate_data:make_integer(length(?FATE_LIST_VALUE(A)));
op(int_to_str, A) when ?IS_FATE_INTEGER(A) ->
    aeb_fate_data:make_string(integer_to_binary(?FATE_INTEGER_VALUE(A)));
op(int_to_addr, A) when ?IS_FATE_INTEGER(A) ->
    aeb_fate_data:make_address(<<A:256>>);
op(addr_to_str, A) when ?IS_FATE_ADDRESS(A) ->
    <<I:256>> = ?FATE_ADDRESS_VALUE(A),
    aeb_fate_data:make_string(integer_to_base58(I));
op(str_reverse, A) when ?IS_FATE_STRING(A) ->
    aeb_fate_data:make_string(binary_reverse(?FATE_STRING_VALUE(A)));
op(bits_all, N)  when ?IS_FATE_INTEGER(N) ->
    ?FATE_BITS((1 bsl (N)) - 1);
op(bits_sum, A)  when ?IS_FATE_BITS(A) ->
    ?FATE_BITS(Bits) = A,
    if Bits < 0 -> abort({arithmetic_error, bits_sum_on_infinite_set});
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
    if B =:= 0 -> abort(division_by_zero);
       true -> A div B
    end;
op(pow, A, B)  when ?IS_FATE_INTEGER(A)
                    , ?IS_FATE_INTEGER(B) ->
    %% TODO: Implement arbitrary precision pow function.
    try round(math:pow(A, B)) of
        I -> I
    catch error:badarith ->
            abort(pow_too_large_exp)
    end;
op(mod, A, B)  when ?IS_FATE_INTEGER(A)
                    , ?IS_FATE_INTEGER(B) ->
    if B =:= 0 -> abort(mod_by_zero);
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
        void -> abort(missing_map_key);
        Res -> Res
    end;
op(map_member, Map, Key) when ?IS_FATE_MAP(Map),
                              not ?IS_FATE_MAP(Key) ->
    aeb_fate_data:make_boolean(maps:is_key(Key, ?FATE_MAP_VALUE(Map)));
op(cons, Hd, Tail) when ?IS_FATE_LIST(Tail) ->
    case ?FATE_LIST_VALUE(Tail) of
        [] -> aeb_fate_data:make_list([Hd|?FATE_LIST_VALUE(Tail)]);
        [OldHd|_] = Tail ->
            case check_type(type(OldHd), Hd) of
                true ->
                    aeb_fate_data:make_list([Hd|?FATE_LIST_VALUE(Tail)]);
                false ->
                    abort({type_error, cons, Hd, type(OldHd)})
            end
    end;
op(str_equal, A, B) when ?IS_FATE_STRING(A)
                         , ?IS_FATE_STRING(B) ->
    aeb_fate_data:make_boolean(?FATE_STRING_VALUE(A)
                           =:=
                               ?FATE_STRING_VALUE(B));
op(str_join, A, B) when ?IS_FATE_STRING(A)
                         , ?IS_FATE_STRING(B) ->
    aeb_fate_data:make_string(<<?FATE_STRING_VALUE(A)/binary,
                            ?FATE_STRING_VALUE(B)/binary>>);
op(variant_test, A, B)  when ?IS_FATE_VARIANT(A)
                         , ?IS_FATE_INTEGER(B)
                         , B >= 0 ->
    ?FATE_VARIANT(_S, T,_Values) = A,
    aeb_fate_data:make_boolean(T =:= B);
op(variant_element, A, B)  when ?IS_FATE_VARIANT(A)
                                , ?IS_FATE_INTEGER(B)
                                , B >= 0 ->
    ?FATE_VARIANT(_S, _T, Values) = A,
    if size(Values) >= B ->
            element(B, Values);
       true ->
            abort({type_error, variant_element, B, A})
    end;

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
    aeb_fate_data:make_map(Res).




bits_sum(0, Sum) -> Sum;
bits_sum(N, Sum) -> bits_sum(N bsr 1, Sum + (N band 2#1)).



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

get_trace(#{trace := T}) -> T.

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

%% helpers
integer_to_base58(0) -> <<"1">>;
integer_to_base58(Integer) ->
    Base58String = integer_to_base58(Integer, []),
    list_to_binary(Base58String).

integer_to_base58(0, Acc) -> Acc;
integer_to_base58(Integer, Acc) ->
       Quot = Integer div 58,
       Rem = Integer rem 58,
       integer_to_base58(Quot, [base58char(Rem)|Acc]).

base58char(Char) ->
    binary:at(<<"123456789ABCDEFGHJKLMNPQRSTUVWXYZ"
                "abcdefghijkmnopqrstuvwxyz">>, Char).

binary_reverse(Binary) ->
    Size = erlang:size(Binary)*8,
    <<X:Size/integer-little>> = Binary,
    <<X:Size/integer-big>>.
