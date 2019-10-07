%% Implements Fate operations.
%%
-module(aefa_fate_op).

-export([ return/1
        , returnr/2
        , call/2
        , call_r/6
        , call_t/2
        , call_gr/7
        , call_value/2
        , jump/2
        , jumpif/3
        , switch/4
        , switch/5
        , switch/3
        , push/2
        , dup/1
        , dup/2
        , pop/2
        , store/3
        , inc/1
        , inc/2
        , dec/1
        , dec/2
        , add/4
        , sub/4
        , mul/4
        , divide/4
        , modulo/4
        , pow/4
        , lt/4
        , gt/4
        , eq/4
        , elt/4
        , egt/4
        , neq/4
        , and_op/4
        , or_op/4
        , not_op/3
        , tuple/3
        , element_op/4
        , map_empty/2
        , map_lookup/4
        , map_lookup/5
        , map_update/5
        , map_delete/4
        , map_member/4
        , map_from_list/3
        , map_to_list/3
        , map_size_/3
        , nil/2
        , is_nil/3
        , cons/4
        , hd/3
        , tl/3
        , length/3
        , append/4
        , str_join/4
        , str_length/3
        , int_to_str/3
        , addr_to_str/3
        , str_reverse/3
        , int_to_addr/3
        , variant/5
        , variant_test/4
        , variant_element/4
        , bits_none/1
        , bits_none/2
        , bits_all/1
        , bits_all/2
        , bits_all_n/3
        , bits_set/4
        , bits_clear/4
        , bits_test/4
        , bits_sum/3
        , bits_or/4
        , bits_and/4
        , bits_diff/4
        , address/2
        , contract_creator/2
        , balance/2
        , balance_other/3
        , origin/2
        , caller/2
        , gasprice/2
        , blockhash/3
        , beneficiary/2
        , timestamp/2
        , generation/2
        , microblock/2
        , difficulty/2
        , gaslimit/2
        , gas/2
        , log/2
        , log/3
        , log/4
        , log/5
        , log/6
        , log/7
        , deactivate/1
        , spend/3
        , oracle_register/8
        , oracle_query/9
        , oracle_respond/7
        , oracle_extend/4
        , oracle_get_answer/6
        , oracle_get_question/6
        , oracle_query_fee/3
        , oracle_check/5
        , oracle_check_query/6
        , is_oracle/3
        , is_contract/3
        , is_payable/3
        , aens_resolve/5
        , aens_preclaim/4
        , aens_claim/6
        , aens_update/1
        , aens_transfer/5
        , aens_revoke/4
        , verify_sig/5
        , verify_sig_secp256k1/5
        , ecverify_secp256k1/5
        , ecrecover_secp256k1/4
        , contract_to_address/3
        , address_to_contract/3
        , sha3/3
        , sha256/3
        , blake2b/3
        , setelement/5
        , abort/2
        , exit/2
        , nop/1
        , unused_1/1
        , unused_2/1
        , auth_tx_hash/2
        , bytes_to_int/3
        , bytes_to_str/3
        , bytes_concat/4
        , bytes_split/4
        ]).

-include_lib("aebytecode/include/aeb_fate_data.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include("../../aecore/include/blocks.hrl").

%% ------------------------------------------------------------------------
%% Operations
%% ------------------------------------------------------------------------

-spec unused_1(_) -> no_return().
unused_1(ES) ->
    aefa_fate:abort(bad_bytecode, ES).

-spec unused_2(_) -> no_return().
unused_2(ES) ->
    aefa_fate:abort(bad_bytecode, ES).

%% ------------------------------------------------------
%% Call/return instructions
%% ------------------------------------------------------
return(EngineState) ->
    aefa_fate:pop_call_stack(EngineState).

returnr(Arg0, EngineState) ->
    ES1 = un_op(get, {{stack, 0}, Arg0}, EngineState),
    aefa_fate:pop_call_stack(ES1).

call(Arg0, EngineState) ->
    ES1 = aefa_fate:push_return_address(EngineState),
    {Fun, ES2} = get_op_arg(Arg0, ES1),
    Signature = aefa_fate:get_function_signature(Fun, ES2),
    ES3 = aefa_fate:bind_args_from_signature(Signature, ES2),
    {jump, 0, aefa_fate:set_local_function(Fun, ES3)}.

call_t(Arg0, EngineState) ->
    {Fun, ES1} = get_op_arg(Arg0, EngineState),
    Signature = aefa_fate:get_function_signature(Fun, ES1),
    ES2 = aefa_fate:bind_args_from_signature(Signature, ES1),
    {jump, 0, aefa_fate:set_local_function(Fun, ES2)}.

call_r(Arg0, Arg1, Arg2, Arg3, Arg4, EngineState) ->
    ES1 = aefa_fate:push_return_address(EngineState),
    {[Contract, ArgType, RetType, Value], ES2} = get_op_args([Arg0, Arg2, Arg3, Arg4], ES1),
    ES3 = remote_call_common(Contract, Arg1, ArgType, RetType, Value, ES2),
    {jump, 0, ES3}.

call_gr(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, EngineState) ->
    ES1 = aefa_fate:push_return_address(EngineState),
    {[Contract, ArgType, RetType, Value, GasCap], ES2} = get_op_args([Arg0, Arg2, Arg3, Arg4, Arg5], ES1),
    ES3 = aefa_fate:push_gas_cap(GasCap, ES2),
    ES4 = remote_call_common(Contract, Arg1, ArgType, RetType, Value, ES3),
    {jump, 0, ES4}.

remote_call_common(Contract, Function, ?FATE_TYPEREP({tuple, ArgTypes}), ?FATE_TYPEREP(RetType), Value, EngineState) ->
    Current   = aefa_engine_state:current_contract(EngineState),
    Arity     = length(ArgTypes),
    ES1       = aefa_fate:unfold_store_maps_in_args(Arity, EngineState),
    ES2       = aefa_fate:check_remote(Contract, ES1),
    ES3       = aefa_fate:set_remote_function(Contract, Function, Value > 0, true, ES2),
    Signature = aefa_fate:get_function_signature(Function, ES3),
    ES4       = aefa_fate:check_signature_and_bind_args(Arity, Signature, ES3),
    TVars     = aefa_engine_state:current_tvars(ES4),
    ES5       = aefa_fate:push_return_type_check(Signature, {ArgTypes, RetType}, TVars, ES4),
    transfer_value(Current, Contract, Value, ES5).

transfer_value(_From, ?FATE_CONTRACT(_To), Value, ES) when not ?IS_FATE_INTEGER(Value) ->
    aefa_fate:abort({value_does_not_match_type, Value, integer}, ES);
transfer_value(From, ?FATE_CONTRACT(To), Value, ES) ->
    case ?FATE_INTEGER_VALUE(Value) of
        IntValue when IntValue < 0 ->
            aefa_fate:abort({call_error, negative_value}, ES);
        0 ->
            aefa_engine_state:set_call_value(0, ES);
        IntValue ->
            ES1 = aefa_engine_state:set_call_value(IntValue, ES),
            API = aefa_engine_state:chain_api(ES1),
            case aefa_chain_api:transfer_value(From, To, IntValue, API) of
                {ok, API1} ->
                    aefa_engine_state:set_chain_api(API1, ES1);
                {error, What} ->
                    aefa_fate:abort({call_error, What}, ES1)
            end
    end.

%% ------------------------------------------------------
%% Control flow instructions
%% ------------------------------------------------------
jump(Arg0, EngineState) ->
    {jump, Arg0, EngineState}.

jumpif(Arg0, Arg1, EngineState) ->
    {Value, ES1} = get_op_arg(Arg0, EngineState),
    case Value of
        true -> {jump, Arg1, ES1};
        false -> {next, ES1}
    end.

switch(Arg0, Arg1, Arg2, EngineState) ->
    {Value, ES1} = get_op_arg(Arg0, EngineState),
    if ?IS_FATE_VARIANT(Value) ->
            ?FATE_VARIANT(Arities, Tag, _T) = Value,
            if length(Arities) =:= 2 ->
                    %% Tag can only be 0 or 1 or the variant is broken.
                    case Tag of
                        0 -> {jump, Arg1, ES1};
                        1 -> {jump, Arg2, ES1}
                    end;
               true -> aefa_fate:abort({bad_variant_size, length(Arities)}, ES1)
            end;
       true -> aefa_fate:abort({value_does_not_match_type,Value, variant}, ES1)
    end.

switch(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    {Value, ES1} = get_op_arg(Arg0, EngineState),
    if ?IS_FATE_VARIANT(Value) ->
            ?FATE_VARIANT(Arities, Tag, _T) = Value,
            if length(Arities) =:= 3 ->
                    %% Tag can only be 0, 1 or 2 or the variant is broken.
                    case Tag of
                        0 -> {jump, Arg1, ES1};
                        1 -> {jump, Arg2, ES1};
                        2 -> {jump, Arg3, ES1}
                    end;
               true -> aefa_fate:abort({bad_variant_size, length(Arities)}, ES1)
            end;
       true -> aefa_fate:abort({value_does_not_match_type,Value, variant}, ES1)
    end.

switch(Arg0, Arg1, EngineState) ->
    N = length(Arg1),
    {Value, ES1} = get_op_arg(Arg0, EngineState),
    if ?IS_FATE_VARIANT(Value) ->
            ?FATE_VARIANT(Arities, Tag, _T) = Value,
            if length(Arities) =:= N ->
                    BB = lists:nth(Tag + 1, Arg1),
                    {jump, BB, ES1};
               true -> aefa_fate:abort({bad_variant_tag, Tag}, ES1)
            end;
       true -> aefa_fate:abort({value_does_not_match_type, Value, variant}, ES1)
    end.


%% ------------------------------------------------------
%% Stack instructions
%% ------------------------------------------------------

push(Arg0, EngineState) ->
    un_op(get, {{stack, 0}, Arg0}, EngineState).

%% Consider moving the call to aefa_fate_eval directly...
dup(EngineState) -> aefa_fate:dup(EngineState).

dup(Arg0, EngineState) ->
    {Value, ES1} = get_op_arg(Arg0, EngineState),
    aefa_fate:dup(Value, ES1).

pop(Arg0, EngineState) ->
    un_op(get, {Arg0, {stack, 0}}, EngineState).

%% ------------------------------------------------------
%% Memory instructions
%% ------------------------------------------------------
store(Arg0, Arg1, EngineState) ->
    un_op(get, {Arg0, Arg1}, EngineState).

%% ------------------------------------------------------
%% Integer instructions
%% ------------------------------------------------------

inc(EngineState) ->
    un_op(inc, {{stack, 0}, {stack, 0}}, EngineState).

inc(Arg0, EngineState) ->
    un_op(inc, {Arg0, Arg0}, EngineState).

dec(EngineState) ->
    un_op(dec, {{stack, 0}, {stack, 0}}, EngineState).

dec(Arg0, EngineState) ->
    un_op(dec, {Arg0, Arg0}, EngineState).

add(Arg0, Arg1, Arg2, EngineState) ->
    {A, ES1} = get_op_arg(Arg1, EngineState),
    {B, ES2} = get_op_arg(Arg2, ES1),
    Res = gop(add, A, B, ES2),
    ES3 = aefa_engine_state:spend_gas_for_new_cells(words_used(Res), ES2),
    write(Arg0, Res, ES3).

sub(Arg0, Arg1, Arg2, EngineState) ->
    {A, ES1} = get_op_arg(Arg1, EngineState),
    {B, ES2} = get_op_arg(Arg2, ES1),
    Res = gop(sub, A, B, ES2),
    ES3 = aefa_engine_state:spend_gas_for_new_cells(words_used(Res), ES2),
    write(Arg0, Res, ES3).

mul(Arg0, Arg1, Arg2, EngineState) ->
    {A, ES1} = get_op_arg(Arg1, EngineState),
    {B, ES2} = get_op_arg(Arg2, ES1),
    Res = gop(mul, A, B, ES2),
    ES3 = aefa_engine_state:spend_gas_for_new_cells(words_used(Res), ES2),
    write(Arg0, Res, ES3).

divide(Arg0, Arg1, Arg2, EngineState) ->
    {A, ES1} = get_op_arg(Arg1, EngineState),
    {B, ES2} = get_op_arg(Arg2, ES1),
    Res = gop('div', A, B, ES2),
    ES3 = aefa_engine_state:spend_gas_for_new_cells(words_used(Res), ES2),
    write(Arg0, Res, ES3).

modulo(Arg0, Arg1, Arg2, EngineState) ->
    {A, ES1} = get_op_arg(Arg1, EngineState),
    {B, ES2} = get_op_arg(Arg2, ES1),
    Res = gop(mod, A, B, ES2),
    ES3 = aefa_engine_state:spend_gas_for_new_cells(words_used(Res), ES2),
    write(Arg0, Res, ES3).

pow(Arg0, Arg1, Arg2, EngineState) ->
    {Base, ES1} = get_op_arg(Arg1, EngineState),
    {Exponent, ES2} = get_op_arg(Arg2, ES1),
    if ?IS_FATE_INTEGER(Base) andalso ?IS_FATE_INTEGER(Exponent) ->
            if Exponent < 0 ->
                    aefa_fate:abort({arithmetic_error, negative_exponent}, ES2);
               true ->
                    {Res, ES3} = pow(Base, Exponent, ES2),
                    write(Arg0, Res, ES3)
            end
    end.


%% ------------------------------------------------------
%% Comparison instructions
%% ------------------------------------------------------
lt(Arg0, Arg1, Arg2, EngineState) ->
    bin_comp(lt, {Arg0, Arg1, Arg2}, EngineState).

gt(Arg0, Arg1, Arg2, EngineState) ->
    bin_comp(gt, {Arg0, Arg1, Arg2}, EngineState).

eq(Arg0, Arg1, Arg2, EngineState) ->
    bin_comp(eq, {Arg0, Arg1, Arg2}, EngineState).

elt(Arg0, Arg1, Arg2, EngineState) ->
    bin_comp(elt, {Arg0, Arg1, Arg2}, EngineState).

egt(Arg0, Arg1, Arg2, EngineState) ->
    bin_comp(egt, {Arg0, Arg1, Arg2}, EngineState).

neq(Arg0, Arg1, Arg2, EngineState) ->
    bin_comp(neq, {Arg0, Arg1, Arg2}, EngineState).

%% ------------------------------------------------------
%% Boolean instructions
%% ------------------------------------------------------
and_op(Arg0, Arg1, Arg2, EngineState) ->
    bin_op('and', {Arg0, Arg1, Arg2}, EngineState).

or_op(Arg0, Arg1, Arg2, EngineState) ->
    bin_op('or', {Arg0, Arg1, Arg2}, EngineState).

not_op(Arg0, Arg1, EngineState) ->
    un_op('not', {Arg0, Arg1}, EngineState).

%% ------------------------------------------------------
%% Tuple instructions
%% ------------------------------------------------------

%% Make tuple only takes a fixed size.
%% NOTE: There is no type checking on the arguments on the stack.
tuple(Arg0, Arg1, EngineState) ->
    if is_integer(Arg1) andalso (Arg1 >= 0) ->
            make_tuple(Arg0, Arg1, EngineState);
       true -> aefa_fate:abort({invalid_tuple_size, Arg1}, EngineState)
    end.

make_tuple(To, Size, ES) ->
    {Elements, ES1} = aefa_fate:pop_n(Size, ES),
    Tuple = list_to_tuple(Elements),
    FateTuple = aeb_fate_data:make_tuple(Tuple),
    ES2 = spend_tuple_gas(Size, ES1),
    write(To, FateTuple, ES2).

element_op(To, Which, TupleArg, ES) ->
    {Index, ES1} = get_op_arg(Which, ES),
    {FateTuple, ES2} = get_op_arg(TupleArg, ES1),
    case ?IS_FATE_INTEGER(Index)
        andalso (?FATE_INTEGER_VALUE(Index) >= 0)
        andalso ?IS_FATE_TUPLE(FateTuple) of
        false -> aefa_fate:abort({bad_arguments_to_element, Index, FateTuple}, ES);
        true ->
            ?FATE_TUPLE(Tuple) = FateTuple,
            case size(Tuple) > Index of
                true ->
                    V = element(Index+1, Tuple),
                    write(To, V, ES2);
                false ->
                    aefa_fate:abort({element_index_out_of_bounds, Index}, ES)
            end
    end.

%% Setting an element of a tuple actually creates a copy of all elements in
%% the original tuple, so we pay gas in relation to the size of the tuple.
setelement(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    {Index, ES1} = get_op_arg(Arg1, EngineState),
    {FateTuple, ES2} = get_op_arg(Arg2, ES1),
    {Element, ES3} = get_op_arg(Arg3, ES2),
    case ?IS_FATE_INTEGER(Index)
        andalso (?FATE_INTEGER_VALUE(Index) >= 0)
        andalso ?IS_FATE_TUPLE(FateTuple) of
        false -> aefa_fate:abort({bad_arguments_to_setelement, Index, FateTuple}, ES3);
        true ->
            ?FATE_TUPLE(Tuple) = FateTuple,
            Size = size(Tuple),
            case Size > Index of
                true ->
                    NewT = ?FATE_TUPLE(erlang:setelement(Index+1, Tuple, Element)),
                    ES4 = spend_tuple_gas(Size, ES3),
                    write(Arg0, NewT, ES4);
                false ->
                    aefa_fate:abort({element_index_out_of_bounds, Index}, ES3)
            end
    end.


%% ------------------------------------------------------
%% Map instructions
%% ------------------------------------------------------
map_empty(Arg0, EngineState) ->
    ES1 = un_op(get, {Arg0,
                      {immediate, aeb_fate_data:make_map(#{})}},
                EngineState),
    aefa_engine_state:spend_gas_for_new_cells(2, ES1).

map_lookup(Arg0, Arg1, Arg2, EngineState) ->
    {[Map, Key], ES1} = get_op_args([Arg1, Arg2], EngineState),
    {Result, ES2} = map_lookup1(Key, Map, ES1),
    case Result of
        error     -> aefa_fate:abort(missing_map_key, ES2);
        {ok, Val} -> write(Arg0, Val, ES2)
    end.

map_lookup(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    {[Map, Key, Default], ES1} = get_op_args([Arg1, Arg2, Arg3], EngineState),
    {Result, ES2} = map_lookup1(Key, Map, ES1),
    case Result of
        error     -> write(Arg0, Default, ES2);
        {ok, Val} -> write(Arg0, Val, ES2)
    end.

map_member(Arg0, Arg1, Arg2, EngineState) ->
    {[Map, Key], ES1} = get_op_args([Arg1, Arg2], EngineState),
    case Map of
        _ when ?IS_FATE_MAP(Map) ->
            write(Arg0, aeb_fate_data:make_boolean(maps:is_key(Key, ?FATE_MAP_VALUE(Map))), ES1);
        ?FATE_STORE_MAP(Cache, MapId) ->
            {Member, ES2} = store_map_member(Cache, MapId, Key, ES1),
            write(Arg0, Member, ES2)
    end.

map_lookup1(Key, Map, ES) when ?IS_FATE_MAP(Map) ->
    case maps:get(Key, ?FATE_MAP_VALUE(Map), void) of
        void -> {error, ES};
        Res  -> {{ok, Res}, ES}
    end;
map_lookup1(Key, ?FATE_STORE_MAP(Cache, MapId), ES) ->
    store_map_lookup(Cache, MapId, Key, ES);
map_lookup1(Key, Map, _ES) ->
    error({map_lookup1, Key, Map}).

map_update(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    ES1 = ter_op(map_update, {Arg0, Arg1, Arg2, Arg3}, EngineState),
    aefa_engine_state:spend_gas_for_new_cells(2, ES1).

map_delete(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(map_delete, {Arg0, Arg1, Arg2}, EngineState).

map_from_list(Arg0, Arg1, EngineState) ->
    {List, ES1} = get_op_arg(Arg1, EngineState),
    Map = gop(map_from_list, List, ES1),
    ES2 = write(Arg0, Map, ES1),
    Size = map_size(?FATE_MAP_VALUE(Map)),
    spend_tuple_gas(Size, ES2).

map_to_list(Arg0, Arg1, EngineState) ->
    {Map, ES1} = get_op_arg(Arg1, EngineState),
    case Map of
        _ when ?IS_FATE_MAP(Map) ->
            Tuples = [aeb_fate_data:make_tuple({K, V})
                      || {K, V} <- maps:to_list(?FATE_MAP_VALUE(Map))],
            ES2 = write(Arg0, aeb_fate_data:make_list(Tuples), ES1),
            Size = map_size(?FATE_MAP_VALUE(Map)),
            aefa_engine_state:spend_gas_for_new_cells(Size * 2, ES2);
        ?FATE_STORE_MAP(Cache, MapId) ->
            {List, ES2} = store_map_to_list(Cache, MapId, ES1),
            ES3 = write(Arg0, List, ES2),
            Size = length(?FATE_LIST_VALUE(List)),
            aefa_engine_state:spend_gas_for_new_cells(Size * 2, ES3)
    end.

map_size_(Arg0, Arg1, EngineState) ->
    {Map, ES1} = get_op_arg(Arg1, EngineState),
    case Map of
        _ when ?IS_FATE_MAP(Map) ->
            Size = aeb_fate_data:make_integer(map_size(?FATE_MAP_VALUE(Map))),
            write(Arg0, Size, ES1);
        ?FATE_STORE_MAP(Cache, MapId) ->
            {Size, ES2} = store_map_size(Cache, MapId, ES1),
            write(Arg0, Size, ES2)
    end.

%% ------------------------------------------------------
%% List instructions
%% ------------------------------------------------------
nil(Arg0, EngineState) ->
    un_op(get, {Arg0,
                       {immediate, aeb_fate_data:make_list([])}},
                 EngineState).

is_nil(Arg0, Arg1, EngineState) ->
    un_op(is_nil, {Arg0, Arg1}, EngineState).

cons(Arg0, Arg1, Arg2, EngineState) ->
    ES1 = bin_op(cons, {Arg0, Arg1, Arg2}, EngineState),
    aefa_engine_state:spend_gas_for_new_cells(2, ES1).

hd(Arg0, Arg1, EngineState) ->
    un_op(hd, {Arg0, Arg1}, EngineState).

tl(Arg0, Arg1, EngineState) ->
    un_op(tl, {Arg0, Arg1}, EngineState).

length(Arg0, Arg1, EngineState) ->
    un_op(length, {Arg0, Arg1}, EngineState).

append(Arg0, Arg1, Arg2, EngineState) ->
    ES1 = bin_op(append, {Arg0, Arg1, Arg2}, EngineState),
    %% We will create a new copy of the first list.
    {List, _} = get_op_arg(Arg1, EngineState),
    Size = length(?FATE_LIST_VALUE(List)),
    aefa_engine_state:spend_gas_for_new_cells(Size * 2, ES1).

%% ------------------------------------------------------
%% String instructions
%% ------------------------------------------------------

str_join(Arg0, Arg1, Arg2, EngineState) ->
    {LeftValue, ES1} = get_op_arg(Arg1, EngineState),
    {RightValue, ES2} = get_op_arg(Arg2, ES1),
    Result = gop(str_join, LeftValue, RightValue, ES2),
    Cells = string_cells(Result),
    ES3 = aefa_engine_state:spend_gas_for_new_cells(Cells + 1, ES2),
    write(Arg0, Result, ES3).

str_length(Arg0, Arg1, EngineState) ->
    un_op(str_length, {Arg0, Arg1}, EngineState).

int_to_str(Arg0, Arg1, EngineState) ->
    {LeftValue, ES1} = get_op_arg(Arg1, EngineState),
    Result = gop(int_to_str, LeftValue, ES1),
    Cells = string_cells(Result),
    ES2 = aefa_engine_state:spend_gas_for_new_cells(Cells + 1, ES1),
    write(Arg0, Result, ES2).

addr_to_str(Arg0, Arg1, EngineState) ->
    {LeftValue, ES1} = get_op_arg(Arg1, EngineState),
    Result = gop(addr_to_str, LeftValue, ES1),
    Cells = string_cells(Result),
    ES2 = aefa_engine_state:spend_gas_for_new_cells(Cells + 1, ES1),
    write(Arg0, Result, ES2).

str_reverse(Arg0, Arg1, EngineState) ->
    {LeftValue, ES1} = get_op_arg(Arg1, EngineState),
    Result = gop(str_reverse, LeftValue, ES1),
    Cells = string_cells(Result),
    ES2 = aefa_engine_state:spend_gas_for_new_cells(Cells + 1, ES1),
    write(Arg0, Result, ES2).

int_to_addr(Arg0, Arg1, EngineState) ->
    {LeftValue, ES1} = get_op_arg(Arg1, EngineState),
    Result = gop(int_to_addr, LeftValue, ES1),
    Cells = address_cells(Result),
    ES2 = aefa_engine_state:spend_gas_for_new_cells(Cells + 1, ES1),
    write(Arg0, Result, ES2).

%% One Cell per 64 bit word
string_cells(String) when ?IS_FATE_STRING(String) ->
    byte_size(?FATE_STRING_VALUE(String)) div 8.

address_cells(A) when ?IS_FATE_ADDRESS(A) ->
    byte_size(?FATE_ADDRESS_VALUE(A)) div 8.

bytes_cells(B) when ?IS_FATE_BYTES(B) ->
    byte_size(?FATE_BYTES_VALUE(B)) div 8.

%% ------------------------------------------------------
%% Variant instructions
%% ------------------------------------------------------
%% A Variant type has a list of arities.
%%  (the arity of each tag).
%% A Variant also has a tag.
%% A Variant has a tuple of values which size and types
%%   are decided by the tag.
%% Note: At the momement the types of the values are not
%%       specified.
%%       Also, tags are only numbers (in Sophia tags will
%%       correspond to names)
%% There are some variant_switch instructions for
%% variants of size 2, 3 and N.

variant(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    {Arities, ES1} = get_op_arg(Arg1, EngineState),
    {Tag, ES2} = get_op_arg(Arg2, ES1),
    {N, ES3} = get_op_arg(Arg3, ES2),
    {Result, ES4} = make_variant(Arities, Tag, N, ES3),
    write(Arg0, Result, ES4).

variant_test(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(variant_test, {Arg0, Arg1, Arg2}, EngineState).

variant_element(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(variant_element, {Arg0, Arg1, Arg2}, EngineState).

%% ------------------------------------------------------
%% Bits instructions
%% ------------------------------------------------------
%% A bit field is represented by an integer.
%% Bit fields "starting" with Bits.all are represented by a
%% negative integer (with infinite set bits to the left.)
%% Take Bits.sum of an infinite set fails with an `arithmetic_error`.

%% Bits.none : bits
%% An empty bit set.
bits_none(EngineState) ->
    ES1 = aefa_engine_state:spend_gas_for_new_cells(1, EngineState),
    aefa_fate:push(?FATE_BITS(0), ES1).

bits_none(Arg0, EngineState) ->
    ES1 = aefa_engine_state:spend_gas_for_new_cells(1, EngineState),
    un_op(get, {Arg0, {immediate, ?FATE_BITS(0)}}, ES1).

%% Bits.all : bits
%% A bit field with all (an infinite amount) bits set
bits_all(EngineState) ->
    ES1 = aefa_engine_state:spend_gas_for_new_cells(1, EngineState),
    aefa_fate:push(?FATE_BITS(-1), ES1).

bits_all(Arg0, EngineState) ->
    ES1 = aefa_engine_state:spend_gas_for_new_cells(1, EngineState),
    un_op(get, {Arg0, {immediate, ?FATE_BITS(-1)}}, ES1).

%% Bits.all_n : bits
%% A bit field with n bits set
bits_all_n(Arg0, Arg1, EngineState) ->
    {Value, ES1} = get_op_arg(Arg1, EngineState),
    Result = gop(bits_all, Value, ES1),
    ?FATE_BITS(AsInt) = Result,
    Cells = words_used(AsInt),
    ES2 = aefa_engine_state:spend_gas_for_new_cells(Cells, ES1),
    write(Arg0, Result, ES2).

%% Bits.set(b : bits, i : int) : bits
%% Set bit i
bits_set(Arg0, Arg1, Arg2, EngineState) ->
    {Bits, ES1} = get_op_arg(Arg1, EngineState),
    {I, ES2} = get_op_arg(Arg2, ES1),
    Result = gop(bits_set, Bits, I, ES2),
    ?FATE_BITS(AsInt) = Result,
    Cells = words_used(AsInt),
    ES3 = aefa_engine_state:spend_gas_for_new_cells(Cells, ES2),
    write(Arg0, Result, ES3).

%% Bits.clear(b : bits, i : int) : bits
%% Clear bit i
bits_clear(Arg0, Arg1, Arg2, EngineState) ->
    {Bits, ES1} = get_op_arg(Arg1, EngineState),
    {I, ES2} = get_op_arg(Arg2, ES1),
    Result = gop(bits_clear, Bits, I, ES2),
    ?FATE_BITS(AsInt) = Result,
    Cells = words_used(AsInt),
    ES3 = aefa_engine_state:spend_gas_for_new_cells(Cells, ES2),
    write(Arg0, Result, ES3).


%% Bits.test(b : bits, i : int) : bool
%% Check if bit i is set
bits_test(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(bits_test, {Arg0, Arg1, Arg2}, EngineState).

%% Bits.sum(b : bits) : int
%% Count the number of set bits.
%% Throws an exception for infinite bit sets (starting from Bits.all)
bits_sum(Arg0, Arg1, EngineState) ->
    un_op(bits_sum, {Arg0, Arg1}, EngineState).

%% Bits.union(a : bits, b : bits) : bits
%% For all i:
%%   Bits.test(Bits.union(a, b), i) == (Bits.test(a, i) || Bits.test(b, i))
bits_or(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(bits_union, {Arg0, Arg1, Arg2}, EngineState).

%% Bits.intersection(a : bits, b : bits) : bits
%% For all i:
%% Bits.test(Bits.intersection(a, b), i) == (Bits.test(a, i) && Bits.test(b, i))
bits_and(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(bits_intersection, {Arg0, Arg1, Arg2}, EngineState).

%% Bits.difference(a : bits, b : bits) : bits
%% For all i:
%%  Bits.test(Bits.difference(a, b), i) == (Bits.test(a, i) && !Bits.test(b, i))
bits_diff(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(bits_difference, {Arg0, Arg1, Arg2}, EngineState).

address(Arg0, EngineState) ->
    Pubkey = aefa_engine_state:current_contract(EngineState),
    Address = aeb_fate_data:make_address(Pubkey),
    write(Arg0, Address, EngineState).

contract_creator(Arg0, EngineState) ->
    Pubkey  = aefa_engine_state:current_contract(EngineState),
    API     = aefa_engine_state:chain_api(EngineState),
    Creator = aefa_chain_api:creator(Pubkey, API),
    Address = aeb_fate_data:make_address(Creator),
    write(Arg0, Address, EngineState).

balance(Arg0, EngineState) ->
    API = aefa_engine_state:chain_api(EngineState),
    Pubkey = aefa_engine_state:current_contract(EngineState),
    {ok, Balance, API1} = aefa_chain_api:account_balance(Pubkey, API),
    write(Arg0, Balance, aefa_engine_state:set_chain_api(API1, EngineState)).

auth_tx_hash(Arg0, EngineState) ->
    API   = aefa_engine_state:chain_api(EngineState),
    TxEnv = aefa_chain_api:tx_env(API),
    {Size, Val} =
        case aetx_env:ga_tx_hash(TxEnv) of
            undefined -> {1, aeb_fate_data:make_variant([0, 1], 0, {})};
            TxHash    -> {2, aeb_fate_data:make_variant([0, 1], 1, {?FATE_BYTES(TxHash)})}
        end,
    write(Arg0, Val, spend_tuple_gas(Size, EngineState)).

bytes_to_int(Arg0, Arg1, EngineState) ->
    un_op(bytes_to_int, {Arg0, Arg1}, EngineState).

bytes_to_str(Arg0, Arg1, EngineState) ->
    {LeftValue, ES1} = get_op_arg(Arg1, EngineState),
    Result = gop(bytes_to_str, LeftValue, ES1),
    Cells = string_cells(Result),
    ES2 = aefa_engine_state:spend_gas_for_new_cells(Cells + 1, ES1),
    write(Arg0, Result, ES2).

bytes_concat(Arg0, Arg1, Arg2, EngineState) ->
    {Bytes1, ES1} = get_op_arg(Arg1, EngineState),
    {Bytes2, ES2} = get_op_arg(Arg2, ES1),
    Result = gop(bytes_concat, Bytes1, Bytes2, ES2),
    Cells = bytes_cells(Result),
    ES3 = aefa_engine_state:spend_gas_for_new_cells(Cells, ES2),
    write(Arg0, Result, ES3).

bytes_split(Arg0, Arg1, Arg2, EngineState) ->
    ES1  = bin_op(bytes_split, {Arg0, Arg1, Arg2}, EngineState),
    spend_tuple_gas(2, ES1).

balance_other(Arg0, Arg1, ES) ->
    API = aefa_engine_state:chain_api(ES),
    case get_op_arg(Arg1, ES) of
        {?FATE_ADDRESS(Pubkey), ES1} ->
            case aefa_chain_api:account_balance(Pubkey, API) of
                {ok, Balance, API1} ->
                    ES2 = aefa_engine_state:set_chain_api(API1, ES1),
                    write(Arg0, Balance, ES2);
                error ->
                    %% Unknown accounts have balance 0
                    write(Arg0, aeb_fate_data:make_integer(0), ES)
            end;
        {Value, ES1} ->
            aefa_fate:abort({value_does_not_match_type, Value, address}, ES1)
    end.

origin(Arg0, EngineState) ->
    API = aefa_engine_state:chain_api(EngineState),
    write(Arg0, aefa_chain_api:origin(API), EngineState).

caller(Arg0, EngineState) ->
    Address = aefa_engine_state:caller(EngineState),
    write(Arg0, Address, EngineState).

gasprice(Arg0, EngineState) ->
    API = aefa_engine_state:chain_api(EngineState),
    write(Arg0, aefa_chain_api:gas_price(API), EngineState).

blockhash(Arg0, Arg1, ES) ->
    case get_op_arg(Arg1, ES) of
        {?FATE_INTEGER_VALUE(N), ES1} when ?IS_FATE_INTEGER(N) ->
            GenesisHeight = aec_block_genesis:height(),
            API = aefa_engine_state:chain_api(ES1),
            CurrentHeight = aefa_chain_api:generation(API),
            case (N < GenesisHeight orelse
                  N >= CurrentHeight orelse
                  N =< CurrentHeight - 256) of
                true ->
                    write(Arg0, aeb_fate_data:make_variant([0, 1], 0, {}), ES1);
                false ->
                    FateHash = aefa_chain_api:blockhash(N, API),
                    write(Arg0, aeb_fate_data:make_variant([0, 1], 1, {FateHash}), ES1)
            end;
        {Value, ES1} ->
            aefa_fate:abort({value_does_not_match_type, Value, integer}, ES1)
    end.

beneficiary(Arg0, EngineState) ->
    API = aefa_engine_state:chain_api(EngineState),
    write(Arg0, aefa_chain_api:beneficiary(API), EngineState).

timestamp(Arg0, EngineState) ->
    API = aefa_engine_state:chain_api(EngineState),
    write(Arg0, aefa_chain_api:timestamp_in_msecs(API), EngineState).

generation(Arg0, EngineState) ->
    API = aefa_engine_state:chain_api(EngineState),
    write(Arg0, aefa_chain_api:generation(API), EngineState).

microblock(_Arg0, _EngineState) -> exit({error, op_not_implemented_yet}).

difficulty(Arg0, EngineState) ->
    API = aefa_engine_state:chain_api(EngineState),
    write(Arg0, aefa_chain_api:difficulty(API), EngineState).

gaslimit(Arg0, EngineState) ->
    API = aefa_engine_state:chain_api(EngineState),
    write(Arg0, aefa_chain_api:gas_limit(API), EngineState).

gas(Arg0, EngineState) ->
    Gas = aefa_engine_state:gas(EngineState),
    write(Arg0, aeb_fate_data:make_integer(Gas), EngineState).

call_value(Arg0, EngineState) ->
    Value = aefa_engine_state:call_value(EngineState),
    write(Arg0, aeb_fate_data:make_integer(Value), EngineState).

log(Arg0, EngineState) ->
    log_([Arg0], EngineState).

log(Arg0, Arg1, EngineState) ->
    log_([Arg0, Arg1], EngineState).

log(Arg0, Arg1, Arg2, EngineState) ->
    log_([Arg0, Arg1, Arg2], EngineState).

log(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    log_([Arg0, Arg1, Arg2, Arg3], EngineState).

log(Arg0, Arg1, Arg2, Arg3, Arg4, EngineState) ->
    log_([Arg0, Arg1, Arg2, Arg3, Arg4], EngineState).

log(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, EngineState) ->
    log_([Arg0, Arg1, Arg2, Arg3, Arg4, Arg5], EngineState).

-define(MAX_WORD_INT,
        16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
log_(Args, EngineState) ->
    Pubkey = aefa_engine_state:current_contract(EngineState),
    {[Payload | Indices], ES1} = get_op_args(Args, EngineState),
    ToBin = fun(?FATE_STRING(Bin)) when ?IS_FATE_STRING(Bin) -> Bin;
               (?FATE_BYTES(Bin))  -> Bin;
               (Other)             -> aefa_fate:abort({value_does_not_match_type, Other, string})
            end,
    ToWord = fun(N) when is_integer(N),
                         N >= 0,
                         N =< ?MAX_WORD_INT -> <<N:256>>;
                (?FATE_FALSE)          -> <<0:256>>;
                (?FATE_TRUE)           -> <<1:256>>;
                (?FATE_BITS(N)) when N >= 0,
                                     N =< ?MAX_WORD_INT -> <<N:256>>;
                (?FATE_ADDRESS(Addr))  -> Addr;
                (?FATE_CONTRACT(Addr)) -> Addr;
                (?FATE_ORACLE(Addr))   -> Addr;
                (?FATE_ORACLE_Q(Addr)) -> Addr;
                (?FATE_BYTES(Bin)) when byte_size(Bin) =< 32 ->
                     W = byte_size(Bin),
                     <<0:(32 - W)/unit:8, Bin/binary>>;
                (N) when is_integer(N) -> aefa_fate:abort({log_illegal_int, N}, EngineState);
                (?FATE_BITS(_)) -> aefa_fate:abort(log_illegal_bits, EngineState);
                (Other) -> aefa_fate:abort({value_does_not_match_type, Other, word}, EngineState)
             end,
    PayloadBin = ToBin(Payload),
    LogEntry = {Pubkey, lists:map(ToWord, Indices), PayloadBin},
    Size = length(Indices) * 8 + byte_size(PayloadBin),
    Gas = Size * aec_governance:byte_gas(),
    spend_gas(Gas, aefa_engine_state:add_log(LogEntry, ES1)).

deactivate(_EngineState) -> exit({error, op_not_implemented_yet}).

spend(Arg0, Arg1, ES0) ->
    FromPubkey = aefa_engine_state:current_contract(ES0),
    case get_op_arg(Arg0, ES0) of
        {?FATE_ADDRESS(ToPubkey), ES1} ->
            {Amount, ES2} = get_op_arg(Arg1, ES1),
            [aefa_fate:abort({value_does_not_match_type, Amount, integer}, ES2)
             || not ?IS_FATE_INTEGER(Amount)],
            API = aefa_engine_state:chain_api(ES2),
            case aefa_chain_api:spend(FromPubkey, ToPubkey, Amount, API) of
                {ok, API1}    -> aefa_engine_state:set_chain_api(API1, ES2);
                {error, What} -> aefa_fate:abort({primop_error, spend, What}, ES2)
            end;
        {Other, ES2} ->
            aefa_fate:abort({value_does_not_match_type, Other, address}, ES2)
    end.

-define(FATE_REL_TTL(X), ?FATE_VARIANT([1,1], 0, {X})).
-define(FATE_ABS_TTL(X), ?FATE_VARIANT([1,1], 1, {X})).

oracle_register(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, EngineState) ->
    {[Signature, Address, QFee, TTL, QType, RType], ES1} =
        get_op_args([Arg1, Arg2, Arg3, Arg4, Arg5, Arg6], EngineState),
    if
        not ?IS_FATE_ADDRESS(Address) ->
            aefa_fate:abort({value_does_not_match_type, Address, address}, ES1);
        not ?IS_FATE_BYTES(64, Signature) ->
            aefa_fate:abort({value_does_not_match_type, Signature, bytes64}, ES1);
        not ?IS_FATE_INTEGER(QFee) ->
            aefa_fate:abort({value_does_not_match_type, QFee, integer}, ES1);
        not ?IS_FATE_VARIANT(TTL) ->
            aefa_fate:abort({value_does_not_match_type, QFee, ttl}, ES1);
        not (?IS_FATE_TYPEREP(QType) andalso ?IS_FATE_TYPEREP(RType)) ->
            aefa_fate:abort({primop_error, oracle_register, bad_types}, ES1);
        true ->
            ok
    end,
    case TTL of
        ?FATE_REL_TTL(X) when ?IS_FATE_INTEGER(X) -> ok;
        ?FATE_ABS_TTL(X) when ?IS_FATE_INTEGER(X) -> ok;
        _ ->
            aefa_fate:abort({primop_error, oracle_register, bad_ttl}, ES1)
    end,
    oracle_register_(Arg0, Signature, Address, QFee, TTL, QType, RType, ES1).

oracle_register_(Arg0, ?FATE_BYTES(Signature), ?FATE_ADDRESS(Address),
                 ?FATE_INTEGER_VALUE(QFee), TTL,
                 ?FATE_TYPEREP(QType),
                 ?FATE_TYPEREP(RType),
                 ES) ->
    {TTLType, TTLVal} =
        case TTL of
            ?FATE_REL_TTL(R) when ?IS_FATE_INTEGER(R) -> {relative, R};
            ?FATE_ABS_TTL(A) when ?IS_FATE_INTEGER(A) -> {absolute, A};
             _ ->
                aefa_fate:abort({primop_error, oracle_register, bad_ttl}, ES)
        end,
    QFormat = iolist_to_binary(aeb_fate_encoding:serialize_type(QType)),
    RFormat = iolist_to_binary(aeb_fate_encoding:serialize_type(RType)),
    ES1     = check_delegation_signature(oracle_register, Address, Signature, ES),
    API     = aefa_engine_state:chain_api(ES1),
    case aefa_chain_api:oracle_register(Address, QFee, TTLType, TTLVal,
                                        QFormat, RFormat, ?ABI_FATE_SOPHIA_1,
                                        API) of
        {ok, DynamicGas, API1} ->
            ES2 = aefa_engine_state:set_chain_api(API1, ES1),
            ES3 = spend_gas(DynamicGas, ES2),
            write(Arg0, ?FATE_ORACLE(Address), ES3);
        {error, What} ->
            aefa_fate:abort({primop_error, oracle_register, What}, ES1)
    end.

oracle_query(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, EngineState) ->
    {[Oracle, Question, QFee, QTTL, RTTL, QType, RType], ES1} =
        get_op_args([Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7], EngineState),
    if
        not ?IS_FATE_ORACLE(Oracle) ->
            aefa_fate:abort({value_does_not_match_type, Oracle, oracle}, ES1);
        not ?IS_FATE_INTEGER(QFee) ->
            aefa_fate:abort({value_does_not_match_type, QFee, integer}, ES1);
        not ?FATE_INTEGER_VALUE(QFee) >= 0 ->
            aefa_fate:abort({primop_error, oracle_query, too_low_fee}, ES1);
        not (?IS_FATE_TYPEREP(QType) orelse ?IS_FATE_TYPEREP(RType)) ->
            aefa_fate:abort({primop_error, oracle_query, bad_type}, ES1);
        true ->
            ok
    end,
    RTTLVal =
        case RTTL of
            ?FATE_REL_TTL(X) when ?IS_FATE_INTEGER(X) -> X;
            _ ->
                aefa_fate:abort({primop_error, oracle_query, bad_ttl}, ES1)
        end,
    {QTTLType, QTTLVal} =
        case QTTL of
            ?FATE_REL_TTL(R) when ?IS_FATE_INTEGER(R) -> {relative, R};
            ?FATE_ABS_TTL(A) when ?IS_FATE_INTEGER(A) -> {absolute, A};
             _ ->
                aefa_fate:abort({primop_error, oracle_query, bad_ttl}, ES1)
        end,
    ?FATE_ORACLE(OraclePubkey) = Oracle,
    SenderPubkey = aefa_engine_state:current_contract(ES1),
    QFeeVal = ?FATE_INTEGER_VALUE(QFee),
    API = aefa_engine_state:chain_api(ES1),
    case aefa_chain_api:oracle_query(OraclePubkey, SenderPubkey, Question,
                                     QFeeVal, QTTLType, QTTLVal, RTTLVal,
                                     ?ABI_FATE_SOPHIA_1, QType, RType, API) of
        {ok, QueryId, DynamicGas, API1} ->
            ES2 = aefa_engine_state:set_chain_api(API1, ES1),
            ES3 = spend_gas(DynamicGas, ES2),
            write(Arg0, aeb_fate_data:make_oracle_query(QueryId), ES3);
        {error, What} ->
            aefa_fate:abort({primop_error, oracle_query, What}, ES1)
    end.

oracle_respond(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, EngineState) ->
    {[Signature, Oracle, Query, Response, QType, RType], ES1} =
        get_op_args([Arg0, Arg1, Arg2, Arg3, Arg4, Arg5], EngineState),
    if
        not ?IS_FATE_ORACLE(Oracle) ->
            aefa_fate:abort({value_does_not_match_type, Oracle, oracle}, ES1);
        not ?IS_FATE_BYTES(64, Signature) ->
            aefa_fate:abort({value_does_not_match_type, Signature, bytes64}, ES1);
        not ?IS_FATE_ORACLE_Q(Query) ->
            aefa_fate:abort({value_does_not_match_type, Query, oracle_query}, ES1);
        not (?IS_FATE_TYPEREP(QType) orelse ?IS_FATE_TYPEREP(RType)) ->
            aefa_fate:abort({primop_error, oracle_respond, bad_type}, ES1);
        true ->
            ok
    end,
    ?FATE_ORACLE_Q(QueryId) = Query,
    ?FATE_ORACLE(OraclePubkey) = Oracle,
    ?FATE_BYTES(SignBin) = Signature,
    ES2 = check_delegation_signature(oracle_respond, {OraclePubkey, QueryId}, SignBin, ES1),
    API = aefa_engine_state:chain_api(ES2),
    case aefa_chain_api:oracle_respond(OraclePubkey, QueryId, Response,
                                       ?ABI_FATE_SOPHIA_1, QType, RType, API) of
        {ok, DynamicGas, API1} ->
            ES3 = spend_gas(DynamicGas, ES2),
            aefa_engine_state:set_chain_api(API1, ES3);
        {error, What} ->
            aefa_fate:abort({primop_error, oracle_respond, What}, ES2)
    end.

oracle_extend(Arg0, Arg1, Arg2, EngineState) ->
    {[Signature, Oracle, TTL], ES1} =
        get_op_args([Arg0, Arg1, Arg2], EngineState),
    if
        not ?IS_FATE_ORACLE(Oracle) ->
            aefa_fate:abort({value_does_not_match_type, Oracle, oracle}, ES1);
        not ?IS_FATE_BYTES(64, Signature) ->
            aefa_fate:abort({value_does_not_match_type, Signature, bytes64}, ES1);
        true ->
            ok
    end,
    {TTLType, TTLVal} =
        case TTL of
            %% TTL Must be relative for extends
            ?FATE_REL_TTL(R) when ?IS_FATE_INTEGER(R) -> {relative, R};
             _ ->
                aefa_fate:abort({primop_error, oracle_extend, bad_ttl}, EngineState)
        end,
    ?FATE_ORACLE(OraclePubkey) = Oracle,
    ?FATE_BYTES(SignBin) = Signature,
    ES2 = check_delegation_signature(oracle_extend, OraclePubkey, SignBin, ES1),
    API = aefa_engine_state:chain_api(ES2),
    case aefa_chain_api:oracle_extend(OraclePubkey, TTLType, TTLVal, API) of
        {ok, DynamicGas, API1} ->
            ES3 = spend_gas(DynamicGas, ES2),
            aefa_engine_state:set_chain_api(API1, ES3);
        {error, What} ->
            aefa_fate:abort({primop_error, oracle_extend, What}, ES2)
    end.

oracle_get_question(Arg0, Arg1, Arg2, Arg3, Arg4, EngineState) ->
    {[Oracle, Query, QType, RType], ES1} =
        get_op_args([Arg1, Arg2, Arg3, Arg4], EngineState),
    if
        not ?IS_FATE_ORACLE(Oracle) ->
            aefa_fate:abort({value_does_not_match_type, Oracle, oracle}, ES1);
        not ?IS_FATE_ORACLE_Q(Query) ->
            aefa_fate:abort({value_does_not_match_type, Query, oracle_query}, ES1);
        not (?IS_FATE_TYPEREP(QType) orelse ?IS_FATE_TYPEREP(RType)) ->
            aefa_fate:abort({primop_error, oracle_get_question, bad_type}, ES1);
        true ->
            ok
    end,
    ?FATE_ORACLE(OraclePubkey) = Oracle,
    ?FATE_ORACLE_Q(QueryId) = Query,
    API = aefa_engine_state:chain_api(ES1),
    case aefa_chain_api:oracle_get_question(OraclePubkey, QueryId,
                                            QType, RType, API) of
        {ok, Question, API1} ->
            ES2 = aefa_engine_state:set_chain_api(API1, ES1),
            write(Arg0, Question, ES2);
        {error, What} ->
            aefa_fate:abort({primop_error, oracle_get_question, What}, ES1)
    end.

oracle_get_answer(Arg0, Arg1, Arg2, Arg3, Arg4, EngineState) ->
    {[Oracle, Query, QType, RType], ES1} =
        get_op_args([Arg1, Arg2, Arg3, Arg4], EngineState),
    if
        not ?IS_FATE_ORACLE(Oracle) ->
            aefa_fate:abort({value_does_not_match_type, Oracle, oracle}, ES1);
        not ?IS_FATE_ORACLE_Q(Query) ->
            aefa_fate:abort({value_does_not_match_type, Query, oracle_query}, ES1);
        not (?IS_FATE_TYPEREP(QType) orelse ?IS_FATE_TYPEREP(RType)) ->
            aefa_fate:abort({primop_error, oracle_get_answer, bad_type}, ES1);
        true ->
            ok
    end,
    ?FATE_ORACLE(OraclePubkey) = Oracle,
    ?FATE_ORACLE_Q(QueryId) = Query,
    API = aefa_engine_state:chain_api(ES1),
    case aefa_chain_api:oracle_get_answer(OraclePubkey, QueryId, QType, RType, API) of
        {ok, Answer, API1} ->
            ES2 = aefa_engine_state:set_chain_api(API1, ES1),
            write(Arg0, Answer, ES2);
        {error, What} ->
            aefa_fate:abort({primop_error, oracle_get_answer, What}, ES1)
    end.


oracle_query_fee(Arg0, Arg1, EngineState) ->
    case get_op_arg(Arg1, EngineState) of
        {?FATE_ORACLE(OraclePubkey), ES1} ->
            API = aefa_engine_state:chain_api(ES1),
            case aefa_chain_api:oracle_query_fee(OraclePubkey, API) of
                {ok, FeeVal, API1} ->
                    ES2 = aefa_engine_state:set_chain_api(API1, ES1),
                    write(Arg0, aeb_fate_data:make_integer(FeeVal), ES2);
                {error, What} ->
                    aefa_fate:abort({primop_error, oracle_query_fee, What}, ES1)
            end;
        {Other, ES1} ->
            aefa_fate:abort({value_does_not_match_type, Other, oracle}, ES1)
    end.

oracle_check(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    {[Oracle, QType, RType], ES1} = get_op_args([Arg1, Arg2, Arg3], EngineState),
    if
        not ?IS_FATE_ORACLE(Oracle) ->
            aefa_fate:abort({value_does_not_match_type, Oracle, oracle}, ES1);
        not (?IS_FATE_TYPEREP(QType) orelse ?IS_FATE_TYPEREP(RType)) ->
            aefa_fate:abort({primop_error, oracle_check, bad_type}, ES1);
        true ->
            ok
    end,
    ?FATE_ORACLE(OraclePubkey) = Oracle,
    API = aefa_engine_state:chain_api(ES1),
    {ok, Answer, API1} = aefa_chain_api:oracle_check(OraclePubkey, QType, RType, API),
    ES2 = aefa_engine_state:set_chain_api(API1, ES1),
    write(Arg0, Answer, ES2).

oracle_check_query(Arg0, Arg1, Arg2, Arg3, Arg4, EngineState) ->
    {[Oracle, Query, QType, RType], ES1} = get_op_args([Arg1, Arg2, Arg3, Arg4], EngineState),
    if
        not ?IS_FATE_ORACLE(Oracle) ->
            aefa_fate:abort({value_does_not_match_type, Oracle, oracle}, ES1);
        not ?IS_FATE_ORACLE_Q(Query) ->
            aefa_fate:abort({value_does_not_match_type, Query, oracle_query}, ES1);
        not (?IS_FATE_TYPEREP(QType) orelse ?IS_FATE_TYPEREP(RType)) ->
            aefa_fate:abort({primop_error, oracle_check, bad_type}, ES1);
        true ->
            ok
    end,
    ?FATE_ORACLE(OraclePubkey) = Oracle,
    ?FATE_ORACLE_Q(QueryId) = Query,
    API = aefa_engine_state:chain_api(ES1),
    {ok, Answer, API1} = aefa_chain_api:oracle_check_query(OraclePubkey, QueryId, QType, RType, API),
    ES2 = aefa_engine_state:set_chain_api(API1, ES1),
    write(Arg0, Answer, ES2).

is_oracle(Arg0, Arg1, EngineState) ->
    address_is_x(is_oracle, Arg0, Arg1, EngineState).

is_contract(Arg0, Arg1, EngineState) ->
    address_is_x(is_contract, Arg0, Arg1, EngineState).

is_payable(Arg0, Arg1, EngineState) ->
    address_is_x(is_payable, Arg0, Arg1, EngineState).

address_is_x(What, Arg0, Arg1, EngineState) ->
    {Addr, ES1} = get_op_arg(Arg1, EngineState),
    if
        not ?IS_FATE_ADDRESS(Addr) ->
            aefa_fate:abort({value_does_not_match_type, Addr, address}, ES1);
        true ->
            ok
    end,
    ?FATE_ADDRESS(Pubkey) = Addr,
    API = aefa_engine_state:chain_api(ES1),
    {ok, Answer, API1} = aefa_chain_api:What(Pubkey, API),
    ES2 = aefa_engine_state:set_chain_api(API1, ES1),
    write(Arg0, Answer, ES2).

aens_resolve(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    {[NameString, Key, Type], ES1} =
         get_op_args([Arg1, Arg2, Arg3], EngineState),
    if
        not ?IS_FATE_STRING(NameString) ->
            aefa_fate:abort({value_does_not_match_type, NameString, string}, ES1);
        not ?IS_FATE_STRING(Key) ->
            aefa_fate:abort({value_does_not_match_type, Key, string}, ES1);
        not ?IS_FATE_TYPEREP(Type) ->
            aefa_fate:abort({value_does_not_match_type, Type, typerep}, ES1);
        true ->
            ok
    end,
    aens_resolve_(Arg0, NameString, Key, Type, ES1).

aens_resolve_(Arg0, ?FATE_STRING(NameString), ?FATE_STRING(Key), ?FATE_TYPEREP(Type),
              ES) ->
    API = aefa_engine_state:chain_api(ES),
    None = aeb_fate_data:make_variant([0,1], 0, {}),
    case aefa_chain_api:aens_resolve(NameString, Key, API) of
        none ->
            write(Arg0, None, ES);
        {ok, Tag, Pubkey, API1} ->
            ES1 = aefa_engine_state:set_chain_api(API1, ES),
            case aens_tag_to_val(Type, Tag, Pubkey) of
                none ->
                    write(Arg0, None, ES1);
                {error, What} ->
                    aefa_fate:abort({primop_error, aens_resolve, What}, ES1);
                {ok, InnerVal} ->
                    Val = aeb_fate_data:make_variant([0,1], 1, {InnerVal}),
                    case aefa_fate:check_type(Type, Val) of
                        #{} ->
                            write(Arg0, Val, ES1);
                        false ->
                            write(Arg0, None, ES1)
                    end
            end
    end.

aens_tag_to_val({variant, [{tuple, []}, {tuple, [Type]}]}, Tag, Pubkey) ->
    case Type =:= string of
        true  -> {ok, ?FATE_STRING(Pubkey)};
        false ->
            case Tag of
                account  -> {ok, ?FATE_ADDRESS(Pubkey)};
                oracle   -> {ok, ?FATE_ORACLE(Pubkey)};
                contract -> {ok, ?FATE_CONTRACT(Pubkey)};
                _        -> none
            end
    end;
aens_tag_to_val(_Other,_Tag,_PubKey) ->
    {error, bad_type}.

aens_preclaim(Arg0, Arg1, Arg2, EngineState) ->
    {[Signature, Account, Hash], ES1} =
        get_op_args([Arg0, Arg1, Arg2], EngineState),
    if
        not ?IS_FATE_BYTES(64, Signature) ->
            aefa_fate:abort({value_does_not_match_type, Signature, bytes64}, ES1);
        not ?IS_FATE_BYTES(32, Hash) ->
            aefa_fate:abort({value_does_not_match_type, Hash, bytes32}, ES1);
        not ?IS_FATE_ADDRESS(Account) ->
            aefa_fate:abort({value_does_not_match_type, Account, address}, ES1);
        true ->
            ok
    end,
    ?FATE_BYTES(SignBin) = Signature,
    ?FATE_ADDRESS(Pubkey) = Account,
    ?FATE_BYTES(HashBin) = Hash,
    ES2 = check_delegation_signature(aens_preclaim, Pubkey, SignBin, ES1),
    API = aefa_engine_state:chain_api(ES2),
    case aefa_chain_api:aens_preclaim(Pubkey, HashBin, API) of
        {ok, API1} ->
            aefa_engine_state:set_chain_api(API1, ES2);
        {error, What} ->
            aefa_fate:abort({primop_error, aens_preclaim, What}, ES2)
    end.


aens_claim(Arg0, Arg1, Arg2, Arg3, Arg4, EngineState) ->
    {[Signature, Account, NameString, Salt, NameFee], ES1} =
        get_op_args([Arg0, Arg1, Arg2, Arg3, Arg4], EngineState),
    if
        not ?IS_FATE_BYTES(64, Signature) ->
            aefa_fate:abort({value_does_not_match_type, Signature, bytes64}, ES1);
        not ?IS_FATE_ADDRESS(Account) ->
            aefa_fate:abort({value_does_not_match_type, Account, address}, ES1);
        not ?IS_FATE_STRING(NameString) ->
            aefa_fate:abort({value_does_not_match_type, NameString, string}, ES1);
        not ?IS_FATE_INTEGER(Salt) ->
            aefa_fate:abort({value_does_not_match_type, Salt, integer}, ES1);
        not ?FATE_INTEGER_VALUE(Salt) >= 0 ->
            aefa_fate:abort({primop_error, aens_claim, negative_salt}, ES1);
        not ?IS_FATE_INTEGER(NameFee) ->
            aefa_fate:abort({value_does_not_match_type, NameFee, integer}, ES1);
        not ?FATE_INTEGER_VALUE(NameFee) >= 0 ->
            aefa_fate:abort({primop_error, aens_claim, negative_fee}, ES1);
        true ->
            ok
    end,
    ?FATE_BYTES(SignBin) = Signature,
    ?FATE_ADDRESS(Pubkey) = Account,
    NameBin = ?FATE_STRING_VALUE(NameString),
    case aens:get_name_hash(NameBin) of
        {error, What} ->
            aefa_fate:abort({primop_error, aens_claim, What}, ES1);
        {ok, HashBin} ->
            ES2 = check_delegation_signature(aens_claim, {Pubkey, HashBin}, SignBin, ES1),
            SaltInt = ?FATE_INTEGER_VALUE(Salt),
            NameFeeInt = ?FATE_INTEGER_VALUE(NameFee),
            API = aefa_engine_state:chain_api(ES2),
            case aefa_chain_api:aens_claim(Pubkey, NameBin, SaltInt, NameFeeInt, API) of
                {ok, DynamicGas, API1} ->
                    ES3 = spend_gas(DynamicGas, ES2),
                    aefa_engine_state:set_chain_api(API1, ES3);
                {error, What} ->
                    aefa_fate:abort({primop_error, aens_claim, What}, ES2)
            end
    end.

aens_update(_EngineState) ->
    exit({error, op_not_implemented_yet}).

aens_transfer(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    {[Signature, From, To, NameString], ES1} =
        get_op_args([Arg0, Arg1, Arg2, Arg3], EngineState),
    if
        not ?IS_FATE_BYTES(64, Signature) ->
            aefa_fate:abort({value_does_not_match_type, Signature, bytes64}, ES1);
        not ?IS_FATE_ADDRESS(From) ->
            aefa_fate:abort({value_does_not_match_type, From, address}, ES1);
        not ?IS_FATE_ADDRESS(To) ->
            aefa_fate:abort({value_does_not_match_type, To, address}, ES1);
        not ?IS_FATE_STRING(NameString) ->
            aefa_fate:abort({value_does_not_match_type, NameString, string}, ES1);
        true ->
            ok
    end,
    HashBin = hash_name(aens_transfer, NameString, ES1),
    ?FATE_BYTES(SignBin) = Signature,
    ?FATE_ADDRESS(FromPubkey) = From,
    ?FATE_ADDRESS(ToPubkey) = To,
    ES2 = check_delegation_signature(aens_transfer, {FromPubkey, HashBin}, SignBin, ES1),
    API = aefa_engine_state:chain_api(ES2),
    case aefa_chain_api:aens_transfer(FromPubkey, HashBin, ToPubkey, API) of
        {ok, API1} ->
            aefa_engine_state:set_chain_api(API1, ES2);
        {error, What} ->
            aefa_fate:abort({primop_error, aens_transfer, What}, ES2)
    end.

aens_revoke(Arg0, Arg1, Arg2, EngineState) ->
    {[Signature, Account, NameString], ES1} =
        get_op_args([Arg0, Arg1, Arg2], EngineState),
    if
        not ?IS_FATE_BYTES(64, Signature) ->
            aefa_fate:abort({value_does_not_match_type, Signature, bytes64}, ES1);
        not ?IS_FATE_ADDRESS(Account) ->
            aefa_fate:abort({value_does_not_match_type, Account, address}, ES1);
        not ?IS_FATE_STRING(NameString) ->
            aefa_fate:abort({value_does_not_match_type, NameString, name}, ES1);
        true ->
            ok
    end,
    HashBin = hash_name(aens_revoke, NameString, ES1),
    ?FATE_BYTES(SignBin) = Signature,
    ?FATE_ADDRESS(Pubkey) = Account,
    ES2 = check_delegation_signature(aens_revoke, {Pubkey, HashBin}, SignBin, ES1),
    API = aefa_engine_state:chain_api(ES2),
    case aefa_chain_api:aens_revoke(Pubkey, HashBin, API) of
        {ok, API1} ->
            aefa_engine_state:set_chain_api(API1, ES2);
        {error, What} ->
            aefa_fate:abort({primop_error, aens_revoke, What}, ES2)
    end.

hash_name(Primop, NameString, ES) ->
    case aens_utils:to_ascii(NameString) of
        {ok, NameAscii} ->
            aens_hash:name_hash(NameAscii);
        {error, What} ->
            aefa_fate:abort({primop_error, Primop, What}, ES)
    end.

check_delegation_signature(Type, Data, Signature, ES) ->
    Current = aefa_engine_state:current_contract(ES),
    check_delegation_signature(Type, Data, Signature, Current, ES).

check_delegation_signature(Type, Data, SignBin, Current, ES0) ->
    {Bin, Pubkey} = delegation_signature_data(Type, Data, Current),
    case Pubkey =:= Current of
        true ->
            ES0;
        false ->
            %% Charge gas for the extra work of verifying the signature
            VerifyOp = aeb_fate_opcodes:m_to_op('VERIFY_SIG'),
            ES = spend_gas(aeb_fate_opcodes:gas_cost(VerifyOp), ES0),
            API = aefa_engine_state:chain_api(ES),
            case aefa_chain_api:check_delegation_signature(Pubkey, Bin, SignBin, API) of
                {ok, API1} ->
                    aefa_engine_state:set_chain_api(API1, ES);
                error ->
                    aefa_fate:abort({primop_error, Type, bad_signature}, ES)
            end
    end.

delegation_signature_data(Type, Pubkey, Current) when Type =:= aens_preclaim;
                                                      Type =:= oracle_register;
                                                      Type =:= oracle_extend ->
    {<<Pubkey/binary, Current/binary>>, Pubkey};
delegation_signature_data(oracle_respond, {Pubkey, QueryId}, Current) ->
    {<<QueryId/binary, Current/binary>>, Pubkey};
delegation_signature_data(Type, {Pubkey, Hash}, Current) when Type =:= aens_claim;
                                                              Type =:= aens_transfer;
                                                              Type =:= aens_revoke ->
    {<<Pubkey/binary, Hash/binary, Current/binary>>, Pubkey}.

spend_gas(Delta, ES) when is_integer(Delta), Delta > 0 ->
    aefa_engine_state:spend_gas(Delta, ES).

spend_tuple_gas(TupleSize, ES) ->
    aefa_engine_state:spend_gas_for_new_cells(TupleSize + 2, ES).

verify_sig(Arg0, Arg1, Arg2, Arg3, ES) ->
    ter_op(verify_sig, {Arg0, Arg1, Arg2, Arg3}, ES).

verify_sig_secp256k1(Arg0, Arg1, Arg2, Arg3, ES) ->
    ter_op(verify_sig_secp256k1, {Arg0, Arg1, Arg2, Arg3}, ES).

ecverify_secp256k1(Arg0, Arg1, Arg2, Arg3, ES) ->
    ter_op(ecverify_secp256k1, {Arg0, Arg1, Arg2, Arg3}, ES).

ecrecover_secp256k1(Arg0, Arg1, Arg2, ES) ->
    bin_op(ecrecover_secp256k1, {Arg0, Arg1, Arg2}, ES).

contract_to_address(Arg0, Arg1, ES) ->
    un_op(contract_to_address, {Arg0, Arg1}, ES).

address_to_contract(Arg0, Arg1, ES) ->
    un_op(address_to_contract, {Arg0, Arg1}, ES).

sha3(Arg0, Arg1, EngineState) ->
    un_op(sha3, {Arg0, Arg1}, EngineState).

sha256(Arg0, Arg1, EngineState) ->
    un_op(sha256, {Arg0, Arg1}, EngineState).

blake2b(Arg0, Arg1, EngineState) ->
    un_op(blake2b, {Arg0, Arg1}, EngineState).

-spec abort(_, _) -> no_return().
abort(Arg0, EngineState) ->
    {Value, ES1} = get_op_arg(Arg0, EngineState),
    case ?IS_FATE_STRING(Value) of
        true  -> aefa_fate:runtime_revert(Value, ES1);
        false -> aefa_fate:abort({value_does_not_match_type, Value, string}, ES1)
    end.

-spec exit(_, _) -> no_return().
exit(Arg0, EngineState) ->
    {Value, ES1} = get_op_arg(Arg0, EngineState),
    case ?IS_FATE_STRING(Value) of
        true  -> aefa_fate:runtime_exit(?FATE_STRING_VALUE(Value), ES1);
        false -> aefa_fate:abort({value_does_not_match_type, Value, string}, ES1)
    end.

nop(EngineState) ->
    EngineState.

%% ------------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------------

%% Guarded ops
gop(Op, Arg, ES) ->
    try op(Op, Arg) of
        Res -> Res
    catch
        {add_engine_state, E} -> aefa_fate:abort(E, ES)
    end.

gop(Op, Arg1, Arg2, ES) ->
    try op(Op, Arg1, Arg2) of
        Res -> Res
    catch
        {add_engine_state, E} -> aefa_fate:abort(E, ES)
    end.

gop(Op, Arg1, Arg2, Arg3, ES) ->
    try op(Op, Arg1, Arg2, Arg3) of
        Res -> Res
    catch
        {add_engine_state, E} -> aefa_fate:abort(E, ES)
    end.


un_op(Op, {To, What}, ES) ->
    {Value, ES1} = get_op_arg(What, ES),
    Result = gop(Op, Value, ES1),
    write(To, Result, ES1).

bin_op(Op, {To, Left, Right}, ES) ->
    {LeftValue, ES1} = get_op_arg(Left, ES),
    {RightValue, ES2} = get_op_arg(Right, ES1),
    Result = gop(Op, LeftValue, RightValue, ES2),
    write(To, Result, ES2).

ter_op(Op, {To, One, Two, Three}, ES) ->
    {ValueOne, ES1} = get_op_arg(One, ES),
    {ValueTwo, ES2} = get_op_arg(Two, ES1),
    {ValueThree, ES3} = get_op_arg(Three, ES2),
    Result = gop(Op, ValueOne, ValueTwo, ValueThree, ES3),
    write(To, Result, ES3).

get_op_args([H|T], ES) ->
    {X, ES1} = get_op_arg(H, ES),
    {Xs, ES2} = get_op_args(T, ES1),
    {[X|Xs], ES2};
get_op_args([], ES) ->
    {[], ES}.

get_op_arg({stack, 0}, ES) ->
    aefa_engine_state:pop_accumulator(ES);
get_op_arg({arg,_N} = Var, ES) ->
    aefa_fate:lookup_var(Var, ES);
get_op_arg({var,_N} = Var, ES) ->
    aefa_fate:lookup_var(Var, ES);
get_op_arg({immediate, X}, ES) ->
    {X, ES}.

write({stack, 0}, Val, ES) ->
    aefa_engine_state:push_accumulator(Val, ES);
write({var, _} = Name,  Val, ES) ->
    aefa_fate:store_var(Name, Val, ES);
write({arg, _} = Arg, Val, ES) ->
    aefa_fate:store_var(Arg, Val, ES).


%% ------------------------------------------------------
%% Variant instructions
%% ------------------------------------------------------


make_variant(Arities, Tag, NoElements, ES)  when ?IS_FATE_LIST(Arities)
                                              , ?IS_FATE_INTEGER(Tag)
                                              , ?IS_FATE_INTEGER(NoElements)
                                              , NoElements >= 0
                                              , Tag < length(?FATE_LIST_VALUE(Arities))
                                              , Tag >= 0 ->
    {Elements, ES2} = aefa_fate:pop_n(NoElements, ES),
    Values = list_to_tuple(Elements),
    Cells = length(?FATE_LIST_VALUE(Arities)) * 2 + NoElements + 4,
    ES3 = aefa_engine_state:spend_gas_for_new_cells(Cells, ES2),
    {aeb_fate_data:make_variant(Arities, Tag, Values), ES3};
make_variant(Arities, Tag, NoElements, ES) ->
    aefa_fate:abort({bad_arguments_to_make_variant, Arities, Tag, NoElements}, ES).


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
        [] -> aefa_fate:abort(hd_on_empty_list);
        [Hd|_] -> Hd
    end;
op(is_nil, A) when ?IS_FATE_LIST(A) ->
    aeb_fate_data:make_boolean(?FATE_LIST_VALUE(A) =:= []);
op(tl, A) when ?IS_FATE_LIST(A) ->
    case ?FATE_LIST_VALUE(A) of
        [] -> aefa_fate:abort(tl_on_empty_list);
        [_|Tl] -> Tl
    end;
op(length, A) when ?IS_FATE_LIST(A) ->
    aeb_fate_data:make_integer(length(?FATE_LIST_VALUE(A)));
op(int_to_str, A) when ?IS_FATE_INTEGER(A) ->
    aeb_fate_data:make_string(integer_to_binary(?FATE_INTEGER_VALUE(A)));
op(int_to_addr, A) when ?IS_FATE_INTEGER(A) ->
    aeb_fate_data:make_address(<<A:256>>);
op(addr_to_str, A) when ?IS_FATE_ADDRESS(A) ->
    Val = ?FATE_ADDRESS_VALUE(A),
    aeser_api_encoder:encode(account_pubkey, Val);
op(str_reverse, A) when ?IS_FATE_STRING(A) ->
    aeb_fate_data:make_string(binary_reverse(?FATE_STRING_VALUE(A)));
op(str_length, A) when ?IS_FATE_STRING(A) ->
    aeb_fate_data:make_integer(byte_size(?FATE_STRING_VALUE(A)));
op(bits_all, N)  when ?IS_FATE_INTEGER(N) ->
    ?FATE_BITS((1 bsl (N)) - 1);
op(bits_sum, A)  when ?IS_FATE_BITS(A) ->
    ?FATE_BITS(Bits) = A,
    if Bits < 0 -> aefa_fate:abort({arithmetic_error, bits_sum_on_infinite_set});
       true -> bits_sum(Bits, 0)
    end;
op(bytes_to_int, ?FATE_BYTES(Bin)) ->
    N = byte_size(Bin),
    <<Val:N/unit:8>> = Bin,
    Val;
op(bytes_to_str, ?FATE_BYTES(Bin)) ->
    Str = list_to_binary(aeu_hex:bin_to_hex(Bin)),
    ?FATE_STRING(Str);
op(sha3, A) ->
    Bin  = binary_for_hashing(A),
    Hash = aec_hash:hash(evm, Bin),
    ?FATE_BYTES(Hash);
op(sha256, A) ->
    Bin  = binary_for_hashing(A),
    Hash = aec_hash:sha256_hash(Bin),
    ?FATE_BYTES(Hash);
op(blake2b, A) ->
    Bin  = binary_for_hashing(A),
    Hash = aec_hash:blake2b_256_hash(Bin),
    ?FATE_BYTES(Hash);
op(contract_to_address, A) when ?IS_FATE_CONTRACT(A) ->
    ?FATE_ADDRESS(?FATE_CONTRACT_VALUE(A));
op(address_to_contract, A) when ?IS_FATE_ADDRESS(A) ->
    ?FATE_CONTRACT(?FATE_ADDRESS_VALUE(A)).

binary_for_hashing(S) when ?IS_FATE_STRING(S) ->
    ?FATE_STRING_VALUE(S);  %% Makes Crypto.sha3 and String.sha3 coincide.
binary_for_hashing(?FATE_BYTES(Bin)) ->
    Bin;    %% To make it possible to predict the result for the user.
binary_for_hashing(X) ->
    aeb_fate_encoding:serialize(X).

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
    if B =:= 0 -> aefa_fate:abort(division_by_zero);
       true -> A div B
    end;
op(mod, A, B)  when ?IS_FATE_INTEGER(A)
                    , ?IS_FATE_INTEGER(B) ->
    if B =:= 0 -> aefa_fate:abort(mod_by_zero);
       true -> A rem B
    end;
op('and', A, B)  when ?IS_FATE_BOOLEAN(A)
                    , ?IS_FATE_BOOLEAN(B) ->
    A and B;
op('or', A, B)  when ?IS_FATE_BOOLEAN(A)
                    , ?IS_FATE_BOOLEAN(B) ->
    A or B;
op(map_delete, Map, Key) when ?IS_FATE_MAP(Map),
                              not ?IS_FATE_MAP(Key) ->
    maps:remove(Key, ?FATE_MAP_VALUE(Map));
op(map_delete, ?FATE_STORE_MAP(Cache, Id), Key) ->
    ?FATE_STORE_MAP(Cache#{ Key => ?FATE_MAP_TOMBSTONE }, Id);
op(cons, Hd, Tail) when ?IS_FATE_LIST(Tail) ->
    case ?FATE_LIST_VALUE(Tail) of
        [] -> aeb_fate_data:make_list([Hd|?FATE_LIST_VALUE(Tail)]);
        [OldHd|_] = Tail ->
            case aefa_fate:terms_are_of_same_type(OldHd, Hd) of
                true ->
                    aeb_fate_data:make_list([Hd|?FATE_LIST_VALUE(Tail)]);
                false ->
                    aefa_fate:abort({type_error, cons})
            end
    end;
op(append, A, B) when ?IS_FATE_LIST(A), ?IS_FATE_LIST(B) ->
    aeb_fate_data:make_list(?FATE_LIST_VALUE(A) ++ ?FATE_LIST_VALUE(B));
op(str_join, A, B) when ?IS_FATE_STRING(A)
                         , ?IS_FATE_STRING(B) ->
    aeb_fate_data:make_string(<<?FATE_STRING_VALUE(A)/binary,
                            ?FATE_STRING_VALUE(B)/binary>>);
op(bytes_concat, ?FATE_BYTES(A), ?FATE_BYTES(B)) ->
    ?FATE_BYTES(<<A/binary, B/binary>>);
op(bytes_split, ?FATE_BYTES(A), B) when ?IS_FATE_INTEGER(B) ->
    N = ?FATE_INTEGER_VALUE(B),
    case A of
        <<L:N/binary, R/binary>> -> ?FATE_TUPLE({?FATE_BYTES(L), ?FATE_BYTES(R)});
        _                        -> aefa_fate:abort({type_error, bytes_split})
    end;
op(variant_test, A, B)  when ?IS_FATE_VARIANT(A)
                         , ?IS_FATE_INTEGER(B)
                         , B >= 0 ->
    ?FATE_VARIANT(_S, T,_Values) = A,
    aeb_fate_data:make_boolean(T =:= B);
op(variant_element, A, B)  when ?IS_FATE_VARIANT(A)
                                , ?IS_FATE_INTEGER(B)
                                , B >= 0 ->
    ?FATE_VARIANT(_S, _T, Values) = A,
    if size(Values) > B ->
            element(B + 1, Values);
       true ->
            aefa_fate:abort({type_error, variant_element, B, A})
    end;

op(bits_set, A, B)  when ?IS_FATE_BITS(A), ?IS_FATE_INTEGER(B) ->
    if B < 0 -> aefa_fate:abort({arithmetic_error, negative_bit_position});
       true ->
            ?FATE_BITS(Bits) = A,
            ?FATE_BITS(Bits bor (1 bsl B))
    end;
op(bits_clear, A, B)  when ?IS_FATE_BITS(A), ?IS_FATE_INTEGER(B) ->
    if B < 0 -> aefa_fate:abort({arithmetic_error, negative_bit_position});
       true ->
            ?FATE_BITS(Bits) = A,
            ?FATE_BITS(Bits band (bnot (1 bsl B)))
    end;
op(bits_test, A, B)  when ?IS_FATE_BITS(A), ?IS_FATE_INTEGER(B) ->
    if B < 0 -> aefa_fate:abort({arithmetic_error, negative_bit_position});
       true ->
            ?FATE_BITS(Bits) = A,
            ((Bits band (1 bsl B)) > 0)
    end;
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
    ?FATE_BITS((BitsA band BitsB) bxor BitsA);
op(ecrecover_secp256k1, Msg, Sig) when ?IS_FATE_BYTES(32, Msg)
                                     , ?IS_FATE_BYTES(65, Sig) ->
    {?FATE_BYTES(Msg1), ?FATE_BYTES(Sig1)} = {Msg, Sig},
    case aeu_crypto:ecrecover(secp256k1, Msg1, Sig1) of
        false      -> aeb_fate_data:make_variant([0, 1], 0, {});
        {ok, Addr} -> aeb_fate_data:make_variant([0, 1], 1, {?FATE_BYTES(Addr)})
    end.

%% Terinay operations
op(map_update, Map, Key, Value) when ?IS_FATE_MAP(Map),
                              not ?IS_FATE_MAP(Key) ->
    Res = maps:put(Key, Value, ?FATE_MAP_VALUE(Map)),
    aeb_fate_data:make_map(Res);
op(map_update, ?FATE_STORE_MAP(Cache, Id), Key, Value) ->
    ?FATE_STORE_MAP(Cache#{ Key => Value }, Id);
op(verify_sig, Msg, PK, Sig) when ?IS_FATE_BYTES(32, Msg)
                                , ?IS_FATE_ADDRESS(PK)
                                , ?IS_FATE_BYTES(64, Sig) ->
    {?FATE_BYTES(Msg1), ?FATE_ADDRESS(PK1), ?FATE_BYTES(Sig1)} = {Msg, PK, Sig},
    aeu_crypto:verify_sig(Msg1, PK1, Sig1);
op(verify_sig_secp256k1, Msg, PK, Sig) when ?IS_FATE_BYTES(32, Msg)
                                          , ?IS_FATE_BYTES(64, PK)
                                          , ?IS_FATE_BYTES(64, Sig) ->
    {?FATE_BYTES(Msg1), ?FATE_BYTES(PK1), ?FATE_BYTES(Sig1)} = {Msg, PK, Sig},
    aeu_crypto:verify_sig(secp256k1, Msg1, PK1, Sig1);
op(ecverify_secp256k1, Msg, PK, Sig) when ?IS_FATE_BYTES(32, Msg)
                                        , ?IS_FATE_BYTES(20, PK)
                                        , ?IS_FATE_BYTES(65, Sig) ->
    {?FATE_BYTES(Msg1), ?FATE_BYTES(PK1), ?FATE_BYTES(Sig1)} = {Msg, PK, Sig},
    aeu_crypto:ecverify(secp256k1, Msg1, PK1, Sig1).


bits_sum(0, Sum) -> Sum;
bits_sum(N, Sum) -> bits_sum(N bsr 1, Sum + (N band 2#1)).

store_map_lookup(Cache, MapId, Key, ES) ->
    case maps:get(Key, Cache, void) of
        ?FATE_MAP_TOMBSTONE -> {error, ES};
        void ->
            Pubkey        = aefa_engine_state:current_contract(ES),
            {Store, ES1}  = aefa_fate:ensure_contract_store(Pubkey, ES),
            {Res, Store1} = aefa_stores:store_map_lookup(Pubkey, MapId, Key, Store),
            ES2           = aefa_engine_state:set_stores(Store1, ES1),
            {Res, ES2};
        Val -> {{ok, Val}, ES}
    end.

store_map_member(Cache, MapId, Key, ES) ->
    case maps:get(Key, Cache, void) of
        ?FATE_MAP_TOMBSTONE -> {?FATE_FALSE, ES};
        void ->
            Pubkey        = aefa_engine_state:current_contract(ES),
            {Store, ES1}  = aefa_fate:ensure_contract_store(Pubkey, ES),
            {Res, Store1} = aefa_stores:store_map_member(Pubkey, MapId, Key, Store),
            ES2           = aefa_engine_state:set_stores(Store1, ES1),
            {aeb_fate_data:make_boolean(Res), ES2};
        _Val -> {?FATE_TRUE, ES}
    end.

store_map_size(Cache, MapId, ES) ->
    Pubkey         = aefa_engine_state:current_contract(ES),
    {Store, ES1}   = aefa_fate:ensure_contract_store(Pubkey, ES),
    {Size, Store1} = aefa_stores:store_map_size(Pubkey, MapId, Store),
    Delta  = fun(Key, ?FATE_MAP_TOMBSTONE, N) ->
                     case aefa_stores:store_map_member(Pubkey, MapId, Key, Store1) of
                         {false, _Store} -> N;
                         {true,  _Store} -> N - 1
                     end;
                (Key, _, N) ->
                     case aefa_stores:store_map_member(Pubkey, MapId, Key, Store) of
                         {false, _Store} -> N + 1;
                         {true,  _Store} -> N
                     end
             end,
    ES2 = aefa_engine_state:set_stores(Store1, ES1),
    {maps:fold(Delta, Size, Cache), ES2}.

store_map_to_list(Cache, MapId, ES) ->
    Pubkey              = aefa_engine_state:current_contract(ES),
    {Store, ES1}        = aefa_fate:ensure_contract_store(Pubkey, ES),
    {StoreList, Store1} = aefa_stores:store_map_to_list(Pubkey, MapId, Store),
    StoreMap            = maps:from_list(StoreList),
    Upd = fun(Key, ?FATE_MAP_TOMBSTONE, M) -> maps:remove(Key, M);
             (Key, Val, M)                 -> maps:put(Key, Val, M) end,
    Map = maps:fold(Upd, StoreMap, Cache),
    ES2 = aefa_engine_state:set_stores(Store1, ES1),
    {aeb_fate_data:make_list([ ?FATE_TUPLE(KV) || KV <- maps:to_list(Map) ]), ES2}.

%% ------------------------------------------------------
%% Comparison instructions
%% ------------------------------------------------------

bin_comp(Comp, {To, Left, Right}, ES) ->
    {LeftValue,   ES1} = get_op_arg(Left, ES),
    {LeftValue1,  ES2} = aefa_fate:unfold_store_maps(LeftValue, ES1),
    {RightValue,  ES3} = get_op_arg(Right, ES2),
    {RightValue1, ES4} = aefa_fate:unfold_store_maps(RightValue, ES3),
    Result = comp(Comp, LeftValue1, RightValue1),
    write(To, Result, ES4).

comp( lt, A, B) -> A < B;
comp( gt, A, B) -> A > B;
comp(elt, A, B) -> A =< B;
comp(egt, A, B) -> A >= B;
comp( eq, A, B) -> A =:= B;
comp(neq, A, B) -> A =/= B.



binary_reverse(Binary) ->
    Size = erlang:size(Binary)*8,
    <<X:Size/integer-little>> = Binary,
    <<X:Size/integer-big>>.

pow(A, B, ES) ->
    power(A, B, 1, ES).

power(_, 0, R, ES)                    -> {R, ES};
power(A, B, R, ES) when B rem 2 == 0 ->
    Words1 = words_used(A),
    Words2 = Words1 * 2,
    ES1 = aefa_engine_state:spend_gas_for_new_cells(Words2, ES),
    power(A * A, B bsr 1, R, ES1);
power(A, B, R, ES)                   ->
    Words1 = words_used(R),
    Words2 = words_used(A),
    Words3 = Words1 + Words2 * 3,
    ES1 = aefa_engine_state:spend_gas_for_new_cells(Words3, ES),
    Res = R * A,
    power(A * A, B bsr 1, Res, ES1).


words_used(0) -> 1;
words_used(I) when is_integer(I) ->
    A = abs(I),
    shift_word(A, 0).

shift_word(0, N) -> N;
shift_word(A, N) ->
    shift_word(A bsr 64, N + 1).

