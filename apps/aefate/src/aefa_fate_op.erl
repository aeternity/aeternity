%% Implements Fate operations.
%%
-module(aefa_fate_op).

-export([ return/1
        , returnr/2
        , call/2
        , call_r/6
        , call_t/2
        , call_gr/7
        , call_pgr/8
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
        , str_to_list/3
        , str_from_list/3
        , str_to_lower/3
        , str_to_upper/3
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
        , char_to_int/3
        , char_from_int/3
        , address/2
        , contract_creator/2
        , balance/2
        , balance_other/3
        , origin/2
        , caller/2
        , gasprice/2
        , fee/2
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
        , create/4
        , clone/5
        , clone_g/6
        , bytecode_hash/3
        , oracle_register/8
        , oracle_query/9
        , oracle_respond/7
        , oracle_extend/4
        , oracle_get_answer/6
        , oracle_get_question/6
        , oracle_expiry/3
        , oracle_query_fee/3
        , oracle_check/5
        , oracle_check_query/6
        , is_oracle/3
        , is_contract/3
        , is_payable/3
        , aens_resolve/5
        , aens_preclaim/4
        , aens_claim/6
        , aens_update/7
        , aens_transfer/5
        , aens_revoke/4
        , aens_lookup/3
        , verify_sig/5
        , verify_sig_secp256k1/5
        , ecverify_secp256k1/5
        , ecrecover_secp256k1/4
        , bls12_381_g1_neg/3
        , bls12_381_g1_norm/3
        , bls12_381_g1_valid/3
        , bls12_381_g1_is_zero/3
        , bls12_381_g1_add/4
        , bls12_381_g1_mul/4
        , bls12_381_g2_neg/3
        , bls12_381_g2_norm/3
        , bls12_381_g2_valid/3
        , bls12_381_g2_is_zero/3
        , bls12_381_g2_add/4
        , bls12_381_g2_mul/4
        , bls12_381_gt_inv/3
        , bls12_381_gt_add/4
        , bls12_381_gt_mul/4
        , bls12_381_gt_pow/4
        , bls12_381_gt_is_one/3
        , bls12_381_pairing/4
        , bls12_381_miller_loop/4
        , bls12_381_final_exp/3
        , bls12_381_int_to_fr/3
        , bls12_381_int_to_fp/3
        , bls12_381_fr_to_int/3
        , bls12_381_fp_to_int/3
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
        , auth_tx/2
        , auth_tx_hash/2
        , bytes_to_int/3
        , bytes_to_str/3
        , bytes_concat/4
        , bytes_split/4
        , load_pre_iris_map_ordering/0
        ]).

-include_lib("aebytecode/include/aeb_fate_data.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include("../../aecore/include/blocks.hrl").
-include("../../aecontract/include/hard_forks.hrl").

-define(AVAILABLE_FROM(When, ES),
        aefa_engine_state:vm_version(ES) >= When
        orelse aefa_fate:abort({primop_error, ?FUNCTION_NAME, not_supported}, ES)).
-define(UNAVAILABLE_FROM(When, ES),
        aefa_engine_state:vm_version(ES) < When
        orelse aefa_fate:abort({primop_error, ?FUNCTION_NAME, not_supported}, ES)).
-define(PRE_IRIS_MAP_ORDERING, {?MODULE, pre_iris_map_ordering}).

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
    ES1 = push(Arg0, EngineState),
    aefa_fate:pop_call_stack(ES1).

call(Arg0, EngineState) ->
    {Fun, ES1}  = get_op_arg(Arg0, EngineState),
    Signature   = aefa_fate:get_function_signature(Fun, ES1),
    {Args, ES2} = aefa_fate:pop_args_from_signature(Signature, ES1),
    ES3         = aefa_fate:push_return_address(ES2),
    ES4         = aefa_fate:bind_args(Args, ES3),
    AllowInit = case aefa_engine_state:vm_version(ES3) of
                    ?VM_FATE_SOPHIA_1 -> true;
                    _ -> false
                end,
    {jump, 0, aefa_fate:set_local_function(Fun, AllowInit, ES4)}.

call_t(Arg0, EngineState) ->
    {Fun, ES1}  = get_op_arg(Arg0, EngineState),
    Signature   = aefa_fate:get_function_signature(Fun, ES1),
    {Args, ES2} = aefa_fate:pop_args_from_signature(Signature, ES1),
    ES3         = aefa_fate:bind_args(Args, ES2),
    AllowInit = case aefa_engine_state:vm_version(ES3) of
                    ?VM_FATE_SOPHIA_1 -> true;
                    _ -> false
                end,
    {jump, 0, aefa_fate:set_local_function(Fun, AllowInit, ES3)}.

call_r(Arg0, Arg1, Arg2, Arg3, Arg4, EngineState) ->
    {[Contract, ArgType, RetType, Value], ES1} = get_op_args([Arg0, Arg2, Arg3, Arg4], EngineState),
    {ok, ES2} = remote_call_common(Contract, Arg1, ArgType, RetType, Value, no_gas_cap, unprotected, ES1),
    {jump, 0, ES2}.

call_gr(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, EngineState) ->
    {[Contract, ArgType, RetType, Value, GasCap], ES1} = get_op_args([Arg0, Arg2, Arg3, Arg4, Arg5], EngineState),
    {ok, ES2} = remote_call_common(Contract, Arg1, ArgType, RetType, Value, {gas_cap, GasCap}, unprotected, ES1),
    {jump, 0, ES2}.

call_pgr(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, EngineState) ->
    ?AVAILABLE_FROM(?VM_FATE_SOPHIA_2, EngineState),
    {[Contract, ArgType, RetType, Value, GasCap, Prot], ES1} = get_op_args([Arg0, Arg2, Arg3, Arg4, Arg5, Arg6], EngineState),
    Protected =
        case Prot of
            false -> unprotected;
            true  -> protected;
            _     -> aefa_fate:abort({value_does_not_match_type, Prot, bool}, EngineState)
        end,
    case remote_call_common(Contract, Arg1, ArgType, RetType, Value, {gas_cap, GasCap}, Protected, ES1) of
        {ok, ES2} ->
            {jump, 0, ES2};
        {failed_protected_call, ES2} ->
            {next, push({immediate, make_none()}, ES2)}
    end.
remote_call_common(Contract, Function, ArgTypes, RetType, Value, GasCap, Protected, EngineState) ->
    remote_call_common(Contract, Function, ArgTypes, RetType, Value, GasCap, Protected, identity, EngineState).
remote_call_common(Contract, Function, ?FATE_TYPEREP({tuple, ArgTypes}), ?FATE_TYPEREP(RetType), Value, GasCap, Protected, Continuation, EngineState) ->
    Current      = aefa_engine_state:current_contract(EngineState),
    Caller       = aeb_fate_data:make_address(Current),
    Arity        = length(ArgTypes),
    {Args0, ES1} = aefa_fate:pop_args(Arity, EngineState),
    {Args, ES2}  = aefa_fate:unfold_store_maps(Args0, ES1, unfold),
    protect(Protected, fun() ->
            ES3       = aefa_fate:push_return_address(ES2),
            ES4       = case GasCap of
                            no_gas_cap        -> ES3;
                            {gas_cap, Cap} -> aefa_fate:push_gas_cap(Cap, ES3)
                        end,
            ES5       = aefa_fate:check_remote(Contract, ES4),
            {FunName, AllowInit} = case Function of
                                       init -> {?FATE_INIT_ID, true};
                                       _ -> {Function, false}
                                   end,
            ES6       = aefa_fate:set_remote_function(Caller, Contract, FunName, Value > 0, true, AllowInit, ES5),
            Signature = aefa_fate:get_function_signature(FunName, ES6),
            ES7       = aefa_fate:check_signature(Args, Signature, ES6),
            TVars     = aefa_engine_state:current_tvars(ES7),
            ES8       = case Continuation of
                            identity -> ES7;
                            _ when is_function(Continuation, 1) ->
                                aefa_fate:push_continuation(Continuation, ES7)
                        end,
            ES9       = aefa_fate:push_return_type_check(Signature, {ArgTypes, RetType}, TVars, Protected, ES8),
            ES10      = aefa_fate:bind_args(Args, ES9),
            {ok, transfer_value(Current, Contract, Value, ES10)}
        end, fun() -> {failed_protected_call, ES2} end).

protect(unprotected, Action, _) -> Action();
protect(protected, Action, Recover) ->
    try Action() catch _:_ -> Recover() end.

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
    Protocol = aefa_engine_state:consensus_version(EngineState),
    case Value of
        true -> {jump, Arg1, ES1};
        false -> {next, ES1};
        _ when Protocol >= ?IRIS_PROTOCOL_VSN ->
            aefa_fate:abort({value_does_not_match_type, Value, boolean}, ES1)
    end.

switch(Arg0, Arg1, Arg2, EngineState) ->
    {Value, ES1} = get_op_arg(Arg0, EngineState),
    Protocol = aefa_engine_state:consensus_version(EngineState),
    if ?IS_FATE_VARIANT(Value) ->
            ?FATE_VARIANT(Arities, Tag, _T) = Value,
            if length(Arities) =:= 2 ->
                    %% Tag can only be 0 or 1 or the variant is broken.
                    case Tag of
                        0 -> {jump, Arg1, ES1};
                        1 -> {jump, Arg2, ES1};
                        _ when Protocol >= ?IRIS_PROTOCOL_VSN ->
                            aefa_fate:abort({bad_variant_tag, Tag}, ES1)
                    end;
               true -> aefa_fate:abort({bad_variant_size, length(Arities)}, ES1)
            end;
       true -> aefa_fate:abort({value_does_not_match_type, Value, variant}, ES1)
    end.

switch(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    {Value, ES1} = get_op_arg(Arg0, EngineState),
    Protocol = aefa_engine_state:consensus_version(EngineState),
    if ?IS_FATE_VARIANT(Value) ->
            ?FATE_VARIANT(Arities, Tag, _T) = Value,
            if length(Arities) =:= 3 ->
                    %% Tag can only be 0, 1 or 2 or the variant is broken.
                    case Tag of
                        0 -> {jump, Arg1, ES1};
                        1 -> {jump, Arg2, ES1};
                        2 -> {jump, Arg3, ES1};
                        _ when Protocol >= ?IRIS_PROTOCOL_VSN ->
                            aefa_fate:abort({bad_variant_tag, Tag}, ES1)
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
    inc({stack, 0}, EngineState).

inc(Arg0, EngineState) ->
    {Val, ES1} = get_op_arg(Arg0, EngineState),
    case ?IS_FATE_INTEGER(Val) orelse
         aefa_engine_state:consensus_version(ES1) < ?IRIS_PROTOCOL_VSN of
        true ->
            write(Arg0, Val + 1, ES1);
        false ->
            aefa_fate:abort({value_does_not_match_type, Val, integer}, ES1)
    end.

dec(EngineState) ->
    dec({stack, 0}, EngineState).

dec(Arg0, EngineState) ->
    {Val, ES1} = get_op_arg(Arg0, EngineState),
    case ?IS_FATE_INTEGER(Val) orelse
         aefa_engine_state:consensus_version(ES1) < ?IRIS_PROTOCOL_VSN of
        true ->
            write(Arg0, Val - 1, ES1);
        false ->
            aefa_fate:abort({value_does_not_match_type, Val, integer}, ES1)
    end.

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
    {Val, ES1} = get_op_arg(Arg1, EngineState),
    case ?IS_FATE_BOOLEAN(Val) orelse
         aefa_engine_state:consensus_version(ES1) < ?IRIS_PROTOCOL_VSN of
        true ->
            write(Arg0, not Val, ES1);
        false ->
            aefa_fate:abort({value_does_not_match_type, Val, boolean}, ES1)
    end.

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
        false -> aefa_fate:abort({type_error, element, [Index, FateTuple]}, ES);
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
    map_check(map_lookup, Map, EngineState),
    {Result, ES2} = map_lookup1(Key, Map, ES1),
    case Result of
        error     -> aefa_fate:abort(missing_map_key, ES2);
        {ok, Val} -> write(Arg0, Val, ES2)
    end.

map_lookup(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    {[Map, Key, Default], ES1} = get_op_args([Arg1, Arg2, Arg3], EngineState),
    map_check(map_lookupd, Map, EngineState),
    {Result, ES2} = map_lookup1(Key, Map, ES1),
    case Result of
        error     -> write(Arg0, Default, ES2);
        {ok, Val} -> write(Arg0, Val, ES2)
    end.

map_member(Arg0, Arg1, Arg2, EngineState) ->
    {[Map, Key], ES1} = get_op_args([Arg1, Arg2], EngineState),
    map_check(map_member, Map, EngineState),
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
    map_check(map_to_list, Map, EngineState),
    case Map of
        _ when ?IS_FATE_MAP(Map) ->
            List = map_to_sorted_list(Map, ES1),
            ES2 = write(Arg0, List, ES1),
            Size = map_size(?FATE_MAP_VALUE(Map)),
            aefa_engine_state:spend_gas_for_new_cells(Size * 2, ES2);
        ?FATE_STORE_MAP(Cache, MapId) ->
            {CleanMap, ES2} = store_map_get_clean(Cache, MapId, ES1),
            List = map_to_sorted_list(CleanMap, ES2),
            ES3 = write(Arg0, List, ES2),
            Size = length(?FATE_LIST_VALUE(List)),
            aefa_engine_state:spend_gas_for_new_cells(Size * 2, ES3)
    end.

map_to_sorted_list(Map, ES) ->
    List = maps:to_list(?FATE_MAP_VALUE(Map)),
    ConsensusVersion = aefa_engine_state:consensus_version(ES),
    Tuples = if
        ConsensusVersion < ?IRIS_PROTOCOL_VSN ->
            MapOrdering = persistent_term:get(?PRE_IRIS_MAP_ORDERING),
            case maps:get(?FATE_MAP_VALUE(Map), MapOrdering, default) of
                default ->
                    [aeb_fate_data:make_tuple(KV)
                     || KV <- List];
                Ordering -> Ordering
            end;
        true ->
            [aeb_fate_data:make_tuple(KV)
             || KV <- lists:sort(fun ({K1,_}, {K2,_}) -> aeb_fate_data:lt(K1, K2) end, List)]
    end,
    aeb_fate_data:make_list(Tuples).

load_pre_iris_map_ordering() ->
    MapOrdering = aec_fork_block_settings:pre_iris_map_ordering(),
    persistent_term:put(?PRE_IRIS_MAP_ORDERING, MapOrdering).

map_size_(Arg0, Arg1, EngineState) ->
    {Map, ES1} = get_op_arg(Arg1, EngineState),
    map_check(map_size, Map, EngineState),
    case Map of
        _ when ?IS_FATE_MAP(Map) ->
            Size = aeb_fate_data:make_integer(map_size(?FATE_MAP_VALUE(Map))),
            write(Arg0, Size, ES1);
        ?FATE_STORE_MAP(Cache, MapId) ->
            {Size, ES2} = store_map_size(Cache, MapId, ES1),
            write(Arg0, Size, ES2)
    end.

map_check(Op, Map, ES) ->
    Protocol = aefa_engine_state:consensus_version(ES),
    case ?IS_FATE_MAP(Map) orelse ?IS_FATE_STORE_MAP(Map) of
        false when Protocol >= ?IRIS_PROTOCOL_VSN ->
            aefa_fate:abort({bad_map, Op, Map}, ES);
        _ ->
            ok
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
    %% if you want this back, please fix the gas cost to be linear
    ?UNAVAILABLE_FROM(?VM_FATE_SOPHIA_2, EngineState),
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
-define(IS_CHAR(C), (C >= 0 andalso C =< 16#10ffff)).

str_join(Arg0, Arg1, Arg2, EngineState) ->
    {LeftValue, ES1} = get_op_arg(Arg1, EngineState),
    {RightValue, ES2} = get_op_arg(Arg2, ES1),
    Result = gop(str_join, LeftValue, RightValue, ES2),
    Cells = string_cells(Result),
    ES3 = aefa_engine_state:spend_gas_for_new_cells(Cells + 1, ES2),
    write(Arg0, Result, ES3).

str_length(Arg0, Arg1, EngineState) ->
    {StrValue, ES1} = get_op_arg(Arg1, EngineState),
    if not ?IS_FATE_STRING(StrValue) ->
        aefa_fate:abort({value_does_not_match_type, StrValue, string}, ES1);
       true -> ok
    end,
    case aefa_engine_state:vm_version(ES1) of
        ?VM_FATE_SOPHIA_1 ->
            Res = aeb_fate_data:make_integer(
                    byte_size(?FATE_STRING_VALUE(StrValue))),
            write(Arg0, Res, ES1);
        Vm when Vm > ?VM_FATE_SOPHIA_1 ->
            case unicode:characters_to_nfc_list(?FATE_STRING_VALUE(StrValue)) of
                {error, _, _} ->
                    aefa_fate:abort({value_does_not_match_type, StrValue, string_utf8}, ES1);
                Chars ->
                    Res = aeb_fate_data:make_integer(length(Chars)),
                    ES2 = aefa_engine_state:spend_gas_for_new_cells(length(Chars), ES1),
                    write(Arg0, Res, ES2)
            end
    end.

str_to_list(Arg0, Arg1, EngineState) ->
    {Value, ES1} = get_op_arg(Arg1, EngineState),
    if not ?IS_FATE_STRING(Value) ->
        aefa_fate:abort({value_does_not_match_type, Value, string}, ES1);
       true -> ok
    end,
    case unicode:characters_to_nfc_list(?FATE_STRING_VALUE(Value)) of
        {error, _, _} ->
            aefa_fate:abort({value_does_not_match_type, Value, string_utf8}, ES1);
        Chars ->
            ES2 = aefa_engine_state:spend_gas_for_new_cells(length(Chars) * 2 + 1, ES1),
            write(Arg0, Chars, ES2)
    end.

str_from_list(Arg0, Arg1, EngineState) ->
    {Value, ES1} = get_op_arg(Arg1, EngineState),
    if not ?IS_FATE_LIST(Value) ->
        aefa_fate:abort({value_does_not_match_type, Value, {list, char}}, ES1);
       true -> ok
    end,
    Protocol = aefa_engine_state:consensus_version(EngineState),
    ES2 = aefa_engine_state:spend_gas_for_traversal(Value, simple, ES1),
    case check_char_list(Value) of
        false when Protocol >= ?IRIS_PROTOCOL_VSN ->
            aefa_fate:abort({value_does_not_match_type, Value, {list, char}}, ES2);
        _ ->
            case unicode:characters_to_nfc_binary(Value) of
                {error, _, _} ->
                    aefa_fate:abort({value_does_not_match_type, Value, {list, char}}, ES2);
                Str ->
                    Cells = string_cells(?FATE_STRING_VALUE(Str)),
                    ES3 = aefa_engine_state:spend_gas_for_new_cells(Cells + 1, ES2),
                    write(Arg0, aeb_fate_data:make_string(Str), ES3)
            end
    end.

check_char_list([]) -> true;
check_char_list([C | Cs]) when ?IS_FATE_INTEGER(C), ?IS_CHAR(C) ->
    check_char_list(Cs);
check_char_list(_)  -> false.

str_to_upper(Arg0, Arg1, EngineState) ->
    {Value, ES1} = get_op_arg(Arg1, EngineState),
    if not ?IS_FATE_STRING(Value) ->
        aefa_fate:abort({value_does_not_match_type, Value, string}, ES1);
       true -> ok
    end,
    UpperStr = string:uppercase(?FATE_STRING_VALUE(Value)),
    ES2 = aefa_engine_state:spend_gas_for_new_cells(byte_size(UpperStr), ES1),
    write(Arg0, aeb_fate_data:make_string(UpperStr), ES2).

str_to_lower(Arg0, Arg1, EngineState) ->
    {Value, ES1} = get_op_arg(Arg1, EngineState),
    if not ?IS_FATE_STRING(Value) ->
        aefa_fate:abort({value_does_not_match_type, Value, string}, ES1);
       true -> ok
    end,
    LowerStr = string:lowercase(?FATE_STRING_VALUE(Value)),
    ES2 = aefa_engine_state:spend_gas_for_new_cells(byte_size(LowerStr), ES1),
    write(Arg0, aeb_fate_data:make_string(LowerStr), ES2).

char_to_int(Arg0, Arg1, EngineState) ->
    {Char, ES1} = get_op_arg(Arg1, EngineState),
    if not ?IS_FATE_INTEGER(Char) ->
        aefa_fate:abort({value_does_not_match_type, Char, char}, ES1);
       true -> ok
    end,
    write(Arg0, Char, ES1).

char_from_int(Arg0, Arg1, EngineState) ->
    {Int, ES1} = get_op_arg(Arg1, EngineState),
    Protocol = aefa_engine_state:consensus_version(EngineState),
    if not ?IS_FATE_INTEGER(Int) ->
        aefa_fate:abort({value_does_not_match_type, Int, int}, ES1);
       not ?IS_CHAR(Int) andalso Protocol >= ?IRIS_PROTOCOL_VSN ->
        aefa_fate:abort({value_does_not_match_type, Int, char}, ES1);
       true -> ok
    end,
    case unicode:characters_to_nfc_list([Int]) of
        [Char] ->
            ES2 = aefa_engine_state:spend_gas_for_new_cells(9, ES1),
            write(Arg0, make_some(Char), ES2);
        _ ->
            ES2 = aefa_engine_state:spend_gas_for_new_cells(8, ES1),
            write(Arg0, make_none(), ES2)
    end.

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
    Op = case aefa_engine_state:vm_version(EngineState) >= ?VM_FATE_SOPHIA_2 of
             true  -> str_reverse_unicode;
             false -> str_reverse
         end,
    Result = gop(Op, LeftValue, ES1),
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
    Cells = Value div 64 + 1,
    ES2 = aefa_engine_state:spend_gas_for_new_cells(Cells, ES1),
    Result = gop(bits_all, Value, ES2),
    write(Arg0, Result, ES2).

%% Bits.set(b : bits, i : int) : bits
%% Set bit i
bits_set(Arg0, Arg1, Arg2, EngineState) ->
    {Bits, ES1} = get_op_arg(Arg1, EngineState),
    {I, ES2} = get_op_arg(Arg2, ES1),
    Cells = I div 64 + 1,
    ES3 = aefa_engine_state:spend_gas_for_new_cells(Cells, ES2),
    Result = gop(bits_set, Bits, I, ES3),
    write(Arg0, Result, ES3).

%% Bits.clear(b : bits, i : int) : bits
%% Clear bit i
bits_clear(Arg0, Arg1, Arg2, EngineState) ->
    {Bits, ES1} = get_op_arg(Arg1, EngineState),
    {I, ES2} = get_op_arg(Arg2, ES1),
    Cells = I div 64 + 1,
    ES3 = aefa_engine_state:spend_gas_for_new_cells(Cells, ES2),
    Result = gop(bits_clear, Bits, I, ES3),
    write(Arg0, Result, ES3).


%% Bits.test(b : bits, i : int) : bool
%% Check if bit i is set
bits_test(Arg0, Arg1, Arg2, EngineState) ->
    {Bits, ES1} = get_op_arg(Arg1, EngineState),
    {I, ES2} = get_op_arg(Arg2, ES1),
    Cells = I div 64 + 1,
    ES3 = aefa_engine_state:spend_gas_for_new_cells(Cells, ES2),
    Result = gop(bits_test, Bits, I, ES3),
    write(Arg0, Result, ES3).

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
    Pubkey       = aefa_engine_state:current_contract(EngineState),
    CreatorCache = aefa_engine_state:creator_cache(EngineState),
    {CreatorKey, ES1} =
        case maps:get(Pubkey, CreatorCache, void) of
            void ->
                API     = aefa_engine_state:chain_api(EngineState),
                Creator = aefa_chain_api:creator(Pubkey, API),
                CCache1 = maps:put(Pubkey, Creator, CreatorCache),
                {Creator, aefa_engine_state:set_creator_cache(CCache1, EngineState)};
            Creator ->
                {Creator, EngineState}
        end,

    Address = aeb_fate_data:make_address(CreatorKey),

    write(Arg0, Address, ES1).

balance(Arg0, EngineState) ->
    API = aefa_engine_state:chain_api(EngineState),
    Pubkey = aefa_engine_state:current_contract(EngineState),
    {ok, Balance, API1} = aefa_chain_api:account_balance(Pubkey, API),
    write(Arg0, Balance, aefa_engine_state:set_chain_api(API1, EngineState)).

auth_tx(Arg0, EngineState) ->
    ?AVAILABLE_FROM(?VM_FATE_SOPHIA_2, EngineState),
    API   = aefa_engine_state:chain_api(EngineState),
    TxEnv = aefa_chain_api:tx_env(API),
    {Cells, Val} =
        case aetx_env:ga_tx(TxEnv) of
            undefined -> {8, make_none()};
            Tx        -> make_fate_tx(Tx, EngineState)
        end,
    ES1 = aefa_engine_state:spend_gas_for_new_cells(Cells, EngineState),
    write(Arg0, Val, ES1).

make_fate_tx(Aetx, ES) ->
    make_fate_tx(Aetx, [], undefined, ES).

make_fate_tx(Aetx, GAMetas, PayFor, ES) ->
    case aetx:specialize_type(Aetx) of
        {ga_meta_tx, GAMetaTx} ->
            GAMeta = #{ actor => aetx:origin(Aetx), fee => aetx:fee(Aetx) },
            make_fate_tx(aetx_sign:tx(aega_meta_tx:tx(GAMetaTx)), GAMetas ++ [GAMeta], PayFor, ES);
        {paying_for_tx, PayForTx} ->
            PayFor1 = #{ actor => aetx:origin(Aetx), fee => aetx:fee(Aetx) },
            make_fate_tx(aetx_sign:tx(aec_paying_for_tx:tx(PayForTx)), GAMetas, PayFor1, ES);
        {BaseTxType, BaseTx} ->
            MkWrapper = fun(PK, F) ->
                            aeb_fate_data:make_variant([2], 0, {?FATE_ADDRESS(PK), F})
                        end,
            {Cells0, FateBaseTx} = make_fate_base_tx(BaseTxType, BaseTx, ES),
            {Cells1, FatePayFor} =
                case PayFor of
                    undefined -> {8, make_none()};
                    #{ actor := PK, fee := F } ->
                        {9 + 8 + 4 + 2, %% Some('a) + variant + address + fee
                         make_some(MkWrapper(PK, F))}
                end,
            {Cells2, FateGAMetas} =
                {length(GAMetas) * (8 + 4 + 2), %% variant + address + fee
                 [ MkWrapper(PK, F) || #{ actor := PK, fee := F } <- GAMetas ]},
            Res = ?FATE_TUPLE({FatePayFor, FateGAMetas, ?FATE_ADDRESS(aetx:origin(Aetx)),
                               aetx:fee(Aetx), aetx:ttl(Aetx), FateBaseTx}),
            {Cells0 + Cells1 + Cells2 + 6 + 2 + 4 + 2 + 1 + 9, %% 6-tuple + 2, + address + 2 * int + Some('a)
             make_some(Res)}
    end.

make_fate_base_tx(BaseTxType, BaseTx, ES) ->
    Arities = [3, 0, 0, 0, 0, 0, 1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 0],
    MkVar   = fun(Tag, Args) -> aeb_fate_data:make_variant(Arities, Tag, Args) end,
    VarGas  = fun(NArgs) -> 2 * length(Arities) + NArgs + 4 end,
    AddrGas = 4,
    AmtGas  = 2,
    case BaseTxType of
        spend_tx ->
            {_, Recv} = aeser_id:specialize(aec_spend_tx:recipient_id(BaseTx)),
            Amount    = aec_spend_tx:amount(BaseTx),
            Payload   = aec_spend_tx:payload(BaseTx),
            {VarGas(3) + AddrGas + AmtGas + string_cells(Payload),
             MkVar(0, {?FATE_ADDRESS(Recv), Amount, ?FATE_STRING(Payload)})};

        oracle_register_tx -> {VarGas(0), MkVar(1, {})};
        oracle_query_tx    -> {VarGas(0), MkVar(2, {})};
        oracle_response_tx -> {VarGas(0), MkVar(3, {})};
        oracle_extend_tx   -> {VarGas(0), MkVar(4, {})};

        name_preclaim_tx   -> {VarGas(0), MkVar(5, {})};
        name_claim_tx ->
            Name = aens_claim_tx:name(BaseTx),
            {VarGas(1) + string_cells(Name), MkVar(6, {?FATE_STRING(Name)})};
        name_update_tx ->
            NHash = aens_update_tx:name_hash(BaseTx),
            {VarGas(1) + AddrGas, MkVar(7, {?FATE_BYTES(NHash)})};
        name_revoke_tx ->
            NHash = aens_revoke_tx:name_hash(BaseTx),
            {VarGas(1) + AddrGas, MkVar(8, {?FATE_BYTES(NHash)})};
        name_transfer_tx ->
            To    = aens_transfer_tx:recipient_pubkey(BaseTx),
            NHash = aens_transfer_tx:name_hash(BaseTx),
            {VarGas(2) + 2 * AddrGas, MkVar(9, {?FATE_ADDRESS(To), ?FATE_BYTES(NHash)})};

        channel_create_tx ->
            Other = aesc_create_tx:responder_pubkey(BaseTx),
            {VarGas(1) + AddrGas, MkVar(10, {?FATE_ADDRESS(Other)})};
        channel_deposit_tx ->
            Channel = aesc_deposit_tx:channel_pubkey(BaseTx),
            Amount  = aesc_deposit_tx:amount(BaseTx),
            {VarGas(2) + AddrGas + AmtGas, MkVar(11, {?FATE_ADDRESS(Channel), Amount})};
        channel_withdraw_tx ->
            Channel = aesc_withdraw_tx:channel_pubkey(BaseTx),
            Amount  = aesc_withdraw_tx:amount(BaseTx),
            {VarGas(2) + AddrGas + AmtGas, MkVar(12, {?FATE_ADDRESS(Channel), Amount})};
        channel_force_progress_tx ->
            Channel = aesc_force_progress_tx:channel_pubkey(BaseTx),
            {VarGas(1) + AddrGas, MkVar(13, {?FATE_ADDRESS(Channel)})};
        channel_close_mutual_tx ->
            Channel = aesc_close_mutual_tx:channel_pubkey(BaseTx),
            {VarGas(1) + AddrGas, MkVar(14, {?FATE_ADDRESS(Channel)})};
        channel_close_solo_tx ->
            Channel = aesc_close_solo_tx:channel_pubkey(BaseTx),
            {VarGas(1) + AddrGas, MkVar(15, {?FATE_ADDRESS(Channel)})};
        channel_slash_tx ->
            Channel = aesc_slash_tx:channel_pubkey(BaseTx),
            {VarGas(1) + AddrGas, MkVar(16, {?FATE_ADDRESS(Channel)})};
        channel_settle_tx ->
            Channel = aesc_settle_tx:channel_pubkey(BaseTx),
            {VarGas(1) + AddrGas, MkVar(17, {?FATE_ADDRESS(Channel)})};
        channel_snapshot_solo_tx ->
            Channel = aesc_snapshot_solo_tx:channel_pubkey(BaseTx),
            {VarGas(1) + AddrGas, MkVar(18, {?FATE_ADDRESS(Channel)})};

        contract_create_tx ->
            Amount = aect_create_tx:amount(BaseTx),
            {VarGas(1) + AmtGas, MkVar(19, {Amount})};
        contract_call_tx ->
            Ct     = aect_call_tx:contract_pubkey(BaseTx),
            Amount = aect_call_tx:amount(BaseTx),
            {VarGas(2) + AddrGas + AmtGas, MkVar(20, {?FATE_ADDRESS(Ct), Amount})};

        ga_attach_tx ->
            {VarGas(0), MkVar(21, {})};

        InvalidTx ->
            aefa_fate:abort({auth_tx_type_not_handled, InvalidTx}, ES)
    end.

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

fee(Arg0, EngineState) ->
    ?AVAILABLE_FROM(?VM_FATE_SOPHIA_2, EngineState),
    API = aefa_engine_state:chain_api(EngineState),
    write(Arg0, aefa_chain_api:fee(API), EngineState).


blockhash(Arg0, Arg1, ES) ->
    case get_op_arg(Arg1, ES) of
        {?FATE_INTEGER_VALUE(N), ES1} when ?IS_FATE_INTEGER(N) ->
            GenesisHeight = aec_block_genesis:height(),
            API = aefa_engine_state:chain_api(ES1),
            VMVsn = aefa_engine_state:vm_version(ES1),
            CurrentHeight = aefa_chain_api:generation(API),
            case (N < GenesisHeight orelse
                  %% BlockHash at current height available from FATE VM version 2
                  (N == CurrentHeight andalso VMVsn < ?VM_FATE_SOPHIA_2) orelse
                  N > CurrentHeight orelse
                  N =< CurrentHeight - 256) of
                true ->
                    write(Arg0, make_none(), ES1);
                false ->
                    FateHash = aefa_chain_api:blockhash(N, API),
                    write(Arg0, make_some(FateHash), ES1)
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

-spec microblock(_, _) -> no_return().
microblock(_Arg0, EngineState) ->
    Protocol = aefa_engine_state:consensus_version(EngineState),
    case Protocol >= ?IRIS_PROTOCOL_VSN of
        true  -> aefa_fate:abort({op_not_implemented, microblock}, EngineState);
        false -> exit({error, op_not_implemented_yet})
    end.

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

-spec deactivate(_) -> no_return().
deactivate(EngineState) ->
    Protocol = aefa_engine_state:consensus_version(EngineState),
    case Protocol >= ?IRIS_PROTOCOL_VSN of
        true  -> aefa_fate:abort({op_not_implemented, microblock}, EngineState);
        false -> exit({error, op_not_implemented_yet})
    end.

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

create(Arg0, Arg1, Arg2, ES0) ->
    ?AVAILABLE_FROM(?VM_FATE_SOPHIA_2, ES0),
    {[?FATE_CONTRACT_BYTEARRAY(Code), InitArgsTypes, Value], ES1} =
        get_op_args([Arg0, Arg1, Arg2], ES0),
    deploy_contract({code, Code}, InitArgsTypes, Value, no_gas_cap, false, ES1).

clone(Arg0, Arg1, Arg2, Arg3, ES0) ->
    ?AVAILABLE_FROM(?VM_FATE_SOPHIA_2, ES0),
    {[?FATE_CONTRACT(CloneePK), InitArgsTypes, Value, Prot], ES1} =
        get_op_args([Arg0, Arg1, Arg2, Arg3], ES0),
    deploy_contract({ref, CloneePK}, InitArgsTypes, Value, no_gas_cap, Prot, ES1).

clone_g(Arg0, Arg1, Arg2, Arg3, Arg4, ES0) ->
    ?AVAILABLE_FROM(?VM_FATE_SOPHIA_2, ES0),
    {[?FATE_CONTRACT(CloneePK), InitArgsTypes, Value, GasCap, Prot], ES1} =
        get_op_args([Arg0, Arg1, Arg2, Arg3, Arg4], ES0),
    deploy_contract({ref, CloneePK}, InitArgsTypes, Value, {gas_cap, GasCap}, Prot, ES1).

-define(CREATE_GAS(BYTELEN), BYTELEN * aec_governance:store_byte_gas()).
deploy_contract(CodeOrPK, InitArgsTypes, Value, GasCap, Prot, ES0) ->
    Protected =
        case Prot of
            false -> unprotected;
            true  -> protected;
            _     -> aefa_fate:abort({value_does_not_match_type, Prot, bool}, ES0)
        end,

    ES1 = case CodeOrPK of
              {ref, _} -> ES0;
              {code, Code} ->
                  spend_gas(?CREATE_GAS(size(Code)), ES0)
          end,

    {ContractPK, ES2} = put_contract(CodeOrPK, Value, ES1),
    ReplaceInitResult
        = fun(ES0_) ->
                  %% init returns unit, so we get rid of it
                  %% if the call was protected, rewrap the result
                  case Protected of
                      unprotected ->
                          {{tuple, {}}, ES1_} = get_op_arg({stack, 0}, ES0_),
                          write({stack, 0}, ?FATE_CONTRACT(ContractPK), ES1_);
                      protected ->
                          case get_op_arg({stack, 0}, ES0_) of
                              {{variant,[0, 1], 1, {{tuple, {}}}}, ES1_} ->
                                  write( {stack, 0}, make_some(?FATE_CONTRACT(ContractPK))
                                       , ES1_
                                       );
                              {{variant, [0, 1], 0, {}}, ES1_} ->
                                  %% Call to `init` failed due to runtime error
                                  ES2_ = unput_contract(ContractPK, Value, ES1_),
                                  write({stack, 0}, make_none(), ES2_)
                          end
                  end
          end,

    ES3 = aefa_engine_state:set_chain_api(
            begin
                {ok, API} = aefa_chain_api:eval_primops(
                  [ case CodeOrPK of
                        {ref, Ref} ->
                            aefa_chain_api:tx_event_op(clone, {Ref, Value, GasCap, ContractPK}, <<"Chain.clone">>);
                        {code, _} ->
                            aefa_chain_api:tx_event_op(create, {Value, GasCap, ContractPK}, <<"Chain.create">>)
                    end
                  ], aefa_engine_state:chain_api(ES2)),
                API
            end,
            ES2
           ),

    case remote_call_common(
           ?FATE_CONTRACT(ContractPK),
           init,
           InitArgsTypes,
           ?FATE_TYPEREP({tuple, []}),
           0,
           GasCap,
           Protected,
           ReplaceInitResult,
           ES3)
    of
        {ok, ES4} ->
            {jump, 0, ES4};
        {failed_protected_call, ES4} ->
            ES5 = unput_contract(ContractPK, Value, ES4),
            {next, write({stack, 0}, make_none(), ES5)}
    end.

put_contract(CodeOrPK, Amount, ES0) ->
    Current = aefa_engine_state:current_contract(ES0),

    AS0              = aefa_engine_state:chain_api(ES0),
    {ok, Nonce, AS1} = aefa_chain_api:next_nonce(Current, AS0),

    Contract0  =
        case CodeOrPK of
            {code, Code} -> aect_contracts:new(
                              Current, Nonce,
                              #{vm  => aefa_engine_state:vm_version(ES0)
                              , abi => ?ABI_FATE_SOPHIA_1
                              }, Code, 0);
            {ref, PK} ->
                {ok, FinalPK, VMVsn} =
                    aefa_engine_state:contract_find_final_ref(PK, ES0),
                aect_contracts:new_clone(
                  Current, Nonce,
                  #{vm => VMVsn
                  , abi => ?ABI_FATE_SOPHIA_1
                  }, FinalPK, 0)
        end,
    Store      = aefa_stores:initial_contract_store(),
    Contract1  = aect_contracts:set_state(Store, Contract0),
    ContractPK = aect_contracts:pubkey(Contract1),

    AS2 = aefa_chain_api:put_contract(Contract1, AS1),
    {ok, AS3} = aefa_chain_api:transfer_value(Current, ContractPK, Amount, AS2),

    ES1 = aefa_engine_state:set_chain_api(AS3, ES0),
    {ContractPK, ES1}.

%% Removes contract and returns the put tokens.
%% Does not remove contract's account as it could
%% have existed before.
unput_contract(PK, Balance, ES0) ->
    Current = aefa_engine_state:current_contract(ES0),
    ES1 = aefa_engine_state:remove_contract(PK, ES0),
    ES2 = aefa_engine_state:set_chain_api(
            begin
                {ok, API_ILoveErlangScoping} =
                    aefa_chain_api:transfer_value(
                      PK, Current, Balance,
                      aefa_engine_state:chain_api(ES1)),
                API_ILoveErlangScoping
            end,
            ES1),
    ES2.


-define(BYTECODE_HASH_GAS(BYTELEN), BYTELEN).
bytecode_hash(Arg0, Arg1, ES0) ->
    ?AVAILABLE_FROM(?VM_FATE_SOPHIA_2, ES0),
    {?FATE_CONTRACT(Pubkey), ES1} = get_op_arg(Arg1, ES0),
    case aefa_engine_state:contract_fate_bytecode(Pubkey, ES1) of
        {ok, ByteCode, ?VM_FATE_SOPHIA_2, ES2} ->
            SerByteCode = aeb_fate_code:serialize(ByteCode),
            ES3         = spend_gas(?BYTECODE_HASH_GAS(size(SerByteCode)), ES2),
            Hashed      = aeb_fate_data:make_hash(aec_hash:hash(fate_code, SerByteCode)),
            write(Arg0, make_some(Hashed), ES3);
        error ->
            write(Arg0, make_none(), ES1)
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
    {[Oracle, Question0, QFee, QTTL, RTTL, QType, RType], ES1} =
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
    {Question, ES2} =
        case aefa_engine_state:consensus_version(ES1) < ?IRIS_PROTOCOL_VSN of
            true ->
                {Question0, ES1};
            false ->
                aefa_fate:unfold_store_maps(Question0, ES1, unfold_serial)
        end,
    ?FATE_ORACLE(OraclePubkey) = Oracle,
    SenderPubkey = aefa_engine_state:current_contract(ES2),
    QFeeVal = ?FATE_INTEGER_VALUE(QFee),
    API = aefa_engine_state:chain_api(ES2),
    case aefa_chain_api:oracle_query(OraclePubkey, SenderPubkey, Question,
                                     QFeeVal, QTTLType, QTTLVal, RTTLVal,
                                     ?ABI_FATE_SOPHIA_1, QType, RType, API) of
        {ok, QueryId, DynamicGas, API1} ->
            ES3 = aefa_engine_state:set_chain_api(API1, ES2),
            ES4 = spend_gas(DynamicGas, ES3),
            write(Arg0, aeb_fate_data:make_oracle_query(QueryId), ES4);
        {error, What} ->
            aefa_fate:abort({primop_error, oracle_query, What}, ES2)
    end.

oracle_respond(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, EngineState) ->
    {[Signature, Oracle, Query, Response0, QType, RType], ES1} =
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
    {Response, ES2} =
        case aefa_engine_state:consensus_version(ES1) < ?IRIS_PROTOCOL_VSN of
            true ->
                {Response0, ES1};
            false ->
                aefa_fate:unfold_store_maps(Response0, ES1, unfold_serial)
        end,
    ?FATE_ORACLE_Q(QueryId) = Query,
    ?FATE_ORACLE(OraclePubkey) = Oracle,
    ?FATE_BYTES(SignBin) = Signature,
    ES3 = check_delegation_signature(oracle_respond, {OraclePubkey, QueryId}, SignBin, ES2),
    API = aefa_engine_state:chain_api(ES3),
    case aefa_chain_api:oracle_respond(OraclePubkey, QueryId, Response,
                                       ?ABI_FATE_SOPHIA_1, QType, RType, API) of
        {ok, DynamicGas, API1} ->
            ES4 = spend_gas(DynamicGas, ES3),
            aefa_engine_state:set_chain_api(API1, ES4);
        {error, What} ->
            aefa_fate:abort({primop_error, oracle_respond, What}, ES3)
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

oracle_get_int_value(IntValueFun, Arg0, Arg1, EngineState) ->
    case get_op_arg(Arg1, EngineState) of
        {?FATE_ORACLE(OraclePubkey), ES1} ->
            API = aefa_engine_state:chain_api(ES1),
            case aefa_chain_api:IntValueFun(OraclePubkey, API) of
                {ok, FeeVal, API1} ->
                    ES2 = aefa_engine_state:set_chain_api(API1, ES1),
                    write(Arg0, aeb_fate_data:make_integer(FeeVal), ES2);
                {error, What} ->
                    aefa_fate:abort({primop_error, IntValueFun, What}, ES1)
            end;
        {Other, ES1} ->
            aefa_fate:abort({value_does_not_match_type, Other, oracle}, ES1)
    end.

oracle_query_fee(Arg0, Arg1, EngineState) ->
    oracle_get_int_value(oracle_query_fee, Arg0, Arg1, EngineState).

oracle_expiry(Arg0, Arg1, EngineState) ->
    ?AVAILABLE_FROM(?VM_FATE_SOPHIA_2, EngineState),
    oracle_get_int_value(oracle_expiry, Arg0, Arg1, EngineState).

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
    DontCrashOnBadName = aefa_engine_state:vm_version(ES) >= ?VM_FATE_SOPHIA_2,
    case aefa_chain_api:aens_resolve(NameString, Key, API) of
        none ->
            write(Arg0, make_none(), ES);
        {ok, Tag, Pubkey, API1} ->
            ES1 = aefa_engine_state:set_chain_api(API1, ES),
            case aens_tag_to_val(Type, Tag, Pubkey) of
                none ->
                    write(Arg0, make_none(), ES1);
                {error, What} ->
                    aefa_fate:abort({primop_error, aens_resolve, What}, ES1);
                {ok, InnerVal} ->
                    Val = aeb_fate_data:make_variant([0,1], 1, {InnerVal}),
                    case aefa_fate:check_type(Type, Val) of
                        #{} ->
                            write(Arg0, Val, ES1);
                        false ->
                            write(Arg0, make_none(), ES1)
                    end
            end;
        %% Nicer to just return None in case name is invalid
        {error, _What} when DontCrashOnBadName ->
            write(Arg0, make_none(), ES)
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
    VmVersion = aefa_engine_state:vm_version(ES2),
    case aefa_chain_api:aens_preclaim(Pubkey, HashBin, API, VmVersion) of
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

aens_lookup(Arg0, Arg1, EngineState) ->
    ?AVAILABLE_FROM(?VM_FATE_SOPHIA_2, EngineState),

    {[NameString], ES1} = get_op_args([Arg1], EngineState),

    if not ?IS_FATE_STRING(NameString) ->
        aefa_fate:abort({value_does_not_match_type, NameString, string}, ES1);
       true ->
        ok
    end,

    ?FATE_STRING(Name) = NameString,
    API = aefa_engine_state:chain_api(ES1),
    case aefa_chain_api:aens_lookup(Name, API) of
        {ok, NameObj, API1} ->
            ES2 = aefa_engine_state:set_chain_api(API1, ES1),
            Owner = ?FATE_ADDRESS(maps:get(owner, NameObj)),
            TTL   = ?FATE_ABS_TTL(aeb_fate_data:make_integer(maps:get(ttl, NameObj))),
            MkKey = fun(Pt) -> ?FATE_STRING(aens_pointer:key(Pt)) end,
            %% Pointers are serialized as a list, pre-Iris pointers might not
            %% necessarily be valid, so explicitly sanitize the pointers
            Pointers = maps:from_list(
                         [ {MkKey(Pt), mk_fate_pointee(Pt)}
                           || Pt <- aens_pointer:sanitize_pointers(maps:get(pointers, NameObj)) ]),
            Res = make_some(aeb_fate_data:make_variant([3], 0, {Owner, TTL, Pointers})),
            write(Arg0, Res, ES2);
        none ->
            write(Arg0, make_none(), ES1);
        {error, _What} ->
            write(Arg0, make_none(), ES1)
    end.

mk_fate_pointee(Pt) ->
    Id = aens_pointer:id(Pt),
    case aeser_id:specialize(Id) of
        {account, PK}  -> aeb_fate_data:make_variant([1, 1, 1, 1], 0, {?FATE_ADDRESS(PK)});
        {oracle, PK}   -> aeb_fate_data:make_variant([1, 1, 1, 1], 1, {?FATE_ADDRESS(PK)});
        {contract, PK} -> aeb_fate_data:make_variant([1, 1, 1, 1], 2, {?FATE_ADDRESS(PK)});
        {channel, PK}  -> aeb_fate_data:make_variant([1, 1, 1, 1], 3, {?FATE_ADDRESS(PK)})
    end.

aens_update(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, EngineState) ->
    ?AVAILABLE_FROM(?VM_FATE_SOPHIA_2, EngineState),
    {[Signature, Owner, NameString, OptTTL, OptClientTTL, OptPointers], ES1} =
        get_op_args([Arg0, Arg1, Arg2, Arg3, Arg4, Arg5], EngineState),
    if
        not ?IS_FATE_BYTES(64, Signature) ->
            aefa_fate:abort({value_does_not_match_type, Signature, bytes64}, ES1);
        not ?IS_FATE_ADDRESS(Owner) ->
            aefa_fate:abort({value_does_not_match_type, Owner, address}, ES1);
        not ?IS_FATE_STRING(NameString) ->
            aefa_fate:abort({value_does_not_match_type, NameString, string}, ES1);
        not ?IS_FATE_VARIANT(OptTTL) ->
            aefa_fate:abort({value_does_not_match_type, OptTTL, variant}, ES1);
        not ?IS_FATE_VARIANT(OptClientTTL) ->
            aefa_fate:abort({value_does_not_match_type, OptClientTTL, variant}, ES1);
        not ?IS_FATE_VARIANT(OptPointers) ->
            aefa_fate:abort({value_does_not_match_type, OptPointers, variant}, ES1);
        true ->
            ok
    end,
    ?FATE_BYTES(SignBin) = Signature,
    ?FATE_ADDRESS(Pubkey) = Owner,
    ?FATE_STRING(NameBin) = NameString,
    TTL =
        case OptTTL of
            ?FATE_VARIANT([_,_], 0, {}) -> undefined;
            ?FATE_VARIANT([_,_], 1, {?FATE_REL_TTL(H)}) -> {relative_ttl, H};
            ?FATE_VARIANT([_,_], 1, {?FATE_ABS_TTL(H)}) -> {fixed_ttl, H}
        end,
    ClientTTL =
        case OptClientTTL of
            ?FATE_VARIANT([_,_], 0, {}) -> undefined;
            ?FATE_VARIANT([_,_], 1, {ClientTTL_}) -> ClientTTL_
        end,
    Pointers =
        case OptPointers of
            ?FATE_VARIANT([_,_], 0, {}) -> undefined;
            ?FATE_VARIANT([_,_], 1, {Pointers_}) ->
                maps:fold(
                  fun (PtrKey, ?FATE_VARIANT([1, 1, 1, 1], AddrTag, {{address, Addr}}), Acc) ->
                          IdType = case AddrTag of    % type pointee =
                                      0 -> account;   %   | AccountPt(address)
                                      1 -> oracle;    %   | OraclePt(address)
                                      2 -> contract;  %   | ContractPt(address)
                                      3 -> channel    %   | ChannelPt(address)
                                   end,
                          [aens_pointer:new(PtrKey, aeser_id:create(IdType, Addr)) | Acc]
                  end, [], Pointers_)
        end,
    HashBin = hash_name(aens_update, NameBin, ES1),
    ES2 = check_delegation_signature(aens_update, {Pubkey, HashBin}, SignBin, ES1),
    API = aefa_engine_state:chain_api(ES2),
    case aefa_chain_api:aens_update(Pubkey, HashBin, TTL, ClientTTL, Pointers, API) of
        {ok, API1} ->
            aefa_engine_state:set_chain_api(API1, ES2);
        {error, What} ->
            aefa_fate:abort({primop_error, aens_update, What}, ES2)
    end.

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
    ?FATE_STRING(NameBin) = NameString,
    HashBin = hash_name(aens_transfer, NameBin, ES1),
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
    ?FATE_STRING(NameBin) = NameString,
    HashBin = hash_name(aens_revoke, NameBin, ES1),
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
                                                              Type =:= aens_update;
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

bls12_381_g1_neg(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_g1_neg, {Arg0, Arg1}, ES).

bls12_381_g1_norm(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_g1_norm, {Arg0, Arg1}, ES).

bls12_381_g1_valid(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_g1_valid, {Arg0, Arg1}, ES).

bls12_381_g1_is_zero(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_g1_is_zero, {Arg0, Arg1}, ES).

bls12_381_g1_add(Arg0, Arg1, Arg2, ES) ->
    bls12_381_op(bin_op, bls12_381_g1_add, {Arg0, Arg1, Arg2}, ES).

bls12_381_g1_mul(Arg0, Arg1, Arg2, ES) ->
    bls12_381_op(bin_op, bls12_381_g1_mul, {Arg0, Arg1, Arg2}, ES).

bls12_381_g2_neg(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_g2_neg, {Arg0, Arg1}, ES).

bls12_381_g2_norm(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_g2_norm, {Arg0, Arg1}, ES).

bls12_381_g2_valid(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_g2_valid, {Arg0, Arg1}, ES).

bls12_381_g2_is_zero(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_g2_is_zero, {Arg0, Arg1}, ES).

bls12_381_g2_add(Arg0, Arg1, Arg2, ES) ->
    bls12_381_op(bin_op, bls12_381_g2_add, {Arg0, Arg1, Arg2}, ES).

bls12_381_g2_mul(Arg0, Arg1, Arg2, ES) ->
    bls12_381_op(bin_op, bls12_381_g2_mul, {Arg0, Arg1, Arg2}, ES).

bls12_381_gt_inv(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_gt_inv, {Arg0, Arg1}, ES).

bls12_381_gt_mul(Arg0, Arg1, Arg2, ES) ->
    bls12_381_op(bin_op, bls12_381_gt_mul, {Arg0, Arg1, Arg2}, ES).

bls12_381_gt_pow(Arg0, Arg1, Arg2, ES) ->
    bls12_381_op(bin_op, bls12_381_gt_pow, {Arg0, Arg1, Arg2}, ES).

bls12_381_gt_is_one(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_gt_is_one, {Arg0, Arg1}, ES).

bls12_381_pairing(Arg0, Arg1, Arg2, ES) ->
    bls12_381_op(bin_op, bls12_381_pairing, {Arg0, Arg1, Arg2}, ES).

bls12_381_miller_loop(Arg0, Arg1, Arg2, ES) ->
    bls12_381_op(bin_op, bls12_381_miller_loop, {Arg0, Arg1, Arg2}, ES).

bls12_381_final_exp(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_final_exp, {Arg0, Arg1}, ES).

bls12_381_gt_add(Arg0, Arg1, Arg2, ES) ->
    bls12_381_op(bin_op, bls12_381_gt_add, {Arg0, Arg1, Arg2}, ES).

bls12_381_int_to_fr(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_int_to_fr, {Arg0, Arg1}, ES).

bls12_381_int_to_fp(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_int_to_fp, {Arg0, Arg1}, ES).

bls12_381_fr_to_int(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_fr_to_int, {Arg0, Arg1}, ES).

bls12_381_fp_to_int(Arg0, Arg1, ES) ->
    bls12_381_op(un_op, bls12_381_fp_to_int, {Arg0, Arg1}, ES).

bls12_381_op(OpType, Op, Args, ES) ->
    case aefa_engine_state:vm_version(ES) >= ?VM_FATE_SOPHIA_2 of
        false ->
            aefa_fate:abort({primop_error, Op, not_supported}, ES);
        true when OpType == un_op ->
            un_op(Op, Args, ES);
        true when OpType == bin_op ->
            bin_op(Op, Args, ES)
    end.

contract_to_address(Arg0, Arg1, ES) ->
    un_op(contract_to_address, {Arg0, Arg1}, ES).

address_to_contract(Arg0, Arg1, ES) ->
    un_op(address_to_contract, {Arg0, Arg1}, ES).

sha3(Arg0, Arg1, EngineState) ->
    un_op_unfold(sha3, {Arg0, Arg1}, EngineState).

sha256(Arg0, Arg1, EngineState) ->
    un_op_unfold(sha256, {Arg0, Arg1}, EngineState).

blake2b(Arg0, Arg1, EngineState) ->
    un_op_unfold(blake2b, {Arg0, Arg1}, EngineState).

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

un_op_unfold(Op, {To, What}, ES) ->
    {Value0, ES1} = get_op_arg(What, ES),
    {Value,  ES2} = aefa_fate:unfold_store_maps(Value0, ES1, unfold_serial),
    Result = gop(Op, Value, ES2),
    write(To, Result, ES2).

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
make_none() ->
    aeb_fate_data:make_variant([0,1], 0, {}).

make_some(Val) ->
    aeb_fate_data:make_variant([0,1], 1, {Val}).


make_variant(Arities, Tag, NoElements, ES)  when ?IS_FATE_LIST(Arities)
                                               , ?IS_FATE_INTEGER(Tag)
                                               , ?IS_FATE_INTEGER(NoElements)
                                               , NoElements >= 0
                                               , Tag < length(?FATE_LIST_VALUE(Arities))
                                               , Tag >= 0 ->
    case aefa_engine_state:consensus_version(ES) < ?IRIS_PROTOCOL_VSN of
        true  -> make_variant1(Arities, Tag, NoElements, ES);
        false -> make_variant2(Arities, Tag, NoElements, ES)
    end;
make_variant(Arities, Tag, NoElements, ES) ->
    aefa_fate:abort({type_error, make_variant, [Arities, Tag, NoElements]}, ES).

make_variant1(Arities, Tag, NoElements, ES) ->
    {Elements, ES2} = aefa_fate:pop_n(NoElements, ES),
    Values = list_to_tuple(Elements),
    Cells = length(?FATE_LIST_VALUE(Arities)) * 2 + NoElements + 4,
    ES3 = aefa_engine_state:spend_gas_for_new_cells(Cells, ES2),
    {aeb_fate_data:make_variant(Arities, Tag, Values), ES3}.

make_variant2(Arities, Tag, NoElements, ES) ->
    %% Check for bad arities
    case lists:filter(fun(A) -> not is_integer(A) orelse abs(A) > 255 end, Arities) of
        []      -> ok;
        [_ | _] -> aefa_fate:abort({type_error, make_variant, [Arities, Tag, NoElements]}, ES)
    end,
    %% Check arity
    case lists:nth(Tag + 1, Arities) == NoElements of
        true  -> ok;
        false -> aefa_fate:abort({type_error, make_variant, [Arities, Tag, NoElements]}, ES)
    end,
    make_variant1(Arities, Tag, NoElements, ES).

-define(FATE_FR(X), ?FATE_BYTES(<<(X):32/binary>>)).
-define(FATE_FP(X), ?FATE_BYTES(<<(X):48/binary>>)).
-define(FATE_FP2(X1, X2), ?FATE_TUPLE({?FATE_FP(X1), ?FATE_FP(X2)})).

-define(FATE_G1(X, Y, Z), ?FATE_TUPLE({?FATE_FP(X), ?FATE_FP(Y), ?FATE_FP(Z)})).
-define(FATE_G2(X1, X2, Y1, Y2, Z1, Z2),
        ?FATE_TUPLE({?FATE_FP2(X1, X2), ?FATE_FP2(Y1, Y2), ?FATE_FP2(Z1, Z2)})).
-define(FATE_GT(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12),
        ?FATE_TUPLE({?FATE_FP(X1), ?FATE_FP(X2), ?FATE_FP(X3), ?FATE_FP(X4), ?FATE_FP(X5), ?FATE_FP(X6),
                     ?FATE_FP(X7), ?FATE_FP(X8), ?FATE_FP(X9), ?FATE_FP(X10), ?FATE_FP(X11), ?FATE_FP(X12)})).

-define(MAX_FR, 16#73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000000).
-define(MAX_FP, 16#1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab).

%% Unary operations
op(get, A) ->
    A;
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
    %% aeb_fate_data:make_address(<<A:256>>);
    aefa_fate:abort({disabled_operation, 'INT_TO_ADDR'});
op(addr_to_str, A) when ?IS_FATE_ADDRESS(A) ->
    Val = ?FATE_ADDRESS_VALUE(A),
    aeser_api_encoder:encode(account_pubkey, Val);
op(str_reverse, A) when ?IS_FATE_STRING(A) ->
    aeb_fate_data:make_string(binary_reverse(?FATE_STRING_VALUE(A)));
op(str_reverse_unicode, A) when ?IS_FATE_STRING(A) ->
    Bin = ?FATE_STRING_VALUE(A),
    CharList = unicode:characters_to_nfc_list(Bin),
    CharListR = lists:reverse(CharList),
    BinR = unicode:characters_to_nfc_binary(CharListR),
    aeb_fate_data:make_string(BinR);
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
    ?FATE_CONTRACT(?FATE_ADDRESS_VALUE(A));
op(bls12_381_g1_neg, ?FATE_G1(X1, Y1, Z1)) ->
    P = emcl:bnG1_neg(g1_to_emcl(X1, Y1, Z1)),
    g1_to_fate(P);
op(bls12_381_g1_norm, ?FATE_G1(X1, Y1, Z1)) ->
    P = emcl:bnG1_normalize(g1_to_emcl(X1, Y1, Z1)),
    g1_to_fate(P);
op(bls12_381_g1_valid, ?FATE_G1(X1, Y1, Z1)) ->
    aeb_fate_data:make_boolean(emcl:bnG1_is_valid(g1_to_emcl(X1, Y1, Z1)));
op(bls12_381_g1_is_zero, ?FATE_G1(X1, Y1, Z1)) ->
    aeb_fate_data:make_boolean(emcl:bnG1_is_zero(g1_to_emcl(X1, Y1, Z1)));
op(bls12_381_g2_neg, ?FATE_G2(X11, X12, Y11, Y12, Z11, Z12)) ->
    P = emcl:bnG2_neg(g2_to_emcl(X11, X12, Y11, Y12, Z11, Z12)),
    g2_to_fate(P);
op(bls12_381_g2_norm, ?FATE_G2(X11, X12, Y11, Y12, Z11, Z12)) ->
    P = emcl:bnG2_normalize(g2_to_emcl(X11, X12, Y11, Y12, Z11, Z12)),
    g2_to_fate(P);
op(bls12_381_g2_valid, ?FATE_G2(X11, X12, Y11, Y12, Z11, Z12)) ->
    aeb_fate_data:make_boolean(emcl:bnG2_is_valid(g2_to_emcl(X11, X12, Y11, Y12, Z11, Z12)));
op(bls12_381_g2_is_zero, ?FATE_G2(X11, X12, Y11, Y12, Z11, Z12)) ->
    aeb_fate_data:make_boolean(emcl:bnG2_is_zero(g2_to_emcl(X11, X12, Y11, Y12, Z11, Z12)));
op(bls12_381_gt_inv, ?FATE_GT(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110, X111, X112)) ->
    P = emcl:bnGt_inv(gt_to_emcl(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110, X111, X112)),
    gt_to_fate(P);
op(bls12_381_gt_is_one, ?FATE_GT(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110, X111, X112)) ->
    aeb_fate_data:make_boolean(
      emcl:bnGt_is_one(gt_to_emcl(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110, X111, X112)));
op(bls12_381_final_exp, ?FATE_GT(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110, X111, X112)) ->
    P = emcl:bn_final_exp(gt_to_emcl(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110, X111, X112)),
    gt_to_fate(P);
op(bls12_381_int_to_fr, I) when ?IS_FATE_INTEGER(I), I >= 0, I < ?MAX_FR ->
    Fr = emcl:mk_Fr(?FATE_INTEGER_VALUE(I)),
    ?FATE_FR(emcl:bnFr_to_bin(Fr));
op(bls12_381_int_to_fp, I) when ?IS_FATE_INTEGER(I), I >= 0, I < ?MAX_FP ->
    Fp = emcl:mk_Fp(?FATE_INTEGER_VALUE(I)),
    ?FATE_FP(emcl:bnFp_to_bin(Fp));
op(bls12_381_fr_to_int, ?FATE_FR(X)) ->
    I = emcl:bnFr_to_int(emcl:mk_Fr(X)),
    aeb_fate_data:make_integer(I);
op(bls12_381_fp_to_int, ?FATE_FP(X)) ->
    I = emcl:bnFp_to_int(emcl:mk_Fp(X)),
    aeb_fate_data:make_integer(I);
op(Op, Arg) ->
    aefa_fate:abort({type_error, Op, [Arg]}).

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
    aeb_fate_data:make_list([Hd | ?FATE_LIST_VALUE(Tail)]);
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
        _                        -> aefa_fate:abort({type_error, bytes_split, [?FATE_BYTES(A), B]})
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
            aefa_fate:abort({type_error, variant_element, [A, B]})
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
    end;
op(bls12_381_g1_add, ?FATE_G1(X1, Y1, Z1), ?FATE_G1(X2, Y2, Z2)) ->
    P = emcl:bnG1_add(g1_to_emcl(X1, Y1, Z1), g1_to_emcl(X2, Y2, Z2)),
    g1_to_fate(P);
op(bls12_381_g1_mul, ?FATE_FR(K), ?FATE_G1(X1, Y1, Z1)) ->
    P = emcl:bnG1_mul(g1_to_emcl(X1, Y1, Z1), emcl:mk_Fr(K)),
    g1_to_fate(P);
op(bls12_381_g2_add, ?FATE_G2(X11, X12, Y11, Y12, Z11, Z12),
                     ?FATE_G2(X21, X22, Y21, Y22, Z21, Z22)) ->
    P = emcl:bnG2_add(g2_to_emcl(X11, X12, Y11, Y12, Z11, Z12),
                      g2_to_emcl(X21, X22, Y21, Y22, Z21, Z22)),
    g2_to_fate(P);
op(bls12_381_g2_mul, ?FATE_FR(K), ?FATE_G2(X11, X12, Y11, Y12, Z11, Z12)) ->
    P = emcl:bnG2_mul(g2_to_emcl(X11, X12, Y11, Y12, Z11, Z12), emcl:mk_Fr(K)),
    g2_to_fate(P);
op(bls12_381_gt_add, ?FATE_GT(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110, X111, X112),
                     ?FATE_GT(X21, X22, X23, X24, X25, X26, X27, X28, X29, X210, X211, X212)) ->
    P = emcl:bnGt_add(gt_to_emcl(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110, X111, X112),
                      gt_to_emcl(X21, X22, X23, X24, X25, X26, X27, X28, X29, X210, X211, X212)),
    gt_to_fate(P);
op(bls12_381_gt_mul, ?FATE_GT(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110, X111, X112),
                     ?FATE_GT(X21, X22, X23, X24, X25, X26, X27, X28, X29, X210, X211, X212)) ->
    P = emcl:bnGt_mul(gt_to_emcl(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110, X111, X112),
                      gt_to_emcl(X21, X22, X23, X24, X25, X26, X27, X28, X29, X210, X211, X212)),
    gt_to_fate(P);
op(bls12_381_gt_pow, ?FATE_GT(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110, X111, X112), ?FATE_FR(K)) ->
    P = emcl:bnGt_pow(gt_to_emcl(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110, X111, X112), emcl:mk_Fr(K)),
    gt_to_fate(P);
op(bls12_381_pairing, ?FATE_G1(X1, Y1, Z1), ?FATE_G2(X11, X12, Y11, Y12, Z11, Z12)) ->
    P = emcl:bn_pairing(g1_to_emcl(X1, Y1, Z1), g2_to_emcl(X11, X12, Y11, Y12, Z11, Z12)),
    gt_to_fate(P);
op(bls12_381_miller_loop, ?FATE_G1(X1, Y1, Z1), ?FATE_G2(X11, X12, Y11, Y12, Z11, Z12)) ->
    P = emcl:bn_miller_loop(g1_to_emcl(X1, Y1, Z1), g2_to_emcl(X11, X12, Y11, Y12, Z11, Z12)),
    gt_to_fate(P);
op(Op, Arg1, Arg2) ->
    aefa_fate:abort({type_error, Op, [Arg1, Arg2]}).

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
    aeu_crypto:ecverify(secp256k1, Msg1, PK1, Sig1);
op(Op, Arg1, Arg2, Arg3) ->
    aefa_fate:abort({type_error, Op, [Arg1, Arg2, Arg3]}).


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

store_map_get_clean(Cache, MapId, ES) ->
    Pubkey           = aefa_engine_state:current_contract(ES),
    {Store, ES1}     = aefa_fate:ensure_contract_store(Pubkey, ES),
    ConsensusVersion = aefa_engine_state:consensus_version(ES),
    STORE_MAP_TO_LIST =
        if
        ConsensusVersion < ?IRIS_PROTOCOL_VSN ->
            fun aefa_stores_lima:store_map_to_list/3;
        true ->
            fun aefa_stores:store_map_to_list/3
        end,
    {StoreList, Store1} = STORE_MAP_TO_LIST(Pubkey, MapId, Store),
    StoreMap            = maps:from_list(StoreList),
    Upd = fun(Key, ?FATE_MAP_TOMBSTONE, M) -> maps:remove(Key, M);
             (Key, Val, M)                 -> maps:put(Key, Val, M) end,
    Map = maps:fold(Upd, StoreMap, Cache),
    ES2 = aefa_engine_state:set_stores(Store1, ES1),
    {Map, ES2}.

%% ------------------------------------------------------
%% Comparison instructions
%% ------------------------------------------------------

bin_comp(Comp, {To, Left, Right}, ES) ->
    {LeftValue,   ES1} = get_op_arg(Left, ES),
    {LeftValue1,  ES2} = aefa_fate:unfold_store_maps(LeftValue, ES1, unfold_compare),
    {RightValue,  ES3} = get_op_arg(Right, ES2),
    {RightValue1, ES4} = aefa_fate:unfold_store_maps(RightValue, ES3, unfold_compare),
    ConsensusVersion = aefa_engine_state:consensus_version(ES),
    COMP = if
               ConsensusVersion < ?IRIS_PROTOCOL_VSN ->
                   fun comp/3;
               true ->
                   fun comp_iris/3
           end,
    Result = COMP(Comp, LeftValue1, RightValue1),
    write(To, Result, ES4).

comp( lt, A, B) -> A < B;
comp( gt, A, B) -> A > B;
comp(elt, A, B) -> A =< B;
comp(egt, A, B) -> A >= B;
comp( eq, A, B) -> A =:= B;
comp(neq, A, B) -> A =/= B.

comp_iris( lt, A, B) -> aeb_fate_data:lt(A, B);
comp_iris( gt, A, B) -> aeb_fate_data:lt(B, A);
% equal or less than <==> not greater than
comp_iris(elt, A, B) -> not aeb_fate_data:lt(B, A);
% equal or greater than <==> not less than
comp_iris(egt, A, B) -> not aeb_fate_data:lt(A, B);
% equals <==> not less than and not greater than
comp_iris( eq, A, B) -> (not aeb_fate_data:lt(A, B)) and (not aeb_fate_data:lt(B, A));
% not equals <==> less than or greater than
comp_iris(neq, A, B) -> aeb_fate_data:lt(A, B) or aeb_fate_data:lt(B, A).


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

g1_to_fate(P) ->
    {X, Y, Z} = emcl:bnG1_to_bin(P),
    ?FATE_G1(X, Y, Z).

g2_to_fate(P) ->
    {{X1, X2}, {Y1, Y2}, {Z1, Z2}} = emcl:bnG2_to_bin(P),
    ?FATE_G2(X1, X2, Y1, Y2, Z1, Z2).

gt_to_fate(P) ->
    {X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12} = emcl:bnGt_to_bin(P),
    ?FATE_GT(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12).

g1_to_emcl(X, Y, Z) ->
    emcl:mk_G1(emcl:mk_Fp(X), emcl:mk_Fp(Y), emcl:mk_Fp(Z)).

g2_to_emcl(X1, X2, Y1, Y2, Z1, Z2) ->
    emcl:mk_G2(emcl:mk_Fp2(emcl:mk_Fp(X1), emcl:mk_Fp(X2)),
               emcl:mk_Fp2(emcl:mk_Fp(Y1), emcl:mk_Fp(Y2)),
               emcl:mk_Fp2(emcl:mk_Fp(Z1), emcl:mk_Fp(Z2))).

gt_to_emcl(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12) ->
    emcl:mk_Gt(emcl:mk_Fp(X1), emcl:mk_Fp(X2), emcl:mk_Fp(X3), emcl:mk_Fp(X4),
               emcl:mk_Fp(X5), emcl:mk_Fp(X6), emcl:mk_Fp(X7), emcl:mk_Fp(X8),
               emcl:mk_Fp(X9), emcl:mk_Fp(X10), emcl:mk_Fp(X11), emcl:mk_Fp(X12)).
