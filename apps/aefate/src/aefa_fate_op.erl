%% Implements Fate operations.
%%
-module(aefa_fate_op).

-export([ return/1
        , returnr/2
        , call/2
        , call_r/4
        , call_t/2
        , call_tr/4
        , call_gr/5
        , call_gtr/5
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
        , oracle_query/7
        , oracle_respond/5
        , oracle_extend/4
        , oracle_get_answer/4
        , oracle_get_question/4
        , oracle_query_fee/3
        , aens_resolve/1
        , aens_preclaim/1
        , aens_claim/1
        , aend_update/1
        , aens_transfer/1
        , aens_revoke/1
        , ecverify/5
        , ecverify_secp256k1/5
        , contract_to_address/3
        , sha3/3
        , sha256/3
        , blake2b/3
        , setelement/5
        , dummyarg/8
        , dummyarg/9
        , abort/2
        , exit/2
        , nop/1
        , auth_tx_hash/2
        ]).

-include_lib("aebytecode/include/aeb_fate_data.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include("../../aecore/include/blocks.hrl").

%% ------------------------------------------------------------------------
%% Operations
%% ------------------------------------------------------------------------

%% ------------------------------------------------------
%% Call/return instructions
%% ------------------------------------------------------
return(EngineState) ->
    ES = aefa_fate:check_return_type(EngineState),
    aefa_fate:pop_call_stack(ES).

returnr(Arg0, EngineState) ->
    ES1 = un_op(get, {{stack, 0}, Arg0}, EngineState),
    ES2 = aefa_fate:check_return_type(ES1),
    aefa_fate:pop_call_stack(ES2).

call(Arg0, EngineState) ->
    ES1 = aefa_fate:push_return_address(EngineState),
    call_t(Arg0, ES1).

call_t(Arg0, EngineState) ->
    {Fun, ES1} = get_op_arg(Arg0, EngineState),
    Signature = aefa_fate:get_function_signature(Fun, ES1),
    ES2 = aefa_fate:check_signature_and_bind_args(Signature, ES1),
    {jump, 0, aefa_fate:set_local_function(Fun, ES2)}.

call_r(Arg0, Arg1, Arg2, EngineState) ->
    ES1 = aefa_fate:push_return_address(EngineState),
    call_tr(Arg0, Arg1, Arg2, ES1).

call_tr(Arg0, Arg1, Arg2, EngineState) ->
    {Contract, ES1} = get_op_arg(Arg0, EngineState),
    {Value, ES2} = get_op_arg(Arg2, ES1),
    ES3 = remote_call_common(Contract, Arg1, Value, ES2),
    {jump, 0, ES3}.

call_gr(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    ES1 = aefa_fate:push_return_address(EngineState),
    call_gtr(Arg0, Arg1, Arg2, Arg3, ES1).

call_gtr(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    {Contract, ES1} = get_op_arg(Arg0, EngineState),
    {Value, ES2}   = get_op_arg(Arg2, ES1),
    {GasCap, ES3}  = get_op_arg(Arg3, ES2),
    ES4 = remote_call_common(Contract, Arg1, Value, ES3),
    ES5 = aefa_fate:push_gas_cap(GasCap, ES4),
    {jump, 0, ES5}.

remote_call_common(Contract, Function, Value, EngineState) ->
    Current   = aefa_engine_state:current_contract(EngineState),
    ES1       = aefa_fate:check_remote(Contract, EngineState),
    ES2       = aefa_fate:set_remote_function(Contract, Function, ES1),
    Signature = aefa_fate:get_function_signature(Function, ES2),
    ES3       = aefa_fate:check_signature_and_bind_args(Signature, ES2),
    transfer_value(Current, Contract, Value, ES3).

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
            case aefa_chain_api:spend(From, To, IntValue, API) of
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
    un_op(inc, {{stack, 0}, {stack, 0}}, EngineState).

dec(Arg0, EngineState) ->
    un_op(dec, {Arg0, Arg0}, EngineState).

add(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(add, {Arg0, Arg1, Arg2}, EngineState).

sub(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(sub, {Arg0, Arg1, Arg2}, EngineState).

mul(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(mul, {Arg0, Arg1, Arg2}, EngineState).

divide(Arg0, Arg1, Arg2, EngineState) ->
    bin_op('div', {Arg0, Arg1, Arg2}, EngineState).

modulo(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(mod, {Arg0, Arg1, Arg2}, EngineState).

pow(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(pow, {Arg0, Arg1, Arg2}, EngineState).

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
    {Elements, ES2} = aefa_fate:pop_n(Size, ES),
    Tuple = list_to_tuple(Elements),
    FateTuple = aeb_fate_data:make_tuple(Tuple),
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
            case size(Tuple) > Index of
                true ->
                    NewT = ?FATE_TUPLE(erlang:setelement(Index+1, Tuple, Element)),
                    write(Arg0, NewT, ES3);
                false ->
                    aefa_fate:abort({element_index_out_of_bounds, Index}, ES3)
            end
    end.


%% ------------------------------------------------------
%% Map instructions
%% ------------------------------------------------------
map_empty(Arg0, EngineState) ->
    un_op(get, {Arg0,
                {immediate, aeb_fate_data:make_map(#{})}},
          EngineState).

map_lookup(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(map_lookup, {Arg0, Arg1, Arg2}, EngineState).

map_lookup(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    ter_op(map_lookup_default, {Arg0, Arg1, Arg2, Arg3}, EngineState).

map_update(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    ter_op(map_update, {Arg0, Arg1, Arg2, Arg3}, EngineState).

map_delete(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(map_delete, {Arg0, Arg1, Arg2}, EngineState).

map_member(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(map_member, {Arg0, Arg1, Arg2}, EngineState).

map_from_list(Arg0, Arg1, EngineState) ->
    un_op(map_from_list, {Arg0, Arg1}, EngineState).

map_to_list(Arg0, Arg1, EngineState) ->
    un_op(map_to_list, {Arg0, Arg1}, EngineState).

map_size_(Arg0, Arg1, EngineState) ->
    un_op(map_size, {Arg0, Arg1}, EngineState).

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
    bin_op(cons, {Arg0, Arg1, Arg2}, EngineState).

hd(Arg0, Arg1, EngineState) ->
    un_op(hd, {Arg0, Arg1}, EngineState).

tl(Arg0, Arg1, EngineState) ->
    un_op(tl, {Arg0, Arg1}, EngineState).

length(Arg0, Arg1, EngineState) ->
    un_op(length, {Arg0, Arg1}, EngineState).

append(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(append, {Arg0, Arg1, Arg2}, EngineState).

%% ------------------------------------------------------
%% String instructions
%% ------------------------------------------------------

str_join(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(str_join, {Arg0, Arg1, Arg2}, EngineState).

str_length(Arg0, Arg1, EngineState) ->
    un_op(str_length, {Arg0, Arg1}, EngineState).

int_to_str(Arg0, Arg1, EngineState) ->
    un_op(int_to_str, {Arg0, Arg1}, EngineState).

addr_to_str(Arg0, Arg1, EngineState) ->
    un_op(addr_to_str, {Arg0, Arg1}, EngineState).

str_reverse(Arg0, Arg1, EngineState) ->
    un_op(str_reverse, {Arg0, Arg1}, EngineState).

int_to_addr(Arg0, Arg1, EngineState) ->
    un_op(int_to_addr, {Arg0, Arg1}, EngineState).

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
    aefa_fate:push(?FATE_BITS(0), EngineState).

bits_none(Arg0, EngineState) ->
    un_op(get, {Arg0, {immediate, ?FATE_BITS(0)}}, EngineState).

%% Bits.all : bits
%% A bit field with all (an infinite amount) bits set
bits_all(EngineState) ->
    aefa_fate:push(?FATE_BITS(-1), EngineState).

bits_all(Arg0, EngineState) ->
    un_op(get, {Arg0, {immediate, ?FATE_BITS(-1)}}, EngineState).

%% Bits.all_n : bits
%% A bit field with n bits set
bits_all_n(Arg0, Arg1, EngineState) ->
    un_op(bits_all, {Arg0, Arg1}, EngineState).

%% Bits.set(b : bits, i : int) : bits
%% Set bit i
bits_set(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(bits_set, {Arg0, Arg1, Arg2}, EngineState).

%% Bits.clear(b : bits, i : int) : bits
%% Clear bit i
bits_clear(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(bits_clear, {Arg0, Arg1, Arg2}, EngineState).

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

balance(Arg0, EngineState) ->
    API = aefa_engine_state:chain_api(EngineState),
    Pubkey = aefa_engine_state:current_contract(EngineState),
    {ok, Balance, API1} = aefa_chain_api:account_balance(Pubkey, API),
    write(Arg0, Balance, aefa_engine_state:set_chain_api(API1, EngineState)).

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
                    write(Arg0, aeb_fate_data:make_integer(0), ES1);
                false ->
                    write(Arg0, aefa_chain_api:blockhash(N, API), ES1)
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

log(_Arg0, _EngineState) -> exit({error, op_not_implemented_yet}).

log(_Arg0, _Arg1, _EngineState) -> exit({error, op_not_implemented_yet}).

log(_Arg0, _Arg1, _Arg2, _EngineState) -> exit({error, op_not_implemented_yet}).

log(_Arg0, _Arg1, _Arg2, _Arg3, _EngineState) -> exit({error, op_not_implemented_yet}).

log(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4, _EngineState) -> exit({error, op_not_implemented_yet}).

log(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4, _Arg5, _EngineState) -> exit({error, op_not_implemented_yet}).

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
        not ?IS_FATE_SIGNATURE(Signature) ->
            aefa_fate:abort({value_does_not_match_type, Signature, signature}, ES1);
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

oracle_register_(Arg0, ?FATE_SIGNATURE(Signature), ?FATE_ADDRESS(Address),
                 ?FATE_INTEGER_VALUE(QFee), TTL,
                 ?FATE_TYPEREP(QType),
                 ?FATE_TYPEREP(RType),
                 ES) ->
    {TTLType, TTLVal} =
        case TTL of
            ?FATE_REL_TTL(R) when ?IS_FATE_INTEGER(R) -> {relative, R};
            ?FATE_ABS_TTL(A) when ?IS_FATE_INTEGER(A) -> {absolute, A};
             _ ->
                aefa_fate:abort({primop_error, oracle_query, bad_ttl}, ES)
        end,
    QFormat = iolist_to_binary(aeb_fate_encoding:serialize_type(QType)),
    RFormat = iolist_to_binary(aeb_fate_encoding:serialize_type(RType)),
    ES1     = check_delegation_signature(oracle_register, Address, Signature, ES),
    API     = aefa_engine_state:chain_api(ES1),
    case aefa_chain_api:oracle_register(Address, QFee, TTLType, TTLVal,
                                        QFormat, RFormat, ?ABI_FATE_SOPHIA_1,
                                        API) of
        {ok, API1} ->
            ES2 = aefa_engine_state:set_chain_api(API1, ES1),
            write(Arg0, ?FATE_ORACLE(Address), ES2);
        {error, What} ->
            aefa_fate:abort({primop_error, oracle_register, What}, ES1)
    end.

oracle_query(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, EngineState) ->
    {[Oracle, Question, QFee, QTTL, RTTL], ES1} =
        get_op_args([Arg1, Arg2, Arg3, Arg4, Arg5], EngineState),
    if
        not ?IS_FATE_ORACLE(Oracle) ->
            aefa_fate:abort({value_does_not_match_type, Oracle, oracle}, ES1);
        not ?IS_FATE_INTEGER(QFee) ->
            aefa_fate:abort({value_does_not_match_type, QFee, integer}, ES1);
        not ?FATE_INTEGER_VALUE(QFee) >= 0 ->
            aefa_fate:abort({primop_error, oracle_query, too_low_fee}, ES1);
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
                                     ?ABI_FATE_SOPHIA_1, API) of
        {ok, QueryId, API1} ->
            ES2 = aefa_engine_state:set_chain_api(API1, ES1),
            write(Arg0, aeb_fate_data:make_oracle_query(QueryId), ES2);
        {error, What} ->
            aefa_fate:abort({primop_error, oracle_query, What}, ES1)
    end.

oracle_respond(Arg0, Arg1, Arg2, Arg3, EngineState) ->
    {[Signature, Oracle, Query, Response], ES1} =
        get_op_args([Arg0, Arg1, Arg2, Arg3], EngineState),
    if
        not ?IS_FATE_ORACLE(Oracle) ->
            aefa_fate:abort({value_does_not_match_type, Oracle, oracle}, ES1);
        not ?IS_FATE_SIGNATURE(Signature) ->
            aefa_fate:abort({value_does_not_match_type, Signature, string}, ES1);
        not ?IS_FATE_ORACLE_Q(Query) ->
            aefa_fate:abort({value_does_not_match_type, Query, oracle_query}, ES1);
        true ->
            ok
    end,
    ?FATE_ORACLE_Q(QueryId) = Query,
    ?FATE_ORACLE(OraclePubkey) = Oracle,
    ?FATE_SIGNATURE(SignBin) = Signature,
    ES2 = check_delegation_signature(oracle_respond, {OraclePubkey, QueryId}, SignBin, ES1),
    API = aefa_engine_state:chain_api(ES2),
    case aefa_chain_api:oracle_respond(OraclePubkey, QueryId, Response, ?ABI_FATE_SOPHIA_1, API) of
        {ok, API1} ->
            aefa_engine_state:set_chain_api(API1, ES2);
        {error, What} ->
            aefa_fate:abort({primop_error, oracle_respond, What}, ES2)
    end.

oracle_extend(Arg0, Arg1, Arg2, EngineState) ->
    {[Signature, Oracle, TTL], ES1} =
        get_op_args([Arg0, Arg1, Arg2], EngineState),
    if
        not ?IS_FATE_ORACLE(Oracle) ->
            aefa_fate:abort({value_does_not_match_type, Oracle, oracle}, ES1);
        not ?IS_FATE_SIGNATURE(Signature) ->
            aefa_fate:abort({value_does_not_match_type, Signature, signature}, ES1);
        true ->
            ok
    end,
    {TTLType, TTLVal} =
        case TTL of
            %% TTL Must be relative for extends
            ?FATE_REL_TTL(R) when ?IS_FATE_INTEGER(R) -> {relative, R};
             _ ->
                aefa_fate:abort({primop_error, oracle_query, bad_ttl}, EngineState)
        end,
    ?FATE_ORACLE(OraclePubkey) = Oracle,
    ?FATE_SIGNATURE(SignBin) = Signature,
    ES2 = check_delegation_signature(oracle_extend, OraclePubkey, SignBin, ES1),
    API = aefa_engine_state:chain_api(ES2),
    case aefa_chain_api:oracle_extend(OraclePubkey, TTLType, TTLVal, API) of
        {ok, API1} ->
            aefa_engine_state:set_chain_api(API1, ES2);
        {error, What} ->
            aefa_fate:abort({primop_error, oracle_extend, What}, ES2)
    end.

oracle_get_question(Arg0, Arg1, Arg2, EngineState) ->
    {[Oracle, Query], ES1} =
        get_op_args([Arg1, Arg2], EngineState),
    if
        not ?IS_FATE_ORACLE(Oracle) ->
            aefa_fate:abort({value_does_not_match_type, Oracle, oracle}, ES1);
        not ?IS_FATE_ORACLE_Q(Query) ->
            aefa_fate:abort({value_does_not_match_type, Query, oracle_query}, ES1);
        true ->
            ok
    end,
    ?FATE_ORACLE(OraclePubkey) = Oracle,
    ?FATE_ORACLE_Q(QueryId) = Query,
    API = aefa_engine_state:chain_api(ES1),
    case aefa_chain_api:oracle_get_question(OraclePubkey, QueryId, API) of
        {ok, Question, API1} ->
            %% TODO: Should we check the type of the query?
            ES2 = aefa_engine_state:set_chain_api(API1, ES1),
            write(Arg0, Question, ES2);
        {error, What} ->
            aefa_fate:abort({primop_error, oracle_get_question, What}, ES1)
    end.

oracle_get_answer(Arg0, Arg1, Arg2, EngineState) ->
    {[Oracle, Query], ES1} =
        get_op_args([Arg1, Arg2], EngineState),
    if
        not ?IS_FATE_ORACLE(Oracle) ->
            aefa_fate:abort({value_does_not_match_type, Oracle, oracle}, ES1);
        not ?IS_FATE_ORACLE_Q(Query) ->
            aefa_fate:abort({value_does_not_match_type, Query, oracle_query}, ES1);
        true ->
            ok
    end,
    ?FATE_ORACLE(OraclePubkey) = Oracle,
    ?FATE_ORACLE_Q(QueryId) = Query,
    API = aefa_engine_state:chain_api(ES1),
    case aefa_chain_api:oracle_get_answer(OraclePubkey, QueryId, API) of
        {ok, Answer, API1} ->
            %% TODO: Should we check the type of the query?
            ES2 = aefa_engine_state:set_chain_api(API1, ES1),
            write(Arg0, Answer, ES2);
        {error, What} ->
            aefa_fate:abort({primop_error, oracle_get_question, What}, ES1)
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

check_delegation_signature(Type, Data, Signature, ES) ->
    Current = aefa_engine_state:current_contract(ES),
    check_delegation_signature(Type, Data, Signature, Current, ES).

check_delegation_signature(Type, Data, SignBin, Current, ES) ->
    {Bin, Pubkey} = delegation_signature_data(Type, Data, Current),
    case Pubkey =:= Current of
        true ->
            ES;
        false ->
            API = aefa_engine_state:chain_api(ES),
            case aefa_chain_api:check_delegation_signature(Pubkey, Bin, SignBin, API) of
                {ok, API1} ->
                    aefa_engine_state:set_chain_api(API1, ES);
                error ->
                    aefa_fate:abort({primop_error, Type, bad_signature}, ES)
            end
    end.

delegation_signature_data(oracle_register, Pubkey, Current) ->
    {<<Pubkey/binary, Current/binary>>, Pubkey};
delegation_signature_data(oracle_extend, Pubkey, Current) ->
    {<<Pubkey/binary, Current/binary>>, Pubkey};
delegation_signature_data(oracle_respond, {Pubkey, QueryId}, Current) ->
    {<<QueryId/binary, Current/binary>>, Pubkey}.

aens_resolve(_EngineState) -> exit({error, op_not_implemented_yet}).

aens_preclaim(_EngineState) -> exit({error, op_not_implemented_yet}).

aens_claim(_EngineState) -> exit({error, op_not_implemented_yet}).

aend_update(_EngineState) -> exit({error, op_not_implemented_yet}).

aens_transfer(_EngineState) -> exit({error, op_not_implemented_yet}).

aens_revoke(_EngineState) -> exit({error, op_not_implemented_yet}).

ecverify(Arg0, Arg1, Arg2, Arg3, ES) ->
    ter_op(ecverify, {Arg0, Arg1, Arg2, Arg3}, ES).

ecverify_secp256k1(Arg0, Arg1, Arg2, Arg3, ES) ->
    ter_op(ecverify_secp256k1, {Arg0, Arg1, Arg2, Arg3}, ES).

contract_to_address(Arg0, Arg1, ES) ->
    un_op(contract_to_address, {Arg0, Arg1}, ES).

sha3(Arg0, Arg1, EngineState) ->
    un_op(sha3, {Arg0, Arg1}, EngineState).

sha256(Arg0, Arg1, EngineState) ->
    un_op(sha256, {Arg0, Arg1}, EngineState).

blake2b(Arg0, Arg1, EngineState) ->
    un_op(blake2b, {Arg0, Arg1}, EngineState).

dummyarg(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4, _Arg5, _Arg6, _EngineState) ->
 exit({error, op_not_implemented_yet}).

dummyarg(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4, _Arg5, _Arg6, _Arg7, _EngineState) ->
 exit({error, op_not_implemented_yet}).

-spec abort(_, _) -> no_return().
abort(Arg0, EngineState) ->
    {Value, ES1} = get_op_arg(Arg0, EngineState),
    case ?IS_FATE_STRING(Value) of
        true  -> aefa_fate:abort({abort, ?FATE_STRING_VALUE(Value)}, ES1);
        false -> aefa_fate:abort({value_does_not_match_type, Value, string}, ES1)
    end.

exit(_Arg0, _EngineState) ->
 exit({error, op_not_implemented_yet}).

nop(EngineState) ->
    EngineState.

auth_tx_hash(_Arg0, _EngineState) ->
    exit(nyi).

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
write({arg, N}, _, ES) ->
    aefa_fate:abort({cannot_write_to_arg, N}, ES).


%% ------------------------------------------------------
%% Variant instructions
%% ------------------------------------------------------


make_variant(Arities, Tag, NoElements, ES)  when ?IS_FATE_LIST(Arities)
                                              , ?IS_FATE_INTEGER(Tag)
                                              , ?IS_FATE_INTEGER(NoElements)
                                              , NoElements >= 0
                                              , Tag < length(Arities)
                                              , Tag >= 0 ->
    {Elements, ES2} = aefa_fate:pop_n(NoElements, ES),
    Values = list_to_tuple(Elements),
    {aeb_fate_data:make_variant(Arities, Tag, Values), ES2};
make_variant(Arities, Tag, NoElements, ES) ->
    aefa_fate:abort({bad_arguments_to_make_variant, Arities, Tag, NoElements}, ES).

%% ------------------------------------------------------
%% Tuple instructions
%% ------------------------------------------------------





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
op(map_to_list, A) when ?IS_FATE_MAP(A) ->
    Tuples = [aeb_fate_data:make_tuple({K, V})
              || {K, V} <- maps:to_list(?FATE_MAP_VALUE(A))],
    aeb_fate_data:make_list(Tuples);
op(map_size, A) when ?IS_FATE_MAP(A) ->
    aeb_fate_data:make_integer(map_size(?FATE_MAP_VALUE(A)));
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
op(sha3, A) ->
    Bin  = binary_for_hashing(A),
    Hash = aec_hash:hash(evm, Bin),
    ?FATE_HASH(Hash);
op(sha256, A) ->
    Bin  = binary_for_hashing(A),
    Hash = aec_hash:sha256_hash(Bin),
    ?FATE_HASH(Hash);
op(blake2b, A) ->
    Bin  = binary_for_hashing(A),
    Hash = aec_hash:blake2b_256_hash(Bin),
    ?FATE_HASH(Hash);
op(contract_to_address, A) when ?IS_FATE_CONTRACT(A) ->
    ?FATE_ADDRESS(?FATE_CONTRACT_VALUE(A)).

binary_for_hashing(S) when ?IS_FATE_STRING(S) ->
    ?FATE_STRING_VALUE(S);  %% Makes Crypto.sha3 and String.sha3 coincide.
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
op(pow, A, B)  when ?IS_FATE_INTEGER(A), ?IS_FATE_INTEGER(B) ->
    if B < 0 ->
           aefa_fate:abort({arithmetic_error, negative_exponent});
       true ->
           pow(A, B)
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
op(map_lookup, Map, Key) when ?IS_FATE_MAP(Map),
                              not ?IS_FATE_MAP(Key) ->
    case maps:get(Key, ?FATE_MAP_VALUE(Map), void) of
        void -> aefa_fate:abort(missing_map_key);
        Res -> Res
    end;
op(map_member, Map, Key) when ?IS_FATE_MAP(Map),
                              not ?IS_FATE_MAP(Key) ->
    aeb_fate_data:make_boolean(maps:is_key(Key, ?FATE_MAP_VALUE(Map)));
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
    ?FATE_BITS((BitsA band BitsB) bxor BitsA).

%% Terinay operations
op(map_lookup_default, Map, Key, Default) when ?IS_FATE_MAP(Map),
                                               not ?IS_FATE_MAP(Key) ->
    maps:get(Key, ?FATE_MAP_VALUE(Map), Default);
op(map_update, Map, Key, Value) when ?IS_FATE_MAP(Map),
                              not ?IS_FATE_MAP(Key) ->
    Res = maps:put(Key, Value, ?FATE_MAP_VALUE(Map)),
    aeb_fate_data:make_map(Res);
op(ecverify, Msg, PK, Sig) when ?IS_FATE_HASH(Msg)
                              , ?IS_FATE_ADDRESS(PK)
                              , ?IS_FATE_SIGNATURE(Sig) ->
    {?FATE_HASH(Msg1), ?FATE_ADDRESS(PK1), ?FATE_SIGNATURE(Sig1)} = {Msg, PK, Sig},
    aeu_crypto:ecverify(Msg1, PK1, Sig1);
op(ecverify_secp256k1, Msg, PK, Sig) when ?IS_FATE_HASH(Msg)
                                        , ?IS_FATE_SIGNATURE(PK)
                                        , ?IS_FATE_SIGNATURE(Sig) ->
    {?FATE_HASH(Msg1), ?FATE_SIGNATURE(PK1), ?FATE_SIGNATURE(Sig1)} = {Msg, PK, Sig},
    aeu_crypto:ecverify(secp256k1, Msg1, PK1, Sig1).


bits_sum(0, Sum) -> Sum;
bits_sum(N, Sum) -> bits_sum(N bsr 1, Sum + (N band 2#1)).



%% ------------------------------------------------------
%% Comparison instructions
%% ------------------------------------------------------

bin_comp(Comp, {To, Left, Right}, ES) ->
    {LeftValue, ES1} = get_op_arg(Left, ES),
    {RightValue, ES2} = get_op_arg(Right, ES1),
    Result = comp(Comp, LeftValue, RightValue),
    write(To, Result, ES2).

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

%% TODO: we should check Gas as we go along here...
pow(A, B) ->
    pow(A, B, 1).

pow(_, 0, R)                   -> R;
pow(A, B, R) when B rem 2 == 0 -> pow(A * A, B bsr 1, R);
pow(A, B, R)                   -> pow(A * A, B bsr 1, R * A).


