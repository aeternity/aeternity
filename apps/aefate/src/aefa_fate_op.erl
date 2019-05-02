%% Implements Fate operations.
%%
-module(aefa_fate_op).

-export([ return/1
        , returnr/2
        , call/2
        , call_r/3
        , call_t/2
        , call_tr/3
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
        , tuple/2
        , element_op/4
        , map_empty/2
        , map_lookup/4
        , map_lookup/5
        , map_update/5
        , map_delete/4
        , map_member/4
        , map_from_list/3
        , nil/2
        , is_nil/3
        , cons/4
        , hd/3
        , tl/3
        , length/3
        , str_eq/4
        , str_join/4
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
        , log/3
        , log/4
        , log/5
        , log/6
        , log/7
        , deactivate/1
        , spend/3
        , oracle_register/7
        , oracle_query/1
        , oracle_respond/1
        , oracle_extend/1
        , oracle_get_answer/1
        , oracle_get_question/1
        , oracle_query_fee/1
        , aens_resolve/1
        , aens_preclaim/1
        , aens_claim/1
        , aend_update/1
        , aens_transfer/1
        , aens_revoke/1
        , ecverify/1
        , sha3/1
        , sha256/1
        , blake2b/1
        , setelement/5
        , dummyarg/8
        , dummyarg/9
        , abort/2
        , exit/2
        , nop/1
        ]).

-include_lib("aebytecode/include/aeb_fate_data.hrl").
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
    Signature = aefa_fate:get_function_signature(Arg0, ES1),
    {ok, ES2} = aefa_fate:check_signature_and_bind_args(Signature, ES1),
    {jump, 0, aefa_fate:set_local_function(Arg0, ES2)}.

call_r(Arg0, Arg1, EngineState) ->
    ES1 = aefa_fate:push_return_address(EngineState),
    {Address, ES2} = get_op_arg(Arg0, ES1),
    ES3 = aefa_fate:set_function(Address, Arg1, ES2),
    Signature = aefa_fate:get_function_signature(Arg1, ES3),
    {ok, ES4} = aefa_fate:check_signature_and_bind_args(Signature, ES3),
    {jump, 0, ES4}.

call_t(Arg0, EngineState) ->
    Signature = aefa_fate:get_function_signature(Arg0, EngineState),
    {ok, ES2} = aefa_fate:check_signature_and_bind_args(Signature, EngineState),
    {jump, 0, aefa_fate:set_local_function(Arg0, ES2)}.

call_tr(Arg0, Arg1, EngineState) ->
    {Address, ES1} = get_op_arg(Arg0, EngineState),
    ES2 = aefa_fate:set_function(Address, Arg1, ES1),
    Signature = aefa_fate:get_function_signature(Arg1, ES2),
    {ok, ES3} = aefa_fate:check_signature_and_bind_args(Signature, ES2),
    {jump, 0, ES3}.

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
tuple(Arg0, EngineState) ->
    if is_integer(Arg0) andalso (Arg0 >= 0) ->
            make_tuple(Arg0, EngineState);
       true -> aefa_fate:abort({invalid_tuple_size, Arg0}, EngineState)
    end.

make_tuple(Size, ES) ->
    {Elements, ES2} = aefa_fate:pop_n(Size, ES),
    Tuple = list_to_tuple(Elements),
    FateTuple = aeb_fate_data:make_tuple(Tuple),
    aefa_fate:push(FateTuple, ES2).


element_op(To, Which, TupleArg, ES) ->
    {Index, ES1} = get_op_arg(Which, ES),
    {FateTuple, ES2} = get_op_arg(TupleArg, ES1),
    case aefa_fate:check_type(integer, Index)
        andalso (Index >= 0)
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
    case aefa_fate:check_type(integer, Index)
        andalso (Index >= 0)
        andalso ?IS_FATE_TUPLE(FateTuple) of
        false -> aefa_fate:abort({bad_arguments_to_setelement, Index, FateTuple}, ES3);
        true ->
            ?FATE_TUPLE(Tuple) = FateTuple,
            case size(Tuple) > Index of
                true ->
                    NewT = erlang:setelement(Index+1, Tuple, Element),
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

%% ------------------------------------------------------
%% String instructions
%% ------------------------------------------------------

str_eq(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(str_equal, {Arg0, Arg1, Arg2}, EngineState).

str_join(Arg0, Arg1, Arg2, EngineState) ->
    bin_op(str_join, {Arg0, Arg1, Arg2}, EngineState).

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
    Address = ?FATE_ADDRESS(_) = aefa_engine_state:current_contract(EngineState),
    write(Arg0, Address, EngineState).

balance(Arg0, EngineState) ->
    API = aefa_engine_state:chain_api(EngineState),
    ?FATE_ADDRESS(Pubkey) = aefa_engine_state:current_contract(EngineState),
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

log(_Arg0, _Arg1, _EngineState) -> exit({error, op_not_implemented_yet}).

log(_Arg0, _Arg1, _Arg2, _EngineState) -> exit({error, op_not_implemented_yet}).

log(_Arg0, _Arg1, _Arg2, _Arg3, _EngineState) -> exit({error, op_not_implemented_yet}).

log(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4, _EngineState) -> exit({error, op_not_implemented_yet}).

log(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4, _Arg5, _EngineState) -> exit({error, op_not_implemented_yet}).

deactivate(_EngineState) -> exit({error, op_not_implemented_yet}).

spend(Arg0, Arg1, ES0) ->
    ?FATE_ADDRESS(FromPubkey) = aefa_engine_state:current_contract(ES0),
    {Amount, ES1} = get_op_arg(Arg0, ES0),
    [aefa_fate:abort({value_does_not_match_type, Amount, integer}, ES1)
     || not ?IS_FATE_INTEGER(Amount)],
    case get_op_arg(Arg1, ES1) of
        {?FATE_ADDRESS(ToPubkey), ES2} ->
            API = aefa_engine_state:chain_api(ES2),
            case aefa_chain_api:spend(FromPubkey, ToPubkey, Amount, API) of
                {ok, API1}    -> aefa_engine_state:set_chain_api(API1, ES2);
                {error, What} -> aefa_fate:abort({primop_error, spend, What}, ES2)
            end;
        {Other, ES2} ->
            aefa_fate:abort({value_does_not_match_type, Other, address}, ES2)
    end.

oracle_register(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4, _Arg5, _EngineState) -> exit({error, op_not_implemented_yet}).

oracle_query(_EngineState) -> exit({error, op_not_implemented_yet}).

oracle_respond(_EngineState) -> exit({error, op_not_implemented_yet}).

oracle_extend(_EngineState) -> exit({error, op_not_implemented_yet}).

oracle_get_answer(_EngineState) -> exit({error, op_not_implemented_yet}).

oracle_get_question(_EngineState) -> exit({error, op_not_implemented_yet}).

oracle_query_fee(_EngineState) -> exit({error, op_not_implemented_yet}).

aens_resolve(_EngineState) -> exit({error, op_not_implemented_yet}).

aens_preclaim(_EngineState) -> exit({error, op_not_implemented_yet}).

aens_claim(_EngineState) -> exit({error, op_not_implemented_yet}).

aend_update(_EngineState) -> exit({error, op_not_implemented_yet}).

aens_transfer(_EngineState) -> exit({error, op_not_implemented_yet}).

aens_revoke(_EngineState) -> exit({error, op_not_implemented_yet}).

ecverify(_EngineState) -> exit({error, op_not_implemented_yet}).

sha3(_EngineState) -> exit({error, op_not_implemented_yet}).

sha256(_EngineState) -> exit({error, op_not_implemented_yet}).

blake2b(_EngineState) -> exit({error, op_not_implemented_yet}).

dummyarg(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4, _Arg5, _Arg6, _EngineState) ->
 exit({error, op_not_implemented_yet}).

dummyarg(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4, _Arg5, _Arg6, _Arg7, _EngineState) ->
 exit({error, op_not_implemented_yet}).

abort(_Arg0, _EngineState) ->
 exit({error, op_not_implemented_yet}).

exit(_Arg0, _EngineState) ->
 exit({error, op_not_implemented_yet}).

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

get_op_arg({stack, 0}, ES) ->
    aefa_engine_state:pop_accumulator(ES);
get_op_arg({arg,_N} = Var, ES) ->
    Value = aefa_fate:lookup_var(Var, ES),
    {Value, ES};
get_op_arg({var,_N} = Var, ES) ->
    Value = aefa_fate:lookup_var(Var, ES),
    {Value, ES};
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
op(bits_all, N)  when ?IS_FATE_INTEGER(N) ->
    ?FATE_BITS((1 bsl (N)) - 1);
op(bits_sum, A)  when ?IS_FATE_BITS(A) ->
    ?FATE_BITS(Bits) = A,
    if Bits < 0 -> aefa_fate:abort({arithmetic_error, bits_sum_on_infinite_set});
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
    if B =:= 0 -> aefa_fate:abort(division_by_zero);
       true -> A div B
    end;
op(pow, A, B)  when ?IS_FATE_INTEGER(A)
                    , ?IS_FATE_INTEGER(B) ->
    %% TODO: Implement arbitrary precision pow function.
    try round(math:pow(A, B)) of
        I -> I
    catch error:badarith ->
            aefa_fate:abort(pow_too_large_exp)
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
            case aefa_fate:check_type(aefa_fate:type(OldHd), Hd) of
                true ->
                    aeb_fate_data:make_list([Hd|?FATE_LIST_VALUE(Tail)]);
                false ->
                    aefa_fate:abort({type_error, cons, Hd, aefa_fate:type(OldHd)})
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
            aefa_fate:abort({type_error, variant_element, B, A})
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
