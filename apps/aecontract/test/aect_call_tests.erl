%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Unit tests for contract interaction object ADTs
%%% @end
%%%-------------------------------------------------------------------

-module(aect_call_tests).

-include_lib("eunit/include/eunit.hrl").

-import(aect_call, [ deserialize/1
                   , return_value/1
                   , gas_used/1
                   , height/1
                   , id/1
                   , new/2
                   , contract_address/1
                   , caller_address/1
                   , caller_nonce/1
                   , serialize/1
                   , set_return_value/2
                   , set_gas_used/2
                   , set_height/2
                   , set_contract_address/2
                   , set_caller_address/2
                   , set_caller_nonce/2
                   ]).

basic_test_() ->
    [ {"Serialization test", fun basic_serialize/0}
    , {"Access test", fun basic_getters/0}
    , {"Update test", fun basic_setters/0}
    ].

basic_serialize() ->
    C = new(call_tx(), 1),
    ?assertEqual(C, deserialize(serialize(C))),
    ok.

basic_getters() ->
    I = new(call_tx(), 1),
    ?assert(is_binary(id(I))),
    ?assert(is_binary(contract_address(I))),
    ?assert(is_binary(caller_address(I))),
    ?assert(is_integer(caller_nonce(I))),
    ?assert(is_integer(height(I))),
    ?assert(is_binary(return_value(I))),
    ?assert(is_integer(gas_used(I))),
    ok.

basic_setters() ->
    I = aect_call:new(call_tx(), 1),
    ?assertError({illegal, _, _}, set_return_value(foo, I)),
    _ = set_return_value(<<"123">>, I),
    ?assertError({illegal, _, _}, set_gas_used(foo, I)),
    _ = set_gas_used(123, I),
    ?assertError({illegal, _, _}, set_contract_address(foo, I)),
    _ = set_contract_address(<<123:65/unit:8>>, I),
    ?assertError({illegal, _, _}, set_caller_address(foo, I)),
    _ = set_caller_address(<<123:65/unit:8>>, I),
    ?assertError({illegal, _, _}, set_caller_nonce(-1, I)),
    _ = set_caller_nonce(1, I),
    ?assertError({illegal, _, _}, set_height(-1, I)),
    _ = set_height(1, I),
    ok.

call_tx() ->
    call_tx(#{}).

call_tx(Override) ->
    Map = #{ caller     => <<42:65/unit:8>>
           , nonce      => 42
           , contract   => <<4711:65/unit:8>>
           , fee        => 100
           , vm_version => 1
           , amount     => 100
           , gas        => 100
           , gas_price  => 1
           , call_data  => <<"CALL DATA">>
           },
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aect_call_tx:new(Map1),
    {aect_call_tx, CTx} = aetx:specialize_type(Tx),
    CTx.
