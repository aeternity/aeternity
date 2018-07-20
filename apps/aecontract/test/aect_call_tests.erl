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
                   , gas_price/1
                   , gas_used/1
                   , height/1
                   , id/1
                   , new/5
                   , contract_id/1
                   , contract_pubkey/1
                   , caller_id/1
                   , caller_pubkey/1
                   , caller_nonce/1
                   , serialize/1
                   , set_return_value/2
                   , set_gas_used/2
                   , set_height/2
                   , set_contract/2
                   , set_caller/3
                   , set_caller_nonce/2
                   ]).

basic_test_() ->
    [ {"Serialization test", fun basic_serialize/0}
    , {"Access test", fun basic_getters/0}
    , {"Update test", fun basic_setters/0}
    ].

basic_serialize() ->
    C = new_call(call_tx(), 1),
    ?assertEqual(C, deserialize(serialize(C))),
    ok.

basic_getters() ->
    I = new_call(call_tx(), 1),
    ?assert(is_binary(id(I))),
    ?assert(is_binary(contract_pubkey(I))),
    ?assert(is_binary(caller_pubkey(I))),
    ?assert(is_integer(caller_nonce(I))),
    ?assert(is_integer(height(I))),
    ?assert(is_binary(return_value(I))),
    ?assert(is_integer(gas_price(I))),
    ?assert(is_integer(gas_used(I))),
    ok.

basic_setters() ->
    I = new_call(call_tx(), 1),
    ?assertError({illegal, _, _}, set_return_value(foo, I)),
    _ = set_return_value(<<"123">>, I),
    ?assertError({illegal, _, _}, set_gas_used(foo, I)),
    _ = set_gas_used(123, I),
    ?assertError({illegal, _, _}, set_contract(foo, I)),
    _ = set_contract(<<123:32/unit:8>>, I),
    ?assertError({illegal, _, _}, set_caller(account, foo, I)),
    _ = set_caller(account, <<123:32/unit:8>>, I),
    ?assertError({illegal, _, _}, set_caller(contract, foo, I)),
    _ = set_caller(contract, <<123:32/unit:8>>, I),
    ?assertError({illegal, _, _}, set_caller_nonce(-1, I)),
    _ = set_caller_nonce(1, I),
    ?assertError({illegal, _, _}, set_height(-1, I)),
    _ = set_height(1, I),
    ok.

new_call(Tx, Height) ->
    CallerId   = aect_call_tx:caller_id(Tx),
    Nonce      = aect_call_tx:nonce(Tx),
    ContractId = aect_call_tx:contract_id(Tx),
    GasPrice   = aect_call_tx:gas_price(Tx),
    new(CallerId, Nonce, ContractId, Height, GasPrice).

call_tx() ->
    call_tx(#{}).

call_tx(Override) ->
    Map = #{ caller_id   => aec_id:create(account, <<42:32/unit:8>>)
           , nonce       => 42
           , contract_id => aec_id:create(contract, <<4711:32/unit:8>>)
           , fee         => 100
           , ttl         => 100
           , vm_version  => 1
           , amount      => 100
           , gas         => 100
           , gas_price   => 1
           , call_data   => <<"CALL DATA">>
           },
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aect_call_tx:new(Map1),
    {contract_call_tx, CTx} = aetx:specialize_type(Tx),
    CTx.
