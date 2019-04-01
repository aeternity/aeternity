%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Unit tests for contract create tx ADT
%%% @end
%%%-------------------------------------------------------------------

-module(aect_create_tx_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/aecontract.hrl").

basic_test_() ->
    [ {"Gas-related getters test", fun gas_getters/0}
    ].

gas_getters() ->
    Tx = create_tx(#{gas_price => 123, gas => 456}),
    ?assertEqual(123, aect_create_tx:gas_price(Tx)),
    ?assertEqual(456, aect_create_tx:gas_limit(Tx)),
    ?assertMatch(G when G >= 456, aect_create_tx:gas(Tx)),
    ok.

create_tx(Override) ->
    Map = #{ owner_id    => aeser_id:create(account, <<4711:32/unit:8>>)
           , nonce       => 42
           , code        => <<"THIS IS NOT ACTUALLY PROPER BYTE CODE">>
           , vm_version  => ?VM_AEVM_SOLIDITY_1
           , abi_version => ?ABI_SOLIDITY_1
           , fee         => 10
           , ttl         => 100
           , deposit     => 100
           , amount      => 50
           , gas         => 100
           , gas_price   => 5
           , call_data   => <<"NOT ENCODED ACCORDING TO ABI">>
           },
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aect_create_tx:new(Map1),
    {contract_create_tx, CTx} = aetx:specialize_type(Tx),
    CTx.
