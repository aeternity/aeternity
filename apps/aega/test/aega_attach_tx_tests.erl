%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Unit tests for contract create tx ADT
%%% @end
%%%-------------------------------------------------------------------

-module(aega_attach_tx_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("apps/aecontract/src/aecontract.hrl").

basic_test_() ->
    [ {"Gas-related getters test", fun gas_getters/0}
    ].

gas_getters() ->
    Tx = attach_tx(#{gas_price => 123, gas => 456}),
    ?assertEqual(123, aega_attach_tx:gas_price(Tx)),
    ?assertEqual(456, aega_attach_tx:gas_limit(Tx)),
    ?assertMatch(G when G >= 456, aega_attach_tx:gas(Tx)),
    ok.

attach_tx(Override) ->
    Map = #{ owner_id    => aeser_id:create(account, <<4711:32/unit:8>>)
           , nonce       => 42
           , code        => <<"THIS IS NOT ACTUALLY PROPER BYTE CODE">>
           , auth_fun    => <<"NOT REALLY AN AUTH-FUN EITHER...">>
           , vm_version  => ?VM_AEVM_SOLIDITY_1
           , abi_version => ?ABI_SOLIDITY_1
           , fee         => 10
           , ttl         => 100
           , gas         => 100
           , gas_price   => 5
           , call_data   => <<"NOT ENCODED ACCORDING TO ABI">>
           },
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aega_attach_tx:new(Map1),
    {ga_attach_tx, CTx} = aetx:specialize_type(Tx),
    CTx.
