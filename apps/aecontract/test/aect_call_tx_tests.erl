%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Unit tests for contract call tx ADT
%%% @end
%%%-------------------------------------------------------------------

-module(aect_call_tx_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    [ {"Gas-related getters test", fun gas_getters/0}
    ].

gas_getters() ->
    Tx = call_tx(#{gas_price => 123, gas => 456}),
    ?assertEqual(123, aect_call_tx:gas_price(Tx)),
    ?assertEqual(456, aect_call_tx:gas_limit(Tx)),
    ?assertMatch(G when G >= 456, aect_call_tx:gas(Tx)),
    ok.

call_tx(Override) ->
    Map = #{ caller_id   => aeser_id:create(account, <<42:32/unit:8>>)
           , nonce       => 42
           , contract_id => aeser_id:create(contract, <<4711:32/unit:8>>)
           , fee         => 100
           , ttl         => 100
           , abi_version => 1
           , amount      => 100
           , gas         => 100
           , gas_price   => 1
           , call_data   => <<"CALL DATA">>
           },
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aect_call_tx:new(Map1),
    {contract_call_tx, CTx} = aetx:specialize_type(Tx),
    CTx.
