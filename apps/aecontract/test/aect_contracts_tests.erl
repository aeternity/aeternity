%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Unit tests for contract objects ADT
%%% @end
%%%-------------------------------------------------------------------

-module(aect_contracts_tests).

-include_lib("eunit/include/eunit.hrl").

-import(aect_contracts, [ deserialize/2
                        , id/1
                        , new/2
                        , owner/1
                        , pubkey/1
                        , state/1
                        , serialize/1
                        , set_owner/2
                        , set_state/2
                        ]).

basic_test_() ->
    [ {"Serialization test", fun basic_serialize/0}
    , {"Access test", fun basic_getters/0}
    , {"Update test", fun basic_setters/0}
    , {"Update state test", fun state_setter/0}
    ].

basic_serialize() ->
    C = aect_contracts:new(create_tx()),
    ContractPubKey = aect_contracts:pubkey(C),
    ?assertEqual(C, deserialize(ContractPubKey, serialize(C))),
    ok.

basic_getters() ->
    C = aect_contracts:new(create_tx()),
    ?assert(is_binary(id(C))),
    ?assert(is_binary(owner(C))),
    ?assert(is_binary(pubkey(C))),
    ok.

basic_setters() ->
    C = aect_contracts:new(create_tx()),
    ?assertError({illegal, _, _}, set_owner(<<4711:64/unit:8>>, C)),
    _ = set_owner(<<42:32/unit:8>>, C),
    ok.

state_setter() ->
    C = aect_contracts:new(create_tx()),
    ?assertEqual(#{}, state(set_state(#{}, C))),
    ?assertEqual(#{<<1>> => <<"v">>},
                 state(set_state(#{<<1>> => <<"v">>}, C))),
    ?assertEqual(#{<<1>> => <<"v2">>,
                   <<2>> => <<"v1">>},
                 state(set_state(#{<<1>> => <<"v2">>,
                                   <<2>> => <<"v1">>}, C))),
    ?assertError({illegal, _, _}, set_state(#{<<>> => <<"v">>}, C)),
    ?assertEqual(#{<<1>> => <<>>}, state(set_state(#{<<1>> => <<>>}, C))),
    ?assertError({illegal, _, _}, set_state(#{<<1:4>> => <<"v">>}, C)),
    ?assertError({illegal, _, _}, set_state(#{<<1>> => <<1:4>>}, C)),
    ok.


create_tx() ->
    create_tx(#{}).

create_tx(Override) ->
    Map = #{ owner      => <<4711:32/unit:8>>
           , nonce      => 42
           , code       => <<"THIS IS NOT ACTUALLY PROPER BYTE CODE">>
           , vm_version => 2
           , fee        => 10
           , ttl        => 100
           , deposit    => 100
           , amount     => 50
           , gas        => 100
           , gas_price  => 5
           , call_data  => <<"NOT ENCODED ACCORDING TO ABI">>
           },
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aect_create_tx:new(Map1),
    {contract_create_tx, CTx} = aetx:specialize_type(Tx),
    CTx.
