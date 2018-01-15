%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Unit tests for contract objects ADT
%%% @end
%%%-------------------------------------------------------------------

-module(aect_contracts_tests).

-include_lib("eunit/include/eunit.hrl").

-import(aect_contracts, [ deserialize/1
                        , id/1
                        , new/2
                        , owner/1
                        , serialize/1
                        , set_owner/2
                        ]).

basic_test_() ->
    [ {"Serialization test", fun basic_serialize/0}
    , {"Access test", fun basic_getters/0}
    , {"Update test", fun basic_setters/0}
    ].

basic_serialize() ->
    C = aect_contracts:new(create_tx(), 1),
    ?assertEqual(C, deserialize(serialize(C))),
    ok.

basic_getters() ->
    C = aect_contracts:new(create_tx(), 1),
    ?assert(is_binary(id(C))),
    ?assert(is_binary(owner(C))),
    ok.

basic_setters() ->
    C = aect_contracts:new(create_tx(), 1),
    ?assertError({illegal, _, _}, set_owner(<<4711:64/unit:8>>, C)),
    _ = set_owner(<<42:65/unit:8>>, C),
    ok.


create_tx() ->
    create_tx(#{}).

create_tx(Override) ->
    Map = #{ owner => <<4711:65/unit:8>>
           , nonce => 42
           , fee   => 10
           },
    Map1 = maps:merge(Map, Override),
    {ok, R} = aect_create_tx:new(Map1),
    R.
