%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Unit tests for oracle objects ADT
%%% @end
%%%-------------------------------------------------------------------

-module(aeo_oracles_tests).

-include_lib("eunit/include/eunit.hrl").

-import(aeo_oracles, [ deserialize/1
                     , expires/1
                     , id/1
                     , new/2
                     , owner/1
                     , query_fee/1
                     , query_format/1
                     , response_format/1
                     , serialize/1
                     , set_expires/2
                     , set_owner/2
                     , set_query_fee/2
                     , set_query_format/2
                     , set_response_format/2
                     ]).

basic_test_() ->
    [ {"Serialization test", fun basic_serialize/0}
    , {"Access test", fun basic_getters/0}
    , {"Update test", fun basic_setters/0}
    ].

basic_serialize() ->
    O = aeo_oracles:new(register_tx(), 1),
    ?assertEqual(O, deserialize(serialize(O))),
    ok.

basic_getters() ->
    O = aeo_oracles:new(register_tx(), 1),
    ?assert(is_integer(expires(O))),
    ?assert(is_binary(id(O))),
    ?assert(is_binary(owner(O))),
    ?assert(is_integer(query_fee(O))),
    ?assert(is_binary(query_format(O))),
    ?assert(is_binary(response_format(O))),
    ok.

basic_setters() ->
    O = aeo_oracles:new(register_tx(), 1),
    ?assertError({illegal, _, _}, set_expires(foo, O)),
    _ = set_expires(100, O),
    ?assertError({illegal, _, _}, set_owner(<<4711:64/unit:8>>, O)),
    _ = set_owner(<<42:65/unit:8>>, O),
    ?assertError({illegal, _, _}, set_query_fee(foo, O)),
    _ = set_query_fee(123, O),
    ?assertError({illegal, _, _}, set_query_format("foo", O)),
    _ = set_query_format(<<"string()"/utf8>>, O),
    ?assertError({illegal, _, _}, set_response_format("bar", O)),
    _ = set_response_format(<<"boolean()"/utf8>>, O),
    ok.


register_tx() ->
    register_tx(#{}).

register_tx(Override) ->
    Map = #{ account       => <<4711:65/unit:8>>
           , nonce         => 42
           , query_spec    => <<"{foo: bar}"/utf8>>
           , response_spec => <<"boolean()"/utf8>>
           , query_fee     => 10
           , ttl           => {delta, 100}
           , fee           => 10
           },
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aeo_register_tx:new(Map1),
    {aeo_register_tx, RTx} = aetx:specialize_type(Tx),
    RTx.
