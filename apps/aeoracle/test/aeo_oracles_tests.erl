%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Unit tests for oracle objects ADT
%%% @end
%%%-------------------------------------------------------------------

-module(aeo_oracles_tests).

-include_lib("eunit/include/eunit.hrl").

-import(aeo_oracles, [ deserialize/2
                     , expires/1
                     , id/1
                     , pubkey/1
                     , new/2
                     , query_fee/1
                     , query_format/1
                     , response_format/1
                     , serialize/1
                     , set_expires/2
                     , set_pubkey/2
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
    Pubkey = aeo_oracles:pubkey(O),
    ?assertEqual(O, deserialize(Pubkey, serialize(O))),
    ok.

basic_getters() ->
    O = aeo_oracles:new(register_tx(), 1),
    ?assertEqual(oracle, aec_id:specialize_type(id(O))),
    ?assert(is_integer(expires(O))),
    ?assert(is_binary(pubkey(O))),
    ?assert(is_integer(query_fee(O))),
    ?assert(is_binary(query_format(O))),
    ?assert(is_binary(response_format(O))),
    ok.

basic_setters() ->
    O = aeo_oracles:new(register_tx(), 1),
    ?assertError({illegal, _, _}, set_expires(foo, O)),
    _ = set_expires(100, O),
    ?assertError({illegal, _, _}, set_pubkey(<<4711:64/unit:8>>, O)),
    _ = set_pubkey(<<42:32/unit:8>>, O),
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
    Map = #{ account_id      => aec_id:create(account, <<4711:32/unit:8>>)
           , nonce           => 42
           , query_format    => <<"{foo: bar}"/utf8>>
           , response_format => <<"boolean()"/utf8>>
           , query_fee       => 10
           , oracle_ttl      => {delta, 100}
           , fee             => 10
           , ttl             => 100
           },
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aeo_register_tx:new(Map1),
    {oracle_register_tx, RTx} = aetx:specialize_type(Tx),
    RTx.
