%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Unit tests for oracle query object ADTs
%%% @end
%%%-------------------------------------------------------------------

-module(aeo_query_tests).

-include_lib("eunit/include/eunit.hrl").

-import(aeo_query, [ deserialize/1
                   , expires/1
                   , fee/1
                   , id/1
                   , new/2
                   , oracle_address/1
                   , response/1
                   , response_ttl/1
                   , sender_address/1
                   , sender_nonce/1
                   , serialize/1
                   , set_expires/2
                   , set_fee/2
                   , set_oracle_address/2
                   , set_response/2
                   , set_response_ttl/2
                   , set_sender_address/2
                   , set_sender_nonce/2
                   ]).

basic_test_() ->
    [ {"Serialization test", fun basic_serialize/0}
    , {"Access test", fun basic_getters/0}
    , {"Update test", fun basic_setters/0}
    ].

basic_serialize() ->
    O = new(query_tx(), 1),
    ?assertEqual(O, deserialize(serialize(O))),
    ok.

basic_getters() ->
    I = new(query_tx(), 1),
    ?assert(is_integer(expires(I))),
    ?assert(is_integer(fee(I))),
    ?assert(is_binary(id(I))),
    ?assert(is_binary(oracle_address(I))),
    ?assertMatch(undefined, response(I)),
    ?assertMatch({delta, _}, response_ttl(I)),
    ?assert(is_binary(sender_address(I))),
    ?assert(is_integer(sender_nonce(I))),
    ok.

basic_setters() ->
    I = aeo_query:new(query_tx(), 1),
    ?assertError({illegal, _, _}, set_expires(foo, I)),
    _ = set_expires(100, I),
    ?assertError({illegal, _, _}, set_fee(foo, I)),
    _ = set_fee(123, I),
    ?assertError({illegal, _, _}, set_oracle_address(foo, I)),
    _ = set_oracle_address(<<123:65/unit:8>>, I),
    ?assertError({illegal, _, _}, set_response(true, I)),
    _ = set_response(<<"true">>, I),
    ?assertError({illegal, _, _}, set_response_ttl({block, 100}, I)),
    _ = set_response_ttl({delta, 10}, I),
    ?assertError({illegal, _, _}, set_sender_address(foo, I)),
    _ = set_sender_address(<<123:65/unit:8>>, I),
    ?assertError({illegal, _, _}, set_sender_nonce(-1, I)),
    _ = set_sender_nonce(1, I),
    ok.

query_tx() ->
    query_tx(#{}).

query_tx(Override) ->
    Map = #{ sender        => <<42:65/unit:8>>
           , nonce         => 42
           , oracle        => <<4711:65/unit:8>>
           , query         => <<"{foo: bar}"/utf8>>
           , query_fee     => 10
           , query_ttl     => {delta, 100}
           , response_ttl  => {delta, 100}
           , fee           => 100
           },
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aeo_query_tx:new(Map1),
    {aeo_query_tx, QTx} = aetx:specialize_type(Tx),
    QTx.
