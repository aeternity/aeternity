%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Unit tests for oracle extend transaction ADT
%%% @end
%%%-------------------------------------------------------------------

-module(aeo_extend_tx_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aeoracle/include/oracle_txs.hrl").

-import(aeo_extend_tx, [ deserialize/2
                       , serialize/1
                       ]).

no_absolute_extension_ttl_test() ->
    {Vsn, GoodFields} = _SerializedGoodExtendTx = serialize(extend_tx()),
    TypeF = oracle_ttl_type,
    ValueF = oracle_ttl_value,
    ?assertMatch({_, ?ttl_delta_int}, lists:keyfind(TypeF, 1, GoodFields)), %% Hardcoded expectation on test data.
    ?assertMatch({_, X} when is_integer(X) andalso X > 0, lists:keyfind(ValueF, 1, GoodFields)), %% Hardcoded expectation on test data.
    _ = deserialize(Vsn, GoodFields), %% Sanity check on happy path i.e. tx with relative TTL.
    BadFields = lists:keyreplace(TypeF, 1, GoodFields, {TypeF, ?ttl_block_int}),
    ?assertMatch({_, ?ttl_block_int}, lists:keyfind(TypeF, 1, BadFields)), %% Hardcoded expectation on test data.
    ?assertError(function_clause, deserialize(Vsn, BadFields)),
    ok.


extend_tx() ->
    extend_tx(#{}).

extend_tx(Override) ->
    Map = #{ oracle_id  => aeser_id:create(oracle, <<4711:32/unit:8>>)
           , nonce      => 42
           , oracle_ttl => {delta, 100}
           , fee        => 10
           , ttl        => 100
           },
    Map1 = maps:merge(Map, Override),
    {ok, Tx} = aeo_extend_tx:new(Map1),
    {oracle_extend_tx, ETx} = aetx:specialize_type(Tx),
    ETx.
