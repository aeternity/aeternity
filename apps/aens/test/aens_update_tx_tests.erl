%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aens_update_tx_tests).

-include_lib("eunit/include/eunit.hrl").

-import(aens_update_tx, [ deserialize/2
                        , serialize/1
                        , serialize/2
                        ]).

round_trip_test_() ->
    [ {"Serialization - no raw data pointers",
       fun() -> serialization_of_pointers(old_pointers(), 1) end}
    , {"Serialization - with raw data pointers",
       fun() -> serialization_of_pointers(new_pointers(), 2) end}
    ].

bad_version_test_() ->
    [ {"Incorrect round-trip - no raw data pointers in v2",
       fun() -> incorrect_serialization(old_pointers(), 2, invalid_serialization_version) end}
    , {"Incorrect round-trip - raw data pointers in v1",
       fun() -> incorrect_serialization(new_pointers(), 1, function_clause) end}
    ].

serialization_of_pointers(Pointers, ExpectedVsn) ->
    Tx = update_tx(#{pointers => Pointers}),
    {Vsn, Fields} = serialize(Tx),
    ?assertEqual(Tx, deserialize(Vsn, Fields)),
    ?assertEqual(ExpectedVsn, Vsn).

incorrect_serialization(Pointers, Vsn, ExpectedErr) ->
    Tx = update_tx(#{pointers => Pointers}),
    {Vsn, Fields} = serialize(Tx, Vsn),
    ?assertError(ExpectedErr, deserialize(Vsn, Fields)).


old_pointers() ->
    [ aens_pointer:new(<<"key1">>, aeser_id:create(account, <<1:32/unit:8>>))
    , aens_pointer:new(<<"key2">>, aeser_id:create(contract, <<1:32/unit:8>>)) ].

new_pointers() ->
    [ aens_pointer:new(<<"key1">>, aeser_id:create(account, <<1:32/unit:8>>))
    , aens_pointer:new(<<"key2">>, <<42:15/unit:8>>) ].

update_tx(Override) ->
    Map = #{ account_id => aeser_id:create(account, <<111:32/unit:8>>)
           , nonce => 3
           , name_id => aeser_id:create(name, <<222:32/unit:8>>)
           , client_ttl => 345
           , name_ttl => 123
           , pointers => []
           , fee => 50000
           },
    Map1 = maps:merge(Map, Override),
    [] = maps:keys(Map1) -- maps:keys(Map), %% Hardcode expectation of no unknown keys.
    {ok, Tx} = aens_update_tx:new(Map1),
    {name_update_tx, UTx} = aetx:specialize_type(Tx),
    UTx.
