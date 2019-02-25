%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aens_transfer_tx_tests).

-include_lib("eunit/include/eunit.hrl").

-import(aens_transfer_tx, [ deserialize/2
                          , serialize/1
                          ]).

-define(TEST_ID_VAL, <<42:32/unit:8>>).

serialization_test_() ->
    [ {"Serialization - case recipient is account",
       fun() -> serialization_of_recipient(aeser_id:create(account, ?TEST_ID_VAL)) end}
    , {"Serialization - case recipient is name",
       fun() -> serialization_of_recipient(aeser_id:create(name   , ?TEST_ID_VAL)) end}
    ].

serialization_of_recipient(Recipient) ->
    Tx = transfer_tx(#{recipient_id => Recipient}),
    {Vsn, Fields} = serialize(Tx),
    ?assertEqual(Tx, deserialize(Vsn, Fields)).

transfer_tx(Override) ->
    Map = #{ account_id => aeser_id:create(account, <<111:32/unit:8>>)
           , nonce => 3
           , name_id => aeser_id:create(name, <<222:32/unit:8>>)
           , recipient_id => aeser_id:create(account, <<333:32/unit:8>>)
           , fee => 5
           },
    Map1 = maps:merge(Map, Override),
    [] = maps:keys(Map1) -- maps:keys(Map), %% Hardcode expectation of no unknown keys.
    {ok, Tx} = aens_transfer_tx:new(Map1),
    {name_transfer_tx, TTx} = aetx:specialize_type(Tx),
    TTx.
