-module(endpoints_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

definitions_test() ->
    JEP = [ ok = jesse:add_schema(Def, Schema) || {Def, Schema} <- endpoints:definitions() ],
    {_Good, Bad} = lists:splitwith(fun(X) -> X == ok end, JEP),
    ?assertEqual([], Bad),
    ?assertEqual({ok, lists:seq(1,42)}, 
                 jesse:validate("/definitions/Pow", 
                                [ I || I<-lists:seq(1,42) ])),
    {error, [{data_invalid, _, wrong_type, _, _}]} = 
        jesse:validate("/definitions/Pow", [ "bla" || _ <- lists:seq(1,4) ]),
    {error, [{data_invalid, _, wrong_size, _, _}]} = 
        jesse:validate("/definitions/Pow", [ I || I <- lists:seq(1,4) ]).

ref_test() ->
    JEP = [ ok = jesse:add_schema(Def, Schema) || {Def, Schema} <- endpoints:definitions() ],
    {_Good, Bad} = lists:splitwith(fun(X) -> X == ok end, JEP),
    Pow =  #{<<"$ref">> => <<"/definitions/Pow">>},
    ?assertEqual([], Bad),
    ?assertEqual({ok, lists:seq(1,42)}, 
                 validate(Pow, [ I || I<-lists:seq(1,42) ])),
    {error, [{data_invalid, _, wrong_type, _, _}]} = 
        validate(Pow, [ "bla" || _ <- lists:seq(1,4) ]),
    {error, [{data_invalid, _, wrong_size, _, _}]} = 
        validate(Pow, [ I || I <- lists:seq(1,4) ]).

validate(Schema, Data) ->
  endpoints:validate(Schema, Data).

%% This tests reveals uncovered cases in creation json_schema from swagger
%% The difference are in "error" versus error tags
json_schema_test() ->
    Map = endpoints:json_schema(),
    Schema = jsx:prettify(jsx:encode(Map)),
    ?assertEqual(Map, jsx:decode(Schema, [return_maps])).

ping_pong_test() ->
    ok = jesse:add_schema("__response", #{<<"$ref">> => <<"/definitions/Ping">>}),
    {ok, _} = 
        jesse:validate("__response",
                       #{<<"best_hash">> =>
                         <<"bh$NHVtevgB9TCDpXXjJN3u5BZBzHpVdTXuWF4UobdB8BKdQt4fC">>,
                         <<"difficulty">> => 1.0,
                         <<"genesis_hash">> =>
                         <<"bh$NHVtevgB9TCDpXXjJN3u5BZBzHpVdTXuWF4UobdB8BKdQt4fC">>,
                         <<"peers">> => [],
                         <<"pong">> => <<"pong">>,
                         <<"share">> => 32,
                         <<"source">> => <<"http://ThomasComputer:3023">>}
                      ).

top_test() ->
    ok = jesse:add_schema("__response", #{<<"$ref">> => <<"/definitions/Top">>}),
    {ok, _} = 
        jesse:validate("__response",  
                       #{<<"hash">> => <<"bh$2CyMeYQnn3DgQyi7zBnJEzmuES1fZiuqpZeHYxG7zryGNPLRq7">>,
                         <<"height">> => 1,
                         <<"nonce">> => 11516624686304028612,
                         <<"pow">> => [221,739,1225,1926,2410,3816,4720,4746,5263,6962,7321,9085,9483,9836,9933,11771,11866,12229,12571,12686,13561,13785,14697,15958,16484,17205,17810,18192,18575,20864,21099,21158,22178,24027,24937,26110,26743,26774,27691,28031,30484,31347],
                         <<"prev_hash">> => <<"bh$NHVtevgB9TCDpXXjJN3u5BZBzHpVdTXuWF4UobdB8BKdQt4fC">>,
                         <<"state_hash">> => <<"bs$2FenHp7BchKxDBcjzwVxqwVJezWyXayhKZSWfJkGgdf9T8YLNU">>,
                         <<"target">> => 553713663,
                         <<"time">> => 1517230982680,
                         <<"txs_hash">> => <<"bx$MM7HikzBE9itWmwqtW19Z8SuNQucTcnGvDpsmazVJn8GexSUX">>,
                         <<"version">> => 5}).

in_path_test() ->
   ?assertEqual(<<"/v2/block/height/3">>, 
      iolist_to_binary(endpoints:path(get, 'GetBlockByHeightInternal', 
                                      #{"height" => 3, "tx_objects" => true}))).


-endif.
