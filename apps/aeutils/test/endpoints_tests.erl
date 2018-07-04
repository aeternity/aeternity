-module(endpoints_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

operation_method_test() ->
   ?assertEqual([get], maps:keys(endpoints:operation('GetTop'))).

path_without_test() ->
   ?assertEqual(<<"/v2/blocks/top">>, endpoints:path(get, 'GetTop', #{})).

path_with_params_test() ->
   ?assertEqual(<<"/v2/block/height/3">>,
      iolist_to_binary(endpoints:path(get, 'GetKeyBlockByHeight',
                                      #{"height" => 3, "tx_objects" => true}))),
   ?assertEqual(<<"/v2/block/height/3">>,
      iolist_to_binary(endpoints:path(get, 'GetKeyBlockByHeight',
                                      #{"height" => 3}))),
   ?assertThrow({error, _},
      iolist_to_binary(endpoints:path(get, 'GetKeyBlockByHeight',
                                      #{}))).

-endif.
