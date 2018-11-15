-module(endpoints_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

operation_method_test() ->
   ?assertEqual([get], maps:keys(endpoints:operation('GetTopBlock'))).

path_without_test() ->
   ?assertEqual(<<"/v2/blocks/top">>, endpoints:path(get, 'GetTopBlock', #{})).

path_with_params_test() ->
   ?assertEqual(<<"/v2/key-blocks/height/3">>,
      iolist_to_binary(endpoints:path(get, 'GetKeyBlockByHeight',
                                      #{"height" => 3}))),
   ?assertThrow({error, _},
      iolist_to_binary(endpoints:path(get, 'GetKeyBlockByHeight',
                                      #{}))).

-endif.
