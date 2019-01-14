-module(aestratum_nonce_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aestratum_nonce).

nonce_test_() ->
    [new(badarg_nonce),
     new(badarg_part_nonce),
     new(valid_nonce),
     new(valid_part_nonce),
     to_hex(badarg),
     to_hex(valid_nonce),
     to_hex(valid_part_nonce),
     to_int(badarg_nonce),
     to_int(badarg_part_nonce),
     to_int(valid_nonce),
     to_int(valid_part_nonce),
     max(badarg),
     merge(badarg),
     merge(valid),
     type(badarg),
     type(valid),
     value(badarg),
     value(valid),
     nbytes(badarg),
     nbytes(valid)].

new(badarg_nonce) ->
    L = [atom, [], -1, -1.0, <<>>, 16#ffffffffffffffff + 1],
    [?_assertException(error, badarg, ?TEST_MODULE:new(I)) || I <- L];
new(badarg_part_nonce) ->
    L = [{x, y, z}, {0, 1, 1}, {unknown_type, 1000, 5},
         {extra, -1, 4}, {extra, 1.0, 1}, {miner, 0, 0},
         {extra, 1, 8}, {miner, -1, 0}, {miner, 4, 16#ffffffff + 1}],
    [?_assertException(error, badarg, ?TEST_MODULE:new(T, I, N)) || {T, I, N} <- L];
new(valid_nonce) ->
    [?_assertEqual(<<"0000000000000000">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(0))),
     ?_assertEqual(<<"0100000000000000">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(1))),
     ?_assertEqual(<<"0f00000000000000">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(16#f))),
     ?_assertEqual(<<"ff00000000000000">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(16#ff))),
     ?_assertEqual(<<"f101ff0000000000">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(16#ff01f1))),
     ?_assertEqual(<<"4523010000000000">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(16#012345)))];
new(valid_part_nonce) ->
    [?_assertEqual(<<"00">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(miner, 0, 1))),
     ?_assertEqual(<<"0100">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(extra, 1, 2))),
     ?_assertEqual(<<"ff0000">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(extra, 16#ff, 3))),
     ?_assertEqual(<<"0f000000">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(extra, 16#f, 4))),
     ?_assertEqual(<<"0a010f00000000">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(miner, 16#0f010a, 7)))].

to_hex(badarg) ->
    L = [foo, {}, <<>>, 1000, 0.0, -1],
    [?_assertException(error, badarg, ?TEST_MODULE:to_hex(I)) || I <- L];
to_hex(valid_nonce) ->
    [?_assertEqual(<<"0000000000000000">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(0))),
     ?_assertEqual(<<"0807060504030201">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(16#0102030405060708))),
     ?_assertEqual(<<"ffffffff00000000">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(16#ffffffff)))];
to_hex(valid_part_nonce) ->
    [?_assertEqual(<<"00">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(extra, 0, 1))),
     ?_assertEqual(<<"0a0b">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(miner, 16#0b0a, 2))),
     ?_assertEqual(<<"04030201">>, ?TEST_MODULE:to_hex(?TEST_MODULE:new(miner, 16#01020304, 4)))].

to_int(badarg_nonce) ->
    L = [<<>>, <<"00">>, 16#ff, [],
         <<"010203040506070">>, <<"0102030405060708f">>],
    [?_assertException(error, badarg, ?TEST_MODULE:to_int(B)) || B <- L];
to_int(badarg_part_nonce) ->
    L = [{no_type, <<"00">>, 1}, {extra, <<>>, 1}, {miner, <<"0">>, 1},
         {miner, <<"111">>, 2}, {extra, [], 5}, {extra, <<"01020304050607">>, 8},
         {extra, <<"0102030405060708">>, 8}],
    [?_assertException(error, badarg, ?TEST_MODULE:to_int(T, B, N)) || {T, B, N} <- L];
to_int(valid_nonce) ->
    [?_assertEqual(0, ?TEST_MODULE:to_int(<<"0000000000000000">>)),
     ?_assertEqual(16#abcdef, ?TEST_MODULE:to_int(<<"efcdab0000000000">>)),
     ?_assertEqual(16#0101010102020202, ?TEST_MODULE:to_int(<<"0202020201010101">>))
    ];
to_int(valid_part_nonce) ->
    [?_assertEqual(0, ?TEST_MODULE:to_int(miner, <<"00">>, 1)),
     ?_assertEqual(16#ab, ?TEST_MODULE:to_int(miner, <<"ab000000">>, 4)),
     ?_assertEqual(16#1020304000, ?TEST_MODULE:to_int(miner, <<"0040302010">>, 5))].

max(badarg) ->
    L = [0, -1, 1.0, not_int, 8],
    [?_assertException(error, badarg, ?TEST_MODULE:max(I)) || I <- L].

merge(badarg) ->
    L = [{0, 0}, {<<>>, []}, {1, 2},
         {?TEST_MODULE:new(extra, 0, 4), ?TEST_MODULE:new(extra, 0, 4)},
         {?TEST_MODULE:new(extra, 0, 4), ?TEST_MODULE:new(miner, 0, 3)},
         {?TEST_MODULE:new(miner, 1, 5), ?TEST_MODULE:new(miner, 1, 2)}],
    [?_assertException(error, badarg, ?TEST_MODULE:merge(P1, P2)) || {P1, P2} <- L];
merge(valid) ->
    L = [{?TEST_MODULE:new(extra, 0, 7),
          ?TEST_MODULE:new(miner, 0, 1),
          0},
         %% 01 00 00 00 00 | 00 00 00 (little) -> 00 00 00 00 00 00 00 01 (big)
         {?TEST_MODULE:new(extra, 0, 3),
          ?TEST_MODULE:new(miner, 1, 5),
          16#0000000000000001},
         %% 01 00 | 00 00 00 00 00 00 (little) -> 00 00 00 00 00 00 00 01 (big)
         {?TEST_MODULE:new(miner, 1, 2),
          ?TEST_MODULE:new(extra, 0, 6),
          16#0000000000000001},
         %% 00 00 00 00 00 00 | 01 00 (little) -> 00 01 00 00 00 00 00 00 (big)
         {?TEST_MODULE:new(extra, 1, 2),
          ?TEST_MODULE:new(miner, 0, 6),
          16#0001000000000000},
         %% d0 c0 b0 a0 | 40 30 20 10 (little) -> 10 20 30 40 a0 b0 c0 d0 (big)
         {?TEST_MODULE:new(miner, 16#a0b0c0d0, 4),
          ?TEST_MODULE:new(extra, 16#10203040, 4),
          16#10203040a0b0c0d0}],
    [?_assertEqual(R, ?TEST_MODULE:value(?TEST_MODULE:merge(P1, P2))) || {P1, P2, R} <- L].

type(badarg) ->
    L = [{}, <<>>, atom, ?TEST_MODULE:new(999)],
    [?_assertException(error, badarg, ?TEST_MODULE:type(I)) || I <- L];
type(valid) ->
    [?_assertEqual(extra, ?TEST_MODULE:type(?TEST_MODULE:new(extra, 100, 4))),
     ?_assertEqual(miner, ?TEST_MODULE:type(?TEST_MODULE:new(miner, 999, 3)))].

value(badarg) ->
    L = [<<"not nonce">>, {1, 2, 3}, foo],
    [?_assertException(error, badarg, ?TEST_MODULE:type(I)) || I <- L];
value(valid) ->
    [?_assertEqual(999, ?TEST_MODULE:value(?TEST_MODULE:new(999))),
     ?_assertEqual(0, ?TEST_MODULE:value(?TEST_MODULE:new(0))),
     ?_assertEqual(12345, ?TEST_MODULE:value(?TEST_MODULE:new(extra, 12345, 3))),
     ?_assertEqual(90590500, ?TEST_MODULE:value(?TEST_MODULE:new(miner,90590500, 5)))].

nbytes(badarg) ->
    L = [atom, <<>>, {foo, bar}],
    [?_assertException(error, badarg, ?TEST_MODULE:nbytes(I)) || I <- L];
nbytes(valid) ->
    [?_assertEqual(8, ?TEST_MODULE:nbytes(?TEST_MODULE:new(123456))),
     ?_assertEqual(3, ?TEST_MODULE:nbytes(?TEST_MODULE:new(miner, 1000, 3))),
     ?_assertEqual(1, ?TEST_MODULE:nbytes(?TEST_MODULE:new(miner, 120, 1)))].

