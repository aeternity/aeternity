%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aeu_requests module
%%% @end
%%%=============================================================================
-module(aeu_hex_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Hex to binary",
       fun() ->
               ?assertEqual(<<1,2,100,255,128,12,64,123>>, aeu_hex:hex_to_bin("010264ff800c407b"))
       end},
      {"Binary to hex",
       fun() ->
               ?assertEqual("010264FF800C407B", aeu_hex:bin_to_hex(<<1,2,100,255,128,12,64,123>>))
        end}
     ]
    }.

setup() ->
    ok.
teardown(_) ->
    ok.
-endif.
