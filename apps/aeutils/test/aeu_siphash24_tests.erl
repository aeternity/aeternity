%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aeu_siphash24 module
%%%
%%%   In addition the aeu_siphash24 module was compared to the C reference
%%%   implementation by writing a QuickCheck property.
%%% @end
%%%=============================================================================
-module(aeu_siphash24_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

siphash_test_() ->
    {"Tests for SipHash24 hash implementation",
     [ fun() -> siphash(Data) end || Data <- test_data_siphash() ]}.

test_data_siphash() ->
    %% {K0, K1, Nonce, ExpectedResult}
    [ {16#0000000000000000, 16#0000000000000000, 16#0000000000000000, 16#1e924b9d737700d7}
    , {16#0000000000000001, 16#0000000000000001, 16#0000000000000001, 16#557e0560400e9b89}
    , {16#96c0a21270372000, 16#aef47a4f1f44d000, 16#dcfebf56eb8c6000, 16#0706a2102a9ea876}
    , {16#bb56ac4491e8c000, 16#5f6172c681af8000, 16#939351be791e5000, 16#d7187bd0bcba3847}
    , {16#6eee0301703d8000, 16#3f079c27f3594000, 16#009ff6e8842dc000, 16#300c39474750a7f6}
    , {16#a8a5fa84647a5000, 16#021e7cf8e29bc000, 16#e1f40b6177e1c000, 16#4cd59ee216485db0}
    , {16#0b56ce6a5d3ac000, 16#df51b066aa406000, 16#5312f1f41360e000, 16#50515a9800f263ec}
    , {16#fc40db5be1a15800, 16#f634d578f51e9000, 16#618c0113cdced000, 16#c4f9bf08c14e37d1} ].

siphash({K0, K1, Nonce, ExpectedOut}) ->
    Res = aeu_siphash24:hash(K0, K1, Nonce),
    ?assertEqual(Res, ExpectedOut).

-endif.
