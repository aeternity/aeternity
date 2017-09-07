-module(aec_headers_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_headers).

getters_test() ->
    BlockHeader = #header{height = 11},
    ?assertEqual(11, ?TEST_MODULE:height(BlockHeader)).
