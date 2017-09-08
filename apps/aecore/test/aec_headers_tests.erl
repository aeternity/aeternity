-module(aec_headers_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_headers).

getters_test() ->
    BlockHeader = #header{height = 11,
                          prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>},
    ?assertEqual(11, ?TEST_MODULE:height(BlockHeader)),
    ?assertEqual(<<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
                 ?TEST_MODULE:prev_hash(BlockHeader)).
