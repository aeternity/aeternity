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

network_serialization_test() ->
    Header = #header{},
    {ok, SerializedHeader} = ?TEST_MODULE:serialize_for_network(Header),
    {ok, DeserializedHeader} =
        ?TEST_MODULE:deserialize_from_network(SerializedHeader),
    ?assertEqual(Header, DeserializedHeader),
    ?assertEqual({ok, SerializedHeader},
                 ?TEST_MODULE:serialize_for_network(DeserializedHeader)).

hash_test() ->
    Header = #header{},
    {ok, SerializedHeader} = ?TEST_MODULE:serialize_for_network(Header),
    {ok, HeaderHash} =
        ?TEST_MODULE:hash_network_serialization(SerializedHeader),
    ?assertEqual({ok, HeaderHash},
                 ?TEST_MODULE:hash_internal_representation(Header)),
    ?assertEqual(aec_sha256:hash(SerializedHeader), HeaderHash).
