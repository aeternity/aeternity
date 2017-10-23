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
    {ok, SerializedHeader} = ?TEST_MODULE:serialize_to_map(Header),
    {ok, DeserializedHeader} =
        ?TEST_MODULE:deserialize_from_map(SerializedHeader),
    ?assertEqual(Header, DeserializedHeader),
    ?assertEqual({ok, SerializedHeader},
                 ?TEST_MODULE:serialize_to_map(DeserializedHeader)).

hash_test() ->
    Header = #header{},
    {ok, _HeaderHash} = ?TEST_MODULE:hash_header(Header).

validate_test_() ->
    {foreach,
     fun() ->
             meck:new(aec_pow_cuckoo, [passthrough]),
             meck:new(aeu_time, [passthrough])
     end,
     fun(_) ->
             meck:unload(aec_pow_cuckoo),
             meck:unload(aeu_time)
     end,
     [fun() ->
              Header = #header{version = 736},
              ?assertEqual({error, protocol_version_mismatch}, ?TEST_MODULE:validate(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, false),
              Header = #header{},
              ?assertEqual({error, incorrect_pow}, ?TEST_MODULE:validate(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, true),
              NowTime = 7592837461,
              meck:expect(aeu_time, now_in_msecs, 0, NowTime),
              Header = #header{time = 2 * NowTime},
              ?assertEqual({error, block_from_the_future}, ?TEST_MODULE:validate(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, true),
              Header = #header{},
              ?assertEqual(ok, ?TEST_MODULE:validate(Header))
      end]}.
