-module(aec_headers_tests).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

-define(TEST_MODULE, aec_headers).

getters_test() ->
    BlockHeader = #header{height = 11,
                          prev_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
                          version = ?PROTOCOL_VERSION},
    ?assertEqual(11, ?TEST_MODULE:height(BlockHeader)),
    ?assertEqual(<<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
                 ?TEST_MODULE:prev_hash(BlockHeader)).

network_serialization_test() ->
    Header = #header{ root_hash = <<0:32/unit:8>>,
                      version = ?PROTOCOL_VERSION },
    SerializedHeader = ?TEST_MODULE:serialize_to_binary(Header),
    DeserializedHeader =
        ?TEST_MODULE:deserialize_from_binary(SerializedHeader),
    ?assertEqual(Header, DeserializedHeader),
    ?assertEqual(SerializedHeader,
                 ?TEST_MODULE:serialize_to_binary(DeserializedHeader)).

hash_test() ->
    Header = #header{version = ?PROTOCOL_VERSION},
    {ok, _HeaderHash} = ?TEST_MODULE:hash_header(Header).

validate_test_() ->
    {foreach,
     fun() ->
             meck:new(aec_governance, [passthrough]),
             meck:new(aec_pow_cuckoo, [passthrough]),
             meck:new(aec_chain, [passthrough]),
             meck:expect(aec_chain, get_header, 1, error),
             meck:new(aeu_time, [passthrough])
     end,
     fun(_) ->
             meck:unload(aec_pow_cuckoo),
             meck:unload(aeu_time),
             meck:unload(aec_chain),
             meck:unload(aec_governance)
     end,
     [fun() ->
              Header = #header{version = 736},
              ?assertEqual({error, unknown_protocol_version}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              GV = ?GENESIS_VERSION,
              meck:expect(aec_governance, sorted_protocol_versions, 0,
                          [GV, 1+GV, 3+GV]),
              meck:expect(aec_governance, protocols, 0,
                          #{GV => ?GENESIS_HEIGHT,
                            1+GV => 100 + ?GENESIS_HEIGHT,
                            3+GV => 150 + ?GENESIS_HEIGHT}),
              %% Check for any off-by-one errors around first switch.
              ?assertEqual({error, {protocol_version_mismatch, GV}},
                           ?TEST_MODULE:validate_key_block_header(
                              #header{height = 99 + ?GENESIS_HEIGHT,
                                      version = 1+GV})),
              ?assertEqual({error, {protocol_version_mismatch, 1+GV}},
                           ?TEST_MODULE:validate_key_block_header(
                              #header{height = 100 + ?GENESIS_HEIGHT,
                                      version = GV})),
              ?assertEqual({error, {protocol_version_mismatch, 1+GV}},
                           ?TEST_MODULE:validate_key_block_header(
                              #header{height = 101 + ?GENESIS_HEIGHT,
                                      version = 3+GV})),
              %% Check for any off-by-one errors around second switch.
              ?assertEqual({error, {protocol_version_mismatch, 1+GV}},
                           ?TEST_MODULE:validate_key_block_header(
                              #header{height = 149 + ?GENESIS_HEIGHT,
                                      version = 3+GV})),
              ?assertEqual({error, {protocol_version_mismatch, 3+GV}},
                           ?TEST_MODULE:validate_key_block_header(
                              #header{height = 150 + ?GENESIS_HEIGHT,
                                      version = 1+GV})),
              ?assertEqual({error, {protocol_version_mismatch, 3+GV}},
                           ?TEST_MODULE:validate_key_block_header(
                              #header{height = 151 + ?GENESIS_HEIGHT,
                                      version = 1+GV})),
              ok
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, false),
              Header = #header{height = ?GENESIS_HEIGHT, version = ?GENESIS_VERSION},
              ?assertEqual({error, incorrect_pow}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, true),
              NowTime = 7592837461,
              meck:expect(aeu_time, now_in_msecs, 0, NowTime),
              Header = #header{time = 2 * NowTime,
                               height = ?GENESIS_HEIGHT, version = ?GENESIS_VERSION},
              ?assertEqual({error, block_from_the_future}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, true),
              Header = #header{height = ?GENESIS_HEIGHT, version = ?GENESIS_VERSION, time = ?GENESIS_TIME + 1},
              ?assertEqual(ok, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, false),
              Header = #header{height = ?GENESIS_HEIGHT, version = ?GENESIS_VERSION, time = ?GENESIS_TIME + 1},
              ?assertEqual({error, incorrect_pow}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, true),
              Header = #header{height = ?GENESIS_HEIGHT, version = ?GENESIS_VERSION},
              ?assertEqual({error, block_from_the_past}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, true),
              Header = #header{height = ?GENESIS_HEIGHT, version = ?GENESIS_VERSION,
                               time = aeu_time:now_in_msecs() + aec_governance:accepted_future_block_time_shift() + 100},
              ?assertEqual({error, block_from_the_future}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, true),
              Header = #header{nonce = -1,
                               height = ?GENESIS_HEIGHT, version = ?GENESIS_VERSION},
              ?assertError(function_clause, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, true),
              Header = #header{nonce = 16#1ffffffffffffffff,
                               height = ?GENESIS_HEIGHT, version = ?GENESIS_VERSION},
              ?assertError(function_clause, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              Header = #header{height = ?GENESIS_HEIGHT, version = ?GENESIS_VERSION},
              ?assertEqual(ok, ?TEST_MODULE:validate_micro_block_header(Header))
      end,
      fun() ->
              Header = #header{height = ?GENESIS_HEIGHT, version = ?GENESIS_VERSION,
                               time = aeu_time:now_in_msecs() + aec_governance:accepted_future_block_time_shift() + 100},
              ?assertEqual({error, block_from_the_future}, ?TEST_MODULE:validate_micro_block_header(Header))
      end]}.
