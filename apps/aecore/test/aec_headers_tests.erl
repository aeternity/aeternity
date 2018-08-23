-module(aec_headers_tests).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

-import(aec_headers, [raw_key_header/0,
                      raw_micro_header/0
                     ]).

-define(TEST_MODULE, aec_headers).

network_key_serialization_test() ->
    Header = raw_key_header(),
    SerializedHeader = ?TEST_MODULE:serialize_to_binary(Header),
    DeserializedHeader =
        ?TEST_MODULE:deserialize_from_binary(SerializedHeader),
    ?assertEqual(Header, DeserializedHeader),
    ?assertEqual(SerializedHeader,
                 ?TEST_MODULE:serialize_to_binary(DeserializedHeader)).

network_micro_serialization_test() ->
    Header = raw_micro_header(),
    SerializedHeader = ?TEST_MODULE:serialize_to_binary(Header),
    DeserializedHeader =
        ?TEST_MODULE:deserialize_from_binary(SerializedHeader),
    ?assertEqual(Header, DeserializedHeader),
    ?assertEqual(SerializedHeader,
                 ?TEST_MODULE:serialize_to_binary(DeserializedHeader)).

hash_test() ->
    {ok, _HeaderHash1} = ?TEST_MODULE:hash_header(raw_key_header()),
    {ok, _HeaderHash2} = ?TEST_MODULE:hash_header(raw_micro_header()).

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
              Header = ?TEST_MODULE:set_version(raw_key_header(), 736),
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
                              ?TEST_MODULE:set_version_and_height(
                                 raw_key_header(),
                                 1+GV,
                                 99 + ?GENESIS_HEIGHT))),
              ?assertEqual({error, {protocol_version_mismatch, 1+GV}},
                           ?TEST_MODULE:validate_key_block_header(
                              ?TEST_MODULE:set_version_and_height(
                                 raw_key_header(),
                                 GV,
                                 100 + ?GENESIS_HEIGHT))),
              ?assertEqual({error, {protocol_version_mismatch, 1+GV}},
                           ?TEST_MODULE:validate_key_block_header(
                              ?TEST_MODULE:set_version_and_height(
                                 raw_key_header(),
                                 3+GV,
                                 101 + ?GENESIS_HEIGHT))),
              %% Check for any off-by-one errors around second switch.
              ?assertEqual({error, {protocol_version_mismatch, 1+GV}},
                           ?TEST_MODULE:validate_key_block_header(
                              ?TEST_MODULE:set_version_and_height(
                                 raw_key_header(),
                                 3+GV,
                                 149 + ?GENESIS_HEIGHT))),
              ?assertEqual({error, {protocol_version_mismatch, 3+GV}},
                           ?TEST_MODULE:validate_key_block_header(
                              ?TEST_MODULE:set_version_and_height(
                                 raw_key_header(),
                                 1+GV,
                                 150 + ?GENESIS_HEIGHT))),
              ?assertEqual({error, {protocol_version_mismatch, 3+GV}},
                           ?TEST_MODULE:validate_key_block_header(
                              ?TEST_MODULE:set_version_and_height(
                                 raw_key_header(),
                                 1+GV,
                                 151 + ?GENESIS_HEIGHT))),
              ok
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, false),
              Header = ?TEST_MODULE:set_version_and_height(
                          raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              ?assertEqual({error, incorrect_pow}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, true),
              NowTime = 7592837461,
              meck:expect(aeu_time, now_in_msecs, 0, NowTime),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_time_in_msecs(Header0, 2 * NowTime),
              ?assertEqual({error, block_from_the_future}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, true),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_time_in_msecs(Header0, ?GENESIS_TIME + 1),
              ?assertEqual(ok, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, false),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_time_in_msecs(Header0, ?GENESIS_TIME + 1),
              ?assertEqual({error, incorrect_pow}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, true),
              Header = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              ?assertEqual({error, block_from_the_past}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, true),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_time_in_msecs(Header0,
                                                      aeu_time:now_in_msecs() + aec_governance:accepted_future_block_time_shift() + 100),
              ?assertEqual({error, block_from_the_future}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, true),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_nonce(Header0, -1),
              ?assertError(function_clause, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_pow_cuckoo, verify, 4, true),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_nonce(Header0, 16#1ffffffffffffffff),
              ?assertError(function_clause, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              Header = ?TEST_MODULE:set_version_and_height(
                           raw_micro_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              ?assertEqual(ok, ?TEST_MODULE:validate_micro_block_header(Header))
      end,
      fun() ->
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_micro_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_time_in_msecs(Header0,
                                                      aeu_time:now_in_msecs() + aec_governance:accepted_future_block_time_shift() + 100),
              ?assertEqual({error, block_from_the_future}, ?TEST_MODULE:validate_micro_block_header(Header))
      end]}.
