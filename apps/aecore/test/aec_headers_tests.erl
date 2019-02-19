-module(aec_headers_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-include("blocks.hrl").

-import(aec_headers, [raw_key_header/0,
                      raw_micro_header/0
                     ]).

-define(TEST_MODULE, aec_headers).
-define(GENESIS_HEIGHT, aec_block_genesis:height()).
-define(GENESIS_VERSION, aec_block_genesis:version()).
-define(GENESIS_TIME, aec_block_genesis:time_in_msecs()).

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

raw_key_header_minerva(MinervaHeight) ->
    ?TEST_MODULE:set_version_and_height(raw_key_header(), ?MINERVA_PROTOCOL_VSN, MinervaHeight).

raw_key_header_roma(MinervaHeight) ->
    ?TEST_MODULE:set_version_and_height(raw_key_header(), ?ROMA_PROTOCOL_VSN, MinervaHeight - 1).

info_test_() ->
    MinervaHeight = 10,
    {foreach,
     fun() ->
             meck:new(aec_hard_forks, [passthrough]),
             meck:expect(aec_hard_forks, protocol_effective_at_height,
                         fun(X) when X < MinervaHeight -> ?ROMA_PROTOCOL_VSN;
                            (X) when X >= MinervaHeight -> ?MINERVA_PROTOCOL_VSN
                         end),
             ok
     end,
     fun(_) ->
             meck:unload(aec_hard_forks)
     end,
     [{"Serialization/deserialization of set info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, <<123:?OPTIONAL_INFO_BYTES/unit:8>>),
               SerializedWithInfo = ?TEST_MODULE:serialize_to_binary(WithInfo),
               ?assertEqual(WithInfo,
                            ?TEST_MODULE:deserialize_from_binary(SerializedWithInfo)),
               ok
       end},
      {"Client serialization/deserialization of set info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, <<123:?OPTIONAL_INFO_BYTES/unit:8>>),
               SerializedWithInfo = ?TEST_MODULE:serialize_for_client(WithInfo, key),
               Serialized = SerializedWithInfo#{<<"nonce">> => ?TEST_MODULE:nonce(WithInfo),
                                                <<"pow">>   => ?TEST_MODULE:pow(WithInfo)
                                               },
               ?assertEqual({ok, WithInfo},
                            ?TEST_MODULE:deserialize_from_client(key, Serialized)),
               ok
       end},
      {"Serialization/deserialization of unset info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, <<>>),
               SerializedWithInfo = ?TEST_MODULE:serialize_to_binary(WithInfo),
               ?assertEqual(WithInfo,
                            ?TEST_MODULE:deserialize_from_binary(SerializedWithInfo)),
               ok
       end},
      {"Client serialization/deserialization of unset info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, <<>>),
               SerializedWithInfo = ?TEST_MODULE:serialize_for_client(WithInfo, key),
               Serialized = SerializedWithInfo#{<<"nonce">> => ?TEST_MODULE:nonce(WithInfo),
                                                <<"pow">>   => ?TEST_MODULE:pow(WithInfo)
                                               },
               ?assertEqual({ok, WithInfo},
                            ?TEST_MODULE:deserialize_from_client(key, Serialized)),
               ok
       end},
      {"Serialization of too small info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               Size = ?OPTIONAL_INFO_BYTES - 1,
               WithInfo = ?TEST_MODULE:set_info(RawKey, <<123:Size/unit:8>>),
               ?assertException(error, illegal_info_field, ?TEST_MODULE:serialize_to_binary(WithInfo)),
               ok
       end},
      {"Serialization of too big info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               Size = ?OPTIONAL_INFO_BYTES + 1,
               WithInfo = ?TEST_MODULE:set_info(RawKey, <<123:Size/unit:8>>),
               ?assertException(error, illegal_info_field, ?TEST_MODULE:serialize_to_binary(WithInfo)),
               ok
       end},
      {"Serialization of set info in Roma",
       fun() ->
               RawKey = raw_key_header_roma(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, <<123:?OPTIONAL_INFO_BYTES/unit:8>>),
               ?assertException(error, illegal_info_field, ?TEST_MODULE:serialize_to_binary(WithInfo)),
               ok
       end},
      {"Deserialization of set info in Roma",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               RomaHeight = ?TEST_MODULE:height(raw_key_header_roma(MinervaHeight)),
               WithInfo = ?TEST_MODULE:set_info(RawKey, <<123:?OPTIONAL_INFO_BYTES/unit:8>>),
               SerMinerva = ?TEST_MODULE:serialize_to_binary(WithInfo),
               <<_Version:32, Flags:32, Height:64, Rest/binary>> = SerMinerva,
               SerRoma = <<?ROMA_PROTOCOL_VSN:64, Flags:32, RomaHeight:32, Rest/binary>>,
               ?assertException(error, malformed_header,
                                ?TEST_MODULE:deserialize_from_binary(SerRoma)),
               ok
       end},
      {"Deserialization of too big info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, <<123:?OPTIONAL_INFO_BYTES/unit:8>>),
               SerMinerva = ?TEST_MODULE:serialize_to_binary(WithInfo),
               TestBinary = <<SerMinerva/binary, 0:8>>,
               ?assertException(error, malformed_header,
                               ?TEST_MODULE:deserialize_from_binary(TestBinary)),
               ok
       end},
      {"Deserialization of too small info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, <<123:?OPTIONAL_INFO_BYTES/unit:8>>),
               SerMinerva = ?TEST_MODULE:serialize_to_binary(WithInfo),
               Size = byte_size(SerMinerva) - 1,
               <<TestBinary:Size/binary, _:1/unit:8>> = SerMinerva,
               ?assertException(error, malformed_header,
                               ?TEST_MODULE:deserialize_from_binary(TestBinary)),
               ok
       end},
      {"Deserialization of no info with info flag set",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, <<123:?OPTIONAL_INFO_BYTES/unit:8>>),
               SerMinerva = ?TEST_MODULE:serialize_to_binary(WithInfo),
               Size = byte_size(SerMinerva) - ?OPTIONAL_INFO_BYTES,
               <<TestBinary:Size/binary, _:?OPTIONAL_INFO_BYTES/unit:8>> = SerMinerva,
               ?assertException(error, malformed_header,
                               ?TEST_MODULE:deserialize_from_binary(TestBinary)),
               ok
       end}
     ]}.


validate_test_() ->
    {foreach,
     fun() ->
             meck:new(aec_hard_forks, [passthrough]),
             meck:new(aec_mining, [passthrough]),
             meck:new(aec_chain, [passthrough]),
             meck:expect(aec_chain, get_header, 1, error),
             meck:new(aeu_time, [passthrough])
     end,
     fun(_) ->
             meck:unload(aec_mining),
             meck:unload(aeu_time),
             meck:unload(aec_chain),
             meck:unload(aec_hard_forks)
     end,
     [fun() ->
              Header = ?TEST_MODULE:set_version(raw_key_header(), 736),
              ?assertEqual({error, unknown_protocol_version}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              GV = ?GENESIS_VERSION,
              Protocols = #{GV   =>       ?GENESIS_HEIGHT,
                            1+GV => 100 + ?GENESIS_HEIGHT,
                            3+GV => 150 + ?GENESIS_HEIGHT},
              MockFun =
                  fun(V, H) ->
                          aec_hard_forks:check_protocol_version_validity(V, H, Protocols)
                  end,
              meck:expect(aec_hard_forks, check_protocol_version_validity,
                          MockFun),
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
              meck:expect(aec_mining, verify, 4, false),
              Header = ?TEST_MODULE:set_version_and_height(
                          raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              ?assertEqual({error, incorrect_pow}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_mining, verify, 4, true),
              NowTime = 7592837461,
              meck:expect(aeu_time, now_in_msecs, 0, NowTime),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_time_in_msecs(Header0, 2 * NowTime),
              ?assertEqual({error, block_from_the_future}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_mining, verify, 4, true),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_time_in_msecs(Header0, ?GENESIS_TIME + 1),
              ?assertEqual(ok, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_mining, verify, 4, false),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_time_in_msecs(Header0, ?GENESIS_TIME + 1),
              ?assertEqual({error, incorrect_pow}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_mining, verify, 4, true),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_time_in_msecs(Header0,
                                                      aeu_time:now_in_msecs() + aec_governance:accepted_future_block_time_shift() + 100),
              ?assertEqual({error, block_from_the_future}, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_mining, verify, 4, true),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_nonce(Header0, -1),
              ?assertError(function_clause, ?TEST_MODULE:validate_key_block_header(Header))
      end,
      fun() ->
              meck:expect(aec_mining, verify, 4, true),
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
