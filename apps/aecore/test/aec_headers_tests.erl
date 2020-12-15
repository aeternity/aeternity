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

%% If OTP 21 or later, a parse transform (aec_plugin_xform) will modify
%% the code so that what would previously result in a function_clause exception,
%% will instead become a case_clause. While this should not be seen as API-breaking
%% it does break some tests in this EUnit suite.
%%
-ifdef(OTP_RELEASE).
-define(EXCEPTION, {case_clause, _}).
-else.
-define(EXCEPTION, function_clause).
-endif.


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

raw_key_header_fortuna(FortunaHeight) ->
    ?TEST_MODULE:set_version_and_height(raw_key_header(), ?FORTUNA_PROTOCOL_VSN, FortunaHeight).

info_test_() ->
    MinervaHeight = 10,
    FortunaHeight = 15,
    {foreach,
     fun() ->
             meck:new(aec_hard_forks, [passthrough]),
             meck:expect(aec_hard_forks, protocol_effective_at_height,
                         fun(X) when X <  MinervaHeight -> ?ROMA_PROTOCOL_VSN;
                            (X) when X <  FortunaHeight -> ?MINERVA_PROTOCOL_VSN;
                            (X) when X >= FortunaHeight -> ?FORTUNA_PROTOCOL_VSN
                         end),
             ok
     end,
     fun(_) ->
             meck:unload(aec_hard_forks)
     end,
     [{"Serialization/deserialization of set info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, 123),
               SerializedWithInfo = ?TEST_MODULE:serialize_to_binary(WithInfo),
               ?assertEqual(WithInfo,
                            ?TEST_MODULE:deserialize_from_binary(SerializedWithInfo)),
               ok
       end},
      {"Client serialization/deserialization of set info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, 123),
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
               WithInfo = ?TEST_MODULE:set_info(RawKey, default),
               SerializedWithInfo = ?TEST_MODULE:serialize_to_binary(WithInfo),
               ?assertEqual(WithInfo,
                            ?TEST_MODULE:deserialize_from_binary(SerializedWithInfo)),
               ok
       end},
      {"Client serialization/deserialization of unset info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, default),
               SerializedWithInfo = ?TEST_MODULE:serialize_for_client(WithInfo, key),
               Serialized = SerializedWithInfo#{<<"nonce">> => ?TEST_MODULE:nonce(WithInfo),
                                                <<"pow">>   => ?TEST_MODULE:pow(WithInfo)
                                               },
               ?assertEqual({ok, WithInfo},
                            ?TEST_MODULE:deserialize_from_client(key, Serialized)),
               ok
       end},
      {"Serialization of set info in Roma",
       fun() ->
               RawKey = raw_key_header_roma(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, default),
               ?assertMatch(X when is_binary(X), ?TEST_MODULE:serialize_to_binary(WithInfo)),
               ok
       end},
      {"Deserialization of set info in Roma",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               RomaHeight = ?TEST_MODULE:height(raw_key_header_roma(MinervaHeight)),
               WithInfo = ?TEST_MODULE:set_info(RawKey, 123),
               SerMinerva = ?TEST_MODULE:serialize_to_binary(WithInfo),
               CommonVersionBits = 32,
               CommonFlagsBits = 32,
               CommonHeightBits = 64,
               <<?MINERVA_PROTOCOL_VSN:CommonVersionBits, Flags:CommonFlagsBits, MinervaHeight:CommonHeightBits, Rest/binary>> = SerMinerva,
               SerRoma = <<?ROMA_PROTOCOL_VSN:CommonVersionBits, Flags:CommonFlagsBits, RomaHeight:CommonHeightBits, Rest/binary>>,
               ?assertException(error, malformed_header,
                                ?TEST_MODULE:deserialize_from_binary(SerRoma)),
               ok
       end},
      {"Deserialization of too big info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, 123),
               SerMinerva = ?TEST_MODULE:serialize_to_binary(WithInfo),
               TestBinary = <<SerMinerva/binary, 0:8>>,
               ?assertException(error, malformed_header,
                               ?TEST_MODULE:deserialize_from_binary(TestBinary)),
               ok
       end},
      {"Deserialization of too small info",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:set_info(RawKey, 123),
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
               WithInfo = ?TEST_MODULE:set_info(RawKey, 123),
               SerMinerva = ?TEST_MODULE:serialize_to_binary(WithInfo),
               Size = byte_size(SerMinerva) - ?OPTIONAL_INFO_BYTES,
               <<TestBinary:Size/binary, _:?OPTIONAL_INFO_BYTES/unit:8>> = SerMinerva,
               ?assertException(error, malformed_header,
                               ?TEST_MODULE:deserialize_from_binary(TestBinary)),
               ok
       end},
      {"Default value of the info field in the pre release of Fortuna: Minerva protocol",
       fun() ->
               RawKey = raw_key_header_minerva(MinervaHeight),
               WithInfo = ?TEST_MODULE:new_key_header(
                             ?TEST_MODULE:height(RawKey),
                             ?TEST_MODULE:prev_hash(RawKey),
                             ?TEST_MODULE:prev_key_hash(RawKey),
                             ?TEST_MODULE:root_hash(RawKey),
                             ?TEST_MODULE:miner(RawKey),
                             ?TEST_MODULE:beneficiary(RawKey),
                             ?TEST_MODULE:target(RawKey),
                             ?TEST_MODULE:pow(RawKey),
                             ?TEST_MODULE:nonce(RawKey),
                             ?TEST_MODULE:time_in_msecs(RawKey),
                             default,
                             ?MINERVA_PROTOCOL_VSN),
               Info = ?KEY_HEADER_INFO_LIMA_POINT_RELEASE,
               ?assertEqual(Info, ?TEST_MODULE:info(WithInfo))
       end},
      {"Default value of the info field in the pre release of Fortuna: Roma protocol",
       fun() ->
               RawKey = raw_key_header_roma(MinervaHeight),
               WithInfo = ?TEST_MODULE:new_key_header(
                             ?TEST_MODULE:height(RawKey),
                             ?TEST_MODULE:prev_hash(RawKey),
                             ?TEST_MODULE:prev_key_hash(RawKey),
                             ?TEST_MODULE:root_hash(RawKey),
                             ?TEST_MODULE:miner(RawKey),
                             ?TEST_MODULE:beneficiary(RawKey),
                             ?TEST_MODULE:target(RawKey),
                             ?TEST_MODULE:pow(RawKey),
                             ?TEST_MODULE:nonce(RawKey),
                             ?TEST_MODULE:time_in_msecs(RawKey),
                             default,
                             ?ROMA_PROTOCOL_VSN),
               Info = undefined,
               ?assertEqual(Info, ?TEST_MODULE:info(WithInfo))
       end},
      {"Default value of the info field in the pre release of Fortuna: Fortuna protocol",
       fun() ->
               RawKey = raw_key_header_fortuna(FortunaHeight),
               WithInfo = ?TEST_MODULE:new_key_header(
                             ?TEST_MODULE:height(RawKey),
                             ?TEST_MODULE:prev_hash(RawKey),
                             ?TEST_MODULE:prev_key_hash(RawKey),
                             ?TEST_MODULE:root_hash(RawKey),
                             ?TEST_MODULE:miner(RawKey),
                             ?TEST_MODULE:beneficiary(RawKey),
                             ?TEST_MODULE:target(RawKey),
                             ?TEST_MODULE:pow(RawKey),
                             ?TEST_MODULE:nonce(RawKey),
                             ?TEST_MODULE:time_in_msecs(RawKey),
                             default,
                             ?FORTUNA_PROTOCOL_VSN),
               Info = ?KEY_HEADER_INFO_LIMA_POINT_RELEASE,
               ?assertEqual(Info, ?TEST_MODULE:info(WithInfo))
       end}
     ]}.


validate_test_() ->
    {foreach,
     fun() ->
             meck:new(aec_hard_forks, [passthrough]),
             meck:new(aec_mining, [passthrough]),
             meck:new(aec_chain, [passthrough]),
             meck:expect(aec_chain, dirty_get_header, 1, error),
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
              ?assertEqual({error, protocol_version_mismatch},
                           ?TEST_MODULE:validate_key_block_header(Header, 1))
      end,
      fun() ->
              GV = ?GENESIS_VERSION,

              %% Check for any off-by-one errors around first switch.
              ?assertEqual({error, protocol_version_mismatch},
                           ?TEST_MODULE:validate_key_block_header(
                              ?TEST_MODULE:set_version_and_height(
                                 raw_key_header(),
                                 1+GV,
                                 99 + ?GENESIS_HEIGHT), GV)),
              ?assertEqual({error, protocol_version_mismatch},
                           ?TEST_MODULE:validate_key_block_header(
                              ?TEST_MODULE:set_version_and_height(
                                 raw_key_header(),
                                 GV,
                                 100 + ?GENESIS_HEIGHT), 1+GV)),
              ?assertEqual({error, protocol_version_mismatch},
                           ?TEST_MODULE:validate_key_block_header(
                              ?TEST_MODULE:set_version_and_height(
                                 raw_key_header(),
                                 3+GV,
                                 101 + ?GENESIS_HEIGHT), 1+GV)),
              %% Check for any off-by-one errors around second switch.
              ?assertEqual({error, protocol_version_mismatch},
                           ?TEST_MODULE:validate_key_block_header(
                              ?TEST_MODULE:set_version_and_height(
                                 raw_key_header(),
                                 3+GV,
                                 149 + ?GENESIS_HEIGHT), 1+GV)),
              ?assertEqual({error, protocol_version_mismatch},
                           ?TEST_MODULE:validate_key_block_header(
                              ?TEST_MODULE:set_version_and_height(
                                 raw_key_header(),
                                 1+GV,
                                 150 + ?GENESIS_HEIGHT), 3+GV)),
              ?assertEqual({error, protocol_version_mismatch},
                           ?TEST_MODULE:validate_key_block_header(
                              ?TEST_MODULE:set_version_and_height(
                                 raw_key_header(),
                                 1+GV,
                                 151 + ?GENESIS_HEIGHT), 3+GV)),
              ok
      end,
      fun() ->
              meck:expect(aec_mining, verify, 4, false),
              Header = ?TEST_MODULE:set_version_and_height(
                          raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              ?assertEqual({error, incorrect_pow},
                           ?TEST_MODULE:validate_key_block_header(Header, ?GENESIS_VERSION))
      end,
      fun() ->
              meck:expect(aec_mining, verify, 4, true),
              NowTime = 7592837461,
              meck:expect(aeu_time, now_in_msecs, 0, NowTime),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_time_in_msecs(Header0, 2 * NowTime),
              ?assertEqual({error, block_from_the_future},
                           ?TEST_MODULE:validate_key_block_header(Header, ?GENESIS_VERSION))
      end,
      fun() ->
              meck:expect(aec_mining, verify, 4, true),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_time_in_msecs(Header0, ?GENESIS_TIME + 1),
              ?assertEqual(ok, ?TEST_MODULE:validate_key_block_header(Header, ?GENESIS_VERSION))
      end,
      fun() ->
              meck:expect(aec_mining, verify, 4, false),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_time_in_msecs(Header0, ?GENESIS_TIME + 1),
              ?assertEqual({error, incorrect_pow}, ?TEST_MODULE:validate_key_block_header(Header, ?GENESIS_VERSION))
      end,
      fun() ->
              meck:expect(aec_mining, verify, 4, true),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_time_in_msecs(Header0,
                                                      aeu_time:now_in_msecs() + aec_governance:accepted_future_block_time_shift() + 100),
              ?assertEqual({error, block_from_the_future}, ?TEST_MODULE:validate_key_block_header(Header, ?GENESIS_VERSION))
      end,
      fun() ->
              meck:expect(aec_mining, verify, 4, true),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_nonce(Header0, -1),
              ?assertError(function_clause, ?TEST_MODULE:validate_key_block_header(Header, ?GENESIS_VERSION))
      end,
      fun() ->
              meck:expect(aec_mining, verify, 4, true),
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_key_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_nonce(Header0, 16#1ffffffffffffffff),
              ?assertError(function_clause, ?TEST_MODULE:validate_key_block_header(Header, ?GENESIS_VERSION))
      end,
      fun() ->
              Header = ?TEST_MODULE:set_version_and_height(
                           raw_micro_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              ?assertEqual(ok, ?TEST_MODULE:validate_micro_block_header(Header, ?GENESIS_VERSION))
      end,
      fun() ->
              Header0 = ?TEST_MODULE:set_version_and_height(
                           raw_micro_header(), ?GENESIS_VERSION, ?GENESIS_HEIGHT),
              Header = ?TEST_MODULE:set_time_in_msecs(Header0,
                                                      aeu_time:now_in_msecs() + aec_governance:accepted_future_block_time_shift() + 100),
              ?assertEqual({error, block_from_the_future}, ?TEST_MODULE:validate_micro_block_header(Header, ?GENESIS_VERSION))
      end]}.
