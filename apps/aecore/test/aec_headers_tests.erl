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

reserved_key_flag_bits_rejected_test() ->
    Header = raw_key_header(),
    <<Version:32, Tag:1, Secondary:1, _Reserved:30, Rest/bits>> =
        ?TEST_MODULE:serialize_to_binary(Header),
    %% Set an unused/reserved flag bit - must be rejected.
    Tampered = <<Version:32, Tag:1, Secondary:1, 1:30, Rest/bits>>,
    ?assertException(error, malformed_header,
                      ?TEST_MODULE:deserialize_from_binary(Tampered)).

reserved_micro_flag_bits_rejected_test() ->
    Header = raw_micro_header(),
    <<Version:32, Tag:1, Secondary:1, _Reserved:30, Rest/bits>> =
        ?TEST_MODULE:serialize_to_binary(Header),
    Tampered = <<Version:32, Tag:1, Secondary:1, 1:30, Rest/bits>>,
    ?assertException(error, malformed_header,
                      ?TEST_MODULE:deserialize_from_binary(Tampered)).

%% The HOLE/EOE bits are Hyperchains constructs. Under PoS they must round-trip;
%% under PoW they are reserved bits that 7.2.2 rejects, so accepting them would
%% be a chain split. Both directions are pinned here.
hc_flag_gating_test_() ->
    {foreach,
     fun() ->
             meck:new(aec_consensus, [passthrough]),
             ok
     end,
     fun(_) ->
             meck:unload(aec_consensus)
     end,
     [ {"EOE round-trip under PoS",        fun eoe_flag_roundtrip_pos/0}
     , {"HOLE accepted under PoS",         fun hole_flag_accepted_pos/0}
     , {"HOLE rejected under PoW",         fun hole_flag_rejected_pow/0}
     , {"EOE rejected under PoW",          fun eoe_flag_rejected_pow/0}
     , {"client HOLE/EOE accepted on PoS", fun client_hc_flags_accepted_pos/0}
     , {"client HOLE rejected on PoW",     fun client_hole_flag_rejected_pow/0}
     ]}.

set_consensus_type(Type) ->
    meck:expect(aec_consensus, get_consensus_type, 0, Type).

%% Only Hyperchains block production sets HOLE (via aec_block_hole_candidate),
%% so there is no set_hole/2 to build one with - tamper the bit instead.
key_header_binary_with_flag(Flag) ->
    <<Version:32, Flags:?FLAG_BITS, Rest/bits>> =
        ?TEST_MODULE:serialize_to_binary(raw_key_header()),
    <<Version:32, (Flags bor Flag):?FLAG_BITS, Rest/bits>>.

client_key_header_with_flags(Flags) ->
    RawKey = raw_key_header(),
    Serialized = ?TEST_MODULE:serialize_for_client(RawKey, key),
    Serialized#{<<"nonce">> => ?TEST_MODULE:nonce(RawKey),
                <<"pow">>   => ?TEST_MODULE:pow(RawKey),
                <<"flags">> => aeser_api_encoder:encode(bytearray, <<Flags:?FLAG_BITS>>)
               }.

eoe_flag_roundtrip_pos() ->
    set_consensus_type(pos),
    Header = ?TEST_MODULE:set_eoe(raw_key_header(), true),
    SerializedHeader = ?TEST_MODULE:serialize_to_binary(Header),
    DeserializedHeader = ?TEST_MODULE:deserialize_from_binary(SerializedHeader),
    ?assertEqual(Header, DeserializedHeader),
    ?assert(?TEST_MODULE:is_eoe(DeserializedHeader)),
    ?assertNot(?TEST_MODULE:is_hole(DeserializedHeader)).

hole_flag_accepted_pos() ->
    set_consensus_type(pos),
    Header = ?TEST_MODULE:deserialize_from_binary(
                key_header_binary_with_flag(?HOLE_FLAG)),
    ?assert(?TEST_MODULE:is_hole(Header)),
    ?assertNot(?TEST_MODULE:is_eoe(Header)).

hole_flag_rejected_pow() ->
    set_consensus_type(pow),
    ?assertException(error, malformed_header,
                     ?TEST_MODULE:deserialize_from_binary(
                       key_header_binary_with_flag(?HOLE_FLAG))).

eoe_flag_rejected_pow() ->
    set_consensus_type(pow),
    ?assertException(error, malformed_header,
                     ?TEST_MODULE:deserialize_from_binary(
                       key_header_binary_with_flag(?EOE_FLAG))).

%% Also the control for client_hole_flag_rejected_pow/0: the same map must
%% deserialize cleanly under pos, so the pow failure is attributable to the
%% consensus gate rather than to a malformed fixture - deserialize_from_client
%% catches every exception and reports the same invalid_header either way.
client_hc_flags_accepted_pos() ->
    set_consensus_type(pos),
    WithHcFlags = client_key_header_with_flags(
                    ?KEY_HEADER_FLAG bor ?HOLE_FLAG bor ?EOE_FLAG),
    {ok, Header} = ?TEST_MODULE:deserialize_from_client(key, WithHcFlags),
    ?assert(?TEST_MODULE:is_hole(Header)),
    ?assert(?TEST_MODULE:is_eoe(Header)).

client_hole_flag_rejected_pow() ->
    set_consensus_type(pow),
    WithHoleFlag = client_key_header_with_flags(?KEY_HEADER_FLAG bor ?HOLE_FLAG),
    ?assertEqual({error, invalid_header},
                 ?TEST_MODULE:deserialize_from_client(key, WithHoleFlag)).

client_reserved_flag_bits_rejected_test() ->
    RawKey = raw_key_header(),
    Serialized = ?TEST_MODULE:serialize_for_client(RawKey, key),
    WithBadFlags = Serialized#{<<"nonce">> => ?TEST_MODULE:nonce(RawKey),
                               <<"pow">>   => ?TEST_MODULE:pow(RawKey),
                               <<"flags">> => aeser_api_encoder:encode(bytearray, <<1:32>>)
                              },
    ?assertEqual({error, invalid_header},
                 ?TEST_MODULE:deserialize_from_client(key, WithBadFlags)).

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
               Info = aeu_info:block_info(),
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
               Info = aeu_info:block_info(),
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
