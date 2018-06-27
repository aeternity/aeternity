%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_base58c_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aec_base58c).
-define(TYPES, [ {block_hash        , 32}
               , {block_tx_hash     , 32}
               , {block_state_hash  , 32}
               , {channel           , 32}
               , {contract_pubkey   , 32}
               , {transaction       , not_applicable}
               , {tx_hash           , 32}
               , {oracle_pubkey     , 32}
               , {oracle_query_id   , 32}
               , {account_pubkey    , 32}
               , {signature         , 64}
               , {name              , not_applicable}
               , {commitment        , 32}
               , {peer_pubkey       , 32}
               , {state             , 32}
               , {poi               , not_applicable}]).

encode_decode_test_() ->
    [{"Byte sizes are correct",
      fun() ->
          lists:foreach(
              fun({Type, ByteSize}) ->
                  {_Type, _, ByteSize} = {Type, ByteSize,
                                          ?TEST_MODULE:byte_size_for_type(Type)}
              end,
              ?TYPES)
      end
     },
     {"Serialize/deserialize known types",
      fun() ->
          lists:foreach(
              fun({Type, Size0}) ->
                    ByteSize =
                        case Size0 of
                            not_applicable -> 42;
                            _ when is_integer(Size0) -> Size0
                        end,
                    Key = <<42:ByteSize/unit:8>>,
                    EncodedKey = ?TEST_MODULE:encode(Type, Key),
                    {Type, Key} = ?TEST_MODULE:decode(EncodedKey),
                    {ok, Key} = ?TEST_MODULE:safe_decode(Type, EncodedKey)
              end,
              ?TYPES)
      end
     },
     {"Key size check works",
      fun() ->
          lists:foreach(
              fun({_Type, not_applicable}) -> ok;
                 ({Type, ByteSize}) ->
                    CheckIlligalSize =
                        fun(S) ->
                            Key = <<42:S/unit:8>>,
                            EncodedKey = ?TEST_MODULE:encode(Type, Key),
                            {error, invalid_encoding} = ?TEST_MODULE:safe_decode(Type, EncodedKey)
                        end,
                    CheckIlligalSize(0),
                    CheckIlligalSize(ByteSize - 1),
                    CheckIlligalSize(ByteSize + 1)
              end,
              ?TYPES)
      end
     },
     {"Missing prefix",
      fun() ->
          lists:foreach(
              fun({Type, Size0}) ->
                    ByteSize =
                        case Size0 of
                            not_applicable -> 42;
                            _ when is_integer(Size0) -> Size0
                        end,
                    Key = <<42:ByteSize/unit:8>>,
                    EncodedKey = ?TEST_MODULE:encode(Type, Key),
                    <<_PartOfPrefix:1/unit:8, RestOfKey/binary>> = EncodedKey,
                    {error, invalid_encoding} = ?TEST_MODULE:safe_decode(Type, RestOfKey),

                    <<_PrefixWithoutDelimiter:2/unit:8, RestOfKey1/binary>> = EncodedKey,
                    {error, invalid_encoding} = ?TEST_MODULE:safe_decode(Type, RestOfKey1),

                    <<_WholePrefix:3/unit:8, RestOfKey2/binary>> = EncodedKey,
                    {error, invalid_encoding} = ?TEST_MODULE:safe_decode(Type, RestOfKey2)
              end,
              ?TYPES)
      end
     },
     {"Piece of encoded key",
      fun() ->
          lists:foreach(
              fun({Type, Size0}) ->
                    ByteSize =
                        case Size0 of
                            not_applicable -> 42;
                            _ when is_integer(Size0) -> Size0
                        end,
                    Key = <<42:ByteSize/unit:8>>,
                    EncodedKey = ?TEST_MODULE:encode(Type, Key),
                    HalfKeySize = byte_size(EncodedKey) div 2,
                    <<HalfKey:HalfKeySize/unit:8, RestOfKey/binary>> = EncodedKey,
                    {error, invalid_encoding} = ?TEST_MODULE:safe_decode(Type, HalfKey),
                    {error, invalid_encoding} = ?TEST_MODULE:safe_decode(Type, RestOfKey)
              end,
              ?TYPES)
      end
     }
     ].
