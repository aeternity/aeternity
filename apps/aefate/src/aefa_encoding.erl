%% Second draft of FATE serialization encoding/decoding.
%% Goal is to experiment with the encoding, to make sure we can
%% serialize and deserialize all FATE values.
%% TODO: This code is not production ready yet.
%% TODO: The FATE side of data is not set at all yet and will
%% probably change. FATE data is now defined in aefa_data.erl

%% The FATE serialization has to fullfill the following properties:
%% * There has to be 1 and only 1 byte sequence
%%     representing each unique value in FATE.
%% * A valid byte sequence has to be deserializable to a FATE value.
%% * A valid byte sequence must not contain any trailing bytes.
%% * A serialization is a sequence of 8-bit bytes.

%% The serialization function should fullfill the following:
%% * A valid FATE value should be serialized to a byte sequence.
%% * Any other argument, not representing a valid FATE value should
%%     throw an exception

%% The deserialization function should fullfill the following:
%% * A valid byte sequence should be deserialized to a valid FATE value.
%% * Any other argument, not representing a valid byte sequence should
%%     throw an exception

-module(aefa_encoding).

-export([ deserialize/1
        , serialize/1
        ]).

-include("aefa_data.hrl").

%% Definition of tag scheme.
%% This has to follow the protocol specification.

-define(SMALL_INT    ,        2#0). %% sxxxxxx0 6 bit integer with sign bit
%%                                            1 Set below
-define(LONG_STRING  , 2#00000001). %% 0000000  RLP encoded array, size >= 64
-define(SHORT_STRING ,       2#01). %% xxxxxx0  [bytes], 0 < size < 64
%%                                           11  Set below
-define(SHORT_LIST   ,     2#0011). %% xxxx00   [encoded elements],  0 < length < 16
%%                                     000001   FREE
%%                                     xxxx01   FREE
-define(LONG_TUPLE   , 2#00001011). %% 000010   RLP encoded (size - 16) + [encoded elements],
-define(SHORT_TUPLE  ,     2#1011). %% xxxx10   [encoded elements], 0  <  size < 16
%%                                         1111 Set below
-define(LONG_LIST    , 2#00011111). %% 0001     RLP encoded (length - 16) + [Elements]
-define(MAP          , 2#00101111). %% 0010     RLP encoded size + [encoded key, encoded value]
-define(EMPTY_TUPLE  , 2#00111111). %% 0011
%%                                  %% 0100     FREE
-define(EMPTY_STRING , 2#01011111). %% 0101
-define(POS_BIG_INT  , 2#01101111). %% 0110     RLP encoded (integer - 64)
-define(FALSE        , 2#01111111). %% 0111
%%                                  %% 1000     FREE
-define(ADDRESS      , 2#10011111). %% 1001     [32 bytes]
-define(VARIANT      , 2#10101111). %% 1010     encoded tag + encoded values
-define(NIL          , 2#10111111). %% 1011     Empty list
%%                                  %% 1100     FREE
-define(EMPTY_MAP    , 2#11011111). %% 1101
-define(NEG_BIG_INT  , 2#11101111). %% 1110     RLP encoded (integer - 64)
-define(TRUE         , 2#11111111). %% 1111

-define(SHORT_TUPLE_SIZE, 16).
-define(SHORT_LIST_SIZE , 16).
-define(SMALL_INT_SIZE  , 64).
-define(SHORT_STRING_SIZE, 64).

-define(POS_SIGN, 0).
-define(NEG_SIGN, 1).


%% --------------------------------------------------
%% Serialize
%% Serialized a Fate data value into a sequence of bytes
%% according to the Fate serialization specification.
%% TODO: The type Fate Data is not final yet.
-spec serialize(aefa_data:fate_type()) -> binary().
serialize(?FATE_TRUE)        -> <<?TRUE>>;
serialize(?FATE_FALSE)       -> <<?FALSE>>;
serialize(?FATE_NIL)         -> <<?NIL>>;     %% ! Untyped
serialize(?FATE_UNIT)        -> <<?EMPTY_TUPLE>>;  %% ! Untyped
serialize(M) when ?IS_FATE_MAP(M), ?FATE_MAP_SIZE(M) =:= 0 -> <<?EMPTY_MAP>>;  %% ! Untyped
serialize(?FATE_EMPTY_STRING) -> <<?EMPTY_STRING>>;
serialize(I) when ?IS_FATE_INTEGER(I) -> serialize_integer(I);
serialize(String) when ?IS_FATE_STRING(String),
                       ?FATE_STRING_SIZE(String) > 0,
                       ?FATE_STRING_SIZE(String) < ?SHORT_STRING_SIZE ->
    Size = ?FATE_STRING_SIZE(String),
    Bytes = ?FATE_STRING_VALUE(String),
    <<Size:6, ?SHORT_STRING:2, Bytes/binary>>;
serialize(String) when ?IS_FATE_STRING(String),
                       ?FATE_STRING_SIZE(String) > 0,
                       ?FATE_STRING_SIZE(String) >= ?SHORT_STRING_SIZE ->
    Bytes = ?FATE_STRING_VALUE(String),
    <<?LONG_STRING, (aeu_rlp:encode(Bytes))/binary>>;
serialize(?FATE_ADDRESS(Address)) when is_binary(Address), size(Address) =:= 32 ->
    <<?ADDRESS, Address/binary>>;
serialize(?FATE_TUPLE(T)) when size(T) > 0 ->
    S = size(T),
    L = tuple_to_list(T),
    Rest = << <<(serialize(E))/binary>> || E <- L >>,
    if S < ?SHORT_TUPLE_SIZE ->
            <<S:4, ?SHORT_TUPLE:4, Rest/binary>>;
       true ->
            Size = rlp_integer(S - ?SHORT_TUPLE_SIZE),
            <<?LONG_TUPLE:8, Size/binary, Rest/binary>>
    end;
serialize(L) when ?IS_FATE_LIST(L) ->
    [_E|_] = List = ?FATE_LIST_VALUE(L),
    S = length(List),
    Rest = << <<(serialize(El))/binary>> || El <- List >>,
    if S < ?SHORT_LIST_SIZE ->
            <<S:4, ?SHORT_LIST:4, Rest/binary>>;
       true ->
            Val = rlp_integer(S - ?SHORT_LIST_SIZE),
            <<?LONG_LIST, Val/binary, Rest/binary>>
    end;
serialize(Map) when ?IS_FATE_MAP(Map) ->
    L = [{_K,_V}|_] = maps:to_list(?FATE_MAP_VALUE(Map)),
    Size = length(L),
    %% TODO:  check all K same type, and all V same type
    %%        check K =/= map
    Elements = << <<(serialize(K1))/binary, (serialize(V1))/binary>> || {K1,V1} <- L >>,
    <<?MAP,
      (rlp_integer(Size))/binary,
      (Elements)/binary>>;
serialize(?FATE_VARIANT(Tag, Values)) when 0 =< Tag
                                           , Tag < 256 ->
    <<?VARIANT,
      (serialize(?MAKE_FATE_INTEGER(Tag)))/binary,
      (serialize(?FATE_TUPLE(Values)))/binary
    >>.


%% -----------------------------------------------------


rlp_integer(S) when S >= 0 ->
    aeu_rlp:encode(binary:encode_unsigned(S)).

serialize_integer(I) when ?IS_FATE_INTEGER(I) ->
    V = ?FATE_INTEGER_VALUE(I),
    Abs = abs(V),
    Sign = case V < 0 of
               true  -> ?NEG_SIGN;
               false -> ?POS_SIGN
           end,
    if Abs < ?SMALL_INT_SIZE -> <<Sign:1, Abs:6, ?SMALL_INT:1>>;
       Sign =:= ?NEG_SIGN -> <<?NEG_BIG_INT,
                               (rlp_integer(Abs - ?SMALL_INT_SIZE))/binary>>;
       Sign =:= ?POS_SIGN -> <<?POS_BIG_INT,
                               (rlp_integer(Abs - ?SMALL_INT_SIZE))/binary>>
    end.

-spec deserialize(binary()) -> aefa_data:fate_type().
deserialize(B) ->
    {T, <<>>} = deserialize2(B),
    T.

deserialize2(<<?POS_SIGN:1, I:6, ?SMALL_INT:1, Rest/binary>>) ->
    {?MAKE_FATE_INTEGER(I), Rest};
deserialize2(<<?NEG_SIGN:1, I:6, ?SMALL_INT:1, Rest/binary>>) ->
    {?MAKE_FATE_INTEGER(-I), Rest};
deserialize2(<<?NEG_BIG_INT, Rest/binary>>) ->
    {Bint, Rest2} = aeu_rlp:decode_one(Rest),
    {?MAKE_FATE_INTEGER(-binary:decode_unsigned(Bint) - ?SMALL_INT_SIZE),
     Rest2};
deserialize2(<<?POS_BIG_INT, Rest/binary>>) ->
    {Bint, Rest2} = aeu_rlp:decode_one(Rest),
    {?MAKE_FATE_INTEGER(binary:decode_unsigned(Bint) + ?SMALL_INT_SIZE),
     Rest2};
deserialize2(<<?LONG_STRING, Rest/binary>>) ->
    {String, Rest2} = aeu_rlp:decode_one(Rest),
    {?MAKE_FATE_STRING(String), Rest2};
deserialize2(<<S:6, ?SHORT_STRING:2, Rest/binary>>) ->
    String = binary:part(Rest, 0, S),
    Rest2 = binary:part(Rest, byte_size(Rest), - (byte_size(Rest) - S)),
    {?MAKE_FATE_STRING(String), Rest2};
deserialize2(<<?ADDRESS, Rest/binary>>) ->
    A = binary:part(Rest, 0, 32),
    Rest2 = binary:part(Rest, byte_size(Rest), - (byte_size(Rest) - 32)),
    {?FATE_ADDRESS(A), Rest2};
deserialize2(<<?TRUE, Rest/binary>>) ->
    {?FATE_TRUE, Rest};
deserialize2(<<?FALSE, Rest/binary>>) ->
    {?FATE_FALSE, Rest};
deserialize2(<<?NIL, Rest/binary>>) ->
    {?FATE_NIL, Rest};
deserialize2(<<?EMPTY_TUPLE, Rest/binary>>) ->
    {?FATE_UNIT, Rest};
deserialize2(<<?EMPTY_MAP, Rest/binary>>) ->
    {?MAKE_FATE_MAP(#{}), Rest};
deserialize2(<<?EMPTY_STRING, Rest/binary>>) ->
    {?FATE_EMPTY_STRING, Rest};
deserialize2(<<?LONG_TUPLE, Rest/binary>>) ->
    {BSize, Rest1} = aeu_rlp:decode_one(Rest),
    N = binary:decode_unsigned(BSize) + ?SHORT_TUPLE_SIZE,
    {List, Rest2} = deserialize_elements(N, Rest1),
    {?FATE_TUPLE(list_to_tuple(List)), Rest2};
deserialize2(<<S:4, ?SHORT_TUPLE:4, Rest/binary>>) ->
    {List, Rest1} = deserialize_elements(S, Rest),
    {?FATE_TUPLE(list_to_tuple(List)), Rest1};
deserialize2(<<?LONG_LIST, Rest/binary>>) ->
    {BLength, Rest1} = aeu_rlp:decode_one(Rest),
    Length = binary:decode_unsigned(BLength) + ?SHORT_LIST_SIZE,
    {List, Rest2} = deserialize_elements(Length, Rest1),
    {?MAKE_FATE_LIST(List), Rest2};
deserialize2(<<S:4, ?SHORT_LIST:4, Rest/binary>>) ->
    {List, Rest1} = deserialize_elements(S, Rest),
    {?MAKE_FATE_LIST(List), Rest1};
deserialize2(<<?MAP, Rest/binary>>) ->
    {BSize, Rest1} = aeu_rlp:decode_one(Rest),
    Size = binary:decode_unsigned(BSize),
    {List, Rest2} = deserialize_elements(2*Size, Rest1),
    Map = insert_kv(List, #{}),
    {?MAKE_FATE_MAP(Map), Rest2};
deserialize2(<<?VARIANT, Rest/binary>>) ->
    {?MAKE_FATE_INTEGER(Tag), Rest1} = deserialize2(Rest),
    if Tag > 255 -> exit({too_large_tag_in_variant, Tag});
       true ->
            {?FATE_TUPLE(T), Rest2} = deserialize2(Rest1),
            {?FATE_VARIANT(Tag, T), Rest2}
    end.

insert_kv([], M) -> M;
insert_kv([K,V|R], M) -> insert_kv(R, maps:put(K, V, M)).

deserialize_elements(0, Rest) ->
    {[], Rest};
deserialize_elements(N, Es) ->
    {E, Rest} = deserialize2(Es),
    {Tail, Rest2} = deserialize_elements(N-1, Rest),
    {[E|Tail], Rest2}.
