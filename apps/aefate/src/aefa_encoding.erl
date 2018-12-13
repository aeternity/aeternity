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
        , types_to_typereps/1
        , deserialize_typereps/2]).

%% Definition of tag scheme.
%% This has to follow the protocol specification.

-define(SMALL_INT    ,        2#0). %% sxxxxxx0 6 bit integer with sign bit
%%                                            1 Set below
-define(LONG_STRING  , 2#00000001). %% 0000000  RLP encoded array, size >= 64
-define(SHORT_STRING ,       2#01). %% xxxxxx0  [bytes], 0 < size < 64
%%                                           11  Set below
-define(SHORT_LIST   ,     2#0011). %% xxxx00   typerep + [encoded elements],  0 < length < 16
%%                                     000001   FREE
%%                                     xxxx01   FREE
-define(LONG_TUPLE   , 2#00001011). %% 000010   RLP encoded (size - 16) + [encoded elements],
-define(SHORT_TUPLE  ,     2#1011). %% xxxx10   [encoded elements], 0  <  size < 16
%%                                         1111 Set below
-define(LONG_LIST    , 2#00011111). %% 0001     typerep + RLP encoded (length - 16) + [Elements]
-define(MAP          , 2#00101111). %% 0010     RLP encoded size + typerep(A) + typerep(B) + [encoded key, encoded value]
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
serialize(true)        -> <<?TRUE>>;
serialize(false)       -> <<?FALSE>>;
serialize([])          -> <<?NIL>>;     %% ! Untyped
serialize({tuple, {}}) -> <<?EMPTY_TUPLE>>;  %% ! Untyped
serialize(M) when is_map(M), map_size(M) =:= 0 -> <<?EMPTY_MAP>>;  %% ! Untyped
serialize(<<>>)        -> <<?EMPTY_STRING>>;
serialize(I) when is_integer(I) ->
    serialize_integer(I);
serialize(String) when byte_size(String) > 0, byte_size(String) < ?SHORT_STRING_SIZE ->
    S = size(String),
    <<S:6, ?SHORT_STRING:2, String/binary>>;
serialize(String) when is_binary(String), size(String) > 0, size(String) >= ?SHORT_STRING_SIZE ->
    <<?LONG_STRING, (aeu_rlp:encode(String))/binary>>;
serialize({address, Address}) when is_binary(Address), size(Address) =:= 32 ->
    <<?ADDRESS, Address/binary>>;
serialize({tuple, T}) when size(T) > 0 ->
    S = size(T),
    L = tuple_to_list(T),
    Rest = << <<(serialize(E))/binary>> || E <- L >>,
    if S < ?SHORT_TUPLE_SIZE ->
            <<S:4, ?SHORT_TUPLE:4, Rest/binary>>;
       true ->
            Size = rlp_integer(S - ?SHORT_TUPLE_SIZE),
            <<?LONG_TUPLE:8, Size/binary, Rest/binary>>
    end;
serialize([E|_] = L) ->
    S = length(L),
    T = value_to_typerep(E),
    %% TODO assert all E of T
    Rest = << <<(serialize(El))/binary>> || El <- L >>,
    if S < ?SHORT_LIST_SIZE ->
            <<S:4, ?SHORT_LIST:4, T/binary, Rest/binary>>;
       true ->
            Val = rlp_integer(S - ?SHORT_LIST_SIZE),
            <<?LONG_LIST, T/binary, Val/binary, Rest/binary>>
    end;
serialize(Map) when is_map(Map) ->
    L = [{K,V}|_] = maps:to_list(Map),
    Size = length(L),
    %% TODO:  check all K same type, and all V same type
    %%        check K =/= map
    Elements = << <<(serialize(K1))/binary, (serialize(V1))/binary>> || {K1,V1} <- L >>,
    <<?MAP,
      (value_to_typerep(K))/binary,
      (value_to_typerep(V))/binary,
      (rlp_integer(Size))/binary,
      (Elements)/binary>>;
serialize({variant, Tag, Values}) when is_integer(Tag),
                                       is_tuple(Values) ->
    <<?VARIANT,
      (serialize(Tag))/binary,
      (serialize({tuple, Values}))/binary
    >>.


%% -----------------------------------------------------


value_to_typerep({}) -> <<0>>;
value_to_typerep(I) when is_integer(I) -> <<1>>;
value_to_typerep(true) -> <<2>>;
value_to_typerep(false) -> <<2>>;
value_to_typerep({address, _}) -> <<3>>;
value_to_typerep(S) when is_binary(S) -> <<4>>;
value_to_typerep({tuple, {}}) -> <<5, 0>>;
value_to_typerep({tuple, T}) ->
    S = size(T),
    Es = << <<(value_to_typerep(E))/binary>> || E <- tuple_to_list(T) >>,
    <<5, S, Es/binary>>;
value_to_typerep([]) -> <<6>>;
value_to_typerep([E|_]) -> <<7, (value_to_typerep(E))/binary >>;
value_to_typerep(M) when is_map(M) ->
    case maps:to_list(M) of
        [{A,B}|_] ->
            <<8,
              (value_to_typerep(A))/binary,
              (value_to_typerep(B))/binary>>;
        [] -> <<10>>
    end;
value_to_typerep({variant, Cases}) ->
    %% TODO: handle or forbid more than 255 cases and type parameters
    <<9, (length(Cases)),
      <<
        << (length(Types)),
           << << (value_to_typerep(Type))/binary >> || Type <- Types >>
        >>
        || Types <- Cases >>
    >>.


type_to_typerep(unit)        -> <<0>>;
type_to_typerep(integer)     -> <<1>>;
type_to_typerep(boolean)     -> <<2>>;
type_to_typerep(address)     -> <<3>>;
type_to_typerep(string)      -> <<4>>;
type_to_typerep(empty_tuple) -> <<5, 0>>;
type_to_typerep({tuple, T}) ->
    S = size(T),
    Es = << <<(type_to_typerep(E))/binary>> || E <- tuple_to_list(T) >>,
    <<5, S, Es/binary>>;
type_to_typerep(nil)         -> <<6>>;
type_to_typerep({list, T})   -> <<7, (type_to_typerep(T))/binary >>;
type_to_typerep(empty_map)   -> <<10>>;
type_to_typerep({map, A, B}) -> <<8, (type_to_typerep(A))/binary, (type_to_typerep(B))/binary >>;
type_to_typerep({variant, Cases}) ->
    <<9, (length(Cases)),
      <<
        << (length(Types)),
           << << (type_to_typerep(Type))/binary >> || Type <- Types >>
        >>
        || Types <- Cases >>
    >>.


deserialize_typerep(<<0, R/binary>>) -> {unit, R};
deserialize_typerep(<<1, R/binary>>) -> {integer, R};
deserialize_typerep(<<2, R/binary>>) -> {boolean, R};
deserialize_typerep(<<3, R/binary>>) -> {address, R};
deserialize_typerep(<<4, R/binary>>) -> {string, R};
deserialize_typerep(<<5, 0, R/binary>>) -> {empty_tuple, R};
deserialize_typerep(<<5, S, R/binary>>) ->
    {List, Rest} = deserialize_typerep_elements(S, R),
    {{tuple, list_to_tuple(List)}, Rest};
deserialize_typerep(<<6, R/binary>>) -> {nil, R};
deserialize_typerep(<<7, T/binary>>) ->
    {Type, Rest} = deserialize_typerep(T),
    {{list, Type}, Rest};
deserialize_typerep(<<8, R/binary>>) ->
    {A, R1} = deserialize_typerep(R),
    {B, R2} = deserialize_typerep(R1),
    {{map, A, B}, R2};
deserialize_typerep(<<9, C, Rest/binary>>) ->
    {List, Rest} = deserialize_type_parameters(C, Rest),
    {{variant, [List]}, Rest};
deserialize_typerep(<<10, Rest/binary>>) ->
    {empty_map, Rest}.

deserialize_typerep_elements(0, Rest) ->
    {[], Rest};
deserialize_typerep_elements(N, Es) ->
    {E, Rest} = deserialize_typerep(Es),
    {Tail, Rest2} = deserialize_typerep_elements(N-1, Rest),
    {[E|Tail], Rest2}.


deserialize_type_parameters(0, Rest) ->
    {[], Rest};
deserialize_type_parameters(N, <<S,Es/binary>>) ->
    {List, Rest} = deserialize_typerep_elements(S, Es),
    {Tail, Rest2} = deserialize_type_parameters(N-1, Rest),
    {[List|Tail], Rest2}.



types_to_typereps(Types) ->
    << <<(type_to_typerep(Type))/binary >> || Type <- Types >>.


rlp_integer(S) when S >= 0 ->
    aeu_rlp:encode(binary:encode_unsigned(S)).

serialize_integer(I) when is_integer(I) ->
    Abs = abs(I),
    Sign = case I < 0 of
               true  -> ?NEG_SIGN;
               false -> ?POS_SIGN
           end,
    if Abs < ?SMALL_INT_SIZE -> <<Sign:1, Abs:6, ?SMALL_INT:1>>;
       Sign =:= ?NEG_SIGN -> <<?NEG_BIG_INT, (rlp_integer(Abs - ?SMALL_INT_SIZE))/binary>>;
       Sign =:= ?POS_SIGN -> <<?POS_BIG_INT, (rlp_integer(Abs - ?SMALL_INT_SIZE))/binary>>
    end.

-spec deserialize(binary()) -> aefa_data:fate_type().
deserialize(B) ->
    {T, <<>>} = deserialize2(B),
    T.

deserialize2(<<?POS_SIGN:1, I:6, ?SMALL_INT:1, Rest/binary>>) ->
    {I, Rest};
deserialize2(<<?NEG_SIGN:1, I:6, ?SMALL_INT:1, Rest/binary>>) ->
    {-I, Rest};
deserialize2(<<?NEG_BIG_INT, Rest/binary>>) ->
    {Bint, Rest2} = aeu_rlp:decode_one(Rest),
    {-binary:decode_unsigned(Bint) + ?SMALL_INT_SIZE, Rest2};
deserialize2(<<?POS_BIG_INT, Rest/binary>>) ->
    {Bint, Rest2} = aeu_rlp:decode_one(Rest),
    {binary:decode_unsigned(Bint) + ?SMALL_INT_SIZE, Rest2};
deserialize2(<<?LONG_STRING, Rest/binary>>) ->
    aeu_rlp:decode_one(Rest);
deserialize2(<<S:6, ?SHORT_STRING:2, Rest/binary>>) ->
    String = binary:part(Rest, 0, S),
    Rest2 = binary:part(Rest, byte_size(Rest), - (byte_size(Rest) - S)),
    {String, Rest2};
deserialize2(<<?ADDRESS, Rest/binary>>) ->
    A = binary:part(Rest, 0, 32),
    Rest2 = binary:part(Rest, byte_size(Rest), - (byte_size(Rest) - 32)),
    {{address, A}, Rest2};
deserialize2(<<?TRUE, Rest/binary>>) ->
    {true, Rest};
deserialize2(<<?FALSE, Rest/binary>>) ->
    {false, Rest};
deserialize2(<<?NIL, Rest/binary>>) ->
    {[], Rest};
deserialize2(<<?EMPTY_TUPLE, Rest/binary>>) ->
    {{tuple, {}}, Rest};
deserialize2(<<?EMPTY_MAP, Rest/binary>>) ->
    {#{}, Rest};
deserialize2(<<?EMPTY_STRING, Rest/binary>>) ->
    {<<>>, Rest};
deserialize2(<<?LONG_TUPLE, Rest/binary>>) ->
    {BSize, Rest1} = aeu_rlp:decode_one(Rest),
    N = binary:decode_unsigned(BSize) + ?SHORT_TUPLE_SIZE,
    {List, Rest2} = deserialize_elements(N, Rest1),
    {{tuple, list_to_tuple(List)}, Rest2};
deserialize2(<<S:4, ?SHORT_TUPLE:4, Rest/binary>>) ->
    {List, Rest1} = deserialize_elements(S, Rest),
    {{tuple, list_to_tuple(List)}, Rest1};
deserialize2(<<?LONG_LIST, Rest/binary>>) ->
    {_Typerep, Rest2} = deserialize_typerep(Rest),
    {BLength, Rest3} = aeu_rlp:decode_one(Rest2),
    Length = binary:decode_unsigned(BLength) + ?SHORT_LIST_SIZE,
    {List, Rest4} = deserialize_elements(Length, Rest3),
    %% TODO: check typerep == type of List
    {List, Rest4};
deserialize2(<<S:4, ?SHORT_LIST:4, Rest/binary>>) ->
    {_Typerep, Rest2} = deserialize_typerep(Rest),
    {List, Rest3} = deserialize_elements(S, Rest2),
    %% TODO: check typerep == type of List
    {List, Rest3};
deserialize2(<<?MAP, Rest/binary>>) ->
    {_Typerep_A, Rest2} = deserialize_typerep(Rest),
    {_Typerep_B, Rest3} = deserialize_typerep(Rest2),
    {BSize, Rest4} = aeu_rlp:decode_one(Rest3),
    Size = binary:decode_unsigned(BSize),
    {List, Rest5} = deserialize_elements(2*Size, Rest4),
    Map = insert_kv(List, #{}),
    %% TODO: check typereps == type of {K,V}
    {Map, Rest5};
deserialize2(<<?VARIANT, Rest/binary>>) ->
    {Tag, Rest1} = deserialize2(Rest),
    {{tuple, Values}, Rest2} = deserialize2(Rest1),
    {{variant, Tag, Values}, Rest2}.

deserialize_typereps(0, Rest) ->
    {[], Rest};
deserialize_typereps(N, Rest) ->
    {T, Rest2} = deserialize_typerep(Rest),
    {List, Rest3} = deserialize_typereps(N-1, Rest2),
    {[T|List], Rest3}.


insert_kv([], M) -> M;
insert_kv([K,V|R], M) -> insert_kv(R, maps:put(K, V, M)).

deserialize_elements(0, Rest) ->
    {[], Rest};
deserialize_elements(N, Es) ->
    {E, Rest} = deserialize2(Es),
    {Tail, Rest2} = deserialize_elements(N-1, Rest),
    {[E|Tail], Rest2}.
