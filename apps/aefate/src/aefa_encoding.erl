%% First draft of FATE serialization encoding/decoding.
%% Goal is to experiment with the encoding.
%% The FATE side of data is not set at all yet and will
%% probably change.
-module(aefa_encoding).

-export([ deserialize/1
        , serialize/1]).

%% Very ecperimental:
-compile([export_all]).

%% Definition of tag scheme.
%% This has to follow the protocol specification.

%% small integer: sxxxxxx0 6 bit integer with sign bit
%%                       1  Set below
%% short string : xxxxxx01 (at least one x =/= 0) xxxxxx = byte array size + [bytes]
%% string       : 00000001 + RLP encoded array
-define(NSTRING,2#00000001).
%%                      11  Set below
%% tuple        : xxxx1011 + [encoded elements] when 0 < size(tuple) < 16
-define(ITUPLE_SIZE, 16).
%% tuple        : 00001011 + 1 byte size + [encoded elements]
-define(TUPLE,      2#1011).
%% list         : xxxx1011 + typerep + [encoded elements],  0 < length < 16, xxxx = length
-define(ILIST_SIZE, 16).
%% list         : 00000011 + typerep + encoded length + [encoded elements]
-define(LIST,       2#0011).
%%                     111  Set below
%%                xxxx0111 Free (16)
%%                00001111 Free
%%                10001111 Free
%%                01001111 Free
%%                11001111 Free
%% map          : 00101111 + RLP encoded size + typerep(A) + typerep(B) + [encoded key, encoded value]
-define(MAP    ,2#00101111).
%% variant type : 10101111 + RLP encoded size + [RLP encoded size + [typereps]]
-define(VARIANT,2#10101111).
%% integer      : 10101111 + RLP encoded integer
-define(PINT   ,2#01101111).
%% -integer     : 01101111 + RLP encoded integer
-define(NINT   ,2#11101111).
%% long list    : 00011111 + typerep + RLP encoded length + [Elements]
-define(LLIST,  2#00011111).
%% address      : 10011111 + [32 bytes]
-define(ADDRESS,2#10011111).
%% ""           : 01011111
-define(EMPTYS, 2#01011111).
%% #{}          : 11011111
-define(EMPTYM, 2#11011111).
%% {}           : 00111111
-define(TUPLE0, 2#00111111).
%% nil          : 10111111
-define(NIL,    2#10111111).
%% false        : 01111111
-define(FALSE,  2#01111111).
%% true         : 11111111
-define(TRUE,   2#11111111).

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



serialize(true)        -> <<?TRUE>>;
serialize(false)       -> <<?FALSE>>;
serialize([])          -> <<?NIL>>;     %% ! Untyped
serialize({tuple, {}}) -> <<?TUPLE0>>;  %% ! Untyped
serialize(M) when is_map(M), map_size(M) =:= 0 -> <<?EMPTYM>>;  %% ! Untyped
serialize(<<>>)        -> <<?EMPTYS>>;
serialize(I) when is_integer(I) ->
    case I >= 0 of
        true ->
            serialize_pos_int(I);
        false ->
            serialize_neg_int(-I)
    end;
serialize(String) when is_binary(String), size(String) > 0, size(String) < 64 ->
    S = size(String),
    <<S:6, 2#01:2, String/binary>>;
serialize(String) when is_binary(String), size(String) > 0, size(String) >= 64 ->
    <<?NSTRING, (aeu_rlp:encode(String))/binary>>;
serialize({address, Address}) when is_binary(Address), size(Address) =:= 32 ->
    <<?ADDRESS, Address/binary>>;
serialize({tuple, T}) when size(T) > 0 ->
    S = size(T),
    L = tuple_to_list(T),
    Rest = << <<(serialize(E))/binary>> || E <- L >>,
    if S < ?ITUPLE_SIZE ->
            <<S:4, ?TUPLE:4, Rest/binary>>;
       true ->
            <<?TUPLE:8, S, Rest/binary>>
    end;
serialize([E|_] = L) ->
    S = length(L),
    T = value_to_typerep(E),
    %% TODO assert all E of T
    Rest = << <<(serialize(El))/binary>> || El <- L >>,
    if S < ?ILIST_SIZE ->
            <<S:4, ?LIST:4, T/binary, Rest/binary>>;
       S < 256 ->
            <<?LIST:8, T/binary, S, Rest/binary>>;
       true ->
            <<?LLIST, T/binary, (serialize_integer(S))/binary, Rest/binary>>
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
      (serialize_integer(Size))/binary,
      (Elements)/binary>>;
serialize({variant, Cases}) when is_list(Cases) ->
    NCases = serialize_integer(length(Cases)),
    ECases =
        <<
          << (serialize_integer(length(Types)))/binary,
             (types_to_typereps(Types))/binary
          >>
          || Types <- Cases
        >>,

    <<?VARIANT,
      (NCases)/binary,
      ECases/binary
    >>.

types_to_typereps(Types) ->
    << <<(type_to_typerep(Type))/binary >> || Type <- Types >>.





serialize_integer(S) -> aeu_rlp:encode(binary:encode_unsigned(S)).


serialize_pos_int(I) when is_integer(I), I >= 0 ->
    if I < 64 ->
            <<0:1, I:6, 0:1>>;
       true ->
            <<?PINT, (serialize_integer(I))/binary>>
    end.

serialize_neg_int(I) when is_integer(I), I > 0 ->
    if I < 64 ->
            <<1:1, I:6, 0:1>>;
       true ->
            <<?NINT, (serialize_integer(I))/binary>>
    end.

deserialize(B) ->
    {T, <<>>} = deserialize2(B),
    T.

deserialize2(<<S:1, I:6, 0:1, Rest/binary>>) -> %% Integer
    if S =:= 1 -> {-I, Rest};
       true -> {I, Rest}
    end;
deserialize2(<<?NINT, Rest/binary>>) -> %% Large int
    {Bint, Rest2} = aeu_rlp:decode_one(Rest),
    {-binary:decode_unsigned(Bint), Rest2}; %% todo: export decode_one
deserialize2(<<?PINT, Rest/binary>>) ->
    {Bint, Rest2} = aeu_rlp:decode_one(Rest),
    {binary:decode_unsigned(Bint), Rest2};
deserialize2(<<S:6, 2#01:2, Rest/binary>>) -> %% String
    String = binary:part(Rest, 0, S),
    Rest2 = binary:part(Rest, byte_size(Rest), - (byte_size(Rest) - S)),
    {String, Rest2};
deserialize2(<<?ADDRESS, Rest/binary>>) ->
    A = binary:part(Rest, 0, 32),
    Rest2 = binary:part(Rest, byte_size(Rest), - (byte_size(Rest) - 32)),
    {A, Rest2};
deserialize2(<<?TRUE, Rest/binary>>) -> {true, Rest};
deserialize2(<<?FALSE, Rest/binary>>) -> {false, Rest};
deserialize2(<<?NIL, Rest/binary>>) -> {[], Rest};
deserialize2(<<?TUPLE0, Rest/binary>>) -> {{tuple, {}}, Rest};
deserialize2(<<?EMPTYM, Rest/binary>>) -> {#{}, Rest};
deserialize2(<<?EMPTYS, Rest/binary>>) -> {<<>>, Rest};
deserialize2(<<S:4, ?TUPLE:4, Rest/binary>>) ->
    {N, E} =
        if S =:= 0 ->
                <<Size, Elements/binary>> = Rest,
                {Size, Elements};
           true -> {S, Rest}
        end,
    {List, Rest2} = deserialize_elements(N, E),
    {{tuple, list_to_tuple(List)}, Rest2};
deserialize2(<<S:4, ?LIST:4, Rest/binary>>) ->
    {_Typerep, Rest2} = deserialize_typerep(Rest),
    {N, E} =
        if S =:= 0 ->
                <<Size, Elements/binary>> = Rest2,
                {Size, Elements};
           true -> {S, Rest2}
        end,
    {List, Rest3} = deserialize_elements(N, E),
    %% TODO: check typerep == type of List
    {List, Rest3};
deserialize2(<<?LLIST, Rest/binary>>) ->
    {_Typerep, Rest2} = deserialize_typerep(Rest),
    {BLength, Rest3} = aeu_rlp:decode_one(Rest2),
    Length = binary:decode_unsigned(BLength),
    {List, Rest4} = deserialize_elements(Length, Rest3),
    %% TODO: check typerep == type of List
    {List, Rest4};
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
    {BSize, Rest1} = aeu_rlp:decode_one(Rest),
    Size = binary:decode_unsigned(BSize),
    {List, Rest2} = deserialize_variant_cases(Size, Rest1),
    {List, Rest2}.

deserialize_variant_cases(0, Rest) ->
    {[], Rest};
deserialize_variant_cases(N, Rest) ->
    {BSize, Rest1} = aeu_rlp:decode_one(Rest),
    Size = binary:decode_unsigned(BSize),
    {TList, Rest2} = deserialize_typereps(Size, Rest1),
    {CaseList, Rest3} = deserialize_variant_cases(N-1, Rest2),
    {[TList|CaseList], Rest3}.

deserialize_typereps(0, Rest) ->
    {[], Rest};
deserialize_typereps(N, Rest) ->
    {T, Rest2} = deserialize_typerep(Rest),
    {List, Rest3} = deserialize_typereps(N-1, Rest2),
    {[T|List], Rest3}.



%% variant type : 10101111 + RLP encoded size + [RLP encoded size + [typereps]]

insert_kv([], M) -> M;
insert_kv([K,V|R], M) -> insert_kv(R, maps:put(K, V, M)).




deserialize_elements(0, Rest) ->
    {[], Rest};
deserialize_elements(N, Es) ->
    {E, Rest} = deserialize2(Es),
    {Tail, Rest2} = deserialize_elements(N-1, Rest),
    {[E|Tail], Rest2}.







