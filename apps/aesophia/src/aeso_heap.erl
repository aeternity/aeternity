-module(aeso_heap).

-export([ to_binary/1
        , to_binary/2
        , from_heap/3
        , from_binary/2
        , from_binary/3
        , maps_with_next_id/1
        , set_next_id/2
        , heap_fragment/3
        , heap_value/3
        , heap_value/4
        , heap_value_pointer/1
        , heap_value_maps/1
        , heap_value_offset/1
        , heap_value_heap/1
        , heap_fragment_maps/1
        , heap_fragment_offset/1
        , heap_fragment_heap/1
        ]).

-export_type([binary_value/0, heap_value/0, offset/0, heap_fragment/0]).

-include("aeso_icode.hrl").
-include_lib("aesophia/include/aeso_heap.hrl").

-type word()            :: non_neg_integer().
-type pointer()         :: word().
-opaque heap_fragment() :: #heap{}.
-type offset()          :: non_neg_integer().
-type binary_value()    :: binary().
-type heap_value()      :: {pointer(), heap_fragment()}.


-spec maps_with_next_id(heap_fragment()) -> #maps{}.
%% Create just a maps value, don't keep rest of Heap
maps_with_next_id(#heap{maps = #maps{next_id = N}}) ->
    #maps{ next_id = N }.

-spec set_next_id(heap_fragment(), non_neg_integer()) -> heap_fragment().
set_next_id(Heap, N) -> 
    Heap#heap{ maps = Heap#heap.maps#maps{ next_id = N } }.

%% -- data type heap_fragment

-spec heap_fragment(binary() | #{non_neg_integer() => non_neg_integer()}) -> heap_fragment().
heap_fragment(Heap) ->
    heap_fragment(#maps{ next_id = 0 }, 0, Heap).

-spec heap_fragment(#maps{}, offset(), 
                    binary() | #{non_neg_integer() => non_neg_integer()}) -> heap_fragment().
heap_fragment(Maps, Offset, Heap) ->
    #heap{maps = Maps, offset = Offset, heap = Heap}.

-spec heap_fragment_maps(heap_fragment()) -> #maps{}.
heap_fragment_maps(#heap{maps = Maps}) ->
    Maps.

-spec heap_fragment_offset(heap_fragment()) -> offset().
heap_fragment_offset(#heap{offset = Offs}) ->
    Offs.

-spec heap_fragment_heap(heap_fragment()) -> binary() | #{non_neg_integer() => non_neg_integer()}.
heap_fragment_heap(#heap{heap = Heap}) ->
    Heap.


%% -- data type heap_value

-spec heap_value(#maps{}, pointer(), 
                 binary() | #{non_neg_integer() => non_neg_integer()}) -> heap_value().
heap_value(Maps, Ptr, Heap) ->
    heap_value(Maps, Ptr, Heap, 0).

-spec heap_value(#maps{}, pointer(), 
                 binary() | #{non_neg_integer() => non_neg_integer()}, offset()) -> heap_value().
heap_value(Maps, Ptr, Heap, Offs) ->
    {Ptr, heap_fragment(Maps, Offs, Heap)}.

-spec heap_value_pointer(heap_value()) -> pointer().
heap_value_pointer({Ptr, _}) -> Ptr.

-spec heap_value_maps(heap_value()) -> #maps{}.
heap_value_maps({_, Heap}) -> Heap#heap.maps.

-spec heap_value_offset(heap_value()) -> offset().
heap_value_offset({_, Heap}) -> Heap#heap.offset.

-spec heap_value_heap(heap_value()) -> 
                             binary() | #{non_neg_integer() => non_neg_integer()}.
heap_value_heap({_, Heap}) -> Heap#heap.heap.
    
%% -- Value to binary --------------------------------------------------------

-spec to_binary(aeso_sophia:data()) -> aeso_sophia:heap().
%% Encode the data as a heap where the first word is the value (for unboxed
%% types) or a pointer to the value (for boxed types).
to_binary(Data) ->
    to_binary(Data, 0).

to_binary(Data, BaseAddress) ->
    {Address, Memory} = to_binary1(Data, BaseAddress + 32),
    R = <<Address:256, Memory/binary>>,
    R.


%% Allocate the data in memory, from the given address.  Return a pair
%% of memory contents from that address and the value representing the
%% data.
to_binary1(Data,_Address) when is_integer(Data) ->
    {Data,<<>>};
to_binary1(Data, Address) when is_binary(Data) ->
    %% a string
    Words = aeso_memory:binary_to_words(Data),
    {Address,<<(size(Data)):256, << <<W:256>> || W <- Words>>/binary>>};
to_binary1(none, Address)            -> to_binary1({variant, 0, []}, Address);
to_binary1({some, Value}, Address)   -> to_binary1({variant, 1, [Value]}, Address);
to_binary1(word, Address)            -> to_binary1({?TYPEREP_WORD_TAG}, Address);
to_binary1(string, Address)          -> to_binary1({?TYPEREP_STRING_TAG}, Address);
to_binary1(typerep, Address)         -> to_binary1({?TYPEREP_TYPEREP_TAG}, Address);
to_binary1(function, Address)        -> to_binary1({?TYPEREP_FUN_TAG}, Address);
to_binary1({list, T}, Address)       -> to_binary1({?TYPEREP_LIST_TAG, T}, Address);
to_binary1({option, T}, Address)     -> to_binary1({variant, [[], [T]]}, Address);
to_binary1({tuple, Ts}, Address)     -> to_binary1({?TYPEREP_TUPLE_TAG, Ts}, Address);
to_binary1({variant, Cons}, Address) -> to_binary1({?TYPEREP_VARIANT_TAG, Cons}, Address);
to_binary1({map, K, V}, Address)     -> to_binary1({?TYPEREP_MAP_TAG, K, V}, Address);
to_binary1({variant, Tag, Args}, Address) ->
    to_binary1(list_to_tuple([Tag | Args]), Address);
to_binary1(Map, Address) when is_map(Map) ->
    Size = maps:size(Map),
    %% Sort according to binary ordering
    KVs = lists:sort([ {to_binary(K), to_binary(V)} || {K, V} <- maps:to_list(Map) ]),
    {Address, <<Size:256, << <<(byte_size(K)):256, K/binary,
                               (byte_size(V)):256, V/binary>> || {K, V} <- KVs >>/binary >>};
to_binary1({}, _Address) ->
    {0, <<>>};
to_binary1(Data, Address) when is_tuple(Data) ->
    {Elems,Memory} = to_binaries(tuple_to_list(Data),Address+32*size(Data)),
    ElemsBin = << <<W:256>> || W <- Elems>>,
    {Address,<< ElemsBin/binary, Memory/binary >>};
to_binary1([],_Address) ->
    <<Nil:256>> = <<(-1):256>>,
    {Nil,<<>>};
to_binary1([H|T],Address) ->
    to_binary1({H,T},Address).


to_binaries([],_Address) ->
    {[],<<>>};
to_binaries([H|T],Address) ->
    {HRep,HMem} = to_binary1(H,Address),
    {TRep,TMem} = to_binaries(T,Address+size(HMem)),
    {[HRep|TRep],<<HMem/binary, TMem/binary>>}.

%% Interpret a return value (a binary) using a type rep.

-spec from_heap(Type :: ?Type(), Heap :: binary(), Ptr :: integer()) ->
        {ok, term()} | {error, term()}.
from_heap(Type, Heap, Ptr) ->
    try {ok, from_binary(#{}, Type, Heap, Ptr)}
    catch _:Err ->
        %% io:format("** Error: from_heap failed with ~p\n  ~p\n", [Err, erlang:get_stacktrace()]),
        {error, Err}
    end.

%% Base address is the address of the first word of the given heap.
-spec from_binary(T :: ?Type(),
                  Heap :: binary(),
                  BaseAddr :: non_neg_integer()) ->
        {ok, term()} | {error, term()}.
from_binary(T, Heap = <<V:256, _/binary>>, BaseAddr) ->
    from_heap(T, <<0:BaseAddr/unit:8, Heap/binary>>, V);
from_binary(_, Bin, _BaseAddr) ->
    {error, {binary_too_short, Bin}}.

-spec from_binary(?Type(), binary()) -> {ok, term()} | {error, term()}.
from_binary(T, Heap) ->
    from_binary(T, Heap, 0).

from_binary(_, word, _, V) ->
    V;
from_binary(_, signed_word, _, V) ->
    <<N:256/signed>> = <<V:256>>,
    N;
from_binary(_, bool, _, V) ->
    case V of
        0 -> false;
        1 -> true
    end;
from_binary(_, string, Heap, V) ->
    StringSize = heap_word(Heap,V),
    BitAddr = 8*(V+32),
    <<_:BitAddr,Bytes:StringSize/binary,_/binary>> = Heap,
    Bytes;
from_binary(_, {tuple, []}, _, _) ->
    {};
from_binary(Visited, {tuple,Cpts}, Heap, V) ->
    check_circular_refs(Visited, V),
    NewVisited = Visited#{V => true},
    ElementNums = lists:seq(0, length(Cpts)-1),
    TypesAndPointers = lists:zip(Cpts, ElementNums),
    ElementAddress = fun(Index) -> V + 32 * Index end,
    Element = fun(Index) ->
                      heap_word(Heap, ElementAddress(Index))
              end,
    Convert = fun(Type, Index) ->
                from_binary(NewVisited, Type, Heap, Element(Index))
              end,
    Elements = [Convert(T, I) || {T,I} <- TypesAndPointers],
    list_to_tuple(Elements);
from_binary(Visited, {list, Elem}, Heap, V) ->
    <<Nil:256>> = <<(-1):256>>,
    if V==Nil ->
          [];
       true ->
          {H,T} = from_binary(Visited, {tuple,[Elem,{list,Elem}]},Heap,V),
          [H|T]
    end;
from_binary(Visited, {option, A}, Heap, V) ->
    from_binary(Visited, {variant_t, [{none, []}, {some, [A]}]}, Heap, V);
from_binary(Visited, {variant, Cons}, Heap, V) ->
    Tag      = heap_word(Heap, V),
    Args     = lists:nth(Tag + 1, Cons),
    Visited1 = Visited#{V => true},
    {variant, Tag, tuple_to_list(from_binary(Visited1, {tuple, Args}, Heap, V + 32))};
from_binary(Visited, {variant_t, TCons}, Heap, V) ->   %% Tagged variants
    {Tags, Cons} = lists:unzip(TCons),
    {variant, I, Args} = from_binary(Visited, {variant, Cons}, Heap, V),
    Tag = lists:nth(I + 1, Tags),
    case Args of
        []  -> Tag;
        _   -> list_to_tuple([Tag | Args])
    end;
from_binary(_Visited, {map, A, B}, Heap, Ptr) ->
    %% FORMAT: [Size] [KeySize] Key [ValSize] Val .. [KeySize] Key [ValSize] Val
    Size = heap_word(Heap, Ptr),
    map_binary_to_value(A, B, Size, Heap, Ptr + 32);
from_binary(Visited, typerep, Heap, V) ->
    check_circular_refs(Visited, V),
    Tag = heap_word(Heap, V),
    Arg1 = fun(T, I) -> from_binary(Visited#{V => true}, T, Heap, heap_word(Heap, V + 32 * I)) end,
    Arg  = fun(T) -> Arg1(T, 1) end,
    case Tag of
        ?TYPEREP_WORD_TAG    -> word;
        ?TYPEREP_STRING_TAG  -> string;
        ?TYPEREP_TYPEREP_TAG -> typerep;
        ?TYPEREP_LIST_TAG    -> {list,    Arg(typerep)};
        ?TYPEREP_TUPLE_TAG   -> {tuple,   Arg({list, typerep})};
        ?TYPEREP_VARIANT_TAG -> {variant, Arg({list, {list, typerep}})};
        ?TYPEREP_MAP_TAG     -> {map,     Arg(typerep), Arg1(typerep, 2)};
        ?TYPEREP_FUN_TAG     -> function
    end.

map_binary_to_value(KeyType, ValType, N, Bin, Ptr) ->
    %% Avoid looping on bogus sizes
    MaxN = byte_size(Bin) div 64,
    Heap = heap_fragment(Bin),
    map_from_binary({value, KeyType, ValType}, min(N, MaxN), Heap, Ptr, #{}).

map_from_binary(_, 0, _, _, Map) -> Map;
map_from_binary({value, KeyType, ValType} = Output, I, Heap, Ptr, Map) ->
    KeySize = get_word(Heap, Ptr),
    KeyPtr  = Ptr + 32,
    KeyBin  = get_chunk(Heap, KeyPtr, KeySize),
    ValSize = get_word(Heap, KeyPtr + KeySize),
    ValPtr  = KeyPtr + KeySize + 32,
    ValBin  = get_chunk(Heap, ValPtr, ValSize),
    %% Keys and values are self contained binaries
    {ok, Key} = from_binary(KeyType, KeyBin),
    {ok, Val} = from_binary(ValType, ValBin),
    map_from_binary(Output, I - 1, Heap, ValPtr + ValSize,  Map#{Key => Val}).

check_circular_refs(Visited, V) ->
    case maps:is_key(V, Visited) of
        true ->  exit(circular_references);
        false -> ok
    end.

heap_word(Heap, Addr) when is_binary(Heap) ->
    BitSize = 8*Addr,
    <<_:BitSize,W:256,_/binary>> = Heap,
    W;
heap_word(Heap, Addr) when is_map(Heap) ->
    0 = Addr rem 32, %% Check that it's word aligned.
    maps:get(Addr, Heap, 0).

get_word(#heap{offset = Offs, heap = Mem}, Addr) when Addr >= Offs ->
    get_word(Mem, Addr - Offs);
get_word(Mem, Addr) when is_binary(Mem) ->
    <<_:Addr/unit:8, Word:256, _/binary>> = Mem,
    Word.

get_chunk(#heap{offset = Offs, heap = Mem}, Addr, Bytes) when Addr >= Offs ->
    get_chunk(Mem, Addr - Offs, Bytes);
get_chunk(Mem, Addr, Bytes) when is_binary(Mem) ->
    <<_:Addr/unit:8, Chunk:Bytes/binary, _/binary>> = Mem,
    Chunk.




