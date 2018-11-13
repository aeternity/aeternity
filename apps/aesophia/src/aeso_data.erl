-module(aeso_data).

-export([ to_binary/1
        , to_binary/2
        , binary_to_words/1
        , from_heap/3
        , binary_to_heap/4
        , heap_to_heap/3
        , heap_to_heap/4
        , heap_to_binary/3
        , heap_to_binary/4
        , binary_to_binary/2
        , heap_value/3
        , heap_value/4
        , heap_value_pointer/1
        , heap_value_maps/1
        , heap_value_offset/1
        , heap_value_heap/1
        , used_maps/2
        , has_maps/1
        , from_binary/2
        , from_binary/3
        , get_function_hash_from_calldata/1
        , sophia_type_to_typerep/1
        ]).

-export_type([binary_value/0, binary_heap_value/0, heap_value/0, heap_fragment/0]).

-include("aeso_icode.hrl").
-include("aeso_data.hrl").

-record(heap, { maps   :: #maps{},
                offset :: offset(),
                heap   :: binary() | #{non_neg_integer() => non_neg_integer()} }).

-type store() :: aect_contracts:store().  %% #{ binary() => binary() }

-type word()            :: non_neg_integer().
-type pointer()         :: word().
-type offset()          :: non_neg_integer().
-type binary_value()    :: binary().
-opaque heap_fragment() :: #heap{}.
-type heap_value()      :: {pointer(), heap_fragment()}.
-type binary_heap_value() :: binary().

%% There are four different representations of Sophia values:
%%
%%  term()
%%      Erlang term representation, returned by from_binary and consumed by
%%      to_binary.
%%
%%  binary_value()
%%      Self contained binary representation. First word is a pointer to the
%%      value. Maps are stored as a size followed by a sequence of key/value
%%      pairs. These are used when communicating values between the outside
%%      world and the VM (contract transaction calldata and return values and
%%      remote calls).
%%
%%  binary_heap_value()
%%      Same as binary_value() except maps are only stored as their identifier.
%%      These are used as map values in the store and VM state and the contract
%%      state data.
%%
%%  heap_value()
%%      A binary_heap_value() (but with the pointer and the rest of the binary
%%      split) together with the maps used by the value. Also allows an
%%      arbitrary offset for the binary. These are used when copying values
%%      inside the VM. The full VM state (heap and maps) together with a
%%      pointer can treated as a heap value.
%%

%% -- Manipulating heap values -----------------------------------------------

no_store() -> #{}.

no_maps(N) when is_integer(N) -> #maps{ next_id = N };
no_maps(#heap{maps = #maps{next_id = N}}) -> no_maps(N).

add_map(Map, #maps{ maps = Maps, next_id = Id }) ->
    {Id, #maps{ next_id = Id + 1,
                maps    = Maps#{ Id => Map } }}.

merge_maps(#maps{maps = Maps1}, #maps{maps = Maps2, next_id = N}) ->
    #maps{maps = maps:merge(Maps1, Maps2), next_id = N}.

set_next_id(Heap, N) -> Heap#heap{ maps = Heap#heap.maps#maps{ next_id = N } }.

heap_fragment(Offs, Heap) ->
    heap_fragment(no_maps(0), Offs, Heap).

heap_fragment(Maps, Offs, Heap) ->
    #heap{maps = Maps, offset = Offs, heap = Heap}.

-spec heap_value(aevm_eeevm_maps:maps() | #maps{}, pointer(), binary(), offset()) -> heap_value().
heap_value(Maps, Ptr, Heap, Offs) ->
    {Ptr, heap_fragment(Maps, Offs, Heap)}.

-spec heap_value(aevm_eeevm_maps:maps(), pointer(), binary()) -> heap_value().
heap_value(Maps, Ptr, Heap) ->
    heap_value(Maps, Ptr, Heap, 0).

-spec heap_value_pointer(heap_value()) -> pointer().
heap_value_pointer({Ptr, _}) -> Ptr.

-spec heap_value_maps(heap_value()) -> aevm_eeevm_maps:maps().
heap_value_maps({_, Heap}) -> Heap#heap.maps.

-spec heap_value_offset(heap_value()) -> offset().
heap_value_offset({_, Heap}) -> Heap#heap.offset.

-spec heap_value_heap(heap_value()) -> binary().
heap_value_heap({_, Heap}) -> Heap#heap.heap.

%% -- Binary to heap ---------------------------------------------------------

-spec binary_to_heap(Type :: ?Type(), Bin :: binary_value(), NextId :: non_neg_integer(), Offs :: offset()) ->
        {ok, heap_value()} | {error, term()}.
%% Takes a binary encoded with to_binary/1 and returns a heap fragment starting at Offs.
binary_to_heap(Type, <<Ptr:32/unit:8, Heap/binary>>, NextId, Offs) ->
    try
        {Addr, {Maps, _, Mem}} = convert(binary, heap, infinity, no_store(), #{}, Type, Ptr,
                                         heap_fragment(no_maps(NextId), 32, Heap), Offs),
        {ok, heap_value(Maps, Addr, list_to_binary(Mem), Offs)}
    catch _:Err ->
        %%io:format("** Error: binary_to_heap failed with ~p\n  ~p\n", [Err, erlang:get_stacktrace()]),
        {error, Err}
    end;
binary_to_heap(_Type, <<>>, _NextId, _Offs) ->
    {error, binary_too_short}.

%% -- Heap to binary ---------------------------------------------------------

-spec heap_to_binary(Type :: ?Type(), Store :: store(), Heap :: heap_value()) ->
        {ok, binary_value()} | {error, term()}.
heap_to_binary(Type, Store, HeapVal) ->
    heap_to_binary(Type, Store, HeapVal, infinity).

-spec heap_to_binary(Type :: ?Type(), Store :: store(), Heap :: heap_value(), infinity | non_neg_integer()) ->
        {ok, binary_value()} | {error, term()}.
heap_to_binary(Type, Store, {Ptr, Heap}, MaxSize) ->
    try
        {Addr, {_, _, Memory}} = convert(heap, binary, MaxSize, Store, #{}, Type, Ptr, Heap, 32),
        {ok, <<Addr:256, (list_to_binary(Memory))/binary>>}
    catch
        throw:max_size_exceeded ->
            {error, out_of_gas};
        _:Err ->
            %%io:format("** Error: heap_to_binary failed with ~p\n  ~p\n", [Err, erlang:get_stacktrace()]),
            {error, Err}
    end.

%% -- Binary to binary -------------------------------------------------------

-spec binary_to_binary(Type :: ?Type(), Bin :: binary_value()) ->
        {ok, binary_value()} | {error, term()}.
binary_to_binary(Type, <<Ptr:32/unit:8, Heap/binary>>) ->
    try
        {Addr, {_, _, Memory}} = convert(binary, binary, infinity, no_store(), #{}, Type, Ptr,
                                         heap_fragment(no_maps(0), 32, Heap), 32),
        {ok, <<Addr:256, (list_to_binary(Memory))/binary>>}
    catch _:Err ->
        %%io:format("** Error: binary_to_binary failed with ~p\n  ~p\n", [Err, erlang:get_stacktrace()]),
        {error, Err}
    end.

%% -- Heap to heap -----------------------------------------------------------

%% Used for the state
-spec heap_to_heap(Type :: ?Type(), Heap :: heap_value(), Offs :: offset()) ->
        {ok, heap_value()} | {error, term()}.
heap_to_heap(Type, HeapVal, Offs) ->
    heap_to_heap(Type, HeapVal, Offs, infinity).

-spec heap_to_heap(Type :: ?Type(), Heap :: heap_value(), Offs :: offset(), MaxSize :: non_neg_integer() | infinity) ->
        {ok, heap_value()} | {error, term()}.
heap_to_heap(Type, {Ptr, Heap}, Offs, MaxSize) ->
    try
        {Addr, {Maps, _, Mem}} = convert(heap, heap, MaxSize, no_store(), #{}, Type, Ptr, Heap, Offs),
        {ok, heap_value(Maps, Addr, list_to_binary(Mem), Offs)}
    catch _:Err ->
        %%io:format("** Error: heap_to_heap failed with ~p\n  ~p\n", [Err, erlang:get_stacktrace()]),
        {error, Err}
    end.

%% -- Generic heap/binary conversion -----------------------------------------

-type visited() :: #{pointer() => true}.
-type format() :: heap | binary.

-spec convert(Input :: format(), Output :: format(), MaxSize :: non_neg_integer() | infinity,
              Store :: store(), visited(), ?Type(), pointer(),
              heap_fragment(), offset()) -> {pointer(), {#maps{}, offset(), [iodata()]}}.
convert(_, _, _, _, _, word, Val, Heap, _) ->
    {Val, {no_maps(Heap), 0, []}};
convert(_, _, MaxSize, _, _Visited, string, Val, Heap, BaseAddr) ->
    Size  = get_word(Heap, Val),
    Words = 1 + (Size + 31) div 32, %% 1 + ceil(Size / 32)
    Bytes = Words * 32,
    check_size(MaxSize, Bytes),
    {BaseAddr, {no_maps(Heap), Bytes, [get_chunk(Heap, Val, Bytes)]}};
convert(Input, Output, MaxSize, Store, Visited, {list, T}, Val, Heap, BaseAddr) ->
    <<Nil:256>> = <<(-1):256>>,   %% empty list is -1
    case Val of
        Nil -> {Nil, {no_maps(Heap), 0, []}};
        _   -> convert(Input, Output, MaxSize, Store, Visited, {tuple, [T, {list, T}]}, Val, Heap, BaseAddr)
    end;
convert(Input, binary, MaxSize, Store, _Visited, {map, KeyT, ValT}, MapId, Heap, BaseAddr) ->
    {_NoMaps, Map0} = convert_map(Input, binary, Store, KeyT, ValT, MapId, Heap),
    %% Will be a no-op if Input == binary
    Map  = aevm_eeevm_maps:flatten_map(Store, MapId, Map0),
    KVs  = maps:to_list(Map#pmap.data),
    Size = maps:size(Map#pmap.data),
    {RMem, FinalBase} =
        lists:foldl(fun({Key, Val}, {Mem, Base}) ->
                        KeySize = byte_size(Key),
                        ValSize = byte_size(Val),
                        Base1   = Base + KeySize + 32 + ValSize + 32,
                        {[[<<KeySize:256>>, Key, <<ValSize:256>>, Val] | Mem],
                         Base1}
                    end, {[], BaseAddr + 32}, KVs),
    %% Checking after the fact, but the resulting binary is made up of binaries
    %% already existing in the state so it can't be arbitrarily big.
    check_size(MaxSize, FinalBase - BaseAddr),
    Mem  = lists:reverse(RMem),
    %% Target is binary so no maps required
    {BaseAddr, {no_maps(Heap), FinalBase - BaseAddr, [<<Size:256>>, Mem]}};
convert(Input, heap, _MaxSize, Store, _Visited, {map, KeyT, ValT}, Ptr, Heap, _BaseAddr) ->
    {InnerMaps, PMap} = convert_map(Input, heap, Store, KeyT, ValT, Ptr, Heap),
    case PMap#pmap.data of
        stored -> %% Keep the id of stored maps (only possible if Input == heap)
            Maps = InnerMaps#maps{ maps = (InnerMaps#maps.maps)#{ Ptr => PMap } },
            {Ptr, {Maps, 0, []}};
        _ ->
            case Input of   %% Don't allocate new maps for heap->heap
                heap ->
                    Maps = InnerMaps#maps{ maps = (InnerMaps#maps.maps)#{ Ptr => PMap } },
                    {Ptr, {Maps, 0, []}};
                binary ->
                    {Id, Maps} = add_map(PMap, InnerMaps),
                    {Id, {Maps, 0, []}}
            end
    end;
convert(_, _, _, _, _, {tuple, []}, _Ptr, Heap, _BaseAddr) ->
    {0, {no_maps(Heap), 0, []}}; %% Use 0 for the empty tuple (need a unique value).
convert(Input, Output, MaxSize, Store, Visited, {tuple, Ts}, Ptr, Heap, BaseAddr) ->
    Visited1  = visit(Visited, Ptr),
    TupleCellSize = 32 * length(Ts),
    BaseAddr1 = BaseAddr + TupleCellSize,  %% store component data after the tuple cell
    Ptrs      = [ P || <<P:256>> <= get_chunk(Heap, Ptr, 32 * length(Ts)) ],
    {BaseAddr2, Maps, NewPtrs, Memory} = convert_components(Input, Output, subtract_size(MaxSize, TupleCellSize), Store, Visited1, Ts, Ptrs, Heap, BaseAddr1),
    check_size(MaxSize, BaseAddr2 - BaseAddr),
    {BaseAddr, {Maps, BaseAddr2 - BaseAddr, [NewPtrs, Memory]}};
convert(Input, Output, MaxSize, Store, Visited, {variant, Cs}, Ptr, Heap, BaseAddr) ->
    Tag = get_word(Heap, Ptr),
    Ts  = lists:nth(Tag + 1, Cs),
    convert(Input, Output, MaxSize, Store, Visited, {tuple, [word | Ts]}, Ptr, Heap, BaseAddr);
convert(Input, Output, MaxSize, Store, Visited, typerep, Ptr, Heap, BaseAddr) ->
    Typerep = {variant, [[],                         %% word
                         [],                         %% string
                         [typerep],                  %% list
                         [{list, typerep}],          %% tuple
                         [{list, {list, typerep}}],  %% variant
                         [],                         %% typerep
                         [typerep, typerep]          %% map
                        ]},
    convert(Input, Output, MaxSize, Store, Visited, Typerep, Ptr, Heap, BaseAddr).

convert_components(Input, Output, MaxSize, Store, Visited, Ts, Ps, Heap, BaseAddr) ->
    convert_components(Input, Output, MaxSize, Store, Visited, Ts, Ps, Heap, BaseAddr, [], [], no_maps(Heap)).

convert_components(_, _, _, _, _Visited, [], [], _Heap, BaseAddr, PtrAcc, MemAcc, Maps) ->
    {BaseAddr, Maps, lists:reverse(PtrAcc), lists:reverse(MemAcc)};
convert_components(Input, Output, MaxSize, Store, Visited, [T | Ts], [Ptr | Ptrs], Heap, BaseAddr, PtrAcc, MemAcc, Maps) ->
    %% (Ab)use the next_id field in the input heap for suitable next_id of the output heap.
    Heap1 = set_next_id(Heap, Maps#maps.next_id),
    {NewPtr, {Maps1, Size, Mem}} = convert(Input, Output, MaxSize, Store, Visited, T, Ptr, Heap1, BaseAddr),
    convert_components(Input, Output, subtract_size(MaxSize, Size), Store, Visited, Ts, Ptrs, Heap, BaseAddr + Size,
                         [<<NewPtr:256>> | PtrAcc], [Mem | MemAcc], merge_maps(Maps, Maps1)).

subtract_size(infinity, _)   -> infinity;
subtract_size(MaxSize, Size) ->
    check_size(MaxSize, Size),
    MaxSize - Size.

check_size(MaxSize, Size) when Size > MaxSize -> throw(max_size_exceeded);
check_size(_, _) -> ok.

get_map(heap, _KeyT, _ValT, MapId, Heap) ->
    #{ MapId := Map } = Heap#heap.maps#maps.maps,
    Map;
get_map(binary, KeyT, ValT, Ptr, Heap) ->
    Size = get_word(Heap, Ptr),
    Map  = map_binary_to_heap(Size, Heap, Ptr + 32),
    #pmap{ key_t = KeyT, val_t = ValT, size = maps:size(Map), parent = none, data = Map }.

convert_map(Input, Output, Store, KeyT, ValT, Ptr, Heap) ->
    Map = #pmap{ data = Data } = get_map(Input, KeyT, ValT, Ptr, Heap),
    {InnerMaps, Data1} =
        convert_map_values(Input, Output, Store, ValT, Data, Heap),
    {InnerMaps, Map#pmap{ data = Data1 }}.

convert_map_values(_, _, _Store, _ValT, stored, Heap) ->
    {no_maps(Heap), stored};
convert_map_values(Input, Output, Store, ValT, Data, Heap) ->
    KVs = maps:to_list(Data),
    {Maps, Data1} =
        lists:foldl(fun({K, V}, {VMaps, D}) ->
                        Heap1 = set_next_id(Heap, VMaps#maps.next_id),
                        {VMaps1, V1} = convert_map_value(Input, Output, Store, ValT, V, Heap1),
                        {merge_maps(VMaps, VMaps1), D#{ K => V1 }}
                    end, {no_maps(Heap), #{}}, KVs),
    {Maps, Data1}.

convert_map_value(_Input, _Output, _Store, _ValT, tombstone, Heap) ->
    {no_maps(Heap), tombstone};
convert_map_value(Input, Output, Store, ValT, <<ValPtr:256, ValBin/binary>>, Heap) ->
    ValHeap = heap_fragment(Heap#heap.maps, 32, ValBin),
    Visited = #{},  %% Map values are self contained so start with fresh circularity check
    %% Converting from a binary heap so we don't have to limit the size
    {ValPtr1, {Maps, _Size, ValBin1}} = convert(Input, Output, infinity, Store, Visited, ValT, ValPtr, ValHeap, 32),
    {Maps, <<ValPtr1:256, (list_to_binary(ValBin1))/binary>>}.

%% -- Compute used map ids ---------------------------------------------------

-spec used_maps(?Type(), binary_heap_value()) -> [non_neg_integer()].
used_maps(Type, <<Ptr:256, Mem/binary>>) ->
    used_maps(#{}, Type, Ptr, heap_fragment(32, Mem)).

has_maps({map, _, _})   -> true;
has_maps(word)          -> false;
has_maps(string)        -> false;
has_maps(typerep)       -> false;
has_maps({list, T})     -> has_maps(T);
has_maps({tuple, Ts})   -> lists:any(fun has_maps/1, Ts);
has_maps({variant, Cs}) -> lists:any(fun has_maps/1, lists:append(Cs)).

used_maps(Visited, Type, Ptr, Heap) ->
    case has_maps(Type) of
        false -> [];
        true  -> used_maps1(Visited, Type, Ptr, Heap)
    end.

used_maps1(_, {map, _, _}, Id, _) -> [Id];
used_maps1(Visited, {list, T}, Val, Heap) ->
    <<Nil:256>> = <<(-1):256>>,   %% empty list is -1
    case Val of
        Nil -> [];
        _   -> used_maps1(Visited, {tuple, [T, {list, T}]}, Val, Heap)
    end;
used_maps1(Visited, {tuple, Ts}, Ptr, Heap) ->
    Visited1 = visit(Visited, Ptr),
    Used     = [ begin
                    P = get_word(Heap, Ptr + I * 32),
                    used_maps(Visited1, T, P, Heap)
                 end || {T, I} <- lists:zip(Ts, lists:seq(0, length(Ts) - 1)) ],
    lists:umerge(Used);
used_maps1(Visited, {variant, Cs}, Ptr, Heap) ->
    Tag = get_word(Heap, Ptr),
    Ts  = lists:nth(Tag + 1, Cs),
    used_maps(Visited, {tuple, [word | Ts]}, Ptr, Heap).

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
    Words = binary_to_words(Data),
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

binary_to_words(<<>>) ->
    [];
binary_to_words(<<N:256,Bin/binary>>) ->
    [N|binary_to_words(Bin)];
binary_to_words(Bin) ->
    binary_to_words(<<Bin/binary,0>>).

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
    Heap = heap_fragment(0, Bin),
    map_from_binary({value, KeyType, ValType}, min(N, MaxN), Heap, Ptr, #{}).

map_binary_to_heap(N, Heap, Ptr) ->
    %% Avoid looping on bogus sizes
    MaxN = byte_size(Heap#heap.heap) div 64,
    map_from_binary(heap, min(N, MaxN), Heap, Ptr, #{}).

map_from_binary(_, 0, _, _, Map) -> Map;
map_from_binary(Output, I, Heap, Ptr, Map) ->
    KeySize = get_word(Heap, Ptr),
    KeyPtr  = Ptr + 32,
    KeyBin  = get_chunk(Heap, KeyPtr, KeySize),
    ValSize = get_word(Heap, KeyPtr + KeySize),
    ValPtr  = KeyPtr + KeySize + 32,
    ValBin  = get_chunk(Heap, ValPtr, ValSize),
    Map1    =
        case Output of
            {value, KeyType, ValType} ->
                %% Keys and values are self contained binaries
                {ok, Key} = from_binary(KeyType, KeyBin),
                {ok, Val} = from_binary(ValType, ValBin),
                Map#{Key => Val};
            heap ->
                Map#{KeyBin => ValBin}
        end,
    map_from_binary(Output, I - 1, Heap, ValPtr + ValSize, Map1).

visit(Visited, V) ->
    check_circular_refs(Visited, V),
    Visited#{ V => true }.

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
    Word;
get_word(Mem, Addr) when is_map(Mem) ->
    0 = Addr rem 32,
    maps:get(Addr, Mem, 0).

get_chunk(#heap{offset = Offs, heap = Mem}, Addr, Bytes) when Addr >= Offs ->
    get_chunk(Mem, Addr - Offs, Bytes);
get_chunk(Mem, Addr, Bytes) when is_binary(Mem) ->
    <<_:Addr/unit:8, Chunk:Bytes/binary, _/binary>> = Mem,
    Chunk;
get_chunk(Mem, Addr, Bytes) when is_map(Mem) ->
    0 = Addr  rem 32,
    0 = Bytes rem 32,
    Words = Bytes div 32,
    << <<(maps:get(Addr + 32 * I, Mem, 0)):256>> || I <- lists:seq(0, Words - 1) >>.

-spec get_function_hash_from_calldata(Calldata::binary()) ->
                                             {ok, binary()} | {error, term()}.
get_function_hash_from_calldata(Calldata) ->
    case from_binary({tuple, [word]}, Calldata) of
        {ok, {HashInt}} -> {ok, <<HashInt:256>>};
        {error, _} = Error -> Error
    end.


sophia_type_to_typerep(String) ->
    {ok, Ast} = aeso_parser:type(String),
    try aeso_ast_to_icode:ast_typerep(Ast) of
        Type -> {ok, Type}
    catch _:_ -> {error, bad_type}
    end.

