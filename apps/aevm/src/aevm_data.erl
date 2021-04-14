-module(aevm_data).

-export([ binary_to_heap/4
        , heap_to_heap/3
        , heap_to_heap/4
        , heap_to_binary/3
        , heap_to_binary/4
        , binary_to_binary/2
        , used_maps/2
        , has_maps/1
        ]).

-include_lib("aebytecode/include/aeb_heap.hrl").
-include_lib("../../aecontract/include/aecontract.hrl").

-type store() :: aect_contracts:store().  %% #{ binary() => binary() }

-type word()            :: non_neg_integer().
-type pointer()         :: word().
-type offset()          :: non_neg_integer().
-type binary_value()    :: aeb_heap:binary_value().
-type heap_fragment()   :: aeb_heap:heap_fragment().
-type heap_value()      :: aeb_heap:heap_value().
-type binary_heap_value() :: binary().

%% There are four different representations of Sophia values:
%%
%%  term()
%%      Erlang term representation, returned by from_binary and consumed by
%%      to_binary in the aeb_heap module
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
%%      pointer can be treated as a heap value.
%%

%% -- Manipulating heap values -----------------------------------------------

no_store() -> aect_contracts_store:new().

no_maps(N) when is_integer(N) -> #maps{ next_id = N }.

add_map(Map, #maps{ maps = Maps, next_id = Id }) ->
    {Id, #maps{ next_id = Id + 1,
                maps    = Maps#{ Id => Map } }}.

merge_maps(#maps{maps = Maps1}, #maps{maps = Maps2, next_id = N}) ->
    #maps{maps = maps:merge(Maps1, Maps2), next_id = N}.

%% -- Binary to heap ---------------------------------------------------------

-spec binary_to_heap(Type :: aeb_aevm_data:type(), Bin :: binary_value(), NextId :: non_neg_integer(), Offs :: offset()) ->
        {ok, heap_value()} | {error, term()}.
%% Takes a binary encoded with to_binary/1 and returns a heap fragment starting at Offs.
binary_to_heap(Type, <<Ptr:32/unit:8, Heap/binary>>, NextId, Offs) ->
    try
        {Addr, {Maps, _, Mem}} = convert(binary, heap, infinity, no_store(), #{}, Type, Ptr,
                                         aeb_heap:heap_fragment(no_maps(NextId), 32, Heap), Offs),
        {ok, aeb_heap:heap_value(Maps, Addr, list_to_binary(Mem), Offs)}
    catch _:Err ->
        %% io:format("** Error: binary_to_heap failed with ~p\n  ~p\n", [Err, erlang:get_stacktrace()]),
        {error, Err}
    end;
binary_to_heap(_Type, <<>>, _NextId, _Offs) ->
    {error, binary_too_short}.

%% -- Heap to binary ---------------------------------------------------------

-spec heap_to_binary(Type :: aeb_aevm_data:type(), Store :: store(), Heap :: heap_value()) ->
        {ok, binary_value()} | {error, term()}.
heap_to_binary(Type, Store, HeapVal) ->
    heap_to_binary(Type, Store, HeapVal, infinity).

-spec heap_to_binary(Type :: aeb_aevm_data:type(), Store :: store(), Heap :: heap_value(), infinity | non_neg_integer()) ->
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

-spec binary_to_binary(Type :: aeb_aevm_data:type(), Bin :: binary_value()) ->
        {ok, binary_value()} | {error, term()}.
binary_to_binary(Type, <<Ptr:32/unit:8, Heap/binary>>) ->
    try
        {Addr, {_, _, Memory}} = convert(binary, binary, infinity, no_store(), #{}, Type, Ptr,
                                         aeb_heap:heap_fragment(no_maps(0), 32, Heap), 32),
        {ok, <<Addr:256, (list_to_binary(Memory))/binary>>}
    catch _:Err ->
        %%io:format("** Error: binary_to_binary failed with ~p\n  ~p\n", [Err, erlang:get_stacktrace()]),
        {error, Err}
    end.

%% -- Heap to heap -----------------------------------------------------------

%% Used for the state
-spec heap_to_heap(Type :: aeb_aevm_data:type(), Heap :: heap_value(), Offs :: offset()) ->
        {ok, heap_value()} | {error, term()}.
heap_to_heap(Type, HeapVal, Offs) ->
    heap_to_heap(Type, HeapVal, Offs, infinity).

-spec heap_to_heap(Type :: aeb_aevm_data:type(), Heap :: heap_value(), Offs :: offset(), MaxSize :: non_neg_integer() | infinity) ->
        {ok, heap_value()} | {error, term()}.
heap_to_heap(Type, {Ptr, Heap}, Offs, MaxSize) ->
    try
        {Addr, {Maps, _, Mem}} = convert(heap, heap, MaxSize, no_store(), #{}, Type, Ptr, Heap, Offs),
        {ok, aeb_heap:heap_value(Maps, Addr, list_to_binary(Mem), Offs)}
    catch _:Err ->
        %%io:format("** Error: heap_to_heap failed with ~p\n  ~p\n", [Err, erlang:get_stacktrace()]),
        {error, Err}
    end.

%% -- Generic heap/binary conversion -----------------------------------------

-type visited() :: #{pointer() => true}.
-type format() :: heap | binary.

-spec convert(Input :: format(), Output :: format(), MaxSize :: non_neg_integer() | infinity,
              Store :: store(), visited(), aeb_aevm_data:type(), pointer(),
              heap_fragment(), offset()) -> {pointer(), {#maps{}, offset(), [iodata()]}}.
convert(_, _, _, _, _, word, Val, Heap, _) ->
    {Val, {aeb_heap:maps_with_next_id(Heap), 0, []}};
convert(_, _, MaxSize, _, _Visited, string, Val, Heap, BaseAddr) ->
    Size  = get_word(Heap, Val),
    Words = 1 + (Size + 31) div 32, %% 1 + ceil(Size / 32)
    Bytes = Words * 32,
    check_size(MaxSize, Bytes),
    {BaseAddr, {aeb_heap:maps_with_next_id(Heap), Bytes, [get_chunk(Heap, Val, Bytes)]}};
convert(Input, Output, MaxSize, Store, Visited, {list, T}, Val, Heap, BaseAddr) ->
    <<Nil:256>> = <<(-1):256>>,   %% empty list is -1
    case Val of
        Nil -> {Nil, {aeb_heap:maps_with_next_id(Heap), 0, []}};
        _   -> convert(Input, Output, MaxSize, Store, Visited, {tuple, [T, {list, T}]}, Val, Heap, BaseAddr)
    end;
convert(Input, binary, MaxSize, Store, _Visited, {map, KeyT, ValT}, MapId, Heap, BaseAddr) ->
    {_NoMaps, Map} = convert_map(Input, binary, Store, KeyT, ValT, MapId, Heap),
    KVs  = lists:keysort(1, maps:to_list(Map#pmap.data)),
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
    {BaseAddr, {aeb_heap:maps_with_next_id(Heap), FinalBase - BaseAddr, [<<Size:256>>, Mem]}};
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
    {0, {aeb_heap:maps_with_next_id(Heap), 0, []}}; %% Use 0 for the empty tuple (need a unique value).
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
    convert_components(Input, Output, MaxSize, Store, Visited, Ts, Ps, Heap, BaseAddr, [], [], aeb_heap:maps_with_next_id(Heap)).

convert_components(_, _, _, _, _Visited, [], [], _Heap, BaseAddr, PtrAcc, MemAcc, Maps) ->
    {BaseAddr, Maps, lists:reverse(PtrAcc), lists:reverse(MemAcc)};
convert_components(Input, Output, MaxSize, Store, Visited, [T | Ts], [Ptr | Ptrs], Heap, BaseAddr, PtrAcc, MemAcc, Maps) ->
    %% (Ab)use the next_id field in the input heap for suitable next_id of the output heap.
    Heap1 = aeb_heap:set_next_id(Heap, Maps#maps.next_id),
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
    Maps = aeb_heap:heap_fragment_maps(Heap),
    #{ MapId := Map } = Maps#maps.maps,
    Map;
get_map(binary, KeyT, ValT, Ptr, Heap) ->
    Size = get_word(Heap, Ptr),
    Map  = map_binary_to_heap(Size, Heap, Ptr + 32),
    #pmap{ key_t = KeyT, val_t = ValT, size = maps:size(Map), parent = none, data = Map }.

convert_map(Input, Output, Store, KeyT, ValT, Ptr, Heap) ->
    Map0 = get_map(Input, KeyT, ValT, Ptr, Heap),
    Map = #pmap{ data = Data } =
        case Output of  %% Will be a no-op if Input == binary
            binary -> aevm_eeevm_maps:flatten_map(Store, Ptr, Map0);
            heap   -> Map0
        end,
    {InnerMaps, Data1} =
        convert_map_values(Input, Output, Store, ValT, Data, Heap),
    {InnerMaps, Map#pmap{ data = Data1 }}.

convert_map_values(_, _, _Store, _ValT, stored, Heap) ->
    {aeb_heap:maps_with_next_id(Heap), stored};
convert_map_values(Input, Output, Store, ValT, Data, Heap) ->
    KVs = lists:keysort(1, maps:to_list(Data)),
    {Maps, Data1} =
        lists:foldl(fun({K, V}, {VMaps, D}) ->
                        Heap1 = aeb_heap:set_next_id(Heap, VMaps#maps.next_id),
                        {VMaps1, V1} = convert_map_value(Input, Output, Store, ValT, V, Heap1),
                        {merge_maps(VMaps, VMaps1), D#{ K => V1 }}
                    end, {aeb_heap:maps_with_next_id(Heap), #{}}, KVs),
    {Maps, Data1}.

convert_map_value(_Input, _Output, _Store, _ValT, tombstone, Heap) ->
    {aeb_heap:maps_with_next_id(Heap), tombstone};
convert_map_value(Input, Output, Store, ValT, <<ValPtr:256, ValBin/binary>>, Heap) ->
    ValHeap = aeb_heap:heap_fragment(aeb_heap:heap_fragment_maps(Heap), 32, ValBin),
    Visited = #{},  %% Map values are self contained so start with fresh circularity check
    %% Converting from a binary heap so we don't have to limit the size
    {ValPtr1, {Maps, _Size, ValBin1}} = convert(Input, Output, infinity, Store, Visited, ValT, ValPtr, ValHeap, 32),
    {Maps, <<ValPtr1:256, (list_to_binary(ValBin1))/binary>>}.

%% -- Compute used map ids ---------------------------------------------------

-spec used_maps(aeb_aevm_data:type(), binary_heap_value()) -> [non_neg_integer()].
used_maps(Type, <<Ptr:256, Mem/binary>>) ->
    used_maps(#{}, Type, Ptr, aeb_heap:heap_fragment(no_maps(0), 32, Mem)).

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

map_binary_to_heap(N, Heap, Ptr) ->
    %% Avoid looping on bogus sizes
    MaxN = byte_size(aeb_heap:heap_fragment_heap(Heap)) div 64,
    map_from_binary_heap(min(N, MaxN), Heap, Ptr, #{}).

map_from_binary_heap(0, _, _, Map) -> Map;
map_from_binary_heap(I, Heap, Ptr, Map) ->
    KeySize = get_word(Heap, Ptr),
    KeyPtr  = Ptr + 32,
    KeyBin  = get_chunk(Heap, KeyPtr, KeySize),
    ValSize = get_word(Heap, KeyPtr + KeySize),
    ValPtr  = KeyPtr + KeySize + 32,
    ValBin  = get_chunk(Heap, ValPtr, ValSize),
    map_from_binary_heap(I - 1, Heap, ValPtr + ValSize, Map#{KeyBin => ValBin}).

visit(Visited, V) ->
    check_circular_refs(Visited, V),
    Visited#{ V => true }.

check_circular_refs(Visited, V) ->
    case maps:is_key(V, Visited) of
        true ->  exit(circular_references);
        false -> ok
    end.

get_word(Mem, Addr) when is_binary(Mem) ->
    <<_:Addr/unit:8, Word:256, _/binary>> = Mem,
    Word;
get_word(Mem, Addr) when is_map(Mem) ->
    0 = Addr rem 32,
    maps:get(Addr, Mem, 0);
get_word(Heap, Addr) ->
    Offs = aeb_heap:heap_fragment_offset(Heap),
    Mem = aeb_heap:heap_fragment_heap(Heap),
    case  Addr >= Offs of
        true -> get_word(Mem, Addr - Offs);
        false -> exit(address_before_offset)
    end.

get_chunk(Mem, Addr, Bytes) when is_binary(Mem) ->
    <<_:Addr/unit:8, Chunk:Bytes/binary, _/binary>> = Mem,
    Chunk;
get_chunk(Mem, Addr, Bytes) when is_map(Mem) ->
    0 = Addr  rem 32,
    0 = Bytes rem 32,
    Words = Bytes div 32,
    << <<(maps:get(Addr + 32 * I, Mem, 0)):256>> || I <- lists:seq(0, Words - 1) >>;
get_chunk(Heap, Addr, Bytes) ->
    Offs = aeb_heap:heap_fragment_offset(Heap),
    Mem = aeb_heap:heap_fragment_heap(Heap),
    case  Addr >= Offs of
        true -> get_chunk(Mem, Addr - Offs, Bytes);
        false -> exit(address_before_offset)
    end.





