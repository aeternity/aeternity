%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Handle store
%%% @end
%%% Created : 9 Oct 2017
%%%-------------------------------------------------------------------

-module(aevm_eeevm_store).

-export([ load/2
        , store/3
        , init/2
        , to_binary/1
        , get_sophia_state/1
        , get_sophia_state_type/1
        , from_sophia_state/2
        , set_sophia_state/3
        , is_valid_key/2
        , get_map_data/2
        , map_lookup/3
        , next_map_id/1
        ]).

-include("aevm_eeevm.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include_lib("aebytecode/include/aeb_heap.hrl").

-define(SOPHIA_STATE_KEY,      <<0>>).
-define(SOPHIA_STATE_TYPE_KEY, <<1>>).
-define(SOPHIA_STATE_MAPS_KEY, <<2>>).

%% -define(DEBUG, true).
-ifdef(DEBUG).
-define(DEBUG_PRINT(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(DEBUG_PRINT(Fmt, Args), ok).
-endif.

%%====================================================================
%% API
%%====================================================================

-spec init(aect_contracts:store(), aevm_eeevm_state:state()) -> aevm_eeevm_state:state().
init(Store, State) ->
    Map = aect_contracts_store:contents(Store),
    State#{ storage => binary_to_integer_map(Map) }.

-spec to_binary(aevm_eeevm_state:state()) -> aect_contracts:store().
to_binary(#{ storage := Storage }) ->
    aect_contracts_store:put_map(integer_to_binary_map(Storage),
                                 aect_contracts_store:new()).

-spec load(integer(), aevm_eeevm_state:state()) -> integer().
load(Address, State) ->
    Store = aevm_eeevm_state:storage(State),
    Value = storage_read(Address, Store),
    Value.

-spec store(integer(), integer(), aevm_eeevm_state:state()) -> aevm_eeevm_state:state().
store(Address, Value, State) when is_integer(Value) ->
    Store = aevm_eeevm_state:storage(State),
    %% Make sure value fits in 256 bits.
    Value256 = Value band ?MASK256,
    Store1 = storage_write(Address, Value256, Store),
    aevm_eeevm_state:set_storage(Store1, State).

%% -- Sophia API -------------------------------------------------------------

%% Map format
%%
%% Maps in contract state are saved in the store using the following format:
%%
%%  ?SOPHIA_STATE_MAPS_KEY => << <<MapId:256>> || MapId <- AllMapIds >>
%%  <<MapId:256>>          => ?MapInfo(RealId, RefCount, Size, Bin)
%%      where
%%          RealId   - indirection to allow inplace updates
%%          RefCount - number of times this map occurs in the map value of another map
%%          Size     - the size of the map (number of keys)
%%          Bin      - aeb_heap:to_binary({KeyType, ValType})
%%  <<RealId:256, KeyBin/binary>> => <<ValBin/binary>>
%%      KeyBin :: aeb_heap:binary_value()      -- cannot contain other maps
%%      ValBin :: aeb_heap:binary_heap_value() -- crucially: maps as map ids
%%
%% Garbage collection
%%
%%  In order to allow inplace update of modified store maps it's important to
%%  know when a specific map is no longer used. This is also important to
%%  prevent the store from growing indefinitely. When updating the contract
%%  state we get given the maps used in the new state except those appearing
%%  inside map values. To get the latter we have to traverse the new maps
%%  (`update_ref_counts`). Crucially, we don't have to look at maps already in
%%  the store except for the keys that get modified.
%%
%%  If a map is updated and the original is no longer in use it will be updated
%%  in place.

-define(MapInfo(RealId, RefCount, Size, Bin),
        <<(RealId):256, (RefCount):256, (Size):256, (Bin)/binary>>).

%% The argument should be a binary encoding a pair of a typerep and a value of that type.
-spec from_sophia_state(aect_contracts:version(), aeb_heap:binary_value()) ->
            {ok, aect_contracts:store()} | {error, term()}.
from_sophia_state(Version = #{vm := VM}, Data) when VM =< ?VM_AEVM_SOPHIA_3 ->
    aevm_eeevm_store_vm3:from_sophia_state(Version, Data);
from_sophia_state(_Version, Data) ->
    %% TODO: less encoding/decoding
    case aeb_heap:from_binary({tuple, [typerep]}, Data) of
        {ok, {Type}} ->
            %% Strip the type from the binary
            Data1 = second_component(Data),
            case aevm_data:binary_to_heap(Type, Data1, 0, 32) of
                {ok, StateValue} ->
                    TypeData  = aeb_heap:to_binary(Type),
                    Mem       = aeb_heap:heap_value_heap(StateValue),
                    Ptr       = aeb_heap:heap_value_pointer(StateValue),
                    StateData = <<Ptr:256, Mem/binary>>,
                    Maps      = aeb_heap:heap_value_maps(StateValue),
                    Store     = store_maps(Maps,
                                    store_put(?SOPHIA_STATE_KEY,      StateData,
                                    store_put(?SOPHIA_STATE_TYPE_KEY, TypeData,
                                    store_empty()))),
                    ?DEBUG_PRINT("Initial state:\n~s\n", [show_store(Store)]),
                    {ok, Store};
                E = {error, _} ->
                    E
            end;
        E = {error, _} ->
            E
    end.

%% Drop the first component (the typerep) from the initial state.
-spec second_component(aeb_heap:binary_value()) -> aeb_heap:binary_value().
second_component(<<Ptr:256, Heap/binary>> = Data) ->
    <<_:Ptr/unit:8, _:256, Snd:256, _/binary>> = Data,
    <<Snd:256, Heap/binary>>.

-spec set_sophia_state(aect_contracts:version(), aeb_heap:heap_value(), aect_contracts:store()) -> aect_contracts:store().
set_sophia_state(Version = #{vm := VM}, Value, Store) when VM =< ?VM_AEVM_SOPHIA_3 ->
    aevm_eeevm_store_vm3:set_sophia_state(Version, Value, Store);
set_sophia_state(_Version, Value, Store) ->
    Ptr = aeb_heap:heap_value_pointer(Value),
    Mem = aeb_heap:heap_value_heap(Value),
    Maps = aeb_heap:heap_value_maps(Value),
    store_maps(Maps, store_put(?SOPHIA_STATE_KEY, <<Ptr:256, Mem/binary>>, Store)).

-spec get_sophia_state(aect_contracts:store()) -> aeb_heap:heap_value().
get_sophia_state(Store) ->
    <<Ptr:256, Heap/binary>> = store_get(?SOPHIA_STATE_KEY, Store),
    MapKeys = all_map_ids(Store),
    Maps = maps:from_list(
        [ begin
              {KeyT, ValT} = map_types(MapId, Store),
              Size         = map_size(MapId, Store),
              {MapId, #pmap{ key_t = KeyT, val_t = ValT, size = Size, parent = none, data = stored }}
          end || MapId <- MapKeys ]),
    aeb_heap:heap_value(#maps{next_id = lists:max([-1 | MapKeys]) + 1, maps = Maps}, Ptr, Heap, 32).

-spec get_sophia_state_type(aect_contracts:store()) -> false | aeb_aevm_data:type().
get_sophia_state_type(Store) ->
    case store_get(?SOPHIA_STATE_TYPE_KEY, Store) of
        <<>> -> false;
        Bin  ->
            {ok, Type} = aeb_heap:from_binary(typerep, Bin),
            Type
    end.

-spec get_map_data(aevm_eeevm_maps:map_id(), aect_contracts:store()) -> #{binary() => binary()}.
get_map_data(MapId, Store) ->
    RealMapId = real_id(MapId, Store),
    store_subtree(RealMapId, Store).

-spec map_lookup(aevm_eeevm_maps:map_id(), binary(), aevm_eeevm_state:state()) -> binary() | false.
map_lookup(Id, Key, State) ->
    Store = aevm_eeevm_state:storage(State),
    get_value(Id, Key, Store).

-spec next_map_id(aect_contracts:store()) -> aevm_eeevm_maps:map_id().
next_map_id(#{?SOPHIA_STATE_MAPS_KEY := MapKeys}) ->
    1 + lists:max([-1 | [ Id || <<Id:256>> <= MapKeys ]]);
next_map_id(_) -> 0.

-ifdef(DEBUG).
show_store(Store0) ->
    Store = aect_contracts_store:subtree(<<>>, Store0),
    Show = fun(?SOPHIA_STATE_KEY)      -> "?SOPHIA_STATE_KEY";
              (?SOPHIA_STATE_TYPE_KEY) -> "?SOPHIA_STATE_TYPE_KEY";
              (?SOPHIA_STATE_MAPS_KEY) -> "?SOPHIA_STATE_MAPS_KEY";
              (<<Id:256>>)             -> integer_to_list(Id);
              (<<Id:256, Key/binary>>) -> io_lib:format("~p:~w", [Id, aevm_test_utils:dump_words(Key)])
           end,
    ShowAt  = fun(Type, Bin) ->
                    case aeb_heap:from_binary(Type, Bin) of
                        {ok, Val} -> io_lib:format("~p", [Val]);
                        Other -> io_lib:format("(~w : ~p => ~p)", [aevm_test_utils:dump_words(Bin), Type, Other])
                    end
              end,
    ShowVal =
            fun(?SOPHIA_STATE_TYPE_KEY, Val) -> ShowAt(typerep, Val);
               (<<_:256>>, ?MapInfo(RealId, RefCount, Size, Bin)) ->
                    io_lib:format("?MapInfo(~p, ~p, ~p, ~s)",
                                  [RealId, RefCount, Size, ShowAt({tuple, [typerep, typerep]}, Bin)]);
               (_, Val) -> io_lib:format("~p", [aevm_test_utils:dump_words(Val)])
            end,
    io_lib:format("~s\n", [[io_lib:format("  ~s =>\n    ~s\n",
                             [Show(Key), ShowVal(Key, Val)]) || {Key, Val} <- maps:to_list(Store)]]).
-endif.

%% -- Updating Sophia maps --

store_maps(Maps0, Store) ->
    Maps       = lists:keysort(1, maps:to_list(Maps0#maps.maps)),

    RefCounts  = get_ref_counts(Store),
    OldMapKeys = maps:keys(RefCounts),
    NewMapKeys = [ Id || {Id, _} <- Maps ],

    DeltaRefCounts = update_ref_counts(OldMapKeys, NewMapKeys, Maps, RefCounts, Store),

    Garbage    = [ G || G <- OldMapKeys -- NewMapKeys, 0 == maps:get(G, RefCounts, 0) + maps:get(G, DeltaRefCounts, 0) ],
    AllMapKeys = lists:usort(NewMapKeys ++ OldMapKeys) -- Garbage,

    Updates = compute_map_updates(Garbage, Maps),
    ?DEBUG_PRINT("Updates: ~p\n", [Updates]),

    Store1 = store_put(?SOPHIA_STATE_MAPS_KEY, << <<Id:256>> || Id <- AllMapKeys >>, Store),
    DeltaRefCounts1 = maps:filter(fun(Id, _) -> lists:member(Id, AllMapKeys) end, DeltaRefCounts),
    PerformUpdate = fun(Upd, S) -> perform_update(Upd, S) end,
    NewStore = add_ref_counts(DeltaRefCounts1, lists:foldl(PerformUpdate, Store1, Updates)),
    ?DEBUG_PRINT("NewStore:\n~s\n", [show_store(NewStore)]),
    NewStore.

perform_update({new_inplace, NewId, OldId, Size}, Store) ->
    OldKey   = <<OldId:256>>,
    NewKey   = <<NewId:256>>,
    ?MapInfo(RId, RefCount, _, Bin) = store_get(OldKey, Store),
    Entry = ?MapInfo(RId, RefCount, Size, Bin),
    %% Subtle: Don't remove the RealId entry because the mp trees requires
    %% there to be a value at any node that we want to get a subtree for. We
    %% need this for store_subtree.
    RealId   = real_id(OldId, Store),
    Store1   = if RealId /= OldId -> store_remove(OldKey, Store);
                  true            -> Store
               end,
    store_put(NewKey, Entry, Store1);
perform_update({insert, Id, ValType, Key, Val}, Store) ->
    RealId  = real_id(Id, Store),
    RealKey = <<RealId:256, Key/binary>>,
    Store1  = store_put(RealKey, Val, Store),
    %% We also need to subtract reference counts for the value that got
    %% overwritten (broken pre-lima).
    subtract_removed_ref_counts(store_get(RealKey, Store), ValType, Store1);
perform_update({delete, Id, ValType, Key}, Store) ->
    RealId  = real_id(Id, Store),
    RealKey = <<RealId:256, Key/binary>>,
    Store1  = store_remove(RealKey, Store),
    %% We also need to subtract reference counts for the value that got
    %% deleted (broken pre-lima).
    subtract_removed_ref_counts(store_get(RealKey, Store), ValType, Store1);
perform_update({new, Id, Map0}, Store0) ->
    %% Here we need to add reference counts for the copied entries (broken
    %% pre-lima).
    Map      = aevm_eeevm_maps:flatten_map(Store0, Id, Map0),
    Store    = add_copied_ref_counts(Map0, Map, Store0),
    RefCount = 0,   %% Set later
    Size     = Map0#pmap.size,
    Bin      = aeb_heap:to_binary({Map#pmap.key_t, Map#pmap.val_t}),
    Info = [{<<Id:256>>, ?MapInfo(Id, RefCount, Size, Bin)}],
    Data = [ {<<Id:256, Key/binary>>, Val} || {Key, Val} <- lists:keysort(1, maps:to_list(Map#pmap.data)) ],
    lists:foldl(fun({K, V}, S) -> store_put(K, V, S) end, Store, Info ++ Data);
perform_update({gc, Id}, Store) ->
    RealId = real_id(Id, Store),
    %% Remove map info entry. Also remove RealId which we kept around for mp
    %% tree reasons (see note at `new_inplace` case above), and all the data.
    ToRemove = [ <<Id:256>>, <<RealId:256>> |
               [ <<RealId:256, Key/binary>> || {Key, _} <- store_to_list(RealId, Store) ]],
    lists:foldl(fun store_remove/2, Store, ToRemove).

update_ref_counts(OldMapKeys, NewMapKeys, Maps, RefCounts, Store) ->
    DeltaCounts      = update_ref_counts1(Maps, #{}, Store),
    PotentialGarbage = OldMapKeys -- NewMapKeys,
    ref_count_garbage(PotentialGarbage, [], Maps, RefCounts, DeltaCounts, Store).

subtract_removed_ref_counts(<<>>, _, Store) -> Store;
subtract_removed_ref_counts(Val, ValType, Store) ->
    Used = aevm_data:used_maps(ValType, Val),
    lists:foldl(fun(Id, S) -> add_ref_count(Id, -1, S) end, Store, Used).

add_copied_ref_counts(#pmap{data = Updates}, #pmap{val_t = ValType, data = Data}, Store) ->
    %% Increase ref counts for all entries not overwritten by Updates
    Overwritten =
        case Updates of
            stored -> [];
            _      -> maps:keys(Updates)
        end,
    Used = [ Id || Val <- maps:values(maps:without(Overwritten, Data)),
                   Id  <- aevm_data:used_maps(ValType, Val) ],
    lists:foldl(fun(Id, S) -> add_ref_count(Id, 1, S) end,
                Store, Used).

ref_count_garbage(PotentialGarbage, ActualGarbage, Maps, RefCounts, DeltaCounts, Store) ->
    Garbage = [ G || G <- PotentialGarbage, 0 == maps:get(G, RefCounts, 0) + maps:get(G, DeltaCounts, 0) ],
    {ActualGarbage1, _} = do_inplace_assignment(Garbage, Maps),
    case ActualGarbage1 -- ActualGarbage of
        []         ->  DeltaCounts;    %% No new garbage
        NewGarbage ->
            DeltaCounts1 = lists:foldl(fun(Id, RfC) -> gc_ref_count(Id, RfC, Store) end,
                                       DeltaCounts, NewGarbage),
            ref_count_garbage(PotentialGarbage, ActualGarbage1, Maps, RefCounts, DeltaCounts1, Store)
    end.

gc_ref_count(Id, RefCounts, Store) ->
    {_, ValType} = map_types(Id, Store),
    case aevm_data:has_maps(ValType) of
        false -> RefCounts; %% Nothing to do if there are no nested maps
        true  ->
            UsedMaps =
                lists:append([ aevm_data:used_maps(ValType, Val)
                                || {_Key, Val} <- store_to_list(Id, Store) ]),
            lists:foldl(fun(Used, RfC) ->
                            maps:update_with(Used, fun(N) -> N - 1 end, -1, RfC)
                        end, RefCounts, UsedMaps)
    end.

update_ref_counts1([], RefCounts, _Store) ->
    RefCounts;
update_ref_counts1([{_Id, Map} | Maps], RefCounts, Store) ->
    case Map#pmap.data of
        stored ->
            %% Old map, no change to ref counts
            update_ref_counts1(Maps, RefCounts, Store);
        Data ->
            ValType = Map#pmap.val_t,
            DeltaCount =
                fun({_Key, Val}, Counts) ->
                        New =
                            case Val of
                                tombstone -> [];
                                _         -> aevm_data:used_maps(ValType, Val)
                            end,
                        %% We don't know if this map will be copied or
                        %% updated in-place so don't subtract the old
                        %% value yet (broken pre-lima).
                        Updates = [ {I, 1} || I <- New ],
                        lists:foldl(fun({I, Count}, RfC) ->
                                        maps:update_with(I, fun(N) -> N + Count end, Count, RfC)
                                    end, Counts, Updates)
                end,
            RefCounts1 = lists:foldl(DeltaCount, RefCounts, maps:to_list(Data)),
            update_ref_counts1(Maps, RefCounts1, Store)
    end.

do_inplace_assignment(Garbage, Maps) ->
    %% Which maps can be updated inplace? Only _one_ map can be an inplace
    %% update of a given parent (hence parent as key).
    InplaceAssignment =
        maps:from_list(
            [ {P, Id} || {Id, #pmap{ parent = P }} <- Maps,
                         P /= none, lists:member(P, Garbage) ]),
    Inplace    = [ E || E = {Id, #pmap{ parent = P }} <- Maps,
                        Id == maps:get(P, InplaceAssignment, none) ],

    %% Unused maps that are not used for inplace updates should be garbage
    %% collected.
    ActualGarbage = Garbage -- [ P || {_, #pmap{ parent = P }} <- Inplace ],

    {ActualGarbage, Inplace}.

compute_map_updates(Garbage, Maps0) ->

    %% Ignore maps that are already in the store.
    Maps = [ E || {_, #pmap{ data = D }} = E <- Maps0, D /= stored ],

    %% Whenever possible we should do inplace updates of maps
    {ActualGarbage, Inplace} = do_inplace_assignment(Garbage, Maps),

    %% Any map that can't be updated in place needs to be copied.
    Copy       = [ E || E = {Id, _} <- Maps, not lists:keymember(Id, 1, Inplace) ],

    lists:flatten(
        %% Copy first since inplace update might destroy data needed by the
        %% copy.
        [ [{new, Id, Map} || {Id, Map} <- Copy]
        , [ [{new_inplace, Id, Parent, Size},
            [ case Val of
                tombstone -> {delete, Id, ValType, Key};
                _         -> {insert, Id, ValType, Key, Val}
              end || {Key, Val} <- lists:keysort(1, maps:to_list(Data)) ]]
           || {Id, #pmap{ parent = Parent, val_t = ValType, data = Data, size = Size }} <- Inplace ]
        , [{gc, Id} || Id <- ActualGarbage ]
        ]).

%% -- Access functions for maps --

all_map_ids(Store) ->
    [ Id || <<Id:256>> <= store_get(?SOPHIA_STATE_MAPS_KEY, Store) ].

map_types(Id, Store) ->
    ?MapInfo(_, _, _, Bin) = store_get(<<Id:256>>, Store),
    {ok, Types} = aeb_heap:from_binary({tuple, [typerep, typerep]}, Bin),
    Types.

map_size(Id, Store) ->
    ?MapInfo(_, _, Size, _) = store_get(<<Id:256>>, Store),
    Size.

real_id(Id, Store) ->
    ?MapInfo(RealId, _, _, _) = store_get(<<Id:256>>, Store),
    RealId.

ref_count(Id, Store) ->
    ?MapInfo(_, RefCount, _, _) = store_get(<<Id:256>>, Store),
    RefCount.

add_ref_count(Id, Delta, Store) ->
    ?MapInfo(RealId, RefCount, Size, Bin) = store_get(<<Id:256>>, Store),
    store_put(<<Id:256>>, ?MapInfo(RealId, RefCount + Delta, Size, Bin), Store).

add_ref_counts(RefCounts, Store) ->
    maps:fold(fun(Id, Delta, St) ->
                    add_ref_count(Id, Delta, St)
                end, Store, RefCounts).

get_ref_counts(Store) ->
    maps:from_list([ {Id, ref_count(Id, Store)} || Id <- all_map_ids(Store) ]).

get_value(Id, Key, Store) ->
    RealId = real_id(Id, Store),
    case store_get(<<RealId:256, Key/binary>>, Store) of
        <<>> -> false;
        Val  -> Val
    end.

is_valid_key(#{vm := VM}, ?SOPHIA_STATE_KEY)      when ?IS_AEVM_SOPHIA(VM) -> true;
is_valid_key(#{vm := VM}, ?SOPHIA_STATE_TYPE_KEY) when ?IS_AEVM_SOPHIA(VM) -> true;
is_valid_key(#{vm := VM}, ?SOPHIA_STATE_MAPS_KEY) when ?IS_AEVM_SOPHIA(VM) -> true;
is_valid_key(#{vm := VM}, K) when ?IS_AEVM_SOPHIA(VM) -> is_binary(K) andalso byte_size(K) >= 32;
is_valid_key(#{vm := ?VM_AEVM_SOLIDITY_1}, K) -> is_binary_map_key(K).

%% -- Store API --
%%   To make it possible to change the representation of the store

store_empty() ->
    aect_contracts_store:new().

store_get(Key, Store) ->
    aect_contracts_store:get(Key, Store).

store_remove(Key, Store) ->
    aect_contracts_store:remove(Key, Store).

store_put(Key, Val, Store) ->
    aect_contracts_store:put(Key, Val, Store).

store_to_list(Id, Store) ->
    lists:keysort(1, maps:to_list(store_subtree(Id, Store))).

store_subtree(Id, Store) ->
    aect_contracts_store:subtree(<<Id:256>>, Store).

%%====================================================================
%% Internal functions
%%====================================================================

storage_read(Address, Mem) -> maps:get(Address, Mem, 0).

%% No alignment or size check. Don't use directly.
storage_write(Address,     0, Mem) -> maps:remove(Address, Mem);
storage_write(Address, Value, Mem) -> maps:put(Address, Value, Mem).

binary_to_integer_map(ChainStore) ->
    ToInt = fun(K, Val, Map) ->
                    Address = binary_to_integer_map_key(K),
                    case binary:decode_unsigned(Val) of
                        0 -> Map;
                        V -> Map#{ Address => V }
                    end
            end,
    maps:fold(ToInt, #{}, ChainStore).

integer_to_binary_map(Store) ->
    ToBin = fun(A, Val, Map) ->
                    Key = integer_to_binary_map_key(A),
                    case binary:encode_unsigned(Val) of
                        <<0>> -> Map;
                        V -> Map#{ Key => V}
                    end
            end,
    maps:fold(ToBin, #{}, Store).

binary_to_integer_map_key(K) -> binary:decode_unsigned(K).
integer_to_binary_map_key(K) -> binary:encode_unsigned(K).

is_binary_map_key(K) ->
    K =:= integer_to_binary_map_key(binary_to_integer_map_key(K)).
