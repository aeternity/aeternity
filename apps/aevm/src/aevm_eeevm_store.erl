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
        , from_sophia_state/1
        , set_sophia_state/2
        , is_valid_key/2
        , get_map_data/2
        , map_lookup/3
        , next_map_id/1
        ]).

-include("aevm_eeevm.hrl").
-include_lib("aecontract/src/aecontract.hrl").
-include_lib("aesophia/src/aeso_data.hrl").

-define(SOPHIA_STATE_KEY,      <<0>>).
-define(SOPHIA_STATE_TYPE_KEY, <<1>>).
-define(SOPHIA_STATE_MAPS_KEY, <<2>>).

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
%%          Bin      - aeso_data:to_binary({KeyType, ValType})
%%  <<RealId:256, KeyBin/binary>> => <<ValBin/binary>>
%%      KeyBin :: aeso_data:binary_value()      -- cannot contain other maps
%%      ValBin :: aeso_data:binary_heap_value() -- crucially: maps as map ids
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
-spec from_sophia_state(aeso_data:binary_value()) -> aect_contracts:store().
from_sophia_state(Data) ->
    %% TODO: less encoding/decoding
    {ok, {Type}}    = aeso_data:from_binary({tuple, [typerep]}, Data),
    %% Strip the type from the binary
    Data1 = second_component(Data),
    {ok, StateValue} = aeso_data:binary_to_heap(Type, Data1, 0, 32),
    TypeData  = aeso_data:to_binary(Type),
    Mem       = aeso_data:heap_value_heap(StateValue),
    Ptr       = aeso_data:heap_value_pointer(StateValue),
    StateData = <<Ptr:256, Mem/binary>>,
    Maps      = aeso_data:heap_value_maps(StateValue),
    Store     = store_maps(Maps,
                    store_put(?SOPHIA_STATE_KEY,      StateData,
                    store_put(?SOPHIA_STATE_TYPE_KEY, TypeData,
                    store_empty()))),
    %% io:format("Initial state:\n~s\n", [show_store(Store)]),
    Store.

%% Drop the first component (the typerep) from the initial state.
-spec second_component(aeso_data:binary_value()) -> aeso_data:binary_value().
second_component(<<Ptr:256, Heap/binary>> = Data) ->
    <<_:Ptr/unit:8, _:256, Snd:256, _/binary>> = Data,
    <<Snd:256, Heap/binary>>.

-spec set_sophia_state(aeso_data:heap_value(), aect_contracts:store()) -> aect_contracts:store().
set_sophia_state(Value, Store) ->
    Ptr = aeso_data:heap_value_pointer(Value),
    Mem = aeso_data:heap_value_heap(Value),
    Maps = aeso_data:heap_value_maps(Value),
    store_maps(Maps, store_put(?SOPHIA_STATE_KEY, <<Ptr:256, Mem/binary>>, Store)).

-spec get_sophia_state(aect_contracts:store()) -> aeso_data:heap_value().
get_sophia_state(Store) ->
    <<Ptr:256, Heap/binary>> = store_get(?SOPHIA_STATE_KEY, Store),
    MapKeys = all_map_ids(Store),
    Maps = maps:from_list(
        [ begin
              {KeyT, ValT} = map_types(MapId, Store),
              Size         = map_size(MapId, Store),
              {MapId, #pmap{ key_t = KeyT, val_t = ValT, size = Size, parent = none, data = stored }}
          end || MapId <- MapKeys ]),
    aeso_data:heap_value(#maps{next_id = lists:max([-1 | MapKeys]) + 1, maps = Maps}, Ptr, Heap, 32).

-spec get_sophia_state_type(aect_contracts:store()) -> false | aeso_sophia:type().
get_sophia_state_type(Store) ->
    case store_get(?SOPHIA_STATE_TYPE_KEY, Store) of
        <<>> -> false;
        Bin  ->
            {ok, Type} = aeso_data:from_binary(typerep, Bin),
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

%% show_store(Store) ->
%%     Show = fun(?SOPHIA_STATE_KEY)      -> "?SOPHIA_STATE_KEY";
%%               (?SOPHIA_STATE_TYPE_KEY) -> "?SOPHIA_STATE_TYPE_KEY";
%%               (?SOPHIA_STATE_MAPS_KEY) -> "?SOPHIA_STATE_MAPS_KEY";
%%               (<<Id:256>>)             -> integer_to_list(Id);
%%               (<<Id:256, Key/binary>>) -> io_lib:format("~p:~p", [Id, aeso_test_utils:dump_words(Key)])
%%            end,
%%     io_lib:format("~s\n", [[io_lib:format("  ~s =>\n    ~p\n",
%%                              [Show(Key), aeso_test_utils:dump_words(Val)]) || {Key, Val} <- maps:to_list(Store)]]).

%% -- Updating Sophia maps --

store_maps(Maps0, Store) ->
    Maps       = maps:to_list(Maps0#maps.maps),

    RefCounts  = get_ref_counts(Store),
    OldMapKeys = maps:keys(RefCounts),
    NewMapKeys = [ Id || {Id, _} <- Maps ],

    NewRefCounts = update_ref_counts(OldMapKeys, NewMapKeys, Maps, RefCounts, Store),

    Garbage    = [ G || G <- OldMapKeys -- NewMapKeys, 0 == maps:get(G, NewRefCounts, 0) ],
    AllMapKeys = lists:usort(NewMapKeys ++ OldMapKeys) -- Garbage,

    Updates = compute_map_updates(Garbage, Maps),

    Store1 = store_put(?SOPHIA_STATE_MAPS_KEY, << <<Id:256>> || Id <- AllMapKeys >>, Store),
    NewRefCounts1 = maps:filter(fun(Id, _) -> lists:member(Id, AllMapKeys) end, NewRefCounts),
    NewStore = set_ref_counts(NewRefCounts1, lists:foldl(fun perform_update/2, Store1, Updates)),
    %% io:format("NewStore:\n~s\n", [show_store(NewStore)]),
    NewStore.

perform_update({new_inplace, NewId, OldId}, Store) ->
    OldKey   = <<OldId:256>>,
    NewKey   = <<NewId:256>>,
    OldEntry = store_get(OldKey, Store),
    %% Subtle: Don't remove the RealId entry because the mp trees requires
    %% there to be a value at any node that we want to get a subtree for. We
    %% need this for store_subtree.
    RealId   = real_id(OldId, Store),
    Store1   = if RealId /= OldId -> store_remove(OldKey, Store);
                  true            -> Store
               end,
    store_put(NewKey, OldEntry, Store1);
perform_update({insert, Id, Key, Val}, Store) ->
    RealId = real_id(Id, Store),
    store_put(<<RealId:256, Key/binary>>, Val, Store);
perform_update({delete, Id, Key}, Store) ->
    RealId = real_id(Id, Store),
    store_remove(<<RealId:256, Key/binary>>, Store);
perform_update({new, Id, Map0}, Store) ->
    Map = aevm_eeevm_maps:flatten_map(Store, Id, Map0),
    RefCount = 0,   %% Set later
    Size     = Map0#pmap.size,
    Bin      = aeso_data:to_binary({Map#pmap.key_t, Map#pmap.val_t}),
    Info = [{<<Id:256>>, ?MapInfo(Id, RefCount, Size, Bin)}],
    Data = [ {<<Id:256, Key/binary>>, Val} || {Key, Val} <- maps:to_list(Map#pmap.data) ],
    lists:foldl(fun({K, V}, S) -> store_put(K, V, S) end, Store, Info ++ Data);
perform_update({gc, Id}, Store) ->
    RealId = real_id(Id, Store),
    %% Remove map info entry. Also remove RealId which we kept around for mp
    %% tree reasons (see note at `new_inplace` case above), and all the data.
    ToRemove = [ <<Id:256>>, <<RealId:256>> |
               [ <<RealId:256, Key/binary>> || {Key, _} <- store_to_list(RealId, Store) ]],
    lists:foldl(fun store_remove/2, Store, ToRemove).

update_ref_counts(OldMapKeys, NewMapKeys, Maps, RefCounts, Store) ->
    RefCounts1       = update_ref_counts1(Maps, RefCounts, Store),
    PotentialGarbage = OldMapKeys -- NewMapKeys,
    ref_count_garbage(PotentialGarbage, [], Maps, RefCounts1, Store).

ref_count_garbage(PotentialGarbage, ActualGarbage, Maps, RefCounts, Store) ->
    Garbage = [ G || G <- PotentialGarbage, 0 == maps:get(G, RefCounts, 0) ],
    {ActualGarbage1, _} = do_inplace_assignment(Garbage, Maps),
    case ActualGarbage1 -- ActualGarbage of
        []         ->  RefCounts;    %% No new garbage
        NewGarbage ->
            RefCounts1 = lists:foldl(fun(Id, RfC) -> gc_ref_count(Id, RfC, Store) end,
                                     RefCounts, NewGarbage),
            ref_count_garbage(PotentialGarbage, ActualGarbage1, Maps, RefCounts1, Store)
    end.

gc_ref_count(Id, RefCounts, Store) ->
    {_, ValType} = map_types(Id, Store),
    case aeso_data:has_maps(ValType) of
        false -> RefCounts; %% Nothing to do if there are no nested maps
        true  ->
            UsedMaps =
                lists:append([ aeso_data:used_maps(ValType, Val)
                                || {_Key, Val} <- store_to_list(Id, Store) ]),
            lists:foldl(fun(Used, RfC) ->
                            maps:update_with(Used, fun(N) -> N - 1 end, RfC)
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
                fun({Key, Val}, Counts) ->
                        New =
                            case Val of
                                tombstone -> [];
                                _         -> aeso_data:used_maps(ValType, Val)
                            end,
                        Old =
                            case Map#pmap.parent of
                                none -> [];
                                PId  ->
                                    case get_value(PId, Key, Store) of
                                        false  -> [];
                                        OldVal -> aeso_data:used_maps(ValType, OldVal)
                                    end
                            end,
                        %% Subtract old from new
                        Updates = [ {I, 1} || I <- New ] ++ [ {I, -1} || I <- Old ],
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
        , [ [{new_inplace, Id, Parent},
            [ case Val of
                tombstone -> {delete, Id, Key};
                _         -> {insert, Id, Key, Val}
              end || {Key, Val} <- maps:to_list(Data) ]]
           || {Id, #pmap{ parent = Parent, data = Data }} <- Inplace ]
        , [{gc, Id} || Id <- ActualGarbage ]
        ]).

%% -- Access functions for maps --

all_map_ids(Store) ->
    [ Id || <<Id:256>> <= store_get(?SOPHIA_STATE_MAPS_KEY, Store) ].

map_types(Id, Store) ->
    ?MapInfo(_, _, _, Bin) = store_get(<<Id:256>>, Store),
    {ok, Types} = aeso_data:from_binary({tuple, [typerep, typerep]}, Bin),
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

set_ref_count(Id, RefCount, Store) ->
    ?MapInfo(RealId, _, Size, Bin) = store_get(<<Id:256>>, Store),
    store_put(<<Id:256>>, ?MapInfo(RealId, RefCount, Size, Bin), Store).

set_ref_counts(RefCounts, Store) ->
    lists:foldl(fun({Id, RefCount}, St) ->
                    set_ref_count(Id, RefCount, St)
                end, Store, maps:to_list(RefCounts)).

get_ref_counts(Store) ->
    maps:from_list([ {Id, ref_count(Id, Store)} || Id <- all_map_ids(Store) ]).

get_value(Id, Key, Store) ->
    RealId = real_id(Id, Store),
    case store_get(<<RealId:256, Key/binary>>, Store) of
        <<>> -> false;
        Val  -> Val
    end.

is_valid_key(VM, ?SOPHIA_STATE_KEY)      when ?IS_AEVM_SOPHIA(VM) -> true;
is_valid_key(VM, ?SOPHIA_STATE_TYPE_KEY) when ?IS_AEVM_SOPHIA(VM) -> true;
is_valid_key(VM, ?SOPHIA_STATE_MAPS_KEY) when ?IS_AEVM_SOPHIA(VM) -> true;
is_valid_key(VM, K) when ?IS_AEVM_SOPHIA(VM) -> is_binary(K) andalso byte_size(K) >= 32;
is_valid_key(?AEVM_01_Solidity_01, K) -> is_binary_map_key(K).

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
    maps:to_list(store_subtree(Id, Store)).

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
