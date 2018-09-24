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
        ]).

-include("aevm_eeevm.hrl").
-include_lib("aecontract/src/aecontract.hrl").

-define(SOPHIA_STATE_KEY,      <<0>>).
-define(SOPHIA_STATE_TYPE_KEY, <<1>>).

%%====================================================================
%% API
%%====================================================================

-spec init(aect_contracts:store(), aevm_eeevm_state:state()) -> aevm_eeevm_state:state().
init(Store, State) -> State#{ storage => binary_to_integer_map(Store) }.

-spec to_binary(aevm_eeevm_state:state()) -> aect_contracts:store().
to_binary(#{ storage := Storage }) -> integer_to_binary_map(Storage).

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

%% The argument should be a binary encoding a pair of a typerep and a value of that type.
-spec from_sophia_state(binary()) -> aect_contracts:store().
from_sophia_state(Data) ->
    %% TODO: less encoding/decoding
    {ok, {Type}}     = aeso_data:from_binary({tuple, [typerep]},    Data),
    {ok, {_, Value}} = aeso_data:from_binary({tuple, [word, Type]}, Data),
    StateData          = aeso_data:to_binary(Value),
    TypeData           = aeso_data:to_binary(Type),
    #{ ?SOPHIA_STATE_KEY      => StateData,
       ?SOPHIA_STATE_TYPE_KEY => TypeData }.

-spec set_sophia_state(binary(), aect_contracts:store()) -> aect_contracts:store().
set_sophia_state(Data, Store) -> Store#{?SOPHIA_STATE_KEY => Data}.

-spec get_sophia_state(aect_contracts:store()) -> binary().
get_sophia_state(Store) -> maps:get(?SOPHIA_STATE_KEY, Store, <<>>).

-spec get_sophia_state_type(aect_contracts:store()) -> false | binary().
get_sophia_state_type(Store) -> maps:get(?SOPHIA_STATE_TYPE_KEY, Store, false).

is_valid_key(?AEVM_01_Sophia_01, ?SOPHIA_STATE_KEY) -> true;
is_valid_key(?AEVM_01_Sophia_01, ?SOPHIA_STATE_TYPE_KEY) -> true;
is_valid_key(?AEVM_01_Sophia_01, _) -> false;
is_valid_key(?AEVM_01_Solidity_01, K) -> is_binary_map_key(K).

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
