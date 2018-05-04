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
        ]).

-include("aevm_eeevm.hrl").

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

%%====================================================================
%% Internal functions
%%====================================================================

storage_read(Address, Mem) -> maps:get(Address, Mem, 0).

%% No alignment or size check. Don't use directly.
storage_write(Address,     0, Mem) -> maps:remove(Address, Mem);
storage_write(Address, Value, Mem) -> maps:put(Address, Value, Mem).

binary_to_integer_map(ChainStore) ->
    ToInt = fun(K, Val, Map) ->
                    Address = binary:decode_unsigned(K),
                    case binary:decode_unsigned(Val) of
                        0 -> Map;
                        V -> Map#{ Address => V }
                    end
            end,
    maps:fold(ToInt, #{}, ChainStore).

integer_to_binary_map(Store) ->
    ToBin = fun(A, Val, Map) ->
                    Key = binary:encode_unsigned(A),
                    case binary:encode_unsigned(Val) of
                        0 -> Map;
                        V -> Map#{ Key => V}
                    end
            end,
    maps:fold(ToBin, #{}, Store).

