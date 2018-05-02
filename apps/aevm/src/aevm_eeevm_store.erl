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

-spec init(binary(), aevm_eeevm_state:state()) -> aevm_eeevm_state:state().
init(Store, State) ->
    State#{ storage =>
		write_words([ B || <<B:256>> <= Store ], 0, #{})
	  }.
-spec to_binary(aevm_eeevm_state:state()) -> binary().
to_binary(#{ storage := Storage }) -> << <<W:256>>  || W <- to_list(Storage)>>.

-spec load(integer(), aevm_eeevm_state:state()) -> byte().
load(Address, State) ->
    Store = aevm_eeevm_state:storage(State),
    Value = storage_read(Address, Store),
    Value.

-spec store(integer(), byte(), aevm_eeevm_state:state()) -> aevm_eeevm_state:state().
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

write_words([W | Rest], Address, Mem) ->
    write_words(Rest, Address + 32, storage_write(Address, W, Mem));
write_words([], _, Mem) -> Mem.

to_list(Mem) -> to_list(0, lists:sort(maps:to_list(Mem))).

to_list(_, [])              -> [];
to_list(N, [{N, W} | Rest]) -> [W || to_list(N+32, Rest)];
to_list(N, Rest)            -> [0 || to_list(N+32, Rest)].
