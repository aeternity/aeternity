%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Handle memory
%%% @end
%%% Created : 2 Oct 2017
%%%-------------------------------------------------------------------

-module(aevm_eeevm_memory).

-export([ get_area/3
        , load/2
        , store/3
        , store8/3
        ]).

-include("aevm_eeevm.hrl").
-include("aevm_gas.hrl").

%%====================================================================
%% API
%%====================================================================

get_area(From, To, State) ->
  Mem = aevm_eeevm_state:mem(State),
  list_to_binary([read(X, 1, Mem) || X <- lists:seq(From, To)]).

load(Address, State) ->
    Mem = aevm_eeevm_state:mem(State),
    Value = read(Address, 32, Mem),
    Value.

store(Address, Value, State) when is_integer(Value) ->
    case (Address band ?ALIGN256) of
	%% 256-bits-word aligned
	0 -> Mem = aevm_eeevm_state:mem(State),
	     %% Make sure value fits in 256 bits.
	     Value256 = Value band ?MASK256,
	     Mem1 = write(Address, Value256, Mem),
	     aevm_eeevm_state:set_mem(Mem1, State);
	_ -> %% Unligned
	    error({unaligned_sstore_not_handled, Address, Value})
    end.

store8(Address, Value, State) when is_integer(Value) ->
    Mem = aevm_eeevm_state:mem(State),
    Byte = Value band 255,
    AlignedAddress = (Address bor ?ALIGN256) - ?ALIGN256,
    WordVal = maps:get(AlignedAddress , Mem, 0),
    ByteOffset = Address - AlignedAddress,
    NewWord = (WordVal band (bnot (255 bsl ByteOffset))) bor (Byte bsl ByteOffset),
    Mem1 = write(AlignedAddress, NewWord, Mem),
    aevm_eeevm_state:set_mem(Mem1, State).


%%====================================================================
%% Internal functions
%%====================================================================

%% No alignment or size check. Don't use directly.
write(Address,     0, Mem) -> maps:remove(Address, Mem);
write(Address, Value, Mem) -> maps:put(Address, Value, Mem).

read(Address, 1, Mem) ->
    AlignedAddress = (Address bor ?ALIGN256) - ?ALIGN256,
    WordVal = maps:get(AlignedAddress , Mem, 0),
    ByteOffset = 31 - (Address - AlignedAddress),
    Byte = ((WordVal bsr (ByteOffset*8)) band 255),
    Byte;
read(Address, 32, Mem) ->
    AlignedAddress = (Address bor ?ALIGN256) - ?ALIGN256,
    case AlignedAddress =:= Address of
	true -> %% Aligned.
	    maps:get(AlignedAddress , Mem, 0);
	false -> %%
	    error(unaligned_mem_read_not_implemented)
    end.
