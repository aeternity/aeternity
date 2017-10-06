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
        , size_in_words/1
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
    %% Make sure value fits in 256 bits.
    Value256 = Value band ?MASK256,
    Mem = aevm_eeevm_state:mem(State),
    Mem1 = case (Address band ?ALIGN256) of
               0 -> %% 256-bits-word aligned
                   write(Address, Value256, Mem);
               _ -> %% Unaligned
                   write_unaligned(Address, Value256, Mem)
           end,
    aevm_eeevm_state:set_mem(Mem1, State).

store8(Address, Value, State) when is_integer(Value) ->
    Mem = aevm_eeevm_state:mem(State),
    Byte = Value band 255,
    AlignedAddress = (Address bor ?ALIGN256) - ?ALIGN256,
    WordVal = maps:get(AlignedAddress, Mem, 0),
    BitOffset = (Address - AlignedAddress) * 8,
    <<Pre:BitOffset, _:8, Post/bits>> = <<WordVal:256>>,
    <<NewWord:256>> = <<Pre:BitOffset, Byte:8, Post/bits>>,
    Mem1 = write(AlignedAddress, NewWord, Mem),
    aevm_eeevm_state:set_mem(Mem1, State).

size_in_words(State) ->
    Mem = aevm_eevm_state:mem(State),
    case maps:get(highest_aligned_address, Mem, undefined) of
        undefined -> 0;
        Highest -> (Highest bsr 5) + 1
    end.

%%====================================================================
%% Internal functions
%%====================================================================

extend(AlignedAddress, Mem) when is_integer(AlignedAddress) ->
    Highest = maps:get(highest_aligned_address, Mem, undefined),
    case (AlignedAddress > Highest) orelse (Highest =:= undefined) of
        true -> maps:put(highest_aligned_address, AlignedAddress, Mem);
        false -> Mem
    end.

%% No alignment or size check. Don't use directly.
write(Address,     0, Mem) -> maps:remove(Address, Mem);
write(Address, Value, Mem) -> extend(Address, maps:put(Address, Value, Mem)).

write_unaligned(Address, Value256, Mem) ->
    LowAligned = (Address bor ?ALIGN256) - ?ALIGN256,
    HighAligned = LowAligned + 32,
    OldLow  = read(LowAligned, 32, Mem),
    OldHigh = read(HighAligned, 32, Mem),
    BitOffsetLow = (Address - LowAligned)*8,
    BitOffsetHigh = 256 - BitOffsetLow,
    <<Pre:BitOffsetLow, _/bits>> = <<OldLow:256>>,
    <<_:BitOffsetHigh, Post/bits>> = <<OldHigh:256>>,
    <<NewLow:256, NewHigh:256>> = <<Pre:BitOffsetLow, Value256:256, Post/bits>>,
    Mem1 = write(HighAligned, NewHigh, Mem),
    write(LowAligned, NewLow, Mem1).

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
