%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Utility functions for EEEVM
%%% @end
%%% Created : 7 Oct 2017
%%%-------------------------------------------------------------------
-module(aevm_eeevm_utils).
-export([ bin_copy/3
        , get_area/3
        , write_area/3
        ]).

-include("aevm_eeevm.hrl").

-type mem() :: #{ mem_size := integer(), integer() => integer()}.
-type memmap() :: #{ integer() => integer() }.

-spec bin_copy(Pos::integer(), N::integer(), Bin::binary()) -> binary().
%%  @doc Return a zero extended copy of size N from position Pos of Bin.
bin_copy(Pos, N, Bin) ->
    Size = byte_size(Bin),
    BitPos = Pos * 8,
    if (Pos+N >= Size) andalso (Pos > Size) ->
	    ByteSize = N*8,
	    <<0:ByteSize>>;
       Pos+N >= Size ->
	    Extend = (N - (Size - Pos)) * 8,
	    <<_:BitPos, Copy:N/binary, _/binary>> = <<Bin/binary, 0:Extend>>,
	    Copy;
       true ->
	    <<_:BitPos, Copy:N/binary, _/binary>> = Bin,
	    Copy
    end.

-spec get_area(From::integer(), Size::integer(), memmap()) -> binary().
get_area(_From, 0, Mem) -> <<>>;
get_area(From, Size, Mem) ->
    To = From + Size - 1,
    list_to_binary([read_raw_byte(X, Mem) || X <- lists:seq(From, To)]).

%%====================================================================
%% Internal functions
%%====================================================================

read_raw_byte(Address, Mem) ->
    AlignedAddress = (Address bor ?ALIGN256) - ?ALIGN256,
    WordVal = maps:get(AlignedAddress , Mem, 0),
    ByteOffset = 31 - (Address - AlignedAddress),
    Byte = ((WordVal bsr (ByteOffset*8)) band 255),
    Byte.

-spec write_area(From::integer(), Bytes::binary(), Mem::mem()) -> mem().
write_area(From, Bytes, Mem) ->
    write_bytes(From, [B || <<B:8>> <= Bytes], Mem).

write_bytes(_, [], Mem) -> Mem;
write_bytes(Address, [Byte|Bytes], Mem) ->
    write_bytes(Address+1, Bytes, store8(Address, Byte, Mem)).


-spec store8(integer(), byte(), mem()) -> mem().
store8(Address, Value, Mem) when is_integer(Value) ->
    Byte = Value band 255,
    AlignedAddress = (Address bor ?ALIGN256) - ?ALIGN256,
    WordVal = maps:get(AlignedAddress, Mem, 0),
    BitOffset = (Address - AlignedAddress) * 8,
    <<Pre:BitOffset, _:8, Post/bits>> = <<WordVal:256>>,
    <<NewWord:256>> = <<Pre:BitOffset, Byte:8, Post/bits>>,
    Mem1 = write(AlignedAddress, NewWord, Mem),
    Mem1.

%% No alignment or size check. Don't use directly.
write(Address,     0, Mem) -> extend(Address, maps:remove(Address, Mem));
write(Address, Value, Mem) -> extend(Address, maps:put(Address, Value, Mem)).

extend(AlignedAddress, Mem) when is_integer(AlignedAddress) ->
    NewSize = (AlignedAddress bsr 5) + 1,
    case maps:get(mem_size, Mem, undefined) of
        undefined -> maps:put(mem_size, NewSize, Mem);
        Size when NewSize > Size -> maps:put(mem_size, NewSize, Mem);
        _ -> Mem
    end.
