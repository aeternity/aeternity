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
        , write_area/3
        ]).

-include("aevm_eeevm.hrl").

%%====================================================================
%% API
%%====================================================================

%% get_area(From, To, State) when From =:= To ->
%%     {<<>>, State};
get_area(_From, 0, State) ->
    {<<>>, State};
get_area(From, Size, State) ->
    Mem = aevm_eeevm_state:mem(State),
    To = From + Size - 1,
    {_, Mem1} = read(To, 1, Mem),
    State1 = aevm_eeevm_state:set_mem(Mem1, State),
    Res = list_to_binary([read_raw(X, 1, Mem1) || X <- lists:seq(From, To)]),
    {Res, State1}.

write_area(From, Bytes, State) ->
    write_bytes(From, [B || <<B:8>> <= Bytes], State).

write_bytes(_, [], State) -> State;
write_bytes(Address, [Byte|Bytes], State) ->
    write_bytes(Address+1, Bytes, store8(Address, Byte, State)).

load(Address, State) ->
    Mem = aevm_eeevm_state:mem(State),
    {Res, Mem1} = read(Address, 32, Mem),
    {Res, aevm_eeevm_state:set_mem(Mem1, State)}.

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
    Mem = aevm_eeevm_state:mem(State),
    maps:get(mem_size, Mem, 0).

%%====================================================================
%% Internal functions
%%====================================================================

extend(AlignedAddress, Mem) when is_integer(AlignedAddress) ->
    NewSize = (AlignedAddress bsr 5) + 1,
    case maps:get(mem_size, Mem, undefined) of
        undefined -> maps:put(mem_size, NewSize, Mem);
        Size when NewSize > Size -> maps:put(mem_size, NewSize, Mem);
        _ -> Mem
    end.

%% No alignment or size check. Don't use directly.
write(Address,     0, Mem) -> extend(Address, maps:remove(Address, Mem));
write(Address, Value, Mem) -> extend(Address, maps:put(Address, Value, Mem)).

write_unaligned(Address, Value256, Mem) ->
    LowAligned = (Address bor ?ALIGN256) - ?ALIGN256,
    HighAligned = LowAligned + 32,
    {OldLow, _} = read(LowAligned, 32, Mem),   %% We can skip the extend since
    {OldHigh, _} = read(HighAligned, 32, Mem), %% we will write in the end
    BitOffsetLow = (Address - LowAligned)*8,
    BitOffsetHigh = 256 - BitOffsetLow,
    <<Pre:BitOffsetLow, _/bits>> = <<OldLow:256>>,
    <<_:BitOffsetLow, Post:BitOffsetHigh>> = <<OldHigh:256>>,
    <<NewLow:256, NewHigh:256>> =
        <<Pre:BitOffsetLow, Value256:256, Post:BitOffsetHigh>>,
    Mem1 = write(HighAligned, NewHigh, Mem),
    write(LowAligned, NewLow, Mem1).

read_raw(Address, N, Mem) ->
    {Res, _} = read(Address, N, Mem),
    Res.

read(Address, 1, Mem) ->
    AlignedAddress = (Address bor ?ALIGN256) - ?ALIGN256,
    WordVal = maps:get(AlignedAddress , Mem, 0),
    ByteOffset = 31 - (Address - AlignedAddress),
    Byte = ((WordVal bsr (ByteOffset*8)) band 255),
    {Byte, extend(AlignedAddress, Mem)};
read(Address, 32, Mem) ->
    AlignedAddress = (Address bor ?ALIGN256) - ?ALIGN256,
    case AlignedAddress =:= Address of
        true -> %% Aligned.
            {maps:get(AlignedAddress , Mem, 0), extend(AlignedAddress, Mem)};
        false -> %%
            Lo = maps:get(AlignedAddress , Mem, 0),
            Hi = maps:get(AlignedAddress+32 , Mem, 0),
            Offset = (Address - AlignedAddress)*8,
            HiBitsSize = ?WORDSIZE-Offset,
            <<_:Offset, HiBits:HiBitsSize>> = <<Lo:?WORDSIZE>>,
            LoBitsSize = Offset,
            <<LoBits:LoBitsSize, _:HiBitsSize>> = <<Hi:?WORDSIZE>>,
            <<Word:256>> = <<HiBits:HiBitsSize,LoBits:LoBitsSize>>,
            {Word, extend(AlignedAddress+32, Mem)}
    end.
