-module(aeso_data).

-export([to_binary/1,binary_to_words/1,from_binary/2]).

-include("aeso_icode.hrl").

%% Encode the data as a heap fragment starting at address 64. The first word is
%% a pointer into the heap fragment. The reason we store it at address 64 is to
%% leave room for the state pointer at address 0.
to_binary(Data) ->
    {Address, Memory} = to_binary(Data, 64),
    <<Address:256, Memory/binary>>.

%% Allocate the data in memory, from the given address.  Return a pair
%% of memory contents from that address and the value representing the
%% data.
to_binary(Data,_Address) when is_integer(Data) ->
    {Data,<<>>};
to_binary(Data, Address) when is_binary(Data) ->
    %% a string
    Words = binary_to_words(Data),
    {Address,<<(size(Data)):256, << <<W:256>> || W <- Words>>/binary>>};
to_binary(none, Address) -> to_binary([], Address);
to_binary({some, Value}, Address) -> to_binary({Value}, Address);
to_binary(Data, Address) when is_tuple(Data) ->
    {Elems,Memory} = to_binaries(tuple_to_list(Data),Address+32*size(Data)),
    ElemsBin = << <<W:256>> || W <- Elems>>,
    {Address,<< ElemsBin/binary, Memory/binary >>};
to_binary([],_Address) ->
    <<Nil:256>> = <<(-1):256>>,
    {Nil,<<>>};
to_binary([H|T],Address) ->
    to_binary({H,T},Address).


to_binaries([],_Address) ->
    {[],<<>>};
to_binaries([H|T],Address) ->
    {HRep,HMem} = to_binary(H,Address),
    {TRep,TMem} = to_binaries(T,Address+size(HMem)),
    {[HRep|TRep],<<HMem/binary, TMem/binary>>}.

binary_to_words(<<>>) ->
    [];
binary_to_words(<<N:256,Bin/binary>>) ->
    [N|binary_to_words(Bin)];
binary_to_words(Bin) ->
    binary_to_words(<<Bin/binary,0>>).

%% Interpret a return value (a binary) using a type rep.

from_binary(T,Heap= <<V:256,_/binary>>) ->
    from_binary(T,Heap,V).

from_binary(word,_,V) ->
    V;
from_binary(signed_word, _, V) ->
    <<N:256/signed>> = <<V:256>>,
    N;
from_binary(string,Heap,V) ->
    StringSize = heap_word(Heap,V),
    BitAddr = 8*(V+32),
    <<_:BitAddr,Bytes:StringSize/binary,_/binary>> = Heap,
    Bytes;
from_binary({tuple,Cpts},Heap,V) ->
    list_to_tuple([from_binary(T,Heap,heap_word(Heap,V+32*I))
		   || {T,I} <- lists:zip(Cpts,lists:seq(0,length(Cpts)-1))]);
from_binary({list,Elem},Heap,V) ->
    <<Nil:256>> = <<(-1):256>>,
    if V==Nil ->
	    [];
       true ->
	    {H,T} = from_binary({tuple,[Elem,{list,Elem}]},Heap,V),
	    [H|T]
    end;
from_binary({option, A}, Heap, V) ->
    <<None:256>> = <<(-1):256>>,
    if V == None -> none;
       true      ->
         {Elem} = from_binary({tuple, [A]}, Heap, V),
         {some, Elem}
    end;
from_binary(typerep, Heap, V) ->
    Tag = from_binary(word, Heap, heap_word(Heap, V)),
    Arg = fun(T) -> from_binary(T, Heap, heap_word(Heap, V + 32)) end,
    case Tag of
        ?TYPEREP_WORD_TAG   -> word;
        ?TYPEREP_STRING_TAG -> string;
        ?TYPEREP_LIST_TAG   -> {list,   Arg(typerep)};
        ?TYPEREP_OPTION_TAG -> {option, Arg(typerep)};
        ?TYPEREP_TUPLE_TAG  -> {tuple,  Arg({list, typerep})}
    end.

heap_word(Heap,Addr) ->
    BitSize = 8*Addr,
    <<_:BitSize,W:256,_/binary>> = Heap,
    W.
