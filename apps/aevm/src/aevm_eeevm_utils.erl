%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Utility functions for EEEVM
%%% @end
%%% Created : 7 Oct 2017
%%%-------------------------------------------------------------------
-module(aevm_eeevm_utils).
-export([bin_copy/3]).


%%  @spec bin_copy(Pos::integer(), N::integer(), Bin::binary()) -> binary()
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

    
