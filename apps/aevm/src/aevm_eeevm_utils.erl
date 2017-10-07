-module(aevm_eeevm_utils).
%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Utility functions for EEEVM
%%% @end
%%% Created : 7 Oct 2017
%%%-------------------------------------------------------------------

-export([bin_copy/3]).


%%  @spec bin_copy(Pos::integer(), N::integer(), Bin::binary()) -> binary()
%%  Return a zero extended copy of size N from position Pos of Bin.
bin_copy(Pos, N, Bin) ->
    Size = byte_size(Bin),
    BitPos = Pos * 8,
    ZeroExtendedBinary =
	if Pos+N >= Size ->
		Extend = (N - (Size - Pos)) * 8,
		<<Bin/binary, 0:Extend>>;
	   true ->
		Bin
	end,
    <<_:BitPos, Copy:N/binary, _/binary>> = ZeroExtendedBinary,
    Copy.
    
