%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%
%%% @end
%%%=============================================================================

-module(aec_sha256).

-export([hash/1,
         binary_to_scientific/1,
         scientific_to_integer/1,
         integer_to_scientific/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("sha256.hrl").


-type hashable() :: term().

%% Integers represented in scientific notation:
%%   256 * <base-2 exponent> + the most significant byte (i.e., the significand,
%%   see https://en.wikipedia.org/wiki/Significand)
-type sci_int() :: integer().

-export_type([hashable/0,
              sci_int/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Calculate the SHA256 hash value of a binary or an erlang term
%%------------------------------------------------------------------------------
-spec hash(hashable()) -> binary().
hash(Data) when is_binary(Data) ->
    <<Hash:?HASH_BITS, _/bitstring>> = crypto:hash(sha256, Data),
    <<Hash:?HASH_BITS>>;
hash(Term) ->
    hash(term_to_binary(Term)).

%%------------------------------------------------------------------------------
%% Conversion functions for scientific notation
%%------------------------------------------------------------------------------

binary_to_scientific(Bin) ->
    binary_to_scientific(Bin, 0).

binary_to_scientific(<<0:1, Tail/bitstring>>, Zeros) ->
    binary_to_scientific(Tail, Zeros + 1);
binary_to_scientific(<<Significand:8, _Tail/bitstring>>, Zeros) ->
    %% We assume difficulty is encoded similarly
    256*(?HASH_BITS - Zeros) + Significand.

scientific_to_integer(S) ->
    Exp = S div 256,
    Significand = S rem 256,
    Significand bsl (Exp - 7).

integer_to_scientific(I) ->
    Exp = log2(I),
    256*Exp + (I bsr (Exp - 7)).

%%------------------------------------------------------------------------------
%% Base-2 integer logarithm
%%------------------------------------------------------------------------------
log2(1) ->
    0;
log2(N) when N > 1 ->
    1 + log2(N div 2).
