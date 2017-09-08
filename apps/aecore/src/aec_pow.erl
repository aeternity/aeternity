%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%
%%% @end
%%%=============================================================================
-module(aec_pow).

-export([binary_to_scientific/1,
         scientific_to_integer/1,
         integer_to_scientific/1,
         pick_nonce/0,
         recalculate_difficulty/3,
         pow_module/0]).


-ifdef(TEST).
-compile(export_all).
-endif.

-include("sha256.hrl").

%% 10^24, approx. 2^80
-define(NONCE_RANGE, 1000000000000000000000000).
-define(POW_MODULE, aec_pow_cuckoo).

%% Integers represented in scientific notation:
%%   256 * <base-2 exponent> + the most significant byte (i.e., the significand,
%%   see https://en.wikipedia.org/wiki/Significand)
-type sci_int() :: integer().
%% Optional evidence for PoW verification
-opaque pow_evidence() :: 'no_value' | term().
-type pow_result() :: {'ok', {Nonce :: integer, Solution :: pow_evidence()}} |
                      {'error', 'generation_count_exhausted'}.

-export_type([sci_int/0,
              pow_evidence/0,
              pow_result/0]).

%%%=============================================================================
%%% Behaviour
%%%=============================================================================

-callback generate(Data :: aec_sha256:hashable(), Difficulty :: aec_pow:sci_int(),
                   Count :: integer()) -> 
    aec_pow:pow_result().

-callback verify(Data :: aec_sha256:hashable(), Nonce :: integer(),
                 Evd :: aec_pow:pow_evidence(), Difficulty :: aec_pow:sci_int()) ->
    boolean().

%%%=============================================================================
%%% API
%%%=============================================================================

binary_to_scientific(Bin) ->
    binary_to_scientific(Bin, 0).

-spec scientific_to_integer(sci_int()) -> integer().
scientific_to_integer(S) ->
    Exp = S div 256,
    Significand = S rem 256,
    Significand bsl (Exp - 7).

-spec integer_to_scientific(integer()) -> sci_int().
integer_to_scientific(I) ->
    Exp = log2(I),
    256*Exp + (I bsr (Exp - 7)).

-spec pick_nonce() -> integer().
pick_nonce() ->
    rand:uniform(?NONCE_RANGE).

%%------------------------------------------------------------------------------
%% Adjust difficulty so that generation of new blocks proceeds at the expected pace
%%------------------------------------------------------------------------------
-spec recalculate_difficulty(sci_int(), integer(), integer()) -> sci_int().
recalculate_difficulty(Difficulty, Expected, Actual) ->
    DiffInt = scientific_to_integer(Difficulty),
    integer_to_scientific(max(1, DiffInt * Expected div Actual)).


-spec pow_module() -> module().
pow_module() ->
    ?POW_MODULE.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

binary_to_scientific(<<0:1, Tail/bitstring>>, Zeros) ->
    binary_to_scientific(Tail, Zeros + 1);
binary_to_scientific(<<Significand:8, _Tail/bitstring>>, Zeros) ->
    %% We assume difficulty is encoded similarly
    256*(?HASH_BITS - Zeros) + Significand.

%%------------------------------------------------------------------------------
%% Base-2 integer logarithm
%%------------------------------------------------------------------------------
log2(1) ->
    0;
log2(N) when N > 1 ->
    1 + log2(N div 2).
