%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module dealing with SHA256 Proof of Work calculation and hash generation
%%% @end
%%%=============================================================================
-module(pow_sha256).

-export([generate/3,
         recalculate_difficulty/3,
         pick_nonce/0]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("sha256.hrl").

%% 10^24, approx. 2^80
-define(NONCE_RANGE, 1000000000000000000000000).

%% Integers represented in scientific notation:
%%   256 * <base-2 exponent> + the most significant byte (i.e., the significand,
%%   see https://en.wikipedia.org/wiki/Significand)
-type sci_int() :: integer().
-type pow_result() :: {ok, integer()} | {'error', 'generation_count_exhausted'}.

-export_type([sci_int/0,
              pow_result/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Generate a nonce value that, when used in hash calculation of 'Data', results in
%% a smaller value than the difficulty. Return {error, not_found} if the condition
%% is still not satisfied after trying 'Count' times with incremented nonce values.
%%------------------------------------------------------------------------------
-spec generate(Data :: sha256:hashable(), Difficulty :: sci_int(), Count :: integer()) ->
                      pow_result().
generate(Data, Difficulty, Count) ->
    %% Pick a nonce between 0 and 10^24 (approx. 2^80)
    Nonce = ?MODULE:pick_nonce(),
    generate_with_nonce(Data, Difficulty, Nonce, Count).

%%------------------------------------------------------------------------------
%% Adjust difficulty so that generation of new blocks proceeds at the expected pace
%%------------------------------------------------------------------------------
-spec recalculate_difficulty(sci_int(), integer(), integer()) -> sci_int().
recalculate_difficulty(Difficulty, Expected, Actual) ->
    DiffInt = scientific_to_integer(Difficulty),
    integer_to_scientific(max(1, DiffInt * Expected div Actual)).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


-spec generate_with_nonce(Data :: sha256:hashable(), Difficulty :: sci_int(),
                          Nonce :: integer(), Count :: integer()) -> pow_result().
generate_with_nonce(_Data, _Difficulty, _Nonce, 0) ->
    %% Count exhausted: fail
    {error, generation_count_exhausted};
generate_with_nonce(Data, Difficulty, Nonce, Count) ->
    Hash1 = sha256:hash(Data),
    Hash2 = sha256:hash(<<Hash1/binary, Difficulty:16, Nonce:?HASH_BITS>>),
    case binary_to_scientific(Hash2) of
        HD when HD < Difficulty ->
            %% Hash satisfies condition: return nonce
            %% Note: only the trailing 32 bytes of it are actually used in
            %% hash calculation
            {ok, Nonce};
        _ ->
            %% Keep trying
            generate_with_nonce(Data, Difficulty, Nonce + 1, Count - 1)
    end.

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

pick_nonce() ->
    crypto:rand_uniform(0, ?NONCE_RANGE).
