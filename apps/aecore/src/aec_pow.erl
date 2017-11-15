%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Common functions and behaviour for Proof of Work
%%% @end
%%%=============================================================================
-module(aec_pow).

-export([test_target/2,
         pick_nonce/0,
         next_nonce/1,
         target_to_difficulty/1,
         recalculate_difficulty/3,

         scientific_to_integer/1,
         integer_to_scientific/1]).

%% For testing only
-export([pow_module/0]).


-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("pow.hrl").
-include("sha256.hrl").


%% 10^24, approx. 2^80
-define(NONCE_RANGE, 1000000000000000000000000).
-define(POW_MODULE, aec_pow_cuckoo).



%%------------------------------------------------------------------------------
%%                      Target threshold and difficulty
%%
%% The mining rate is controlled by setting a target threshold. The PoW nonce
%% is accepted if a hash value (the hash of the header for SHA-256, the hash of
%% the solution graph for Cuckoo Cycleas, converted to an integer) is below
%% this target threshold.
%%
%% A lower target represents a harder task (requiers the hash to start with a
%% number of zeros).
%%
%% The target thershold relates to another value: the diifculty. This is
%% proportional to the hardness of the PoW task:
%%
%% Difficulty = <Target of difficulty 1> / Target,
%%
%% a floating point value.
%% Bitcoin uses 0x00000000FFFF0000000000000000000000000000000000000000000000000000
%% as Difficulty 1 target (0x1d00ffff in scientific notation, see below). For
%% Cuckoo Cycle we need a lighter filtering of solutions than for SHA-256 as the
%% basic algorithm is much slower than a simple hash generation, so we use the
%% largest possible value:
%% 0xFFFF000000000000000000000000000000000000000000000000000000000000 (0x2100ffff
%% in scientific notation) as difficulty 1.
%%
%% We store the current target threshold in the block header in scientific notation.
%% Difficulty is used to select the winning fork of new blocks: the difficulty of a
%% chain of blocks is the sum of the diffculty of each block.
%%
%% Integers represented in scientific notation:
%%   2^24 * <base-2 exponent + 3> + the first 3 most significant bytes (i.e.,
%%   the significand, see https://en.wikipedia.org/wiki/Significand).
%%   The + 3 corresponds to the length of the
%%   significand (i.e., the int value is 0.<significand> * 8^<exponent>).
%%   https://en.bitcoin.it/wiki/Difficulty#How_is_difficulty_stored_in_blocks.3F)
%%------------------------------------------------------------------------------
-type sci_int() :: integer().

%% Optional evidence for PoW verification
-type pow_evidence() :: 'no_value' | term().
-type pow_result() :: {'ok', {Nonce :: integer, Solution :: pow_evidence()}} |
                      {error, no_solution | {runtime, term()}}.
%% Difficulty: max threshold (0x00000000FFFF0000000000000000000000000000000000000000000000000000)
%% over the actual one. Always positive.
-type difficulty() :: float().

-export_type([sci_int/0,
              difficulty/0,
              pow_evidence/0,
              pow_result/0]).

%%%=============================================================================
%%% Behaviour
%%%=============================================================================

-callback generate(Data :: aec_sha256:hashable(), Difficulty :: aec_pow:sci_int(),
                   Nonce :: integer()) ->
    aec_pow:pow_result().

-callback verify(Data :: aec_sha256:hashable(), Nonce :: integer(),
                 Evd :: aec_pow:pow_evidence(), Difficulty :: aec_pow:sci_int()) ->
    boolean().

%%%=============================================================================
%%% API
%%%=============================================================================

-spec scientific_to_integer(sci_int()) -> integer().
scientific_to_integer(S) ->
    {Exp, Significand} = break_up_scientific(S),
    E3 = Exp - 3,
    case Exp >= 0 of
        true ->
            Significand bsl (8 * E3);
        false ->
            Significand bsr (-8 * E3)
    end.

-spec integer_to_scientific(integer()) -> sci_int().
integer_to_scientific(I) ->
    %% Find exponent and significand
    {Exp, Significand} = integer_to_scientific(I, 3),
    case Exp >= 0 of
        true ->
            %% 1st byte: exponent, next 3 bytes: significand
            (Exp bsl 24) + Significand;
        false ->
            %% flip sign bit in significand
            ((-Exp) bsl 24) + 16#800000 + Significand
    end.

-spec target_to_difficulty(sci_int()) -> float().
target_to_difficulty(Th) ->
    %% Max threshold over the current one
    ?HIGHEST_TARGET_INT/scientific_to_integer(Th).

-spec pick_nonce() -> integer().
pick_nonce() ->
    rand:uniform(?NONCE_RANGE) band ?MAX_NONCE.

-spec next_nonce(integer()) -> integer().
next_nonce(N) ->
    (N + 1) band ?MAX_NONCE.

%%------------------------------------------------------------------------------
%% Adjust difficulty so that generation of new blocks proceeds at the expected pace
%% Expected and Actual are rates (if Actual is too small, we need to increase
%% difficulty).
%%------------------------------------------------------------------------------
-spec recalculate_difficulty(sci_int(), integer(), integer()) -> sci_int().
recalculate_difficulty(Difficulty, _Expected, 0) ->
    %% In the unexpected case the the two blocks used for rate calculation
    %% have the same timestamp, do not update difficulty (as rate is meaningless).
    %% TODO: handle this case. Most likely that this is case we failed to enforce
    %% increasing timestaps in headers.
    Difficulty;
recalculate_difficulty(Difficulty, Expected, Actual) ->
    DiffInt = scientific_to_integer(Difficulty),
    %% Prevent 0 difficulty: 0 can never be increased
    integer_to_scientific(min(?HIGHEST_TARGET_INT,
                              max(1, ((DiffInt * Expected) div Actual)))).


%%------------------------------------------------------------------------------
%% Test if binary is under the target threshold
%%------------------------------------------------------------------------------
-spec test_target(binary(), sci_int()) -> boolean().
test_target(Bin, Target) ->
    {Exp, Significand} = break_up_scientific(Target),
    L = size(Bin),
    %% We expect L - Exp zero bytes and Exp nonzero bytes
    Zeros = 8*max(0, L - Exp),
    case Exp of
        _E when _E >=0,
                _E < 3 ->
            %% Less than 3 bytes behind zeros
            compare_bin_to_significand(Bin, Significand bsr (8*(3 - Exp)), Zeros, 8*Exp);
        _ when Exp > L,
               Exp < L + 3 ->
            %% Exponent larger than length of Bin
            Skip = 8*(Exp - L),
            Compare = 24 - Skip,
            case Significand bsr Compare of
                0 ->
                    %% Ok, we do not lose significant bits
                    compare_bin_to_significand(Bin, Significand bsl Skip, 0, 24);
                _ ->
                    %% Not supposed to happen
                    {error, {incorrect_target_exponent, Exp}}
            end;
        _E when _E >= 0 ->
            %% At least 3 bytes after zeros
            compare_bin_to_significand(Bin, Significand, Zeros, 24);
        _E when _E < 0 ->
            %% All bits must be zero
            Bits = 8*L,
            Bin == <<0:Bits>>
    end.

compare_bin_to_significand(Bin, Significand, Zeros, NumBits) ->
    case Bin of
        <<0:Zeros, I:NumBits, _T/bitstring>> ->
            %% Compare with significand
            I < Significand;
        <<0:Zeros, _T/bitstring>> ->
            {error, {fewer_bits_than_required, NumBits, 8*size(Bin) - Zeros}};
        _ ->
            %% Fewer zeros than expected
            false
    end.

-spec pow_module() -> 'aec_pow_cuckoo' | 'aec_pow_sha256'.
pow_module() ->
    ?POW_MODULE.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

integer_to_scientific(I, Exp) when I > 16#7fffff ->
    integer_to_scientific(I bsr 8, Exp + 1);
integer_to_scientific(I, Exp) when I < 16#008000 ->
    integer_to_scientific(I bsl 8, Exp - 1);
integer_to_scientific(I, Exp) ->
    %% Add the number of bytes in the significand
    {Exp, I}.

%% Return the exponent and significand of a sci_int().
break_up_scientific(S) ->
    SigMask = (1 bsl 24) - 1,
    Exp = ((S bxor SigMask) bsr 24),
    Significand = S band SigMask,
    %% Remove the sign bit, apply to exponent
    case 16#800000 band Significand of
        0 ->
            {Exp, Significand};
        _ ->
            {-Exp, Significand - 16#800000}
    end.
