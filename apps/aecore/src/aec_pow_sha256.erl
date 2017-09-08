%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module dealing with SHA256 Proof of Work calculation and hash generation
%%% @end
%%%=============================================================================
-module(aec_pow_sha256).

-behaviour(aec_pow).

-export([generate/3,
         verify/4]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("sha256.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Generate a nonce value that, when used in hash calculation of 'Data', results in
%% a smaller value than the difficulty. Return {error, not_found} if the condition
%% is still not satisfied after trying 'Count' times with incremented nonce values.
%%------------------------------------------------------------------------------
-spec generate(Data :: aec_sha256:hashable(), Difficulty :: aec_pow:sci_int(),
               Count :: integer()) -> aec_pow:pow_result().
generate(Data, Difficulty, Count) ->
    Nonce = aec_pow:pick_nonce(),
    generate_with_nonce(Data, Difficulty, Nonce, Count).

%%------------------------------------------------------------------------------
%% Proof of Work verification (with difficulty check)
%%------------------------------------------------------------------------------
-spec verify(Data :: aec_sha256:hashable(), Nonce :: integer(),
             Evd :: aec_pow:pow_evidence(), Difficulty :: aec_pow:sci_int()) ->
                    boolean().
verify(Data, Nonce, Evd, Difficulty) when Evd == no_value ->
    %% Verification: just try if current Nonce satisfies Difficulty
    case generate_with_nonce(Data, Difficulty, Nonce, 1) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================


-spec generate_with_nonce(Data :: aec_sha256:hashable(), Difficulty :: aec_pow:sci_int(),
                          Nonce :: integer(), Count :: integer()) -> aec_pow:pow_result().
generate_with_nonce(_Data, _Difficulty, _Nonce, 0) ->
    %% Count exhausted: fail
    {error, generation_count_exhausted};
generate_with_nonce(Data, Difficulty, Nonce, Count) ->
    Hash1 = aec_sha256:hash(Data),
    Hash2 = aec_sha256:hash(<<Hash1/binary, Difficulty:16, Nonce:?HASH_BITS>>),
    case aec_pow:binary_to_scientific(Hash2) of
        HD when HD < Difficulty ->
            %% Hash satisfies condition: return nonce
            %% Note: only the trailing 32 bytes of it are actually used in
            %% hash calculation
            {ok, {Nonce, no_value}};
        _ ->
            %% Keep trying
            generate_with_nonce(Data, Difficulty, Nonce + 1, Count - 1)
    end.
