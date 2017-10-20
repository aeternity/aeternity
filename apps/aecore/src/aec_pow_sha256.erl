%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module dealing with SHA256 Proof of Work calculation and hash generation
%%% @end
%%%=============================================================================
-module(aec_pow_sha256).

-behaviour(aec_pow).

-export([generate/5,
         verify/4]).


-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("sha256.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Generate a nonce value that, when used in hash calculation of 'Data', results in
%% a smaller value than the target. Return {error, not_found} if the condition
%% is still not satisfied after trying 'Count' times with incremented nonce values.
%%------------------------------------------------------------------------------
-spec generate(Data :: aec_sha256:hashable(), Target :: aec_pow:sci_int(),
               Count :: integer(), Nonce :: integer(),
               MaxNonce :: integer()) -> aec_pow:pow_result().
generate(Data, Target, Count, Nonce, MaxNonce) ->
    generate_with_nonce(Data, Target, Nonce, MaxNonce, Count).

%%------------------------------------------------------------------------------
%% Proof of Work verification (with target check)
%%------------------------------------------------------------------------------
-spec verify(Data :: aec_sha256:hashable(), Nonce :: integer(),
             Evd :: aec_pow:pow_evidence(), Target :: aec_pow:sci_int()) ->
                    boolean().
verify(Data, Nonce, Evd, Target) when Evd == no_value ->
    %% Verification: just try if current Nonce satisfies target threshold
    case generate_with_nonce(Data, Target, Nonce, -1, 1) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================


-spec generate_with_nonce(Data :: aec_sha256:hashable(), Target :: aec_pow:sci_int(),
                          Nonce :: integer(), MaxNonce :: integer(),
                          Count :: integer()) -> aec_pow:pow_result().
generate_with_nonce(_Data, _Target, _Nonce, _MaxNonce, 0) ->
    %% Count exhausted: fail
    {error, generation_count_exhausted};
generate_with_nonce(_Data, _Target, Nonce, Nonce, _Count) ->
    {error, nonce_range_exhausted};
generate_with_nonce(Data, Target, Nonce, MaxNonce, Count) ->
    Nonce32 = Nonce band 16#7fffffff,
    Hash1 = aec_sha256:hash(Data),
    Hash2 = aec_sha256:hash(<<Hash1/binary, Target:16, Nonce32:?HASH_BITS>>),
    case aec_pow:test_target(Hash2, Target) of
        true ->
            %% Hash satisfies condition: return nonce
            {ok, {Nonce, no_value}};
        false ->
            %% Keep trying
            generate_with_nonce(Data, Target, Nonce + 1, MaxNonce, Count - 1)
    end.
