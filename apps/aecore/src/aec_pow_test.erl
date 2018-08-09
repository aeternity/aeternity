%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%    A library providing PoW generation and verification for testing purposes
%%%    The solution is always 42.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aec_pow_test).

-behaviour(aec_pow).

-export([generate/3,
         verify/4]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("pow.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================
-spec generate(Data :: aec_hash:hashable(), Target :: aec_pow:sci_int(),
               Nonce :: aec_pow:nonce()) -> aec_pow:pow_result().
generate(_Data, _Target, Nonce) ->
    % as stated in aec_headers.erl, pow evidence must be of ?POW_EV_SIZE (which is 42)
    % size in order to be admissible
    Solution = lists:duplicate(42, 42),
    {ok, {Nonce, Solution}}.

%%------------------------------------------------------------------------------
%% Proof of Work verification 
%%------------------------------------------------------------------------------
-spec verify(Data :: aec_hash:hashable(), Nonce :: aec_pow:nonce(),
             Evd :: aec_pow:pow_evidence(), Target :: aec_pow:sci_int()) ->
                    boolean().
verify(_Data, _Nonce, Evidence, _Target) when is_list(Evidence) ->
    true;
verify(_Data, _Nonce, _Evidence, _Target) ->
    false.
