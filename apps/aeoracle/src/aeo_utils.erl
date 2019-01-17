%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Utility functions for AE Oracles
%%% @end
%%%-------------------------------------------------------------------

-module(aeo_utils).

-export([ttl_delta/2,
         ttl_expiry/2
        ]).

-include_lib("apps/aecontract/src/aecontract.hrl").

-spec ttl_delta(aec_blocks:height(), aeo_oracles:ttl()) ->
    non_neg_integer() | {error, too_low_height}.
ttl_delta(_CurrHeight, {delta, D}) -> D;
ttl_delta(CurrHeight, {block, H}) when H > CurrHeight -> H - CurrHeight;
ttl_delta(_CurrHeight, {block, _H}) -> {error, too_low_height}.

-spec ttl_expiry(aec_blocks:height(), aeo_oracles:ttl()) ->
    aec_blocks:height() | {error, too_low_height}.
ttl_expiry(CurrentHeight, TTL) ->
    case ttl_delta(CurrentHeight, TTL) of
        TTLDelta when is_integer(TTLDelta) ->
            CurrentHeight + TTLDelta;
        {error, _Rsn} = Err ->
            Err
    end.
