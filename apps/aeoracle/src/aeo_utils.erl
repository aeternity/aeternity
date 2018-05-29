%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Utility functions for AE Oracles
%%% @end
%%%-------------------------------------------------------------------

-module(aeo_utils).

-export([check_ttl_fee/3,
         ttl_delta/2,
         ttl_expiry/2,
         ttl_fee/2]).

-define(ORACLE_TTL_FEE, 0.001).

%% TODO: This should also include size of the thing that has a TTL
-spec check_ttl_fee(aec_blocks:height(), aeo_oracles:ttl(), non_neg_integer()) ->
                        ok | {error, too_low_fee} | {error, too_low_height}.
check_ttl_fee(Height, TTL, Fee) ->
    try
        NBlocks = ttl_delta(Height, TTL),
        case ttl_fee(0, NBlocks) =< Fee of
            true  -> ok;
            false -> {error, too_low_fee}
        end
    catch error:{too_low_height, _, _} ->
        {error, too_low_height}
    end.

-spec ttl_delta(aec_blocks:height(), aeo_oracles:ttl()) -> non_neg_integer().
ttl_delta(CurrHeight, TTL) ->
    case TTL of
        {delta, D} -> D;
        {block, H} when H > CurrHeight -> H - CurrHeight;
        {block, H} -> error({too_low_height, H, CurrHeight})
    end.

-spec ttl_expiry(aec_blocks:height(), aeo_oracles:ttl()) -> aec_blocks:height().
ttl_expiry(CurrentHeight, TTL) ->
    CurrentHeight + ttl_delta(CurrentHeight, TTL).

-spec ttl_fee(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
ttl_fee(_Size, NBlocks) ->
    ceil(NBlocks * ?ORACLE_TTL_FEE).

