%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Oracles subscription events
%%% @end
%%%-------------------------------------------------------------------

%% API
-module(aeo_subscription).

-export([notify_query_tx/2,
         notify_response_tx/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec notify_query_tx(any(), %% Dialyzer can't handle aeo_query_tx:query_tx(),
                      list({aec_subscribe:id(), aec_subscribe:event()})) -> ok.
notify_query_tx(Tx, Subs) ->
    [ WS ! {oracle_query_tx, Tx} || {{ws, WS}, {aeo, {query, OId}}} <- Subs,
                                    OId == aeo_query_tx:oracle(Tx) ],
    ok.

-spec notify_response_tx(any(), %% Dialyzer can't handle aeo_response_tx:response_tx(),
                         list({aec_subscribe:id(), aec_subscribe:event()})) -> ok.
notify_response_tx(Tx, Subs) ->
    [ WS ! {oracle_response_tx, Tx} || {{ws, WS}, {aeo, {response, IId}}} <- Subs,
                                       IId == aeo_response_tx:interaction_id(Tx) ],
    ok.

