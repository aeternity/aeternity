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

-spec notify_query_tx(aetx:tx(),
                      list({aec_subscribe:id(), aec_subscribe:event()})) -> ok.
notify_query_tx(Tx, Subs) ->
    {aeo_query_tx, QTx} = aetx:specialize_type(Tx),
    [ WS ! {event, oracle_query_tx, QTx} || {{ws, WS}, {oracle, {query, OId}}} <- Subs,
                                            OId == aeo_query_tx:oracle(QTx) ],
    ok.

-spec notify_response_tx(aetx:tx(),
                         list({aec_subscribe:id(), aec_subscribe:event()})) -> ok.
notify_response_tx(Tx, Subs) ->
    {aeo_response_tx, RTx} = aetx:specialize_type(Tx),
    [ WS ! {event, oracle_response_tx, RTx} || {{ws, WS}, {oracle, {response, QId}}} <- Subs,
                                               QId == aeo_response_tx:query_id(RTx) ],
    ok.

