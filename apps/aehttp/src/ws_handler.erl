%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Websocket handler.
%%% @end
%%%-------------------------------------------------------------------
-module(ws_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-export([send_msg/4, broadcast/2, broadcast/3]).

-define(GPROC_KEY, {p, l, {?MODULE, broadcast}}).

-define(SUBSCRIBE_EVENTS, [ block_created              %% node mined a block
                          ]).

-type id() :: pid().
-export_type([id/0]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {QsVals, Req1} = cowboy_req:qs_vals(Req),
    SubscribeEvents = subscribe_events(QsVals),
    case jobs:ask(ws_handlers_queue) of
        {ok, JobId} ->
            gproc:reg(?GPROC_KEY),
            lists:foreach(
              fun(Event) ->
                      aec_events:subscribe(Event)
              end,
              SubscribeEvents),
            {ok, Req1, JobId};
        {error, rejected} ->
            {shutdown, Req1}
    end.

websocket_handle({text, MsgBin}, Req, State) ->
    ws_task_worker:execute(MsgBin), %% async, pool
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({gproc_ps_event, Event, Data}, Req, State) ->
    Msg = create_message_from_event(Event, Data),
    {reply, {text, jsx:encode(Msg)}, Req, State};
websocket_info({send, SenderName, Action, Payload}, Req, State) ->
    Msg = create_message(SenderName, Action, Payload),
    {reply, {text, jsx:encode(Msg)}, Req, State};
websocket_info({oracle_query_tx, QTx}, Req, State) ->
    Msg = create_message_from_event(oracle_query_tx, QTx),
    {reply, {text, jsx:encode(Msg)}, Req, State};
websocket_info({oracle_response_tx, RTx}, Req, State) ->
    Msg = create_message_from_event(oracle_response_tx, RTx),
    {reply, {text, jsx:encode(Msg)}, Req, State};
websocket_info(Info, Req, State) ->
    lager:info("Unhandled message ~p", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, JobId) ->
    jobs:done(JobId),
    ok.

%% Should be used only in case when aec_events is not appropriate
broadcast(SenderName, Action) ->
    broadcast(SenderName, Action, []).
broadcast(SenderName, Action, Payload) ->
    try
        gproc:send(?GPROC_KEY, {send, SenderName, Action, Payload})
    catch error:badarg ->
            lager:info("ws_handler broadcasting when there is no client")
    end.

send_msg(WsPid, SenderName, Action, Payload) ->
    WsPid ! {send, SenderName, Action, Payload}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

subscribe_events(QsVals) ->
    case proplists:get_value(<<"subscribe_to">>, QsVals, <<"all">>) of
        <<"all">> ->
            ?SUBSCRIBE_EVENTS;
        SubscribeTo ->
            EventsList = binary:split(SubscribeTo, <<",">>),
            binary_events_to_codes(EventsList)
    end.

binary_events_to_codes(BinarySubscribeEvents) ->
    lists:foldl(
      fun maybe_include_subscribe_code/2,
      [], BinarySubscribeEvents).

maybe_include_subscribe_code(BinaryEvent, List) ->
    try
        SubscribeEvent = binary_to_existing_atom(BinaryEvent, utf8),
        case lists:member(SubscribeEvent, ?SUBSCRIBE_EVENTS) of
            true ->
                [SubscribeEvent | List];
            false ->
                lager:info("Unknown subscribe event ~p", [SubscribeEvent]),
                List
        end
    catch error:badarg ->
            lager:info("Unknown subscribe event ~p", [BinaryEvent]),
            List
    end.

create_message(SenderName, Action, Payload) ->
    MsgWithoutPayload = #{origin => SenderName,
                          action => Action},
    EmptyPayload =
        case Payload of
            _ when is_list(Payload) ->
                Payload == [];
            _ when is_map(Payload) ->
                maps:size(Payload) == 0
        end,
    case EmptyPayload of
        true ->
            MsgWithoutPayload;
        false ->
            maps:put(payload, Payload, MsgWithoutPayload)
    end.

create_message_from_event(block_created, #{info := Block}) ->
    BlockHeight = aec_blocks:height(Block),
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    Payload =
        [{height, BlockHeight},
         {hash, aec_base58c:encode(block_hash, BlockHash)}],
    create_message(miner, mined_block, Payload);
create_message_from_event(oracle_query_tx, OracleQueryTx) ->
    %% TODO: Add TTL of the query to payload
    Sender = aeo_query_tx:sender(OracleQueryTx),
    Nonce  = aeo_query_tx:nonce(OracleQueryTx),
    Oracle = aeo_query_tx:oracle(OracleQueryTx),
    QId    = aeo_query:id(Sender, Nonce, Oracle),
    Payload =
        [{sender,   aec_base58c:encode(account_pubkey, Sender)},
         {query,    aeo_query_tx:query(OracleQueryTx)},
         {query_id, aec_base58c:encode(oracle_query_id, QId)}],
    create_message(node, new_oracle_query, Payload);
create_message_from_event(oracle_response_tx, OracleResponseTx) ->
    %% TODO: Add TTL of the response to payload
    QId = aeo_response_tx:query_id(OracleResponseTx),
    Payload =
        [{query_id, aec_base58c:encode(oracle_query_id, QId)},
         {response, aeo_response_tx:response(OracleResponseTx)}],
    create_message(node, new_oracle_response, Payload).
