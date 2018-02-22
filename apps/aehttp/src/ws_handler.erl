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
-export([send_msg/5, broadcast/2, broadcast/3]).

-define(GPROC_KEY, {p, l, {?MODULE, broadcast}}).

-type id() :: pid().
-export_type([id/0]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    case jobs:ask(ws_handlers_queue) of
        {ok, JobId} ->
            gproc:reg(?GPROC_KEY),
            {ok, Req, JobId};
        {error, _} ->
            {shutdown, Req}
    end.

websocket_handle({text, MsgBin}, Req, State) ->
    ws_task_worker:execute(MsgBin), %% async, pool
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({send, SenderName, Action, Payload}, Req, State) ->
    Msg = create_message(SenderName, Action, Payload),
    {reply, {text, jsx:encode(Msg)}, Req, State};
websocket_info({send, SenderName, Action, Tag, Payload}, Req, State) ->
    Msg = create_message(SenderName, Action, Tag, Payload),
    {reply, {text, jsx:encode(Msg)}, Req, State};
websocket_info({event, Event, EventData}, Req, State) ->
    case create_message_from_event(Event, EventData) of
        {ok, Msg} ->
            {reply, {text, jsx:encode(Msg)}, Req, State};
        {error, bad_event} ->
            {ok, Req, State}
    end;
websocket_info(Info, Req, State) ->
    lager:info("Unhandled message ~p", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, JobId) ->
    WsPid = self(),
    aec_subscribe:unsubscribe_all({ws, WsPid}),
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

send_msg(WsPid, SenderName, Action, Tag, Payload) ->
    WsPid ! {send, SenderName, Action, Tag, Payload}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

create_message(SenderName, Action, Tag, Payload) ->
    create_message_(SenderName, Action, Payload, #{tag => Tag}).

create_message(SenderName, Action, Payload) ->
    create_message_(SenderName, Action, Payload, #{}).

create_message_(SenderName, Action, Payload, Msg0) ->
    MsgWithoutPayload = Msg0#{origin => SenderName,
                              action => Action},
    NoPayload =
        case Payload of
            _ when is_list(Payload) ->
                Payload == [];
            _ when is_map(Payload) ->
                maps:size(Payload) == 0
        end,
    case NoPayload of
        true ->
            MsgWithoutPayload;
        false ->
            maps:put(payload, Payload, MsgWithoutPayload)
    end.

create_message_from_event(BlockEvent, {BlockHeight, BlockHash})
        when BlockEvent == mined_block; BlockEvent == new_block ->
    Payload = [{height, BlockHeight},
               {hash, aec_base58c:encode(block_hash, BlockHash)}],
    {ok, create_message(chain, BlockEvent, Payload)};
create_message_from_event(chain_tx, TxHash) ->
    Payload = [{tx_hash, aec_base58c:encode(tx_hash, TxHash)}],
    {ok, create_message(chain, tx_chain, Payload)};
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
    {ok, create_message(chain, new_oracle_query, Payload)};
create_message_from_event(oracle_response_tx, OracleResponseTx) ->
    %% TODO: Add TTL of the response to payload
    QId = aeo_response_tx:query_id(OracleResponseTx),
    Payload =
        [{query_id, aec_base58c:encode(oracle_query_id, QId)},
         {response, aeo_response_tx:response(OracleResponseTx)}],
    {ok, create_message(chain, new_oracle_response, Payload)};
create_message_from_event(Event, _EventData) ->
    lager:error("Unexpected event: ~p", [Event]),
    {error, bad_event}.
