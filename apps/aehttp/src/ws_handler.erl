%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Websocket handler.
%%% @end
%%%-------------------------------------------------------------------
-module(ws_handler).

-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).
-export([send_msg/5, broadcast/2, broadcast/3]).

-define(GPROC_KEY, {p, l, {?MODULE, broadcast}}).

-type id() :: pid().
-export_type([id/0]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(_Opts) ->
    case jobs:ask(ws_handlers_queue) of
        {ok, JobId} ->
            gproc:reg(?GPROC_KEY),
            {ok, JobId};
        {error, _} ->
            {stop, undefined}
    end.

websocket_handle({text, MsgBin}, State) ->
    ws_task_worker:execute(MsgBin), %% async, pool
    {ok, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({send, SenderName, Action, Payload}, State) ->
    Msg = create_message(SenderName, Action, Payload),
    {reply, {text, jsx:encode(Msg)}, State};
websocket_info({send, SenderName, Action, Tag, Payload}, State) ->
    Msg = create_message(SenderName, Action, Tag, Payload),
    {reply, {text, jsx:encode(Msg)}, State};
websocket_info({event, Event, EventData}, State) ->
    case create_message_from_event(Event, EventData) of
        {ok, Msg} ->
            {reply, {text, jsx:encode(Msg)}, State};
        {error, bad_event} ->
            {ok, State}
    end;
websocket_info(Info, State) ->
    lager:info("Unhandled message ~p", [Info]),
    {ok, State}.

terminate(_Reason, _PartialReq, JobId) ->
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

create_message_from_event(mined_block, {BlockHeight, BlockHash}) ->
    Payload = [{height, BlockHeight},
               {hash, aec_base58c:encode(key_block_hash, BlockHash)}],
    {ok, create_message(chain, mined_block, Payload)};
create_message_from_event(added_micro_block, {BlockHeight, BlockHash}) ->
    Payload = [{height, BlockHeight},
               {hash, aec_base58c:encode(micro_block_hash, BlockHash)}],
    {ok, create_message(chain, added_micro_block, Payload)};
create_message_from_event(new_block, {BlockType, BlockHeight, BlockHash}) ->
    BlockHash1 =
        case BlockType of
            key -> aec_base58c:encode(key_block_hash, BlockHash);
            micro -> aec_base58c:encode(micro_block_hash, BlockHash)
        end,
    Payload = [{height, BlockHeight},
               {hash, BlockHash1}],
    {ok, create_message(chain, new_block, Payload)};
create_message_from_event(chain_tx, TxHash) ->
    Payload = [{tx_hash, aec_base58c:encode(tx_hash, TxHash)}],
    {ok, create_message(chain, tx_chain, Payload)};
create_message_from_event(Event, _EventData) ->
    lager:error("Unexpected event: ~p", [Event]),
    {error, bad_event}.
