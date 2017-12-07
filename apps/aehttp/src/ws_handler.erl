-module(ws_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-export([send_msg/4, broadcast/2, broadcast/3]).

-define(GPROC_KEY, {p, l, {?MODULE, broadcast}}).

-define(SUBSCRIBE_EVENTS, [block_created %% node mined a block
                          ]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    case jobs:ask(ws_handlers_queue) of
        {ok, JobId} ->
            gproc:reg(?GPROC_KEY),
            lists:foreach(
                fun(Event) ->
                    aec_events:subscribe(Event)
                end,
                ?SUBSCRIBE_EVENTS),
            {ok, Req, JobId};
        {error, rejected} ->
            {shutdown, Req}
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
         {hash, base64:encode(BlockHash)}],
    create_message(miner, mined_block, Payload).
