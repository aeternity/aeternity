-module(ws_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-export([send_msg/4, broadcast/2, broadcast/3]).

-define(GPROC_KEY, {p, l, {?MODULE, broadcast}}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    case jobs:ask(ws_handlers_queue) of
        {ok, JobId} ->
            gproc:reg(?GPROC_KEY),
            {ok, Req, JobId};
        {error, rejected} ->
            {shutdown, Req}
    end.

websocket_handle({text, MsgBin}, Req, State) ->
    ws_task_worker:execute(MsgBin), %% async, pool
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({send, SenderName, Action, Payload}, Req, State) ->
    MsgWithoutPayload = #{origin =>SenderName, action => Action},
    EmptyPayload =
        case Payload of
            _ when is_list(Payload) ->
                length(Payload) == 0;
            _ when is_map(Payload) ->
                maps:size(Payload) == 0
        end,
    Msg =
        case EmptyPayload of
            true ->
                MsgWithoutPayload;
            false ->
                maps:put(payload, Payload, MsgWithoutPayload)
        end,
    {reply, {text, jsx:encode(Msg)}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, JobId) ->
    jobs:done(JobId),
    ok.

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
