%%%-------------------------------------------------------------------
%%% @doc Cowboy WebSocket handler for the JSON-RPC subscribe transport.
%%%
%%% Same wire format as the HTTP endpoint: each text frame is one
%%% JSON-RPC envelope (single request or batch). For `ae_subscribe' /
%%% `ae_unsubscribe' the handler talks to `aerpc_subscriptions'
%%% directly so the registry can monitor this conn's pid and route
%%% async notifications back. All other methods delegate to
%%% `aerpc:dispatch/1', exactly like the HTTP path.
%%%
%%% Notification frames have eth's standard `ae_subscription' shape
%%% (no `id', a `params: {subscription, result}' object).
%%% @end
%%%-------------------------------------------------------------------
-module(aehttp_rpc_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

-define(JSONRPC_VSN, <<"2.0">>).

init(Req, _Opts) ->
    process_flag(trap_exit, true),
    {cowboy_websocket, Req, #{}}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Frame}, State) ->
    Reply = handle_frame(Frame),
    {reply, {text, jsx:encode(Reply)}, State};
websocket_handle({binary, _}, State) ->
    %% Eth WS implementations are text-only; reject binary frames with
    %% a parse error.
    Err = aerpc_jsonrpc:error(null, -32700, <<"Parse error">>),
    {reply, {text, jsx:encode(Err)}, State};
websocket_handle(_Other, State) ->
    {ok, State}.

websocket_info({aerpc_notify, SubId, ResultTerm}, State) ->
    Frame = notification_frame(SubId, ResultTerm),
    {reply, {text, jsx:encode(Frame)}, State};
websocket_info(_Other, State) ->
    {ok, State}.

terminate(_Reason, _PartialReq, _State) ->
    %% Tell the registry to drop every subscription we own.
    aerpc_subscriptions:drop_owner(self()),
    ok.

%% ===================================================================
%% Frame handling
%% ===================================================================

handle_frame(Frame) ->
    try jsx:decode(Frame, [return_maps]) of
        Decoded when is_map(Decoded); is_list(Decoded) ->
            handle_request(Decoded);
        _Other ->
            aerpc_jsonrpc:error(null, -32700, <<"Parse error">>)
    catch
        _:_ -> aerpc_jsonrpc:error(null, -32700, <<"Parse error">>)
    end.

handle_request(Batch) when is_list(Batch) ->
    [handle_request(Req) || Req <- Batch];
handle_request(#{<<"jsonrpc">> := ?JSONRPC_VSN,
                 <<"method">>  := <<"ae_subscribe">>} = Req) ->
    Id = maps:get(<<"id">>, Req, null),
    case maps:get(<<"params">>, Req, []) of
        [<<"newHeads">>] ->
            do_subscribe(Id, newHeads, undefined);
        [<<"newHeads">>, _Opts] ->
            %% eth ignores the second arg for newHeads in practice;
            %% accept and discard.
            do_subscribe(Id, newHeads, undefined);
        [<<"logs">>] ->
            do_subscribe(Id, logs, #{});
        [<<"logs">>, Crit] when is_map(Crit) ->
            do_subscribe(Id, logs, Crit);
        _Other ->
            aerpc_jsonrpc:error(Id, -32602, <<"Invalid params">>)
    end;
handle_request(#{<<"jsonrpc">> := ?JSONRPC_VSN,
                 <<"method">>  := <<"ae_unsubscribe">>} = Req) ->
    Id = maps:get(<<"id">>, Req, null),
    case maps:get(<<"params">>, Req, []) of
        [SubId] when is_binary(SubId) ->
            Removed = aerpc_subscriptions:unsubscribe(self(), SubId),
            aerpc_jsonrpc:result(Id, Removed);
        _Other ->
            aerpc_jsonrpc:error(Id, -32602, <<"Invalid params">>)
    end;
handle_request(Decoded) ->
    %% Everything else: behave exactly like the HTTP transport.
    aerpc:dispatch(Decoded).

do_subscribe(Id, Kind, Criteria) ->
    case aerpc_subscriptions:subscribe(self(), Kind, Criteria) of
        {ok, SubId} ->
            aerpc_jsonrpc:result(Id, SubId);
        {error, Code, Msg} ->
            aerpc_jsonrpc:error(Id, Code, Msg)
    end.

notification_frame(SubId, Result) ->
    #{<<"jsonrpc">> => ?JSONRPC_VSN,
      <<"method">>  => <<"ae_subscription">>,
      <<"params">>  => #{<<"subscription">> => SubId,
                         <<"result">>       => Result}}.
