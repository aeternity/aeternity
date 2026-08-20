%%%-------------------------------------------------------------------
%%% @doc Cowboy WebSocket handler for the JSON-RPC subscribe transport.
%%%
%%% Same wire format as the HTTP endpoint: each text frame is one
%%% JSON-RPC envelope (single request or batch). For `eth_subscribe' /
%%% `eth_unsubscribe' the handler talks to `aerpc_subscriptions'
%%% directly so the registry can monitor this conn's pid and route
%%% async notifications back. All other methods delegate to
%%% `aerpc:dispatch/1', exactly like the HTTP path.
%%%
%%% A socket here is expected to sit idle in the inbound direction --
%%% subscribe once, then only receive -- so this handler sets its own
%%% idle policy and pings; see ?PING_INTERVAL_MS.
%%%
%%% Supported kinds: `newHeads', `logs', `newPendingTransactions'. A
%%% kind outside that set is reported as unsupported rather than as
%%% invalid params, so a client can tell "this node will never do that"
%%% from "you called it wrong" and fall back to the poll filter.
%%%
%%% Notification frames have eth's standard `eth_subscription' shape
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

%% Exposed for coverage: the point of both is that they are explicit
%% rather than inherited from Cowboy, and that they hold a relationship
%% to each other.
-export([ws_opts/0, ping_interval/0, keepalive_message/0]).

-define(JSONRPC_VSN, <<"2.0">>).

%% == Keeping a listening-only socket open ==
%%
%% Cowboy's `idle_timeout' counts time with no INBOUND data and defaults
%% to 60s (`cowboy_websocket.erl:378'). A subscription client is the one
%% shape that never sends anything: it subscribes once and then only
%% receives. Under the default it was closed at 60.0s every time,
%% however many frames the server had pushed to it -- outbound traffic
%% does not reset that timer.
%%
%% Raising the timeout alone only moves the deadline, and `infinity'
%% trades a wrong disconnect for a process that survives a dead peer
%% forever. So: keep a finite timeout AND keep the connection provably
%% alive from this side. A ping every ?PING_INTERVAL_MS draws an
%% automatic pong from any conforming client, and an inbound pong is
%% inbound data, so the idle timer resets. A peer that has really gone
%% away sends no pong and is reaped after ?IDLE_TIMEOUT_MS.
%%
%% The interval is deliberately under a minute: the common reverse proxy
%% in front of a node -- nginx `proxy_read_timeout' -- also defaults to
%% 60s, and a keepalive that only satisfied Cowboy would still be cut by
%% the proxy.
-define(PING_INTERVAL_MS, 30000).
-define(IDLE_TIMEOUT_MS, 120000).
-define(PING_MSG, aerpc_ws_keepalive).

init(Req, _Opts) ->
    process_flag(trap_exit, true),
    {cowboy_websocket, Req, #{}, ws_opts()}.

%% @doc Cowboy per-connection WebSocket options. Never the default map:
%% see the comment above ?PING_INTERVAL_MS for why the 60s inbound-idle
%% default is wrong for this endpoint specifically.
-spec ws_opts() -> map().
ws_opts() ->
    #{idle_timeout => ?IDLE_TIMEOUT_MS}.

%% @doc How often this side pings. Must stay comfortably below the idle
%% timeout, or the keepalive cannot do its job.
-spec ping_interval() -> pos_integer().
ping_interval() ->
    ?PING_INTERVAL_MS.

%% @doc The message this handler sends itself to fire a keepalive.
%% Exposed so coverage can drive the real clause rather than restate the
%% macro, which a copy would let drift.
-spec keepalive_message() -> atom().
keepalive_message() ->
    ?PING_MSG.

websocket_init(State) ->
    {ok, schedule_ping(State)}.

%% The timer reference is kept in the connection state. It is what makes
%% "the keepalive recurs" an observable property rather than a claim,
%% and it gives a later cancel something to hold.
schedule_ping(State) when is_map(State) ->
    Ref = erlang:send_after(?PING_INTERVAL_MS, self(), ?PING_MSG),
    State#{ping_ref => Ref}.

websocket_handle({text, Frame}, State) ->
    Reply = handle_frame(Frame),
    {reply, {text, jsx:encode(Reply)}, State};
websocket_handle({binary, _}, State) ->
    %% Eth WS implementations are text-only; reject binary frames with
    %% a parse error.
    Err = aerpc_jsonrpc:error(null, -32700, <<"Parse error">>),
    {reply, {text, jsx:encode(Err)}, State};
websocket_handle({pong, _Payload}, State) ->
    %% The reply to our keepalive. Cowboy has already reset the idle
    %% timer by the time this arrives; nothing more to do.
    {ok, State};
websocket_handle(_Other, State) ->
    {ok, State}.

websocket_info({aerpc_notify, SubId, ResultTerm}, State) ->
    Frame = notification_frame(SubId, ResultTerm),
    {reply, {text, jsx:encode(Frame)}, State};
websocket_info(?PING_MSG, State) ->
    %% Reschedule as we go: the keepalive has to recur for the life of
    %% the connection, not fire once and leave it to die at the second
    %% idle window.
    {reply, ping, schedule_ping(State)};
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
    %% This transport cannot hand the whole batch to `aerpc:dispatch/1'
    %% (subscribe/unsubscribe need this connection's pid), so apply the
    %% same cap here rather than leaving the WS path uncapped.
    Max = aerpc:max_batch_size(),
    case length(Batch) > Max of
        true ->
            {error, Code, Msg} = aerpc_errors:batch_too_large(Max),
            aerpc_jsonrpc:error(null, Code, Msg);
        false ->
            [handle_request(Req) || Req <- Batch]
    end;
handle_request(#{<<"jsonrpc">> := ?JSONRPC_VSN,
                 <<"method">>  := <<"eth_subscribe">>} = Req) ->
    Id = maps:get(<<"id">>, Req, null),
    %% Which kinds exist is the registry's business, not the transport's.
    case aerpc_subscriptions:parse_subscribe_params(
           maps:get(<<"params">>, Req, [])) of
        {ok, Kind, Criteria} -> do_subscribe(Id, Kind, Criteria);
        {error, Code, Msg}   -> aerpc_jsonrpc:error(Id, Code, Msg)
    end;
handle_request(#{<<"jsonrpc">> := ?JSONRPC_VSN,
                 <<"method">>  := <<"eth_unsubscribe">>} = Req) ->
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
      <<"method">>  => <<"eth_subscription">>,
      <<"params">>  => #{<<"subscription">> => SubId,
                         <<"result">>       => Result}}.
