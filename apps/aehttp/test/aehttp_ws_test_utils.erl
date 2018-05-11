-module(aehttp_ws_test_utils).
-behaviour(websocket_client).

%% API
-export([start_link/2,
         start_link_channel/4,
         start_channel/4,
         set_role/2,
         send/3, send/4, send/5,
         send_tagged/4,
         register_test_for_event/3,
         unregister_test_for_event/3,
         register_test_for_channel_event/2,
         unregister_test_for_channel_event/2,
         wait_for_event/3,
         wait_for_event/4,
         wait_for_channel_event/2,
         wait_for_connect/1,
         wait_for_connect_any/0,
         stop/1]).

%% behaviour exports
-export([
         init/1,
         onconnect/2,
         ondisconnect/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

-define(DEFAULT_EVENT_TIMEOUT, 5000).
-define(DEFAULT_SUB_TIMEOUT, 1000).

-define(CHANNEL, channel).

-record(state, {role :: atom() | undefined,
                regs:: map() | undefined
               }).

start_link(Host, Port) ->
    WsAddress = "ws://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/websocket",
    ct:log("connecting to ~p", [WsAddress]),
    {ok, Pid} = websocket_client:start_link(WsAddress, ?MODULE, self()),
    wait_for_connect(Pid).

start_link_channel(Host, Port, RoleA, Opts) when is_atom(RoleA) ->
    Role = atom_to_binary(RoleA, utf8),
    WsAddress = make_channel_connect_address(Host, Port, Role, Opts),
    ct:log("connecting to Channel ~p as ~p", [WsAddress, Role]),
    {ok, Pid} = websocket_client:start_link(WsAddress, ?MODULE, self()),
    wait_for_connect(Pid).

start_channel(Host, Port, RoleA, Opts) when is_atom(RoleA) ->
    Role = atom_to_binary(RoleA, utf8),
    WsAddress = make_channel_connect_address(Host, Port, Role, Opts),
    ct:log("connecting to Channel ~p as ~p", [WsAddress, Role]),
    {ok, Pid} = websocket_client:start(WsAddress, ?MODULE, self()),
    case wait_for_connect(Pid) of
        {ok, Pid} = Res ->
            set_role(Pid, RoleA),
            Res;
        {error, _} = Err -> Err
    end.


make_channel_connect_address(Host, Port, Role, Opts0) ->
    Opts = maps:put(role, Role, Opts0),
    Params = encode_params(Opts),
    "ws://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/channel" ++ Params.

wait_for_connect_any() ->
    case wait_for_event_any(websocket, connected) of
        {ok, Pid} ->
            {ok, Pid};
        timeout ->
            {error, none_connected}
    end.

wait_for_connect(Pid) ->
    case wait_for_event(Pid, websocket, connected, ?DEFAULT_SUB_TIMEOUT) of
        ok ->
            {ok, Pid};
        timeout ->
            case process_info(Pid) of
                undefined -> {error, rejected};
                _ -> {error, still_connecting, Pid}
            end
    end.

set_role(ConnPid, Role) when is_atom(Role) ->
    ConnPid ! {set_role, Role}.

stop(ConnPid) ->
    ok = websocket_client:stop(ConnPid).

send(ConnPid, Action, Payload) ->
    send_(ConnPid, none, Action, Payload, #{}).

send_tagged(ConnPid, Action, Tag, Payload) ->
    send_(ConnPid, none, Action, Payload, #{tag => Tag}).

send(ConnPid, Target, Action, Tag, Payload) ->
    send_(ConnPid, Target, Action, Payload, #{tag => Tag}).

send(ConnPid, Target, Action, Payload) ->
    send_(ConnPid, Target, Action, Payload, #{}).

send_(ConnPid, Target, Action, Payload, Msg0) ->
    Msg1 = case Target of
              none -> Msg0#{action => Action};
              _ -> Msg0#{target => Target,
                          action => Action}
           end,
    Msg =
        case Payload == [] orelse Payload == #{} of
            true  -> Msg1;
            false -> maps:put(payload, Payload, Msg1)
        end,
    ConnPid ! {send_to_client, jsx:encode(Msg)},
    ok.

%% when an WS event occours, the registered process will receive a message in
%% the mailbox. Message structure will be:
%% {Sender::pid(), websocket_event, Origin::atom(), Action::atom()}
%% {Sender::pid(), websocket_event, Origin::atom(), Action::atom(), Payload::map()}
%% where the Origin and Action are the same as the ones for which the process
%% had registered. This message is consumed by wait_for_event/2,3
register_test_for_event(ConnPid, Origin, Action) ->
    Event = {Origin, Action},
    ConnPid ! {register_test, self(), Event},
    ok = wait_for_event(ConnPid, registered_test, Event, ?DEFAULT_SUB_TIMEOUT),
    ok.

unregister_test_for_event(ConnPid, Origin, Action) ->
    Event = {Origin, Action},
    ConnPid ! {unregister_test, self(), Event},
    ok = wait_for_event( ConnPid, unregistered_test, Event, ?DEFAULT_SUB_TIMEOUT),
    ok.

register_test_for_channel_event(ConnPid, Action) ->
    register_test_for_event(ConnPid, ?CHANNEL, Action).

unregister_test_for_channel_event(ConnPid, Action) ->
    unregister_test_for_event(ConnPid, ?CHANNEL, Action).


%% consumes messages from the mailbox:
%% {Sender::pid(), websocket_event, Origin::atom(), Action::atom()}
%% {Sender::pid(), websocket_event, Origin::atom(), Action::atom(), Payload::map()}
-spec wait_for_event_any(atom(), atom()) -> {ok, pid()}
                                          | {ok, pid(), map()}
                                          | {ok, pid(), term(), map()}
                                          | timeout.
wait_for_event_any(Origin, Action) ->
    wait_for_event_any(Origin, Action, ?DEFAULT_EVENT_TIMEOUT).

-spec wait_for_event_any(atom(), atom(), integer()) -> ok | {ok, map()} | timeout.
wait_for_event_any(Origin, Action, Timeout) ->
    receive
        {AnyConnPid, websocket_event, Origin, Action, Tag, Payload} ->
            {ok, AnyConnPid, Tag, Payload};
        {AnyConnPid, websocket_event, Origin, Action, Payload} ->
            {ok, AnyConnPid, Payload};
        {AnyConnPid, websocket_event, Origin, Action} ->
            {ok, AnyConnPid}
    after Timeout ->
        timeout
    end.

%% consumes messages from the mailbox:
%% {Sender::pid(), websocket_event, Origin::atom(), Action::atom()}
%% {Sender::pid(), websocket_event, Origin::atom(), Action::atom(), Payload::map()}
-spec wait_for_event(pid(), atom(), atom()) -> ok | {ok, map()} | timeout.
wait_for_event(ConnPid, Origin, Action) ->
    wait_for_event(ConnPid, Origin, Action, ?DEFAULT_EVENT_TIMEOUT).

-spec wait_for_event(pid(), atom(), atom(), integer()) -> ok | {ok, map()} | timeout.
wait_for_event(ConnPid, Origin, Action, Timeout) ->
    receive
        {ConnPid, websocket_event, Origin, Action, Tag, Payload} ->
            {ok, Tag, Payload};
        {ConnPid, websocket_event, Origin, Action, Payload} ->
            {ok, Payload};
        {ConnPid, websocket_event, Origin, Action} ->
            ok
    after Timeout ->
        timeout
    end.

%% consumes messages from the mailbox:
%% {Sender::pid(), websocket_event, Origin::atom(), Action::atom()}
%% {Sender::pid(), websocket_event, Origin::atom(), Action::atom(), Payload::map()}
-spec wait_for_channel_event(pid(), atom()) -> ok | {ok, map()} | timeout.
wait_for_channel_event(ConnPid, Action) ->
    wait_for_channel_event(ConnPid, Action, ?DEFAULT_EVENT_TIMEOUT).

-spec wait_for_channel_event(pid(), atom(), integer()) -> ok | {ok, map()} | timeout.
wait_for_channel_event(ConnPid, Action, Timeout) ->
    wait_for_event(ConnPid, ?CHANNEL, Action, Timeout).

inform_registered(RegisteredPid, Origin, Action) ->
    inform_registered(RegisteredPid, Origin, Action, undefined, []).

inform_registered(RegisteredPid, Origin, Action, Tag, Payload) ->
    case Payload == [] orelse Payload == #{} of
        true ->
            RegisteredPid ! {self(), websocket_event, Origin, Action};
        false when Tag == undefined ->
            RegisteredPid ! {self(), websocket_event, Origin, Action, Payload};
        false ->
            RegisteredPid ! {self(), websocket_event, Origin, Action, Tag, Payload}
    end.

init(WaitingPid) ->
    Regs = put_registration(WaitingPid, waiting_connected, #{}),
    {once, #state{regs = Regs}}.

onconnect(_WSReq, State) ->
    ct:log("Ws connected"),
    self() ! ping,
    {ok, State}.


ondisconnect({error, {400, <<"Bad Request">>}}, State) ->
    {close, normal, State};
ondisconnect({remote, closed}, State) ->
    ct:log("Connection closed, closing"),
    {close, normal, State}.

websocket_handle({pong, Nonce}, _ConnState, #state{regs=Regs}=State) ->
    case get_registered(waiting_nonce, Regs) of
        [Nonce] ->
            Regs1 = delete_registered(Nonce, waiting_nonce, Regs),
            [WaitingPid] = get_registered(waiting_connected, Regs1),
            inform_registered(WaitingPid, websocket, connected),
            Regs2 = delete_registered(WaitingPid, waiting_connected, Regs1),
            {ok, State#state{regs=Regs2}};
        [] ->
            {ok, State}
    end;
websocket_handle({text, MsgBin}, _ConnState, #state{regs=Regs, role=Role}=State) ->
    Msg = jsx:decode(MsgBin, [return_maps]),
    case Role of
        undefined ->
            ct:log("Received msg ~p~n", [Msg]);
        _ ->
            ct:log("[~p] Received msg ~p~n", [Role, Msg])
    end,
    Origin = binary_to_existing_atom(maps:get(<<"origin">>, Msg,
                                              atom_to_binary(?CHANNEL, utf8)), utf8),
    Action = binary_to_existing_atom(maps:get(<<"action">>, Msg), utf8),
    Tag = maps:get(<<"tag">>, Msg, undefined),
    Payload = maps:get(<<"payload">>, Msg, []),
    Registered = get_registered({Origin, Action}, Regs),
    lists:foreach(
        fun(Pid) -> inform_registered(Pid, Origin, Action, Tag, Payload) end,
        Registered),

    % for easier debugging
    case Registered == [] of
        true -> ct:log("No test registered for this event");
        false -> pass
    end,
    {ok, State}.

websocket_info(ping, _ConnState, #state{regs=Regs}=State) ->
    Nonce = list_to_binary(ref_to_list(make_ref())),
    Regs1 = put_registration(Nonce, waiting_nonce, Regs),
    {reply, {ping, Nonce}, State#state{regs=Regs1}};
websocket_info(stop, _ConnState, _State) ->
    {close, <<>>, "stopped"};
websocket_info({register_test, RegisteredPid, Event}, _ConnState,
                #state{regs=Regs0}=State) ->
    Regs = put_registration(RegisteredPid, Event, Regs0),
    inform_registered(RegisteredPid, registered_test, Event),
    {ok, State#state{regs=Regs}};
websocket_info({unregister_test, RegisteredPid, Event}, _ConnState,
                #state{regs=Regs0}=State) ->
    Regs = delete_registered(RegisteredPid, Event, Regs0),
    inform_registered(RegisteredPid, unregistered_test, Event),
    {ok, State#state{regs=Regs}};
websocket_info({set_role, Role}, _ConnState, #state{role=undefined}=State) ->
    {ok, State#state{role=Role}};
websocket_info({send_to_client, Msg}, _ConnState, #state{role=Role}=State) ->
    case Role of
        undefined ->
            ct:log("Sending to server ~p", [Msg]);
        _ ->
            ct:log("[~p] Sending to server ~p", [Role, Msg])
    end,
    {reply, {text, Msg}, State}.

websocket_terminate(Reason, _ConnState, State) ->
    ct:log("Websocket closed in state ~p wih reason ~p~n",
              [State, Reason]),
    ok.

get_registered(Event, Regs) ->
    maps:get(Event, Regs, []).

put_registration(RegPid, Event, Regs) ->
    Pids0 = get_registered(Event, Regs),
    false = lists:member(RegPid, Pids0), % assert there is no double registration
    maps:put(Event, [RegPid | Pids0], Regs).

delete_registered(RegPid, Event, Regs) ->
    PidsLeft =
        lists:filter(
            fun(Pid) -> Pid =/= RegPid end,
            get_registered(Event, Regs)),
    case PidsLeft == [] of
        true -> maps:remove(Event, Regs);
        false -> maps:put(Event, PidsLeft, Regs)
    end.

encode_params(#{} = Ps) ->
    encode_params(maps:to_list(Ps));
encode_params([{K,V}|T]) ->
    ["?", [str(K),"=",uenc(V)
           | [["&", str(K1), "=", uenc(V1)]
              || {K1, V1} <- T]]];
encode_params([]) ->
    [].

str(A) when is_atom(A) ->
    str(atom_to_binary(A, utf8));
str(S) when is_list(S); is_binary(S) ->
    S.

uenc(I) when is_integer(I) ->
    uenc(integer_to_list(I));
uenc(A) when is_atom(A) ->
    uenc(atom_to_binary(A, utf8));
uenc(V) ->
    http_uri:encode(V).

