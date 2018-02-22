-module(aehttp_ws_test_utils).
-behaviour(websocket_client).

%% API
-export([start_link/2,
         send/4, send/5,
         register_test_for_event/3,
         unregister_test_for_event/3,
         wait_for_event/2,
         wait_for_event/3,
         wait_for_connect/1,
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

start_link(Host, Port) ->
    WsAddress = "ws://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/websocket",
    ct:log("connecting to ~p", [WsAddress]),
    {ok, Pid} = websocket_client:start_link(WsAddress, ?MODULE, self()),
    wait_for_connect(Pid).

wait_for_connect(Pid) ->
    case wait_for_event(websocket, connected, ?DEFAULT_SUB_TIMEOUT) of
        ok ->
            {ok, Pid};
        timeout ->
            case process_info(Pid) of
                undefined -> {error, rejected};
                _ -> {error, still_connecting, Pid}
            end
    end.

stop(ConnPid) ->
    ok = websocket_client:stop(ConnPid).

send(ConnPid, Target, Action, Tag, Payload) ->
    send_(ConnPid, Target, Action, Payload, #{tag => Tag}).

send(ConnPid, Target, Action, Payload) ->
    send_(ConnPid, Target, Action, Payload, #{}).

send_(ConnPid, Target, Action, Payload, Msg0) ->
    Msg1 = Msg0#{target => Target,
                 action => Action},
    Msg =
        case Payload == [] orelse Payload == #{} of
            true  -> Msg1;
            false -> maps:put(payload, Payload, Msg1)
        end,
    ct:log("Sending to server ~p", [Msg]),
    websocket_client:cast(ConnPid, {text, jsx:encode(Msg)}),
    ok.

%% when an WS event occours, the registered process will receive a message in
%% the mailbox. Message structure will be:
%% {websocket_event, Origin::atom(), Action::atom()}
%% {websocket_event, Origin::atom(), Action::atom(), Payload::map()}
%% where the Origin and Action are the same as the ones for which the process
%% had registered. This message is consumed by wait_for_event/2,3
register_test_for_event(ConnPid, Origin, Action) ->
    Event = {Origin, Action},
    ConnPid ! {register_test, self(), Event},
    ok = wait_for_event(registered_test, Event, ?DEFAULT_SUB_TIMEOUT),
    ok.

unregister_test_for_event(ConnPid, Origin, Action) ->
    Event = {Origin, Action},
    ConnPid ! {unregister_test, self(), Event},
    ok = wait_for_event(unregistered_test, Event, ?DEFAULT_SUB_TIMEOUT),
    ok.


%% consumes messages from the mailbox:
%% {websocket_event, Origin::atom(), Action::atom()}
%% {websocket_event, Origin::atom(), Action::atom(), Payload::map()}
-spec wait_for_event(atom(), atom()) -> ok | {ok, map()} | timeout.
wait_for_event(Origin, Action) ->
    wait_for_event(Origin, Action, ?DEFAULT_EVENT_TIMEOUT).

-spec wait_for_event(atom(), atom(), integer()) -> ok | {ok, map()} | timeout.
wait_for_event(Origin, Action, Timeout) ->
    receive
        {websocket_event, Origin, Action, Tag, Payload} ->
            {ok, Tag, Payload};
        {websocket_event, Origin, Action, Payload} ->
            {ok, Payload};
        {websocket_event, Origin, Action} ->
            ok
    after Timeout ->
        timeout
    end.

inform_registered(RegisteredPid, Origin, Action) ->
    inform_registered(RegisteredPid, Origin, Action, undefined, []).

inform_registered(RegisteredPid, Origin, Action, Tag, Payload) ->
    case Payload == [] orelse Payload == #{} of
        true ->
            RegisteredPid ! {websocket_event, Origin, Action};
        false when Tag == undefined ->
            RegisteredPid ! {websocket_event, Origin, Action, Payload};
        false ->
            RegisteredPid ! {websocket_event, Origin, Action, Tag, Payload}
    end.

init(WaitingPid) ->
    Regs = put_registration(WaitingPid, waiting_connected, #{}),
    {once, Regs}.

onconnect(_WSReq, Regs) ->
    ct:log("Ws connected"),
    [WaitingPid] = get_registered(waiting_connected, Regs),
    Regs1 = delete_registered(WaitingPid, waiting_connected, Regs),
    inform_registered(WaitingPid, websocket, connected),
    {ok, Regs1}.


ondisconnect({error, {400, <<"Bad Request">>}}, Regs) ->
    {close, normal, Regs};
ondisconnect({remote, closed}, Regs) ->
    ct:log("Connection closed, closing"),
    {close, "closed", Regs}.

websocket_handle({pong, _}, _ConnState, Regs) ->
    {ok, Regs};
websocket_handle({text, MsgBin}, _ConnState, Regs) ->
    Msg = jsx:decode(MsgBin, [return_maps]),
    ct:log("Received msg ~p~n", [Msg]),
    Origin = binary_to_existing_atom(maps:get(<<"origin">>, Msg), utf8),
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
    {ok, Regs}.

websocket_info(stop, _ConnState, _Regs) ->
    {close, <<>>, "stopped"};
websocket_info({register_test, RegisteredPid, Event}, _ConnState, Regs0) ->
    Regs = put_registration(RegisteredPid, Event, Regs0),
    inform_registered(RegisteredPid, registered_test, Event),
    {ok, Regs};
websocket_info({unregister_test, RegisteredPid, Event}, _ConnState, Regs0) ->
    Regs = delete_registered(RegisteredPid, Event, Regs0),
    inform_registered(RegisteredPid, unregistered_test, Event),
    {ok, Regs}.

websocket_terminate(Reason, _ConnState, Regs) ->
    ct:log("Websocket closed in state ~p wih reason ~p~n",
              [Regs, Reason]),
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

