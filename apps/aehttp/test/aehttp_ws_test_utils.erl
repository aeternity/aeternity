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
         register_test_for_events/3,
         unregister_test_for_event/3,
         unregister_test_for_events/3,
         register_test_for_channel_event/2,
         register_test_for_channel_events/2,
         register_test_for_all_channel_events/1,
         unregister_test_for_channel_event/2,
         unregister_test_for_channel_events/2,
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

-define(DEFAULT_EVENT_TIMEOUT, 12000).
-define(DEFAULT_SUB_TIMEOUT, 8000).

-define(CHANNEL, channel).

-record(register, {mon_refs = #{} :: map(),
                   events   = #{} :: map()}).

-record(state, {role :: atom() | undefined,
                regs:: #register{}
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
    ct:log("connecting to Channel ~s as ~p", [iolist_to_binary(WsAddress), Role]),
    {ok, Pid} = websocket_client:start(WsAddress, ?MODULE, self()),
    case wait_for_connect(Pid) of
        {ok, Pid} = Res ->
            set_role(Pid, RoleA),
            Res;
        {error, _, _} = Err -> Err
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
    Event = {Origin, [Action]},
    ConnPid ! {register_test, self(), Event},
    ok = wait_for_event(ConnPid, registered_test, Event, ?DEFAULT_SUB_TIMEOUT),
    ok.

register_test_for_events(ConnPid, Origin, Actions) ->
    Event = {Origin, Actions},
    ConnPid ! {register_test, self(), Event},
    ok = wait_for_event(ConnPid, registered_test, Event, ?DEFAULT_SUB_TIMEOUT),
    ok.

unregister_test_for_event(ConnPid, Origin, Action) ->
    Event = {Origin, [Action]},
    ConnPid ! {unregister_test, self(), Event},
    ok = wait_for_event( ConnPid, unregistered_test, Event, ?DEFAULT_SUB_TIMEOUT),
    ok.

unregister_test_for_events(ConnPid, Origin, Actions) ->
    Event = {Origin, Actions},
    ConnPid ! {unregister_test, self(), Event},
    ok = wait_for_event( ConnPid, unregistered_test, Event, ?DEFAULT_SUB_TIMEOUT),
    ok.

register_test_for_channel_event(ConnPid, Action) ->
    register_test_for_events(ConnPid, ?CHANNEL, [Action]).

register_test_for_channel_events(ConnPid, Actions) when is_list(Actions)->
    register_test_for_events(ConnPid, ?CHANNEL, Actions).

register_test_for_all_channel_events(ConnPid) ->
    Actions = aesc_fsm:report_tags(),
    register_test_for_events(ConnPid, ?CHANNEL, Actions).


unregister_test_for_channel_event(ConnPid, Action) ->
    unregister_test_for_events(ConnPid, ?CHANNEL, [Action]).

unregister_test_for_channel_events(ConnPid, Actions) ->
    unregister_test_for_events(ConnPid, ?CHANNEL, Actions).


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
    Register = put_registration(WaitingPid, waiting_connected, #register{}),
    {once, #state{regs = Register}}.

onconnect(_WSReq, State) ->
    ct:log("Ws connected"),
    self() ! ping,
    {ok, State}.


ondisconnect({error, {400, <<"Bad Request">>}}, State) ->
    {close, normal, State};
ondisconnect({remote, closed}, State) ->
    ct:log("Connection closed, closing"),
    {close, normal, State}.

websocket_handle({pong, Nonce}, _ConnState, #state{regs=Register}=State) ->
    case get_registered_pids(waiting_nonce, Register) of
        [Nonce] ->
            Reg1 = delete_registered(Nonce, waiting_nonce, Register),
            [WaitingPid] = get_registered_pids(waiting_connected, Reg1),
            inform_registered(WaitingPid, websocket, connected),
            Reg2 = delete_registered(WaitingPid, waiting_connected, Reg1),
            {ok, State#state{regs=Reg2}};
        [] ->
            {ok, State}
    end;
websocket_handle({text, MsgBin}, _ConnState, #state{regs=Register, role=Role}=State) ->
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
    RegisteredPids = get_registered_pids({Origin, Action}, Register),
    lists:foreach(
        fun(Pid) -> inform_registered(Pid, Origin, Action, Tag, Payload) end,
        RegisteredPids),

    % for easier debugging
    case RegisteredPids == [] of
        true -> ct:log("No test registered for this event");
        false -> pass
    end,
    {ok, State}.

websocket_info(ping, _ConnState, #state{regs=Register}=State) ->
    Nonce = list_to_binary(ref_to_list(make_ref())),
    Register1 = put_registration(Nonce, waiting_nonce, Register),
    {reply, {ping, Nonce}, State#state{regs=Register1}};
websocket_info(stop, _ConnState, _State) ->
    {close, <<>>, "stopped"};
websocket_info({register_test, RegisteredPid, Events}, _ConnState,
                #state{regs=Register0}=State) ->
    {Origin, Actions} = Events,
    Register =
        lists:foldl(
            fun(Action, AccumRegister) ->
                Event = {Origin, Action},
                put_registration(RegisteredPid, Event, AccumRegister)
            end,
            Register0,
            Actions),
    inform_registered(RegisteredPid, registered_test, Events),
    {ok, State#state{regs=Register}};
websocket_info({unregister_test, RegisteredPid, Events}, _ConnState,
                #state{regs=Register0}=State) ->
    {Origin, Actions} = Events,
    Register =
        lists:foldl(
            fun(Action, AccumRegister) ->
                Event = {Origin, Action},
                delete_registered(RegisteredPid, Event, AccumRegister)
            end,
            Register0,
            Actions),
    inform_registered(RegisteredPid, unregistered_test, Events),
    {ok, State#state{regs=Register}};
websocket_info({set_role, Role}, _ConnState, #state{role=undefined}=State) ->
    {ok, State#state{role=Role}};
websocket_info({'DOWN', _Ref, process, Pid, _}, _ConnState,
               #state{regs=Register0}=State) ->
    {ok, State#state{regs= delete_pid(Pid, Register0)}};
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

get_registered_pids(Event, #register{events=Events}) ->
    maps:get(Event, Events, []).

delete_registered_event(Event, #register{events=Events}=R) ->
    R#register{events=maps:remove(Event, Events)}.

set_registered_event(Event, Pids, #register{events=Events}=R) ->
    R#register{events=maps:put(Event, Pids, Events)}.

delete_registered_ref(Pid, #register{mon_refs=Refs}=R) when is_pid(Pid) ->
    case maps:get(Pid, Refs, no_ref) of
        no_ref -> pass;
        Ref -> demonitor(Ref)
    end,
    R#register{mon_refs=maps:remove(Pid, Refs)};
delete_registered_ref(_NotPid, R) -> R.

put_registered_ref(Pid, Ref, #register{mon_refs=Refs}=R) ->
    R#register{mon_refs=maps:put(Pid, Ref, Refs)}.

put_registration(RegPid, Event, Register0) ->
    Pids0 = get_registered_pids(Event, Register0),
    false = lists:member(RegPid, Pids0), % assert there is no double registration
    Register =
        case is_pid(RegPid) of
            false -> Register0;
            true ->
                Ref = monitor(process, RegPid),
                put_registered_ref(RegPid, Ref, Register0)
        end,
    set_registered_event(Event, [RegPid | Pids0], Register).

delete_registered(RegPid, Event, Register) ->
    %% do no demonitor the pid because other events could still be registered
    PidsLeft =
        lists:filter(
            fun(Pid) -> Pid =/= RegPid end,
            get_registered_pids(Event, Register)),
    case PidsLeft == [] of
        true -> delete_registered_event(Event, Register);
        false -> set_registered_event(Event, PidsLeft, Register)
    end.

delete_pid(RegPid, #register{events=Events0} = Register0) ->
    Events =
        maps:fold(
            fun(Event, Pids0, Acc) ->
                case lists:filter(fun(Pid) -> Pid =/= RegPid end, Pids0) of
                    [] -> Acc; % no more pids for that event
                    Pids -> maps:put(Event, Pids)
                end
            end,
            #{},
            Events0),
    Register = delete_registered_ref(RegPid, Register0),
    Register#register{events=Events}.

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

