-module(aehttp_ws_test_utils).
-behaviour(websocket_client).

%% API
-export([start_link/2,
         start_link_channel/4,
         start_channel/4,
         start_channel/5,
         set_log_file/2,
         set_role/2,
         log/3,
         send/3, send/4, send/5,
         send_tagged/4,
         get_registered_events/1,
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
         wait_for_msg/4,
         wait_for_channel_msg/2,
         wait_for_any_channel_msg/1,
         wait_for_channel_msg_event/3,
         wait_for_connect/1,
         wait_for_connect_any/0,
         json_rpc_notify/2,
         json_rpc_call/2,
         json_rpc_call/3,
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
-define(DEFAULT_SUB_TIMEOUT, 10000).

-define(CHANNEL, channel).

-record(register, {mon_refs = #{} :: map(),
                   events   = #{} :: map()}).

-record(state, {role :: atom() | undefined,
                log :: string() | undefined,
                ws_address :: string() | undefined,
                regs:: #register{},
                calls = [] :: [{non_neg_integer(), pid()}],
                protocol :: json_rpc
               }).

start_link(Host, Port) ->
    % TODO: Make it configurable whether to use `ws` or `wss`
    WsAddress = "wss://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/websocket",
    ct:log("connecting to ~p", [WsAddress]),
    {ok, Pid} = websocket_client:start_link(WsAddress, ?MODULE, {self(), []}),
    wait_for_connect(Pid).

start_link_channel(Host, Port, RoleA, Opts) when is_atom(RoleA) ->
    Role = to_binary(RoleA),
    WsAddress = make_channel_connect_address(Host, Port, Role, Opts),
    ct:log("connecting to Channel ~p as ~p", [WsAddress, Role]),
    {ok, Pid} = websocket_client:start_link(WsAddress, ?MODULE, {self(), []},
                                            extra_headers()),
    wait_for_connect(Pid).

start_channel(Host, Port, RoleA, Opts0) when is_atom(RoleA) ->
    start_channel(Host, Port, RoleA, [], Opts0).

start_channel(Host, Port, RoleA, DefaultChannelActions, Opts0) when is_atom(RoleA) ->
    Role = to_binary(RoleA),
    {LogFile, Opts} = get_logfile(Opts0),
    WsAddress = make_channel_connect_address(Host, Port, Role, Opts),
    ct:log("connecting to Channel ~s as ~p", [iolist_to_binary(WsAddress), Role]),
    %% There is no websocket_client:start/4 ...
    {ok, Pid} = websocket_client:start_link(WsAddress, ?MODULE, {self(), DefaultChannelActions},
                                            extra_headers()),
    unlink(Pid),
    case wait_for_connect(Pid) of
        {ok, Pid} = Res ->
            set_options(Pid, #{role => RoleA,
                               logfile => LogFile,
                               ws_address => WsAddress,
                               opts => Opts}),
            Res;
        {error, _} = Err -> Err
    end.

extra_headers() ->
    [{extra_headers, [{<<"Content-Type">>, <<"application/json">>}]}].


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
    try wait_for_event(Pid, websocket, connected, ?DEFAULT_SUB_TIMEOUT) of
        ok ->
            {ok, Pid}
    catch
        error:{timeout, _} ->
            case process_info(Pid) of
                undefined -> {error, rejected};
                _ -> {error, {still_connecting, Pid}}
            end;
        error:{connection_died,_} ->
            {error, rejected}
    end.

set_role(ConnPid, Role) when is_atom(Role) ->
    set_options(ConnPid, #{role => Role}).

set_log_file(ConnPid, LogFile) ->
    ConnPid ! {set_logfile, LogFile}.

log(ConnPid, Type, Info) when Type==info;
                              Type==init;
                              Type==send;
                              Type==recv ->
    ConnPid ! {log, Type, Info},
    ok.

set_options(ConnPid, Opts) when is_map(Opts) ->
    ConnPid ! {set_options, Opts}.

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

json_rpc_notify(ConnPid, Msg) ->
    ConnPid ! { send_to_client, jsx:encode(Msg#{ <<"jsonrpc">> => <<"2.0">> }) },
    ok.

json_rpc_call(ConnPid, Req) ->
    json_rpc_call(ConnPid, Req, ?DEFAULT_EVENT_TIMEOUT).

json_rpc_call(ConnPid, Req, Timeout) when is_map(Req) ->
    Id = erlang:unique_integer([monotonic]),
    MRef = erlang:monitor(process, ConnPid),
    ConnPid ! { json_rpc, self(), Id, jsx:encode(Req#{ <<"jsonrpc">> => <<"2.0">>
                                                     , <<"id">>      => Id }) },
    try
        receive
            { ConnPid, #{ <<"jsonrpc">> := <<"2.0">>
                        , <<"id">>      := Id
                        , <<"result">>  := Result } } ->
                Result;
            { ConnPid, #{ <<"jsonrpc">> := <<"2.0">>
                        , <<"id">>      := Id
                        , <<"error">>   := Error } } ->
                erlang:error({json_rpc_error, Error});
            {'DOWN', MRef, _, _, Reason} ->
                erlang:error({connpid_died, Reason})
        after Timeout ->
                erlang:error(timeout)
        end
    after
        erlang:demonitor(MRef)
    end.


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
    try ok = wait_for_event( ConnPid, unregistered_test, Event,
                             ?DEFAULT_SUB_TIMEOUT)
    catch error:{connection_died, _} -> pass
    end,
    ok.

unregister_test_for_events(ConnPid, Origin, Actions) ->
    Event = {Origin, Actions},
    ConnPid ! {unregister_test, self(), Event},
    try ok = wait_for_event( ConnPid, unregistered_test, Event,
                             ?DEFAULT_SUB_TIMEOUT)
    catch error:{connection_died, _} -> pass
    end,
    ok.

register_test_for_channel_event(ConnPid, Action) when is_atom(Action) ->
    register_test_for_events(ConnPid, ?CHANNEL, [Action]).

register_test_for_channel_events(ConnPid, Actions) when is_list(Actions)->
    register_test_for_events(ConnPid, ?CHANNEL, Actions).

register_test_for_all_channel_events(ConnPid) ->
    FSMActions = aesc_fsm:report_tags(),
    register_test_for_events(ConnPid, ?CHANNEL, [system | FSMActions]).


unregister_test_for_channel_event(ConnPid, Action) ->
    unregister_test_for_events(ConnPid, ?CHANNEL, [Action]).

unregister_test_for_channel_events(ConnPid, Actions) ->
    unregister_test_for_events(ConnPid, ?CHANNEL, Actions).

get_registered_events(ConnPid) ->
    Me = self(),
    ConnPid ! {get_registered_events, Me},
    wait_for_msg(msg, ConnPid, registered_events, Me, ?DEFAULT_EVENT_TIMEOUT).

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

wait_for_event(ConnPid, Origin, Action) ->
    wait_for_event(ConnPid, Origin, Action, ?DEFAULT_EVENT_TIMEOUT).

wait_for_event(ConnPid, Origin, Action, Timeout) ->
    wait_for_msg(payload, ConnPid, Origin, Action, Timeout).

%% Whereas wait_for_event() only returns the <<"payload">> value, wait_for_msg()
%% returns the whole msg.
wait_for_msg(ConnPid, Origin, Action, Timeout) ->
    wait_for_msg(msg, ConnPid, Origin, Action, Timeout).

wait_for_any_msg(ConnPid, Origin, Timeout) ->
    wait_for_any_msg(msg, ConnPid, Origin, Timeout).

%% consumes messages from the mailbox:
%% {Sender::pid(), websocket_event, Origin::atom(), Action::atom()}
%% {Sender::pid(), websocket_event, Origin::atom(), Action::atom(), Payload::map()}
%% {Sender::pid(), websocket_event, Origin::atom(), Action::atom(), Tag::term(), Payload::map()}
-spec wait_for_msg(msg | payload, pid(), atom(), atom(), integer()) ->
    ok | {ok, map()} | {ok, Tag::term(), map()}.
wait_for_msg(Type, ConnPid, Origin, Action, Timeout) ->
    MRef = monitor(process, ConnPid),
    try
        receive
            {ConnPid, websocket_event, Origin, Action, Tag, Msg} ->
                {ok, Tag, msg_data(Type, Msg)};
            {ConnPid, websocket_event, Origin, Action, Msg} ->
                {ok, msg_data(Type, Msg)};
            {ConnPid, websocket_event, Origin, Action} ->
                ok;
            {'DOWN', MRef, _, _, Reason} ->
                error({connection_died, Reason})
        after
            Timeout ->
                error({timeout, process_info(self(), messages)})
        end
    after
        demonitor(MRef)
    end.

-spec wait_for_any_msg(msg | payload, pid(), atom(), integer()) ->
    ok | {ok, map()} | {ok, atom(), Tag::term(), map()}.
wait_for_any_msg(Type, ConnPid, Origin, Timeout) ->
    MRef = monitor(process, ConnPid),
    try
        receive
            {ConnPid, websocket_event, Origin, Action, Tag, Msg} ->
                {ok, Action, Tag, msg_data(Type, Msg)};
            {ConnPid, websocket_event, Origin, Action, Msg} ->
                {ok, Action, msg_data(Type, Msg)};
            {ConnPid, Action, websocket_event, Origin, Action} ->
                ok;
            {'DOWN', MRef, _, _, Reason} ->
                error({connection_died, Reason})
        after
            Timeout ->
                error({timeout, process_info(self(), messages)})
        end
    after
        demonitor(MRef)
    end.

-spec wait_for_msg_event(msg | payload, pid(), atom(), atom(), binary(), integer()) ->
    ok | {ok, map()} | {ok, Tag::term(), map()}.
wait_for_msg_event(Type, ConnPid, Origin, Action, Event, Timeout) ->
    MRef = erlang:monitor(process, ConnPid),
    try receive
            {ConnPid, websocket_event, Origin, Action, Tag,
             #{<<"params">> := #{ <<"data">> := #{ <<"event">> := Event }}} = Msg} ->
                {ok, Tag, msg_data(Type, Msg)};
            {ConnPid, websocket_event, Origin, Action,
             #{<<"params">> := #{ <<"data">> := #{ <<"event">> := Event }}} = Msg} ->
                {ok, msg_data(Type, Msg)};
            {'DOWN', MRef, _, _, Reason} ->
                error({connection_died, Reason})
        after Timeout ->
                erlang:error({timeout, process_info(self(), messages)})
        end
    after
        erlang:demonitor(MRef)
    end.


wait_for_channel_msg(ConnPid, Action) ->
    wait_for_msg(ConnPid, ?CHANNEL, Action, ?DEFAULT_EVENT_TIMEOUT).

wait_for_any_channel_msg(ConnPid) ->
    wait_for_any_msg(ConnPid, ?CHANNEL, ?DEFAULT_EVENT_TIMEOUT).

wait_for_channel_msg_event(ConnPid, Action, Event) ->
    wait_for_msg_event(msg, ConnPid, ?CHANNEL, Action, Event, ?DEFAULT_EVENT_TIMEOUT).

msg_data(payload, #{<<"payload">> := P}) -> P;
msg_data(payload, Msg) -> Msg;
msg_data(msg, Msg) -> Msg.

%% consumes messages from the mailbox:
%% {Sender::pid(), websocket_event, Origin::atom(), Action::atom()}
%% {Sender::pid(), websocket_event, Origin::atom(), Action::atom(), Payload::map()}
-spec wait_for_channel_event(pid(), atom()) -> ok | {ok, map()} | timeout.
wait_for_channel_event(ConnPid, Action) ->
    wait_for_channel_event(ConnPid, Action, ?DEFAULT_EVENT_TIMEOUT).

-spec wait_for_channel_event(pid(), atom(), integer()) -> ok | {ok, map()} | timeout.
wait_for_channel_event(ConnPid, Action, Timeout) ->
    wait_for_event(ConnPid, ?CHANNEL, Action, Timeout).


inform(Origin, Action, #state{} = State) ->
    inform(Origin, Action, undefined, [], State).

inform(Origin, Action, Tag, Payload, #state{regs = Register}) ->
    ct:log("inform(~p, ~p, ~p, ~p)", [Origin, Action, Tag, Payload]),
    RegisteredPids = get_registered_pids({Origin, Action}, Register),
    lists:foreach(
      fun(Pid) -> inform_registered(Pid, Origin, Action, Tag, Payload) end,
      RegisteredPids),
    %% for easier debugging
    case RegisteredPids == [] of
        true -> ct:log("No test registered for this event");
        false -> pass
    end.


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

init({WaitingPid, DefaultChannelActions}) ->
    Register0 = put_registration(WaitingPid, waiting_connected, #register{}),
    Register1 =
        lists:foldl(
            fun(Action, AccumRegister) ->
                Event = {?CHANNEL, Action},
                put_registration(WaitingPid, Event, AccumRegister)
            end,
            Register0,
            DefaultChannelActions),
    {once, #state{regs = Register1}}.

onconnect(_WSReq, #state{regs=Register}=State) ->
    ct:log("Ws connected"),
    [WaitingPid] = get_registered_pids(waiting_connected, Register),
    inform_registered(WaitingPid, websocket, connected),
    Reg = delete_registered(WaitingPid, waiting_connected, Register),
    self() ! ping,
    {ok, State#state{regs=Reg}}.


ondisconnect({error, {400, <<"Bad Request">>}}, State) ->
    {close, normal, State};
ondisconnect({remote, closed}, State) ->
    ct:log("Connection closed, closing"),
    {close, normal, State}.

websocket_handle({pong, Nonce}, _ConnState, #state{regs=Register}=State) ->
    case get_registered_pids(waiting_nonce, Register) of
        [Nonce] ->
            Reg1 = delete_registered(Nonce, waiting_nonce, Register),
            {ok, State#state{regs=Reg1}};
        [] ->
            {ok, State}
    end;
websocket_handle({text, MsgBin}, _ConnState, #state{role=Role}=State) ->
    Msg = jsx:decode(MsgBin, [return_maps]),
    do_log(recv, MsgBin, State),
    maybe_produce_info_from_msg(Msg, State),
    case Role of
        undefined ->
            ct:log("Received msg ~p~n", [Msg]);
        _ ->
            ct:log("[~p] Received msg ~p~n", [Role, Msg])
    end,
    websocket_handle_enc(Msg, State).

websocket_handle_enc(#{ <<"jsonrpc">> := <<"2.0">>
                      , <<"id">>      := null
                      , <<"error">>   := _ } = Msg, #state{regs=Register} = State) ->
    maybe_inform(origin(Msg), error, undefined, Msg, Register),
    {ok, State};
websocket_handle_enc(#{ <<"jsonrpc">> := <<"2.0">>
                      , <<"id">>      := Id } = Msg, #state{calls=Calls}=State) ->
    case lists:keyfind(Id, 1, Calls) of
        {_, Pid} = Match ->
            Pid ! {self(), Msg},
            {ok, State#state{calls = Calls -- [Match]}};
        false ->
            erlang:error({json_rpc_call_mismatch, Msg})
    end;
websocket_handle_enc(Msg, #state{regs=Register}=State) when is_map(Msg) ->
    Origin = to_atom(maps:get(<<"origin">>, Msg, to_binary(?CHANNEL))),
    {Action, Tag} = get_action_tag(Msg),
    ct:log("Origin = ~p, Action = ~p, Tag = ~p", [Origin, Action, Tag]),
    ct:log("Register = ~p", [Register]),
    %% Action = binary_to_atom(maps:get(<<"action">>, Msg), utf8),
    %% Tag = maps:get(<<"tag">>, Msg, undefined),
    maybe_inform(Origin, Action, Tag, Msg, Register),
    {ok, State}.

maybe_inform(Origin, Action, Tag, Msg, Register) ->
    RegisteredPids = get_registered_pids({Origin, Action}, Register),
    lists:foreach(
      fun(Pid) -> inform_registered(Pid, Origin, Action, Tag, Msg) end,
      RegisteredPids),
    %% for easier debugging
    case RegisteredPids == [] of
        true -> ct:log("No test registered for this event (Msg = ~p)", [Msg]);
        false -> pass
    end.


websocket_info(ping, _ConnState, #state{regs=Register}=State) ->
    Nonce = list_to_binary(ref_to_list(make_ref())),
    Register1 = put_registration(Nonce, waiting_nonce, Register),
    {reply, {ping, Nonce}, State#state{regs=Register1}};
websocket_info(stop, _ConnState, _State) ->
    {close, <<>>, "stopped"};
websocket_info({get_registered_events, Pid}, _ConnState, #state{regs=Regs}=State) ->
    Events = get_events_for_pid(Pid, Regs),
    inform_registered(Pid, registered_events, Pid, undefined, Events),
    {ok, State};
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
websocket_info({set_options, #{ role := Role
                              , logfile := LogFile
                              , ws_address := WsAddress
                              , opts := Opts }}, _ConnState, #state{ log  = Log0
                                                                   , role = undefined
                                                                   , protocol = Proto0 }=State) ->
    Log1 = open_log(Log0, LogFile, Role),
    State1 = State#state{role     = Role,
                         log      = Log1,
                         protocol = get_protocol(Opts, Proto0)},
    do_log(init, WsAddress, State1),
    {ok, State1};
websocket_info({set_logfile, LogFile}, _ConnState, #state{ role = Role
                                                         , log  = Log0 } = State) ->
    Log1 = open_log(Log0, LogFile, Role),
    State1 = State#state{log = Log1},
    {ok, State1};
websocket_info({log, Type, Msg}, _ConnState, State) ->
    do_log(Type, Msg, State),
    {ok, State};
websocket_info({'DOWN', _Ref, process, Pid, _}, _ConnState,
               #state{regs=Register0}=State) ->
    {ok, State#state{regs= delete_pid(Pid, Register0)}};
websocket_info({json_rpc, Pid, Id, Req}, _ConnState, #state{calls = Calls}=State) ->
    do_log(send, Req, State),
    ct:log("Sending JSON-RPC req: ~p", [Req]),
    {reply, {text, Req}, State#state{calls = [{Id, Pid}|Calls]}};
websocket_info({send_to_client, Msg}, _ConnState, #state{role=Role}=State) ->
    do_log(send, Msg, State),
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
    inform(websocket, closed, State),
    ok.

origin(Msg) ->
    to_atom(maps:get(<<"origin">>, Msg, to_binary(?CHANNEL))).

get_action_tag(#{<<"jsonrpc">> := _, <<"method">> := Method}) ->
    case binary:split(Method, [<<".">>], [global]) of
        [<<"channels">>, Action] ->
            {to_atom(Action), undefined};
        [<<"channels">>, Action, <<"reply">>] ->
            {to_atom(Action), undefined};   % "reply" doesn't count as a tag (bw compat reasons)
        [<<"channels">>, Action, Tag | _] ->
            {to_atom(Action), Tag};
        _ ->
            {undefined, undefined}
    end;
get_action_tag(#{<<"action">> := Action} = Msg) ->
    {to_atom(Action), maps:get(<<"tag">>, Msg, undefined)}.

to_atom(A) when is_atom(A)  -> A;
to_atom(B) when is_binary(B)-> binary_to_atom(B, utf8).

to_binary(A) when is_atom(A)   -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) -> B.

get_events_for_pid(Pid, #register{events = Events}) ->
    maps:filter(fun(_Event, Pids) ->
                        lists:member(Pid, Pids)
                end, Events).

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

put_registration(RegPid, Event, Register) ->
    Pids0 = get_registered_pids(Event, Register),
    case is_pid(RegPid) of
        false -> Register;
        true ->
            case lists:member(RegPid, Pids0) of
                true -> Register;
                false ->
                    Ref = monitor(process, RegPid),
                    Register1 = put_registered_ref(RegPid, Ref, Register),
                    set_registered_event(Event, [RegPid | Pids0], Register1)
            end
    end.

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
                    Pids -> maps:put(Event, Pids, Acc)
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
    str(to_binary(A));
str(S) when is_list(S); is_binary(S) ->
    S.

uenc(I) when is_integer(I) ->
    uenc(integer_to_list(I));
uenc(A) when is_atom(A) ->
    uenc(to_binary(A));
uenc(V) ->
    http_uri:encode(V).

get_protocol(#{protocol := Proto}, _) ->
    to_atom(Proto);
get_protocol(_, Default) ->
    Default.

get_logfile(Opts) ->
    case maps:take({int,logfile}, Opts) of
        error ->
            {undefined, Opts};
        {F, Opts1} ->
            {F, Opts1}
    end.

open_log(_, undefined, _) ->
    undefined;
open_log(#{name := Name, role := R, pid := LogPid} = L, F, R) ->
    case filename:basename(F) of
        Name ->
            L;
        _Other ->
            %% This can happen e.g. if a channel is set up during
            %% init_per_group()
            LogPid ! {self(), close},
            open_log(undefined, F, R)
    end;
open_log(undefined, F, Role) ->
    filelib:ensure_dir(F),
    Name = filename:basename(F),
    Parent = self(),
    Pid = spawn_link(
            fun() ->
                    MRef = erlang:monitor(process, Parent),
                    process_flag(trap_exit, true),
                    {ok, _} = disk_log:open([{name, Name},
                                             {file, F},
                                             {format, external}]),
                    Parent ! {self(), ok},
                    logger_loop(Parent, #{ log    => Name
                                         , role   => Role
                                         , parent => Parent
                                         , mref   => MRef })
            end),
    receive
        {Pid, ok} ->
            #{name => Name, pid => Pid, role => Role}
    end.

logger_loop(Parent, #{ log    := Log
                     , parent := Parent
                     , mref   := MRef } = St) ->
    case receive Msg -> Msg end of
        {Parent, log, LogMsg} ->
            disk_log:blog(Log, LogMsg);
        {Parent, close} ->
            disk_log:close(Log);
        {'EXIT', Parent, Reason} ->
            logger_parent_died('EXIT', Reason, St);
        {'DOWN', MRef, process, _, Reason} ->
            logger_parent_died('DOWN', Reason, St)
    end,
    logger_loop(Parent, St).

logger_parent_died(Cat, Reason, #{ log    := Log
                                 , parent := Parent
                                 , role   := Role }) ->
    ct:log("Logger's parent (~p) died (~p): ~p", [Parent, Cat, Reason]),
    LogMsg = log_msg(disc, [], Role),
    disk_log:blog(Log, LogMsg),
    disk_log:close(Log),
    exit(normal).

do_log(_Dir, _Msg, #state{log = undefined}) ->
    ok;
do_log(info, Msg, #state{log = Log, role = Role}) ->
    log_pid(Log) ! {self(), log, log_msg(info, Msg, Role)};
do_log(Dir, Msg, #state{log = Log, role = Role}) ->
    log_pid(Log) ! {self(), log, log_msg(Dir, Msg, Role)}.

log_pid(#{pid := Pid}) ->
    Pid.

log_msg(info, Msg, Role) ->
    io_lib:format("~n"
                  "#### ~w info~n"
                  "> ~s~n", [Role, Msg]);
log_msg(disc = Dir, _Msg, Role) ->
    io_lib:format(
      "~n"
      "#### ~s~n", [log_header_str(Dir, Role)]);
log_msg(Dir, Msg, Role) ->
    {Lang, MsgStr} = try {"javascript", jsx:prettify(Msg)}
                     catch error:_ -> {"", Msg}
                     end,
    io_lib:format(
      "~n"
      "#### ~s~n"
      "```~s~n"
      "~s~n"
      "```~n", [log_header_str(Dir, Role), Lang, MsgStr]).

log_header_str(Dir, Role) ->
    {Fmt, Args} =
        case Dir of
            send -> {"~w ---> node", [Role]};
            recv -> {"~w <--- node", [Role]};
            init -> {"~w opens a WebSocket connection", [Role]};
            disc -> {"~w closes WebSocket connection", [Role]};
            info -> {"~w info (~s)", [Role]}
        end,
    io_lib:format(Fmt, Args).

maybe_produce_info_from_msg(#{<<"params">> := #{<<"data">> := #{<<"event">> := Event}}},
                            S) ->
    maybe_produce_info_for_event(Event, S);
maybe_produce_info_from_msg(_Msg, _S) ->
    pass.

maybe_produce_info_for_event(<<"fsm_up">>, S) ->
    do_log(info, "The local fsm has been started", S);
maybe_produce_info_for_event(<<"channel_open">>, S) ->
    do_log(info, "Received an WebSocket opening request", S);
maybe_produce_info_for_event(<<"channel_accept">>, S) ->
    do_log(info, "Received an WebSocket connection accepted", S);
maybe_produce_info_for_event(<<"own_funding_locked">>, S) ->
    do_log(info, "Funding has been confirmed locally on-chain", S);
maybe_produce_info_for_event(<<"funding_locked">>, S) ->
    do_log(info, "Funding has been confirmed on-chain by other party", S);
maybe_produce_info_for_event(<<"open">>, S) ->
    do_log(info, "Channel is `open` and ready to use", S);
maybe_produce_info_for_event(_, _S) ->
    pass.
