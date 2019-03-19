-module(aestratum_handler).

-behaviour(ranch_protocol).
-behaviour(gen_server).

%% TODO: eunit
%% TODO: type spec

%% API
-export([start_link/4]).

%% gen_server.
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2
        ]).

-record(state, {
          socket,
          transport,
          session
         }).

-define(IS_MSG(T), ((T =:= tcp) or (T =:= ssl))).
-define(IS_CLOSE(C), ((C =:= tcp_closed) or (C =:= ssl_closed))).
-define(IS_ERROR(E), ((E =:= tcp_error) or (E =:= ssl_error))).

%% API.

start_link(Ref, Socket, Transport, Opts) ->
	{ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

%% Callbacks.

init({Ref, Socket, Transport, _Opts}) ->
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}, {packet, line},
                                    {keepalive, true}]),
    gen_server:cast(self(), init_session),
    gen_server:enter_loop(?MODULE, [], #state{socket = Socket,
                                              transport = Transport}).

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(init_session, #state{} = State) ->
    %% TODO: subscribe to chain events
    Session = aestratum_session:new(),
    Res = aestratum_session:handle_event({conn, init}, Session),
    result(Res, State).

handle_info({SocketType, _Socket, Data}, State) when ?IS_MSG(SocketType) ->
    handle_socket_data(Data, State);
handle_info({SocketClose, _Socket}, State) when ?IS_CLOSE(SocketClose) ->
	handle_socket_close(State);
handle_info({SocketError, _Socket, Rsn}, State) when ?IS_ERROR(SocketError) ->
    handle_socket_error(Rsn, State);
handle_info(timeout, State) ->
    handle_socket_timeout(State);
%% TODO: handle new key block candidate.
handle_info({chain, Event}, State) ->
    handle_chain_event(Event, State);
handle_info(_Info, State) ->
	{stop, normal, State}.

terminate(_Reason, #state{session = Session}) ->
    aestratum_session:close(Session).

%% Internal functions.

handle_socket_data(Data, #state{socket = Socket, transport = Transport,
                                session = Session} = State) ->
	Res = aestratum_session:handle_event({conn, Data}, Session),
	case is_stop(Res) of
	    true -> ok;
	    false -> Transport:setopts(Socket, [{active, once}])
    end,
    result(Res, State).

handle_socket_close(#state{session = Session} = State) ->
    Res = aestratum_session:handle_event({conn, close}, Session),
    result(Res, State).

handle_socket_error(_Rsn, #state{session = Session} = State) ->
    %% TODO: log error
    Res = aestratum_session:handle_event({conn, close}, Session),
    result(Res, State).

handle_socket_timeout(#state{session = Session} = State) ->
    Res = aestratum_session:handle_event({conn, timeout}, Session),
    result(Res, State).

handle_chain_event(Event, #state{session = Session} = State) ->
    Res = aestratum_session:handle_event({chain, Event}, Session),
    result(Res, State).

result({send, Data, Session},
       #state{socket = Socket, transport = Transport} = State) ->
    case send_data(Data, Socket, Transport) of
        ok ->
            {noreply, State#state{session = Session}};
        {error, _Rsn} ->
            Res = aestratum_session:handle_event({conn, close}, Session),
            result(Res, State)
    end;
result({no_send, Session}, State) ->
    {noreply, State#state{session = Session}};
result({stop, Session}, State) ->
    {stop, normal, State#state{session = Session}}.

is_stop({stop, _Session}) -> true;
is_stop(_Other) -> false.

send_data(Data, Socket, Transport) ->
    Transport:send(Socket, Data).
