-module(aestratum_handler).

-behaviour(ranch_protocol).
-behaviour(gen_server).

%% TODO: eunit

%% API
-export([start_link/4,
         set/3,
         status/1
        ]).

%% gen_server.
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2
        ]).

-include("aestratum_log.hrl").

-type ref()         :: term().

-type socket()      :: gen_tcp:socket().

-type transport()   :: gen_tcp
                     | ssl.

-type session()     :: aestratum_session:session() | undefined.

-type opts()        :: proplists:proplist().

-type key()         :: aestratum_session:key().

-type value()       :: aestratum_session:value().

-record(state, {
          socket    :: socket(),
          transport :: transport(),
          session   :: session()
         }).

-define(IS_MSG(T), ((T =:= tcp) or (T =:= ssl))).
-define(IS_CLOSE(C), ((C =:= tcp_closed) or (C =:= ssl_closed))).
-define(IS_ERROR(E), ((E =:= tcp_error) or (E =:= ssl_error))).

%% API.

-spec start_link(ref(), socket(), transport(), opts()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
	{ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

-spec set(key(), value(), pid()) -> ok.
set(Key, Val, Pid) ->
    gen_server:cast(Pid, {set, Key, Val}).

-spec status(pid()) -> map().
status(Pid) ->
    gen_server:call(Pid, status).

%% Callbacks.

init({Ref, Socket, Transport, _Opts}) ->
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}, {packet, line},
                                    {keepalive, true}]),
    {ok, {Ip, Port}} = inet:peername(Socket),
    ?INFO("new_client_connection, ip: ~p, port: ~p", [Ip, Port]),
    gen_server:cast(self(), init_session),
    gen_server:enter_loop(?MODULE, [], #state{socket = Socket,
                                              transport = Transport}).

handle_call(status, _From, State) ->
    Reply = handle_status(State),
    {reply, Reply, State}.

handle_cast(init_session, #state{} = State) ->
    Session = aestratum_session:new(),
    Res = aestratum_session:handle_event({conn, #{event => init}}, Session),
    result(Res, State);
handle_cast({set, Key, Val}, #state{session = Session} = State) when
      Session =/= undefined ->
    Session1 = aestratum_session:set(Key, Val, Session),
    {noreply, State#state{session = Session1}};
handle_cast({set, _Key, _Val}, #state{session = undefined} = State) ->
    {noreply, State}.

handle_info({SocketType, _Socket, Data}, State) when ?IS_MSG(SocketType) ->
    handle_socket_data(Data, State);
handle_info({SocketClose, _Socket}, State) when ?IS_CLOSE(SocketClose) ->
	handle_socket_close(State);
handle_info({SocketError, _Socket, Rsn}, State) when ?IS_ERROR(SocketError) ->
    handle_socket_error(Rsn, State);
handle_info({conn, _Event} = ConnEvent, State) ->
    handle_conn_event(ConnEvent, State);
handle_info({chain, _Event} = ChainEvent, State) ->
    handle_chain_event(ChainEvent, State);
handle_info(_Info, State) ->
	{stop, normal, State}.

terminate(_Rsn, #state{session = Session}) when Session =/= undefined ->
    aestratum_session:close(Session);
terminate(_Rsn, _State) ->
    ok.

%% Internal functions.

handle_socket_data(Data, #state{socket = Socket, transport = Transport,
                                session = Session} = State) ->
    Event = #{event => recv_data, data => Data},
	Res = aestratum_session:handle_event({conn, Event}, Session),
	case is_stop(Res) of
	    true -> ok;
	    false -> Transport:setopts(Socket, [{active, once}])
    end,
    result(Res, State).

handle_socket_close(#state{session = Session} = State) ->
    ?INFO("socket_close", []),
    Res = aestratum_session:handle_event({conn, #{event => close}}, Session),
    result(Res, State).

handle_socket_error(Rsn, #state{session = Session} = State) ->
    ?ERROR("socket_error, reason: ~p", [Rsn]),
    Res = aestratum_session:handle_event({conn, #{event => close}}, Session),
    result(Res, State).

handle_conn_event(ConnEvent, #state{session = Session} = State) ->
    Res = aestratum_session:handle_event(ConnEvent, Session),
    result(Res, State).

handle_chain_event(ChainEvent, #state{session = Session} = State) ->
    Res = aestratum_session:handle_event(ChainEvent, Session),
    result(Res, State).

handle_status(#state{socket = Socket, transport = Transport,
                     session = Session}) ->
    #{conn => #{socket => Socket, transport => Transport},
      session => aestratum_session:status(Session)}.

result({send, Data, Session},
       #state{socket = Socket, transport = Transport} = State) ->
    case send_data(Data, Socket, Transport) of
        ok ->
            {noreply, State#state{session = Session}};
        {error, Rsn} ->
            ?ERROR("socket_send, reason: ~p, data: ~p", [Rsn, Data]),
            Event = #{event => close},
            Res = aestratum_session:handle_event({conn, Event}, Session),
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
