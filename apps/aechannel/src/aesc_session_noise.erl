-module(aesc_session_noise).
-behaviour(gen_server).

-include("aesc_codec.hrl").

-export([connect/3
       , accept/2
       , close/1]).

-export([channel_open/2
       , channel_accept/2
       , funding_created/2
       , funding_signed/2
       , funding_locked/2
       , channel_reestablish/2
       , update_deposit/2
       , update_withdrawal/2
       , shutdown/2]).

-export([init/1
       , handle_call/3
       , handle_cast/2
       , handle_info/2
       , terminate/2
       , code_change/3]).

-record(st, {parent :: pid()
           , econn  :: enoise:noise_socket()}).

channel_open       (Session, Msg) -> cast(Session, {msg, ?CH_OPEN     , Msg}).
channel_accept     (Session, Msg) -> cast(Session, {msg, ?CH_ACCEPT   , Msg}).
funding_created    (Session, Msg) -> cast(Session, {msg, ?FND_CREATED , Msg}).
funding_signed     (Session, Msg) -> cast(Session, {msg, ?FND_SIGNED  , Msg}).
funding_locked     (Session, Msg) -> cast(Session, {msg, ?FND_LOCKED  , Msg}).
channel_reestablish(Session, Msg) -> cast(Session, {msg, ?CH_REESTABL , Msg}).
update_deposit     (Session, Msg) -> cast(Session, {msg, ?UPD_DEPOSIT , Msg}).
update_withdrawal  (Session, Msg) -> cast(Session, {msg, ?UPD_WITHDRAW, Msg}).
shutdown           (Session, Msg) -> cast(Session, {msg, ?SHUTDOWN    , Msg}).

close(Session) ->
    cast(Session, close).

-define(GEN_SERVER_OPTS, [{debug, [trace]}]).

%% Connection establishment

connect(Host, Port, Opts) ->
    gen_server:start_link(
      ?MODULE, {self(), {connect, Host, Port, Opts}}, ?GEN_SERVER_OPTS).

accept(Port, Opts) ->
    gen_server:start_link(
      ?MODULE, {self(), {accept, Port, Opts}}, ?GEN_SERVER_OPTS).

init({Parent, Op}) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    St = establish(Op, #st{parent = Parent}),
    gen_server:enter_loop(?MODULE, ?GEN_SERVER_OPTS, St).

establish({accept, Port, Opts}, St) ->
    {ok, LSock} = gen_tcp:listen(Port, tcp_opts(listen, Opts)),
    {ok, TcpSock} = gen_tcp:accept(LSock, tcp_opts(accept, Opts)),
    {ok, EConn} = enoise:accept(TcpSock, enoise_opts(accept, Opts)),
    St#st{econn = EConn};
establish({connect, Host, Port, Opts}, St) ->
    {ok, TcpSock} = gen_tcp:connect(Host, Port, tcp_opts(connect, Opts)),
    {ok, EConn} = enoise:connect(TcpSock, enoise_opts(connect, Opts)),
    St#st{econn = EConn}.


handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast({msg, M, Info}, #st{econn = EConn} = St) ->
    enoise:send(EConn, aesc_codec:enc(M, Info)),
    {noreply, St};
handle_cast(close, St) ->
    {stop, close, St};
handle_cast(_Msg, St) ->
    {noreply, St}.


handle_info({noise, EConn, Data}, #st{econn = EConn, parent = Parent} = St) ->
    {_Type, _Info} = Msg = aesc_codec:dec(Data),
    aesc_fsm:message(Parent, Msg),
    {noreply, St};
handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

cast(P, Msg) ->
    gen_server:cast(P, Msg).


tcp_opts(_Op, Opts) ->
    case lists:keyfind(tcp, 1, Opts) of
        false -> [{active, true}];
        {_, TcpOpts} ->
            TcpOpts
    end.

enoise_opts(_Op, Opts) ->
    lists:keydelete(tcp, 1, Opts).
