-module(aesc_session_noise).
-behaviour(gen_server).

-include("aesc_codec.hrl").
-include_lib("aeutils/include/aeu_stacktrace.hrl").

-export([connect/3
       , accept/2
       , close/1]).

-export([channel_open/2
       , channel_accept/2
       , funding_created/2
       , funding_signed/2
       , funding_locked/2
       , deposit_created/2
       , deposit_signed/2
       , deposit_locked/2
       , channel_reestablish/2
       , channel_reestablish_ack/2
       , update/2
       , update_ack/2
       , update_error/2
       , dep_created/2
       , dep_signed/2
       , dep_locked/2
       , dep_error/2
       , wdraw_created/2
       , wdraw_signed/2
       , wdraw_locked/2
       , wdraw_error/2
       , error/2
       , inband_msg/2
       , leave/2
       , leave_ack/2
       , shutdown/2
       , shutdown_ack/2
       , shutdown_error/2]).

-export([init/1
       , handle_call/3
       , handle_cast/2
       , handle_info/2
       , terminate/2
       , code_change/3]).

-export([ patterns/0
        , record_fields/1]).

-export([start_link/1]).

-record(st, { init_op     :: op()
            , fsm         :: undefined | pid()
            , fsm_mon_ref :: undefined | reference()
            , econn }).

-define(ACCEPT_TIMEOUT, 2*60*1000).  %% 2 minutes

-type responder_accept_opts() :: #{ reestablish   => false
                                  , initiator := aec_keys:pubkey() | any
                                  , responder := aec_keys:pubkey()
                                  , port      := port() }.
-type responder_reestabl_opts() :: #{ reestablish := true
                                    , chain_hash  := binary()
                                    , channel_id  := binary()
                                    , responder   := aec_keys:pubkey()
                                    , port        := port() }.
-type session_opts() :: responder_accept_opts()
                      | responder_reestabl_opts().
-type host() :: inet:socket_address() | inet:hostname().
-type port_number() :: inet:port_number().
-type opts() :: list().  %% TODO: improve type spec
-type op() :: {accept, session_opts(), opts()}
            | {connect, host(), port_number(), opts()}.

channel_open       (Session, Msg) -> cast(Session, {msg, ?CH_OPEN      , Msg}).
channel_accept     (Session, Msg) -> cast(Session, {msg, ?CH_ACCEPT    , Msg}).
funding_created    (Session, Msg) -> cast(Session, {msg, ?FND_CREATED  , Msg}).
funding_signed     (Session, Msg) -> cast(Session, {msg, ?FND_SIGNED   , Msg}).
funding_locked     (Session, Msg) -> cast(Session, {msg, ?FND_LOCKED   , Msg}).
deposit_created    (Session, Msg) -> cast(Session, {msg, ?DEP_CREATED  , Msg}).
deposit_signed     (Session, Msg) -> cast(Session, {msg, ?DEP_SIGNED   , Msg}).
deposit_locked     (Session, Msg) -> cast(Session, {msg, ?DEP_LOCKED   , Msg}).
channel_reestablish(Session, Msg) -> cast(Session, {msg, ?CH_REESTABL  , Msg}).
channel_reestablish_ack(Sn , Msg) -> cast(Sn     , {msg, ?CH_REEST_ACK , Msg}).
update             (Session, Msg) -> cast(Session, {msg, ?UPDATE       , Msg}).
update_ack         (Session, Msg) -> cast(Session, {msg, ?UPDATE_ACK   , Msg}).
update_error       (Session, Msg) -> cast(Session, {msg, ?UPDATE_ERR   , Msg}).
dep_created        (Session, Msg) -> cast(Session, {msg, ?DEP_CREATED  , Msg}).
dep_signed         (Session, Msg) -> cast(Session, {msg, ?DEP_SIGNED   , Msg}).
dep_locked         (Session, Msg) -> cast(Session, {msg, ?DEP_LOCKED   , Msg}).
dep_error          (Session, Msg) -> cast(Session, {msg, ?DEP_ERR      , Msg}).
wdraw_created      (Session, Msg) -> cast(Session, {msg, ?WDRAW_CREATED, Msg}).
wdraw_signed       (Session, Msg) -> cast(Session, {msg, ?WDRAW_SIGNED , Msg}).
wdraw_locked       (Session, Msg) -> cast(Session, {msg, ?WDRAW_LOCKED , Msg}).
wdraw_error        (Session, Msg) -> cast(Session, {msg, ?WDRAW_ERR    , Msg}).
error              (Session, Msg) -> cast(Session, {msg, ?ERROR        , Msg}).
inband_msg         (Session, Msg) -> cast(Session, {msg, ?INBAND_MSG   , Msg}).
leave              (Session, Msg) -> cast(Session, {msg, ?LEAVE        , Msg}).
leave_ack          (Session, Msg) -> cast(Session, {msg, ?LEAVE_ACK    , Msg}).
shutdown           (Session, Msg) -> cast(Session, {msg, ?SHUTDOWN     , Msg}).
shutdown_ack       (Session, Msg) -> cast(Session, {msg, ?SHUTDOWN_ACK , Msg}).
shutdown_error     (Session, Msg) -> cast(Session, {msg, ?SHUTDOWN_ERR , Msg}).

close(Session) ->
    try call(Session, close)
    ?_catch_(E, R, StackTrace)
        case {R, E} of
            {error, _} ->
                ok;
            {exit, {noproc, _}} ->
                unlink(Session),
                ok;
            {exit, R} ->
                lager:error("Unhandled exit error during session closing: ~p, ~p", [R, StackTrace]),
                unlink(Session),
                ok
        end
    end.

-define(GEN_SERVER_OPTS, []).

%% ==================================================================
%% for tracing
-define(EXCEPTION_TRACE, {'_', [], [{exception_trace}]}).
patterns() ->
    [{?MODULE, '_', '_', [?EXCEPTION_TRACE]}].
    %% exports(?MODULE).

%% exports(M) ->
%%     [{M, F, A, [?EXCEPTION_TRACE]} || {F,A} <- M:module_info(exports)].


record_fields(st) -> record_info(fields, st);
record_fields(_ ) -> no.
%% ==================================================================

%% Connection establishment

connect(Host, Port, Opts) ->
    start_link(#{fsm => self(), op => {connect, Host, Port, Opts}}).

accept(#{ port := Port, responder := R } = Opts0, NoiseOpts) ->
    TcpOpts = tcp_opts(listen, NoiseOpts),
    lager:debug("listen: TcpOpts = ~p", [TcpOpts]),
    {ok, LSock} = aesc_listeners:listen(Port, R, TcpOpts),
    Opts = Opts0#{lsock => LSock},
    accept_(Opts, NoiseOpts).

accept_(#{ reestablish := true
         , chain_hash  := ChainH
         , channel_id  := ChId
         , responder   := R
         , port        := Port } = Opts, NoiseOpts) ->
    gproc:reg(responder_reestabl_regkey(ChainH, ChId, R, Port)),
    {ok, _Pid} = aesc_sessions_sup:start_child(
                   [#{fsm => self(), op => {accept, Opts, NoiseOpts}}]);
accept_(#{ initiator := I, responder := R, port := Port } = Opts, NoiseOpts) ->
    gproc:reg(responder_regkey(R, I, Port)),
    aesc_sessions_sup:start_child([#{fsm => self(), op => {accept, Opts, NoiseOpts}}]).

start_link(Arg) when is_map(Arg) ->
    %% Res = gen_server:start_link(?MODULE, Arg#{parent => self()}, ?GEN_SERVER_OPTS),
    %% We don't use gen_server:start_link/3, since we want to use gen_server:enter_loop/3
    %% in the init/1 function, and we want to avoid redundant `init_ack()` messages
    %% sent by the `gen_server:init_it/6 function (a different kind of support has been
    %% introduced in later versions of OTP.)
    Res = proc_lib:start_link(?MODULE, init, [Arg#{parent => self()}]),
    lager:debug("Res = ~p", [Res]),
    Res.

init(#{fsm := Fsm, parent := Parent, op := Op} = Arg) ->
    lager:debug("Arg = ~p", [Arg]),
    %% trap exits to avoid ugly crash reports. We rely on the monitor to
    %% ensure that we close when the fsm dies
    %%
    process_flag(trap_exit, true),
    proc_lib:init_ack(Parent, {ok, self()}),
    FsmMonRef = monitor(process, Fsm),
    St = establish(Op, #st{ init_op = Op
                          , fsm = Fsm
                          , fsm_mon_ref = FsmMonRef }),
    %% As the monitor was created and the session established we don't need the link anymore
    unlink(Fsm),
    gen_server:enter_loop(?MODULE, ?GEN_SERVER_OPTS, St).

establish({accept, #{responder := R, port := Port} = Opts, NoiseOpts}, St) ->
    LSock = maps:get(lsock, Opts),
    lager:debug("LSock = ~p", [LSock]),
    AcceptTimeout = proplists:get_value(accept_timeout, NoiseOpts, ?ACCEPT_TIMEOUT),
    {ok, TcpSock} = accept_tcp(LSock, Port, R, AcceptTimeout),
    lager:debug("Accept TcpSock = ~p", [TcpSock]),
    %% TODO: extract/check something from FinalState?
    EnoiseOpts = enoise_opts(accept, NoiseOpts),
    lager:debug("EnoiseOpts (accept) = ~p", [EnoiseOpts]),
    {ok, EConn, _FinalSt} = enoise:accept(TcpSock, EnoiseOpts),
    %% At this point, we should de-couple from the parent fsm and instead
    %% attach to the fsm we eventually pair with, once the `CH_OPEN`
    %% (or `CH_REESTABL`) message arrives from the other side.
    erlang:demonitor(St#st.fsm_mon_ref),
    St#st{econn = EConn,
          fsm = undefined,
          fsm_mon_ref = undefined};
establish({connect, Host, Port, Opts}, St) ->
    TcpOpts = tcp_opts(connect, Opts),
    lager:debug("connect: TcpOpts = ~p", [TcpOpts]),
    {ok, TcpSock} =
        connect_tcp(Host, Port, St#st.fsm_mon_ref, TcpOpts),
    lager:debug("Connect TcpSock = ~p", [TcpSock]),
    %% TODO: extract/check something from FinalState?
    EnoiseOpts = enoise_opts(connect, Opts),
    lager:debug("EnoiseOpts (connect) = ~p", [EnoiseOpts]),
    {ok, EConn, _FinalSt} = enoise:connect(TcpSock, EnoiseOpts),
    St#st{econn = EConn}.

accept_tcp(LSock, Port, R, AcceptTimeout) ->
    case gen_tcp:accept(LSock, AcceptTimeout) of
        {ok, _} = Ok ->
            Ok;
        {error, timeout} ->
            case aesc_listeners:lsock_info(LSock, [port, responders]) of
                #{ port := Port, responders := Rs } ->
                    case lists:member(R, Rs) of
                        true ->
                            lager:debug("Still listeners on this port, loop", []),
                            accept_tcp(LSock, Port, R, AcceptTimeout);
                        false ->
                            lager:debug("No listeners for ~p on this port", [R]),
                            exit(normal)
                    end;
                _ ->
                    exit(normal)
            end;
        Error ->
            error(Error)
    end.

connect_tcp(Host, Port, Ref, Opts) ->
    {Timeout, Retries} = get_reconnect_params(),
    connect_tcp(Retries, Host, Port, Ref, Opts, Timeout).

connect_tcp(0, _, _, _, _, _) ->
    erlang:error(connect_timeout);
connect_tcp(Retries, Host, Port, Ref, Opts, Timeout)
  when Retries > 0 ->
    case gen_tcp:connect(Host, Port, Opts, Timeout) of
        {ok, _TcpSock} = Ok ->
            Ok;
        {error, _} ->
            sleep(Timeout, Ref),
            connect_tcp(Retries-1, Host, Port, Ref, Opts, Timeout)
    end.

sleep(T, Ref) ->
    receive
        {'$gen_call', From, close} = Msg ->
            lager:debug("Got ~p while sleeping", [Msg]),
            gen:reply(From, ok),
            exit(normal);
        {'DOWN', Ref, _, _, _} = Msg ->
            lager:debug("Got ~p while sleeping", [Msg]),
            exit(shutdown)
    after T ->
            ok
    end.

-ifdef(TEST).
get_reconnect_params() ->
    %% The increased granuality decreases the runtime of some tests by half :)
    {50, 20}. %% Up to a second in the test environment
-else.
get_reconnect_params() ->
    %% {ConnectTimeout, Retries}
    %% max 4 minutes (up to 40 retries,
    %%  where each retry is up to 3 seconds TCP connection timeout and 3 seconds sleep time).
    {3000, 40}.
-endif.

handle_call(close, _From, #st{econn = EConn} = St) ->
    lager:debug("got close request", []),
    close_econn(EConn),
    {stop, normal, ok, St};
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast(Msg, St) ->
    try handle_cast_(Msg, St)
    ?_catch_(error, Reason, Trace)
        lager:error("CAUGHT error:~p trace: ~p", [Reason, Trace]),
        erlang:error(Reason, Trace)
    end.

handle_cast_({msg, M, Info}, #st{econn = EConn} = St) ->
    enoise:send(EConn, aesc_codec:enc(M, Info)),
    {noreply, St};
handle_cast_(_Msg, St) ->
    {noreply, St}.

%% FSM had died
handle_info({'DOWN', Ref, process, Pid, _Reason},
            #st{fsm_mon_ref=Ref, fsm=Pid, econn=EConn}=St) ->
    lager:debug("Got DOWN from FSM (~p)", [Pid]),
    close_econn(EConn),
    {stop, shutdown, St};
handle_info(Msg, St) ->
    try handle_info_(Msg, St)
    ?_catch_(error, Reason, Trace)
        lager:error("CAUGHT error:~p trace: ~p", [Reason, Trace]),
        erlang:error(Reason, Trace)
    end.

handle_info_({noise, EConn, Data}, #st{econn = EConn, fsm = Fsm} = St) ->
    {Type, Info} = Msg = aesc_codec:dec(Data),
    St1 = case {Type, Fsm} of
              {?CH_OPEN, undefined} ->
                  locate_fsm(Type, Info, St);
              {?CH_REESTABL, undefined} ->
                  locate_fsm(Type, Info, St);
              _ ->
                  St
          end,
    tell_fsm(Msg, St1),
    enoise:set_active(EConn, once),
    {noreply, St1};
handle_info_({tcp_closed, _Port}, #st{fsm = Pid} = St) ->
    aesc_fsm:connection_died(Pid),
    {stop, normal, St};
handle_info_(_Msg, St) ->
    lager:debug("Unknown handle_info_(~p)", [_Msg]),
    {noreply, St}.

terminate(_Reason, #st{econn = EConn}) ->
    close_econn(EConn),
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

locate_fsm(Type, MInfo, #st{ init_op = {accept, SnInfo, _Opts} } = St) ->
    Info = maps:merge(SnInfo, MInfo),  % any duplicates overridden by what was received
    Cands = get_cands(Type, Info),
    lager:debug("Cands = ~p", [Cands]),
    try_cands(Cands, 3, 0, Type, Info, St).

get_cands(?CH_OPEN, #{ initiator := I
                     , responder := R
                     , port      := Port }) ->
    %% we expect to get at most one R+I pair match, and possibly multiple
    %% matches on R+any. In the ordered_set gproc table, the 'any' matches
    %% will come first, but we want the explicit I match to take precedence,
    %% so we reverse the result. This ought to be faster than performing
    %% two select operations.
    lists:reverse(
      gproc:select(
        {l,p},
        [{ {responder_regkey(R, '$1', Port), '_', '_'},
           [{'orelse',
             {'=:=', '$1', I},
             {'=:=', '$1', any}}], ['$_'] }]));
get_cands(?CH_REESTABL, #{ chain_hash := Chain
                         , channel_id := ChId
                         , responder  := R
                         , port := Port}) ->
    gproc:select({l,p},
                 [{ {responder_reestabl_regkey(Chain, ChId, R, Port), '_', '_'},
                    [], ['$_'] }]).

responder_reestabl_regkey(Chain, ChId, R, Port) ->
    {p, l, {?MODULE, accept_reestabl, Chain, ChId, R, Port}}.

responder_regkey(R, I, Port) ->
    {p, l, {?MODULE, accept, R, I, Port}}.

try_cands([{K, Pid, _} | Cands], Tries, Races, Type, Info, St) ->
    case aesc_fsm:attach_responder(Pid, Info#{ gproc_key => K }) of
        {error, taken} ->
            lager:debug("Couldn't attach to ~p: taken", [Pid]),
            try_cands(Cands, Tries, Races+1, Type, Info, St);
        {error, _} = E ->
            lager:debug("Couldn't attach to ~p: ~p", [Pid, E]),
            try_cands(Cands, Tries, Races, Type, Info, St);
        ok ->
            lager:debug("Attached to ~p", [Pid]),
            MRef = erlang:monitor(process, Pid),
            St#st{ fsm = Pid
                 , fsm_mon_ref = MRef }
    end;
try_cands([], 0, 0, _, _, _) ->
    erlang:error(cannot_locate_fsm);
try_cands([], Tries, 0, Type, Info, St) ->
    lager:debug("Exhausted candidates (no races); retrying ...", []),
    receive after 100 -> ok end,
    try_cands(get_cands(Type, Info), Tries-1, 0, Type, Info, St);
try_cands([], Tries, Races, Type, Info, St) ->
    lager:debug("Exhausted candidates (Races = ~p); retrying at once", [Races]),
    try_cands(get_cands(Type, Info), Tries, 0, Type, Info, St).

close_econn(undefined) ->
    ok;
close_econn(EConn) ->
    try enoise:close(EConn)
    catch _:_ -> ok
    end.

cast(P, Msg) ->
    lager:debug("to noise session ~p: ~p", [P, Msg]),
    gen_server:cast(P, Msg).

call(P, Msg) ->
    lager:debug("Call to noise session ~p: ~p", [P, Msg]),
    gen_server:call(P, Msg).

tell_fsm({_, _} = Msg, #st{fsm = Fsm}) ->
    aesc_fsm:message(Fsm, Msg).

tcp_opts(_Op, Opts) ->
    case lists:keyfind(tcp, 1, Opts) of
        false ->
            tcp_defaults();
        {_, TcpOpts} ->
            [Opt || Opt <- tcp_defaults(), not tcp_opt_member(Opt, TcpOpts)]
            ++ TcpOpts
    end.

noise_defaults() ->
    [{noise, <<"Noise_XK_25519_ChaChaPoly_BLAKE2b">>}].

tcp_defaults() ->
    [ {active, true}
    , {reuseaddr, true}
    , {mode, binary}
    ].

tcp_opt_member({mode,M}, L) ->
    %% handle supported short forms 'binary' and 'list'
    lists:keymember(mode, 1, L) orelse lists:member(M, L);
tcp_opt_member({K,_}, L) ->
    lists:keymember(K, 1, L).

enoise_opts(_Op, Opts0) ->
    Opts = lists:keydelete(tcp, 1, Opts0),
    [Opt || {K,_} = Opt <- noise_defaults(), not lists:keymember(K, 1, Opts)]
    ++ Opts.
