%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(aesc_session_noise).

-behaviour(gen_server).

-include("aesc_codec.hrl").

%% ==================================================================
%% Process API
-export([ connect/3                 % called by FSM
        , accept/2                  % called by FSM
        , close/1
        , start_generic_acceptor/3  % called by acceptor pool
        , start_link/1
        ]).

%% ==================================================================
%% Protocol API
-export([ channel_open/2
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
        , generic_error/2
        , inband_msg/2
        , leave/2
        , leave_ack/2
        , shutdown/2
        , shutdown_ack/2
        , shutdown_error/2
        ]).

%% ==================================================================
%% gen_server API
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%% ==================================================================
%% helpers
-export([ patterns/0
        , record_fields/1
        , pp_msg/1
        ]).

-record(st, { init_op     :: op()
            , fsm         :: undefined | pid()
            , fsm_mon_ref :: undefined | reference()
            , econn }).

-define(ACCEPT_TIMEOUT, 2*60*1000).  %% 2 minutes

-type responder_accept_opts() :: #{ reestablish   => false
                                  , initiator := aec_keys:pubkey() | any
                                  , responder := aec_keys:pubkey()
                                  , lsock     := term()
                                  , port      := port() }.
-type responder_reestabl_opts() :: #{ reestablish := true
                                    , chain_hash  := binary()
                                    , channel_id  := binary()
                                    , lsock       := term()
                                    , responder   := aec_keys:pubkey()
                                    , port        := port() }.
-type session_opts() :: responder_accept_opts()
                      | responder_reestabl_opts().
-type host() :: inet:socket_address() | inet:hostname().
-type port_number() :: inet:port_number().
-type opts() :: list().  %% TODO: improve type spec
-type op() :: {accept, session_opts(), opts()}
            | {connect, host(), port_number(), opts()}.

%% ==================================================================
%% Process API

start_link(Arg) when is_map(Arg) ->
    gen_server:start_link(?MODULE, Arg, []).

%% Called by the initiator FSM. This starts a connector process.
%% The connector will 
connect(Host, Port, Opts) ->
    StartOpts = [#{fsm => self(), op => {connect, Host, Port, Opts}}],
    aesc_session_noise_sup:start_child(StartOpts).

%% Called by the responder FSM. The FSM doesn't actually start an acceptor, but
%% simply register with a special Gproc key so that the automatically started
%% acceptors (see `start_generic_acceptor/3') can find them.
%%
accept(#{ port := Port } = Opts, NoiseOpts) ->
    case aesc_listeners:ensure_listener(Port, NoiseOpts) of
        {ok, _} ->
            register_responder(Opts);
        {error, _} = Error ->
            Error
    end.

start_generic_acceptor(Port, LSock, Opts) ->
    TcpOpts = tcp_opts(listen, Opts),
    NoiseOpts = proplists:get_value(noise, Opts, noise_defaults()),
    lager:debug("generic acceptor: Opts = ~p; TcpOpts = ~p", [Opts, TcpOpts]),
    Op = {accept, #{port => Port, lsock => LSock}, NoiseOpts},
    StartOpts = #{ type => generic_acceptor, port => Port, op => Op
                  , noise => NoiseOpts},
    start_link(StartOpts).

close(undefined) ->
    ok;
close(Session) ->
    try call(Session, close)
    catch E:R:StackTrace ->
        case {E, R} of
            {error, _} ->
                ok;
            {exit, {noproc, _}} ->
                unlink(Session),
                ok;
            {exit, {normal,_}} ->
                ok;
            {exit, R} ->
                lager:error("Unexpected exit error during session closing: ~p, ~p", [R, StackTrace]),
                unlink(Session),
                kill_session(Session),
                ok
        end
    end.

-spec kill_session(pid() | port()) -> 'ok'.
kill_session(Session) ->
    MRef = erlang:monitor(process, Session),
    exit(Session, kill),
    receive
        {'DOWN', MRef, _, _, _} ->
            ok
    end.

%% ==================================================================
%% Protocol API

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
generic_error      (Session, Msg) -> cast(Session, {msg, ?ERROR        , Msg}).
inband_msg         (Session, Msg) -> cast(Session, {msg, ?INBAND_MSG   , Msg}).
leave              (Session, Msg) -> cast(Session, {msg, ?LEAVE        , Msg}).
leave_ack          (Session, Msg) -> cast(Session, {msg, ?LEAVE_ACK    , Msg}).
shutdown           (Session, Msg) -> cast(Session, {msg, ?SHUTDOWN     , Msg}).
shutdown_ack       (Session, Msg) -> cast(Session, {msg, ?SHUTDOWN_ACK , Msg}).
shutdown_error     (Session, Msg) -> cast(Session, {msg, ?SHUTDOWN_ERR , Msg}).

%% ==================================================================
%% helpers
-define(EXCEPTION_TRACE, {'_', [], [{exception_trace}]}).
patterns() ->
    [{?MODULE, '_', '_', [?EXCEPTION_TRACE]}].
    %% exports(?MODULE).

%% exports(M) ->
%%     [{M, F, A, [?EXCEPTION_TRACE]} || {F,A} <- M:module_info(exports)].


record_fields(st) -> record_info(fields, st);
record_fields(_ ) -> no.

%% ==================================================================
%% gen_server API

init(#{type := generic_acceptor, port := Port, op := Op}) ->
    lager:debug("generic_acceptor for port ~p", [Port]),
    St = #st{ init_op = Op},
    cast(self(), post_init),
    {ok, St};
init(#{fsm := Fsm, op := Op} = Arg) ->
    lager:debug("Arg = ~p", [Arg]),
    %% trap exits to avoid ugly crash reports. We rely on the monitor to
    %% ensure that we close when the fsm dies
    process_flag(trap_exit, true),
    FsmMonRef = monitor(process, Fsm),
    St = #st{ init_op = Op
            , fsm = Fsm
            , fsm_mon_ref = FsmMonRef },
    %% The work to establish the connection is postponed until after process
    %% setup is finished. This is such that the caller is not blocked as the TCP
    %% connection setup can take some time.
    cast(self(), post_init),
    {ok, St}.

handle_call(close, _From, #st{econn = EConn} = St) ->
    lager:debug("got close request", []),
    close_econn(EConn),
    {stop, normal, ok, St};
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast(post_init, #st{fsm = Fsm, init_op = Op} = St) ->
    case establish(Op, St) of
        {ok, St1} ->
            %% As the monitor was created and the session established we don't
            %% need the link anymore.
            if is_pid(Fsm) ->
                    unlink(Fsm);
               true ->
                    ok
            end,
            {noreply, St1};
        {error, Err} ->
            %% If we can't establish a connection the process must terminate to
            %% inform the FSM properly.
            {stop, Err, St}
    end;
handle_cast(Msg, St) ->
    try
        handle_cast_(Msg, St)
    catch error:Reason:Trace ->
        lager:error("CAUGHT error:~p trace: ~p", [Reason, Trace]),
        error(Reason, Trace)
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
    try
        handle_info_(Msg, St)
    catch error:Reason:Trace ->
        lager:error("CAUGHT error:~p trace: ~p", [Reason, Trace]),
        error(Reason, Trace)
    end.

handle_info_({noise, EConn, Data}, #st{econn = EConn, fsm = Fsm} = St) ->
    {Type, Info} = Msg = aesc_codec:dec(Data),
    lager:debug("Msg = ~p", [pp_msg(Msg)]),
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

%% Lager Debugging: The following code reduces logging noise related to hashes and keys
%% Given that lager does compile-time transformation of the code, these functions are only
%% called when there will actually be output.

pp_msg({Tag, M}) when is_atom(Tag), is_map(M) ->
    {Tag, pp_msg(M)};
pp_msg({Tag1, Tag2, M}) when is_atom(Tag1), is_atom(Tag2), is_map(M) ->
    {Tag1, Tag2, pp_msg(M)};
pp_msg(M) when is_map(M) ->
    maps:map(fun pp_elem/2, M);
pp_msg(M) ->
    M.

pp_elem(K, Hash) when K == chain_hash
                    ; K == initiator
                    ; K == responder
                    ; K == temporary_channel_id
                    ; K == block_hash
                    ; K == channel_id ->
    abbrev_hash(Hash);
pp_elem(data, D) ->
    case D of
        #{tx := Hash} ->
            D#{tx := abbrev_hash(Hash)};
        _ ->
            D
    end;
pp_elem(_, X) ->
    X.

abbrev_hash(H) when byte_size(H) > 12 ->
    First = binary:part(H, 0, 5),
    Last  = binary:part(H, byte_size(H), -5),
    <<First/binary, 0,0,0,0, Last/binary>>;
abbrev_hash(H) ->
    H.

%% End Lager Debugging

%% ==================================================================
%% gen_server API (internal)

locate_fsm(Type, Msg, #st{init_op = {accept, #{port := Port}, _}} = St) ->
    Info = case {Type, Msg} of
               {?CH_OPEN, #{ chain_hash := _
                           , initiator  := _
                           , responder  := _ }} ->
                   Msg#{port => Port};
               {?CH_REESTABL, #{ chain_hash := ChainHash
                               , channel_id := ChanId }} ->
                   #{ reestablish => true
                    , chain_hash => ChainHash
                    , channel_id => ChanId
                    , port       => Port };
               _ ->
                   error(protocol_error)
           end,
    Cands = get_cands(Type, Info),
    lager:debug("Cands = ~p", [Cands]),
    try_cands(Cands, 5, 0, Type, Info, St).

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
                         , port := Port}) ->
    gproc:select({l,p},
                 [{ {responder_reestabl_regkey(Chain, ChId, Port), '_', '_'},
                    [], ['$_'] }]).

responder_reestabl_regkey(Chain, ChId, Port) ->
    K = {p, l, {?MODULE, accept_reestabl, Chain, ChId, Port}},
    lager:debug("RegKey = ~p", [K]),
    K.

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
    error(cannot_locate_fsm);
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
    lager:debug("to noise session ~p: ~p", [P, pp_msg(Msg)]),
    gen_server:cast(P, Msg).

call(P, Msg) ->
    lager:debug("Call to noise session ~p: ~p", [P, pp_msg(Msg)]),
    gen_server:call(P, Msg).

tell_fsm({_, _} = Msg, #st{fsm = Fsm}) ->
    aesc_fsm:message(Fsm, Msg).

%% ==================================================================
%% Process API (internal)

register_responder(#{ reestablish := true
                    , chain_hash  := ChainH
                    , channel_id  := ChId
                    , port        := Port }) ->
    Regkey = responder_reestabl_regkey(ChainH, ChId, Port),
    lager:debug("Regkey = ~p", [Regkey]),
    gproc:reg(Regkey),
    ok;
register_responder(#{ initiator := I, responder := R, port := Port }) ->
    Regkey = responder_regkey(R, I, Port),
    lager:debug("Regkey = ~p", [Regkey]),
    gproc:reg(Regkey),
    ok.

establish({accept, #{port := Port, lsock := LSock}, NoiseOpts}, St) ->
    lager:debug("LSock = ~p", [LSock]),
    case accept_tcp(LSock, Port) of
        {ok, TcpSock} ->
            lager:debug("Accept TcpSock = ~p", [TcpSock]),
            %% Since we have a connection, no need to hold up the acceptor pool
            aesc_acceptors:worker_done(Port),
            EnoiseOpts = enoise_opts(accept, NoiseOpts),
            lager:debug("EnoiseOpts (accept) = ~p", [EnoiseOpts]),
            case enoise:accept(TcpSock, EnoiseOpts) of
                {ok, EConn, _FinalSt} ->
                    St1 = St#st{ econn = EConn },
                    {ok, St1};
                Err1 ->
                    Err1
            end;
Err ->
    Err
    end;
establish({connect, Host, Port, Opts}, St) ->
    TcpOpts = tcp_opts(connect, Opts),
    lager:debug("connect: TcpOpts = ~p", [TcpOpts]),
    {Timeout, Retries} = get_reconnect_params(),
    establish_connect(Retries, Host, Port, TcpOpts, Opts, Timeout, St).

establish_connect(0, _Host, _Port, _TcpOpts, _Opts, _Timeout, _St) ->
    {error, connect_timeout};
establish_connect(Retries, Host, Port, TcpOpts, Opts, Timeout, St)
  when Retries > 0 ->
    case gen_tcp:connect(Host, Port, TcpOpts, Timeout) of
        {ok, TcpSock} ->
            lager:debug("Connect TcpSock = ~p", [TcpSock]),
            %% TODO: extract/check something from FinalState?
            EnoiseOpts = enoise_opts(connect, Opts),
            lager:debug("EnoiseOpts (connect) = ~p", [EnoiseOpts]),
            case enoise:connect(TcpSock, EnoiseOpts) of
                {ok, EConn, _FinalSt} ->
                    aesc_fsm:noise_connected(St#st.fsm),
                    St1 = St#st{econn = EConn},
                    {ok, St1};
                Err1 ->
                    lager:debug("Failed to pair with responder: ~p", [Err1]),
                    sleep_retry_connect(Retries-1, Host, Port, TcpOpts, Opts, Timeout, St)
            end;
        Err ->
            lager:debug("TCP connect failure: ~p", [Err]),
            sleep_retry_connect(Retries-1, Host, Port, TcpOpts, Opts, Timeout, St)
    end.

sleep_retry_connect(Retries, Host, Port, TcpOpts, Opts, Timeout, St) ->
    sleep(Timeout, St#st.fsm_mon_ref),
    establish_connect(Retries, Host, Port, TcpOpts, Opts, Timeout, St).

accept_tcp(LSock, Port) ->
    case gen_tcp:accept(LSock) of
        {ok, _} = Ok ->
            Ok;
        {error, Reason} ->
            lager:debug("Error from accept (~p); terminate: ~p", [Port, Reason]),
            {error, shutdown}
    end.

sleep(T, Ref) ->
    receive
        {'$gen_call', From, close} ->
            lager:debug("Got close request from ~p while sleeping", [From]),
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
    %% The increased granularity decreases the runtime of some tests by half :)
    {50, 20}. %% Up to a second in the test environment
-else.
get_reconnect_params() ->
    %% {ConnectTimeout, Retries}
    %% max 4 minutes (up to 40 retries,
    %%  where each retry is up to 3 seconds TCP connection timeout and 3 seconds sleep time).
    {3000, 40}.
-endif.

tcp_opts(_Op, Opts) ->
    case lists:keyfind(tcp, 1, Opts) of
        false ->
            tcp_defaults();
        {_, TcpOpts} ->
            [Opt || Opt <- tcp_defaults(), not tcp_opt_member(Opt, TcpOpts)]
            ++ TcpOpts
    end.

noise_defaults() ->
    [{noise, <<"Noise_NN_25519_ChaChaPoly_BLAKE2b">>}].

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
