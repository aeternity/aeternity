%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module encapsulating a P2P communication channel
%%% @end
%%%=============================================================================
-module(aec_peer_connection).

-behaviour(ranch_protocol).

%% Functions for incoming connections
-export([ start_link/4 % for ranch_protocol behaviour
        , accept_init/4
        ]).

%% Functions for outgoing connections
-export([ connect/1
        , disconnect/1
        , connect_start_link/2 % for aec_peer_connection_sup
        ]).

%% API functions
-export([ get_generation/2
        , get_generation/3
        , get_header_by_hash/2
        , get_header_by_height/3
        , get_n_successors/4
        , ping/1
        , send_block/2
        , send_tx/2
        , stop/1
        ]).

%% API Mempool sync
-export([ tx_pool_sync_finish/2
        , tx_pool_sync_get/2
        , tx_pool_sync_init/1
        , tx_pool_sync_unfold/2
        ]).

-include("aec_peer_messages.hrl").

-behaviour(gen_server).

-import(aeu_debug, [pp/1]).

-define(P2P_PROTOCOL_VSN, 3).

-define(DEFAULT_CONNECT_TIMEOUT, 1000).
-define(DEFAULT_FIRST_PING_TIMEOUT, 30000).
-define(DEFAULT_NOISE_HS_TIMEOUT, 5000).
-define(DEFAULT_CLOSE_TIMEOUT, 3000).
-define(REQUEST_TIMEOUT, 5000).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% -- BEHAVIOUR ranch_protocol CALLBACKS -------------------------------------

start_link(Ref, Socket, Transport, Opts) ->
    Args = [Ref, Socket, Transport, Opts],
    {ok, proc_lib:spawn_link(?MODULE, accept_init, Args)}.

%% -- API --------------------------------------------------------------------

connect(#{} = Options) ->
    epoch_sync:debug("New peer connection to ~p", [Options]),
    aec_peer_connection_sup:start_peer_connection(Options).

connect_start_link(Port, Opts) ->
    Opts1 = Opts#{ ext_sync_port => Port },
    gen_server:start_link(?MODULE, [Opts1], []).

%% Asynchronously close the connection, optionally notifying aec_peers.
disconnect(PeerId) ->
    cast(PeerId, disconnect).

ping(PeerId) ->
    call(PeerId, {ping, []}).

get_header_by_hash(PeerId, Hash) ->
    call(PeerId, {get_header_by_hash, [Hash]}).

get_header_by_height(PeerId, Height, TopHash) ->
    call(PeerId, {get_header_by_height, [Height, TopHash]}).

get_n_successors(PeerId, FromHash, TargetHash, N) ->
    call(PeerId, {get_n_successors, [FromHash, TargetHash, N]}).

get_generation(PeerId, Hash) ->
    get_generation(PeerId, Hash, backward).

get_generation(PeerId, Hash, Dir) ->
    call(PeerId, {get_generation, [Hash, Dir]}).

tx_pool_sync_init(PeerId) ->
    call(PeerId, {tx_pool, [sync_init]}).

tx_pool_sync_unfold(PeerId, Unfolds) ->
    call(PeerId, {tx_pool, [sync_unfold, Unfolds]}).

tx_pool_sync_get(PeerId, TxHashes) ->
    call(PeerId, {tx_pool, [sync_get, TxHashes]}).

tx_pool_sync_finish(PeerId, Done) ->
    call(PeerId, {tx_pool, [sync_finish, Done]}).

send_tx(PeerId, Tx) ->
    cast(PeerId, {send_tx, Tx}).

send_block(PeerId, Tx) ->
    cast(PeerId, {send_block, Tx}).

stop(PeerCon) ->
    gen_server:cast(PeerCon, stop).


call(PeerCon, Call) when is_pid(PeerCon) ->
    try gen_server:call(PeerCon, Call, ?REQUEST_TIMEOUT + 2000)
    catch
      exit:{normal, _}  -> {error, aborted};
      exit:{timeout, _} -> {error, timeout};
      exit:{noproc, _}  -> {error, no_connection}
    end;
call(PeerId, Call) when is_binary(PeerId) ->
    cast_or_call(PeerId, call, Call).

cast(PeerCon, Cast) when is_pid(PeerCon) ->
    try gen_server:cast(PeerCon, Cast)
    catch exit:{noproc, _} -> {error, no_connection}
    end;
cast(PeerId, Cast) when is_binary(PeerId) ->
    cast_or_call(PeerId, cast, Cast).

cast_or_call(PeerId, Action, CastOrCall) ->
    case aec_peers:get_connection(PeerId) of
        {ok, PeerCon} when Action == call -> call(PeerCon, CastOrCall);
        {ok, PeerCon} when Action == cast -> cast(PeerCon, CastOrCall);
        Err = {error, _} ->
            Err
    end.

%% -- gen_server callbacks ---------------------------------------------------

%% Called when accepting an incoming connection
accept_init(Ref, TcpSock, ranch_tcp, Opts) ->
    ok = ranch:accept_ack(Ref),
    Version = <<?P2P_PROTOCOL_VSN:64>>,
    Genesis = aec_chain:genesis_hash(),
    HSTimeout = noise_hs_timeout(),
    case inet:peername(TcpSock) of
        {error, Reason} ->
            epoch_sync:info("Connection accept failed: ~p", [Reason]),
            gen_tcp:close(TcpSock);
        {ok, {Addr, _Port}} ->
            S0 = Opts#{ tcp_sock => TcpSock
                      , role => responder
                      , kind => permanent
                      , address => Addr
                      , host => list_to_binary(inet:ntoa(Addr))
                      , version => Version
                      , genesis => Genesis },
            S = #{ version := Version, genesis := Genesis,
                   pubkey := PubKey } = ensure_genesis(S0),

            %% ======  Entering critical section with Private keys in memory. ======
            PrevSensitive = process_flag(sensitive, true),
            {ok, SecKey} = aec_keys:peer_privkey(),
            NoiseOpts = [ {noise, <<"Noise_XK_25519_ChaChaPoly_BLAKE2b">>}
                        , {s, enoise_keypair:new(dh25519, SecKey, PubKey)}
                        , {prologue, <<Version/binary, Genesis/binary>>}
                        , {timeout, HSTimeout} ],
            %% Keep the socket passive until here to avoid premature message
            %% receiving...
            inet:setopts(TcpSock, [
                {active, true},
                {send_timeout, 1000},
                {send_timeout_close, true}
            ]),
            case enoise:accept(TcpSock, NoiseOpts) of
                {ok, ESock, FinalState} ->
                    RemoteKeys = enoise_hs_state:remote_keys(FinalState),
                    RemotePub = enoise_keypair:pubkey(RemoteKeys),
                    garbage_collect(),
                    process_flag(sensitive, PrevSensitive),
                    %% ======  Exit critical section with Private keys in memory. ======

                    PingTimeout = first_ping_timeout(),
                    epoch_sync:debug("Connection accepted from ~p", [RemotePub]),
                    %% Report this to aec_peers!? And possibly fail?
                    %% Or, we can't do this yet we don't know the port?!
                    TRef = erlang:start_timer(PingTimeout, self(), first_ping_timeout),
                    S1 = S#{ status => {connected, ESock}
                           , r_pubkey => RemotePub
                           , first_ping_tref => TRef },
                    gen_server:enter_loop(?MODULE, [], S1);
                {error, Reason} ->
                    garbage_collect(),
                    process_flag(sensitive, PrevSensitive),
                    %% ======  Exit critical section with Private keys in memory. ======

                    %% What to do here? Close the socket and stop?
                    epoch_sync:info("Connection accept failed - ~p was from ~p",
                                    [Reason, maps:get(host, S)]),
                    gen_tcp:close(TcpSock)
            end
    end.

%% Called when connecting to a peer
init([Opts]) ->
    Version = <<?P2P_PROTOCOL_VSN:64>>,
    Genesis = aec_chain:genesis_hash(),
    Opts1 = Opts#{ role => initiator
                 , kind => permanent
                 , version => Version
                 , genesis => Genesis},
    {ok, Opts1, 0}.

handle_call(Request, From, State) ->
    handle_request(State, Request, From).

handle_cast({send_tx, Hash}, State) ->
    send_send_tx(State, Hash),
    {noreply, State};
handle_cast({send_block, Hash}, State) ->
    send_send_block(State, Hash),
    {noreply, State};
handle_cast(stop, State) ->
    State1 = cleanup_connection(State),
    {stop, normal, State1};
handle_cast(disconnect, #{ status := {connecting, _} } = S) ->
    epoch_sync:debug("Disconnect received, aborting connection"),
    {stop, normal, S};
handle_cast(disconnect, #{ status := {connected, _} } = S) ->
    epoch_sync:debug("Disconnect received, closing connection"),
    {noreply, close(S)};
handle_cast(disconnect, #{ status := {disconnecting, _} } = S) ->
    {noreply, S};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, S = #{ role := initiator, host := Host, port := Port }) ->
    Self = self(),
    ConnTimeout = connect_timeout(),
    Pid = spawn(fun() -> try_connect(Self, Host, Port, ConnTimeout) end),
    {noreply, S#{ status => {connecting, Pid} }};
handle_info(timeout, S) ->
    {noreply, S};
handle_info({timeout, Ref, {request, Kind}}, S) ->
    handle_request_timeout(S, Ref, Kind);
handle_info({timeout, Ref, first_ping_timeout}, S) ->
    NonRegAccept = is_non_registered_accept(S),
    case maps:get(first_ping_tref, S, undefined) of
        Ref when NonRegAccept ->
            epoch_sync:info("Connecting peer never sent ping, stopping"),
            %% Consider blocking this peer
            S1 = cleanup_connection(S),
            {stop, normal, S1};
        _ ->
            epoch_sync:debug("Got stale first_ping_timeout", []),
            {noreply, S}
    end;
handle_info({timeout, _Ref, close_timeout}, #{ status := {disconnecting, ESock} } = S) ->
    epoch_sync:debug("Force close connection to ~p", [maps:get(host, S)]),
    enoise:close(ESock),
    {stop, normal, S};
handle_info({connected, Pid, Err = {error, _}}, S = #{ status := {connecting, Pid} }) ->
    epoch_sync:debug("Failed to connected to ~p: ~p", [{maps:get(host, S), maps:get(port, S)}, Err]),
    connect_fail(S);
handle_info({connected, Pid, {ok, TcpSock}}, S0 = #{ status := {connecting, Pid} }) ->
    case inet:peername(TcpSock) of
        {error, Reason} ->
            epoch_sync:info("Connection failed: ~p", [Reason]),
            gen_tcp:close(TcpSock),
            connect_fail(S0);
        {ok, {Addr, _Port}} ->
            HSTimeout = noise_hs_timeout(),
            S = #{ version := Version, genesis := Genesis,
                   pubkey := PubKey, r_pubkey := RemotePub } = ensure_genesis(S0),
            %% ======  Entering critical section with Private keys in memory. ======
            PrevSensitive = process_flag(sensitive, true),
            {ok, SecKey} = aec_keys:peer_privkey(),
            NoiseOpts = [ {noise, <<"Noise_XK_25519_ChaChaPoly_BLAKE2b">>}
                        , {s, enoise_keypair:new(dh25519, SecKey, PubKey)}
                        , {prologue, <<Version/binary, Genesis/binary>>}
                        , {rs, enoise_keypair:new(dh25519, RemotePub)}
                        , {timeout, HSTimeout} ],
            %% Keep the socket passive until here to avoid premature message
            %% receiving...
            inet:setopts(TcpSock, [
                {active, true},
                {send_timeout, 1000},
                {send_timeout_close, true}
            ]),
            case enoise:connect(TcpSock, NoiseOpts) of
                {ok, ESock, _} ->
                    garbage_collect(),
                    process_flag(sensitive, PrevSensitive),
                    %% ======  Exit critical section with Private keys in memory. ======

                    epoch_sync:debug("Peer connected to ~p",
                                     [{maps:get(host, S), maps:get(port, S)}]),
                    case aec_peers:peer_connected(peer_id(S), self()) of
                        ok ->
                            {noreply, S#{ status => {connected, ESock}, address => Addr}};
                        {error, Reason} ->
                            epoch_sync:debug("Dropping unnecessary connection to ~p (~p)",
                                             [maps:get(host, S), Reason]),
                            enoise:close(ESock),
                            {stop, normal, S}
                    end;
                {error, Reason} ->
                    garbage_collect(),
                    process_flag(sensitive, PrevSensitive),
                    %% ======  Exit critical section with Private keys in memory. ======

                    %% For now lets report connect_fail
                    %% Maybe instead report permanent fail + block?!
                    epoch_sync:info("Connection handshake failed - ~p was from ~p",
                                    [Reason, maps:get(host, S)]),
                    gen_tcp:close(TcpSock),
                    connect_fail(S)
            end
    end;

handle_info({noise, _, <<?MSG_FRAGMENT:16, N:16, M:16, Fragment/binary>>}, S) ->
    handle_fragment(S, N, M, Fragment);
handle_info({noise, _, <<Type:16, Payload/binary>>}, S) ->
    case aec_peer_messages:deserialize(Type, Payload) of
        {response, _Vsn, #{ result := false, type := MsgType, reason := Reason }} ->
            {noreply, handle_msg(S, MsgType, true, {error, Reason})};
        {response, _Vsn, #{ result := true, type := MsgType, msg := {MsgType, Vsn, Msg} }} ->
            {noreply, handle_msg(S, MsgType, true, {ok, Vsn, Msg})};
        {close, _Vsn, _Msg} ->
            case S of
                #{ status := {disconnecting, _} } ->
                    {stop, normal, S};
                #{ status := {connected, ESock} } ->
                    aec_peers:connection_closed(peer_id(S), self()),
                    epoch_sync:debug("Connection closed by the other side ~p",
                                     [maps:get(host, S)]),
                    enoise:close(ESock),
                    {stop, normal, S}
            end;
        {MsgType, Vsn, Msg} ->
            {noreply, handle_msg(S, MsgType, false, {ok, Vsn, Msg})};
        Err = {error, _} ->
            epoch_sync:info("Could not deserialize message ~p", [Err]),
            {noreply, S}
    end;
handle_info({enoise_error, _, Reason}, S) ->
    epoch_sync:debug("Peer connection got enoise_error: ~p", [Reason]),
    handle_general_error(S);
handle_info({tcp_closed, _}, #{ status := {disconnecting, _} } = S) ->
    {stop, normal, S};
handle_info({tcp_closed, _}, S) ->
    epoch_sync:debug("Peer connection got tcp_closed", []),
    handle_general_error(S);
handle_info(_Msg, S) ->
    {noreply, S}.

terminate(normal, _State) ->
    ok;
terminate(NonNormal, State) ->
    epoch_sync:info("Non-normal terminate, reason: ~p", [NonNormal]),
    cleanup_connection(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- Local functions --------------------------------------------------------

maybe_close(#{ kind := temporary, status := {connected, _} } = S) ->
    epoch_sync:debug("Closing temporary connection"),
    notify_close(S);
maybe_close(S) ->
    S.

notify_close(S) ->
    aec_peers:connection_closed(peer_id(S), self()),
    close(S).

close(#{ status := {connected, ESock} } = S) ->
    send_msg(S, close, aec_peer_messages:serialize(close, #{})),
    CloseTimeout = close_timeout(),
    erlang:start_timer(CloseTimeout, self(), close_timeout),
    S#{ status := {disconnecting, ESock} }.

handle_general_error(S) ->
    S1 = cleanup_connection(S),
    case is_non_registered_accept(S) of
        true ->
            {stop, normal, S1};
        false ->
            connect_fail(S1)
    end.

get_request(S, Kind) ->
    maps:get(map_request(Kind), maps:get(requests, S, #{}), none).

set_request(S, Kind, From) ->
    Rs = maps:get(requests, S, #{}),
    TRef = erlang:start_timer(?REQUEST_TIMEOUT, self(), {request, Kind}),
    S#{ requests => Rs#{ Kind => {Kind, From, TRef} } }.

remove_request_fld(S, Kind) ->
    Rs = maps:get(requests, S, #{}),
    case get_request(S, Kind) of
        none ->
            ok;
        {_MappedKind, _From, TRef} ->
            erlang:cancel_timer(TRef)
    end,
    S#{ requests => maps:remove(map_request(Kind), Rs) }.

handle_request_timeout(S, Ref, Kind) ->
    case get_request(S, Kind) of
        {_MappedKind, From, Ref} ->
            epoch_sync:info("~p request timeout, stopping peer_connection", [Kind]),
            gen_server:reply(From, {error, request_timeout}),
            handle_general_error(S);
        _Other ->
            %% We got a stale timeout
            {noreply, S}
    end.

map_request(get_header_by_hash)   -> get_header;
map_request(get_header_by_height) -> get_header;
map_request(header)               -> get_header;
map_request(header_hashes)        -> get_n_successors;
map_request(generation)           -> get_generation;
map_request(txps_init)            -> tx_pool;
map_request(txps_unfold)          -> tx_pool;
map_request(txs)                  -> tx_pool;
map_request(txps_finish)          -> tx_pool;
map_request(Request)              -> Request.

try_connect(Parent, Host, Port, Timeout) when is_binary(Host) ->
    try_connect(Parent, binary_to_list(Host), Port, Timeout);
try_connect(Parent, Host, Port, Timeout) ->
    Res =
        try
            gen_tcp:connect(Host, Port, [binary, {reuseaddr, true}, {active, false}], Timeout)
        catch _E:R ->
            {error, R}
        end,
    case Res of
        {ok, Sock} -> gen_tcp:controlling_process(Sock, Parent);
        {error, _} -> ok
    end,
    Parent ! {connected, self(), Res}.

ensure_genesis(S = #{ genesis := G }) when is_binary(G) ->
    S;
ensure_genesis(S) ->
    case aec_chain:genesis_hash() of
        undefined ->
            timer:sleep(500),
            ensure_genesis(S);
        GHash ->
            S#{ genesis => GHash }
    end.

is_non_registered_accept(State) ->
    maps:get(role, State, no_role) == responder
        andalso maps:get(port, State, no_port) == no_port.

cleanup_connection(State) ->
    case maps:get(status, State, error) of
        {connected, ESock}     -> enoise:close(ESock);
        {disconnecting, ESock} -> enoise:close(ESock);
        _                      -> ok
    end,
    case maps:get(gen_tcp, State, undefined) of
        TSock when is_port(TSock) -> gen_tcp:close(TSock);
        _                         -> ok
    end,
    cleanup_requests(State).

cleanup_requests(State) ->
    Reqs = maps:to_list(maps:get(requests, State, #{})),
    [ begin
        gen_server:reply(From, {error, disconnected}),
        erlang:cancel_timer(TRef)
      end || {_, {_, From, TRef}} <- Reqs ],
    maps:remove(requests, State).

connect_fail(S0) ->
    S = cleanup_requests(S0),
    aec_peers:connection_failed(peer_id(S), self()),
    {stop, normal, S}.

handle_fragment(S, 1, _M, Fragment) ->
    {noreply, S#{ fragments => [Fragment] }};
handle_fragment(S = #{ fragments := Fragments }, M, M, Fragment) ->
    Msg = list_to_binary(lists:reverse([Fragment | Fragments])),
    self() ! {noise, unused, Msg},
    {noreply, maps:remove(fragments, S)};
handle_fragment(S = #{ fragments := Fragments }, N, _M, Fragment) when N == length(Fragments) + 1 ->
    {noreply, S#{ fragments := [Fragment | Fragments] }};
handle_fragment(S = #{ fragments := Fragments }, N, M, _Fragment) ->
    epoch_sync:error("Got fragment ~p, expected ~p (out of ~p)", [N, length(Fragments) + 1, M]),
    S1 = cleanup_connection(S),
    connect_fail(maps:remove(fragments, S1)).

handle_msg(S, MsgType, IsResponse, Result) ->
    handle_msg(S, MsgType, IsResponse, get_request(S, MsgType), Result).

handle_msg(S, MsgType, true, none, Result) ->
    epoch_sync:info("Peer ~p got unexpected ~p response - ~p",
                    [peer_id(S), MsgType, Result]),
    S;
handle_msg(S, _MsgType, true, {RequestFld, From, _TRef}, {error, Reason}) ->
    gen_server:reply(From, {error, Reason}),
    remove_request_fld(S, RequestFld);
handle_msg(S, MsgType, false, Request, {ok, Vsn, Msg}) ->
    case MsgType of
        ping                 -> handle_ping(S, Request, Msg);
        get_header_by_hash   -> handle_get_header_by_hash(S, Msg);
        get_header_by_height -> handle_get_header_by_height(S, Vsn, Msg);
        get_n_successors     -> handle_get_n_successors(S, Vsn, Msg);
        get_generation       -> handle_get_generation(S, Msg);
        block                -> handle_new_block(S, Msg);
        txs                  -> handle_new_txs(S, Msg);
        txps_init            -> handle_tx_pool_sync_init(S, Msg);
        txps_unfold          -> handle_tx_pool_sync_unfold(S, Msg);
        txps_get             -> handle_tx_pool_sync_get(S, Msg);
        txps_finish          -> handle_tx_pool_sync_finish(S, Msg)
    end;
handle_msg(S, MsgType, true, Request, {ok, _Vsn, Msg}) ->
    case MsgType of
        ping          -> handle_ping_rsp(S, Request, Msg);
        header        -> handle_header_rsp(S, Request, Msg);
        header_hashes -> handle_header_hashes_rsp(S, Request, Msg);
        generation    -> handle_get_generation_rsp(S, Request, Msg);
        txps_init     -> handle_tx_pool_sync_rsp(S, init, Request, Msg);
        txps_unfold   -> handle_tx_pool_sync_rsp(S, unfold, Request, Msg);
        txs           -> handle_tx_pool_sync_rsp(S, get, Request, Msg);
        txps_finish   -> handle_tx_pool_sync_rsp(S, finish, Request, Msg)

    end.

handle_request(S, {Request, Args}, From) ->
    MappedRequest = map_request(Request),
    case get_request(S, Request) of
        none ->
            handle_request(S, Request, Args, From);
        {MappedRequest, _From, _TRef} ->
            {reply, {error, request_already_in_progress}, S}
    end.

handle_request(S = #{ status := error }, _Req, _Args, _From) ->
    {reply, {error, disconnected}, S};
handle_request(S, tx_pool, TxPoolArgs, From) ->
    handle_tx_pool(S, TxPoolArgs, From);
handle_request(S, Request, Args, From) ->
    ReqData = prepare_request_data(S, Request, Args),
    send_msg(S, Request, aec_peer_messages:serialize(Request, ReqData)),
    {noreply, set_request(S, map_request(Request), From)}.

prepare_request_data(S, ping, []) ->
    ping_obj(local_ping_obj(S), [peer_id(S)]);
prepare_request_data(_S, get_header_by_hash, [Hash]) ->
    #{ hash => Hash };
prepare_request_data(_S, get_header_by_height, [Height, TopHash]) ->
    #{ height => Height, top_hash => TopHash };
prepare_request_data(_S, get_n_successors, [FromHash, TargetHash, N]) ->
    #{ from_hash => FromHash, target_hash => TargetHash, n => N };
prepare_request_data(_S, get_generation, [Hash, Dir]) ->
    #{ hash => Hash, forward => (Dir == forward) }.

handle_tx_pool(S, Args, From) ->
    {Msg, MsgData} =
        case Args of
            [sync_init] ->
                {txps_init, #{}};
            [sync_unfold, Unfolds] ->
                {txps_unfold, #{ unfolds => Unfolds }};
            [sync_get, TxHashes] ->
                {txps_get, #{ tx_hashes => TxHashes }};
            [sync_finish, Done] ->
                {txps_finish, #{ done => Done }}
        end,
    send_msg(S, Msg, aec_peer_messages:serialize(Msg, MsgData)),
    {noreply, set_request(S#{ tx_pool => Msg }, tx_pool, From)}.

%% -- Ping message -----------------------------------------------------------

handle_ping_rsp(S, {ping, From, _TRef}, RemotePingObj) ->
    Res = handle_ping_msg(S, RemotePingObj),
    gen_server:reply(From, Res),
    remove_request_fld(S, ping).

handle_ping(S, none, RemotePingObj) ->
    {PeerOk, S1} = handle_first_ping(S, RemotePingObj),

    Response =
        case PeerOk of
            ok ->
                case handle_ping_msg(S1, RemotePingObj) of
                    ok ->
                        {ok, ping_obj_rsp(S1, RemotePingObj)};
                    {error, Reason} ->
                        {error, Reason}
                end;
            {error, Reason} ->
                {error, Reason}
        end,
    send_response(S, ping, Response),
    maybe_close(S1);
handle_ping(S, {ping, _From, _TRef}, RemotePingObj) ->
    epoch_sync:info("Peer ~p got new ping when waiting for PING_RSP - ~p",
                    [peer_id(S), RemotePingObj]),
    send_response(S, ping, {error, already_pinging}),
    S.

handle_first_ping(S, RemotePingObj) ->
    case is_non_registered_accept(S) of
        true ->
            %% This is the first ping, now we have everything - accept the peer
            #{port := Port} = RemotePingObj,
            PeerInfo = #{
                host => maps:get(host, S),
                port => Port,
                pubkey => maps:get(r_pubkey, S)
            },

            %% Cancel the first ping_timeout
            case maps:get(first_ping_tref, S, undefined) of
                undefined -> ok;
                Ref -> try erlang:cancel_timer(Ref, [{async, true}, {info, false}])
                       catch error:_ -> ok end
            end,

            NewS = maps:remove(first_ping_tref, S#{ port => Port }),
            case aec_peers:peer_accepted(PeerInfo, self()) of
                {error, _} = Error ->
                    gen_server:cast(self(), stop),
                    {Error, NewS};
                ConnKind when ConnKind =:= temporary; ConnKind =:= permanent ->
                    {ok, NewS#{ kind := ConnKind }}
            end;
        false ->
            {ok, S}
    end.

handle_ping_msg(S, RemotePingObj) ->
    #{ genesis_hash := LGHash,
       best_hash    := LTHash,
       sync_allowed := LSyncAllowed,
       difficulty   := LDiff } = local_ping_obj(S),
    #{ address := SourceAddr } = S,
    PeerId = peer_id(S),
    case decode_remote_ping(RemotePingObj) of
        {ok, true, RGHash, RTHash, RDiff, RPeers}
          when RGHash == LGHash, LSyncAllowed =:= true ->
            case {{LTHash, LDiff}, {RTHash, RDiff}} of
                {{T, _}, {T, _}} ->
                    epoch_sync:debug("Same top blocks", []),
                    aec_sync:get_generation(PeerId, T),
                    aec_events:publish(chain_sync, {chain_sync_done, PeerId}),
                    ok;
                {{_, DL}, {_, DR}} when DL > DR ->
                    epoch_sync:debug("Our difficulty is higher", []),
                    aec_events:publish(chain_sync, {chain_sync_done, PeerId}),
                    ok;
                {{_, _}, {_, DR}} ->
                    aec_sync:start_sync(PeerId, RTHash, DR)
            end,
            ok = aec_peers:add_peers(SourceAddr, RPeers),
            aec_tx_pool_sync:connect(PeerId, self()),
            ok;
        {ok, _SyncAllowed, RGHash, _RTHash, _RDiff, RPeers}
          when RGHash == LGHash ->
            epoch_sync:debug("Temporary connection, not synchronizing", []),
            ok = aec_peers:add_peers(SourceAddr, RPeers);
        {ok, _SyncAllowed, _RGHash, _RTHash, _RDiff, _RPeers} ->
            {error, wrong_genesis_hash};
        {error, Reason} ->
            {error, Reason}
    end.

decode_remote_ping(#{ genesis_hash := GHash,
                      best_hash    := THash,
                      peers        := Peers,
                      difficulty   := Difficulty,
                      sync_allowed := SyncAllowed}) ->
    {ok, SyncAllowed, GHash, THash, Difficulty, Peers};
decode_remote_ping(_) ->
    {error, bad_ping_message}.

%% Encode hashes and get peers for PingObj
ping_obj(PingObj, Exclude) ->
    #{ share := Share } = PingObj,
    Peers = aec_peers:get_random(Share, Exclude),
    PingObj#{peers => Peers}.

ping_obj_rsp(S, RemotePingObj) ->
    PeerId = peer_id(S),
    #{ share := Share, peers := TheirPeers } = RemotePingObj,
    LocalPingObj = local_ping_obj(S),
    ping_obj(LocalPingObj#{ share => Share },
             [PeerId | [aec_peers:peer_id(P) || P <- TheirPeers]]).

local_ping_obj(#{ kind := ConnKind, ext_sync_port := Port }) ->
    GHash = aec_chain:genesis_hash(),
    TopHash = aec_chain:top_key_block_hash(),
    {ok, Difficulty} = aec_chain:difficulty_at_top_block(),
    #{ genesis_hash => GHash,
       best_hash    => TopHash,
       difficulty   => Difficulty,
       sync_allowed => ConnKind =/= temporary,
       share        => 32,  % TODO: make this configurable
       peers        => [],
       port         => Port }.

%% -- Get Header by Hash -----------------------------------------------------

handle_get_header_by_hash(S, Msg) ->
    Response = get_header(maps:get(hash, Msg, undefined)),
    send_response(S, header, Response),
    S.

handle_header_rsp(S, {get_header, From, _TRef}, Msg) ->
    try
        Header = aec_headers:deserialize_from_binary(maps:get(hdr, Msg)),
        gen_server:reply(From, {ok, Header})
    catch _:_ ->
        gen_server:reply(From, {error, bad_response})
    end,
    remove_request_fld(S, get_header).

get_header(Hash) when is_binary(Hash) ->
    get_header(hash, Hash);
get_header(N) when is_integer(N), N >= 0 ->
    get_header(height, N);
get_header(_) ->
    {error, bad_request}.

get_header(hash, Hash) ->
    get_header(fun aec_chain:get_header/1, Hash);
get_header(height, N) ->
    get_header(fun aec_chain:get_key_header_by_height/1, N);
get_header(Fun, Arg) ->
    case Fun(Arg) of
        {ok, Header} ->
            HH = aec_headers:serialize_to_binary(Header),
            {ok, #{ hdr => HH }};
        Err when Err == error orelse Err == {error, chain_too_short} ->
            {error, header_not_found}
    end.

%% -- Get Header by Height -----------------------------------------------------

handle_get_header_by_height(S, ?VSN_1, Msg) ->
    Response = get_header(maps:get(height, Msg, undefined)),
    send_response(S, header, Response),
    S;
handle_get_header_by_height(S, ?GET_HEADER_BY_HEIGHT_VSN,
                            #{ height := H, top_hash := TopHash}) ->
    case {aec_chain:get_key_header_by_height(H),
          aec_chain:hash_is_in_main_chain(TopHash)} of
        {{ok, Header}, true} ->
            SerHeader = aec_headers:serialize_to_binary(Header),
            send_response(S, header, {ok, #{ hdr => SerHeader }});
        {_, false} ->
            send_response(S, header, {error, not_on_chain});
        {Err, _} when Err == error orelse Err == {error, chain_too_short} ->
            send_response(S, header, {error, header_not_found})
    end,
    S.

%% Response is handled above in Get Header by Hash

%% -- Get N Successors -------------------------------------------------------

do_get_n_successors(Hash, N) ->
    case aec_chain:get_at_most_n_generation_headers_forward_from_hash(Hash, N+1) of
        {ok, [_ | Headers]} ->
            HHashes = [ begin
                            {ok, HHash} = aec_headers:hash_header(H),
                            <<(aec_headers:height(H)):64, HHash/binary>>
                        end || H <- Headers ],
            {ok, #{ header_hashes => HHashes }};
        error ->
            {error, block_not_found}
    end.

handle_get_n_successors(S, Vsn, Msg) ->
    Response =
        case Msg of
            #{ hash := Hash, n := N } when Vsn == ?VSN_1 ->
                do_get_n_successors(Hash, N);
            #{ from_hash := FromHash, target_hash := TargetHash, n := N }
                when Vsn == ?GET_N_SUCCESSORS_VSN ->
                Res = do_get_n_successors(FromHash, N),
                case aec_chain:hash_is_in_main_chain(TargetHash) of
                    true  -> Res;
                    false -> {error, not_on_chain}
                end;
            _ ->
                {error, bad_request}
        end,
    send_response(S, header_hashes, Response),
    S.

handle_header_hashes_rsp(S, {get_n_successors, From, _TRef}, Msg) ->
    case maps:get(header_hashes, Msg, undefined) of
        Hdrs when is_list(Hdrs) ->
            Hdrs1 = [ {Height, Hash} || <<Height:64, Hash/binary>> <- Hdrs ],
            gen_server:reply(From, {ok, Hdrs1});
        _ ->
            gen_server:reply(From, {error, no_headers_in_response})
    end,
    remove_request_fld(S, get_n_successors).

%% -- Get Generation ----------------------------------------------------------

handle_get_generation(S, Msg) ->
    Forward = maps:get(forward, Msg),
    Response =
        case do_get_generation(maps:get(hash, Msg), Forward) of
            {ok, KeyBlock, MicroBlocks} ->
                SerKeyBlock = aec_blocks:serialize_to_binary(KeyBlock),
                SerMicroBlocks = [ aec_blocks:serialize_to_binary(B) || B <- MicroBlocks ],
                {ok, #{key_block => SerKeyBlock, micro_blocks => SerMicroBlocks, forward => Forward }};
            error ->
                {error, block_not_found}
        end,
    send_response(S, generation, Response),
    S.

do_get_generation(Hash, true) ->
    aec_chain:get_generation(Hash);
do_get_generation(Hash, false) ->
    aec_chain:get_prev_generation(Hash).

handle_get_generation_rsp(S, {get_generation, From, _TRef}, Msg) ->
    SerKeyBlock = maps:get(key_block, Msg),
    SerMicroBlocks = maps:get(micro_blocks, Msg),
    Dir = case maps:get(forward, Msg) of true -> forward; false -> backward end,
    Res =
        case aec_blocks:deserialize_from_binary(SerKeyBlock) of
            {ok, KeyBlock} ->
                case deserialize_micro_blocks(SerMicroBlocks, []) of
                    {ok, MicroBlocks} ->
                        {ok, KeyBlock, MicroBlocks, Dir};
                    Err = {error, _} ->
                        Err
                end;
            Err = {error, _} ->
                Err
        end,
    gen_server:reply(From, Res),
    remove_request_fld(S, get_generation).

deserialize_micro_blocks([], Acc) ->
    {ok, lists:reverse(Acc)};
deserialize_micro_blocks([SMB | SMBs], Acc) ->
    case aec_blocks:deserialize_from_binary(SMB) of
        {ok, MB}         -> deserialize_micro_blocks(SMBs, [MB | Acc]);
        Err = {error, _} -> Err
    end.

%% -- TX Pool ----------------------------------------------------------------

handle_tx_pool_sync_init(S, _MsgObj) ->
    case aec_tx_pool_sync:accept(peer_id(S), self()) of
        ok ->
            send_response(S, txps_init, {ok, #{}});
        Err = {error, _} ->
            send_response(S, txps_init, Err)
    end,
    S.

handle_tx_pool_sync_unfold(S, MsgObj) ->
    Unfolds = maps:get(unfolds, MsgObj),
    case aec_tx_pool_sync:sync_unfold(peer_id(S), Unfolds) of
        {ok, NewUnfolds} ->
            send_response(S, txps_unfold, {ok, #{ unfolds => NewUnfolds }});
        Err = {error, _} ->
            send_response(S, txps_unfold, Err)
    end,
    S.

handle_tx_pool_sync_get(S, MsgObj) ->
    TxHashes = maps:get(tx_hashes, MsgObj),
    case aec_tx_pool_sync:sync_get(peer_id(S), TxHashes) of
        {ok, Txs} ->
            SerTxs = [ aetx_sign:serialize_to_binary(Tx) || Tx <- Txs ],
            send_response(S, txs, {ok, #{ txs => SerTxs }});
        Err = {error, _} ->
            send_response(S, txs, Err)
    end,
    S.

handle_tx_pool_sync_finish(S, MsgObj) ->
    aec_tx_pool_sync:sync_finish(peer_id(S), {done, maps:get(done, MsgObj)}),
    send_response(S, txps_finish, {ok, #{ done => maps:get(done, MsgObj) }}),
    S.

handle_tx_pool_sync_rsp(S, Action, {tx_pool, From, _TRef}, MsgObj) ->
    case Action of
        init ->
            gen_server:reply(From, ok);
        unfold ->
            gen_server:reply(From, {ok, maps:get(unfolds, MsgObj)});
        get ->
            Txs = [ aetx_sign:deserialize_from_binary(SerTx)
                    || SerTx <- maps:get(txs, MsgObj) ],
            gen_server:reply(From, {ok, Txs});
        finish ->
            gen_server:reply(From, {ok, maps:get(done, MsgObj)})
    end,
    remove_request_fld(S, tx_pool).

%% -- Send Block --------------------------------------------------------------

send_send_block(#{ status := error }, _Block) ->
    ok;
send_send_block(S = #{ status := {connected, _ESock} }, Block) ->
    SerBlock = aec_blocks:serialize_to_binary(Block),
    Msg = aec_peer_messages:serialize(block, #{ block => SerBlock }),
    send_msg(S, block, Msg).

handle_new_block(S, Msg) ->
    try
        {ok, Block} = aec_blocks:deserialize_from_binary(maps:get(block, Msg)),
        Header = aec_blocks:to_header(Block),
        {ok, HH} = aec_headers:hash_header(Header),
        epoch_sync:debug("Got new block: ~s", [pp(HH)]),
        aec_conductor:post_block(Block)
    catch _:_ ->
        ok
    end,
    S.

%% -- Send TX --------------------------------------------------------------

send_send_tx(#{ status := error }, _Tx) ->
    ok;
send_send_tx(S = #{ status := {connected, _ESock} }, Tx) ->
    TxSerialized = aetx_sign:serialize_to_binary(Tx),
    Msg = aec_peer_messages:serialize(txs, #{ txs => [TxSerialized] }),
    send_msg(S, txs, Msg).

handle_new_txs(S, Msg) ->
    try
        #{ txs := EncSignedTxs } = Msg,
        SignedTxs = [ aetx_sign:deserialize_from_binary(Tx) || Tx <- EncSignedTxs ],
        [ aec_tx_pool:push(SignedTx, tx_received) || SignedTx <- SignedTxs ]
    catch _:_ ->
        ok
    end,
    S.

%% -- Send message -----------------------------------------------------------
send_msg(#{ status := {connected, ESock} }, Type, Msg) when is_binary(Msg) ->
    do_send(ESock, <<(aec_peer_messages:tag(Type)):16, Msg/binary>>).

send_response(S, Type, Response) ->
    Msg = aec_peer_messages:serialize_response(Type, Response),
    send_msg(S, response, Msg).

%% If the message is more than 65533 bytes it won't fit in a single Noise
%% message and we need to fragment it.
%%
%% Fragments have the format <<?MSG_FRAGMENT:16, N:16, M:16,
%% PayLoad:65529/binary>> (where the last fragment has a smaller Payload),
%% saying that this is fragment N out of M fragments.
%%
%% For testing purpose - set the max packet size to 16#FF while testing

-ifndef(TEST).
-define(MAX_PACKET_SIZE, 16#FFFF).
-else.
-define(MAX_PACKET_SIZE, 16#1FF).
-endif.
-define(FRAGMENT_SIZE, (?MAX_PACKET_SIZE - 6)).
do_send(ESock, Msg) when byte_size(Msg) < ?MAX_PACKET_SIZE - 2 ->
    enoise_send(ESock, Msg);
do_send(ESock, Msg) ->
    NChunks = (?FRAGMENT_SIZE + byte_size(Msg) - 1) div ?FRAGMENT_SIZE,
    send_chunks(ESock, 1, NChunks, Msg).

send_chunks(ESock, M, M, Msg) ->
    enoise:send(ESock, <<?MSG_FRAGMENT:16, M:16, M:16, Msg/binary>>);
send_chunks(ESock, N, M, <<Chunk:?FRAGMENT_SIZE/binary, Rest/binary>>) ->
    enoise_send(ESock, <<?MSG_FRAGMENT:16, N:16, M:16, Chunk/binary>>),
    send_chunks(ESock, N + 1, M, Rest).

peer_id(#{ r_pubkey := PK }) ->
    PK.

enoise_send(ESock, Msg) ->
    case enoise:send(ESock, Msg) of
        ok ->
            ok;
        Err = {error, Reason} ->
            self() ! {enoise_error, ESock, Reason},
            epoch_sync:info("Failed to send message: ~p", [Err]),
            Err
    end.

%% -- Configuration ----------------------------------------------------------

connect_timeout() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"connect_timeout">>],
                               aecore, sync_connect_timeout,
                               ?DEFAULT_CONNECT_TIMEOUT).

first_ping_timeout() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"first_ping_timeout">>],
                               aecore, sync_first_ping_timeout,
                               ?DEFAULT_FIRST_PING_TIMEOUT).

noise_hs_timeout() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"noise_hs_timeout">>],
                               aecore, sync_noise_hs_timeout,
                               ?DEFAULT_NOISE_HS_TIMEOUT).

close_timeout() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"close_timeout">>],
                               aecore, sync_close_timeout,
                               ?DEFAULT_CLOSE_TIMEOUT).

