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
        , set_sync_height/2
        , get_node_info/2
        ]).

%% API Mempool sync
-export([ tx_pool_sync_finish/2
        , tx_pool_sync_get/2
        , tx_pool_sync_init/1
        , tx_pool_sync_unfold/2
        ]).

%% API for gossip serialization
-export([gossip_serialize_block/1,
         gossip_serialize_tx/1]).

-ifdef(TEST).
-export([is_node_info_sharing_enabled/0]).
-endif.

-include("aec_peer_messages.hrl").
-include("blocks.hrl").

-behaviour(gen_server).

-import(aeu_debug, [pp/1]).

-define(P2P_PROTOCOL_VSN, 1).

-define(DEFAULT_CONNECT_TIMEOUT, 1000).
-define(DEFAULT_FIRST_PING_TIMEOUT, 30000).
-define(DEFAULT_NOISE_HS_TIMEOUT, 5000).
-define(DEFAULT_CLOSE_TIMEOUT, 3000).
%% The number of peers sent in ping message.
-define(DEFAULT_GOSSIPED_PEERS_COUNT, 32).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% -- BEHAVIOUR ranch_protocol CALLBACKS -------------------------------------

start_link(Ref, Socket, Transport, Opts) ->
    Args = [Ref, Socket, Transport, Opts],
    {ok, proc_lib:spawn_link(?MODULE, accept_init, Args)}.

%% -- API --------------------------------------------------------------------

connect(#{conn_type := noise} = Options) ->
    epoch_sync:debug("New peer connection to ~p", [Options]),
    aec_peer_connection_sup:start_peer_connection(Options);
connect(#{conn_type := tcp} = Options) ->
    epoch_sync:debug("New TCP probe to ~p", [Options]),
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

send_tx(PeerId, SerTx) ->
    cast(PeerId, {send_tx, SerTx}).

send_block(PeerId, SerBlock) ->
    cast(PeerId, {send_block, SerBlock}).

%% Indicate that we are syncing, and that gossips should be dropped.
set_sync_height(PeerId, Height) ->
    cast(PeerId, {set_sync_height, Height}).

stop(PeerCon) ->
    gen_server:cast(PeerCon, stop).

get_node_info(PeerId, Timeout) ->
    call(PeerId, {get_node_info, [Timeout]}, Timeout).


call(P, Call) ->
    call(P, Call, ?REQUEST_TIMEOUT).

call(PeerCon, Call, Timeout) when is_pid(PeerCon) ->
    try gen_server:call(PeerCon, Call, Timeout + 2000)
    catch
      exit:{normal, _}  -> {error, aborted};
      exit:{timeout, _} -> {error, timeout};
      exit:{noproc, _}  -> {error, no_connection}
    end;
call(PeerId, Call, Timeout) when is_binary(PeerId) ->
    cast_or_call(PeerId, call, Call, Timeout).

cast(PeerCon, Cast) when is_pid(PeerCon) ->
    try gen_server:cast(PeerCon, Cast)
    catch exit:{noproc, _} -> {error, no_connection}
    end;
cast(PeerId, Cast) when is_binary(PeerId) ->
    cast_or_call(PeerId, cast, Cast, not_used).

cast_or_call(PeerId, Action, CastOrCall, Timeout) ->
    case aec_peers:get_connection(PeerId) of
        {ok, PeerCon} when Action == call -> call(PeerCon, CastOrCall, Timeout);
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
            S = #{ genesis := Genesis, pubkey := PubKey } = ensure_genesis(S0),

            %% ======  Entering critical section with Private keys in memory. ======
            PrevSensitive = process_flag(sensitive, true),
            {ok, SecKey} = aec_keys:peer_privkey(),
            Prologue = aec_governance:add_network_id_last(<<Version/binary,
                                                            Genesis/binary>>),
            NoiseOpts = [ {noise, <<"Noise_XK_25519_ChaChaPoly_BLAKE2b">>}
                        , {s, enoise_keypair:new(dh25519, SecKey, PubKey)}
                        , {prologue, Prologue}
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
init([#{conn_type := noise} = Opts]) ->
    Version = <<?P2P_PROTOCOL_VSN:64>>,
    Genesis = aec_chain:genesis_hash(),
    Opts1 = Opts#{ conn_type => noise
                 , role => initiator
                 , kind => permanent
                 , version => Version
                 , genesis => Genesis},
    {ok, Opts1, 0};
init([#{conn_type := tcp} = Opts]) ->
    Opts1 = Opts#{ conn_type => tcp
                 , role => initiator
                 , kind => temporary},
    {ok, Opts1, 0}.

handle_call(Request, From, State) ->
    handle_request(State, Request, From).

handle_cast({send_tx, SerTx}, State) ->
    send_send_tx(State, SerTx),
    {noreply, State};
handle_cast({send_block, SerBlock}, State) ->
    send_send_block(State, SerBlock),
    {noreply, State};
handle_cast({set_sync_height, none}, State) ->
    {noreply, maps:remove(sync_height, State)};
handle_cast({set_sync_height, Height}, State) when is_integer(Height) ->
    {noreply, State#{ sync_height => Height }};
handle_cast({expand_micro_block, MicroBlockFragment}, State) ->
    {noreply, expand_micro_block(State, MicroBlockFragment)};
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
handle_info({connected, Pid, Err = {error, _}},
            S = #{ conn_type := noise, status := {connecting, Pid} }) ->
    epoch_sync:debug("Failed to connect to ~p: ~p", [{maps:get(host, S), maps:get(port, S)}, Err]),
    connect_fail(S);
handle_info({connected, Pid, Err = {error, _}},
            S = #{ conn_type := tcp, status := {connecting, Pid} }) ->
    epoch_sync:debug("Failed TCP probe to ~p: ~p", [{maps:get(host, S), maps:get(port, S)}, Err]),
    tcp_probe_fail(S);
handle_info({connected, Pid, {ok, TcpSock}},
            S0 = #{ conn_type := noise, status := {connecting, Pid} }) ->
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
            Prologue = aec_governance:add_network_id_last(<<Version/binary,
                                                            Genesis/binary>>),
            NoiseOpts = [ {noise, <<"Noise_XK_25519_ChaChaPoly_BLAKE2b">>}
                        , {s, enoise_keypair:new(dh25519, SecKey, PubKey)}
                        , {prologue, Prologue}
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
handle_info({connected, Pid, {ok, _TcpSock}},
            S = #{ conn_type := tcp, status := {connecting, Pid} }) ->
    aec_peers:peer_alive(peer_id(S), self()),
    {stop, normal, S};
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
    set_request(S, Kind, From, ?REQUEST_TIMEOUT).

set_request(S, Kind, From, Timeout) ->
    Rs = maps:get(requests, S, #{}),
    TRef = erlang:start_timer(Timeout, self(), {request, Kind}),
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
            do_reply(From, {error, request_timeout}),
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
map_request(get_node_info)        -> node_info;
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
        do_reply(From, {error, disconnected}),
        erlang:cancel_timer(TRef)
      end || {_, {_, From, TRef}} <- Reqs ],
    maps:remove(requests, State).

connect_fail(S0) ->
    S = cleanup_requests(S0),
    aec_peers:connection_failed(peer_id(S), self()),
    {stop, normal, S}.

tcp_probe_fail(S) ->
    aec_peers:peer_dead(peer_id(S), self()),
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

do_reply(From = {Pid, _Tag}, Msg) when is_pid(Pid) ->
    gen_server:reply(From, Msg);
do_reply(_Other, _Msg) ->
    ok.

handle_msg(S, block_txs, true, none, Result) ->
    handle_get_block_txs_rsp(S, Result);
handle_msg(S, MsgType, true, none, Result) ->
    epoch_sync:info("Peer ~p got unexpected ~p response - ~p",
                    [peer_id(S), MsgType, Result]),
    S;
handle_msg(S, _MsgType, true, {RequestFld, From, _TRef}, {error, Reason}) ->
    do_reply(From, {error, Reason}),
    remove_request_fld(S, RequestFld);
handle_msg(S, MsgType, false, Request, {ok, Vsn, Msg}) ->
    case MsgType of
        ping                 -> handle_ping(S, Request, Msg);
        get_header_by_hash   -> handle_get_header_by_hash(S, Msg);
        get_header_by_height -> handle_get_header_by_height(S, Vsn, Msg);
        get_n_successors     -> handle_get_n_successors(S, Vsn, Msg);
        get_generation       -> handle_get_generation(S, Msg);
        get_block_txs        -> handle_get_block_txs(S, Msg);
        key_block            -> handle_new_key_block(S, Msg);
        micro_block          -> handle_new_micro_block(S, Msg);
        txs                  -> handle_new_txs(S, Msg);
        txps_init            -> handle_tx_pool_sync_init(S, Msg);
        txps_unfold          -> handle_tx_pool_sync_unfold(S, Msg);
        txps_get             -> handle_tx_pool_sync_get(S, Msg);
        txps_finish          -> handle_tx_pool_sync_finish(S, Msg);
        get_node_info        -> handle_get_node_info(S)
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
        txps_finish   -> handle_tx_pool_sync_rsp(S, finish, Request, Msg);
        node_info     -> handle_node_info_rsp(S, Request, Msg)
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
    CustomTimeout = custom_timeout(Request, Args),
    send_msg(S, Request, aec_peer_messages:serialize(Request, ReqData)),
    {noreply, set_request(S, map_request(Request), From, CustomTimeout)}.

prepare_request_data(S, ping, []) ->
    ping_obj(local_ping_obj(S), [peer_id(S)]);
prepare_request_data(_S, get_header_by_hash, [Hash]) ->
    #{ hash => Hash };
prepare_request_data(_S, get_header_by_height, [Height, TopHash]) ->
    #{ height => Height, top_hash => TopHash };
prepare_request_data(_S, get_n_successors, [FromHash, TargetHash, N]) ->
    #{ from_hash => FromHash, target_hash => TargetHash, n => N };
prepare_request_data(_S, get_generation, [Hash, Dir]) ->
    #{ hash => Hash, forward => (Dir == forward) };
prepare_request_data(_S, get_node_info, _) ->
    #{}.

custom_timeout(get_node_info, [Timeout]) -> Timeout;
custom_timeout(_, _) -> ?REQUEST_TIMEOUT.

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
            case aec_peers:peer_accepted(PeerInfo, maps:get(address, NewS), self()) of
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
    DecodedPingObj = decode_remote_ping(RemotePingObj),
    case DecodedPingObj of
        {ok, true, RGHash0, RTHash0, RDiff0, _} ->
            aec_peer_analytics:log_peer_status(S, RGHash0, RTHash0, RDiff0);
        {ok, false, RGHash0, RTHash0, RDiff0, _} ->
            aec_peer_analytics:log_temporary_peer_status(S, RGHash0, RTHash0, RDiff0);
        {error, _} -> ok
    end,
    case DecodedPingObj of
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
                      share        := Share,
                      peers        := Peers,
                      difficulty   := Difficulty,
                      sync_allowed := SyncAllowed}) ->
    case length(Peers) =< Share of
        true ->
            case Share =< max_gossiped_peers_count() of
                true ->
                    {ok, SyncAllowed, GHash, THash, Difficulty, Peers};
                false ->
                    {error, too_many_peers_in_ping_message}
            end;
        false ->
            {error, bad_ping_message}
    end;
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
             [PeerId | [aec_peer:id(P) || P <- TheirPeers]]).

local_ping_obj(#{ kind := ConnKind, ext_sync_port := Port }) ->
    GHash = aec_chain:genesis_hash(),
    TopHash = aec_chain:top_key_block_hash(),
    {ok, Difficulty} = aec_chain:difficulty_at_top_block(),
    #{ genesis_hash => GHash,
       best_hash    => TopHash,
       difficulty   => Difficulty,
       sync_allowed => ConnKind =/= temporary,
       share        => gossiped_peers_count(),
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
    F = fun() -> aec_chain:get_at_most_n_generation_headers_forward_from_hash(Hash, N+1) end,
    case aec_db:ensure_activity(async_dirty, F) of
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
            {ok, #{ key_block := KB, micro_blocks := MBs }} ->
                SerKB = serialize_key_block(KB),
                SerMBs = [ serialize_micro_block(B) || B <- MBs ],
                {ok, #{key_block => SerKB, micro_blocks => SerMBs, forward => Forward }};
            error ->
                {error, block_not_found}
        end,
    send_response(S, generation, Response),
    S.

do_get_generation(Hash, Forward) ->
    aec_db:ensure_activity(async_dirty, fun() ->
        do_get_generation_(Hash, Forward) end).

do_get_generation_(Hash, true) ->
    aec_chain:get_generation_by_hash(Hash, forward);
do_get_generation_(Hash, false) ->
    aec_chain:get_generation_by_hash(Hash, backward).

handle_get_generation_rsp(S, {get_generation, From, _TRef}, Msg) ->
    SerKeyBlock = maps:get(key_block, Msg),
    SerMicroBlocks = maps:get(micro_blocks, Msg),
    Dir = case maps:get(forward, Msg) of true -> forward; false -> backward end,
    Res =
        case deserialize_key_block(SerKeyBlock) of
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

send_send_block(#{ status := error }, _SerBlock) ->
    ok;
send_send_block(#{ status := {disconnecting, _ESock} }, _SerBlock) ->
    ok;
send_send_block(S = #{ status := {connected, _ESock} }, {Type, SerBlock}) ->
    {MsgType, Msg} =
        case Type of
            key_block         -> {key_block, #{ key_block => SerBlock }};
            light_micro_block -> {micro_block, #{ micro_block => SerBlock, light => true }}
        end,
    send_msg(S, MsgType, aec_peer_messages:serialize(MsgType, Msg)).

handle_new_key_block(S, Msg) ->
    try
        {ok, KeyBlock} = deserialize_key_block(maps:get(key_block, Msg)),
        Header = aec_blocks:to_header(KeyBlock),
        case check_gossiped_header_height(S, Header) of
            drop -> ok;
            ok ->
                {ok, HH} = aec_headers:hash_header(Header),
                epoch_sync:debug("Got new block: ~s", [pp(HH)]),
                aec_conductor:post_block(KeyBlock)
        end
    catch _:_ ->
        ok
    end,
    S.

handle_new_micro_block(S, Msg) ->
    try
        case maps:get(light, Msg, false) of
            false ->
                {ok, MicroBlock} = deserialize_micro_block(maps:get(micro_block, Msg)),
                Header = aec_blocks:to_header(MicroBlock),
                case check_gossiped_header_height(S, Header) of
                    drop -> ok;
                    ok ->
                        {ok, HH} = aec_headers:hash_header(Header),
                        epoch_sync:debug("Got new block: ~s", [pp(HH)]),
                        %% in the full micro block case - conductor will do the necessary checks
                        aec_conductor:post_block(MicroBlock)
                end;
            true ->
                {ok, #{ header := Header, tx_hashes := TxHashes, pof := PoF }} =
                    deserialize_light_micro_block(maps:get(micro_block, Msg)),
                case check_gossiped_header_height(S, Header) of
                    drop -> ok;
                    ok   -> handle_light_micro_block(S, Header, TxHashes, PoF)
                end
        end
    catch _:_ ->
        ok
    end,
    S.

check_gossiped_header_height(S, Header) ->
    case aec_headers:height(Header) - 1 =< maps:get(sync_height, S, infinity) of
        true  -> ok;
        false -> drop
    end.

handle_light_micro_block(_S, Header, TxHashes, PoF) ->
    %% Before assembling the block, check if it is known, and valid
    case pre_assembly_check(Header) of
        known ->
            ok;
        E = {error, _} ->
            {ok, HH} = aec_headers:hash_header(Header),
            epoch_sync:info("Dropping gossiped light micro_block (~s): ~p", [pp(HH), E]),
            case aec_chain:get_header(aec_headers:prev_key_hash(Header)) of
                {ok, PrevHeader} ->
                    epoch_sync:debug("miner beneficiary: ~p", [aec_headers:beneficiary(PrevHeader)]),
                    ok;
                _ ->
                    ok
            end;

        ok ->
            case get_micro_block_txs(TxHashes) of
                {all, Txs} ->
                    MicroBlock = aec_blocks:new_micro_from_header(Header, Txs, PoF),
                    {ok, HH} = aec_headers:hash_header(Header),
                    epoch_sync:debug("Got new block: ~s", [pp(HH)]),
                    aec_conductor:post_block(MicroBlock);
                {some, TxsAndTxHashes} ->
                    epoch_sync:info("Missing txs: ~p",
                                    [[ TH || TH <- TxsAndTxHashes, is_binary(TH) ]]),
                    cast(self(), {expand_micro_block, #{ header => Header,
                                                         tx_data => TxsAndTxHashes,
                                                         pof => PoF
                                                       }}),
                    ok
            end
    end.

%% -- Get block txs ----------------------------------------------------------

expand_micro_block(State, #{ header := Header,
                             tx_data := TxsAndTxHashes } = MicroBlockFragment) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    case get_request(State, {expand, Hash}) of
        none ->
            MissingTxs = [ T || T <- TxsAndTxHashes, is_binary(T) ],
            MsgData    = #{ hash => Hash, tx_hashes => MissingTxs },
            Msg = aec_peer_messages:serialize(get_block_txs, MsgData),
            send_msg(State, get_block_txs, Msg),
            set_request(State, {expand, Hash}, MicroBlockFragment);
        {_, _, _} ->
            %% Already working on this one
            epoch_sync:info("Got the same block twice from the same source (Hash: ~s)",
                            [pp(Hash)]),
            State
    end.

handle_get_block_txs(S, Msg) ->
    Hash = maps:get(hash, Msg),
    TxHashes = maps:get(tx_hashes, Msg),
    case find_txs(TxHashes) of
        {ok, Txs} ->
            SerTxs = [ aetx_sign:serialize_to_binary(Tx) || Tx <- Txs ],
            send_response(S, block_txs, {ok, #{ hash => Hash, txs => SerTxs }});
        Err = {error, _} ->
            send_response(S, block_txs, Err)
    end,
    S.

find_txs(TxHashes) ->
    find_txs(TxHashes, []).

find_txs([], Acc) ->
    {ok, lists:reverse(Acc)};
find_txs([TxHash | TxHashes], Acc) ->
    case aec_db:find_signed_tx(TxHash) of
        none ->
            %% TODO: this is perhaps not a true error
            epoch_sync:error("Missing TX ~s", [pp(TxHash)]),
            {error, tx_missing};
        {value, Tx} ->
            find_txs(TxHashes, [Tx | Acc])
    end.

handle_get_block_txs_rsp(State, {error, _}) ->
    State; %% We can't do any cleanup - the request will timeout later
handle_get_block_txs_rsp(State, {ok, _Vsn, Msg}) ->
    Hash = maps:get(hash, Msg),
    case get_request(State, {expand, Hash}) of
        none ->
            epoch_sync:info("Got unexpected 'block_txs' message for ~s", [pp(Hash)]),
            State;
        {{expand, Hash}, MicroBlockFragment, _TRef} ->
            Txs = [ aetx_sign:deserialize_from_binary(STx) || STx <- maps:get(txs, Msg) ],
            #{ header := Header, tx_data := TxsAndTxHashes, pof := PoF } = MicroBlockFragment,
            case fill_txs(TxsAndTxHashes, Txs) of
                {ok, AllTxs} ->
                    MB = aec_blocks:new_micro_from_header(Header, AllTxs, PoF),
                    epoch_sync:info("Assembled new block: ~s", [pp(Hash)]),
                    aec_conductor:post_block(MB);
                error ->
                    epoch_sync:info("Failed to assemble micro block ~s", [pp(Hash)])
            end
    end,
    remove_request_fld(State, {expand, Hash}).

fill_txs(TATHs, Txs) ->
    fill_txs(TATHs, Txs, []).

fill_txs([], _, Acc) ->
    {ok, lists:reverse(Acc)};
fill_txs([Tx | TATHs], Txs, Acc) when not is_binary(Tx) ->
    fill_txs(TATHs, Txs, [Tx | Acc]);
fill_txs([TH | TATHs], [Tx | Txs], Acc) ->
    case aetx_sign:hash(Tx) == TH of
        true -> fill_txs(TATHs, Txs, [Tx | Acc]);
        false -> error
    end;
fill_txs([_TH | _TATHs], [], _Acc) ->
    error.


%% -- Send TX --------------------------------------------------------------

send_send_tx(#{ status := error }, _SerTx) ->
    ok;
send_send_tx(#{ status := {disconnecting, _ESock} }, _SerTx) ->
    ok;
send_send_tx(S = #{ status := {connected, _ESock} }, SerTx) ->
    Msg = aec_peer_messages:serialize(txs, #{ txs => [SerTx] }),
    send_msg(S, txs, Msg).

handle_new_txs(S, Msg) ->
    try
        #{ txs := EncSignedTxs } = Msg,
        SignedTxs = [ aetx_sign:deserialize_from_binary(Tx) || Tx <- EncSignedTxs ],
        %% Offload the handling to a temporary worker process - it might block
        %% on the tx_pool_push queue.
        spawn(fun() -> [ tx_push(SignedTx) || SignedTx <- SignedTxs ] end)
    catch _:_ ->
        ok
    end,
    S.

tx_push(STx) ->
    try aec_tx_pool:push(STx, tx_received)
    catch _:_ -> rejected end.

%% -- Send message -----------------------------------------------------------
send_msg(#{ status := {disconnecting, _ESock} }, _Type, _Msg) -> ok;
send_msg(#{ status := {connected, ESock} }, Type, Msg) when is_binary(Msg) ->
    do_send(ESock, <<(aec_peer_messages:tag(Type)):16, Msg/binary>>).

send_response(S, Type, Response) ->
    Msg = aec_peer_messages:serialize_response(Type, Response),
    send_msg(S, response, Msg).

%% If the message is more than 0xFFFF - 2 - 16 bytes (2 bytes for the length at
%% Noise protocol level and 16 bytes for the encryption MAC since we are using
%% ChaChaPoly) it won't fit in a single Noise message and we need to fragment
%% it.
%%
%% Fragments have the format <<?MSG_FRAGMENT:16, N:16, M:16,
%% PayLoad:65527/binary>> (where the last fragment has a smaller Payload, and
%% where 16 bytes of the payload is the encryption MAC), saying that this is
%% fragment N out of M fragments.
%%
-define(NOISE_PACKET_LENGTH_SIZE, 2).
-define(MAX_PACKET_SIZE, 16#FFFF - ?NOISE_PACKET_LENGTH_SIZE).

-define(CHACHAPOLY_MAC_SIZE, 16).
-define(MAX_PAYLOAD_SIZE, (?MAX_PACKET_SIZE - ?CHACHAPOLY_MAC_SIZE)).

-define(FRAGMENT_HEADER_SIZE, (2 + 2 + 2)).
-define(MAX_FRAGMENT_PAYLOAD_SIZE,
        (?MAX_PACKET_SIZE - ?FRAGMENT_HEADER_SIZE - ?CHACHAPOLY_MAC_SIZE)).

do_send(ESock, Msg) when byte_size(Msg) =< ?MAX_PAYLOAD_SIZE ->
    enoise_send(ESock, Msg);
do_send(ESock, Msg) ->
    NChunks = (?MAX_FRAGMENT_PAYLOAD_SIZE + byte_size(Msg) - 1)
                div ?MAX_FRAGMENT_PAYLOAD_SIZE,
    send_chunks(ESock, 1, NChunks, Msg).

send_chunks(ESock, M, M, Msg) ->
    enoise_send(ESock, <<?MSG_FRAGMENT:16, M:16, M:16, Msg/binary>>);
send_chunks(ESock, N, M, <<Chunk:?MAX_FRAGMENT_PAYLOAD_SIZE/binary, Rest/binary>>) ->
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

gossiped_peers_count() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"gossiped_peers_count">>],
                               aecore, gossiped_peers_count,
                               ?DEFAULT_GOSSIPED_PEERS_COUNT).

max_gossiped_peers_count() ->
    {ok, Max} = aeu_env:schema([<<"sync">>, <<"properties">>,
                                <<"gossiped_peers_count">>, <<"maximum">>]),
    Max.

%% -- Helper functions -------------------------------------------------------

gossip_serialize_block(Block) ->
    case aec_blocks:type(Block) of
        key   -> {key_block, serialize_key_block(Block)};
        micro -> {light_micro_block, serialize_light_micro_block(Block)}
    end.

gossip_serialize_tx(Tx) ->
    aetx_sign:serialize_to_binary(Tx).

serialize_key_block(KeyBlock) ->
    key = aec_blocks:type(KeyBlock),
    aec_blocks:serialize_to_binary(KeyBlock).

serialize_micro_block(MicroBlock) ->
    micro = aec_blocks:type(MicroBlock),
    aec_blocks:serialize_to_binary(MicroBlock).

serialize_light_micro_block(MicroBlock) ->
    micro = aec_blocks:type(MicroBlock),
    Hdr = aec_headers:serialize_to_binary(aec_blocks:to_header(MicroBlock)),
    TxHashes = [ aetx_sign:hash(STx) || STx <- aec_blocks:txs(MicroBlock) ],
    PoF = aec_blocks:pof(MicroBlock),
    Vsn = aec_blocks:version(MicroBlock),
    Template = light_micro_template(),
    aeser_chain_objects:serialize(
        light_micro_block, Vsn, Template,
        [{header, Hdr}, {tx_hashes, TxHashes}, {pof, aec_pof:serialize(PoF)}]).

light_micro_template() ->
    [{header, binary}, {tx_hashes, [binary]}, {pof, [binary]}].

deserialize_key_block(SKB) ->
    deserialize_block(key, SKB).

deserialize_micro_block(SMB) ->
    deserialize_block(micro, SMB).

deserialize_light_micro_block(SSMB) ->
    deserialize_block(light_micro, SSMB).

deserialize_block(light_micro, Binary) ->
    case aeser_chain_objects:deserialize_type_and_vsn(Binary) of
        {light_micro_block, Vsn, _RawFlds} ->
            Template = light_micro_template(),
            [{header, SerHdr}, {tx_hashes, TxHashes}, {pof, SerPof}] =
                aeser_chain_objects:deserialize(light_micro_block, Vsn, Template, Binary),
            Hdr = aec_headers:deserialize_from_binary(SerHdr),
            PoF = aec_pof:deserialize(SerPof),
            {ok, #{ header => Hdr, tx_hashes => TxHashes, pof => PoF}};
        _ ->
            {error, bad_light_micro_block}
    end;
deserialize_block(Type, Binary) ->
    case aec_blocks:deserialize_from_binary(Binary) of
        Err = {error, _} -> Err;
        {ok, Block} ->
            case aec_blocks:type(Block) of
                Type      -> {ok, Block};
                WrongType -> {error, {wrong_block_type, WrongType, expected, Type}}
            end
    end.

deserialize_micro_blocks([], Acc) ->
    {ok, lists:reverse(Acc)};
deserialize_micro_blocks([SMB | SMBs], Acc) ->
    case deserialize_micro_block(SMB) of
        {ok, MB}         -> deserialize_micro_blocks(SMBs, [MB | Acc]);
        Err = {error, _} -> Err
    end.

get_micro_block_txs(TxHashes) ->
    get_micro_block_txs(TxHashes, all, []).

get_micro_block_txs([], State, Acc) ->
    {State, lists:reverse(Acc)};
get_micro_block_txs([TxHash | TxHashes], State, Acc) ->
    case aec_db:find_signed_tx(TxHash) of
        none         -> get_micro_block_txs(TxHashes, some, [TxHash | Acc]);
        {value, STx} -> get_micro_block_txs(TxHashes, State, [STx | Acc])
    end.

pre_assembly_check(MicroHeader) ->
    {ok, Hash} = aec_headers:hash_header(MicroHeader),
    case aec_db:has_block(Hash) of
        true -> known;
        false ->
            {PrevHeader, PrevKeyHeader} = get_onchain_env(MicroHeader),
            Validators = [fun validate_micro_block_header/3,
                          fun validate_connected_to_chain/3,
                          fun validate_delta_height/3,
                          fun validate_prev_key_block/3,
                          fun validate_micro_signature/3],
            aeu_validation:run(Validators, [MicroHeader, PrevHeader, PrevKeyHeader])
    end.

get_onchain_env(MicroHeader) ->
    PrevHeaderHash = aec_headers:prev_hash(MicroHeader),
    PrevKeyHash = aec_headers:prev_key_hash(MicroHeader),
    case aec_chain:get_header(PrevHeaderHash) of
        {ok, PrevHeader} ->
            case aec_headers:type(PrevHeader) of
                key ->
                    {PrevHeader, PrevHeader};
                micro ->
                    case aec_chain:get_header(PrevKeyHash) of
                        {ok, PrevKeyHeader} -> {PrevHeader, PrevKeyHeader};
                        error               -> {PrevHeader, not_found}
                    end
            end;
        error ->
            {not_found, not_found}
    end.

validate_micro_block_header(MicroHeader, _PrevHeader, PrevKeyHeader)
  when PrevKeyHeader =/= not_found ->
    Protocol = aec_headers:version(PrevKeyHeader),
    aec_headers:validate_micro_block_header(MicroHeader, Protocol);
validate_micro_block_header(_MicroHeader, _PrevHeader, not_found) ->
    {error, prev_key_block_not_found}.

validate_connected_to_chain(MicroHeader, _PrevHeader, _PrevKeyHeader) ->
    PrevHeaderHash = aec_headers:prev_hash(MicroHeader),
    case aec_chain_state:hash_is_connected_to_genesis(PrevHeaderHash) of
        true  -> ok;
        false -> {error, orphan_blocks_not_allowed}
    end.

validate_delta_height(MicroHeader, _PrevHeader, _PrevKeyHeader) ->
    Height = aec_headers:height(MicroHeader),
    case aec_chain:top_header() of
        undefined -> ok;
        TopHeader ->
            MaxDelta = aec_chain_state:gossip_allowed_height_from_top(),
            case Height >= aec_headers:height(TopHeader) - MaxDelta of
                true  -> ok;
                false -> {error, too_far_below_top}
            end
    end.

validate_prev_key_block(MicroHeader, PrevHeader, _PrevKeyHeader)
  when PrevHeader =/= not_found ->
    case aec_headers:height(PrevHeader) =:= aec_headers:height(MicroHeader) of
        false -> {error, wrong_prev_key_block_height};
        true ->
            case aec_headers:type(PrevHeader) of
                key ->
                    case aec_headers:prev_key_hash(MicroHeader) =:=
                        aec_headers:prev_hash(MicroHeader) of
                        true -> ok;
                        false -> {error, wrong_prev_key_hash}
                    end;
                micro ->
                    case aec_headers:prev_key_hash(MicroHeader) =:=
                        aec_headers:prev_key_hash(PrevHeader) of
                        true -> ok;
                        false -> {error, wrong_prev_key_hash}
                    end
            end
    end;
validate_prev_key_block(_MicroHeader, not_found, _PrevKeyHeader) ->
    {error, orphan_blocks_not_allowed}.

validate_micro_signature(MicroHeader, _PrevHeader, PrevKeyHeader)
  when PrevKeyHeader =/= not_found ->
    Bin = aec_headers:serialize_to_signature_binary(MicroHeader),
    Sig = aec_headers:signature(MicroHeader),
    Signer = aec_headers:miner(PrevKeyHeader),
    case enacl:sign_verify_detached(Sig, Bin, Signer) of
        {ok, _}    -> ok;
        {error, _} -> {error, signature_verification_failed}
    end;
validate_micro_signature(_MicroHeader, _PrevHeader, not_found) ->
    {error, signer_not_found}.

%% -- Node info  message -----------------------------------------------------------
handle_get_node_info(S) ->
    case is_node_info_sharing_enabled() of
        false -> S;
        true ->
            NodeVersion = aeu_info:get_version(),
            NodeRevision = aeu_info:get_revision(),
            OS = aeu_info:get_os(),
            Verified = aec_peers:count(verified),
            Unverified = aec_peers:count(unverified),
            NodeInfo = #{ version => NodeVersion
                        , revision => NodeRevision 
                        , vendor => aeu_info:vendor()
                        , os => OS
                        , network_id => aec_governance:get_network_id()
                        , verified_peers => Verified
                        , unverified_peers => Unverified },
            send_response(S, node_info, {ok, NodeInfo}),
            S
    end.

handle_node_info_rsp(S, {node_info, From, _TRef} = _Request, NodeInfo) ->
    gen_server:reply(From, {ok, NodeInfo}),
    remove_request_fld(S, get_node_info).

-spec is_node_info_sharing_enabled() -> boolean().
is_node_info_sharing_enabled() ->
    case aeu_env:find_config([<<"sync">>, <<"provide_node_info">>], [user_config, schema_default, {value, true}]) of
        {ok, Val} -> Val;
        undefined -> true
    end.
