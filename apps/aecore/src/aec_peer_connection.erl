%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module encapsulating a P2P communication channel
%%% @end
%%%=============================================================================
-module(aec_peer_connection).

-export([ accept/2
        , connect/1
        , retry/1

        , get_block/2
        , get_header_by_hash/2
        , get_header_by_height/2
        , get_mempool/1
        , get_n_successors/3
        , ping/1
        , send_block/2
        , send_tx/2
        , start_link/2
        , stop/1
        ]).

-include("aec_peer_messages.hrl").

-behaviour(gen_server).

-import(aeu_debug, [pp/1]).

-define(P2P_PROTOCOL_VSN, 2).

-define(FIRST_PING_TIMEOUT, 30000).
-define(NOISE_HS_TIMEOUT, 5000).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% -- API --------------------------------------------------------------------

connect(#{} = Options) ->
    lager:debug("New peer connection to ~p", [Options]),
    aec_peer_connection_sup:start_peer_connection(Options#{ role => initiator}).

accept(TcpSock, Options) ->
    {ok, {Host, _Port}} = inet:peername(TcpSock),
    Options1 = Options#{ tcp_sock => TcpSock
                       , role => responder
                       , host => list_to_binary(inet:ntoa(Host))},
    case aec_peer_connection_sup:start_peer_connection(Options1) of
        {ok, Pid} ->
            gen_tcp:controlling_process(TcpSock, Pid),
            gen_server:cast(Pid, give_tcp_socket_ownership),
            {ok, Pid};
        Res ->
            Res
    end.

start_link(Port, Opts) ->
    Opts1 = Opts#{ext_sync_port => Port},
    gen_server:start_link(?MODULE, [Opts1], []).

retry(PeerCon) ->
    gen_server:cast(PeerCon, retry).

ping(PeerId) ->
    call(PeerId, {ping, []}).

get_header_by_hash(PeerId, Hash) ->
    call(PeerId, {get_header_by_hash, [Hash]}).

get_header_by_height(PeerId, Hash) ->
    call(PeerId, {get_header_by_height, [Hash]}).

get_n_successors(PeerId, Hash, N) ->
    call(PeerId, {get_n_successors, [Hash, N]}).

get_block(PeerId, Hash) ->
    call(PeerId, {get_block, [Hash]}).

get_mempool(PeerId) ->
    call(PeerId, {get_mempool, []}).

send_tx(PeerId, Tx) ->
    cast(PeerId, {send_tx, Tx}).

send_block(PeerId, Tx) ->
    cast(PeerId, {send_block, Tx}).

stop(PeerCon) ->
    gen_server:cast(PeerCon, stop).


call(PeerCon, Call) when is_pid(PeerCon) ->
    try gen_server:call(PeerCon, Call)
    catch exit:{noproc, _} -> {error, no_connection}
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
        {ok, PeerCon} ->
            case PeerCon of
                {connected, Pid} when Action == call -> call(Pid, CastOrCall);
                {connected, Pid} when Action == cast -> cast(Pid, CastOrCall);
                _ -> {error, no_connection}
            end;
        Err = {error, _} ->
            Err
    end.

%% -- gen_server callbacks ---------------------------------------------------

init([Opts]) ->
    Version = <<?P2P_PROTOCOL_VSN:64>>,
    Genesis = aec_chain:genesis_hash(),
    {ok, Opts#{ version => Version, genesis => Genesis }, 0}.

handle_call(Request, From, State) ->
    handle_request(State, Request, From).

handle_cast(retry, S = #{ host := Host, port := Port, status := error }) ->
    Self = self(),
    Pid = spawn(fun() -> try_connect(Self, Host, Port, 1000) end),
    {noreply, S#{ status => {connecting, Pid} }};
handle_cast(give_tcp_socket_ownership, S0 = #{ role := responder, tcp_sock := TcpSock }) ->
    S = #{ version := Version, genesis := Genesis,
           seckey := SecKey, pubkey := PubKey } = ensure_genesis(S0),
    NoiseOpts = [ {noise, <<"Noise_XK_25519_ChaChaPoly_BLAKE2b">>}
                , {s, enoise_keypair:new(dh25519, SecKey, PubKey)}
                , {prologue, <<Version/binary, Genesis/binary>>}
                , {timeout, ?NOISE_HS_TIMEOUT} ],
    %% Keep the socket passive until here to avoid premature message
    %% receiving...
    inet:setopts(TcpSock, [{active, true}]),
    case enoise:accept(TcpSock, NoiseOpts) of
        {ok, ESock, FinalState} ->
            RemotePub = enoise_keypair:pubkey(enoise_hs_state:remote_keys(FinalState)),
            lager:debug("Connection accepted from ~p", [RemotePub]),
            %% Report this to aec_peers!? And possibly fail?
            %% Or, we can't do this yet we don't know the port?!
            TRef = erlang:start_timer(?FIRST_PING_TIMEOUT, self(), first_ping_timeout),
            {noreply, S#{ status => {connected, ESock}, r_pubkey => RemotePub,
                          first_ping_tref => TRef }};
        {error, Reason} ->
            %% What to do here? Close the socket and stop?
            lager:info("Connection accept failed - ~p was from ~p", [Reason, maps:get(host, S)]),
            gen_tcp:close(TcpSock),
            {stop, normal, S}
    end;
handle_cast({send_tx, Hash}, State) ->
    send_send_tx(State, Hash),
    {noreply, State};
handle_cast({send_block, Hash}, State) ->
    send_send_block(State, Hash),
    {noreply, State};
handle_cast(stop, State) ->
    cleanup_connection(State),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, S = #{ role := initiator, host := Host, port := Port }) ->
    Self = self(),
    Pid = spawn(fun() -> try_connect(Self, Host, Port, 1000) end),
    {noreply, S#{ status => {connecting, Pid} }};
handle_info(timeout, S) ->
    {noreply, S};
handle_info({timeout, Ref, first_ping_timeout}, S) ->
    NonRegAccept = is_non_registered_accept(S),
    case maps:get(first_ping_tref, S, undefined) of
        Ref when NonRegAccept ->
            lager:info("Connecting peer never sent ping, stopping"),
            %% Consider blocking this peer
            cleanup_connection(S),
            {stop, normal, S};
        _ ->
            lager:debug("Got stale first_ping_timeout", []),
            {noreply, S}
    end;
handle_info({connected, Pid, Err = {error, _}}, S = #{ status := {connecting, Pid}}) ->
    lager:debug("Failed to connected to ~p: ~p", [{maps:get(host, S), maps:get(port, S)}, Err]),
    connect_fail(S);
handle_info({connected, Pid, {ok, TcpSock}}, S0 = #{ status := {connecting, Pid} }) ->
    S = #{ version := Version, genesis := Genesis,
       seckey := SecKey, pubkey := PubKey, r_pubkey := RemotePub } = ensure_genesis(S0),
    NoiseOpts = [ {noise, <<"Noise_XK_25519_ChaChaPoly_BLAKE2b">>}
                , {s, enoise_keypair:new(dh25519, SecKey, PubKey)}
                , {prologue, <<Version/binary, Genesis/binary>>}
                , {rs, enoise_keypair:new(dh25519, RemotePub)}
                , {timeout, ?NOISE_HS_TIMEOUT} ],
    %% Keep the socket passive until here to avoid premature message
    %% receiving...
    inet:setopts(TcpSock, [{active, true}]),
    case enoise:connect(TcpSock, NoiseOpts) of
        {ok, ESock, _} ->
            lager:debug("Peer connected to ~p", [{maps:get(host, S), maps:get(port, S)}]),
            case aec_peers:connect_peer(peer_id(S), self()) of
                ok ->
                    {noreply, S#{ status => {connected, ESock} }};
                {error, _} ->
                    lager:debug("Dropping unnecessary connection to ~p", [maps:get(host, S)]),
                    enoise:close(ESock),
                    {stop, normal, S}
            end;
        {error, Reason} ->
            %% For now lets report connect_fail
            %% Maybe instead report permanent fail + block?!
            lager:info("Connection handshake failed - ~p was from ~p",
                       [Reason, maps:get(host, S)]),
            gen_tcp:close(TcpSock),
            connect_fail(S)
    end;

handle_info({noise, _, <<?MSG_FRAGMENT:16, N:16, M:16, Fragment/binary>>}, S) ->
    handle_fragment(S, N, M, Fragment);
handle_info({noise, _, <<Type:16, Payload/binary>>}, S) ->
    case aec_peer_messages:deserialize(Type, Payload) of
        {response, _Vsn, #{ result := false, type := MsgType, reason := Reason }} ->
            {noreply, handle_msg(S, MsgType, true, {error, Reason})};
        {response, _Vsn, #{ result := true, type := MsgType, msg := Msg }} ->
            {noreply, handle_msg(S, MsgType, true, {ok, Msg})};
        {MsgType, _Vsn, Msg} ->
            {noreply, handle_msg(S, MsgType, false, {ok, Msg})};
        Err = {error, _} ->
            lager:info("Could not deserialize message ~p", [Err]),
            {noreply, S}
    end;
handle_info({tcp_closed, _}, S) ->
    lager:debug("Peer connection got tcp_closed", []),
    case is_non_registered_accept(S) of
        true ->
            {stop, normal, S};
        false ->
            connect_fail(S)
    end;
handle_info(_Msg, S) ->
    {noreply, S}.

terminate(normal, _State) ->
    ok;
terminate(NonNormal, State) ->
    lager:info("Non-normal terminate, reason: ~p", [NonNormal]),
    cleanup_connection(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- Local functions --------------------------------------------------------
get_request(S, Kind) ->
    maps:get(map_request(Kind), maps:get(requests, S, #{}), none).

set_request(S, Kind, From) ->
    Rs = maps:get(requests, S, #{}),
    S#{ requests => Rs#{ Kind => {Kind, From} } }.

remove_request_fld(S, Kind) ->
    Rs = maps:get(requests, S, #{}),
    S#{ requests => maps:remove(map_request(Kind), Rs) }.

map_request(mempool)              -> get_mempool;
map_request(get_header_by_hash)   -> get_header;
map_request(get_header_by_height) -> get_header;
map_request(header)               -> get_header;
map_request(header_hashes)        -> get_n_successors;
map_request(block)                -> get_block;
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
        {connected, ESock} -> enoise:close(ESock);
        _                  -> ok
    end,
    case maps:get(gen_tcp, State, undefined) of
        TSock when is_port(TSock) -> gen_tcp:close(TSock);
        _                         -> ok
    end,
    cleanup_requests(State).

cleanup_requests(State) ->
    Reqs = maps:to_list(maps:get(requests, State, #{})),
    [ gen_server:reply(From, {error, disconnected})
      || {_Request, From} <- Reqs ].

connect_fail(S) ->
    cleanup_requests(S),
    case aec_peers:connect_fail(peer_id(S), self()) of
        keep ->
            {noreply, S#{ status := error }};
        close ->
            {stop, normal, S}
    end.

handle_fragment(S, 1, _M, Fragment) ->
    {noreply, S#{ fragments => [Fragment] }};
handle_fragment(S = #{ fragments := Fragments }, M, M, Fragment) ->
    Msg = list_to_binary(lists:reverse([Fragment | Fragments])),
    self() ! {noise, unused, Msg},
    {noreply, maps:remove(fragments, S)};
handle_fragment(S = #{ fragments := Fragments }, N, _M, Fragment) when N == length(Fragments) + 1 ->
    {noreply, S#{ fragments := [Fragment | Fragments] }};
handle_fragment(S = #{ fragments := Fragments }, N, M, _Fragment) ->
    lager:error("Got fragment ~p, expected ~p (out of ~p)", [N, length(Fragments) + 1, M]),
    cleanup_connection(S),
    connect_fail(maps:remove(fragments, S)).

handle_msg(S, MsgType, IsResponse, Result) ->
    handle_msg(S, MsgType, IsResponse, get_request(S, MsgType), Result).

handle_msg(S, MsgType, true, none, Result) ->
    lager:info("Peer ~p got unexpected ~p response - ~p",
               [peer_id(S), MsgType, Result]),
    S;
handle_msg(S, _MsgType, true, {RequestFld, From}, {error, Reason}) ->
    gen_server:reply(From, {error, Reason}),
    remove_request_fld(S, RequestFld);
handle_msg(S, MsgType, false, Request, {ok, Msg}) ->
    case MsgType of
        ping                 -> handle_ping(S, Request, Msg);
        get_mempool          -> handle_get_mempool(S, Msg);
        get_header_by_hash   -> handle_get_header_by_hash(S, Msg);
        get_header_by_height -> handle_get_header_by_height(S, Msg);
        get_n_successors     -> handle_get_n_successors(S, Msg);
        get_block            -> handle_get_block(S, Msg);
        block                -> handle_new_block(S, Msg);
        tx                   -> handle_new_tx(S, Msg)
    end;
handle_msg(S, MsgType, true, Request, {ok, {MsgType, _Vsn, Msg}}) ->
    case MsgType of
        ping    -> handle_ping_rsp(S, Request, Msg);
        mempool -> handle_get_mempool_rsp(S, Request, Msg);
        header  -> handle_header_rsp(S, Request, Msg);
        header_hashes -> handle_header_hashes_rsp(S, Request, Msg);
        block   -> handle_get_block_rsp(S, Request, Msg)
    end.

handle_request(S, {Request, Args}, From) ->
    MappedRequest = map_request(Request),
    case get_request(S, Request) of
        none ->
            handle_request(S, Request, Args, From);
        {MappedRequest, _From} ->
            {reply, {error, request_already_in_progress}, S}
    end.

handle_request(S = #{ status := error }, _Req, _Args, _From) ->
    {reply, {error, disconnected}, S};
handle_request(S, Request, Args, From) ->
    ReqData = prepare_request_data(S, Request, Args),
    send_msg(S, Request, aec_peer_messages:serialize(Request, ReqData)),
    {noreply, set_request(S, map_request(Request), From)}.

prepare_request_data(S, ping, []) ->
    ping_obj(local_ping_obj(S), [peer_id(S)]);
prepare_request_data(_S, get_header_by_hash, [Hash]) ->
    #{ hash => Hash };
prepare_request_data(_S, get_header_by_height, [Height]) ->
    #{ height => Height };
prepare_request_data(_S, get_n_successors, [Hash, N]) ->
    #{ hash => Hash, n => N };
prepare_request_data(_S, get_block, [Hash]) ->
    #{ hash => Hash };
prepare_request_data(_S, get_mempool, []) ->
    #{}.

%% -- Ping message -----------------------------------------------------------

handle_ping_rsp(S, {ping, From}, RemotePingObj) ->
    Res = handle_ping_msg(S, RemotePingObj),
    gen_server:reply(From, Res),
    remove_request_fld(S, ping).

handle_ping(S, none, RemotePingObj) ->
    {PeerOk, S1} = handle_first_ping(S, RemotePingObj),

    Response =
        case {PeerOk, handle_ping_msg(S1, RemotePingObj)} of
            {ok, ok} ->
                {ok, ping_obj_rsp(S1, RemotePingObj)};
            {{error, Reason}, _} ->
                {error, Reason};
            {_, {error, Reason}} ->
                {error, Reason}
        end,
    send_response(S, ping, Response),
    S1;
handle_ping(S, {ping, _From}, RemotePingObj) ->
    lager:info("Peer ~p got new ping when waiting for PING_RSP - ~p",
               [peer_id(S), RemotePingObj]),
    send_response(S, ping, {error, already_pinging}),
    S.

handle_first_ping(S, RemotePingObj) ->
    case is_non_registered_accept(S) of
        true ->
            %% This is the first ping, now we have everything - accept the peer
            #{port := Port} = RemotePingObj,
            Peer = #{ host => maps:get(host, S), port => Port,
                      pubkey => maps:get(r_pubkey, S), connection => self(),
                      ping => false, trusted => false },

            %% Cancel the first ping_timeout
            case maps:get(first_ping_tref, S, undefined) of
                undefined -> ok;
                Ref -> try erlang:cancel_timer(Ref, [{async, true}, {info, false}])
                       catch error:_ -> ok end
            end,

            NewS = maps:remove(first_ping_tref, S#{ port => Port }),
            case aec_peers:accept_peer(Peer, self()) of
                ok ->
                    {ok, NewS};
                Err = {error, _} ->
                    gen_server:cast(self(), stop),
                    {Err, NewS}
            end;
        false ->
            {ok, S}
    end.

handle_ping_msg(S, RemotePingObj) ->
    #{ genesis_hash := LGHash,
       best_hash    := LTHash,
       difficulty   := LDiff } = local_ping_obj(S),
    PeerId = peer_id(S),
    case decode_remote_ping(RemotePingObj) of
        {ok, RGHash, RTHash, RDiff, RPeers} when RGHash == LGHash ->
            case {{LTHash, LDiff}, {RTHash, RDiff}} of
                {{T, _}, {T, _}} ->
                    lager:debug("Same top blocks", []),
                    aec_sync:server_get_missing_blocks(PeerId);
                {{_, DL}, {_, DR}} when DL > DR ->
                    lager:debug("Our difficulty is higher", []),
                    aec_sync:server_get_missing_blocks(PeerId);
                {{_, _}, {_, DR}} ->
                    aec_sync:start_sync(PeerId, RTHash, DR)
            end,
            ok = aec_peers:add_and_ping_peers(RPeers),
            ok = aec_sync:fetch_mempool(PeerId),
            ok;
        {ok, _RGHash, _RTHash, _RDiff, _RPeers} ->
            {error, wrong_genesis_hash};
        {error, Reason} ->
            {error, Reason}
    end.

decode_remote_ping(#{ genesis_hash := GHash,
                      best_hash    := THash,
                      peers        := Peers,
                      difficulty   := Difficulty }) ->
    {ok, GHash, THash, Difficulty, Peers};
decode_remote_ping(_) ->
    {error, bad_ping_message}.

%% Encode hashes and get peers for PingObj
ping_obj(PingObj, Exclude) ->
    #{share := Share} = PingObj,
    Peers = aec_peers:get_random(Share, Exclude),
    PingObj#{peers => Peers}.

ping_obj_rsp(S, RemotePingObj) ->
    PeerId = peer_id(S),
    #{ share := Share, peers := TheirPeers } = RemotePingObj,
    LocalPingObj = local_ping_obj(S),
    ping_obj(LocalPingObj#{ share => Share },
             [PeerId | [aec_peers:peer_id(P) || P <- TheirPeers]]).

local_ping_obj(#{ext_sync_port := Port}) ->
    GHash = aec_chain:genesis_hash(),
    TopHash = aec_chain:top_header_hash(),
    {ok, Difficulty} = aec_chain:difficulty_at_top_block(),
    #{genesis_hash => GHash,
      best_hash    => TopHash,
      difficulty   => Difficulty,
      share        => 32,  % TODO: make this configurable
      peers        => [],
      port         => Port}.

%% -- Get Header by Hash -----------------------------------------------------

handle_get_header_by_hash(S, Msg) ->
    Response = get_header(maps:get(hash, Msg, undefined)),
    send_response(S, header, Response),
    S.

handle_header_rsp(S, {get_header, From}, Msg) ->
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
    get_header(fun aec_chain:get_header_by_height/1, N);
get_header(Fun, Arg) ->
    case Fun(Arg) of
        {ok, Header} ->
            HH = aec_headers:serialize_to_binary(Header),
            {ok, #{ hdr => HH }};
        Err when Err == error orelse Err == {error, chain_too_short} ->
            {error, header_not_found}
    end.

%% -- Get Header by Height -----------------------------------------------------

handle_get_header_by_height(S, Msg) ->
    Response = get_header(maps:get(height, Msg, undefined)),
    send_response(S, header, Response),
    S.

%% Response is handled above in Get Header by Hash

%% -- Get N Successors -------------------------------------------------------

handle_get_n_successors(S, Msg) ->
    Response =
        case Msg of
            #{ hash := Hash, n := N } ->
                 case aec_chain:get_at_most_n_headers_forward_from_hash(Hash, N+1) of
                     {ok, [_ | Headers]} ->
                         HHashes = [ begin
                                         {ok, HHash} = aec_headers:hash_header(H),
                                         <<(aec_headers:height(H)):64, HHash/binary>>
                                     end || H <- Headers ],
                         {ok, #{ header_hashes => HHashes }};
                     error ->
                         {error, block_not_found}
                 end;
            _ ->
                {error, bad_request}
        end,
    send_response(S, header_hashes, Response),
    S.

handle_header_hashes_rsp(S, {get_n_successors, From}, Msg) ->
    case maps:get(header_hashes, Msg, undefined) of
        Hdrs when is_list(Hdrs) ->
            Hdrs1 = [ {Height, Hash} || <<Height:64, Hash/binary>> <- Hdrs ],
            gen_server:reply(From, {ok, Hdrs1});
        _ ->
            gen_server:reply(From, {error, no_headers_in_response})
    end,
    remove_request_fld(S, get_n_successors).

%% -- Get Block --------------------------------------------------------------

handle_get_block(S, Msg) ->
    Response =
        case maps:get(hash, Msg, undefined) of
            Hash when is_binary(Hash) ->
                case aec_chain:get_block(Hash) of
                    {ok, Block} ->
                        SerBlock = aec_blocks:serialize_to_binary(Block),
                        {ok, #{block => SerBlock}};
                    error ->
                        {error, block_not_found}
                end;
            _ ->
                {error, bad_request}
        end,
    send_response(S, block, Response),
    S.

handle_get_block_rsp(S, {get_block, From}, Msg) ->
    case maps:get(block, Msg, undefined) of
        Block0 when is_binary(Block0) ->
            Block = aec_blocks:deserialize_from_binary(Block0),
            gen_server:reply(From, {ok, Block});
        _ ->
            gen_server:reply(From, {error, no_block_in_response})
    end,
    remove_request_fld(S, get_block).

%% -- Get Mempool --------------------------------------------------------------

handle_get_mempool(S, _MsgObj) ->
    {ok, Txs0} = aec_tx_pool:peek(infinity),
    Txs = [ aetx_sign:serialize_to_binary(T) || T <- Txs0 ],
    send_response(S, mempool, {ok, #{ txs => Txs }}),
    S.

handle_get_mempool_rsp(S, {get_mempool, From}, MsgObj) ->
    case maps:get(txs, MsgObj, undefined) of
        Txs0 when is_list(Txs0) ->
            Txs = [ aetx_sign:deserialize_from_binary(T) || T <- Txs0 ],
            gen_server:reply(From, {ok, Txs});
        _ ->
            gen_server:reply(From, {error, bad_response})
    end,
    remove_request_fld(S, get_mempool).

%% -- Send Block --------------------------------------------------------------

send_send_block(#{ status := error }, _Block) ->
    ok;
send_send_block(S = #{ status := {connected, _ESock} }, Block) ->
    SerBlock = aec_blocks:serialize_to_binary(Block),
    Msg = aec_peer_messages:serialize(block, #{ block => SerBlock }),
    send_msg(S, block, Msg).

handle_new_block(S, Msg) ->
    try
        Block = aec_blocks:deserialize_from_binary(maps:get(block, Msg)),
        Header = aec_blocks:to_header(Block),
        {ok, HH} = aec_headers:hash_header(Header),
        lager:debug("Got new block: ~s", [pp(HH)]),
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
    Msg = aec_peer_messages:serialize(tx, #{ tx => TxSerialized }),
    send_msg(S, tx, Msg).

handle_new_tx(S, Msg) ->
    try
        #{ tx := EncSignedTx } = Msg,
        SignedTx = aetx_sign:deserialize_from_binary(EncSignedTx),
        aec_tx_pool:push(SignedTx, tx_received)
    catch _:_ ->
        ok
    end,
    S.

%% -- Send message -----------------------------------------------------------
send_msg(#{ status := {connected, ESock} }, Type, Msg) when is_binary(Msg) ->
    lager:debug("Sending ~p - ~p", [Type, Msg]),
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
    enoise:send(ESock, Msg);
do_send(ESock, Msg) ->
    NChunks = (?FRAGMENT_SIZE + byte_size(Msg) - 1) div ?FRAGMENT_SIZE,
    send_chunks(ESock, 1, NChunks, Msg).

send_chunks(ESock, M, M, Msg) ->
    enoise:send(ESock, <<?MSG_FRAGMENT:16, M:16, M:16, Msg/binary>>);
send_chunks(ESock, N, M, <<Chunk:?FRAGMENT_SIZE/binary, Rest/binary>>) ->
    enoise:send(ESock, <<?MSG_FRAGMENT:16, N:16, M:16, Chunk/binary>>),
    send_chunks(ESock, N + 1, M, Rest).

peer_id(#{ r_pubkey := PK }) ->
    PK.
