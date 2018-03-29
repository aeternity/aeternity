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

        , stop/1
        ]).

-behaviour(gen_server).

-import(aeu_debug, [pp/1]).

-define(P2P_PROTOCOL_VSN, 1).

-define(MSG_FRAGMENT, 0).
-define(MSG_PING, 1).
-define(MSG_PING_RSP, 2).
-define(MSG_GET_HEADER_BY_HASH, 3).
-define(MSG_GET_HEADER_BY_HASH_RSP, 4).
-define(MSG_GET_N_SUCCESSORS, 5).
-define(MSG_GET_N_SUCCESSORS_RSP, 6).
-define(MSG_GET_BLOCK, 7).
-define(MSG_GET_BLOCK_RSP, 8).
-define(MSG_SEND_TX, 9).
-define(MSG_SEND_TX_RSP, 10).
-define(MSG_SEND_BLOCK, 11).
-define(MSG_SEND_BLOCK_RSP, 12).
-define(MSG_GET_MEMPOOL, 13).
-define(MSG_GET_MEMPOOL_RSP, 14).
-define(MSG_GET_HEADER_BY_HEIGHT, 15).
-define(MSG_GET_HEADER_BY_HEIGHT_RSP, 16).

-define(FIRST_PING_TIMEOUT, 30000).
-define(NOISE_HS_TIMEOUT, 5000).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% -- API --------------------------------------------------------------------

connect(Options) ->
    lager:debug("New peer connection to ~p", [Options]),
    gen_server:start_link(?MODULE, [Options#{ role => initiator }], []).

accept(TcpSock, Options) ->
    {ok, {Host, _Port}} = inet:peername(TcpSock),
    case gen_server:start_link(?MODULE, [Options#{ tcp_sock => TcpSock, role => responder,
                                                   host => inet_parse:ntoa(Host)}], []) of
        {ok, Pid} ->
            gen_tcp:controlling_process(TcpSock, Pid),
            gen_server:cast(Pid, give_tcp_socket_ownership),
            {ok, Pid};
        Res ->
            Res
    end.

retry(PeerCon) ->
    gen_server:cast(PeerCon, retry).

ping(PeerId) ->
    call(PeerId, ping).

get_header_by_hash(PeerId, Hash) ->
    call(PeerId, {get_header_by_hash, Hash}).

get_header_by_height(PeerId, Hash) ->
    call(PeerId, {get_header_by_height, Hash}).

get_n_successors(PeerId, Hash, N) ->
    call(PeerId, {get_n_successors, Hash, N}).

get_block(PeerId, Hash) ->
    call(PeerId, {get_block, Hash}).

get_mempool(PeerId) ->
    call(PeerId, get_mempool).

send_tx(PeerId, Tx) ->
    call(PeerId, {send_tx, Tx}).

send_block(PeerId, Tx) ->
    call(PeerId, {send_block, Tx}).

stop(PeerCon) ->
    gen_server:cast(PeerCon, stop).


call(PeerCon, Call) when is_pid(PeerCon) ->
    gen_server:call(PeerCon, Call);
call(PeerId, Call) when is_binary(PeerId) ->
    case aec_peers:get_connection(PeerId) of
        {ok, PeerCon}    ->
            case PeerCon of
                {connected, Pid} -> call(Pid, Call);
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

handle_call(ping, From, State) ->
    send_ping(State, From, local_ping_obj(State));
handle_call({get_header_by_hash, Hash}, From, State) ->
    send_get_header_by_hash(State, From, Hash);
handle_call({get_header_by_height, Hash}, From, State) ->
    send_get_header_by_height(State, From, Hash);
handle_call({get_n_successors, Hash, N}, From, State) ->
    send_get_n_successors(State, From, Hash, N);
handle_call({get_block, Hash}, From, State) ->
    send_get_block(State, From, Hash);
handle_call(get_mempool, From, State) ->
    send_get_mempool(State, From);
handle_call({send_tx, Hash}, From, State) ->
    send_send_tx(State, From, Hash);
handle_call({send_block, Hash}, From, State) ->
    send_send_block(State, From, Hash).

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
handle_cast(stop, State) ->
    close_connection(State),
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
            close_connection(S),
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
                    lager:debug("Dropping unnecessary connection to ", [maps:get(host, S)]),
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
    {ok, Msg} = msgpack:unpack(Payload),
    S1 = handle_msg(S, Type, Msg),
    {noreply, S1};
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

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- Local functions --------------------------------------------------------
get_request(S, Kind) ->
    maps:get(Kind, maps:get(requests, S, #{}), none).

set_request(S, Kind, From) ->
    Rs = maps:get(requests, S, #{}),
    S#{ requests => Rs#{ Kind => {Kind, From} } }.

remove_request_fld(S, Kind) ->
    Rs = maps:get(requests, S, #{}),
    S#{ requests => maps:remove(Kind, Rs) }.

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

close_connection(State) ->
    case maps:get(status, State, error) of
        {connected, ESock} -> enoise:close(ESock);
        _                  -> ok
    end,
    case maps:get(gen_tcp, State, undefined) of
        TSock when is_port(TSock) -> gen_tcp:close(TSock);
        _                         -> ok
    end.

connect_fail(S) ->
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
    close_connection(S),
    connect_fail(maps:remove(fragments, S)).

handle_msg(S, ?MSG_PING, PingObj) ->
    handle_ping(S, get_request(S, ping), PingObj);
handle_msg(S, ?MSG_PING_RSP, PingObj) ->
    handle_ping_rsp(S, get_request(S, ping), PingObj);
handle_msg(S, ?MSG_GET_HEADER_BY_HASH, MsgObj) ->
    handle_get_header_by_hash(S, MsgObj);
handle_msg(S, ?MSG_GET_HEADER_BY_HASH_RSP, MsgObj) ->
    handle_get_header_by_hash_rsp(S, get_request(S, get_header_by_hash), MsgObj);
handle_msg(S, ?MSG_GET_HEADER_BY_HEIGHT, MsgObj) ->
    handle_get_header_by_height(S, MsgObj);
handle_msg(S, ?MSG_GET_HEADER_BY_HEIGHT_RSP, MsgObj) ->
    handle_get_header_by_height_rsp(S, get_request(S, get_header_by_height), MsgObj);
handle_msg(S, ?MSG_GET_N_SUCCESSORS, MsgObj) ->
    handle_get_n_successors(S, MsgObj);
handle_msg(S, ?MSG_GET_N_SUCCESSORS_RSP, MsgObj) ->
    handle_get_n_successors_rsp(S, get_request(S, get_n_successors), MsgObj);
handle_msg(S, ?MSG_GET_BLOCK, MsgObj) ->
    handle_get_block(S, MsgObj);
handle_msg(S, ?MSG_GET_BLOCK_RSP, MsgObj) ->
    handle_get_block_rsp(S, get_request(S, get_block), MsgObj);
handle_msg(S, ?MSG_GET_MEMPOOL, MsgObj) ->
    handle_get_mempool(S, MsgObj);
handle_msg(S, ?MSG_GET_MEMPOOL_RSP, MsgObj) ->
    handle_get_mempool_rsp(S, get_request(S, get_mempool), MsgObj);
handle_msg(S, ?MSG_SEND_TX, MsgObj) ->
    handle_send_tx(S, MsgObj);
handle_msg(S, ?MSG_SEND_TX_RSP, MsgObj) ->
    handle_send_tx_rsp(S, get_request(S, send_tx), MsgObj);
handle_msg(S, ?MSG_SEND_BLOCK, MsgObj) ->
    handle_send_block(S, MsgObj);
handle_msg(S, ?MSG_SEND_BLOCK_RSP, MsgObj) ->
    handle_send_block_rsp(S, get_request(S, send_block), MsgObj);
handle_msg(S, MsgType, MsgObj) ->
    lager:info("Peer got unexpected message of type ~p (~p).", [MsgType, MsgObj]),
    S.

handle_ping_rsp(S, none, RemotePingObj) ->
    lager:info("Peer ~p got unexpected PING_RSP - ~p", [peer_id(S), RemotePingObj]),
    S;
handle_ping_rsp(S, {ping, From}, RemotePingObj) ->
    case handle_ping_msg(S, RemotePingObj) of
        {ok, _}          -> gen_server:reply(From, ok);
        Err = {error, _} -> gen_server:reply(From, Err)
    end,
    remove_request_fld(S, ping).

handle_ping(S, none, RemotePingObj) ->
    {PeerOk, S1} =
        case is_non_registered_accept(S) of
             true ->
                %% This is the first ping, now we have everything - accept the peer
                #{<<"port">> := Port} = RemotePingObj,
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
         end,

    PingObj =
        case PeerOk of
            ok ->
                case handle_ping_msg(S1, RemotePingObj) of
                    {ok, TheirPeers} ->
                        PeerId = peer_id(S1),
                        #{ <<"share">> := Share } = RemotePingObj,
                        LocalPingObj = local_ping_obj(S1),
                        ping_obj(LocalPingObj#{ <<"pong">> => <<"pong">>,
                                                <<"share">> => Share },
                                [PeerId | [aec_peers:peer_id(P) || P <- TheirPeers]]);
                    {error, Reason} ->
                        #{ <<"pong">> => <<"pang">>, <<"reason">> => Reason }
                end;
            {error, Reason} ->
                 #{ <<"pong">> => <<"pang">>, <<"reason">> => Reason }
        end,
    send_msg(S, ?MSG_PING_RSP, PingObj),
    S1;
handle_ping(S, {ping, _From}, RemotePingObj) ->
    lager:info("Peer ~p got new ping when waiting for PING_RSP - ~p",
               [peer_id(S), RemotePingObj]),
    PingObj = #{ <<"pong">> => <<"pang">>, <<"reason">> => <<"already pinging">> },
    send_msg(S, ?MSG_PING_RSP, PingObj),
    S.

handle_ping_msg(S, RemotePingObj) ->
    #{ <<"genesis_hash">> := LGHash,
       <<"best_hash">>    := LTHash,
       <<"difficulty">>   := LDiff } = local_ping_obj(S),
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
            {ok, RPeers};
        _Err ->
            {error, <<"wrong genesis_hash">>}
    end.

decode_remote_ping(#{ <<"genesis_hash">> := EncGHash,
                      <<"best_hash">>    := EncTHash,
                      <<"peers">>        := Peers,
                      <<"difficulty">>   := Difficulty } = PingObj) ->
    case maps:get(<<"pong">>, PingObj, <<"pong">>) of
        <<"pong">> ->
            case {aec_base58c:safe_decode(block_hash, EncGHash),
                  aec_base58c:safe_decode(block_hash, EncTHash),
                  decode_remote_peers(Peers)} of
                {{ok, GHash}, {ok, THash}, {ok, Peers1}} ->
                    {ok, GHash, THash, Difficulty, Peers1};
                _ ->
                    {error, different_genesis_block}
            end;
        <<"pang">> ->
            {error, ping_failed}
    end;
decode_remote_ping(_) ->
    {error, different_genesis_block}.

decode_remote_peers(Ps) ->
    {ok, [ #{ host => H, port => P, pubkey => PK }
           || #{<<"host">> := H, <<"port">> := P, <<"pubkey">> := PK } <- Ps ]}.

send_ping(S = #{ status := error }, _From, _PingObj) ->
    {reply, {error, disconnected}, S};
send_ping(S = #{ status := {connected, _ESock} }, From, PingObj) ->
    send_msg(S, ?MSG_PING, ping_obj(PingObj, [peer_id(S)])),
    {noreply, set_request(S, ping, From)}.

%% Encode hashes and get peers for PingObj
ping_obj(PingObj, Exclude) ->
    #{<<"share">>        := Share,
      <<"genesis_hash">> := GHash,
      <<"best_hash">>    := TopHash } = PingObj,
    Peers = aec_peers:get_random(Share, Exclude),
    PingObj#{<<"peers">> => Peers,
             <<"genesis_hash">> => aec_base58c:encode(block_hash, GHash),
             <<"best_hash">>  => aec_base58c:encode(block_hash, TopHash) }.

local_ping_obj(S) ->
    PingObj = aec_sync:local_ping_object(),
    PingObj#{ <<"host">> => maps:get(local_host, S),
              <<"port">> => maps:get(local_port, S) }.

%% -- Get Header by Hash -----------------------------------------------------
handle_get_header_by_hash(S, MsgObj) ->
    Msg = get_header(maps:get(<<"hash">>, MsgObj, undefined)),
    send_msg(S, ?MSG_GET_HEADER_BY_HASH_RSP, Msg),
    S.

handle_get_header_by_hash_rsp(S, none, MsgObj) ->
    lager:info("Peer ~p got unexpected GET_HEADER_BY_HASH_RSP - ~p", [peer_id(S), MsgObj]),
    S;
handle_get_header_by_hash_rsp(S, {get_header_by_hash, From}, MsgObj) ->
    case maps:get(<<"result">>, MsgObj, <<"error">>) of
        <<"ok">> ->
            case aehttp_api_parser:decode(header, maps:get(<<"header">>, MsgObj)) of
                {ok, Header} ->
                    gen_server:reply(From, {ok, Header});
                Err = {error, _} ->
                    gen_server:reply(From, Err)
            end;
        <<"error">> ->
            gen_server:reply(From, {error, maps:get(<<"reason">>, MsgObj)})
    end,
    remove_request_fld(S, get_header_by_hash).

send_get_header_by_hash(S = #{ status := error }, _From, _Hash) ->
    {reply, {error, disconnected}, S};
send_get_header_by_hash(S = #{ status := {connected, _ESock} }, From, Hash) ->
    Msg = #{ <<"hash">> => aec_base58c:encode(block_hash, Hash) },
    send_msg(S, ?MSG_GET_HEADER_BY_HASH, Msg),
    {noreply, set_request(S, get_header_by_hash, From)}.

get_header(EncHash) when is_binary(EncHash) ->
    case aec_base58c:safe_decode(block_hash, EncHash) of
        {ok, Hash} ->
            get_header(hash, Hash);
        {error, Reason} ->
            #{ <<"result">> => error, <<"reason">> => Reason }
    end;
get_header(N) when N >= 0 ->
    get_header(height, N);
get_header(_) ->
    #{ <<"result">> => error, <<"reason">> => <<"bad request">> }.

get_header(hash, Hash) ->
    get_header(fun aec_chain:get_header/1, Hash);
get_header(height, N) ->
    get_header(fun aec_chain:get_header_by_height/1, N);
get_header(Fun, Arg) ->
    case Fun(Arg) of
        {ok, Header} ->
            HH = aehttp_api_parser:encode(header, Header),
            #{result => ok, header => HH};
        error ->
            #{result => error, reason => <<"Block not found">>}
    end.


%% -- Get Header by Height -----------------------------------------------------

send_get_header_by_height(S = #{ status := error }, _From, _Hash) ->
    {reply, {error, disconnected}, S};
send_get_header_by_height(S = #{ status := {connected, _ESock} }, From, Height) ->
    Msg = #{ <<"height">> => Height },
    send_msg(S, ?MSG_GET_HEADER_BY_HEIGHT, Msg),
    {noreply, set_request(S, get_header_by_height, From)}.

handle_get_header_by_height(S, MsgObj) ->
    Msg = get_header(maps:get(<<"height">>, MsgObj, undefined)),
    send_msg(S, ?MSG_GET_HEADER_BY_HEIGHT_RSP, Msg),
    S.

handle_get_header_by_height_rsp(S, none, MsgObj) ->
    lager:info("Peer ~p got unexpected GET_HEADER_BY_HEIGHT_RSP - ~p", [peer_id(S), MsgObj]),
    S;
handle_get_header_by_height_rsp(S, {get_header_by_height, From}, MsgObj) ->
    case maps:get(<<"result">>, MsgObj, <<"error">>) of
        <<"ok">> ->
            case aehttp_api_parser:decode(header, maps:get(<<"header">>, MsgObj)) of
                {ok, Header} ->
                    gen_server:reply(From, {ok, Header});
                Err = {error, _} ->
                    gen_server:reply(From, Err)
            end;
        <<"error">> ->
            gen_server:reply(From, {error, maps:get(<<"reason">>, MsgObj)})
    end,
    remove_request_fld(S, get_header_by_height).

%% -- Get N Successors -------------------------------------------------------

send_get_n_successors(S = #{ status := error }, _From, _Hash, _N) ->
    {reply, {error, disconnected}, S};
send_get_n_successors(S = #{ status := {connected, _ESock} }, From, Hash, N) ->
    Msg = #{ <<"hash">> => aec_base58c:encode(block_hash, Hash), <<"n">> => N },
    send_msg(S, ?MSG_GET_N_SUCCESSORS, Msg),
    {noreply, set_request(S, get_n_successors, From)}.

handle_get_n_successors(S, MsgObj) ->
    Msg =
        case {maps:get(<<"hash">>, MsgObj, undefined),
              maps:get(<<"n">>, MsgObj, undefined)} of
            {EncHash, N} when is_binary(EncHash), is_integer(N), N > 0 ->
                case aec_base58c:safe_decode(block_hash, EncHash) of
                    {ok, Hash} ->
                        case aec_chain:get_at_most_n_headers_forward_from_hash(Hash, N+1) of
                            {ok, [_ | Headers]} ->
                                HHashes = [ begin
                                                {ok, HHash} = aec_headers:hash_header(H),
                                                #{ height => aec_headers:height(H), hash => HHash}
                                            end || H <- Headers ],
                                #{result => ok, headers => HHashes};
                            error ->
                                #{result => error, reason => <<"Block not found">>}
                        end;
                    {error, Reason} ->
                        #{ <<"result">> => error, <<"reason">> => Reason }
                end;
            _ ->
                #{ <<"result">> => error, <<"reason">> => <<"bad request">> }
        end,
    send_msg(S, ?MSG_GET_N_SUCCESSORS_RSP, Msg),
    S.

handle_get_n_successors_rsp(S, none, MsgObj) ->
    lager:info("Peer ~p got unexpected GET_N_SUCCESSORS_RSP - ~p", [peer_id(S), MsgObj]),
    S;
handle_get_n_successors_rsp(S, {get_n_successors, From}, MsgObj) ->
    case maps:get(<<"result">>, MsgObj, <<"error">>) of
        <<"ok">> ->
            case maps:get(<<"headers">>, MsgObj, undefined) of
                Hdrs when is_list(Hdrs) ->
                    Hdrs1 = [ {Height, Hash} || #{ <<"height">> := Height, <<"hash">> := Hash } <- Hdrs ],
                    gen_server:reply(From, {ok, Hdrs1});
                _ ->
                    gen_server:reply(From, {error, no_headers_in_response})
            end;
        <<"error">> ->
            gen_server:reply(From, {error, maps:get(<<"reason">>, MsgObj)})
    end,
    remove_request_fld(S, get_n_successors).

%% -- Get Block --------------------------------------------------------------

send_get_block(S = #{ status := error }, _From, _Hash) ->
    {reply, {error, disconnected}, S};
send_get_block(S = #{ status := {connected, _ESock} }, From, Hash) ->
    Msg = #{ <<"hash">> => aec_base58c:encode(block_hash, Hash) },
    send_msg(S, ?MSG_GET_BLOCK, Msg),
    {noreply, set_request(S, get_block, From)}.

handle_get_block(S, MsgObj) ->
    Msg =
        case maps:get(<<"hash">>, MsgObj, undefined) of
            EncHash when is_binary(EncHash) ->
                case aec_base58c:safe_decode(block_hash, EncHash) of
                    {ok, Hash} ->
                        case aec_chain:get_block(Hash) of
                            {ok, Block} ->
                                BlockMap = aehttp_api_parser:encode(block, Block),
                                #{result => ok, block => BlockMap};
                            error ->
                                #{result => error, reason => <<"Block not found">>}
                        end;
                    {error, Reason} ->
                        #{ <<"result">> => error, <<"reason">> => Reason }
                end;
            _ ->
                #{ <<"result">> => error, <<"reason">> => <<"bad request">> }
        end,
    send_msg(S, ?MSG_GET_BLOCK_RSP, Msg),
    S.

handle_get_block_rsp(S, none, MsgObj) ->
    lager:info("Peer ~p got unexpected GET_BLOCK_RSP - ~p", [peer_id(S), MsgObj]),
    S;
handle_get_block_rsp(S, {get_block, From}, MsgObj) ->
    case maps:get(<<"result">>, MsgObj, <<"error">>) of
        <<"ok">> ->
            case maps:get(<<"block">>, MsgObj, undefined) of
                Block0 when is_map(Block0) ->
                    {ok, Block} = aehttp_api_parser:decode(block, Block0),
                    gen_server:reply(From, {ok, Block});
                _ ->
                    gen_server:reply(From, {error, no_block_in_response})
            end;
        <<"error">> ->
            gen_server:reply(From, {error, maps:get(<<"reason">>, MsgObj)})
    end,
    remove_request_fld(S, get_block).

%% -- Get Mempool --------------------------------------------------------------

send_get_mempool(S = #{ status := error }, _From) ->
    {reply, {error, disconnected}, S};
send_get_mempool(S = #{ status := {connected, _ESock} }, From) ->
    send_msg(S, ?MSG_GET_MEMPOOL, []),
    {noreply, set_request(S, get_mempool, From)}.

handle_get_mempool(S, _MsgObj) ->
    {ok, Txs0} = aec_tx_pool:peek(infinity),
    Txs = [ aetx_sign:serialize_to_binary(T) || T <- Txs0 ],
    Msg = #{result => ok, txs => Txs},
    send_msg(S, ?MSG_GET_MEMPOOL_RSP, Msg),
    S.

handle_get_mempool_rsp(S, none, MsgObj) ->
    lager:info("Peer ~p got unexpected GET_MEMPOOL_RSP - ~p", [peer_id(S), MsgObj]),
    S;
handle_get_mempool_rsp(S, {get_mempool, From}, MsgObj) ->
    case maps:get(<<"result">>, MsgObj, <<"error">>) of
        <<"ok">> ->
            case maps:get(<<"txs">>, MsgObj, undefined) of
                Txs0 when is_list(Txs0) ->
                    Txs = [ aetx_sign:deserialize_from_binary(T) || T <- Txs0 ],
                    gen_server:reply(From, {ok, Txs});
                _ ->
                    gen_server:reply(From, {error, no_transactions_in_response})
            end;
        <<"error">> ->
            gen_server:reply(From, {error, maps:get(<<"reason">>, MsgObj)})
    end,
    remove_request_fld(S, get_mempool).

%% -- Send Block --------------------------------------------------------------

send_send_block(S = #{ status := error }, _From, _Block) ->
    {reply, {error, disconnected}, S};
send_send_block(S = #{ status := {connected, _ESock} }, From, Block) ->
    BlockMap = aehttp_api_parser:encode(block, Block),
    send_msg(S, ?MSG_SEND_BLOCK, BlockMap),
    {noreply, set_request(S, send_block, From)}.

handle_send_block(S, MsgObj) ->
    Msg =
        case aehttp_api_parser:decode(block, MsgObj) of
            {ok, Block} ->
                Header = aec_blocks:to_header(Block),
                {ok, HH} = aec_headers:hash_header(Header),
                lager:debug("'PostBlock'; header hash: ~s", [pp(HH)]),
                aec_conductor:post_block(Block),
                #{ <<"result">> => ok };
            _ ->
                #{ <<"result">> => error, <<"reason">> => <<"bad serialization">> }
        end,
    send_msg(S, ?MSG_SEND_BLOCK_RSP, Msg),
    S.

handle_send_block_rsp(S, none, MsgObj) ->
    lager:info("Peer ~p got unexpected SEND_BLOCK_RSP - ~p", [peer_id(S), MsgObj]),
    S;
handle_send_block_rsp(S, {send_block, From}, MsgObj) ->
    case maps:get(<<"result">>, MsgObj, <<"error">>) of
        <<"ok">> ->
            gen_server:reply(From, ok);
        <<"error">> ->
            gen_server:reply(From, {error, maps:get(<<"reason">>, MsgObj)})
    end,
    remove_request_fld(S, send_block).

%% -- Send TX --------------------------------------------------------------

send_send_tx(S = #{ status := error }, _From, _Tx) ->
    {reply, {error, disconnected}, S};
send_send_tx(S = #{ status := {connected, _ESock} }, From, Tx) ->
    TxSerialized = aetx_sign:serialize_to_binary(Tx),
    send_msg(S, ?MSG_SEND_TX, #{tx => TxSerialized}),
    {noreply, set_request(S, send_tx, From)}.

handle_send_tx(S, MsgObj) ->
    Msg =
        %% Top level function has already msgpack:unpack:ed
        try
            #{ <<"tx">> := EncSignedTx } = MsgObj,
            SignedTx = aetx_sign:deserialize_from_binary(EncSignedTx),
            aec_tx_pool:push(SignedTx, tx_received),
            #{ <<"result">> => ok }
        catch _:_ ->
            #{ <<"result">> => error, <<"reason">> => <<"bad serialization">> }
        end,
    send_msg(S, ?MSG_SEND_TX_RSP, Msg),
    S.

handle_send_tx_rsp(S, none, MsgObj) ->
    lager:info("Peer ~p got unexpected SEND_TX_RSP - ~p", [peer_id(S), MsgObj]),
    S;
handle_send_tx_rsp(S, {send_tx, From}, MsgObj) ->
    case maps:get(<<"result">>, MsgObj, <<"error">>) of
        <<"ok">> ->
            gen_server:reply(From, ok);
        <<"error">> ->
            gen_server:reply(From, {error, maps:get(<<"reason">>, MsgObj)})
    end,
    remove_request_fld(S, send_tx).

%% -- Send message -----------------------------------------------------------
send_msg(#{ status := {connected, ESock} }, Type, Msg0) ->
    Msg = <<Type:16, (msgpack:pack(Msg0))/binary>>,
    do_send(ESock, Msg).

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

peer_id(#{ r_pubkey := PK, host := H, port := P }) when is_binary(H) ->
    <<PK/binary, P:16, H/binary>>;
peer_id(#{ r_pubkey := PK, host := H, port := P }) ->
    <<PK/binary, P:16, (list_to_binary(H))/binary>>.

