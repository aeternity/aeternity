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
    lager:debug("ZZPC: connect to ~p", [Options]),
    gen_server:start_link(?MODULE, [Options#{ role => initiator }], []).

accept(TcpSock, Options) ->
    {ok, {Host, Port}} = inet:peername(TcpSock),
    lager:debug("ZZPC: got incoming ~p from ~p", [TcpSock, {Host, Port}]),
    case gen_server:start_link(?MODULE, [Options#{ tcp_sock => TcpSock, role => responder,
                                                   host => inet_parse:ntoa(Host)}], []) of
        {ok, Pid} ->
            gen_tcp:controlling_process(TcpSock, Pid),
            gen_server:cast(Pid, owns_tcp_socket),
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
    Version = <<"1.2.3">>,
    Genesis = <<1,2,3,4>>,
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
    lager:debug("ZZPC: connecting to ~p", [{Host, Port}]),
    Pid = spawn(fun() -> try_connect(Self, Host, Port, 1000) end),
    {noreply, S#{ status => {connecting, Pid} }};
handle_cast(owns_tcp_socket, S = #{ role := responder, tcp_sock := TcpSock }) ->
    #{ version := Version, genesis := Genesis,
       seckey := SecKey, pubkey := PubKey } = S,
    NoiseOpts = [ {noise, <<"Noise_XK_25519_ChaChaPoly_BLAKE2b">>}
                , {s, enoise_keypair:new(dh25519, SecKey, PubKey)}
                , {prologue, <<Version/binary, Genesis/binary>>}
                , {timeout, ?NOISE_HS_TIMEOUT} ],
    lager:debug("ZZPC: initiating responder", []),
    %% Keep the socket passive until here to avoid premature message
    %% receiving...
    inet:setopts(TcpSock, [{active, true}]),
    case enoise:accept(TcpSock, NoiseOpts) of
        {ok, ESock, FinalState} ->
            RemotePub = enoise_keypair:pubkey(enoise_hs_state:remote_keys(FinalState)),
            lager:debug("ZZPC: eniose:accept() successful remotepub ~p", [RemotePub]),
            %% Report this to aec_peers!? And possibly fail?
            %% Or, we can't do this yet we don't know the port?!
            TRef = erlang:start_timer(?FIRST_PING_TIMEOUT, self(), first_ping_timeout),
            {noreply, S#{ status => {connected, ESock}, r_pubkey => RemotePub,
                          first_ping_tref => TRef }};
        {error, _Reason} ->
            %% What to do here? Close the socket and stop?
            lager:debug("ZZPC: eniose:accept() failed - ~p", [_Reason]),
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
    lager:debug("ZZPC: connecting to ~p", [{Host, Port}]),
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
    lager:debug("ZZPC: failed to connected to ~p: ~p", [{maps:get(host, S), maps:get(port, S)}, Err]),
    case aec_peers:connect_fail(peer_id(S), self()) of
        keep ->
            {noreply, S#{ status := error }};
        close ->
            {stop, normal, S}
    end;
handle_info({connected, Pid, {ok, TcpSock}}, S = #{ status := {connecting, Pid} }) ->
    #{ version := Version, genesis := Genesis,
       seckey := SecKey, pubkey := PubKey, r_pubkey := RemotePub } = S,
    lager:debug("ZZPC: connected to ~p", [{maps:get(host, S), maps:get(port, S)}]),
    NoiseOpts = [ {noise, <<"Noise_XK_25519_ChaChaPoly_BLAKE2b">>}
                , {s, enoise_keypair:new(dh25519, SecKey, PubKey)}
                , {prologue, <<Version/binary, Genesis/binary>>}
                , {rs, enoise_keypair:new(dh25519, RemotePub)}
                , {timeout, ?NOISE_HS_TIMEOUT} ],
    case enoise:connect(TcpSock, NoiseOpts) of
        {ok, ESock, _} ->
            lager:debug("ZZPC: eniose:connect() successful", []),
            case aec_peers:connect_peer(peer_id(S), self()) of
                ok ->
                    lager:debug("ZZPC: eniose:connect() successful - use it", []),
                    {noreply, S#{ status => {connected, ESock} }};
                {error, _} ->
                    lager:debug("ZZPC: drop unnecessary connection", []),
                    enoise:close(ESock),
                    {stop, normal, S}
            end;
        {error, _Reason} ->
            %% For now lets report connect_fail
            %% Maybe instead report permanent fail + block?!
            lager:debug("ZZPC: eniose:connect() failed", []),
            gen_tcp:close(TcpSock),
            case aec_peers:connect_fail(peer_id(S), self()) of
                keep  -> {noreply, S#{ status := error }};
                close -> {stop, normal, S}
            end
    end;

handle_info({noise, _, <<Type:16, Payload/binary>>}, S) ->
    {ok, Msg} = msgpack:unpack(Payload),
    S1 = handle_msg(S, Type, Msg),
    {noreply, S1};
handle_info({tcp_closed, _}, S) ->
    lager:debug("ZZPC: got tcp_closed", []),
    case is_non_registered_accept(S) of
        true ->
            {stop, normal, S};
        false ->
            case aec_peers:connect_fail(peer_id(S), self()) of
                keep ->
                    {noreply, S#{ status := error }};
                close ->
                    {stop, normal, S}
            end
    end;
handle_info(Msg, S) ->
    lager:debug("ZZPC: unexpected message ~p", [Msg]),
    {noreply, S}.

terminate(_Reason, _State) ->
    lager:debug("ZZPC: PC terminating ~p", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- Local functions --------------------------------------------------------
get_request(S, Kind) ->
    maps:get(Kind, maps:get(requests, S, #{}), none).

set_request(S, Kind, From) ->
    Rs = maps:get(requests, S, #{}),
    S#{ requests => Rs#{ Kind => {Kind, From} } }.

drop_request(S, Kind) ->
    Rs = maps:get(requests, S, #{}),
    S#{ requests => maps:remove(Kind, Rs) }.

try_connect(Parent, Host, Port, Timeout) ->
    Res = gen_tcp:connect(Host, Port, [binary, {reuseaddr, true}, {active, true}], Timeout),
    case Res of
        {ok, Sock} -> gen_tcp:controlling_process(Sock, Parent);
        {error, _} -> ok
    end,
    Parent ! {connected, self(), Res}.

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
    lager:info("ZZPC: Peer got unexpected message of type ~p (~p).", [MsgType, MsgObj]),
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
    drop_request(S, ping).

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
                                [PeerId | TheirPeers]);
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
    lager:debug("ZZPC: GOT PING: ~p", [RemotePingObj]),
    #{ <<"genesis_hash">> := LGHash,
       <<"best_hash">>    := LTHash,
       <<"difficulty">>   := LDiff } = local_ping_obj(S),
    PeerId = peer_id(S),
    lager:debug("ZZPC: CMP PING: ~p", [local_ping_obj(S)]),
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
            aec_peers:add_and_ping_peers(RPeers),
            aec_sync:fetch_mempool(PeerId),
            {ok, RPeers};
        _Err ->
            lager:debug("ZZPC: CMP Fail: ~p", [_Err]),
            %% aec_peers:block_peer(PeerId),
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
    lager:debug("ZZPC: ping ~p", [PingObj]),
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
    lager:debug("ZZPC: got header_by_hash ~p", [MsgObj]),
    Msg =
        case maps:get(<<"hash">>, MsgObj, undefined) of
            EncHash when is_binary(EncHash) ->
                case aec_base58c:safe_decode(block_hash, EncHash) of
                    {ok, Hash} ->
                        case aec_chain:get_header(Hash) of
                            {ok, Header} ->
                                lager:debug("Header = ~p", [pp(Header)]),
                                {ok, HH} = aec_headers:serialize_to_map(Header),
                                #{result => ok, header => HH};
                            error ->
                                #{result => error, reason => <<"Block not found">>}
                        end;
                    {error, Reason} ->
                        #{ <<"result">> => error, <<"reason">> => Reason }
                end;
            _ ->
                #{ <<"result">> => error, <<"reason">> => <<"bad request">> }
        end,
    send_msg(S, ?MSG_GET_HEADER_BY_HASH_RSP, Msg),
    S.

handle_get_header_by_hash_rsp(S, none, MsgObj) ->
    lager:info("Peer ~p got unexpected GET_HEADER_BY_HASH_RSP - ~p", [peer_id(S), MsgObj]),
    S;
handle_get_header_by_hash_rsp(S, {get_header_by_hash, From}, MsgObj) ->
    lager:debug("ZZPC: got header_by_hash_rsp ~p", [MsgObj]),
    case maps:get(<<"result">>, MsgObj, <<"error">>) of
        <<"ok">> ->
            case aec_headers:deserialize_from_map(maps:get(<<"header">>, MsgObj)) of
                {ok, Header} ->
                    gen_server:reply(From, {ok, Header});
                Err = {error, _} ->
                    gen_server:reply(From, Err)
            end;
        <<"error">> ->
            gen_server:reply(From, {error, maps:get(<<"reason">>, MsgObj)})
    end,
    drop_request(S, get_header_by_hash).

send_get_header_by_hash(S = #{ status := error }, _From, _Hash) ->
    {reply, {error, disconnected}, S};
send_get_header_by_hash(S = #{ status := {connected, _ESock} }, From, Hash) ->
    lager:debug("ZZPC: get_header_by_hash ~p", [Hash]),
    Msg = #{ <<"hash">> => aec_base58c:encode(block_hash, Hash) },
    send_msg(S, ?MSG_GET_HEADER_BY_HASH, Msg),
    {noreply, set_request(S, get_header_by_hash, From)}.

%% -- Get Header by Height -----------------------------------------------------

send_get_header_by_height(S = #{ status := error }, _From, _Hash) ->
    {reply, {error, disconnected}, S};
send_get_header_by_height(S = #{ status := {connected, _ESock} }, From, Height) ->
    lager:debug("ZZPC: get_header_by_height ~p", [Height]),
    Msg = #{ <<"height">> => Height },
    send_msg(S, ?MSG_GET_HEADER_BY_HEIGHT, Msg),
    {noreply, set_request(S, get_header_by_height, From)}.

handle_get_header_by_height(S, MsgObj) ->
    lager:debug("ZZPC: got header_by_height ~p", [MsgObj]),
    Msg =
        case maps:get(<<"height">>, MsgObj, undefined) of
            N when is_integer(N), N > 0 ->
                case aec_chain:get_header_by_height(N) of
                    {ok, Header} ->
                        lager:debug("Header = ~p", [pp(Header)]),
                        {ok, HH} = aec_headers:serialize_to_map(Header),
                        #{result => ok, header => HH};
                    {error, _} ->
                        #{result => error, reason => <<"Block not found">>}
                end;
            _ ->
                #{ <<"result">> => error, <<"reason">> => <<"bad request">> }
        end,
    send_msg(S, ?MSG_GET_HEADER_BY_HEIGHT_RSP, Msg),
    S.

handle_get_header_by_height_rsp(S, none, MsgObj) ->
    lager:info("Peer ~p got unexpected GET_HEADER_BY_HEIGHT_RSP - ~p", [peer_id(S), MsgObj]),
    S;
handle_get_header_by_height_rsp(S, {get_header_by_height, From}, MsgObj) ->
    lager:debug("ZZPC: got header_by_height_rsp ~p", [MsgObj]),
    case maps:get(<<"result">>, MsgObj, <<"error">>) of
        <<"ok">> ->
            case aec_headers:deserialize_from_map(maps:get(<<"header">>, MsgObj)) of
                {ok, Header} ->
                    gen_server:reply(From, {ok, Header});
                Err = {error, _} ->
                    gen_server:reply(From, Err)
            end;
        <<"error">> ->
            gen_server:reply(From, {error, maps:get(<<"reason">>, MsgObj)})
    end,
    drop_request(S, get_header_by_height).

%% -- Get N Successors -------------------------------------------------------

send_get_n_successors(S = #{ status := error }, _From, _Hash, _N) ->
    {reply, {error, disconnected}, S};
send_get_n_successors(S = #{ status := {connected, _ESock} }, From, Hash, N) ->
    lager:debug("ZZPC: get_n_successors ~p", [Hash]),
    Msg = #{ <<"hash">> => aec_base58c:encode(block_hash, Hash), <<"n">> => N },
    send_msg(S, ?MSG_GET_N_SUCCESSORS, Msg),
    {noreply, set_request(S, get_n_successors, From)}.

handle_get_n_successors(S, MsgObj) ->
    lager:debug("ZZPC: got n_successors ~p", [MsgObj]),
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
                                lager:debug("Headers = ~p", [HHashes]),
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
    lager:debug("ZZPC: got n_successors_rsp ~p", [MsgObj]),
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
    drop_request(S, get_n_successors).

%% -- Get Block --------------------------------------------------------------

send_get_block(S = #{ status := error }, _From, _Hash) ->
    {reply, {error, disconnected}, S};
send_get_block(S = #{ status := {connected, _ESock} }, From, Hash) ->
    lager:debug("ZZPC: get_block ~p", [Hash]),
    Msg = #{ <<"hash">> => aec_base58c:encode(block_hash, Hash) },
    send_msg(S, ?MSG_GET_BLOCK, Msg),
    {noreply, set_request(S, get_block, From)}.

handle_get_block(S, MsgObj) ->
    lager:debug("ZZPC: got block ~p", [MsgObj]),
    Msg =
        case maps:get(<<"hash">>, MsgObj, undefined) of
            EncHash when is_binary(EncHash) ->
                case aec_base58c:safe_decode(block_hash, EncHash) of
                    {ok, Hash} ->
                        case aec_chain:get_block(Hash) of
                            {ok, Block} ->
                                lager:debug("Block = ~p", [pp(Block)]),
                                #{result => ok, block => aec_blocks:serialize_to_map(Block)};
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
    lager:debug("ZZPC: got block_rsp ~p", [MsgObj]),
    case maps:get(<<"result">>, MsgObj, <<"error">>) of
        <<"ok">> ->
            case maps:get(<<"block">>, MsgObj, undefined) of
                Block0 when is_map(Block0) ->
                    {ok, Block} = aec_blocks:deserialize_from_map(Block0),
                    gen_server:reply(From, {ok, Block});
                _ ->
                    gen_server:reply(From, {error, no_block_in_response})
            end;
        <<"error">> ->
            gen_server:reply(From, {error, maps:get(<<"reason">>, MsgObj)})
    end,
    drop_request(S, get_block).

%% -- Get Mempool --------------------------------------------------------------

send_get_mempool(S = #{ status := error }, _From) ->
    {reply, {error, disconnected}, S};
send_get_mempool(S = #{ status := {connected, _ESock} }, From) ->
    lager:debug("ZZPC: get_mempool", []),
    send_msg(S, ?MSG_GET_MEMPOOL, []),
    {noreply, set_request(S, get_mempool, From)}.

handle_get_mempool(S, _MsgObj) ->
    lager:debug("ZZPC: got get_mempool ~p", [_MsgObj]),
    {ok, Txs0} = aec_tx_pool:peek(infinity),
    Txs = [ aetx_sign:serialize_to_binary(T) || T <- Txs0 ],
    Msg = #{result => ok, txs => Txs},
    send_msg(S, ?MSG_GET_MEMPOOL_RSP, Msg),
    S.

handle_get_mempool_rsp(S, none, MsgObj) ->
    lager:info("Peer ~p got unexpected GET_MEMPOOL_RSP - ~p", [peer_id(S), MsgObj]),
    S;
handle_get_mempool_rsp(S, {get_mempool, From}, MsgObj) ->
    lager:debug("ZZPC: got mempool_rsp ~p", [MsgObj]),
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
    drop_request(S, get_mempool).

%% -- Send Block --------------------------------------------------------------

send_send_block(S = #{ status := error }, _From, _Block) ->
    {reply, {error, disconnected}, S};
send_send_block(S = #{ status := {connected, _ESock} }, From, Block) ->
    lager:debug("ZZPC: send_block ~p", [Block]),
    BlockMap = aec_blocks:serialize_to_map(Block),
    send_msg(S, ?MSG_SEND_BLOCK, BlockMap),
    {noreply, set_request(S, send_block, From)}.

handle_send_block(S, MsgObj) ->
    lager:debug("ZZPC: got block ~p", [MsgObj]),
    Msg =
        case aec_blocks:deserialize_from_map(MsgObj) of
            {ok, Block} ->
                Header = aec_blocks:to_header(Block),
                {ok, HH} = aec_headers:hash_header(Header),
                lager:debug("'PostBlock'; header hash: ~p", [HH]),
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
    lager:debug("ZZPC: got send_block_rsp ~p", [MsgObj]),
    case maps:get(<<"result">>, MsgObj, <<"error">>) of
        <<"ok">> ->
            gen_server:reply(From, ok);
        <<"error">> ->
            gen_server:reply(From, {error, maps:get(<<"reason">>, MsgObj)})
    end,
    drop_request(S, send_block).

%% -- Send TX --------------------------------------------------------------

send_send_tx(S = #{ status := error }, _From, _Tx) ->
    {reply, {error, disconnected}, S};
send_send_tx(S = #{ status := {connected, _ESock} }, From, Tx) ->
    lager:debug("ZZPC: send_tx ~p", [Tx]),
    TxSerialized = aetx_sign:serialize_to_binary(Tx),
    send_msg(S, ?MSG_SEND_TX, TxSerialized),
    {noreply, set_request(S, send_tx, From)}.

handle_send_tx(S, MsgObj) ->
    lager:debug("ZZPC: got tx ~p", [MsgObj]),
    Msg =
        %% Top level function has already msgpack:unpack:ed
        try
            SignedTx = aetx_sign:deserialize(MsgObj),
            lager:debug("deserialized: ~p", [pp(SignedTx)]),
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
    lager:debug("ZZPC: got send_tx_rsp ~p", [MsgObj]),
    case maps:get(<<"result">>, MsgObj, <<"error">>) of
        <<"ok">> ->
            gen_server:reply(From, ok);
        <<"error">> ->
            gen_server:reply(From, {error, maps:get(<<"reason">>, MsgObj)})
    end,
    drop_request(S, send_tx).


send_msg(#{ status := {connected, ESock} }, Type, Msg) when is_binary(Msg) ->
    enoise:send(ESock, <<Type:16, Msg/binary>>);
send_msg(#{ status := {connected, ESock} }, Type, Msg0) ->
    Msg = <<Type:16, (msgpack:pack(Msg0))/binary>>,
    enoise:send(ESock, Msg).


peer_id(#{ r_pubkey := PK, host := H, port := P }) when is_binary(H) ->
    <<PK/binary, P:16, H/binary>>;
peer_id(#{ r_pubkey := PK, host := H, port := P }) ->
    <<PK/binary, P:16, (list_to_binary(H))/binary>>.

