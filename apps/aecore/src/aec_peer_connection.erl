%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module encapsulating a P2P communication channel
%%% @end
%%%=============================================================================
-module(aec_peer_connection).

-export([ accept/2
        , connect/1
        , get_header_by_hash/2

        , ping/1
        %% , disconnect/1
        %% , start_link/1
        , status/1
        , stop/1
        ]).

-behaviour(gen_server).

-import(aeu_debug, [pp/1]).

-define(MSG_PING, 1).
-define(MSG_PING_RSP, 2).
-define(MSG_GET_HEADER_BY_HASH, 3).
-define(MSG_GET_HEADER_BY_HASH_RSP, 4).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% -- API --------------------------------------------------------------------

connect(Options) ->
    gen_server:start_link(?MODULE, [Options#{ role => initiator }], []).

accept(TcpSock, Options) ->
    {ok, {Host, Port}} = inet:peername(TcpSock),
    lager:debug("ZZPC: got incoming ~p from ~p", [TcpSock, {Host, Port}]),
    case gen_server:start_link(?MODULE, [Options#{ tcp_sock => TcpSock, role => responder,
                                                   host => inet_parse:ntoa(Host)}], []) of
        {ok, Pid} ->
            gen_tcp:controlling_process(TcpSock, Pid),
            {ok, Pid};
        Res ->
            Res
    end.

ping(PeerCon) ->
    gen_server:call(PeerCon, ping).

get_header_by_hash(PeerCon, Hash) ->
    gen_server:call(PeerCon, {get_header_by_hash, Hash}).

status(PeerCon) ->
    gen_server:call(PeerCon, status).

stop(PeerCon) ->
    gen_server:call(PeerCon, stop).

%% -- gen_server callbacks ---------------------------------------------------
init([Opts]) ->
    Version = <<"1.2.3">>,
    Genesis = <<1,2,3,4>>,
    {ok, Opts#{ version => Version, genesis => Genesis }, 50}.

handle_call(ping, From, State) ->
    send_ping(State, From, local_ping_obj(State));
handle_call({get_header_by_hash, Hash}, From, State) ->
    send_get_header_by_hash(State, From, Hash);
handle_call(status, _From, State) ->
    {reply, maps:get(status, State, error), State};
handle_call(stop, _From, State) ->
    case maps:get(status, State, error) of
        {connected, ESock} -> enoise:close(ESock);
        _                  -> ok
    end,
    case maps:get(gen_tcp, State, undefined) of
        TSock when is_port(TSock) -> gen_tcp:close(TSock);
        _                         -> ok
    end,
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, S = #{ role := initiator, host := Host, port := Port }) ->
    Self = self(),
    %% TODO: monitor this one?!
    lager:debug("ZZPC: connecting to ~p", [{Host, Port}]),
    Pid = spawn(fun() -> try_connect(Self, Host, Port, 1000) end),
    {noreply, S#{ status => {connecting, Pid} }};

handle_info({connected, Pid, Err = {error, _}}, S = #{ status := {connecting, Pid}}) ->
    lager:debug("ZZPC: failed to connected to ~p: ~p", [{maps:get(host, S), maps:get(port, S)}, Err]),
    {noreply, S#{ status := error }};

handle_info({connected, Pid, {ok, TcpSock}}, S = #{ status := {connecting, Pid} }) ->
    #{ version := Version, genesis := Genesis,
       seckey := SecKey, pubkey := PubKey, r_pubkey := RemotePub } = S,
    lager:debug("ZZPC: connected to ~p", [{maps:get(host, S), maps:get(port, S)}]),
    NoiseOpts = [ {noise, <<"Noise_XK_25519_ChaChaPoly_BLAKE2b">>}
                , {s, enoise_keypair:new(dh25519, SecKey, PubKey)}
                , {prologue, <<Version/binary, Genesis/binary>>}
                , {rs, enoise_keypair:new(dh25519, RemotePub)} ],
    case enoise:connect(TcpSock, NoiseOpts) of
        {ok, ESock, _} ->
            lager:debug("ZZPC: eniose:connect() successful", []),
            {noreply, S#{ status => {connected, ESock} }};
        {error, _Reason} ->
            %% What to do here? Close the socket and stop?
            lager:debug("ZZPC: eniose:connect() failed", []),
            gen_tcp:close(TcpSock),
            {stop, normal, S}
    end;

handle_info(timeout, S = #{ role := responder, tcp_sock := TcpSock }) ->
    #{ version := Version, genesis := Genesis,
       seckey := SecKey, pubkey := PubKey } = S,
    NoiseOpts = [ {noise, <<"Noise_XK_25519_ChaChaPoly_BLAKE2b">>}
                , {s, enoise_keypair:new(dh25519, SecKey, PubKey)}
                , {prologue, <<Version/binary, Genesis/binary>>} ],
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
            {noreply, S#{ status => {connected, ESock}, r_pubkey => RemotePub }};
        {error, _Reason} ->
            %% What to do here? Close the socket and stop?
            lager:debug("ZZPC: eniose:accept() failed", []),
            gen_tcp:close(TcpSock),
            {stop, normal, S}
    end;
handle_info({noise, _, <<Type:16, Payload/binary>>}, S) ->
    {ok, Msg} = msgpack:unpack(Payload),
    S1 = handle_msg(S, Type, Msg),
    {noreply, S1};
handle_info(Msg, S) ->
    lager:debug("ZZPC: unexpected message ~p", [Msg]),
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- Local functions --------------------------------------------------------
try_connect(Parent, Host, Port, Timeout) ->
    Res = gen_tcp:connect(Host, Port, [binary, {reuseaddr, true}, {active, true}], Timeout),
    case Res of
        {ok, Sock} -> gen_tcp:controlling_process(Sock, Parent);
        {error, _} -> ok
    end,
    Parent ! {connected, self(), Res}.

handle_msg(S, ?MSG_PING, PingObj) ->
    handle_ping(S, maps:get(request, S, none), PingObj);
handle_msg(S, ?MSG_PING_RSP, PingObj) ->
    handle_ping_rsp(S, maps:get(request, S, none), PingObj);
handle_msg(S, ?MSG_GET_HEADER_BY_HASH, MsgObj) ->
    handle_get_header_by_hash(S, MsgObj);
handle_msg(S, ?MSG_GET_HEADER_BY_HASH_RSP, MsgObj) ->
    handle_get_header_by_hash_rsp(S, maps:get(request, S, none), MsgObj);
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
    S#{ request => undefined }.

handle_ping(S, none, RemotePingObj) ->
    S1 = case maps:get(port, S, undefined) of
             undefined ->
                %% This is the first ping, now we have everything - add the peer
                #{<<"port">> := Port} = RemotePingObj,
                aec_peers:add(#{ host => maps:get(host, S), port => Port,
                                 pubkey => maps:get(r_pubkey, S), connection => self(),
                                 ping => false, trusted => false }),
                S#{ port => Port };
             _ ->
                S
         end,


    PingObj =
        case handle_ping_msg(S1, RemotePingObj) of
            {ok, TheirPeers} ->
                PeerId = peer_id(S1),
                aec_peers:update_last_seen(PeerId),
                #{ <<"share">> := Share } = RemotePingObj,
                LocalPingObj = local_ping_obj(S1),
                ping_obj(LocalPingObj#{ <<"pong">> => <<"pong">>,
                                        <<"share">> => Share },
                         [PeerId | TheirPeers]);
            {error, Reason} ->
                #{ <<"pong">> => <<"pang">>, <<"reason">> => Reason }
        end,
    send_msg(S, ?MSG_PING_RSP, PingObj),
    S1;
handle_ping(S, {ping, _From}, RemotePingObj) ->
    lager:info("Peer ~p got new ping when waiting for PING_RSP - ~p",
               [peer_id(S), RemotePingObj]),
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
            aec_sync:fetch_mempool(PeerId),
            {ok, RPeers};
        _Err ->
            lager:debug("ZZPC: CMP Fail: ~p", [_Err]),
            aec_peers:block_peer(PeerId),
            {error, <<"wrong genesis_hash">>}
    end.

decode_remote_ping(#{ <<"genesis_hash">> := EncGHash,
                      <<"best_hash">>    := EncTHash,
                      <<"peers">>        := Peers,
                      <<"difficulty">>   := Difficulty } = PingObj) ->
    case maps:get(<<"pong">>, PingObj, <<"pong">>) of
        <<"pong">> ->
            case {aec_base58c:safe_decode(block_hash, EncGHash),
                  aec_base58c:safe_decode(block_hash, EncTHash)} of
                {{ok, GHash}, {ok, THash}} ->
                    {ok, GHash, THash, Difficulty, Peers};
                _ ->
                    {error, different_genesis_block}
            end;
        <<"pang">> ->
            {error, ping_failed}
    end;
decode_remote_ping(_) ->
    {error, different_genesis_block}.


send_ping(S = #{ status := error }, _From, _PingObj) ->
    {reply, {error, disconnected}, S};
send_ping(S = #{ status := {connected, _ESock} }, From, PingObj) ->
    lager:debug("ZZPC: ping ~p", [PingObj]),
    send_msg(S, ?MSG_PING, ping_obj(PingObj, [peer_id(S)])),
    {noreply, S#{ request => {ping, From} }}.

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
            gen_server:reply(From, {ok, maps:get(<<"header">>, MsgObj)});
        <<"error">> ->
            gen_server:reply(From, {error, maps:get(<<"reason">>, MsgObj)})
    end,
    S#{ request => undefined }.

send_get_header_by_hash(S = #{ status := error }, _From, _Hash) ->
    {reply, {error, disconnected}, S};
send_get_header_by_hash(S = #{ status := {connected, _ESock} }, From, Hash) ->
    lager:debug("ZZPC: get_header_by_hash ~p", [Hash]),
    Msg = #{ <<"hash">> => aec_base58c:encode(block_hash, Hash) },
    send_msg(S, ?MSG_GET_HEADER_BY_HASH, Msg),
    {noreply, S#{ request => {get_header_by_hash, From} }}.

send_msg(#{ status := {connected, ESock} }, Type, Msg0) ->
    Msg = <<Type:16, (msgpack:pack(Msg0))/binary>>,
    enoise:send(ESock, Msg).

peer_id(#{ r_pubkey := PK, host := H, port := P }) when is_binary(H) ->
    <<PK/binary, P:16, H/binary>>;
peer_id(#{ r_pubkey := PK, host := H, port := P }) ->
    <<PK/binary, P:16, (list_to_binary(H))/binary>>.

