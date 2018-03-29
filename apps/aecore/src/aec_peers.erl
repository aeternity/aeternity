%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module storing peers list and providing functions for peers interaction.
%%%
%%% The external API takes PeerInfo-map:s or PeerId:s as input. They contain,
%%% host, port and public key either in a map or in a single binary.
%%%
%%% The module keep track of all discovered peers, and their associated
%%% peer_connections. It handles connects, disconnects, pings and newly
%%% discovered peers.
%%%
%%% @end
%%%=============================================================================
-module(aec_peers).

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([accept_peer/2,
         add_and_ping_peers/1,
         add_and_ping_peers/2,
         block_peer/1,
         connect_fail/2,
         connect_peer/2,
         unblock_peer/1,
         is_blocked/1,
         remove/1,
         all/0,
         blocked/0,
         get_connection/1,
         get_random/1,
         get_random/2,
         parse_peer_address/1,
         encode_peer_address/1,
         set_local_peer_info/1,
         get_local_peer_info/0]).

%% API only used in aec_sync
-export([ add/1
        , log_ping/2
        , peer_id/1
        , ppp/1
        ]).

%% API used by aec_peer_connection_listener
-export([sync_port/0]).

-export([check_env/0]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-define(DEFAULT_SYNC_PORT, 3015).

-define(DEFAULT_PING_INTERVAL, 120 * 1000).
-define(BACKOFF_TIMES, [5, 15, 30, 60, 120, 300, 600]).

-define(DEFAULT_UNBLOCK_INTERVAL, 15 * 60 * 1000).


-record(peer, {
          pubkey            :: binary(),
          host              :: string() | binary(),
          port              :: inet:port_number(),
          connection        :: term(),
          retries = 0       :: non_neg_integer() | undefined,
          timer_tref        :: reference() | undefined,
          trusted = false   :: boolean() % Is it a pre-configured peer
         }).

-type peer_id() :: binary(). %% The Peer is identified by its pubkey for now.
-type peer_info() :: #{ host    := string() | binary(),
                        port    := inet:port_number(),
                        pubkey  := pubkey(),
                        seckey  => pubkey(),
                        ping    => boolean(),
                        trusted => boolean() }.
-type peer() :: #peer{}.

-export_type([peer_info/0]).

%%%=============================================================================

%%------------------------------------------------------------------------------
%% Add peer
%%------------------------------------------------------------------------------
-spec add(peer_info()) -> ok.
add(PeerInfo) ->
    gen_server:cast(?MODULE, {add, PeerInfo}).

%%------------------------------------------------------------------------------
%% Add peers and configure them for a ping
%%------------------------------------------------------------------------------
-spec add_and_ping_peers([peer_info()]) -> ok.
add_and_ping_peers(Peers) ->
    [ add(Peer#{ ping => true }) || Peer <- Peers ],
    ok.

%%------------------------------------------------------------------------------
%% Add peers and configure them for a ping. Indicate whether they are trusted
%% (i.e. pre-configured) or not.
%%------------------------------------------------------------------------------
-spec add_and_ping_peers([peer_info()], boolean()) -> ok.
add_and_ping_peers(Peers, Trusted) ->
    add_and_ping_peers([ Peer#{ trusted => Trusted } || Peer <- Peers ]).

%%------------------------------------------------------------------------------
%% Block peer
%%------------------------------------------------------------------------------
-spec block_peer(peer_id()) -> ok | {error, term()}.
block_peer(Peer) ->
    gen_server:call(?MODULE, {block, Peer}).


connect_peer(PeerId, PeerCon) ->
    gen_server:call(?MODULE, {connect_peer, PeerId, PeerCon}).

connect_fail(PeerId, PeerCon) ->
    gen_server:call(?MODULE, {connect_fail, PeerId, PeerCon}).

accept_peer(PeerInfo, PeerCon) ->
    gen_server:call(?MODULE, {accept_peer, PeerInfo, PeerCon}).

%%------------------------------------------------------------------------------
%% Unblock peer
%%------------------------------------------------------------------------------
-spec unblock_peer(peer_id()) -> ok | {error, term()}.
unblock_peer(Peer) ->
    gen_server:call(?MODULE, {unblock, Peer}).

%%------------------------------------------------------------------------------
%% Check if peer is blocked. Erroneous Peers is by definition blocked.
%%------------------------------------------------------------------------------
-spec is_blocked(peer_id()) -> boolean().
is_blocked(Peer) ->
    gen_server:call(?MODULE, {is_blocked, Peer}).

%%------------------------------------------------------------------------------
%% Remove peer.
%% At the moment also removes peer from the blocked list
%%------------------------------------------------------------------------------
-spec remove(peer_id()) -> ok.
remove(Peer) ->
    gen_server:cast(?MODULE, {remove, Peer}).

%%------------------------------------------------------------------------------
%% Get list of all peers. The list may be big. Use with caution.
%% Consider using get_random instead.
%%------------------------------------------------------------------------------
-spec all() -> list(peer_info()).
all() ->
    gen_server:call(?MODULE, all).

-spec blocked() -> list(peer_info()).
blocked() ->
    gen_server:call(?MODULE, blocked).

%%------------------------------------------------------------------------------
%% Get up to N random peers.
%%
%% The peers are randomly distributed in the sorted gb_tree (due to the use of hash_uri),
%% so we can find a random peer by choosing a point and getting the next peer in gb_tree.
%% That's what this function does
%%------------------------------------------------------------------------------
-spec get_random(all | non_neg_integer()) -> [peer_info()].
get_random(NumberOfPeers) ->
    get_random(NumberOfPeers, []).

%%------------------------------------------------------------------------------
%% Get up to N random peers, but not peers included in the Exclude list
%%
%% The peers are randomly distributed in the sorted gb_tree (due to the use of hash_uri),
%% so we can find a random peer by choosing a point and getting the next peer in gb_tree.
%% That's what this function does
%%------------------------------------------------------------------------------
-spec get_random(all | non_neg_integer(), [peer_id()]) -> [peer_info()].
get_random(N, Exclude) when is_list(Exclude),
    N == all orelse (is_integer(N) andalso N >= 0) ->
    gen_server:call(?MODULE, {get_random, N, Exclude}).

%%------------------------------------------------------------------------------
%% Set our own peer id
%%------------------------------------------------------------------------------
-spec set_local_peer_info(peer_info()) -> ok.
set_local_peer_info(Peer) ->
    gen_server:cast(?MODULE, {set_local_peer_info, Peer}).

%%------------------------------------------------------------------------------
%% Set our own peer id
%%------------------------------------------------------------------------------
-spec get_local_peer_info() -> peer_info().
get_local_peer_info() ->
    gen_server:call(?MODULE, get_local_peer_info).

get_connection(PeerId) ->
    gen_server:call(?MODULE, {get_connection, PeerId}).

-spec log_ping(peer_id(), ok | error) -> ok | {error, any()}.
log_ping(Peer, Result) ->
    gen_server:cast(?MODULE, {log_ping, Result, Peer, timestamp()}).

%%------------------------------------------------------------------------------
%% Check user-provided environment
%%------------------------------------------------------------------------------
check_env() ->
    ok.

-ifdef(TEST).
unblock_all() ->
    gen_server:call(?MODULE, unblock_all).
-endif.

%%%=============================================================================
%%% gen_server functions
%%%=============================================================================


-record(state, {peers                   :: gb_trees:tree(binary(), peer()),
                blocked = gb_sets:new() :: gb_sets:set(peer_id()),
                local_peer              :: peer_info(),  %% for universal handling of URIs
                next_unblock = 0        :: integer() %% Erlang timestamp in ms.
               }).

start_link() ->
    gen_server:start_link({local, ?MODULE} ,?MODULE, ok, []).

init(ok) ->
    {_Scheme, Host, _Port} = aeu_env:local_peer(),
    Port = sync_port(),
    {ok, SecKey} = aec_keys:peer_privkey(),
    {ok, PubKey} = aec_keys:peer_pubkey(),
    LocalPeer = #{host => Host, port => Port, seckey => SecKey, pubkey => PubKey},
    lager:info("aec_peers started at ~p", [LocalPeer]),
    {ok, #state{peers = gb_trees:empty(),
                local_peer = LocalPeer}}.

handle_call({set_local_peer_info, Peer}, _From, State) ->
    {reply, ok, State#state{local_peer = Peer}};
handle_call(get_local_peer_info, _From, State) ->
    {reply, State#state.local_peer, State};
handle_call({is_blocked, Peer}, _From, State0) ->
    State = maybe_unblock(State0),
    {reply, is_blocked(Peer, State), State};
handle_call({block, PeerId}, _From, State0) ->
    State = maybe_unblock(State0),
    #state{peers = Peers, blocked = Blocked} = State,
    NewState =
        case lookup_peer(PeerId, State) of
            {value, _Key, #peer{ trusted = true }} -> State;
            {value, _Key, Peer} ->
                aec_events:publish(peers, {blocked, PeerId}),
                stop_connection(Peer),
                State#state{peers   = remove_peer(PeerId, Peers),
                            blocked = add_blocked(PeerId, Blocked)};
            none ->
                aec_events:publish(peers, {blocked, PeerId}),
                State#state{blocked = add_blocked(PeerId, Blocked)}
        end,
    {reply, ok, NewState};
handle_call({unblock, PeerId}, _From, #state{blocked = Blocked} = State) ->
    NewState =
        case is_blocked(PeerId, State) of
            false -> State;
            true ->
                aec_events:publish(peers, {unblocked, PeerId}),
                State#state{blocked = del_blocked(PeerId, Blocked)}
        end,
    {reply, ok, NewState};
%% unblock_all is only available for test
handle_call(unblock_all, _From, State) ->
    {reply, ok, State#state{blocked = gb_sets:new(), next_unblock = next_unblock()}};
handle_call(all, _From, State = #state{peers = Peers}) ->
    PeerIds = [ peer_info(P) || P <- gb_trees:values(Peers) ],
    {reply, PeerIds, State};
handle_call(blocked, _From, State) ->
    Peers = [ peer_info(Peer) || Peer <- gb_sets:to_list(State#state.blocked) ],
    {reply, Peers, State};
handle_call({get_connection, PeerId}, _From, State) ->
    Res =
        case lookup_peer(PeerId, State) of
            {value, _, #peer{ connection = Conn }} -> {ok, Conn};
            none -> {error, no_connection}
        end,
    {reply, Res, State};
handle_call({get_random, N, Exclude}, _From, #state{peers = Peers} = State) ->
    %% first, remove all errored peers
    ActivePeers = [ P || P = #peer{ connection = {connected, _} } <- gb_trees:values(Peers),
                         not lists:member(peer_id(P), Exclude) ],

    SelectedPeers = pick_n(N, ActivePeers),
    {reply, [ peer_info(P) || P <- SelectedPeers ], State};

handle_call({connect_fail, PeerId, PeerCon}, _From, State = #state{ peers = Peers }) ->
    case lookup_peer(PeerId, State) of
        none ->
            lager:info("Got connect_fail from unknown Peer - ~p", [ppp(PeerId)]),
            {reply, close, State};
        {value, _Key, Peer = #peer{ connection = Con, retries = Rs }} ->
            {Res, NewPeers} =
                case Con of
                    {connected, PeerCon} ->
                        lager:debug("Connection to ~p failed - {connected, ~p} -> error",
                                    [ppp(PeerId), PeerCon]),
                        Peer1 = Peer#peer{ connection = {error, PeerCon}, retries = 0 },
                        Timeout = backoff_timeout(Peer1),
                        Peer2 = set_retry_timeout(Peer1, Timeout),
                        {keep, enter_peer(Peer2, Peers)};
                    {pending, PeerCon} ->
                        lager:debug("Connection to ~p failed - {pending, ~p} -> error",
                                    [ppp(PeerId), PeerCon]),
                        Peer1 = Peer#peer{ connection = {error, PeerCon}, retries = Rs + 1 },
                        case backoff_timeout(Peer1) of
                            stop ->
                                lager:info("Connection failed - remove peer: ~p",
                                           [ppp(PeerId)]),
                                {close, remove_peer(PeerId, Peers)};
                            Timeout ->
                                Peer2 = set_retry_timeout(Peer1, Timeout),
                                {keep, enter_peer(Peer2, Peers)}
                        end;
                    _PeerCon ->
                        lager:debug("Stale connection to ~p attempt failed - ~p",
                                    [ppp(PeerId), _PeerCon]),
                        {close, Peers}
                end,
            State1 = State#state{peers = NewPeers},
            {reply, Res, metrics(State1)}
    end;

handle_call({connect_peer, PeerId, PeerCon}, _From, State) ->
    case lookup_peer(PeerId, State) of
        none ->
            lager:info("Got connected_peer from unknown Peer - ~p", [ppp(PeerId)]),
            {reply, {error, invalid}, State};
        {value, _Key, Peer = #peer{ connection = Con }} ->
            {Res, NewPeer} =
                case Con of
                    {connected, PeerCon1} when PeerCon1 /= PeerCon->
                        %% Someone beat us to it...
                        lager:debug("Connection to ~p up - {connected, ~p} - keep using it",
                                    [ppp(PeerId), PeerCon1]),
                        {{error, already_connected}, Peer};
                    {ConState, PeerCon} ->
                        lager:debug("Connection to ~p up - {~p, ~p} -> connected",
                                    [ppp(PeerId), ConState, PeerCon]),
                        {ok, Peer#peer{ connection = {connected, PeerCon} }};
                    {ConState, PeerCon1} ->
                        lager:debug("Connection to ~p up - {~p, ~p} - use ~p",
                                    [ppp(PeerId), ConState, PeerCon1, PeerCon]),
                        lager:debug("Closing ~p ", [PeerCon1]),
                        aec_peer_connection:stop(PeerCon1),
                        {ok, Peer#peer{ connection = {connected, PeerCon} }}
                end,
            case Res of
                ok ->
                    lager:debug("Will ping peer ~p", [ppp(PeerId)]),
                    aec_sync:schedule_ping(PeerId);
                {error, _} ->
                    ok
            end,

            State1 = State#state{peers = enter_peer(NewPeer, State#state.peers)},
            {reply, Res, metrics(State1)}
    end;

handle_call({accept_peer, PeerInfo, PeerCon}, _From, State) ->
    Peer    = #peer{ host = maps:get(host, PeerInfo),
                     port = maps:get(port, PeerInfo),
                     pubkey = maps:get(pubkey, PeerInfo) },
    PeerId  = peer_id(Peer),
    case is_local(PeerId, State) orelse is_blocked(PeerId, State) of
        true ->
            lager:debug("Will not add peer ~p", [ppp(PeerId)]),
            {reply, {error, blocked}, State};
        false ->
            {Res, NewPeer} =
                case lookup_peer(PeerId, State) of
                    none ->
                        lager:debug("New peer from accept ~p", [PeerInfo]),
                        {ok, Peer#peer{ connection = {connected, PeerCon} }};
                    {value, _Key, FoundPeer = #peer{ connection = Con, pubkey = PKey }} ->
                        #{ pubkey := LPKey } = State#state.local_peer,
                        case Con of
                            undefined ->
                                lager:debug("Peer ~p accepted - undefined - use ~p",
                                            [ppp(PeerId), PeerCon]),
                                {ok, FoundPeer#peer{ connection = {connected, PeerCon} }};
                            {pending, PeerCon1} ->
                                lager:debug("Peer ~p accepted - {pending, ~p} - use ~p",
                                            [ppp(PeerId), PeerCon1, PeerCon]),
                                {ok, FoundPeer#peer{ connection = {connected, PeerCon} }};
                            {error, PeerCon1} ->
                                lager:debug("Peer ~p accepted - {error, ~p} - use ~p",
                                            [ppp(PeerId), PeerCon1, PeerCon]),
                                lager:debug("Closing ~p ", [PeerCon1]),
                                aec_peer_connection:stop(PeerCon1),
                                {ok, FoundPeer#peer{ connection = {connected, PeerCon} }};
                            {connected, PeerCon1} when LPKey > PKey ->
                                %% We slash the accepted connection and use PeerCon1
                                lager:debug("Peer ~p accepted - {connected, ~p} when LPK > PK - use ~p",
                                            [ppp(PeerId), PeerCon1, PeerCon1]),
                                {{error, already_connected}, FoundPeer};
                            {connected, PeerCon1} ->
                                %% Stop using PeerCon1 - use PeerCon
                                lager:debug("Peer ~p accepted - {connected, ~p} when PK > LPK - use ~p",
                                            [ppp(PeerId), PeerCon1, PeerCon]),
                                lager:debug("Closing ~p ", [PeerCon1]),
                                aec_peer_connection:stop(PeerCon1),
                                {ok, FoundPeer#peer{ connection = {connected, PeerCon} }}
                        end
                end,
            State1 = State#state{peers = enter_peer(NewPeer, State#state.peers)},
            {reply, Res, metrics(State1)}
    end.

handle_cast({log_ping, Result, PeerId, _Time}, State) ->
    %% Log the ping and schedule another as long as we have a connection
    update_ping_metrics(Result),
    case lookup_peer(PeerId, State) of
        none ->
            lager:debug("Reported ping event for unknown peer - ~p", [ppp(PeerId)]),
            {noreply, State};
        {value, _Hash, Peer = #peer{ connection = {connected, _PeerCon} }} ->
            %% TODO: if a ping keeps failing we should do something?!
            Peer1 = set_ping_timeout(Peer, ping_interval()),
            {noreply, State#state{ peers = enter_peer(Peer1, State#state.peers) }};
        {value, _Hash, _Peer} ->
            lager:debug("Log ping event for ~p - no new ping", [ppp(PeerId)]),
            {noreply, State}
    end;

handle_cast({add, PeerInfo}, State0) ->
    Peer    = #peer{ host = maps:get(host, PeerInfo),
                     port = maps:get(port, PeerInfo),
                     pubkey = maps:get(pubkey, PeerInfo) },
    PeerId  = peer_id(Peer),
    case is_local(PeerId, State0) orelse is_blocked(PeerId, State0) of
        false ->
            NewPeer =
                case lookup_peer(PeerId, State0) of
                    none ->
                        lager:debug("Adding peer ~p", [PeerInfo]),
                        case maps:get(ping, PeerInfo, false) of
                            true ->
                                #{ seckey := SKey, pubkey := PKey,
                                   host := LHost, port := LPort } = State0#state.local_peer,
                                ConnInfo = PeerInfo#{ r_pubkey => maps:get(pubkey, PeerInfo),
                                                      local_host => LHost, local_port => LPort,
                                                      seckey => SKey, pubkey => PKey },
                                {ok, Pid} = aec_peer_connection:connect(ConnInfo),
                                Peer#peer{ connection = {pending, Pid} };
                            false ->
                                Peer#peer{ connection = undefined }
                        end;
                    {value, _Key, FoundPeer} ->
                        FoundPeer
                end,
            State1 = State0#state{peers = enter_peer(NewPeer, State0#state.peers)},
            {noreply, metrics(State1)};
        true ->
            lager:debug("Will not add peer ~p", [ppp(PeerId)]),
            {noreply, State0}
    end;
handle_cast({remove, PeerIdOrInfo}, State = #state{peers = Peers, blocked = Blocked}) ->
    PeerId = peer_id(PeerIdOrInfo),
    case lookup_peer(PeerId, State) of
        none -> ok;
        {value, _Key, Peer} -> stop_connection(Peer)
    end,
    lager:debug("Will remove peer ~p", [ppp(PeerId)]),
    NewState = State#state{peers   = remove_peer(PeerId, Peers),
                           blocked = del_blocked(PeerId, Blocked)},
    {noreply, metrics(NewState)}.

handle_info({timeout, Ref, {Msg, PeerId}}, State) ->
    case lookup_peer(PeerId, State) of
        none ->
            lager:debug("timer msg for unknown peer (~p)", [ppp(PeerId)]),
            {noreply, State};
        {value, _Hash, #peer{timer_tref = Ref} = Peer} when Msg == ping ->
            maybe_ping_peer(PeerId, State),
            Peers = enter_peer(Peer#peer{timer_tref = undefined},
                               State#state.peers),
            {noreply, State#state{peers = Peers}};
        {value, _Hash, #peer{timer_tref = Ref} = Peer} when Msg == retry ->
            maybe_retry_peer(Peer),
            Peers = enter_peer(Peer#peer{timer_tref = undefined},
                               State#state.peers),
            {noreply, State#state{peers = Peers}};
        {value, _, #peer{}} ->
            lager:debug("stale ping_peer timer msg (~p) - ignore", [ppp(PeerId)]),
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

metrics(#state{peers = Peers, blocked = Blocked} = State) ->
    aec_metrics:try_update([ae,epoch,aecore,peers,count],
                           gb_trees:size(Peers)),
    aec_metrics:try_update([ae,epoch,aecore,peers,blocked],
                           gb_sets:size(Blocked)),
    State.

maybe_unblock(State = #state{ next_unblock = 0 }) ->
    State#state{ next_unblock = next_unblock() };
maybe_unblock(State = #state{ next_unblock = NextT }) ->
    case timestamp() > NextT of
        false -> State;
        true  -> State#state{ blocked = gb_sets:new(),
                              next_unblock = next_unblock() }
    end.

next_unblock() ->
    timestamp() +
        application:get_env(aecore, peer_unblock_interval, ?DEFAULT_UNBLOCK_INTERVAL).

%% Updates if already exists.
enter_peer(#peer{} = P, Peers) ->
    gb_trees:enter(hash_peer(P), P, Peers).

timestamp() ->
    erlang:system_time(millisecond).

-spec hash_peer(peer() | binary()) -> binary().
hash_peer(#peer{} = P) ->
    hash_peer(peer_id(P));
hash_peer(<<PK:32/binary, _/binary>>) ->
    aec_hash:hash(peer_id, PK).

remove_peer(PeerId, Peers) when is_binary(PeerId) ->
    gb_trees:delete_any(hash_peer(PeerId), Peers).

pick_n(all, Xs) -> Xs;
pick_n(N, Xs) when length(Xs) =< N -> Xs;
pick_n(N, Xs) ->
    pick_n(N, Xs, length(Xs), []).

pick_n(0, _Xs, _L, Acc) ->
    Acc;
pick_n(N, Xs, L, Acc) ->
    {Ys, [X | Zs]} = lists:split(rand:uniform(L)-1, Xs),
    pick_n(N-1, Ys ++ Zs, L-1, [X | Acc]).

lookup_peer(PeerId, #state{peers = Peers}) when is_binary(PeerId) ->
    Key = hash_peer(PeerId),
    case gb_trees:lookup(Key, Peers) of
        none -> none;
        {value, P} ->
            {value, Key, P}
    end.

is_blocked(<<PubKey:32/binary, _/binary>>, #state{blocked = Blocked}) ->
    gb_sets:is_element(PubKey, Blocked).


update_ping_metrics(Res) ->
    Name = case Res of
               ok    -> [ae,epoch,aecore,peers,ping,success];
               error -> [ae,epoch,aecore,peers,ping,failure]
           end,
    aec_metrics:try_update(Name, 1).

add_blocked(_PeerId = <<PubKey:32/binary, _/binary>>, Blocked) ->
    gb_sets:add_element(PubKey, Blocked).

del_blocked(_PeerId = <<PubKey:32/binary, _/binary>>, Blocked) ->
    gb_sets:delete_any(PubKey, Blocked).

stop_connection(#peer{ connection = Pid }) when is_pid(Pid) ->
    aec_peer_connection:stop(Pid);
stop_connection(_) ->
    ok.


maybe_ping_peer(PeerId, State) ->
    case is_local(PeerId, State) orelse is_blocked(PeerId, State) of
        false ->
            lager:debug("Will ping peer ~p", [ppp(PeerId)]),
            aec_sync:schedule_ping(PeerId);
        true ->
            lager:debug("Will not ping ~p", [ppp(PeerId)]),
            ignore
    end.

maybe_retry_peer(P = #peer{ connection = {error, PeerCon} }) ->
    lager:debug("Will retry peer ~p", [peer_info(P)]),
    aec_peer_connection:retry(PeerCon);
maybe_retry_peer(P) ->
    lager:debug("No need to retry peer ~p", [ppp(peer_id(P))]),
    ok.

is_local(Peer, #state{local_peer = LocalPeer}) ->
    #{ pubkey := PubKey1 } = peer_info(Peer),
    #{ pubkey := PubKey2 } = peer_info(LocalPeer),
    PubKey1 == PubKey2.

peer_id(PeerId) when is_binary(PeerId) ->
    PeerId;
peer_id(#{ host := Host, port := Port, pubkey := PubKey }) ->
    peer_id(PubKey, Host, Port);
peer_id(#peer{ host = Host, port = Port, pubkey = PubKey }) ->
    peer_id(PubKey, Host, Port).

peer_info(PeerId) when is_binary(PeerId) ->
    split_peer_id(PeerId);
peer_info(#peer{ host = H, port = P, pubkey = PK }) ->
    #{ host => H, port => P, pubkey => PK };
peer_info(PeerInfo = #{ host := _, port := _, pubkey := _ }) ->
    PeerInfo.

peer_id(PubKey, Host, Port) when is_binary(Host) ->
    <<PubKey/binary, Port:16, Host/binary>>;
peer_id(PubKey, Host, Port) when is_list(Host) ->
    peer_id(PubKey, list_to_binary(Host), Port).

split_peer_id(<<PubKey:32/binary, Port:16, Host/binary>>) ->
    #{ pubkey => PubKey, host => Host, port => Port }.


ppp(PeerId) when is_binary(PeerId) ->
    #{ pubkey := PK } = split_peer_id(PeerId),
    Str = lists:flatten(io_lib:format("~p", [aec_base58c:encode(peer_pubkey, PK)])),
    lists:sublist(Str, 4, 10) ++ "..." ++ lists:sublist(Str, 48, 8);
ppp(#{ pubkey := PK }) ->
    Str = lists:flatten(io_lib:format("~p", [aec_base58c:encode(peer_pubkey, PK)])),
    lists:sublist(Str, 4, 10) ++ "..." ++ lists:sublist(Str, 48, 8);
ppp(X) ->
    X.

set_retry_timeout(Peer, Timeout) ->
    set_timeout(Peer, Timeout, retry).

set_ping_timeout(Peer, Timeout) ->
    set_timeout(Peer, Timeout, ping).

set_timeout(Peer = #peer{ timer_tref = Prev }, Timeout, Msg) ->
    lager:debug("Starting ~p timer for ~p: ~p", [Msg, ppp(peer_id(Peer)), Timeout]),
    TRef = erlang:start_timer(Timeout, self(), {Msg, peer_id(Peer)}),
    case Prev of
        undefined -> ok;
        _ -> try erlang:cancel_timer(Prev, [{async, true}, {info, false}])
             catch error:_ -> ok end
    end,
    Peer#peer{timer_tref = TRef}.

backoff_timeout(#peer{ retries = Retries, trusted = Trusted }) ->
    case Retries of
        N when N < length(?BACKOFF_TIMES) ->
            lists:nth(N+1, ?BACKOFF_TIMES) * 1000;
        _ when Trusted ->
            600 * 1000;
        _ ->
            stop
    end.

ping_interval() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"ping_interval">>],
                               aecore, ping_interval, ?DEFAULT_PING_INTERVAL).

sync_port() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"port">>], aecore, sync_port, ?DEFAULT_SYNC_PORT).

parse_peer_address(PeerAddress) ->
    case http_uri:parse(PeerAddress) of
        {ok, {aenode, EncPubKey, Host, Port, _Path, _Query}} ->
            case aec_base58c:safe_decode(peer_pubkey, to_binary(EncPubKey)) of
                {ok, PubKey} ->
                    {ok, #{ host => to_binary(Host), port => Port, pubkey => PubKey }};
                Err = {error, _} ->
                    Err
            end;
        Err = {error, _Reason} ->
            Err
    end.

encode_peer_address(#{ host := Host, port := Port, pubkey := PubKey }) ->
    list_to_binary(["aenode://", aec_base58c:encode(peer_pubkey, PubKey), "@",
                    Host, ":", integer_to_list(Port)]).

to_binary(Bin) when is_binary(Bin) -> Bin;
to_binary(List) when is_list(List) -> list_to_binary(List).
