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
         get_local_peer_info/0]).

%% API only used in aec_sync
-export([ add/1
        , log_ping/2
        , peer_id/1
        , ppp/1
        ]).

-export([check_env/0]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

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
-type peer_info() :: #{ host    => string() | binary(),
                        port    => inet:port_number(),
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
-spec block_peer(peer_info()) -> ok | {error, term()}.
block_peer(PeerInfo) ->
    gen_server:call(?MODULE, {block, PeerInfo}).

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
unblock_peer(PeerId) ->
    gen_server:call(?MODULE, {unblock, PeerId}).

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
%------------------------------------------------------------------------------
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

-spec get_local_peer_info() -> peer_info().
get_local_peer_info() ->
    gen_server:call(?MODULE, get_local_peer_info).

get_connection(PeerId) ->
    gen_server:call(?MODULE, {get_connection, PeerId}).

-spec log_ping(peer_id(), ok | error) -> ok | {error, any()}.
log_ping(PeerId, Result) ->
    gen_server:cast(?MODULE, {log_ping, Result, PeerId, timestamp()}).

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


-record(state, {peers            :: gb_trees:tree(peer_id(), peer()),
                blocked          :: gb_trees:tree(peer_id(), peer_info()),
                local_peer       :: peer_info(),  %% for universal handling of URIs
                next_unblock = 0 :: integer(), %% Erlang timestamp in ms.
                peer_monitors    :: ets:tab()
               }).

start_link() ->
    gen_server:start_link({local, ?MODULE} ,?MODULE, ok, []).


init(ok) ->
    {ok, SecKey} = aec_keys:peer_privkey(),
    {ok, PubKey} = aec_keys:peer_pubkey(),
    LocalPeer = #{seckey => SecKey, pubkey => PubKey},
    PMons = ets:new(aec_peer_monitors, [named_table, protected]),
    epoch_sync:info("aec_peers started at ~p", [LocalPeer]),
    {ok, #state{ peers = gb_trees:empty(),
                 blocked = gb_trees:empty(),
                 local_peer = LocalPeer,
                 peer_monitors = PMons
               }}.

handle_call(get_local_peer_info, _From, State) ->
    {reply, State#state.local_peer, State};
handle_call({is_blocked, PeerId}, _From, State0) ->
    State = maybe_unblock(State0),
    {reply, is_blocked(PeerId, State), State};
handle_call({block, PeerInfo}, _From, State0) ->
    PeerId = peer_id(PeerInfo),
    State = maybe_unblock(State0),
    #state{peers = Peers, blocked = Blocked} = State,
    NewState =
        case lookup_peer(PeerId, State) of
            {value, _Key, #peer{ trusted = true }} -> State;
            {value, _Key, Peer} ->
                aec_events:publish(peers, {blocked, PeerId}),
                stop_connection(Peer),
                State#state{peers   = remove_peer(PeerId, Peers),
                            blocked = add_blocked(PeerId, peer_info(Peer), Blocked)};
            none ->
                aec_events:publish(peers, {blocked, PeerId}),
                State#state{blocked = add_blocked(PeerId, PeerInfo, Blocked)}
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
    {reply, ok, State#state{blocked = gb_trees:empty(), next_unblock = next_unblock()}};
handle_call(all, _From, State = #state{peers = Peers}) ->
    PeerIds = [ peer_info(P) || P <- gb_trees:values(Peers) ],
    {reply, PeerIds, State};
handle_call(blocked, _From, State = #state{blocked = Blocked}) ->
    Peers = [ PeerInfo || PeerInfo <- gb_trees:values(Blocked) ],
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
            epoch_sync:info("Got connect_fail from unknown Peer - ~p", [ppp(PeerId)]),
            {reply, close, State};
        {value, _Key, Peer = #peer{ connection = Con, retries = Rs }} ->
            {Res, NewPeers} =
                case Con of
                    {connected, PeerCon} ->
                        epoch_sync:debug("Connection to ~p failed - {connected, ~p} -> error",
                                         [ppp(PeerId), PeerCon]),
                        Peer1 = Peer#peer{ connection = {error, PeerCon}, retries = 0 },
                        Timeout = backoff_timeout(Peer1),
                        Peer2 = set_retry_timeout(Peer1, Timeout),
                        {keep, enter_peer(Peer2, Peers)};
                    {pending, PeerCon} ->
                        epoch_sync:debug("Connection to ~p failed - {pending, ~p} -> error",
                                         [ppp(PeerId), PeerCon]),
                        Peer1 = Peer#peer{ connection = {error, PeerCon}, retries = Rs + 1 },
                        case backoff_timeout(Peer1) of
                            stop ->
                                epoch_sync:info("Connection failed - remove peer: ~p",
                                                [ppp(PeerId)]),
                                {close, remove_peer(PeerId, Peers)};
                            Timeout ->
                                Peer2 = set_retry_timeout(Peer1, Timeout),
                                {keep, enter_peer(Peer2, Peers)}
                        end;
                    _PeerCon ->
                        epoch_sync:debug("Stale connection to ~p attempt failed - ~p",
                                         [ppp(PeerId), _PeerCon]),
                        {close, Peers}
                end,
            State1 = State#state{peers = NewPeers},
            {reply, Res, metrics(State1)}
    end;

handle_call({connect_peer, PeerId, PeerCon}, _From, State) ->
    case lookup_peer(PeerId, State) of
        none ->
            epoch_sync:info("Got connected_peer from unknown Peer - ~p", [ppp(PeerId)]),
            {reply, {error, invalid}, State};
        {value, _Key, Peer = #peer{ connection = Con }} ->
            {Res, NewPeer} =
                case Con of
                    {connected, PeerCon1} when PeerCon1 /= PeerCon->
                        %% Someone beat us to it...
                        epoch_sync:debug("Connection to ~p up - {connected, ~p} - keep using it",
                                         [ppp(PeerId), PeerCon1]),
                        {{error, already_connected}, Peer};
                    {ConState, PeerCon} ->
                        epoch_sync:debug("Connection to ~p up - {~p, ~p} -> connected",
                                         [ppp(PeerId), ConState, PeerCon]),
                        {ok, Peer#peer{ connection = {connected, PeerCon} }};
                    {ConState, PeerCon1} ->
                        epoch_sync:debug("Connection to ~p up - {~p, ~p} - use ~p",
                                         [ppp(PeerId), ConState, PeerCon1, PeerCon]),
                        epoch_sync:debug("Closing ~p ", [PeerCon1]),
                        aec_peer_connection:stop(PeerCon1),
                        {ok, Peer#peer{ connection = {connected, PeerCon} }}
                end,
            case Res of
                ok ->
                    epoch_sync:debug("Will ping peer ~p", [ppp(PeerId)]),
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
            epoch_sync:debug("Will not add peer ~p", [ppp(PeerId)]),
            {reply, {error, blocked}, State};
        false ->
            {Res, NewPeer} =
                case lookup_peer(PeerId, State) of
                    none ->
                        epoch_sync:debug("New peer from accept ~p", [PeerInfo]),
                        {ok, Peer#peer{ connection = {connected, PeerCon} }};
                    {value, _Key, FoundPeer = #peer{ connection = Con, pubkey = PKey }} ->
                        #{ pubkey := LPKey } = State#state.local_peer,
                        case Con of
                            undefined ->
                                epoch_sync:debug("Peer ~p accepted - undefined - use ~p",
                                                 [ppp(PeerId), PeerCon]),
                                {ok, FoundPeer#peer{ connection = {connected, PeerCon} }};
                            {pending, PeerCon1} ->
                                epoch_sync:debug("Peer ~p accepted - {pending, ~p} - use ~p",
                                                 [ppp(PeerId), PeerCon1, PeerCon]),
                                {ok, FoundPeer#peer{ connection = {connected, PeerCon} }};
                            {error, PeerCon1} ->
                                epoch_sync:debug("Peer ~p accepted - {error, ~p} - use ~p",
                                                 [ppp(PeerId), PeerCon1, PeerCon]),
                                epoch_sync:debug("Closing ~p ", [PeerCon1]),
                                aec_peer_connection:stop(PeerCon1),
                                {ok, FoundPeer#peer{ connection = {connected, PeerCon} }};
                            {connected, PeerCon1} when LPKey > PKey ->
                                %% We slash the accepted connection and use PeerCon1
                                epoch_sync:debug("Peer ~p accepted - {connected, ~p} when LPK > PK - use ~p",
                                                 [ppp(PeerId), PeerCon1, PeerCon1]),
                                {{error, already_connected}, FoundPeer};
                            {connected, PeerCon1} ->
                                %% Stop using PeerCon1 - use PeerCon
                                epoch_sync:debug("Peer ~p accepted - {connected, ~p} when PK > LPK - use ~p",
                                                 [ppp(PeerId), PeerCon1, PeerCon]),
                                epoch_sync:debug("Closing ~p ", [PeerCon1]),
                                aec_peer_connection:stop(PeerCon1),
                                {ok, FoundPeer#peer{ connection = {connected, PeerCon} }}
                        end
                end,
            maybe_add_monitor(NewPeer, State),
            State1 = State#state{peers = enter_peer(NewPeer, State#state.peers)},
            {reply, Res, metrics(State1)}
    end.

handle_cast({log_ping, Result, PeerId, _Time}, State) ->
    %% Log the ping and schedule another as long as we have a connection
    update_ping_metrics(Result),
    case lookup_peer(PeerId, State) of
        none ->
            epoch_sync:debug("Reported ping event for unknown peer - ~p", [ppp(PeerId)]),
            {noreply, State};
        {value, _Hash, Peer = #peer{ connection = {connected, _PeerCon} }} ->
            %% TODO: if a ping keeps failing we should do something?!
            Peer1 = set_ping_timeout(Peer, ping_interval()),
            {noreply, State#state{ peers = enter_peer(Peer1, State#state.peers) }};
        {value, _Hash, _Peer} ->
            epoch_sync:debug("Log ping event for ~p - no new ping", [ppp(PeerId)]),
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
                        epoch_sync:debug("Adding peer ~p", [PeerInfo]),
                        case maps:get(ping, PeerInfo, false) of
                            true ->
                                #{ seckey := SKey, pubkey := PKey } = State0#state.local_peer,
                                ConnInfo = PeerInfo#{ r_pubkey => maps:get(pubkey, PeerInfo),
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
            maybe_add_monitor(NewPeer, State1),
            {noreply, metrics(State1)};
        true ->
            epoch_sync:debug("Will not add peer ~p", [ppp(PeerId)]),
            {noreply, State0}
    end;
handle_cast({remove, PeerIdOrInfo}, State = #state{peers = Peers, blocked = Blocked}) ->
    PeerId = peer_id(PeerIdOrInfo),
    case lookup_peer(PeerId, State) of
        none -> ok;
        {value, _Key, Peer} -> stop_connection(Peer)
    end,
    epoch_sync:debug("Will remove peer ~p", [ppp(PeerId)]),
    NewState = State#state{peers   = remove_peer(PeerId, Peers),
                           blocked = del_blocked(PeerId, Blocked)},
    {noreply, metrics(NewState)}.

handle_info({timeout, Ref, {Msg, PeerId}}, State) ->
    case lookup_peer(PeerId, State) of
        none ->
            epoch_sync:debug("timer msg for unknown peer (~p)", [ppp(PeerId)]),
            {noreply, State};
        {value, _Hash, #peer{timer_tref = Ref} = Peer} when Msg == ping ->
            maybe_ping_peer(PeerId, State),
            Peers = enter_peer(Peer#peer{timer_tref = undefined},
                               State#state.peers),
            {noreply, State#state{peers = Peers}};
        {value, _Hash, #peer{timer_tref = Ref} = Peer} when Msg == retry ->
            Peer1 = maybe_retry_peer(Peer),
            Peers = enter_peer(Peer1#peer{timer_tref = undefined},
                               State#state.peers),
            {noreply, State#state{peers = Peers}};
        {value, _, #peer{}} ->
            epoch_sync:debug("stale ping_peer timer msg (~p) - ignore", [ppp(PeerId)]),
            {noreply, State}
    end;
handle_info({'DOWN', Ref, process, Pid, _}, #state{peer_monitors = PMons} = State) ->
    [{Pid, Ref, PeerId}] = ets:lookup(PMons, Pid),
    ets:delete(PMons, Pid),
    case lookup_peer(PeerId, State) of
        {value, _, #peer{connection = {_, Pid}}} ->
            %% This was an unexpected process down. Clean it up.
            %% TODO: This should probably be restarted.
            epoch_sync:warning("Peer connection died - removing: ~p : ~p",
                               [Pid, ppp(PeerId)]),
            Peers = remove_peer(PeerId, State#state.peers),
            {noreply, State#state{peers = Peers}};
        _ ->
            %% This was a stale process monitor message
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

maybe_add_monitor(#peer{connection = {_, Pid}} = Peer, #state{peer_monitors = PMons}) ->
    case ets:lookup(PMons, Pid) =:= [] of
        true ->
            Ref = monitor(process, Pid),
            PeerId = peer_id(Peer),
            ets:insert(PMons, [{Pid, Ref, PeerId}]),
            ok;
        false ->
            ok
    end;
maybe_add_monitor(#peer{}, #state{}) ->
    ok.

metrics(#state{peers = Peers, blocked = Blocked} = State) ->
    aec_metrics:try_update([ae,epoch,aecore,peers,count],
                           gb_trees:size(Peers)),
    aec_metrics:try_update([ae,epoch,aecore,peers,blocked],
                           gb_trees:size(Blocked)),
    State.

maybe_unblock(State = #state{ next_unblock = 0 }) ->
    State#state{ next_unblock = next_unblock() };
maybe_unblock(State = #state{ next_unblock = NextT }) ->
    case timestamp() > NextT of
        false -> State;
        true  -> State#state{ blocked = gb_trees:empty(),
                              next_unblock = next_unblock() }
    end.

next_unblock() ->
    timestamp() +
        application:get_env(aecore, peer_unblock_interval, ?DEFAULT_UNBLOCK_INTERVAL).

%% Updates if already exists.
enter_peer(#peer{} = P, Peers) ->
    gb_trees:enter(peer_id(P), P, Peers).

timestamp() ->
    erlang:system_time(millisecond).

remove_peer(PeerId, Peers) when is_binary(PeerId) ->
    gb_trees:delete_any(PeerId, Peers).

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
    case gb_trees:lookup(PeerId, Peers) of
        none -> none;
        {value, P} ->
            {value, PeerId, P}
    end.

is_blocked(PeerId, #state{blocked = Blocked}) ->
    gb_trees:is_defined(PeerId, Blocked).

update_ping_metrics(Res) ->
    Name = case Res of
               ok    -> [ae,epoch,aecore,peers,ping,success];
               error -> [ae,epoch,aecore,peers,ping,failure]
           end,
    aec_metrics:try_update(Name, 1).

add_blocked(PeerId, PeerInfo, Blocked) ->
    gb_trees:enter(PeerId, PeerInfo, Blocked).

del_blocked(PeerId, Blocked) ->
    gb_trees:delete_any(PeerId, Blocked).

stop_connection(#peer{ connection = Pid }) when is_pid(Pid) ->
    aec_peer_connection:stop(Pid);
stop_connection(_) ->
    ok.

maybe_ping_peer(PeerId, State) ->
    case is_local(PeerId, State) orelse is_blocked(PeerId, State) of
        false ->
            epoch_sync:debug("Will ping peer ~p", [ppp(PeerId)]),
            aec_sync:schedule_ping(PeerId);
        true ->
            epoch_sync:debug("Will not ping ~p", [ppp(PeerId)]),
            ignore
    end.

maybe_retry_peer(P = #peer{ connection = {error, PeerCon} }) ->
    epoch_sync:debug("Will retry peer ~p", [peer_info(P)]),
    aec_peer_connection:retry(PeerCon),
    P#peer{ connection = {pending, PeerCon} };
maybe_retry_peer(P) ->
    epoch_sync:debug("No need to retry peer ~p", [ppp(peer_id(P))]),
    P.

is_local(PeerId1, #state{local_peer = LocalPeer}) ->
    PeerId2 = peer_id(LocalPeer),
    PeerId1 == PeerId2.

peer_id(PeerId) when is_binary(PeerId) ->
    PeerId;
peer_id(#{ pubkey := PubKey }) ->
    PubKey;
peer_id(#peer{ pubkey = PubKey }) ->
    PubKey.

peer_info(PeerId) when is_binary(PeerId) ->
    #{ host => <<"unknown">>, port => 0, pubkey => PeerId };
peer_info(#peer{ host = H, port = P, pubkey = PK }) ->
    #{ host => H, port => P, pubkey => PK };
peer_info(PeerInfo = #{ pubkey := _ }) ->
    PeerInfo.

ppp(PeerId) when is_binary(PeerId) ->
    Str = lists:flatten(io_lib:format("~p", [aec_base58c:encode(peer_pubkey, PeerId)])),
    lists:sublist(Str, 4, 10) ++ "..." ++ lists:sublist(Str, length(Str) - 9, 7);
ppp(#{ pubkey := PK }) ->
    Str = lists:flatten(io_lib:format("~p", [aec_base58c:encode(peer_pubkey, PK)])),
    lists:sublist(Str, 4, 10) ++ "..." ++ lists:sublist(Str, length(Str) - 9, 7);
ppp(X) ->
    X.

set_retry_timeout(Peer, Timeout) ->
    set_timeout(Peer, Timeout, retry).

set_ping_timeout(Peer, Timeout) ->
    set_timeout(Peer, Timeout, ping).

set_timeout(Peer = #peer{ timer_tref = Prev }, Timeout, Msg) ->
    epoch_sync:debug("Starting ~p timer for ~p: ~p", [Msg, ppp(peer_id(Peer)), Timeout]),
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
