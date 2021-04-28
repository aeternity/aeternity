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
%%% ONLY SUPPORTS IPv4.
%%%
%%% @end
%%%=============================================================================
-module(aec_peers).

-behaviour(gen_server).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== EXPORTS ===================================================================

%% API to manage peers.
-export([add_trusted/1]).
-export([add_peers/2]).
-export([del_peer/1]).
-export([block_peer/1]).
-export([unblock_peer/1]).
-export([peer_suspect/1]).

%% API for getters and flags.
-export([is_blocked/1]).
-export([count/1]).
-export([connected_peers/0, connected_peers/1]).
-export([available_peers/0, available_peers/1]).
-export([blocked_peers/0]).
-export([get_random/1, get_random/2]).
-export([get_random_connected/1]).
-export([get_connection/1]).

%% Utility functions.
-export([parse_peer_address/1]).
-export([encode_peer_address/1]).
-export([local_peer_address/0]).

%% API only to be used by aec_peer_connection.
-export([peer_connected/2]).
-export([peer_accepted/3]).
-export([peer_alive/2]).
-export([connection_failed/2]).
-export([connection_closed/2]).
-export([peer_dead/2]).

%% API only used in aec_sync.
-export([log_ping/2]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

%=== MACROS ====================================================================

-define(MIN_CONNECTION_INTERVAL,             100).
-define(MAX_CONNECTION_INTERVAL,       30 * 1000). % 30 seconds

-define(MIN_TCP_PROBE_INTERVAL,              100).
-define(MAX_TCP_PROBE_INTERVAL,       1 * 1000). % 1 second 

-define(DEFAULT_PING_INTERVAL,        120 * 1000). %  2 minutes
-define(DEFAULT_UNBLOCK_INTERVAL, 15 * 60 * 1000). % 15 minutes
-define(DEFAULT_MAX_INBOUND,                 100).
-define(DEFAULT_MAX_OUTBOUND,                 10).
-define(DEFAULT_MAX_TCP_PROBES,              100).
-define(DEFAULT_SINGLE_OUTBOUND_PER_GROUP,  true).
-define(DEFAULT_RESOLVE_MAX_SIZE,            100).
-define(DEFAULT_RESOLVE_MAX_RETRY,             7).
-define(DEFAULT_RESOLVE_BACKOFF_TIMES,
        [5000, 15000, 30000, 60000, 120000, 300000, 600000]).
-define(DEFAULT_LOG_PEER_CONNECTION_COUNT_INTERVAL, 5000).

%=== TYPES =====================================================================

-record(conn, {
    peer              :: aec_peer:peer(),
    state             :: connecting | connected,
    type              :: inbound | outbound,
    tcp_probe = false :: boolean(),
    kind = undefined  :: undefined | temporary | permanent,
    ping = undefined  :: undefined | reference(),
    pid               :: pid()
}).

-record(state, {
    pool              :: aec_peers_pool:state(),
    %$ The address groups we have an outbound connection for.
    groups            :: #{binary() => #{aec_peer:id() => true}},
    %% The peer connections.
    conns             :: #{aec_peer:id() => conn()},
    %% The number of inbound connections.
    inbound = 0       :: non_neg_integer(),
    %% The number of outbound connections.
    outbound = 0      :: non_neg_integer(),
    %% The number of currently waiting tcp probes
    tcp_probes = 0    :: non_neg_integer(),
    %% The connect timer reference.
    connect_ref       :: undefined | reference(),
    %% The verified TCP probe timer reference.
    tcp_verified_probe_ref     :: undefined | reference(),
    %% The unverified TCP probe timer reference.
    tcp_unverified_probe_ref   :: undefined | reference(),
    %% The last time we try connecting to a peer.
    last_connect_time :: non_neg_integer(),
    %% The last time there was a TCP probe to a verified peer.
    last_tcp_verified_probe_time :: undefined | non_neg_integer(),
    %% The last time there was a TCP probe to an unverified peer.
    last_tcp_unverified_probe_time :: undefined | non_neg_integer(),
    %% The blocked peers.
    blocked           :: gb_trees:tree(aec_peer:id(), aec_peer:info()),
    %% The set of known addresses and ports
    known_sockets     :: gb_trees:tree(aec_peer:socket(), aec_peer:id()),
    %% For universal handling of URIs.
    local_peer        :: aec_peer:info(),
    %% The time of next unblock as an erlang timestamp in ms.
    next_unblock = 0  :: non_neg_integer(),
    %% The connection monitors.
    monitors          :: #{pid() => {reference(), aec_peer:id()}},
    %% The hostnames that failed to resolve and are pending for retries.
    hostnames         :: #{Host::string() =>
                                 %% Host data.
                                 {ResolverTimer :: reference(),
                                  RetryCount :: pos_integer(),
                                  #{ aec_peer:id() =>
                                         %% Peer data.
                                         {SourceAddress :: inet:ip_address()
                                                         | undefined,
                                          aec_peer:info(),
                                          Trusted :: boolean()}}}}
}).

-type peer_pool_name() :: verified | unverified.
-type conn() :: #conn{}.
-type state() :: #state{}.

%=== API FUNCTIONS =============================================================

%--- PEER MANAGMENT FUNCTIONS --------------------------------------------------

%% @doc Adds a trusted peer or a list of trusted peers.
-spec add_trusted(aec_peer:info() | [aec_peer:info()]) -> ok.
add_trusted(PeerInfos) when is_list(PeerInfos) ->
    lists:foreach(fun(I) -> add_trusted(I) end, PeerInfos);
add_trusted(PeerInfo) ->
    async_add_peer(undefined, PeerInfo, true).

%% @doc Adds a normal peer or a list of normal peers.
%% The IP address of the node that gave us this peer must be specified.
-spec add_peers(inet:ip_address(), aec_peer:info() | [aec_peer:info()]) -> ok.
add_peers(SourceAddr, PeerInfos) when is_list(PeerInfos) ->
    lists:foreach(fun(I) -> add_peers(SourceAddr, I) end, PeerInfos);
add_peers(SourceAddr, PeerInfo) ->
    async_add_peer(SourceAddr, PeerInfo, false).

%% @doc Removes a peer; if connected, closes the connection.
%% At the moment also removes peer from the blocked list.
-spec del_peer(aec_peer:id() | aec_peer:info()) -> ok.
del_peer(PeerId) ->
    gen_server:cast(?MODULE, {del_peer, PeerId}).

%% @doc Blocks given peer.
-spec block_peer(aec_peer:info()) -> ok | {error, term()}.
block_peer(PeerInfo) ->
    gen_server:call(?MODULE, {block_peer, PeerInfo}).

%% @doc Unblocks given peer.
-spec unblock_peer(aec_peer:id()) -> ok | {error, term()}.
unblock_peer(PeerId) ->
    gen_server:call(?MODULE, {unblock_peer, PeerId}).

peer_suspect(PeerId) ->
    gen_server:call(?MODULE, {peer_suspect, PeerId}).

%--- GETTERS AND FLAGS FUNCTIONS -----------------------------------------------

%% @doc Checks if given peer is blocked.
%% Erroneous Peers are by definition blocked.
-spec is_blocked(aec_peer:id()) -> boolean().
is_blocked(PeerId) ->
    gen_server:call(?MODULE, {is_blocked, PeerId}).

%% Gives the number of:
%%  * `connections' : Both inbound and outbound connections.
%%  * `inbound' : Inbound connections.
%%  * `outbound' : Outbound connections.
%%  * `peers` : Pooled peers.
%%  * `verified` : Pooled verified peers.
%%  * `unverified` : Pooled unverified peers.
%%  * `standby` : Peers that failed and are in standby for retry.
%%  * `hostnames` : Hostnames pending for resolution to add peers.
-spec count(connections | inbound | outbound | blocked | peers
            | verified | unverified | available | standby | hostnames)
    -> non_neg_integer().
count(Tag)
  when Tag =:= connections; Tag =:= inbound; Tag =:= outbound;
       Tag =:= peers; Tag =:= verified; Tag =:= unverified;
       Tag =:= available; Tag =:= standby; Tag =:= hostnames; Tag =:= blocked
  ->
    gen_server:call(?MODULE, {count, Tag}).

%% @doc Gets the list of peers available to connect to.
%% The result could be very large; use only for debugging/testing.
-spec available_peers() -> [aec_peer:info()].
available_peers() ->
    gen_server:call(?MODULE, {available_peers, both}).

%% @doc Gets the list of peers available to connect to.
%%  * `both': both verified and unverified peers.
%%  * `verified': only available verified peers.
%%  * `unverified': only available unverified peers.
%% The result could be very large; use only for debugging/testing.
-spec available_peers(both | verified | unverified) -> [aec_peer:info()].
available_peers(Tag) when Tag =:= both; Tag =:= verified; Tag =:= unverified ->
    gen_server:call(?MODULE, {available_peers, Tag}).

%% @doc Gets the list of all connected peers.
-spec connected_peers() -> [aec_peer:info()].
connected_peers() ->
    connected_peers(all).

%% @doc Gets the list of connected peers.
%%  * `all': All connected peers.
%%  * `outbound': All peers with outbound connection to.
%%  * `inbound': All peers with inbound connection from.
-spec connected_peers(all | inbound | outbound) -> [aec_peer:info()].
connected_peers(Tag) when Tag =:= all; Tag =:= inbound; Tag =:= outbound ->
    gen_server:call(?MODULE, {connected_peers, Tag}).

%% @doc Gets the list of blocked peers.
-spec blocked_peers() -> [aec_peer:info()].
blocked_peers() ->
    gen_server:call(?MODULE, blocked_peers).

%% @doc Gets up to N random peers.
-spec get_random(all | non_neg_integer()) -> [aec_peer:info()].
get_random(NumberOfPeers) ->
    get_random(NumberOfPeers, undefined).

%% @doc Gets up to N random peers, but none of the peers from the Exclude list.
-spec get_random(all | non_neg_integer(), [aec_peer:id()] | undefined)
    -> [aec_peer:info()].
get_random(N, Exclude)
  when (Exclude =:= undefined) orelse is_list(Exclude),
       (N =:= all) orelse (is_integer(N) andalso N >= 0) ->
    gen_server:call(?MODULE, {get_random, N, Exclude}).


%% @doc Gets up to N random connected peers.
-spec get_random_connected(pos_integer()) -> [aec_peer:info()].
get_random_connected(N) when (is_integer(N) andalso N > 0) ->
    gen_server:call(?MODULE, {get_random_connected, N}).

%% @doc Gets a connection PID from a peer identifier.
-spec get_connection(aec_peer:id()) -> {ok, pid()} | {error, term()}.
get_connection(PeerId) ->
    gen_server:call(?MODULE, {get_connection, PeerId}).

%--- UTILITY FUNCTIONS ---------------------------------------------------------

%% @doc Parses a peer URI to a peer info map.
-spec parse_peer_address(binary() | string())
    -> {ok, aec_peer:info()} | {error, term()}.
parse_peer_address(PeerAddress) ->
    case http_uri:parse(PeerAddress) of
        {ok, {aenode, EncPubKey, Host, Port, _Path, _Query}} ->
            case aeser_api_encoder:safe_decode(peer_pubkey, to_binary(EncPubKey)) of
                {ok, PubKey} ->
                    PeerInfo = aec_peer:info(PubKey, Host, Port),
                    {ok, PeerInfo};
                {error, _} = Error ->
                    Error
            end;
        {error, _Reason} = Error ->
            Error
    end.

%% @doc Encodes peer info map to a peer URI.
-spec encode_peer_address(aec_peer:info()) -> binary().
encode_peer_address(#{ pubkey := PubKey, host := Host, port := Port }) ->
    list_to_binary(["aenode://", aeser_api_encoder:encode(peer_pubkey, PubKey), "@",
                    Host, ":", integer_to_list(Port)]).

%% @doc Obtains the local peer address
-spec local_peer_address() -> binary().
local_peer_address() ->
    {ok, PubKey} = aec_keys:peer_pubkey(),
    LocalExternalPort = aec_connection_sup:ext_sync_port(),
    aec_peers:encode_peer_address(aec_peer:info(PubKey, <<"127.0.0.1">>, LocalExternalPort)).

%--- CONNECTION MANAGMENT FUNCTIONS FOR aec_peer_connection ONLY ---------------

%% @doc Informs that an outbound connection successfully connected.
-spec peer_connected(aec_peer:id(), pid()) -> ok | {error, term()}.
peer_connected(PeerId, PeerCon) ->
    gen_server:call(?MODULE, {peer_connected, PeerId, PeerCon}).

%% @doc Informs that an inbound connection has been accepted.
%% If it returns `temporary' the connection should be closed as soon as
%% the first ping has been responded.
-spec peer_accepted(aec_peer:info(), inet:ip_address(), pid())
    -> permanent | temporary | {error, term()}.
peer_accepted(PeerInfo, Addr, PeerCon) ->
    gen_server:call(?MODULE, {peer_accepted, Addr, PeerInfo, PeerCon}).

-spec peer_alive(aec_peer:id(), pid()) -> ok | {error, term()}.
peer_alive(PeerId, PeerCon) ->
    gen_server:call(?MODULE, {peer_alive, PeerId, PeerCon}).

%% @doc Informs that a connection failed unexpectedly; either when connecting
%% or while already being connected.
-spec connection_failed(aec_peer:id(), pid()) -> ok.
connection_failed(PeerId, PeerCon) ->
    gen_server:call(?MODULE, {connection_failed, PeerId, PeerCon}).

%% @doc Informs that a connection got closed cleanly.
-spec connection_closed(aec_peer:id(), pid()) -> ok.
connection_closed(PeerId, PeerCon) ->
    gen_server:call(?MODULE, {connection_closed, PeerId, PeerCon}).

-spec peer_dead(aec_peer:id(), pid()) -> ok | {error, term()}.
peer_dead(PeerId, PeerCon) ->
    gen_server:call(?MODULE, {peer_dead, PeerId, PeerCon}).

-ifdef(TEST).
all() ->
    gen_server:call(?MODULE, {all_peers, both}).

all(PeerPool) when PeerPool =:= verified
            orelse PeerPool =:= unverified
            orelse PeerPool =:= both ->
    gen_server:call(?MODULE, {all_peers, PeerPool}).
-endif.
%--- UTILITY FUNCTIONS FOR aec_sync ONLY ---------------------------------------

-spec log_ping(aec_peer:id(), ok | error) -> ok | {error, any()}.
log_ping(PeerId, Outcome) ->
    gen_server:cast(?MODULE, {log_ping, Outcome, PeerId, timestamp()}).

%--- Utility functions for extraction of peer id ------------------------------

-spec local_peer_id(state()) -> aec_peer:id().
local_peer_id(#state{ local_peer = LocalPeerInfo }) ->
    aec_peer:id(LocalPeerInfo).

-spec peer_id(conn()) -> aec_peer:id().
peer_id(#conn{ peer = Peer }) ->
    aec_peer:id(Peer).

%--- TESTING FUNCTIONS ---------------------------------------------------------

-ifdef(TEST).

unblock_all() ->
    gen_server:call(?MODULE, unblock_all).

-spec local_peer_info() -> aec_peer:info().
local_peer_info() ->
    {ok, LP} = gen_server:call(?MODULE, local_peer_info),
    LP.

-endif.

%--- BEHAVIOUR gen_server CALLBACKS AND API FUNCTIONS --------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE} ,?MODULE, ok, []).

init(ok) ->
    {ok, PubKey} = aec_keys:peer_pubkey(),
    LocalExternalPort = aec_connection_sup:ext_sync_port(),
    %% since we don't know the external IP, the host is set to localhost.
    %% do not use the host in real life scenario (it is useful for tests,
    %% though)
    LocalPeerInfo =
        aec_peer:info(PubKey, <<"127.0.0.1">>, LocalExternalPort),
    LogTimeout = log_peer_connection_count_interval(),
    erlang:send_after(LogTimeout, self(), log_peer_conn_count_timeout),
    epoch_sync:info("aec_peers started for ~p", [aec_peer:ppp(LocalPeerInfo)]),
    {ok, #state{
        local_peer = LocalPeerInfo,
        pool = aec_peers_pool:new(pool_config()),
        groups = #{},
        conns = #{},
        last_connect_time = timestamp(),
        monitors = #{},
        hostnames = #{},
        blocked = gb_trees:empty(),
        known_sockets = gb_trees:empty()
    }}.

handle_call({is_blocked, PeerId}, _From, State0) ->
    State = maybe_unblock(State0),
    {reply, is_blocked(PeerId, State), update_peer_metrics(State)};
handle_call({block_peer, PeerInfo}, _From, State0) ->
    State = block_peer(PeerInfo, maybe_unblock(State0)),
    {reply, ok, update_peer_metrics(schedule_connect(State))};
handle_call({unblock_peer, PeerId}, _From, State0) ->
    State = unblock_peer(PeerId, maybe_unblock(State0)),
    {reply, ok, update_peer_metrics(schedule_connect(State))};
handle_call(unblock_all, _From, State) -> % Only used in tests
    {reply, ok, update_peer_metrics(schedule_connect(unblock_all(State)))};
handle_call(local_peer_info, _From, #state{local_peer = LocalPeer} = State) -> % Only used in tests
    {reply, {ok, LocalPeer}, State};
handle_call({peer_suspect, PeerId}, _From, State0) ->
    State = on_peer_suspect(PeerId, State0),
    {reply, ok, update_peer_metrics(State)};
handle_call({count, Tag}, _From, State) ->
    {reply, count(Tag, State), State};
handle_call({connected_peers, Tag}, _From, State) ->
    {reply, connected_peers(Tag, State), State};
handle_call({available_peers, Tag}, _From, State) ->
    {reply, available_peers(Tag, State), State};
handle_call(blocked_peers, _From, State) ->
     #state{ blocked = Blocked } = State,
    Result = [ PeerInfo || PeerInfo <- gb_trees:values(Blocked) ],
    {reply, Result, State};
handle_call({get_connection, PeerId}, _From, State) ->
    {reply, conn_pid(PeerId, State), State};
handle_call({get_random, N, Exclude}, _From, State0) ->
    {Subset, State} = pool_random_subset(N, Exclude, State0),
    Result = [ aec_peer:info(P) || {_, P} <- Subset ],
    {reply, Result, State};
handle_call({get_random_connected, N}, _From, State) ->
    {reply, random_connected_peers(N, all, State), State};
handle_call({connection_failed, PeerId, PeerCon}, _From, State0) ->
    State = on_connection_failed(PeerId, PeerCon, State0),
    {reply, ok, update_peer_metrics(schedule_connect(State))};
handle_call({connection_closed, PeerId, PeerCon}, _From, State0) ->
    State = on_connection_closed(PeerId, PeerCon, State0),
    {reply, ok, update_peer_metrics(schedule_connect(State))};
handle_call({peer_dead, PeerId, PeerCon}, _From, State0) ->
    State = on_peer_dead(PeerId, PeerCon, State0),
    {reply, ok, update_peer_metrics(maybe_start_tcp_probe_timers(State))};
handle_call({peer_connected, PeerId, PeerCon}, _From, State0) ->
    {Result, State} = on_peer_connected(PeerId, PeerCon, State0),
    {reply, Result, update_peer_metrics(schedule_connect(State))};
handle_call({peer_accepted, PeerAddr, PeerInfo, PeerCon}, _From, State0) ->
    {Result, State} = on_peer_accepted(PeerAddr, PeerInfo, PeerCon, State0),
    {reply, Result, update_peer_metrics(State)};
handle_call({peer_alive, PeerId, PeerCon}, _From, State0) ->
    {Result, State} = on_peer_alive(PeerId, PeerCon, State0),
    {reply, Result, update_peer_metrics(maybe_start_tcp_probe_timers(State))};
handle_call({all_peers, PeerPool}, _From, State) ->
    #state{pool = Pool} = State,
    Result = [ aec_peer:info(P) || {_, P} <- aec_peers_pool:all(Pool, PeerPool) ],
    {reply, Result, update_peer_metrics(maybe_start_tcp_probe_timers(State))}.

handle_cast({log_ping, Outcome, PeerId, _Time}, State) ->
    update_ping_metrics(Outcome),
    {noreply, on_log_ping(PeerId, Outcome, State)};
handle_cast({add_peer, Peer}, State0) ->
    State = on_add_peer(Peer, State0),
    {noreply, update_peer_metrics(schedule_connect(State))};
handle_cast({del_peer, PeerId}, State0) ->
    State = on_del_peer(PeerId, State0),
    {noreply, update_peer_metrics(schedule_connect(State))};
handle_cast({resolved_hostname, Host, error}, #state{hostnames = HostMap} = State) ->
    case maps:find(Host, HostMap) of
        {ok, {_, RetryCount, PeerMap}} ->
            HostData = {undefined, RetryCount + 1, PeerMap},
            HostMap2 = resolver_schedule(Host, HostData, HostMap),
            {noreply, update_peer_metrics(schedule_connect(State#state{ hostnames = HostMap2 }))};
        _ ->
            %% This hostname is no longer of interest
            {noreply, State}
    end;
handle_cast({resolved_hostname, Host, {ok, Addr}}, #state{hostnames = HostMap} = State) ->
    case maps:find(Host, HostMap) of
        {ok, {_, _, PeerMap}} ->
            HostMap2 = maps:remove(Host, HostMap),
            State2 = State#state{ hostnames = HostMap2 },
            NewState =
                maps:fold(fun
                              (_, {undefined, PeerInfo, IsTrusted}, S) ->
                                  Peer = aec_peer:new(Addr, Addr, PeerInfo, IsTrusted),
                                  on_add_peer(Peer, S);
                              (_, {SourceAddr, PeerInfo, IsTrusted}, S) ->
                                  Peer = aec_peer:new(Addr, SourceAddr, PeerInfo, IsTrusted),
                                  on_add_peer(Peer, S)
                          end, State2, PeerMap),
            {noreply, update_peer_metrics(schedule_connect(NewState))};
        _ ->
            %% This hostname must have been resolved already
            {noreply, State}
    end;
handle_cast({resolve_peer, SourceAddr, PeerInfo, IsTrusted}, State) ->
    {noreply, on_resolve_peer(SourceAddr, PeerInfo, IsTrusted, State)}.

handle_info({timeout, Ref, {ping, PeerId}}, State) ->
    {noreply, on_ping_timeout(PeerId, Ref, State)};
handle_info({timeout, Ref, connect}, State) ->
    {noreply, update_peer_metrics(on_connect_timeout(Ref, State))};
handle_info({timeout, Ref, {tcp_probe, PeerPoolName}}, State) ->
    {noreply, update_peer_metrics(on_tcp_probe_timeout(Ref, PeerPoolName, State))};
handle_info({timeout, Ref, {resolve, Hostname}}, #state{ hostnames = HostMap } = State) ->
    %% Handles timeout of hostname resolution event.
    case maps:find(Hostname, HostMap) of
        {ok, {Ref, _, _}} ->
            async_resolve_host(Hostname);
        _ ->
            ok
    end,
    {noreply, State};
handle_info({'DOWN', Ref, process, Pid, _}, State0) ->
    State = on_process_down(Pid, Ref, State0),
    {noreply, update_peer_metrics(schedule_connect(State))};
handle_info(log_peer_conn_count_timeout, State) ->
    %% Outbound - node initiated connections to other nodes.
    %% Inbound - other nodes initiated connections to this node.
    lager:info("Peer connections outbound: ~p/~p, inbound: ~p/~p, verified ~p, unverified ~p, standby ~p",
               [conn_count(outbound, State), max_outbound(),
                conn_count(inbound, State), max_inbound(),
                count(verified, State),
                count(unverified, State),
                count(standby, State)]),
    LogTimeout = log_peer_connection_count_interval(),
    erlang:send_after(LogTimeout, self(), log_peer_conn_count_timeout),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%=== INTERNAL FUNCTIONS ========================================================

%% Gives a millisecond timestamp.
-spec timestamp() -> non_neg_integer().
timestamp() ->
    erlang:system_time(millisecond).

%% Cancels a timer, if defined.
-spec cancel_timer(undefined | reference()) -> ok.
cancel_timer(undefined) -> ok;
cancel_timer(OldTimer) ->
    try erlang:cancel_timer(OldTimer, [{async, true}, {info, false}])
    catch error:_ -> ok end.

%% Starts a timer.
-spec start_timer(term(), non_neg_integer()) -> reference().
start_timer(Msg, Timeout) ->
    erlang:start_timer(Timeout, self(), Msg).

%% Tells if a given peer is the local peer.
-spec is_local(aec_peer:id(), state()) -> boolean().
is_local(PeerId, State) ->
    PeerId =:= local_peer_id(State).

%% Format the given address or the given peer or connection address.
%% Only supports IPv4 for now.
-spec format_conn_address(conn()) -> binary().
format_conn_address(#conn{ peer = Peer }) ->
    aec_peer:format_address(Peer).

to_binary(Bin) when is_binary(Bin) -> Bin;
to_binary(List) when is_list(List) -> list_to_binary(List).

to_list(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_list(List) when is_list(List) -> List.

safe_max(undefined, Value) -> Value;
safe_max(Value1, Value2) -> max(Value1, Value2).

%--- METRICS FUNCTIONS ---------------------------------------------------------

%% Updates peer metrics.
-spec update_peer_metrics(state()) -> state().
update_peer_metrics(State) ->
    aec_metrics:try_update([ae,epoch,aecore,peers,count],
                           conn_count(both, State)),
    aec_metrics:try_update([ae,epoch,aecore,peers,inbound],
                           conn_count(inbound, State)),
    aec_metrics:try_update([ae,epoch,aecore,peers,outbound],
                           conn_count(outbound, State)),
    aec_metrics:try_update([ae,epoch,aecore,peers,verified],
                           count(verified, State)),
    aec_metrics:try_update([ae,epoch,aecore,peers,unverified],
                           count(unverified, State)),
    aec_metrics:try_update([ae,epoch,aecore,peers,errored],
                           count(standby, State)),
    aec_metrics:try_update([ae,epoch,aecore,peers,blocked],
                           count(blocked, State)),
    State.

%% Updates the ping metric in function of the outcome.
-spec update_ping_metrics(ok | error) -> ok.
update_ping_metrics(Outcome) ->
    MetricName = case Outcome of
        ok    -> [ae,epoch,aecore,peers,ping,success];
        error -> [ae,epoch,aecore,peers,ping,failure]
    end,
    aec_metrics:try_update(MetricName, 1).

%--- API CALLS HELPER FUNCTIONS ------------------------------------------------

%% Adds or update a peer; called in the caller's process.
%% We should not block the caller, so we just spawn to make this happen in
%% the background. If the process silently fails in the background, we
%% will be able to detect that in the logs since we won't be able to
%% connect.
-spec async_add_peer(inet:ip_address() | undefined, aec_peer:info(), boolean()) -> ok.
async_add_peer(SourceAddr0, #{ host := Host} = PeerInfo, IsTrusted) ->
    spawn(fun() ->
             case inet:getaddr(to_list(Host), inet) of
                 {error, nxdomain} ->
                     epoch_sync:debug("Peer ~p - failed to resolve hostname ~p; "
                                     "retrying later", [aec_peer:ppp(PeerInfo), Host]),
                     gen_server:cast(?MODULE, {resolve_peer, SourceAddr0,
                                               PeerInfo, IsTrusted});
                 {ok, Addr} when SourceAddr0 == undefined ->
                     Peer = aec_peer:new(Addr, Addr, PeerInfo, IsTrusted),
                     gen_server:cast(?MODULE, {add_peer, Peer});
                 {ok, Addr} when SourceAddr0 =/= undefined ->
                     Peer = aec_peer:new(Addr, SourceAddr0, PeerInfo, IsTrusted),
                     gen_server:cast(?MODULE, {add_peer, Peer})
             end
          end),
    ok.

async_resolve_host(Host) ->
    spawn(fun() ->
             case inet:getaddr(Host, inet) of
                 {error, nxdomain} ->
                     epoch_sync:info("Failed to resolve hostname ~p", [Host]),
                     gen_server:cast(?MODULE, {resolved_hostname, Host, error});
                 {ok, Addr} ->
                     gen_server:cast(?MODULE, {resolved_hostname, Host, {ok, Addr}})
             end
          end).


-spec count(connections | inbound | outbound | blocked | peers
            | verified | unverified | available | standby | hostnames, state())
    -> non_neg_integer().
count(connections, State) -> conn_count(both, State);
count(Tag, State) when Tag =:= inbound; Tag =:= outbound ->
    conn_count(Tag, State);
count(blocked, State) ->
    #state{ blocked = Blocked } = State,
    gb_trees:size(Blocked);
count(peers, State) ->
    #state{ pool = Pool } = State,
    aec_peers_pool:count(Pool, all, both);
count(available, State) ->
    #state{ pool = Pool } = State,
    aec_peers_pool:count(Pool, available, both);
count(standby, State) ->
    #state{ pool = Pool } = State,
    aec_peers_pool:count(Pool, standby, both);
count(hostnames, State) ->
    #state{ hostnames = Hostnames } = State,
    maps:size(Hostnames);
count(Tag, State) when Tag =:= verified; Tag =:= unverified ->
    #state{ pool = Pool } = State,
    aec_peers_pool:count(Pool, all, Tag).

-spec connected_peers(all | inbound | outbound, state()) -> [aec_peer:info()].
connected_peers(all, #state{ conns = Conns }) ->
    [ aec_peer:info(P) || #conn{ peer = P, state = connected }
                      <- maps:values(Conns) ];
connected_peers(inbound, #state{ conns = Conns }) ->
    [ aec_peer:info(P) || #conn{ peer = P, type = inbound, state = connected }
                      <- maps:values(Conns) ];
connected_peers(outbound, #state{ conns = Conns }) ->
    [ aec_peer:info(P) || #conn{ peer = P, type = outbound, state = connected }
                      <- maps:values(Conns) ].

-spec random_connected_peers(N :: pos_integer(), all | inbound | outbound, state())
    -> [aec_peer:info()].
random_connected_peers(N, Tag, State) ->
    Keyed = [{rand:uniform(), P} || P <- connected_peers(Tag, State)],
    Sorted = lists:keysort(1, Keyed),
    [P || {_, P} <- lists:sublist(Sorted, N)].

-spec available_peers(both | verified | unverified, state()) -> [aec_peer:info()].
available_peers(Tag, #state{ pool = Pool }) ->
    [ aec_peer:info(P) || {_, P} <- aec_peers_pool:available(Pool, Tag) ].

%--- OUTBOUND CONNECTION HANDLING FUNCTIONS ------------------------------------

%% Connects to a random peer if the maximum number of outbound connections has
%% not yet been reached; schedule the next connection timeout if required.
-spec connect(state()) -> state().
connect(State0) ->
    State = cancel_connect_timer(State0),
    case is_outbound_allowed(State) of
        false ->
            %% note that at this moment the timer is canceled and not
            %% restarted. This relies on scheduling a connect once a
            %% connection is dropped
            State;
        true ->
            case pool_random_select(State) of
                {unavailable, State2} ->
                    epoch_sync:debug("No peers available for outbound connection", []),
                    State2;
                {wait, Delay, State2} ->
                    epoch_sync:debug("No peers available for outbound connection, "
                                     "waiting ~b ms", [Delay]),
                    schedule_connect(Delay, State2);
                {selected, Peer, State2} ->
                    schedule_connect(undefined, start_connection(noise, Peer, State2))
            end
    end.

%% Connects to a random peer from verified or the unverified pool to check if
%% the selected peer is alive; schedule the next TCP probe timeout
-spec tcp_probe(peer_pool_name(), state()) -> state().
tcp_probe(PeerPoolName, State0) ->
    State = cancel_tcp_probe_timer(PeerPoolName, State0),
    case is_tcp_probe_allowed(State) of
        false ->
            schedule_tcp_probe(?MAX_TCP_PROBE_INTERVAL, PeerPoolName, State0);
        true ->
            case pool_random_select(PeerPoolName, State) of
                {unavailable, State2} ->
                    epoch_sync:debug("No peers available for TCP probe in ~p",
                                     [PeerPoolName]),
                    schedule_tcp_probe(?MAX_TCP_PROBE_INTERVAL, PeerPoolName, State2);
                {wait, Delay, State2} ->
                    epoch_sync:debug("No peers available for TCP probe in ~p, "
                                    "waiting ~b ms", [PeerPoolName, Delay]),
                    schedule_tcp_probe(Delay, PeerPoolName, State2);
                {selected, Peer, State2} ->
                    schedule_tcp_probe(undefined, PeerPoolName,
                                       start_connection({tcp, PeerPoolName}, Peer, State2))
            end
    end.

%% Connects to given peer if the maximum number of outbound connections has not
%% yet been reached; schedule the next connection timeout if required.
connect(PeerId, State0) ->
    State = cancel_connect_timer(State0),
    case is_outbound_allowed(State) of
        false -> State;
        true ->
            case pool_find(PeerId, State) of
                error ->
                    epoch_sync:debug("Peer ~p - requested to connect to "
                                     "unknown peer", [aec_peer:ppp(PeerId)]),
                    State;
                {ok, Peer} ->
                    State2 = pool_select(PeerId, State),
                    schedule_connect(undefined, start_connection(noise, Peer, State2))
            end
    end.

-spec schedule_connect(state()) -> state().
schedule_connect(State) ->
    schedule_connect(undefined, State).

%% Schedules a connection to a random peer if possible.
%% A minimum delay can be specified to override the standard one.
-spec schedule_connect(undefined | non_neg_integer(), state()) -> state().
schedule_connect(MinDelay, State0) ->
    State = cancel_connect_timer(State0),
    case next_connect_delay(State) of
        infinity -> State;
        NextDelay ->
            Delay = safe_max(MinDelay, NextDelay),
            epoch_sync:debug("Waiting ~b ms for the next connection", [Delay]),
            Ref = start_timer(connect, Delay),
            State#state{ connect_ref = Ref }
    end.

-spec schedule_tcp_probe(undefined | non_neg_integer(), peer_pool_name(), state()) -> state().
schedule_tcp_probe(MinDelay, PeerPoolName, State0) ->
    State = cancel_tcp_probe_timer(PeerPoolName, State0),
    NextDelay = next_tcp_probe_delay(PeerPoolName, State),
    Delay = safe_max(MinDelay, NextDelay),
    epoch_sync:debug("Waiting ~b ms for the next TCP probe in ~p",
                     [Delay, PeerPoolName]),
    Ref = start_timer({tcp_probe, PeerPoolName}, Delay),
    case PeerPoolName of
        verified -> State#state{ tcp_verified_probe_ref = Ref };
        unverified -> State#state{ tcp_unverified_probe_ref = Ref }
    end.

%% Gives the time to wait for the next connect or `infinity' if no more
%% connection should be established for now.
-spec next_connect_delay(state()) -> infinity | non_neg_integer().
next_connect_delay(State) ->
    case is_outbound_allowed(State) of
        false  -> infinity;
        true ->
            case aec_peer_analytics:enabled() of
                false ->
                    #state{ last_connect_time = LastTime, outbound = Outbound } = State,
                    ExpDelay = floor(math:pow(2, Outbound - 1)) * 1000,
                    BoundDelay = min(ExpDelay, ?MAX_CONNECTION_INTERVAL),
                    max(?MIN_CONNECTION_INTERVAL, BoundDelay - (timestamp() - LastTime));
                %% If this is a monitoring node then aggressively connect to peers
                true -> ?MIN_CONNECTION_INTERVAL
            end
    end.

%% Gives the node some time to receive peers and establish outbound connections
%% before it starts TCP probes of verified and unverified peers.
-spec initial_tcp_probe_delay() -> non_neg_integer().
initial_tcp_probe_delay() ->
    floor(ping_interval() * 1.5).

-spec next_tcp_probe_delay(peer_pool_name(), state()) -> non_neg_integer().
next_tcp_probe_delay(PeerPoolName, State) ->
    LastTime =
        case PeerPoolName of
            verified -> State#state.last_tcp_verified_probe_time;
            unverified -> State#state.last_tcp_unverified_probe_time
        end,
    #state{ outbound = Outbound } = State,
    ExpDelay = floor(math:pow(2, Outbound - 1)) * 1000,
    BoundDelay = min(ExpDelay, ?MAX_TCP_PROBE_INTERVAL),
    max(?MIN_TCP_PROBE_INTERVAL, BoundDelay - (timestamp() - LastTime)).

%% Cancel the connect timer if defined.
-spec cancel_connect_timer(state()) -> state().
cancel_connect_timer(State) ->
    #state{ connect_ref = Ref } = State,
    cancel_timer(Ref),
    State#state{ connect_ref = undefined }.

%% Starts TCP probe timer if it hasn't been started yet.
-spec maybe_start_tcp_probe_timers(state()) -> state().
maybe_start_tcp_probe_timers(State) ->
    State1 = maybe_start_tcp_probe_timer(verified, State),
    _State = maybe_start_tcp_probe_timer(unverified, State1).

-spec maybe_start_tcp_probe_timer(peer_pool_name(), state()) -> state().
maybe_start_tcp_probe_timer(verified, #state{ tcp_verified_probe_ref = undefined } = State) ->
    Delay = initial_tcp_probe_delay(),
    RefVerified = start_timer({tcp_probe, verified}, Delay),
    State#state{ last_tcp_verified_probe_time = timestamp(),
                 tcp_verified_probe_ref = RefVerified };
maybe_start_tcp_probe_timer(unverified, #state{ tcp_unverified_probe_ref = undefined } = State) ->
    Delay = initial_tcp_probe_delay(),
    RefUnverified = start_timer({tcp_probe, unverified}, Delay),
    State#state{ last_tcp_unverified_probe_time = timestamp(),
                 tcp_unverified_probe_ref = RefUnverified };
maybe_start_tcp_probe_timer(_PeerPoolName, State) ->
    State.

%% Cancel TCP probe timer if defined.
-spec cancel_tcp_probe_timer(peer_pool_name(), state()) -> state().
cancel_tcp_probe_timer(verified, State) ->
    #state{ tcp_verified_probe_ref = Ref } = State,
    cancel_timer(Ref),
    State#state{ tcp_verified_probe_ref = undefined };
cancel_tcp_probe_timer(unverified, State) ->
    #state{ tcp_unverified_probe_ref = Ref } = State,
    cancel_timer(Ref),
    State#state{ tcp_unverified_probe_ref = undefined }.

%% Starts a connection process for the given peer.
-spec start_connection(noise | {tcp, peer_pool_name()}, aec_peer:peer(), state()) -> state().
start_connection(ConnType0, Peer, State) ->
    #{ pubkey := NodePKey } = State#state.local_peer,
    RemPKey = aec_peer:pubkey(Peer),
    Host = aec_peer:host(Peer),
    Port = aec_peer:port(Peer),
    epoch_sync:debug("Peer ~p - starting Noise connection to ~s",
                     [aec_peer:ppp(Peer), aec_peer:format_address(Peer)]),
    ConnType =
        case ConnType0 of
            noise -> noise;
            {tcp, _PeerPoolName} -> tcp
        end,
    ConnOpts = #{
        conn_type => ConnType,
        pubkey => NodePKey,
        r_pubkey => RemPKey,
        host => Host,
        port => Port
    },
    IsTcpProbe =
        case ConnType0 of
            {tcp, _} -> true;
            noise -> false
        end,
    case aec_peer_connection:connect(ConnOpts) of
        {ok, Pid} ->
            Conn = #conn{
                peer = Peer,
                type = outbound,
                tcp_probe = IsTcpProbe,
                state = connecting,
                pid = Pid
            },
            State2 =
                case ConnType0 of
                    {tcp, verified}  ->
                        State#state{ last_tcp_verified_probe_time = timestamp() };
                    {tcp, unverified}  ->
                        State#state{ last_tcp_unverified_probe_time = timestamp() };
                    noise ->
                        State#state{ last_connect_time = timestamp() }
                end,
            conn_add(Conn, conn_monitor(Conn, State2));
        {error, Reason} ->
            epoch_sync:info("Peer ~p - failed to start connection process: ~p",
                            [aec_peer:ppp(Peer), Reason]),
            State
    end.

%--- RESOLVER HELPER FUNCTIONS -------------------------------------------------

-spec resolver_schedule(string(), {reference() | undefined, pos_integer(),
                        map()}, map()) -> map().
resolver_schedule(Host, {OldRef, RetryCount, PeerMap}, HostMap) ->
    cancel_timer(OldRef),
    case RetryCount > resolve_max_retries() of
        false ->
            NewRef = resolver_start_timer(Host, RetryCount),
            HostData = {NewRef, RetryCount, PeerMap},
            HostMap#{ Host => HostData };
        true ->
            PeerMap2 = maps:filter(fun(_, {_, _, Trusted}) -> Trusted end,
                                   PeerMap),
            case maps:size(PeerMap2) > 0 of
                false -> maps:remove(Host, HostMap);
                true ->
                    NewRef = resolver_start_timer(Host, RetryCount),
                    HostData = {NewRef, RetryCount, PeerMap2},
                    HostMap#{ Host => HostData }
            end
    end.

-spec resolver_start_timer(string(), pos_integer()) -> reference().
resolver_start_timer(Host, RetryCount) ->
    BackoffTable = resolve_backoff_times(),
    BackoffIndex = min(RetryCount, length(BackoffTable)),
    Delay = lists:nth(BackoffIndex, BackoffTable),
    start_timer({resolve, Host}, Delay).

%--- EVENT HANDLING FUNCTIONS --------------------------------------------------

%% Handles a connection failure.
-spec on_connection_failed(aec_peer:id(), pid(), state()) -> state().
on_connection_failed(PeerId, Pid, State) ->
    case conn_take(PeerId, State) of
        {#conn{ pid = Pid } = Conn, State2} ->
            epoch_sync:debug("Peer ~p - connection to ~s failed by process ~p",
                             [aec_peer:ppp(PeerId), format_conn_address(Conn), Pid]),
            pool_release(PeerId, conn_cleanup(Conn, State2));
        {#conn{ pid = OtherPid }, _State2} ->
            epoch_sync:info("Peer ~p - got connection_failed from unexpected "
                            "process ~p; supposed to come from ~p",
                            [aec_peer:ppp(PeerId), Pid, OtherPid]),
            State;
        error ->
            epoch_sync:info("Peer ~p - got connection_failed for unknown "
                            "connection from process ~p", [aec_peer:ppp(PeerId), Pid]),
            State
    end.

%% Handles a connection clean disconnection.
-spec on_connection_closed(aec_peer:id(), pid(), state()) -> state().
on_connection_closed(PeerId, Pid, State) ->
    case conn_take(PeerId, State) of
        {#conn{ pid = Pid, tcp_probe = false } = Conn, State2} ->
            epoch_sync:debug("Peer ~p - Connection ~s closed by ~p",
                             [aec_peer:ppp(PeerId), format_conn_address(Conn), Pid]),
            pool_release(PeerId, conn_cleanup(Conn, State2));
        {#conn{ pid = OtherPid }, _State2} ->
            epoch_sync:info("Peer ~p - got connection_closed from unexpected "
                            "process ~p; supposed to come from ~p",
                            [aec_peer:ppp(PeerId), Pid, OtherPid]),
            State;
        error ->
            epoch_sync:info("Peer ~p - got connection_closed for unknown "
                            "connection from process ~p", [aec_peer:ppp(PeerId), Pid]),
            State
    end.

-spec on_peer_dead(aec_peer:id(), pid(), state()) -> state().
on_peer_dead(PeerId, Pid, State) ->
    case conn_take(PeerId, State) of
        {#conn{ state = connecting, type = outbound, tcp_probe = true,
                pid = Pid } = Conn, State2 } ->
            epoch_sync:debug("Peer ~p - TCP probe failed through process ~p",
                             [aec_peer:ppp(PeerId), Pid]),
            pool_reject(PeerId, conn_cleanup(Conn, State2));
        {#conn{ pid = OtherPid }, _State2} ->
            epoch_sync:info("Peer ~p - got peer_dead from unexpected "
                            "process ~p; supposed to come from ~p",
                            [aec_peer:ppp(PeerId), Pid, OtherPid]),
            State;
        error ->
            epoch_sync:info("Peer ~p - got peer_dead for unknown "
                            "connection from process ~p", [aec_peer:ppp(PeerId), Pid]),
            State
    end.

-spec on_peer_suspect(aec_peer:id(), state()) -> state().
on_peer_suspect(PeerId, State) ->
    case conn_take(PeerId, State) of
        {#conn{} = Conn, State2} ->
            epoch_sync:debug("Peer ~p flagged as suspect", [aec_peer:ppp(PeerId)]),
            pool_reject(PeerId, conn_cleanup(Conn, State2));
        error ->
            epoch_sync:info("Peer ~p flagged as suspect, but no current connection",
                            [aec_peer:ppp(PeerId)]),
            State
    end.

%% Handles outbound connection being successfully estabished.
-spec on_peer_connected(aec_peer:id(), pid(), state())
    -> {ok, state()} | {{error, term()}, state()}.
on_peer_connected(PeerId, Pid, State) ->
    case conn_find(PeerId, State) of
        error ->
            epoch_sync:info("Peer ~p - got peer_connected for unknown peer "
                            "from process ~p", [aec_peer:ppp(PeerId), Pid]),
            {{error, invalid}, State};
        {ok, #conn{ state = connecting, type = outbound, tcp_probe = false,
                    pid = Pid } = Conn} ->
            epoch_sync:debug("Peer ~p - connected to ~s through process ~p",
                             [aec_peer:ppp(PeerId), format_conn_address(Conn), Pid]),
            epoch_sync:debug("Peer ~p - schedule ping", [aec_peer:ppp(PeerId)]),
            aec_sync:schedule_ping(PeerId),
            State2 = pool_verify(PeerId, State),
            {ok, conn_set_connected(PeerId, State2)};
        {ok, #conn{ state = ConnState, type = Type, pid = Pid }} ->
            epoch_sync:info("Peer ~p - got unexpected peer_connected for ~p ~p "
                            "peer from process ~p; rejecting the connection",
                            [aec_peer:ppp(PeerId), ConnState, Type, Pid]),
            State2 = pool_reject(PeerId, conn_del(PeerId, State)),
            {{error, invalid}, State2};
        {ok, #conn{ pid = OtherPid }} ->
            epoch_sync:info("Peer ~p - got peer_connected from unexpected "
                            "process ~p; supposed to come from ~p; "
                            "rejecting the connection",
                            [aec_peer:ppp(PeerId), Pid, OtherPid]),
            {{error, already_connected}, State}
    end.

%% Handles inbound connection being accepted.
%% Validates it is not local or blocked.
%% Supports not trusted peer address changing.
-spec on_peer_accepted(inet:ip_address(), aec_peer:info(), pid(), state())
    -> {ok, state()} | {temporary, state()} | {{error, term()}, state()}.
on_peer_accepted(PeerAddr, PeerInfo, Pid, State0) ->
    State = maybe_unblock(State0),
    Port = aec_peer:port(PeerInfo),
    PeerId  = aec_peer:id(PeerInfo),
    case is_local(PeerId, State) orelse is_blocked(PeerId, State) of
        true ->
            epoch_sync:debug("Peer ~p - will not be accepted; local or blocked",
                             [aec_peer:ppp(PeerId)]),
            {{error, blocked}, State};
        false ->
            case pool_find(PeerId, State) of
                error ->
                    % New unknown peer.
                    Peer = aec_peer:new(PeerAddr, PeerAddr, PeerInfo, false),
                    new_inbound_resolve_conflicts(Peer, Pid, State);
                {ok, FoundPeer} ->
                    ExpectedSocket = aec_peer:socket(PeerAddr, Port),
                    FoundPSocket = aec_peer:socket(FoundPeer),
                    Trusted = aec_peer:is_trusted(FoundPeer),
                    case ExpectedSocket =:= FoundPSocket of
                        true ->
                            % Known peer.
                            FoundPeer1 = aec_peer:set_source(FoundPeer, PeerAddr),
                            new_inbound_resolve_conflicts(FoundPeer1, Pid, State);
                        false when not Trusted ->
                            % Peer's address changed and it is not trusted;
                            % deleting the old peer and its connections.
                            NewPeer = aec_peer:new(PeerAddr, PeerAddr, PeerInfo, false),
                            epoch_sync:info("Peer ~p - peer address change from ~s "
                                            "to ~s", [aec_peer:ppp(PeerId),
                                            aec_peer:format_address(FoundPeer),
                                            aec_peer:format_address(NewPeer)]),
                            State2 = conn_del(PeerId, State),
                            State3 = pool_delete(PeerId, pool_release(PeerId, State2)),
                            new_inbound_resolve_conflicts(NewPeer, Pid, State3);
                        false ->
                            % Trusted peers' address is not allowed to change.
                            epoch_sync:info("Peer ~p - trusted peer address changed "
                                            "from ~s to ~s; rejecting connection",
                                            [aec_peer:ppp(PeerId),
                                             aec_peer:format_address(FoundPeer),
                                             aec_peer:format_address({PeerAddr, Port})]),
                            {{error, trusted_address_changed}, State}
                    end
            end
    end.

-spec on_peer_alive(aec_peer:id(), pid(), state())
    -> {ok, state()} | {{error, term()}, state()}.
on_peer_alive(PeerId, Pid, State) ->
    case conn_take(PeerId, State) of
        {#conn{ state = connecting, type = outbound, tcp_probe = true,
                pid = Pid } = Conn, State2} ->
            epoch_sync:info("Peer ~p - TCP probe successful through process ~p",
                            [aec_peer:ppp(PeerId), Pid]),
            State3 = conn_cleanup(Conn, State2),
            {ok, pool_verify(PeerId, pool_release(PeerId, State3))};
        {#conn{ state = ConnState, type = Type, pid = Pid2 } = Conn, State2} ->
            epoch_sync:info("Peer ~p - got unexpected peer_alive for ~p ~p "
                            "peer from process ~p; rejecting the connection",
                            [aec_peer:ppp(PeerId), ConnState, Type, Pid2]),
            {{error, invalid}, pool_reject(PeerId, conn_cleanup(Conn, State2))};
        error ->
            epoch_sync:info("Peer ~p - got peer_alive for unknown peer "
                            "from process ~p", [aec_peer:ppp(PeerId), Pid]),
            {{error, invalid}, State}
    end.

%% Adds a new inbound connection if not conflicting with an existing one.
-spec new_inbound_resolve_conflicts(aec_peer:peer(), pid(), state())
    -> {ok, state()} | {temporary, state()} | {{error, term()}, state()}.
new_inbound_resolve_conflicts(Peer, Pid, State) ->
    LocalId = local_peer_id(State),
    PeerId  = aec_peer:id(Peer),
    Conn = #conn{ peer = Peer, type = inbound, state = connected, pid = Pid },
    WouldBeTemporary = not is_inbound_allowed(State),
    case conn_find(PeerId, State) of
        error ->
            % No connection for this peer; add a new one.
            new_inbound_accepted(Conn, State);
        {ok, #conn{ pid = OldPid }}
          when Pid =/= OldPid, LocalId > PeerId, WouldBeTemporary ->
            %% We would have slashed the outbound connection but that
            %% would make the inbound connetion temporary.
            epoch_sync:debug("Peer ~p - rejecting second connection from "
                             "process ~p because it would be temporary; keep "
                             "using the old one from process ~p",
                             [aec_peer:ppp(PeerId), Pid, OldPid]),
            {{error, already_connected}, State};
        {ok, #conn{ state = connected, pid = OldPid }}
          when Pid =/= OldPid, LocalId > PeerId ->
            %% We slash the accepted connection and use the old one.
            epoch_sync:debug("Peer ~p - rejecting second connection from "
                             "process ~p due to LPK > PK; keep using the old "
                             "one from process ~p", [aec_peer:ppp(PeerId), Pid, OldPid]),
            {{error, already_connected}, State};
        {ok, #conn{ state = ConnState, type = Type, pid = OldPid } = OldConn}
          when Pid =/= OldPid ->
            % We slash the old one and keep the new one.
            epoch_sync:debug("Peer ~p - closing ~p ~p connection ~s from "
                             "process ~p", [aec_peer:ppp(PeerId), ConnState, Type,
                             format_conn_address(OldConn), OldPid]),
            State2 = conn_del(PeerId, pool_reject(PeerId, State)),
            new_inbound_accepted(Conn, State2)
    end.

%% Adds the new accepted connection and updates the pool.
-spec new_inbound_accepted(conn(), state())
    -> {ok, state()} | {temporary, state()} | {{error, term()}, state()}.
new_inbound_accepted(Conn, State) ->
    #conn{ peer = Peer, pid = Pid } = Conn,
    case pool_upversel(Peer, State) of
        {ok, State2} ->
            InboundKind = new_inbound_kind(State2),
            epoch_sync:debug("Peer ~p - ~p inbound connection ~s accepted from "
                             "process ~p", [aec_peer:ppp(Peer), InboundKind,
                             format_conn_address(Conn), Pid]),
            Conn2 = Conn#conn{ kind = InboundKind, tcp_probe = false },
            State3 = conn_add(Conn2, State2),
            {InboundKind, conn_monitor(Conn2, State3)};
        Error -> Error
    end.

%% Returns if a new inbound connection should be a temporary one used
%% only for the first gossip ping.
-spec new_inbound_kind(state()) -> permanent | temporary.
new_inbound_kind(State) ->
    case is_inbound_allowed(State) of
        true -> permanent;
        false -> temporary
    end.

%% Handles ping logging event.
-spec on_log_ping(aec_peer:id(), ok | error, state()) -> state().
on_log_ping(PeerId, Outcome, State) ->
    case conn_find(PeerId, State) of
        error ->
            epoch_sync:debug("Peer ~p - got ping event ~p for unknown "
                             "peer connection", [aec_peer:ppp(PeerId), Outcome]),
            State;
        {ok, #conn{ state = connected }} ->
            %% TODO: if a ping keeps failing we should do something?!
            conn_schedule_ping(PeerId, ping_interval(), State);
        {ok, _Conn} ->
            epoch_sync:debug("Peer ~p - got ping event ~p for disconnected "
                             "peer; no more ping scheduled",
                             [aec_peer:ppp(PeerId), Outcome]),
            State
    end.

%% Handles peer hostname resolution event.
-spec on_resolve_peer(inet:ip_address() | undefined, aec_peer:info(),
                      boolean(), state()) ->
    state().
on_resolve_peer(SourceAddr, PeerInfo, IsTrusted, State) ->
    #state{ hostnames = HostMap } = State,
    case maps:size(HostMap) < ?DEFAULT_RESOLVE_MAX_SIZE of
        false -> State;
        true ->
            #{ host := BinHost } = PeerInfo,
            Host = to_list(BinHost),
            PeerId = aec_peer:id(PeerInfo),
            PeerData = {SourceAddr, PeerInfo, IsTrusted},
            HostData = case maps:find(Host, HostMap) of
                error ->
                    {undefined, 1, #{ PeerId => PeerData }};
                {ok, {Ref, RetryCount, Map}} ->
                    {Ref, RetryCount, Map#{ PeerId => PeerData }}
            end,
            HostMap2 = resolver_schedule(Host, HostData, HostMap),
            State#state{ hostnames = HostMap2 }
    end.


%% Handles event adding a new peer to the pool.
-spec on_add_peer(aec_peer:peer(), state()) -> state().
on_add_peer(Peer, State0) ->
    State = maybe_unblock(State0),
    PeerId = aec_peer:id(Peer),
    SourceAddr = aec_peer:source(Peer),
    LogMismatch =
        fun(OtherPeer) ->
            epoch_sync:info("Peer ~p - ignoring peer address changed "
                            "from ~s to ~s by ~s", [aec_peer:ppp(PeerId),
                            aec_peer:format_address(OtherPeer),
                            aec_peer:format_address(Peer),
                            aec_peer:format_address(SourceAddr)])
        end,
    case is_local(PeerId, State) orelse is_blocked(PeerId, State) of
        true ->
            epoch_sync:debug("Peer ~p - will not be added; local or blocked",
                             [aec_peer:ppp(PeerId)]),
            State;
        false ->
            PeerSocket = aec_peer:socket(Peer),
            case {pool_find_by_socket(PeerSocket, State),
                  pool_find(PeerId, State)} of
                {error, error}  -> %% unknown peer and unknown address and port
                    %% The TCP probe timer is started with addition of the firts peer.
                    State2 = maybe_start_tcp_probe_timers(State),
                    add_peer(Peer, State2);
                {{ok, PeerId}, {ok, Peer2}} ->
                    case aec_peer:id(Peer2) of
                        PeerId -> %% same peer id and same IP and port
                            % Only update gossip time and source.
                            {_, State2} = pool_update(aec_peer:set_source(Peer2, SourceAddr),State),
                            State2;
                        _ ->
                            LogMismatch(Peer2),
                            State
                    end;
                {_, {ok, Peer2}} ->
                    LogMismatch(Peer2),
                    State;
                {{ok, OtherPeerPubkey}, error} ->
                    epoch_sync:info("Peer ~p with address ~p - ignoring peer pubkey changed "
                                    "from ~s to ~s by ~s", [aec_peer:ppp(PeerId),
                                    aec_peer:format_address(Peer),
                                    aec_peer:ppp(PeerId),
                                    aec_peer:ppp(OtherPeerPubkey),
                                    aec_peer:format_address(SourceAddr)]),
                    State
            end
    end.

%% Adds a NEW peer to the pool and connect to it if it is trusted.
-spec add_peer(aec_peer:peer(), state()) -> state().
add_peer(Peer, State) ->
    case pool_update(Peer, State) of
        {ignored, State2} -> State2;
        {_, State2} ->
            IsTrusted = aec_peer:is_trusted(Peer),
            case IsTrusted of
                true ->
                    PeerId = aec_peer:id(Peer),
                    epoch_sync:debug("Peer ~p - adding trusted peer ~s",
                                     [aec_peer:ppp(PeerId), aec_peer:format_address(Peer)]),
                    connect(PeerId, State2);
                false ->
                    SourceAddr = aec_peer:source(Peer),
                    epoch_sync:debug("Peer ~p - adding peer ~s given by ~s",
                                     [aec_peer:ppp(Peer), aec_peer:format_address(Peer),
                                      aec_peer:format_address(SourceAddr)]),
                    State2
            end
    end.

%% Handles event removing a peer.
-spec on_del_peer(aec_peer:id(), state()) -> state().
on_del_peer(PeerId, State) ->
    case pool_find(PeerId, State) of
        error -> State;
        {ok, _Peer} ->
            epoch_sync:debug("Peer ~p - deleting peer", [aec_peer:ppp(PeerId)]),
            pool_delete(PeerId, pool_release(PeerId, conn_del(PeerId, State)))
    end.

%% Handles ping timeout event.
-spec on_ping_timeout(aec_peer:id(), reference(), state()) -> state().
on_ping_timeout(PeerId, TimeoutRef, State) ->
    case conn_find(PeerId, State) of
        error ->
            epoch_sync:info("Peer ~p - ping timeout for unknown connection",
                            [aec_peer:ppp(PeerId)]),
            State;
        {ok, #conn{ ping = TimeoutRef }} ->
            epoch_sync:debug("Peer ~p - schedule ping", [aec_peer:ppp(PeerId)]),
            aec_sync:schedule_ping(PeerId),
            conn_reset_ping(PeerId, State);
        {ok, _Conn} ->
            epoch_sync:info("Peer ~p - ping timeout with invalid reference",
                             [aec_peer:ppp(PeerId)]),
            State
    end.

%% handles connect timeout event.
-spec on_connect_timeout(reference(), state()) -> state().
on_connect_timeout(Ref, #state{ connect_ref = Ref } = State) ->
    connect(State#state{ connect_ref = undefined });
on_connect_timeout(_Ref, State) ->
    epoch_sync:info("Connect timeout with invalid reference", []),
    State.

-spec on_tcp_probe_timeout(reference(), peer_pool_name(), state()) -> state().
on_tcp_probe_timeout(Ref, verified, #state{ tcp_verified_probe_ref = Ref } = State) ->
    tcp_probe(verified, State#state{ tcp_verified_probe_ref = undefined });
on_tcp_probe_timeout(Ref, unverified, #state{ tcp_unverified_probe_ref = Ref } = State) ->
    tcp_probe(unverified, State#state{ tcp_unverified_probe_ref = undefined });
on_tcp_probe_timeout(_Ref, PeerPoolName, State) ->
    epoch_sync:info("TCP probe timeout with invalid reference in ~p",
                    [PeerPoolName]),
    State.

%% Handles ping timeout event.
-spec on_process_down(pid(), reference(), state()) -> state().
on_process_down(Pid, MonitorRef, State) ->
    case conn_take_monitor(Pid, State) of
        {PeerId, MonitorRef, State2} ->
            case conn_take(PeerId, State2) of
                error ->
                    epoch_sync:info("Peer ~p - process down for unknwon "
                                    "connection", [aec_peer:ppp(PeerId)]),
                    State2;
                {Conn, State3} ->
                    pool_reject(PeerId, conn_cleanup(Conn, State3))
            end;
        {PeerId, _OtherMonitorRef, _State2} ->
            epoch_sync:info("Peer ~p - process down with invalid reference",
                             [aec_peer:ppp(PeerId)]),
            State;
        error ->
            epoch_sync:info("Unknown process down message for process ~p",
                            [Pid]),
            State
    end.

%--- ADDRESS GROUP FUNCTIONS ---------------------------------------------------

%% Adds given connection to the address group lookup table.
%% Only tracks outbound connection.
group_add(#conn{ peer = Peer, type = outbound, tcp_probe = false }, State) ->
    #state{ groups = Groups } = State,
    Addr = aec_peer:ip(Peer),
    PeerId = aec_peer:id(Peer),
    Group = aec_peers_pool:address_group(Addr),
    Peers = maps:get(Group, Groups, #{}),
    State#state{ groups = Groups#{ Group => Peers#{ PeerId => true } } };
group_add(_Conn, State) ->
    State.

%% Deletes given connection from the address group lookup table.
%% Only tracks outbound connection.
group_del(#conn{ peer = Peer, type = outbound, tcp_probe = false }, State) ->
    #state{ groups = Groups } = State,
    Addr = aec_peer:ip(Peer),
    PeerId = aec_peer:id(Peer),
    Group = aec_peers_pool:address_group(Addr),
    Peers = maps:get(Group, Groups, #{}),
    Peers2 = maps:remove(PeerId, Peers),
    case maps:size(Peers2) of
        0 -> State#state{ groups = maps:remove(Group, Groups) };
        _ -> State#state{ groups = Groups#{ Group => Peers2 } }
    end;
group_del(_Conn, State) ->
    State.

%--- POOL MANAGMENT FUNCTIONS --------------------------------------------------

%% Logs the pooling changes of a peer.
-spec pool_log_changes(aec_peer:id(), undefined | unverified | verified,
                      undefined | verified | unverified | ignored) -> ok.
pool_log_changes(_Id, Same, Same) -> ok;
pool_log_changes(Id, undefined, New)
  when New =:= undefined; New =:= ignored ->
    epoch_sync:debug("Peer ~p - failed to add peer to the pool", [aec_peer:ppp(Id)]);
pool_log_changes(Id, Old, New)
  when New =:= undefined; New =:= ignored ->
    epoch_sync:debug("Peer ~p - peer removed from ~p pool", [aec_peer:ppp(Id), Old]);
pool_log_changes(Id, undefined, New) ->
    epoch_sync:debug("Peer ~p - peer added to ~p pool", [aec_peer:ppp(Id), New]);
pool_log_changes(Id, Old, New) ->
    epoch_sync:debug("Peer ~p - peer moved from ~p pool to ~p pool",
                     [aec_peer:ppp(Id), Old, New]).

%% Tries to add given peer to the pool.
-spec pool_update(aec_peer:peer(), state())
    -> {ignored | verified | unverified, state()}.
pool_update(Peer, State) ->
    #state{ pool = Pool
          , known_sockets = KnownSockets} = State,
    PeerId = aec_peer:id(Peer),
    Now = timestamp(),
    {OldPoolName, _} = aec_peers_pool:peer_state(Pool, PeerId),
    {NewPoolName, Pool2} = aec_peers_pool:update(Pool, Now, Peer),
    pool_log_changes(PeerId, OldPoolName, NewPoolName),

    Socket = aec_peer:socket(Peer),
    {NewPoolName, State#state{ pool = Pool2
                             , known_sockets = gb_trees:enter(Socket,
                                                              PeerId,
                                                              KnownSockets)}}.

-spec pool_random_select(state())
    -> {selected, aec_peer:peer(), state()}
     | {wait, non_neg_integer(), state()}
     | {unavailable, state()}.
pool_random_select(State) ->
    pool_random_select(both, State).

%% Select a random peer optionally excluding the ones the node already have
%% an outbound connection from the same address group.
-spec pool_random_select(aec_peers_pool:select_target(), state())
    -> {selected, aec_peer:peer(), state()}
     | {wait, non_neg_integer(), state()}
     | {unavailable, state()}.
pool_random_select(Target, State) ->
    #state{ pool = Pool } = State,
    Now = timestamp(),
    FilterFun = pool_select_filter_fun(State),
    case aec_peers_pool:random_select(Pool, Now, Target, FilterFun) of
        {selected, {_, Peer}, Pool2} ->
            {selected, Peer, State#state{ pool = Pool2 }};
        {wait, Delay, Pool2} ->
            {wait, Delay, State#state{ pool = Pool2 }};
        {unavailable, Pool2} ->
            {unavailable, State#state{ pool = Pool2 }}
    end.

%% Select given peer so we will not try connecting to it.
-spec pool_select(aec_peer:id(), state()) -> state().
pool_select(PeerId, State) ->
    #state{ pool = Pool } = State,
    Now = timestamp(),
    Pool2 = aec_peers_pool:select(Pool, Now, PeerId),
    State#state{ pool = Pool2 }.

%% Trys to move given peer to the verified pool.
-spec pool_verify(aec_peer:id(), state()) -> state().
pool_verify(PeerId, State) ->
    #state{ pool = Pool } = State,
    Now = timestamp(),
    {OldPoolName, _} = aec_peers_pool:peer_state(Pool, PeerId),
    {NewPoolName, Pool2} = aec_peers_pool:verify(Pool, Now, PeerId),
    pool_log_changes(PeerId, OldPoolName, NewPoolName),
    State#state{ pool = Pool2 }.

%% Updates, verify and select given peer.
%% `update' to be sure the peer is pooled, `verify' to move it to the
%% verified pool because it is connected, and `select' so we will not try
%% to connect to it later on.
-spec pool_upversel(aec_peer:peer(), state())
    -> {ok, state()} | {{error, term()}, state()}.
pool_upversel(Peer, State) ->
    #state{ pool = Pool } = State,
    PeerId = aec_peer:id(Peer),
    Now = timestamp(),
    {OldPoolName, _} = aec_peers_pool:peer_state(Pool, PeerId),
    case aec_peers_pool:update(Pool, Now, Peer) of
        {unverified, Pool2} ->
            {NewPoolName, Pool3} = aec_peers_pool:verify(Pool2, Now, PeerId),
            Pool4 = aec_peers_pool:select(Pool3, Now, PeerId),
            pool_log_changes(PeerId, OldPoolName, NewPoolName),
            {ok, State#state{ pool = Pool4 }};
        {verified, Pool2} ->
            Pool3 = aec_peers_pool:select(Pool2, Now, PeerId),
            pool_log_changes(PeerId, OldPoolName, verified),
            {ok, State#state{ pool = Pool3 }};
        {ignored, Pool2} ->
            pool_log_changes(PeerId, OldPoolName, ignored),
            {{error, ignored}, State#state{ pool = Pool2 }}
    end.

%% Rejects a select peer.
-spec pool_reject(aec_peer:id(), state()) -> state().
pool_reject(PeerId, State) ->
    #state{ pool = Pool } = State,
    Now = timestamp(),
    Pool2 = aec_peers_pool:reject(Pool, Now, PeerId),
    State#state{ pool = Pool2 }.

%% Releases a selected peer.
-spec pool_release(aec_peer:id(), state()) -> state().
pool_release(PeerId, State) ->
    #state{ pool = Pool } = State,
    Now = timestamp(),
    Pool2 = aec_peers_pool:release(Pool, Now, PeerId),
    State#state{ pool = Pool2 }.

%% Removes a peer from the pool and disconnect
-spec pool_delete(aec_peer:id(), state()) -> state().
pool_delete(PeerId, State0) when is_binary(PeerId) ->
    #state{ pool = Pool
          , known_sockets = KnownSockets } = State0,
    {OldPoolName, _} = aec_peers_pool:peer_state(Pool, PeerId),
    Pool2 = aec_peers_pool:delete(Pool, PeerId),
    pool_log_changes(PeerId, OldPoolName, undefined),
    State1 = State0#state{pool = Pool2},
    case pool_find(PeerId, State0) of
        error -> State1;
        {ok, Peer} ->
            PeerSocket = aec_peer:socket(Peer),
            State1#state{known_sockets = gb_trees:delete_any(PeerSocket,
                                                             KnownSockets)}
    end.

%% Gets a peer record from the pool.
-spec pool_find(aec_peer:id(), state()) -> error | {ok, aec_peer:peer()}.
pool_find(PeerId, State) when is_binary(PeerId) ->
    #state{ pool = Pool } = State,
    aec_peers_pool:find(Pool, PeerId).

%% Gets a peer record from the pool by its socket
-spec pool_find_by_socket(aec_peer:socket(), state()) -> error | {ok, aec_peer:id()}.
pool_find_by_socket(PeerSocket, State) ->
    #state{ known_sockets = KnownSockets} = State,
    case gb_trees:lookup(PeerSocket, KnownSockets) of
        none -> error; %% not found
        {value, PeerId} -> {ok, PeerId}
    end.

%% Gets a random subset of pooled peers.
-spec pool_random_subset(non_neg_integer(), [aec_peer:id()] | undefined, state())
    -> {[{aec_peer:id(), aec_peer:peer()}], state()}.
pool_random_subset(N, Exclude, State) ->
    #state{ pool = Pool } = State,
    FilterFun = pool_subset_filter_fun(Exclude),
    {Result, Pool2} = aec_peers_pool:random_subset(Pool, N, FilterFun),
    {Result, State#state{ pool = Pool2 }}.

%% Returns the filter function to be used when getting a random subset.
-spec pool_subset_filter_fun(undefined | [aec_peer:id()])
    -> undefined | aec_peers_pool:filter_fun().
pool_subset_filter_fun(undefined) -> undefined;
pool_subset_filter_fun(Exclude) ->
    fun(PeerId, _Peer) -> not lists:member(PeerId, Exclude) end.

%% Returns the filter function to be used when selecting a random peer.
-spec pool_select_filter_fun(state())
    -> undefined | aec_peers_pool:filter_fun().
pool_select_filter_fun(State) ->
    case single_outbound_per_group() of
        false -> undefined;
        true ->
            #state{ groups = Groups } = State,
            fun(_PeerId, Peer) ->
                Addr = aec_peer:ip(Peer),
                Group = aec_peers_pool:address_group(Addr),
                not  maps:is_key(Group, Groups)
            end
    end.

%--- PEER BLOCKING FUNCTIONS ---------------------------------------------------

%% Tells if a peer is blocked.
-spec is_blocked(aec_peer:id(), state()) -> boolean().
is_blocked(PeerId, State) ->
    #state{ blocked = Blocked } = State,
    gb_trees:is_defined(PeerId, Blocked).

%% Blocks given peer.
-spec block_peer(aec_peer:info(), state()) -> state().
block_peer(PeerInfo, State) ->
    #state{ blocked = Blocked } = State,
    PeerId = aec_peer:id(PeerInfo),
    case pool_find(PeerId, State) of
        {ok, Peer} ->
            IsTrusted = aec_peer:is_trusted(Peer),
            case IsTrusted of
                true ->
                    State;
                false ->
                    aec_events:publish(peers, {blocked, PeerId}),
                    Blocked2 = add_blocked(PeerId, aec_peer:info(Peer), Blocked),
                    State2 = State#state{ blocked = Blocked2 },
                    pool_delete(PeerId, pool_release(PeerId, conn_del(PeerId, State2)))
              end;
        error ->
            aec_events:publish(peers, {blocked, PeerId}),
            Blocked2 = add_blocked(PeerId, PeerInfo, Blocked),
            State#state{ blocked = Blocked2 }
    end.

%% Unblocks given peer.
-spec unblock_peer(aec_peer:id(), state()) -> state().
unblock_peer(PeerId, State) ->
    #state{ blocked = Blocked } = State,
    case is_blocked(PeerId, State) of
        false -> State;
        true ->
            aec_events:publish(peers, {unblocked, PeerId}),
            State#state{ blocked = del_blocked(PeerId, Blocked) }
    end.

add_blocked(PeerId, PeerInfo, Blocked) ->
    gb_trees:enter(PeerId, PeerInfo, Blocked).

del_blocked(PeerId, Blocked) ->
    gb_trees:delete_any(PeerId, Blocked).

%% Unblocks peers if required by locking policy.
%% The unblocking is currently done by batch, not by peer; this could result
%% in a blocked peer to be unblocked right away.
-spec maybe_unblock(state()) -> state().
maybe_unblock(State = #state{ next_unblock = 0 }) ->
    State#state{ next_unblock = next_unblock_time() };
maybe_unblock(State = #state{ next_unblock = NextT }) ->
    case timestamp() > NextT of
        false -> State;
        true  -> unblock_all(State)
    end.

%% Unblocks all blocked peers.
-spec unblock_all(state()) -> state().
unblock_all(State) ->
    State#state{
        blocked = gb_trees:empty(),
        next_unblock = next_unblock_time()
    }.

%% Gives the next time blocked peers should be reset.
-spec next_unblock_time() -> pos_integer().
next_unblock_time() ->
    timestamp() + unblock_interval().

%--- CONNECTIONS FUNCTIONS -----------------------------------------------------

%% Tells if a new outbound connection is allowed.
-spec is_outbound_allowed(state()) -> boolean().
is_outbound_allowed(State) ->
    case {max_outbound(), conn_count(outbound, State)} of
        {infinity, _} -> true;
        {Max, Count} -> Max > Count
    end.

%% Tells if a new tcp probe is allowed.
-spec is_tcp_probe_allowed(state()) -> boolean().
is_tcp_probe_allowed(State) ->
    case {max_tcp_probes(), State#state.tcp_probes} of
        {infinity, _} -> true;
        {Max, Count} -> Max > Count
    end.

%% Tells if a new inbound connection is allowed.
-spec is_inbound_allowed(state()) -> boolean().
is_inbound_allowed(State) ->
    case {max_inbound(), conn_count(inbound, State)} of
        {infinity, _} -> true;
        {Max, Count} -> Max > Count
    end.

%% Gives the current number of connection, both connecting and connected.
-spec conn_count(inbound | outbound | both, state()) -> non_neg_integer().
conn_count(both, State) ->
    #state{ inbound = Inbound, outbound = Outbound } = State,
    Inbound + Outbound;
conn_count(inbound, State) ->
    #state{ inbound = Inbound } = State,
    Inbound;
conn_count(outbound, State) ->
    #state{ outbound = Outbound } = State,
    Outbound.

%% Gives the connection record for given peer identifier.
-spec conn_find(aec_peer:id(), state()) -> {ok, conn()} | error.
conn_find(PeerId, State) ->
    #state{ conns = Conns } = State,
    maps:find(PeerId, Conns).

%% Takes the connection record for given peer identifier, removing it.
%% Updates the connection counters but doesn't cancel timers and monitors;
%% this is so the function has no side-effects.
-spec conn_take(aec_peer:id(), state()) -> {conn(), state()} | error.
conn_take(PeerId, State) ->
    #state{ conns = Conns,
            inbound = Inbound,
            outbound = Outbound,
            tcp_probes = TcpProbes } = State,
    case maps:take(PeerId, Conns) of
        error -> error;
        {#conn{ type = inbound, tcp_probe = false } = Conn, Conns2} ->
            ?assert(Inbound > 0),
            {Conn, State#state{ conns = Conns2, inbound = Inbound - 1 }};
        {#conn{ type = outbound, tcp_probe = false } = Conn, Conns2} ->
            ?assert(Outbound > 0),
            {Conn, State#state{ conns = Conns2, outbound = Outbound - 1 }};
        {#conn{tcp_probe = true} = Conn, Conns2} ->
            {Conn, State#state{ conns = Conns2, tcp_probes = TcpProbes - 1}}
    end.

%% Add a new connection.
-spec conn_add(conn(), state()) -> state().
conn_add(Conn, State0) ->
    State = group_add(Conn, State0),
    #state{ conns = Conns,
            inbound = Inbound,
            outbound = Outbound,
            tcp_probes = TcpProbes } = State0,
    #conn{ peer = Peer } = Conn,
    PeerId = aec_peer:id(Peer),
    ?assertNot(maps:is_key(PeerId, Conns)),
    Conns2 = Conns#{ PeerId => Conn },
    case Conn of
        #conn{ type = inbound, tcp_probe = false } ->
            State#state{ conns = Conns2, inbound = Inbound + 1 };
        #conn{ type = outbound, tcp_probe = false } ->
            State#state{ conns = Conns2, outbound = Outbound + 1 };
        #conn{ tcp_probe = true } ->
            State#state{ conns = Conns2, tcp_probes = TcpProbes + 1 }
    end.

%% Set connections state.
-spec conn_set_connected(aec_peer:id(), state()) -> state().
conn_set_connected(PeerId, #state{ conns = Conns } = State) ->
    #{ PeerId := Conn } = Conns,
    Conn2 = Conn#conn{ state = connected },
    State#state{ conns = Conns#{ PeerId := Conn2} }.

%% Deletes a connection.
%% Cancels timers and monitors; Notify the connection process.
-spec conn_del(aec_peer:id(), state()) -> state().
conn_del(PeerId, State) ->
    case conn_take(PeerId, State) of
        error -> State;
        {#conn{ pid = Pid } = Conn, State2} ->
            aec_peer_connection:disconnect(Pid),
            conn_cleanup(Conn, State2)
    end.

%% Gives the connection process for given peer identifier.
-spec conn_pid(aec_peer:id(), state()) -> {ok, pid()} | {error, term()}.
conn_pid(PeerId, State) ->
    case conn_find(PeerId, State) of
        {ok, #conn{ state = connected, pid = Pid }} -> {ok, Pid};
        {ok, #conn{ state = connecting }} -> {error, connecting};
        error -> {error, no_connection}
    end.

-spec conn_schedule_ping(aec_peer:id(), non_neg_integer(), state()) -> state().
conn_schedule_ping(PeerId, Timeout, #state{ conns = Conns } = State) ->
    #{ PeerId := Conn } = Conns,
    #conn{ ping = OldRef } = Conn,
    PeerId = peer_id(Conn),
    cancel_timer(OldRef),
    epoch_sync:debug("Peer ~p - wait ~b ms for next ping",
                     [aec_peer:ppp(PeerId), Timeout]),
    Conn2 = Conn#conn{ ping = start_timer({ping, PeerId}, Timeout) },
    State#state{ conns = Conns#{ PeerId := Conn2} }.

-spec conn_reset_ping(aec_peer:id(), state()) -> state().
conn_reset_ping(PeerId, #state{ conns = Conns } = State) ->
    #{ PeerId := Conn } = Conns,
    State#state{ conns = Conns#{ PeerId := Conn#conn{ ping = undefined } } }.

%% Cleanup resource used by a connection.
%% The connection SHOULD NOT be in the state anymore.
-spec conn_cleanup(conn(), state()) -> state().
conn_cleanup(#conn{ ping = OldRef } = Conn, State) ->
    ?assertNot(maps:is_key(peer_id(Conn), State#state.conns)),
    cancel_timer(OldRef),
    conn_demonitor(Conn, group_del(Conn, State)).

%--- CONNECTION MONITORING FUNCTIONS -------------------------------------------

%% Monitors the given connection if not already doing so.
%% The connection record may have been already removed from the state.
-spec conn_monitor(conn(), state()) -> state().
conn_monitor(#conn{ peer = Peer, pid = Pid }, State) when is_pid(Pid) ->
    #state{ monitors = Monitors } = State,
    PeerId = aec_peer:id(Peer),
    case maps:find(Pid, Monitors) of
        {ok, {_, PeerId}} -> State;
        error ->
            Ref = erlang:monitor(process, Pid),
            State#state{ monitors = Monitors#{ Pid => {Ref, PeerId} } }
    end.

%% Demonitors the given connection.
%% The connection record may have been already removed from the state.
-spec conn_demonitor(conn(), state()) -> state().
conn_demonitor(#conn{ peer = Peer, pid = Pid }, State) when is_pid(Pid) ->
    #state{ monitors = Monitors } = State,
    PeerId = aec_peer:id(Peer),
    case maps:take(Pid, Monitors) of
        error -> State;
        {{Ref, PeerId}, Monitors2} ->
            erlang:demonitor(Ref, [flush]),
            State#state{ monitors = Monitors2 }
    end.

%% Remove given monitoring entries; assumes it is already demonitored
%% or the monitore fired.
-spec conn_take_monitor(pid(), state())
    -> error | {aec_peer:id(), reference(), state()}.
conn_take_monitor(Pid, State) ->
    #state{ monitors = Monitors } = State,
    case maps:take(Pid, Monitors) of
        error -> error;
        {{Ref, PeerId}, Monitors2} ->
            {PeerId, Ref, State#state{ monitors = Monitors2 }}
    end.

%--- CONFIGURATION FUNCTIONS ---------------------------------------------------

ping_interval() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"ping_interval">>],
                               aecore, ping_interval,
                               ?DEFAULT_PING_INTERVAL).

unblock_interval() ->
    application:get_env(aecore, peer_unblock_interval,
                        ?DEFAULT_UNBLOCK_INTERVAL).

pool_config() ->
    application:get_env(aecore, peer_pool, []).

max_inbound() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"max_inbound">>],
                               aecore, sync_max_inbound,
                               ?DEFAULT_MAX_INBOUND).

max_outbound() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"max_outbound">>],
                               aecore, sync_max_outbound,
                               ?DEFAULT_MAX_OUTBOUND).

max_tcp_probes() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"max_tcp_probes">>],
                               aecore, sync_max_tcp_probes,
                               ?DEFAULT_MAX_TCP_PROBES).

single_outbound_per_group() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"single_outbound_per_group">>],
                               aecore, sync_single_outbound_per_group,
                               ?DEFAULT_SINGLE_OUTBOUND_PER_GROUP).

resolve_max_retries() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"resolver_max_retries">>],
                               aecore, sync_resolver_max_retries,
                               ?DEFAULT_RESOLVE_MAX_RETRY).

resolve_backoff_times() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"resolver_backoff_times">>],
                               aecore, sync_resolver_backoff_times,
                               ?DEFAULT_RESOLVE_BACKOFF_TIMES).

log_peer_connection_count_interval() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"log_peer_connection_count_interval">>],
                               aecore, log_peer_connection_count_interval,
                               ?DEFAULT_LOG_PEER_CONNECTION_COUNT_INTERVAL).

