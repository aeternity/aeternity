%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module storing peers list and providing functions for peers interaction.
%%%
%%% API
%%% TODO: Update the doc...
%%% The external API takes a http_uri:uri() as input. We first check
%%% the validity of this input by parsing it (into a peer()). Only
%%% valid peer() data types are send to the gen_server.
%%%
%%% Internally, the peer() data structure is known and the gen_server is
%%% called directly with the peer as argument.
%%%
%%% The parsing is performed with continuation style success and
%%% error result continuations. The function valid_uri should only be
%%% used internally.
%%% @end
%%%=============================================================================
-module(aec_peers).

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([add_and_ping_peers/1,
         add_and_ping_peers/2,
         block_peer/1,
         unblock_peer/1,
         is_blocked/1,
         remove/1,
         all/0,
         blocked/0,
         get_connection/1,
         get_random/1,
         get_random/2,
         set_local_peer_info/1,
         get_local_peer_info/0,
         update_last_seen/1]).

%% API only used in aec_sync
-export([ add/1,
          log_ping/2
        ]).

-export([check_env/0]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-define(MIN_PING_INTERVAL,   3000).
-define(MAX_PING_INTERVAL, 120000).

-define(DEFAULT_UNBLOCK_INTERVAL, 15 * 60 * 1000).
-define(PEER_ERROR_EXPIRY, 60 * 60 * 1000).


-record(peer, {
          pubkey            :: binary(),
          host              :: string() | binary(),
          port              :: inet:port_number(),
          connection        :: term(),
          last_seen = 0     :: integer(), % Erlang system time (POSIX time)
          expire            :: undefined | integer(), %% Erlang system time: when to drop this peer
          last_pings = []   :: [integer()], % Erlang system time
          ping_tref         :: reference() | undefined,
          trusted = false   :: boolean() % Is it a pre-configured peer
         }).

-type peer_id() :: binary(). %% The Peer is identified by its pubkey for now.
-type peer_info() :: #{ host    := string() | binary(),
                        port    := inet:port_number(),
                        pubkey  := pubkey(),
                        seckey  => pubkey(),
                        conn    => term(),
                        ping    => boolean(),
                        trusted => boolean() }.
-type peer() :: #peer{}.

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

%%------------------------------------------------------------------------------
%% Update `last_seen` timestamp
%%------------------------------------------------------------------------------
-spec update_last_seen(peer_id()) -> ok.
update_last_seen(Peer) ->
    gen_server:cast(?MODULE, {update_last_seen, Peer, timestamp()}).

-spec log_ping(peer(), good | error) -> ok | {error, any()}.
log_ping(Peer, Result) ->
    gen_server:cast(?MODULE, {log_ping, Result, Peer, timestamp()}).

%%------------------------------------------------------------------------------
%% Check user-provided environment
%%------------------------------------------------------------------------------
check_env() ->
    check_ping_interval_env(),
    ok.

check_ping_interval_env() ->
    {DefMin, DefMax} = ping_interval_limits(),
    Min = case aeu_env:user_config([<<"sync">>,<<"ping_interval">>,<<"min">>]) of
              {ok, UserMin} -> UserMin;
              undefined -> DefMin
          end,
    Max = case aeu_env:user_config([<<"sync">>,<<"ping_interval">>,<<"max">>]) of
              {ok, UserMax} -> UserMax;
              undefined -> DefMax
          end,
    application:set_env(aecore, ping_interval_limits, {Min, Max}).

-ifdef(TEST).
unblock_all() ->
    gen_server:call(?MODULE, unblock_all).
-endif.

%%%=============================================================================
%%% gen_server functions
%%%=============================================================================


-record(state, {peers                   :: gb_trees:tree(binary(), peer()),
                blocked = gb_sets:new() :: gb_sets:set(peer_id()),
                errored = gb_sets:new() :: gb_sets:set(peer_id()),
                local_peer              :: peer_info(),  %% for universal handling of URIs
                next_unblock = 0        :: integer() %% Erlang timestamp in ms.
               }).

start_link() ->
    gen_server:start_link({local, ?MODULE} ,?MODULE, ok, []).

init(ok) ->
    {_Scheme, Host, _Port} = aeu_env:local_peer(),
    Port = aeu_env:user_config_or_env(<<"sync_port">>, aecore, sync_port, 1234),
    {SecKey, PubKey} =  aeu_env:user_config_or_env(<<"sync_keys">>, aecore, sync_keys, {<<>>, <<0:(32*8)>>}),
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
handle_call({block, Peer}, _From, State0) ->
    State = maybe_unblock(State0),
    #state{peers = Peers, blocked = Blocked} = State,
    NewState =
        case lookup_peer(Peer, State) of
            {value, _Key, #peer{ trusted = true }} -> State;
            {value, Key, _} ->
                aec_events:publish(peers, {blocked, Peer}),
                State#state{peers   = gb_trees:delete_any(Key, Peers),
                            blocked = gb_sets:add_element(Peer, Blocked)};
            none ->
                aec_events:publish(peers, {blocked, Peer}),
                State#state{blocked = gb_sets:add_element(Peer, Blocked)}
        end,
    {reply, ok, NewState};
handle_call({unblock, Peer}, _From, #state{blocked = Blocked} = State) ->
    NewState =
        case gb_sets:is_element(Peer, Blocked) of
            false -> State;
            true ->
                aec_events:publish(peers, {unblocked, Peer}),
                State#state{blocked = gb_sets:del_element(Peer, Blocked)}
        end,
    {reply, ok, NewState};
%% unblock_all is only available for test
handle_call(unblock_all, _From, State) ->
    {reply, ok, State#state{blocked = gb_sets:new(), next_unblock = next_unblock()}};
handle_call(all, _From, State = #state{peers = Peers}) ->
    PeerIds = [ {peer_info(P), LastSeen}
                || P = #peer{last_seen = LastSeen} <- gb_trees:values(Peers) ],
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
handle_call({get_random, N0, Exclude}, _From, #state{peers = Tree0,
                                                     errored = Errored} = State) ->
    %% first, remove all errored peers
    Tree = exclude_from_set(Errored, Tree0, State),
    N = case N0 of
            all -> gb_trees:size(Tree);
            N0 when is_integer(N0) -> N0
        end,
    Pruned = lists:foldl(
               fun(P, Acc) ->
                       remove_peer(P, Acc)
               end, Tree, Exclude),
    lager:debug("Pruned = ~p", [Pruned]),
    Peers =
        case gb_trees:size(Pruned) of
            Sz when Sz =< N ->
                gb_trees:values(Pruned);
            Sz ->
                Ps = random_values(N, Sz),
                pick_values(Ps, gb_trees:iterator(Pruned))
        end,
    {reply, [ peer_info(P) || P <- Peers ], State}.

handle_cast({update_last_seen, PeerId, Time}, State = #state{peers = Peers}) ->
    case is_local(PeerId, State) orelse is_blocked(PeerId, State)
            orelse lookup_peer(PeerId, State) == none of
        false ->
            {value, _Hash, Peer} = lookup_peer(PeerId, State),
            NewPeer = start_ping_timer(fun set_max_retry/1, Peer),
            NewPeers = enter_peer(NewPeer#peer{last_seen = Time}, Peers),
            Errored = update_errored(ok, PeerId, State#state.errored),
            {noreply, State#state{peers = NewPeers, errored = Errored}};
        true ->
            lager:debug("Ignoring last_seen ~p", [PeerId]),
            {noreply, State}
    end;
handle_cast({log_ping, error, Peer, Time}, State) ->
    {noreply, log_ping_and_set_reping(
              error,
              fun calc_backoff_retry/1, Peer, Time, State)};
handle_cast({log_ping, good, Peer, Time}, State) ->
    {noreply, log_ping_and_set_reping(
              ok,
              fun set_max_retry/1, Peer, Time, State)};
handle_cast({add, PeerInfo}, State0) ->
    %% only add if not already present: prevent overwriting other fields
    Peer   = #peer{ host = maps:get(host, PeerInfo),
                    port = maps:get(port, PeerInfo),
                    pubkey = maps:get(pubkey, PeerInfo),
                    connection = maps:get(connection, PeerInfo, undefined) },
    PeerId = peer_id(Peer),
    lager:debug("ZZPC: add ~p", [PeerInfo]),
    case is_local(PeerId, State0) orelse is_blocked(PeerId, State0) of
        false ->
            NewPeer =
                case lookup_peer(PeerId, State0) of
                    none ->
                        case maps:get(ping, PeerInfo, false) of
                            true ->
                                #{ seckey := SKey, pubkey := PKey,
                                   host := LHost, port := LPort } = State0#state.local_peer,
                                ConnInfo = PeerInfo#{ r_pubkey => maps:get(pubkey, PeerInfo),
                                                      local_host => LHost, local_port => LPort,
                                                      seckey => SKey, pubkey => PKey },
                                {ok, Pid} = aec_peer_connection:connect(ConnInfo),
                                lager:debug("will ping peer ~p", [PeerId]),
                                aec_sync:schedule_ping(PeerId),
                                Peer#peer{ connection = Pid };
                            false ->
                                lager:debug("ZZPC: new peer no ping ~p", [PeerId]),
                                lager:debug("ZZPC: new peer no ping ~p", [State0#state.peers]),
                                Peer
                        end;
                    {value, _Key, FoundPeer = #peer{ connection = FConn }} ->
                        %% Pick "the best" connection
                        case {FConn, Peer#peer.connection} of
                            {undefined, Pid} when is_pid(Pid) ->
                                FoundPeer#peer{ connection = Pid };
                            {Pid1, Pid2} when Pid1 /= Pid2, is_pid(Pid1), is_pid(Pid2) ->
                                %% We pick the new one since that one is definitely working or
                                %% else we would not have gotten this {add, Peer}
                                lager:debug("Two connection alternatives ~p and ~p", [Pid1, Pid2]),
                                lager:debug("Closing ~p ", [Pid1]),
                                aec_peer_connection:stop(Pid1),
                                FoundPeer#peer{ connection = Pid2 };
                                %% case {aec_peer_connection:status(Pid1),
                                %%       aec_peer_connection:status(Pid2)} of
                                %%     {Ok, error} when Ok /= error ->
                                %%         lager:debug("Closing ~p ", [Pid2]),
                                %%         aec_peer_connection:stop(Pid2),
                                %%         FoundPeer;
                                %%     {error, Ok} when Ok /= error ->
                                %%         lager:debug("Closing ~p ", [Pid1]),
                                %%         aec_peer_connection:stop(Pid1),
                                %%         FoundPeer#peer{ connection = Pid2 };
                                %%     {_, _} ->
                                %%         case PeerId < peer_id(State0#state.local_peer) of
                                %%             true ->
                                %%                 lager:debug("Closing ~p ", [Pid2]),
                                %%                 aec_peer_connection:stop(Pid2),
                                %%                 FoundPeer;
                                %%             false ->
                                %%                 lager:debug("Closing ~p ", [Pid1]),
                                %%                 aec_peer_connection:stop(Pid1),
                                %%                 FoundPeer#peer{ connection = Pid2 }
                                %%         end
                                %% end;
                            {_, _} ->
                                FoundPeer
                        end
                end,
            State1 = State0#state{peers = enter_peer(NewPeer, State0#state.peers)},
            {noreply, metrics(State1)};
        true ->
            lager:debug("Will not add peer ~p", [PeerId]),
            {noreply, State0}
    end;
handle_cast({remove, PeerId}, State = #state{peers = Peers, blocked = Blocked}) ->
    case lookup_peer(PeerId, State) of
        none -> ok;
        {value, _Key, Peer} -> stop_connection(Peer)
    end,
    NewState = State#state{peers   = remove_peer(PeerId, Peers),
                           blocked = gb_sets:del_element(PeerId, Blocked)},
    {noreply, metrics(NewState)}.

handle_info({timeout, Ref, {ping_peer, PeerId}}, State) ->
    lager:debug("got ping_peer timer msg for ~p", [PeerId]),
    case lookup_peer(PeerId, State) of
        none ->
            lager:debug("ping_peer timer msg for unknown uri (~p)", [PeerId]),
            {noreply, State};
        {value, _Hash, #peer{ping_tref = Ref} = Peer} ->
            %% TODO: use jobs workers instead
            maybe_ping_peer(PeerId, State),
            Peers = enter_peer(
                      Peer#peer{ping_tref = undefined},
                      State#state.peers),
            {noreply, State#state{peers = Peers}};
        {value, _, #peer{}} ->
            lager:debug("stale ping_peer timer msg (~p) - ignore", [PeerId]),
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

metrics(#state{peers = Peers, blocked = Blocked,
               errored = Errored} = State) ->
    aec_metrics:try_update([ae,epoch,aecore,peers,count],
                           gb_trees:size(Peers)),
    aec_metrics:try_update([ae,epoch,aecore,peers,blocked],
                           gb_sets:size(Blocked)),
    aec_metrics:try_update([ae,epoch,aecore,peers,errored],
                           gb_sets:size(Errored)),
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

exclude_from_set(Set, Tree, S) ->
    gb_sets:fold(
      fun(Peer, Acc) ->
              exclude_peer(Peer, Acc, S)
      end, Tree, Set).


exclude_peer(Peer, T, S) ->
    case lookup_peer(Peer, S) of
        none ->
            T;
        {value, Key, _} ->
            gb_trees:delete_any(Key, T)
    end.

remove_peer(PeerInfo = #{}, Peers) ->
    remove_peer(peer_id(PeerInfo), Peers);
remove_peer(#peer{} = Peer, Peers) ->
    remove_peer(peer_id(Peer), Peers);
remove_peer(PeerId, Peers) when is_binary(PeerId) ->
    gb_trees:delete_any(hash_peer(PeerId), Peers).

random_values(N, Sz) ->
    random_values(N, Sz, ordsets:new()).

random_values(N, Sz, Acc) when N > 0 ->
    R = rand:uniform(Sz),
    case ordsets:is_element(R, Acc) of
  true ->
      random_values(N, Sz, Acc);
  false ->
      random_values(N-1, Sz, ordsets:add_element(R, Acc))
    end;
random_values(_, _, Acc) ->
    Acc.

pick_values(Ps, Iter) ->
    pick_values(Ps, 1, gb_trees:next(Iter), []).

pick_values([H|T], H, {_, V, Iter}, Acc) ->
    pick_values(T, H+1, gb_trees:next(Iter), [V|Acc]);
pick_values([], _, _, Acc) ->
    Acc;
pick_values(Ps, N, {_, _, Iter}, Acc) ->
    pick_values(Ps, N+1, gb_trees:next(Iter), Acc).

lookup_peer(PeerId, #state{peers = Peers}) when is_binary(PeerId) ->
    Key = hash_peer(PeerId),
    case gb_trees:lookup(Key, Peers) of
        none -> none;
        {value, P} ->
            {value, Key, P}
    end.

is_blocked(Peer, #state{blocked = Blocked}) ->
    lager:debug("Check for blocked ~p in ~p\n", [Peer, Blocked]),
    gb_sets:is_element(Peer, Blocked).


log_ping_and_set_reping(Res, CalcF, PeerId, Time, State) ->
    case lookup_peer(PeerId, State) of
        none ->
            lager:debug("Reported ping event for unknown peer (~p)", [PeerId]),
            State;
        {value, _Hash, Peer} ->
            update_ping_metrics(Res),
            Peer1 = save_ping_event(Res, Time, Peer),
            %% drop an errored peer if it has expired
            case Time > Peer1#peer.expire andalso not Peer#peer.trusted of
                 true ->
                     lager:debug("ZZPC: remove_peer ~p > ~p - ~p", [Time, Peer1#peer.expire, PeerId]),
                     stop_connection(Peer1),
                     State#state{peers = remove_peer(PeerId, State#state.peers),
                                 errored = gb_sets:delete_any(PeerId, State#state.errored)};
                 false ->
                     Peer2 = start_ping_timer(CalcF, Peer1),
                     lager:debug("ZZPC: set ping_timer for ~p : ~p", [peer_info(PeerId), Peer2#peer.ping_tref]),
                     Peers = enter_peer(Peer2, State#state.peers),
                     Errored = update_errored(Res, PeerId, State#state.errored),
                     State#state{peers = Peers, errored = Errored}
            end
    end.

update_ping_metrics(Res) ->
    Name = case Res of
               ok    -> [ae,epoch,aecore,peers,ping,success];
               error -> [ae,epoch,aecore,peers,ping,failure]
           end,
    aec_metrics:try_update(Name, 1).

update_errored(ok, PeerId, Errored) ->
    gb_sets:delete_any(PeerId, Errored);
update_errored(error, PeerId, Errored) ->
    gb_sets:add_element(PeerId, Errored).

stop_connection(#peer{ connection = Pid }) when is_pid(Pid) ->
    lager:debug("ZZPC: stopping connection ~p", [Pid]),
    aec_peer_connection:stop(Pid);
stop_connection(_) ->
    ok.

start_ping_timer(CalcF, Peer) ->
    NewTime = CalcF(Peer),
    lager:debug("Starting re-ping timer for ~p: ~p", [peer_info(Peer), NewTime]),
    TRef = erlang:start_timer(
             NewTime, self(), {ping_peer, peer_id(Peer)}),
    save_ping_timer(TRef, Peer).

save_ping_event(Res, T, Peer) ->
    LastPings = case Peer#peer.last_pings of
                    []      -> [T];
                    [A]     -> [T,A];
                    [A,B|_] -> [T,A,B]
                end,
    {LastSeen, Expire} =
        case Res of
            ok    -> {T, T + application:get_env(aecore, peer_error_expiry, ?PEER_ERROR_EXPIRY)};
            error -> {Peer#peer.last_seen,
                      if Peer#peer.expire == undefined -> T;
                         true -> Peer#peer.expire
                      end}
        end,
    Peer#peer{last_seen = LastSeen, last_pings = LastPings, expire = Expire}.

save_ping_timer(TRef, #peer{ping_tref = Prev} = Peer) ->
    case Prev of
        undefined -> ok;
        _ -> try erlang:cancel_timer(Prev, [{async, true}, {info, false}])
             catch error:_ -> ok end
    end,
    Peer#peer{ping_tref = TRef}.

calc_backoff_retry(#peer{last_pings = Last}) ->
    lager:debug("calc_backoff_retry; Last = ~p", [Last]),
    {Min, Max} = ping_interval_limits(),
    Intervals = intervals(Last, Min),
    lager:debug("Intervals = ~p", [Intervals]),
    erlang:min(Max, calc_retry(Intervals, Min)).

set_max_retry(_) ->
    {_, Max} = ping_interval_limits(),
    Max.

intervals([A, B | T], Min) when A > B ->
    [A-B | intervals([B | T], Min)];
intervals([_, B | T], Min) ->
    [0 | intervals([B | T], Min)];
intervals([_], _) -> [];
intervals([], _) -> [].

calc_retry([A,B|_], Min) when A < B ->
    A + Min;
calc_retry([A,B|_], _) ->
    A + B;
calc_retry(_, Min) ->
    Min.

ping_interval_limits() ->
    Default = {?MIN_PING_INTERVAL, ?MAX_PING_INTERVAL},
    case aeu_env:get_env(aecore, ping_interval_limits, Default) of
        {Min, Max} when is_integer(Min),
                        is_integer(Max),
                        Max >= Min ->
            {Min, Max};
        Other ->
            lager:debug("invalid ping limits: ~p; using default (~p)",
                        [Other, Default]),
            Default
    end.

maybe_ping_peer(Peer, State) ->
    case is_local(Peer, State) orelse is_blocked(Peer, State) of
        false ->
            lager:debug("will ping peer ~p", [Peer]),
            aec_sync:schedule_ping(Peer);
        true ->
            lager:debug("will not ping ~p", [Peer]),
            ignore
    end.

is_local(Peer, #state{local_peer = LocalPeer}) ->
    #{ pubkey := PubKey1 } = peer_info(Peer),
    #{ pubkey := PubKey2 } = peer_info(LocalPeer),
    R = PubKey1 == PubKey2,
    lager:debug("is_local_uri(~p) -> ~p (~p)", [Peer, R, LocalPeer]),
    R.

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
