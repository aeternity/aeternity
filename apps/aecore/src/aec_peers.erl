%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module storing peers list and providing functions for peers interaction.
%%%
%%% API
%%%
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

%% API
-export([add_and_ping_peers/1,
         add_and_ping_peers/2,
         block_peer/1,
         unblock_peer/1,
         is_blocked/1,
         remove/1,
         all/0,
         blocked/0,
         get_random/1,
         get_random/2,
         set_local_peer_uri/1,
         get_local_peer_uri/0,
         update_last_seen/1]).

%% API only used in aec_sync
-export([ add/2,
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


%% We parse the uri's with http_uri, therefore we use types from that module.
-record(peer, {
          uri = ""          :: http_uri_uri(),   %% try not to use!
          scheme            :: http_uri:scheme(),
          host              :: http_uri_host(),
          port              :: http_uri_port(),
          last_seen = 0     :: integer(), % Erlang system time (POSIX time)
          expire            :: undefined | integer(), %% Erlang system time: when to drop this peer
          last_pings = []   :: [integer()], % Erlang system time
          ping_tref         :: reference() | undefined,
          trusted = false   :: boolean() % Is it a pre-configured peer
         }).

-type http_uri_uri() :: string() | unicode:unicode_binary(). %% From https://github.com/erlang/otp/blob/OTP-20.2.3/lib/inets/doc/src/http_uri.xml#L57
-type http_uri_host() :: string() | unicode:unicode_binary(). %% From https://github.com/erlang/otp/blob/OTP-20.2.3/lib/inets/doc/src/http_uri.xml#L64
-type http_uri_port() :: pos_integer(). %% https://github.com/erlang/otp/blob/OTP-20.2.3/lib/inets/doc/src/http_uri.xml#L66

-type peer() :: #peer{}.

%%%=============================================================================

-spec valid_uri(http_uri_uri(), fun((peer()) -> T)) -> T | {error, any()}.
valid_uri(Uri, Success) ->
    valid_uri(Uri, Success, fun(X) -> X end).

-spec valid_uri(http_uri_uri(), fun((peer()) -> T1), fun(({error, any()}) -> T2)) -> T1 | T2.
valid_uri(Uri, Success, Failure) ->
    case parse_uri(Uri) of
        {error, _} = Error ->
            Failure(Error);
        #peer{} = ParsedPeer ->
            Success(ParsedPeer)
        end.

%% parse the uri's and keep the valid once as peer.
-spec valid_uris(list(http_uri_uri())) -> list(peer()).
valid_uris(Uris) ->
    [ Peer || Uri <- Uris, #peer{} = Peer <- [parse_uri(Uri)] ].


%%------------------------------------------------------------------------------
%% Add peer by url. Connect if `Connect==true`
%%------------------------------------------------------------------------------
-spec add(http_uri_uri() | peer(), boolean()) -> ok | {error, any()}.
add(Uri, Connect) when is_boolean(Connect) ->
    valid_uri(Uri, fun(Peer) ->
                       gen_server:cast(?MODULE, {add, Peer, Connect})
                   end).

%%------------------------------------------------------------------------------
%% Add peers by uri.
%%------------------------------------------------------------------------------
-spec add_and_ping_peers([http_uri_uri()]) -> ok.
add_and_ping_peers(Uris) ->
    add_and_ping_peers(Uris, false).

%%------------------------------------------------------------------------------
%% Add peers by uri. Indicate whether they are trusted (i.e. pre-configured) or
%% not.
%%------------------------------------------------------------------------------
-spec add_and_ping_peers([http_uri_uri()], boolean()) -> ok.
add_and_ping_peers(Uris, Trusted) ->
    case valid_uris(Uris) of
        [] -> ok;
        Peers0 when is_list(Peers0) ->
            Peers = [ P#peer{ trusted = Trusted } || P <- Peers0 ],
            gen_server:cast(?MODULE, {add_and_ping, Peers})
    end.

%%------------------------------------------------------------------------------
%% Block peer
%%------------------------------------------------------------------------------
-spec block_peer(http_uri_uri()) -> ok | {error, any()}.
block_peer(Uri) ->
    valid_uri(Uri, 
              fun(Peer) -> gen_server:call(?MODULE, {block, Peer}) end).


%%------------------------------------------------------------------------------
%% Unblock peer
%%------------------------------------------------------------------------------
-spec unblock_peer(http_uri_uri()) -> ok | {error, any()}.
unblock_peer(Uri) ->
    valid_uri(Uri, 
              fun(Peer) -> gen_server:call(?MODULE, {unblock, Peer}) end).

%%------------------------------------------------------------------------------
%% Check if peer is blocked. Erroneous URI is by definition blocked.
%%------------------------------------------------------------------------------
-spec is_blocked(http_uri_uri()) -> boolean().
is_blocked(Uri) ->
    valid_uri(Uri, 
              fun(Peer) -> gen_server:call(?MODULE, {is_blocked, Peer}) end,
              fun(_) -> true end).

%%------------------------------------------------------------------------------
%% Remove peer by url.
%% At the moment also removes uri from the blocked list
%%------------------------------------------------------------------------------
-spec remove(http_uri_uri()) -> ok | {error, any()}.
remove(Uri) ->
    valid_uri(Uri,
              fun(Peer) -> gen_server:cast(?MODULE, {remove, Peer}) end).

%%------------------------------------------------------------------------------
%% Get list of all peers. The list may be big. Use with caution.
%% Consider using get_random instead.
%%------------------------------------------------------------------------------
-spec all() -> list(http_uri_uri()).
all() ->
    gen_server:call(?MODULE, all).

-spec blocked() -> list(http_uri_uri()).
blocked() ->
    gen_server:call(?MODULE, blocked).

%%------------------------------------------------------------------------------
%% Get up to N random peers.
%%
%% The peers are randomly distributed in the sorted gb_tree (due to the use of hash_uri),
%% so we can find a random peer by choosing a point and getting the next peer in gb_tree.
%% That's what this function does
%%------------------------------------------------------------------------------
-spec get_random(all | non_neg_integer()) -> [http_uri_uri()].
get_random(NumberOfPeers) ->
    get_random(NumberOfPeers, []).

%%------------------------------------------------------------------------------
%% Get up to N random peers, but not peers included in the Exclude list
%%
%% The peers are randomly distributed in the sorted gb_tree (due to the use of hash_uri),
%% so we can find a random peer by choosing a point and getting the next peer in gb_tree.
%% That's what this function does
%%------------------------------------------------------------------------------
-spec get_random(all | non_neg_integer(), [http_uri_uri()]) -> [http_uri_uri()].
get_random(N, Exclude) when is_list(Exclude), 
    N == all orelse (is_integer(N) andalso N >= 0) ->
    gen_server:call(?MODULE, {get_random, N, valid_uris(Exclude)}).

%%------------------------------------------------------------------------------
%% Set our own peer address
%%------------------------------------------------------------------------------
-spec set_local_peer_uri(http_uri_uri()) -> ok | {error, any()}.
set_local_peer_uri(Uri) ->
    valid_uri(Uri,
              fun(Peer) -> 
                  gen_server:cast(?MODULE, {set_local_peer_uri, Peer}) 
              end).

%%------------------------------------------------------------------------------
%% Set our own peer address
%%------------------------------------------------------------------------------
-spec get_local_peer_uri() -> http_uri_uri().
get_local_peer_uri() ->
    gen_server:call(?MODULE, get_local_peer_uri).

%%------------------------------------------------------------------------------
%% Update `last_seen` timestamp
%%------------------------------------------------------------------------------
-spec update_last_seen(http_uri_uri()) -> ok | {error, any()}.
update_last_seen(Uri) ->
    valid_uri(Uri,
              fun(Peer) -> 
                  gen_server:cast(?MODULE, {update_last_seen, Peer, timestamp()}) 
              end).

-spec log_ping(http_uri_uri(), good | error) -> ok | {error, any()}.
log_ping(Uri, Result) ->
    valid_uri(Uri,
              fun(Peer) -> 
                  gen_server:cast(?MODULE, {log_ping, Result, Peer, timestamp()})
              end).

%%------------------------------------------------------------------------------
%% Check user-provided environment
%%------------------------------------------------------------------------------
check_env() ->
    [check_env_(UKey, AKey) ||
        {UKey, AKey} <- [{<<"peers">>, peers},
                         {<<"blocked_peers">>, blocked_peers}]],
    check_ping_interval_env(),
    ok.

check_env_(UKey, AKey) ->
    case aeu_env:user_config(UKey) of
        {ok, Peers0} when is_list(Peers0) ->
            Peers = [binary_to_list(P) || P <- Peers0],
            application:set_env(aecore, AKey, Peers);
        undefined ->
            ok
    end.

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


-record(state, {peers :: gb_trees:tree(binary(), peer()),
                blocked = gb_sets:new() :: gb_sets:set(http_uri_uri()),
                errored = gb_sets:new() :: gb_sets:set(http_uri_uri()),
                local_peer :: peer(),  %% for universal handling of URIs
                next_unblock = 0 :: integer() %% Erlang timestamp in ms.
               }).

start_link() ->
    gen_server:start_link({local, ?MODULE} ,?MODULE, ok, []).

init(ok) ->
    {Scheme, Host, Port} = aeu_env:local_peer(),
    LocalPeer = #peer{scheme = Scheme, host = Host, port = Port},
    {ok, #state{peers = gb_trees:empty(),
                local_peer = LocalPeer}}.

handle_call({set_local_peer_uri, Peer}, _From, State) ->
    {reply, ok, State#state{local_peer = Peer}};
handle_call(get_local_peer_uri, _From, State) ->
    {reply, uri_of_peer(State#state.local_peer), State};
handle_call({is_blocked, Peer}, _From, State0) ->
    State = maybe_unblock(State0),
    {reply, is_blocked(Peer, State), State};
handle_call({block, Peer}, _From, State0) ->
    State = maybe_unblock(State0),
    #state{peers = Peers, blocked = Blocked} = State,
    Uri = uri_of_peer(Peer),
    NewState =
        case lookup_peer(Uri, State) of
            {value, _Key, #peer{ trusted = true }} -> State;
            {value, Key, _} ->
                aec_events:publish(peers, {blocked, Uri}),
                State#state{peers   = gb_trees:delete_any(Key, Peers),
                            blocked = gb_sets:add_element(Uri, Blocked)};
            none ->
                aec_events:publish(peers, {blocked, Uri}),
                State#state{blocked = gb_sets:add_element(Uri, Blocked)}
        end,
    {reply, ok, NewState};
handle_call({unblock, Peer}, _From, #state{blocked = Blocked} = State) ->
    Uri = uri_of_peer(Peer),
    NewState = 
        case gb_sets:is_element(Uri, Blocked) of
            false -> State;
            true ->
                aec_events:publish(peers, {unblocked, Uri}),
                State#state{blocked = gb_sets:del_element(Uri, Blocked)}
        end,
    {reply, ok, NewState};
%% unblock_all is only available for test
handle_call(unblock_all, _From, State) ->
    {reply, ok, State#state{blocked = gb_sets:new(), next_unblock = next_unblock()}};
handle_call(all, _From, State) ->
    Uris = [ {uri_of_peer(Peer), LastSeen} || #peer{last_seen=LastSeen} = Peer
                                  <- gb_trees:values(State#state.peers) ],
    {reply, Uris, State};
handle_call(blocked, _From, State) ->
    Uris = [ Uri || Uri <- gb_sets:to_list(State#state.blocked) ],
    {reply, Uris, State};
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
    {reply, [ uri_of_peer(P) || P <- Peers ], State}.

handle_cast({update_last_seen, Peer, Time}, State = #state{peers = Peers}) ->
    Uri = uri_of_peer(Peer),
    case is_local_uri(Peer, State) orelse is_blocked(Peer, State) of
        false ->
            NewPeer = 
                case lookup_peer(Uri, State) of
                    none ->
                        %% can happen e.g. at first ping
                        start_ping_timer(
                            fun set_max_retry/1,
                            Peer);
                    {value, _Hash, FoundPeer} ->
                        start_ping_timer(
                            fun set_max_retry/1,
                            FoundPeer)
                end,
            NewPeers = enter_peer(NewPeer#peer{last_seen = Time}, Peers),
            Errored = update_errored(ok, Uri, State#state.errored),
            {noreply, State#state{peers = NewPeers, errored = Errored}};
        true ->
            lager:debug("Ignoring last_seen ~p", [Uri]),
            {noreply, State}
    end;
handle_cast({log_ping, error, Peer, Time}, State) ->
    {noreply, log_ping_and_set_reping(
              error,
              fun calc_backoff_retry/1, uri_of_peer(Peer), Time, State)};
handle_cast({log_ping, good, Peer, Time}, State) ->
    {noreply, log_ping_and_set_reping(
              ok,
              fun set_max_retry/1, uri_of_peer(Peer), Time, State)};
handle_cast({add, Peer, Connect}, State0) ->
    Uri = uri_of_peer(Peer),
    %% only add if not already present: prevent overwriting other fields
    case is_local_uri(Peer, State0) orelse is_blocked(Peer, State0) orelse 
         lookup_peer(Uri, State0) =/= none of
        false ->
            State1 = State0#state{peers = enter_peer(Peer, State0#state.peers)}, 
            case Connect andalso not has_been_seen(Peer) of
                true ->
                    lager:debug("will ping peer ~p", [Uri]),
                    aec_sync:schedule_ping(Uri);
                false -> ok
            end,
            {noreply, metrics(State1)};
        true ->
            lager:debug("Will not add peer ~p", [Uri]),
            {noreply, State0}
    end;
handle_cast({add_and_ping, Peers}, State0) ->
    State = maybe_unblock(State0),
    lager:debug("add and ping peers ~p", [Peers]),
    State1 = 
        lists:foldl(
          fun(P, #state{peers = Ps} = S) ->
              Uri = uri_of_peer(P), 
              case is_local_uri(P, S) orelse is_blocked(P, S) orelse lookup_peer(Uri, S) =/= none of
                  false ->
                      lager:debug("will ping peer ~p", [Uri]),
                      aec_sync:schedule_ping(Uri),
                      S#state{peers = enter_peer(P, Ps)};
                  true ->
                      lager:debug("Don't insert nor ping peer (~p)", [Uri]),
                      S
              end
          end, State, Peers),
    lager:debug("known peers: ~p", [gb_trees:to_list(State1#state.peers)]),
    {noreply, metrics(State1)};
handle_cast({remove, Peer}, State = #state{peers = Peers, blocked = Blocked}) ->
    Uri = uri_of_peer(Peer),
    NewState = 
        State#state{peers   = remove_peer(Peer, Peers),
                    blocked = gb_sets:del_element(Uri, Blocked)},
    {noreply, metrics(NewState)}.

handle_info({timeout, Ref, {ping_peer, Uri}}, State) ->
    lager:debug("got ping_peer timer msg for ~p", [Uri]),
    case lookup_peer(Uri, State) of
        none ->
            lager:debug("ping_peer timer msg for unknown uri (~p)", [Uri]),
            {noreply, State};
        {value, _Hash, #peer{ping_tref = Ref} = Peer} ->
            %% TODO: use jobs workers instead
            maybe_ping_peer(Peer, State),
            Peers = enter_peer(
                      Peer#peer{ping_tref = undefined},
                      State#state.peers),
            {noreply, State#state{peers = Peers}};
        {value, _, #peer{}} ->
            lager:debug("stale ping_peer timer msg (~p) - ignore", [Uri]),
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
    gb_trees:enter(hash_uri(uri_of_peer(P)), P, Peers).

timestamp() ->
    erlang:system_time(millisecond).

-spec hash_uri(http_uri_uri()) -> binary().
hash_uri(Uri) ->
    Hash = crypto:hash(md4, Uri),
    <<Hash/binary, Uri/binary>>.

exclude_from_set(Set, Tree, S) ->
    gb_sets:fold(
      fun(Uri, Acc) ->
              exclude_peer_uri(Uri, Acc, S)
      end, Tree, Set).


exclude_peer_uri(Uri, T, S) ->
    case lookup_peer(Uri, S) of
        none ->
            T;
        {value, Key, _} ->
            gb_trees:delete_any(Key, T)
    end.

remove_peer(#peer{} = Peer, Peers) ->
    gb_trees:delete_any(hash_uri(uri_of_peer(Peer)), Peers).

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

has_been_seen(Peer) ->
    Peer#peer.last_seen =/= 0.

lookup_peer(Uri, #state{peers = Peers}) when is_binary(Uri) ->
    Key = hash_uri(Uri),
    case gb_trees:lookup(Key, Peers) of
        none -> none;
        {value, P} ->
            {value, Key, P}
    end.

is_blocked(Peer, #state{blocked = Blocked}) ->
    Uri = uri_of_peer(Peer),
    lager:debug("Check for blocked ~p in ~p\n", [Uri, Blocked]), 
    gb_sets:is_element(Uri, Blocked).


log_ping_and_set_reping(Res, CalcF, Uri, Time, State) ->
    case lookup_peer(Uri, State) of
        none ->
            lager:debug("Reported ping event for unknown peer (~p)", [Uri]),
            State;
        {value, _Hash, Peer} ->
            update_ping_metrics(Res),
            Peer1 = save_ping_event(Res, Time, Peer),
            %% drop an errored peer if it has expired
            case Time > Peer1#peer.expire andalso not Peer#peer.trusted of
                 true ->
                     State#state{peers = remove_peer(Peer, State#state.peers), 
                                 errored = gb_sets:delete_any(
                                            uri_of_peer(Peer), State#state.errored)};
                 false ->
                     Peer2 = start_ping_timer(CalcF, Peer1),
                     Peers = enter_peer(Peer2, State#state.peers),
                     Errored = update_errored(Res, uri_of_peer(Peer), State#state.errored),
                     State#state{peers = Peers, errored = Errored}
            end
    end.

update_ping_metrics(Res) ->
    Name = case Res of
               ok    -> [ae,epoch,aecore,peers,ping,success];
               error -> [ae,epoch,aecore,peers,ping,failure]
           end,
    aec_metrics:try_update(Name, 1).

update_errored(ok, Uri, Errored) ->
    gb_sets:delete_any(Uri, Errored);
update_errored(error, Uri, Errored) ->
    gb_sets:add_element(Uri, Errored).

start_ping_timer(CalcF, Peer) ->
    Uri = uri_of_peer(Peer),
    NewTime = CalcF(Peer),
    lager:debug("Starting re-ping timer for ~p: ~p", [Uri, NewTime]),
    TRef = erlang:start_timer(
             NewTime, self(), {ping_peer, Uri}),
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
    Uri = uri_of_peer(Peer),
    case is_local_uri(Peer, State) orelse is_blocked(Peer, State) of
        false ->
            lager:debug("will ping peer ~p", [Uri]),
            aec_sync:schedule_ping(Uri);
        true ->
            lager:debug("will not ping ~p", [Uri]),
            ignore
    end.

is_local_uri(Peer, #state{local_peer = LocalPeer}) ->
    R = Peer#peer.port == LocalPeer#peer.port andalso 
        lists:member(Peer#peer.host, [<<"localhost">>, <<"127.0.0.1">>, LocalPeer#peer.host]),
    lager:debug("is_local_uri(~p) -> ~p (~p)", [uri_of_peer(Peer), R, uri_of_peer(LocalPeer)]),
    R.

-spec parse_uri(http_uri_uri()) -> peer() | {error, any()}.
parse_uri(Uri) ->
    case http_uri:parse(Uri) of
        {ok, {Scheme, _UserInfo, Host, Port, _Path, _Query, _Fragment}} ->
            #peer{scheme = Scheme, host = iolist_to_binary(Host), port = Port};
        {ok, {Scheme, _UserInfo, Host, Port, _Path, _Query}} ->
            #peer{scheme = Scheme, host = iolist_to_binary(Host), port = Port};
        {error, _} = Error ->
            Error
    end.

-spec uri_of_peer(peer()) -> http_uri_uri().
uri_of_peer(#peer{host = Host, scheme = Scheme, port = Port}) ->
    aeu_requests:pp_uri({Scheme, Host, Port}).
