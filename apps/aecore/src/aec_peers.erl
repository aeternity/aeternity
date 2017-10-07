%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module storing peers list and providing funtionc for peers interaction
%%% @end
%%%=============================================================================
-module(aec_peers).

-behaviour(gen_server).

%% API
-export([add/1,
         add/2,
         register_alias/2,
         add_and_ping_peers/1,
         remove/1,
         info/1,
         all/0,
         get_random/0,
         get_random/1,
         get_random/2,
         uri_from_ip_port/2,
         uri/1,
         set_local_peer_uri/1,
         get_local_peer_uri/0,
         update_last_seen/1]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("peers.hrl").

-type uri() :: http_uri:uri().
-type get_peer_result() :: {ok, peer()} | {error, string()}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Add peer by url or supplying full peer() record. Try to connect.
%%------------------------------------------------------------------------------
-spec add(uri() | peer()) -> ok.
add(Peer) ->
    add(Peer, true).

%%------------------------------------------------------------------------------
%% Add peer by url or supplying full peer() record. Connect if `Connect==true`
%%------------------------------------------------------------------------------
-spec add(uri() | peer(), boolean()) -> ok.
add(Peer, Connect) when is_boolean(Connect) ->
    gen_server:cast(?MODULE, {add, peer_record(Peer), Connect}),
    ok.

%%------------------------------------------------------------------------------
%% Register an alias A of known peer P. Ignored if P is unknown
%%
%% This is mainly intended to handle cases where a pinging peer and the
%% pinged peer use different uris (peer uri vs "source" elem in ping obj).
%% Use of peer uri and alias will both lead to the same peer record
%% (and the 'uri' elem in the record reveals which is the origin and which
%% is the alias; this usually doesn't matter.)
%%------------------------------------------------------------------------------
-spec register_alias(uri() | peer(), uri()) -> ok.
register_alias(Peer, Alias) ->
    gen_server:cast(?MODULE, {alias, normalize_uri(uri(Peer)),
                              normalize_uri(Alias)}),
    ok.

%%------------------------------------------------------------------------------
%% Add peer by url or supplying full peer() record. Connect if `Connect==true`
%%------------------------------------------------------------------------------
-spec add_and_ping_peers([uri()]) -> ok.
add_and_ping_peers(Peers) ->
    case [peer_record(P) || P <- Peers] of
        [] -> ok;
        [_|_] = PeerRecs ->
            gen_server:cast(?MODULE, {add_and_ping, PeerRecs})
    end.

%%------------------------------------------------------------------------------
%% Remove peer by url
%%------------------------------------------------------------------------------
-spec remove(uri()) -> ok.
remove(PeerUri) ->
    gen_server:cast(?MODULE, {remove, normalize_uri(uri(PeerUri))}),
    ok.

%%------------------------------------------------------------------------------
%% Get information (peer() record) of peer with supplied url.
%% Normally returns {ok, Peer}
%% If peer not found gives {error, "peer unknown"}
%%------------------------------------------------------------------------------
-spec info(uri()) -> get_peer_result().
info(PeerUri) ->
    gen_server:call(?MODULE, {info, normalize_uri(uri(PeerUri))}).

%%------------------------------------------------------------------------------
%% Get list of all peers. The list may be big. Use with caution.
%% Consider using get_random instead.
%%------------------------------------------------------------------------------
-spec all() -> list(peer()).
all() ->
    gen_server:call(?MODULE, all).

%%------------------------------------------------------------------------------
%% Get random peer. This may return error when no peer is known to us (unlikely).
%% Normally returns {ok, Peer}
%%
%% The peers are randomly distributed in the sorted gb_tree (due to the use of hash_uri),
%% so we can find a random peer by choosing a point and getting the next peer in gb_tree.
%% That's what this function does
%%------------------------------------------------------------------------------
-spec get_random() -> get_peer_result().
get_random() ->
    gen_server:call(?MODULE, get_random).

%%------------------------------------------------------------------------------
%% Get up to N random peers.
%%
%% The peers are randomly distributed in the sorted gb_tree (due to the use of hash_uri),
%% so we can find a random peer by choosing a point and getting the next peer in gb_tree.
%% That's what this function does
%%------------------------------------------------------------------------------
-spec get_random(all | non_neg_integer()) -> [peer()].
get_random(all) ->
    get_random(all, []);
get_random(N) when is_integer(N), N >= 0 ->
    get_random(N, []).

%%------------------------------------------------------------------------------
%% Get up to N random peers, but not peers included in the Exclude list
%%
%% The peers are randomly distributed in the sorted gb_tree (due to the use of hash_uri),
%% so we can find a random peer by choosing a point and getting the next peer in gb_tree.
%% That's what this function does
%%------------------------------------------------------------------------------
-spec get_random(all | non_neg_integer(), [peer() | uri()]) -> [peer()].
get_random(all, Exclude) when is_list(Exclude) ->
    gen_server:call(?MODULE, {get_random, all, Exclude});
get_random(N, Exclude) when is_integer(N), N >= 0, is_list(Exclude) ->
    gen_server:call(?MODULE, {get_random, N, Exclude}).

%%------------------------------------------------------------------------------
%% Get url from IP and port. IP format: xxx.xxx.xxx.xxx
%%------------------------------------------------------------------------------
-spec uri_from_ip_port(IP :: string(), Port :: number()) -> uri().
uri_from_ip_port(IP, Port) ->
    "http://" ++ IP ++ ":" ++ integer_to_list(Port) ++ "/".

%%------------------------------------------------------------------------------
%% Get uri of peer
%%------------------------------------------------------------------------------
-spec uri(peer() | uri()) -> uri().
uri(#peer{uri = Uri}) ->
    Uri;
uri(Uri) when is_list(Uri) ->
    Uri;
uri(Uri) when is_binary(Uri) ->
    binary_to_list(Uri).

%%------------------------------------------------------------------------------
%% Set our own peer address
%%------------------------------------------------------------------------------
-spec set_local_peer_uri(peer()) -> ok.
set_local_peer_uri(Peer) ->
    P = peer_record(Peer),
    gen_server:call(?MODULE, {set_local_peer_uri, P#peer.uri}).

%%------------------------------------------------------------------------------
%% Set our own peer address
%%------------------------------------------------------------------------------
-spec get_local_peer_uri() -> uri().
get_local_peer_uri() ->
    gen_server:call(?MODULE, get_local_peer_uri).

%%------------------------------------------------------------------------------
%% Update `last_seen` timestamp
%%------------------------------------------------------------------------------
-spec update_last_seen(uri()) -> ok.
update_last_seen(Uri) ->
    gen_server:cast(?MODULE, {update_last_seen, uri(Uri), timestamp()}).

%%%=============================================================================
%%% gen_server functions
%%%=============================================================================

-record(state, {peers :: gb_trees:tree(binary(),peer()),
                aliases :: gb_trees:tree(uri(), binary()),
                local_peer_uri :: uri()}).

start_link() ->
    gen_server:start_link({local, ?MODULE} ,?MODULE, ok, []).

init(ok) ->
    PeerUri = aehttp_app:local_peer_uri(),
    {ok, #state{peers=gb_trees:empty(),
                aliases=gb_trees:empty(),
                local_peer_uri = PeerUri}}.

handle_call({set_local_peer_uri, Peer}, _From, State) ->
    {reply, ok, State#state{local_peer_uri = Peer}};
handle_call(get_local_peer_uri, _From, State) ->
    {reply, State#state.local_peer_uri, State};
handle_call({info, PeerUri}, _From, State) ->
    case lookup_peer(PeerUri, State) of
        {value, _, Peer} ->
            {reply, {ok, Peer}, State};
        none ->
            {reply, {error, "peer not found"}, State}
    end;
handle_call(all, _From, State) ->
    {reply, gb_trees:values(State#state.peers), State};
handle_call({get_random, N, Exclude}, _From, State) ->
    {reply, get_random_n(N, Exclude, State#state.peers), State};
handle_call(get_random, _From, State) ->
    case gb_trees:is_empty(State#state.peers) of
        true ->
            {reply, {error, "we don't know any peers"}, State};
        false ->
            RandomHash = [rand:uniform(256)-1 || _ <- lists:seq(1, 16)],
            %RandomHash = <<RandomInts:16/big-unsigned-integer-unit:8>>,  %This line generates a fairly random 16byte hex string
            {LargestKey, _} = gb_trees:largest(State#state.peers),
            case (binary_to_list(LargestKey) < RandomHash) of
                true ->  %we choese an Hash after the last Hash in tree so we return the first element
                    {_, Peer} = gb_trees:smallest(State#state.peers),
                    {reply, {ok, Peer}, State};
                false ->
                    Iterator = gb_trees:iterator_from(list_to_binary(RandomHash), State#state.peers),
                    {_, Peer, _} = gb_trees:next(Iterator),
                    {reply, {ok, Peer}, State}
            end
    end.

handle_cast({update_last_seen, Uri, Time}, State = #state{peers = Peers}) ->
    NewPeers =
        case lookup_peer(Uri, State) of
            none ->
                %% can happen e.g. at first ping
                Peer = peer_record(Uri),
                Hash = hash_uri(Uri),
                gb_trees:enter(
                  Hash, Peer#peer{last_seen = Time}, Peers);
            {value, Hash, Peer} ->
                gb_trees:enter(
                  Hash, Peer#peer{last_seen = Time}, Peers)
        end,
    {noreply, State#state{peers = NewPeers}};
handle_cast({ping_error, Uri, Time}, State) ->
    {noreply, log_ping_and_set_reping(
                error,
                fun calc_backoff_retry/1, Uri, Time, State)};
handle_cast({good_ping, Uri, Time}, State) ->
    {noreply, log_ping_and_set_reping(
                ok,
                fun set_max_retry/1, Uri, Time, State)};
handle_cast({add, Peer, Connect}, State = #state{peers = Peers})
  when is_record(Peer, peer) ->
    Key = hash_uri(Peer#peer.uri),
    case Connect andalso not has_been_seen(Key, Peers) of
        true -> spawn(fun() -> ping_peer(Peer) end);
        false -> ok
    end,
    {noreply, enter_peer(Key, Peer, State)};
handle_cast({alias, PeerUri, Alias} = _R, State = #state{aliases = As}) ->
    lager:debug("R = ~p", [_R]),
    As1 = case {peer_or_alias(Alias, State),
                peer_or_alias(PeerUri, State)} of
              {neither, P} when P == neither; P == peer ->
                  gb_trees:insert(Alias, PeerUri, As);
              {peer, neither} ->
                  gb_trees:insert(PeerUri, Alias, As);
              Other ->
                  lager:debug("cannot register alias (~p)", [Other]),
                  As
          end,
    {noreply, State#state{aliases = As1}};
handle_cast({add_and_ping, PeerUris}, State) ->
    lager:debug("add and ping peers ~p", [PeerUris]),
    #state{peers = Peers} = State1 = insert_peer_uris(PeerUris, State),
    lager:debug("known peers: ~p", [gb_trees:to_list(Peers)]),
    lists:foreach(
      fun(P) ->
              Key = hash_uri(P#peer.uri),
              case has_been_seen(Key, Peers) of
                  true -> ok;
                  false ->
                      %% TODO: use jobs workers instead
                      spawn(fun() -> ping_peer(P) end)
              end
      end, PeerUris),
    {noreply, State1};
handle_cast({remove, PeerUri}, State = #state{peers = Peers}) ->
    HashUri = hash_uri(normalize_uri(PeerUri)),
    case gb_trees:lookup(HashUri, Peers) of
        none -> {noreply, State};
       {value, Peer} ->
            {noreply, do_remove_peer(HashUri, Peer, State)}
    end.

handle_info({timeout, Ref, {ping_peer, Uri}}, State) ->
    lager:debug("got ping_peer timer msg for ~p", [Uri]),
    case lookup_peer(Uri, State) of
        none ->
            lager:debug("ping_peer timer msg for unknown uri (~p)", [Uri]),
            {noreply, State};
        {value, Hash, #peer{ping_tref = Ref} = Peer} ->
            %% TODO: use jobs workers instead
            spawn(fun() -> ping_peer(Peer) end),
            Peers = gb_trees:enter(
                      Hash, Peer#peer{ping_tref = undefined},
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

timestamp() ->
    erlang:system_time(millisecond).

-spec log_ping_error(uri()) -> ok.
log_ping_error(Uri) ->
    gen_server:cast(?MODULE, {ping_error, uri(Uri), timestamp()}).

-spec log_good_ping(uri()) -> ok.
log_good_ping(Uri) ->
    gen_server:cast(?MODULE, {good_ping, uri(Uri), timestamp()}).

-spec hash_uri(uri()) -> binary().
hash_uri(Uri) when is_list(Uri)->
    hash_uri(list_to_binary(Uri));
hash_uri(Uri) ->
    Hash = crypto:hash(md4,Uri),  %TODO add some random for execution but constant salt, so people can't mess with uri-s to hide on our list.
    <<Hash/binary, Uri/binary>>.

peer_record(Peer = #peer{}) ->
    normalize_peer(Peer);
peer_record(PeerUri) ->
    normalize_peer(#peer{uri=PeerUri}).

normalize_peer(Peer = #peer{uri = Uri}) ->
    Peer#peer{uri = normalize_uri(Uri)}.

normalize_uri(#peer{uri = Uri}) ->
    ensure_trailing_slash(Uri);
normalize_uri(Uri) ->
    ensure_trailing_slash(Uri).

ensure_trailing_slash(Uri) ->
    B = iolist_to_binary(Uri),
    Sz = byte_size(B),
    PSz = Sz - 1,
    case B of
  <<_:PSz/binary, "/">> ->
      binary_to_list(B);
  Pfx ->
      binary_to_list(<<Pfx/binary, "/">>)
    end.

get_random_n(N0, Exclude, Tree) ->
    N = case N0 of
            all -> gb_trees:size(Tree);
            N0 when is_integer(N0) -> N0
        end,
    Pruned = lists:foldl(
               fun(P, Acc) ->
                       exclude_peer(P, Acc)
               end, Tree, Exclude),
    case gb_trees:size(Pruned) of
  Sz when Sz =< N ->
      gb_trees:values(Pruned);
  Sz ->
      Ps = random_values(N, Sz),
      I = gb_trees:iterator(Pruned),
      pick_values(Ps, I)
    end.

exclude_peer(#peer{uri = Uri}, T) ->
    gb_trees:delete_any(hash_uri(Uri), T);
exclude_peer(Uri, T) when is_list(Uri); is_binary(Uri) ->
    gb_trees:delete_any(
      hash_uri(normalize_uri(Uri)), T);
exclude_peer(_, T) ->
    T.

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

pick_values(Ps, I) ->
    pick_values(Ps, 1, gb_trees:next(I), ordsets:new()).

pick_values([H|T], H, {_, V, I}, Acc) ->
    pick_values(T, H+1, gb_trees:next(I), [V|Acc]);
pick_values(Ps, N, {_, _, I}, Acc) ->
    pick_values(Ps, N+1, gb_trees:next(I), Acc);
pick_values([], _, _, Acc) ->
    Acc.

has_been_seen(Key, Peers) ->
    case gb_trees:lookup(Key, Peers) of
        none ->
            false;
        {value, Peer} ->
            Peer#peer.last_seen =/= 0
    end.

ping_peer(Peer) ->
    %% Don't ping until our own HTTP endpoint is up. This is not strictly
    %% needed for the ping itself, but given that a ping can quickly
    %% lead to a greater discovery, we should be prepared to handle pings
    %% ourselves at this point.
    await_aehttp(),
    Res = aeu_requests:ping(Peer),
    lager:debug("ping result (~p): ~p", [Peer, Res]),
    case Res of
        {ok, Map} ->
            log_good_ping(Peer#peer.uri),
            Peers = maps:get(<<"peers">>, Map, []),
            [add(P, true) || P <- Peers],
            ok;
        _ ->
            log_ping_error(Peer#peer.uri),
            ok
    end.

%% The gproc name below is registered in the start function of
%% aehttp_app, and serves as a synch point. The timeout is hopefully
%% large enough to reflect only error conditions. Expected wait time
%% should be a fraction of a second, if any.
await_aehttp() ->
    gproc:await({n,l,{epoch,app,aehttp}}, 10000). % should never timeout

lookup_peer(Uri, #state{aliases = As, peers = Peers}) ->
    case gb_trees:lookup(Uri, As) of
        none ->
            do_lookup_peer(Uri, Peers);
        {value, ToUri} ->
            do_lookup_peer(ToUri, Peers)
    end.

do_lookup_peer(Uri, Peers) ->
    Key = hash_uri(uri(Uri)),
    case gb_trees:lookup(Key, Peers) of
        none -> none;
        {value, P} ->
            {value, Key, P}
    end.

peer_or_alias(Uri, #state{aliases = As, peers = Peers}) ->
    case gb_trees:is_defined(Uri, As) of
        true ->
            alias;
        false ->
            case gb_trees:is_defined(hash_uri(Uri), Peers) of
                true ->
                    peer;
                false ->
                    neither
            end
    end.

do_remove_peer(Key, #peer{} = P,
               #state{aliases = As, peers = Peers} = State) ->
    Peers1 = gb_trees:delete(Key, Peers),
    As1 = lists:foldl(
            fun(A, T) ->
                    gb_trees:delete_any(A, T)
            end, As, aliases(uri(P), As)),
    State#state{peers = Peers1, aliases = As1}.

aliases(Uri, As) ->
    aliases(Uri, gb_trees:next(gb_trees:iterator(As)), []).

aliases(Uri, {Key, Uri, Iter2}, Acc) ->
    aliases(Uri, gb_trees:next(Iter2), [Key|Acc]);
aliases(Uri, {_, _, Iter2}, Acc) ->
    aliases(Uri, gb_trees:next(Iter2), Acc);
aliases(_, none, Acc) ->
    Acc.

enter_peer(#peer{uri = Uri} = Peer, #state{} = State) ->
    enter_peer(hash_uri(Uri), Peer, State).

enter_peer(Key, Peer, #state{aliases = As, peers = Peers} = State) ->
    PeerUri = uri(Peer),
    %% An uri shouldn't be both a peer and an alias (this would be, at best,
    %% redundant if it points to itself, and at worst, very confusing if the
    %% alias points to some other peer than the peer with the same uri.)
    As1 = gb_trees:delete_any(PeerUri, As),
    Peers1 = gb_trees:enter(Key, Peer, Peers),
    State#state{aliases = As1, peers = Peers1}.

insert_peer_uris(Uris, #state{} = S) ->
    lists:foldl(
      fun(Uri, Sx) ->
              enter_peer(peer_record(Uri), Sx)
      end, S, Uris).

log_ping_and_set_reping(Res, CalcF, Uri, Time, State) ->
    case lookup_peer(Uri, State) of
        none ->
            lager:debug("Reported ping event for unknown peer (~p)", [Uri]),
            State;
        {value, Hash, Peer} ->
            Peer1 = save_ping_event(Res, Time, Peer),
            NewTime = CalcF(Peer1),
            lager:debug("Starting re-ping timer for ~p: ~p", [Uri, NewTime]),
            TRef = erlang:start_timer(
                     NewTime, self(), {ping_peer, Uri}),
            Peer2 = save_ping_timer(TRef, Peer1),
            Peers = gb_trees:enter(
                       Hash, Peer2, State#state.peers),
            State#state{peers = Peers}
    end.

save_ping_event(Res, T, #peer{} = Peer) ->
    LastPings = case Peer#peer.last_pings of
                    []      -> [T];
                    [A]     -> [T,A];
                    [A,B|_] -> [T,A,B]
                end,
    LastSeen = case Res of
                   ok    -> T;
                   error -> Peer#peer.last_seen
               end,
    Peer#peer{last_seen = LastSeen, last_pings = LastPings}.

save_ping_timer(TRef, #peer{ping_tref = Prev} = Peer) ->
    case Prev of
        undefined -> ok;
        _ -> erlang:cancel_timer(Prev, [{info, false}])
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
    Default = {3000, 60000},
    case application:get_env(aecore, ping_interval_limits, {3000, 60000}) of
        {Min, Max} = Res when is_integer(Min),
                              is_integer(Max),
                              Max >= Min ->
            Res;
        Other ->
            lager:debug("invalid ping limits: ~p; using default (~p)",
                        [Other, Default]),
            Default
    end.
