%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module storing peers list and providing funtions for peers interaction.
%%% @end
%%%=============================================================================
-module(aec_peers).

-behaviour(gen_server).

%% API
-export([add/1,
         add/2,
         register_source/2,
         add_and_ping_peers/1,
         block_peer/1,
         unblock_peer/1,
         is_blocked/1,
         remove/1,
         info/0,
         info/1,
         all/0,
         aliases/0,
         get_random/0,
         get_random/1,
         get_random/2,
         uri/1,
         set_local_peer_uri/1,
         get_local_peer_uri/0,
         update_last_seen/1]).

-export([check_env/0]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("peers.hrl").
-define(MIN_PING_INTERVAL,   3000).
-define(MAX_PING_INTERVAL, 120000).


-type uri() :: http_uri:uri().
-type get_peer_result() :: {ok, peer()} | {error, string()}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Add peer by url or supplying full peer() record. Try to connect.
%%------------------------------------------------------------------------------
-spec add(http_uri:uri() | peer()) -> ok.
add(Peer) ->
    add(Peer, true).

%%------------------------------------------------------------------------------
%% Add peer by url or supplying full peer() record. Connect if `Connect==true`
%%------------------------------------------------------------------------------
-spec add(http_uri:uri() | peer(), boolean()) -> ok.
add(Peer, Connect) when is_boolean(Connect) ->
    gen_server:cast(?MODULE, {add, peer_record(Peer), Connect}),
    ok.

%%------------------------------------------------------------------------------
%% Register a reported source address of known peer P. Ignored if P is unknown
%%
%% This is mainly intended to handle cases where a pinging peer and the
%% pinged peer use different uris (peer uri vs "source" elem in ping obj).
%% Use of peer uri and alias will both lead to the same peer record
%% (and the 'uri' elem in the record reveals which is the origin and which
%% is the alias; this usually doesn't matter.)
%%------------------------------------------------------------------------------
-spec register_source(http_uri:uri() | peer(), uri()) -> ok.
register_source(Peer, Alias) ->
    gen_server:cast(?MODULE, {source, normalize_uri(uri(Peer)),
                              normalize_uri(Alias)}),
    ok.

%%------------------------------------------------------------------------------
%% Add peer by url or supplying full peer() record. Connect if `Connect==true`
%%------------------------------------------------------------------------------
-spec add_and_ping_peers([http_uri:uri()]) -> ok.
add_and_ping_peers(Uris) ->
    case [peer_record(Uri) || Uri <- Uris] of
        [] -> ok;
        Peers when is_list(Peers) ->
            gen_server:cast(?MODULE, {add_and_ping, Peers})
    end.

%%------------------------------------------------------------------------------
%% Block peer
%%------------------------------------------------------------------------------
-spec block_peer(http_uri:uri()) -> ok.
block_peer(Uri) ->
    gen_server:cast(?MODULE, {block, normalize_uri(uri(Uri))}),
    ok.

%%------------------------------------------------------------------------------
%% Unblock peer
%%------------------------------------------------------------------------------
-spec unblock_peer(http_uri:uri()) -> ok.
unblock_peer(Uri) ->
    gen_server:cast(?MODULE, {unblock, normalize_uri(uri(Uri))}),
    ok.

%%------------------------------------------------------------------------------
%% Check if peer is blocked
%%------------------------------------------------------------------------------
-spec is_blocked(http_uri:uri()) -> boolean().
is_blocked(Uri) ->
    gen_server:call(?MODULE, {is_blocked, normalize_uri(uri(Uri))}).

%%------------------------------------------------------------------------------
%% Remove peer by url
%%------------------------------------------------------------------------------
-spec remove(http_uri:uri()) -> ok.
remove(Uri) ->
    gen_server:cast(?MODULE, {remove, normalize_uri(uri(Uri))}),
    ok.

%%------------------------------------------------------------------------------
%% Get information on all known peers, aliases and the blocklist
%%------------------------------------------------------------------------------
-spec info() -> [{peers | aliases | blocks, list()}].
info() ->
    gen_server:call(?MODULE, info).

%%------------------------------------------------------------------------------
%% Get information (peer() record) of peer with supplied url.
%% Normally returns {ok, Peer}
%% If peer not found gives {error, "peer unknown"}
%%------------------------------------------------------------------------------
-spec info(http_uri:uri()) -> get_peer_result().
info(Uri) ->
    gen_server:call(?MODULE, {info, normalize_uri(uri(Uri))}).

%%------------------------------------------------------------------------------
%% Get list of all peers. The list may be big. Use with caution.
%% Consider using get_random instead.
%%------------------------------------------------------------------------------
-spec all() -> list(http_uri:uri()).
all() ->
    gen_server:call(?MODULE, all).

%%------------------------------------------------------------------------------
%% Get list of all aliases. The list may be big. Use with caution.
%%------------------------------------------------------------------------------
-spec aliases() -> list({http_uri:uri(), http_uri:uri()}).
aliases() ->
    gen_server:call(?MODULE, aliases).

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
-spec get_random(all | non_neg_integer(), [peer() | http_uri:uri()]) -> [peer()].
get_random(all, Exclude) when is_list(Exclude) ->
    gen_server:call(?MODULE, {get_random, all, Exclude});
get_random(N, Exclude) when is_integer(N), N >= 0, is_list(Exclude) ->
    gen_server:call(?MODULE, {get_random, N, Exclude}).

%%------------------------------------------------------------------------------
%% Get uri of peer
%%------------------------------------------------------------------------------
-spec uri(peer() | http_uri:uri()) -> http_uri:uri().
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
-spec get_local_peer_uri() -> http_uri:uri().
get_local_peer_uri() ->
    gen_server:call(?MODULE, get_local_peer_uri).

%%------------------------------------------------------------------------------
%% Update `last_seen` timestamp
%%------------------------------------------------------------------------------
-spec update_last_seen(http_uri:uri()) -> ok.
update_last_seen(Uri) ->
    gen_server:cast(?MODULE, {update_last_seen, uri(Uri), timestamp()}).

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


%%%=============================================================================
%%% gen_server functions
%%%=============================================================================

-record(state, {peers :: gb_trees:tree(binary(),peer()),
                aliases :: gb_trees:tree(uri(), uri()),
                blocked = gb_sets:new() :: gb_sets:set(uri()),
                errored = gb_sets:new() :: gb_sets:set(uri()),
                local_peer_uri :: uri(),
                local_peer_host :: string() | undefined,
                local_peer_port :: integer() | undefined}).

start_link() ->
    gen_server:start_link({local, ?MODULE} ,?MODULE, ok, []).

init(ok) ->
    LocalPeer = aeu_env:local_peer(),
    PeerUri = aeu_requests:pp_uri(LocalPeer),  %% TODO: keep datatype, not string
    do_set_local_peer_uri(
      PeerUri,
      #state{peers=gb_trees:empty(),
             aliases=gb_trees:empty()}).

handle_call(info, _From, #state{peers = Ps,
                                aliases = As,
                                blocked = Bs} = State) ->
    {reply, [{peers, gb_trees:to_list(Ps)},
             {aliases, gb_trees:to_list(As)},
             {blocked, gb_sets:to_list(Bs)}], State};
handle_call({set_local_peer_uri, PeerUri}, _From, State) ->
    case do_set_local_peer_uri(PeerUri, State) of
        {ok, State1} ->
            {reply, ok, State1};
        {error, _} = Error ->
            {reply, Error, State}
    end;
handle_call(get_local_peer_uri, _From, State) ->
    {reply, State#state.local_peer_uri, State};
handle_call({info, PeerUri}, _From, State) ->
    case lookup_peer(PeerUri, State) of
        {value, _, Peer} ->
            {reply, {ok, Peer}, State};
        none ->
            {reply, {error, "peer not found"}, State}
    end;
handle_call({is_blocked, Uri}, _From, #state{blocked = Blocked} = State) ->
    case gb_sets:is_element(Uri, Blocked) of
        true -> {reply, true, State};
        false ->
            case lookup_peer(Uri, State) of
                {value, _, #peer{blocked = true}} ->
                    %% shouldn't happen, but let's repair if it does
                    Blocked1 = gb_sets:add_element(Uri, Blocked),
                    {reply, true, State#state{blocked = Blocked1}};
                {value, _, _} ->
                    {reply, false, State};
                none -> {reply, false, State}
            end
    end;
handle_call(all, _From, State) ->
    Uris = [ uri(Peer) || Peer <-  gb_trees:values(State#state.peers) ],
    {reply, Uris, State};
handle_call(aliases, _From, State) ->
    {reply, gb_trees:to_list(State#state.aliases), State};
handle_call({get_random, N, Exclude}, _From, State) ->
    {reply, get_random_n(N, Exclude, State), State};
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
    case is_local_uri(Uri, State) of
        false ->
            {NewPeers, ActualUri} =
                case lookup_peer(Uri, State) of
                    none ->
                        %% can happen e.g. at first ping
                        Peer = start_ping_timer(
                                 fun set_max_retry/1,
                                 peer_record(Uri)),
                        {enter_peer(
                           Peer#peer{last_seen = Time}, Peers), Peer#peer.uri};
                    {value, _Hash, Peer} ->
                        Peer1 = start_ping_timer(
                                  fun set_max_retry/1,
                                  Peer),
                        {enter_peer(Peer1#peer{last_seen = Time}, Peers),
                         Peer1#peer.uri}
                end,
            Errored = update_errored(ok, ActualUri, State#state.errored),
            {noreply, State#state{peers = NewPeers, errored = Errored}};
        _Other ->
            lager:debug("Ignoring last_seen (~p): ~p", [Uri, _Other]),
            {noreply, State}
    end;
handle_cast({ping_error, Uri, Time}, State) ->
    {noreply, log_ping_and_set_reping(
                error,
                fun calc_backoff_retry/1, Uri, Time, State)};
handle_cast({good_ping, Uri, Time}, State) ->
    {noreply, log_ping_and_set_reping(
                ok,
                fun set_max_retry/1, Uri, Time, State)};
handle_cast({add, #peer{uri = Uri} = Peer0, Connect}, State0) ->
    case is_local_uri(Uri, State0) of
        false ->
            {Peer, Blocked} = check_block_status(Peer0, State0#state.blocked),
            State = State0#state{blocked = Blocked},
            Key = hash_uri(Uri),
            #state{peers = Peers1} = State1 =
                enter_peer(Key, Peer, State),
            case Connect andalso not has_been_seen(Key, Peers1) of
                true ->
                    maybe_ping_peer(Peer, State1);
                false -> ok
            end,
            {noreply, metrics(State1)};
        _Other ->
            lager:debug("Will not add peer (~p): ~p", [Uri, _Other]),
            {noreply, State0}
    end;
handle_cast({block, Uri}, State) ->
    {noreply, set_block_flag(true, Uri, State)};
handle_cast({unblock, Uri}, State) ->
    {noreply, set_block_flag(false, Uri, State)};
handle_cast({source, SrcUri, Alias} = _R,
            State = #state{peers = Peers, aliases = As}) ->
    lager:debug("R = ~p", [_R]),
    case {do_lookup_peer(Alias, Peers),
          do_lookup_peer(SrcUri, Peers)} of
        {none, {value, _Hash, _Peer}} ->
            As1 = gb_trees:enter(Alias, SrcUri, As),
            {noreply, State#state{aliases = As1}};
        {{value, Hash, Peer}, none} ->
            NewPeer = Peer#peer{uri = SrcUri},
            Peers1 = enter_peer(NewPeer,
                                gb_trees:delete(Hash, Peers)),
            As1 = gb_trees:enter(Alias, SrcUri, As),
            {noreply, metrics(State#state{peers = Peers1, aliases = As1})};
        {{value, Ha, Pa}, {value, _Hs, Ps}} ->
            Peer = if Ps#peer.last_seen < Pa#peer.last_seen ->
                           Pa;
                      true ->
                           Ps
                   end,
            Peers1 = enter_peer(
                       Peer#peer{uri = SrcUri},
                       gb_trees:delete(Ha, Peers)),
            As1 = gb_trees:enter(Alias, SrcUri, As),
            {noreply, metrics(State#state{peers = Peers1, aliases = As1})};
        _Other ->
            lager:debug("unexpected result (source): ~p", [_Other]),
            {noreply, State}
    end;
handle_cast({add_and_ping, PeerRecs}, State) ->
    lager:debug("add and ping peers ~p", [PeerRecs]),
    #state{peers = Peers} = State1 = insert_peers(PeerRecs, State),
    lager:debug("known peers: ~p", [gb_trees:to_list(Peers)]),
    lists:foreach(
      fun(P0) ->
              %% need to fetch the stored peer record to get proper block
              %% status
              case lookup_peer(P0#peer.uri, State1) of
                  none ->
                      lager:debug("Couldn't find just added ~p", [P0#peer.uri]),
                      ok;
                  {value, _Key, Peer} ->
                      case has_been_seen_(Peer) of
                          true -> ok;
                          false ->
                              maybe_ping_peer(Peer, State1)
                      end
              end
      end, PeerRecs),
    {noreply, metrics(State1)};
handle_cast({remove, PeerUri}, State = #state{peers = Peers}) ->
    HashUri = hash_uri(normalize_uri(PeerUri)),
    case gb_trees:lookup(HashUri, Peers) of
        none -> {noreply, State};
       {value, Peer} ->
            {noreply, metrics(do_remove_peer(HashUri, Peer, State))}
    end.

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

enter_peer(#peer{uri = Uri} = P, Peers) ->
    gb_trees:enter(hash_uri(Uri), P, Peers).

set_block_flag(Flag, Uri0, #state{peers = Peers,
                                  blocked = Blocked} = State)
  when is_boolean(Flag) ->
    case do_lookup_peer(Uri0, Peers) of
        none ->
            PrevInBlocked = gb_sets:is_element(Uri0, Blocked),
            Blocked1 = update_blocked(Flag, Uri0, Blocked),
            maybe_publish_block(PrevInBlocked =/= Flag, Uri0, Flag),
            State#state{blocked = Blocked1};
        {value, _Hash, #peer{uri = Uri} = Peer} ->
            PrevInBlocked = gb_sets:is_element(Uri, Blocked),
            PrevBlocked = PrevInBlocked orelse Peer#peer.blocked,
            NewPeer = Peer#peer{blocked = Flag},
            Blocked1 = update_blocked(Flag, Uri, Blocked),
            Peers1 = enter_peer(NewPeer, Peers),
            maybe_publish_block(PrevBlocked =/= Flag, Uri, Flag),
            State#state{peers = Peers1,
                        blocked = Blocked1}
    end.

update_blocked(true, Uri, Blocked) ->
    gb_sets:add_element(Uri, Blocked);
update_blocked(false, Uri, Blocked) ->
    gb_sets:del_element(Uri, Blocked).

maybe_publish_block(false, _, _) ->
    ok;
maybe_publish_block(true, Uri, Flag) ->
    Msg = if Flag -> blocked;
             true -> unblocked
          end,
    aec_events:publish(peers, {Msg, Uri}).

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

get_random_n(N0, Exclude, #state{peers = Tree0,
                                 blocked = Blocked,
                                 errored = Errored} = S) ->
    %% first, remove all blocked peers
    Tree = exclude_from_set(
             Errored,
             exclude_from_set(Blocked, Tree0, S),
             S),
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
    pick_values(Ps, 1, gb_trees:next(I), []).

pick_values([H|T], H, {_, V, I}, Acc) ->
    pick_values(T, H+1, gb_trees:next(I), [V|Acc]);
pick_values([], _, _, Acc) ->
    Acc;
pick_values(Ps, N, {_, _, I}, Acc) ->
    pick_values(Ps, N+1, gb_trees:next(I), Acc).

has_been_seen(Key, Peers) ->
    case gb_trees:lookup(Key, Peers) of
        none ->
            false;
        {value, Peer} ->
            has_been_seen_(Peer)
    end.

has_been_seen_(Peer) ->
    Peer#peer.last_seen =/= 0.

ping_peer(Peer) ->
    %% Don't ping until our own HTTP endpoint is up. This is not strictly
    %% needed for the ping itself, but given that a ping can quickly
    %% lead to a greater discovery, we should be prepared to handle pings
    %% ourselves at this point.
    case await_aehttp() of
        ok ->
            Res = aeu_requests:ping(Peer),
            lager:debug("ping result (~p): ~p", [Peer, Res]),
            case Res of
                {ok, Map} ->
                    log_good_ping(Peer#peer.uri),
                    Peers = maps:get(<<"peers">>, Map, []),
                    add_and_ping_peers(Peers);
                _ ->
                    log_ping_error(Peer#peer.uri)
            end;
        {error, timeout} ->
            lager:debug("timeout waiting for aehttp - no ping (~p) will retry", [Peer#peer.uri]),
            ping_peer(Peer)
    end.

%% The gproc name below is registered in the start function of
%% aehttp_app, and serves as a synch point. The timeout is hopefully
%% large enough to reflect only error conditions. Expected wait time
%% should be a fraction of a second, if any.
await_aehttp() ->
    try
      gproc:await({n,l,{epoch,app,aehttp}}, 10000), % should (almost) never timeout
      ok
    catch error:{_Reason, _Args} ->
      {error, timeout}
    end.

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

do_remove_peer(Key, #peer{uri = Uri} = P,
               #state{aliases = As, peers = Peers,
                      blocked = Blocked} = State) ->
    Peers1 = gb_trees:delete(Key, Peers),
    As1 = lists:foldl(
            fun(A, T) ->
                    gb_trees:delete_any(A, T)
            end, As, aliases(uri(P), As)),
    State#state{peers = Peers1, aliases = As1,
                blocked = gb_sets:del_element(Uri, Blocked)}.

aliases(Uri, As) ->
    aliases(Uri, gb_trees:next(gb_trees:iterator(As)), []).

aliases(Uri, {Key, Uri, Iter2}, Acc) ->
    aliases(Uri, gb_trees:next(Iter2), [Key|Acc]);
aliases(Uri, {_, _, Iter2}, Acc) ->
    aliases(Uri, gb_trees:next(Iter2), Acc);
aliases(_, none, Acc) ->
    Acc.

enter_peer(_Key, Peer, #state{aliases = As, peers = Peers} = State) ->
    PeerUri = uri(Peer),
    %% An uri shouldn't be both a peer and an alias (this would be, at best,
    %% redundant if it points to itself, and at worst, very confusing if the
    %% alias points to some other peer than the peer with the same uri.)
    As1 = gb_trees:delete_any(PeerUri, As),
    Peers1 = enter_peer(Peer, Peers),
    State#state{aliases = As1, peers = Peers1}.

insert_peers(PRecs, #state{} = S) ->
    lists:foldl(
      fun(#peer{uri = Uri} = P, #state{} = Sx) ->
              case is_local_uri(Uri, Sx) orelse is_blocked(P, Sx) of
                  false ->
                      try_insert_peer(P, Sx);
                  _Other ->
                      lager:debug("Don't insert peer (~p): ~p", [Uri, _Other]),
                      Sx
              end
      end, S, PRecs).

try_insert_peer(#peer{uri = Uri} = P0,
                #state{aliases = As, peers = Peers} = S) ->
    Hash = hash_uri(Uri),
    case gb_trees:lookup(Hash, Peers) of
        none ->
            case gb_trees:lookup(Uri, As) of
                none ->
                    {P, Blocked} = check_block_status(P0, S#state.blocked),
                    Peers1 = gb_trees:insert(Hash, P, Peers),
                    S#state{peers = Peers1, blocked = Blocked};
                {value, _} ->
                    %% known alias; don't insert as peer
                    S
            end;
        _ ->
            S
    end.

check_block_status(#peer{uri = Uri} = P, Blocked) ->
    Preblocked = gb_sets:is_member(Uri, Blocked),
    Blocked1 = if P#peer.blocked andalso not Preblocked ->
                       gb_sets:add_element(Uri, Blocked);
                  true ->
                       Blocked
               end,
    BlockFlag = P#peer.blocked orelse Preblocked,
    {P#peer{blocked = BlockFlag}, Blocked1}.

is_blocked(Peer, #state{blocked = Blocked} = State) ->
    Uri = uri(Peer),
    lager:debug("Check for blocked ~p in ~p\n", [Uri, State]), 
    %% Check whether it is an alias and if so, check whether it is blocked.
    gb_sets:is_element(Uri, Blocked).


log_ping_and_set_reping(Res, CalcF, Uri, Time, State) ->
    case lookup_peer(Uri, State) of
        none ->
            lager:debug("Reported ping event for unknown peer (~p)", [Uri]),
            State;
        {value, _Hash, Peer} ->
            update_ping_metrics(Res),
            Peer1 = save_ping_event(Res, Time, Peer),
            Peer2 = start_ping_timer(CalcF, Peer1),
            Peers = enter_peer(Peer2, State#state.peers),
            Errored = update_errored(Res, Peer#peer.uri, State#state.errored),
            State#state{peers = Peers, errored = Errored}
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


start_ping_timer(_, #peer{blocked = true} = Peer) ->
    %% Don't ping blocked peers
    Peer;
start_ping_timer(CalcF, #peer{uri = Uri} = Peer) ->
    NewTime = CalcF(Peer),
    lager:debug("Starting re-ping timer for ~p: ~p", [Uri, NewTime]),
    TRef = erlang:start_timer(
             NewTime, self(), {ping_peer, Uri}),
    save_ping_timer(TRef, Peer).

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
        _ -> try erlang:cancel_timer(Prev, [{async,true}, {info, false}])
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

maybe_ping_peer(#peer{uri = Uri, blocked = true}, _) ->
    %% never ping a blocked peer
    lager:debug("Peer ~p is blocked - will not ping", [Uri]),
    ignore;
maybe_ping_peer(#peer{uri = Uri} = Peer, #state{} = State) ->
    case is_local_uri(Uri, State) of
        false ->
            lager:debug("will ping peer ~p", [Uri]),
            aec_sync:schedule_ping(Peer, fun ping_peer/1);
        _Other ->
            lager:debug("will not ping ~p (~p)", [Uri, _Other]),
            ignore
    end.

is_local_uri(Uri, #state{local_peer_host = PH,
                         local_peer_port = PP}) ->
    R = case aeu_requests:parse_uri(Uri) of
            {_, Host, PP} ->
                lists:member(Host, ["localhost", "127.0.0.1", PH]);
            {_, _, _} ->
                false;
            error ->
                error
        end,
    lager:debug("is_local_uri(~p) -> ~p (~p, ~p)", [Uri, R, PH, PP]),
    R.

do_set_local_peer_uri(Uri, State) ->
    case aeu_requests:parse_uri(Uri) of
        {_, Host, Port} ->
            {ok, State#state{local_peer_uri = Uri,
                             local_peer_host = Host,
                             local_peer_port = Port}};
        error ->
            {error, {cannot_parse, Uri}}
    end.
