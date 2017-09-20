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
%% Remove peer by url
%%------------------------------------------------------------------------------
-spec remove(uri()) -> ok.
remove(PeerUri) ->
    gen_server:cast(?MODULE, {remove, PeerUri}),
    ok.

%%------------------------------------------------------------------------------
%% Get information (peer() record) of peer with supplied url.
%% Normally returns {ok, Peer}
%% If peer not found gives {error, "peer unknown"}
%%------------------------------------------------------------------------------
-spec info(uri()) -> get_peer_result().
info(PeerUri) ->
    gen_server:call(?MODULE, {info, PeerUri}).

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
-spec get_random(non_neg_integer()) -> [peer()].
get_random(N) when is_integer(N), N >= 0 ->
    get_random(N, []).

%%------------------------------------------------------------------------------
%% Get up to N random peers, but not peers included in the Exclude list
%%
%% The peers are randomly distributed in the sorted gb_tree (due to the use of hash_uri),
%% so we can find a random peer by choosing a point and getting the next peer in gb_tree.
%% That's what this function does
%%------------------------------------------------------------------------------
-spec get_random(non_neg_integer(), [peer() | uri()]) -> [peer()].
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
    gen_server:cast(?MODULE, {update_last_seen, uri(Uri), erlang:system_time()}).

%%%=============================================================================
%%% gen_server functions
%%%=============================================================================

-record(state, {peers :: gb_trees:tree(binary(),peer()),
    local_peer_uri :: uri()}).

start_link() ->
    gen_server:start_link({local, ?MODULE} ,?MODULE, ok, []).

init(ok) ->
    {ok, #state{peers=gb_trees:empty()}}.

handle_call({set_local_peer_uri, Peer}, _From, State) ->
    {reply, ok, State#state{local_peer_uri = Peer}};
handle_call(get_local_peer_uri, _From, State) ->
    {reply, State#state.local_peer_uri, State};
handle_call({info, PeerUri}, _From, State) -> 
    Key = hash_uri(PeerUri),
    case gb_trees:lookup(Key, State#state.peers) of
        {value, Peer} ->
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
    Hash = hash_uri(Uri),
    NewPeers = case gb_trees:lookup(Hash, Peers) of
                   none -> Peers;
                   {value, Peer} ->
                       gb_trees:update(
                         Hash, Peer#peer{last_seen = Time}, Peers)
               end,
    {noreply, State#state{peers = NewPeers}};
handle_cast({add, Peer, Connect}, State = #state{peers = Peers})
  when is_record(Peer, peer) ->
    Key = hash_uri(Peer#peer.uri),
    case Connect andalso not has_been_seen(Key, Peers) of
        true -> spawn(fun() -> ping_peer(Peer) end);
        false -> ok
    end,
    NewPeers = gb_trees:enter(hash_uri(Peer#peer.uri), Peer, Peers),
    {noreply, State#state{peers=NewPeers}};
handle_cast({remove, PeerUri}, State = #state{peers = Peers}) ->
    HashUri = hash_uri(normalize_uri(PeerUri)),
    NewPeers = case gb_trees:lookup(HashUri, Peers) of
       none -> Peers;
       {value, _} ->
           gb_trees:delete(HashUri, Peers)
         end,
    {noreply, State#state{peers=NewPeers}}.

handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec hash_uri(uri()) -> binary().
hash_uri(Uri) when is_list(Uri)->
    hash_uri(list_to_binary(Uri));
hash_uri(Uri) ->
    Hash = crypto:hash(md4,Uri),  %TODO add some random for execution but constant salt, so people can't mess with uri-s to hide on our list.
    <<Hash/binary, Uri/binary>>.

peer_record(Peer = #peer{}) ->
    Peer;
peer_record(PeerUri) ->
    normalize_peer(#peer{uri=PeerUri}).

normalize_peer(Peer = #peer{uri = Uri}) ->
    Peer#peer{uri = normalize_uri(Uri)}.

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

get_random_n(N, Exclude, Tree) ->
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
    Res = aeu_requests:ping(Peer),
    lager:debug("ping result (~p): ~p", [Peer, Res]),
    case Res of
        {ok, #{<<"pong">> := <<"pong">>} = Map} ->
            update_last_seen(Peer),
            Peers = maps:get(<<"peers">>, Map, []),
            [add(P, true) || P <- Peers],
            ok;
        _ ->
            ok
    end.
