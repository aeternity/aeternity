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
         remove/1,
         info/1,
         all/0,
         get_random/0,
         uri_from_ip_port/2,
         uri/1]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("peers.hrl").

-type get_peer_result() :: {ok, peer()} | {error, string()}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Add peer by url or supplying full peer() record.
%%------------------------------------------------------------------------------
-spec add(http_uri:uri() | peer()) -> ok.
add(Peer) when is_record(Peer, peer) ->
    gen_server:cast(?MODULE, {add, Peer}),
    ok;
add(PeerUri) ->
    Peer = #peer{uri=PeerUri, last_seen=erlang:system_time()},
    add(Peer).

%%------------------------------------------------------------------------------
%% Remove peer by url
%%------------------------------------------------------------------------------
-spec remove(http_uri:uri()) -> ok.
remove(PeerUri) ->
    gen_server:cast(?MODULE, {remove, PeerUri}),
    ok.

%%------------------------------------------------------------------------------
%% Get information (peer() record) of peer with supplied url.
%% Normally returns {ok, Peer}
%% If peer not found gives {error, "peer unknown"}
%%------------------------------------------------------------------------------
-spec info(http_uri:uri()) -> get_peer_result().
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
%% Get url from IP and port. IP format: xxx.xxx.xxx.xxx
%%------------------------------------------------------------------------------
-spec uri_from_ip_port(IP :: string(), Port :: number()) -> http_uri:uri().
uri_from_ip_port(IP, Port) ->
    "http://" ++ IP ++ ":" ++ integer_to_list(Port) ++ "/".

%%------------------------------------------------------------------------------
%% Get uri of peer
%%------------------------------------------------------------------------------
-spec uri(peer()) -> http_uri:uri().
uri(Peer) ->
    Peer#peer.uri.

%%%=============================================================================
%%% gen_server functions
%%%=============================================================================

-record(state, {peers :: gb_trees:tree(binary(),peer())}).

start_link() ->
    gen_server:start_link({local, ?MODULE} ,?MODULE, ok, []).

init(ok) ->
    {ok, #state{peers=gb_trees:empty()}}.

handle_call({info, PeerUri}, _From, State) -> 
    Key = hash_uri(PeerUri),
    case gb_trees:is_defined(Key, State#state.peers) of
        true ->
            Peer = gb_trees:get(Key, State#state.peers),
            {reply, {ok, Peer}, State};
        false ->
            {reply, {error, "peer not found"}, State}
    end;
handle_call(all, _From, State) -> 
    {reply, gb_trees:values(State#state.peers), State};
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

handle_cast({add, Peer}, State) when is_record(Peer, peer) ->
    NewPeers = gb_trees:enter(hash_uri(Peer#peer.uri), Peer, State#state.peers),
    {noreply, State#state{peers=NewPeers}};
handle_cast({remove, PeerUri}, State) ->
    NewPeers = gb_trees:delete_any(hash_uri(PeerUri), State#state.peers),
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

-spec hash_uri(http_uri:uri()) -> binary().
hash_uri(Uri) when is_list(Uri)->
    hash_uri(list_to_binary(Uri));
hash_uri(Uri) ->
    Hash = crypto:hash(md4,Uri),  %TODO add some random for execution but constant salt, so people can't mess with uri-s to hide on our list.
    <<Hash/binary, Uri/binary>>.
