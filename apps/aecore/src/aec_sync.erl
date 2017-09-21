%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module storing peers list and providing funtionc for peers interaction
%%% @end
%%%=============================================================================
-module(aec_sync).

-behaviour(gen_server).
-compile({parse_transform, lager_transform}).
-include("peers.hrl").

%% API
-export([connect_peer/1,
         ping_from_remote/1]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
	 handle_info/2,  terminate/2, code_change/3]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec connect_peer(http_uri:uri()) -> ok.
connect_peer(Uri) ->
    gen_server:cast(?MODULE, {connect, Uri}).

-spec ping_from_remote(http_uri:uri()) -> ok.
ping_from_remote(Uri) ->
    gen_server:cast(?MODULE, {ping_from, Uri}).

%%%=============================================================================
%%% gen_server functions
%%%=============================================================================

-record(state, {peers = gb_trees:empty(),
                pings = gb_trees:empty()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(_, _From, State) ->
    {reply, error, State}.

handle_cast({connect, Uri}, State) ->
    case aec_peers:info(Uri) of
        {error, _} ->
            {noreply, ping_peer(Uri, State)};
        {ok, _} ->
            %% already connected
            {noreply, State}
    end;
handle_cast({ping_from, Uri}, State) ->
    aec_peers:add(Uri, false),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({aec_peers, peer_added, Peer}, State) ->
    {noreply, ping_peer(Peer, State)};
handle_info({aec_peers, peer_removed, Peer}, State) ->
    NewPeers = gb_trees:delete_any(Peer#peer.uri, State#state.peers),
    {noreply, State#state{peers = NewPeers}};
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

ping_peer(Peer, State) ->
    Res = aeu_requests:ping(Peer),
    lager:debug("Ping result (~p): ~p", [Peer, Res]),
    case Res of
        {ok, pong} ->
            aec_peers:add(Peer);
        _ ->
            ok
    end,
    State.
