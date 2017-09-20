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
-export([connect_peer/1]).

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
    aec_peers:add(Uri, _Connect = true),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

