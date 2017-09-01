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
         uri_from_ip_port/2]).

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

add(Peer) when is_record(Peer, peer)->
    ok; %TODO implement
add(PeerUri)->
    ok. %TODO implement
%%------------------------------------------------------------------------------
%% Remove peer by url
%%------------------------------------------------------------------------------
-spec remove(http_uri:uri()) -> ok.
remove(PeerUri) ->
    ok. %TODO implement

%%------------------------------------------------------------------------------
%% Get information (peer() record) of peer with supplied url.
%%------------------------------------------------------------------------------
-spec info(http_uri:uri()) -> get_peer_result().
info(PeerUri) ->
    {error, "unimplemented"}. %TODO implement

%%------------------------------------------------------------------------------
%% Get list of all peers. The list may be big. Use with caution.
%% Consider using get_random instead.
%%------------------------------------------------------------------------------
-spec all() -> list(peer()).
all() ->
    []. %TODO implement

%%------------------------------------------------------------------------------
%% Get random peer. This may return error when no peer is known to us (unlikely).
%%------------------------------------------------------------------------------
-spec get_random() -> get_peer_result().
get_random() ->
    {error, "unimplemented"}. %TODO implement

%%------------------------------------------------------------------------------
%% Get url from IP and port. IP format: xxx.xxx.xxx.xxx
%%------------------------------------------------------------------------------
-spec uri_from_ip_port(IP :: string(), Port :: number()) -> http_uri:uri().
uri_from_ip_port(IP, Port) ->
    "http://" ++ IP ++ ":" ++ integer_to_list(Port) ++ "/".

%%%=============================================================================
%%% gen_server functions
%%%=============================================================================

-record(state, {peers :: dict:dict(http_uri:uri(),peer())}).

start_link() ->
    gen_server:start_link({local, ?MODULE} ,?MODULE, ok, []).

init(ok) ->
    {ok, #state{peers=dict:new()}}.

handle_call({info, PeerUri}, _From, State) -> {reply, #peer{}, State}; %TODO implement
handle_call(all, _From, State) -> {reply, [], State}; %TODO implement
handle_call(get_random, _From, State) -> {reply, {error, "unimplemented"}, State}. %TODO implement

handle_cast({add, Peer}, State) when is_record(Peer, peer) ->
    {noreply, State}; %TODO implement
handle_cast({add, PeerUrl}, State) ->
    {noreply, State}; %TODO implement
handle_cast({remove, PeerUrl}, State) ->
    {noreply, State}. %TODO implement

handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

