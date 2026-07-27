%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Chain state version marker attached to HTTP API responses.
%%%
%%%    A client talking to a pool of nodes behind a load balancer cannot tell a
%%%    response computed from an up-to-date chain from one computed by a lagging
%%%    or still syncing node. Every API response therefore carries the height of
%%%    the node's chain top, which lets a client discard answers from a node
%%%    that is behind a state it has already observed.
%%%
%%%    The marker is read *before* the request handler reads any chain data, so
%%%    the body always reflects a chain state at or after the advertised height,
%%%    never before it. Clients may treat the value as a lower bound.
%%%
%%%    Resolution is one generation: micro blocks applied within the current
%%%    generation do not bump it. That is the deliberate trade-off from GH-4186
%%%    - a marker that changes rarely stays cache-friendly for reverse proxies
%%%    in front of the node, and computing a micro block position would cost a
%%%    height index lookup plus a walk back to the key block on every request.
%%% @end
%%%=============================================================================
-module(aehttp_state_version).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , header_name/0
        , set_resp_header/1
        , top_height/0
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%% Not RFC 6648 clean, but the name agreed on in GH-4186 and the one SDKs look
%% for. Cowboy expects response header names lowercased.
-define(HEIGHT_HEADER, <<"x-ae-height">>).
-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).
-define(HEIGHT_KEY, height).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec header_name() -> binary().
header_name() ->
    ?HEIGHT_HEADER.

%% @doc Attach the state version marker to the response headers of Req.
%%
%% The marker is left out when the chain top cannot be read - during startup
%% the chain tables are not readable yet, and no marker is a better answer than
%% a wrong one. Clients are expected to treat a missing header as "unknown".
-spec set_resp_header(cowboy_req:req()) -> cowboy_req:req().
set_resp_header(Req) ->
    case top_height() of
        undefined ->
            Req;
        Height ->
            cowboy_req:set_resp_header(?HEIGHT_HEADER,
                                       integer_to_binary(Height), Req)
    end.

%% @doc Height of the node's chain top, or undefined if it is not known yet.
%%
%% Hot path: a single ETS read of the value this server caches from
%% `top_changed' events, so the per-request cost is O(1) with no dirty chain
%% read or header decode. Falls back to a one-off dirty read while the cache is
%% not populated (startup, or the maintainer process restarting). Micro block
%% headers carry the height of their generation, so this is the current
%% generation height regardless of the top block type.
-spec top_height() -> undefined | aec_blocks:height().
top_height() ->
    try ets:lookup(?TAB, ?HEIGHT_KEY) of
        [{?HEIGHT_KEY, Height}] -> Height;
        []                      -> dirty_top_height()
    catch
        %% Table not created yet (this server not started). Fall back so the
        %% marker still works even if the cache is unavailable.
        error:badarg -> dirty_top_height()
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    _ = ets:new(?TAB, [named_table, public, {read_concurrency, true}]),
    %% aec_events:subscribe/1 returns `true', not `ok'.
    true = aec_events:subscribe(top_changed),
    %% Seed from the current top so the header is populated before the first
    %% `top_changed' after start, rather than falling back on every early request.
    case dirty_top_height() of
        undefined -> ok;
        Height    -> ets:insert(?TAB, {?HEIGHT_KEY, Height})
    end,
    {ok, #{}}.

handle_call(_Req, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% One generation resolution: every top change (key or micro) carries the
%% generation height, so caching it here keeps the marker a cheap ETS read.
handle_info({gproc_ps_event, top_changed, #{info := #{height := Height}}}, State) ->
    true = ets:insert(?TAB, {?HEIGHT_KEY, Height}),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

%% Dirty read of the chain top height; returns undefined if the chain tables
%% are not readable (e.g. during startup) or aec_chain errors.
-spec dirty_top_height() -> undefined | aec_blocks:height().
dirty_top_height() ->
    try aec_chain:dirty_top_header() of
        undefined -> undefined;
        Header    -> aec_headers:height(Header)
    catch
        Class:Reason ->
            lager:debug("Chain top not readable for X-Ae-Height: ~p:~p", [Class, Reason]),
            undefined
    end.
