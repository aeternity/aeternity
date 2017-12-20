%%% @copyright (C) 2018, Aeternity
%%% @doc
%%%     Subscription/External event manager for AE node
%%% @end
%%%-------------------------------------------------------------------
-module(aec_subscribe).

-behaviour(gen_server).

%% API
-export([start_link/0,
         stop/0,
         subscribe/2,
         unsubscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(sub, { oracle_query = [], oracle_response = [] }).
-record(state, { subscribed = #sub{} }).

-type id() :: {ws, ws_handler:id()}.
-type aeo_event() :: {query, aeo_oracles:id()}
                   | {response,  aeo_oracles:id(), aeo_interaction:id()}.
-type event() :: {aeo, aeo_event()}.

-export_type([event/0, id/0]).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

-spec subscribe(id(), event()) -> ok.
subscribe(Id, Event) ->
    gen_server:cast(?SERVER, {subscribe, Id, Event}).

-spec unsubscribe(id()) -> ok.
unsubscribe(Id) ->
    gen_server:cast(?SERVER, {unsubscribe, Id}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    lager:info("Initializing subscription/event manager"),
    %% TODO: Think about the startup/sync phase...
    aec_events:subscribe(top_changed),
    aec_events:subscribe(block_created),
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({subscribe, Id, Event}, S = #state{ subscribed = Sub }) ->
    {noreply, S#state{ subscribed = add_subscribed(Id, Event, Sub) }};
handle_cast({unsubscribe, Id}, S = #state{ subscribed = Sub }) ->
    {noreply, S#state{ subscribed = del_subscribed(Id, Sub) }};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, Event, #{ info := Block }}, State = #state{ subscribed = Subs })
    when Event == top_changed; Event == block_created ->
    case Subs of
        [] -> ok;
        _  -> notify_subscribers(aec_blocks:txs(Block), Subs)
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- Internal funs ----------------------------------------------------------
add_subscribed(Id, Event = {aeo, {query, _}}, Sub = #sub{ oracle_query = OQs }) ->
    Sub#sub{ oracle_query = [{Id, Event} | OQs] };
add_subscribed(Id, Event = {aeo, {response, _}}, Sub = #sub{ oracle_response = ORs }) ->
    Sub#sub{ oracle_response = [{Id, Event} | ORs] };
add_subscribed(Id, Event, Sub) ->
    lager:error("Unhandled subscription event kind ~p from ~p", [Event, Id]),
    Sub.

del_subscribed(Id, Sub = #sub{ oracle_query = OQs, oracle_response = ORs }) ->
    NotId = fun({IdX, _}) -> IdX =/= Id end,
    Sub#sub{ oracle_query    = lists:filter(NotId, OQs),
             oracle_response = lists:filter(NotId, ORs) }.

notify_subscribers([], _Sub) ->
    ok;
notify_subscribers([SignedTx | Rest], Sub) ->
    Tx = aec_tx_sign:data(SignedTx),
    case aec_tx_dispatcher:handler(Tx) of
        aeo_query_tx    -> aeo_subscription:notify_query_tx(Tx, Sub#sub.oracle_query);
        aeo_response_tx -> aeo_subscription:notify_response_tx(Tx, Sub#sub.oracle_response);
        _Other          -> ok
    end,
    notify_subscribers(Rest, Sub).

