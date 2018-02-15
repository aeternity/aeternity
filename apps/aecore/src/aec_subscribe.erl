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
         unsubscribe/2,
         unsubscribe_all/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(sub, { chain = [], chain_tx = [],
               oracle_query = [], oracle_response = [] }).
-record(state, { subscribed = #sub{} }).

-type id() :: {ws, ws_handler:id()}.
-type aeo_event() :: {query, aeo_oracles:id()}
                   | {response, aeo_query:id()}.
-type event() :: {oracle, aeo_event()}.

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

-spec unsubscribe_all(id()) -> ok.
unsubscribe_all(Id) ->
    gen_server:cast(?SERVER, {unsubscribe, Id}).

-spec unsubscribe(id(), event()) -> ok.
unsubscribe(Id, Event) ->
    gen_server:cast(?SERVER, {unsubscribe, Id, Event}).

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
handle_cast({unsubscribe, Id, Event}, S = #state{ subscribed = Sub }) ->
    {noreply, S#state{ subscribed = del_subscribed(Id, Event, Sub) }};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, Event, #{ info := Block }},
            State = #state{ subscribed = Subs })
        when Event == top_changed; Event == block_created ->
    notify_chain_subscribers(Event, Block, Subs),
    notify_tx_subscribers(aec_blocks:txs(Block), Subs),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- Internal funs ----------------------------------------------------------
add_subscribed(Id, {chain, Event}, Sub = #sub{ chain = Cs }) ->
    Sub#sub{ chain = [{Id, Event} | Cs] };
add_subscribed(Id, {chain_tx, Event}, Sub = #sub{ chain_tx = CTs }) ->
    Sub#sub{ chain_tx = [{Id, Event} | CTs] };
add_subscribed(Id, Event = {oracle, {query, _}}, Sub = #sub{ oracle_query = OQs }) ->
    Sub#sub{ oracle_query = [{Id, Event} | OQs] };
add_subscribed(Id, Event = {oracle, {response, _}}, Sub = #sub{ oracle_response = ORs }) ->
    Sub#sub{ oracle_response = [{Id, Event} | ORs] };
add_subscribed(Id, Event, Sub) ->
    lager:error("Unhandled subscription event kind ~p from ~p", [Event, Id]),
    Sub.

del_subscribed(Id, Sub = #sub{ chain = Cs, chain_tx = Ts,
                               oracle_query = OQs, oracle_response = ORs }) ->
    NotId = fun({IdX, _}) -> IdX =/= Id end,
    Sub#sub{ chain           = lists:filter(NotId, Cs),
             chain_tx        = lists:filter(NotId, Ts),
             oracle_query    = lists:filter(NotId, OQs),
             oracle_response = lists:filter(NotId, ORs) }.

del_subscribed(Id, {chain, Event}, Sub = #sub{ chain = Cs }) ->
    Sub#sub{ chain = lists:delete({Id, Event}, Cs) };
del_subscribed(Id, {chain_tx, Event}, Sub = #sub{ chain_tx = Ts }) ->
    Sub#sub{ chain_tx = lists:delete({Id, Event}, Ts) };
del_subscribed(Id, Event = {oracle, {query, _}}, Sub = #sub{ oracle_query = OQs }) ->
    Sub#sub{ oracle_query = lists:delete({Id, Event}, OQs) };
del_subscribed(Id, Event = {oracle, {response, _}}, Sub = #sub{ oracle_response = ORs }) ->
    Sub#sub{ oracle_response = lists:delete({Id, Event}, ORs) };
del_subscribed(Id, Event, Sub) ->
    lager:error("Unhandled unsubscribe event kind ~p from ~p", [Event, Id]),
    Sub.

notify_tx_subscribers([], _Sub) ->
    ok;
notify_tx_subscribers([SignedTx | Rest], Sub) ->
    Tx = aetx_sign:tx(SignedTx),
    notify_tx(Tx, Sub#sub.chain_tx),
    case aetx:tx_type(Tx) of
        <<"aeo_query_tx">>    -> aeo_subscription:notify_query_tx(Tx, Sub#sub.oracle_query);
        <<"aeo_response_tx">> -> aeo_subscription:notify_response_tx(Tx, Sub#sub.oracle_response);
        _Other                -> ok
    end,
    notify_tx_subscribers(Rest, Sub).

notify_chain_subscribers(Event, Block, #sub{ chain = Cs }) ->
    BlockHeight = aec_blocks:height(Block),
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    case Event of
        block_created ->
            [ Ws ! {event, mined_block, {BlockHeight, BlockHash}}
              || {{ws, Ws}, mined_block} <- Cs ];
        top_changed -> %% New block from another node
            ok
    end,
    [ Ws ! {event, new_block, {BlockHeight, BlockHash}}
      || {{ws, Ws}, new_block} <- Cs ],
    ok.

notify_tx(_Tx, []) -> ok;
notify_tx(Tx, Subs) ->
    TxHash = aetx:hash(Tx),
    [ Ws ! {event, chain_tx, TxHash}
      || {{ws, Ws}, {tx, TxHash1}} <- Subs, TxHash == TxHash1 ],
    ok.

