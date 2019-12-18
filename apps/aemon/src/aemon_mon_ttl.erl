-module(aemon_mon_ttl).

-behaviour(gen_server).

-export([publisher_tx/2]).
-export([on_chain_tx/1, on_chain_height/1]).

-export([start_link/0]).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {  publisher_height = 0
                , publisher_txs = #{} :: #{SignedTxHash::binary() := TxTtl::pos_integer() }
               }).

publisher_tx(Height, Tx) ->
    gen_server:cast(?MODULE, {tx, Height, Tx}).

on_chain_tx(TxHash) ->
    gen_server:cast(?MODULE, {on_chain_tx, TxHash}).

on_chain_height(Height) ->
    gen_server:cast(?MODULE, {on_chain_height, Height}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callback


init(_) ->
    aemon_metrics:create(ttl),
    {ok, #state{}}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast({tx, Height, SignTx}, St0) ->
    St = txs_size_metric(Height, St0),
    Hash = aetx_sign:hash(SignTx),
    TTL = aetx:ttl(aetx_sign:tx(SignTx)),
    NewTxs = maps:put(Hash, TTL, St#state.publisher_txs),
    {noreply, St#state{publisher_txs = NewTxs}};

handle_cast({on_chain_tx, TxHash}, St = #state{publisher_txs = Txs}) ->
    NewTxs = maps:remove(TxHash, Txs),
    {noreply, St#state{publisher_txs = NewTxs}};

handle_cast({on_chain_height, Height}, St = #state{publisher_txs = Txs}) ->
    NewTxs = maps:filter(fun(_, AccHeight) -> AccHeight >= Height end, Txs),
    TTLExpired = maps:size(Txs) - maps:size(NewTxs),
    aemon_metrics:ttl_expired(TTLExpired),
    {noreply, St#state{publisher_txs = NewTxs}};

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Msg, St) ->
    {noreply, St}.

%% helpers

txs_size_metric(Height, State = #state{publisher_height = PHeight}) when Height > PHeight ->
    Size =  maps:size(State#state.publisher_txs),
    aemon_metrics:queue_size(Size),
    State#state{publisher_height = Height};
txs_size_metric(_, State) ->
    State.

