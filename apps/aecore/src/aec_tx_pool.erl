%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Memory pool of unconfirmed transactions.
%%%
%%% Unconfirmed transactions are transactions not included in any
%%% block in the longest chain.
%%%
%%% @TODO Minimize space used by key of transaction in storage.
%%% @TODO Limit number of transactions stored considering memory usage.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_tx_pool).

-behaviour(gen_server).

-include("common.hrl").
-include("txs.hrl").

%% API
-export([start_link/0,
         stop/0]).
-export([push/1,
         delete/1,
         peek/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {txs :: pool_db()}).

-type negated_fee() :: non_pos_integer().
-type non_pos_integer() :: neg_integer() | 0.

-type pool_db_key() ::
        {negated_fee(), %% Negated fee - not fee - in order to sort by decreasing fee.
         tx()}. %% TODO Minimize e.g.: (1) hash of tx; (2) initiating account and its nonce.
-type pool_db_value() :: signed_tx().
-type pool_db() :: gb_trees:tree(pool_db_key(), pool_db_value()).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%% Ensure the specified transaction is stored in the pool.
%%
%% This function is synchronous in order to apply backpressure on the
%% client hence attempting to prevent growing message queue for the
%% pool process.
-spec push(signed_tx()) -> ok.
push(Tx) ->
    gen_server:call(?SERVER, {push, Tx}).

%% Ensure the specified transaction is not stored in the pool.
%%
%% This function is synchronous in order to apply backpressure on the
%% client hence attempting to prevent growing message queue for the
%% pool process.
-spec delete(signed_tx()) -> ok.
delete(Tx) ->
    gen_server:call(?SERVER, {delete, Tx}).

%% Read from the pool the transactions with highest fee - up to the
%% specified number of transactions.  The transactions are returned in
%% order of decreasing fee.
%%
%% The specified maximum number of transactions avoids requiring
%% building in memory the complete list of all transactions in the
%% pool.
-spec peek(MaxNumberOfTxs::pos_integer()) -> {ok, [signed_tx()]}.
peek(MaxNumberOfTxs) ->
    gen_server:call(?SERVER, {peek, MaxNumberOfTxs}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% No `process_flag(trap_exit, true)` because no cleaning up
    %% needed in `terminate/2`.
    {ok, Db} = pool_db_open(),
    State = #state{txs = Db},
    {ok, State}.

handle_call({push, Tx}, _From, State) ->
    {ok, NewDb} = pool_db_put(State#state.txs, pool_db_key(Tx), Tx),
    NewState = State#state{txs = NewDb},
    Reply = ok,
    {reply, Reply, NewState};
handle_call({delete, Tx}, _From, State) ->
    {ok, NewDb} = pool_db_delete(State#state.txs, pool_db_key(Tx)),
    NewState = State#state{txs = NewDb},
    Reply = ok,
    {reply, Reply, NewState};
handle_call({peek, MaxNumberOfTxs}, _From, State)
  when is_integer(MaxNumberOfTxs), MaxNumberOfTxs > 0 ->
    {ok, Txs} = pool_db_peek(State#state.txs, MaxNumberOfTxs),
    Reply = {ok, Txs},
    {reply, Reply, State};
handle_call(Request, From, State) ->
    lager:warning("Ignoring unknown call request from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("Ignoring unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

pool_db_key(SignedTx) ->
    Tx = aec_tx_sign:data(SignedTx),
    {-aec_tx:fee(Tx), Tx}.

-spec pool_db_open() -> {ok, pool_db()}.
pool_db_open() ->
    {ok, gb_trees:empty()}.

-spec pool_db_peek(pool_db(), MaxNumber::pos_integer()) ->
                          {ok, [pool_db_value()]}.
pool_db_peek(Db, MaxN) ->
    %% Do not require the complete list of all elements to be built in
    %% memory at one time.
    GbTreesPeekFun =
        fun
            F(_, 0, AccIn) ->
                _AccOut = AccIn;
            F(none, _, AccIn) ->
                _AccOut = AccIn;
            F({_K, V, Iter}, N, AccIn) when is_integer(N), N > 0 ->
                F(gb_trees:next(Iter), N - 1, [V | AccIn])
        end,
    Vs = GbTreesPeekFun(gb_trees:next(gb_trees:iterator(Db)),
                        MaxN,
                        []),
    {ok, lists:reverse(Vs)}.

-spec pool_db_put(pool_db(), pool_db_key(), pool_db_value()) ->
                         {ok, NewDb::pool_db()}.
pool_db_put(Db, K, V) ->
    {ok, gb_trees:enter(K, V, Db)}.

-spec pool_db_delete(pool_db(), pool_db_key()) -> {ok, NewDb::pool_db()}.
pool_db_delete(Db, K) ->
    {ok, gb_trees:delete_any(K, Db)}.
