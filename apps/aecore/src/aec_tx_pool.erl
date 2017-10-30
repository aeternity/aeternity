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
-define(TAB, ?MODULE).

-record(state, {}).

-type negated_fee() :: non_pos_integer().
-type non_pos_integer() :: neg_integer() | 0.

-type pool_db_key() ::
        {negated_fee(), %% Negated fee - not fee - in order to sort by decreasing fee.
         tx()}. %% TODO Minimize e.g.: (1) hash of tx; (2) initiating account and its nonce.
-type pool_db_value() :: signed_tx().

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    ensure_tab(),
    case gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of
        {ok, Pid} = Ok ->
            ets:give_away(?TAB, Pid, []),
            Ok;
        Other ->
            Other
    end.

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
-spec peek(pos_integer() | infinity) -> {ok, [signed_tx()]}.
peek(MaxN) when is_integer(MaxN), MaxN >= 0; MaxN =:= infinity ->
    {ok, pool_db_peek(MaxN)}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% No `process_flag(trap_exit, true)` because no cleaning up
    %% needed in `terminate/2`.
    {ok, #state{}}.

handle_call({push, Tx}, _From, State) ->
    case aec_tx_sign:verify(Tx) of
        ok ->
            ets_insert({pool_db_key(Tx), Tx}),
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, {verification_error, Reason}}, State}
    end;
handle_call({delete, Tx}, _From, State) ->
    ets_delete(pool_db_key(Tx)),
    {reply, ok, State};
handle_call(Request, From, State) ->
    lager:warning("Ignoring unknown call request from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

handle_info({'ETS-TRANSFER', _, _, _}, State) ->
    {noreply, State};
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

-spec pool_db_key(pool_db_value()) -> pool_db_key().
pool_db_key(SignedTx) ->
    Tx = aec_tx_sign:data(SignedTx),
    Sigs = aec_tx_sign:signatures(SignedTx),
    {-aec_tx:fee(Tx), Sigs}.

-spec pool_db_peek(MaxNumber::pos_integer()) -> [pool_db_value()].
pool_db_peek(MaxN) when is_integer(MaxN), MaxN >= 0; MaxN =:= infinity ->
    ets_select([{ {'_', '$1'}, [], ['$1'] }], MaxN).

ets_select(Pat, Limit) ->
    ets:select(?TAB, Pat, Limit).

-spec ets_insert({pool_db_key(), pool_db_value()}) -> true.
ets_insert(Obj) ->
    ets:insert(?TAB, Obj).

-spec ets_delete(pool_db_key()) -> true.
ets_delete(K) ->
    ets:delete(?TAB, K).

-spec ensure_tab() -> true.
ensure_tab() ->
    case ets:info(?TAB, size) of
        undefined ->
            ets:new(?TAB, [ordered_set, public, named_table,
                           {heir, self(), []}]);
        _ ->
            true
    end.
