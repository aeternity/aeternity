%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity
%%% @doc
%%%     Service to hold merkle tree of state
%%% @end
%%% Created : 20 Sep 2017
%%%-------------------------------------------------------------------
-module(aec_state).

-behaviour(gen_server).

-include("common.hrl").
-include("blocks.hrl").

%% API
-export([start_link/0, stop/0]).

%% API designed for chain service
-export([get_trees/0,
         apply_txs/2,
         force_trees/2]).

%% API to support consequences of allowing blocks out of order
-export([check_chain_for_successor/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_CALL_TIMEOUT, infinity).

-record(state, {trees  :: trees(),
                height = ?GENESIS_HEIGHT :: height()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_trees() -> {ok, {height(), trees()}}.
get_trees() ->
    gen_server:call(?SERVER, get_trees, ?DEFAULT_CALL_TIMEOUT).

-spec apply_txs(list(), height()) -> {ok, {height(), trees()}} | {{error, atom()}, trees()}.
apply_txs(Txs, AtHeight) ->
    gen_server:call(?SERVER, {apply_txs, {Txs, AtHeight}}, ?DEFAULT_CALL_TIMEOUT).

%% API needed when external fork has more POW and we need to restart tree from common ancestor
-spec force_trees(trees(), height()) -> {ok, {height(), trees()}}.
force_trees(Trees, AtHeight) ->
    gen_server:call(?SERVER, {force_tree, {Trees, AtHeight}}, ?DEFAULT_CALL_TIMEOUT).

stop() ->
    gen_server:stop(?SERVER).

-spec check_chain_for_successor(trees(), height()) -> {trees(), height()}.
check_chain_for_successor(Trees, AtHeight) ->
    SuccessorHeight = AtHeight+1,
    case aec_chain:get_block_by_height(SuccessorHeight) of
        {ok, Block} ->
            {ok, TreesUpdated} = ?MODULE:apply_txs(aec_blocks:txs(Block), SuccessorHeight),
            check_chain_for_successor(TreesUpdated, SuccessorHeight);
        {error, {_, _}} -> %% block_not_found, chain_too_short
            {Trees, AtHeight}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, TopBlock} = aec_chain:top_block(),
    TopHeight = aec_blocks:height(TopBlock),
    {ok, EmptyTrees} = aec_trees:all_trees_new(),
    CurrentTrees = setup_trees(TopHeight, EmptyTrees),

    {ok, #state{trees = CurrentTrees, height = TopHeight}}.

handle_call(get_trees, _From, #state{trees = Trees, height = Height} = State) ->
    {reply, {ok, {Height, Trees}}, State};

handle_call({force_trees, {Trees, AtHeight}}, _From, State) ->
    {reply, {ok, {AtHeight, Trees}}, State#state{trees = Trees, height = AtHeight}};

handle_call({apply_txs, {Txs, AtHeight}}, _From,
            #state{trees = Trees, height = CurrentHeigth} = State) ->
    {Reply, TreesUpdated, HeightUpdated}
        = case validate_height(CurrentHeigth, AtHeight) of
              true ->
                  {ok, TreesUpdated0} = apply_txs_internal(Txs, Trees, AtHeight),
                  spawn(?MODULE, check_chain_for_successor, [TreesUpdated0, AtHeight]),
                  {ok, TreesUpdated0, AtHeight};
              false ->
                  {{error, not_next_block}, Trees, CurrentHeigth}
          end,
    {reply, {Reply, {HeightUpdated, TreesUpdated}},
        State#state{trees=TreesUpdated, height = HeightUpdated}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% INFO: not optimized (by local lookup of prev-block by hash in current-block)
%%       because of incoming optimization with check points
-spec setup_trees(height(), trees()) -> trees().
setup_trees(0, Trees) ->
    Trees;
setup_trees(N, Trees) ->
    {ok, Block} = aec_chain:get_block_by_height(N),
    Txs = aec_blocks:txs(Block),
    {ok, TreesUpdated} = apply_txs_internal(Txs, N, Trees),
    setup_trees(N-1, TreesUpdated).

-spec apply_txs_internal(list(), height(), trees()) -> {ok, trees()}.
apply_txs_internal(Txs, AtHeight, Trees) ->
    {ok, _Trees0} = aec_tx:apply_signed(Txs, AtHeight, Trees).

-spec validate_height(non_neg_integer(), height()) -> boolean().
validate_height(CurrentHeigth, AtHeight) ->
    CurrentHeigth + 1 == AtHeight.


