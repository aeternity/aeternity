%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module that handle candidate block generation.
%%% @end
%%%=============================================================================
-module(aec_block_generator).

%% API
-export([start_link/0, stop/0]).

-export([get_candidate/0]).

-export([start_generation/0, stop_generation/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ifdef(TEST).
-export([update_block_candidate/4]).
-endif.

-record(state,
        { generating = false    :: boolean()
        , worker = undefined    :: undefined | {pid(), term()}
        , candidate = undefined :: undefined | aec_blocks:block()
        , candidate_state = undefined :: undefined | term()
        , new_txs = []          :: list(term())
        }).

-include("blocks.hrl").

%% -- API --------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

-spec get_candidate() -> {ok, aec_blocks:block()} | {error, no_candidate}.
get_candidate() ->
    try
        gen_server:call(?MODULE, get_candidate, 100)
    catch _:_ ->
        {error, no_candidate}
    end.

start_generation() ->
    gen_server:cast(?MODULE, start_generation).

stop_generation() ->
    gen_server:cast(?MODULE, stop_generation).

%% -- gen_server callbacks ---------------------------------------------------

init([]) ->
    aec_events:subscribe(tx_created),
    aec_events:subscribe(tx_received),
    aec_events:subscribe(block_created),
    aec_events:subscribe(top_synced),
    aec_events:subscribe(top_changed),
    {ok, #state{}}.

handle_call(get_candidate, _From, State = #state{ candidate = undefined }) ->
    {reply, {error, no_candidate}, State};
handle_call(get_candidate, _From, State = #state{ candidate = Candidate }) ->
    {reply, {ok, Candidate}, State};
handle_call(Req, _From, State) ->
    lager:info("Unexpected call: ~p", [Req]),
    {reply, ok, State}.

handle_cast(start_generation, State) ->
    lager:debug("start_generation"),
    {noreply, do_start_generation(State)};
handle_cast(stop_generation, State) ->
    lager:debug("stop_generation"),
    {noreply, do_stop_generation(State)};
handle_cast({new_candidate, Candidate, CandidateState}, State) ->
    epoch_mining:info("New candidate generated", []),
    lager:debug("New candidate generated", []),
    publish_candidate(Candidate),
    State1 = finish_worker(State),
    State2 = State1#state{ candidate = Candidate
                         , candidate_state = CandidateState },
    {noreply, maybe_start_worker_txs(State2)};
handle_cast({worker_done, Result}, State = #state{ worker = {Pid, _}}) ->
    State1 = finish_worker(State),
    lager:debug("Candidate worker ~p done ~p", [Pid, Result]),
    {noreply, State1};
handle_cast(Msg, State) ->
    lager:info("Unexpected message ~p", [Msg]),
    {noreply, State}.

handle_info({gproc_ps_event, Event, _Data}, State = #state{ generating = false }) ->
    lager:debug("ignored event ~p", [Event]),
    {noreply, State};
handle_info({gproc_ps_event, Event, #{info := Info}}, State) ->
    lager:debug("got event ~p", [Event]),
    State1 =
        case Event of
            block_created -> preempt_generation(State, Info);
            top_synced    -> preempt_generation(State, Info);
            top_changed   -> preempt_generation(State, Info);
            tx_created    -> add_new_tx(State, Info);
            tx_received   -> add_new_tx(State, Info);
            _             ->
                lager:info("Ignoring spurious event ~p", [Event]),
                State
        end,
    {noreply, State1};
handle_info({'DOWN', Ref, process, Pid, Reason}, State = #state{ worker = {Pid, Ref} }) ->
    lager:debug("Worker died with reason ~p", [Reason]),
    State1 = finish_worker(State),
    {noreply, maybe_start_worker_txs(State1)};
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    %% Stale monitor message
    {noreply, State};
handle_info(Msg, State) ->
    lager:info("Unexpected message ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    stop_worker(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% -- Local functions --------------------------------------------------------

do_start_generation(S = #state{ generating = false }) ->
    S1 = start_worker(S),
    S1#state{ generating = true };
do_start_generation(S = #state{ candidate = Candidate }) ->
    %% If we are asked to start generation and already have a block, signal this.
    [ publish_candidate(Candidate) || Candidate /= undefined ],
    S.

do_stop_generation(S = #state{ generating = true }) ->
    S1 = stop_worker(S),
    S1#state{ generating = false };
do_stop_generation(S) ->
    S.

add_new_tx(S = #state{ worker = Worker }, Tx) ->
    case Worker of
        undefined      -> start_worker_txs(S, [Tx]);
        {_WPid, _WRef} -> S#state{ new_txs = [Tx | S#state.new_txs] }
    end.

preempt_generation(S, NewTop) ->
    S1 = stop_worker(S),
    start_worker_block(S1, NewTop).

stop_worker(S = #state{ worker = {WPid, WRef} }) ->
    erlang:demonitor(WRef, [flush]),
    erlang:exit(WPid, finished),
    lager:debug("stopped worker ~p", [WPid]),
    S#state{ worker = undefined };
stop_worker(S) ->
    S.

finish_worker(S = #state{ worker = {_WPid, WRef} }) ->
    erlang:demonitor(WRef, [flush]),
    S#state{ worker = undefined }.

start_worker(S) ->
    case aec_chain:top_block() of
        undefined -> S;
        Block     -> start_worker_block(S, Block)
    end.

start_worker_block(S = #state{ worker = undefined }, BlockOrBlockHash) ->
    Pid = spawn(fun() -> create_block_candidate(BlockOrBlockHash) end),
    lager:debug("Worker ~p created", [Pid]),
    Ref = erlang:monitor(process, Pid),
    S#state{ worker = {Pid, Ref}, new_txs = [], candidate = undefined }.

start_worker_txs(S = #state{ worker = undefined, candidate = Candidate
                           , candidate_state = CState }, Txs) ->
    Pid = spawn(fun() -> update_block_candidate(Candidate, CState, Txs) end),
    lager:debug("Worker ~p created", [Pid]),
    Ref = erlang:monitor(process, Pid),
    S#state{ worker = {Pid, Ref}, new_txs = [] }.

maybe_start_worker_txs(S) ->
    case S#state.new_txs of
        []  -> S;
        Txs -> start_worker_txs(S, Txs)
    end.

%% Generate block candidate
create_block_candidate(BlockOrBlockHash) ->
    case aec_block_candidate:create(BlockOrBlockHash) of
        {ok, NewBlock, BlockInfo} ->
            gen_server:cast(?MODULE, {new_candidate, NewBlock, BlockInfo});
        {error, Reason} ->
            failed_attempt(Reason)
    end.

update_block_candidate(Block, BlockInfo = #{ adj_chain := AdjChain }, Txs) ->
    NTxs = length(aec_blocks:txs(Block)),
    MaxTxs = aec_governance:max_txs_in_block(),
    case NTxs < MaxTxs of
        false -> failed_attempt(block_is_full);
        true  ->
            SortedTxs = sort_txs(Txs),
            case update_block_candidate(MaxTxs - NTxs, Block, SortedTxs, BlockInfo) of
                {ok, NewBlock, NewBlockInfo} ->
                    case aec_block_candidate:adjust_target(NewBlock, AdjChain) of
                        {ok, AdjBlock} ->
                            gen_server:cast(?MODULE, {new_candidate, AdjBlock, NewBlockInfo});
                        {error, _} ->
                            failed_attempt(failed_to_adjust_target)
                    end;
                {error, _} ->
                    failed_attempt(no_update_to_block_candidate)
            end
    end.

update_block_candidate(MaxNTxs, Block, Txs, BlockInfo) ->
    #{ trees := Trees, tot_fee := TotFee, txs_tree := TxsTree } = BlockInfo,
    case add_txs_to_trees(MaxNTxs, Trees, Txs,
                          aec_blocks:height(Block), aec_blocks:version(Block)) of
        {[], _Trees} ->
            {error, no_change};
        {Txs1, Trees1} ->
        Txs0 = aec_blocks:txs(Block),
            Fee = aec_block_candidate:calculate_fee(Txs1),
            Trees2 = aec_trees:grant_fee_to_miner(aec_blocks:miner(Block), Trees1, Fee),
            TxsTree1 = aec_txs_trees:add_txs(Txs1, length(Txs0), TxsTree),
            {ok, TxsRootHash} = aec_txs_trees:root_hash(TxsTree1),
            NewBlock = Block#block{ txs_hash = TxsRootHash
                                  , root_hash = aec_trees:hash(Trees2)
                                  , txs =  Txs0 ++ Txs1
                                  , time = aeu_time:now_in_msecs() },
            NewBlockInfo = BlockInfo#{ trees => Trees2, tot_fee => TotFee + Fee,
                                       txs_tree => TxsTree1 },
            {ok, NewBlock, NewBlockInfo}
    end.

add_txs_to_trees(MaxN, Trees, Txs, Height, Version) ->
    add_txs_to_trees(MaxN, Trees, Txs, [], Height, Version).

add_txs_to_trees(0, Trees, _Txs, Acc, _Height, _Version) ->
    {lists:reverse(Acc), Trees};
add_txs_to_trees(_N, Trees, [], Acc, _Height, _Version) ->
    {lists:reverse(Acc), Trees};
add_txs_to_trees(N, Trees, [Tx | Txs], Acc, Height, Version) ->
    case aec_trees:apply_txs_on_state_trees([Tx], Trees, Height, Version) of
        {ok, [], _} ->
            add_txs_to_trees(N, Trees, Txs, Acc, Height, Version);
        {ok, [Tx], Trees1} ->
            add_txs_to_trees(N+1, Trees1, Txs, [Tx | Acc], Height, Version)
    end.

failed_attempt(Reason) ->
    gen_server:cast(?MODULE, {worker_done, {failed, Reason}}).

publish_candidate(_Block) ->
    aec_events:publish(candidate_block, new_candidate).

%% Respect nonces order
sort_txs(Txs) ->
    Cmp =
        fun(STx1, STx2) ->
            Tx1 = aetx_sign:tx(STx1),
            Tx2 = aetx_sign:tx(STx2),
            {O1, N1} = {aetx:origin(Tx1), aetx:nonce(Tx1)},
            {O2, N2} = {aetx:origin(Tx2), aetx:nonce(Tx2)},
            {O1, N1} =< {O2, N2}
        end,
    lists:sort(Cmp, Txs).
