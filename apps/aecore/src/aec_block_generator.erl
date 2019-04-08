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

-export([ get_generation_state/0
        , start_generation/0
        , stop_generation/0
        ]).

-export([prep_stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        { generating = false    :: boolean()
        , worker = undefined    :: undefined | {pid(), term()}
        , candidate = undefined :: undefined | aec_blocks:block()
        , candidate_state = undefined :: undefined
                                       | aec_block_micro_candidate:block_info()
        , new_txs = []          :: list(aetx_sign:signed_tx())
        }).

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
    gen_server:call(?MODULE, stop_generation).

-spec get_generation_state() -> 'running' | 'stopped'.
get_generation_state() ->
    gen_server:call(?MODULE, get_generation_state).

prep_stop() ->
    gen_server:call(?MODULE, prep_stop).

%% -- gen_server callbacks ---------------------------------------------------

init([]) ->
    aec_events:subscribe(tx_created),
    aec_events:subscribe(tx_received),
    aec_events:subscribe(top_changed),
    {ok, #state{}}.

handle_call(get_candidate, _From, State = #state{ candidate = undefined }) ->
    {reply, {error, no_candidate}, State};
handle_call(get_candidate, _From, State = #state{ candidate = Candidate }) ->
    {reply, {ok, Candidate}, State};
handle_call(get_generation_state, _From, State = #state{ generating = IsGenerating }) ->
   Reply = case IsGenerating of
        true -> running;
        false -> stopped
    end,
    {reply, Reply, State};
handle_call(prep_stop, _From, State) ->
    {reply, ok, do_stop_generation(State)};
handle_call(stop_generation, _From, State) ->
    lager:debug("stop_generation"),
    {reply, ok, do_stop_generation(State)};
handle_call(Req, _From, State) ->
    lager:info("Unexpected call: ~p", [Req]),
    {reply, ok, State}.

handle_cast(start_generation, State) ->
    lager:debug("start_generation"),
    {noreply, do_start_generation(State)};
handle_cast({worker_done, Pid, {candidate, Candidate, CandidateState}},
            State = #state{ worker = {Pid, _} }) ->
    %% Only publish non-empty micro-blocks
    case aec_blocks:txs(Candidate) of
        [] ->
            lager:debug("New empty microblock candidate generated, and discarded", []),
            ok;
        _  ->
            epoch_mining:info("New microblock candidate generated", []),
            publish_candidate(Candidate)
    end,
    State1 = finish_worker(State),
    State2 = State1#state{ candidate = Candidate
                         , candidate_state = CandidateState },
    {noreply, maybe_start_worker_txs(State2)};
handle_cast({worker_done, Pid, {failed, Reason}}, State = #state{ worker = {Pid, _}}) ->
    State1 = finish_worker(State),
    lager:debug("Candidate worker ~p failed ~p", [Pid, Reason]),
    {noreply, State1};
handle_cast({worker_done, OldPid, Result}, State) ->
    lager:debug("Ignored stale worker reply ~p (from worker ~p)", [Result, OldPid]),
    {noreply, State};
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
    [ publish_candidate(Candidate)
      || Candidate /= undefined andalso aec_blocks:txs(Candidate) /= [] ],
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

preempt_generation(S, #{ block_hash := NewTop }) ->
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
    case aec_block_micro_candidate:create(BlockOrBlockHash) of
        {ok, NewBlock, NewBlockInfo} ->
            new_candidate(NewBlock, NewBlockInfo);
        {error, Reason} ->
            failed_attempt(Reason)
    end,
    ok.

update_block_candidate(Block, BlockInfo, Txs) ->
    case aec_block_micro_candidate:update(Block, Txs, BlockInfo) of
        {ok, NewBlock, NewBlockInfo} ->
            new_candidate(NewBlock, NewBlockInfo);
        {error, Reason} ->
            failed_attempt(Reason)
    end.

failed_attempt(Reason) ->
    gen_server:cast(?MODULE, {worker_done, self(), {failed, Reason}}).

new_candidate(NewBlock, NewBlockInfo) ->
    gen_server:cast(?MODULE, {worker_done, self(), {candidate, NewBlock, NewBlockInfo}}).

publish_candidate(_Block) ->
    aec_events:publish(candidate_block, new_candidate).
