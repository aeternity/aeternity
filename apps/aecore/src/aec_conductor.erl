%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Main conductor of the mining
%%%
%% The aec_conductor is the main hub of the mining engine.
%%%
%%% The mining has two states of operation 'running' and 'stopped'
%%% Passing the option {autostart, bool()} to the initialization
%%% controls which mode to start in. In the running mode, block candidates
%%% are generated and mined in separate workers. When mining is successful,
%%% the mined block is published and added to the chain if the state of the
%%% chain allows that. In the stopped mode only blocks arriving from other
%%% miners are added to the chain.
%%%
%%% The mining can be controlled by the API functions start_mining/0
%%% and stop_mining/0. The stop_mining is preemptive (i.e., all workers
%%% involved in mining are killed).
%%%
%%% The aec_conductor operates by delegating all heavy operations to
%%% worker processes in order to be responsive. (See doc at the worker
%%% handling section.)
%%%
%%% The work flow in mining is divided into stages:
%%%  - wait for keys (of the miner)
%%%  - generate block candidate
%%%  - start mining
%%%  - retry mining
%%%
%%% The principle is to optimistically try to start mining, and fall
%%% back to an earlier stage if the preconditions are not met. The next
%%% stage of mining should be triggered in the worker reply for each
%%% stage based on the postconditions of that stage.
%%%
%%% E.g. If the start_mining stage is attempted without having a block
%%% candidate, it should fall back to generating a block candidate.
%%%
%%% E.g. When the mining worker returns it should either start mining a
%%% new block or retry mining based on the return of the mining.
%%% @end
%%% --------------------------------------------------------------------

-module(aec_conductor).

-behaviour(gen_server).

%% Mining API
-export([ get_mining_state/0
        , get_mining_workers/0
        , start_mining/0
        , stop_mining/0
        ]).

%% Chain API
-export([ add_synced_block/1
        , get_block_candidate/0
        , post_block/1
        , post_header/1
        ]).

%% for tests
-export([reinit_chain/0
        ]).

%% gen_server API
-export([ start_link/0
        , start_link/1
        , stop/0 %% For testing
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

-export_type([options/0]).

-include("common.hrl").
-include("blocks.hrl").
-include("aec_conductor.hrl").

-define(SERVER, ?MODULE).

-define(DEFAULT_MINING_ATTEMPT_TIMEOUT, 60 * 60 * 1000). %% milliseconds

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% Gen server API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

stop() ->
    gen_server:stop(?SERVER).

%%%===================================================================
%%% Mining API

-spec start_mining() -> 'ok'.
start_mining() ->
    gen_server:call(?SERVER, start_mining).

-spec stop_mining() -> 'ok'.
stop_mining() ->
    gen_server:call(?SERVER, stop_mining).

-spec get_mining_state() -> mining_state().
get_mining_state() ->
    gen_server:call(?SERVER, get_mining_state).

-spec get_mining_workers() -> [pid()].
get_mining_workers() ->
    gen_server:call(?SERVER, get_mining_workers).

%%%===================================================================
%%% Chain API

-spec post_block(#block{}) -> 'ok' | {'error', any()}.
post_block(#block{} = Block) ->
    gen_server:call(?SERVER, {post_block, Block}).

-spec add_synced_block(#block{}) -> 'ok' | {'error', any()}.
add_synced_block(Block) ->
    gen_server:call(?SERVER, {add_synced_block, Block}).

-spec post_header(#header{}) -> 'ok' | {'error', any()}.
post_header(#header{} = Header) ->
    gen_server:call(?SERVER, {post_header, Header}).

-spec get_block_candidate() -> {'ok', aec_blocks:block()} | {'error', atom()}.
get_block_candidate() ->
    gen_server:call(?SERVER, get_block_candidate).

-spec reinit_chain() -> aec_headers:header().
reinit_chain() ->
    gen_server:call(?SERVER, reinit_chain).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Options) ->
    process_flag(trap_exit, true),
    State1 = set_option(autostart, Options, #state{}),
    State2 = set_option(fetch_new_txs_from_pool, Options, State1),
    ok     = init_chain_state(),
    TopBlockHash = aec_chain:top_block_hash(),
    State3 = State2#state{seen_top_block_hash = TopBlockHash},
    epoch_mining:info("Miner process initilized ~p", [State3]),
    %% NOTE: The init continues at handle_info(init_continue, State).
    self() ! init_continue,
    {ok, State3}.

init_chain_state() ->
    case aec_chain:genesis_hash() of
        undefined ->
            {GB, _GBState} = aec_block_genesis:genesis_block_with_state(),
            ok = aec_chain_state:insert_block(GB);
        Hash when is_binary(Hash) ->
            ok
    end.

reinit_chain_state() ->
    %% NOTE: ONLY FOR TEST
    aec_db:transaction(fun() ->
                               aec_db:clear_db(),
                               init_chain_state()
                       end).

handle_call({add_synced_block, Block},_From, State) ->
    {Reply, State1} = handle_synced_block(Block, State),
    {reply, Reply, State1};
handle_call(get_block_candidate,_From, State) ->
    Res =
        case State#state.block_candidate of
            undefined when State#state.mining_state =:= stopped ->
                {error, not_mining};
            undefined when State#state.mining_state =:= running ->
                {error, miner_starting};
            #candidate{block=Block} ->
                {ok, Block}
        end,
    {reply, Res, State};
handle_call({post_block, Block},_From, State) ->
    {Reply, State1} = handle_post_block(Block, State),
    {reply, Reply, State1};
handle_call({post_header, Block},_From, State) ->
    {Reply, State1} = handle_post_header(Block, State),
    {reply, Reply, State1};
handle_call(stop_mining,_From, State) ->
    epoch_mining:info("Mining stopped"),
    State1 = kill_all_workers(State),
    {reply, ok, State1#state{mining_state = 'stopped',
                             block_candidate = undefined}};
handle_call(start_mining,_From, State) ->
    epoch_mining:info("Mining started"),
    State1 = start_mining(State#state{mining_state = 'running'}),
    {reply, ok, State1};
handle_call(get_mining_state,_From, State) ->
    {reply, State#state.mining_state, State};
handle_call(get_mining_workers, _From, State) ->
    {reply, worker_pids_by_tag(mining, State), State};
handle_call(reinit_chain, _From, State1) ->
    %% NOTE: ONLY FOR TEST
    ok = reinit_chain_state(),
    TopBlockHash = aec_chain:top_block_hash(),
    State2 = State1#state{seen_top_block_hash = TopBlockHash},
    State =
        case State2#state.mining_state of
            stopped  ->
                State2;
            running ->
                epoch_mining:info("Mining stopped"),
                State3 = kill_all_workers(State2),
                epoch_mining:info("Mining started"),
                start_mining(State3#state{mining_state = running,
                                          block_candidate = undefined})
        end,
    {reply, ok, State};
handle_call(Request, _From, State) ->
    epoch_mining:error("Received unknown request: ~p", [Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Other, State) ->
    epoch_mining:error("Received unknown cast: ~p", [Other]),
    {noreply, State}.

handle_info(init_continue, State) ->
    %% Continue the initialization by (possibly) starting the miner
    {noreply, start_mining(State)};
handle_info({worker_reply, Pid, Res}, State) ->
    State1 = handle_worker_reply(Pid, Res, State),
    {noreply, State1};
handle_info({'DOWN', Ref, process, Pid, Why}, State) when Why =/= normal->
    State1 = handle_monitor_message(Ref, Pid, Why, State),
    {noreply, State1};
handle_info(Other, State) ->
    %% TODO: Handle monitoring messages
    epoch_mining:error("Received unknown info message: ~p", [Other]),
    {noreply, State}.

terminate(_Reason, State) ->
    kill_all_workers(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Handle init options

set_option(autostart, Options, State) ->
    case get_option(autostart, Options) of
        undefined   -> State;
        {ok, true}  -> State#state{mining_state = running};
        {ok, false} -> State#state{mining_state = stopped}
    end;
set_option(fetch_new_txs_from_pool, Options, State) ->
    case get_option(fetch_new_txs_from_pool, Options) of
        undefined -> State;
        {ok, Val} -> State#state{fetch_new_txs_from_pool = Val}
    end.

get_option(Opt, Options) ->
    case proplists:lookup(Opt, Options) of
        none -> application:get_env(aecore, Opt);
        {_, Val} -> {ok, Val}
    end.

%%%===================================================================
%%% Handle monitor messages

handle_monitor_message(Ref, Pid, Why, State) ->
    Workers = State#state.workers,
    Blocked = State#state.blocked_tags,
    case lookup_worker(Ref, Pid, State) of
        not_found ->
            epoch_mining:info("Got unknown monitor DOWN message: ~p",
                              [{Ref, Pid, Why}]),
            State;
        {ok, Tag} ->
            epoch_mining:error("Worker died: ~p", [{Tag, Pid, Why}]),
            State1 = State#state{workers = orddict:erase(Pid, Workers),
                                 blocked_tags = ordsets:del_element(Tag, Blocked)
                                },
            start_mining(State1)
    end.

%%%===================================================================
%%% Worker handling
%%% @private
%%% @doc
%%%
%%% Worker functions are funs of arity 0 with a tag to determine the
%%% type of worker. Tags are enforced to be 'singleton' (only one worker
%%% allowed) or 'concurrent' (allow concurrent processes).
%%%
%%% The worker processes are monitored. Some types of worker are
%%% killed after a timeout.
%%%
%%% The worker processes provide return values through message
%%% passing. Return values are passed as messages, and the reply is
%%% handled based on the tag. The worker fun does not need to handle
%%% the message passing itself. This is taken care of by the
%%% dispatcher.
%%%
%%% Note that when the reply is handled, the state is the current
%%% server state, not the state in which the worker was
%%% dispatched. Any consistency checks for staleness must be handled
%%% in the reply handler.
%%%
%%% Workers can be killed (e.g., on preemption because of a changed
%%% chain) based on tag. Note that since the worker might have sent an
%%% answer before it is killed, it is good to check answers for
%%% staleness. TODO: This could be done by the framework.

lookup_worker(Ref, Pid, State) ->
    case orddict:find(Pid, State#state.workers) of
        {ok, #worker_info{mon = Ref} = Info} -> {ok, Info#worker_info.tag};
        error -> not_found
    end.

worker_pids_by_tag(Tag, State) ->
    orddict:fetch_keys(
      orddict:filter(
        fun(_, Info) -> Info#worker_info.tag == Tag end,
        State#state.workers)).

dispatch_worker(Tag, Fun, State) ->
    case is_tag_blocked(Tag, State) of
        true ->
            epoch_mining:error("Disallowing dispatch of additional ~p worker",
                               [Tag]),
            State;
        false ->
            {Pid, Info} = spawn_worker(Tag, Fun),
            State1 = block_tag(Tag, State),
            Workers = orddict:store(Pid, Info, State1#state.workers),
            State1#state{workers = Workers}
    end.

is_tag_blocked(Tag, State) ->
    ordsets:is_element(Tag, State#state.blocked_tags).

block_tag(Tag, #state{blocked_tags = B} = State) ->
    State#state{blocked_tags = ordsets:add_element(Tag, B)}.

spawn_worker(Tag, Fun) ->
    Timeout = worker_timeout(Tag),
    spawn_worker(Tag, Fun, Timeout).

worker_timeout(create_block_candidate) ->
    infinity;
worker_timeout(mining) ->
    aeu_env:get_env(aecore, mining_attempt_timeout, ?DEFAULT_MINING_ATTEMPT_TIMEOUT);
worker_timeout(wait_for_keys) ->
    infinity.

spawn_worker(Tag, Fun, Timeout) ->
    Wrapper = wrap_worker_fun(Fun),
    {Pid, Ref} = spawn_monitor(Wrapper),
    Timer = case Timeout of
                infinity ->
                    no_timer;
                TimeMs when is_integer(TimeMs), TimeMs > 0 ->
                    {ok, TRef} = timer:exit_after(TimeMs, Pid, shutdown),
                    {t, TRef}
            end,
    {Pid, #worker_info{tag = Tag, mon = Ref, timer = Timer}}.

wrap_worker_fun(Fun) ->
    Server = self(),
    fun() ->
            Server ! {worker_reply, self(), Fun()}
    end.

handle_worker_reply(Pid, Reply, State) ->
    Workers = State#state.workers,
    Blocked = State#state.blocked_tags,
    case orddict:find(Pid, Workers) of
        {ok, Info} ->
            cleanup_after_worker(Info),
            Tag = Info#worker_info.tag,
            State1 = State#state{workers = orddict:erase(Pid, Workers),
                                 blocked_tags = ordsets:del_element(Tag, Blocked)
                                },
            worker_reply(Tag, Reply, State1);
        error ->
            epoch_mining:error("Got unsolicited worker reply: ~p",
                               [{Pid, Reply}]),
            State
    end.

worker_reply(create_block_candidate, Res, State) ->
    handle_block_candidate_reply(Res, State);
worker_reply(mining, Res, State) ->
    handle_mining_reply(Res, State);
worker_reply(wait_for_keys, Res, State) ->
    handle_wait_for_keys_reply(Res, State).

%%%===================================================================
%%% Preemption of workers if the top of the chain changes.

preempt_if_new_top(#state{seen_top_block_hash = TopHash} = State, Publish) ->
    case aec_chain:top_block_hash() of
        TopHash -> no_change;
        TopBlockHash ->
            maybe_publish_top(Publish, TopBlockHash),
            update_tx_pool_on_top_change(TopHash, TopBlockHash),
            State1 = State#state{seen_top_block_hash = TopBlockHash},
            State2 = kill_all_workers_with_tag(mining, State1),
            State3 = kill_all_workers_with_tag(create_block_candidate, State2),
            {changed, State3#state{block_candidate = undefined}}
    end.

maybe_publish_top(none,_TopHash) -> ok;
maybe_publish_top(block_created,_TopHash) ->
    %% A new block we created is published unconditionally below.
    ok;
maybe_publish_top(block_received, TopHash) ->
    %% The received block changed the top. Publish the new top.
    {ok, Block} = aec_chain:get_block(TopHash),
    aec_events:publish(top_changed, Block),
    update_chain_metrics(Block).

maybe_publish_block(none,_Block) -> ok;
maybe_publish_block(block_received,_Block) ->
    %% We don't publish all blocks, only if it changes the top.
    ok;
maybe_publish_block(block_created = T, Block) ->
    %% This is a block we created ourselves. Always publish.
    aec_events:publish(T, Block),
    update_chain_metrics(Block).

update_chain_metrics(Block) ->
    aec_metrics:try_update([ae,epoch,aecore,chain,height],
                           aec_blocks:height(Block)).


cleanup_after_worker(Info) ->
    case Info#worker_info.timer of
        no_timer -> ok;
        {t, TRef} -> timer:cancel(TRef)
    end,
    demonitor(Info#worker_info.mon, [flush]),
    ok.

kill_worker(Pid, Info, State) ->
    Workers = State#state.workers,
    Blocked = State#state.blocked_tags,
    cleanup_after_worker(Info),
    exit(Pid, shutdown),
    State#state{workers = orddict:erase(Pid, Workers),
                blocked_tags = ordsets:del_element(
                                 Info#worker_info.tag,
                                 Blocked)
               }.

kill_all_workers(#state{workers = Workers} = State) ->
    lists:foldl(
      fun({Pid, Info}, S) ->
              kill_worker(Pid, Info, S)
      end,
      State, Workers).

kill_all_workers_with_tag(Tag, #state{workers = Workers} = State) ->
    lists:foldl(
      fun({Pid, Info}, S) ->
              case Tag =:= Info#worker_info.tag of
                  true  -> kill_worker(Pid, Info, S);
                  false -> S
              end
      end, State, Workers).

%%%===================================================================
%%% Handling update in transaction pool

update_tx_pool_on_top_change(Hash1, Hash2) when is_binary(Hash1),
                                                is_binary(Hash2) ->
    epoch_mining:info("Updating transactions ~p ~p", [Hash1, Hash2]),
    {ok, Ancestor} = aec_chain:find_common_ancestor(Hash1, Hash2),
    {ok, TxsOnOldChain} = aec_chain:get_transactions_between(Hash1, Ancestor),
    {ok, TxsOnNewChain} = aec_chain:get_transactions_between(Hash2, Ancestor),
    ok = aec_tx_pool:fork_update(TxsOnNewChain, TxsOnOldChain),
    ok.

%%%===================================================================
%%% Worker: Wait for keys to appear

-define(WAIT_FOR_KEYS_RETRIES, 10).

wait_for_keys(State) ->
    Fun = fun wait_for_keys_worker/0,
    dispatch_worker(wait_for_keys, Fun, State).

wait_for_keys_worker() ->
    wait_for_keys_worker(?WAIT_FOR_KEYS_RETRIES).

wait_for_keys_worker(0) ->
    timeout;
wait_for_keys_worker(N) ->
    case aec_keys:pubkey() of
        {ok, _Pubkey} -> keys_ready; %% TODO: We could keep the key in the state
        {error, _} ->
            timer:sleep(500),
            wait_for_keys_worker(N - 1)
    end.

handle_wait_for_keys_reply(keys_ready, State) ->
    start_mining(State#state{keys_ready = true});
handle_wait_for_keys_reply(timeout, State) ->
    %% TODO: We should probably die hard at some point instead of retrying.
    epoch_mining:error("Timed out waiting for keys. Retrying."),
    wait_for_keys(State#state{keys_ready = false}).

%%%===================================================================
%%% Worker: Start mining

start_mining(#state{keys_ready = false} = State) ->
    %% We need to get the keys first
    wait_for_keys(State);
start_mining(#state{mining_state = 'stopped'} = State) ->
    State;
start_mining(#state{block_candidate = undefined} = State) ->
    %% We need to generate a new block candidate first.
    create_block_candidate(State);
start_mining(#state{block_candidate = #candidate{top_hash = OldHash},
                    seen_top_block_hash = SeenHash
                   } = State) when OldHash =/= SeenHash ->
    %% Candidate generated with stale top hash.
    %% Regenerate the candidate.
    create_block_candidate(State#state{block_candidate = undefined});
start_mining(#state{block_candidate = Candidate} = State) ->
    epoch_mining:info("Starting mining"),
    HeaderBin = Candidate#candidate.bin,
    Nonce     = Candidate#candidate.nonce,
    Target    = aec_blocks:target(Candidate#candidate.block),
    Info      = [{top_block_hash, State#state.seen_top_block_hash}],
    aec_events:publish(start_mining, Info),
    Fun = fun() ->
                  {aec_mining:mine(HeaderBin, Target, Nonce)
                  , HeaderBin}
          end,
    dispatch_worker(mining, Fun, State).

handle_mining_reply(_Reply, #state{block_candidate = undefined} = State) ->
    %% Something invalidated the block candidate already.
    start_mining(State);
handle_mining_reply({{ok, {Nonce, Evd}}, HeaderBin}, #state{} = State) ->
    Candidate = State#state.block_candidate,
    %% Check that the solution is for this block
    case HeaderBin =:= Candidate#candidate.bin of
        true ->
            aec_metrics:try_update([ae,epoch,aecore,mining,blocks_mined], 1),
            State1 = State#state{block_candidate = undefined},
            Block = aec_blocks:set_pow(Candidate#candidate.block, Nonce, Evd),
            case handle_mined_block(Block, State1) of
                {ok, State2} ->
                    State2;
                {{error, Reason}, State2} ->
                    epoch_mining:error("Block insertion failed: ~p.", [Reason]),
                    start_mining(State2)
            end;
        _Other ->
            %% This mining effort was for an earlier block candidate.
            epoch_mining:error("Found solution for old block", []),
            start_mining(State)
    end;
handle_mining_reply({{error, no_solution}, _}, State) ->
    Candidate = State#state.block_candidate,
    aec_metrics:try_update([ae,epoch,aecore,mining,retries], 1),
    epoch_mining:debug("Failed to mine block, no solution (nonce ~p); "
                       "retrying.", [Candidate#candidate.nonce]),
    retry_mining(State);
handle_mining_reply({{error, {runtime, Reason}}, _}, State) ->
    aec_metrics:try_update([ae,epoch,aecore,mining,retries], 1),
    Candidate = State#state.block_candidate,
    epoch_mining:error("Failed to mine block, runtime error; "
                       "retrying with different nonce (was ~p). "
                       "Error: ~p", [Candidate#candidate.nonce, Reason]),
    retry_mining(State).

%%%===================================================================
%%% Retry mining when we failed to find a solution.

retry_mining(#state{block_candidate = #candidate{top_hash = Hash1},
                    seen_top_block_hash = Hash2} = State) when Hash1 =/= Hash2 ->
    %% Stale hash for candidate. Rebuild it.
    epoch_mining:info("Stale hash for candidate"),
    start_mining(State#state{block_candidate = undefined});
retry_mining(#state{block_candidate = Candidate,
                    fetch_new_txs_from_pool = true} = State) ->
    Block = Candidate#candidate.block,
    %% We should first see if we can get a new candidate.
    case aec_mining:need_to_regenerate(Block) of
        false ->
            epoch_mining:debug("No new txs available"),
            retry_mining_with_new_nonce(Candidate, State);
        true ->
            epoch_mining:debug("New txs added for mining"),
            start_mining(State#state{block_candidate = undefined})
    end;
retry_mining(#state{block_candidate = Candidate} = State) ->
    retry_mining_with_new_nonce(Candidate, State).

retry_mining_with_new_nonce(#candidate{nonce = N, max_nonce = N}, State) ->
    epoch_mining:info("Failed to mine block, "
                      "nonce range exhausted (was ~p)", [N]),
    start_mining(State#state{block_candidate = undefined});
retry_mining_with_new_nonce(Candidate, State) ->
    Nonce = Candidate#candidate.nonce,
    NewNonce = aec_pow:next_nonce(Nonce),
    epoch_mining:debug("Not fetching new txs; "
                       "continuing mining with bumped nonce "
                       "(was ~p, is ~p).", [Nonce, NewNonce]),
    NewCandidate = Candidate#candidate{nonce = NewNonce},
    start_mining(State#state{block_candidate = NewCandidate}).

%%%===================================================================
%%% Worker: Generate new block candidates

new_candidate(Block, Nonce, MaxNonce, State) ->
    HeaderBin = aec_headers:serialize_for_hash(aec_blocks:to_header(Block)),
    #candidate{block = Block,
               bin = HeaderBin,
               nonce = Nonce,
               max_nonce = MaxNonce,
               top_hash = State#state.seen_top_block_hash
              }.

create_block_candidate(#state{keys_ready = false} = State) ->
    %% Keys are needed for creating a candidate
    wait_for_keys(State);
create_block_candidate(State) ->
    epoch_mining:info("Creating block candidate"),
    Fun = fun() ->
                  {TopBlock, TopBlockState} = aec_chain:top_block_with_state(),
                  AdjChain = get_adjustment_headers(TopBlock),
                  {aec_mining:create_block_candidate(TopBlock, TopBlockState,
                                                     AdjChain),
                   State#state.seen_top_block_hash}
          end,
    dispatch_worker(create_block_candidate, Fun, State).

get_adjustment_headers(TopBlock) ->
    {ok, TopHash} = aec_blocks:hash_internal_representation(TopBlock),
    N = aec_governance:blocks_to_check_difficulty_count(),
    case aec_blocks:height(TopBlock) < N of
        true  -> [];
        false ->
            {ok, Headers} = aec_chain:get_n_headers_from_hash(TopHash, N),
            Headers
    end.

handle_block_candidate_reply({_Result, OldTopHash}, State)
  when OldTopHash =/= State#state.seen_top_block_hash ->
    %% The top hash is stale. Regenerate the candidate.
    start_mining(State#state{block_candidate = undefined});
handle_block_candidate_reply({Result,_OldTopHash}, State) ->
    case Result of
        {ok, BlockCandidate, RandomNonce} ->
            Nonce = aec_pow:next_nonce(RandomNonce),
            epoch_mining:info("Created block candidate and nonce "
                              "(max ~p, current ~p). "
                              "Its target is ~p (= difficulty ~p).",
                              [RandomNonce, Nonce,
                               aec_blocks:target(BlockCandidate),
                               aec_blocks:difficulty(BlockCandidate)]),
            Candidate = new_candidate(BlockCandidate, Nonce,
                                      RandomNonce, State),
            State1 = State#state{block_candidate = Candidate},
            start_mining(State1);
        {error, key_not_found} ->
            start_mining(State#state{keys_ready = false});
        {error, Reason} ->
            epoch_mining:error("Creation of block candidate failed: ~p",
                               [Reason]),
            %% TODO: Should we wait for something else here?
            create_block_candidate(State)
    end.

%%%===================================================================
%%% In server context: A block was given to us from the outside world

handle_synced_block(Block, State) ->
    epoch_mining:info("sync_block: ~p", [Block]),
    handle_add_block(Block, State, none).

handle_post_block(Block, State) ->
    epoch_mining:info("post_block: ~p", [Block]),
    handle_add_block(Block, State, block_received).

handle_mined_block(Block, State) ->
    epoch_mining:info("Block mined: Height = ~p; Hash = ~s",
                      [aec_blocks:height(Block),
                       as_hex(aec_blocks:root_hash(Block))]),
    handle_add_block(Block, State, block_created).

as_hex(S) ->
    [io_lib:format("~2.16.0b", [X]) || <<X:8>> <= S].

handle_add_block(Block, State, Publish) ->
    Header = aec_blocks:to_header(Block),
    {ok, Hash} = aec_headers:hash_header(Header),
    case aec_chain:has_block(Hash) of
        true ->
            epoch_mining:debug("Block already in chain", []),
            {ok, State};
        false ->
            %% NOTE: Need to validate both header and block
            %% TODO: Block validation should also validate header
            case {aec_headers:validate(Header), aec_blocks:validate(Block)} of
                {ok, ok} ->
                    case aec_chain_state:insert_block(Block) of
                        ok ->
                            maybe_publish_block(Publish, Block),
                            case preempt_if_new_top(State, Publish) of
                                no_change -> {ok, State};
                                {changed, State1} -> {ok, start_mining(State1)}
                            end;
			{error, Reason} ->
                            lager:error("Couldn't insert block (~p)", [Reason]),
                            {{error, Reason}, State}
                    end;
                {{error, Reason}, _} ->
                    epoch_mining:info("Header failed validation: ~p", [Reason]),
                    {{error, Reason}, State};
                {ok, {error, Reason}} ->
                    epoch_mining:info("Block failed validation: ~p", [Reason]),
                    {{error, Reason}, State}
            end
    end.

%%%===================================================================
%%% In server context: A header was given to us from the outside world

handle_post_header(Header, State) ->
    epoch_mining:info("post_header: ~p", [Header]),
    {ok, Hash} = aec_headers:hash_header(Header),
    case aec_chain:has_header(Hash) of
        true ->
            epoch_mining:debug("Posted header already in chain", []),
            {ok, State};
        false ->
            case aec_headers:validate(Header) of
                ok ->
                    case aec_chain_state:insert_header(Header) of
                        ok ->
                            %% This might have caused a fork
                            case preempt_if_new_top(State, none) of
                                no_change -> {ok, State};
                                {changed, State1} -> {ok, start_mining(State1)}
                            end;
			{error, Reason} ->
                            lager:debug("Couldn't insert block (~p)", [Reason]),
                            {{error, Reason}, State}
                    end;
                {error, Reason} ->
                    {{error, Reason}, State}
            end
    end.
