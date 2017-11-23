%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Main conductor of the mining
%%% @end
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The aec_conductor is the main hub of the mining engine.
%%--------------------------------------------------------------------

-module(aec_conductor).

-behaviour(gen_server).

%% API
-export([ get_mining_state/0
        , post_block/1
        , start_mining/0
        , stop_mining/0
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

-include("common.hrl").
-include("blocks.hrl").

-define(SERVER, ?MODULE).

-define(FETCH_NEW_TXS_FROM_POOL, true).

-type workers() :: orddict:orddict(pid(), atom()).
-type candidate() :: {block(),
                      CurrentNonce :: integer(),
                      MaxNonce :: integer()}.

-record(state, {block_candidate                   :: candidate() | 'undefined',
                blocked_tags            = []      :: ordsets:ordsets(atom()),
                fetch_new_txs_from_pool = true    :: boolean(),
                mining_state            = running :: 'running' | 'stopped',
                seen_top_block_hash               :: binary() | 'undefined',
                workers                 = []      :: workers()
               }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

stop() ->
    gen_server:stop(?SERVER).

start_mining() ->
    gen_server:call(?SERVER, start_mining).

stop_mining() ->
    gen_server:call(?SERVER, stop_mining).

get_mining_state() ->
    gen_server:call(?SERVER, get_mining_state).

post_block(Block) ->
    gen_server:call(?SERVER, {post_block, Block}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Options) ->
    process_flag(trap_exit, true),
    State1 = set_option(autostart, Options, #state{}),
    State2 = set_option(fetch_new_txs_from_pool, Options, State1),
    State3 = State2#state{seen_top_block_hash = aec_chain:top_block_hash()},
    epoch_mining:info("Miner process initilized ~p~n", [State3]),
    %% NOTE: The init continues at handle_info(timeout, State).
    {ok, State3, 0}.

handle_call({post_block, Block}, From, State) ->
    State1 = handle_post_block(Block, From, State),
    {noreply, State1};
handle_call(stop_mining,_From, State) ->
    epoch_mining:info("Mining stopped"),
    State1 = kill_all_workers_except_tag(post_block, State),
    {reply, ok, State1#state{mining_state = 'stopped'}};
handle_call(start_mining,_From, State) ->
    epoch_mining:info("Mining started"),
    State1 = start_mining(State#state{mining_state = 'running'}),
    {reply, ok, State1};
handle_call(get_mining_state,_From, State) ->
    {reply, State#state.mining_state, State};
handle_call(Request, _From, State) ->
    epoch_mining:error("Received unknown request: ~p", [Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Other, State) ->
    epoch_mining:error("Received unknown cast: ~p", [Other]),
    {noreply, State}.

handle_info(timeout, State) ->
    %% Initial timeout
    {noreply, start_mining(State)};
handle_info({worker_reply, Pid, Res}, State) ->
    State1 = handle_worker_reply(Pid, Res, State),
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

%% NOTE: State is passed through in preparation for a functional chain State.
save_mined_block(Block, State) ->
    Header = aec_blocks:to_header(Block),
    case aec_chain:insert_header(Header) of
        ok ->
            case aec_chain:write_block(Block) of
                ok ->
                    try exometer:update([ae,epoch,aecore,mining,blocks_mined], 1)
                    catch error:_ -> ok end,
                    epoch_mining:info("Block inserted: Height = ~p"
                                      "~nHash = ~s",
                                      [Block#block.height,
                                       as_hex(Block#block.root_hash)]),
                    aec_events:publish(block_created, Block),
                    State;
                {error, Reason} ->
                    epoch_mining:error("Block insertion failed: ~p.", [Reason]),
                    State
            end;
        {error, Reason} ->
            epoch_mining:error("Header insertion failed: ~p.", [Reason]),
            State
    end.

as_hex(S) ->
    [io_lib:format("~2.16.0b",[X]) || <<X:8>> <= S].

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
%%% Start mining

start_mining(#state{mining_state = 'stopped'} = State) ->
    State;
start_mining(#state{block_candidate = undefined} = State) ->
    %% We need to generate a new block candidate first.
    create_block_candidate(State);
start_mining(#state{} = State) ->
    epoch_mining:info("Starting mining"),
    {Block, Nonce,_MaxNonce} = State#state.block_candidate,
    Fun = fun() -> {aec_mining:mine(Block, Nonce),
                    State#state.block_candidate}
          end,
    dispatch_worker(mining, Fun, State).

handle_mining_reply({{ok, Block},{_OldBlock, _Nonce,_MaxNonce}}, State) ->
    %% TODO: This should listen on some event instead
    ws_handler:broadcast(miner, mined_block,
                         [{height, aec_blocks:height(Block)}]),
    epoch_mining:info("Miner ~p finished with ~p ~n", [self(), {ok, Block}]),
    State1 = save_mined_block(Block, State),
    State2 = State1#state{block_candidate = undefined},
    case preempt_if_new_top(State2) of
        no_change         -> start_mining(State2);
        {changed, State3} -> start_mining(State3)
    end;
handle_mining_reply({{error, no_solution}, {Block, Nonce, MaxNonce}},
                    State) ->
    try exometer:update([ae,epoch,aecore,mining,retries], 1)
    catch error:_ -> ok end,
    epoch_mining:info("Failed to mine block, no solution (nonce ~p); "
                      "retrying.", [Nonce]),
    retry_mining(Block, Nonce, MaxNonce, State);
handle_mining_reply({{error, {runtime, Reason}}, {Block, Nonce, MaxNonce}},
                    State) ->
    try exometer:update([ae,epoch,aecore,mining,retries], 1)
    catch error:_ -> ok end,
    epoch_mining:error("Failed to mine block, runtime error; "
                       "retrying with different nonce (was ~p). "
                       "Error: ~p", [Nonce, Reason]),
    retry_mining(Block, Nonce, MaxNonce, State).

%%%===================================================================
%%% Retry mining when we failed to find a solution.

retry_mining(Block, Nonce, MaxNonce,
             #state{fetch_new_txs_from_pool = true} = State) ->
    %% We should first see if we can get a new candidate.
    case aec_mining:apply_new_txs(Block) of
        {ok, Block} ->
            epoch_mining:info("No new txs available"),
            retry_mining_with_new_nonce(Block, Nonce, MaxNonce, State);
        {ok, NewBlock, RandomNonce} ->
            epoch_mining:info("New txs added for mining"),
            NewNonce = aec_pow:next_nonce(RandomNonce),
            NewCandidate = {NewBlock, NewNonce, RandomNonce},
            start_mining(State#state{block_candidate = NewCandidate});
        {error, What} ->
            epoch_mining:error("Retrying with new nonce after unexpected error"
                               " in applying new txs: ~p", [What]),
            retry_mining_with_new_nonce(Block, Nonce, MaxNonce, State)
    end;
retry_mining(Block, Nonce, MaxNonce, State) ->
    retry_mining_with_new_nonce(Block, Nonce, MaxNonce, State).

retry_mining_with_new_nonce(_Block, MaxNonce, MaxNonce, State) ->
    epoch_mining:info("Failed to mine block, "
                      "nonce range exhausted (was ~p)", [MaxNonce]),
    start_mining(State#state{block_candidate = undefined});
retry_mining_with_new_nonce(Block, Nonce, MaxNonce, State) ->
    NewNonce = aec_pow:next_nonce(Nonce),
    epoch_mining:info("Not fetching new txs; "
                      "continuing mining with bumped nonce "
                      "(was ~p, is ~p).", [Nonce, NewNonce]),
    start_mining(State#state{block_candidate = {Block, NewNonce, MaxNonce}}).

%%%===================================================================
%%% Wait for keys to appear

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
    create_block_candidate(State);
handle_wait_for_keys_reply(timeout, State) ->
    %% TODO: We should probably die hard at some point instead of retrying.
    epoch_mining:error("Timed out waiting for keys. Retrying."),
    wait_for_keys(State).

%%%===================================================================
%%% Block candidates

create_block_candidate(State) ->
    epoch_mining:info("Creating block candidate"),
    Fun = fun aec_mining:create_block_candidate/0,
    dispatch_worker(create_block_candidate, Fun, State).

handle_block_candidate_reply(Result, State) ->
    case Result of
        {ok, BlockCandidate, RandomNonce} ->
            Nonce = aec_pow:next_nonce(RandomNonce),
            epoch_mining:info("Created block candidate and nonce "
                              "(max ~p, current ~p).",
                              [RandomNonce, Nonce]),
            State1 = State#state{block_candidate = {BlockCandidate, Nonce, RandomNonce}},
            start_mining(State1);
        {error, key_not_found} ->
            wait_for_keys(State);
        {error, Reason} ->
            epoch_mining:error("Creation of block candidate failed: ~p",
                               [Reason]),
            %% TODO: Should we wait for something else here?
            create_block_candidate(State)
    end.

%%%===================================================================
%%% A block was given to us from the outside world

handle_post_block(Block, From, State) ->
    epoch_mining:info("Handling post block"),
    Fun = fun() -> post_block_worker(From, Block) end,
    dispatch_worker(post_block, Fun, State).

post_block_worker(From, Block) ->
    epoch_mining:info("write_block: ~p", [Block]),
    Header = aec_blocks:to_header(Block),
    {ok, HH} = aec_headers:hash_header(Header),
    case aec_chain:get_block_by_hash(HH) of
        {ok, _Existing} ->
            epoch_mining:debug("Aleady have block", []),
            {ok, From};
        {error, _} ->
            case {aec_headers:validate(Header), aec_blocks:validate(Block)} of
                {ok, ok} ->
                    case aec_chain:insert_header(Header) of
                        ok ->
                            Res = aec_chain:write_block(Block),
                            epoch_mining:debug("write_block result: ~p", [Res]),
                            {block_added, Block, From};
			{error, Reason} ->
                            lager:debug("Couldn't insert header (~p)", [Reason]),
                            {error, Reason, From}
                    end;
                {{error, Reason}, _} ->
                    {error, Reason, From};
                {ok, {error, Reason}} ->
                    {error, Reason, From}
            end
    end.

handle_post_block_reply({block_added, Block, From}, State) ->
    aec_events:publish(block_received, Block),
    gen_server:reply(From, ok),
    case preempt_if_new_top(State) of
        no_change         -> State;
        {changed, State1} -> start_mining(State1)
    end;
handle_post_block_reply({ok, From}, State) ->
    gen_server:reply(From, ok),
    State;
handle_post_block_reply({error, Reason, From}, State) ->
    lager:info("Malformed block posted to the node (~p)", [Reason]),
    gen_server:reply(From, {error, Reason}),
    State.

%%%===================================================================
%%% Hup for the server if the top block changes

preempt_if_new_top(#state{seen_top_block_hash = TopHash} = State) ->
    case aec_chain:top_block_hash() of
        TopHash -> no_change;
        TopBlockHash ->
            update_transactions(TopHash, TopBlockHash),
            State1 = State#state{seen_top_block_hash = TopBlockHash},
            State2 = kill_all_workers_with_tag(mining, State1),
            State3 = kill_all_workers_with_tag(create_block_candidate, State2),
            {changed, State3}
    end.

update_transactions(Hash1, Hash2) when is_binary(Hash1), is_binary(Hash2) ->
    epoch_mining:info("Updating transactions ~p ~p", [Hash1, Hash2]),
    {ok, Ancestor} = aec_chain:common_ancestor(Hash1, Hash2),
    {ok, TransactionsOnOldChain} =
        aec_chain:get_transactions_between(Hash1, Ancestor),
    {ok, TransactionsOnNewChain} =
        aec_chain:get_transactions_between(Hash2, Ancestor),
    ok = aec_tx_pool:fork_update(TransactionsOnNewChain, TransactionsOnOldChain),
    ok.

%%%===================================================================
%%% Worker handling

dispatch_worker(Tag, Fun, State) ->
    case is_worker_allowed(Tag, State) of
        true ->
            Wrapper = wrap_worker_fun(Fun),
            {Pid, Ref} = spawn_monitor(Wrapper),
            State1 = maybe_block_tag(Tag, State),
            Workers = orddict:store(Pid, {Tag, Ref}, State1#state.workers),
            State#state{workers = Workers};
        false ->
            epoch_mining:error("Disallowing dispatch of aditional ~p worker",
                               [Tag]),
            State
    end.

is_worker_allowed(Tag, State) ->
    not ordsets:is_element(Tag, State#state.blocked_tags).

maybe_block_tag(Tag, State) ->
    Blocked = ordsets:add_element(Tag, State#state.blocked_tags),
    BlockedState = State#state{blocked_tags = Blocked},
    case Tag of
        wait_for_keys          -> BlockedState;
        mining                 -> BlockedState;
        create_block_candidate -> BlockedState;
        post_block             -> State
    end.

kill_worker(Pid, Tag, Ref, State) ->
    Workers = State#state.workers,
    Blocked = State#state.blocked_tags,
    demonitor(Ref, [flush]),
    exit(Pid, kill),
    State#state{workers = orddict:erase(Pid, Workers),
                blocked_tags = ordsets:del_element(Tag, Blocked)
               }.

kill_all_workers(#state{workers = Workers} = State) ->
    lists:foldl(fun({Pid, {Tag, Ref}}, S) -> kill_worker(Pid, Tag, Ref, S)end,
                State, Workers).

kill_all_workers_except_tag(Tag, #state{workers = Workers} = State) ->
    lists:foldl(fun({Pid, {PidTag, Ref}}, S) ->
                        case Tag =/= PidTag of
                            true  -> kill_worker(Pid, Tag, Ref, S);
                            false -> S
                        end
                end, State, Workers).

kill_all_workers_with_tag(Tag, #state{workers = Workers} = State) ->
    lists:foldl(fun({Pid, {PidTag, Ref}}, S) ->
                        case Tag =:= PidTag of
                            true  -> kill_worker(Pid, Tag, Ref, S);
                            false -> S
                        end
                end, State, Workers).

wrap_worker_fun(Fun) ->
    Server = self(),
    fun() ->
            Result = Fun(),
            Server ! {worker_reply, self(), Result}
    end.

handle_worker_reply(Pid, Reply, State) ->
    Workers = State#state.workers,
    Blocked = State#state.blocked_tags,
    case orddict:find(Pid, Workers) of
        {ok, {Tag, Ref}} ->
            demonitor(Ref, [flush]),
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
worker_reply(post_block, Res, State) ->
    handle_post_block_reply(Res, State);
worker_reply(mining, Res, State) ->
    handle_mining_reply(Res, State);
worker_reply(wait_for_keys, Res, State) ->
    handle_wait_for_keys_reply(Res, State).
