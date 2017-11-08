%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%    A continuous miner
%%% @end
%%%-------------------------------------------------------------------
-module(aec_miner).
-behaviour(gen_statem).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------
-export([start_link/0,
         start_link/1,
         stop/0,
         resume/0,
         suspend/0,
         get_balance/0,
         post_block/1
        ]).

%%------------------------------------------------------------------------------
%% gen_statem callbacks
%%------------------------------------------------------------------------------
-export([init/1,
         idle/3,
         configure/3,
         running/3,
         waiting_for_keys/3,
         terminate/3,
         code_change/4,
         callback_mode/0]).

-include("common.hrl").
-include("blocks.hrl").


-define(SERVER, ?MODULE).

-define(FETCH_NEW_TXS_FROM_POOL, true).

-record(state, {block_candidate :: block() | undefined,
                initial_cycle_nonce = 0 :: integer(),
                max_block_candidate_nonce = 0 :: integer(),
                fetch_new_txs_from_pool = ?FETCH_NEW_TXS_FROM_POOL:: boolean(),
                miner = none :: pid() | none,
                autostart = true :: boolean()
               }
       ).

%%%===================================================================
%%% API
%%%===================================================================

%%------------------------------------------------------------------------------
%% Start the state machine
%%------------------------------------------------------------------------------
-spec start_link() -> {'ok', pid()} | {error | term()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_link(Options::list()) -> {'ok', pid()} | {error | term()}.
start_link(Options) ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, Options, []).

%%------------------------------------------------------------------------------
%% Stop the state machine
%%------------------------------------------------------------------------------
-spec  stop() -> 'ok'.
stop() ->
    gen_statem:stop(?SERVER).

%%------------------------------------------------------------------------------
%% Resume mining
%%------------------------------------------------------------------------------
-spec resume() -> 'ok' | {'error' | term()}.
resume() ->
    gen_statem:call(?SERVER, start).

%%------------------------------------------------------------------------------
%% Suspend mining
%%------------------------------------------------------------------------------
-spec suspend() -> 'ok' | {'error' | term()}.
suspend() ->
    gen_statem:call(?SERVER, suspend).

%%------------------------------------------------------------------------------
%% Get the account balance
%%------------------------------------------------------------------------------
-spec get_balance() -> integer() | {'error', term()}.
get_balance() ->
    {ok, Pubkey} = aec_keys:pubkey(),
    {ok, LastBlock} = aec_chain:top(),
    Trees = aec_blocks:trees(LastBlock),
    AccountsTree = aec_trees:accounts(Trees),
    case aec_accounts:get(Pubkey, AccountsTree) of
        {ok, #account{balance = B}} ->
            B;
        _ ->
            {error, account_not_found}
    end.

%%------------------------------------------------------------------------------
%% Post a block
%%------------------------------------------------------------------------------
-spec post_block(block()) -> ok.
post_block(Block) ->
    gen_statem:cast(?SERVER, {post_block, Block}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% If an option in not provided in the application environment nor
%% in the argument to the init function the default from the state
%% record definition is used.
%% If the application environment provides an option it overrides
%% the default.
%% If the call argument provides an option it overrides both the
%% application environment and the default.
%%
%% The options are:
%%  autostart                             :: boolean()
%%  fetch_new_txs_from_pool_during_mining :: boolean()
%% @end
%%--------------------------------------------------------------------
init(Options) ->
    DefaultState = #state{},
    FetchNewTxsFromPool =
        get_option(fetch_new_txs_from_pool_during_mining, DefaultState, Options),
    AutoStart = get_option(autostart,  DefaultState, Options),
    State =
        DefaultState#state{fetch_new_txs_from_pool = FetchNewTxsFromPool,
                           autostart = AutoStart
                          },
    AutoStart = start_if_auto(State),
    Res =
        if AutoStart ->
                {ok, configure, State};
           true ->
                {ok, idle, State}
        end,
    epoch_mining:info("Miner process initilized ~p~n", [State]),
    Res.

get_option(fetch_new_txs_from_pool_during_mining = Name, State, Options) ->
    EnvVal =
        application:get_env(aecore, Name,
                            State#state.fetch_new_txs_from_pool),
    option_override(Name, EnvVal, Options);
get_option(autostart = Name, State, Options) ->
    EnvVal = application:get_env(aecore, Name,
                                 State#state.autostart),
    option_override(Name, EnvVal, Options).

option_override(Name, Default, Options) ->
    case proplists:lookup(Name, Options) of
        {_, Value} -> Value;
        none -> Default
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_statem receives an event sent using
%% gen_statem:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Type, Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, [Actions]}
%% @end
%%--------------------------------------------------------------------

%% -------------------------------------------------------------------
%% State machine states
%%           start
%%           /   \     +-----------------+----+
%%          /     \    |                 |    |
%%         v  2 1  v   v                 v    |8
%%       idle <-> configure -> wait_for_keys -+
%%         ^      5,6| ^    5
%%         |         v |4
%%         +----- running <-+
%%             2     |7     |
%%                   +------+
%%
%% -------------------------------------------------------------------
%% Events
%%  1 call:start
%%  2 call:suspend
%%
%%  3 cast:post_block Block
%%  4 cast:miner_done Miner
%%  5 cast:create_block_candidate
%%  6 cast:bump_initial_cycle_nonce
%%  7 cast:mine
%%  8 cast:check_keys
%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% The Idle state
%% -------------------------------------------------------------------
%% Calls: start | suspend
idle({call, From}, start, State) ->
    epoch_mining:info("Mining resumed by user.", []),
    {next_state, configure, State,
     [{reply, From, ok}, candidate_event()]};
idle({call, From}, suspend, State) ->
    {keep_state, State, [{reply, From, ok}]};
idle({call, From}, _Msg, State) ->
    {keep_state, State, [{reply, From, {error, not_supported}}]};

%% Casts: post_block | miner_done | create_block_candidate |
%%        bump_initial_cycle_nonce | mine | check_keys
idle(cast, {post_block, Block}, State) ->
    idle_post_block(Block, State);
idle(cast, {miner_done, Miner}, #state{ miner = Miner } = State) ->
    {next_state, configure, State#state{ miner = none }};
idle(cast, {miner_done, Miner}, #state{ miner = OtherMiner } = State) ->
    epoch_mining:error("Unknown miner finished: ~p~n", [Miner]),
    {keep_state, State#state{ miner = OtherMiner }};
idle(cast, create_block_candidate, State) ->
    %% Wait for a resume.
    {keep_state, State, [postpone]};
idle(cast, bump_initial_cycle_nonce, State) ->
    %% Wait for a resume.
    {keep_state, State, [postpone]};
idle(cast, mine, State) ->
    %% Wait for a resume.
    {keep_state, State, [postpone]};
idle(cast, check_keys, State) ->
    %% Wait for a resume.
    {keep_state, State, [postpone]};
%% Catchall, log & drop unwanted messages.
idle(_Type, _Msg, State) ->
    epoch_mining:error("Unknown event in idle: ~p:~p~n", [_Type,_Msg]),
    {keep_state, State}.

%% -------------------------------------------------------------------
%% The Configure state
%% -------------------------------------------------------------------
%% Calls: start | suspend
configure({call, From}, start, State) ->
    {keep_state, State, [{reply, From, ok}]};
configure({call, From}, suspend, State) ->
    epoch_mining:info("Mining suspended by user", []),
    {next_state, idle, State, [{reply, From, ok}]};

%% Casts: post_block | miner_done | create_block_candidate |
%%        bump_initial_cycle_nonce | mine | check_keys
configure(cast, {post_block, Block}, State) ->
    configure_post_block(Block, State);
configure(cast, {miner_done, Miner}, #state{ miner = Miner } = State) ->
    %% The current miner is done. Rmove it from the state.
    {next_state, configure, State#state{ miner = none }};
configure(cast, {miner_done, Miner}, #state{ miner = OtherMiner } = State) ->
    epoch_mining:error("Unknown miner finished: ~p~n", [Miner]),
    {keep_state, State#state{ miner = OtherMiner }};
configure(cast, create_block_candidate, State) ->
    case aec_mining:create_block_candidate() of
        {ok, BlockCandidate, RandomNonce} ->
            CurrentMiningNonce = aec_pow:next_nonce(RandomNonce),
            epoch_mining:info("Created block candidate and nonce "
                              "(max ~p, current ~p).",
                              [RandomNonce, CurrentMiningNonce]),
            {next_state, running,
             State#state{block_candidate = BlockCandidate,
                         initial_cycle_nonce = CurrentMiningNonce,
                         max_block_candidate_nonce = RandomNonce},
            [mine_event()]};
        {error, key_not_found} ->
            report_suspended(),
            {next_state, waiting_for_keys, State,
            [{next_event, cast, check_keys}]};
        {error, Reason} ->
            epoch_mining:error("Creation of block candidate failed: ~p", [Reason]),
            {next_state, configure, State,
             [{next_event, cast, create_block_candidate}]}
    end;
configure(cast, bump_initial_cycle_nonce,
          #state{fetch_new_txs_from_pool = _,
                 initial_cycle_nonce = N,
                 max_block_candidate_nonce = N} = State) ->
    epoch_mining:info("Failed to mine block, "
                      "nonce range exhausted (was ~p); "
                      "regenerating block candidate.", [N]),
    {keep_state, State, [candidate_event()]};
configure(cast, bump_initial_cycle_nonce,
          #state{fetch_new_txs_from_pool = true,
                 block_candidate = BlockCandidate,
                 initial_cycle_nonce = Nonce0} = State) ->
    case aec_mining:apply_new_txs(BlockCandidate) of
        {ok, BlockCandidate} ->
            %% No new txs applied to the block.
            %% Continue incrementing nonce, without block candidate modifications.
            Nonce = aec_pow:next_nonce(Nonce0),
            epoch_mining:info("No new txs available; "
                              "continuing mining with bumped nonce "
                              "(was ~p, is ~p).", [Nonce0, Nonce]),
            {next_state, running,
             State#state{initial_cycle_nonce = Nonce},
             [mine_event()]
            };
        {ok, NewBlockCandidate, RandomNonce} ->
            CurrentMiningNonce = aec_pow:next_nonce(RandomNonce),
            epoch_mining:info("New txs added for mining; "
                              "regenerated block candidate and nonce "
                              "(max ~p, current ~p).",
                              [RandomNonce, CurrentMiningNonce]),
            {next_state, running,
             State#state{block_candidate = NewBlockCandidate,
                         initial_cycle_nonce = CurrentMiningNonce,
                         max_block_candidate_nonce = RandomNonce},
             [mine_event()]};
        {error, key_not_found} ->
            report_suspended(),
            {next_state, waiting_for_keys, State,
             [{next_event, cast, check_keys}]};
        {error, Reason} ->
            epoch_mining:error("Application of new txs failed: ~p", [Reason]),
            {next_state, configure, State,
            [{next_event, cast, bump_initial_cycle_nonce}]}
    end;
configure(cast, bump_initial_cycle_nonce,
          #state{fetch_new_txs_from_pool = false,
                 initial_cycle_nonce = Nonce0} = State) ->
    Nonce = aec_pow:next_nonce(Nonce0),
    epoch_mining:info("Not fetching new txs; "
                      "continuing mining with bumped nonce "
                      "(was ~p, is ~p).", [Nonce0, Nonce]),
    {next_state, running,
     State#state{initial_cycle_nonce = Nonce},
     [{next_event, cast, mine}]
    };
configure(cast, mine, State) ->
    %% The mine message should come when running.
    epoch_mining:error("Got mine event in configure state~n", []),
    %% Keep the event and wait for a state transition.
    {keep_state, State, [postpone]};
configure(cast, check_keys, State) ->
    %% The check_keys message should come when waiting.
    epoch_mining:error("Got check_keys event in configure state~n", []),
    %% Keep the event and wait for a state transition.
    {keep_state, State, [postpone]};
configure(_Type, _Msg, State) ->
    %% log and drop unknown events.
    epoch_mining:error("Unknown event in configure: ~p:~p~n", [_Type,_Msg]),
    {keep_state, State}.

%% -------------------------------------------------------------------
%% The running state
%% -------------------------------------------------------------------
%% Calls: start | suspend
running({call, From}, start, #state{ miner = none} = State) ->
    epoch_mining:info("Mining resumed by user.", []),
    {next_state, configure, State,
     [{reply, From, ok},
      {next_event, cast, create_block_candidate}]
    };
running({call, From}, start, State) ->
    {keep_state, State, [{reply, From, ok}]};
running({call, From}, suspend, State) ->
    epoch_mining:info("Mining suspended by user.", []),
    {next_state, idle, State, [{reply, From, ok}]};

%% Casts: post_block | miner_done | create_block_candidate |
%%        bump_initial_cycle_nonce | mine | check_keys
running(cast, {post_block, Block}, State) ->
    running_post_block(Block, State);
running(cast, {miner_done, Miner}, #state{ miner = Miner } = State) ->
    {next_state, configure, State#state{ miner = none }};
running(cast, {miner_done, Miner}, #state{ miner = OtherMiner } = State) ->
    epoch_mining:error("Unknown miner finished: ~p~n", [Miner]),
    {keep_state, State#state{ miner = OtherMiner }};
running(cast, create_block_candidate, State) ->
    {next_state, configure, State, [postpone]};
running(cast, bump_initial_cycle_nonce, State) ->
    {next_state, configure, State, [postpone]};
running(cast, mine, #state{block_candidate = BlockCandidate,
                           initial_cycle_nonce = CurrentMiningNonce,
                           miner = none}
        = State) ->
    Miner = mine(BlockCandidate, CurrentMiningNonce, State),
    {keep_state, State#state{ miner = Miner }};
running(cast, mine, #state{miner = _Other}= State) ->
    %% Already started mining
    %% (Future implementations could allow this, starting from new nonce.)
    epoch_mining:info("Trying to start while mining: ~p~n", [_Other]),
    {keep_state, State, [postpone]};
running(cast, check_keys, State) ->
    %% The check_keys message should come when waiting.
    epoch_mining:error("Got check_keys event in running state~n", []),
    %% Keep the event and wait for a state transition.
    {keep_state, State, [postpone]};
%% Catchall, log & drop unwanted messages.
running(_Type, _Msg, State) ->
    epoch_mining:error("Unknown event in running: ~p:~p~n", [_Type,_Msg]),
    {keep_state, State}.

%% -------------------------------------------------------------------
%% The Waiting state
%% -------------------------------------------------------------------
%% Calls: start | suspend
waiting_for_keys({call, From}, start, State) ->
    {keep_state, State, [{reply, From, ok}]};
waiting_for_keys({call, From}, suspend, State) ->
    {next_state, idle, State, [{reply, From, ok}]};

%% Casts: post_block | miner_done | create_block_candidate |
%%        bump_initial_cycle_nonce | mine | check_keys
waiting_for_keys(cast, {post_block, Block}, State) ->
    waiting_post_block(Block, State);
waiting_for_keys(cast, {miner_done, Miner}, #state{ miner = Miner } = State) ->
    {keep_state, State#state{ miner = none }};
waiting_for_keys(cast, {miner_done, Miner},
                 #state{ miner = OtherMiner } = State) ->
    epoch_mining:error("Unknown miner finished: ~p~n", [Miner]),
    {keep_state, State#state{ miner = OtherMiner }};
waiting_for_keys(cast, create_block_candidate, State) ->
    {keep_state, State, [postpone]};
waiting_for_keys(cast, bump_initial_cycle_nonce, State) ->
    {keep_state, State, [postpone]};
waiting_for_keys(cast, mine, State) ->
    {keep_state, State, [postpone]};
waiting_for_keys(cast, check_keys, State) ->
    timer:sleep(1000),
    case aec_keys:pubkey() of
        {ok, _Pubkey} ->
            epoch_mining:info("Key available, mining resumed.", []),
            {next_state, configure, State,
             [candidate_event()]};
        {error, _} ->
            report_suspended(),
            %% Add a new check_keys event to the *end* of the queue.
            gen_statem:cast(?SERVER, check_keys),
            {keep_state, State}
    end;
%% Catchall, log & drop unwanted messages.
waiting_for_keys(_Type, _Msg, State) ->
    epoch_mining:error("Unknown event in waiting: ~p:~p~n", [_Type,_Msg]),
    {keep_state, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

callback_mode() ->
    state_functions.

%%%===================================================================
%%% Internal functions
%%%===================================================================

mine(BlockCandidate, CurrentMiningNonce, State) ->
    epoch_mining:info("Start mining~n", []),
    %% Run the mining concurrently.
    Miner =
        spawn_link(fun () ->
                           int_mine(BlockCandidate, CurrentMiningNonce, State)
                   end),
    epoch_mining:info("Miner ~p with nonce ~p~n", [Miner, CurrentMiningNonce]),
    Miner.

int_mine(BlockCandidate, CurrentMiningNonce, State) ->
    Res = aec_mining:mine(BlockCandidate, CurrentMiningNonce),
    epoch_mining:info("Miner ~p finished with ~p ~n", [self(), Res]),

    gen_statem:cast(?SERVER, {miner_done, self()}),
    case Res of
        {ok, Block} ->
            ws_handler:broadcast(miner, mined_block, [{height,
                                                       aec_blocks:height(Block)}]),
            ok = save_mined_block(Block),
            start_if_auto(State);
        {error, no_solution} ->
            try exometer:update([ae,epoch,aecore,mining,retries], 1)
            catch error:_ -> ok end,
            epoch_mining:info("Failed to mine block, "
                              "no solution (nonce ~p); "
                              "retrying.", [CurrentMiningNonce]),
            gen_statem:cast(?SERVER, bump_initial_cycle_nonce);
        {error, {runtime, Reason}} ->
            try exometer:update([ae,epoch,aecore,mining,retries], 1)
            catch error:_ -> ok end,
            epoch_mining:error("Failed to mine block, runtime error; "
                               "retrying with different nonce (was ~p). "
                               "Error: ~p", [CurrentMiningNonce, Reason]),
            gen_statem:cast(?SERVER, bump_initial_cycle_nonce)
    end.



-spec save_mined_block(block()) -> ok.
save_mined_block(Block) ->
    Header = aec_blocks:to_header(Block),
    case aec_chain:insert_header(Header) of
        ok ->
            {ok, OldTop} = aec_chain:top(),
            case aec_chain:write_block(Block) of
                ok ->
                    try exometer:update([ae,epoch,aecore,mining,blocks_mined], 1)
                    catch error:_ -> ok end,
                    epoch_mining:info("Block inserted: Height = ~p"
                                      "~nHash = ~s",
                                      [Block#block.height,
                                       as_hex(Block#block.root_hash)]),
                    aec_events:publish(block_created, Block),
                    maybe_update_transactions(OldTop),
                    ok;
                {error, Reason} ->
                    epoch_mining:error("Block insertion failed: ~p.", [Reason])
            end;
        {error, Reason} ->
            epoch_mining:error("Header insertion failed: ~p.", [Reason])
    end.

report_suspended() ->
    epoch_mining:error("Mining suspended as no keys are avaiable for signing.", []).


idle_post_block(Block, State) ->
    case int_post_block(Block, State) of
        new_top ->
            start_if_auto(State),
            {next_state, idle, State};
        ok ->
            {next_state, idle, State}
    end.

configure_post_block(Block, State) ->
    case int_post_block(Block, State) of
        new_top ->
            start_if_auto(State),
            {next_state, configure, State};
        ok ->
            {next_state, configure, State}
    end.

running_post_block(Block, State) ->
    case int_post_block(Block, State) of
        new_top ->
            start_if_auto(State),
            {next_state, configure, State};
        ok ->
            epoch_mining:info("Post block not on top of chain"),
            {next_state, running, State}
    end.

waiting_post_block(Block, State) ->
    int_post_block(Block, State),
    {next_state, waiting_for_keys, State}.

int_post_block(Block,_State) ->
    epoch_mining:info("write_block: ~p", [Block]),
    Header = aec_blocks:to_header(Block),
    {ok, HH} = aec_headers:hash_header(Header),
    case aec_chain:get_block_by_hash(HH) of
        {ok, _Existing} ->
            epoch_mining:debug("Aleady have block", []),
            ok;
        {error, _} ->
            case {aec_headers:validate(Header), aec_blocks:validate(Block)} of
                {ok, ok} ->
                    case aec_chain:insert_header(Header) of
                        ok ->
                            %% Write block
                            {ok, OldTop} = aec_chain:top(),
                            Res = aec_chain:write_block(Block),
                            epoch_mining:debug("write_block result: ~p", [Res]),
                            %% Gossip
                            aec_events:publish(block_received, Block),
                            maybe_update_transactions(OldTop);
			{error, Reason} ->
                            lager:debug("Couldn't insert header (~p)", [Reason]),
                            ok
                    end;
                {{error, Reason}, _} ->
                    lager:info("Malformed block posted to the node (~p)", [Reason]),
                    error;
                {ok, {error, Reason}} ->
                    lager:info("Malformed block posted to the node (~p)", [Reason]),
                    error
            end
    end.

maybe_update_transactions(OldTop) ->
    case aec_chain:top() of
        {ok, OldTop} ->
            ok;
        {ok, NewTop} ->
            update_transactions(OldTop, NewTop),
            new_top
    end.

update_transactions(OldTop, NewTop) ->
    Hash1 = block_to_hash(OldTop),
    Hash2 = block_to_hash(NewTop),
    {ok, Ancestor} = aec_chain:common_ancestor(Hash1, Hash2),
    {ok, TransactionsOnOldChain} =
        aec_chain:get_transactions_between(Hash1, Ancestor),
    {ok, TransactionsOnNewChain} =
        aec_chain:get_transactions_between(Hash2, Ancestor),
    ok = aec_tx_pool:fork_update(TransactionsOnNewChain, TransactionsOnOldChain),
    ok.

block_to_hash(Block) ->
    Header = aec_blocks:to_header(Block),
    {ok, Hash} = aec_headers:hash_header(Header),
    Hash.

start_if_auto(#state{autostart = true}) ->
    gen_statem:cast(?SERVER, create_block_candidate),
    true;
start_if_auto(_) -> false.

as_hex(S) ->
    [io_lib:format("~2.16.0b",[X]) || <<X:8>> <= S].

candidate_event()     -> cast_event(create_block_candidate).
mine_event()          -> cast_event(mine).
cast_event(Msg)       -> next_event(cast, Msg).
next_event(Type, Msg) -> {next_event, Type, Msg}.
