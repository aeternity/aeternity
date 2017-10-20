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
         stop/0,
         resume/0,
         suspend/0,
         get_balance/0]).

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

%% Cuckoo Cycle has a solution probability of 2.2% so we are
%% likely to succeed in 100 steps (~90%).
-define(MINING_ATTEPTS_PER_CYCLE, 10).
-define(FETCH_NEW_TXS_FROM_POOL, true).

-record(state, {block_candidate :: block() | undefined,
                cycle_attempts_count = ?MINING_ATTEPTS_PER_CYCLE :: non_neg_integer(),
                initial_cycle_nonce = 0 :: integer(),
                max_block_candidate_nonce = 0 :: integer(),
                fetch_new_txs_from_pool :: boolean()}).

%%%===================================================================
%%% API
%%%===================================================================

%%------------------------------------------------------------------------------
%% Start the state machine
%%------------------------------------------------------------------------------
-spec start_link() -> {'ok', pid()} | {error | term()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

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
%% @end
%%--------------------------------------------------------------------
init(_) ->
    CycleAttemptsCount = application:get_env(aecore, mining_cycle_attempts_count, ?MINING_ATTEPTS_PER_CYCLE),
    FetchNewTxsFromPool = application:get_env(aecore, fetch_new_txs_from_pool_during_mining, ?FETCH_NEW_TXS_FROM_POOL),

    gen_statem:cast(?SERVER, create_block_candidate),
    {ok, configure, #state{cycle_attempts_count = CycleAttemptsCount,
                           fetch_new_txs_from_pool = FetchNewTxsFromPool}}.

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
idle({call, From}, start, State) ->
    epoch_mining:info("Mining resumed by user.", []),
    gen_statem:cast(?SERVER, create_block_candidate),
    {next_state, configure, State, [{reply, From, ok}]};
idle({call, From}, suspend, State) ->
    {next_state, idle, State, [{reply, From, {error, not_started}}]};
idle({call, From}, _Msg, State) ->
    {next_state, idle, State, [{reply, From, {error, not_supported}}]};
idle(_Type, _Msg, State) ->
    {next_state, idle, State}.

configure({call, From}, start, State) ->
    {next_state, configure, State, [{reply, From, {error, already_started}}]};
configure({call, From}, suspend, State) ->
    epoch_mining:info("Mining suspended by user", []),
    {next_state, idle, State, [{reply, From, ok}]};
configure(cast, create_block_candidate, State) ->
    case aec_mining:create_block_candidate() of
        {ok, BlockCandidate, InitialNonce, MaxMiningNonce} ->
            gen_statem:cast(?SERVER, mine),
            {next_state, running, State#state{block_candidate = BlockCandidate,
                                              initial_cycle_nonce = InitialNonce,
                                              max_block_candidate_nonce = MaxMiningNonce}};
        {error, key_not_found} ->
            report_suspended(),
            gen_statem:cast(?SERVER, check_keys),
            {next_state, waiting_for_keys, State};
        {error, Reason} ->
            epoch_mining:error("Creation of block candidate failed: ~p", [Reason]),
            gen_statem:cast(?SERVER, create_block_candidate),
            {next_state, configure, State}
    end;
configure(cast, bump_initial_cycle_nonce, #state{fetch_new_txs_from_pool = true,
                                                 block_candidate = BlockCandidate,
                                                 cycle_attempts_count = CycleAttemptsCount,
                                                 initial_cycle_nonce = InitialCycleNonce0} = State) ->
    case aec_mining:apply_new_txs(BlockCandidate) of
        {ok, BlockCandidate} ->
            %% No new txs applied to the block.
            %% Continue incrementing nonce, without block candidate modifications.
            epoch_mining:info("No new txs available; continuing mining with bumped nonce"),
            InitialCycleNonce = (InitialCycleNonce0 + CycleAttemptsCount) band 16#7fffffff,
            gen_statem:cast(?SERVER, mine),
            {next_state, running, State#state{initial_cycle_nonce = InitialCycleNonce}};
        {ok, NewBlockCandidate, InitialNonce, MaxMiningNonce} ->
            epoch_mining:info("New txs added for mining"),
            gen_statem:cast(?SERVER, mine),
            {next_state, running, State#state{block_candidate = NewBlockCandidate,
                                              initial_cycle_nonce = InitialNonce,
                                              max_block_candidate_nonce = MaxMiningNonce}};
        {error, key_not_found} ->
            report_suspended(),
            gen_statem:cast(?SERVER, check_keys),
            {next_state, waiting_for_keys, State};
        {error, Reason} ->
            epoch_mining:error("Application of new txs failed: ~p", [Reason]),
            gen_statem:cast(?SERVER, bump_initial_cycle_nonce),
            {next_state, configure, State}
    end;
configure(cast, bump_initial_cycle_nonce, #state{initial_cycle_nonce = InitialCycleNonce0,
                                                 cycle_attempts_count = CycleAttemptsCount} = State) ->
    InitialCycleNonce = (InitialCycleNonce0 + CycleAttemptsCount) band 16#7fffffff,
    gen_statem:cast(?SERVER, mine),
    {next_state, running, State#state{initial_cycle_nonce = InitialCycleNonce}};
configure({call, From}, _Msg, State) ->
    {next_state, configure, State, [{reply, From, {error, not_supported}}]};
configure(_Type, _Msg, State) ->
    {next_state, idle, State}.

running({call, From}, start, State) ->
    {next_state, running, State, [{reply, From, {error, already_started}}]};
running({call, From}, suspend, State) ->
    epoch_mining:info("Mining suspended by user.", []),
    {next_state, idle, State, [{reply, From, ok}]};
running(cast, mine, #state{block_candidate = BlockCandidate,
                           cycle_attempts_count = CycleAttemptsCount,
                           initial_cycle_nonce = CurrentMiningNonce,
                           max_block_candidate_nonce = MaxMiningNonce} = State) ->
    case aec_mining:mine(BlockCandidate, CycleAttemptsCount, CurrentMiningNonce, MaxMiningNonce) of
        {ok, Block} ->
            ok = save_mined_block(Block),
            gen_statem:cast(?SERVER, create_block_candidate),
            {next_state, configure, State};
        {error, generation_count_exhausted} ->
            try exometer:update([ae,epoch,aecore,mining,retries], 1)
            catch error:_ -> ok end,
            epoch_mining:info("Failed to mine block in ~p attempts, retrying.",
                              [CycleAttemptsCount]),
            gen_statem:cast(?SERVER, bump_initial_cycle_nonce),
            {next_state, configure, State};
        {error, nonce_range_exhausted} ->
            try exometer:update([ae,epoch,aecore,mining,retries], 1)
            catch error:_ -> ok end,
            epoch_mining:info("Failed to mine block, nonce range exhausted; regenerating block candidate"),
            gen_statem:cast(?SERVER, create_block_candidate),
            {next_state, configure, State}
    end;
running({call, From}, _Msg, State) ->
    {next_state, running, State, [{reply, From, {error, not_supported}}]};
running(cast, _Msg, State) ->
    {next_state, idle, State}.

waiting_for_keys(cast, check_keys, State) ->
    timer:sleep(1000),
    case aec_keys:pubkey() of
        {ok, _Pubkey} ->
            epoch_mining:info("Key available, mining resumed.", []),
            gen_statem:cast(?SERVER, create_block_candidate),
            {next_state, configure, State};
        {error, _} ->
            report_suspended(),
            gen_statem:cast(?SERVER, check_keys),
            {next_state, waiting_for_keys, State}
    end;
waiting_for_keys({call, From}, resume, State) ->
    {next_state, idle, State, [{reply, From, {error, already_started}}]};
waiting_for_keys({call, From}, suspend, State) ->
    {next_state, idle, State, [{reply, From, ok}]};
waiting_for_keys(_, _, State) ->
    {next_state, waiting_for_keys, State}.

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

-spec save_mined_block(block()) -> ok.
save_mined_block(Block) ->
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
                    aec_sync:block_created(Block);
                {error, Reason} ->
                    epoch_mining:error("Block insertion failed: ~p.", [Reason])
            end;
        {error, Reason} ->
            epoch_mining:error("Header insertion failed: ~p.", [Reason])
    end.

report_suspended() ->
    epoch_mining:error("Mining suspended as no keys are avaiable for signing.", []).

as_hex(S) ->
    [io_lib:format("~2.16.0b",[X]) || <<X:8>> <= S].
