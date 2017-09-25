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
         running/3,
         waiting_for_keys/3,
         terminate/3,
         code_change/4,
         callback_mode/0]).

-include("common.hrl").
-include("blocks.hrl").


-define(SERVER, ?MODULE).
%% TODO: make it configurable
-define(MINING_ATTEPTS_PER_CYCLE, 10).

-record(state, {}).

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
    gen_statem:cast(?SERVER, mine),
    {ok, running, #state{}}.

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
    gen_statem:cast(?SERVER, mine),
    {next_state, running, State, [{reply, From, ok}]};
idle({call, From}, suspend, State) ->
    {next_state, idle, State, [{reply, From, {error, not_started}}]};
idle({call, From}, _Msg, State) ->
    {next_state, idle, State, [{reply, From, {error, not_supported}}]};
idle(_Type, _Msg, State) ->
    {next_state, idle, State}.

running({call, From}, start, State) ->
    {next_state, running, State, [{reply, From, {error, already_started}}]};
running({call, From}, suspend, State) ->
    epoch_mining:info("Mining suspended by user.", []),
    {next_state, idle, State, [{reply, From, ok}]};
running(cast, mine, State) ->
    case aec_mining:mine(?MINING_ATTEPTS_PER_CYCLE) of
        {ok, Block} ->
            Header = aec_blocks:to_header(Block),
            case aec_chain:insert_header(Header) of
                ok ->
                    case aec_chain:write_block(Block) of
                        ok ->
                            epoch_mining:info("Block inserted: Height = ~p"
                                              "~nHash = ~s",
                                              [Block#block.height,
                                               as_hex(Block#block.root_hash)]);
                        {error, Reason} ->
                            epoch_mining:error("Block insertion failed: ~p.", [Reason])
                    end;
                {error, Reason} ->
                    epoch_mining:error("Header insertion failed: ~p.", [Reason])
            end,
            gen_statem:cast(?SERVER, mine),
            {next_state, running, State};
        {error, generation_count_exhausted} ->
            %% Needs more attempts, go on trying
            epoch_mining:info("Failed to mine block in ~p attempts, retrying.",
                       [?MINING_ATTEPTS_PER_CYCLE]),
            gen_statem:cast(?SERVER, mine),
            {next_state, running, State};
        {error, key_not_found} ->
            report_suspended(),
            gen_statem:cast(?SERVER, check_keys),
            {next_state, waiting_for_keys, State};
        {error, Reason} ->
            epoch_mining:error("Mining attempt failed with error: ~p.", [Reason]),
            gen_statem:cast(?SERVER, mine),
            {next_state, running, State}
    end;
running({call, From}, _Msg, State) ->
    {next_state, running, State, [{reply, From, {error, not_supported}}]};
running(_Type, _Msg, State) ->
    {next_state, idle, State}.

waiting_for_keys(cast, check_keys, State) ->
    timer:sleep(1000),
    case aec_keys:pubkey() of
        {ok, _Pubkey} ->
            epoch_mining:info("Key available, mining resumed.", []),
            gen_statem:cast(?SERVER, mine),
            {next_state, running, State};
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

report_suspended() ->
    epoch_mining:error("Mining suspended as no keys are avaiable for signing.", []).

as_hex(S) ->
    [io_lib:format("~2.16.0b",[X]) || <<X:8>> <= S].
