%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity
%%% @doc
%%% Manage interaction with hyperchain parent chain
%%% @end
%%%-------------------------------------------------------------------

-module(aec_parent_chain_cache).

%% Functionality:
%% - cache the view of the parent chain's blocks. The `aec_parent_connector`
%%   reports any new blocks and they are stored in a cache.
%%   `aec_parent_connector` is also used for fetching blocks as we go along
%% - keeps track of current child chain top and fetches releated blocks. The
%%   node is syncing blocks from the past. In this case the cache is loading
%%   the lowest required parent chain block and then starts syncing upwards
%%   till it reaches the top. After that the node is fully synced and we are
%%   waiting for notifications from the parent chain nodes for top changes
%% - provides parent chain blocks to the consensus module on demand. If it
%%   asks for an older block, it is fetched as well. This may take some
%%   more time as more blocks need to be queried
%% - cleans up older states
%% - is fork aware
-behaviour(gen_server).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([start_link/3, stop/0]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-export([post_block/1,
         get_block_by_height/1]).

-export([get_state/0]).

-define(SERVER, ?MODULE).
-define(FOLLOW_PC_TOP, follow_parent_chain_top).
-define(FOLLOW_CHILD_TOP, sync_child_chain).

-record(state,
    {
        child_top_height                    :: non_neg_integer(),
        child_start_height                  :: non_neg_integer(),
        pc_confirmations                    :: non_neg_integer(),
        max_size                            :: non_neg_integer(),
        blocks          = #{}               :: #{non_neg_integer() => aec_parent_chain_block:block()},
        top_height      = 0                 :: non_neg_integer(),
        sign_module     = aec_preset_keys   :: atom(), %% TODO: make it configurable
        initial_commits_heights = []         :: list()
    }).
-type state() :: #state{}.



%%%=============================================================================
%%% API
%%%=============================================================================
%% Start the parent chain cache process
-spec start_link(non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(Height, Size, Confirmations) ->
    Args = [Height, Size, Confirmations],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

stop() ->
    gen_server:stop(?SERVER).

-spec post_block(aec_parent_chain_block:block()) -> ok.
post_block(Block) ->
    gen_server:cast(?SERVER, {post_block, Block}).

-spec get_block_by_height(non_neg_integer()) -> {ok, aec_parent_chain_block:block()}
                                              | {error, not_in_cache}
                                              | {error, {not_enough_confirmations, aec_parent_chain_block:block()}}.
get_block_by_height(Height) ->
    case gen_server:call(?SERVER, {get_block_by_height, Height}) of
        {ok, _B} = OK -> OK;
        {error, not_in_cache} = Err ->
            Err;
        {error, {not_enough_confirmations, _}} = Err -> Err
    end.

-spec get_state() -> {ok, map()}.
get_state() ->
    gen_server:call(?SERVER, get_state).



%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init([any()]) -> {ok, #state{}}.
init([StartHeight, Size, Confirmations]) ->
    aec_events:subscribe(top_changed),
    ChildHeight = aec_chain:top_height(),
    true = is_integer(ChildHeight),
    InitialCommitsHeights = lists:seq(StartHeight, StartHeight + Confirmations - 1),
    self() ! initialize_cache,

    {ok, #state{child_start_height      = StartHeight,
                child_top_height        = ChildHeight,
                pc_confirmations        = Confirmations, 
                max_size                = Size,
                blocks                  = #{},
                initial_commits_heights = InitialCommitsHeights}}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({get_block_by_height, Height}, _From,
            #state{pc_confirmations = Confirmations,
                   top_height = TopHeight } = State) ->
    Reply = 
        case get_block(Height, State) of
            {error, _} = Err -> Err;
            {ok, Block} when Height > TopHeight - Confirmations ->
                {error, {not_enough_confirmations, Block}};
            {ok, _Block} = OK -> OK
        end,
    {reply, Reply, State};
handle_call(get_state, _From, State) ->
    Reply = {ok, state_to_map(State)},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = unhandled,
    {reply, Reply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({post_block, Block}, #state{} = State0) ->
    State = post_block(Block, State0),
    State1 = maybe_post_initial_commitments(Block, State),
    {noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(initialize_cache, State) ->
    TargetHeight = target_parent_height(State),
    case aec_parent_connector:fetch_block_by_height(TargetHeight) of
        {ok, B} ->
            aec_parent_connector:request_top(),
            State1 = post_block(B, State),
            State2 = maybe_post_initial_commitments(B, State1),
            {noreply, State2};
        {error, not_found} ->
            lager:debug("Waiting for block ~p to be mined on the parent chain", [TargetHeight]),
            timer:send_after(1000, initialize_cache),
            {noreply, State};
        {error, no_parent_chain_agreement} ->
            lager:warning("Failed to initialize cache for height ~p", [TargetHeight]),
            timer:send_after(1000, initialize_cache),
            {noreply, State}
    end;
handle_info({gproc_ps_event, top_changed, #{info := #{block_type := key,
                                                      block_hash := Hash,
                                                      height     := Height}}},
            #state{child_top_height = OldHeight,
                   max_size = MaxSize} = State0) ->
    State1 = State0#state{child_top_height = Height},
    State2 =
        case OldHeight + MaxSize < Height of
            true -> State1#state{blocks = #{}}; %% we flush the whole state - this should not happen
            false -> State1
        end,
    TargetHeight = target_parent_height(State2),
    aec_parent_connector:request_block_by_height(TargetHeight),
    State = maybe_post_commitments(Hash, State2),
    {noreply, State};
handle_info({gproc_ps_event, top_changed, _}, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    lager:debug("Unhandled info: ~p", [_Info]),
    {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec insert_block(aec_parent_chain_block:block(), state()) -> state().
insert_block(Block, #state{blocks = Blocks} = State) ->
    Height = aec_parent_chain_block:height(Block),
    State#state{blocks = maps:put(Height, Block, Blocks)}.

-spec get_block(non_neg_integer(), state()) -> {ok, aec_parent_chain_block:block()} | {error, not_in_cache}.
get_block(Height, #state{blocks = Blocks}) ->
    case maps:find(Height, Blocks) of
        {ok, _Block} = OK -> OK;
        error ->
            {error, not_in_cache}
    end.
    
-spec max_block(state()) -> non_neg_integer() | empty_cache.
max_block(#state{blocks = Blocks}) when map_size(Blocks) =:= 0 ->
    empty_cache;
max_block(#state{blocks = Blocks}) ->
    lists:max(maps:keys(Blocks)).
    
-spec delete_block(non_neg_integer(), state()) -> state().
delete_block(Height, #state{blocks = Blocks} = State) ->
    State#state{blocks = maps:remove(Height, Blocks)}.

state_to_map(#state{child_start_height = StartHeight,
                    child_top_height   = ChildHeight,
                    pc_confirmations   = Confirmations,
                    max_size     = MaxSize,
                    blocks       = Blocks, 
                    top_height   = TopHeight}) ->
    #{  child_start_height => StartHeight,
        child_top_height   => ChildHeight,
        pc_confirmations   => Confirmations,
        max_size     => MaxSize,
        blocks       => Blocks, 
        top_height   => TopHeight}.

target_parent_height(#state{child_start_height    = StartHeight,
                            child_top_height      = ChildHeight}) ->
    ChildHeight + StartHeight.

min_cachable_parent_height(#state{max_size   = CacheSize,
                                  top_height = ParentTop} = State) ->
    min(target_parent_height(State), ParentTop - CacheSize ).

post_block(Block, #state{max_size = MaxSize,
                         top_height = TopHeight} = State0) ->
    TargetHeight = target_parent_height(State0),
    BlockHeight = aec_parent_chain_block:height(Block),
    MaxBlockToRequest = TargetHeight + MaxSize - 1,
    GCHeight = min_cachable_parent_height(State0),
    case BlockHeight < GCHeight of
        true ->
            State0;
        false ->
            case BlockHeight > MaxBlockToRequest of
                true ->
                    %% we are syncing and this is a new top block that is too far away
                    %% from the future; we ignore it;
                    %% we still keep the knowledge of seeing this block
                    State = State0#state{top_height = max(TopHeight, BlockHeight)},
                    MaxBlock =
                        case max_block(State) of
                            empty_cache -> TargetHeight;
                            H -> H
                        end,
                    maybe_request_next_block(MaxBlock, State),
                    State;
                false ->
                    %% the block received might be the top one or a previous one; we try GCing
                    %% older blocks according to the top block only;
                    %% if the previous block is missing, fetch it (if above the GC height)
                    State1 =
                        case BlockHeight > GCHeight of
                            true ->
                                insert_block(Block, State0);
                            false ->
                                State0
                        end,
                    TryGCHeight = BlockHeight - MaxSize,
                    State2 =
                        case TryGCHeight >= 0 of
                            true ->
                                delete_block(TryGCHeight, State1);
                            false -> State1
                        end,
                    State3 = State2#state{top_height = max(TopHeight, BlockHeight)},
                    maybe_request_previous_block(BlockHeight, State3),
                    maybe_request_next_block(BlockHeight, State3),
                    State3
            end
    end.

maybe_request_previous_block(0 = _BlockHeight, _State) -> pass;
maybe_request_previous_block(BlockHeight, #state{} = State) ->
    PrevHeight = BlockHeight - 1,
    case PrevHeight >= min_cachable_parent_height(State) of
        true ->
            %% check if the previous block is missing
            case get_block(PrevHeight, State) of
                {ok, _} -> pass;
                {error, _} -> %% missing block detected
                    lager:debug("Missing block with height ~p detected, requesting it", [PrevHeight]),
                    aec_parent_connector:request_block_by_height(PrevHeight)
            end;
        false ->
            pass
    end.

maybe_request_next_block(BlockHeight, #state{top_height = TopHeight}) when BlockHeight >= TopHeight ->
    pass;
maybe_request_next_block(BlockHeight, #state{max_size = MaxSize } = State) ->
    TargetHeight = target_parent_height(State),
    MaxBlockToRequest = TargetHeight + MaxSize,
    NextHeight = BlockHeight + 1,
    case MaxBlockToRequest > BlockHeight andalso NextHeight =< MaxBlockToRequest of
        true ->
            case get_block(NextHeight, State) of
                {ok, _} -> pass;
                {error, not_in_cache} ->
                    lager:debug("Populating the cache forward, requesting block with height ~p",
                                [NextHeight]),
                    aec_parent_connector:request_block_by_height(NextHeight)
            end;
        _ -> %% cache is already full
            pass
    end.

maybe_post_commitments(TopHash, #state{sign_module = SignModule} = State) ->
    case posting_commitments_enabled() of
        true ->
            post_commitments(TopHash, #state{sign_module = SignModule} = State);
        false ->
            lager:debug("Will not post commitments, disabled", []),
            State
    end.


post_commitments(TopHash, #state{sign_module = SignModule} = State) ->
    %% use the key top hash as there could be a race condition. If there is a
    %% microblock right after the keyblock, the state trees could have changed
    %% (and could have modified the staker's state) and using
    %% aetx_env:tx_env_and_trees_from_top/1 could be dangerous
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_hash(aetx_transaction, TopHash),
    {ok, AllStakers} = aec_consensus_hc:parent_chain_validators(TxEnv, Trees),
    LocalStakers =
        lists:filter(fun SignModule:is_key_present/1, AllStakers),
    Commitment = aeser_api_encoder:encode(key_block_hash, TopHash),
    lists:foreach(
        fun(Staker) ->
            case aec_parent_connector:post_commitment(Staker, Commitment) of
                ok ->
                    lager:debug("Posted commitment for staker ~p", [Staker]),
                    ok;
                {error, _Err} ->
                    %% TODO: maybe repost?
                    lager:debug("Did NOT post commitment for staker ~p because of ~p", [Staker, _Err]),
                    pass
            end,
            ok
        end,
        LocalStakers),
    State.


%% we post commitments when there is a new block on the child chain
%% the situation is a bit different for commitments right after the
%% genesis block before the block with height 1: those are not posted as
%% there is no trigger for the genesis block itself
%% We detect once there is a block that would trigger one with height 1
maybe_post_initial_commitments(Block, #state{pc_confirmations = Confirmations,
                                             initial_commits_heights = InitialCommitsHeights} = State) ->
    Height = aec_parent_chain_block:height(Block),
    ChildStartHeight = State#state.child_start_height,
    %% we start one block before the parent chain height that would result in block 1 of the child chain
    IsFirstCommitment = Height >= ChildStartHeight - 1 andalso
                        Height < ChildStartHeight + Confirmations,
    NotPostedYet = lists:member(Height, InitialCommitsHeights),
    IsPublishingCommitments = posting_commitments_enabled(),
    case IsFirstCommitment andalso NotPostedYet andalso IsPublishingCommitments of
        true ->
            Hash = aec_chain:genesis_hash(), %% TODO: maybe reconsider if we shall post Genesis hash before seeing any block on the parent chain or a different approach should be taken
            State1 = maybe_post_commitments(Hash, State),
            State1#state{initial_commits_heights = lists:delete(Height, InitialCommitsHeights)};
        false ->
            State
    end.

posting_commitments_enabled() ->
    case aec_conductor:get_mining_state() of
        running -> true;
        stopped -> false
    end.


