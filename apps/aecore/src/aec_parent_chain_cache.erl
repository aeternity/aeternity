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
-export([start_link/5, stop/0]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-export([post_block/1,
         get_block_by_height/1,
         get_commitments/1]).

-ifdef(TEST).

-export([get_state/0]).

-endif.

-define(SERVER, ?MODULE).
-define(FOLLOW_PC_TOP, follow_parent_chain_top).
-define(FOLLOW_CHILD_TOP, sync_child_chain).

-record(state,
    {
        child_top_height                        :: non_neg_integer(),
        child_top_hash                          :: aec_blocks:block_header_hash(),
        child_start_height                      :: non_neg_integer(),
        max_size                                :: non_neg_integer(),
        block_cache         = #{}               :: #{non_neg_integer() => aec_parent_chain_block:block() | requested},
        blocks_hash_index   = #{}               :: #{aec_parent_chain_block:hash() => non_neg_integer()},
        top_height          = 0                 :: non_neg_integer(),
        sign_module         = aec_preset_keys   :: atom(), %% TODO: make it configurable
        producing_blocks    = false             :: boolean(),
        enabled_commitments = false             :: boolean(),
        is_syncing          = false             :: boolean(),
        posted_commitment   = false             :: boolean()
    }).
-type state() :: #state{}.



%%%=============================================================================
%%% API
%%%=============================================================================
%% Start the parent chain cache process
-spec start_link(non_neg_integer(), non_neg_integer(), non_neg_integer(),
                 boolean(), boolean()) ->
    {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(Height, Size, _Confirmations, IsProducingBlocks, IsPublishingCommitments) ->
    Args = [Height, Size, IsProducingBlocks, IsPublishingCommitments],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

stop() ->
    gen_server:stop(?SERVER).

-spec post_block(aec_parent_chain_block:block()) -> ok.
post_block(Block) ->
    gen_server:cast(?SERVER, {post_block, Block}).

-spec get_block_by_height(non_neg_integer()) -> {ok, aec_parent_chain_block:block()}
                                              | {error, not_in_cache}.
get_block_by_height(Height) ->
    case gen_server:call(?SERVER, {get_block_by_height, Height}) of
        {ok, _B} = OK -> OK;
        {error, not_in_cache} = Err ->
            Err
    end.

-spec get_commitments(binary()) -> {ok, list()}
                                | {error, not_in_cache}
                                | {error, not_fetched}.
get_commitments(Hash) ->
    case gen_server:call(?SERVER, {get_commitments, Hash}) of
        {ok, _B} = OK -> OK;
        {error, not_in_cache} = Err ->
            Err;
        {error, not_fetched} = Err ->
            Err
    end.


-ifdef(TEST).
-spec get_state() -> {ok, map()}.
get_state() ->
    gen_server:call(?SERVER, get_state).
-endif.


%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init([any()]) -> {ok, #state{}}.
init([StartHeight, Size, BlockProducing, EnabledCommitments]) ->
    aec_events:subscribe(top_changed),
    aec_events:subscribe(start_mining),
    aec_events:subscribe(stop_mining),
    aec_events:subscribe(chain_sync),
    TopHeader = aec_chain:top_header(),
    ChildHeight = aec_headers:height(TopHeader),
    {ok, ChildHash} = aec_headers:hash_header(TopHeader),
    true = is_integer(ChildHeight),
    self() ! initialize_cache,
    {ok, #state{child_start_height      = StartHeight,
                child_top_height        = ChildHeight,
                child_top_hash          = ChildHash,
                max_size                = Size,
                block_cache             = #{},
                producing_blocks        = BlockProducing,
                enabled_commitments     = EnabledCommitments
                }}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({get_block_by_height, Height}, _From, State) ->
    Reply =
        case get_block(Height, State) of
            {error, _} = Err -> Err;
            {ok, _Block} = OK -> OK
        end,
    {reply, Reply, State};
handle_call({get_commitments, Hash}, _From, State) ->
    Reply =
        case get_block_height_by_hash(Hash, State) of
            {error, _} = Err -> Err;
            {ok, Height} ->
                {ok, Block} = get_block(Height, State),
                case aec_parent_chain_block:commitments(Block) of
                    error -> {error, not_fetched}; %% TODO: maybe request them?
                    {ok, _Commitments} = Ok -> Ok
                end
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
    State1 =
        case State#state.top_height > State0#state.top_height of
            true ->
                maybe_post_initial_commitments(Block, State);
            false ->
                State
        end,
    {noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(initialize_cache, State) ->
    TargetHeight = target_parent_height(State),
    case catch aec_parent_connector:fetch_block_by_height(TargetHeight) of
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
            {noreply, State};
        {'EXIT', _} ->
            lager:warning("Failed to initialize cache for height EXIT ~p", [TargetHeight]),
            timer:send_after(1000, initialize_cache),
            {noreply, State}
    end;
handle_info({gproc_ps_event, top_changed, #{info := #{block_type := key,
                                                      block_hash := Hash,
                                                      height     := Height}}},
            #state{child_top_height = OldHeight,
                   max_size = MaxSize} = State0) ->
    State1 = State0#state{child_top_height = Height,
                          child_top_hash = Hash,
                          posted_commitment = false},
    State2 =
        case OldHeight + MaxSize < Height of
            true  -> State1#state{block_cache = #{}}; %% we flush the whole state - this should not happen
            false -> State1
        end,
    TargetHeight = target_parent_height(State2),
    maybe_request_block(TargetHeight, State2),
    State = maybe_post_commitments(TargetHeight, Hash, State2),
    {noreply, State};
handle_info({gproc_ps_event, chain_sync, #{info := {is_syncing, IsSyncing}}}, State0) ->
    State1 = State0#state{is_syncing = IsSyncing},
    State =
        case IsSyncing =:= false andalso not State0#state.posted_commitment of
            true ->
                {ok, TopBlock} = aec_chain:top_key_block(),
                {ok, TopHash} = aec_blocks:hash_internal_representation(TopBlock),

                _State1 = maybe_post_commitments(State0#state.child_start_height + aec_blocks:height(TopBlock), TopHash, State1);
            false ->
                lager:debug("Not posting commitment", []),
                State1
        end,
    {noreply, State};
handle_info({gproc_ps_event, top_changed, _}, State) ->
    {noreply, State};
handle_info({gproc_ps_event, start_mining, _}, State) ->
    {noreply, State#state{producing_blocks = true}};
handle_info({gproc_ps_event, stop_mining, _}, State) ->
    {noreply, State#state{producing_blocks = false}};
handle_info({gproc_ps_event, chain_sync, _}, State) ->
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
insert_block(Block, #state{block_cache = Blocks,
                           blocks_hash_index = Index} = State) ->
    Height = aec_parent_chain_block:height(Block),
    Hash = aec_parent_chain_block:hash(Block),
    State#state{block_cache = maps:put(Height, Block, Blocks),
                blocks_hash_index = maps:put(Hash, Height, Index)}.

-spec get_block(non_neg_integer(), state()) -> {ok, aec_parent_chain_block:block()} | {error, not_in_cache}.
get_block(Height, #state{block_cache = Blocks}) ->
    case maps:find(Height, Blocks) of
        {ok, requested}   -> {error, not_in_cache};
        {ok, _Block} = OK -> OK;
        error             -> {error, not_in_cache}
    end.

-spec get_block_height_by_hash(aec_parent_chain_block:hash(), state()) ->
    {ok, non_neg_integer()} | {error, not_in_cache}.
get_block_height_by_hash(Hash, #state{blocks_hash_index = Index}) ->
    case maps:find(Hash, Index) of
        {ok, _Height} = OK -> OK;
        error ->
            {error, not_in_cache}
    end.

-spec max_block(state()) -> non_neg_integer() | empty_cache.
max_block(#state{block_cache = Blocks}) when map_size(Blocks) =:= 0 ->
    empty_cache;
max_block(#state{block_cache = Blocks}) ->
    lists:max(maps:keys(Blocks)).

-spec delete_block(non_neg_integer(), state()) -> state().
delete_block(Height, #state{block_cache = Blocks,
                            blocks_hash_index = Index} = State) ->
  case maps:find(Height, Blocks) of
        {ok, requested} ->
            State#state{block_cache = maps:remove(Height, Blocks)};
        {ok, Block} ->
            Hash = aec_parent_chain_block:hash(Block),
            State#state{block_cache = maps:remove(Height, Blocks),
                        blocks_hash_index = maps:remove(Hash, Index)};
        error -> State
    end.

state_to_map(#state{child_start_height = StartHeight,
                    child_top_height   = ChildHeight,
                    max_size           = MaxSize,
                    block_cache        = Blocks,
                    top_height         = TopHeight}) ->
    #{ child_start_height => StartHeight,
       child_top_height   => ChildHeight,
       max_size           => MaxSize,
       blocks             => Blocks,
       top_height         => TopHeight}.

target_parent_height(#state{child_start_height = StartHeight,
                            child_top_height   = ChildHeight}) ->
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
                    maybe_request_next_block(MaxBlock, State);
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
                    State4 = maybe_request_previous_block(BlockHeight, State3),
                    maybe_request_next_block(BlockHeight, State4)
            end
    end.

request_block(Height, #state{block_cache = Blocks} = State) ->
    case maps:get(Height, Blocks, undefined) of
        undefined ->
            lager:debug("Missing block with height ~p detected, requesting it", [Height]),
            aec_parent_connector:request_block_by_height(Height),
            State#state{block_cache = Blocks#{Height => requested}};
        _ ->
            State
    end.

maybe_request_previous_block(0 = _BlockHeight, State) -> State;
maybe_request_previous_block(BlockHeight, #state{} = State) ->
    PrevHeight = BlockHeight - 1,
    case PrevHeight >= min_cachable_parent_height(State) of
        true ->
            request_block(PrevHeight, State);
        false ->
            State
    end.

maybe_request_block(BlockHeight, #state{top_height = TopHeight}) when BlockHeight > TopHeight ->
    aec_parent_connector:request_block_by_height(BlockHeight);
maybe_request_block(BlockHeight, State) ->
    maybe_request_next_block(BlockHeight - 1, State).

maybe_request_next_block(BlockHeight, #state{top_height = TopHeight} = State) when BlockHeight >= TopHeight ->
    State;
maybe_request_next_block(BlockHeight, #state{max_size = MaxSize} = State) ->
    TargetHeight = target_parent_height(State),
    MaxBlockToRequest = TargetHeight + MaxSize - 1,
    NextHeight = BlockHeight + 1,
    case MaxBlockToRequest > BlockHeight andalso NextHeight =< MaxBlockToRequest of
        true ->
            case get_block(NextHeight, State) of
                {ok, _} ->
                    maybe_request_next_block(NextHeight, State);
                {error, not_in_cache} ->
                    request_block(NextHeight, State)
            end;
        _ -> %% cache is already full
            State
    end.

maybe_post_commitments(TargetHeight, TopHash, #state{is_syncing = IsSyncing,
                                                     top_height = TopHeight,
                                                     posted_commitment = PostedCommitment} = State) ->
    PostingCommitments = posting_commitments_enabled(State),
    GoodHeight = TargetHeight >= TopHeight orelse TopHeight == 0,
    case PostingCommitments andalso not IsSyncing andalso not PostedCommitment andalso GoodHeight of
        true ->
            lager:debug("posting commitments", []),
            post_commitments(TopHash, State#state{posted_commitment = true});
        false ->
            lager:debug("Will not post commitments, disabled, is Syncing ~p, is posting commitments ~p, already posted ~p, good height ~p",
                          [IsSyncing, PostingCommitments, State#state.posted_commitment, GoodHeight]),
            State#state{posted_commitment = false}
    end.


post_commitments(TopHash, #state{sign_module = SignModule} = State) ->
    %% use the key top hash as there could be a race condition. If there is a
    %% microblock right after the keyblock, the state trees could have changed
    %% (and could have modified the staker's state) and using
    %% aetx_env:tx_env_and_trees_from_top/1 could be dangerous
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_hash(aetx_transaction, TopHash),
    {ok, AllStakers} = aec_consensus_hc:parent_chain_validators(TxEnv, Trees),
    NetworkId = aec_governance:get_network_id(),
    LocalStakers = lists:filter(fun SignModule:is_key_present/1, AllStakers),
    lists:foreach(
        fun(Staker) ->
            CommitmentBin = aec_parent_chain_block:encode_commitment_btc(Staker, TopHash, NetworkId),
            case aec_parent_connector:post_commitment(Staker, CommitmentBin) of
                ok ->
                    <<THashPart:7/binary, _/binary>> = aec_hash:sha256_hash(TopHash),
                    lager:debug("Posted a commitment [height ~p] for staker ~p with tophash: ~p",
                                [aetx_env:height(TxEnv), Staker, THashPart]),
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
maybe_post_initial_commitments(Block, State) ->
    Height = aec_parent_chain_block:height(Block),
    ChildStartHeight = State#state.child_start_height,
    %% we start one block before the parent chain height that would result in block 1 of the child chain
    IsFirstCommitment = Height =:= ChildStartHeight,
    IsPublishingCommitments = posting_commitments_enabled(State),
    case IsFirstCommitment andalso IsPublishingCommitments of
        true ->
            Hash = aec_chain:genesis_hash(), %% TODO: maybe reconsider if we shall post Genesis hash before seeing any block on the parent chain or a different approach should be taken
            _State1 = maybe_post_commitments(Height, Hash, State);
        false ->
            State
    end.

posting_commitments_enabled(#state{enabled_commitments = Enabled,
                                   producing_blocks = Producing}) ->
    Enabled andalso Producing.

