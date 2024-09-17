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
         get_block_by_height/2]).

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
        parent_target_fun                       :: fun((integer()) -> [integer()]),
        child_start_height                      :: non_neg_integer(),
        max_size                                :: non_neg_integer(),
        retry_interval                          :: non_neg_integer(),
        block_cache         = #{}               :: #{non_neg_integer() => aec_parent_chain_block:block() | {requested,  non_neg_integer()}},
        blocks_hash_index   = #{}               :: #{aec_parent_chain_block:hash() => non_neg_integer()},
        top_height          = 0                 :: non_neg_integer(),
        sign_module         = aec_preset_keys   :: atom(), %% TODO: make it configurable
        is_syncing          = false             :: boolean()
    }).
-type state() :: #state{}.



%%%=============================================================================
%%% API
%%%=============================================================================
%% Start the parent chain cache process
-spec start_link(non_neg_integer(), non_neg_integer(),
                 fun((integer()) -> [integer()]),
                 non_neg_integer(), non_neg_integer()) ->
    {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(Height, RetryInterval, ParentTargetFun, Size, _Confirmations) ->
    Args = [Height, RetryInterval, ParentTargetFun, Size],
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

%% TODO: This is really ugly, properly fix at some point
-spec get_block_by_height(non_neg_integer(), integer()) ->
           {ok, aec_parent_chain_block:block()} | {error, not_in_cache}.
get_block_by_height(Height, Timeout) ->
    case get_block_by_height(Height) of
        {ok, _B} = OK -> OK;
        _Err when Timeout > 0 ->
            timer:sleep(10),
            get_block_by_height(Height, Timeout - 10);
        Err -> Err
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
init([StartHeight, RetryInterval, ParentTargetFun, Size]) ->
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
                retry_interval          = RetryInterval,
                parent_target_fun       = ParentTargetFun,
                child_top_hash          = ChildHash,
                max_size                = Size,
                block_cache             = #{}
                }}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({get_block_by_height, Height}, _From, State) ->
    {Reply, State1} =
                    case get_block(Height, State) of
                        {error, _} = Err ->
                            NewState = request_block(Height, State),
                            {Err, NewState};
                        {ok, _Block} = OK ->
                            {OK, State}
                    end,
    {reply, Reply, State1};

handle_call(get_state, _From, State) ->
    Reply = {ok, state_to_map(State)},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = unhandled,
    {reply, Reply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({post_block, Block}, #state{} = State0) ->
    State = post_block(Block, State0),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(initialize_cache, State) ->
    % Fetch the last block, which will be the block required for finality
    [TargetHeight|RestHeights] = lists:reverse(lists:sort(target_parent_heights(State))),
    case catch aec_parent_connector:fetch_block_by_height(TargetHeight) of
        {ok, B} ->
            State1 = maybe_request_blocks(RestHeights, State),
            State2 = post_block(B, State1),
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
                          child_top_hash = Hash},
    State2 =
        case OldHeight + MaxSize < Height of
            true  -> State1#state{block_cache = #{}}; %% we flush the whole state - this should not happen
            false -> State1
        end,
    TargetHeights = target_parent_heights(State2),
    State = maybe_request_blocks(TargetHeights, State2),
    {noreply, State};
handle_info({gproc_ps_event, chain_sync, #{info := {is_syncing, IsSyncing}}}, State0) ->
    State1 = State0#state{is_syncing = IsSyncing},
    {noreply, State1};
handle_info({gproc_ps_event, top_changed, _A} , State) ->
    {noreply, State};
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
        {ok, {requested, _RequestTime}} -> {error, not_in_cache};
        {ok, _Block} = OK               -> OK;
        error                           -> {error, not_in_cache}
    end.

% -spec get_block_height_by_hash(aec_parent_chain_block:hash(), state()) ->
%     {ok, non_neg_integer()} | {error, not_in_cache}.
% get_block_height_by_hash(Hash, #state{blocks_hash_index = Index}) ->
%     case maps:find(Hash, Index) of
%         {ok, _Height} = OK -> OK;
%         error ->
%             {error, not_in_cache}
%     end.

-spec maybe_delete_oldest_block(state()) -> state().
maybe_delete_oldest_block(#state{block_cache = Blocks,
                            max_size = MaxSize} = State) when map_size(Blocks) < MaxSize ->
    State;
maybe_delete_oldest_block(#state{block_cache = Blocks,
                            blocks_hash_index = Index} = State) ->
  [Height|_] = lists:sort(maps:keys(Blocks)),
  case maps:find(Height, Blocks) of
        {ok, {requested, _RequestTime}} ->
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

target_parent_heights(#state{parent_target_fun  = ParentTargetFun,
                            child_top_height    = ChildHeight}) ->
    ParentTargetFun(ChildHeight).

max_cachable_parent_height(State) ->
    lists:max(target_parent_heights(State)).

post_block(Block, #state{top_height = TopHeight} = State0) ->
    BlockHeight = aec_parent_chain_block:height(Block),
    MaxBlockToRequest = max_cachable_parent_height(State0),
    State1 =
        case BlockHeight =< MaxBlockToRequest of
            true ->
                insert_block(Block, State0);
            false ->
                State0
        end,
    State2 = maybe_delete_oldest_block(State1),
    State3 = maybe_request_blocks(State2),
    State3#state{top_height = max(TopHeight, BlockHeight)}.

request_block(Height, #state{block_cache = Blocks, retry_interval=RetryInterval} = State) ->
    Now = erlang:system_time(millisecond),
    case maps:get(Height, Blocks, undefined) of
        undefined ->
            lager:debug("Missing block with height ~p detected, requesting it", [Height]),
            aec_parent_connector:request_block_by_height(Height),
            State#state{block_cache = Blocks#{Height => {requested, Now}}};
        {requested, Timestamp} when Now > (Timestamp + RetryInterval) ->
            lager:debug("Missing block with height ~p detected, rerequesting it", [Height]),
            aec_parent_connector:request_block_by_height(Height),
            State#state{block_cache = Blocks#{Height => {requested, Now}}};
        _ ->
            State
    end.


maybe_request_blocks(#state{block_cache = Blocks} = State) ->
    maybe_request_blocks(maps:keys(Blocks), State).

maybe_request_blocks(BlockHeights, State) ->
    lists:foldl(fun(BlockHeight, NewState) -> maybe_request_block(BlockHeight, NewState) end, State, BlockHeights).

maybe_request_block(BlockHeight, State) ->
    case get_block(BlockHeight, State) of
        {ok, _} ->
            State;
        {error, not_in_cache} ->
            request_block(BlockHeight, State)
    end.


