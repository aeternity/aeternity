%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%% The state machine keeps synchronized state at the particular branch of the parent chain;
%% The state machine manages fetch and fork switching procedures by traversing hash-pointers of the parent blocks;
%% The state machine fetches missing blocks via appropriate interface provider (connector);
%% The state machine produces side effect with updated DB log with the actual parent chain view;
%% The state machine produces appropriate state change events for the conductor;
%% The state machine operates in 3 modes:
%% a) fetched (adding a new blocks);
%% b) orphaned (fork switching);
%% c) synced (consistent, ready to use mode).
%%% @end
-module(aehc_parent_tracker).

-behaviour(gen_statem).

%% API
-export([start_link/2
        , publish_block/2
        , fetched/3
        , orphaned/3
        , synced/3]).

%% gen_statem callbacks
-export([init/1, terminate/3, callback_mode/0]).

-define(SERVER(Name), {via, gproc, {n, l, {?MODULE, Name}}}).

-include_lib("aehyperchains/include/aehc_types.hrl").

-type height() :: non_neg_integer().

%% The data record represents the current synchronized view of a particular parent chain within state machine;
-record(data, {
    %% The name of dedicated parent chain state machine;
    name :: binary(),
    %% The genesis hash from the config;
    genesis_hash :: hash(),
    %% The genesis height;
    genesis_height :: undefined | height(),
    %% Responsible connector module;
    connector :: aehc_connector:connector(),
    %% The current synchronized top block hash from the Db;
    current_hash :: hash(),
    %% The current synchronized top block height from the Db;
    current_height :: height(),
    %% Textual description of a parent chain view;
    note :: binary()
}).
-type data() :: #data{}.

-type fetch_action() :: enter | internal.
-type state_action() :: keep_state | next_state.
-type sync_action() :: enter | cast.
-type synchronicity() :: synced | orphaned.


%% API

-spec start_link(term(), map()) -> {ok, pid()} | ignore | {error, term()}.
start_link(View, Conf) ->
    gen_statem:start_link(?SERVER(View), ?MODULE, Conf, []).

-spec publish_block(binary(), aehc_parent_block:parent_block()) -> ok.
publish_block(View, Block) ->
    gen_server:cast(?SERVER(View), {publish_block, Block}).

%% Entering into parent chain fetching log state;
-spec fetched(fetch_action(), tuple(), data()) ->
    {state_action(), data()} |
    {state_action(), synchronicity(), data()} |
    {keep_state, data(), [postpone]}.
fetched(enter, _OldState, Data) ->
    %% TODO: Place for the sync initiation announcement;
    {keep_state, Data};
%% Processing parent chain fetching log state;
fetched(internal, {added_block, Block, SynchedBlock}, Data) ->
    Hash = aehc_parent_block:hash_block(Block),
    PrevHash = aehc_parent_block:prev_hash_block(Block),
    Height = aehc_parent_block:height_block(Block),

    SynchedHash = aehc_parent_block:hash_block(SynchedBlock),
    SynchedHeight = aehc_parent_block:height_block(SynchedBlock),

    case Hash of
        SynchedHash ->
            %% Sync is done in the fetch mode;
            %% This case is also applicable for the initial DB state too (For the initial run Current == Genesis);
            {next_state, synced, Data};
        _ when Height > SynchedHeight ->
            %% TODO: Place for the new added block announcement;
            %% Sync procedure is continue it the fetch mode (the current persisted block isn't achieved);
            %% To persist the fetched block;
            aehc_parent_db:write_parent_block(Block),
            {ok, PrevBlock} = aehc_connector:get_block_by_hash(connector(Data), PrevHash),
            {keep_state, Data, [{next_event, internal, {added_block, PrevBlock, SynchedBlock}}]};
        _ ->
            %% Sync procedure is continue on the fork switch mode
            %% (we passed the synced height but the matched condition isn't satisfied);
            {next_state, orphaned, Data, [{next_event, internal, {added_block, Block, SynchedBlock}}]}
    end;
%% Postponing service requests until fetching is done;
fetched(_, _, Data) ->
    {keep_state, Data, [postpone]}.

%% Parent chain switching state (fork);
-spec orphaned(fetch_action(), tuple(), data()) -> {state_action(), data()}.
orphaned(enter, _OldState, Data) ->
    {keep_state, Data};
orphaned(internal, {added_block, Block, SynchedBlock}, Data) ->
    Hash = aehc_parent_block:hash_block(Block),
    PrevHash = aehc_parent_block:prev_hash_block(Block),
    Height = aehc_parent_block:height_block(Block),

    SynchedHash = aehc_parent_block:hash_block(SynchedBlock),
    GenesisHeight = genesis_height(Data),

    case Hash of
        SynchedHash ->
            %% Sync is done in the fork mode;
            {next_state, synced, Data};
        _ when Height > GenesisHeight ->
            %% TODO: Place for the new added block announcement;
            %% TODO: Place for abandoned block announcement;
            %% Sync procedure is continue it the fetch mode (the current persisted block isn't achived);
            aehc_parent_db:write_parent_block(Block),
            {ok, PrevBlock} = aehc_connector:get_block_by_hash(connector(Data), PrevHash),
            SynchedPrevHash = aehc_parent_block:prev_hash_block(SynchedBlock),
            PrevSynchedBlock = aehc_parent_db:get_parent_block(SynchedPrevHash),
            {keep_state, Data, [{next_event, internal, {added_block, PrevBlock, PrevSynchedBlock}}]};
        _ ->
            %% NOTE: This case has designed by taking into account the dynamic nature of HC which relies on parent chains;
            %% Genesis hash entry has to be chosen precisely and by the most optimal way (productivity VS security);
            %% If the worst case got happened and fork exceeded pre-configured genesis hash entry the system should be:
            %%  a) Reconfigured by the new (older ones) genesis entry;
            %%  b) Restarted;
            aehc_parent_db:write_parent_block(Block),
            Info = "Parent chain state machine got exceeded genesis entry (genesis: ~p, synched: ~p, height: ~p, note: ~p)",
            GenesisHash = genesis_hash(Data),
            Note = note(Data),
            lager:info(Info, [GenesisHash, SynchedHash, Height, Note]),
            {next_state, synced, Data}
    end;
%% Postponing service requests until fork solving is done;
orphaned(_, _, Data) ->
    {keep_state, Data, [postpone]}.

%% Synchronized state (ready to use);
-spec synced(sync_action(), tuple(), data()) -> {state_action(), data()}.
synced(enter, _OldState, Data) ->
    %% TODO: Place for the sync finalization announcement;
    SynchedHash = current_hash(Data),
    CurrentHeight = current_height(Data),
    Note = note(Data),
    Info = "Parent chain state machine has synched (synched: ~p, height: ~p, note: ~p)",
    lager:info(Info, [SynchedHash, CurrentHeight, Note]),
    {keep_state, Data};
synced(cast, {publish_block, Block}, Data) ->
    SynchedBlock = aehc_parent_db:get_parent_block(current_hash(Data)),
    {next_state, fetched, Data, [{next_event, internal, {added_block, Block, SynchedBlock}}]}.

%%%===================================================================
%%%  State machine callbacks
%%%===================================================================

init(Conf) ->
    Data = conf_data(Conf),
    %% Execute acceptance procedure;
    ok = aehc_connector:accept(Conf),
    %% Db initialization
    InitData = init_data(Data),
    %% The top block from the connector;
    {ok, Block} = aehc_connector:get_top_block(connector(InitData)),
    %% The top block of the current view;
    SynchedBlock = aehc_parent_db:get_parent_block(current_hash(InitData)),
    %% Apply fetched top block as the top of view;
    SyncData = sync_data(InitData, Block),
    {ok, fetched, SyncData, [{next_event, internal, {added_block, Block, SynchedBlock}}]}.

callback_mode() -> [state_functions, state_enter].

terminate(_Reason, _State, _Data) -> ok.

%%%===================================================================
%%%  Data access layer
%%%===================================================================

-spec current_height(data()) -> height().
current_height(Data) ->
    Data#data.current_height.

-spec current_height(data(), height()) -> data().
current_height(Data, Height) ->
    Data#data{current_height = Height}.

-spec current_hash(data()) -> hash().
current_hash(Data) ->
    Data#data.current_hash.

-spec current_hash(data(), hash()) -> data().
current_hash(Data, Hash) ->
    Data#data{current_hash = Hash}.

-spec genesis_hash(data()) -> hash().
genesis_hash(Data) ->
    Data#data.genesis_hash.

-spec genesis_height(data()) -> height().
genesis_height(Data) ->
    Data#data.genesis_height.

-spec genesis_height(data(), height()) -> data().
genesis_height(Data, Height) ->
    Data#data{genesis_height = Height}.

-spec connector(data()) -> aehc_connector:connector().
connector(Data) ->
    Data#data.connector.

-spec note(data()) -> binary().
note(Data) ->
    Data#data.note.

%%%===================================================================
%%%  Data constructor
%%%===================================================================

%% Initialization of the view within Db;
%% Initial Db keeps pinpointed genesis hash address as the top of the parent view + fetched block;
-spec init_data(data()) -> data().
init_data(Data) ->
    GenesisHash = genesis_hash(Data),
    {ok, GenesisBlock} = aehc_connector:get_block_by_hash(connector(Data), GenesisHash),
    GenesisHeight = aehc_parent_block:height_block(GenesisBlock),
    Res = aehc_parent_db:get_parent_chain_view(GenesisHash),
    TopBlockHash =
        case Res of
            undefined ->
                %% Initialize parent view by genesis -> genesis if the db log is empty;
                aehc_parent_db:write_parent_chain_view(GenesisHash, GenesisHash),
                aehc_parent_db:write_parent_block(GenesisBlock),
                GenesisHash;
            _ when is_binary(Res) ->
                Res
        end,
    CurrentBlock = aehc_parent_db:get_parent_top_block(TopBlockHash),
    sync_data(genesis_height(Data, GenesisHeight), CurrentBlock).

-spec sync_data(data(), aehc_parent_block:parent_block()) -> data().
sync_data(Data, Block) ->
    Hash = aehc_parent_block:hash_block(Block),
    Height = aehc_parent_block:height_block(Block),
    current_height(current_hash(Data, Hash), Height).

%%%===================================================================
%%%  Configuration access layer
%%%===================================================================

-spec conf_data(map()) -> data().
conf_data(Conf) ->
    GenesisHash = maps:get(<<"genesis_hash">>, Conf),
    Module = aehc_connector:module(Conf),
    #data{
        name = maps:get(<<"name">>, Conf),
        current_height = 0,
        current_hash = GenesisHash,
        genesis_hash = GenesisHash,
        connector = Module,
        note = maps:get(<<"note">>, Conf)
    }.
