%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%% The state machine which represents the attached blockchain (parent chain).
%% The main responsibility are:
%% - to manage the state change when fork switching event occurs;
%% - to traverse hash-pointers via connector network interface;
%% - to update database log by the current parent chain state;
%% - to emit appropriate state change events on aehc_parent_mng queue
%% The main operational modes are:
%% a) fetched (adding a new blocks);
%% b) orphaned (fork switching);
%% c) synced (consistent, ready to use mode).

%%% @end
-module(aehc_parent_tracker).

-behaviour(gen_statem).

%% API
-export([start/3]).
-export([send_tx/4]).
-export([stop/1]).

-export([publish_block/2]).

%% gen_statem.
-export([init/1]).
-export([terminate/3]).
-export([callback_mode/0]).

%% state transitions
-export([fetched/3, migrated/3, synced/3]).

-type connector() :: aeconnector:connector().
-type block() :: aeconnector_block:block().

-type aehc_parent_block() :: aehc_parent_block:parent_block().


-spec start(connector(), map(), binary()) -> {ok, pid()} | {error, term()}.
start(Connector, Args, Genesis) ->
    Data = data(Connector, Args, Genesis),
    gen_statem:start(?MODULE, Data, []).

%% reply(From :: from(), Reply :: term())
-spec send_tx(pid(), binary(), binary(), term()) -> ok | {error, term()}.
send_tx(Pid, Delegate, Payload, From) ->
    gen_statem:cast(Pid, {send_tx, Delegate, Payload, From}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

-spec publish_block(connector(), block()) -> ok.
publish_block(_Connector, Block) ->
    gen_statem:cast(?MODULE, {publish_block, Block}).

%%%===================================================================
%%%  State machine callbacks
%%%===================================================================

-record(data, {
    %% The the real world blockchain interface: https://github.com/aeternity/aeconnector/wiki
    connector :: connector(),
    %% Connector configuration which should be passed the module:connect/2
    args :: map(),
    %% The block address on which state machine history begins
    genesis:: binary(),
    %% The current synchronized top block hash
    hash :: binary(),
    %% The current synchronized top block height
    height :: non_neg_integer()
}).

-type data() :: #data{}.

init(Conf) ->
    Data = conf_data(Conf),
    %% Db initialization
    InitData = init_data(Data),
    %% The top block from the connector;
    {ok, Block} = aeconnector:get_top_block(connector(InitData)),
    %% The top block of the current view;
    SynchedBlock = aehc_parent_db:get_parent_block(current_hash(InitData)),
    %% Apply fetched top block as the top of view;
    SyncData = sync_data(InitData, Block),
    {ok, fetched, SyncData, [{next_event, internal, {added_block, Block, SynchedBlock}}]}.

callback_mode() ->
    [state_functions, state_enter].

terminate(_Reason, _State, _Data) ->
    ok.

%%%===================================================================
%%%  State machine callbacks
%%%===================================================================
%% Entering into parent chain fetching log state;
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
            %% TODO: Place for the new added block anouncement;
            %% Sync procedure is continue it the fetch mode (the current persisted block isn't achieved);
            %% To persist the fetched block;
            aehc_parent_db:write_parent_block(Block),
            {ok, PrevBlock} = aeconnector:get_block_by_hash(connector(Data), PrevHash),
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
            {ok, PrevBlock} = aeconnector:get_block_by_hash(connector(Data), PrevHash),
            SynchedPrevHash = aehc_parent_block:prev_hash_block(SynchedBlock),
            PrevSynchedBlock = aehc_parent_db:get_parent_block(SynchedPrevHash),
            {keep_state, Data, [{next_event, internal, {added_block, PrevBlock, PrevSynchedBlock}}]};
        _ ->
            %% NOTE: This case is designed with the dynamic nature of HC which relies on the parent chains
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
synced(enter, _OldState, Data) ->
    %% TODO: Place for the sync finalization anouncement;
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
%%%  Data access layer
%%%===================================================================
%% Initialization of the Db view;
%% Initial Db keeps pinpointed genesis hash address as the top of the parent view + fetched block;
-spec init_data(data()) -> data().
init_data(Data) ->
    GenesisHash = genesis_hash(Data),
    {ok, GenesisBlock} = aeconnector:get_block_by_hash(connector(Data), GenesisHash),
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

-spec id(data()) -> term().
id(Data) ->
    Data#data.id.

-spec id(data(), term()) -> data().
id(Data, Id) ->
    Data#data{ id = Id }.

-spec current_height(data()) -> non_neg_integer().
current_height(Data) ->
    Data#data.current_height.

-spec current_height(data(), non_neg_integer()) -> data().
current_height(Data, Height) ->
    Data#data{ current_height = Height }.

-spec current_hash(data()) -> binary().
current_hash(Data) ->
    Data#data.current_hash.

-spec current_hash(data(), binary()) -> data().
current_hash(Data, Hash) ->
    Data#data{ current_hash = Hash }.

-spec genesis_hash(data()) -> binary().
genesis_hash(Data) ->
    Data#data.genesis_hash.

-spec genesis_height(data()) -> non_neg_integer().
genesis_height(Data) ->
    Data#data.genesis_height.

-spec genesis_height(data(), non_neg_integer()) -> data().
genesis_height(Data, Height) ->
    Data#data{ genesis_height = Height }.

-spec connector(data()) -> aeconnector:connector().
connector(Data) ->
    Data#data.connector.

-spec note(data()) -> binary().
note(Data) ->
    Data#data.note.
