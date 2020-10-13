%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% @end
-module(aehc_parent_tracker).

-behaviour(gen_statem).

%% API
-export([start_link/2]).
-export([get_commitment/2, get_candidates/3, publish_block/2]).
-export([init/1]).
-export([fetched/3, orphaned/3, synced/3]).
-export([terminate/3]).

-define(SERVER(Name), {via, gproc, {n, l, {?MODULE, Name}}}).

%% The data record represents the current synchronized view of a particular parent chain within state machine;
-record(data, {
    %% The name of a synchronized view;
    name :: binary(),
    %% The genesis hash from the config;
    genesis_hash :: binary(),
    %% Responsible connector module;
    connector :: aehc_connector:connector(),
    %% The current syncronized top block hash from the Db;
    current_hash :: binary(),
    %% The previous syncronized block hash from the Db;
    previous_hash :: undefined | binary(),
    %% The current syncronized top block height from the Db;
    current_height :: non_neg_integer(),
    %% Textual description of a tracker;
    note :: binary()
}).

-type data() :: #data{}.

start_link(View, Conf) ->
    gen_statem:start_link(?SERVER(View), ?MODULE, Conf, []).

get_commitment(View, Hash) ->
    gen_statem:call(?SERVER(View), {get_commitment, Hash}).

get_candidates(View, Height, Hash) ->
    gen_statem:call(?SERVER(View), {get_candidates, Height, Hash}).

-spec publish_block(binary(), aehc_parent_block:parent_block()) -> ok.
publish_block(View, Block) ->
    gen_statem:cast(?SERVER(View), {publish_block, Block}).

%%%===================================================================
%%%  State machine callbacks
%%%===================================================================
%% The state machine is responsible for managing fetch and fork switching procedures;
%% The state machine produces side effect with updated DB log by the actual parent chain view;
%% The state machine can produce appropriate events for the conductor (gorbak25, radrow your opinions?);
%% The state machine operates in 3 modes:
%% a) fetched (adding a new blocks);
%% b) orphaned (fork switching);
%% c) synced (consistent, ready to use mode).
init(Conf) ->
    Data = conf_data(Conf),
    %% Execute acceptance procedure;
    ok = aehc_connector:accept(Conf),
    %% Db initialization
    InitData = init_db(Data),
    {ok, fetched, InitData}.

callback_mode() ->
    [state_functions, state_enter].

%% Parent chain fetching log state;
fetched(enter, _OldState, Data) ->

    {keep_state, Data, [{next_event, internal, {added_block, Block}}]};

fetched(internal, {added_block, Block}, Data) ->
    Connector = connector(Track),
    GenesisHash = genesis_hash(Track),
    Height = current_height(Track),
    Info = "Block has fetched (name: ~p, genesis hash: ~p, current hash: ~p current height: ~p)",
    lager:info(Info, [Connector, GenesisHash, Height]),
    {keep_state, Data};

fetched(_, _, Data) ->
    {keep_state, Data, [postpone]}.

%% Parent chain switching log state (fork);
orphaned(enter, OldState, Data) ->
    {keep_state, Data, [{next_event, internal, {added_block, Block}}]};

orphaned(internal, {added_block, Block}, Data) ->
    {keep_state, Data};

orphaned(_, _, Data) ->
    {keep_state, Data, [postpone]}.

%% Synchronized state (ready to use);
synced(enter, _OldState, Data) ->
    {keep_state, NewData};

synced({call,From}, {get_commitment, Hash}, Data) ->
    {keep_state, NewData};

synced({call,From}, {get_candidates, Height, Hash}, Data) ->
    {keep_state, NewData};

synced(info, {publish_block, Block}, Data) ->
    %% Log event
    Hash = aehc_parent_block:hash_block(Block),
    Height = aehc_parent_block:height_block(Block),
    Length = length(aehc_parent_block:commitments_in_block(Block)),
    Info = "Block has accepted (view: ~p, hash: ~p, height: ~p, capacity: ~p)",
    lager:info(Info, [name(Data), Hash, Height, Length]),
    {next_state, fetched, Data, [{next_event, internal, {added_block, Block}}]}.

terminate(_Reason, State, _Data) ->
    ok.

%%%===================================================================
%%%  Jobs scheduling
%%%===================================================================
%%% The main responsibility of tracker's job:
%%%  a) To ensure synchronized state at the particular branch of the parent chain;
%%%  b) To fetch missing blocks via appropriate interface provider (connector);
%%%  c) To manage fork switching by traversing hash-pointers of the parent blocks.
%%% Each time a fork occurs:
%%%  a) The database is updated by the new parent chain representation;
%%%  b) The child chain is reverted and the new election process is started from the last synced track height;
-spec sync_track(track()) -> pid().
sync_track(Track) ->
    Pid = self(),
    set_tracker(Track, Pid),
    {ok, Block} = aehc_connector:get_top_block(connector(Track)),
    Res = sync_track(Track, Block, [], []),
    reset_tracker(Res),
    Pid.

%% Sync of track by event;
-spec sync_track(track(), aehc_parent_block:parent_block()) -> pid().
sync_track(Track, Block) ->
    Pid = self(),
    set_tracker(Track, Pid),
    Res = sync_track(Track, Block, [], []),
    reset_tracker(Res),
    Pid.

-spec sync_track(data(), aehc_parent_block:parent_block(), list(), list()) -> data().
sync_track(Track, Block, Fetched, Reverted) ->
    %% To persist the current block;
    ok = aehc_parent_db:write_parent_block(Block),

    Hash = aehc_parent_block:hash_block(Block),
    PrevHash = aehc_parent_block:prev_hash_block(Block),
    Height = aehc_parent_block:height_block(Block),

    CurrentHash = current_hash(Track),
    CurrentPrevHash = previous_hash(Track),
    CurrentHeight = current_height(Track),
    GenesisHash = genesis_hash(Track),

    case Hash of
        CurrentHash when Reverted == [] ->
            %% Sync is done in the fetch mode (Reverted == [] means that fork switch isn't happened);
            %% This case is also applicable for the initial DB state too (For the initial run Current == Genesis);
            ok;
        CurrentHash ->
            %% Sync is done in the fork switch mode until CurrentHash point (Reverted blocks var contains the list);
            %% Place where Reverted blocks could be published;
            ok;
        GenesisHash ->
            %% Sync is done in the fork switch mode until GenesisHash (The all chain view has reverted);
            %% Let boys count loss;
            ok;
        _ when Height > CurrentHeight ->
            %% Sync procedure is continue it the fetch mode (the current persisted block isn't achived);
            {ok, PrevBlock} = aehc_connector:get_block_by_hash(connector(Track), PrevHash),
            PrevSynchedBlock = aehc_parent_db:get_parent_block(CurrentPrevHash),
            sync_track(init_track(Track, PrevSynchedBlock), PrevBlock, [Block|Fetched], Reverted);
        _ ->
            %% Sync procedure is continue it the fork switch mode (we passed the current height but the matched block hasn't achived);
            {ok, PrevBlock} = aehc_connector:get_block_by_hash(connector(Track), PrevHash),
            PrevSynchedBlock = aehc_parent_db:get_parent_block(CurrentPrevHash),
            sync_track(init_track(Track, PrevSynchedBlock), PrevBlock, [Block|Fetched], [CurrentHash|Reverted])
    end,
    _SynchedTrack = init_track(Track, Block).

%%%===================================================================
%%%  Database initialization
%%%===================================================================
%% Initialization of view within Db;
%% Initial Db keeps pinpointed genesis hash address as the top of the parent view + fetched block;
-spec init_db(data()) -> data().
init_db(Data) ->
    GenesisHash = genesis_hash(Data),
    Res = aehc_parent_db:get_parent_chain_view(GenesisHash),
    TopBlockHash =
        case Res of
            undefined ->
                %% Initialize parent view by genesis -> genesis if the db log is empty;
                aehc_parent_db:write_parent_chain_view(GenesisHash, GenesisHash),
                {ok, Block} = aehc_connector:get_block_by_hash(connector(Data), GenesisHash),
                ok = aehc_parent_db:write_parent_block(Block),
                GenesisHash;
            _ when is_binary(Res) ->
                Res
        end,
    CurrentBlock = aehc_parent_db:get_parent_top_block(TopBlockHash),
    sync_data(Data, CurrentBlock).

%%%===================================================================
%%%  Data access layer
%%%===================================================================
-spec name(data()) -> term().
name(Data) ->
    Data#data.name.

-spec current_height(data()) -> non_neg_integer().
current_height(Data) ->
    Data#data.current_height.

-spec current_height(data(), non_neg_integer()) -> data().
current_height(Data, Height) ->
    Data#data{current_height = Height}.

-spec current_hash(data()) -> binary().
current_hash(Data) ->
    Data#data.current_hash.

-spec current_hash(data(), binary()) -> data().
current_hash(Data, Hash) ->
    Data#data{current_hash = Hash}.

-spec previous_hash(data()) -> binary().
previous_hash(Data) ->
    Data#data.previous_hash.

-spec previous_hash(data(), binary()) -> data().
previous_hash(Data, Hash) ->
    Data#data{previous_hash = Hash}.

-spec genesis_hash(data()) -> binary().
genesis_hash(Data) ->
    Data#data.genesis_hash.

-spec connector(data()) -> aehc_connector:connector().
connector(Data) ->
    Data#data.connector.

-spec note(data()) -> binary().
note(Data) ->
    Data#data.note.

%%%===================================================================
%%%  Data constructor
%%%===================================================================
-spec sync_data(data(), aehc_parent_block:parent_block()) -> data().
sync_data(Data, Block) ->
    Hash = aehc_parent_block:hash_block(Block),
    PrevHash = aehc_parent_block:prev_hash_block(Block),
    Height = aehc_parent_block:height_block(Block),
    previous_hash(current_height(current_hash(Data, Hash), Height), PrevHash).

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
