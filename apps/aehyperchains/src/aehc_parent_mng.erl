%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% <h3>Hyperchains parent's process manager</h3>
%%% See the pattern "ProcessManager" (https://www.enterpriseintegrationpatterns.com/patterns/messaging/ProcessManager.html).
%%% This component is responsible for orchestration the abstract parent's chain layer through:
%%%  a) dedicated workers (trackers);
%%%  b) supplied interface providers (conenctors);
%%% - The first supplied connector acts as the "master" and is eligible to dictate the election period through event's announcement;
%%% - A tracker performs synchronization from the highest known block until genesis pointer through "previous_hash" property;
%%% - To be able to run the Hyperchains the system must satisfy the connector's acceptance criteria;
%%%     a) default mode: get_top_block/0, get_block_by_hash/1l
%%%     b) delegate mode: default mode + send_tx/1;
%%% - Each interested developer can supply their own connector's implementation over the particular parent chain;
%%% (TODO: To supply link to the official connector's development guide);
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_mng).

-behaviour(gen_server).

%% API.
-export([start_link/0]).

-export([publish_block/2]).
-export([publish_track/1]).
-export([sync_track/1, sync_track/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-include_lib("aeutils/include/aeu_stacktrace.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% The track record represents the current synchronized view of a particular parent chain within manager;
-record(track, {
        %% Responsible connector module;
        connector :: aehc_connector:connector(),
        %% The current syncronized top block hash from the Db;
        current_hash::binary(),
        %% The previous syncronized block hash from the Db;
        previous_hash:: undefined | binary(),
        %% The current syncronized top block height from the Db;
        current_height::non_neg_integer(),
        %% The genesis hash from the config;
        genesis_hash :: binary(),
        %% The current executable;
        tracker :: undefined | pid(),
        %% Passed arguments;
        args :: map(),
        %% Textual description;
        note :: binary()
    }).

-type track() :: #track{}.
%% API.

-spec start_link() ->
    {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% This event issued each time by connector when the new block is generated;
-spec publish_block(aehc_connector:connector(), aehc_parent_block:parent_block()) -> ok.
publish_block(Connector, Block) ->
    erlang:send(?MODULE, {publish_block, Connector, Block}),
    ok.

-spec publish_track(track()) -> ok.
publish_track(Track) ->
    erlang:send(?MODULE, {publish_track, Track}),
    ok.
%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

-record(state, { master :: aehc_connector:connector() }).

init([]) ->
    init_registry(),
    %% Read configuration;
    {ok, Config} = tracks_config(),
    [Master|_] = Tracks = [conf_track(Track) || Track <- Config],
    %% Db initialization
    InitTracks = [init_db_track(Track) || Track <- Tracks],
    %% Run conenctor's instances;
    [start_connector(Track) || Track <- InitTracks],
    %% Execute acceptance procedure;
    [accept_connector(Track) || Track <- InitTracks],
    %% Forks synchronization by fetched blocks;
    [spawn(?MODULE, sync_track, [Track]) || Track <- InitTracks],
    {ok, #state{ master = connector(Master) }}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({publish_track, Track}, State) ->
    Connector = connector(Track),
    GenesisHash = genesis_hash(Track),
    Height = current_height(Track),
    Info = "Track entry has synchronized (connector: ~p, genesis hash: ~p, current hash: ~p current height: ~p)",
    lager:info(Info, [Connector, GenesisHash, Height]),
    {noreply, State};

handle_info({publish_block, Connector, Block}, #state{} = State) ->
    %% To be able to persists the block we have to be sure that it connects to the previous existed one in the DB;
    %% That guaranties sequential order and consistency of the current chain view;
    %% Check that condition each time when block arrived allows to skip the whole chain traversing procedure;
    try
        true = aehc_parent_block:is_hc_parent_block(Block),
        %% %% Fork synchronization by by the new arrived block;
        [Track] = lookup_registry(Connector),
        spawn(?MODULE, sync_track, [Track, Block]),
        %% Log event
        Hash = aehc_parent_block:hash_block(Block),
        Height = aehc_parent_block:height_block(Block),
        Length = length(aehc_parent_block:commitments_in_block(Block)),
        Info = "Block has accepted (connector: ~p, hash: ~p, height: ~p, capacity: ~p)",
        lager:info(Info, [Connector, Hash, Height, Length])
    catch E:R:StackTrace ->
        lager:error("CRASH: ~p; ~p", [E, StackTrace]),
        {error, E, R}
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    [terminate_connector(Track) || Track <- select_registry()],
    ok.

%%%===================================================================
%%%  Registry access
%%%===================================================================

init_registry() ->
    ets:new(?MODULE, [named_table, public]).

update_registry(Track) ->
    ets:insert(?MODULE, Track).

lookup_registry(Key) ->
    ets:lookup(?MODULE, Key).

select_registry() ->
    ets:tab2list(?MODULE).

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

set_tracker(Track, Pid) ->
    update_registry(tracker(Track, Pid)).

reset_tracker(Track) ->
    update_registry(tracker(Track, undefined)),
    publish_track(Track).

%%%===================================================================
%%%  Connector's management
%%%===================================================================
-spec accept_connector(track()) -> ok | {error, {term(), term()}}.
accept_connector(Track) ->
    Criteria = [fun accept_top_block/1, fun accept_block_by_hash/1, fun accept_send_tx/1],
    try
        [Fun(Track) || Fun <- Criteria],
        ok
    catch E:R ->
        {error, {E, R}}
    end.

-spec start_connector(track()) -> {ok, pid()}.
start_connector(Track) ->
    aehc_connector_sup:start_child(connector(Track), args(Track), note(Track)).

-spec terminate_connector(track()) -> ok.
terminate_connector(Track) ->
    aehc_connector_sup:terminate_child(connector(Track)).

accept_top_block(Track) ->
    %% Ability to request the current top block;
    Con = connector(Track),
    {ok, Block} = aehc_connector:get_top_block(Con),
    Hash = aehc_parent_block:hash_block(Block),
    Height = aehc_parent_block:height_block(Block),
    Info = "Accept get_top_block procedure has passed (connector: ~p, hash: ~p, height: ~p)",
    lager:info(Info, [Con, Hash, Height]).

accept_block_by_hash(Track) ->
    %% Ability to request genesis hash track;
    Con = connector(Track),
    Genesis = genesis_hash(Track),
    {ok, Block} = aehc_connector:get_block_by_hash(Con, Genesis),
    Hash = aehc_parent_block:hash_block(Block),
    Height = aehc_parent_block:height_block(Block),
    Info = "Accept get_block_by_hash procedure has passed (connector: ~p, hash: ~p, height: ~p)",
    lager:info(Info, [Con, Hash, Height]).

accept_send_tx(Track) ->
    %% Ability to execute commitment call;
    delegate_config() == undefined orelse
        begin
            Con = connector(Track),
            {ok, Delegate} = aec_keys:pubkey(),
            KeyblockHash = aec_chain:top_key_block_hash(),
            PoGF = aehc_pogf:hash(no_pogf),
            aehc_connector:send_tx(Con, Delegate, KeyblockHash, PoGF),
            Info = "Accept send_tx procedure has passed (connector: ~p, delegate: ~p, hash: ~p, pogf: ~p)",
            lager:info(Info, [Con, Delegate, KeyblockHash, PoGF])
        end.

%%%===================================================================
%%%  Tracks management
%%%===================================================================
%% Initialization of view within Db;
%% Initial Db keeps pinpointed genesis hash address as the top of the parent view + fetched block;
-spec init_db_track(track()) -> track().
init_db_track(Track) ->
    GenesisHash = genesis_hash(Track),
    Res = aehc_parent_db:get_parent_chain_view(GenesisHash),
    TopBlockHash =
        case Res of
            undefined ->
                %% Initialize parent view by genesis -> genesis if the db log is empty;
                aehc_parent_db:write_parent_chain_view(GenesisHash, GenesisHash),
                {ok, Block} = aehc_connector:get_block_by_hash(connector(Track), GenesisHash),
                ok = aehc_parent_db:write_parent_block(Block),
                GenesisHash;
            _ when is_binary(Res) ->
                Res
        end,
    CurrentBlock = aehc_parent_db:get_parent_top_block(TopBlockHash),
    init_track(Track, CurrentBlock).


%% This function is responsible for managing fetch and fork switching procedures;
%% This function produces side effect with updated DB by the actual parent chain view;
%% This function can produce appropriate events for the conductor (gorbak25, radrow your opinions?);
%% The result is an updated track record with the latest top block entries;
-spec sync_track(track(), aehc_parent_block:parent_block(), list(), list()) -> track().
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

 -spec init_track(track(), aehc_parent_block:parent_block()) -> track().
init_track(Track, Block) ->
    Hash = aehc_parent_block:hash_block(Block),
    PrevHash = aehc_parent_block:prev_hash_block(Block),
    Height = aehc_parent_block:height_block(Block),
    previous_hash(current_height(current_hash(Track, Hash), Height), PrevHash).

-spec conf_track(map()) -> track().
conf_track(Conf) ->
    GenesisHash = maps:get(<<"genesis_hash">>, Conf),
    Connector = binary_to_existing_atom(maps:get(<<"connector">>, Conf), utf8),
    #track{
        current_height = 0,
        current_hash = GenesisHash,
        genesis_hash = GenesisHash,
        connector = Connector,
        args = maps:get(<<"args">>, Conf),
        note = maps:get(<<"note">>, Conf)
    }.


%%%===================================================================
%%%  Configuration access layer
%%%===================================================================
-spec tracks_config() -> {ok, nonempty_list(map())}.
tracks_config() ->
    Res = {ok, _} = aeu_env:user_config([<<"hyperchains">>, <<"tracks">>]),
    Res.

-spec delegate_config() -> undefined | {ok, map()}.
delegate_config() ->
    aeu_env:user_config([<<"hyperchains">>, <<"delegate">>]).

%%%===================================================================
%%%  Fields accessors
%%%===================================================================
-spec current_height(track()) -> non_neg_integer().
current_height(T) ->
    T#track.current_height.

-spec current_height(track(), non_neg_integer()) -> track().
current_height(T, Height) ->
    T#track{current_height = Height}.

-spec current_hash(track()) -> binary().
current_hash(T) ->
    T#track.current_hash.

-spec current_hash(track(), binary()) -> track().
current_hash(T, Hash) ->
    T#track{current_hash = Hash}.

-spec previous_hash(track()) -> binary().
previous_hash(T) ->
    T#track.previous_hash.

-spec previous_hash(track(), binary()) -> track().
previous_hash(T, Hash) ->
    T#track{previous_hash = Hash}.

-spec genesis_hash(track()) -> binary().
genesis_hash(T) ->
    T#track.genesis_hash.

-spec connector(track()) -> aehc_connector:connector().
connector(T) ->
    T#track.connector.

-spec tracker(track(), undefined | pid()) -> track().
tracker(T, Tracker) ->
    T#track{tracker = Tracker}.

-spec args(track()) -> map().
args(T) ->
    T#track.args.

-spec note(track()) -> binary().
note(T) ->
    T#track.note.
