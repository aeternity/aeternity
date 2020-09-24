%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Hyperchains parent's layer manager
%%% This component is responsible to manage the abstract parent's chain layer through so called "tracks".
%%% Each track consists of a synchronized address at the particular parent chain and is supplied with a responsible interface provider.
%%% Please note that:
%%% - The first track supplies the "master" connector and eligible to dictate the election period through event's announcement;
%%% - The synchronization occurs from the current highest block until track's genesis_hash value has achived;
%%% - Here is and the next time we mention term "genesis hash" (please don't confuse with the common "genesis block" terminology);
%%% - The component is responsible to manage fork switching by traversing hash-pointers of the parent blocks. Each time when fork occurs:
%%%     a) The database is updated by the new parent chain representation;
%%%     b) The child chain is reverted and the new election process is started from the last synced track height;
%%% - To be able to run the Hyperchains the system must satisfy the connector's acceptance criteria;
%%%     a) For default mode: get_top_block/0, get_block_by_hash/1l
%%%     b) For delegate mode: default mode + send_tx/1;
%%% - Each interested developer can supply his own connector's implementation for the particular parent chain;
%%% (TODO: To supply link to the official connector's development guide);
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_mng).

-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([accept_connector/1]).

-export([publish_block/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-include_lib("aeutils/include/aeu_stacktrace.hrl").

%% The track record represents the current synchronized view of a particular parent chain within manager;
-record(track, {
        %% The current syncronized top block hash from the Db;
        current_hash::binary(),
        %% The previous syncronized top block hash from the Db;
        previous_hash::binary(),
        %% The current syncronized top block height from the Db;
        current_height::non_neg_integer(),
        %% The genesis hash from the config;
        genesis_hash :: binary(),
        %% Responsible connector module;
        connector :: aehc_connector:connector(),
        %% Passed args;
        args :: map(),
        %% Textual description;
        note :: binary()
    }).

-type track() :: #track{}.
%% API.

-spec start_link() ->
    {ok, pid()} | ingnore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% This event issued each time by connector when the new block is generated;
-spec publish_block(aehc_connector:connector(), aehc_parent_block:parent_block()) -> ok.
publish_block(Connector, Block) ->
    erlang:send(?MODULE, {publish_block, Connector, Block}).

%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

%% TODO: Connector check API;
-record(state, { master :: aehc_connector:connector(), tracks :: list() }).

init([]) ->
    %% TODO: To extract the current block from Db;
    process_flag(trap_exit, true),
    %% Read configuration;;
    Tracks = [conf_track(C) || C <- tracks_config()],
    %% Db initialization
    InitTracks = [init_db_track(Track) || Track <- Tracks],
    %% Execute acceptance procedure;
    [accept_connector(T) || T <- InitTracks],
    %% Run conenctor's instances;
    [start_connector(T) || T <- InitTracks],
    %% Forks synchronization by fetched blocks;
    SynchedTracks = [sync_track(T) || T <- InitTracks],
    %% TODO: To request the current top block hash;
    {ok, #state{ master = connector(hd(SynchedTracks)), tracks = SynchedTracks }}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({publish_block, Connector, Block}, #state{} = State) ->
    %% To be able to persists the block we have to be sure that it connects to the previous existed one in the DB;
    %% That guaranties sequential order and consistency of the current chain view;
    %% Check that condition each time when block arrived allows to skip the whole chain traversing procedure;
    try
        true = aehc_parent_block:is_hc_parent_block(Block),
        %% %% Fork synchronization by by the new arrived block;
        Track = get_track_by_connector(State, Connector),
        SynchedTrack = sync_track(Track, Block),
        %% Log event
        Hash = aehc_parent_block:hash_block(Block),
        Height = aehc_parent_block:height_block(Block),
        Length = length(aehc_parent_block:commitments_in_block(Block)),
        Info = "Block has accepted (connector: ~p, hash: ~p, height: ~p, capacity: ~p)",
        lager:info(Info, [Connector, Hash, Height, Length]),

        {noreply, set_track(State, SynchedTrack)}
        ?_catch_(error, E, StackTrace)
            lager:error("CRASH: ~p; ~p", [E, StackTrace]),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{tracks = Tracks}) ->
    [terminate_connector(Track) || Track <- Tracks],
    ok.

%%%===================================================================
%%%  State management
%%%===================================================================

get_track_by_connector(#state{tracks = Tracks}, Connector) ->
    lists:keyfind(Connector, #track.connector, Tracks).

set_track(#state{tracks = Tracks} = State, Track) ->
    Connector = connector(Track),
    Update = lists:keyreplace(Connector, #track.connector, Tracks, Track),
    State#state{tracks = Update}.

%%%===================================================================
%%%  Connector's management
%%%===================================================================

-spec accept_connector(track()) -> boolean().
accept_connector(Track) ->
    try
        [Criteria(Track) || Criteria <- fun accept_top_block/1, fun accept_block_by_hash/1, fun accept_send_tx/1],
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
    {ok, _} = aehc_connector:get_top_block(Con).

accept_block_by_hash(Track) ->
    %% Ability to request genesis hash track;
    Con = connector(Track), Genesis = genesis_hash(Track),
    {ok, _} = aehc_connector:get_block_by_hash(Con, Genesis).

accept_send_tx(Track) ->
    %% Ability to execute commitment call;
    delegate_config() == undefined orelse
        begin
            Con = connector(Track),
            Delegate = aec_keys:pubkey(),
            KeyblockHash = aec_chain:top_key_block_hash(),
            Payload = aehc_commitment_header:hash(aehc_commitment_header:new(Delegate, KeyblockHash)),
            ok = aehc_connector:send_tx(Con, Payload)
        end.

%%%===================================================================
%%%  Tracks management
%%%===================================================================

%% Initialization of view within Db (initial empty Db keep Genesis hash address as the top of parent view);
-spec init_db_track(track()) -> track().
init_db_track(Track) ->
    GenesisHash = genesis_hash(Track),
    Res = aehc_parent_db:get_parent_chain_track(GenesisHash),
    TopBlockHash =
        case Res of
            undefined ->
                aehc_parent_db:write_parent_chain_track(GenesisHash, GenesisHash),
                GenesisHash;
            _ when is_binary(Res) ->
                Res
        end,
    Block = aehc_parent_db:get_parent_top_block(TopBlockHash),
    init_track(Track, Block).

%% Sync of track by fetching the current top;
-spec sync_track(track()) -> track().
sync_track(Track) ->
    Connector = connector(Track),
    Block = aehc_connector:get_top_block(Connector),
    sync_track(Track, Block).

%% Sync of track by event;
-spec sync_track(track(), aehc_parent_block:parent_block()) -> track().
sync_track(Track, Block) ->
    sync_track(Block, Track, [], []) .

%% This function is responsible to manage fetch and fork switching procedures;
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
            PrevBlock = aehc_connector:get_block_by_hash(connector(Track), PrevHash),
            PrevSynchedBlock = aehc_parent_db:get_parent_block(CurrentPrevHash),
            sync_track(init_track(Track, PrevSynchedBlock), PrevBlock, [Block|Fetched], Reverted);
        _ ->
            %% Sync procedure is continue it the fork switch mode (the current persisted block isn't achived);
            PrevBlock = aehc_connector:get_block_by_hash(connector(Track), PrevHash),
            PrevSynchedBlock = aehc_parent_db:get_parent_block(CurrentPrevHash),
            %% There is a place where erase DB can be done (do we need to delete reverted blocks?)
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

tracks_config() ->
    aeu_env:user_config([<<"hyperchains">>, <<"tracks">>]).

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

-spec args(track()) -> map().
args(T) ->
    T#track.args.

-spec note(track()) -> binary().
note(T) ->
    T#track.note.
