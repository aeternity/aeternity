%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Hyperchains parent's layer manager
%%% This component is responsible to manage the abstract parent's chain layer through so called "pointers".
%%% Each pointer consists of a synchronized address at the particular parent chain and is supplied with a responsible interface provider.
%%% Please note that:
%%% - The first pointer is representing the current parent chain view and supplies the "master" connector;
%%% - Only the master connector is eligible to dictate the election period through event's announcement;
%%% - The synchronization occurs from the current highest block until pointer's hash value has achived;
%%% - Here is and at the next we mention hash pointer as "parent's chain view genesis address" (please don't confuse with the common "genesis block" terminology);
%%% - The component is responsible to manage fork switching by traversing hash-pointers of the parent blocks. Each time when fork occurs:
%%%     a) The database is updated by the new parent chain representation;
%%%     b) The child chain is reverted and the new election process is started from the last synced pointer height;
%%% - To be able to run the Hyperchains the system must satisfy the connector's acceptance criteria;
%%%     a) For default mode: get_top_block/0, get_block_by_hash/1l
%%%     b) For validator mode: default mode + send_tx/1;
%%% - Each interested developer can supply his own connector's implementation for the particular parent chain;
%%% (TODO: To supply link to the official connector's development guide);
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_mng).

-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([accept_connector/1]).
-export([start_connector/3, terminate_connector/1]).

-export([publish_block/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-include_lib("aeutils/include/aeu_stacktrace.hrl").

-record(pointer, { hash :: binary(), connector :: aehc_connector:connector(), args :: map(), note :: binary() }).

-type pointer() :: #pointer{}.
%% API.

-spec start_link() ->
    {ok, pid()} | ingnore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec publish_block(aehc_connector:connector(), aehc_connector:block()) -> ok.
publish_block(Connector, Block) ->
    erlang:send(?MODULE, {publish_block, Connector, Block}).

%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

%% TODO: Connector check API;
-record(state, { master :: aehc_connector:connector(), hash :: binary(), pointers :: list() }).

init([]) ->
    %% TODO: To extract the current block from Db;
    process_flag(trap_exit, true),
    [H|_] = Pointers = [pointer(P) || P <- pointers_config()],
    [aehc_connector_sup:start_child(connector(P), args(P), note(P)) || P <- Pointers],
    %% TODO: To request the current top block hash;
    {ok, #state{ master = connector(H), pointers = Pointers }}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({publish_block, Connector, Block}, State) ->
    %% TODO: Create parent block;
    %% To send parent parent block;
    %% TO Persists parent block;
    %% To be able to persists the block we have to be sure that it connects to the previous existed one in the DB;
    %% That guaranties sequential order and consistency of the current chain view;
    %% Check that condition each time when block arrived allows to skip the whole chain traversing procedure;
    try
        true = aehc_parent_block:is_hc_parent_block(Block),
        Hash = aehc_parent_block:hash_block(Block),
        Height = aehc_parent_block:height_block(Block),
        Length = length(aehc_parent_block:commitments_in_block(Block)),

        Connector == State#state.master andalso
            aec_events:publish({parent_chain, block}, {block_created, Block}),
        %% TODO: To sync block into the database
        %% TODO: To provide fork awareness trough block value comparison;
        %% TODO: To provide sync checks;
        ok = aehc_parent_db:write_parent_block(Block),
        [H|_] = State#state.pointers, _Sync = hash(H),
        Info = "Block has persisted (connector: ~p, hash: ~p, height: ~p, capacity: ~p)",
        lager:info(Info, [Connector, Hash, Height, Length])
        ?_catch_(error, E, StackTrace)
            lager:error("CRASH: ~p; ~p", [E, StackTrace])
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%%  Connector's management
%%%===================================================================

-spec accept_connector(pointer()) -> boolean().
accept_connector(Pointer) ->
    try
        [Criteria(Pointer) || Criteria <- fun accept_top_block/1, fun accept_block_by_hash/1, fun accept_send_tx/1],
        ok
    catch E:R ->
        {error, {E, R}}
    end.

-spec start_connector(aehc_connector:connector(), map(), binary()) -> {ok, pid()}.
start_connector(_Connector, _Args, _Desc) ->
    throw('not implemented').

-spec terminate_connector(aehc_connector:connector()) -> ok.
terminate_connector(_Connector) ->
    throw('not implemented').

accept_top_block(Pointer) ->
    %% Ability to request the current top block;
    Con = connector(Pointer),
    {ok, _} = aehc_connector:get_top_block(Con).

accept_block_by_hash(Pointer) ->
    %% Ability to request genesis hash pointer;
    Con = connector(Pointer), Hash = hash(Pointer),
    {ok, _} = aehc_connector:get_block_by_hash(Con, Hash).

accept_send_tx(Pointer) ->
    %% Ability to execute commitment call;
    delegate_config() == undefined orelse
        begin
            Con = connector(Pointer),
            Delegate = aec_keys:pubkey(),
            KeyblockHash = aec_chain:top_key_block_hash(),
            Payload = aehc_commitment_header:hash(aehc_commitment_header:new(Delegate, KeyblockHash)),
            ok = aehc_connector:send_tx(Con, Payload)
        end.

%%%===================================================================
%%%  Block's management
%%%===================================================================

%% TODO: sync procedure, fork switch procedure


%%%===================================================================
%%%  Configuration access layer
%%%===================================================================

pointers_config() ->
    aeu_env:user_config([<<"hyperchains">>, <<"pointers">>]).

delegate_config() ->
    aeu_env:user_config([<<"hyperchains">>, <<"delegate">>]).

%%%===================================================================
%%%  Fields accessors
%%%===================================================================

-spec hash(pointer()) -> binary().
hash(P) ->
    P#pointer.hash.

-spec connector(pointer()) -> aehc_connector:connector().
connector(P) ->
    P#pointer.connector.

-spec args(pointer()) -> map().
args(P) ->
    P#pointer.args.

-spec note(pointer()) -> binary().
note(P) ->
    P#pointer.note.

-spec pointer(map()) -> pointer().
pointer(Conf) ->
    #pointer{
        hash = maps:get(<<"hash">>, Conf),
        connector = binary_to_existing_atom(maps:get(<<"connector">>, Conf), utf8),
        args = maps:get(<<"args">>, Conf),
        note = maps:get(<<"note">>, Conf)
    }.


%% NOTE: Manager is responsible to start/stop connectors;
