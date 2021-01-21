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
%% b) migrated (fork switching);
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
-type tx() :: aeconnector_tx:tx().

-type parent_block() :: aehc_parent_block:parent_block().
-type commitment() :: aehc_commitment:commitment().


-spec start(connector(), map(), binary()) -> {ok, pid()} | {error, term()}.
start(Connector, Args, Address) ->
    Data = data(Connector, Args, Address),
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
    %% The block address on which state machine history ends
    indicator :: binary(),
    %% The block height on which state machine history ends
    top :: non_neg_integer(),
    %% The current processed state machine hash
    state :: binary(),
    %% The current processed state machine index
    index :: non_neg_integer(),
    %% The genesis block height on which state machine history begins
    genesis :: non_neg_integer(),
    %% The genesis block address on which state machine history begins
    address :: binary()
}).

-type data() :: #data{}.

init(Data) ->
    {ok, Pid} = connect(Data), _Ref = erlang:monitor(process, Pid),

    ok = init_db(Data),
    Data2 = sync_state(Data),
    {ok, Block} = aeconnector:get_top_block(connector(Data)),
    Data3 = indicate(Data2, Block),

    {ok, fetched, Data3, [{next_event, internal, {added_block, Block}}]}.

callback_mode() ->
    [state_functions, state_enter].

terminate(_Reason, _State, Data) ->
    ok = disconnect(Data).

-spec connect(data()) -> {ok, pid()}.
connect(Data) ->
    Con = connector(Data), Args = args(Data), Callback = fun publish_block/2,
    aeconnector:connect(Con, Args, Callback).

-spec disconnect(data()) -> ok.
disconnect(Data) ->
    Con = connector(Data),
    aeconnector:disconnect(Con).

%%%===================================================================
%%%  State machine callbacks
%%%===================================================================
%% Entering into parent chain fetching log state;
fetched(enter, _OldState, Data) ->
    %% TODO: Place for the sync initiation announcement;
    {keep_state, Data};

%% Processing parent chain fetching log state;
fetched(internal, {added_block, Block}, Data) ->
    Hash = aeconnector_block:hash(Block),

    State = state(Data), Index = index(Data),
    case Index of
        _ when Hash == State ->
            %% NOTE Sync is done in the fetch mode
            {next_state, synced, Data};
        _ when Index > 0  ->
            %% TODO: Place for the new added block anouncement;
            %% NOTE Sync procedure is continue it the fetch mode
            aehc_parent_db:write_parent_block(parent_block(Block)),
            PrevHash = aeconnector_block:prev_hash(Block),
            {ok, PrevBlock} = aeconnector:get_block_by_hash(connector(Data), PrevHash),
            Data2 = locate(Data, PrevBlock),
            {keep_state, Data2, [{next_event, internal, {added_block, PrevBlock}}]};
        _ ->
            %% NOTE Sync is continue on the fork switch mode
            {next_state, migrated, Data, [{next_event, internal, {added_block, Block}}]}
    end;

%% Postponing service requests until fetching is done;
fetched(_, _, Data) ->
    {keep_state, Data, [postpone]}.

%% Parent chain switching state (fork);
migrated(enter, _OldState, Data) ->
    {keep_state, Data};

migrated(internal, {added_block, Block}, Data) ->
    aehc_parent_db:write_parent_block(parent_block(Block)),
    %% TODO: Place for the new added block announcement;
    PrevHash = aeconnector_block:prev_hash(Block), Height = aeconnector_block:height(Block),
    State = state(Data), Genesis = genesis(Data),

    %% TODO This block should be announced on a queue and deleted (maybe)
    DbBlock = aehc_parent_db:get_parent_block(State), PrevDbHash = aehc_parent_block:prev_hash_block(DbBlock),

    case Height of
        _ when PrevHash == PrevDbHash ->
            %% Sync is done in the migrated mode;
            {next_state, synced, Data};
        _ when Height >= Genesis ->
            %% NOTE Sync procedure is continue it the migrated mode
            {ok, PrevBlock} = aeconnector:get_block_by_hash(connector(Data), PrevHash),
            Data2 = locate(Data, PrevBlock),
            {keep_state, Data2, [{next_event, internal, {added_block, PrevBlock}}]};
        _ ->
            %% NOTE: This case is designed with the dynamic nature of HC which relies on the parent blockchains
            %% Genesis hash entry has to be chosen precisely and by the most optimal way (productivity VS security);
            %% If the worst case got happened and fork exceeded pre-configured genesis hash entry the system should be:
            %%  a) Reconfigured by the new (older ones) genesis entry;
            %%  b) Restarted;
            Template = "State machine got exceeded genesis entry (genesis: ~p, height: ~p)",
            Reason = io_lib:format(Template, [Genesis, Height]),
            {stop, Reason}
    end;

%% Postponing service requests until fork solving is done;
migrated(_, _, Data) ->
    {keep_state, Data, [postpone]}.

%% Synchronized state (ready to use);
synced(enter, _OldState, Data) ->
    Top = top(Data), Indicator = indicator(Data),
    %% TODO: Place for the sync finalization anouncement;
    Data2 = index(state(Data, Top), Indicator),
    commit_state(Data2),
    {keep_state, Data2};

synced(cast, {publish_block, Block}, Data) ->
    Data2 = indicate(Data, Block),
    {next_state, fetched, Data2, [{next_event, internal, {added_block, Block}}]}.

-spec init_db(data()) -> data().
init_db(Data) ->
    Address = address(Data),
    State = aehc_parent_db:get_parent_state(Address),
    (State == undefined) orelse
    begin
        {ok, Block} = aeconnector:get_block_by_hash(connector(Data), Address),
        %% TODO To transform into parent block
        aehc_parent_db:write_parent_block(parent_block(Block)),
        Height = aehc_parent_block:height_block(parent_block(Block)),
        State2 = aehc_parent_state:parent_state(Address, _Top = Address, Height),
        aehc_parent_db:write_parent_state(State2)
    end,
    ok.

-spec sync_state(data()) -> data().
sync_state(Data) ->
    Address = address(Data),
    State = aehc_parent_db:get_parent_state(Address),
    Top = aehc_parent_state:top(State),
    state(Data, Top).

indicate(Data, Block) ->
    Hash = aeconnector_block:hash(Block), Height = aeconnector_block:height(Block),
    Top = top(Data),
    index(indicator(top(Data, Height), Hash), Height - Top).

locate(Data, Block) ->
    Hash = aeconnector_block:hash(Block),
    Index = index(Data),
    index(state(Data, Hash), Index - 1).

-spec commit_state(data()) -> ok.
commit_state(Data) ->
    Address = address(Data), Hash = state(Data), Index = index(Data),
    State = aehc_parent_state:parent_state(Address, Hash, Index),
    ok = aehc_parent_db:write_parent_state(State).

%%%===================================================================
%%%  HC protocol upgrade
%%%===================================================================

-spec commitment(tx()) -> commitment().
commitment(Tx) ->
    Delegate = aeconnector_tx:account(Tx),
    KeyblockHash = aeconnector_tx:payload(Tx),
    Header = aehc_commitment_header:new(Delegate, KeyblockHash),
    aehc_commitment:new(Header).

-spec parent_block(block()) -> parent_block().
parent_block(Block) ->
    CList = [commitment(Tx) || Tx <- aeconnector_block:txs(Block)],
    CHList = [aehc_commitment:hash(C) || C <- CList],

    Hash = aeconnector_block:hash(Block),
    PrevHash = aeconnector_block:prev_hash(Block),
    Height = aeconnector_block:height(Block),

    Header = aehc_parent_block:new_header(Hash, PrevHash, Height, CHList),
    aehc_parent_block:new_block(Header, CList).

%%%===================================================================
%%%  Data access layer
%%%===================================================================
-spec data(connector(), map(), binary()) -> data().
data(Connector, Args, Address) ->
    #data{
        connector = Connector,
        args = Args,
        address = Address
    }.

-spec connector(data()) -> aeconnector:connector().
connector(Data) ->
    Data#data.connector.

-spec args(data()) -> map().
args(Data) ->
    Data#data.args.

-spec indicator(data()) -> non_neg_integer().
indicator(Data) ->
    Data#data.indicator.

-spec indicator(data(), non_neg_integer()) -> data().
indicator(Data, Value) ->
    Data#data{ indicator = Value }.

-spec top(data()) -> binary().
top(Data) ->
    Data#data.top.

-spec top(data(), non_neg_integer()) -> data().
top(Data, Top) ->
    Data#data{ top = Top }.

-spec state(data()) -> binary().
state(Data) ->
    Data#data.state.

-spec state(data(), binary()) -> data().
state(Data, State) ->
    Data#data{ state = State }.

-spec index(data()) -> non_neg_integer().
index(Data) ->
    Data#data.index.

-spec index(data(), non_neg_integer()) -> data().
index(Data, Index) ->
    Data#data{ index = Index }.

-spec address(data()) -> non_neg_integer().
genesis(Data) ->
    Data#data.genesis.

-spec address(data()) -> binary().
address(Data) ->
    Data#data.address.

