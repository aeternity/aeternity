%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%% The state machine which represents the attached blockchain (parent chain).
%% The main responsibilities are:
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

-export([commit/4]).
-export([read/3]).

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

-spec commit(pid(), binary(), binary(), term()) -> ok | {error, term()}.
commit(Pid, Delegate, Payload, From) ->
    gen_statem:cast(Pid, {commit, Delegate, Payload, From}).

-spec read(pid(), non_neg_integer(), term()) -> [] | [parent_block()].
read(Pid, Height, From) ->
    gen_statem:cast(Pid, {read, Height, From}).

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
    %% The real world blockchain interface: https://github.com/aeternity/aeconnector/wiki
    connector :: connector(),
    %% Connector configuration which should be passed to module:connect/2
    args :: map(),
    %% The block address on which state machine history ends
    indicator :: binary(),
    %% The block height on which state machine history ends
    top :: non_neg_integer(),
    %% The current processed state machine address
    location :: binary(),
    %% The current processed state machine index
    index :: non_neg_integer(),
    %% LIFO stack of fetched blocks
    stack = [] :: [term()],
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
    {ok, Hash} = aeconnector:get_top_block(connector(Data)),
    {ok, Block} = aeconnector:get_block_by_hash(connector(Data), Hash),
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

    Loc = location(Data), Index = index(Data),
    case Index of
        _ when Hash == Loc ->
            %% NOTE Sync is done in the fetch mode
            {next_state, synced, Data};
        _ when Index > 0  ->
            %% TODO: Place for the new added block anouncement;
            %% NOTE Sync procedure is continue it the fetch mode
            ParentBlock = parent_block(Block),
            aehc_parent_db:write_parent_block(ParentBlock), Data2 = push(Data, ParentBlock),
            PrevHash = aeconnector_block:prev_hash(Block),
            {ok, PrevBlock} = aeconnector:get_block_by_hash(connector(Data), PrevHash),
            {keep_state, locate(Data2, PrevBlock), [{next_event, internal, {added_block, PrevBlock}}]};
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
    ParentBlock = parent_block(Block),
    aehc_parent_db:write_parent_block(ParentBlock), Data2 = push(Data, ParentBlock),
    %% TODO: Place for the new added block announcement;
    PrevHash = aeconnector_block:prev_hash(Block), Height = aeconnector_block:height(Block),
    Loc = location(Data2), Genesis = genesis(Data2),

    %% TODO This block should be announced on a queue and deleted (maybe)
    DbBlock = aehc_parent_db:get_parent_block(Loc), PrevDbHash = aehc_parent_block:prev_hash_block(DbBlock),

    case Height of
        _ when PrevHash == PrevDbHash ->
            %% Sync is done in the migrated mode;
            {next_state, synced, Data};
        _ when Height >= Genesis ->
            %% NOTE Sync procedure is continue it the migrated mode
            {ok, PrevBlock} = aeconnector:get_block_by_hash(connector(Data), PrevHash),
            {keep_state, locate(Data2, PrevBlock), [{next_event, internal, {added_block, PrevBlock}}]};
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
    Data2 = index(location(Data, Indicator), 0),
    ok = commit_state(Data2),
    From = self(), Stack = stack(Data2),
    ok = aehc_parent_mng:emit(From, Indicator, Top, Stack),
    {keep_state, stack(Data2, [])};

synced(cast, {commit, Delegate, Payload, From}, Data) ->
    Res = aeconnector:send_tx(connector(Data), Delegate, Payload),
    gen_statem:reply(From, Res),
    {keep_state, Data};

synced(cast, {read, Height, From}, Data) ->
    Loc = location(Data),
    Res = aehc_parent_db:get_parent_blocks(Height, Loc),
    gen_statem:reply(From, Res),
    {keep_state, Data};

synced(cast, {publish_block, Block}, Data) ->
    Data2 = indicate(Data, Block),
    {next_state, fetched, Data2, [{next_event, internal, {added_block, Block}}]}.

-spec init_db(data()) -> data().
init_db(Data) ->
    Address = address(Data),
    State = aehc_parent_db:get_parent_state(Address),
    (State == undefined) andalso
    begin
        {ok, Block} = aeconnector:get_block_by_hash(connector(Data), Address),
        %% TODO To transform into parent block
        GenesisBlock = aehc_parent_block:to_genesis(parent_block(Block)),
        aehc_parent_db:write_parent_block(GenesisBlock),
        Height = aehc_parent_block:height_block(GenesisBlock),
        State2 = aehc_parent_state:parent_state(Address, _Top = Address, Height),
        aehc_parent_db:write_parent_state(State2)
    end,
    ok.

-spec sync_state(data()) -> data().
sync_state(Data) ->
    Address = address(Data),
    State = aehc_parent_db:get_parent_state(Address),
    Top = aehc_parent_state:top(State), Height = aehc_parent_state:height(State),
    top(location(Data, Top), Height).

indicate(Data, Block) ->
    Hash = aeconnector_block:hash(Block), Height = aeconnector_block:height(Block),
    Top = top(Data),
    index(indicator(top(Data, Height), Hash), Height - Top).

locate(Data, Block) ->
    _Hash = aeconnector_block:hash(Block),
    Index = index(Data),
    index(Data, Index - 1).

-spec commit_state(data()) -> ok.
commit_state(Data) ->
    Address = address(Data), Loc = location(Data), Index = index(Data),
    State = aehc_parent_state:parent_state(Address, Loc, Index),
    ok = aehc_parent_db:write_parent_state(State).

%%%===================================================================
%%%  HC protocol upgrade
%%%===================================================================

-spec commitment(tx()) -> commitment().
commitment(Tx) ->
    Delegate = aeconnector_tx:account(Tx),
    _KeyblockHash = aeconnector_tx:payload(Tx),
    Header = aehc_commitment_header:new(Delegate, <<>>),
    aehc_commitment:new(Header).

-spec parent_block(block()) -> parent_block().
parent_block(Block) ->
%%    CList = [commitment(Tx) || Tx <- aeconnector_block:txs(Block)],
%%    _CHList = [aehc_commitment:hash(C) || C <- CList],

    Hash = aeconnector_block:hash(Block),
    PrevHash = aeconnector_block:prev_hash(Block),
    Height = aeconnector_block:height(Block),

    Header = aehc_parent_block:new_header(Hash, PrevHash, Height, []),
    aehc_parent_block:new_block(Header, []).

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

-spec indicator(data()) -> binary().
indicator(Data) ->
    Data#data.indicator.

-spec indicator(data(), binary()) -> data().
indicator(Data, Hash) ->
    Data#data{ indicator = Hash }.

-spec top(data()) -> non_neg_integer().
top(Data) ->
    Data#data.top.

-spec top(data(), non_neg_integer()) -> data().
top(Data, Top) ->
    Data#data{ top = Top }.

-spec location(data()) -> binary().
location(Data) ->
    Data#data.location.

-spec location(data(), binary()) -> data().
location(Data, Loc) ->
    Data#data{ location = Loc }.

-spec stack(data()) -> [term()].
stack(Data) ->
    Data#data.stack.

-spec stack(data(), [term()]) -> data().
stack(Data, Stack) ->
    Data#data{ stack = Stack }.

-spec push(data(), parent_block()) -> data().
push(Data, Block) ->
    Stack = stack(Data),
    stack(Data, [Block|Stack]).

-spec index(data()) -> non_neg_integer().
index(Data) ->
    Data#data.index.

-spec index(data(), non_neg_integer()) -> data().
index(Data, Index) ->
    Data#data{ index = Index }.

-spec genesis(data()) -> non_neg_integer().
genesis(Data) ->
    Data#data.genesis.

-spec address(data()) -> binary().
address(Data) ->
    Data#data.address.

