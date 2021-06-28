%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%% The state machine which represents the attached blockchain (parent chain).
%% The main responsibilities are:
%% - to manage the state change when fork switching event occurs;
%% - to traverse hash cursor via connector network interface;
%% - to update database log by the current parent chain state;
%% - to emit appropriate state change events on aehc_parent_mng queue

%% The main operational states are:
%% a) fetched (adding a new blocks);
%% b) migrated (fork switching)

%% Used patterns:
%% - https://martinfowler.com/eaaCatalog/dataMapper.html
%% - https://martinfowler.com/eaaCatalog/unitOfWork.html
%% - https://www.enterpriseintegrationpatterns.com/patterns/messaging/PollingConsumer.html
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_tracker).

-behaviour(gen_statem).

%% API
-export([start/1]).

-export([send_tx/3]).
-export([get_block_by_hash/3]).

-export([subscribe/2]).

-export([stop/1]).

-export([publish/2]).

%% gen_statem.
-export([init/1]).
-export([terminate/3]).
-export([callback_mode/0]).

%% state transitions
-export([synced/3, subscribed/3, migrated/3]).

-type connector() :: aeconnector:connector().
-type args() :: map().

-type block() :: aeconnector_block:block().
-type tx() :: aeconnector_tx:tx().

-type parent_block() :: aehc_parent_block:parent_block().
-type commitment() :: aehc_commitment:commitment().

-type trees() :: aehc_parent_trees:trees().

-spec start(map()) -> {ok, pid()} | {error, term()}.
start(Setup) ->
    Data = data(Setup),
    gen_statem:start(?MODULE, Data, []).

-spec send_tx(pid(), commitment(), term()) -> ok | {error, term()}.
send_tx(Pid, Payload, From) ->
    gen_statem:cast(Pid, {send_tx, Payload, From}).

-spec get_block_by_hash(pid(), binary(), term()) -> {ok, parent_block()} | {error, term()}.
get_block_by_hash(Pid, Hash, From) ->
    gen_statem:cast(Pid, {get_block_by_hash, Hash, From}).

-spec subscribe(pid(), term()) -> ok | {error, term()}.
subscribe(Pid, From) ->
    gen_statem:cast(Pid, {activate, From}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

-spec publish(pid(), block()) -> ok.
publish(Pid, Block) ->
    gen_statem:cast(Pid, {publish, Block}).

%%%===================================================================
%%%  State machine callbacks
%%%===================================================================

-record(data, {
    %% The real world blockchain interface: https://github.com/aeternity/aeconnector/wiki
    connector :: connector(),
    %% Connector configuration which should be passed to connector:connect/2
    args :: map(),
    %% The current processed state machine height
    height :: non_neg_integer(),
    %% The current processed state machine hash
    hash :: binary(),
    state :: trees()
}).

-type data() :: #data{}.

init(Data) ->
    {ok, Pid} = connect(Data), _Ref = erlang:monitor(process, Pid),

    ok = aeconnector:dry_send_tx(connector(Data), <<"test">>),
    {ok, activated, Data, []}.

callback_mode() ->
    [state_functions, state_enter].

terminate(_Reason, _State, Data) ->
    ok = disconnect(Data).

-spec connect(data()) -> {ok, pid()}.
connect(Data) ->
    Con = connector(Data), Args = args(Data),
    Pid = self(),
    Callback = fun (_, Block) -> publish(Pid, Block) end,
    aeconnector:connect(Con, Args, Callback).

-spec disconnect(data()) -> ok.
disconnect(Data) ->
    Con = connector(Data),
    aeconnector:disconnect(Con).

%%%===================================================================
%%%  State machine callbacks
%%%===================================================================
synced(enter, _OldState, Data) ->
    %% TODO: Place for the sync initiation announcement;
    {keep_state, Data};

synced(cast, {get_block_by_hash, Hash, From}, Data) ->
    Trees = state(Data),
    Data2 = case
                aehc_parent_db:find_parent_block(Hash) of
                none ->
                    {ok, Block} = aeconnector:get_block_by_hash(connector(Data), Hash),

                    Hash = aeconnector_block:hash(Block),
                    Height = aeconnector_block:height(Block),

                    ok = process(Block, Trees),

                    ParentBlock = aehc_parent_db:get_parent_block(Hash),
                    Trees2 = aehc_parent_db:get_parent_block_state(Hash),

                    gen_statem:reply(From, {ok, ParentBlock, Trees2}),

                    height(hash(Data, Hash), Height);
                {value, ParentBlock} ->
                    gen_statem:reply(From, {ok, ParentBlock, Trees}),
                    Data
            end,
    {keep_state, Data2};

synced(cast, {subscribe, From}, Data) ->
    {ok, Hash} = aeconnector:get_top_block(connector(Data)), Data2 = hash(Data, Hash),
    lager:info("~nParent chain got subscribed on top with hash ~p~n",[Hash]),
    gen_statem:reply(From, ok),

    {next_state, activated, Data2, []}.

subscribed(enter, _OldState, Data) ->
    %% TODO: Place for the sync initiation announcement;
    {keep_state, Data};

subscribed(cast, {send_tx, Payload, From}, Data) ->
    Res = aeconnector:send_tx(connector(Data), Payload),
    gen_statem:reply(From, Res),

    {keep_state, Data};

subscribed(cast, {publish, Block}, Data) ->
    Hash = aeconnector_block:hash(Block), PrevHash = aeconnector_block:prev_hash(Block),
    Height = aeconnector_block:height(Block),

    PrevHash = hash(Data),

    Trees = state(Data),
    ok = process(Block, Trees), ok = aehc_parent_mng:push(Hash),

    Data2 = height(hash(Data, Hash), Height),
    {keep_state, Data2}.

%% Parent chain switching state (fork);
migrated(enter, _OldState, Data) ->
    {keep_state, Data};

%% Postponing service requests until fork solving is done;
migrated(_, _, Data) ->
    {keep_state, Data, [postpone]}.


%%%===================================================================
%%%  HC protocol
%%%===================================================================

-spec commitment(tx()) -> commitment().
commitment(Tx) ->
    Account = aeconnector_tx:account(Tx), %% TODO Place to substitute delegate via trees;
    Payload = aeconnector_tx:payload(Tx),
    {key_block_hash, KeyblockHash} = aeser_api_encoder:decode(Payload),

    Header = aehc_commitment_header:new(Account, KeyblockHash),
    aehc_commitment:new(Header).

-spec is_commitment(tx()) -> boolean().
is_commitment(Tx) ->
    Payload = aeconnector_tx:payload(Tx),
    aehc_parent_data:is_commitment(Payload).

-spec is_delegate(tx()) -> boolean().
is_delegate(Tx) ->
    Payload = aeconnector_tx:payload(Tx),
    aehc_parent_data:is_delegate(Payload).

-spec process_delegate(tx(), term()) -> term().
process_delegate(Tx, Tree) ->
    PubKey = aeconnector_tx:account(Tx),
    Delegate = aeconnector_tx:payload(Tx),

    aehc_delegates_trees:enter(PubKey, Delegate, Tree).

-spec process(block(), trees()) -> ok.
process(Block, Data) ->
    Hash = aeconnector_block:hash(Block), Height = aeconnector_block:height(Block),
    Txs = aeconnector_block:txs(Block),

    CList = [commitment(Tx)|| Tx <- Txs, is_commitment(Tx)],

    PrevHash = aeconnector_block:prev_hash(Block),
    Height = aeconnector_block:height(Block),

    CHList = [aehc_commitment:hash(C) || C <- CList],
    Header = aehc_parent_block:new_header(Hash, PrevHash, Height, CHList),

    ParentBlock = aehc_parent_block:new_block(Header, CList),

    DTxs = [Tx|| Tx <- Txs, is_delegate(Tx)],

    State = state(Data),
    Tree = aehc_parent_trees:delegates(State),

    Tree2 = lists:foldl(fun process_delegate/2, Tree, DTxs),

    State2 = aehc_parent_trees:set_delegates(State, Tree2),
    aehc_parent_db:write_parent_block(ParentBlock, State2),

    Data2 = height(hash(state(Data, Tree2), Hash), Height),
    {ParentBlock, Tree2, Data2}.


%% TODO add write into DB operation
%% TODO To introduce pull or prefetch hash
%% TODO Processing should be performed in lazy evaluation mode top block -> new fetched block
%% TODO Only new received blocks should be announced into the parent_mng queue (not processing blocks)
%% TODO Processing state transition should be triggered in synced(cast, {process_block, Hash, From}, Data)
%% TODO If HC private network announcement is needed - let Alex coordinate with me

%%%===================================================================
%%%  Data access
%%%===================================================================

-spec data(map()) -> data().
data(Tracker) ->
    Connector = maps:get(<<"connector">>, Tracker, aeconnector_btc_full_node),
    Args = maps:get(<<"args">>, Tracker, #{}),

    #data{
        connector = Connector,
        args = Args
    }.

-spec connector(data()) -> aeconnector:connector().
connector(Data) ->
    Data#data.connector.

-spec args(data()) -> map().
args(Data) ->
    Data#data.args.

-spec hash(data()) -> binary().
hash(Data) ->
    Data#data.hash.

-spec hash(data(), binary()) -> data().
hash(Data, Hash) ->
    Data#data{ hash = Hash }.

-spec height(data()) -> non_neg_integer().
height(Data) ->
    Data#data.height.

-spec height(data(), non_neg_integer()) -> data().
height(Data, Height) ->
    Data#data{ height = Height }.

-spec state(data()) -> trees().
state(Data) ->
    Data#data.state.

-spec state(data(), trees()) -> data().
state(Data, Trees) ->
    Data#data{ state = Trees }.




