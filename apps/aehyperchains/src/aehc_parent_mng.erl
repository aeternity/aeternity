%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Parent chains process manager
%%% See the pattern "ProcessManager" (https://www.enterpriseintegrationpatterns.com/patterns/messaging/ProcessManager.html).
%%% This component is responsible for orchestration of the Hyperchains backends (parent chains) through:
%%% - dedicated state machines (trackers);
%%% - blockchain interfaces (https://github.com/aeternity/aeconnector/wiki);
%%% TODO: To provide HTTP API for scheduled commitments
%%% a) Setup #1 (monolith)
%%% b) Setup #2 (replica)
%%% c) Setup #3 (shard: history keeper/election master)
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_mng).

-behaviour(gen_statem).

%% API.
-export([start_link/0]).

%% gen_statem.
-export([init/1]).
-export([terminate/3]).
-export([callback_mode/0]).

%% transitions
-export([monolith/3, replica/3, shard/3]).

-export([publish_block/1]).
-export([log_block/1]).

-type conenctor() :: aeconnector:conenctor().
-type parent_block() :: aehc_parent_block:parent_block().

%% API.

-spec start_link() ->
    {ok, pid()} | ignore | {error, term()}.
start_link() ->
    Opt = [user_config, schema_default],
    {ok, Backend} = aeu_env:find_config([<<"hyperchains">>, <<"setup">>], Opt),
    Data = data(Backend),
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Data, []).

-spec publish_block(parent_block()) -> ok.
publish_block(Block) ->
    erlang:send(?MODULE, {publish_block, Block}),
    ok.

-spec log_block(parent_block()) -> ok.
log_block(Block) ->
    Hash = aehc_parent_block:hash_block(Block),
    Height = aehc_parent_block:height_block(Block),
    lager:debug("~nA new replica block ~p has produced at height: ~p~n",[Hash, Height]),
    ok.

%%%===================================================================
%%%  gen_statem behaviour
%%%===================================================================
-record(tracker, {
    pid::pid(),
    ref::reference(),
    %% The the real world blockchain interface: https://github.com/aeternity/aeconnector/wiki
    module::connector(),
    %% Connector configuration which should be passed the module:connect/2
    args::map(),
    %% Commitment capacity. The blocks count accumulated into one commitment transaction
    capacity::integer(),
    %% The block address on which the state machine history begins
    hash::binary()
}).

-type tracker() :: #tracker{}.

-record(data, {
    %% Backend state (monolith, replica, shard)
    state::atom(),
    %% Effective height.
    %% The point where primary blockchain reaches the level of maturity and replicas should be detached
    height::integer() | infinity,
    %% The primary (election) state machine
    primary::tracker(),
    %% Dedicated replica state machines
    replicas :: [] | [tracker()],
    %% Queued announcements
    events::term()
}).

-type data() :: #data{}.

init(Data) ->
    process_flag(trap_exit, true),
    State = state(Data),
    Primary = primary(Data),
    {ok, Pid} = start_tracker(Primary, fun publish_block/1),
    Ref = erlang:monitor(process, Pid),
    Data2 = primary(Data, Primary#tracker{ pid = Pid, ref = Ref }),

    {ok, State, Data2}.

callback_mode() ->
    [state_functions, state_enter].

terminate(_Reason, _State, Data) ->
    Replicas = replicas(Data),
    [ok = stop_tracker(Tracker) || Tracker <- Replicas],
    Primary = primary(Data),
    ok = stop_tracker(Primary).

%%%===================================================================
%%%  State machine callbacks
%%%===================================================================
monolith(enter, _OldState, Data) ->
    {keep_state, Data, []};

monolith(_Event, _Req, Data) ->
    %% TODO
    {keep_state, Data, [postpone]}.

replica(enter, _OldState, Data) ->
    %% NOTE Replica setup assumes the primary backend and arbitrary replica list
    Replicas =
        lists:foldl(
            fun (Tracker, Acc) ->
                {ok, Pid} = start_tracker(Tracker, fun log_block/1),
                Ref = erlang:monitor(process, Pid),
                [Tracker#tracker{ pid = Pid, ref = Ref }|Acc]
            end,
            [],
            replicas(Data)
        ),
    Data2 = replicas(Data, Replicas),
    {keep_state, Data2, []};

replica(_Event, _Req, Data) ->
    %% TODO
    {keep_state, Data, [postpone]}.

shard(enter, _OldState, Data) ->
    %% NOTE Shard setup assumes the two backends: election master and history keeper
    [Tracker] = replicas(Data),
    {ok, Pid} = start_tracker(Tracker, fun log_block/1),
    Ref = erlang:monitor(process, Pid),
    Data2 = replicas(Data, [Tracker#tracker{ pid = Pid, ref = Ref }]),
    {keep_state, Data2, []};

shard(_Event, _Req, Data) ->
    %% TODO
    {keep_state, Data, [postpone]}.

-spec start_tracker(tracker(), function()) -> {ok, pid()}.
start_tracker(Tracker, Callback) ->
    Hash = Tracker#tracker.hash, Args = Tracker#tracker.args,
    aehc_parent_tracker:start(Hash, Args, Callback).

-spec stop_tracker(tracker()) -> ok.
stop_tracker(Tracker) ->
    Pid = Tracker#tracker.pid,
    aehc_parent_tracker:stop(Pid).

%%handle_info({publish_block, Con, Block}, #state{} = State) ->
%% To be able to persists the block we have to be sure that it connects to the previous existing one in the DB;
%% That guaranties sequential order and consistency of the current chain view;
%% Check that condition each time when block is arrived allows us to skip the whole chain traversing procedure;
%%    try
%%        true = aehc_parent_block:is_hc_parent_block(Block),
%% Fork synchronization by by the new arrived block;
%%        aehc_parent_tracker:publish_block(View, Block)
%%    catch E:R:StackTrace ->
%%        lager:error("CRASH: ~p; ~p", [E, StackTrace]),
%%        {error, E, R}
%%    end,
%%    {noreply, State};

%%%===================================================================
%%%  Parent chain simplified proto
%%%===================================================================
%% This API should be used by connector's developer as a wrapper around internal abstract commitments data type;
-spec commitment(Delegate::binary(), KeyblockHash::binary()) ->
    commitment().
commitment(Delegate, KeyblockHash) when
    is_binary(Delegate), is_binary(KeyblockHash) ->
    aehc_commitment:new(aehc_commitment_header:new(Delegate, KeyblockHash)).

%% TODO: Opened for discussion with gorbak25 and radrow;
%%-spec commitment(Delegate::binary(), KeyblockHash::binary(), PoGFHash::binary()) ->
%%    commitment().
%%commitment(Delegate, KeyblockHash, PoGFHash) when
%%    is_binary(Delegate), is_binary(KeyblockHash), is_binary(PoGFHash) ->
%%    aehc_commitment:new(aehc_commitment_header:new(Delegate, KeyblockHash, PoGFHash)).

-spec parent_block(Height::non_neg_integer(), Hash::binary(), PrevHash::binary(), Commitments::[commitment()]) ->
    parent_block().
parent_block(Height, Hash, PrevHash, Commitments) when
    is_integer(Height), is_binary(Hash), is_binary(PrevHash), is_list(Commitments) ->
    Hashes = [aehc_commitment:hash(Commitment) || Commitment <- Commitments],
    Header = aehc_parent_block:new_header(Hash, PrevHash, Height, Hashes),
    aehc_parent_block:new_block(Header, Commitments).


%% NOTE: Safely call to existing atom.
%% In the case of non loaded modules related issues should be addressed;

%%%===================================================================
%%%  Data access layer
%%%===================================================================
-spec tracker(map()) -> tracker().
tracker(Tracker) ->
    Module = maps:get(<<"module">>, Tracker),
    Args = maps:get(<<"args">>, Tracker),
    Hash = maps:get(<<"hash">>, Tracker),
    #tracker{
        module = binary_to_atom(Module, unicode),
        args = Args,
        hash = Hash
    }.

-spec data(map()) -> data().
data(Setup) ->
    State = maps:get(<<"state">>, Setup),
    Primary = maps:get(<<"primary">>, Setup),
    Replicas = maps:get(<<"replicas">>, Setup, []),
    Height = maps:get(<<"height">>, Setup, infinity),
    #data{
        state = binary_to_atom(State, unicode),
        primary = tracker(Primary),
        height = Height,
        replicas = [tracker(R)||R <- Replicas],
        events = queue:new()
    }.

-spec state(data()) -> atom().
state(Data) ->
    Data#data.state.

-spec primary(data()) -> [tracker()].
primary(Data) ->
    Data#data.primary.

-spec primary(data(), tracker()) -> data().
primary(Data, Tracker) ->
    Data#data{ primary = Tracker }.

-spec replicas(data()) -> [tracker()].
replicas(Data) ->
    Data#data.replicas.

-spec replicas(data(), [tracker()]) -> data().
replicas(Data, Trackers) ->
    Data#data{ replicas = Trackers }.
