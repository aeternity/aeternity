%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%% Parent chains process manager
%% This component is responsible for orchestration of the Hyperchains backends (parent chains) through:
%% - dedicated state machines (trackers);
%% - blockchain interfaces (https://github.com/aeternity/aeconnector/wiki);
%% The component traverses parent chain data in lazy evaluation mode (on demand)
%% TODO: To provide HTTP API for scheduled commitments
%% TODO: To show dialog about registry record (show notify + address in telegram, throw error on commitment)
%% a) Setup #1 (monolith)
%% b) Setup #2 (replica)
%% c) Setup #3 (shard: history keeper/election master)
%% Used patterns:
%% - https://www.enterpriseintegrationpatterns.com/patterns/messaging/ProcessManager.html)
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_mng).

-include("../../aecore/include/blocks.hrl").
-include("aehc_utils.hrl").

-behaviour(gen_statem).

%% API.
-export([start_link/0]).

-export([commit/1]).
-export([commitments/1]).
-export([delegates/1]).
-export([pop/0]).

-export([register/1]).
%%-export([setup/0]).

-export([stop/0]).

-export([announce/2]).

%% gen_statem.
-export([init/1]).
-export([terminate/3]).
-export([callback_mode/0]).

%% transitions
-export([monolith/3, replica/3, shard/3]).

-type connector() :: aeconnector:connector().

-type commitment() ::  aehc_commitment:commitment().
-type parent_block() :: aehc_parent_block:parent_block().

-type pubkey() :: aec_keys:pubkey().

%% API.

-spec start_link() ->
    {ok, pid()} | ignore | {error, term()}.
start_link() ->
    Opt = [user_config, schema_default],
    {ok, Backend} = aeu_env:find_config([<<"hyperchains">>, <<"setup">>], Opt),
    Data = data(Backend),
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Data, []).

-spec commit(commitment()) -> ok.
commit(Commitment) ->
    gen_statem:call(?MODULE, {commit, Commitment}).

%% TODO To support method "sync to to"
-spec commitments(binary()) -> [commitment()].
commitments(Hash) ->
    ParentBlock = gen_statem:call(?MODULE, {process, Hash}),

    aehc_parent_block:commitments_in_block(ParentBlock).

-spec delegates(binary()) -> [{binary(), pubkey()}].
delegates(Hash) ->
    _ = gen_statem:call(?MODULE, {process, Hash}),

    Trees = aehc_parent_db:get_parent_block_state(Hash),
    aehc_parent_trees:delegates(Trees).


%% NOTE The starter app should check the empty stack condition at the initialization stage
%% To extract the block from a queue (FIFO)
-spec pop() -> {value, parent_block()} | empty.
pop() ->
    gen_statem:call(?MODULE, {pop}).

%% The announcement of a new parent top
-spec announce(pid(), binary()) -> ok.
announce(From, Top) ->
    gen_statem:cast(?MODULE, {announce, From, Top}).

-spec register(binary()) -> ok.
register(PubKey) ->
    gen_statem:call(?MODULE, {register, PubKey}).

-spec stop() -> ok.
stop() ->
    gen_statem:stop(?MODULE).

%%%===================================================================
%%%  gen_statem behaviour
%%%===================================================================
%% Should be replaced by parent_tracker state
-record(tracker, {
    pid :: term(),
    ref :: term(),
    %% The the real world blockchain interface: https://github.com/aeternity/aeconnector/wiki
    module :: connector(),
    %% Connector configuration which should be passed the module:connect/2
    args :: map(),
    %% Commitment capacity. The blocks count accumulated into one commitment transaction
    capacity :: undefined | integer(),
    %% The pointer (block hash) on which the state machine history begins
    pointer :: binary()
}).

-type tracker() :: #tracker{}.

-record(data, {
    %% Backend mode (monolith, replica, shard)
    mode::atom(),
    %% Effective height.
    %% The point where primary blockchain reaches the level of maturity and replicas can be detached
    height::integer() | infinity,
    %% The primary (election) state machine
    primary::tracker(),
    %% Dedicated replica state machines
    replicas = [] :: [tracker()],
    %% FIFO queue of accumulated commitments
    queue::term()
}).

-type data() :: #data{}.

init(Data) ->
    process_flag(trap_exit, true),
    Primary = primary(Data),
    {ok, Pid} = start_tracker(Primary), Ref = erlang:monitor(process, Pid),
    Data2 = primary(Data, Primary#tracker{ pid = Pid, ref = Ref }),
    Mode = mode(Data),

    {ok, Mode, Data2}.

callback_mode() ->
    [state_functions, state_enter].

terminate(_Reason, _State, Data) ->
    Replicas = replicas(Data), [ok = stop_tracker(Tracker) || Tracker <- Replicas],
    Primary = primary(Data),

    ok = stop_tracker(Primary).

%%%===================================================================
%%%  State machine callbacks
%%%===================================================================

monolith(enter, _OldState, Data) ->
    %% NOTE Monolith setup assumes a solo "primary" state machine
    {keep_state, Data, []};

monolith({call, From}, {pop}, Data) ->
    Primary = primary(Data), Pid = Primary#tracker.pid,
    _ = aehc_parent_tracker:pop(Pid, From),

    {keep_state, Data, []};

monolith({call, From}, {commit, Commitment}, Data) ->
    Data2 = in(Commitment, Data), Primary = primary(Data2),
    Data3 = commit(Primary, Data2, From),

    {keep_state, Data3, []};

monolith({call, From}, {process, Hash}, Data) ->
    Primary = primary(Data), Pid = Primary#tracker.pid,
    _ = aehc_parent_tracker:process_block(Pid, Hash, From),

    {keep_state, Data, []};

monolith({call, From}, {register, PubKey}, Data) ->
    Primary = primary(Data), Pid = Primary#tracker.pid,
    Payload = aehc_parent_data:delegate(PubKey),
    _ = aehc_parent_tracker:send_tx(Pid, Payload, From),

    {keep_state, Data, []};

monolith(cast, {announce, _From, Top}, Data) ->
    _ = aec_events:publish(parent_top_changed, Top),

    {keep_state, Data, []};

monolith(Event, Req, Data) ->
    lager:info("~n~p:monolith(~p, ~p, ~p)~n",[Event, Req, Data]),
    {keep_state, Data, [postpone]}.

replica(enter, _OldState, Data) ->
    %% NOTE Replica setup assumes provision by attached replica state machines
    Replicas =
        lists:foldl(
            fun (Tracker, Acc) ->
                {ok, Pid} = start_tracker(Tracker), Ref = erlang:monitor(process, Pid),
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
    %% NOTE Shard setup assumes decomposition of state machines onto election master and history keeper
    [Tracker] = replicas(Data),
    {ok, Pid} = start_tracker(Tracker), Ref = erlang:monitor(process, Pid),
    Data2 = replicas(Data, [Tracker#tracker{ pid = Pid, ref = Ref }]),

    {keep_state, Data2, []};

shard(_Event, _Req, Data) ->
    %% TODO
    {keep_state, Data, [postpone]}.

-spec start_tracker(tracker()) -> {ok, pid()}.
start_tracker(Tracker) ->
    Module = Tracker#tracker.module, Args = Tracker#tracker.args, Pointer = Tracker#tracker.pointer,
    aehc_parent_tracker:start(Module, Args, Pointer).

-spec stop_tracker(tracker()) -> ok.
stop_tracker(Tracker) ->
    Pid = Tracker#tracker.pid,
    ok = aehc_parent_tracker:stop(Pid).

-spec commit(tracker(), data(), term()) -> data().
commit(Tracker, Data, From) ->
    Queue = queue:to_list(queue(Data)),
    Length = length(Queue), Capacity = Tracker#tracker.capacity,
    if
        Capacity == undefined; (Length rem Capacity) == 0 ->
            %% Design notes:
            %% a) We could utilize compose algorithms to apply the list of commitments as a package
            %% b) We could route commitments between parent chains (based on setup)
            %% TODO To support registry check before commitment
            [Commitment|_] = Queue,
            Primary = primary(Data), Pid = Primary#tracker.pid,
            Payload = aehc_parent_data:commitment(Commitment),

            ok = aehc_parent_tracker:send_tx(Pid, Payload, From),
            queue(Data, queue:new());
        true ->
            Data
    end.

%%%===================================================================
%%%  Data access layer
%%%===================================================================

-spec tracker(map()) -> tracker().
tracker(Tracker) ->
    Module = maps:get(<<"module">>, Tracker),
    Args = maps:get(<<"args">>, Tracker),
    Pointer = maps:get(<<"pointer">>, Tracker),
    #tracker{
        module = binary_to_atom(Module, unicode),
        args = Args,
        pointer = Pointer
    }.

-spec data(map()) -> data().
data(Setup) ->
    Mode = maps:get(<<"mode">>, Setup),
    Primary = maps:get(<<"primary">>, Setup),
    Replicas = maps:get(<<"replicas">>, Setup, []),
    Height = maps:get(<<"height">>, Setup, infinity),
    #data{
        mode = binary_to_atom(Mode, unicode),
        primary = tracker(Primary),
        height = Height,
        replicas = [tracker(R)||R <- Replicas],
        queue = queue:new()
    }.

-spec mode(data()) -> atom().
mode(Data) ->
    Data#data.mode.

-spec primary(data()) -> tracker().
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

-spec queue(data()) -> term().
queue(Data) ->
    Data#data.queue.

-spec queue(data(), term()) -> data().
queue(Data, Queue) ->
    Data#data{ queue = Queue }.

-spec in(term(), data()) -> data().
in(Item, Data) ->
    Queue2 = queue:in(Item, queue(Data)),
    queue(Data, Queue2).
