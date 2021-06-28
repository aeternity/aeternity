%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%% Parent chains process manager
%% This component is responsible for orchestration of the Hyperchains backend (parent chain) through:
%% - dedicated state machine (tracker);
%% - blockchain interface (https://github.com/aeternity/aeconnector/wiki);
%% The component traverses parent chain data in lazy evaluation mode (on demand)
%% TODO: To provide HTTP API for scheduled commitments
%% TODO: To show dialog about registry record (show notify + address in telegram, throw error on commitment)
%% Used patterns:
%% - https://www.enterpriseintegrationpatterns.com/patterns/messaging/ProcessManager.html)
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_mng).

-include("../../aecore/include/blocks.hrl").
-include("aehc_utils.hrl").

-behaviour(gen_server).

%% API.
-export([start_link/0]).

-export([commit/1, register/1]).
-export([get_block_by_hash/1]).
-export([push/1, pop/0]).

-export([stop/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-type parent_block() :: aehc_parent_block:parent_block().
-type commitment() ::  aehc_commitment:commitment().

-type trees() :: aehc_parent_trees:trees().

-type pubkey() :: aec_keys:pubkey().

%% API.

-spec start_link() ->
    {ok, pid()} | ignore | {error, term()}.
start_link() ->
    State = state(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

-spec commit(commitment()) -> ok.
commit(Commitment) ->
    gen_server:call(?MODULE, {commit, Commitment}).

-spec get_block_by_hash(binary()) -> {ok, parent_block(), trees()} | {error, term()}.
get_block_by_hash(Hash) ->
    gen_server:call(?MODULE, {get_block_by_hash, Hash}).

%% To extract the block from a queue (FIFO)
-spec pop() -> {value, parent_block()} | empty.
pop() ->
    gen_server:call(?MODULE, {pop}).

-spec push(parent_block()) -> ok.
push(Block) ->
    gen_server:cast(?MODULE, {push, Block}).

-spec register(binary()) -> ok.
register(PubKey) ->
    gen_server:call(?MODULE, {register, PubKey}).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

-record(state, {
    pid :: term(),
    ref :: term(),
    %% FIFO queue of scheduled parent blocks to process
    queue::term()
}).

-type state() :: #state{}.

init(State) ->
    process_flag(trap_exit, true),

    Opt = [user_config, schema_default],
    {ok, Tracker} = aeu_env:find_config([<<"hyperchains">>, <<"tracker">>], Opt),

    {ok, Pid} = aehc_parent_tracker:start(Tracker), Ref = erlang:monitor(process, Pid),

    State2 = State#state{ pid = Pid, ref = Ref },
    {ok, State2}.

handle_call({pop}, _From, State) ->

    {Res, State2} = out(State),
    {reply, Res, State2};

handle_call({commit, Commitment}, From, State) ->
    Pid = State#state.pid,
    Payload = aehc_parent_data:commitment(Commitment),

    ok = aehc_parent_tracker:send_tx(Pid, Payload, From),
    {noreply, State};

handle_call({get_block_by_hash, Hash}, From, State) ->
    Pid = State#state.pid,

    ok = aehc_parent_tracker:get_block_by_hash(Pid, Hash, From),
    {noreply, State};

handle_call({register, PubKey}, From, State) ->
    Pid = State#state.pid,
    Payload = aehc_parent_data:delegate(PubKey),

    ok = aehc_parent_tracker:send_tx(Pid, Payload, From),
    {noreply, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({push, Block}, State) ->
    Hash = aehc_parent_block:hash_block(Block),
    State2 = in(Block, State),

    ok = aec_events:publish(parent_top_changed, Hash),
    {noreply, State2};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Pid = State#state.pid,
    ok = aehc_parent_tracker:stop(Pid).

%%%===================================================================
%%%  Data access layer
%%%===================================================================

-spec state() -> state().
state() ->
    #state{ queue = queue:new() }.

-spec queue(state()) -> term().
queue(State) ->
    State#state.queue.

-spec queue(state(), term()) -> state().
queue(State, Queue) ->
    State#state{ queue = Queue }.

-spec in(term(), state()) -> state().
in(Item, State) ->
    Queue2 = queue:in(Item, queue(State)),
    queue(State, Queue2).

-spec out(state()) -> {{value, term()}, state()} | {empty, state()}.
out(State) ->
    Queue = queue(State),
    case queue:out(Queue) of
        {Res = {value, _Item}, Queue2} ->
            Data2 = queue(State, Queue2),
            {Res, Data2};
        {Res = empty, _Queue2} ->
            {Res, State}
    end.
