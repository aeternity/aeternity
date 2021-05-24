%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% <h3>Hyperchains parent's process manager</h3>
%%% See the pattern "ProcessManager" (https://www.enterpriseintegrationpatterns.com/patterns/messaging/ProcessManager.html).
%%% This component is responsible for orchestration the abstract parent's chain layer through:
%%%  a) dedicated workers (trackers);
%%%  b) supplied interface providers (connectors);
%%% - The first configured view acts as the "master view" and is eligible to dictate the election period through event's announcement;
%%% - A tracker performs synchronization from the highest known block until genesis pointer through "previous_hash" property;
%%% - To be able to run the Hyperchains the system must satisfy the connector's acceptance criteria;
%%%     a) default mode: get_top_block/0, get_block_by_hash/1l
%%%     b) delegate mode: default mode + dry_send_tx/1;
%%% - Interested developers can develop their own connectors for a particular parent chain;
%%% (TODO: To supply link to the official connector's development guide);
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_mng).

-behaviour(gen_server).

%% API.
-export([start_link/0
        , start_view/2
        , terminate_view/1
        , publish_block/2]).

%% gen_server.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("aeutils/include/aeu_stacktrace.hrl").


%% API.

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start_view(atom(), map()) -> {ok, pid()}.
start_view(Name, Conf) ->
    gen_server:call(?MODULE, {start_view, Name, Conf}).

-spec terminate_view(pid()) -> ok.
terminate_view(Name) ->
    gen_server:call(?MODULE, {terminate_view, Name}).

%% This event issued each time by connector when the new block is generated;
-spec publish_block(term(), aehc_parent_block:parent_block()) -> ok.
publish_block(View, Block) ->
    erlang:send(?MODULE, {publish_block, View, Block}),
    ok.

%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

-record(state, {master :: term(), trackers :: list()}).
init([]) ->
    process_flag(trap_exit, true),
    %% Read configuration;
    Trackers = [aehc_app:tracker_name(Tracker) || Tracker <- aehc_app:trackers_config()],
    [Master | _] = Trackers,
    {ok, #state{master = Master, trackers = Trackers}}.

handle_call({start_view, View, Conf}, _From, State) ->
    Res = aehc_sup:start_view(View, Conf),
    lager:info("~p start parent view: ~p (~p)", [View, aehc_app:tracker_name(Conf)]),
    {reply, Res, State};

handle_call({terminate_view, View}, _From, State) ->
    Res = aehc_sup:terminate_view(View),
    lager:info("~p terminate parent view: ~p", [View]),
    {reply, Res, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({publish_block, View, Block}, #state{} = State) ->
    %% To be able to persists the block we have to be sure that it connects to the previous existing one in the DB;
    %% That guaranties sequential order and consistency of the current chain view;
    %% Check that condition each time the block arrives allows to skip the whole chain traversing procedure;
    try
        true = aehc_parent_block:is_hc_parent_block(Block),
        %% %% Fork synchronization by by the new arrived block;
        aehc_parent_tracker:publish_block(View, Block)
    catch E:R:StackTrace ->
        lager:error("CRASH: ~p; ~p", [E, StackTrace]),
        {error, E, R}
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
