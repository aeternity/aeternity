%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% <h3>Hyperchains parent's process manager</h3>
%%% See the pattern "ProcessManager" (https://www.enterpriseintegrationpatterns.com/patterns/messaging/ProcessManager.html).
%%% This component is responsible for orchestration the abstract parent's chain layer through:
%%%  a) dedicated workers (trackers);
%%%  b) supplied interface providers (conenctors);
%%% - The first configured view acts as the "master view" and is eligible to dictate the election period through event's announcement;
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

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-include_lib("aeutils/include/aeu_stacktrace.hrl").
%% API.

-spec start_link() ->
    {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% This event issued each time by connector when the new block is generated;
-spec publish_block(term(), aehc_parent_block:parent_block()) -> ok.
publish_block(View, Block) ->
    erlang:send(?MODULE, {publish_block, View, Block}),
    ok.

%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

-record(state, { master :: term(), views :: list() }).
init([]) ->
    process_flag(trap_exit, true),
    %% Read configuration;
    {ok, Config} = tracks_config(),
    Views = [name(Track)|| Track <- Config],
    [Master|_] = Views,
    %% Run parent views;
    [aehc_sup:start_view(name(Conf), Conf, note(Conf)) || Conf <- Config],
    {ok, #state{ master = Master, views = Views }}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({publish_block, View, Block}, #state{} = State) ->
    %% To be able to persists the block we have to be sure that it connects to the previous existed one in the DB;
    %% That guaranties sequential order and consistency of the current chain view;
    %% Check that condition each time when block arrived allows to skip the whole chain traversing procedure;
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

-spec name(map()) -> term().
name(Conf) ->
    maps:get(<<"name">>, Conf).

-spec note(map()) -> term().
note(Conf) ->
    maps:get(<<"note">>, Conf).

-spec tracks_config() -> {ok, nonempty_list(map())}.
tracks_config() ->
    Res = {ok, _} = aeu_env:user_config([<<"hyperchains">>, <<"tracks">>]),
    Res.

