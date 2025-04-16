%%%-----------------------------------------------------------------------------
%%% @doc
%%% Service for finding and managing penalties for Hyperchains
%%% @end
%%%-----------------------------------------------------------------------------

-module(aec_hc_penalty_service).
-author("mans@happihacking.se").
-behaviour(gen_server).

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    start_link/0,
    stop/0
]).

%% Callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    test_and_register_block_offence/1
]).

-define(SERVER, ?MODULE).

%% Loop state
-record(loop_state, {

}).
-type loop_state() :: loop_state.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

test_and_register_block_offence(Block) ->
    gen_server:call(?SERVER, { test_and_register_block_offence, Block }).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

init([]) ->
    lager:debug("init: ~p", []),
    LoopState = #loop_state{},
    {ok, LoopState}.

handle_call({test_and_register_block_offence, Block}, _From, State) ->
    {Res, NewState} = case test_block_offence(Block) of
        {ok, []} ->
            {ok, State};
        {ok, Pens} ->
            NS = register_offences(Pens, State),
            {ok, NS};
        {error, Reason} ->
            {{error, Reason}, State}
    end,
    {reply, Res, NewState};

handle_call(_Request, _From, LoopState) ->
    Reply = ok,
    {reply, Reply, LoopState}.

handle_cast(_Msg, LoopState) ->
    {noreply, LoopState}.

handle_info(_Info, LoopState) ->
    {noreply, LoopState}.

terminate(_Reason, _LoopState) ->
    ok.

code_change(_OldVsn, LoopState, _Extra) ->
    {ok, LoopState}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

register_offences(_Pens, State) ->
    lager:debug("register_offences: ~p", [_Pens]),
    State.

test_block_offence(_Block) ->
    lager:debug("test_block_offence: ~p", [_Block]),
    {ok, []}.

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
