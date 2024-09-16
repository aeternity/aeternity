%%%-------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity
%%% @doc
%%% Gather cild->parent chain pinning information for AE HC and make it 
%%% available through API
%%% @end
%%%-------------------------------------------------------------------

-module(aec_pinning_agent).

-behaviour(gen_server).

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([start_link/2]).

%% Callbacks
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2,
    terminate/2, 
    code_change/3
]).

%%%=============================================================================
%%% functional API
%%%=============================================================================

-export([get_pinning_data/0]).

-define(SERVER, ?MODULE).

%% Loop state
-record(state, {
    parent_id,parent_type
}).
-type state() :: state.

-record(pin, {epoch,
        height,
        block_hash,
        parent_type,
        parent_network_id}).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link(binary(), atom()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(ParentId, ParentType) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ParentId, ParentType], []).

get_pinning_data() ->

    %gen_server:call(?SERVER, get_pinning_data_record).
    %get_pinning_data_record(<<"dev1">>, <<"aeternity">>).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

init([ParentId, ParentType]) ->
    LoopState = #state{parent_id = ParentId, parent_type = ParentType}, 
    {ok, LoopState}.

handle_call(get_pinning_data_record, _From, State) ->
    #state{parent_id = Id, parent_type = Type} = State,
    Reply = get_pinning_data_record(Id, Type),
    {reply, Reply, State};

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

get_pinning_data_record(Id, Type) ->
    aec_chain:top
    {1000,
        123456789,
        aeser_api_encoder:encode(key_block_hash, "12344567"),
        Type,
        Id}.

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.