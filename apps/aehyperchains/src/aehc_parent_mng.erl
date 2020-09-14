%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
-module(aehc_parent_mng).

-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-define(DEFAULT_CONNECTOR_IDs, [<<"aehc_aeternity_connector">>]).

%% API.

-spec start_link() ->
    {ok, pid()} | ingnore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%%  gen_server behaviour
%%%===================================================================

-record(pointer, { hash :: binary(), connector :: aehc_connector:connector(), args :: map(), note :: binary() }).

-record(state, { master :: aehc_connector:connector(), pointers :: list() }).

init([]) ->
    process_flag(trap_exit, true),
    [Master|_] = Pointers = [pointer(P) || P <- get_pointers()],
    [aehc_connector_sup:start_child(P#pointer.connector, P#pointer.args) || P <- Pointers],
    {ok, #state{ master = Master, pointers = Pointers }}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%%  Configuration access layer
%%%===================================================================

get_pointers() ->
    aeu_env:user_config([<<"chain">>, <<"hyperchains">>, <<"pointers">>]).

pointer(Conf) ->
    #pointer{
        hash = maps:get(<<"hash">>, Conf),
        connector = binary_to_existing_atom(maps:get(<<"connector">>, Conf), utf8),
        args = maps:get(<<"args">>, Conf),
        note = maps:get(<<"note">>, Conf)
    }.


%% NOTE: Manager is responsible to start/stop connectors;
