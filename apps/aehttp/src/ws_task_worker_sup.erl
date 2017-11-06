%%%-------------------------------------------------------------------
%% @doc aehttp websocket task's worker supervisor 
%% @end
%%%-------------------------------------------------------------------

-module(ws_task_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Execute message
-export([spawn_child_for_msg/2]).

-define(SERVER, ?MODULE).
-define(POOL_NAME, ws_tasks_pool).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    WsTasksPoolSize = pool_size(),
    PoolBoySpec = poolboy:child_spec(?POOL_NAME,
        [{name, {local, ?POOL_NAME}},
          {worker_module, ws_task_worker},
          {size, WsTasksPoolSize},
          {max_overflow, WsTasksPoolSize * 2}],
        [ws_task_worker_args]),
    {ok, { {one_for_one, 10, 10}, [PoolBoySpec]} }.

spawn_child_for_msg(Msg, WsPid) ->
    poolboy:transaction(?POOL_NAME, fun(Worker) ->
        gen_server:call(Worker, {handle_msg, {Msg, WsPid}})
    end).


%%====================================================================
%% Internal functions
%%====================================================================
pool_size() ->
    case application:get_env(aehttp, internal) of
        {ok, InternalConfig} ->
            WsConfig = proplists:get_value(websocket, InternalConfig),
            proplists:get_value(tasks, WsConfig);
        undefined ->
            100
    end.
