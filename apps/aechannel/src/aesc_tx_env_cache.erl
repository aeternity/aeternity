-module(aesc_tx_env_cache).
-behavior(gen_server).

-export([tx_env_and_trees_from_top/1]).

-export([start_link/0]).

%% Behavior exports
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-define(SERVER, ?MODULE).
-record(st, {top_env = #{}}).

tx_env_and_trees_from_top(Type) ->
    gen_server:call(?SERVER, {tx_env_and_trees_from_top, Type}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    true = aec_events:subscribe(top_changed),
    {ok, #st{}}.

handle_call({tx_env_and_trees_from_top, Type}, _From, St) ->
    {Res, St1} = tx_env_and_trees_from_top_(Type, St),
    {reply, Res, St1};
handle_call(_, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({gproc_ps_event, top_changed, _}, #st{top_env = C} = St) when map_size(C) == 0 ->
    {noreply, St};
handle_info({gproc_ps_event, top_changed, _}, St) ->
    %% Evict the old top env from the cache
    {noreply, St#st{top_env = #{}}};
handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.

tx_env_and_trees_from_top_(Type, #st{top_env = Cache} = St) ->
    Key = top_env_ckey(Type),
    case maps:find(Key, Cache) of
        error ->
            Res = aetx_env:tx_env_and_trees_from_top(Type),
            {Res, St#st{top_env = Cache#{Key => Res}}};
        {ok, Res} ->
            {Res, St}
    end.

top_env_ckey(Type) ->
    {top_env_and_trees, Type}.
