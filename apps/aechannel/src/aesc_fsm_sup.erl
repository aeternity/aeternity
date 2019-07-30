-module(aesc_fsm_sup).
-behaviour(supervisor).

-export([ start_child/1
        , start_link/0
        , init/1
        ]).

-define(SUPERVISOR, ?MODULE).
-define(CHILD_MOD, aesc_fsm).

%% Noise sessions have restart type 'temporary', since it makes no sense to
%% restart them.
-define(CHILD(Mod,N,Type), {Mod,{Mod,start_link,[]},temporary,N,Type,[Mod]}).

start_child(Args) when is_list(Args) ->
    supervisor:start_child(?SUPERVISOR, Args).

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(?CHILD_MOD, 5000, worker)]}}.
