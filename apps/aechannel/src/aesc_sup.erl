-module(aesc_sup).
-behaviour(supervisor).

-export([
          start_link/0
        , init/1
        ]).

-define(SERVER, ?MODULE).
-define(CHILD(Mod,N,Type), {Mod,{Mod,start_link,[]},permanent,N,Type,[Mod]}).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, [?CHILD(aesc_state_cache, 5000, worker)]}}.
