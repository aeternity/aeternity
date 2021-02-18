-module(aehc_sup).
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
    Spec = case aehc_utils:hc_enabled() of
        true ->
            lager:info("Starting Hyperchains...."),
            [?CHILD(aehc_parent_mng, 5000, worker)];
        false ->
            []
    end,
    {ok, {{one_for_one, 5, 10}, Spec}}.
