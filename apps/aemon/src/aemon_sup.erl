-module(aemon_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


-define(SERVER, ?MODULE).
-define(CHILD(Mod,N,Type), {Mod,{Mod,start_link,[]},permanent,N,Type,[Mod]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    ChildSpecs = case aemon_config:is_active() of
        false -> [];
        true -> [
                 ?CHILD(aemon_mon_ttl, 5000, worker),
                 ?CHILD(aemon_mon_on_chain, 5000, worker),
                 ?CHILD(aemon_mon_gen_stats, 5000, worker),
                 ?CHILD(aemon_mon, 5000, worker),

                 ?CHILD(aemon_publisher, 5000, worker)
                ]
    end,

    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.
