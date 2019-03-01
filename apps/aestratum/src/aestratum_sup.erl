-module(aestratum_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Childs = [{aestratum_node_sup, {aestratum_node_sup, start_link, []},
               permanent, 5000, supervisor, [aestratum_node_sup]},
              {aestratum_connection_sup, {aestratum_connection_sup, start_link, []},
               permanent, 5000, supervisor, [aestratum_connection_sup]}],
    {ok, {{one_for_one, 5, 10}, Childs}}.
