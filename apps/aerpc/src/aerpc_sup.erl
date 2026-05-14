-module(aerpc_sup).
-behaviour(supervisor).

-export([
          start_link/0
        , init/1
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Subs = #{id      => aerpc_subscriptions,
             start   => {aerpc_subscriptions, start_link, []},
             restart => permanent,
             shutdown => 5000,
             type    => worker,
             modules => [aerpc_subscriptions]},
    Indexer = #{id      => aerpc_log_indexer,
                start   => {aerpc_log_indexer, start_link, []},
                restart => permanent,
                shutdown => 5000,
                type    => worker,
                modules => [aerpc_log_indexer]},
    {ok, {{one_for_one, 5, 10}, [Indexer, Subs]}}.
