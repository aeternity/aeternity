%%%-------------------------------------------------------------------
%%% @doc Supervisor for the JSON-RPC app.
%%%
%%% The public-read surface needs no always-on worker. The log indexer
%%% is deliberately NOT a static child: it subscribes to every
%%% `top_changed' event and pushes each generation's logs into ETS, so
%%% starting it unconditionally puts per-block work and a growing table
%%% on every node that merely has this app installed -- including nodes
%%% whose operator never enabled the endpoint. `aerpc_log_indexer' is
%%% started explicitly by the logs/filters work that needs it, together
%%% with the retention policy that bounds it.
%%%
%%% Until then `eth_getLogs' serves from the inline `aec_chain' walker,
%%% which `aerpc_logs:dispatch_collect/1' already falls back to whenever
%%% the index does not cover the requested range.
%%% @end
%%%-------------------------------------------------------------------
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
    {ok, {{one_for_one, 5, 10}, [Subs]}}.
