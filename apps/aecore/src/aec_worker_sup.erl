-module(aec_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Mod,N,Type), {Mod,{Mod,start_link,[]},permanent,N,Type,[Mod]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs =
        maybe_upnp_worker()
        ++ [?CHILD(aec_metrics_rpt_dest, 5000, worker),
            ?CHILD(aec_keys, 5000, worker),
            ?CHILD(aec_tx_pool, 5000, worker),
            ?CHILD(aec_peer_analytics, 5000, worker),
            ?CHILD(aec_tx_pool_gc, 5000, worker),
            ?CHILD(aec_resilience, 5000, worker),
            ?CHILD(aec_capabilities, 5000, worker),
            ?CHILD(aec_db_gc, 5000, worker) ]
        ++ maybe_vote_tx_pool(),


    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_upnp_worker() ->
    case aec_upnp:is_enabled() of
        true  -> [?CHILD(aec_upnp, 5000, worker)];
        false -> []
    end.

maybe_vote_tx_pool() ->
    case aec_consensus:get_consensus_type() of
        pos -> [?CHILD(aec_hc_vote_pool, 5000, worker)];
        pow -> []
    end.
