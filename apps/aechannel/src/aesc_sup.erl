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
    {ok, {{one_for_one, 5, 10}, [ ?CHILD(aesc_chain_watcher, 5000, worker)
                                , ?CHILD(aesc_state_cache, 5000, worker)
                                , ?CHILD(aesc_tx_env_cache, 5000, worker)
                                , ?CHILD(aesc_limits, 5000, worker)
                                , ?CHILD(aesc_fsm_sup, 5000, supervisor)
                                , ?CHILD(aesc_session_noise_sup, 5000, supervisor)
                                , ?CHILD(aesc_listeners, 5000, worker)
                                ]}}.
