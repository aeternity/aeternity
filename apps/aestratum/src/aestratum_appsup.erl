-module(aestratum_appsup).

-behavior(application).
-behavior(supervisor).

-export([start/2, stop/1]). % APP
-export([init/1]).          % SUP

-include("aestratum.hrl").

%%%%%%%%%% APP

start(_Type, _Args) ->
    aestratum_fn:ok_val_err(aestratum_env:setup(), cant_setup_stratum),
    supervisor:start_link({local, ?MODULE}, ?MODULE, ?ENABLED).

stop(_State) -> ok.

%%%%%%%%%% SUP

init(false) ->
    {ok, {{one_for_all, 1, 5}, []}};
init(true) ->
    aestratum_db:create_tables(disc), % ensure needed tables are present
    Procs = lists:map(fun worker/1, [aestratum,
                                     aestratum_extra_nonce_cache,
                                     aestratum_user_register])
        ++ [ranch_listener()],
    {ok, {{one_for_all, 1, 5}, Procs}}.

worker(M) ->
    {M, {M, start_link, []}, permanent, 5000, worker, [M]}.

ranch_listener() ->
    ranch:child_spec(aestratum_listener, ranch_tcp,
                     [{max_connections, ?MAX_CONNECTIONS},
                      {num_acceptors, ?NUM_ACCEPTORS},
                      {port, ?PORT}], %% TODO: ip
                     aestratum_handler, []).
