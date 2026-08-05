-module(aeutils_app).
-behaviour(application).

-export([ start/2
        , start_phase/3
        , prep_stop/1
        , stop/1 ]).

start(_StartType, _StartArgs) ->
    _ = aeu_info:block_info(), %% init the cache so this process is the owner of the table
    _ = aec_tx_pool_failures:settings(), %% init the cache so this process is the owner of the table
    aeutils_sup:start_link().

start_phase(_Phase, _Type, _Args) ->
    ok.

prep_stop(_State) ->
    ok.

stop(_State) ->
    %% The cache holds what `$user_map'/`$user_config' hold, so it has to go
    %% when they do: unloading aeutils clears the app env but would leave the
    %% cache serving the config of an application that is no longer there.
    ok = aeu_env:invalidate_config_cache(),
    ok.
