-module(aestratum_env).

%% ENV handling
-export([setup/0,
         get/1,
         set/1,
         reset/1,
         unset/0, unset/1,
         keys/0]).

%%%%%%%%%%

setup() ->
    unset(),
    UserConfig = aeu_env:user_config(<<"stratum">>, [{<<"enabled">>, false}]),
    aestratum_config:setup_env(UserConfig).

get(Key) ->
    {ok, Val} = application:get_env(aestratum, Key),
    Val.

set(ConfigMap) ->
    maps:map(fun (K, V) -> application:set_env(aestratum, K, V), V end, ConfigMap).

reset(ConfigMap) ->
    unset(),
    set(ConfigMap),
    ConfigMap.

unset() ->
    unset(keys()).
unset(Keys) ->
    [application:unset_env(aestratum, K) || K <- Keys].

keys() ->
    {Keys, _} = lists:unzip(application:get_all_env(aestratum)),
    Keys.
