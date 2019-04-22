-module(aestratum_env).

%% ENV handling
-export([setup/0,
         get/1,
         get/2,
         set/1,
         set/2,
         reset/1,
         unset/0, unset/1,
         keys/0]).

%%%%%%%%%%

setup() ->
    unset(),
    UserConfig = aeu_env:user_config(<<"stratum">>, [{<<"enabled">>, false}]),
    aestratum_config:setup_env(UserConfig).

get(Key) ->
    case application:get_env(aestratum, Key) of
        {ok, V}   -> V;
        undefined -> error({config_key_not_set, Key})
    end.

get(Key, Default) ->
    application:get_env(aestratum, Key, Default).

set(Config) when is_map(Config) ->
    maps:map(fun set/2, Config).

set(K, V) ->
    application:set_env(aestratum, K, V),
    V.

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
