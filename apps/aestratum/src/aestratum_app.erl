-module(aestratum_app).

-behavior(application).

-export([start/2,
         stop/1]).

start(_Type, _Args) ->
    {ok, Config} = aeu_env:user_config(<<"stratum">>),
    aestratum_sup:start_link(Config).

stop(_State) ->
	ok.
