-module(aestratum_app).

-behavior(application).

-export([start/2,
         stop/1
        ]).

start(_Type, _Args) ->
    {ok, Cfg} = aestratum_config:read(),
    aestratum_sup:start_link(Cfg).

stop(_State) ->
	ok.

