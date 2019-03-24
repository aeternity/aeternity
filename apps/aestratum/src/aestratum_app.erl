-module(aestratum_app).

-behavior(application).

-export([start/2,
         stop/1]).

start(_Type, _Args) ->
    aestratum_sup:start_link(#{}).

stop(_State) ->
	ok.
