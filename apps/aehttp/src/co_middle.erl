-module(co_middle).

-export([execute/2]).

execute(Req, Env) ->
      lager:debug("XXX"),
      {ok, Req, Env}.
