%%% -*- mode: erlang; erlang-indent-level: 4; erlang-tabs-mode: nil -*-
-module(aehc_sup).
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
    Spec = case aehc_utils:hc_enabled() of
        true -> [ ?CHILD(aehc_parent_mng, 5000, worker)
                , ?CHILD(aehc_connector_sup, 5000, supervisor)];
        false ->
            []
    end,
    {ok, {{one_for_one, 5, 10}, Spec}}.

