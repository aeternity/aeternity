%%% -*- mode: erlang; erlang-indent-level: 4; erlang-tabs-mode: nil -*-
-module(aehc_connector_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-export([start_child/2, terminate_child/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Mod,Args,N,Type), {Mod,{Mod,start_link,Args},permanent,N,Type,[Mod]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

start_child(Id, Args) ->
    Res = {ok, Pid} = supervisor:start_child(?SERVER, ?CHILD(Id, Args, 5000, worker)),
    true = is_pid(Pid),
    lager:info("~p start child: ~p", [Id]),
    Res.

terminate_child(Id) ->
    Res = ok = supervisor:terminate_child(?SERVER, Id),
    lager:info("~p terminate child: ~p", [Id]),
    Res.

