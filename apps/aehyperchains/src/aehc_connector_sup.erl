%%% -*- mode: erlang; erlang-indent-level: 4; erlang-tabs-mode: nil -*-
-module(aehc_connector_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-export([start_child/3, terminate_child/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Mod,Args,N,Type), {Mod,{Mod,start_link,Args},permanent,N,Type,[Mod]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

-spec start_child(term(), map(), binary()) -> {ok, pid()}.
start_child(Id, Args, Desc) ->
    Res = {ok, _Pid} = supervisor:start_child(?SERVER, ?CHILD(Id, Args, 5000, worker)),
    lager:info("~p start connector: ~p (~p)", [Id, Desc]),
    Res.

-spec terminate_child(term()) -> ok.
terminate_child(Id) ->
    Res = ok = supervisor:terminate_child(?SERVER, Id),
    lager:info("~p terminate connector: ~p", [Id]),
    Res.

