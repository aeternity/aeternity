%%% -*- mode: erlang; erlang-indent-level: 4; erlang-tabs-mode: nil -*-
-module(aehc_sup).
-behaviour(supervisor).

-export([
          start_link/0
        , start_view/3
        , terminate_view/1
        , init/1
        ]).

-define(SERVER, ?MODULE).
-define(CHILD(Mod,N,Type), {Mod,{Mod,start_link,[]},permanent,N,Type,[Mod]}).
-define(CHILD(Id,Mod,Args,N,Type), {Id,{Mod,start_link,Args},permanent,N,Type,[Mod]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Spec = case aehc_utils:hc_enabled() of
        true -> [?CHILD(aehc_parent_mng, 5000, worker)];
        false ->
            []
    end,
    {ok, {{one_for_one, 5, 10}, Spec}}.


-spec start_view(term(), map(), binary()) -> {ok, pid()}.
start_view(View, Conf, Desc) ->
    Res = {ok, _Pid} = supervisor:start_child(?SERVER, ?CHILD(Id, aehc_parent_sup, [View, Conf], 5000, worker)),
    lager:info("~p start parent view: ~p (~p)", [View, Desc]),
    Res.

-spec terminate_view(term()) -> ok.
terminate_view(View) ->
    Res = ok = supervisor:terminate_child(?SERVER, View),
    lager:info("~p terminate parent view: ~p", [View]),
    Res.
