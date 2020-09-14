%%% -*- mode: erlang; erlang-indent-level: 4; erlang-tabs-mode: nil -*-
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% <h3>Hyperchains parent's view supervisor</h3>
%%% Accordingly to design the synchronized state of the parent view is achieved by:
%%% a) parent chain interface provider (connector);
%%% b) parent chain state machine (tracker)
%%% NOTE: When connector process is broken the tracker should be restarted to ensure sync state;
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_sup).
-behaviour(supervisor).

-export([start_link/2, init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Mod,Args,N,Type), {Mod,{Mod,start_link,Args},permanent,N,Type,[Mod]}).

start_link(View, Conf) ->
    supervisor:start_link(?MODULE, [View, Conf]).

init([View, Conf]) ->
    Con = aehc_connector:module(Conf),
    Args = aehc_connector:args(Conf),
    ConSpec = ?CHILD(Con, [Args], 5000, worker),
    TrackSpec = ?CHILD(aehc_parent_tracker, [View, Conf], 5000, worker),
    {ok, {{one_for_all, 5, 10}, [ConSpec, TrackSpec]}}.
