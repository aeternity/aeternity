%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%% =============================================================================
%%% @copyright 2018-21, Aeternity Anstalt
%%% @doc
%%%    Supervisor for the aeutils application
%%% @end
%%% =============================================================================
-module(aeutils_sup).
-behaviour(supervisor).


-export([start_link/0]).

-export([init/1]).


-define(SERVER, ?MODULE).
-define(CHILD(Mod,N,Type), {Mod,{Mod,start_link,[]},permanent,N,Type,[Mod]}).
-define(CHILD(Mod,N,Type,Params), {Mod,{Mod,start_link,Params},permanent,N,Type,[Mod]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    ChildSpecs = [],

    {ok, {{one_for_one, 0, 10}, ChildSpecs}}.
