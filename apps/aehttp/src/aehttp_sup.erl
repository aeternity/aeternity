%%%-------------------------------------------------------------------
%% @doc aehttp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(aehttp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    %% Maintains the cached chain-state height for the X-Ae-Height response
    %% header (GH-4186), updated from `top_changed' events so the header is a
    %% cheap ETS read on the request hot path.
    StateVersion = #{ id       => aehttp_state_version
                    , start    => {aehttp_state_version, start_link, []}
                    , restart  => permanent
                    , shutdown => 5000
                    , type     => worker
                    , modules  => [aehttp_state_version] },
    {ok, { {one_for_one, 5, 10}, [StateVersion]} }.

%%====================================================================
%% Internal functions
%%====================================================================
