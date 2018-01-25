-module(aemnesia_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         start_phase/3,
         stop/1]).

-export([check_env/0]).

%%====================================================================
%% API
%%====================================================================

start(StartType, StartArgs) ->
    mnesia_app:start(StartType, StartArgs).

start_phase(ae_ensure_mnesia_tables, _StartType, _PhaseArgs) ->
    aec_db:ensure_mnesia_tables().

stop(State) ->
    mnesia_app:stop(State).

check_env() ->
    set_mnesia_dir(
      case aeu_env:user_config([<<"chain">>, <<"db_path">>]) of
          undefined -> "data";
          {ok, V}   -> unicode:characters_to_list(V)
      end).

%%====================================================================
%% Internal functions
%%====================================================================

set_mnesia_dir(V) when is_list(V) ->
    application:set_env(mnesia, dir, V).
