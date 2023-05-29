-module(aesc_app).
-behavior(application).

-export([
          start/2
        , start_phase/3
        , prep_stop/1
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    aesc_fsm:init_active_fsm_counter(),
    aesc_sup:start_link().

start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

prep_stop(_State) ->
    ok.

stop(_State) ->
    catch aesc_fsm:unreg_active_fsm_counter(),
    ok.
