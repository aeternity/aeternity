-module(aehc_app).
-behavior(application).

-export([
          start/2
        , start_phase/3
        , prep_stop/1
        , stop/1
        , check_env/0
        ]).

-export([get_connector_id/0]).

-define(DEFAULT_CONNECTOR_ID, <<"aehc_aeternity_connector">>).

start(_StartType, _StartArgs) ->
    aehc_sup:start_link().

start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

prep_stop(_State) ->
    ok.

stop(_State) ->
    ok.

check_env() ->
    check_env([{[<<"chain">>, <<"hyperchains">>, <<"enabled">>], {set_env, enabled}}]),
    case aehc_utils:hc_enabled() of
        true ->
            lager:info("Hyperchains are enabled"),
            aehc_utils:hc_install();
        false ->
            lager:info("Hyperchains are disabled"),
            ok
    end.

check_env(Spec) ->
    lists:foreach(
      fun({K, F}) ->
              case aeu_env:user_config(K) of
                  undefined -> ignore;
                  {ok, V}   -> set_env(F, V)
              end
      end, Spec).

set_env({set_env, K}, V) when is_atom(K) ->
    io:fwrite("setenv K=~p, V=~p~n", [K, V]),
    application:set_env(aehyperchains, K, V);
set_env(F, V) when is_function(F, 1) ->
    F(V).

get_connector_id() ->
    aeu_env:config_value([<<"chain">>, <<"hyperchains">>, <<"connector">>, <<"id">>],
                         aehyperchains, [hyperchains, connector, id], ?DEFAULT_CONNECTOR_ID).
