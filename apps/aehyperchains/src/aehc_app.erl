-module(aehc_app).
-behavior(application).

-export([
          start/2
        , start_phase/3
        , prep_stop/1
        , stop/1
        , check_env/0
        ]).

-export([trackers_config/0, tracker_name/1, tracker_note/1]).

start(_StartType, _StartArgs) ->
    Res = aehc_sup:start_link(),
    [aehc_parent_mng:start_view(tracker_name(Conf), Conf) || Conf <- trackers_config()],
    Res.

start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

prep_stop(_State) ->
    ok.

stop(_State) ->
    ok.

check_env() ->
    check_env([{[<<"hyperchains">>, <<"enabled">>], {set_env, enabled}}]),
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

-spec trackers_config() -> nonempty_list(map()).
trackers_config() ->
    aeu_env:config_value([<<"hyperchains">>, <<"trackers">>], aehyperchains, [hyperchains, trackers], []).

-spec tracker_name(map()) -> term().
tracker_name(Conf) ->
    maps:get(<<"name">>, Conf).

-spec tracker_note(map()) -> term().
tracker_note(Conf) ->
    maps:get(<<"note">>, Conf).
