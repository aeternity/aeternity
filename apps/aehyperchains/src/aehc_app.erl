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


%% API

start(_StartType, _StartArgs) ->
    Res = aehc_sup:start_link(),
    case aehc_utils:hc_enabled() of
        true ->
            [aehc_parent_mng:start_view(tracker_name(Conf), Conf) || Conf <- trackers_config()];
        false ->
            ok
    end,
    Res.

start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

prep_stop(_State) ->
    ok.

stop(_State) ->
    ok.

check_env() ->
    case aehc_utils:hc_enabled() of
        true ->
            lager:info("Hyperchains are enabled");
        false ->
            lager:info("Hyperchains are disabled"),
            ok
    end.

-spec trackers_config() -> nonempty_list(map()).
trackers_config() ->
    {ok, Trackers} = aeu_env:find_config([<<"hyperchains">>, <<"trackers">>],
        [user_config, schema_default, {env, aehyperchains, [hyperchains, trackers]}]),
    Trackers.

-spec tracker_name(map()) -> term().
tracker_name(Conf) ->
    maps:get(<<"name">>, Conf).

-spec tracker_note(map()) -> term().
tracker_note(Conf) ->
    maps:get(<<"note">>, Conf).
