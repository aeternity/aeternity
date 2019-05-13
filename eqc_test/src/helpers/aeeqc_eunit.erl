-module(aeeqc_eunit).
-export([is_tests_mod/1]).
-export([props_mod_test_repr/2]).
-export([testing_time_multiplier/0]).
-export([tests_mod/1]).

-define(SUFFIX, "_tests").
-define(REV_SUFFIX, "stset_").

is_tests_mod(ModS) ->
    ?REV_SUFFIX = lists:reverse(?SUFFIX),
    case lists:reverse(ModS) of
        ?REV_SUFFIX ++ _ ->
            true;
        _ ->
            false
    end.

props_mod_test_repr(TestsModS, TestingTimeMsFun) ->
    PropsMod = list_to_atom(props_mod(TestsModS)),
    {setup,
     fun() -> eqc:start() end,
     fun(_) -> eqc:stop() end,
     lists:map(
       fun(PropName) ->
               prop_test_repr(PropsMod, PropName, TestingTimeMsFun(PropName))
       end,
       aeeqc_props:prop_names(PropsMod))
     }.

props_mod(TestsModS) ->
    ?REV_SUFFIX = lists:reverse(?SUFFIX),
    ?REV_SUFFIX ++ RevPropsModS = lists:reverse(TestsModS),
    lists:reverse(RevPropsModS).

prop_test_repr(Mod, Name, Ms) when is_integer(Ms), Ms > 0 ->
    {atom_to_list(Mod) ++ ":" ++ atom_to_list(Name),
     {timeout, (Ms * 3) / 1000,
      fun() -> true = eqc:quickcheck(eqc:testing_time(Ms / 1000, Mod:Name())) end
     }}.

testing_time_multiplier() ->
    {ok, [[MS]]} = init:get_argument(eqc_testing_time_multiplier),
    case list_to_integer(MS) of M when is_integer(M), M > 0 -> M end.

tests_mod(ModS) ->
    ModS ++ ?SUFFIX.
