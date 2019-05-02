-module(aeeqc_eunit).
-export([is_tests_mod/1]).
-export([props_mod_test_repr/1]).
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

props_mod_test_repr(TestsModS) ->
    PropsMod = list_to_atom(props_mod(TestsModS)),
    {setup,
     fun() -> eqc:start() end,
     fun(_) -> eqc:stop() end,
     lists:map(
       fun(PropName) -> prop_test_repr(PropsMod, PropName) end,
       aeeqc_props:prop_names(PropsMod))
     }.

props_mod(TestsModS) ->
    ?REV_SUFFIX = lists:reverse(?SUFFIX),
    ?REV_SUFFIX ++ RevPropsModS = lists:reverse(TestsModS),
    lists:reverse(RevPropsModS).

prop_test_repr(Mod, Name) ->
    prop_test_repr(Mod, Name, 500).

prop_test_repr(Mod, Name, Ms) ->
    {atom_to_list(Mod) ++ ":" ++ atom_to_list(Name),
     {timeout, (Ms * 3) / 1000,
      fun() -> true = eqc:quickcheck(eqc:testing_time(Ms / 1000, Mod:Name())) end
     }}.

tests_mod(ModS) ->
    ModS ++ ?SUFFIX.
