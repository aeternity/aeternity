-module(aeeqc_props).
-export([is_props_mod/1]).
-export([prop_names/1]).

-define(MOD_SUFFIX, "_eqc").
-define(REV_MOD_SUFFIX, "cqe_").

is_props_mod(ModS) ->
    ?REV_MOD_SUFFIX = lists:reverse(?MOD_SUFFIX),
    case lists:reverse(ModS) of
        ?REV_MOD_SUFFIX ++ _ ->
            true;
        _ ->
            false
    end.

prop_names(Mod) ->
    lists:map(
      fun({Name, 0}) when is_atom(Name) -> Name end,
      lists:filter(
        fun is_property/1,
        Mod:module_info(exports))).

is_property({Name, Arity}) when is_integer(Arity), Arity > 0,
                                is_atom(Name) ->
    false;
is_property({Name, 0}) ->
    case atom_to_list(Name) of
        "prop_" ++ _ ->
            true;
        _ ->
            false
    end.
