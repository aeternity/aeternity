-module(eqc_test_enabled_tests).
-include_lib("eunit/include/eunit.hrl").

-define(DISABLED_PROPS_MODS,
        [ aec_sync_eqc
        , fate_compiler_eqc
        , txs_eqc
        , txs_ga_eqc
        , txs_glue_eqc
        , txs_sign_eqc
        , txs_sign_meta_eqc
        ]).

-define(REBAR3_BUILD_DIR, "_build").
-define(REBAR3_PROFILE_DIR, "test+eqc").
-define(BEAM_SUFFIX, ".beam").

each_eqc_mod_is_hooked_unless_whitelisted_test() ->
    Path = [_|_] = code:get_path(),
    PropsPath = [_|_] = props_path(Path),
    TestsPath = [_|_] = tests_path(Path),
    PropsMods       = [_|_] = lists:filter(fun is_props_mod/1, mods_from_beam_files([_|_] = find_beam_files_in_path(PropsPath))),
    ActualTestsMods = [_|_] = lists:filter(fun is_tests_mod/1, mods_from_beam_files([_|_] = find_beam_files_in_path(TestsPath))),
    ExpectedTestsMods = lists:map(fun tests_mod/1, PropsMods),
    ExpectedMissingTestsMods = lists:map(fun tests_mod/1, ?DISABLED_PROPS_MODS),
    ?assertEqual(ExpectedMissingTestsMods, lists:sort(ExpectedTestsMods -- ActualTestsMods)). %% This prints missing `*_tests` modules: error message would be clearer if it printed properties (`*_eqc`) modules.

props_path(Path) ->
    lists:filter(fun is_props_dir/1, Path).

is_props_dir(Dir) ->
    shallow_path_prefix(?REBAR3_BUILD_DIR, [?REBAR3_PROFILE_DIR, "extras", "eqc"], Dir).

tests_path(Path) ->
    lists:filter(fun is_tests_dir/1, Path).

is_tests_dir(Dir) ->
    shallow_path_prefix(?REBAR3_BUILD_DIR, [?REBAR3_PROFILE_DIR, "lib", "eqc_test"], Dir).

shallow_path_prefix(ShallowSplittingComponent = [_|_], Prefix = [[_|_]|_], Dir) ->
    Components = lists:map(fun filename_to_string/1, filename:split(Dir)),
    shallow_prefix(ShallowSplittingComponent, Prefix, Components).

shallow_prefix(ShallowSplittingElem, Prefix, List) ->
    case
        lists:splitwith(
          fun(El) -> El =/= ShallowSplittingElem end,
          lists:reverse(List))
    of
        {_, []} ->
            false;
        {RevT, [X|_]} when X =:= ShallowSplittingElem ->
            lists:prefix(Prefix, lists:reverse(RevT))
    end.

find_beam_files_in_path(Path) ->
    lists:flatmap(fun find_beam_files_in_dir/1, Path).

find_beam_files_in_dir(Dir) ->
    filelib:wildcard("**/*" ++ ?BEAM_SUFFIX, Dir).

mods_from_beam_files(Fs) ->
    lists:map(fun mod_from_beam_file/1, Fs).

mod_from_beam_file(F) ->
    ModS = filename_to_string(filename:basename(F, ?BEAM_SUFFIX)),
    list_to_atom(ModS).

is_props_mod(Mod) ->
    aeeqc_props:is_props_mod(atom_to_list(Mod)).

is_tests_mod(Mod) ->
    aeeqc_eunit:is_tests_mod(atom_to_list(Mod)).

tests_mod(Mod) ->
    list_to_atom(aeeqc_eunit:tests_mod(atom_to_list(Mod))).

-spec filename_to_string(file:filename_all() | file:name_all()) -> string().
filename_to_string(Filename) ->
    lists:flatten(io_lib:format("~s", [Filename])).
