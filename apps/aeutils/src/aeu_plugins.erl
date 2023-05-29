-module(aeu_plugins).

-export([load_plugins/0]).

-export([ check_config/3
        , find_config/3
        , validate_config/2
        , is_dev_mode/0
        , suggest_config/2 ]).

-export([ data_dir/1
        , log_dir/1 ]).

check_config(PluginName, SchemaFilename, OsEnvPrefix) ->
    case aeu_env:find_config([<<"system">>, <<"plugins">>],
                             [user_config, schema_default]) of
        {ok, Objs} ->
            NameBin = bin(PluginName),
            case [maps:get(<<"config">>, Obj, #{}) || #{<<"name">> := N} = Obj <- Objs,
                          N == NameBin] of
                [Config] ->
                    Schema = load_schema(NameBin, SchemaFilename),
                    {ok, Config1} = validate(Config, Schema),
                    Result = case aeu_env:apply_os_env(OsEnvPrefix, Schema, Config1) of
                                 no_change ->
                                     Config1;
                                 {error, E} ->
                                     error(E);
                                 Config2 when is_map(Config2) ->
                                     Config2
                             end,
                    cache_plugin_schema(NameBin, Schema),
                    Result;
                [] ->
                    lager:warning("Could not fetch plugin config object (~p)",
                                  [PluginName]),
                    not_found
            end;
        _ ->
            lager:warning("Could not fetch plugin config object (~p)",
                          [PluginName]),
            not_found
    end.

data_dir(PluginName) ->
    filename:join(setup:data_dir(), bin(PluginName)).

log_dir(PluginName) ->
    filename:join(setup:log_dir(), bin(PluginName)).

%% Look up a user config value for a specific plugin.
%% Key is a list of binaries, corresponding to the plugin config schema
%% For top-level config variables, this would be a list of length 1, Example:
%%
%% find_config(<<"aeplugin_dev_mode">>,
%%             [<<"microblock_interval">>],
%%             [user_config, schema_default])
%%
%% Path is a list of lookup types, in priority order:
%% * user_config - check for user-provided config data
%% * schema_default - If a default value is configured in the config schema
%% * {env, Application, Key} - Application environment value, if configured
%% * {value, V} - A (constant) default value.
%%
find_config(PluginName, Key, Path) when is_list(Key), is_list(Path) ->
    PluginNameBin = bin(PluginName),
    find_config_(PluginNameBin, Key, Path).

find_config_(PluginName, Key, [H|T]) ->
    case find_config_step(PluginName, Key, H) of
        undefined -> find_config_(PluginName, Key, T);
        {ok, _} = Ok -> Ok
    end;
find_config_(_, _, []) ->
    undefined.

find_config_step(PluginName, Key, user_config) ->
    aeu_env:find_config([<<"system">>, <<"plugins">>,
                         {<<"name">>, PluginName, <<"config">>} | Key], [user_config]);
find_config_step(PluginName, Key, schema_default) ->
    aeu_env:schema_default(Key, cached_plugin_schema(PluginName));
find_config_step(_, Key, Step) ->
    aeu_env:find_config(Key, [Step]).

validate_config(JSON, SchemaFilename) ->
    {ok, AppName} = application:get_application(),
    Schema = load_schema_(AppName, SchemaFilename),
    validate(JSON, Schema).

is_dev_mode() ->
    aecore_env:is_dev_mode().

%% Checks if a given config key (list of binary keys corresponding to the AE
%% config schema) is already configured. If not, the suggested value is used.
suggest_config(Key, Value) ->
    aeu_env:suggest_config(Key, Value).

load_schema(Plugin, SchemaFilename) ->
    %% If loaded, the Plugin name exists in atom form (the application name)
    AppName = binary_to_existing_atom(Plugin, utf8),
    load_schema_(AppName, SchemaFilename).

load_schema_(AppName, SchemaFilename) ->
    Fname = case filename:pathtype(SchemaFilename) of
                relative ->
                    filename:join(code:priv_dir(AppName), SchemaFilename);
                _Other ->
                    SchemaFilename
            end,
    case filelib:is_regular(Fname) of
        true ->
            [Schema] = jsx:consult(Fname, [return_maps]),
            %% Schema = jsx_read_and_decode(Fname, [return_maps]),
            Schema;
        false ->
            lager:warning("Cannot locate plugin schema (~p) ~p", [AppName, Fname]),
            error(cannot_locate_schema)
    end.

validate(JSON, Schema) when is_map(JSON) ->
    jesse:validate_with_schema(Schema, JSON, []).

cache_plugin_schema(PluginName, Schema) ->
    persistent_term:put({?MODULE, cached_schema, PluginName}, Schema).

cached_plugin_schema(PluginName) ->
    persistent_term:get({?MODULE, cached_schema, PluginName}, #{}).

load_plugins() ->
    case aeu_env:find_config([<<"system">>, <<"plugin_path">>],
                             [user_config, schema_default]) of
        {ok, <<>>} ->
            ok;
        {ok, Path} ->
            Abs = binary_to_list(maybe_expand_relpath(Path)),
            case filelib:is_dir(Abs) of
                true ->
                    load_plugin_apps(Abs);
                false ->
                    lager:info("Plugin dir doesn't exist: ~s", [Abs]),
                    no_plugin_lib_dir
            end
    end.

maybe_expand_relpath(Path) ->
    case filename:pathtype(Path) of
        relative ->
            Abs = filename:absname(Path),
            aeu_env:update_config(#{<<"system">> =>
                                        #{<<"plugin_path">> => Abs}}, false),
            Abs;
        _ ->
            Path
    end.

load_plugin_apps(Path) ->
    case aeu_env:find_config([<<"system">>, <<"plugins">>],
                             [user_config, schema_default]) of
        {ok, []} ->
            ok;
        {ok, Objs} ->
            lager:info("Plugin Objs = ~p", [Objs]),
            NameStrs = [Name || #{<<"name">> := Name} <- Objs],
            ensure_empty_lib_subdir(Path),
            LibDirs = setup:lib_dirs_under_path(Path),
            SelectedDirs = select_dirs(LibDirs, NameStrs),
            FinalLibDirs = final_lib_dirs(Path, SelectedDirs),
            case try_patch_apps(FinalLibDirs) of
                {[], []} ->
                    ok;
                {Apps, []} ->
                    lager:info("== PLUGINS + deps: ~p ==", [Apps]),
                    app_ctrl:check_for_new_applications(),
                    [maybe_start_application(A) || A <- Apps], % will include deps
                    ok;
                {_, Bad} ->
                    error({cannot_load_plugins, Bad})
            end;
        undefined ->
            ok
    end.

ensure_empty_lib_subdir(Path) ->
    Lib = filename:join(Path, "lib"),
    case filelib:is_dir(Lib) of
        true ->
            delete_all_under(Lib);
        false ->
            case filelib:ensure_dir(filename:join(Lib, "foo")) of
                ok ->
                    ok;
                Err ->
                    erlang:error({cannot_create_lib_subdir, Err})
            end
    end.

delete_all_under(Dir) ->
    case file:list_dir(Dir) of
        {ok, Fs} ->
            lists:foreach(
              fun(F) ->
                      %% this handles both dirs and regular files, recursively
                      ok = file:del_dir_r(filename:join(Dir, F))
              end, Fs);
        {error, _} = Err ->
            error({cannot_read_dir, Dir, Err})
    end.

final_lib_dirs(Root, Dirs) ->
    Lib = filename:join(Root, "lib"),
    populate_lib_dir(Dirs, Root, Lib).

populate_lib_dir(Dirs, Root, Lib) ->
    RootNParts = length(filename:split(Root)),
    ArchiveExt = init:archive_extension(),
    lists:foldr(
      fun({N,D,_}, Acc) ->
              populate_lib_dir_(N, D, RootNParts, Root, Lib, ArchiveExt, Acc)
      end,
      [], Dirs).

populate_lib_dir_(N, Dir, RootN, Root, Lib, AExt, Acc) ->
    DirParts = filename:split(Dir),
    Rel = filename:join(lists:nthtail(RootN, DirParts)),
    AppDir = app_dir(Dir),
    case is_archive_path(Rel, AExt) of
        true ->
            ArchiveF = filename:join(Root, hd(filename:split(Rel))),
            zip:foldl(fun(F, _Info, Bin, Acc1) ->
                              case lists:member(AppDir, filename:split(F)) of
                                  true ->
                                      OutF = filename:join(Lib, F),
                                      filelib:ensure_dir(OutF),
                                      Res = file:write_file(OutF, Bin()),
                                      [{F, Res} | Acc1];
                                  false ->
                                      Acc1
                              end
                      end, [], ArchiveF),
            [{N, filename:join([Lib, AppDir, "ebin"])}|Acc];
        false ->
            {AppName, AppVsn} =
                case app_name(Dir) of
                    {A, undefined} ->
                        {A, get_app_vsn(A, Dir)};
                    AppAndVsn ->
                        AppAndVsn
                end,
            Tgt = filename:join(Lib, binary_to_list(bin([AppName, "-", AppVsn]))),
            make_symlink(filename:dirname(Dir), Tgt),
            [{N, filename:join(Tgt, "ebin")} | Acc]
    end.

is_archive_path(F, Ext) ->
    case re:run(F, [$\\|Ext] ++ "/", []) of
        {match,_} ->
            true;
        nomatch ->
            false
    end.

make_symlink(Existing, New) ->
    case file:make_symlink(Existing, New) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        {error, _} = Error ->
            lager:warning("Symlink ~s -> ~s failed: ~p", [New, Existing, Error]),
            Error
    end.

get_app_vsn(Name, Ebin) ->
    {ok, [{application,_,Opts}]} =
        file:consult(filename:join(Ebin, binary_to_list(Name) ++ ".app")),
    proplists:get_value(vsn, Opts).

select_dirs(Fs, Names) ->
    %% Find all top-level plugins, and their dependencies
    %% Ignore dependencies that are already in the system
    Sel1 = select_dirs_(Fs, Names),
    AllDeps = lists:usort(lists:append([Ds || {_,_,Ds} <- Sel1])),
    AllLoadedApps = [A || {A,_,_} <- application:loaded_applications()],
    NewDeps = lists:foldr(
                fun(Da, Acc) ->
                        Db = atom_to_binary(Da),
                        case (lists:member(Db, Names) orelse
                              lists:member(Da, AllLoadedApps)) of
                            true ->
                                Acc;
                            false ->
                                [Db|Acc]
                        end
                end, [], AllDeps),
    DepsDirs = select_dirs_(Fs, NewDeps),
    case NewDeps -- [D || {D,_,_} <- DepsDirs] of
        [] ->
            Sel1 ++ DepsDirs;
        Missing ->
            error({missing_deps, Missing})
    end.

select_dirs_(Fs, Names) ->
    lists:foldr(
      fun(F, Acc) ->
              select_dir_(F, Names, Acc)
      end, [], Fs).

select_dir_(F, Names, Acc) ->
    {App, _} = app_name(F),
    case lists:member(App, Names) of
        true ->
            [{App, F, deps_of_app(App, F)} | Acc];
        false ->
            Acc
    end.

app_dir(F) ->
    ["ebin", AppDir | _] = lists:reverse(filename:split(F)),
    AppDir.

app_name(F) ->
    ["ebin", AppDir | _] = lists:reverse(filename:split(F)),
    case re:split(AppDir, "-", [{return, binary}]) of
        [A] ->
            {A, undefined};
        [A, Vsn] ->
            {A, Vsn}
    end.

deps_of_app(AppBin, F) ->
    AppF = filename:join(F, <<AppBin/binary, ".app">>),
    case setup_file:consult(AppF) of
        {ok, [{application, _, Opts}]} ->
            proplists:get_value(applications, Opts, []);
        Other ->
            error({could_not_read_app_file, AppF, Other})
    end.

try_patch_apps(LibDirs) ->
    lager:debug("LibDirs = ~p", [LibDirs]),
    try_patch_apps(LibDirs, [], []).

try_patch_apps([{Name, LibDir}|LibDirs], Good, Bad) ->
    App = binary_to_atom(Name, utf8),
    lager:debug("Try patching app ~p using LibDir = ~p", [App,LibDir]),
    case setup:patch_app(App, latest, [LibDir]) of
        true ->
            {ok,_} = setup:reload_app(App),
            lager:info("loaded app ~p", [App]),
            try_patch_apps(LibDirs, [App|Good], Bad);
        Other ->
            lager:error("Couldn't add plugin application ~p: ~p", [App, Other]),
            try_patch_apps(LibDirs, Good, [App|Bad])
    end;
try_patch_apps([], Good, Bad) ->
    {lists:reverse(Good), lists:reverse(Bad)}.


maybe_start_application(App) ->
    case is_runnable(App) of
        true ->
            lager:info("Issuing start command for ~p", [App]),
            spawn(fun() -> ensure_started(App) end),
            ok;
        false ->
            ok
    end.
	                
is_runnable(App) ->
    case application:get_key(App, mod) of
        {ok, {_, _}} ->
            true;
        _ ->
            false
    end.

ensure_started(App) ->
    Res = application:ensure_all_started(App),
    lager:info("ensure_started(~p) -> ~p", [App, Res]).

bin(B) when is_binary(B) ->
    B;
bin(Str) ->
    iolist_to_binary(Str).
