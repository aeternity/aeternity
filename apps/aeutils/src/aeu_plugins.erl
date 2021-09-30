-module(aeu_plugins).

-export([load_plugins/0]).

-export([ check_config/3
        , validate_config/2 ]).

check_config(PluginName, SchemaFilename, OsEnvPrefix) ->
    case aeu_env:find_config([<<"system">>, <<"plugins">>],
                             [user_config, schema_default]) of
        {ok, Objs} ->
            NameBin = bin(PluginName),
            case [Conf || #{<<"name">> := N, <<"config">> := Conf} <- Objs,
                          N == NameBin] of
                [Config] ->
                    Schema = load_schema(SchemaFilename),
                    {ok, Config1} = validate(Config, Schema),
                    case aeu_env:apply_os_env(OsEnvPrefix, Schema, Config1) of
                        no_change ->
                            Config1;
                        {error, E} ->
                            error(E);
                        Config2 when is_map(Config2) ->
                            Config2
                    end;
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


validate_config(JSON, SchemaFilename) ->
    Schema = load_schema(SchemaFilename),
    validate(JSON, Schema).

load_schema(SchemaFilename) ->
    {ok, AppName} = application:get_application(),
    Fname = case filename:pathtype(SchemaFilename) of
                relative ->
                    filename:join(code:priv_dir(AppName), SchemaFilename);
                _Other ->
                    SchemaFilename
            end,
    case filelib:is_regular(Fname) of
        true ->
            lager:info("Reading plugin schema (~p) ~p", [AppName, Fname]),
            [Schema] = jsx:consult(Fname, [return_maps]),
            Schema;
        false ->
            lager:warning("Cannot locate plugin schema (~p) ~p", [AppName, Fname]),
            error(cannot_locate_schema)
    end.

validate(JSON, Schema) when is_map(JSON) ->
    jesse:validate_with_schema(Schema, JSON, []).

load_plugins() ->
    case aeu_env:find_config([<<"system">>, <<"plugin_path">>],
                             [user_config, schema_default]) of
        {ok, <<>>} ->
            ok;
        {ok, Path} ->
            Abs = binary_to_list(maybe_expand_relpath(Path)),
            case filelib:is_dir(Abs) of
                true ->
                    lager:info("Plugin lib dir: ~s", [Abs]),
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
            case names_not_found(NameStrs, Path) of
                [] ->
                    case try_patch_apps(NameStrs, setup:lib_dirs_under_path(Path)) of
                        [] ->
                            ok;
                        Apps ->
                            lager:info("== PLUGINS: ~p ==", [Apps]),
                            app_ctrl:check_for_new_applications(),
                            [maybe_start_application(A) || A <- Apps],
                            ok
                    end;
                Missing ->
                    error({missing_plugins, Missing})
            end
    end.

names_not_found(Names, Dir) ->
    case file:list_dir(Dir) of
        {ok, Fs} ->
            [N || N <- Names,
                  not app_name_is_member(N, Fs)];
        {error, Reason} ->
            error({cannot_read_plugin_path, Reason})
    end.

app_name_is_member(Name, Fs) ->
    %% Regexp copied from setup:is_app_dir/2
    Pat = << Name/binary, "(-[0-9]+(\\..+)?)?" >>,
    lists:any(fun(F) ->
                      re:run(F, Pat, []) =/= nomatch
              end, Fs).

try_patch_apps([Name|Ns], LibDirs) ->
    App = binary_to_atom(Name, utf8),
    case setup:patch_app(App, latest, LibDirs) of
        true ->
            {ok,_} = setup:reload_app(App),
            [App | try_patch_apps(Ns, LibDirs)];
        Other ->
            lager:error("Couldn't add plugin application ~p: ~p", [App, Other]),
            error({could_not_add_plugin_app, App})
    end;
try_patch_apps([], _) ->
    [].


maybe_start_application(App) ->
    case is_runnable(App) of
        true ->
            lager:info("Issuing start command for ~p", [App]),
            spawn(fun() -> ensure_started(App) end),
            ok;
        false ->
            lager:info("~p not runnable", [App]),
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
