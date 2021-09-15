-module(aeu_plugins).

-export([load_plugins/0]).

-export([validate_config/2]).

validate_config(JSON, SchemaFilename) ->
    [Schema] = jsx:consult(SchemaFilename, [return_maps]),
    validate(JSON, Schema).

validate(JSON, Schema) when is_map(JSON) ->
    jesse:validate_with_schema(Schema, JSON, []).

load_plugins() ->
    case aeu_env:find_config([<<"system">>, <<"plugin_path">>],
                             [user_config, schema_default]) of
        {ok, <<>>} ->
            ok;
        {ok, Path} ->
            Abs = filename:absname(binary_to_list(Path)),
            case filelib:is_dir(Abs) of
                true ->
                    lager:info("Plugin lib dir: ~s", [Abs]),
                    load_plugin_apps(Abs);
                false ->
                    error(plugin_path_enotdir)
            end
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
            FsBins = [list_to_binary(F) || F <- Fs],
            Names -- FsBins;
        {error, Reason} ->
            error({cannot_read_plugin_path, Reason})
    end.

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
