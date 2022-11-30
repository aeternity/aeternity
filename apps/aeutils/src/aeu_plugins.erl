-module(aeu_plugins).

-export([load_plugins/0]).

-export([ check_config/3
        , find_config/3
        , validate_config/2
        , is_dev_mode/0
        , suggest_config/2 ]).

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
    Schema = load_schema(SchemaFilename),
    validate(JSON, Schema).

is_dev_mode() ->
    aecore_env:is_dev_mode().

%% Checks if a given config key (list of binary keys corresponding to the AE
%% config schema) is already configured. If not, the suggested value is used.
suggest_config(Key, Value) ->
    aeu_env:suggest_config(Key, Value).

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

cache_plugin_schema(PluginName, Schema) ->
    persistent_term:put({?MODULE, cached_schema, PluginName}, Schema).

cached_plugin_schema(PluginName) ->
    persistent_term:get({?MODULE, cached_schema, PluginName}).

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
                            check_for_missing_deps(Apps),
                            app_ctrl:check_for_new_applications(),
                            [maybe_start_application(A) || A <- Apps],
                            ok
                    end;
                Missing ->
                    error({missing_plugins, Missing})
            end;
        undefined ->
            ok
    end.

check_for_missing_deps(Apps) ->
    lists:foldr(fun(A, Acc) ->
                        check_for_missing_deps_(A) ++ Acc
                end, [], Apps).

check_for_missing_deps_(A) ->
    {ok, Deps} = application:get_key(A, applications),
    Unknown = [D || D <- Deps,
                    application:get_key(D, vsn) =:= undefined],
    case Unknown of
        [] -> ok;
        _ ->
            lager:info("UNKNOWN DEPS of ~p: ~p", [A, Unknown])
    end,
    look_for_neighbors(Unknown, A),
    Unknown.

look_for_neighbors(Unknown, A) ->
    {ok, LibDir} = file:read_link(code:lib_dir(A)),
    AParent = filename:dirname(LibDir),
    PatchRes = try_patch_apps([atom_to_binary(App,utf8) || App <- Unknown],
                              setup:lib_dirs_under_path(AParent)),
    lager:info("Trying to patch unknowns: ~p", [PatchRes]).


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
