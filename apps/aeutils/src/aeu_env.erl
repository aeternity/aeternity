%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Bootstrapping and querying the Epoch environment
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aeu_env).

-compile([export_all, nowarn_export_all]).

-export([get_env/2, get_env/3]).

-export([user_config/0, user_config/1, user_config/2]).
-export([user_map/0, user_map/1]).
-export([schema/0, schema/1, schema/2]).
-export([schema_default/1, schema_default/2]).
-export([schema_default_values/1]).
-export([user_config_or_env/3, user_config_or_env/4]).
-export([user_map_or_env/4]).
-export([env_or_user_map/4]).
-export([config_value/4]).
-export([find_config/2]).
-export([nested_map_get/2]).
-export([read_config/0]).
-export([apply_os_env/0,
         apply_os_env/3]).
-export([check_env/2]).
-export([parse_key_value_string/1]).
-export([data_dir/1]).
-export([check_config/1, check_config/2]).

-export([update_config/1,
         update_config/2,
         update_config/3,
         suggest_config/2]).

-type basic_type() :: number() | binary() | boolean().
-type basic_or_list()  :: basic_type() | [basic_type()].
-type config_tree() :: [{binary(), config_tree() | basic_or_list()}].

-type env_key() :: atom() | list().
-type config_key() :: binary() | [binary()].

%% This function is similar to application:get_env/2, except
%% 1. It uses the setup:get_env/2 function, which supports a number
%%    of useful variable expansions (see the setup documentation)
%% 2. It supports a hierarchical key format, [A,B,...], where each
%%    part of the key represents a level in a tree structure.
%% Example:
%% if get_env(A, a) returns {ok, {a, [{1, foo}, {2, bar}]}}, or
%% {ok, [{a, [{1, foo}, {2, bar}]}]}, then
%% get_env(A, [a,1]) will return {ok, foo}
-spec get_env(atom(), atom() | list()) -> undefined | {ok, any()}.
get_env(App, [H|T]) ->
    case setup:get_env(App, H) of
        {ok, V} ->
            get_env_l(T, V);
        undefined ->
            undefined
    end;
get_env(App, K) when is_atom(K) ->
    setup:get_env(App, K).

get_env(App, K, Default) ->
    case get_env(App, K) of
        {ok, V}   -> V;
        undefined -> Default
    end.

get_env_l([], V) ->
    {ok, V};
get_env_l([H|T], [_|_] = L) ->
    case lists:keyfind(H, 1, L) of
        {_, V} ->
            get_env_l(T, V);
        false ->
            undefined
    end;
get_env_l(_, _) ->
    undefined.

-spec user_config() -> config_tree().
user_config() ->
    setup:get_env(aeutils, '$user_config', []).

-spec user_config(list() | binary()) -> undefined | {ok, any()}.
user_config(Key) when is_list(Key) ->
    find_config(Key, [user_config]);
user_config(Key) when is_binary(Key) ->
    get_env(aeutils, ['$user_config',Key]).

-spec user_config(list() | binary(), any()) -> any().
user_config(Key, Default) ->
    case user_config(Key) of
        undefined   -> Default;
        {ok, Value} -> Value
    end.

-spec user_config_or_env(config_key(), atom(), env_key()) -> {ok, any()} | undefined.
user_config_or_env(CfgKey, App, EnvKey) ->
    case user_config(CfgKey) of
        undefined ->
            get_env(App, EnvKey);
        {ok, _Value} = Result ->
            Result
    end.

-spec user_config_or_env(config_key(), atom(), env_key(), any()) -> any().
user_config_or_env(CfgKey, App, EnvKey, Default) ->
    case user_config(CfgKey) of
        undefined ->
            get_env(App, EnvKey, Default);
        {ok, Value} ->
            Value
    end.

-spec user_map_or_env(config_key(), atom(), env_key(), any()) -> any().
user_map_or_env(CfgKey, App, EnvKey, Default) ->
    case user_map(CfgKey) of
        undefined ->
            get_env(App, EnvKey, Default);
        {ok, Value} ->
            Value
    end.

-spec env_or_user_map(config_key(), atom(), env_key(), any()) -> any().
env_or_user_map(CfgKey, App, EnvKey, Default) ->
    case get_env(App, EnvKey) of
        {ok, V}   -> V;
        undefined ->
            case user_map(CfgKey) of
                undefined ->
                    Default;
                {ok, Value} ->
                    Value
            end
    end.

config_value(CfgKey, App, Env, Default) ->
    {ok, Value} =find_config(CfgKey, [ user_config
                                     , {env, App, Env}
                                     , schema_default
                                     , {value, Default} ]),
    Value.

find_config(CfgKey, [H|T]) ->
    case find_config_(CfgKey, H) of
        undefined -> find_config(CfgKey, T);
        {ok,_} = Ok -> Ok
    end;
find_config(_, []) ->
    undefined.

find_config_(K, user_config       ) -> user_map(K);
find_config_(_, {env, App, EnvKey}) -> get_env(App, EnvKey);
find_config_(K, schema_default    ) -> schema_default(K);
find_config_(_, {value, V}        ) -> {ok, V}.


%% The user_map() functions are equivalent to user_config(), but
%% operate on a tree of maps rather than a tree of {K,V} tuples.
%% Actually, the user_map() data is the original validated object,
%% which is then transformed to the Key-Value tree used in user_config().
-spec user_map() -> map().
user_map() ->
    setup:get_env(aeutils, '$user_map', #{}).

-spec user_map([any()] | any()) -> {ok, any()} | undefined.
user_map(Key) when is_list(Key) ->
    M = user_map(),
    case maps:find(Key, M) of
        {ok, _} = Ok ->
            Ok;
        error ->
            nested_map_get(Key, M)
    end;
user_map(Key) ->
    case maps:find(Key, user_map()) of
        {ok, _} = Ok -> Ok;
        error        -> undefined
    end.

nested_map_get([], V) ->
    {ok, V};
nested_map_get([H|T], M) when is_map(M) ->
    case maps:find(H, M) of
        {ok, M1} when is_map(M1) ->
            nested_map_get(T, M1);
        {ok, V} when T == [] ->
            {ok, V};
        _ ->
            undefined
    end;
nested_map_get([H|T], L) when is_list(L) ->
    case lists_map_key_find(H, L) of
        {ok, V} when is_map(V) ->
            nested_map_get(T, V);
        {ok, V} when T == [] ->
            {ok, V};
        error ->
            undefined
    end.

lists_map_key_find({K,V,Then} = Pat, [#{} = H|T]) ->
    case maps:find(K, H) of
        {ok, V} ->
            maps:find(Then, H);
        error ->
            lists_map_key_find(Pat, T)
    end;
lists_map_key_find(K, [#{} = H|T]) ->
    case maps:find(K, H) of
        {ok, _} = Ok ->
            Ok;
        error ->
            lists_map_key_find(K, T)
    end;
lists_map_key_find(K, [{K, V}|_]) ->
    {ok, V};
lists_map_key_find(_, []) ->
    error.

schema() ->
    case pt_get_schema(undefined) of
        undefined ->
            load_schema(),
            pt_get_schema();
        S ->
            S
    end.

pt_get_schema() ->
    persistent_term:get({?MODULE, '$schema'}).

pt_get_schema(Default) ->
    persistent_term:get({?MODULE, '$schema'}, Default).

pt_set_schema(Schema) ->
    persistent_term:put({?MODULE, '$schema'}, Schema).

schema(Key) ->
    schema(Key, schema()).

schema([H|T], Schema) ->
    case Schema of
        #{<<"$schema">> := _, <<"properties">> := #{H := Tree}} ->
            schema_find(T, Tree);
        #{H := Tree} ->
            schema_find(T, Tree);
        _ ->
            undefined
    end;
schema([], Schema) ->
    {ok, Schema};
schema(Key, Schema) ->
    case maps:find(Key, Schema) of
        {ok, _} = Ok -> Ok;
        error        -> undefined
    end.
schema_find([H|T], S) ->
    case S of
        #{<<"properties">> := #{H := Tree}} ->
            schema_find(T, Tree);
        #{H := Tree} ->
            schema_find(T, Tree);
        _ ->
            undefined
    end;
schema_find([], S) ->
    {ok, S}.

schema_default(Key) when is_list(Key) ->
    schema(Key ++ [<<"default">>]).

schema_default(Key, Schema) when is_list(Key) ->
    schema(Key ++ [<<"default">>], Schema).

schema_default_values(Path) ->
    case schema(Path) of
        undefined -> undefined;
        {ok, Tree} ->
            RecursiveDefault =
                fun R(_PName,
                      #{<<"type">> := <<"object">>, <<"properties">> := Props}) ->
                        maps:map(fun(PN, #{<<"type">> := <<"object">>} = PP) ->
                                         R(PN, PP);
                                    (PN, #{<<"type">> := <<"array">>, <<"items">> := Items}) ->
                                         [R(PN, Items)];
                                    (_PN, #{<<"default">> := Def}) -> Def;
                                    (_PN, _) -> undefined
                                 end, Props);
                    R(PName,
                     #{<<"type">> := <<"array">>, <<"items">> := Items}) ->
                        [R(PName, Items)];
                    R(_PName, #{<<"default">> := Def}) ->
                        Def;
                    R(_PName, _) ->
                        undefined
                end,
            Res = RecursiveDefault(<<"root">>, Tree),
            {ok, Res}
      end.

parse_key_value_string(Bin) when is_binary(Bin) ->
    %% Parse: expect (binary) string of type "S1:L1 [, ...] Sn:Ln", where
    %% Sx is a string or non-negative integer, and Lx is a non-neg integer;
    %% allow for whitespace."
    Ls = [{opt_bin_to_integer(A), opt_bin_to_integer(B)}
          || [A, B] <-
                 [re:split(B, <<"\\h*:\\h*">>, [{return,binary}])
                  || B <- re:split(Bin, <<"\\h*,\\h*">>, [{return, binary}])]],
    true = lists:all(fun valid_kv_pair/1, Ls),
    Ls.

opt_bin_to_integer(B) ->
    try binary_to_integer(B)
    catch
        error:_ ->
            B
    end.

valid_kv_pair({A,B}) when is_integer(A), A >= 0;
                             is_binary(A) ->
    is_integer(B) andalso B >= 0;
valid_kv_pair(_) ->
    false.


read_config() ->
    read_config(report).

read_config(Mode) when Mode =:= check; Mode =:= silent; Mode =:= report ->
    case config_file() of
        undefined ->
            info_msg(Mode, "No config file specified; using default settings~n", []),
            ok;
        F ->
            info_msg(Mode, "Reading config file ~s~n", [F]),
            do_read_config(F, schema_filename(), store, Mode)
    end.

apply_os_env() ->
    ok = application:ensure_started(gproc),
    Pfx = "AE",  %% TODO: make configurable
    ConfigMap = application:get_env(aeutils, '$user_map', #{}),
    case apply_os_env(Pfx, schema(), ConfigMap) of
        NewConfig when is_map(NewConfig) ->
            cache_config(NewConfig),
            notify_update_config(NewConfig),
            NewConfig;
        Other ->
            Other
    end.

%% Plugin API version, using plugin schema and config.
%% The plugin API might decide to publish a specific event...
apply_os_env(Pfx, Schema, ConfigMap) ->
    %% We sort on variable names to allow specific values to override object
    %% definitions at a higher level (e.g. AE__MEMPOOL followed by AE__MEMPOOL__TX_TTL)
    %% Note that all schema name parts are converted to uppercase.
    try
    Names = lists:keysort(1, schema_key_names(Pfx, Schema)),
    error_logger:info_msg("OS env config: ~p~n", [Names]),
    Map = lists:foldl(
            fun({_Name, Key, Value}, Acc) ->
                    Value1 = coerce_type(Key, Value, Schema),
                    update_map(to_map(Key, Value1), Acc)
            end, #{}, Names),
    error_logger:info_msg("Map for OS env config: ~p~n", [Map]),
    if map_size(Map) > 0 ->
            update_config_(Map, ConfigMap, Schema, report);
       true ->
            no_change
    end
    catch
        error:E:ST ->
            error_logger:info_msg("CAUGHT error:~p / ~p~n", [E, ST]),
            error(E)
    end.

to_map(K, V) ->
    to_map(K, V, #{}).

to_map([K], Val, M) ->
    M#{K => Val};
to_map([H|T], Val, M) ->
    SubMap = maps:get(H, M, #{}),
    M#{H => to_map(T, Val, SubMap)}.

coerce_type(Key, Value, Schema) ->
    case schema(Key, Schema) of
        {ok, #{<<"type">> := Type}} ->
            case Type of
                <<"integer">> -> to_integer(Value);
                <<"string">>  -> to_string(Value);
                <<"boolean">> -> to_bool(Value);
                <<"array">>   -> jsx:decode(list_to_binary(Value), [return_maps]);
                <<"object">>  -> jsx:decode(list_to_binary(Value), [return_maps])
            end;
        _ ->
            error({unknown_key, Key})
    end.

to_integer(I) when is_integer(I) -> I;
to_integer(L) when is_list(L)    -> list_to_integer(L);
to_integer(B) when is_binary(B)  -> binary_to_integer(B).

to_string(L) when is_list(L)   -> list_to_binary(L);
to_string(B) when is_binary(B) -> B.

to_bool("true")  -> true;
to_bool("false") -> false;
to_bool(B) when is_boolean(B) ->
    B;
to_bool(Other) ->
    error({expected_boolean, Other}).

schema_key_names(Prefix) ->
    schema_key_names(Prefix, schema()).

schema_key_names(Prefix, Schema) ->
    case Schema of
        #{<<"$schema">> := _, <<"properties">> := Props} ->
            schema_key_names(Prefix, [], Props, []);
        _ ->
            []
    end.

schema_key_names(NamePfx, KeyPfx, Map, Acc0) when is_map(Map) ->
    maps:fold(
      fun(SubKey, SubMap, Acc) ->
              NamePfx1 = NamePfx ++ "__" ++ string:to_upper(binary_to_list(SubKey)),
              KeyPfx1 = KeyPfx ++ [SubKey],
              EnvKey = unhyphenate(NamePfx1),
              Acc1 = case os:getenv(EnvKey) of
                         false -> Acc;
                         Value ->
                             [{EnvKey, KeyPfx1, Value} | Acc]
                     end,
              case maps:find(<<"properties">>, SubMap) of
                  error ->
                      Acc1;
                  {ok, Props} ->
                      schema_key_names(NamePfx1, KeyPfx1, Props, Acc1)
              end
      end, Acc0, Map).

%% Unfortunately, the config schema contains some names with hyphens in them.
%% Since hyphens aren't allowed in OS environment names, we replace them with underscores.
%% This should be safe, since we DO NOT stupidly have names that differ only on '-' v '_'.
unhyphenate(Str) ->
    re:replace(Str, <<"\\-">>, <<"_">>, [global, {return, list}]).

%% ======================================================================

check_env(App, Spec) ->
    {ok, MMode} = aeu_env:find_config([<<"system">>, <<"maintenance_mode">>],
                                      [user_config, schema_default]),
    lists:foreach(
      fun({K, F}) ->
              case aeu_env:user_config(K) of
                  undefined -> ignore;
                  {ok, V}   -> maybe_set_env(F, V, App, MMode)
              end
      end, Spec).

maybe_set_env({set_env, K}, V, App, _MMode) when is_atom(K) ->
    lager:debug("setenv A=~p: K=~p, V=~p~n", [App, K, V]),
    application:set_env(App, K, V);
maybe_set_env(F, V, _, _MMode) when is_function(F, 1) ->
    F(V);
maybe_set_env(F, V, _, MMode) when is_function(F, 2) ->
    F(V, MMode).

%% ======================================================================


check_config(F) ->
    do_read_config(F, schema_filename(), check, check).

check_config(F, Schema) ->
    do_read_config(F, Schema, check, check).

data_dir(Name) when is_atom(Name) ->
    filename:join([setup:data_dir(), Name]).

config_file() ->
    case command_line_config_file() of
        undefined ->
            case default_config_file()  of
                undefined -> deprecated_config_file();
                F         -> F
            end;
        F -> F
    end.

command_line_config_file() ->
    case init:get_argument('-config') of
        {ok, [[F]]} -> F;
        _ -> undefined
    end.

default_config_file() ->
    case os:getenv("AETERNITY_CONFIG") of
        false ->
            case setup:get_env(aecore, config) of
                {ok, F} -> F;
                _       -> search_default_config()
            end;
        F ->
            F
    end.

deprecated_config_file() ->
    case os:getenv("EPOCH_CONFIG") of
        false -> search_deprecated_config();
        F     -> F
    end.

search_default_config() ->
    Dirs = [filename:join([os:getenv("HOME"), ".aeternity", "aeternity"]),
            setup:home()],
    search_config(Dirs, "aeternity.{json,yaml}").

search_deprecated_config() ->
    Dirs = [filename:join([os:getenv("HOME"), ".epoch", "epoch"]),
            setup:home()],
    search_config(Dirs, "epoch.{json,yaml}").

search_config(Dirs, FileWildcard) ->
    lists:foldl(
      fun(D, undefined) ->
              error_logger:info_msg("Searching for config file ~s "
                                    "in directory ~s~n", [FileWildcard, D]),
              case filelib:wildcard(FileWildcard, D) of
                  [] -> undefined;
                  [F|_] -> filename:join(D, F)
              end;
         (_, Acc) -> Acc
      end, undefined, Dirs).

do_read_config(F, Schema, Action, Mode) ->
    case {filename:extension(F), Action} of
        {".json", store} -> store(read_json(F, Schema, Mode));
        {".yaml", store} -> store(read_yaml(F, Schema, Mode));
        {".json", check} -> check_config_(catch read_json(F, Schema, Mode));
        {".yaml", check} -> check_config_(catch read_yaml(F, Schema, Mode))
    end.

store([Vars0]) when is_map(Vars0) ->
    cache_config(Vars0).

cache_config(ConfigMap) when is_map(ConfigMap) ->
    set_env(aeutils, '$user_map', ConfigMap),
    ConfigTree = to_tree(ConfigMap),
    set_env(aeutils, '$user_config', ConfigTree),
    ok.

check_config_({yamerl_exception, _StackTrace} = Error) ->
    {error, Error};
check_config_({'EXIT', Reason}) ->
    ShortError = pp_error(Reason),
    {error, ShortError};
check_config_([Vars]) ->
    {ok, {Vars, to_tree(Vars)}}.

pp_error({validation_failed, _}) ->
    validation_failed;
pp_error(Other) ->
    Other.

pp_error_({error, {schema_file_not_found, Schema}}, Mode) ->
    error_format("Schema not found : ~s", [Schema], Mode);
pp_error_({error, [{data_invalid, Schema, Type, Value, Pos}]}, Mode) ->
    SchemaStr = schema_string(Schema, Mode),
    PosStr = pp_pos(Pos),
    ValStr = pp_val(Value),
    TypeStr = pp_type(Type),
    case Mode of
        check ->
            io:format("Validation failed~n"
                      "Position: ~s~n"
                      "Value   : ~s~n"
                      "Schema  :~n~s~n"
                      "Reason  : ~s~n",
                      [PosStr, ValStr, SchemaStr, TypeStr]);
        report ->
            error_format("Validation failed, Pos: ~s~n"
                         "Value: ~s~n"
                         "Schema: ~s~n"
                         "Reason: ~s", [PosStr, ValStr, SchemaStr, TypeStr], Mode)
    end;
pp_error_({error, [{schema_invalid, Section, Description}]}, Mode) ->
    SchemaStr = schema_string(Section, Mode),
    error_format("Reading schema failed~n"
                 "Section: ~n~s~n"
                 "Reason: ~p~n", [SchemaStr, Description], Mode).

silent_as_report(silent) -> report;
silent_as_report(Mode  ) -> Mode.

schema_string(Schema, Mode) ->
    JSONSchema = jsx:encode(Schema),
    case Mode of
        check  -> jsx:prettify(JSONSchema);
        report -> jsx:minify(JSONSchema)
    end.

error_format(Fmt, Args, check) ->
    io:format(Fmt, Args);
error_format(Fmt, Args, report) ->
    Str = io_lib:format(Fmt, Args),
    Parts = re:split(Str, <<"\n">>),
    Out = iolist_to_binary(
            [hd(Parts) |
             [[" | ", P] || P <- tl(Parts)]]),
    Out = re:replace(Str, <<"\n">>, <<" | ">>, [global, {return, binary}]),
    lager:error("~s", [Out]).

pp_pos([A,B|T]) when is_integer(B) ->
    [pp_pos_(A), pp_pos_(B) | pp_pos(T)];
pp_pos([A,B|T]) when is_binary(B) ->
    [pp_pos_(A), "/", pp_pos_(B) | pp_pos(T)];
pp_pos([H|T]) ->
    [pp_pos_(H) | pp_pos(T)];
pp_pos([]) ->
    [].

pp_pos_(I) when is_integer(I) -> ["[", integer_to_list(I), "]"];
pp_pos_(S) when is_binary(S)  -> S.

pp_val(I) when is_integer(I) -> integer_to_list(I);
pp_val(S) when is_binary(S)  -> ["\"", S, "\""];
pp_val(X) ->
    io_lib:fwrite("~w", [X]).

pp_type(data_invalid)     -> "Data invalid";
pp_type(missing_id_field) -> "Missing ID field";
pp_type(missing_required_property) -> "Missing required property";
pp_type(no_match  )       -> "No match";
pp_type(no_extra_properties_allowed) -> "No extra properties allowed";
pp_type(no_extra_items_allowed)      -> "No extra items allowed";
pp_type(not_allowed)      -> "Not allowed";
pp_type(not_unique)       -> "Not unique";
pp_type(not_in_enum)      -> "Not in enum";
pp_type(not_in_range)     -> "Not in range";
pp_type(not_divisible)    -> "Not divisible";
pp_type(wrong_type)       -> "Wrong type";
pp_type(wrong_size)       -> "Wrong size";
pp_type(wrong_length)     -> "Wrong length";
pp_type(wrong_format)     -> "Wrong format";
pp_type(too_many_properties)   -> "Too many properties";
pp_type(too_few_properties)    -> "Too few properties";
pp_type(all_schemas_not_valid) -> "The 'allOf' requirement is not upheld";
pp_type(any_schemas_not_valid) -> "The 'anyOf' requirement is not upheld";
pp_type(not_multiple_of)       -> "Not an instance of 'multipleOf'";
pp_type(not_one_schema_valid)  -> "The 'oneOf' requirement is not upheld";
pp_type(not_schema_valid)      -> "The 'not' requirement is not upheld";
pp_type(wrong_not_schema)      -> "Wrong not schema";
pp_type(external)              -> "External";
pp_type(Other)            ->  io_lib:fwrite("~w", [Other]).

to_tree(Vars) ->
    to_tree_(expand_maps(Vars)).

expand_maps(M) when is_map(M) ->
    [{K, expand_maps(V)} || {K, V} <- maps:to_list(M)];
expand_maps(L) when is_list(L) ->
    [expand_maps(E) || E <- L];
expand_maps(E) ->
    E.

to_tree_(L) when is_list(L) ->
    lists:flatten([to_tree_(E) || E <- L]);
to_tree_({K, V}) ->
    {K, to_tree_(V)};
to_tree_(E) ->
    E.

lst(L) when is_list(L) -> L;
lst(E) -> [E].

update_config(Map) when is_map(Map) ->
    update_config(Map, _Notify = true).

update_config(Map, Notify) ->
    update_config(Map, Notify, report).

update_config(Map, Notify, Mode) when is_map(Map), is_boolean(Notify) ->
    Schema = application:get_env(aeutils, '$schema', #{}),
    ConfigMap = application:get_env(aeutils, '$user_map', #{}),
    ConfigMap1 = update_config_(Map, ConfigMap, Schema, Mode),
    cache_config(ConfigMap1),
    if Notify ->
            notify_update_config(Map);
       true ->
            ok
    end,
    ok.

%% Checks if a given config key (list of binary keys corresponding to the AE
%% config schema) is already configured. If not, the suggested value is used.
suggest_config(Key, Value) ->
    case user_config(Key) of
        {ok, _} ->
            {error, already_configured};
        undefined ->
            Map = kv_to_config_map(Key, Value),
            update_config(Map, false),
            ok
    end.

kv_to_config_map([H], V) ->
    #{H => V};
kv_to_config_map([H|T], V) ->
    #{H => kv_to_config_map(T, V)}.

notify_update_config(Map) ->
    aec_events:publish(update_config, Map).

update_config_(Map, ConfigMap, Schema, Mode) when Mode =:= check;
                                                  Mode =:= silent;
                                                  Mode =:= report ->
    check_validation([jesse:validate_with_schema(Schema, Map, [])],
                     [Map], update_config, Mode),
    NewConfig = update_map(Map, ConfigMap),
    NewConfig.


update_map(With, Map) when is_map(With), is_map(Map) ->
    maps:fold(
      fun(K, V, Acc) ->
              case maps:find(K, Acc) of
                  {ok, Submap} when is_map(Submap) ->
                      Acc#{K => update_map(V, Submap)};
                  {ok, _} ->
                      Acc#{K => V};
                  error ->
                      Acc#{K => V}
              end
      end, Map, With).

set_env(App, K, V) ->
    application:set_env(App, K, V).

read_json(F, Schema, Mode) ->
    validate(
      try_decode(F, fun(F1) ->
                            jsx:consult(F1, [return_maps])
                    end, "JSON", Mode), F, Schema, Mode).

interpret_json(L, F) when is_list(L) ->
    lists:flatten([interpret_json(Elem, F) || Elem <- L]);
interpret_json({K, V}, F) when is_list(V) ->
    #{K => interpret_json(V, F)};
interpret_json({K, V}, _) ->
    #{K => V}.

read_yaml(F, Schema, Mode) ->
    validate(
      try_decode(
        F,
        fun(F1) ->
                yamerl:decode_file(F1, [{str_node_as_binary, true},
                                        {map_node_format, map}])
        end, "YAML", Mode),
      F, Schema, Mode).

try_decode(F, DecF, Fmt, Mode) ->
    try DecF(F)
    catch
        error:E ->
            error_msg(Mode, "Error reading ~s file: ~s~n", [Fmt, F]),
            erlang:error(E)
    end.

validate(JSON, F) ->
    validate(JSON, F, report).
validate(JSON, F, Mode) ->
    validate(JSON, F, schema_filename(), Mode).

validate(JSON, F, Schema, Mode) when is_list(JSON) ->
    check_validation([validate_(Schema, J) || J <- JSON], JSON, F, Mode);
validate(JSON, F, Schema, Mode) when is_map(JSON) ->
    validate([JSON], F, Schema, Mode).

vinfo(Mode, Res, F) ->
    case Res of
        {ok, _} ->
            info_report(Mode, [{validation, F},
                               {result, Res}]);
        {error, Errors} ->
            Mode1 = silent_as_report(Mode),
            [pp_error_(E, Mode1) || E <- Errors],
            ok
    end.

info_report(report, Info) ->
    error_logger:info_report(Info);
info_report(_, _) ->
    ok.

info_msg(report, Fmt, Args) ->
    error_logger:info_msg(Fmt, Args);
info_msg(_, _,_ ) ->
    ok.

error_msg(report, Fmt, Args) ->
    error_logger:error_msg(Fmt, Args);
error_msg(_, _, _) ->
    ok.

check_validation(Res, JSON, F) ->
    check_validation(Res, JSON, F, report).

check_validation(Res, _JSON, F, Mode) ->
    case lists:foldr(
           fun({ok, M}, {Ok,Err}) when is_map(M) ->
                   {[M|Ok], Err};
              (Other, {Ok,Err}) ->
                   {Ok, [Other|Err]}
           end, {[], []}, Res) of
        {Ok, []} ->
            vinfo(Mode, {ok, Ok}, F),
            Ok;
        {_, Errors} ->
            vinfo(Mode, {error, Errors}, F),
            erlang:error(validation_failed)
    end.

validate_(Schema, JSON) ->
    case filelib:is_regular(Schema) of
        true ->
            jesse:validate_with_schema(load_schema(Schema), JSON, []);
        false ->
            {error, {schema_file_not_found, Schema}}
    end.

schema_filename() ->
    filename:join(code:priv_dir(aeutils),
                  "aeternity_config_schema.json").

load_schema() ->
    load_schema(schema_filename()).

load_schema(F) ->
    [Schema] = jsx:consult(F, [return_maps]),
    pt_set_schema(Schema),
    Schema.
