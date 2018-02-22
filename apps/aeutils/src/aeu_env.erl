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

-export([user_config/0, user_config/1]).
-export([user_map/0, user_map/1]).
-export([user_config_or_env/4]).
-export([read_config/0]).
-export([data_dir/1]).
-export([check_config/1, check_config/2]).

-type http_uri_port() :: pos_integer(). %% https://github.com/erlang/otp/blob/OTP-20.2.3/lib/inets/doc/src/http_uri.xml#L66
-type http_uri_host() :: string() | unicode:unicode_binary(). %% From https://github.com/erlang/otp/blob/OTP-20.2.3/lib/inets/doc/src/http_uri.xml#L64

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
    get_env(aeutils, ['$user_config'|Key]);
user_config(Key) when is_binary(Key) ->
    get_env(aeutils, ['$user_config',Key]).

-spec user_config_or_env(config_key(), atom(), env_key(), any()) -> any().
user_config_or_env(CfgKey, App, EnvKey, Default) ->
    case user_config(CfgKey) of
        undefined ->
            get_env(App, EnvKey, Default);
        {ok, Value} ->
            Value
    end.

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
            user_map_l(Key, M)
    end;
user_map(Key) ->
    case maps:find(Key, user_map()) of
        {ok, _} = Ok -> Ok;
        error        -> undefined
    end.

user_map_l([], V) ->
    {ok, V};
user_map_l([H|T], M) when is_map(M) ->
    case maps:find(H, M) of
        {ok, M1} ->
            user_map_l(T, M1);
        error ->
            undefined
    end;
user_map_l([H|T], L) when is_list(L) ->
    case lists_map_key_find(H, L) of
        {ok, V} ->
            user_map_l(T, V);
        error ->
            undefined
    end.

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


read_config() ->
    read_config(report).

read_config(Mode) when Mode =:= silent; Mode =:= report ->
    case config_file() of
        undefined ->
            info_msg(
              Mode,
              "No config file specified; using default settings~n", []),
            ok;
        F ->
            info_msg(Mode, "Reading config file ~s~n", [F]),
            do_read_config(F, schema(), store, Mode)
    end.

check_config(F) ->
    do_read_config(F, schema(), check, silent).

check_config(F, Schema) ->
    do_read_config(F, Schema, check, silent).

config_file() ->
    case os:getenv("EPOCH_CONFIG") of
        false ->
            case setup:get_env(aecore, config) of
                {ok, F} ->
                    F;
                _ ->
                    search_default_config()
            end;
        F ->
            F
    end.

data_dir(Name) when is_atom(Name) ->
  filename:join([setup:data_dir(), Name]).

search_default_config() ->
    Dirs = [filename:join([os:getenv("HOME"), ".epoch", nodename()]),
            setup:home()],
    lists:foldl(
      fun(D, undefined) ->
              W = "epoch.{json,yaml}",
              error_logger:info_msg("Searching for default config file ~s "
                                    "in directory ~s~n", [W, D]),
              case filelib:wildcard(W, D) of
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

nodename() ->
    case node() of
        nonode@nohost ->
            [];
        N ->
            hd(re:split(atom_to_list(N), "@", [{return,list}]))
    end.

store([Vars0]) ->
    Vars = to_tree(Vars0),
    set_env(aeutils, '$user_config', Vars),
    set_env(aeutils, '$user_map', Vars0).

check_config_({'EXIT', Reason}) ->
    ShortError = pp_error(Reason),
    {error, ShortError};
check_config_([Vars]) ->
    {ok, to_tree_(expand_maps(Vars))}.

pp_error({{validation_failed, Errors}, _}) ->
    [pp_error_(E) || E <- Errors],
    validation_failed;
pp_error(Other) ->
    Other.

pp_error_({error, [{data_invalid, Schema, Type, Value, Pos}]}) ->
    SchemaStr = jsx:prettify(jsx:encode(Schema)),
    PosStr = pp_pos(Pos),
    ValStr = pp_val(Value),
    TypeStr = pp_type(Type),
    io:fwrite("Validation failed~n"
              "Position: ~s~n"
              "Value   : ~s~n"
              "Schema  :~n~s~n"
              "Reason  : ~s~n", [PosStr, ValStr, SchemaStr, TypeStr]).

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

set_env(App, K, V) ->
    error_logger:info_msg("Set config (~p): ~p = ~p~n", [App, K, V]),
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
        normalize_yaml(
          try_decode(
            F,
            fun(F1) ->
                    yamerl:decode_file(F1, [{str_node_as_binary, true}])
            end, "YAML", Mode), F),
      F, Schema, Mode).

normalize_yaml([L|_] = Y, F) when is_list(L) ->
    %% array
    [normalize_yaml(Elem, F) || Elem <- Y];
normalize_yaml(Y, F) when is_list(Y) ->
    Res = [normalize_yaml(Elem, F) || Elem <- Y],
    try maps:from_list(Res)
    catch
        error:_ -> Res
    end;
normalize_yaml({K, V}, F) when is_list(V) ->
    {K, normalize_yaml(V, F)};
normalize_yaml(E, _) ->
    E.

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
    validate(JSON, F, schema(), Mode).

validate(JSON, F, Schema, Mode) when is_list(JSON) ->
    check_validation([validate_(Schema, J) || J <- JSON], JSON, F, Mode);
validate(JSON, F, Schema, Mode) when is_map(JSON) ->
    validate([JSON], F, Schema, Mode).

vinfo(silent, _, _) ->
    ok;
vinfo(_, Res, F) ->
    error_logger:info_report([{validation, F},
                              {result, Res}]).

info_msg(silent, _, _) -> ok;
info_msg(report, Fmt, Args) ->
    error_logger:info_msg(Fmt, Args).

error_msg(silent, _, _) -> ok;
error_msg(report, Fmt, Args) ->
    error_logger:error_msg(Fmt, Args).

check_validation(Res, JSON, F) ->
    check_validation(Res, JSON, F, report).

check_validation(Res, _JSON, F, Mode) ->
    vinfo(Mode, Res, F),
    case lists:foldr(
           fun({ok, M}, {Ok,Err}) when is_map(M) ->
                   {[M|Ok], Err};
              (Other, {Ok,Err}) ->
                   {Ok, [Other|Err]}
           end, {[], []}, Res) of
        {Ok, []} ->
            Ok;
        {_, Errors} ->
            erlang:error({validation_failed, Errors})
    end.

validate_(Schema, JSON) ->
    jesse:validate_with_schema(load_schema(Schema), JSON, []).

schema() ->
    filename:join(code:priv_dir(aeutils),
                  "epoch_config_schema.json").

load_schema(F) ->
    [Schema] = jsx:consult(F, [return_maps]),
    Schema.

%%% getting local peer from environment

-define(DEFAULT_SWAGGER_EXTERNAL_PORT, 8043).

-spec local_peer() -> {http_uri:scheme(), http_uri_host(), http_uri_port()}.
local_peer() ->
    ExternalPort = 
        user_config_or_env([<<"http">>, <<"external">>, <<"port">>],
                                   aehttp, swagger_port_external, ?DEFAULT_SWAGGER_EXTERNAL_PORT),
    ExternalAddr = 
        user_config_or_env([<<"http">>, <<"external">>, <<"peer_address">>], 
                           aehttp, local_peer_address, undefined),
    case ExternalAddr of
        undefined ->
            {ok, Host} = inet:gethostname(),
            {http, Host, ExternalPort};
        Uri ->
          case http_uri:parse(Uri) of
              {ok, {Scheme, _UserInfo, Host, Port, _Path, _Query, _Fragment}} ->
                  {Scheme, Host, Port};
              {ok, {Scheme, _UserInfo, Host, Port, _Path, _Query}} ->
                  {Scheme, Host, Port};
              {error, _Reason} ->
                  lager:debug("cannot parse Uri (~p): ~p", [Uri, _Reason]),
                  erlang:error({cannot_parse, [{local_peer_address, Uri}]})
          end
    end.
    

