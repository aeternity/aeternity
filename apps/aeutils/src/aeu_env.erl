%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Bootstrapping and querying the Epoch environment
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aeu_env).

-export([get_env/2, get_env/3]).

-export([user_config/0, user_config/1]).
-export([read_config/0]).

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

user_config() ->
    setup:get_env(aeutils, '$user_config', []).

-spec user_config(list() | binary()) -> undefined | {ok, any()}.
user_config(Key) when is_list(Key) ->
    get_env(aeutils, ['$user_config'|Key]);
user_config(Key) when is_binary(Key) ->
    get_env(aeutils, ['$user_config',Key]).


read_config() ->
    try read_config_()
    catch
        error:E ->
            error_logger:error_report(
              [{?MODULE, read_config},
               {error, E},
               {stacktrace, erlang:get_stacktrace()}])
    end.

read_config_() ->
    case config_file() of
        undefined ->
            error_logger:info_msg(
              "No config file specified; using default settings~n", []),
            ok;
        F ->
            error_logger:info_msg("Reading config file ~s~n", [F]),
            do_read_config(F)
    end.

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

search_default_config() ->
    Dirs = [filename:join([os:getenv("HOME"), ".epoch", nodename()]),
            setup:home()],
    io:fwrite("Dirs = ~p~n", [Dirs]),
    lists:foldl(
      fun(D, undefined) ->
              case filelib:wildcard(
                     "epoch.{json,yaml}", D) of
                  [] -> undefined;
                  [F|_] -> filename:join(D, F)
              end;
         (_, Acc) -> Acc
      end, undefined, Dirs).

do_read_config(F) ->
    case filename:extension(F) of
        ".json" ->
            store(read_json(F));
        ".yaml" ->
            store(read_yaml(F))
    end.

nodename() ->
    case node() of
        nonode@nohost ->
            [];
        N ->
            hd(re:split(atom_to_list(N), "@", [{return,list}]))
    end.

store(Vars) ->
    io:fwrite("store(~p)~n", [Vars]),
    set_env(aeutils, '$user_config', Vars).

set_env(App, K, V) ->
    error_logger:info_msg("Set config (~p): ~p = ~p~n", [App, K, V]),
    application:set_env(App, K, V).

read_json(F) ->
    interpret_json(try_decode(F, fun jsx:consult/1, "JSON"), F).

interpret_json(L, F) when is_list(L) ->
    lists:flatten([interpret_json(Elem, F) || Elem <- L]);
interpret_json({K, V}, F) when is_list(V) ->
    {K, interpret_json(V, F)};
interpret_json(E, _) ->
    E.

read_yaml(F) ->
    interpret_yaml(
      try_decode(
        F, fun(F1) ->
                   yamerl:decode_file(F1, [{str_node_as_binary, true}])
           end, "YAML"), F).

interpret_yaml(L, F) when is_list(L) ->
    lists:flatten([interpret_yaml(Elem, F) || Elem <- L]);
interpret_yaml({K, V}, F) when is_list(V) ->
    {K, interpret_yaml(V, F)};
interpret_yaml(E, _) ->
    E.

try_decode(F, DecF, Fmt) ->
    try DecF(F)
    catch
        error:_E ->
            error_logger:error_msg("Error reading ~s file: ~s~n", [Fmt, F]),
            []
    end.

%% to_str(S) ->
%%     binary_to_list(iolist_to_binary(S)).
