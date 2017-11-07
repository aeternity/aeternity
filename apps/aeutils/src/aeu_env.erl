%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Bootstrapping and querying the Epoch environment
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aeu_env).

-export([get_env/3]).

-export([read_config/0]).


get_env(App, K, Default) ->
    setup:get_env(App, K, Default).


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
    lists:foreach(fun store_var/1, Vars).

store_var({K, Peers}) when K =:= <<"peers">>; K =:= "peers" ->
    set_env(aecore, peers, [to_str(P) || P <- Peers]);
store_var(Other) ->
    error_logger:error_msg("Unknown config: ~p~n", [Other]).

set_env(App, K, V) ->
    error_logger:info_msg("Set config (~p): ~p = ~p~n", [App, K, V]),
    application:set_env(App, K, V).

read_json(F) ->
    interpret_json(try_decode(F, fun jsx:consult/1, "JSON"), F).

interpret_json(L, F) when is_list(L) ->
    lists:flatten([interpret_json(Elem, F) || Elem <- L]);
interpret_json({_K, _V} = E, _) ->
    E.

read_yaml(F) ->
    interpret_yaml(try_decode(F, fun yamerl:decode_file/1, "YAML"), F).

interpret_yaml(L, F) when is_list(L) ->
    lists:flatten([interpret_yaml(Elem, F) || Elem <- L]);
interpret_yaml({_K, _V} = E, _) ->
    E.

try_decode(F, DecF, Fmt) ->
    try DecF(F)
    catch
        error:_E ->
            error_logger:error_msg("Error reading ~s file: ~s~n", [Fmt, F]),
            []
    end.

to_str(S) ->
    binary_to_list(iolist_to_binary(S)).
