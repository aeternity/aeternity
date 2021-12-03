-module(aeu_ext_scripts).

-export([ parse_opts/2
        , parse_opts/3
        , connect_node/1
        , ensure_mode/1
        , restore_mode/1
        , aecore_apps/0 ]).

-type mode() :: atom().

parse_opts(Args, Spec) ->
    parse_opts(Args, Spec, #{}).

parse_opts(Args, Spec0, Opts) when is_map(Spec0) ->
    Args0 = maps:get(arguments, Spec0, []),
    Spec = Spec0#{arguments => Args0 ++ node_arguments()},
    try parse_opts_(Args, Spec, Opts)
    catch
        error:{argparse, Error} ->
            usage(Error, Spec, Opts);
        error:{?MODULE, _} = Error ->
            usage(Error, Spec, Opts);
        Tag:E ->
            io:fwrite("CAUGHT ~p:~p~n", [Tag, E]),
            usage(E, Spec, Opts)
    end.

parse_opts_(Args, Spec, Opts0) ->
    #{arg_map := Opts} = ParseRes = parse(Args, Spec, options(Opts0)),
    NodeKeys = nodekeys(),
    ConnOpts = maps:with(NodeKeys, Opts),
    UserOpts = maps:without(NodeKeys, Opts),
    ParseRes#{ opts => UserOpts
             , connect => check_conn_opts(ConnOpts) }.

nodekeys() ->
    ['$cookie','$sname','$name'].

options(Opts0) ->
    maps:merge(#{progname => script_name()}, Opts0).

parse(Args, Spec, Opts) ->
    Res = argparse:parse(Args, Spec, Opts),
    case Res of
        {ArgMap, CmdSpec} ->
            #{arg_map => ArgMap, cmd_spec => CmdSpec};
        ArgMap when is_map(ArgMap) ->
            #{arg_map => ArgMap}
    end.

script_name() ->
    try escript:script_name() of
        SName ->
            tidy_script_name(SName)
    catch
        error:_ ->
            "erl"
    end.

tidy_script_name(N) ->
    case ["aeternity", "bin"] -- filename:split(N) of
        [] ->
            filename:join("aeternity/bin", filename:basename(N));
        _ ->
            N
    end.

usage(Error, Spec, Opts) ->
    io:fwrite(standard_error, format_error(remove_nodekeys(Spec), Error, Opts), []),
    halt(1).

remove_nodekeys(#{arguments := Args} = Spec) ->
    Args1 = lists:filter(fun(#{name := N}) ->
                                 not lists:member(N, nodekeys());
                            (_) -> true
                         end, Args),
    Spec#{arguments => Args1}.

node_arguments() ->
    Arg = #{ required => false, type => string, nargs => 1 },
    [
      Arg#{ name  => '$cookie', long => "setcookie", required => true }
    , Arg#{ name  => '$sname' , long => "sname"     }
    , Arg#{ name  => '$name'  , long => "name"      }
    ].

check_conn_opts(#{'$cookie' := [Cookie]} = Opts) ->
    {Name, Mode} = name_and_dist_mode(Opts),
    #{ cookie => Cookie
     , name => my_name(Name)
     , dist_mode => Mode
     , target_node => nodename(Name) }.

name_and_dist_mode(Opts) ->
    case Opts of
        #{'$name' := _, '$sname' := _} ->
            error({?MODULE, both_sname_and_name});
        #{'$name' := [Name] } ->
            {Name, longnames};
        #{'$sname' := [Name]} ->
            {Name, shortnames};
        _ ->
            error({?MODULE, no_name_or_sname})
    end.

-define(MODE_SWITCH_TIMEOUT, 30000).

%% Returns the previous app_ctrl mode.
-spec ensure_mode(mode()) -> mode().
ensure_mode(Mode) ->
    case PrevMode = app_ctrl:get_mode() of
        Mode ->
            ok;
        _ ->
            {ok, _} = app_ctrl:set_and_await_mode(Mode, ?MODE_SWITCH_TIMEOUT)
    end,
    PrevMode.

-spec restore_mode(PrevMode) -> ok
              when PrevMode :: mode().
restore_mode(PrevMode) ->
    {ok, _} = app_ctrl:set_and_await_mode(PrevMode, ?MODE_SWITCH_TIMEOUT),
    ok.

aecore_apps() ->
    Apps = [{A, app_deps(A)} || {A, _, _} <- application:which_applications()],
    aecore_apps(Apps).

app_deps(A) ->
    {ok, Ds} = application:get_key(A, applications),
    Ds.

aecore_apps(AppsDeps) ->
    case lists:keytake(aecore, 1, AppsDeps) of
        {value, {_, AecDeps}, Rest} ->
            AppsDeps1 = [App || {A,_} = App <- Rest,
                                not lists:member(A, AecDeps)],
            transitive_deps([aecore], AppsDeps1);
        false ->
            []
    end.

transitive_deps(Apps, AppsDeps) ->
    case [A || {A, Ds} <- AppsDeps,
               intersection(Apps, Ds) =/= []] of
        [] ->
            Apps;
        NewApps ->
            transitive_deps(Apps ++ NewApps, [App || {A, _} = App <- AppsDeps,
                                                     not lists:member(A, NewApps)])
    end.

intersection(A, B) ->
    A -- (A -- B).

connect_node(#{connect := #{name := Name, dist_mode := Mode, cookie := Cookie,
                            target_node := TargetNode}}) ->
    {ok, _} = net_kernel:start([Name, Mode]),
    erlang:set_cookie(node(), list_to_atom(Cookie)),
    connect_and_ping(TargetNode).

my_name(Name) ->
    ScriptName = escript:script_name(),
    append_node_suffix(Name, "_" ++ filename:basename(ScriptName)).

connect_and_ping(Node) ->
    NotResp = fun() ->
                      io:fwrite(standard_error, "Node ~p not responding", [Node]),
                      halt(1)
              end,
    case net_kernel:hidden_connect_node(Node) of
        true ->
            case net_adm:ping(Node) of
                pong ->
                    {ok, Node};
                pang ->
                    NotResp()
            end;
        false ->
            NotResp()
    end.

format_error(_Spec, {?MODULE, no_name_or_sname}, _) ->
    "Either -sname or -name option required~n";
format_error(_Spec, {?MODULE, both_sname_and_name}, _) ->
    "Cannot have both -sname and -name~n";
format_error(Spec, Error, Opts) ->
    argparse:format_error(Error, Spec, Opts).

append_node_suffix(Name, Suffix) ->
    case re:split(Name, "@", [{return, list}, unicode]) of
        [Node, Host] ->
            list_to_atom(lists:concat([Node, Suffix, os:getpid(), "@", Host]));
        [Node] ->
            list_to_atom(lists:concat([Node, Suffix, os:getpid()]))
    end.

nodename(Name) ->
    case re:split(Name, "@", [{return, list}, unicode]) of
        [_Node, _Host] ->
            list_to_atom(Name);
        [Node] ->
            [_, Host] = re:split(atom_to_list(node()), "@", [{return, list}, unicode]),
            list_to_atom(lists:concat([Node, "@", Host]))
    end.
