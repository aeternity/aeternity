-module(aec_metrics_lib).

-export([find_entries_cmd/1,
         find_entries_cmd/2,
         filter_entries/2,
         get_values_cmd/1]).

find_entries_cmd(Arg) ->
    find_entries_(Arg, enabled, fun exometer:select/1).

find_entries_cmd(Arg, Status) ->
    find_entries_(Arg, Status, fun exometer:select/1).

filter_entries(Entries, Arg) ->
    %% Entries shall have the same format as the product of
    %% exometer:select/1, i.e. [{Name, Type, Status}].
    %% Metrics that don't exist are filtered out
    Filtered = lists:filter(
                 fun({N, _Type, Status}) ->
                         Status =:= enabled andalso metric_exists(N)
                 end, Entries),
    Select = fun(Patterns) ->
                     CSpec = ets:match_spec_compile(Patterns),
                     ets:match_spec_run(Filtered, CSpec)
             end,
    find_entries_(Arg, enabled, Select).

find_entries_(Arg, Status, Select) when is_function(Select, 1) ->
    lists:map(
      fun(A) ->
              #{dps := DPs} = Map = type_status_and_dps(A, Status),
              {find_entries_1(Map, Select), DPs}
      end, Arg).

get_values_cmd(Arg) ->
    Res = find_entries_cmd(Arg),
    [[{Name, Type, Status, do_get_value(Name, Status, DPs)}
      || {Name,Type,Status} <- Found] || {Found, DPs} <- Res].

do_get_value(_, disabled, _) -> [];
do_get_value(Name, _, DPs) ->
    case exometer:get_value(Name, DPs) of
        {ok, Value}    -> Value;
        {error, _} = E -> E
    end.

type_status_and_dps(S, Status0) ->
    Parts = re:split(S, "/"),
    lists:foldl(
      fun(P, Acc) ->
              parse_part(part_type(P), P, Acc)
      end, #{name   => [],
             type   => '_',
             status => Status0,
             dps    => default}, Parts).

part_type(<<"type=", _/binary>> ) -> type;
part_type(<<"status=",_/binary>>) -> status;
part_type(<<"[", _/binary>>     ) -> name;
part_type(P) ->
    case re:run(P, "[\\.\\*]") of
        {match,_} -> name;
        nomatch   -> dps
    end.

parse_part(type, <<"type=", T/binary>>, Acc) ->
    NewType = case T of
                  <<"*">> -> '_';
                  _ ->
                      try binary_to_existing_atom(T, latin1)
                      catch error:_ -> T
                      end
              end,
    Acc#{type => NewType};
parse_part(status, <<"status=", St/binary>>, Acc) ->
    NewStatus = case St of
                    <<"enabled">>  -> enabled;
                    <<"disabled">> -> disabled;
                    <<"*">>        -> '_'
                end,
    Acc#{status => NewStatus};
parse_part(dps, DPsBin, #{dps := DPs} = Acc) ->
    NewDPs = merge([to_atom_or_int(D)
                    || D <- re:split(DPsBin, ",")], DPs),
    Acc#{dps => NewDPs};
parse_part(name, S, Acc) ->
    Acc#{name => S}.

to_atom_or_int(D) ->
    try binary_to_existing_atom(D, utf8)
    catch
        error:_ ->
            binary_to_integer(D)
    end.

merge([_|_] = DPs, default) ->
    DPs;
merge([H|T], DPs) ->
    case lists:member(H, DPs) of
        true  -> merge(T, DPs);
        false -> merge(T, DPs ++ [H])
    end;
merge([], DPs) ->
    DPs.

find_entries_1(#{name := S, type := Type, status := Status}, Select) ->
    Patterns = lists:flatten([parse_stat_entry(S, Type, Status)]),
    Select(Patterns).

prefix() ->
    [ae,epoch].

parse_stat_entry(E, Type, Status) when E==<<>>; E==[];
                                       E==<<"*">>; E==<<"**">> ->
    {{prefix() ++ '_', Type, Status}, [], ['$_']};
parse_stat_entry("[" ++ _ = Expr, _Type, _Status) ->
    case erl_scan:string(ensure_trailing_dot(Expr)) of
        {ok, Toks, _} ->
            case erl_parse:parse_exprs(Toks) of
                {ok, [Abst]} ->
                    partial_eval(Abst);
                Error ->
                    io:fwrite("(Parse error for ~p: ~p~n", [Expr, Error]),
                    []
            end;
        ScanErr ->
            io:fwrite("(Scan error for ~p: ~p~n", [Expr, ScanErr]),
            []
    end;
parse_stat_entry(Str, Type, Status) when Status==enabled; Status==disabled ->
    Parts = re:split(Str, "\\.", [{return,list}]),
    Heads = replace_parts(Parts),
    [{{H,Type,Status}, [], ['$_']} || H <- Heads];
parse_stat_entry(Str, Type, '_') ->
    Parts = re:split(Str, "\\.", [{return,list}]),
    Heads = replace_parts(Parts),
    [{{H,Type,'_'}, [], ['$_']} || H <- Heads];
parse_stat_entry(_, _, Status) ->
    io:fwrite("(Illegal status: ~p~n", [Status]).

ensure_trailing_dot(Str) ->
    case lists:last(Str) of
        $. ->
            Str;
        _ ->
            Str ++ "."
    end.

partial_eval({cons,_,H,T}) ->
    [partial_eval(H) | partial_eval(T)];
%% partial_eval({nil,_}) ->
%%     [];
partial_eval({tuple,_,Elems}) ->
    list_to_tuple([partial_eval(E) || E <- Elems]);
%% partial_eval({T,_,X}) when T==atom; T==integer; T==float ->
%%     X;
partial_eval({op,_,'++',L1,L2}) ->
    partial_eval(L1) ++ partial_eval(L2);
partial_eval(X) ->
    erl_parse:normalise(X).

replace_parts(Parts) ->
    case split("**", Parts) of
        {_, []} ->
            [replace_parts_1(Parts)];
        {Before, After} ->
            Head = replace_parts_1(Before),
            Tail = replace_parts_1(After),
            [Head ++ Pad ++ Tail || Pad <- pads()]
    end.

pads() ->
    [['_'],
     ['_','_'],
     ['_','_','_'],
     ['_','_','_','_'],
     ['_','_','_','_','_'],
     ['_','_','_','_','_','_'],
     ['_','_','_','_','_','_','_'],
     ['_','_','_','_','_','_','_','_'],
     ['_','_','_','_','_','_','_','_','_'],
     ['_','_','_','_','_','_','_','_','_','_'],
     ['_','_','_','_','_','_','_','_','_','_','_'],
     ['_','_','_','_','_','_','_','_','_','_','_','_'],
     ['_','_','_','_','_','_','_','_','_','_','_','_','_'],
     ['_','_','_','_','_','_','_','_','_','_','_','_','_','_'],
     ['_','_','_','_','_','_','_','_','_','_','_','_','_','_','_'],
     ['_','_','_','_','_','_','_','_','_','_','_','_','_','_','_','_']].

split(X, L) ->
    split(L, X, []).

split([H|T], H, Acc) ->
    {lists:reverse(Acc), T};
split([H|T], X, Acc) ->
    split(T, X, [H|Acc]);
split([], _, Acc) ->
    {lists:reverse(Acc), []}.

replace_parts_1([H|T]) ->
    R = replace_part(H),
    case T of
        ["**"] -> [R] ++ '_';
        _ -> [R|replace_parts_1(T)]
    end;
replace_parts_1([]) ->
    [].

replace_part(H) ->
    case H of
        "*" -> '_';
        "'" ++ _ ->
            case erl_scan:string(H) of
                {ok, [{atom, _, A}], _} ->
                    A;
                Error ->
                    error(Error)
            end;
        [C|_] when C >= $0, C =< $9 ->
            try list_to_integer(H)
            catch
                error:_ -> list_to_atom(H)
            end;
        _ -> list_to_atom(H)
    end.

metric_exists(N) ->
    case exometer:info(N, name) of
        undefined ->
            false;
        _ ->
            true
    end.
