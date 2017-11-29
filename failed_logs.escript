#! /usr/bin/env escript
%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
-mode(compile).

main([F]) ->
    case file:read_file(F) of
        {ok, B} ->
            main_(F, B);
        {error, _} = Error ->
            io:fwrite("Error reading ~s: ~p~n", [F, Error])
    end.

main_(F, B) ->
    Dir = filename:dirname(F),
    Lines = re:split(B, "\\\n", [{return,binary}]),
    Groups = group(Lines),
    lists:foreach(
      fun({failing_suite, Gs}) ->
              lists:foreach(
                fun(#{logfile := L,
                      data    := Data}) ->
                        Data1 = [<<D1/binary, "\n">> || D1 <- Data],
                        io:fwrite("~s~n", [iolist_to_binary(Data1)]),
                        print_logfile(filename:join(Dir,L))
                end, Gs);
         (Other) ->
              io:fwrite("?????? ~p~n", [Other]),
              ok
      end, Groups).

print_logfile(F) ->
    io:fwrite(
      "============================================================~n"
      " ~s~n"
      "------------------------------------------------------------~n"
      "~s~n"
      "============================================================~n",
      [F, do_print_logfile(F)]).

do_print_logfile(F) ->
    case file:read_file(F) of
        {ok, B} ->
            B;
        {error,_} = Error ->
            io_lib:fwrite("~p", [Error])
    end.

group(Lines) ->
    group(Lines, false, []).

group([<<"=case "   , _/binary>> = A,
       <<"=logfile ", _/binary>> = B,
       <<"=started ", _/binary>> = C,
       <<"=ended "  , _/binary>> = D,
       <<"=result " , _/binary>> = E|T], HasFailure, Acc) ->
    case is_init_per_suite(A) of
        true ->
            if HasFailure ->
                    [{failing_suite, lists:reverse(Acc)}
                     |group_(A,B,C,D,E,T, false, [])];
               true ->
                    group_(A,B,C,D,E,T, false, [])
            end;
        false ->
            group_(A,B,C,D,E,T, HasFailure, Acc)
    end;
group([_|T], HasFailure, Acc) ->
    group(T, HasFailure, Acc);
group([], HasFailure, Acc) ->
    if HasFailure ->
            [{failing_suite, lists:reverse(Acc)}];
       true ->
            []
    end.

group_(A,B,C,D,E, T, HasFailure, Acc) ->
    <<"=logfile ", L/binary>> = B,
    <<"=result " , Res/binary>> = E,
    {Acc1, HasFailure1, T1} =
        case wstrip(Res) of
            <<"failed:", _/binary>> ->
                {FLines, Rest} = collect_failure(T),
                {[#{logfile => wstrip(L),
                    data  => [A,B,C,D,E|FLines]}|Acc], true, Rest};
            _ ->
                {[#{logfile => wstrip(L),
                    data    => [A,B,C,D,E]}|Acc], HasFailure, T}
        end,
    group(T1, HasFailure1, Acc1).

collect_failure(Ls) ->
    collect_failure(Ls, []).

collect_failure([<<"=== *** FAILED", _/binary>> = A,
                 <<"===", _/binary>>            = B | T], Acc) ->
    {lists:reverse([B,A|Acc]), T};
collect_failure([H|T], Acc) ->
    collect_failure(T, [H|Acc]);
collect_failure([], Acc) ->
    {lists:reverse(Acc), []}.

wstrip(<<" ", T/binary>>) ->
    wstrip(T);
wstrip(B) ->
    B.

is_init_per_suite(B) ->
    case re:split(wstrip(B), ":", [{return, binary}]) of
        [_, <<"init_per_suite">>] ->
            true;
        _ ->
            false
    end.
