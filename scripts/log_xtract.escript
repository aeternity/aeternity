#!/usr/bin/env escript
%% -*- mode:erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-


main([Re | Path]) ->
    {Pids, Ps} = lists:foldl(
                   fun(P, Acc) ->
                           grep(Re, P, Acc)
                   end, {ordsets:new(), ordsets:new()}, Path),
    PidPats = pid_patterns(Pids),
    %% lager log files are ordered as:
    %% aeternity.log - newest
    %% aeternity.log.N - higher N is older
    %% Ps is an ordset, so reverse to get oldest first
    [find_pids(P, PidPats) || P <- lists:reverse(Ps)],
    halt(0).

grep(Re, P, {Acc, PAcc}) ->
    x(os:cmd("grep " ++ Re ++ " " ++ P), P, Acc, PAcc).

find_pids(File, Pids) ->
    try find_pids_(File, Pids)
    catch error:E ->
            io:fwrite("CAUGHT ~p (File = ~p)~n", [E, File]),
            ok
    end.

find_pids_(File, Pids) ->
    {ok, Bin} = file:read_file(File),
    Lines = string:lexemes(Bin, [$\n]),
    io:fwrite("*** ~s ***~n", [File]),
    Matches = [L || L <- Lines,
                    match_pids(L, Pids)],
    [io:fwrite("~s~n", [M]) || M <- Matches].

pid_patterns(Pids) ->
    [list_to_binary(pid_to_list(P)) || P <- Pids].

match_pids(L, PidPats) ->
    binary:matches(L, PidPats) =/= [].

x([], _, Acc, PAcc) ->
    {Acc, PAcc};
x(Out, P, Acc, PAcc) ->
    PAcc1 = ordsets:add_element(P, PAcc),
    NewAcc = extract_pids(Out, Acc),
    case NewAcc -- Acc of
        [] ->
            {Acc, PAcc1};
        New ->
            lists:foldl(
              fun(Pid, AccX) ->
                      grep("'" ++ pid_to_list(Pid) ++ "'", P, AccX)
              end, {NewAcc, PAcc1}, New)
    end.
    


extract_pids(String, Acc) ->
    case re:run(String, "<[0-9]+\\.[0-9]+\\.[0-9]+>",
                [global,{capture,all,list}]) of
        {match, Matches} ->
            lists:foldl(
              fun(M, Acc1) ->
                      lists:foldl(
                        fun(P, Acc2) ->
                                try ordsets:add_element(list_to_pid(P), Acc2)
                                catch error:_ -> Acc2
                                end
                        end, Acc1, M)
              end, Acc, Matches);
        _ ->
            []
    end.


