-module(aeu_lib).

-export([pmap/3]).

pmap(Fun, L, Timeout) ->
    Workers =
        lists:map(
            fun(E) ->
                spawn_monitor(
                    fun() ->
                        {WorkerPid, WorkerMRef} =
                            spawn_monitor(
                                fun() ->
                                    Top = Fun(E),
                                    exit({ok, Top})
                                end),
                        Result =
                            receive
                                {'DOWN', WorkerMRef, process, WorkerPid, Res} ->
                                    case Res of
                                        {ok, T} -> {ok, T};
                                        _       -> {error, failed}
                                    end
                            after Timeout -> {error, request_timeout}
                            end,
                        exit(Result)
                    end)
            end,
            L),
    pmap_gather(Workers, [], []).

pmap_gather([], Good, Errs) ->
    {Good, Errs};
pmap_gather([{Pid, MRef} | Pids], Good, Errs) ->
    receive
        {'DOWN', MRef, process, Pid, Res} ->
            case Res of
                {ok, GoodRes} -> pmap_gather(Pids, [GoodRes | Good], Errs);
                {error, _} = Err -> pmap_gather(Pids, Good, [Err | Errs])
            end
    end.