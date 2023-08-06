-module(aeu_lib).

-export([pmap/3, map/3]).

pmap(Fun, L, Timeout) ->
    Workers =
        lists:map(
            fun(E) ->
                spawn_monitor(
                    fun() ->
                        {WorkerPid, WorkerMRef} =
                            spawn_monitor(
                                fun() ->
                                    Res = Fun(E),
                                    exit({ok, Res})
                                end),
                        Result =
                            receive
                                {'DOWN', WorkerMRef, process, WorkerPid, Res} ->
                                    case Res of
                                        {ok, T} -> {ok, T};
                                        Other   -> {error, Other}
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

-spec map(Function, List, Timeout) -> Result
    when Function :: fun((term()) -> {ok, term()} | {error, term()}),
         List     :: list(),
         Timeout  :: infinity | non_neg_integer(),
         Result   :: {ok,      Results    :: [term()]}
                   | {done,    Results    :: [term()],
                               Errors     :: [{Input :: term(), Error :: term()}]}
                   | {timeout, Results    :: [term()],
                               Errors     :: [{Input :: term(), Error :: term()}],
                               Incomplete :: [term()]}.

map(Function, List, Timeout) ->
    {Workers, Count} = work(execute(Function), List, [], 0),
    Timer =
        case Timeout of
            infinity -> none;
            MilliSec -> erlang:send_after(MilliSec, self(), timeout)
        end,
    gather(Workers, Count, Timer).

work(_, [], A, C) ->
    {A, C};
work(E, [H | T], A, C) ->
    work(E, T, [E(H) | A], C + 1).

execute(F) ->
    fun(E) ->
        P = spawn_monitor(fun() -> exit(F(E)) end),
        {pending, P, E}
    end.

gather(Pending, Count, Timer) when Count > 0 ->
    receive
        {'DOWN', _, process, PID, Outcome} ->
            Received = complete(PID, Outcome, Pending),
            gather(Received, Count - 1, Timer);
        timeout ->
            the_final_solution(Pending)
    end;
gather(Results, 0, none) ->
    the_final_solution(Results);
gather(Results, 0, Timer) ->
    _ = erlang:cancel_timer(Timer),
    the_final_solution(Results).

complete(PID, Result, [{pending, {PID, _}, Input} | Rest]) ->
    [{done, Input, Result} | Rest];
complete(PID, Result, [Element | Rest]) ->
    [Element | complete(PID, Result, Rest)].

the_final_solution(Results) ->
    case lists:foldl(fun finalize/2, {[], [], []}, Results) of
        {Final, [], []}             -> {ok, Final};
        {Final, Errors, []}         -> {done, Final, Errors};
        {Final, Errors, Incomplete} -> {timeout, Final, Errors, Incomplete}
    end.

finalize({done, _, {ok, Result}}, {Results, Errors, Incomplete}) ->
    {[Result | Results], Errors, Incomplete};
finalize({done, Input, Error}, {Results, Errors, Incomplete}) ->
    {Results, [{Input, Error} | Errors], Incomplete};
finalize({pending, {PID, Mon}, Input}, {Results, Errors, Incomplete}) ->
    true = exit(PID, kill),
    true = demonitor(Mon, [flush]),
    {Results, Errors, [Input | Incomplete]}.
