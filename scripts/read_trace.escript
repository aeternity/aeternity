#!/usr/bin/env escript
%% -*- mode:erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
-mode(compile).

%% This escript can be used to pretty-print erlang trace output produced
%% using a dbg:trace_port(). This can be useful e.g. for running a long-running
%% or early startup trace. The trace_port() output is on binary form, so this
%% escript runs the corresponding trace_client(), producing readable output which
%% can be piped to less or a file.
%%
%% Example: assuming this sort of code inserted in some strategic function:
%%
%% LogDir = setup:log_dir(),
%% dbg:tracer(port, dbg:trace_port(file, filename:join(LogDir, "trace.out"))),
%% dbg:tp(mnesia,x),
%% dbg:tp(aec_db,x),
%% dbg:tpl(mnesia_loader,x),
%% dbg:tp(rocksdb,x),
%% dbg:p(all,[c]),
%%
%% You can then call, from the appropriate directory:
%% escript .../read_trace.escript trace.out | less

main([TraceFile]) ->
    Pid = dbg:trace_client(file, TraceFile, {fun dhandler/2, user}),
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, process, _, _} ->
            ok
    end.

%% Copied from dbg.erl in OTP 24.1.3 (timestamp pretty-printing added)

dhandler(end_of_trace, Out) ->
    Out;
dhandler(Trace, Out) when element(1, Trace) == trace, tuple_size(Trace) >= 3 ->
    dhandler1(Trace, tuple_size(Trace), out(Out));
dhandler(Trace, Out) when element(1, Trace) == trace_ts, tuple_size(Trace) >= 4 ->
    dhandler1(Trace, tuple_size(Trace)-1, element(tuple_size(Trace),Trace)
             , out(Out));
dhandler(Trace, Out) when element(1, Trace) == drop, tuple_size(Trace) =:= 2 ->
    {Device,Modifier} = out(Out),
    io:format(Device, "*** Dropped ~p messages.~n", [element(2,Trace)]),
    {Device,Modifier};
dhandler(Trace, Out) when element(1, Trace) == seq_trace,
                          tuple_size(Trace) >= 3 ->
    {Device,Modifier} = out(Out),
    SeqTraceInfo = case Trace of
                       {seq_trace, Lbl, STI, TS} ->
                           io:format(Device, "SeqTrace ~p [~p]: ",
                                     [TS, Lbl]),
                           STI;
                       {seq_trace, Lbl, STI} ->
                           io:format(Device, "SeqTrace [~p]: ",
                                     [Lbl]),
                           STI 
                   end,
    case SeqTraceInfo of
        {send, Ser, Fr, To, Mes} ->
            io:format(Device, "(~p) ~p ! ~"++Modifier++"p [Serial: ~p]~n",
                      [Fr, To, Mes, Ser]);
        {'receive', Ser, Fr, To, Mes} ->
            io:format(Device, "(~p) << ~"++Modifier++"p [Serial: ~p, From: ~p]~n",
                      [To, Mes, Ser, Fr]);
        {print, Ser, Fr, _, Info} ->
            io:format(Device, "-> ~"++Modifier++"p [Serial: ~p, From: ~p]~n",
                      [Info, Ser, Fr]);
        Else ->
            io:format(Device, "~"++Modifier++"p~n", [Else])
    end,
    {Device,Modifier};
dhandler(_Trace, Out) ->
    Out.

dhandler1(Trace, Size, {Device,Modifier}) ->
    From = element(2, Trace),
    case element(3, Trace) of
        'receive' ->
            case element(4, Trace) of
                {dbg,ok} -> ok;
                Message ->
                    io:format(Device, "(~p) << ~"++Modifier++"p~n",
                              [From,Message])
            end;
        'send' ->
            Message = element(4, Trace),
            To = element(5, Trace),
            io:format(Device, "(~p) ~p ! ~"++Modifier++"p~n", [From,To,Message]);
        call ->
            case element(4, Trace) of
                MFA when Size == 5 ->
                    Message = element(5, Trace),
                    io:format(Device,
                              "(~p) call ~"++Modifier++"s (~"++Modifier++"p)~n",
                              [From,ffunc(MFA,Modifier),Message]);
                MFA ->
                    io:format(Device, "(~p) call ~"++Modifier++"s~n",
                              [From,ffunc(MFA,Modifier)])
            end;
        return -> %% To be deleted...
            case element(4, Trace) of
                MFA when Size == 5 ->
                    Ret = element(5, Trace),
                    io:format(Device,
                              "(~p) old_ret ~"++Modifier++"s -> ~"++Modifier++
                                  "p~n",
                              [From,ffunc(MFA,Modifier),Ret]);
                MFA ->
                    io:format(Device, "(~p) old_ret ~"++Modifier++"s~n",
                              [From,ffunc(MFA,Modifier)])
            end;
        return_from ->
            MFA = element(4, Trace),
            Ret = element(5, Trace),
            io:format(Device,
                      "(~p) returned from ~"++Modifier++"s -> ~"++Modifier++"p~n",
                      [From,ffunc(MFA,Modifier),Ret]);
        return_to ->
            MFA = element(4, Trace),
            io:format(Device, "(~p) returning to ~"++Modifier++"s~n",
                      [From,ffunc(MFA,Modifier)]);
        spawn when Size == 5 ->
            Pid = element(4, Trace),
            MFA = element(5, Trace),
            io:format(Device, "(~p) spawn ~p as ~"++Modifier++"s~n",
                      [From,Pid,ffunc(MFA,Modifier)]);
        Op ->
            io:format(Device, "(~p) ~p ~"++Modifier++"s~n",
                      [From,Op,ftup(Trace,4,Size,Modifier)])
    end,
    {Device,Modifier}.

dhandler1(Trace, Size, TS, {Device,Modifier}) ->
    From = element(2, Trace),
    {TsFmt, TsArg} = ts(TS),
    case element(3, Trace) of
        'receive' ->
            case element(4, Trace) of
                {dbg,ok} -> ok;
                Message ->
                    io:format(Device,
                              "(~p) << ~"++Modifier++"p (Time: " ++ TsFmt ++ ")~n",
                              [From,Message,TsArg])
            end;
        'send' ->
            Message = element(4, Trace),
            To = element(5, Trace),
            io:format(Device, "(~p) ~p ! ~"++Modifier++"p (Time: " ++ TsFmt ++ ")~n",
                      [From,To,Message,TsArg]);
        call ->
            case element(4, Trace) of
                MFA when Size == 5 ->
                    Message = element(5, Trace),
                    io:format(Device,
                              "(~p) call ~"++Modifier++"s (~"++Modifier++
                                  "p) (Time: " ++ TsFmt ++ ")~n",
                              [From,ffunc(MFA,Modifier),Message,TsArg]);
                MFA ->
                    io:format(Device,
                              "(~p) call ~"++Modifier++"s (Time: " ++ TsFmt ++ ")~n",
                              [From,ffunc(MFA,Modifier),TsArg])
            end;
        return -> %% To be deleted...
            case element(4, Trace) of
                MFA when Size == 5 ->
                    Ret = element(5, Trace),
                    io:format(Device,
                              "(~p) old_ret ~"++Modifier++"s -> ~"++Modifier++
                                  "p (Time: " ++ TsFmt ++ ")~n",
                              [From,ffunc(MFA,Modifier),Ret,TsArg]);
                MFA ->
                    io:format(Device,
                              "(~p) old_ret ~"++Modifier++"s (Time: " ++ TsFmt ++ ")~n",
                              [From,ffunc(MFA,Modifier),TsArg])
            end;
        return_from ->
            MFA = element(4, Trace),
            Ret = element(5, Trace),
            io:format(Device,
                      "(~p) returned from ~"++Modifier++"s -> ~"++Modifier++
                          "p (Time: " ++ TsFmt ++ ")~n",
                      [From,ffunc(MFA,Modifier),Ret,TsArg]);
        return_to ->
            MFA = element(4, Trace),
            io:format(Device,
                      "(~p) returning to ~"++Modifier++"s (Time: " ++ TsFmt ++ ")~n",
                      [From,ffunc(MFA,Modifier),TsArg]);
        spawn when Size == 5 ->
            Pid = element(4, Trace),
            MFA = element(5, Trace),
            io:format(Device,
                      "(~p) spawn ~p as ~"++Modifier++"s (Time: " ++ TsFmt ++ ")~n",
                      [From,Pid,ffunc(MFA,Modifier),TsArg]);
        Op ->
            io:format(Device, "(~p) ~p ~"++Modifier++"s (Time: " ++ TsFmt ++ ")~n",
                      [From,Op,ftup(Trace,4,Size,Modifier),TsArg])
    end,
    {Device,Modifier}.

%%% These f* functions returns non-flat strings

%% {M,F,[A1, A2, ..., AN]} -> "M:F(A1, A2, ..., AN)"
%% {M,F,A}                 -> "M:F/A"
ffunc({M,F,Argl},Modifier) when is_list(Argl) ->
    io_lib:format("~p:~"++Modifier++"p(~"++Modifier++"s)",
                  [M, F, fargs(Argl,Modifier)]);
ffunc({M,F,Arity},Modifier) ->
    io_lib:format("~p:~"++Modifier++"p/~p", [M,F,Arity]);
ffunc(X,Modifier) -> io_lib:format("~"++Modifier++"p", [X]).

%% Integer           -> "Integer"
%% [A1, A2, ..., AN] -> "A1, A2, ..., AN"
fargs(Arity,_) when is_integer(Arity) -> integer_to_list(Arity);
fargs([],_) -> [];
fargs([A],Modifier) ->
    io_lib:format("~"++Modifier++"p", [A]);  %% last arg
fargs([A|Args],Modifier) ->
    [io_lib:format("~"++Modifier++"p,", [A]) | fargs(Args,Modifier)];
fargs(A,Modifier) ->
    io_lib:format("~"++Modifier++"p", [A]). % last or only arg

%% {A_1, A_2, ..., A_N} -> "A_Index A_Index+1 ... A_Size"
ftup(Trace, Index, Index, Modifier) ->
    io_lib:format("~"++Modifier++"p", [element(Index, Trace)]);
ftup(Trace, Index, Size, Modifier) ->
    [io_lib:format("~"++Modifier++"p ", [element(Index, Trace)])
     | ftup(Trace, Index+1, Size, Modifier)].

out({_,_}=Out) ->
    Out;
out(Device) ->
    {Device,modifier(Device)}.

modifier(Device) ->
    Encoding =
        case io:getopts(Device) of
            List when is_list(List) ->
                proplists:get_value(encoding,List,latin1);
            _ ->
                latin1
        end,
    encoding_to_modifier(Encoding).

encoding_to_modifier(latin1) -> "";
encoding_to_modifier(_) -> "t".


ts({_,_,_} = TS) ->
    {"~s", calendar:system_time_to_rfc3339(now_to_system_time(TS), [{unit, microsecond}])};
ts(TS) ->
    %% E.g. 'cpu_timestamp', 'monotonic_timestamp'
    {"~p", TS}.


now_to_system_time({MS, S, US}) ->
    (MS*1000000+S)*1000000+US.
