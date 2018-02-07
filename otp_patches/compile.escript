#!/usr/bin/env escript
%% -*- mode:erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%! -smp enable
-mode(compile).

main([]) ->
    Dir = filename:dirname(escript:script_name()),
    in_dir(fun compile_patches/0, Dir).

in_dir(F, D) ->
    {ok, Cwd} = file:get_cwd(),
    try
        file:set_cwd(D),
        try F() of
            [] -> halt(0);
            Other -> fatal("~p~n", [Other])
        catch
            error:_ -> halt(1)
        end
    after
        file:set_cwd(Cwd)
    end.

compile_patches() ->
    SubDir = determine_subdir(),
    Patches = filelib:wildcard(filename:join(SubDir, "*.patch")),
    info("Patches = ~p~n", [Patches]),
    [R || R <- [patch_and_compile(P) || P <- Patches], R =/= ok].

-spec patch_and_compile(any()) -> ok | {error, any()}.
patch_and_compile(P) ->
    Base = filename:basename(P, ".patch"),
    Mod = filename:basename(Base, ".erl"),
    Beam = Mod ++ ".beam",
    file:delete(filename:join("./ebin", filename:basename(Beam))),
    case code:where_is_file(Beam) of
        non_existing ->
            {error, {source_not_found, Base}};
        BeamFile ->
            case filelib:find_source(BeamFile) of
                {ok, SrcFile} ->
                    case patch_source(P, SrcFile) of
                        {ok, NewF} ->
                            compile(NewF, SrcFile);
                        {error, _} = Error1 ->
                            Error1
                    end;
                {error, _} = Error ->
                    Error
            end
    end.

patch_source(Patch, Src) ->
    New = filename:join(cur_dir(), filename:basename(Src)),
    _DelRes = file:delete(New),
    case lib:nonl(os:cmd("patch -i " ++ Patch ++ " -o " ++ New ++ " " ++ Src ++ " 1>/dev/null 2>/dev/null; echo $?")) of %% TODO Log `patch` stderr. Make command robust to spaces in file names.
        "0" ->
            {ok, New};
        Res ->
            {error, {patch_error, {Res, Patch}}}
    end.

compile(F, Src) ->
    SrcDir = filename:dirname(Src),
    IncDir = filename:join(filename:dirname(SrcDir), "include"),
    ok = filelib:ensure_dir("./ebin/foo"),
    case compile:file(F, [report_errors, report_warnings, debug_info,
                          {i, SrcDir}, {i, IncDir}, {outdir, "ebin"}]) of
        {ok, _Mod} ->
            ok;
        error ->
            {error, {compile_error, F}}
    end.

determine_subdir() ->
    case filelib:wildcard("*/versions.eterm") of
        [] ->
            ".";
        [_|_] = Vsns ->
            match_versions(Vsns)
    end.

match_versions([H|T]) ->
    case file:consult(H) of
        {ok, Apps} ->
            case lists:all(fun match_vsn/1, Apps) of
                true ->
                    filename:dirname(H);
                false ->
                    match_versions(T)
            end;
        {error, Reason} ->
            fatal("Cannot read ~p: ~p~n", [H, Reason])
    end;
match_versions([]) ->
    ".".


match_vsn({App, Re}) ->
    case code:lib_dir(App) of
        {error, _} ->
            false;
        Dir ->
            re_match(filename:basename(Dir), Re)
    end.

re_match(Str, Re) ->
    case re:run(Str, Re) of
        {match, _} -> true;
        nomatch    -> false
    end.


cur_dir() ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

info(Fmt, Args) ->
    try rebar_api:info(Fmt, Args)
    catch
        error:_ ->
            io:fwrite(Fmt, Args)
    end.

fatal(Fmt, Args) ->
    io:fwrite("ERROR: " ++ Fmt, Args),
    halt(1).
