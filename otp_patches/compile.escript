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
            [] -> init:stop(0);
            _Other -> init:stop(-1)
        catch
            error:_ -> init:stop(-1)
        end
    after
        file:set_cwd(Cwd)
    end.

compile_patches() ->
    Patches = filelib:wildcard("*.patch"),
    info("Patches = ~p~n", [Patches]),
    [R || R <- [patch_and_compile(P) || P <- Patches], R =/= ok].

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
                        Error1 ->
                            Error1
                    end;
                Error ->
                    Error
            end
    end.

patch_source(Patch, Src) ->
    New = filename:join(cur_dir(), filename:basename(Src)),
    _DelRes = file:delete(New),
    {ok,_} = file:copy(Src, New),
    case os:cmd("patch <" ++ Patch) of
        "patching file" ++ _ ->
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

cur_dir() ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

info(Fmt, Args) ->
    try rebar_api:info(Fmt, Args)
    catch
        error:_ ->
            io:fwrite(Fmt, Args)
    end.
