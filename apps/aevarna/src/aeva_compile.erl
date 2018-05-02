%%%-------------------------------------------------------------------
%%% @author Robert Virding
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%     Compiler for Varna.
%%% @end
%%% Created : 2018-04-25
%%%
%%%-------------------------------------------------------------------

-module(aeva_compile).

-export([format_error/1]).
-export([file/1,file/2,string/1,string/2,default_options/0]).

-import(lists, [member/2]).

%% Bloody useful
-define(IF(Test,True,False), case Test of true -> True; false -> False end).

-define(DEFAULT_OPTS, [verbose,report]).

-record(comp, {base="",                         %Base name
               ldir=".",                        %Varna file dir
               lfile="",                        %Varna file
               odir=".",                        %Output directory
               opts=[],                         %User options
               code=[],                         %Code last pass
               errors=[],                       %Errors
               warnings=[]                      %Warnings
              }).

%% Errors.
format_error(Error) -> io_lib:write(Error).

default_options() -> ?DEFAULT_OPTS.

%% file(Name) ->
%%      {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns} | error.
%% file(Name, Options) ->
%%      {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns} | error.
%%  Compile the Varna file Name.

file(Name) -> file(Name, default_options()).

file(Name, Opts) -> do_compile({file,Name}, Opts).

%% string(String) ->
%%      {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns} | error.
%% string(String, Options) ->
%%      {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns} | error.
%%  Compile the Varna contract in String.

string(Str) -> string(Str, default_options()).

string(Str, Opts) -> do_compile({string,Str}, Opts).

do_compile(Input, Opts) ->
    Ifun = fun () ->
                   Ret = try
                             internal(Input, Opts)
                         catch
                             error:Reason ->
                                 St = erlang:get_stacktrace(),
                                 {error,{Reason,St}}
                         end,
                   exit(Ret)
           end,
    {Pid,Ref} = spawn_monitor(Ifun),
    receive
        {'DOWN',Ref,_,Pid,Res} -> Res
    end.

%% internal(Input, Options) -> Result.

internal({file,Name}, Opts) -> do_file(Name, Opts);
internal({string,Str}, Opts) -> do_string(Str, Opts).


do_file(Name, Opts) ->
    St0 = #comp{opts=Opts},
    St1 = filenames(Name, ".aev", St0),
    case parse_file(St1#comp.lfile) of
        {ok,Fs} ->
            %% Do the actual compilation work.
            do_forms(St1#comp{code=Fs});
        {error,Error} -> {error,[Error]}
    end.

do_string(Str, Opts) ->
    St0 = #comp{opts=Opts},
    case parse_string(Str) of
	{ok,Fs} ->
            %% Do the actual compilation work.
            do_forms(St0#comp{code=Fs});
        {error,Error} -> {error,[Error]}
    end.

%% filenames(File, Suffix, State) -> State.
%%  The default output dir is the current directory unless an
%%  explicit one has been given in the options.

filenames(File, Suffix, St) ->
    %% Test for explicit outdir.
    Odir = outdir(St#comp.opts, "."),
    Ldir = filename:dirname(File),
    Base = filename:basename(File, Suffix),
    Lfile = aevfile(Ldir, Base, Suffix),
    St#comp{base=Base,
            ldir=Ldir,
            lfile=Lfile,
            odir=Odir
           }.

aevfile(".", Base, Suffix) -> Base ++ Suffix;
aevfile(Dir, Base, Suffix) ->
    filename:join(Dir, Base ++ Suffix).

outdir(Opts, Def) -> lprop(outdir, Opts, Def).

%% lprop(Key, PropList, Default) -> Value.
%%  Find Key, Val from PropList else Default.

lprop(Key, [{Key,Val}|_], _) -> Val;                %Erlang way
lprop(Key, [[Key,Val]|_], _) -> Val;                %LFE way
lprop(Key, [_|List], Def) -> lprop(Key, List, Def);
lprop(_, [], Def) -> Def.

%% parse_file(FileName) -> {ok,Forms} | {error,Errors}.

parse_file(F) ->
    case file:read_file(F) of
        {ok,Bin} -> parse_string(Bin);
        Error -> Error
    end.

parse_string(Str) when is_binary(Str) ->
    parse_string(binary_to_list(Str));
parse_string(Str) ->
    case aeva_scan:string(Str) of
	{ok,Ts,_} -> R = aeva_parse:contract(Ts), io:format("~p\n",[R]), R;
	{error,Error,_} -> {error,Error}
    end.

%% do_forms(State) ->
%%      {ok,Mod,[Core],[Warnings]} | {error,Errors,Warnings} | error.
%%  Run the actual LFE compiler passes.

do_forms(St0) ->
    %% Fill in the common compiler info.
    %% St1 = St0#comp{cinfo=compiler_info(St0)},
    Ps = passes(),
    case do_passes(Ps, St0) of
        {ok,St1} -> do_ok_return(St1);
        {error,St1} -> do_error_return(St1)
    end.

%% passes() -> [Pass].
%% do_passes(Passes, State) -> {ok,State} | {error,State}.
%%
%%  {when_flag,Flag,Cmd}    Do Cmd if Flag is or is not in the
%%  {unless_flag,Flag,Cmd}  option list.
%%
%%  {when_test,Test,Cmd}    Do Cmd if the Test function returns 'true'
%%  {unless_test,Test,Cmd}  or 'false'.
%%
%%  {do,Fun}                Call Fun and then continue.
%%
%%  {listing,PrintFun}      End compilation calling PrintFun to output
%%                          file.
%%
%%  done                    End compilation.
%%
%%  {done,PrintFun}         End compilation calling PrintFun to output
%%                          file, unless 'binary' is specified in which
%%                          current code will be returned.

passes() ->
    [
     %% Check the code.
     {do,fun do_aeva_lint/1},
     done
    ].

do_passes([{when_flag,Flag,Cmd}|Ps], #comp{opts=Opts}=St) ->
    do_passes(?IF(member(Flag, Opts), [Cmd|Ps], Ps), St);
do_passes([{unless_flag,Flag,Cmd}|Ps], #comp{opts=Opts}=St) ->
    do_passes(?IF(member(Flag, Opts), Ps, [Cmd|Ps]), St);
do_passes([{when_test,Test,Cmd}|Ps], St) ->
    do_passes(?IF(Test(St), [Cmd|Ps], Ps), St);
do_passes([{unless_test,Test,Cmd}|Ps], St) ->
    do_passes(?IF(Test(St), Ps, [Cmd|Ps]), St);
do_passes([{do,Fun}|Ps], St0) ->
    case Fun(St0) of
        {ok,St1} -> do_passes(Ps, St1);
        {error,St1} -> {error,St1}
    end;
do_passes([{listing,PrintFun}|_], St) ->
    PrintFun(St);
do_passes([done|_], St) -> {ok,St};             %Just end now
do_passes([error|_], St) -> {error,St};
do_passes([{done,Fun}|_], St) ->
    %% Print unless binary, in which case end.
    do_passes([{unless_flag,binary,{listing,Fun}}], St);
do_passes([], St) -> {ok,St}.                   %Got to the end, everything ok!

%% do_aeva_lint(State) -> {ok,State} | {error,State}.
%%  Run lint on the parsed contract.

do_aeva_lint(#comp{code=Fs,errors=Es,warnings=Ws}=St0) ->
    case aeva_lint:contract(Fs) of
        {ok,Lws} ->
            St1 = St0#comp{warnings=Ws ++ Lws},
            {ok,St1};
        {error,Les,Lws} ->
            St1 = St0#comp{errors=Es ++ Les,warnings=Ws ++ Lws},
            {error,St1}
    end.

%% do_ok_return(State) -> {ok,Contract,...}.
%% do_error_return(State) -> {error,Errors,Warnings} | error.
%%  Return from the compiler depending on whether it succeeded or not
%%  an which options were given.

do_ok_return(State) ->
    {ok,State#comp.code,State#comp.warnings}.

do_error_return(State) ->
    {error,State#comp.errors,State#comp.warnings}.
