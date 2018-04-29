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
-export([file/1,file/2,default_options/0]).

-define(DEFAULT_OPTS, [verbose,report]).

%% -record(comp, {opts=[]}).

%% Errors.
format_error(Error) -> io_lib:write(Error).

default_options() -> ?DEFAULT_OPTS.

%% file(Name) ->
%%      {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns} | error.
%% file(Name, Options) ->
%%      {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns} | error.
%%  Compile the LFE file Name.

file(Name) -> file(Name, default_options()).

file(Name, Opts) -> do_compile({file,Name}, Opts).

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

internal({file,Name}, Opts) -> do_file(Name, Opts).

do_file(Name, _Opts) ->
    case parse_file(Name) of
        {ok,Fs} -> {ok,Fs};
        {error,Error} -> {error,Error}
    end.

parse_file(F) ->
    case file:read_file(F) of
	{ok,Bin} ->
	    case aeva_scan:string(binary_to_list(Bin)) of
		{ok,Ts,_} -> aeva_parse:contract(Ts);
		Error -> Error
	    end;
	Error -> Error
    end.
