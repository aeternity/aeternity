-module(aec_test_event_handler).

-behaviour(gen_event).

-export([install/0]).
-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

install() ->
    gen_event:add_sup_handler(error_logger, ?MODULE, self()).

init(Parent) ->
    io:fwrite(user, "handler init.~n", []),
    Now = os:timestamp(),
    [Parent ! {application_started, Now, A}
     || {A,_,_} <- application:which_applications()],
    {ok, #{parent => Parent}}.

handle_event({info_report, _, {_,_,[{application,Name},
                                    {started_at, _Node}]}},
             #{parent := Parent} = State) ->
    Parent ! {application_started, os:timestamp(), Name},
    io:fwrite(user, "Reporting app started: ~p~n", [Name]),
    {ok, State};
handle_event(_E, State) ->
    {ok, State}.

handle_call(_Req, State) ->
    {ok, {error, unknown_call}, State}.

handle_info(_Msg, State) ->
    {ok, State}.

terminate(Arg, _State) ->
    io:fwrite(user, "Event handler terminating: ~p~n", [Arg]),
    ok.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.
