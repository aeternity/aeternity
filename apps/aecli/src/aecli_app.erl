%%%-------------------------------------------------------------------
%% @doc aecli OTP App
%% @end
%%%-------------------------------------------------------------------

-module(aecli_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = aecli_sup:start_link(),
    %% This starts a process under the cli application simple one for one
    %% Does seem ideal..
    {ok, _Pid} = ecli:open("/var/tmp/mgmtd.cli.socket", aecli),
    {ok, Pid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================