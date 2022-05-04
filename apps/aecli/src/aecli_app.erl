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

    %% Create listen socket path based on node name with callback module aecli
    %% ecli socket related process will be created under the cli application
    %% simple one for one sup
    case os:type() of
        {win32, _} ->
            not_supported;
        _ ->
            Path = ecli_path(),
            {ok, _} = ecli:open(Path, aecli),
            lager:info("Listening for CLI connections at: ~s", [Path])
    end,
    {ok, Pid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
ecli_path() ->
    case node_name() of
        "nonode" ->
            "/var/tmp/mgmtd.cli.socket";
        Node ->
            "/var/tmp/mgmtd." ++ Node ++ ".cli.socket"
    end.

node_name() ->
    [N,_H] = re:split(atom_to_list(node()), "@", [{return,list}]),
    N.