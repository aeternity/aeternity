-module(aec_metrics_test_utils).

-export([make_port_map/2,
         port_map/1,
         maybe_set_statsd_port_in_user_config/3,
         start_statsd_loggers/1,
         stop_statsd_loggers/0,
         fetch_data/0]).

make_port_map(Devs, Config) ->
    Ports = case os:getenv("AE_STATSD_PORTS") of
                false ->
                    lists:seq(11010, 11020);
                Str ->
                    [binary_to_integer(S) ||
                        S <- re:split(Str, <<"\\h*,\\h*">>, [])]
            end,
    ct:log("Ports = ~p", [Ports]),
    PortMap = zip(Devs, Ports),
    ct:log("PortMap = ~p", [PortMap]),
    false = lists:keyfind(logger_port_map, 1, Config),
    lists:keystore(logger_port_map, 1, Config,
                   {logger_port_map, PortMap}).

port_map(Config) ->
    {_, PortMap} = lists:keyfind(logger_port_map, 1, Config),
    PortMap.

maybe_set_statsd_port_in_user_config(Dev, Cfg, CTConfig) ->
    case lists:keyfind(logger_port_map, 1, CTConfig) of
        {_, PM} ->
            case lists:keyfind(Dev, 1, PM) of
                {_, Port} ->
                    insert_port_config(Port, Cfg);
                false ->
                    Cfg
            end;
        false ->
            Cfg
    end.

insert_port_config(Port, Cfg) ->
    ct:log("insert_port_config(~p, ~p)", [Port, Cfg]),
    case maps:find(<<"metrics">>, Cfg) of
        {ok, MMap} ->
            Cfg#{<<"metrics">> => MMap#{<<"port">> => Port}};
        error ->
            Cfg#{<<"metrics">> => #{<<"port">> => Port}}
    end.

start_statsd_loggers(PortMap) ->
    ct:log("PortMap = ~p", [PortMap]),
    [] = loggers(),
    Loggers1 = lists:map(
                 fun({Id, Port}) ->
                         start_statsd_logger_(Id, Port)
                 end, PortMap),
    ct:log("Loggers1 = ~p", [Loggers1]),
    ok.

start_statsd_logger_(Id, Port) ->
    {ok, Pid} = exometer_report_logger:new([{id, Id},
                                            {input, [{mode, udp},
                                                     {port, Port}]},
                                            {output, [{mode, ets}]}]),
    {Id, [{port, Port},
          {pid, Pid}]}.

loggers() ->
    supervisor:which_children(exometer_report_logger_sup).

stop_statsd_loggers() ->
    Children = loggers(),
    ct:log("Logger Children = ~p", [Children]),
    [ok = stop_logger(C) || C <- Children],
    ok.

stop_logger({_, Pid, _, _} = Child) ->
    ct:log("Stopping logger child ~p", [Child]),
    ok = supervisor:terminate_child(exometer_report_logger_sup, Pid).

fetch_data() ->
    [{proplists:get_value(id, I),
      get_ets(I)} || {_, I} <- exometer_report_logger:info()].

get_ets(I) ->
    case lists:keyfind(output, 1, I) of
        {_, O} ->
            case lists:keyfind(ets, 1, O) of
                {_, E} ->
                    case lists:keyfind(tab, 1, E) of
                        {_, Tab} ->
                            ets:tab2list(Tab);
                        _ ->
                            no_tab
                    end;
                _ ->
                    no_ets
            end;
        _ ->
            no_output
    end.

%% Like lists:zip/2, but accepts a longer list in the 2nd arg
zip([H|T], [H1|T1]) ->
    [{H, H1} | zip(T, T1)];
zip([], _) ->
    [].
