-module(aec_metrics_test_utils).

-export([make_port_map/2,
         set_statsd_port_config/3,
         start_statsd_loggers/1,
         stop_statsd_loggers/1,
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
    lists:keystore(logger_port_map, 1, Config,
                   {logger_port_map, PortMap}).

set_statsd_port_config(Dev, Cfg, CTConfig) ->
    case lists:keyfind(logger_port_map, 1, CTConfig) of
        {_, PM} ->
            case lists:keyfind(Dev, 1, PM) of
                {_, Port} ->
                    insert_port_config(Port, Cfg);
                _ ->
                    Cfg
            end;
        _ ->
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

start_statsd_loggers(Config0) ->
    Devs = proplists:get_value(devs, Config0, []),
    {PortMap, Config} = case lists:keyfind(logger_port_map, 1, Config0) of
                            false ->
                                NewConfig = make_port_map(Devs, Config0),
                                {_, PM} = lists:keyfind(
                                            logger_port_map, 1, NewConfig),
                                {PM, NewConfig};
                            {_, PM} ->
                                {PM, Config0}
                        end,
    ct:log("PortMap = ~p", [PortMap]),
    Loggers0 = proplists:get_value(loggers, Config, []),
    ct:log("Loggers0 = ~p", [Loggers0]),
    Loggers1 = lists:foldl(
                 fun({Id, Port}, Acc) ->
                         start_statsd_logger_(Id, Port, Acc)
                 end, Loggers0, PortMap),
    ct:log("Loggers1 = ~p", [Loggers1]),
    lists:keystore(loggers, 1, Config, {loggers, Loggers1}).

start_statsd_logger_(Id, Port, Loggers) ->
    case lists:keyfind(Id, 1, Loggers) of
        {_, Info} ->
            case lists:keyfind(port, 1, Info) of
                {_, Port} -> Loggers;
                {_, OldPort} ->
                    error({logger_on_different_port, [{id, Id},
                                                      {old_port, OldPort},
                                                      {new_port, Port}]})
            end;
        false ->
            application:ensure_all_started(exometer_core),
            {ok, Pid} = exometer_report_logger:new([{id, Id},
                                                    {input, [{mode, udp},
                                                             {port, Port}]},
                                                    {output, [{mode, ets}]}]),
            [{Id, [{port, Port},
                   {pid, Pid}]} | Loggers]
    end.

stop_statsd_loggers(_Config) ->
    Children = supervisor:which_children(exometer_report_logger_sup),
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
