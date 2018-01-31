-module(aec_metrics).
-behaviour(exometer_report).
-behavior(exometer_report_logger).

-export([update/2,
         try_update/2]).

%% exometer_report_logger callbacks
-export([logger_init_input/1,
         logger_init_output/1,
         logger_handle_data/2]).

%% exometer_report logging callbacks
-export([exometer_init/1,
         exometer_report/5,
         exometer_subscribe/5,
         exometer_unsubscribe/4,
         exometer_info/2,
         exometer_call/3,
         exometer_cast/2,
         exometer_terminate/2,
         exometer_setopts/4,
         exometer_newentry/2]).

%% start phase API
-export([create_metrics_probes/0]).
-export([start_reporters/0]).
-export([prep_stop/1]).

-include_lib("kernel/include/inet.hrl").
-define(DEFAULT_PORT, 8125).  % default (dog-)statsd port
-define(DEFAULT_RECONNECT_INTERVAL, 10000).


-record(st, {type_map = [] :: list(),
             prefix = []   :: string()}).

-record(statsd, {
          socket        :: inet:socket() | undefined,
          tref          :: reference() | undefined,
          addr_type     :: 'inet' | 'inet6',
          address       :: inet:ip_address(),
          port          :: inet:port_number(),
          reconnect_interval = ?DEFAULT_RECONNECT_INTERVAL :: pos_integer()
         }).

%% Hard-coded default
default_dests() ->
    [log, send].

%%===================================================================
%% Metrics API
%%===================================================================

update(Metric, Value) ->
    exometer:update(Metric, Value).

-spec try_update(exometer:name(), number()) -> ok | {error, not_found}.
try_update(Metric, Value) ->
    try update(Metric, Value)
    catch
        error:_ ->
            ok
    end.

%%===================================================================
%% PROBE INITIALIZATION
%%===================================================================

create_metrics_probes() ->
    Probes = aeu_env:get_env(aecore, metrics_probes, []),
    lists:foreach(fun create_metrics_probe/1, Probes).

create_metrics_probe({Name, Module}) ->
    ok = exometer:ensure(Name, ad_hoc, Module:ad_hoc_spec()).

%%===================================================================
%% REPORTER API
%%===================================================================

start_reporters() ->
    expect(
      {ok, '$pid'}, {?LINE, start_logger},
      exometer_report_logger:new([{id, aec_metrics_logger},
                                  {input, [{mode, plugin},
                                           {module, ?MODULE},
                                           {state, []}]},
                                  {output, [{mode, plugin},
                                            {module, ?MODULE},
                                            {state, filter}]},
                                  {output, [{mode, plugin},
                                            {module, ?MODULE},
                                            {state, statsd}]},
                                  {output, [{mode, plugin},
                                            {module, ?MODULE},
                                            {state, log}]}
                                 ])),
    expect(
      ok, {?LINE, start_reporter},
      exometer_report:add_reporter(
        aec_metrics_main,
        [{module, ?MODULE},
         {intervals, [{default, 10000}]},
         {report_bulk, true}])).

prep_stop(State) ->
    %% Use disable_reporter/1 instead of remove_reporter/1 since it's
    %% synchronous. We want to avoid spurious errors due to termination
    %% order.
    expect(
      ok, {?LINE, disable_reporter},
      exometer_report:disable_reporter(aec_metrics_main)),
    State.

%%===================================================================
%% exometer_report_logger callbacks
%%===================================================================

logger_init_input(_St0) ->
    Me = self(),
    {ok, spawn_link(fun() ->
                            aec_events:subscribe(metric),
                            subscriber_loop(Me)
                    end)}.

subscriber_loop(Logger) ->
    receive
        {gproc_ps_event, metric, #{info := Data}} ->
            Logger ! {plugin, self(), Data},
            subscriber_loop(Logger)
    end.

logger_init_output(filter) ->
    {ok, filter};
logger_init_output(log) ->
    {ok, log};
logger_init_output(statsd) ->
    {ok, init_statsd()}.

logger_handle_data(#{} = Data, filter = St) ->
    apply_filter(Data, St);  % sets do_log and do_send flags
logger_handle_data(#{do_log := true} = Data, log = St) ->
    epoch_metrics:info("~s", [line(Data)]),
    {Data, St};
logger_handle_data(#{do_send := true} = Data, #statsd{} = St) ->
    statsd_handle_data(Data, St);
logger_handle_data({statsd, Msg}, St) ->
    statsd_handle_msg(Msg, St);
logger_handle_data(Data, St) ->
    {Data, St}.

%%===================================================================
%% exometer_report callbacks
%%===================================================================

exometer_init(Opts) ->
    TypeMap = proplists:get_value(type_map, Opts, []),
    Prefix  = get_opt(prefix, Opts, []),
    {ok, #st{type_map = TypeMap, prefix = Prefix}}.


exometer_subscribe(_Metric, _DataPoint, _Interval, _Extra, St) ->
    {ok, St}.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, St) ->
    {ok, St}.

exometer_report(Metric, DataPoint, Extra, Value, St) ->
    Key = metric_key(Metric, DataPoint),
    Type = case exometer_util:report_type(Key, Extra, St#st.type_map) of
               {ok, T} -> T;
               error   -> gauge % default type for statsd
           end,
    Name = name(St#st.prefix, Metric, DataPoint),
    M = #{metric => Metric,
          datapoint => DataPoint,
          extra => Extra,
          value => Value,
          key => Key,
          name => Name,
          type => Type},
    aec_events:publish(metric, M),
    {ok, St}.

exometer_call(_Req, _From, St) ->
    {ok, St}.

exometer_cast(_Msg, St) ->
    {ok, St}.

exometer_info(_Msg, St) ->
    {ok, St}.

exometer_setopts(_Metric, _Options, _Status, St) ->
    {ok, St}.

exometer_newentry(_Entry, St) ->
    {ok, St}.

exometer_terminate(_, _) ->
    ignore.

%%===================================================================
%% Internal functions
%%===================================================================

line(#{name := Name, value := Value, type := Type, extra := Extra}) ->
    [Name, ":", value(Value), "|", type(Type) | add_tags(Extra)].


add_tags(undefined) -> [];
add_tags(Extra) when is_list(Extra) ->
    case lists:keyfind(tags, 1, Extra) of
        {_, [H|T]} ->
            Str = [fmt_tag(H) | [[",", fmt_tag(Tag)] || Tag <- T]],
            ["|#" | Str];
        _ ->
            []
    end.

fmt_tag({K, V}) ->
    [value(K), ":", value(V)];
fmt_tag(K) when is_atom(K); is_binary(K); is_list(K) ->
    value(K).


type(gauge) -> "g";
type(counter) -> "c";
type(timer) -> "ms";
type(histogram) -> "h";
type(meter) -> "m";
type(set) -> "s". %% datadog specific type, see http://docs.datadoghq.com/guides/dogstatsd/#tags

metric_key(Metric,DataPoint) -> metric_key([],Metric,DataPoint).

metric_key([] , Metric, DataPoint) -> Metric ++ [ DataPoint ];
metric_key(Pfx, Metric, DataPoint) -> [ Pfx | Metric ] ++ [ DataPoint ].

name(Prefix, Metric, DataPoint) ->
    intersperse(".", lists:map(fun thing_to_list/1,
                               metric_key(Prefix, Metric, DataPoint))).

thing_to_list(X) when is_atom(X) -> atom_to_list(X);
thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_binary(X) -> X;
thing_to_list(X) when is_list(X) -> X.

value(V) when is_integer(V) -> integer_to_list(V);
value(V) when is_float(V)   -> io_lib:fwrite("~.3f", [V]);
value(_)                    -> 0.

intersperse(_, [])         -> [];
intersperse(_, [X])        -> [X];
intersperse(Sep, [X | Xs]) -> [X, Sep | intersperse(Sep, Xs)].

get_opt(K, Opts, Def) ->
    exometer_util:get_opt(K, Opts, Def).


%% Report unexpected result, but don't fail.
expect({ok,'$pid'}, _, {ok, Pid} = Ok) when is_pid(Pid) ->
    Ok;
expect(ok, _, ok) ->
    ok;
expect(Expected, Info, Other) ->
    lager:error("Expected ~p for ~p; Other = ~p", [Expected, Info, Other]),
    ok.


%%===================================================================
%% Connection setup and send
%%===================================================================

init_statsd() ->
    %% copied from exometer_report_statsd
    DefaultHost = default_host(),
    {{ok, Host},_} = {inet:gethostbyname(DefaultHost), DefaultHost},
    [IP|_]     = Host#hostent.h_addr_list,
    AddrType   = Host#hostent.h_addrtype,
    Port       = default_port(),

    S = #statsd{reconnect_interval = reconnect_interval(),
                addr_type = AddrType, address = IP, port = Port},
    #statsd{} = try_connect(S).

%% statsd plugin talking to itself
statsd_handle_msg({statsd, Msg} = Data, S) ->
    case Msg of
        reconnect ->
            {Data, try_connect(S)};
        prepare_reconnect ->
            {Data, reconnect_after(S)}
    end.

statsd_handle_data(#{} = Data, S) ->
    try_send(line(Data), S),
    {Data, S}.


try_send(_Data, #statsd{socket = undefined}) ->
    ok;
try_send(Data, #statsd{socket = Sock, address = Addr, port = Port}) ->
    case gen_udp:send(Sock, Addr, Port, Data) of
        ok -> ok;
        {error, _} = Err ->
            lager:debug("Couldn't send metric to (~p:~p/~p): ~p",
                        [Addr, Port, Sock, Err])
    end.

try_connect(#statsd{port = 0} = St) ->
    lager:debug("metrics port = 0 - don't try to connect to statsd", []),
    St#statsd{socket = undefined};
try_connect(#statsd{addr_type = AddrType} = St) ->
    case gen_udp:open(0, [AddrType]) of
        {ok, Sock} ->
            lager:debug("connected to statsd", []),
            St#statsd{socket = Sock};
        {error, _} = Error ->
            lager:debug("Error opening UDP socket: ~p", [Error]),
            prepare_reconnect(),
            St#statsd{socket = undefined}
    end.

prepare_reconnect() ->
    self() ! {exometer_report_logger, self(), {statsd, prepare_reconnect}}.

reconnect_after(#statsd{reconnect_interval = Interval} = S) ->
    Tref= erlang:send_after(
            Interval, self(),
            {exometer_report_logger, self(), {statsd, reconnect}}),
    S#statsd{tref = Tref}.

%%===================================================================
%% Env defaults
%%===================================================================

default_host() ->
    aeu_env:user_config_or_env(
      [<<"metrics">>, <<"host">>], aecore, metrics_host, gethostname()).

gethostname() ->
    {ok, H} = inet:gethostname(),
    H.

default_port() ->
    aeu_env:user_config_or_env(
      [<<"metrics">>, <<"port">>], aecore, metrics_port, ?DEFAULT_PORT).

reconnect_interval() ->
    aeu_env:user_config_or_env(
      [<<"metrics">>, <<"reconnect_interval">>],
      aecore, metrics_reconnect_interval, ?DEFAULT_RECONNECT_INTERVAL).


%%=====================================================================
%% Filter
%%=====================================================================

apply_filter(#{metric := Name, datapoint := DP} = Data, St) ->
    Dests0 = aec_metrics_rpt_dest:get_destinations(Name, DP),
    Dests = case Dests0 of
                undefined -> default_dests();
                Ds -> Ds
            end,
    DoLog = lists:member(log, Dests),
    DoSend = lists:member(send, Dests),
    {Data#{do_log => DoLog, do_send => DoSend}, St}.
