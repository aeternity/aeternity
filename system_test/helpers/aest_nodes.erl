-module(aest_nodes).

-include_lib("eunit/include/eunit.hrl").

%=== EXPORTS ===================================================================

%% Common Test API exports
-export([ct_setup/1]).
-export([ct_cleanup/1]).

%% QuickCheck API exports
-export([eqc_setup/2]).
-export([eqc_cleanup/1]).

%% Generic API exports
-export([setup_nodes/2]).
-export([start_node/2]).
-export([stop_node/3]).
-export([kill_node/2]).
-export([extract_archive/4]).
-export([run_cmd_in_node_dir/3]).
-export([run_cmd_in_node_dir/4]).
-export([connect_node/3]).
-export([disconnect_node/3]).
-export([get_service_address/3]).
-export([get_internal_address/3]).
-export([get_node_pubkey/2]).
-export([export/3]).
-export([read_metric/3]).

%% Helper function exports
-export([shared_temp_file/2]).
-export([read_last_metric/2]).
-export([cluster/2]).
-export([spec/3]).
-export([request/3]).
-export([request/4]).
-export([get/5]).
-export([get_block/2]).
-export([get_top/1]).
-export([wait_for_value/4]).
-export([wait_for_time/3]).
-export([wait_for_time/4]).
-export([wait_for_startup/3]).
-export([time_to_ms/1]).
-export([assert_in_sync/1]).

%=== MACROS ====================================================================

-define(BACKENDS, [aest_docker]).
-define(CALL_TAG, ?MODULE).
-define(CT_CONF_KEY, node_manager).
-define(CALL_TIMEOUT, 120000).
-define(NODE_TEARDOWN_TIMEOUT, 0).
-define(DEFAULT_HTTP_TIMEOUT, 3000).

-define(BASE_SPEC, #{
    backend => aest_docker,
    source  => {pull, "aeternity/epoch:local"}
}).

%% AWK script to keep only error, critical, alert and emergency log lines with
%% all the extra lines following the log lines.
%% FIXME: Temporarily ignore dispatch_worker errors, remove when PT-159071812 fixes it.
%% Example of ignored lines:
%% 2018-07-10 15:48:59.649 [error] <0.1270.0>@aec_conductor:dispatch_worker:394 Disallowing dispatch of additional create_key_block_candidate worker
%% 2018-07-10 15:52:10.864 [error] <0.1270.0>@aec_conductor:dispatch_worker:394 Disallowing dispatch of additional micro_sleep worker
-define(EPOCH_LOG_SCAN_AWK_SCRIPT, "
    /^.*\\[error\\].*aec_conductor:dispatch_worker.*Disallowing dispatch of additional.*$/ {
      matched = 1
      if (buff != \"\") {
        buff = \"\"
      }
    }
    /^.*\\[(error|critical|alert|emergency)\\].*$/ {
      if (!matched) {
        matched = 1
        buff = $0
      }
    }
    /^.*\\[(debug|info|notice|warning)\\].*$/ {
      matched = 1
      if (buff != \"\") {
        print buff
        buff = \"\"
      }
    }
    {
      if (!matched && (buff != \"\")) {
        buff = buff \"\\n\" $0
      }
      matched = 0
    }"
).

%% AWK script to filter out the crash from eper/watchdog
-define(CRASH_LOG_SCAN_AWK_SCRIPT, "
    /^[-0-9: ]*=ERROR REPORT====$/ {
        matched = 1
        state = 1
        if (buff != \"\") print buff
        buff = $0
    }
    /^Error in process <[0-9.]*> on node epoch@localhost with exit value:$/ {
        if (state == 1) {
            matched = 1
            state = 2
            buff = buff \"\\n\" $0
        }
    }
    {
        if (!matched) {
            if (buff != \"\") print buff
            print $0
            buff = \"\"
            state = 0
        }
        matched = 0
    }"
).

%=== TYPES ====================================================================

-type test_ctx() :: pid() | proplists:proplist().
-type node_service() :: ext_http | int_http | int_ws.
-type http_path() :: [atom() | binary() | number()] | binary().
-type http_query() :: #{atom() | binary() => atom() | binary()}.
-type json_object() :: term().
-type milliseconds() :: non_neg_integer().
-type path() :: binary() | string().
-type peer_spec() :: atom() | binary().

-type node_spec() :: #{
    % The unique name of the node
    name    := atom(),
    % If peer is given as an atom it is expected to be a node name,
    % if given as a binary it is expected to be the external URL of the peer.
    peers   := [peer_spec()],
    backend := aest_docker,

%% When `backend` is `aest_docker`:

    % The source of the docker image
    source  := {pull, binary() | string()},
    % Public/private peer key can be specified explicity for the node.
    % Both are required and will be saved, overriding any present keys.
    pubkey => binary(),
    privkey => binary()
}.

-export_type([test_ctx/0]).

%=== COMMON TEST API FUNCTIONS =================================================

%% @doc Setups the the node manager for Common Test.
%% The CT config passed as argument is returned with extra values used
%% to contact with the node manager. This config must be passed to all
%% the the other functions as the `Ctx` parameter.
-spec ct_setup(proplists:proplist()) -> proplists:proplist().
ct_setup(Config) ->
    {data_dir, DataDir} = proplists:lookup(data_dir, Config),
    {priv_dir, PrivDir} = proplists:lookup(priv_dir, Config),
    ct:log("Node logs can be found here: ~n<a href=\"file://~s\">~s</a>",
        [PrivDir, PrivDir]
    ),
    LogFun = fun(Fmt, Args) -> ct:log(Fmt, Args) end,
    case aest_nodes_mgr:start_link([aest_docker], #{ test_id => uid(),
                                                     log_fun => LogFun,
                                                     data_dir => DataDir,
                                                     temp_dir => PrivDir}) of
        {ok, Pid} -> [{?CT_CONF_KEY, Pid} | Config];
        {error, Reason} ->
            erlang:error({system_test_setup_failed, [{reason, Reason}]})
    end.

%% @doc Stops the node manager and all the nodes that were started.
-spec ct_cleanup(test_ctx()) -> ok.
ct_cleanup(Ctx) ->
    Pid = ctx2pid(Ctx),
    Result = validate_logs(Ctx),
    call(Pid, dump_logs),
    call(Pid, cleanup),
    call(Pid, stop),
    wait_for_exit(Pid, ?CALL_TIMEOUT),
    case Result of
        {error, Reason} ->
            %% returning fail will cause common test to see it as test failure 
            {fail, Reason};
        ok -> ok
    end.

%=== QICKCHECK API FUNCTIONS ===================================================

%% @doc Setups the node manager for Quick Check tests.
-spec eqc_setup(path(), path()) -> test_ctx().
eqc_setup(DataDir, TempDir) ->
    case aest_nodes_mgr:start([aest_docker], #{data_dir => DataDir, temp_dir => TempDir}) of
        {ok, Pid} -> Pid;
        {error, Reason} ->
            erlang:error({system_test_setup_failed, [{reason, Reason}]})
    end.

%% @doc Stops the node manager for QuickCheck tests.
-spec eqc_cleanup(test_ctx()) -> ok.
eqc_cleanup(Ctx) ->
    Pid = ctx2pid(Ctx),
    Result = validate_logs(Ctx),
    call(Pid, cleanup),
    call(Pid, stop),
    wait_for_exit(Pid, 120000),
    case Result of
        {error, Reason} -> erlang:error(Reason);
        ok -> ok
    end.

%=== GENERIC API FUNCTIONS =====================================================

%% @doc Creates and setups a list of nodes.
%% The nodes are not started, use `start_node/2` for that.
-spec setup_nodes([node_spec()], test_ctx()) -> ok.
setup_nodes(NodeSpecs, Ctx) when is_list(NodeSpecs) ->
    call(ctx2pid(Ctx), {setup_nodes, NodeSpecs}).

%% @doc Starts a node previously setup.
-spec start_node(atom(), test_ctx()) -> ok.
start_node(NodeName, Ctx) ->
    call(ctx2pid(Ctx), {start_node, NodeName}).

%% @doc Stops a node previously started with explicit timeout (in milliseconds)
%% after which the node will be killed.
-spec stop_node(atom(), milliseconds() | infinity, test_ctx()) -> ok.
stop_node(NodeName, Timeout, Ctx) ->
    call(ctx2pid(Ctx), {stop_node, NodeName, Timeout}).

%% @doc Kills a node.
-spec kill_node(atom(), test_ctx()) -> ok.
kill_node(NodeName, Ctx) ->
    call(ctx2pid(Ctx), {kill_node, NodeName}).

extract_archive(NodeName, Path, Archive, Ctx) ->
    call(ctx2pid(Ctx), {extract_archive, NodeName, Path, Archive}).

run_cmd_in_node_dir(NodeName, Cmd, Ctx) ->
    call(ctx2pid(Ctx), {run_cmd_in_node_dir, NodeName, Cmd, 5000}).

run_cmd_in_node_dir(NodeName, Cmd, Timeout, Ctx) ->
    call(ctx2pid(Ctx), {run_cmd_in_node_dir, NodeName, Cmd, Timeout}).

%% @doc Connect a node to a network.
-spec connect_node(atom(), atom(), test_ctx()) -> ok.
connect_node(NodeName, NetName, Ctx) ->
    call(ctx2pid(Ctx), {connect_node, NodeName, NetName}).

%% @doc Disconnect a node from a network.
-spec disconnect_node(atom(), atom(), test_ctx()) -> ok.
disconnect_node(NodeName, NetName, Ctx) ->
    call(ctx2pid(Ctx), {disconnect_node, NodeName, NetName}).

%% @doc Retrieves the address of a given node's service.
-spec get_service_address(atom(), node_service(), test_ctx()) -> binary().
get_service_address(NodeName, Service, Ctx) ->
    call(ctx2pid(Ctx), {get_service_address, NodeName, Service}).

%% @doc Retrieves the internal address of a given node's service.
-spec get_internal_address(atom(), node_service(), test_ctx()) -> binary().
get_internal_address(NodeName, Service, Ctx) ->
    call(ctx2pid(Ctx), {get_internal_address, NodeName, Service}).

-spec get_node_pubkey(atom(), test_ctx()) -> binary().
get_node_pubkey(NodeName, Ctx) ->
    call(ctx2pid(Ctx), {get_node_pubkey, NodeName}).

%% @doc Performs and HTTP get on a node service (ext_http or int_http).
-spec http_get(atom(), ext_http | int_http, http_path(), http_query(), test_ctx()) ->
        {ok, pos_integer(), json_object()} | {error, term()}.
http_get(NodeName, Service, Path, Query, Ctx) ->
    Addr = get_service_address(NodeName, Service, Ctx),
    http_addr_get(Addr, Path, Query).

export(NodeName, Name, Ctx) ->
    call(ctx2pid(Ctx), {export, NodeName, Name}).

read_metric(NodeName, MetricName, Ctx) ->
    call(ctx2pid(Ctx), {read_metric, NodeName, MetricName}).

%=== HELPER FUNCTIONS ==========================================================

%% @doc Return a tuple with the path of a temporary file in the given node
%% context and in the host context. This temporary file will ve in the log
%% directory for now.
-spec shared_temp_file(atom(), string() | binary()) -> {binary(), binary()}.
shared_temp_file(NodeName, FileName) ->
    {HostLogPath, GuestLogPath} = aest_nodes_mgr:get_log_path(NodeName),
    HostFilePath = filename:join(HostLogPath, FileName),
    GuestFilePath = filename:join(GuestLogPath, FileName),
    {HostFilePath, GuestFilePath}.

read_last_metric(NodeName, MetricName) ->
    {LogPath, _} = aest_nodes_mgr:get_log_path(NodeName),
    MetricsLogPath = binary_to_list(filename:join(LogPath, "epoch_metrics.log")),
    case filelib:is_file(MetricsLogPath) of
        false -> undefined;
        true ->
            EscapedName = escap_for_regex(MetricName),
            Command = "grep '[0-9:\\.]* " ++ EscapedName ++ ":.*' '"
                ++ MetricsLogPath ++ "' | tail -n 1 | sed 's/^[0-9:\\.]*.*:\\([0-9]*\\)|.*$/\\1/'",
            case os:cmd(Command) of
                "" -> undefined;
                ValueStr ->
                    ValueStripped = string:strip(ValueStr, right, $\n),
                    list_to_integer(ValueStripped)
            end
    end.


cluster(Names, Spec) -> [spec(N, Names -- [N], Spec) || N <- Names].

spec(Name, Peers, Spec) ->
    maps:merge(maps:merge(?BASE_SPEC, Spec), #{name => Name, peers => Peers}).

request(Node, Id, Params) ->
    aehttp_client:request(Id, Params, [
        {ext_http, aest_nodes_mgr:get_service_address(Node, ext_http)},
        {ct_log, true}
    ]).

%% @doc Performs an HTTP get request on the node external API.
-spec request(atom(), http_path(), http_query(), test_ctx) -> json_object().
request(NodeName, Path, Query, Ctx) ->
    get(NodeName, ext_http, Path, Query, Ctx).

%% @doc Performs an HTTP get request on a node HTTP service.
-spec get(atom(), int_http | ext_http, http_path(), http_query(), test_ctx()) -> json_object().
get(NodeName, Service, Path, Query, Ctx) ->
    case http_get(NodeName, Service, Path, Query, Ctx) of
        {ok, 200, Response} -> Response;
        {ok, Status, _Response} -> error({unexpected_status, Status});
        {error, Reason} -> error({http_error, Reason})
    end.

get_block(NodeName, Height) ->
    case request(NodeName, 'GetKeyBlockByHeight', #{height => Height}) of
        {ok, 200, Block} -> Block;
        {ok, 404, _} -> undefined
    end.

get_top(NodeName) ->
    {ok, 200, Top} = request(NodeName, 'GetTop', #{}),
    Top.

-spec wait_for_value({balance, binary(), non_neg_integer()}, [atom()], milliseconds(), test_ctx()) -> ok;
                    ({contract_tx, binary(), non_neg_integer()}, [atom()], milliseconds(), test_ctx()) -> ok;
                    ({height, non_neg_integer()}, [atom()], milliseconds(), test_ctx()) -> ok.
wait_for_value({balance, PubKey, MinBalance}, NodeNames, Timeout, _Ctx) ->
    CheckF =
        fun(Node) ->
                case request(Node, 'GetAccountBalance', #{address => PubKey}) of
                    {ok, 200, #{balance := Balance}} when Balance >= MinBalance -> {done, #{PubKey => Balance}};
                    _ -> wait
                end
        end,
    wait_for_value(CheckF, NodeNames, [], 500, Timeout);
wait_for_value({height, MinHeight}, NodeNames, Timeout, _Ctx) ->
    Start = erlang:system_time(millisecond),
    CheckF =
        fun(Node) ->
                case request(Node, 'GetKeyBlockByHeight', #{height => MinHeight}) of
                    {ok, 200, Block} -> {done, Block};
                    _ -> wait
                end
        end,
    Result = wait_for_value(CheckF, NodeNames, [], 500, Timeout),
    Duration = (erlang:system_time(millisecond) - Start) / 1000,
    ct:log("Height ~p reached on nodes ~p after ~.2f seconds",
        [MinHeight, NodeNames, Duration]
    ),
    Result.

wait_for_time(height, NodeNames, Time) ->
    wait_for_time(height, NodeNames, Time, #{}).

wait_for_time(height, NodeNames, TimeUnit, Opts) ->
    Time = time_to_ms(TimeUnit),
    Interval = time_to_ms(maps:get(interval, Opts, {seconds, 10})),
    ProgressFun = fun(Elapsed) ->
        [{_Node, Lowest}|_] = Tops = lists:sort(fun({_, A}, {_, B}) ->
            maps:get(height, A) =< maps:get(height, B)
        end, [{N, get_top(N)} || N <- NodeNames]),
        ct:log("Heights after ~p s: ~p", [
            floor(Elapsed / 1000),
            [{N, H} || {N, #{height := H}} <- Tops]
        ]),
        Lowest
    end,
    Block = repeat(ProgressFun, Interval, Time),
    maps:get(height, Block).

wait_for_startup(Nodes, Height, Cfg) ->
    StartupTimeout = proplists:get_value(startup_timeout, Cfg),
    wait_for_value({height, Height}, Nodes, StartupTimeout, Cfg).

repeat(Fun, Interval, Max) ->
    Start = erlang:system_time(millisecond),
    timer:sleep(Interval),
    repeat(Fun, Interval, Max, Start).

repeat(Fun, Interval, Time, Start) ->
    Elapsed = erlang:system_time(millisecond) - Start,
    case Elapsed >= Time of
        true ->
            Fun(Elapsed);
        false ->
            timer:sleep(Interval),
            _Result = Fun(Elapsed),
            repeat(Fun, Interval, Time, Start)
    end.

time_to_ms(Time) when is_integer(Time) -> Time;
time_to_ms({milliseconds, Time})       -> Time;
time_to_ms({seconds, Time})            -> Time * 1000;
time_to_ms({minutes, Time})            -> Time * 1000 * 60;
time_to_ms({hours, Time})              -> Time * 1000 * 60 * 60.

assert_in_sync(Blocks) when is_map(Blocks) ->
    assert_in_sync(maps:values(Blocks));
assert_in_sync([_Block]) ->
    ok;
assert_in_sync([B1, B2|Blocks]) ->
    ?assertEqual(B1, B2),
    assert_in_sync([B2|Blocks]).

%=== INTERNAL FUNCTIONS ========================================================

escap_for_regex(Str) ->
    lists:flatten(re:replace(Str, "[\\\\^\\$\\.\\|\\(\\)\\?\\*\\+\\{\\-\\[\\]]",
                             "\\\\&", [global, {return, list}])).

uid() ->
    iolist_to_binary([[io_lib:format("~2.16.0B",[X])
                       || <<X:8>> <= crypto:strong_rand_bytes(8) ]]).

ctx2pid(Pid) when is_pid(Pid) -> Pid;
ctx2pid(Props) when is_list(Props) ->
    case proplists:lookup(?CT_CONF_KEY, Props) of
        {?CT_CONF_KEY, Pid} when is_pid(Pid) -> Pid;
        _ ->
            erlang:error({system_test_not_setup, []})
    end.

call(Pid, Msg) ->
    case gen_server:call(Pid, Msg, ?CALL_TIMEOUT) of
        {'$error', Reason, Stacktrace} ->
            erlang:raise(throw, Reason, Stacktrace);
        Reply ->
            Reply
    end.

wait_for_exit(Pid, Timeout) ->
    Ref = erlang:monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _Reason} -> ok
    after Timeout -> error({process_not_stopped, Pid})
    end.

make_expiration(Timeout) ->
    {os:timestamp(), Timeout}.

assert_expiration({StartTime, Timeout}) ->
    Now = os:timestamp(),
    Delta = timer:now_diff(Now, StartTime),
    case Delta > (Timeout * 1000) of
        true -> error(timeout);
        false -> ok
    end.

wait_for_value(CheckF, Nodes, Rem, Delay, Timeout) ->
    Expiration = make_expiration(Timeout),
    wait_for_value(CheckF, Nodes, Rem, Delay, Expiration, #{}).

wait_for_value(_CheckF, [], [], _Delay, _Expiration, Results) -> Results;
wait_for_value(CheckF, [], Rem, Delay, Expiration, Results) ->
    assert_expiration(Expiration),
    timer:sleep(Delay),
    wait_for_value(CheckF, lists:reverse(Rem), [], Delay, Expiration, Results);
wait_for_value(CheckF, [Node | Nodes], Rem, Delay, Expiration, Results) ->
    case CheckF(Node) of
        {done, Result} ->
            wait_for_value(CheckF, Nodes, Rem, Delay, Expiration, maps:put(Node, Result, Results));
        wait ->
            wait_for_value(CheckF, Nodes, [Node | Rem], Delay, Expiration, Results)
    end.

http_addr_get(Addr, Path, Query) ->
    http_send(get, Addr, Path, Query, [], <<>>, #{}).

http_send(Method, Addr, Path, Query, Headers, Body, Opts) ->
    Timeout = maps:get(timeout, Opts, ?DEFAULT_HTTP_TIMEOUT),
    HttpOpts = [{recv_timeout, Timeout}],
    case hackney:request(Method, url(Addr, Path, Query), Headers, Body, HttpOpts) of
        {error, _Reason} = Error -> Error;
        {ok, Status, _RespHeaders, ClientRef} ->
            case hackney_json_body(ClientRef) of
                {error, _Reason} = Error -> Error;
                {ok, Response} -> {ok, Status, Response}
            end
    end.

url(Base, Path, QS) when is_list(Path) ->
    hackney_url:make_url(Base, [to_binary(P) || P <- Path], maps:to_list(QS));
url(Base, Item, QS) ->
    url(Base, [Item], QS).

to_binary(Term) when is_atom(Term) -> atom_to_binary(Term, utf8);
to_binary(Term) when is_integer(Term) -> integer_to_binary(Term);
to_binary(Term)                    -> Term.

hackney_json_body(ClientRef) ->
    case hackney:body(ClientRef) of
        {error, _Reason} = Error -> Error;
        {ok, BodyJson} -> decode(BodyJson)
    end.

decode(<<>>) -> {ok, undefined};
decode(Data) -> decode_json(Data).

decode_json(Data) ->
    try jsx:decode(Data, [{labels, attempt_atom}, return_maps]) of
        JsonObj -> {ok, JsonObj}
    catch
        error:badarg -> {error, {bad_json, Data}}
    end.

validate_logs(Cfg) ->
    Logs = aest_nodes_mgr:get_log_paths(),
    maps:fold(fun(NodeName, {LogPath, _}, Result) ->
        Result1 = check_crash_log(NodeName, LogPath, Cfg, Result),
        Result2 = check_log_for_errors(NodeName, LogPath, "epoch.log", Cfg, Result1),
        Result3 = check_log_for_errors(NodeName, LogPath, "epoch_sync.log", Cfg, Result2),
                  check_log_for_errors(NodeName, LogPath, "epoch_mining.log", Cfg, Result3)
    end, ok, Logs).

check_crash_log(NodeName, LogPath, Cfg, Result) ->
    LogFile = binary_to_list(filename:join(LogPath, "crash.log")),
    case filelib:is_file(LogFile) of
        false -> Result;
        true ->
            case filelib:file_size(LogFile) of
                0 -> Result;
                _ ->
                    Command = "awk '" ?CRASH_LOG_SCAN_AWK_SCRIPT "' '" ++ LogFile ++ "'",
                    case os:cmd(Command) of
                        "" -> Result;
                        ErrorLines ->
                            aest_nodes_mgr:log("Node ~p's crash logs is not empty:~n~s",
                                               [NodeName, ErrorLines]),
                            case proplists:get_value(verify_logs, Cfg, true) of
                                true -> {error, crash_log_not_empty};
                                false -> Result
                            end
                    end
            end
    end.

check_log_for_errors(NodeName, LogPath, LogName, Cfg, Result) ->
    LogFile = binary_to_list(filename:join(LogPath, LogName)),
    case filelib:is_file(LogFile) of
        false -> Result;
        true ->
            Command = "awk '" ?EPOCH_LOG_SCAN_AWK_SCRIPT "' '" ++ LogFile ++ "'",
            case os:cmd(Command) of
                "" -> Result;
                ErrorLines ->
                    aest_nodes_mgr:log("Node ~p's log ~p contains errors:~n~s",
                                       [NodeName, LogName, ErrorLines]),
                    case proplists:get_value(verify_logs, Cfg, true) of
                        true -> {error, log_has_errors};
                        false -> Result
                    end

            end
    end.
