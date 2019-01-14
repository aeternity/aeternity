-module(aestratum_server_session_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aestratum_server_session).
-define(JSONRPC_MODULE, aestratum_jsonrpc).
-define(NONCE_MODULE, aestratum_nonce).
-define(EXTRA_NONCE_CACHE_MODULE, aestratum_extra_nonce_cache).

session_test_() ->
    {setup,
     fun() ->
             ok = application:ensure_started(jsx)
     end,
     fun(_) ->
             ok = application:stop(jsx)
     end,
     [{generator, fun server_session/0}]}.

server_session() ->
    {foreach,
     fun() ->
             meck:new(?EXTRA_NONCE_CACHE_MODULE, [passthrough]),
             meck:new(?TEST_MODULE, [passthrough]),
             {ok, Pid} = aestratum_dummy_handler:start_link(?TEST_MODULE),
             Pid
     end,
     fun(Pid) ->
             meck:unload(?EXTRA_NONCE_CACHE_MODULE),
             meck:unload(?TEST_MODULE),
             aestratum_dummy_handler:stop(Pid)
     end,
     [fun(Pid) -> t(Pid, init()) end,
      %% connected - error
      fun(Pid) -> t(Pid, when_connected(timeout)) end,
      fun(Pid) -> t(Pid, when_connected(authorize)) end,
      fun(Pid) -> t(Pid, when_connected(submit)) end,
      fun(Pid) -> t(Pid, when_connected(not_req)) end,
      fun(Pid) -> t(Pid, when_connected(jsonrpc_errors)) end,
      %% connected - success
      fun(Pid) -> t(Pid, when_connected(configure)) end,
      fun(Pid) -> t(Pid, when_connected(subscribe)) end,
      %% configured - error
      fun(Pid) -> t(Pid, when_configured(timeout)) end,
      fun(Pid) -> t(Pid, when_configured(configure)) end,
      fun(Pid) -> t(Pid, when_configured(authorize)) end,
      fun(Pid) -> t(Pid, when_configured(submit)) end,
      fun(Pid) -> t(Pid, when_configured(not_req)) end,
      fun(Pid) -> t(Pid, when_configured(jsonrpc_errors)) end,
      %% configured - success
      fun(Pid) -> t(Pid, when_configured(subscribe)) end,
      %% subscribed - error
      fun(Pid) -> t(Pid, when_subscribed(timeout)) end,
      %% TODO: fun(Pid) -> t(Pid, when_subscribed(configure)) end,
      fun(Pid) -> t(Pid, when_subscribed(subscribe)) end,
      fun(Pid) -> t(Pid, when_subscribed(submit)) end,
      fun(Pid) -> t(Pid, when_subscribed(not_req)) end,
      fun(Pid) -> t(Pid, when_subscribed(jsonrpc_errors)) end,
      %% subscribed - success
      %% TODO: fun(Pid) -> t(Pid, when_subscribed(authorize)) end,
      %% authorized - error
      fun(Pid) -> t(Pid, when_authorized(timeout)) end,
      %% TODO: fun(Pid) -> t(Pid, when_authorized(configure)) end,
      %% TODO: fun(Pid) -> t(Pid, when_authorized(subscribe)) end,
      fun(Pid) -> t(Pid, when_authorized(authorize)) end,
      fun(Pid) -> t(Pid, when_authorized(not_req)) end,
      fun(Pid) -> t(Pid, when_authorized(jsonrpc_errors)) end
      %% authorize - success
      %% TODO: fun(Pid) -> t(Pid, when_authorized(submit)) end
     ]}.

%% T - title
%% G - test/no_test - generate test/not generate
%% E - event
%% A - action
%% S - session state
%% R - result
t(Pid, Data) ->
    Asserts =
        [begin
            R1 = result(Pid, R, aestratum_dummy_handler:handle_event(Pid, event(E))),
            case G of
                test -> {T, ?_assertEqual(R, R1)};
                no_test -> no_test
            end
         end || {T, G, E, R} <- Data],
    lists:filter(fun({_T, _Assert}) -> true;
                    (no_test) -> false end, Asserts).

event({conn, D}) when is_map(D) ->
    {ok, D1} = ?JSONRPC_MODULE:encode(D),
    {conn, D1};
event(Other) ->
    Other.

result(Pid, {_A, S}, {A1, S1}) ->
    Ks = maps:keys(S),
    S1M = maps:with(Ks, aestratum_dummy_handler:state_to_map(Pid, S1)),
    {A1, S1M};
result(Pid, {_A, S}, {A1, D1, S1}) ->
    Ks = maps:keys(S),
    S1M = maps:with(Ks, aestratum_dummy_handler:state_to_map(Pid, S1)),
    {ok, D1M} = ?JSONRPC_MODULE:decode(D1),
    {A1, D1M, S1M};
result(Pid, {_A, _D, S}, {A1, S1}) ->
    Ks = maps:keys(S),
    S1M = maps:with(Ks, aestratum_dummy_handler:state_to_map(Pid, S1)),
    {A1, S1M};
result(Pid, {_A, D, S}, {A1, D1, S1}) ->
    Ks = maps:keys(S),
    S1M = maps:with(Ks, aestratum_dummy_handler:state_to_map(Pid, S1)),
    {ok, D1M0} = ?JSONRPC_MODULE:decode(D1),
    D1M = maps:with(maps:keys(D), maybe_rsp_result(D, D1M0)),
    {A1, D1M, S1M}.

%% If type is rsp, we need to validate the result.
maybe_rsp_result(#{type := rsp, method := M}, #{type := rsp} = D1M0) ->
    {ok, D1M} = ?JSONRPC_MODULE:validate_rsp(M, D1M0),
    D1M;
maybe_rsp_result(_D, D1M0) ->
    D1M0.

init() ->
    T = <<"init - server">>,
    L = [{{conn, init},
          {no_send,
           #{phase => connected, timer_phase => connected}}
         }],
    [{T, test, E, R} || {E, R} <- L].

when_connected(timeout) ->
    T = <<"when connected - timeout">>,
    L = [{{conn, timeout},
          {stop,
           #{phase => disconnected, timer_phase => undefined,
             extra_nonce => undefined}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(authorize) ->
    T = <<"when connected - authorize">>,
    L = [{{conn, #{type => req, method => authorize, id => 1,
                   user => <<"test_user">>, password => binary:copy(<<"0">>, 64)}},
          {send,
           #{type => rsp, method => authorize, id => 1, reason => not_subscribed},
           #{phase => connected, timer_phase => connected,
            extra_nonce => undefined}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(submit) ->
    T = <<"when connected - submit">>,
    L = [{{conn, #{type => req, method => submit, id => 1,
                   user => <<"test_user">>, job_id => <<"0123456789abcdef">>,
                   miner_nonce => <<"0123456789">>, pow => lists:seq(1, 42)}},
          {send,
           #{type => rsp, method => submit, id => 1, reason => not_subscribed},
           #{phase => connected, timer_phase => connected,
            extra_nonce => undefined}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(not_req) ->
    T = <<"when connected - not_req">>,
    %% Server receives unexpected message - response.
    L = [{{conn, #{type => rsp, method => configure, id => null,
                   reason => parse_error, data => <<"foo">>}},
          {send,
           #{type => rsp, method => configure, id => null,
             reason => unknown_error, data => <<"unexpected_msg">>},
           #{phase => connected, timer_phase => connected,
             extra_nonce => undefined}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(jsonrpc_errors) ->
    T = <<"when connected - jsonrpc_errors">>,
    prep_connected(T) ++ jsonrpc_errors(T, connected, connected);
when_connected(configure) ->
    T = <<"when connected - configure">>,
    L = [{{conn, #{type => req, method => configure, id => 0, params => []}},
          {send,
           #{type => rsp, method => configure, id => 0, result => []},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(subscribe) ->
    Host = <<"ae.testpool.com">>,
    Port = 10000,
    ExtraNonceNBytes = 4,
    ExtraNonce = aestratum_nonce:new(extra, 100, ExtraNonceNBytes),
    application:set_env(aestratum, extra_nonce_nbytes, ExtraNonceNBytes),
    meck:expect(?TEST_MODULE, get_host, fun() -> Host end),
    meck:expect(?TEST_MODULE, get_port, fun() -> Port end),
    meck:expect(?EXTRA_NONCE_CACHE_MODULE, get, fun(_) -> {ok, ExtraNonce} end),

    T = <<"when connected - subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 0,
                   user_agent => <<"aeminer/1.0.0">>, session_id => null,
                   host => Host, port => Port}},
          {send,
           #{type => rsp, method => subscribe, id => 0,
             result => [null, ?NONCE_MODULE:to_bin(ExtraNonce)]},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ExtraNonce}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L].

when_configured(timeout) ->
    T = <<"when configured - timeout">>,
    L = [{{conn, timeout},
          {stop,
           #{phase => disconnected, timer_phase => undefined,
            extra_nonce => undefined}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(configure) ->
    T = <<"when configured - configure">>,
    L = [{{conn, #{type => req, method => configure, id => 1, params => []}},
          {send,
           #{type => rsp, method => configure, id => 1, reason => unknown_error},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(authorize) ->
    T = <<"when configured - authorize">>,
    L = [{{conn, #{type => req, method => authorize, id => 1,
                   user => <<"test_user">>, password => binary:copy(<<"0">>, 64)}},
          {send,
           #{type => rsp, method => authorize, id => 1, reason => not_subscribed},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(submit) ->
    T = <<"when configured - submit">>,
    L = [{{conn, #{type => req, method => submit, id => 1,
                   user => <<"test_user">>, job_id => <<"0123456789abcdef">>,
                   miner_nonce => <<"0123456789">>, pow => lists:seq(1, 42)}},
          {send,
           #{type => rsp, method => submit, id => 1, reason => not_subscribed},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(not_req) ->
    T = <<"when configured - not_req">>,
    L = [{{conn, #{type => rsp, method => configure, id => 2, result => []}},
          {send,
           #{type => rsp, method => configure, id => 2, reason => unknown_error},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(jsonrpc_errors) ->
    T = <<"when configured - jsonrpc_errors">>,
    prep_configured(T) ++ jsonrpc_errors(T, configured, configured);
when_configured(subscribe) ->
    Host = <<"ae.testpool.com">>,
    Port = 10000,
    ExtraNonceValue = 200,
    ExtraNonceNBytes = 4,
    ExtraNonce = aestratum_nonce:new(extra, ExtraNonceValue, ExtraNonceNBytes),
    application:set_env(aestratum, extra_nonce_nbytes, ExtraNonceNBytes),
    meck:expect(?TEST_MODULE, get_host, fun() -> Host end),
    meck:expect(?TEST_MODULE, get_port, fun() -> Port end),
    meck:expect(?EXTRA_NONCE_CACHE_MODULE, get, fun(_) -> {ok, ExtraNonce} end),

    T = <<"when configured - subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 0,
                   user_agent => <<"aeminer/1.0.0">>, session_id => null,
                   host => Host, port => Port}},
          {send,
           #{type => rsp, method => subscribe, id => 0,
             result => [null, ?NONCE_MODULE:to_bin(ExtraNonce)]},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ExtraNonce}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L].

when_subscribed(timeout) ->
    meck:expect(?EXTRA_NONCE_CACHE_MODULE, free, fun(_) -> ok end),

    T = <<"when subscribed - timeout">>,
    L = [{{conn, timeout},
          {stop,
           #{phase => disconnected, timer_phase => undefined,
             extra_nonce => undefined}}
         }],
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(subscribe) ->
    Host = <<"testpool.aeternity.com">>,
    Port = 12345,
    meck:expect(?TEST_MODULE, get_host, fun() -> Host end),
    meck:expect(?TEST_MODULE, get_port, fun() -> Port end),

    T = <<"when subscribed - subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 2,
                   user_agent => <<"aeminer/1.0.0">>, session_id => null,
                   host => Host, port => Port}},
          {send,
           #{type => rsp, method => subscribe, id => 2, reason => unknown_error},
           #{phase => subscribed, timer_phase => subscribed}}
         }],
    %% prep_subscribed/1 sets the ExtraNonceNBytes to 3 and ExtraNonce to 345
    %% TODO: ^^^ Maybe return the state and check extra_nonce?
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(submit) ->
    T = <<"when subscribed - submit">>,
    L = [{{conn, #{type => req, method => submit, id => 2,
                   user => <<"test_user">>, job_id => <<"0123456789abcdef">>,
                   miner_nonce => <<"0123456789">>, pow => lists:seq(1, 42)}},
          {send,
           #{type => rsp, method => submit, id => 2, reason => unauthorized_worker},
           #{phase => subscribed, timer_phase => subscribed}}
         }],
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(not_req) ->
    T = <<"when subscribed - not_req">>,
    L = [{{conn, #{type => rsp, method => configure, id => 3, result => []}},
          {send,
           #{type => rsp, method => configure, id => 3, reason => unknown_error},
           #{phase => subscribed, timer_phase => subscribed}}
         }],
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(jsonrpc_errors) ->
    T = <<"when subscribed - jsonrpc_errors">>,
    prep_subscribed(T) ++ jsonrpc_errors(T, subscribed, subscribed).

when_authorized(timeout) ->
    T = <<"when authorized - timeout">>,
    L = [{{conn, timeout},
          {no_send,
           #{phase => authorized, timer_phase => undefined}}
         }],
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(authorize) ->
    T = <<"when authorized - authorize">>,
    L = [{{conn, #{type => req, method => authorize, id => 3,
                   user => <<"test_user">>, password => binary:copy(<<"0">>, 64)}},
          {send,
           #{type => rsp, method => authorize, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(not_req) ->
    T = <<"when authorized - not_req">>,
    L = [{{conn, #{type => rsp, method => subscribe, id => 3,
                   reason => parse_error, data => null}},
          {send,
           #{type => rsp, method => subscribe, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(jsonrpc_errors) ->
    T = <<"when authorized - jsonrpc_errors">>,
    prep_authorized(T) ++ jsonrpc_errors(T, authorized, undefined).

prep_connected(T) ->
    L = [conn_init()],
    [{T, no_test, E, R} || {E, R} <- L].

prep_configured(T) ->
    L = [conn_init(),
         conn_configure(0)],
    [{T, no_test, E, R} || {E, R} <- L].

prep_subscribed(T) ->
    L = [conn_init(),
         conn_configure(0),
         conn_subscribe(1)],
    [{T, no_test, E, R} || {E, R} <- L].

prep_authorized(T) ->
    L = [conn_init(),
         conn_configure(0),
         conn_subscribe(1),
         conn_authorize(2)],
    [{T, no_test, E, R} || {E, R} <- L].

jsonrpc_errors(T, Phase, TimerPhase) ->
    L = [conn_make_parse_error(Phase, TimerPhase),
         conn_make_invalid_msg(no_id, Phase, TimerPhase),
         conn_make_invalid_msg(id, Phase, TimerPhase),
         conn_make_invalid_method(no_id, Phase, TimerPhase),
         conn_make_invalid_method(id, Phase, TimerPhase),
         conn_make_invalid_param(Phase, TimerPhase)
        ],
    [{T, test, E, R} || {E, R} <- L].

conn_init() ->
    {{conn, init},
     {no_send, #{phase => connected, timer_phase => connected}}
    }.

conn_configure(Id) ->
    {{conn, #{type => req, method => configure, id => Id, params => []}},
     {send,
      #{type => rsp, method => configure, id => Id, result => []},
      #{phase => configured, timer_phase => configured}}
    }.

conn_subscribe(Id) ->
    Host = <<"testpool.com">>,
    Port = 12345,
    ExtraNonceNBytes = 3,
    ExtraNonce = aestratum_nonce:new(extra, 345, ExtraNonceNBytes),
    application:set_env(aestratum, extra_nonce_nbytes, ExtraNonceNBytes),
    meck:expect(?TEST_MODULE, get_host, fun() -> Host end),
    meck:expect(?TEST_MODULE, get_port, fun() -> Port end),
    meck:expect(?EXTRA_NONCE_CACHE_MODULE, get, fun(_) -> {ok, ExtraNonce} end),

    {{conn, #{type => req, method => subscribe, id => Id,
              user_agent => <<"aeminer/1.0.0">>, session_id => null,
              host => Host, port => Port}},
     {send,
      #{type => rsp, method => subscribe, id => Id,
        result => [null, ?NONCE_MODULE:to_bin(ExtraNonce)]},
      #{phase => subscribed, timer_phase => subscribed,
        extra_nonce => ExtraNonce}}
    }.

conn_authorize(Id) ->
    {{conn, #{type => req, method => authorize, id => Id,
              user => <<"test_user">>, password => binary:copy(<<"0">>, 64)}},
     {send,
      #{type => rsp, method => authorize, id => Id, result => true}, %%TODO: meck
      #{phase => authorized, timer_phase => undefined}}
    }.

conn_make_parse_error(Phase, TimerPhase) ->
    {{conn, <<"some random binary">>},
     {send,
      #{type => rsp, method => undefined, id => null, reason => parse_error},
      #{phase => Phase, timer_phase => TimerPhase}}
    }.

conn_make_invalid_msg(no_id, Phase, TimerPhase) ->
    {{conn, <<"{\"jsonrpc\":\"2.0\",\"id\":\"none\"}">>},
     {send,
      #{type => rsp, method => undefined, id => null, reason => invalid_msg},
      #{phase => Phase, timer_phase => TimerPhase}}
    };
conn_make_invalid_msg(id, Phase, TimerPhase) ->
    {{conn, <<"{\"jsonrpc\":\"2.0\",\"id\":100}">>},
     {send,
      #{type => rsp, method => undefined, id => 100, reason => invalid_msg},
      #{phase => Phase, timer_phase => TimerPhase}}
    }.

conn_make_invalid_method(no_id, Phase, TimerPhase) ->
    {{conn, <<"{\"jsonrpc\":\"2.0\",\"id\":-10,\"method\":\"foo\",\"params\":[]}">>},
     {send,
      #{type => rsp, method => undefined, id => null, reason => invalid_method},
      #{phase => Phase, timer_phase => TimerPhase}}
    };
conn_make_invalid_method(id, Phase, TimerPhase) ->
    {{conn, <<"{\"jsonrpc\":\"2.0\",\"id\":200,\"method\":\"foo\",\"params\":[]}">>},
     {send,
      #{type => rsp, method => undefined, id => 200, reason => invalid_method},
      #{phase => Phase, timer_phase => TimerPhase}}
    }.

conn_make_invalid_param(Phase, TimerPhase) ->
    {{conn, <<"{\"jsonrpc\":\"2.0\",\"id\":300,\"method\":\"mining.subscribe\","
              "\"params\":[\"aeminer/1.0\",\"invalid session_id\",\"aepool.com\",9876]}">>},
     {send,
      #{type => rsp, method => subscribe, id => 300, reason => invalid_param},
      #{phase => Phase, timer_phase => TimerPhase}}
    }.

%% TODO: howto create internal error?
%%conn_make_internal_error()

