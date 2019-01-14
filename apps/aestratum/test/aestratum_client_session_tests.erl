-module(aestratum_client_session_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aestratum_client_session).
-define(JSONRPC_MODULE, aestratum_jsonrpc).

session_test_() ->
    {setup,
     fun() ->
             ok = application:ensure_started(jsx)
     end,
     fun(_) ->
             ok = application:stop(jsx)
     end,
     [{generator, fun client_session/0}]}.

client_session() ->
    {foreach,
     fun() ->
             {ok, Pid} = aestratum_dummy_handler:start_link(?TEST_MODULE),
             Pid
     end,
     fun(Pid) ->
             aestratum_dummy_handler:stop(Pid)
     end,
     [fun(Pid) -> t(Pid, init()) end,
      %% connected - error
      fun(Pid) -> t(Pid, when_connected(timeout_0_retries)) end,
      fun(Pid) -> t(Pid, when_connected(timeout_1_retries)) end,
      fun(Pid) -> t(Pid, when_connected(jsonrpc_rsp_parse_error)) end,
      fun(Pid) -> t(Pid, when_connected(jsonrpc_rsp_invalid_msg)) end,
      fun(Pid) -> t(Pid, when_connected(jsonrpc_rsp_invalid_method)) end,
      fun(Pid) -> t(Pid, when_connected(jsonrpc_rsp_invalid_param)) end,
      %% connected - success
      fun(Pid) -> t(Pid, when_connected(success_rsp)) end,
      %% configure - error
      fun(Pid) -> t(Pid, when_configured(timeout)) end,
      fun(Pid) -> t(Pid, when_configured(jsonrpc_rsp_parse_error)) end,
      fun(Pid) -> t(Pid, when_configured(jsonrpc_rsp_invalid_msg)) end,
      fun(Pid) -> t(Pid, when_configured(jsonrpc_rsp_invalid_method)) end,
      fun(Pid) -> t(Pid, when_configured(jsonrpc_rsp_invalid_param)) end,
      %% configure - success
      fun(Pid) -> t(Pid, when_configured(success_rsp)) end,
      %% subscribe - error
      fun(Pid) -> t(Pid, when_subscribed(timeout)) end,
      fun(Pid) -> t(Pid, when_subscribed(jsonrpc_rsp_parse_error)) end,
      fun(Pid) -> t(Pid, when_subscribed(jsonrpc_rsp_invalid_msg)) end,
      fun(Pid) -> t(Pid, when_subscribed(jsonrpc_rsp_invalid_method)) end,
      fun(Pid) -> t(Pid, when_subscribed(jsonrpc_rsp_invalid_param)) end,
      fun(Pid) -> t(Pid, when_subscribed(failure_rsp)) end,
      %% subscribe - success
      fun(Pid) -> t(Pid, when_subscribed(success_rsp)) end,
      %% authorize - ???
      fun(Pid) -> t(Pid, when_authorized(timeout)) end
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
    T = <<"init - client">>,
    L = [{{conn, init},
          {send,
           #{type => req, method => configure, id => 0},
           #{phase => connected, reqs => #{0 => connected}}}
         }],
    [{T, test, E, R} || {E, R} <- L].

when_connected(timeout_0_retries) ->
    application:set_env(aestratum, max_retries, 0),
    T = <<"when connected - timeout, 0 retries">>,
    L = [{{conn, {timeout, 0, connected}},
          {stop,
           #{phase => disconnected, reqs => #{}}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(timeout_1_retries) ->
    application:set_env(aestratum, max_retries, 1),
    T = <<"when connected - timeout, 1 retries">>,
    L = [{{conn, {timeout, 0, connected}},
          {send,
           #{type => req, method => configure},
           #{phase => connected, reqs => #{1 => connected}}}
         },
         {{conn, {timeout, 1, connected}},
          {stop,
           #{phase => disconnected, reqs => #{}}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(jsonrpc_rsp_parse_error) ->
    T = <<"when connected - jsonrpc_rsp_parse_error">>,
    {E, R} = conn_make_parse_error(disconnected, #{}),
    prep_connected(T) ++ [{T, test, E, R}];
when_connected(jsonrpc_rsp_invalid_msg) ->
    T = <<"when connected - jsonrpc_rsp_invalid_msg">>,
    {E, R} = conn_make_invalid_msg(disconnected, #{}),
    prep_connected(T) ++ [{T, test, E, R}];
when_connected(jsonrpc_rsp_invalid_method) ->
    T = <<"when connected - jsonrpc_rsp_invalid_method">>,
    {E, R} = conn_make_invalid_method(disconnected, #{}),
    prep_connected(T) ++ [{T, test, E, R}];
when_connected(jsonrpc_rsp_invalid_param) ->
    T = <<"when connected - jsonrpc_rsp_invalid_param">>,
    {E, R} = conn_make_invalid_param(disconnected, #{}),
    prep_connected(T) ++ [{T, test, E, R}];
when_connected(success_rsp) ->
    T = <<"when connected - success_rsp">>,
    L = [{{conn, #{type => rsp, method => configure, id => 0, result => []}},
          {send,
           #{type => req, method => subscribe, id => 1},
           #{phase => configured, reqs => #{1 => configured}}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L].

when_configured(timeout) ->
    application:set_env(aestratum, max_retries, 1),
    T = <<"when configured - timeout">>,
    L = [{{conn, {timeout, 1, configured}},
          {send,
           #{type => req, method => subscribe, id => 2},
           #{phase => configured, reqs => #{2 => configured}}}
         },
         {{conn, {timeout, 2, configured}},
          {stop,
           #{phase => disconnected, reqs => #{}}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(jsonrpc_rsp_parse_error) ->
    T = <<"when configured - jsonrpc_rsp_parse_error">>,
    {E, R} = conn_make_parse_error(disconnected, #{}),
    prep_configured(T) ++ [{T, test, E, R}];
when_configured(jsonrpc_rsp_invalid_msg) ->
    T = <<"when configured - jsonrpc_rsp_invalid_msg">>,
    {E, R} = conn_make_invalid_msg(disconnected, #{}),
    prep_configured(T) ++ [{T, test, E, R}];
when_configured(jsonrpc_rsp_invalid_method) ->
    T = <<"when configured - jsonrpc_rsp_invalid_method">>,
    {E, R} = conn_make_invalid_method(disconnected, #{}),
    prep_configured(T) ++ [{T, test, E, R}];
when_configured(jsonrpc_rsp_invalid_param) ->
    T = <<"when configured - jsonrpc_rsp_invalid_param">>,
    {E, R} = conn_make_invalid_param(disconnected, #{}),
    prep_configured(T) ++ [{T, test, E, R}];
when_configured(success_rsp) ->
    T = <<"when configured - success_rsp">>,
    L = [{{conn, #{type => rsp, method => subscribe, id => 1,
                   result => [null, <<"01020304">>]}},
          {send,
           #{type => req, method => authorize, id => 2},
           #{phase => subscribed, reqs => #{2 => subscribed}}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L].

when_subscribed(timeout) ->
    T = <<"when subscribed - timeout">>,
    L = [{{conn, {timeout, 2, subscribed}},
          {send,
           #{type => req, method => authorize, id => 3},
           #{phase => subscribed, reqs => #{3 => subscribed}}}
         },
         {{conn, {timeout, 3, subscribed}},
          {stop,
           #{phase => disconnected, reqs => #{}}}
         }],
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(jsonrpc_rsp_parse_error) ->
    T = <<"when subscribed - jsonrpc_rsp_parse_error">>,
    {E, R} = conn_make_parse_error(disconnected, #{}),
    prep_subscribed(T) ++ [{T, test, E, R}];
when_subscribed(jsonrpc_rsp_invalid_msg) ->
    T = <<"when subscribed - jsonrpc_rsp_invalid_msg">>,
    {E, R} = conn_make_invalid_msg(disconnected, #{}),
    prep_subscribed(T) ++ [{T, test, E, R}];
when_subscribed(jsonrpc_rsp_invalid_method) ->
    T = <<"when subscribed - jsonrpc_rsp_invalid_method">>,
    {E, R} = conn_make_invalid_method(disconnected, #{}),
    prep_subscribed(T) ++ [{T, test, E, R}];
when_subscribed(jsonrpc_rsp_invalid_param) ->
    T = <<"when subscribed - jsonrpc_rsp_invalid_param">>,
    {E, R} = conn_make_invalid_param(disconnected, #{}),
    prep_subscribed(T) ++ [{T, test, E, R}];
when_subscribed(failure_rsp) ->
    T = <<"when subscribed - failure_rsp">>,
    L = [{{conn, #{type => rsp, method => authorize, id => 2, result => false}},
          {stop,
           #{phase => disconnected, reqs => #{}}}
         }],
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(success_rsp) ->
    T = <<"when subscribed - success_rsp">>,
    L = [{{conn, #{type => rsp, method => authorize, id => 2, result => true}},
          {no_send,
           #{phase => authorized, reqs => #{}}}
         }],
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L].

when_authorized(timeout) ->
    T = <<"when authorized - timeout">>,
    L = [{{conn, {timeout, 10, subscribed}},
        {no_send,
         #{phase => authorized, reqs => #{}}}
       }],
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L].

prep_connected(T) ->
    L = [conn_init()],
    [{T, no_test, E, R} || {E, R} <- L].

prep_configured(T) ->
    L = [conn_init(),
         conn_configure()],
    [{T, no_test, E, R} || {E, R} <- L].

prep_subscribed(T) ->
    L = [conn_init(),
         conn_configure(),
         conn_subscribe()],
    [{T, no_test, E, R} || {E, R} <- L].

prep_authorized(T) ->
    L = [conn_init(),
         conn_configure(),
         conn_subscribe(),
         conn_authorize()],
    [{T, no_test, E, R} || {E, R} <- L].

%% Configure request with Id 0 is sent.
%% Session state stays in connected phase, req with Id 0 has connected phase.
conn_init() ->
    {{conn, init},
     {send,
      #{type => req, method => configure, id => 0},
      #{phase => connected, reqs => #{0 => connected}}}
    }.

%% Configure response with Id 0 received, subscribe request with Id 1 is sent.
%% Session state moves from connected to configured state,
conn_configure() ->
    {{conn, #{type => rsp, method => configure, id => 0, result => []}},
     {send,
      #{type => req, method => subscribe, id => 1},
      #{phase => configured, reqs => #{1 => configured}}}
    }.

conn_subscribe() ->
    {{conn, #{type => rsp, method => subscribe, id => 1,
              result => [null, <<"010101">>]}},
     {send,
      #{type => req, method => authorize, id => 2},
      #{phase => subscribed, reqs => #{2 => subscribed}}}
    }.

conn_authorize() ->
    {{conn, #{type => rsp, method => authorize, id => 2, result => true}},
     {no_send,
      #{phase => authorized, reqs => #{}}}
    }.

conn_make_parse_error(Phase, Reqs) ->
    {{conn, <<"some random binary">>},
     {stop,
      #{phase => Phase, reqs => Reqs}}
    }.

conn_make_invalid_msg(Phase, Reqs) ->
    {{conn, <<"{\"jsonrpc\":\"2.0\",\"id\":\"none\"}">>},
     {stop,
      #{phase => Phase, reqs => Reqs}}
    }.

conn_make_invalid_method(Phase, Reqs) ->
    {{conn, <<"{\"jsonrpc\":\"2.0\",\"id\":200,\"method\":\"foo\",\"params\":[]}">>},
     {stop,
      #{phase => Phase, reqs => Reqs}}
    }.

conn_make_invalid_param(Phase, Reqs) ->
    {{conn, <<"{\"jsonrpc\":\"2.0\",\"id\":300,\"error\":[\"some invalid error\"}">>},
     {stop,
      #{phase => Phase, reqs => Reqs}}
    }.

