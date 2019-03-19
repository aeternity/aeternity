-module(aestratum_session_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aestratum_session).

-define(ENV_MODULE, aestratum_env).
-define(JSONRPC_MODULE, aestratum_jsonrpc).
-define(NONCE_MODULE, aestratum_nonce).
-define(TARGET_MODULE, aestratum_target).
-define(EXTRA_NONCE_CACHE_MODULE, aestratum_extra_nonce_cache).
-define(USER_REGISTER_MODULE, aestratum_user_register).
-define(JOB_MODULE, aestratum_job).
-define(JOB_QUEUE_MODULE, aestratum_job_queue).
-define(MINER_MODULE, aestratum_miner).
-define(AESTRATUM_MODULE, aestratum).

-define(HOST_VALID, <<"pool.aeternity.com">>).
-define(HOST_INVALID, <<>>).

-define(PORT_VALID, 12345).
-define(PORT_INVALID, 100000).

-define(USER_AGENT, <<"aeminer/1.2.3">>).

-define(EXTRA_NONCE_BYTES_VALID, 4).
-define(EXTRA_NONCE_BYTES_INVALID, 8).

-define(EXTRA_NONCE, ?NONCE_MODULE:new(extra, 16#aabbccdd, 4)).

-define(MINER_NONCE, ?NONCE_MODULE:new(miner, 16#11223344, 4)).
-define(MINER_NONCE_DUPLICATE, ?NONCE_MODULE:new(miner, 0, 4)).
-define(MINER_NONCE_BIN_INVALID, <<"000000">>).

-define(POW, lists:seq(1, 42)).
-define(POW_INVALID, lists:seq(101, 142)).
-define(POW_DUPLICATE, lists:seq(201, 242)).
-define(POW_HIGH_TARGET, lists:seq(1001, 1042)).
-define(POW_SHARE_TARGET, lists:seq(1101, 1142)).
-define(POW_BLOCK_TARGET, lists:seq(1201, 1242)).

-define(ACCOUNT_IN_REGISTER, <<"ak_1111111111111111111111111111111111111111111111111">>).
-define(ACCOUNT_IN_REGISTER_DIFFERENT_CLIENT,
        <<"ak_111111111111111111111111111111111111111eeeeeeeabc">>).
-define(ACCOUNT_NOT_IN_REGISTER, <<"ak_2222222222222222222222222222222222222222222222222">>).
-define(ACCOUNT_NOT_IN_REGISTER_WITH_EXHAUSTED_WORKER_COUNT,
        <<"ak_111111111111111111111111111111111111111fffffffabc">>).

-define(WORKER_IN_REGISTER, <<"worker1">>).
-define(WORKER_NOT_IN_REGISTER, <<"worker2">>).

-define(USER_IN_REGISTER, {?ACCOUNT_IN_REGISTER, ?WORKER_IN_REGISTER}).
-define(USER_NOT_IN_REGISTER, {?ACCOUNT_NOT_IN_REGISTER, ?WORKER_NOT_IN_REGISTER}).
-define(USER_NOT_IN_REGISTER_WITH_EXHAUSTED_WORKER_COUNT,
        {?ACCOUNT_NOT_IN_REGISTER_WITH_EXHAUSTED_WORKER_COUNT, ?WORKER_NOT_IN_REGISTER}).

-define(BLOCK_HASH1, binary:copy(<<"1">>, 64)).
-define(BLOCK_HASH2, binary:copy(<<"2">>, 64)).

-define(BLOCK_VERSION, 1).

-define(BLOCK_TARGET1, 16#ffff00000000000000000000000000000000).
-define(BLOCK_TARGET2, 16#ffee00000000000000000000000000000000).

-define(JOB_ID1, ?JOB_MODULE:make_id(?BLOCK_HASH1, ?BLOCK_VERSION, ?BLOCK_TARGET1)).
-define(JOB_ID2, ?JOB_MODULE:make_id(?BLOCK_HASH2, ?BLOCK_VERSION, ?BLOCK_TARGET2)).

-define(JOB_ID_IN_QUEUE, ?JOB_ID1).
-define(JOB_ID_NOT_IN_QUEUE, <<"0123456789abcdef">>).

-define(SHARE_TARGET, 16#ffff000000000000000000000000000000000000000).

-define(DESIRED_SOLVE_TIME, 30000).

-define(MAX_SOLVE_TIME, 60000).


session_test_() ->
    {setup,
     fun() ->
             {ok, Apps} = application:ensure_all_started(aestratum_lib),
             Apps
     end,
     fun(Apps) ->
             lists:foreach(fun(App) -> ok = application:stop(App) end, Apps)
     end,
     [{generator, fun server_session/0}]}.

server_session() ->
    {foreach,
     fun() ->
             meck:new(?EXTRA_NONCE_CACHE_MODULE, [passthrough]),
             meck:new(?USER_REGISTER_MODULE, [passthrough]),
             meck:new(?TARGET_MODULE, [passthrough]),
             meck:new(?JOB_MODULE, [passthrough]),
             meck:new(?JOB_QUEUE_MODULE, [passthrough]),
             meck:new(?MINER_MODULE, [passthrough]),
             meck:new(?AESTRATUM_MODULE, [passthrough]),
             meck:new(?TEST_MODULE, [passthrough]),
             {ok, Pid} = aestratum_dummy_handler:start_link(?TEST_MODULE),
             Pid
     end,
     fun(Pid) ->
             meck:unload(?EXTRA_NONCE_CACHE_MODULE),
             meck:unload(?USER_REGISTER_MODULE),
             meck:unload(?TARGET_MODULE),
             meck:unload(?JOB_MODULE),
             meck:unload(?JOB_QUEUE_MODULE),
             meck:unload(?MINER_MODULE),
             meck:unload(?AESTRATUM_MODULE),
             meck:unload(?TEST_MODULE),
             aestratum_dummy_handler:stop(Pid)
     end,
     [fun(Pid) -> t(Pid, init()) end,
      %% connected - error
      fun(Pid) -> t(Pid, when_connected(conn_timeout)) end,
      fun(Pid) -> t(Pid, when_connected(conn_authorize)) end,
      fun(Pid) -> t(Pid, when_connected(conn_submit)) end,
      fun(Pid) -> t(Pid, when_connected(conn_not_req)) end,
      fun(Pid) -> t(Pid, when_connected(conn_jsonrpc_errors)) end,
      fun(Pid) -> t(Pid, when_connected(chain_recv_block)) end,
      %% connected - success
      fun(Pid) -> t(Pid, when_connected(conn_configure)) end,
      fun(Pid) -> t(Pid, when_connected(conn_subscribe)) end,

      %% configured - error
      fun(Pid) -> t(Pid, when_configured(conn_timeout)) end,
      fun(Pid) -> t(Pid, when_configured(conn_configure)) end,
      fun(Pid) -> t(Pid, when_configured(conn_authorize)) end,
      fun(Pid) -> t(Pid, when_configured(conn_submit)) end,
      fun(Pid) -> t(Pid, when_configured(conn_not_req)) end,
      fun(Pid) -> t(Pid, when_configured(conn_jsonrpc_errors)) end,
      fun(Pid) -> t(Pid, when_configured(chain_recv_block)) end,
      %% configured - success
      fun(Pid) -> t(Pid, when_configured(conn_subscribe)) end,

      %% subscribed - error
      fun(Pid) -> t(Pid, when_subscribed(conn_timeout)) end,
      fun(Pid) -> t(Pid, when_subscribed(conn_configure)) end,
      fun(Pid) -> t(Pid, when_subscribed(conn_subscribe)) end,
      fun(Pid) -> t(Pid, when_subscribed(conn_authorize_duplicate_account)) end,
      fun(Pid) -> t(Pid, when_subscribed(conn_submit)) end,
      fun(Pid) -> t(Pid, when_subscribed(conn_not_req)) end,
      fun(Pid) -> t(Pid, when_subscribed(conn_jsonrpc_errors)) end,
      fun(Pid) -> t(Pid, when_subscribed(chain_recv_block)) end,
      %% subscribed - success
      fun(Pid) -> t(Pid, when_subscribed(conn_authorize)) end,

      %% authorized - error
      fun(Pid) -> t(Pid, when_authorized(conn_timeout)) end,
      fun(Pid) -> t(Pid, when_authorized(conn_configure)) end,
      fun(Pid) -> t(Pid, when_authorized(conn_subscribe)) end,
      fun(Pid) -> t(Pid, when_authorized(conn_authorize_duplicate_account)) end,
      fun(Pid) -> t(Pid, when_authorized(conn_authorize_duplicate_worker)) end,
      fun(Pid) -> t(Pid, when_authorized(conn_authorize_worker_count_exhausted)) end,
      fun(Pid) -> t(Pid, when_authorized(conn_authorize_account_mismatch)) end,
      fun(Pid) -> t(Pid, when_authorized(conn_submit)) end,
      fun(Pid) -> t(Pid, when_authorized(conn_not_req)) end,
      fun(Pid) -> t(Pid, when_authorized(conn_jsonrpc_errors)) end,
      fun(Pid) -> t(Pid, when_authorized(chain_recv_block)) end,
      %% conn_authorized - success
      fun(Pid) -> t(Pid, when_authorized(conn_authorize)) end,
      fun(Pid) -> t(Pid, when_authorized(chain_set_target)) end,

      %% set_target - error
      fun(Pid) -> t(Pid, when_set_target(conn_timeout)) end,
      fun(Pid) -> t(Pid, when_set_target(conn_configure)) end,
      fun(Pid) -> t(Pid, when_set_target(conn_subscribe)) end,
      fun(Pid) -> t(Pid, when_set_target(conn_authorize_duplicate_account)) end,
      fun(Pid) -> t(Pid, when_set_target(conn_authorize_duplicate_worker)) end,
      fun(Pid) -> t(Pid, when_set_target(conn_authorize_worker_count_exhausted)) end,
      fun(Pid) -> t(Pid, when_set_target(conn_authorize_account_mismatch)) end,
      fun(Pid) -> t(Pid, when_set_target(conn_submit)) end,
      fun(Pid) -> t(Pid, when_set_target(conn_not_req)) end,
      fun(Pid) -> t(Pid, when_set_target(conn_jsonrpc_errors)) end,
      %% set_target - success
      fun(Pid) -> t(Pid, when_set_target(conn_authorize)) end,
      fun(Pid) -> t(Pid, when_set_target(chain_recv_block, no_target_change)) end,
      fun(Pid) -> t(Pid, when_set_target(chain_recv_block, target_change)) end,
      fun(Pid) -> t(Pid, when_set_target(chain_recv_block, no_target_change, skip_1_block)) end,

      %% recv_block - conn_submit error
      fun(Pid) -> t(Pid, when_recv_block(conn_submit, job_not_found)) end,
      %% TODO: test sending a share with an account of a different client
      fun(Pid) -> t(Pid, when_recv_block(conn_submit, user_not_found)) end,
      fun(Pid) -> t(Pid, when_recv_block(conn_submit, invalid_miner_nonce)) end,
      fun(Pid) -> t(Pid, when_recv_block(conn_submit, duplicate_share)) end,
      fun(Pid) -> t(Pid, when_recv_block(conn_submit, invalid_solution)) end,
      fun(Pid) -> t(Pid, when_recv_block(conn_submit, high_target_share)) end,
      fun(Pid) -> t(Pid, when_recv_block(conn_submit, max_solve_time_exceeded)) end,
      %% recv_block - conn_submit success
      fun(Pid) -> t(Pid, when_recv_block(conn_submit, valid_share)) end,
      fun(Pid) -> t(Pid, when_recv_block(conn_submit, valid_block)) end
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

event({conn, D}) ->
    case maps:get(event, D, undefined) of
        E when E =/= undefined ->
            %% Map has event key, so it's an event already.
            {conn, D};
        undefined ->
            %% Map doesn't have event key, so it's a message that needs encoding.
            {ok, D1} = ?JSONRPC_MODULE:encode(D),
            {conn, #{event => recv_data, data => D1}}
    end;
event({chain, D}) ->
    %% This is always an event.
    {chain, D}.

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
    L = [{{conn, #{event => init}},
          {no_send,
           #{phase => connected, timer_phase => connected}}
         }],
    mock_connect(#{}),
    [{T, test, E, R} || {E, R} <- L].

when_connected(conn_timeout) ->
    T = <<"when connected - conn_timeout">>,
    L = [{{conn, #{event => timeout}},
          {stop,
           #{phase => disconnected, timer_phase => undefined,
             extra_nonce => undefined}}
         }],
    mock_connect(#{}),
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(conn_authorize) ->
    T = <<"when connected - conn_authorize">>,
    L = [{{conn, #{type => req, method => authorize, id => 0,
                   user => ?USER_NOT_IN_REGISTER, password => null}},
          {send,
           #{type => rsp, method => authorize, id => 0, reason => not_subscribed},
           #{phase => connected, timer_phase => connected,
            extra_nonce => undefined}}
         }],
    mock_connect(#{}),
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(conn_submit) ->
    T = <<"when connected - conn_submit">>,
    L = [{{conn, #{type => req, method => submit, id => 0,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID1,
                   miner_nonce => <<"0123456789">>, pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 0, reason => not_subscribed},
           #{phase => connected, timer_phase => connected,
            extra_nonce => undefined}}
         }],
    mock_connect(#{}),
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(conn_not_req) ->
    T = <<"when connected - conn_not_req">>,
    %% Server receives unexpected message - response.
    L = [{{conn, #{type => rsp, method => configure, id => null,
                   reason => parse_error, data => <<"foo">>}},
          {send,
           #{type => rsp, method => configure, id => null,
             reason => unknown_error, data => <<"unexpected_msg">>},
           #{phase => connected, timer_phase => connected,
             extra_nonce => undefined}}
         }],
    mock_connect(#{}),
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(conn_jsonrpc_errors) ->
    T = <<"when connected - conn_jsonrpc_errors">>,
    mock_connect(#{}),
    prep_connected(T) ++ jsonrpc_errors(T, connected, connected);
when_connected(chain_recv_block) ->
    T = <<"when connected - chain_recv_block">>,
    L = [{{chain, #{event => recv_block,
                    block => #{block_hash => ?BLOCK_HASH1,
                               block_version => ?BLOCK_VERSION,
                               block_target => ?BLOCK_TARGET1}}},
          {no_send,
           #{phase => connected, timer_phase => connected}}
         }],
    mock_connect(#{}),
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(conn_configure) ->
    T = <<"when connected - conn_configure">>,
    L = [{{conn, #{type => req, method => configure, id => 0, params => []}},
          {send,
           #{type => rsp, method => configure, id => 0, result => []},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    mock_connect(#{}),
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(conn_subscribe) ->
    T = <<"when connected - conn_subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 0,
                   user_agent => ?USER_AGENT, session_id => null,
                   host => ?HOST_VALID, port => ?PORT_VALID}},
          {send,
           #{type => rsp, method => subscribe, id => 0,
             result => [null, ?NONCE_MODULE:to_hex(?EXTRA_NONCE)]},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ?EXTRA_NONCE}}
         }],
    mock_subscribe(#{extra_nonce_bytes => ?EXTRA_NONCE_BYTES_VALID}),
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L].

when_configured(conn_timeout) ->
    T = <<"when configured - conn_timeout">>,
    L = [{{conn, #{event => timeout}},
          {stop,
           #{phase => disconnected, timer_phase => undefined,
            extra_nonce => undefined}}
         }],
    mock_configure(#{}),
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(conn_configure) ->
    T = <<"when configured - conn_configure">>,
    L = [{{conn, #{type => req, method => configure, id => 1, params => []}},
          {send,
           #{type => rsp, method => configure, id => 1, reason => unknown_error},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    mock_configure(#{}),
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(conn_authorize) ->
    T = <<"when configured - conn_authorize">>,
    L = [{{conn, #{type => req, method => authorize, id => 1,
                   user => ?USER_NOT_IN_REGISTER, password => null}},
          {send,
           #{type => rsp, method => authorize, id => 1, reason => not_subscribed},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    mock_configure(#{}),
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(conn_submit) ->
    T = <<"when configured - conn_submit">>,
    L = [{{conn, #{type => req, method => submit, id => 1,
                   user => ?USER_NOT_IN_REGISTER, job_id => ?JOB_ID1,
                   miner_nonce => <<"0123456789">>, pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 1, reason => not_subscribed},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    mock_configure(#{}),
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(conn_not_req) ->
    T = <<"when configured - conn_not_req">>,
    L = [{{conn, #{type => rsp, method => configure, id => 1, result => []}},
          {send,
           #{type => rsp, method => configure, id => 1, reason => unknown_error},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    mock_configure(#{}),
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(conn_jsonrpc_errors) ->
    T = <<"when configured - conn_jsonrpc_errors">>,
    prep_configured(T) ++ jsonrpc_errors(T, configured, configured);
when_configured(chain_recv_block) ->
    T = <<"when configured - chain_recv_block">>,
    L = [{{chain, #{event => recv_block,
                    block => #{block_hash => ?BLOCK_HASH1,
                               block_version => ?BLOCK_VERSION,
                               block_target => ?BLOCK_TARGET1}}},
          {no_send,
           #{phase => configured, timer_phase => configured}}
         }],
    mock_configure(#{}),
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(conn_subscribe) ->
    T = <<"when configured - conn_subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 1,
                   user_agent => ?USER_AGENT, session_id => null,
                   host => ?HOST_VALID, port => ?PORT_VALID}},
          {send,
           #{type => rsp, method => subscribe, id => 1,
             result => [null, ?NONCE_MODULE:to_hex(?EXTRA_NONCE)]},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ?EXTRA_NONCE}}
         }],
    mock_subscribe(#{extra_nonce_bytes => ?EXTRA_NONCE_BYTES_VALID}),
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L].

when_subscribed(conn_timeout) ->
    T = <<"when subscribed - conn_timeout">>,
    L = [{{conn, #{event => timeout}},
          {stop,
           #{phase => disconnected, timer_phase => undefined,
             extra_nonce => undefined}}
         }],
    mock_subscribe(#{extra_nonce_bytes => ?EXTRA_NONCE_BYTES_VALID}),
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(conn_configure) ->
    T = <<"when subscribed - conn_configure">>,
    L = [{{conn, #{type => req, method => configure, id => 2, params => []}},
          {send,
           #{type => rsp, method => configure, id => 2, reason => unknown_error},
           #{phase => subscribed, timer_phase => subscribed}}
         }],
    mock_subscribe(#{extra_nonce_bytes => ?EXTRA_NONCE_BYTES_VALID}),
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(conn_subscribe) ->
    T = <<"when subscribed - conn_subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 2,
                   user_agent => ?USER_AGENT,  session_id => null,
                   host => ?HOST_VALID, port => ?PORT_VALID}},
          {send,
           #{type => rsp, method => subscribe, id => 2, reason => unknown_error},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ?EXTRA_NONCE}}
         }],
    mock_subscribe(#{extra_nonce_bytes => ?EXTRA_NONCE_BYTES_VALID}),
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L];
%% Account is already in register, but has a different connection PID - it's a
%% different client. It's not allowed to have multiple connections with the
%% same account.
when_subscribed(conn_authorize_duplicate_account) ->
    T = <<"when subscribed - conn_authorize_duplicate_account">>,
    L = [{{conn, #{type => req, method => authorize, id => 2,
                   user => {?ACCOUNT_IN_REGISTER_DIFFERENT_CLIENT, ?WORKER_NOT_IN_REGISTER},
                   password => null}},
           {send,
            #{type => rsp, method => authorize, id => 2, result => false},
            #{phase => subscribed, timer_phase => subscribed,
              extra_nonce => ?EXTRA_NONCE}}
         }],
    mock_authorize(#{}),
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(conn_submit) ->
    T = <<"when subscribed - conn_submit">>,
    L = [{{conn, #{type => req, method => submit, id => 2,
                   user => ?USER_NOT_IN_REGISTER, job_id => ?JOB_ID1,
                   miner_nonce => <<"0123456789">>, pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 2, reason => unauthorized_worker},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ?EXTRA_NONCE}}
         }],
    mock_subscribe(#{extra_nonce_bytes => ?EXTRA_NONCE_BYTES_VALID}),
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(conn_not_req) ->
    T = <<"when subscribed - conn_not_req">>,
    L = [{{conn, #{type => rsp, method => configure, id => 2, result => []}},
          {send,
           #{type => rsp, method => configure, id => 2, reason => unknown_error},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ?EXTRA_NONCE}}
         }],
    mock_subscribe(#{extra_nonce_bytes => ?EXTRA_NONCE_BYTES_VALID}),
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(conn_jsonrpc_errors) ->
    T = <<"when subscribed - conn_jsonrpc_errors">>,
    mock_subscribe(#{extra_nonce_bytes => ?EXTRA_NONCE_BYTES_VALID}),
    prep_subscribed(T) ++ jsonrpc_errors(T, subscribed, subscribed);
when_subscribed(chain_recv_block) ->
    T = <<"when subscribed - chain_recv_block">>,
    L = [{{chain, #{event => recv_block,
                    block => #{block_hash => ?BLOCK_HASH1,
                               block_version => ?BLOCK_VERSION,
                               block_target => ?BLOCK_TARGET1}}},
          {no_send,
           #{phase => subscribed, timer_phase => subscribed}}
         }],
    mock_subscribe(#{extra_nonce_bytes => ?EXTRA_NONCE_BYTES_VALID}),
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(conn_authorize) ->
    T = <<"when subscribed - conn_authorize">>,
    L = [{{conn, #{type => req, method => authorize, id => 2,
                   user => ?USER_NOT_IN_REGISTER, password => null}},
           {send,
            #{type => rsp, method => authorize, id => 2, result => true},
            #{phase => authorized, timer_phase => undefined,
              extra_nonce => ?EXTRA_NONCE}}
         }],
    mock_authorize(#{}),
    prep_subscribed(T) ++ [{T, test, E, R} || {E, R} <- L].

when_authorized(conn_timeout) ->
    T = <<"when authorized - conn_timeout">>,
    L = [{{conn, #{event => timeout}},
          {no_send,
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(conn_configure) ->
    T = <<"when authorized - conn_configure">>,
    L = [{{conn, #{type => req, method => configure, id => 3, params => []}},
          {send,
           #{type => rsp, method => configure, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(conn_subscribe) ->
    T = <<"when authorized - conn_subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 3,
                   user_agent => ?USER_AGENT, session_id => null,
                   host => ?HOST_VALID, port => ?PORT_VALID}},
          {send,
           #{type => rsp, method => configure, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L];
%% A worker tries to authorize using an account which is already authorized by
%% a different connection/client. All workers must share the same account name
%% per connection.
when_authorized(conn_authorize_duplicate_account) ->
    T = <<"when authorized - conn_authorize_duplicate_account">>,
    L = [{{conn, #{type => req, method => authorize, id => 3,
                   user => {?ACCOUNT_IN_REGISTER_DIFFERENT_CLIENT, ?WORKER_NOT_IN_REGISTER},
                   password => null}},
          {send,
           #{type => rsp, method => authorize, id => 3, result => false},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(conn_authorize_duplicate_worker) ->
    T = <<"when authorized - conn_authorize_duplicate_worker">>,
    L = [{{conn, #{type => req, method => authorize, id => 3,
                   user => ?USER_IN_REGISTER, password => null}},
          {send,
           #{type => rsp, method => authorize, id => 3, result => false},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(conn_authorize_worker_count_exhausted) ->
    T = <<"when authorized - conn_authorize_worker_count_exhausted">>,
    L = [{{conn, #{type => req, method => authorize, id => 3,
                   user => ?USER_NOT_IN_REGISTER_WITH_EXHAUSTED_WORKER_COUNT,
                   password => null}},
          {send,
           #{type => rsp, method => authorize, id => 3, result => false},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(conn_authorize_account_mismatch) ->
    T = <<"when authorized - conn_authorize_account_mismatch">>,
    L = [{{conn, #{type => req, method => authorize, id => 3,
                   user => ?USER_NOT_IN_REGISTER, password => null}},
          {send,
           #{type => rsp, method => authorize, id => 3, result => false},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(conn_submit) ->
    T = <<"when authorized - conn_submit">>,
    L = [{{conn, #{type => req, method => submit, id => 3,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID1,
                   miner_nonce => <<"0000">>, pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(conn_not_req) ->
    T = <<"when authorized - conn_not_req">>,
    L = [{{conn, #{type => rsp, method => subscribe, id => 3,
                   reason => parse_error, data => null}},
          {send,
           #{type => rsp, method => subscribe, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(conn_jsonrpc_errors) ->
    T = <<"when authorized - conn_jsonrpc_errors">>,
    mock_authorize(#{}),
    prep_authorized(T) ++ jsonrpc_errors(T, authorized, undefined);
when_authorized(chain_recv_block) ->
    %% The set_target must come before the session can start accepting blocks.
    T = <<"when authorized - chain_recv_block">>,
    L = [{{chain, #{event => recv_block,
                    block => #{block_hash => ?BLOCK_HASH1,
                               block_version => ?BLOCK_VERSION,
                               block_target => ?BLOCK_TARGET1}}},
          {no_send,
           #{phase => authorized, timer_phase => undefined,
             initial_share_target => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(conn_authorize) ->
    T = <<"when authorized - conn_authorize">>,
    L = [{{conn, #{type => req, method => authorize, id => 3,
                   user => {?ACCOUNT_IN_REGISTER, ?WORKER_NOT_IN_REGISTER},
                   password => null}},
          {send,
           #{type => rsp, method => authorize, id => 3, result => true},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(chain_set_target) ->
    T = <<"when authorized - chain_set_target">>,
    L = [{{chain, #{event => set_target}},
          {send,
           #{type => ntf, method => set_target,
             target => ?TARGET_MODULE:to_hex(?SHARE_TARGET)},
           #{phase => authorized, timer_phase => undefined,
             initial_share_target => ?SHARE_TARGET}}
         }],
    mock_set_target(#{}),
    prep_authorized(T) ++ [{T, test, E, R} || {E, R} <- L].

when_set_target(conn_timeout) ->
    T = <<"when set target - conn_timeout">>,
    L = [{{conn, #{event => timeout}},
          {no_send,
           #{phase => authorized, initial_share_target => ?SHARE_TARGET}}
         }],
    mock_set_target(#{}),
    prep_set_target(T) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(conn_configure) ->
    T = <<"when set target - conn_configure">>,
    L = [{{conn, #{type => req, method => configure, id => 3, params => []}},
          {send,
           #{type => rsp, method => configure, id => 3, reason => unknown_error},
           #{phase => authorized, initial_share_target => ?SHARE_TARGET}}
         }],
    mock_set_target(#{}),
    prep_set_target(T) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(conn_subscribe) ->
    T = <<"when set target - conn_subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 3,
                   user_agent => ?USER_AGENT, session_id => null,
                   host => ?HOST_VALID, port => ?PORT_VALID}},
          {send,
           #{type => rsp, method => configure, id => 3, reason => unknown_error},
           #{phase => authorized, initial_share_target => ?SHARE_TARGET}}
         }],
    mock_set_target(#{}),
    prep_set_target(T) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(conn_authorize_duplicate_account) ->
    T = <<"when set target - conn_authorize_duplicate_account">>,
    L = [{{conn, #{type => req, method => authorize, id => 3,
                   user => {?ACCOUNT_IN_REGISTER_DIFFERENT_CLIENT, ?WORKER_NOT_IN_REGISTER},
                   password => null}},
          {send,
           #{type => rsp, method => authorize, id => 3, result => false},
           #{phase => authorized, initial_share_target => ?SHARE_TARGET}}
         }],
    mock_set_target(#{}),
    prep_set_target(T) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(conn_authorize_duplicate_worker) ->
    T = <<"when set target - conn_authorize_duplicate_worker">>,
    L = [{{conn, #{type => req, method => authorize, id => 3,
                   user => ?USER_IN_REGISTER, password => null}},
          {send,
           #{type => rsp, method => authorize, id => 3, result => false},
           #{phase => authorized, initial_share_target => ?SHARE_TARGET}}
         }],
    mock_set_target(#{}),
    prep_set_target(T) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(conn_authorize_worker_count_exhausted) ->
    T = <<"when set target - conn_authorize_worker_count_exhausted">>,
    L = [{{conn, #{type => req, method => authorize, id => 3,
                   user => ?USER_NOT_IN_REGISTER_WITH_EXHAUSTED_WORKER_COUNT,
                   password => null}},
          {send,
           #{type => rsp, method => authorize, id => 3, result => false},
           #{phase => authorized, initial_share_target => ?SHARE_TARGET}}
         }],
    mock_set_target(#{}),
    prep_set_target(T) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(conn_authorize_account_mismatch) ->
    T = <<"when set target - conn_authorize_account_mismatch">>,
    L = [{{conn, #{type => req, method => authorize, id => 3,
                   user => ?USER_NOT_IN_REGISTER, password => null}},
          {send,
           #{type => rsp, method => authorize, id => 3, result => false},
           #{phase => authorized, initial_share_target => ?SHARE_TARGET}}
         }],
    mock_set_target(#{}),
    prep_set_target(T) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(conn_submit) ->
    %% There was no block received yet and so there are no jobs in the queue.
    T = <<"when set target - conn_submit">>,
    L = [{{conn, #{type => req, method => submit, id => 3,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID1,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE),
                   pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 3, reason => job_not_found},
           #{phase => authorized, initial_share_target => ?SHARE_TARGET}}
         }],
    mock_set_target(#{}),
    prep_set_target(T) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(conn_not_req) ->
    T = <<"when set target - conn_not_req">>,
    L = [{{conn, #{type => rsp, method => configure, id => 3,
                   reason => parse_error, data => null}},
          {send,
           #{type => rsp, method => subscribe, id => 3, reason => unknown_error},
           #{phase => authorized, initial_share_target => ?SHARE_TARGET}}
         }],
    mock_set_target(#{}),
    prep_set_target(T) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(conn_jsonrpc_errors) ->
    T = <<"when set target - conn_jsonrpc_errors">>,
    mock_set_target(#{}),
    prep_set_target(T) ++ jsonrpc_errors(T, authorized, undefined);
when_set_target(conn_authorize) ->
    T = <<"when set target - conn_authorize">>,
    L = [{{conn, #{type => req, method => authorize, id => 3,
                   user => {?ACCOUNT_IN_REGISTER, ?WORKER_NOT_IN_REGISTER},
                   password => null}},
          {send,
           #{type => rsp, method => authorize, id => 3, result => true},
           #{phase => authorized, initial_share_target => ?SHARE_TARGET}}
         }],
    mock_set_target(#{}),
    prep_set_target(T) ++ [{T, test, E, R} || {E, R} <- L].

when_set_target(chain_recv_block, no_target_change) ->
    T = <<"when set target - chain_recv_block, no_target_change">>,
    L = [{{chain, #{event => recv_block,
                    block => #{block_hash => ?BLOCK_HASH1,
                               block_version => ?BLOCK_VERSION,
                               block_target => ?BLOCK_TARGET1}}},
          {send,
           #{type => ntf, method => notify, job_id => ?JOB_ID1,
             block_hash => ?BLOCK_HASH1, block_version => ?BLOCK_VERSION,
             empty_queue => true},
           #{phase => authorized, accept_blocks => true,
             initial_share_target => ?SHARE_TARGET}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_set_target(T) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(chain_recv_block, target_change) ->
    T = <<"when set target - chain_recv_block, target_change">>,
    L = [{{chain, #{event => recv_block,
                    block => #{block_hash => ?BLOCK_HASH1,
                               block_version => ?BLOCK_VERSION,
                               block_target => ?BLOCK_TARGET1}}},
          {send,
           %% NOTE: we don't test the target value here, there is a dedicated
           %% test module for that (or will be!).
           #{type => ntf, method => set_target},
           #{phase => authorized, accept_blocks => false}}
         },
         %% We test that the next new block doesn't cause any new notification.
         %% First, the session expects the {chain, {notify, Job}} event.
         %% That job caused set_target notification, so it's supposed to be
         %% first after set_target, other new blocks in between are just
         %% skipped.
         {{chain, #{event => recv_block,
                    block => #{block_hash => ?BLOCK_HASH2,
                               block_version => ?BLOCK_VERSION,
                               block_target => ?BLOCK_TARGET2}}},
          {no_send,
           #{phase => authorized, accept_blocks => false}}
         },
         %% The previuos new block was skipped, the notify event will cause
         %% sending of notify notification and will restore processing of
         %% new blocks.
         {{chain, #{event => notify,
                    job_info => #{job_id => ?JOB_ID1, block_hash => ?BLOCK_HASH1,
                                  block_version => ?BLOCK_VERSION,
                                  block_target => ?BLOCK_TARGET1,
                                  share_target => ?SHARE_TARGET,
                                  desired_solve_time => ?DESIRED_SOLVE_TIME,
                                  max_solve_time => ?MAX_SOLVE_TIME}}},
          #{type => ntf, method => notify, job_id => ?JOB_ID1,
            block_hash => ?BLOCK_HASH1, block_version => ?BLOCK_VERSION,
            empty_queue => true},
          #{phase => authorized, accept_blocks => true}
         }],
    mock_recv_block(#{new_share_target => {increase, 10.0},
                      share_target_diff_threshold => 5.0}),
    prep_set_target(T) ++ [{T, test, E, R} || {E, R} <- L].

when_set_target(chain_recv_block, no_target_change, skip_1_block) ->
    T = <<"when set target - chain_recv_block, no_target_change, skip_1_block">>,
    L = [{{chain, #{event => recv_block,
                    block => #{block_hash => ?BLOCK_HASH1,
                               block_version => ?BLOCK_VERSION,
                               block_target => ?BLOCK_TARGET1}}},
          {send,
           #{type => ntf, method => notify, job_id => ?JOB_ID1,
             block_hash => ?BLOCK_HASH1, block_version => ?BLOCK_VERSION,
             empty_queue => true},
           #{phase => authorized, accept_blocks => true,
             initial_share_target => ?SHARE_TARGET}}
         },
         %% The next received block is skipped (no notify is sent).
         {{chain, #{event => recv_block,
                    block => #{block_hash => ?BLOCK_HASH2,
                               block_version => ?BLOCK_VERSION,
                               block_target => ?BLOCK_TARGET2}}},
          {no_send,
           #{phase => authorized, accept_blocks => true,
             initial_share_target => ?SHARE_TARGET}}
         },
         %% The next received block is not skipped.
         {{chain, #{event => recv_block,
                    block => #{block_hash => ?BLOCK_HASH2,
                               block_version => ?BLOCK_VERSION,
                               block_target => ?BLOCK_TARGET2}}},
          {send,
           #{type => ntf, method => notify, job_id => ?JOB_ID2,
             block_hash => ?BLOCK_HASH2, block_version => ?BLOCK_VERSION,
             empty_queue => true},
           #{phase => authorized, accept_blocks => true,
             initial_share_target => ?SHARE_TARGET}}
          }],
    mock_recv_block(#{new_share_target => no_change, skip_num_blocks => 1}),
    prep_set_target(T) ++ [{T, test, E, R} || {E, R} <- L].

when_recv_block(conn_submit, job_not_found) ->
    T = <<"when recv block - conn_submit, job_not_found">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID_NOT_IN_QUEUE,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE), pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 4, reason => job_not_found},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(conn_submit, user_not_found) ->
    T = <<"when recv block - conn_submit, user_not_found">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_NOT_IN_REGISTER, job_id => ?JOB_ID1,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE), pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 4, reason => unauthorized_worker},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(conn_submit, invalid_miner_nonce) ->
    T = <<"when recv block - conn_submit, invalid_miner_nonce">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID_IN_QUEUE,
                   miner_nonce => ?MINER_NONCE_BIN_INVALID, pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 4, reason => unknown_error,
             data => <<"invalid_miner_nonce">>},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(conn_submit, duplicate_share) ->
    T = <<"when recv block - conn_submit, duplicate_share">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID_IN_QUEUE,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE_DUPLICATE),
                   pow => ?POW_DUPLICATE}},
          {send,
           #{type => rsp, method => submit, id => 4, reason => duplicate_share},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(conn_submit, invalid_solution) ->
    T = <<"when recv block - conn_submit, invalid_solution">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID_IN_QUEUE,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE),
                   pow => ?POW_INVALID}},
          {send,
           #{type => rsp, method => submit, id => 4, reason => unknown_error,
             data => <<"invalid_solution">>},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(conn_submit, high_target_share) ->
    T = <<"when recv block - conn_submit, high_target_share">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID_IN_QUEUE,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE),
                   pow => ?POW_HIGH_TARGET}},
          {send,
           #{type => rsp, method => submit, id => 4, reason => low_difficulty_share},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(conn_submit, max_solve_time_exceeded) ->
    T = <<"when recv block - conn_submit, max_solve_time_exceeded">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID_IN_QUEUE,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE),
                   pow => ?POW_BLOCK_TARGET}},
          {send,
           #{type => rsp, method => submit, id => 4, result => false},
           #{phase => authorized, max_solve_time => -100}}
         }],
    mock_recv_block(#{new_share_target => no_change, max_solve_time => -100}),
    prep_recv_block(T) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(conn_submit, valid_share) ->
    T = <<"when recv block - conn_submit, valid_share">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID_IN_QUEUE,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE),
                   pow => ?POW_SHARE_TARGET}},
          {send,
           #{type => rsp, method => submit, id => 4, result => true},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(conn_submit, valid_block) ->
    T = <<"when recv block - conn_submit, valid_block">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID_IN_QUEUE,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE),
                   pow => ?POW_BLOCK_TARGET}},
          {send,
           #{type => rsp, method => submit, id => 4, result => true},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T) ++ [{T, test, E, R} || {E, R} <- L].

mock_connect(_Opts) ->
    ?ENV_MODULE:set(#{msg_timeout => 15000}),
    ok.

mock_configure(Opts) ->
    mock_connect(Opts).

mock_subscribe(#{extra_nonce_bytes := ExtraNoncebytes} = Opts) ->
    mock_configure(#{}),
    case maps:get(is_host_valid, Opts, true) of
        true  -> ?ENV_MODULE:set(#{host => ?HOST_VALID});
        false -> ok
    end,
    case maps:get(is_port_valid, Opts, true) of
        true  -> ?ENV_MODULE:set(#{port => ?PORT_VALID});
        false -> ok
    end,
    ?ENV_MODULE:set(#{extra_nonce_bytes => ExtraNoncebytes}),
    meck:expect(?EXTRA_NONCE_CACHE_MODULE, get,
                fun(N) when N =:= ?EXTRA_NONCE_BYTES_VALID ->
                        {ok, ?EXTRA_NONCE};
                   (N) when N =:= ?EXTRA_NONCE_BYTES_INVALID ->
                        {error, extra_nonce_not_found}
                end),
    meck:expect(?EXTRA_NONCE_CACHE_MODULE, free, fun(_) -> ok end),
    ok.

mock_authorize(Opts) ->
    mock_subscribe(#{extra_nonce_bytes => ?EXTRA_NONCE_BYTES_VALID}),
    ?ENV_MODULE:set(#{skip_num_blocks => maps:get(skip_num_blocks, Opts, 0)}),
    meck:expect(?USER_REGISTER_MODULE, add, fun(_, _, _) -> ok end),
    meck:expect(?USER_REGISTER_MODULE, del, fun(_) -> ok end),
    meck:expect(?USER_REGISTER_MODULE, member,
                fun(A, _) when A =:= ?ACCOUNT_IN_REGISTER ->
                        both;
                   (A, _) when A =:= ?ACCOUNT_IN_REGISTER_DIFFERENT_CLIENT ->
                        account_only;
                   (A, _) when A =:= ?ACCOUNT_NOT_IN_REGISTER ->
                        neither;
                   (A, _) when A =:= ?ACCOUNT_NOT_IN_REGISTER_WITH_EXHAUSTED_WORKER_COUNT ->
                        neither
                end),
    meck:expect(?USER_REGISTER_MODULE, add_worker,
                fun(A, W) when (A =:= ?ACCOUNT_IN_REGISTER) and
                               (W =:= ?WORKER_NOT_IN_REGISTER) ->
                        ok;
                   (A, W) when (A =:= ?ACCOUNT_IN_REGISTER) and
                               (W =:= ?WORKER_IN_REGISTER) ->
                        {error, worker_already_present};
                   (A, _W) when A =:= ?ACCOUNT_NOT_IN_REGISTER_WITH_EXHAUSTED_WORKER_COUNT ->
                        {error, worker_count_exhausted};
                   (A, _W) when A =:= ?ACCOUNT_NOT_IN_REGISTER ->
                        {error, account_not_found}
                end),
    ok.

mock_set_target(Opts) ->
    mock_authorize(Opts),
    ?ENV_MODULE:set(#{initial_share_target => ?SHARE_TARGET,
                      max_share_target => ?SHARE_TARGET,
                      share_target_diff_threshold => 5.0,
                      desired_solve_time => 30000,
                      max_solve_time => 60000}),
    ok.

mock_recv_block(Opts) ->
    mock_set_target(Opts),
    case maps:get(new_share_target, Opts, undefined) of
        NewShareTarget when NewShareTarget =/= undefined ->
            meck:expect(?TARGET_MODULE, diff, fun(_, _) -> NewShareTarget end);
        undefined ->
            ok
    end,
    case maps:get(share_target_diff_threshold, Opts, undefined) of
        ShareTargetDiffThreshold when ShareTargetDiffThreshold =/= undefined ->
            ?ENV_MODULE:set(#{share_target_diff_threshold => ShareTargetDiffThreshold});
        undefined ->
            ok
    end,
    meck:expect(?JOB_QUEUE_MODULE, find,
                fun(J, _) ->
                        case J =:= ?JOB_ID_IN_QUEUE of
                            true ->
                                Job = ?JOB_MODULE:new(?JOB_ID_IN_QUEUE,
                                                      ?BLOCK_HASH1, ?BLOCK_VERSION,
                                                      ?BLOCK_TARGET1,?SHARE_TARGET,
                                                      ?DESIRED_SOLVE_TIME,
                                                      ?MAX_SOLVE_TIME),
                                {ok, Job};
                            false ->
                                {error, not_found}
                        end
                end),
    meck:expect(?NONCE_MODULE, is_valid_bin,
                fun(?MINER_NONCE_BIN_INVALID) -> false;
                   (_)                        -> true
                end),
    meck:expect(?JOB_MODULE, is_share_present,
                fun(MinerNonce, Pow, _) ->
                        MinerNonce =:= ?MINER_NONCE_DUPLICATE andalso
                        Pow =:= ?POW_DUPLICATE
                end),
    meck:expect(?MINER_MODULE, verify_proof,
                %% If Pow is different from POW_INVALID - valid solution.
                fun(_, _, _, Pow, _) -> Pow =/= ?POW_INVALID end),
    meck:expect(?MINER_MODULE, get_target,
                fun(Pow, _) ->
                        case Pow =:= ?POW_HIGH_TARGET of
                            true  -> ?SHARE_TARGET + 1;
                            false ->
                                case Pow =:= ?POW_SHARE_TARGET of
                                    true  -> ?SHARE_TARGET - 1;
                                    false -> ?BLOCK_TARGET1 - 1
                                end
                        end
                end),
    meck:expect(?AESTRATUM_MODULE, submit_share, fun(_, _, _) -> ok end),
    meck:expect(?AESTRATUM_MODULE, submit_solution, fun(_, _, _) -> ok end),
    ?ENV_MODULE:set(#{max_solve_time => maps:get(max_solve_time, Opts, 30000)}),
    ?ENV_MODULE:set(#{edge_bits => maps:get(edge_bits, Opts, 29)}),
    ok.

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

prep_set_target(T) ->
    L = [conn_init(),
         conn_configure(0),
         conn_subscribe(1),
         conn_authorize(2),
         chain_set_target()],
    [{T, no_test, E, R} || {E, R} <- L].

prep_recv_block(T) ->
    L = [conn_init(),
         conn_configure(0),
         conn_subscribe(1),
         conn_authorize(2),
         chain_set_target(),
         chain_recv_block()],
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
    {{conn, #{event => init}},
     {no_send, #{phase => connected, timer_phase => connected}}
    }.

conn_configure(Id) ->
    {{conn, #{type => req, method => configure, id => Id, params => []}},
     {send,
      #{type => rsp, method => configure, id => Id, result => []},
      #{phase => configured, timer_phase => configured}}
    }.

conn_subscribe(Id) ->
    {{conn, #{type => req, method => subscribe, id => Id,
              user_agent => ?USER_AGENT, session_id => null,
              host => ?HOST_VALID, port => ?PORT_VALID}},
     {send,
      #{type => rsp, method => subscribe, id => Id,
        result => [null, ?NONCE_MODULE:to_hex(?EXTRA_NONCE)]},
      #{phase => subscribed, timer_phase => subscribed,
        extra_nonce => ?EXTRA_NONCE}}
    }.

conn_authorize(Id) ->
    {{conn, #{type => req, method => authorize, id => Id,
              user => ?USER_NOT_IN_REGISTER, password => null}},
     {send,
      #{type => rsp, method => authorize, id => Id, result => true},
      #{phase => authorized, timer_phase => undefined}}
    }.

chain_set_target() ->
    {{chain, #{event => set_target}},
     {send,
      #{type => ntf, method => set_target,
        target => ?TARGET_MODULE:to_hex(?SHARE_TARGET)},
      #{phase => authorized, timer_phase => undefined}}
    }.

chain_recv_block() ->
    {{chain, #{event => recv_block,
               block => #{block_hash => ?BLOCK_HASH1,
                          block_version => ?BLOCK_VERSION,
                          block_target => ?BLOCK_TARGET1}}},
     {send,
      #{type => ntf, method => notify, job_id => ?JOB_ID1,
        block_hash => ?BLOCK_HASH1, block_version => ?BLOCK_VERSION,
        empty_queue => true},
      #{phase => authorized, timer_phase => undefined, accept_blocks => true}}
    }.

conn_make_parse_error(Phase, TimerPhase) ->
    {{conn, #{event => recv_data, data => <<"some random binary">>}},
     {send,
      #{type => rsp, method => undefined, id => null, reason => parse_error},
      #{phase => Phase, timer_phase => TimerPhase}}
    }.

conn_make_invalid_msg(no_id, Phase, TimerPhase) ->
    {{conn, #{event => recv_data,
              data => <<"{\"jsonrpc\":\"2.0\",\"id\":\"none\"}">>}},
     {send,
      #{type => rsp, method => undefined, id => null, reason => invalid_msg},
      #{phase => Phase, timer_phase => TimerPhase}}
    };
conn_make_invalid_msg(id, Phase, TimerPhase) ->
    {{conn, #{event => recv_data,
              data => <<"{\"jsonrpc\":\"2.0\",\"id\":100}">>}},
     {send,
      #{type => rsp, method => undefined, id => 100, reason => invalid_msg},
      #{phase => Phase, timer_phase => TimerPhase}}
    }.

conn_make_invalid_method(no_id, Phase, TimerPhase) ->
    {{conn, #{event => recv_data,
              data => <<"{\"jsonrpc\":\"2.0\",\"id\":-10,\"method\":\"foo\",\"params\":[]}">>}},
     {send,
      #{type => rsp, method => undefined, id => null, reason => invalid_method},
      #{phase => Phase, timer_phase => TimerPhase}}
    };
conn_make_invalid_method(id, Phase, TimerPhase) ->
    {{conn, #{event => recv_data,
              data => <<"{\"jsonrpc\":\"2.0\",\"id\":200,\"method\":\"foo\",\"params\":[]}">>}},
     {send,
      #{type => rsp, method => undefined, id => 200, reason => invalid_method},
      #{phase => Phase, timer_phase => TimerPhase}}
    }.

conn_make_invalid_param(Phase, TimerPhase) ->
    {{conn, #{event => recv_data,
              data => <<"{\"jsonrpc\":\"2.0\",\"id\":300,\"method\":\"mining.subscribe\","
                        "\"params\":[\"aeminer/1.0\",\"invalid session_id\",\"aepool.com\",9876]}">>}},
     {send,
      #{type => rsp, method => subscribe, id => 300, reason => invalid_param},
      #{phase => Phase, timer_phase => TimerPhase}}
    }.

%% TODO: howto create internal error?
%%conn_make_internal_error()

