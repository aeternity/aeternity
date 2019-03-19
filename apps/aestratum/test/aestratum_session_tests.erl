-module(aestratum_session_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aestratum_session).

-define(JSONRPC_MODULE, aestratum_jsonrpc).
-define(NONCE_MODULE, aestratum_nonce).
-define(TARGET_MODULE, aestratum_target).
-define(EXTRA_NONCE_CACHE_MODULE, aestratum_extra_nonce_cache).
-define(USER_REGISTER_MODULE, aestratum_user_register).
-define(JOB_MODULE, aestratum_job).
-define(JOB_QUEUE_MODULE, aestratum_job_queue).
-define(MINER_MODULE, aestratum_miner).

-define(HOST_VALID, <<"pool.aeternity.com">>).
-define(HOST_INVALID, <<>>).

-define(PORT_VALID, 12345).
-define(PORT_INVALID, 100000).

-define(USER_AGENT, <<"aeminer/1.2.3">>).

-define(EXTRA_NONCE_NBYTES_VALID, 4).
-define(EXTRA_NONCE_NBYTES_INVALID, 8).

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

-define(USER_IN_REGISTER, <<"ak_1111111111111111111111111111111111111111111111111">>).
-define(USER_NOT_IN_REGISTER, <<"ak_2222222222222222222222222222222222222222222222222">>).

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
      fun(Pid) -> t(Pid, when_connected(chain_recv_block)) end,
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
      fun(Pid) -> t(Pid, when_subscribed(configure)) end,
      fun(Pid) -> t(Pid, when_subscribed(subscribe)) end,
      fun(Pid) -> t(Pid, when_subscribed(submit)) end,
      fun(Pid) -> t(Pid, when_subscribed(not_req)) end,
      fun(Pid) -> t(Pid, when_subscribed(jsonrpc_errors)) end,
      %% subscribed - success
      fun(Pid) -> t(Pid, when_subscribed(authorize_failure)) end,
      fun(Pid) -> t(Pid, when_subscribed(authorize_success)) end,

      %% authorized - error
      fun(Pid) -> t(Pid, when_authorized(timeout)) end,
      fun(Pid) -> t(Pid, when_authorized(configure)) end,
      fun(Pid) -> t(Pid, when_authorized(subscribe)) end,
      fun(Pid) -> t(Pid, when_authorized(authorize)) end,
      fun(Pid) -> t(Pid, when_authorized(submit)) end,
      fun(Pid) -> t(Pid, when_authorized(not_req)) end,
      fun(Pid) -> t(Pid, when_authorized(jsonrpc_errors)) end,
      %% authorized - success
      fun(Pid) -> t(Pid, when_authorized(set_target)) end,

      %% set_target - error
      fun(Pid) -> t(Pid, when_set_target(timeout)) end,
      fun(Pid) -> t(Pid, when_set_target(configure)) end,
      fun(Pid) -> t(Pid, when_set_target(subscribe)) end,
      fun(Pid) -> t(Pid, when_set_target(authorize)) end,
      %% TODO: submit
      fun(Pid) -> t(Pid, when_set_target(not_req)) end,
      fun(Pid) -> t(Pid, when_set_target(jsonrpc_errors)) end,
      %% set_target - success
      fun(Pid) -> t(Pid, when_set_target(recv_block, no_target_change)) end,
      fun(Pid) -> t(Pid, when_set_target(recv_block, target_change)) end,

      %% recv_block - submit error
      fun(Pid) -> t(Pid, when_recv_block(submit, user_not_found)) end,
      fun(Pid) -> t(Pid, when_recv_block(submit, job_not_found)) end,
      fun(Pid) -> t(Pid, when_recv_block(submit, invalid_miner_nonce)) end,
      fun(Pid) -> t(Pid, when_recv_block(submit, duplicate_share)) end,
      fun(Pid) -> t(Pid, when_recv_block(submit, invalid_solution)) end,
      fun(Pid) -> t(Pid, when_recv_block(submit, high_target_share)) end,
      %% recv_block - submit success
      fun(Pid) -> t(Pid, when_recv_block(submit, valid_share)) end,
      fun(Pid) -> t(Pid, when_recv_block(submit, valid_block)) end
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
    [{T, test, E, R} || {E, R} <- L].

when_connected(timeout) ->
    T = <<"when connected - timeout">>,
    L = [{{conn, #{event => timeout}},
          {stop,
           #{phase => disconnected, timer_phase => undefined,
             extra_nonce => undefined}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(authorize) ->
    T = <<"when connected - authorize">>,
    L = [{{conn, #{type => req, method => authorize, id => 0,
                   user => ?USER_NOT_IN_REGISTER, password => null}},
          {send,
           #{type => rsp, method => authorize, id => 0, reason => not_subscribed},
           #{phase => connected, timer_phase => connected,
            extra_nonce => undefined}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(submit) ->
    T = <<"when connected - submit">>,
    L = [{{conn, #{type => req, method => submit, id => 0,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID1,
                   miner_nonce => <<"0123456789">>, pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 0, reason => not_subscribed},
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
when_connected(chain_recv_block) ->
    T = <<"when connected - chain_recv_block">>,
    L = [{{chain, #{event => recv_block,
                    block => #{block_hash => ?BLOCK_HASH1,
                               block_version => ?BLOCK_VERSION,
                               block_target => ?BLOCK_TARGET1}}},
          {no_send,
           #{phase => connected, timer_phase => connected}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
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
    T = <<"when connected - subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 0,
                   user_agent => ?USER_AGENT, session_id => null,
                   host => ?HOST_VALID, port => ?PORT_VALID}},
          {send,
           #{type => rsp, method => subscribe, id => 0,
             result => [null, ?NONCE_MODULE:to_hex(?EXTRA_NONCE)]},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ?EXTRA_NONCE}}
         }],
    mock_subscribe(#{extra_nonce_nbytes => ?EXTRA_NONCE_NBYTES_VALID}),
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L].

when_configured(timeout) ->
    T = <<"when configured - timeout">>,
    L = [{{conn, #{event => timeout}},
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
                   user => ?USER_NOT_IN_REGISTER, password => null}},
          {send,
           #{type => rsp, method => authorize, id => 1, reason => not_subscribed},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(submit) ->
    T = <<"when configured - submit">>,
    L = [{{conn, #{type => req, method => submit, id => 1,
                   user => ?USER_NOT_IN_REGISTER, job_id => ?JOB_ID1,
                   miner_nonce => <<"0123456789">>, pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 1, reason => not_subscribed},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(not_req) ->
    T = <<"when configured - not_req">>,
    L = [{{conn, #{type => rsp, method => configure, id => 1, result => []}},
          {send,
           #{type => rsp, method => configure, id => 1, reason => unknown_error},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(jsonrpc_errors) ->
    T = <<"when configured - jsonrpc_errors">>,
    prep_configured(T) ++ jsonrpc_errors(T, configured, configured);
when_configured(subscribe) ->
    T = <<"when configured - subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 1,
                   user_agent => ?USER_AGENT, session_id => null,
                   host => ?HOST_VALID, port => ?PORT_VALID}},
          {send,
           #{type => rsp, method => subscribe, id => 1,
             result => [null, ?NONCE_MODULE:to_hex(?EXTRA_NONCE)]},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ?EXTRA_NONCE}}
         }],
    mock_subscribe(#{extra_nonce_nbytes => ?EXTRA_NONCE_NBYTES_VALID}),
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L].

when_subscribed(timeout) ->
    T = <<"when subscribed - timeout">>,
    L = [{{conn, #{event => timeout}},
          {stop,
           #{phase => disconnected, timer_phase => undefined,
             extra_nonce => undefined}}
         }],
    mock_subscribe(#{extra_nonce_nbytes => ?EXTRA_NONCE_NBYTES_VALID}),
    prep_subscribed(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(configure) ->
    T = <<"when subscribed - configure">>,
    L = [{{conn, #{type => req, method => configure, id => 2, params => []}},
          {send,
           #{type => rsp, method => configure, id => 2, reason => unknown_error},
           #{phase => subscribed, timer_phase => subscribed}}
         }],
    mock_subscribe(#{extra_nonce_nbytes => ?EXTRA_NONCE_NBYTES_VALID}),
    prep_subscribed(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(subscribe) ->
    T = <<"when subscribed - subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 2,
                   user_agent => ?USER_AGENT,  session_id => null,
                   host => ?HOST_VALID, port => ?PORT_VALID}},
          {send,
           #{type => rsp, method => subscribe, id => 2, reason => unknown_error},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ?EXTRA_NONCE}}
         }],
    mock_subscribe(#{extra_nonce_nbytes => ?EXTRA_NONCE_NBYTES_VALID}),
    prep_subscribed(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(submit) ->
    T = <<"when subscribed - submit">>,
    L = [{{conn, #{type => req, method => submit, id => 2,
                   user => ?USER_NOT_IN_REGISTER, job_id => ?JOB_ID1,
                   miner_nonce => <<"0123456789">>, pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 2, reason => unauthorized_worker},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ?EXTRA_NONCE}}
         }],
    mock_subscribe(#{extra_nonce_nbytes => ?EXTRA_NONCE_NBYTES_VALID}),
    prep_subscribed(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(not_req) ->
    T = <<"when subscribed - not_req">>,
    L = [{{conn, #{type => rsp, method => configure, id => 2, result => []}},
          {send,
           #{type => rsp, method => configure, id => 2, reason => unknown_error},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ?EXTRA_NONCE}}
         }],
    mock_subscribe(#{extra_nonce_nbytes => ?EXTRA_NONCE_NBYTES_VALID}),
    prep_subscribed(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(jsonrpc_errors) ->
    T = <<"when subscribed - jsonrpc_errors">>,
    mock_subscribe(#{extra_nonce_nbytes => ?EXTRA_NONCE_NBYTES_VALID}),
    prep_subscribed(T, #{}) ++ jsonrpc_errors(T, subscribed, subscribed);
when_subscribed(authorize_failure) ->
    T = <<"when subscribed - authorize_failure">>,
    L = [{{conn, #{type => req, method => authorize, id => 2,
                   user => ?USER_IN_REGISTER, password => null}},
           {send,
            #{type => rsp, method => authorize, id => 2, result => false},
            #{phase => subscribed, timer_phase => subscribed,
              extra_nonce => ?EXTRA_NONCE}}
         }],
    mock_authorize(#{}),
    prep_subscribed(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(authorize_success) ->
    T = <<"when subscribed - authorize_success">>,
    L = [{{conn, #{type => req, method => authorize, id => 2,
                   user => ?USER_NOT_IN_REGISTER, password => null}},
           {send,
            #{type => rsp, method => authorize, id => 2, result => true},
            #{phase => authorized, timer_phase => undefined,
              extra_nonce => ?EXTRA_NONCE}}
         }],
    mock_authorize(#{}),
    prep_subscribed(T, #{}) ++ [{T, test, E, R} || {E, R} <- L].

when_authorized(timeout) ->
    T = <<"when authorized - timeout">>,
    L = [{{conn, #{event => timeout}},
          {no_send,
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(configure) ->
    T = <<"when authorized - configure">>,
    L = [{{conn, #{type => req, method => configure, id => 3, params => []}},
          {send,
           #{type => rsp, method => configure, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(subscribe) ->
    T = <<"when authorized - subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 3,
                   user_agent => ?USER_AGENT, session_id => null,
                   host => ?HOST_VALID, port => ?PORT_VALID}},
          {send,
           #{type => rsp, method => configure, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(authorize) ->
    T = <<"when authorized - authorize">>,
    L = [{{conn, #{type => req, method => authorize, id => 3,
                   user => ?USER_IN_REGISTER, password => null}},
          {send,
           #{type => rsp, method => authorize, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(submit) ->
    T = <<"when authorized - submit">>,
    L = [{{conn, #{type => req, method => submit, id => 3,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID1,
                   miner_nonce => <<"0000">>, pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(not_req) ->
    T = <<"when authorized - not_req">>,
    L = [{{conn, #{type => rsp, method => subscribe, id => 3,
                   reason => parse_error, data => null}},
          {send,
           #{type => rsp, method => subscribe, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_authorize(#{}),
    prep_authorized(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(jsonrpc_errors) ->
    T = <<"when authorized - jsonrpc_errors">>,
    mock_authorize(#{}),
    prep_authorized(T, #{}) ++ jsonrpc_errors(T, authorized, undefined);
when_authorized(set_target) ->
    T = <<"when authorized - set_target">>,
    L = [{{chain, #{event => set_target}},
          {send,
           #{type => ntf, method => set_target,
             target => ?TARGET_MODULE:to_hex(?SHARE_TARGET)},
           #{phase => authorized, timer_phase => undefined,
             share_target => ?SHARE_TARGET}}
         }],
    mock_set_target(#{}),
    prep_authorized(T, #{}) ++ [{T, test, E, R} || {E, R} <- L].

when_set_target(timeout) ->
    T = <<"when set target - timeout">>,
    L = [{{conn, #{event => timeout}},
          {no_send,
           #{phase => authorized}}
         }],
    mock_set_target(#{}),
    prep_set_target(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(configure) ->
    T = <<"when set target - configure">>,
    L = [{{conn, #{type => req, method => configure, id => 3, params => []}},
          {send,
           #{type => rsp, method => configure, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_set_target(#{}),
    prep_set_target(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(subscribe) ->
    T = <<"when set target - subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 3,
                   user_agent => ?USER_AGENT, session_id => null,
                   host => ?HOST_VALID, port => ?PORT_VALID}},
          {send,
           #{type => rsp, method => configure, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_set_target(#{}),
    prep_set_target(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(authorize) ->
    T = <<"when set target - authorize">>,
    L = [{{conn, #{type => req, method => authorize, id => 3,
                   user => ?USER_IN_REGISTER, password => null}},
          {send,
           #{type => rsp, method => authorize, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_set_target(#{}),
    prep_set_target(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(not_req) ->
    T = <<"when set target - not_req">>,
    L = [{{conn, #{type => rsp, method => configure, id => 3,
                   reason => parse_error, data => null}},
          {send,
           #{type => rsp, method => subscribe, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock_set_target(#{}),
    prep_set_target(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(jsonrpc_errors) ->
    T = <<"when set target - jsonrpc_errors">>,
    mock_set_target(#{}),
    prep_set_target(T, #{}) ++ jsonrpc_errors(T, authorized, undefined).

when_set_target(recv_block, no_target_change) ->
    T = <<"when set target - recv_block, no_target_change">>,
    L = [{{chain, #{event => recv_block,
                    block => #{block_hash => ?BLOCK_HASH1,
                               block_version => ?BLOCK_VERSION,
                               block_target => ?BLOCK_TARGET1}}},
          {send,
           #{type => ntf, method => notify, job_id => ?JOB_ID1,
             block_hash => ?BLOCK_HASH1, block_version => ?BLOCK_VERSION,
             empty_queue => true},
           #{phase => authorized, accept_blocks => true}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_set_target(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_set_target(recv_block, target_change) ->
    T = <<"when set target - recv_block, target_change">>,
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
    prep_set_target(T, #{}) ++ [{T, test, E, R} || {E, R} <- L].

when_recv_block(submit, user_not_found) ->
    T = <<"when set target - submit, user_not_found">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_NOT_IN_REGISTER, job_id => ?JOB_ID1,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE), pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 4, reason => unauthorized_worker},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(submit, job_not_found) ->
    T = <<"when set target - submit, job_not_found">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID_NOT_IN_QUEUE,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE), pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 4, reason => job_not_found},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(submit, invalid_miner_nonce) ->
    T = <<"when set target - submit, invalid_miner_nonce">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID_IN_QUEUE,
                   miner_nonce => ?MINER_NONCE_BIN_INVALID, pow => ?POW}},
          {send,
           #{type => rsp, method => submit, id => 4, reason => unknown_error,
             data => <<"invalid_miner_nonce">>},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(submit, duplicate_share) ->
    T = <<"when set target - submit, duplicate_share">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID_IN_QUEUE,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE_DUPLICATE),
                   pow => ?POW_DUPLICATE}},
          {send,
           #{type => rsp, method => submit, id => 4, reason => duplicate_share},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(submit, invalid_solution) ->
    T = <<"when set target - submit, invalid_solution">>,
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
    prep_recv_block(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(submit, high_target_share) ->
    T = <<"when set target - submit, high_target_share">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID_IN_QUEUE,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE),
                   pow => ?POW_HIGH_TARGET}},
          {send,
           #{type => rsp, method => submit, id => 4, reason => low_difficulty_share},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(submit, valid_share) ->
    T = <<"when set target - submit, high_target_share">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID_IN_QUEUE,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE),
                   pow => ?POW_SHARE_TARGET}},
          {send,
           #{type => rsp, method => submit, id => 4, result => true},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T, #{}) ++ [{T, test, E, R} || {E, R} <- L];
when_recv_block(submit, valid_block) ->
    T = <<"when set target - submit, high_target_share">>,
    L = [{{conn, #{type => req, method => submit, id => 4,
                   user => ?USER_IN_REGISTER, job_id => ?JOB_ID_IN_QUEUE,
                   miner_nonce => ?NONCE_MODULE:to_hex(?MINER_NONCE),
                   pow => ?POW_BLOCK_TARGET}},
          {send,
           #{type => rsp, method => submit, id => 4, result => true},
           #{phase => authorized}}
         }],
    mock_recv_block(#{new_share_target => no_change}),
    prep_recv_block(T, #{}) ++ [{T, test, E, R} || {E, R} <- L].

mock_subscribe(#{extra_nonce_nbytes := ExtraNonceNBytes} = Opts) ->
    case maps:get(is_host_valid, Opts, true) of
        true  -> application:set_env(aestratum, host, ?HOST_VALID);
        false -> ok
    end,
    case maps:get(is_port_valid, Opts, true) of
        true  -> application:set_env(aestratum, port, ?PORT_VALID);
        false -> ok
    end,
    application:set_env(aestratum, extra_nonce_nbytes, ExtraNonceNBytes),
    meck:expect(?EXTRA_NONCE_CACHE_MODULE, get,
                fun(N) when N =:= ?EXTRA_NONCE_NBYTES_VALID ->
                        {ok, ?EXTRA_NONCE};
                   (N) when N =:= ?EXTRA_NONCE_NBYTES_INVALID ->
                        {error, extra_nonce_not_found}
                end),
    meck:expect(?EXTRA_NONCE_CACHE_MODULE, free, fun(_) -> ok end),
    ok.

mock_authorize(_Opts) ->
    mock_subscribe(#{extra_nonce_nbytes => ?EXTRA_NONCE_NBYTES_VALID}),
    meck:expect(?USER_REGISTER_MODULE, add, fun(_, _) -> ok end),
    meck:expect(?USER_REGISTER_MODULE, del, fun(_) -> ok end),
    meck:expect(?USER_REGISTER_MODULE, member,
                fun(U) when U =:= ?USER_IN_REGISTER     -> true;
                   (U) when U =:= ?USER_NOT_IN_REGISTER -> false
                end),
    ok.

mock_set_target(_Opts) ->
    mock_authorize(#{}),
    application:set_env(aestratum, initial_share_target, ?SHARE_TARGET).

mock_recv_block(Opts) ->
    mock_set_target(#{}),
    case maps:get(new_share_target, Opts, undefined) of
        NewShareTarget when NewShareTarget =/= undefined ->
            meck:expect(?TARGET_MODULE, diff, fun(_, _) -> NewShareTarget end);
        undefined ->
            ok
    end,
    case maps:get(share_target_diff_threshold, Opts, undefined) of
        ShareTargetDiffThreshold when ShareTargetDiffThreshold =/= undefined ->
            application:set_env(aestratum, share_target_diff_threshold,
                                ShareTargetDiffThreshold);
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
    ok.

prep_connected(T) ->
    L = [conn_init()],
    [{T, no_test, E, R} || {E, R} <- L].

prep_configured(T) ->
    L = [conn_init(),
         conn_configure(0)],
    [{T, no_test, E, R} || {E, R} <- L].

prep_subscribed(T, Opts) ->
    L = [conn_init(),
         conn_configure(0),
         conn_subscribe(1, Opts)],
    [{T, no_test, E, R} || {E, R} <- L].

prep_authorized(T, Opts) ->
    L = [conn_init(),
         conn_configure(0),
         conn_subscribe(1, Opts),
         conn_authorize(2, Opts)],
    [{T, no_test, E, R} || {E, R} <- L].

prep_set_target(T, Opts) ->
    L = [conn_init(),
         conn_configure(0),
         conn_subscribe(1, Opts),
         conn_authorize(2, Opts),
         chain_set_target(Opts)],
    [{T, no_test, E, R} || {E, R} <- L].

prep_recv_block(T, Opts) ->
    L = [conn_init(),
         conn_configure(0),
         conn_subscribe(1, Opts),
         conn_authorize(2, Opts),
         chain_set_target(Opts),
         chain_recv_block(Opts)],
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

conn_subscribe(Id, _Opts) ->
    {{conn, #{type => req, method => subscribe, id => Id,
              user_agent => ?USER_AGENT, session_id => null,
              host => ?HOST_VALID, port => ?PORT_VALID}},
     {send,
      #{type => rsp, method => subscribe, id => Id,
        result => [null, ?NONCE_MODULE:to_hex(?EXTRA_NONCE)]},
      #{phase => subscribed, timer_phase => subscribed,
        extra_nonce => ?EXTRA_NONCE}}
    }.

conn_authorize(Id, _Opts) ->
    {{conn, #{type => req, method => authorize, id => Id,
              user => ?USER_NOT_IN_REGISTER, password => null}},
     {send,
      #{type => rsp, method => authorize, id => Id, result => true},
      #{phase => authorized, timer_phase => undefined}}
    }.

chain_set_target(_Opts) ->
    {{chain, #{event => set_target}},
     {send,
      #{type => ntf, method => set_target,
        target => ?TARGET_MODULE:to_hex(?SHARE_TARGET)},
      #{phase => authorized, timer_phase => undefined}}
    }.

chain_recv_block(_Opts) ->
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

