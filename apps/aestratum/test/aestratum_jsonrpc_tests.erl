-module(aestratum_jsonrpc_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aestratum_jsonrpc).

-define(HEX_VALID_8,    <<"02468bDf">>).
-define(HEX_INVALID_8,  <<"0M46RTfa">>).
-define(HEX_VALID_16,   <<"0123456789AbCdEf">>).
-define(HEX_INVALID_16, <<"0097500937XXXacF">>).
-define(HEX_VALID_64,   <<?HEX_VALID_16/binary, ?HEX_VALID_16/binary,
                          ?HEX_VALID_16/binary, ?HEX_VALID_16/binary>>).
-define(HEX_INVALID_64, <<?HEX_INVALID_16/binary, ?HEX_INVALID_16/binary,
                          ?HEX_INVALID_16/binary, ?HEX_INVALID_16/binary>>).

jsonrpc_test_() ->
    {setup,
     fun() -> ok = application:ensure_started(jsx) end,
     fun(_) -> ok = application:stop(jsx) end,
     fun(_) -> [jsonrpc_decode(), jsonrpc_encode()] end}.

jsonrpc_decode() ->
    [dec(parse_error()),
     dec(invalid_msg_no_id()),
     dec(invalid_msg_with_id()),
     dec(invalid_method()),
     dec(invalid_params(configure)),
     dec(invalid_params(subscribe)),
     dec(invalid_params(authorize)),
     dec(invalid_params(submit)),
     dec(invalid_params(reconnect)),
     dec(invalid_params(set_target)),
     dec(invalid_params(notify)),
     dec(invalid_configure_param()),
     dec(invalid_subscribe_param()),
     dec(invalid_authorize_param()),
     dec(invalid_submit_param()),
     dec(invalid_reconnect_param()),
     dec(invalid_set_target_param()),
     dec(invalid_notify_param()),
     dec_rsp(invalid_success_result_param(configure)),
     vld_rsp(invalid_success_result_param(configure)),
     dec_rsp(invalid_success_result_param(subscribe)),
     vld_rsp(invalid_success_result_param(subscribe)),
     dec_rsp(invalid_success_result_param(authorize)),
     vld_rsp(invalid_success_result_param(authorize)),
     dec_rsp(invalid_success_result_param(submit)),
     vld_rsp(invalid_success_result_param(submit)),
     dec_rsp(invalid_decode_error_result_param()),
     vld_rsp(invalid_decode_error_result_param()),
     dec(valid_req()),
     dec(valid_ntf()),
     dec_rsp(valid_rsp()),
     vld_rsp(valid_rsp())].

jsonrpc_encode() ->
    [enc(invalid_configure_param()),
     enc(invalid_subscribe_param()),
     enc(invalid_authorize_param()),
     enc(invalid_submit_param()),
     enc(invalid_reconnect_param()),
     enc(invalid_set_target_param()),
     enc(invalid_notify_param()),
     enc(invalid_success_result_param(configure)),
     enc(invalid_success_result_param(subscribe)),
     enc(invalid_success_result_param(authorize)),
     enc(invalid_success_result_param(submit)),
     enc(invalid_encode_error_result_param()),
     enc2(valid_req()),
     enc2(valid_ntf()),
     enc3(valid_rsp())].

dec(Data) ->
    [{T,
      ?_assertEqual(R, ?TEST_MODULE:decode(B))
     } || {T, B, _M, R} <- Data].

dec_rsp(Data) ->
    [{T,
      ?_assertEqual(DR, ?TEST_MODULE:decode(B))
     } || {T, B, _M, DR, _VR} <- Data].

%% DR == {ok, DM} (DR - decode result, DM - decode map)
vld_rsp(Data) ->
    [{T,
      ?_assertEqual(VR, ?TEST_MODULE:validate_rsp(M, DM))
     } || {T, _B, #{method := M}, {ok, DM}, VR} <- Data].

enc(Data) ->
    [{T,
      ?_assertEqual(R, ?TEST_MODULE:encode(M))
     } || {T, _B, M, R} <- Data].

%% encode map, decode, check it's the same as before
enc2(Data) ->
    [begin
         {ok, B0} = ?TEST_MODULE:encode(M),
         {T, ?_assertEqual(R, ?TEST_MODULE:decode(B0))}
     end || {T, _B, M, R} <- Data].

%% encode rsp, decode, check it's the same as before
enc3(Data) ->
    [begin
         {ok, B0} = ?TEST_MODULE:encode(M),
         DR = {ok, DM} = ?TEST_MODULE:decode(B0),
         {T, ?_assertEqual(VR, ?TEST_MODULE:validate_rsp(Mt, DM))}
     end || {T, _B, #{method := Mt} = M, DR, VR} <- Data].

parse_error() ->
    T = <<"parse error">>,
    R = parse_error,
    L =
        [<<>>,
         <<"[1,2,3">>,
         <<"[100]}">>,
         <<"{\"\"\"jsonrpc\":\"2.0\"}">>,
         <<"{\"jsonrpc\"=\"2.0\",\"method\":\"foo.bar\",\"id\":\"1\"}">>],
    [{T, B, #{}, {error, R}} || B <- L].


invalid_msg_no_id() ->
     T = <<"invalid message without id">>,
     L =
        [{<<"{}">>,
          undefined},
         {<<"[1,2,3,{\"id\":4}]">>,
          undefined},
         {<<"{\"x\":[100]}">>,
          undefined},
         {<<"{\"jsonrpc\":\"2.0\"}">>,
          undefined},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":\"x\",\"method\":\"mining.configure\",\"params\":[]}">>,
          undefined},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"mining.configure\",\"params\":[]}">>,
          null},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":4294967296,\"method\":\"mining.configure\",\"params\":[]}">>,
          undefined},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":-1,\"method\":\"mining.configure\",\"params\":[]}">>,
          undefined},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":null,\"result\":null,\"error\":null}">>,
          null},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":null,\"result\":true,\"error\":null}">>,
          null},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"result\":null,\"error\":null}">>,
          undefined},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":null,\"result\":false,\"error\":[20,\"Error!\",null]}">>,
          null}],
    [{T, B, #{}, {error, {invalid_msg, I}}} || {B, I} <- L].

invalid_msg_with_id() ->
    T = <<"invalid message with id">>,
    L =
        [{<<"{\"id\":10}">>, 10},
         {<<"{\"array\":[1,2,3],\"id\":123}">>, 123},
         {<<"{\"jsonrpc\":\"2.0\", \"id\": 0}">>, 0},
         {<<"{\"jsonrpc\":\"1.0\",\"id\":1,\"method\":\"mining.configure\",\"params\":[]}">>, 1},
         {<<"{\"json-RPC\":\"2.0\",\"id\": 20,\"result\":null,\"error\":null}">>, 20},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":false,\"error\":[20,\"Error!\",null]}">>, 0}],
    [{T, B, #{}, {error, {invalid_msg, I}}} || {B, I} <- L].

invalid_method() ->
    T = <<"invalid method">>,
    L =
        [{<<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"foo\",\"params\":[]}">>, 1},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":12345,\"params\":[null]}">>, 10}],
    [{T, B, #{}, {error, {invalid_method, I}}} || {B, I} <- L].

invalid_params(configure) ->
    %% The same as invalid_configure_param
    [];
invalid_params(subscribe) ->
    T = <<"invalid params - subscribe">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"mining.subscribe\",\"params\":">>,
    I = 1,
    L =
        [[],
         [?HEX_VALID_16],
         [<<"x">>, null],
         [<<"aeminer/1">>, ?HEX_VALID_16, <<"aepool.com">>],
         [<<"x/1.0.0">>, ?HEX_VALID_16, <<"aepool.com">>, 9876, true]],
    [{T, bin(B, P), #{}, {error, {invalid_param, subscribe_params, I}}} || P <- L];
invalid_params(authorize) ->
    T = <<"invalid params - authorize">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"mining.authorize\",\"params\":">>,
    I = 1,
    L =
        [[],
         [null],
         [?HEX_VALID_64, 1234, true]],
    [{T, bin(B, P), #{}, {error, {invalid_param, authorize_params, I}}} || P <- L];
invalid_params(submit) ->
    T = <<"invalid params - submit">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"mining.submit\",\"params\":">>,
    I = 1,
    L =
        [[],
         [?HEX_VALID_16],
         [?HEX_INVALID_16, false],
         [[], ?HEX_VALID_16, lists:seq(1, 42)],
         [1234, ?HEX_VALID_16, <<"aepool.com">>, 9876, <<>>]],
    [{T, bin(B, P), #{}, {error, {invalid_param, submit_params, I}}} || P <- L];
invalid_params(reconnect) ->
    T = <<"invalid params - reconnect">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"client.reconnect\",\"params\":">>,
    I = 1,
    L =
        [[[]],
         [<<>>],
         [<<"foo">>, 9999],
         [null, 9999, 100, 200]],
    [{T, bin(B, P), #{}, {error, {invalid_param, reconnect_params, I}}} || P <- L];
invalid_params(set_target) ->
    T = <<"invalid params - set_target">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"mining.set_target\",\"params\":">>,
    I = null,
    L =
        [[],
         [<<>>, <<>>],
         [true, false, false]],
    [{T, bin(B, P), #{}, {error, {invalid_param, set_target_params, I}}} || P <- L];
invalid_params(notify) ->
    T = <<"invalid params - notify">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"mining.notify\",\"params\":">>,
    I = null,
    L =
        [[],
         [null],
         [?HEX_VALID_16, 1],
         [?HEX_VALID_64, true, 10],
         [?HEX_VALID_16, 1, ?HEX_VALID_64, true, <<>>]],
    [{T, bin(B, P), #{}, {error, {invalid_param, notify_params, I}}} || P <- L].

invalid_configure_param() ->
    T = <<"invalid configure request param">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"mining.configure\",\"params\":">>,
    I = 0,
    M = #{type => req, id => I, method => configure},
    L =
        [{[null], configure_params},
         {[<<>>], configure_params},
         {[<<"param">>], configure_params},
         {[<<"x">>, <<"y">>], configure_params}],
    [{T, bin(B, P), M#{params => P}, {error, {invalid_param, R, I}}} || {P, R} <- L].

invalid_subscribe_param() ->
    T = <<"invalid subscribe request param">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"mining.subscribe\",\"params\":">>,
    I = 1,
    M = #{type => req, id => I, method => subscribe},
    L =
        [{[<<>>, ?HEX_VALID_16, <<"aepool.com">>, 9876], user_agent},
         {[<<"ae\nminer">>, ?HEX_VALID_16, <<"aepool.com">>, 9876], user_agent},
         {[<<"    ">>, ?HEX_VALID_16, <<"aepool.com">>, 9876], user_agent},
         {[0, ?HEX_VALID_16, <<"aepool.com">>, 9876], user_agent},
         {[[1,2], ?HEX_VALID_16, <<"aepool.com">>, 9876], user_agent},
         {[<<"aeminer/1.0">>, <<>>, <<"aepool.com">>, 9876], session_id},
         {[<<"aeminer/1.0">>, <<"invalid">>, <<"aepool.com">>, 9876], session_id},
         {[<<"miner/1">>, <<?HEX_VALID_16/binary, <<$a>>/binary>>, <<"aepool.com">>, 9876], session_id},
         {[<<"miner/1.0">>, ?HEX_INVALID_16, <<"aepool.com">>, 9876], session_id},
         {[<<"miner/1.0">>, ?HEX_VALID_16, <<>>, 9876], host},
         {[<<"miner/1.0">>, null, <<>>, 9876], host},
         {[<<"miner/1.0">>, null, <<$\v>>, 9876], host},
         {[<<"miner/1.0">>, null, <<"host\n">>, 9876], host},
         {[<<"miner/1.0">>, null, <<97:256/unit:8>>, 9876], host},
         {[<<"miner/1.0">>, ?HEX_VALID_16, null, 0], port},
         {[<<"miner/1.0">>, ?HEX_VALID_16, null, -1], port},
         {[<<"miner/1.0">>, ?HEX_VALID_16, null, 65536], port},
         {[<<"miner/1.0">>, ?HEX_VALID_16, <<"aepool.com">>, <<"foo">>], port},
         {[<<"miner/1.0">>, ?HEX_VALID_16, <<"aepool.com">>, <<"1234">>], port}],
    [{T, bin(B, P), M#{user_agent => P1, session_id => P2, host => P3, port => P4},
      {error, {invalid_param, R, I}}
     } || {[P1, P2, P3, P4] = P, R} <- L].

invalid_authorize_param() ->
    T = <<"invalid authorize request param">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"mining.authorize\",\"params\":">>,
    I = 1,
    M = #{type => req, id => I, method => authorize},
    L =
        [{[<<>>, ?HEX_VALID_64], user},
         {[null, ?HEX_VALID_64], user},
         {[<<"user ">>, ?HEX_VALID_64], user},
         {[<<"\t\t\s\v">>, ?HEX_VALID_64], user},
         {[1234, ?HEX_VALID_64], user},
         {[binary:copy(<<$a>>, 65), ?HEX_VALID_64], user},
         {[<<"user1">>, <<>>], password},
         {[<<"user1">>, 1324], password},
         {[<<"user1">>, <<"X">>], password},
         {[<<"user1">>, ?HEX_VALID_16], password},
         {[<<"user1">>, ?HEX_INVALID_64], password}],
    [{T, bin(B, P), M#{user => P1, password => P2},
      {error, {invalid_param, R, I}}
     } || {[P1, P2] = P, R} <- L].

invalid_submit_param() ->
    T = <<"invalid submit request param">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"mining.submit\",\"params\":">>,
    I = 1,
    M = #{type => req, id => I, method => submit},
    L =
        [{[<<>>, ?HEX_VALID_16, ?HEX_VALID_8, lists:seq(1, 42)], user},
         {[false, ?HEX_VALID_16, ?HEX_VALID_8, lists:seq(1, 42)], user},
         {[1234, ?HEX_VALID_16, <<"aepool.com">>, 9876], user},
         {[<<" x ">>, ?HEX_VALID_16, <<"aepool.com">>, 9876], user},
         {[binary:copy(<<$1>>, 65), ?HEX_VALID_16, ?HEX_VALID_8, lists:seq(1, 42)], user},
         {[<<"user1">>, <<>>, ?HEX_VALID_8, lists:seq(1, 42)], job_id},
         {[<<"user1">>, 4894132, ?HEX_VALID_8, lists:seq(1, 42)], job_id},
         {[<<"user1">>, <<"error string">>, ?HEX_VALID_8, lists:seq(1, 42)], job_id},
         {[<<"user1">>, null, ?HEX_VALID_8, lists:seq(1, 42)], job_id},
         {[<<"user1">>, ?HEX_INVALID_16, ?HEX_VALID_8, lists:seq(1, 42)], job_id},
         {[<<"user1">>, ?HEX_VALID_16, <<>>, lists:seq(1, 42)], miner_nonce},
         {[<<"user1">>, ?HEX_VALID_16, <<"0">>, lists:seq(1, 42)], miner_nonce},
         {[<<"user1">>, ?HEX_VALID_16, null, lists:seq(1, 42)], miner_nonce},
         {[<<"user1">>, ?HEX_VALID_16, [], lists:seq(1, 42)], miner_nonce},
         {[<<"user1">>, ?HEX_VALID_16, ?HEX_INVALID_8, lists:seq(1, 42)], miner_nonce},
         {[<<"user1">>, ?HEX_VALID_16, ?HEX_VALID_16, lists:seq(1, 42)], miner_nonce},
         {[<<"user1">>, ?HEX_VALID_16, ?HEX_VALID_8, <<>>], pow},
         {[<<"user1">>, ?HEX_VALID_16, ?HEX_VALID_8, [0,1,2]], pow},
         {[<<"user1">>, ?HEX_VALID_16, ?HEX_VALID_8, null], pow},
         {[<<"user1">>, ?HEX_VALID_16, ?HEX_VALID_8, lists:seq(1, 41)], pow},
         {[<<"user1">>, ?HEX_VALID_16, ?HEX_VALID_8, [4294967296 | lists:seq(1, 41)]], pow},
         {[<<"user1">>, ?HEX_VALID_16, ?HEX_VALID_8, [-1 | lists:seq(1, 41)]], pow},
         {[<<"user1">>, ?HEX_VALID_16, ?HEX_VALID_8, lists:seq(1, 43)], pow}],
    [{T, bin(B, P), M#{user => P1, job_id => P2, miner_nonce => P3, pow => P4},
      {error, {invalid_param, R, I}}
     } || {[P1, P2, P3, P4] = P, R} <- L].

invalid_reconnect_param() ->
    T = <<"invalid reconnect request param">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"client.reconnect\",\"params\":">>,
    I = 1,
    M = #{type => req, id => I, method => reconnect},
    L =
        [{[<<>>, 9999, 100], host},
         {[8904321, 9999, 100], host},
         {[false, 9999, 100], host},
         {[[$x], 9999, 100], host},
         {[<<"ae. pool.com">>, 9999, 100], host},
         {[binary:copy(<<$x>>, 256), 9999, 100], host},
         {[null, 0, 100], port},
         {[<<"aepool.com">>, true, 100], port},
         {[<<"aepool.com">>, <<"1234">>, 100], port},
         {[null, 65536, 100], port},
         {[null, 9999, 0], wait_time},
         {[null, 9999, false], wait_time},
         {[null, 9999, <<>>], wait_time},
         {[null, 9999, 24 * 60 * 60 + 1], wait_time}],
    [{T, bin(B, P), M#{host => P1, port => P2},
      {error, {invalid_param, R, I}}
     } || {[P1, P2] = P, R} <- L].

invalid_set_target_param() ->
    T = <<"invalid set_target notification param">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"mining.set_target\",\"params\":">>,
    I = null,
    M = #{type => ntf, id => I, method => set_target},
    L =
        [{[<<>>], target},
         {[1234], target},
         {[?HEX_VALID_16], target},
         {[<<?HEX_VALID_64/binary, <<$a>>/binary>>], target},
         {[?HEX_INVALID_64], target}],
    [{T, bin(B, P), M#{target => P1},
      {error, {invalid_param, R, I}}
     } || {[P1] = P, R} <- L].

invalid_notify_param() ->
    T = <<"invalid notify notification param">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"mining.notify\",\"params\":">>,
    I = null,
    M = #{type => ntf, id => I, method => notify},
    L =
        [{[<<>>, 1, ?HEX_VALID_64, true], job_id},
         {[7895234432, 1, ?HEX_VALID_64, true], job_id},
         {[null, 1, ?HEX_VALID_64, true], job_id},
         {[true, 1, ?HEX_VALID_64, true], job_id},
         {[<<?HEX_VALID_16/binary, <<$a>>/binary>>, 1, ?HEX_VALID_64, true], job_id},
         {[?HEX_INVALID_16, 1, ?HEX_VALID_64, true], job_id},
         {[?HEX_VALID_16, <<>>, ?HEX_VALID_64, true], block_version},
         {[?HEX_VALID_16, null, ?HEX_VALID_64, true], block_version},
         {[?HEX_VALID_16, -1, ?HEX_VALID_64, true], block_version},
         {[?HEX_VALID_16, 0, ?HEX_VALID_64, true], block_version},
         {[?HEX_VALID_16, 4294967296, ?HEX_VALID_64, true], block_version},
         {[?HEX_VALID_16, null, ?HEX_VALID_64, true], block_version},
         {[?HEX_VALID_16, 1, <<>>, true], block_hash},
         {[?HEX_VALID_16, 1, null, true], block_hash},
         {[?HEX_VALID_16, 1, true, true], block_hash},
         {[?HEX_VALID_16, 1, 1234987, true], block_hash},
         {[?HEX_VALID_16, 1, ?HEX_INVALID_64, true], block_hash},
         {[?HEX_VALID_16, 1, <<?HEX_VALID_64/binary, 1:8>>, true], block_hash},
         {[?HEX_VALID_16, 1, ?HEX_VALID_64, <<>>], empty_queue},
         {[?HEX_VALID_16, 1, ?HEX_VALID_64, null], empty_queue},
         {[?HEX_VALID_16, 1, ?HEX_VALID_64, 129039], empty_queue},
         {[?HEX_VALID_16, 1, ?HEX_VALID_64, <<"true">>], empty_queue}],
    [{T, bin(B, P), M#{job_id => P1, block_version => P2, block_hash => P3, empty_queue => P4},
      {error, {invalid_param, R, I}}
     } || {[P1, P2, P3, P4] = P, R} <- L].

invalid_success_result_param(configure) ->
    T = <<"invalid successful result param - configure">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":null,\"result\":">>,
    I = 1,
    M = #{type => rsp, id => I},
    L =
        [{[<<>>], configure_params},
         {[null], configure_params},
         {[<<"something">>], configure_params}],
    [{T, bin(B, P), M#{method => configure, result => P},
      {ok, M#{result => P}}, {error, {invalid_param, R, I}}
     } || {P, R} <- L];
invalid_success_result_param(subscribe) ->
    T = <<"invalid successful result param - subscribe">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":null,\"result\":">>,
    I = 1,
    M = #{type => rsp, id => I},
    L =
        [{[<<>>, ?HEX_VALID_8], session_id},
         {[true, ?HEX_VALID_8], session_id},
         {[<<"id">>, ?HEX_VALID_8], session_id},
         {[443243, ?HEX_VALID_8], session_id},
         {[?HEX_INVALID_16, ?HEX_VALID_8], session_id},
         {[?HEX_VALID_16, false], extra_nonce},
         {[?HEX_VALID_16, null], extra_nonce},
         {[?HEX_VALID_16, 43439], extra_nonce},
         {[?HEX_VALID_16, ?HEX_INVALID_16], extra_nonce},
         {[?HEX_VALID_16, <<>>], extra_nonce},
         {[?HEX_VALID_16, <<"0">>], extra_nonce},
         {[?HEX_VALID_16, ?HEX_INVALID_8], extra_nonce},
         {[?HEX_VALID_16, ?HEX_VALID_16], extra_nonce}],
    [{T, bin(B, P), M#{method => subscribe, result => P},
      {ok, M#{result => P}}, {error, {invalid_param, R, I}}
     } || {P, R} <- L];
invalid_success_result_param(authorize) ->
    T = <<"invalid successful result param - authorize">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":null,\"result\":">>,
    I = 1,
    M = #{type => rsp, id => I},
    L =
        [{12345, authorize_params},
         {[[]], authorize_params},
         {<<"foo">>, authorize_params},
         {<<"true">>, authorize_params},
         {[[],[]], authorize_params},
         {[true, false, true], authorize_params}],
    [{T, bin(B, P), M#{method => authorize, result => P},
      {ok, M#{result => P}}, {error, {invalid_param, R, I}}
     } || {P, R} <- L];
invalid_success_result_param(submit) ->
    T = <<"invalid successful result param - authorize">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":null,\"result\":">>,
    I = 1,
    M = #{type => rsp, id => I},
    L =
        [{0, submit_params},
         {<<"foo">>, submit_params},
         {[[[]]], submit_params},
         {<<"false">>, submit_params},
         {[true], submit_params}],
    [{T, bin(B, P), M#{method => submit, result => P},
      {ok, M#{result => P}}, {error, {invalid_param, R, I}}
     } || {P, R} <- L].

%% decode, validate
invalid_decode_error_result_param() ->
    T = <<"invalid decode error result param">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":null,\"error\":">>,
    I = 1,
    M = #{type => rsp, id => I},
    L =
        [{[<<>>, <<"Error msg">>, null], error_code},
         {[null, <<"Error msg">>, null], error_code},
         {[0, <<"Error msg">>, null], error_code},
         {[100000, <<"Error msg">>, null], error_code},
         {[-32700, <<>>, null], error_msg},
         {[20, binary:copy(<<$x>>, 256), null], error_msg},
         {[20, 20, 20, null], error_params},
         {[20], error_params}],
    [{T, bin(B, P), #{}, {ok, M#{error => P}}, {error, {invalid_param, R, I}}}
     || {P, R} <- L].

%% encode
invalid_encode_error_result_param() ->
    T = <<"invalid encode error result param">>,
    B = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":null,\"error\":">>,
    I = 1,
    M = #{type => rsp, id => I},
    L =
        [{[<<>>, null], error_reason},
         {[null, null], error_reason},
         {[0, null], error_reason},
         {[100000, null], error_reason},
         {[parse_error, <<>>], error_data},
         {[duplicate_share, <<0:512/unit:8>>], error_data}],
    [{T, bin(B, P), M#{reason => P1, data => P2}, {error, {invalid_param, R, I}}}
     || {{[P1, P2] = P}, R} <- L].

valid_req() ->
    T = <<"valid client request">>,
    L =
        [%% configure
         {<<"{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"mining.configure\",\"params\":[]}">>,
          #{type => req, method => configure, id => 0, params => []}},
         %% subscribe
         {<<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"mining.subscribe\",\"params\":["
            "\"miner/1.0.0\",null,\"aeternity.pool.com\",65535]}">>,
          #{type => req, method => subscribe, id => 1, user_agent => <<"miner/1.0.0">>,
            session_id => null, host => <<"aeternity.pool.com">>, port => 65535}},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"mining.subscribe\",\"params\":["
            "\"miner/1.0.0\",\"0123456789aBcDeF\",\"aepool.com\",9876]}">>,
          #{type => req, method => subscribe, id => 2, user_agent => <<"miner/1.0.0">>,
            session_id => <<"0123456789abcdef">>, host => <<"aepool.com">>, port => 9876}},
         %% authorize
         {<<"{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"mining.authorize\",\"params\":["
            "\"ae_user\",\"0123456789aBcDeF0123456789abcdef0123456789abcdef0123456789abcdef\"]}">>,
          #{type => req, method => authorize, id => 3, user => <<"ae_user">>,
            password => <<"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef">>}},
         %% submit
         {<<"{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"mining.submit\",\"params\":["
            "\"ae_user\",\"ABCDEF0123456789\",\"0123456789ABC\",["
            "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,"
            "31,32,33,34,35,36,37,38,39,40,41,42]]}">>,
          #{type => req, method => submit, id => 4, user => <<"ae_user">>,
            job_id => <<"abcdef0123456789">>, miner_nonce => <<"0123456789abc">>,
            pow => lists:seq(1, 42)}},
         %% reconnect
         {<<"{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"client.reconnect\",\"params\":[]}">>,
          #{type => req, method => reconnect, id => 5, host => null, port => null,
            wait_time => 0}},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"client.reconnect\",\"params\":["
            "null,null,100]}">>,
          #{type => req, method => reconnect, id => 10, host => null, port => null,
            wait_time => 100}},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"client.reconnect\",\"params\":["
            "\"aepool.com\",null,999]}">>,
          #{type => req, method => reconnect, id => 11, host => <<"aepool.com">>,
            port => null, wait_time => 999}},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":12,\"method\":\"client.reconnect\",\"params\":["
            "\"aepool.com\",9999,200]}">>,
          #{type => req, method => reconnect, id => 12, host => <<"aepool.com">>,
            port => 9999, wait_time => 200}}],
    [{T, B, M, {ok, M}} || {B, M} <- L].

valid_ntf() ->
    T = <<"valid notification">>,
    L =
        [%% set_target
         {<<"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"mining.set_target\",\"params\":["
            "\"ffFF000000000000000000000000000000000000000000000000000000000000\"]}">>,
          #{type => ntf, method => set_target, id => null,
            target => <<"ffff000000000000000000000000000000000000000000000000000000000000">>}},
         %% notify
         {<<"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"mining.notify\",\"params\":["
            "\"0123456789ABCdef\",1,"
            "\"0123456789aBcDeF0123456789abcdef0123456789abcdef0123456789abcdef\",true]}">>,
          #{type => ntf, method => notify, id => null, job_id => <<"0123456789abcdef">>,
            block_version => 1,
            block_hash => <<"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef">>,
            empty_queue => true}}],
    [{T, B, M, {ok, M}} || {B, M} <- L].

valid_rsp() ->
    T = <<"valid response">>,
    L =
        [%% success result - configure
         {<<"{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":null,\"result\":[]}">>,
          #{type => rsp, method => configure, id => 1, result => []},
          undefined},
         %% success result - subscribe
         {<<"{\"jsonrpc\":\"2.0\",\"id\":2,\"error\":null,\"result\":["
            "\"0123456789abcdef\",\"0123456789\"]}">>,
          #{type => rsp, method => subscribe, id => 2,
            result => [<<"0123456789abcdef">>, <<"0123456789">>]},
          undefined},
         %% success result - authorize/submit
         {<<"{\"jsonrpc\":\"2.0\",\"id\":3,\"error\":null,\"result\":false}">>,
          #{type => rsp, method => authorize, id => 3, result => false},
          undefined},
         %% error result
         {<<"{\"jsonrpc\":\"2.0\",\"id\":10,\"result\":null,\"error\":["
            "-32700,\"Parse error\",null]}">>,
          #{type => rsp, method => subscribe, id => 10, reason => parse_error,
            msg => <<"Parse error">>, data => null},
          [-32700, <<"Parse error">>, null]},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":11,\"result\":null,\"error\":["
            "-32603,\"Internal error\",\"Server crash\"]}">>,
          #{type => rsp, method => submit, id => 11, reason => internal_error,
            msg => <<"Internal error">>, data => <<"Server crash">>},
          [-32603, <<"Internal error">>, <<"Server crash">>]},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":null,\"result\":null,\"error\":["
            "-32603,\"Internal error\",null]}">>,
          #{type => rsp, method => undefined, id => null,
            reason => internal_error, msg => <<"Internal error">>, data => null},
          [-32603, <<"Internal error">>, null]},
         {<<"{\"jsonrpc\":\"2.0\",\"id\":null,\"result\":null,\"error\":["
            "-32000,\"Invalid request\",null]}">>,
          #{type => rsp, method => subscribe, id => null,
            reason => invalid_msg, msg => <<"Invalid request">>, data => null},
          [-32000, <<"Invalid request">>, null]}],
    [{T, B, M,
      {ok, maps:without([method], M)}, {ok, M}
     } || {B, M, P} <- L, P =:= undefined] ++
    [{T, B, M,
      {ok, maps:merge(#{error => P}, maps:without([method, reason, msg, data], M))}, {ok, M}
     } || {B, M, P} <- L, is_list(P)].

bin(B, P) -> list_to_binary([B, q(P), <<"}">>]).

%% quote
q(null) -> <<"null">>;
q(true) -> <<"true">>;
q(false) -> <<"false">>;
q([]) -> <<"[]">>;
q(X) when is_binary(X) -> list_to_binary([$", X, $"]);
q(X) when is_integer(X) -> integer_to_binary(X);
q(Xs) when is_list(Xs) ->
    Xs1 = lists:map(fun(Y) -> q(Y) end, Xs),
    list_to_binary([$[, lists:join($,, Xs1), $]]).

