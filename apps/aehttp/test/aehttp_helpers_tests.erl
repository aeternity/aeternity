-module(aehttp_helpers_tests).

%% Tests for the parts of aehttp_helpers that do not need a running node.

-include_lib("eunit/include/eunit.hrl").

%% The 503 responses are built here rather than in the dispatch modules so that
%% the external, internal and Rosetta APIs cannot drift apart. The shape matters
%% to cowboy: header names must be lowercase binaries, and Retry-After must be a
%% binary delay-seconds value, not an integer.
service_unavailable_test_() ->
    [ {"queue rejection is reported as a temporary overload",
       fun() ->
           {Code, Headers, Body} = aehttp_helpers:service_unavailable(overload),
           ?assertEqual(503, Code),
           ?assertEqual(#{reason => <<"Temporary overload">>}, Body),
           ?assertEqual([{<<"retry-after">>, <<"3">>}], Headers)
       end}
    , {"an unstable node gets the more conservative back-off hint",
       fun() ->
           {Code, Headers, Body} = aehttp_helpers:service_unavailable(not_stable),
           ?assertEqual(503, Code),
           ?assertEqual(#{reason => <<"Not yet started">>}, Body),
           ?assertEqual([{<<"retry-after">>, <<"30">>}], Headers)
       end}
    , {"every 503 carries a Retry-After clients can parse",
       fun() ->
           lists:foreach(
             fun(Reason) ->
                 {503, Headers, _} = aehttp_helpers:service_unavailable(Reason),
                 {_, Value} = lists:keyfind(<<"retry-after">>, 1, Headers),
                 ?assert(is_binary(Value)),
                 ?assert(binary_to_integer(Value) > 0)
             end, [overload, not_stable])
       end}
    ].

%% The helper only pays off if the dispatch modules actually call it. Each of
%% them catches the same two errors, and nothing above notices if one goes back
%% to building its own 503 - the drift the helper exists to prevent is exactly
%% what the tests above cannot see. So drive the real handle_request/3 with the
%% mode check and the request queue stubbed out, and assert all three APIs shed
%% load identically.
dispatch_shedding_test_() ->
    {foreach,
     fun() ->
         meck:new(app_ctrl, [non_strict]),
         meck:new(aec_jobs_queues, [non_strict])
     end,
     fun(_) ->
         meck:unload(aec_jobs_queues),
         meck:unload(app_ctrl)
     end,
     [ {"a rejected request is shed as an overload by every API",
        fun() ->
            meck:expect(app_ctrl, await_stable_mode, fun(_) -> {ok, stable} end),
            meck:expect(aec_jobs_queues, run,
                        fun(_Queue, _F) -> erlang:error({rejected, counter}) end),
            [ assert_shed(Mod, OpId, <<"3">>, <<"Temporary overload">>)
              || {Mod, OpId} <- dispatchers() ]
        end}
     , {"an unstable node is shed as not-yet-started by every API",
        fun() ->
            meck:expect(app_ctrl, await_stable_mode, fun(_) -> {timeout, starting} end),
            %% The queue would have admitted this one: in all three modules
            %% when_stable/1 wraps the queue, so the mode check sheds first.
            meck:expect(aec_jobs_queues, run, fun(_Queue, F) -> F() end),
            [ assert_shed(Mod, OpId, <<"30">>, <<"Not yet started">>)
              || {Mod, OpId} <- dispatchers() ]
        end}
     ]}.

%% One read operation per dispatch module. It is never run - it only has to be
%% an operation the module's own queue/1 recognises.
dispatchers() ->
    [ {aehttp_dispatch_ext    , 'GetTopBlock'}
    , {aehttp_dispatch_int    , 'GetNetworkStatus'}
    , {aehttp_dispatch_rosetta, networkList}
    ].

assert_shed(Mod, OpId, RetryAfter, Reason) ->
    Actual   = Mod:handle_request(OpId, #{}, #{}),
    Expected = {503, [{<<"retry-after">>, RetryAfter}], #{reason => Reason}},
    %% Tagged with the module so a failure names the API that drifted.
    ?assertEqual({Mod, Expected}, {Mod, Actual}),
    %% aehttp_api_handler:to_headers/1 hands this list to cowboy as a map, so
    %% the conversion has to be lossless or the back-off never reaches a client.
    {503, Headers, _} = Actual,
    ?assertEqual(#{<<"retry-after">> => RetryAfter}, maps:from_list(Headers)).
