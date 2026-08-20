%%%-------------------------------------------------------------------
%%% @doc Coverage for the `/v3/rpc/ws' idle policy.
%%%
%%% Cowboy's `idle_timeout' counts time with no INBOUND data and
%%% defaults to 60000 (`cowboy_websocket.erl:378'). A subscription
%%% client subscribes once and then only receives, so under that default
%%% it was closed at 60.0s however many frames the server had pushed to
%%% it -- outbound traffic does not reset the timer.
%%%
%%% Every case here fails under the default. `init/2' returning the
%%% three-tuple has no options map to inspect, so the first case does
%%% not match at all; there is no `ws_opts/0' or `ping_interval/0' to
%%% call; and nothing schedules a keepalive, so no timer reference
%%% reaches the connection state.
%%% @end
%%%-------------------------------------------------------------------
-module(aehttp_rpc_ws_handler_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% Cowboy's own default, restated so the assertions below are about a
%% number this endpoint chose rather than one it inherited.
-define(COWBOY_DEFAULT_IDLE_MS, 60000).

%% ===================================================================
%% The idle policy itself
%% ===================================================================

idle_policy_test_() ->
    [{"init/2 supplies an explicit Cowboy options map",
      fun() ->
          Req = #{fake_req => true},
          %% The four-tuple form. Under the three-tuple Cowboy applies
          %% its own defaults and there is nothing here to inspect.
          Result = in_clean_process(
                     fun() -> aehttp_rpc_ws_handler:init(Req, []) end),
          ?assertMatch({cowboy_websocket, _Req, _State, _Opts}, Result),
          {cowboy_websocket, GotReq, _State, Opts} = Result,
          ?assertEqual(Req, GotReq),
          ?assert(is_map(Opts)),
          ?assertEqual(aehttp_rpc_ws_handler:ws_opts(), Opts)
      end},
     {"the idle timeout is set, and is not Cowboy's 60s default",
      fun() ->
          Opts = aehttp_rpc_ws_handler:ws_opts(),
          ?assert(maps:is_key(idle_timeout, Opts)),
          Idle = maps:get(idle_timeout, Opts),
          ?assert(is_integer(Idle)),
          ?assertNotEqual(?COWBOY_DEFAULT_IDLE_MS, Idle),
          ?assert(Idle > ?COWBOY_DEFAULT_IDLE_MS)
      end},
     {"the timeout stays finite, so a dead peer is still reaped",
      fun() ->
          %% `infinity' would trade a wrong disconnect for a connection
          %% process that outlives its peer indefinitely.
          ?assertNotEqual(infinity,
                          maps:get(idle_timeout,
                                   aehttp_rpc_ws_handler:ws_opts()))
      end},
     {"the ping interval leaves room for a missed pong",
      fun() ->
          %% The relationship is the mechanism. Both values could be
          %% explicit, and a ping interval above the idle timeout would
          %% still never keep a connection alive.
          Interval = aehttp_rpc_ws_handler:ping_interval(),
          Idle = maps:get(idle_timeout, aehttp_rpc_ws_handler:ws_opts()),
          ?assert(Interval > 0),
          ?assert(Interval * 2 =< Idle)
      end},
     {"the ping interval also clears a default reverse-proxy read timeout",
      fun() ->
          %% nginx's proxy_read_timeout defaults to 60s too, so a
          %% keepalive tuned only to Cowboy would still be cut in front
          %% of the node.
          ?assert(aehttp_rpc_ws_handler:ping_interval()
                    < ?COWBOY_DEFAULT_IDLE_MS)
      end}].

%% ===================================================================
%% The keepalive
%% ===================================================================

keepalive_test_() ->
    [{"websocket_init/1 schedules the first keepalive",
      fun() ->
          {ok, State} = aehttp_rpc_ws_handler:websocket_init(#{}),
          Ref = maps:get(ping_ref, State),
          ?assert(is_reference(Ref)),
          cancel(Ref)
      end},
     {"the keepalive sends a ping AND schedules the next one",
      fun() ->
          %% Firing once would leave the connection to die at the second
          %% idle window, so the reschedule is as load-bearing as the
          %% ping. A fresh timer reference is what proves it happened.
          {ok, State1} = aehttp_rpc_ws_handler:websocket_init(#{}),
          Ref1 = maps:get(ping_ref, State1),
          {reply, ping, State2} =
              aehttp_rpc_ws_handler:websocket_info(
                aehttp_rpc_ws_handler:keepalive_message(), State1),
          Ref2 = maps:get(ping_ref, State2),
          ?assert(is_reference(Ref2)),
          ?assertNotEqual(Ref1, Ref2),
          cancel(Ref1),
          cancel(Ref2)
      end},
     {"an unrelated info message neither pings nor reschedules",
      fun() ->
          {ok, State1} = aehttp_rpc_ws_handler:websocket_init(#{}),
          Ref1 = maps:get(ping_ref, State1),
          ?assertEqual({ok, State1},
                       aehttp_rpc_ws_handler:websocket_info(
                         not_ours, State1)),
          cancel(Ref1)
      end},
     {"a notification still goes out, and does not disturb the keepalive",
      fun() ->
          {ok, State1} = aehttp_rpc_ws_handler:websocket_init(#{}),
          Ref1 = maps:get(ping_ref, State1),
          {reply, {text, Encoded}, State2} =
              aehttp_rpc_ws_handler:websocket_info(
                {aerpc_notify, <<"0x1">>, #{<<"number">> => <<"0x2">>}},
                State1),
          ?assertEqual(Ref1, maps:get(ping_ref, State2)),
          Decoded = jsx:decode(Encoded, [return_maps]),
          ?assertEqual(<<"eth_subscription">>,
                       maps:get(<<"method">>, Decoded)),
          cancel(Ref1)
      end},
     {"a pong is accepted quietly",
      fun() ->
          %% Cowboy has already reset the idle timer by the time this
          %% arrives; the handler must not treat it as a protocol error.
          ?assertEqual({ok, state},
                       aehttp_rpc_ws_handler:websocket_handle(
                         {pong, <<>>}, state))
      end}].

%% ===================================================================
%% Helpers
%% ===================================================================

%% init/2 sets trap_exit on the calling process, which is not something
%% to do to an eunit worker.
in_clean_process(Fun) ->
    Self = self(),
    spawn(fun() -> Self ! {result, Fun()} end),
    receive {result, R} -> R
    after 5000 -> exit(timeout)
    end.

%% The real interval is 30s, so a timer left behind would sit in this
%% process's table long after the case ends.
cancel(Ref) when is_reference(Ref) ->
    _ = erlang:cancel_timer(Ref),
    ok.

-endif.
