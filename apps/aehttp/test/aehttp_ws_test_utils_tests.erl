%%% Regression tests for the CT WebSocket client harness.
%%%
%%% onconnect/2 used to queue `self() ! ping' the instant the handshake
%%% completed, which raced the peer closing a connection it had just rejected.
%%% websocket_client turns *any* failing send into a fatal close
%%% (handle_response/3 maps a non-`ok' encode_and_send/2 to {close, Reason}),
%%% so when the ping lost that race its gen_tcp:send returned {error, closed}
%%% and the client stopped with the rejection frame the server had already
%%% delivered still queued, undecoded, in its own mailbox. The waiting test
%%% then saw {connection_died, {error, closed}} instead of the rejection it
%%% asserts on -- the load-dependent flake in
%%% aehttp_sc_SUITE:sc_ws_broken_open_params.
%%%
%%% The invariant pinned here: onconnect/2 informs the waiting process that the
%%% socket is up and queues nothing for the connection to send. A WS-level ping
%%% is still available to any test that wants one, by sending `ping' to the
%%% connection process.
-module(aehttp_ws_test_utils_tests).

-include_lib("eunit/include/eunit.hrl").

onconnect_test_() ->
    [ {"the waiting process is told the socket is up",
       fun onconnect_informs_waiting_process/0}
    , {"nothing is queued for the connection to send",
       fun onconnect_queues_no_send/0}
    , {"a WS-level ping is still available on demand",
       fun ping_on_demand_still_replies/0}
    ].

onconnect_informs_waiting_process() ->
    ok = drain(),
    _State = onconnect(),
    Conn = self(),
    receive
        {Conn, websocket_event, websocket, connected} -> ok
    after 0 ->
        erlang:error({no_connected_event, mailbox()})
    end.

onconnect_queues_no_send() ->
    ok = drain(),
    _State = onconnect(),
    Conn = self(),
    Connected = {Conn, websocket_event, websocket, connected},
    %% Anything else left in the connection's own mailbox here is work it will
    %% try to do -- and a `ping' in particular is a send that can kill the
    %% connection before an already-received frame has been decoded.
    ?assertEqual([], [Msg || Msg <- mailbox(), Msg =/= Connected]).

ping_on_demand_still_replies() ->
    ok = drain(),
    State = onconnect(),
    ?assertMatch({reply, {ping, _Nonce}, _},
                 aehttp_ws_test_utils:websocket_info(ping, undefined, State)).

%% The harness callbacks run *in* the connection process, so here the test
%% process stands in for it: it is both the connection and the waiting process,
%% and anything onconnect/2 queues lands in this mailbox.
onconnect() ->
    {once, State0} = aehttp_ws_test_utils:init({self(), []}),
    {ok, State} = aehttp_ws_test_utils:onconnect(undefined, State0),
    State.

mailbox() ->
    {messages, Msgs} = process_info(self(), messages),
    Msgs.

drain() ->
    receive _ -> drain() after 0 -> ok end.
