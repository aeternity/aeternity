%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_peers module
%%% @end
%%%=============================================================================
-module(aec_peers_tests).

-ifdef(TEST).

%=== INCLUDES ==================================================================

-include_lib("eunit/include/eunit.hrl").

%=== MACROS ====================================================================

%% This should be in sync with aec_peers so the proper value is restoraed when
%% overriding the application environment.
-define(DEFAULT_MAX_OUTBOUND,                 10).
-define(DEFAULT_PING_INTERVAL,        120 * 1000).
-define(DEFAULT_RESOLVE_MAX_RETRY,             7).
-define(DEFAULT_RESOLVE_BACKOFF_TIMES,
        [5000, 15000, 30000, 60000, 120000, 300000, 600000]).

-define(fail(FORMAT, ARGS), fun() ->
    Message = lists:flatten(io_lib:format((FORMAT), (ARGS))),
    ?assertEqual(failed, Message)
end()).

-define(assertMessage(MSG, TIMEOUT), fun() ->
    receive
        (MSG) -> ok
    after (TIMEOUT) ->
        dump_messages(),
        ?fail("Message ~s not received", [(??MSG)])
    end
end()).

-define(assertInteractiveMessage(MSG, RESP, TIMEOUT), fun() ->
    receive
        {From, Ref, (MSG)} ->
            From ! {Ref, (RESP)},
            ok
    after (TIMEOUT) ->
        dump_messages(),
        ?fail("Message ~s not received", [(??MSG)])
    end
end()).

-define(assertNoInteractiveMessage(MSG, TIMEOUT), fun() ->
    receive
        {From, Ref, (MSG)} ->
            From ! {Ref, error},
            ?fail("~s called unexpectedly", [(??MSG)])
    after (TIMEOUT) -> ok
    end
end()).

-define(assertCalled(LABEL, ARGS, RESULT, TIMEOUT), fun() ->
    receive
        {(LABEL), (ARGS), Result} ->
            case Result of
                (RESULT) -> Result;
                _ ->
                    dump_messages(),
                    ?fail("~s result ~p do not match expected ~s",
                          [(??LABEL), Result, (??RESULT)])
            end
    after (TIMEOUT) ->
        dump_messages(),
        ?fail("~s not called with arguments ~s", [(??LABEL), (??ARGS)])
    end
end()).

-define(assertNotCalled(LABEL, ARGS, TIMEOUT), fun() ->
    receive
        {(LABEL), (ARGS), _Result} ->
            ?fail("~s called unexpectedly : ~s", [(??LABEL), (??ARGS)])
    after (TIMEOUT) -> ok
    end
end()).

%=== TEST CASES ================================================================

aec_peers_test_() ->
    [{setup, fun global_setup/0, fun global_teardown/1, [
        {setup, fun setup/0, fun teardown/1, fun test_single_trusted_peer/0},
        {setup, fun setup/0, fun teardown/1, fun test_single_normal_peer/0},
        {setup, fun setup/0, fun teardown/1, fun test_multiple_trusted_peers/0},
        {setup, fun setup/0, fun teardown/1, fun test_multiple_normal_peers/0},
        {setup, fun() -> setup([
                    {sync_single_outbound_per_group, false, true},
                    {sync_max_outbound, 1, ?DEFAULT_MAX_OUTBOUND},
                    {ping_interval, 1000, ?DEFAULT_PING_INTERVAL}
                ]) end,
                fun teardown/1,
                fun test_tcp_probe/0},
        {setup, fun() -> setup([
                    {sync_single_outbound_per_group, false, true},
                    {sync_resolver_max_retries, 3, ?DEFAULT_RESOLVE_MAX_RETRY},
                    {sync_resolver_backoff_times, [100, 200, 300], ?DEFAULT_RESOLVE_BACKOFF_TIMES},
                    {peer_pool, [{standby_times, [100]}], []}
                ]) end,
                fun teardown/1,
                fun test_invalid_hostname/0},
        {setup, fun() -> setup([{sync_single_outbound_per_group, false, true}]) end,
                fun teardown/1,
                fun test_multiple_peers_same_hostname/0},
        {setup, fun setup/0, fun teardown/1, fun test_address_group_selection/0},
        {setup, fun() -> setup([
                    {sync_single_outbound_per_group, false, true}
                ]) end,
                fun teardown/1,
                fun test_selection_without_address_group/0},
        {setup, fun setup/0, fun teardown/1, fun test_connection_closed/0},
        {setup, fun() -> setup([
                    {peer_pool, [
                        {standby_times, [100, 200, 300, 400]},
                        {max_rejections, 3}
                ], []}]) end,
                fun teardown/1,
                fun test_connection_failure/0},
        {setup, fun() -> setup([
                    {peer_pool, [
                        {standby_times, [100, 200, 300, 400]},
                        {max_rejections, 3}
                ], []}]) end,
                fun teardown/1,
                {timeout, 8, fun test_connection_down/0}},
        {setup, fun() -> setup([
                    {sync_max_inbound, 10, 100}
                ]) end,
                fun teardown/1,
                fun test_inbound_connections/0},
        {setup, fun() -> setup([
                    {sync_max_outbound, 3, 10}
                ]) end,
                fun teardown/1,
                fun test_outbound_connections/0},
        {setup, fun() -> setup([
                    {ping_interval, 500, ?DEFAULT_PING_INTERVAL}
                ]) end,
                fun teardown/1,
                fun test_ping/0},
        {setup, fun setup/0, fun teardown/1, fun test_connection_conflict/0},
        {setup, fun() -> setup([
                    {peer_unblock_interval, 2000, 900000}
                ]) end,
                fun teardown/1,
                {timeout, 12, fun test_blocking/0}},
        {setup, fun() -> setup([
                    {sync_single_outbound_per_group, false, true},
                    {ping_interval, 1000, ?DEFAULT_PING_INTERVAL}
                ]) end,
                fun teardown/1,
                fun test_duplicating_peer/0}
    ]}
].

test_single_trusted_peer() ->
    test_mgr_set_recipient(self()),

    PubKey = <<"ef42d46eace742cd0000000000000000">>,
    Id = peer_id(PubKey),

    ?assertMatch(false, aec_peers:is_blocked(Id)),
    ?assertMatch([], aec_peers:blocked_peers()),
    ?assertMatch([], aec_peers:connected_peers()),
    ?assertMatch([], aec_peers:available_peers()),
    ?assertMatch([], aec_peers:available_peers(both)),
    ?assertMatch([], aec_peers:available_peers(verified)),
    ?assertMatch([], aec_peers:available_peers(unverified)),
    ?assertEqual(0, aec_peers:count(available)),
    ?assertMatch([], aec_peers:get_random(all)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id)),

    ?assertEqual(0, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(0, aec_peers:count(outbound)),
    ?assertEqual(0, aec_peers:count(peers)),
    ?assertEqual(0, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    Peer = peer(PubKey, "10.1.0.1", 4000),
    aec_peers:add_trusted(Peer),
    {ok, Conn} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey }], {ok, _}, 200),

    ?assertMatch([], aec_peers:connected_peers()),
    ?assertMatch([#{ pubkey := PubKey }], aec_peers:get_random(all)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id)),

    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(1, aec_peers:count(outbound)),
    ?assertEqual(1, aec_peers:count(peers)),
    ?assertEqual(1, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    ok = conn_peer_connected(Conn),

    ?assertMatch([#{ pubkey := PubKey }], aec_peers:connected_peers()),
    ?assertMatch([#{ pubkey := PubKey }], aec_peers:get_random(all)),
    ?assertMatch({ok, Conn}, aec_peers:get_connection(Id)),

    ?assertMatch([], aec_peers:available_peers()),
    ?assertMatch([], aec_peers:available_peers(both)),
    ?assertMatch([], aec_peers:available_peers(verified)),
    ?assertMatch([], aec_peers:available_peers(unverified)),
    ?assertEqual(0, aec_peers:count(available)),
    ok.


test_single_normal_peer() ->
    test_mgr_set_recipient(self()),

    Source = {192, 168, 0, 1},
    PubKey = <<"ef42d46eace742cd0000000000000000">>,
    Id = peer_id(PubKey),
    Peer = peer(PubKey, <<"10.1.0.1">>, 4000),

    aec_peers:add_peers(Source, Peer),

    {ok, Conn} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey }], {ok, _}, 200),

    ?assertMatch([], aec_peers:connected_peers()),
    ?assertMatch([], aec_peers:get_random(all)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id)),

    ?assertEqual(0, aec_peers:count(verified)),
    ?assertEqual(1, aec_peers:count(unverified)),

    ok = conn_peer_connected(Conn),

    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(1, aec_peers:count(outbound)),
    ?assertEqual(1, aec_peers:count(peers)),
    ?assertEqual(1, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    ?assertMatch([#{ pubkey := PubKey }], aec_peers:connected_peers()),
    ?assertMatch([#{ pubkey := PubKey }], aec_peers:get_random(all)),
    ?assertMatch({ok, Conn}, aec_peers:get_connection(Id)),

    ?assertMatch([], aec_peers:available_peers()),
    ?assertMatch([], aec_peers:available_peers(both)),
    ?assertMatch([], aec_peers:available_peers(verified)),
    ?assertMatch([], aec_peers:available_peers(unverified)),
    ?assertEqual(0, aec_peers:count(available)),
    ok.

test_duplicating_peer() ->
    test_mgr_set_recipient(self()),

    TrustedPubKey = <<"ef42d46eace742cd0000000000000000">>,
    TrustedPeerAddress = "10.1.0.1",
    TrustedPeerPort = 4000,
    TrustedPeer = peer(TrustedPubKey, TrustedPeerAddress, TrustedPeerPort),
    TrustedId = peer_id(TrustedPubKey),

    ?assertMatch(false, aec_peers:is_blocked(TrustedId)),
    ?assertMatch([], aec_peers:blocked_peers()),
    ?assertMatch([], aec_peers:connected_peers()),
    ?assertMatch([], aec_peers:available_peers()),
    ?assertMatch([], aec_peers:available_peers(both)),
    ?assertMatch([], aec_peers:available_peers(verified)),
    ?assertMatch([], aec_peers:available_peers(unverified)),
    ?assertEqual(0, aec_peers:count(available)),
    ?assertMatch([], aec_peers:get_random(all)),
    ?assertMatch({error, _}, aec_peers:get_connection(TrustedId)),

    ?assertEqual(0, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(0, aec_peers:count(outbound)),
    ?assertEqual(0, aec_peers:count(peers)),
    ?assertEqual(0, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    aec_peers:add_trusted(TrustedPeer),
    {ok, Conn} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := TrustedPubKey }], {ok, _}, 200),

    ?assertMatch([], aec_peers:connected_peers()),
    ?assertMatch([#{ pubkey := TrustedPubKey }], aec_peers:get_random(all)),
    ?assertMatch({error, _}, aec_peers:get_connection(TrustedId)),

    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(1, aec_peers:count(outbound)),
    ?assertEqual(1, aec_peers:count(peers)),
    ?assertEqual(1, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    Source = {192, 168, 0, 1},
    PubKey1 = <<"df42d46eace742cd0000000000000000">>,
    ?assertEqual(true, TrustedPubKey =/= PubKey1),
    %% create a different peer with the same address and port
    Peer1 = peer(PubKey1, TrustedPeerAddress, TrustedPeerPort),
    aec_peers:add_peers(Source, Peer1),

    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(1, aec_peers:count(outbound)),
    ?assertEqual(1, aec_peers:count(peers)),
    ?assertEqual(1, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    ok = conn_peer_connected(Conn),

    ?assertMatch([#{ pubkey := TrustedPubKey }], aec_peers:connected_peers()),
    ?assertMatch([#{ pubkey := TrustedPubKey }], aec_peers:get_random(all)),
    ?assertMatch({ok, Conn}, aec_peers:get_connection(TrustedId)),

    aec_peers:add_peers(Source, Peer1),

    ?assertMatch([#{ pubkey := TrustedPubKey }], aec_peers:connected_peers()),
    ?assertMatch([#{ pubkey := TrustedPubKey }], aec_peers:get_random(all)),
    ?assertMatch({ok, Conn}, aec_peers:get_connection(TrustedId)),


    ?assertMatch([], aec_peers:available_peers()),
    ?assertMatch([], aec_peers:available_peers(both)),
    ?assertMatch([], aec_peers:available_peers(verified)),
    ?assertMatch([], aec_peers:available_peers(unverified)),
    ?assertEqual(0, aec_peers:count(available)),

    TrustedPeerPort2 = 4001,
    PubKey2 = <<"af42d46eace742cd0000000000000000">>,
    %% create a different peer with the same address and a different port
    ?assertEqual(true, TrustedPeerPort =/= TrustedPeerPort2),
    Peer2 = peer(PubKey2, TrustedPeerAddress, TrustedPeerPort2),
    PeerId2 = peer_id(PubKey2),
    aec_peers:add_peers(Source, Peer2),

    {ok, Conn1} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey2 }], {ok, _}, 3500),

    ?assertMatch([#{ pubkey := TrustedPubKey }], aec_peers:connected_peers()),
    ?assertMatch({error, _}, aec_peers:get_connection(PeerId2)),

    ?assertEqual(2, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(2, aec_peers:count(outbound)),
    ?assertEqual(2, aec_peers:count(peers)),
    ?assertEqual(1, aec_peers:count(verified)),
    ?assertEqual(1, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),


    ok.

test_multiple_trusted_peers() ->
    test_mgr_set_recipient(self()),

    PubKey1 = <<"ef42d46eace742cd0000000000000000">>,
    Id1 = peer_id(PubKey1),
    PubKey2 = <<"854a8e1f93f94b950000000000000000">>,
    Id2 = peer_id(PubKey2),
    PubKey3 = <<"eb56e9292dda481c0000000000000000">>,
    Id3 = peer_id(PubKey3),

    Peers = [
        peer(PubKey1, <<"10.1.0.1">>, 4000),
        peer(PubKey2, "10.2.0.1", 4000),
        peer(PubKey3, "10.3.0.1", 4000)
    ],

    aec_peers:add_trusted(Peers),
    {ok, Conn1} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey1 }], {ok, _}, 500),
    {ok, Conn2} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey2 }], {ok, _}, 200),
    {ok, Conn3} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey3 }], {ok, _}, 200),

    ?assertMatch([], aec_peers:connected_peers()),
    ?assertEqual(lists:sort(Peers), lists:sort(aec_peers:get_random(all))),
    ?assertMatch({error, _}, aec_peers:get_connection(Id1)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id2)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id3)),

    ok = conn_peer_connected(Conn1),

    ?assertMatch([#{ pubkey := PubKey1 }], aec_peers:connected_peers()),
    ?assertEqual(lists:sort(Peers), lists:sort(aec_peers:get_random(all))),
    ?assertMatch({ok, Conn1}, aec_peers:get_connection(Id1)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id2)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id3)),

    ok = conn_peer_connected(Conn2),
    ok = conn_peer_connected(Conn3),

    ?assertEqual(lists:sort(Peers), lists:sort(aec_peers:connected_peers())),
    ?assertEqual(lists:sort(Peers), lists:sort(aec_peers:get_random(all))),
    ?assertMatch({ok, Conn1}, aec_peers:get_connection(Id1)),
    ?assertMatch({ok, Conn2}, aec_peers:get_connection(Id2)),
    ?assertMatch({ok, Conn3}, aec_peers:get_connection(Id3)),

    ?assertEqual(3, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(3, aec_peers:count(outbound)),
    ?assertEqual(3, aec_peers:count(peers)),
    ?assertEqual(3, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),
    ok.

test_multiple_normal_peers() ->
    test_mgr_set_recipient(self()),

    Source = {192, 168, 0, 1},
    PubKey1 = <<"ef42d46eace742cd0000000000000000">>,
    Id1 = peer_id(PubKey1),
    PubKey2 = <<"854a8e1f93f94b950000000000000000">>,
    Id2 = peer_id(PubKey2),
    PubKey3 = <<"eb56e9292dda481c0000000000000000">>,
    Id3 = peer_id(PubKey3),

    Peers = [
        peer(PubKey1, <<"10.1.0.1">>, 4000),
        peer(PubKey2, "10.2.0.1", 4000),
        peer(PubKey3, "10.3.0.1", 4000)
    ],

    aec_peers:add_peers(Source, Peers),
    %% First one is connected to right away, the second after 1 second
    %% and the third one after 2 seconds; but there is no way of knowing the
    %% order they will get connect to.
    A = erlang:system_time(millisecond),
    {ok, Conn1} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey1 }], {ok, _}, 3500),
    {ok, Conn2} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey2 }], {ok, _}, 3500),
    {ok, Conn3} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey3 }], {ok, _}, 3500),
    B = erlang:system_time(millisecond),
    Delta = B - A,
    ?assertEqual({Delta, true}, {Delta, (Delta =< 3500)}),

    ok = conn_peer_connected(Conn1),
    ok = conn_peer_connected(Conn2),
    ok = conn_peer_connected(Conn3),

    ?assertEqual(lists:sort(Peers), lists:sort(aec_peers:connected_peers())),
    ?assertEqual(lists:sort(Peers), lists:sort(aec_peers:get_random(all))),
    ?assertMatch({ok, Conn1}, aec_peers:get_connection(Id1)),
    ?assertMatch({ok, Conn2}, aec_peers:get_connection(Id2)),
    ?assertMatch({ok, Conn3}, aec_peers:get_connection(Id3)),

    ?assertEqual(3, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(3, aec_peers:count(outbound)),
    ?assertEqual(3, aec_peers:count(peers)),
    ?assertEqual(3, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),
    ok.

test_tcp_probe() ->
    test_mgr_set_recipient(self()),

    Source = {192, 168, 0, 1},
    PubKey1 = <<"ef42d46eace742cd0000000000000000">>,
    Id1 = peer_id(PubKey1),
    PubKey2 = <<"854a8e1f93f94b950000000000000000">>,
    _Id2 = peer_id(PubKey2),
    PubKey3 = <<"eb56e9292dda481c0000000000000000">>,
    _Id3 = peer_id(PubKey3),

    Peer1 = peer(PubKey1, <<"10.1.0.1">>, 4000),
    Peer2 = peer(PubKey2, <<"10.2.0.1">>, 4000),
    Peer3 = peer(PubKey3, <<"10.3.0.1">>, 4000),

    aec_peers:add_peers(Source, Peer1),
    {ok, Conn1} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey1 }], {ok, _}, 200),
    ok = conn_peer_connected(Conn1),

    aec_peers:add_peers(Source, [Peer2, Peer3]),

    ?assertCalled(schedule_ping, [Id1], ok, 100),
    ok = conn_log_ping(Conn1, ok),

    ?assertCalled(schedule_ping, [Id1], ok, 1100),
    ok = conn_log_ping(Conn1, ok),

    %% After 1.5 * ping_interval there is the first TCP probe. The selected peer
    %% can be either Peer2 or Peer3 as it's random.
    {ok, Conn2} = ?assertCalled(connect, [#{ conn_type := tcp}], {ok, _}, 600),
    ok = conn_peer_dead(Conn2),

    ?assertCalled(schedule_ping, [Id1], ok, 800),
    ok = conn_log_ping(Conn1, ok),


    {ok, Conn3} = ?assertCalled(connect, [#{ conn_type := tcp }], {ok, _}, 600),
    ok = conn_peer_alive(Conn3),

    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(1, aec_peers:count(outbound)),
    ?assertEqual(3, aec_peers:count(peers)),
    ?assertEqual(2, aec_peers:count(verified)),
    ?assertEqual(1, aec_peers:count(unverified)),
    ?assertEqual(1, aec_peers:count(standby)),

    ?assertMatch({ok, Conn1}, aec_peers:get_connection(Id1)),

    AvailPeers = aec_peers:available_peers(),
    ?assertEqual(length(AvailPeers), 1),
    [AvailPeer] = AvailPeers,

    ?assertEqual([Peer1], aec_peers:connected_peers()),
    ?assertEqual(lists:sort([Peer1, AvailPeer]),
                 lists:sort(aec_peers:get_random(all))),
    ?assertMatch([AvailPeer], aec_peers:available_peers(verified)),
    ?assertMatch([], aec_peers:available_peers(unverified)),
    ?assertEqual(1, aec_peers:count(available)),
    ok.

test_invalid_hostname() ->
    test_mgr_set_recipient(self()),
    setup_mock_getaddr(),

    mock_getaddr({error, nxdomain}),

    ?assertMatch(0, aec_peers:count(hostnames)),

    Source = {192, 168, 0, 1},
    PubKey1 = <<"ef42d46eace742cd0000000000000000">>,
    Peer1 = peer(PubKey1, <<"aeternity.com">>, 4000),
    Id1 = peer_id(Peer1),

    PubKey2 = <<"ff92adcbcc684dda0000000000000000">>,
    Peer2 = peer(PubKey2, <<"aeternity.com">>, 5000),

    aec_peers:add_peers(Source, Peer1),
    ?assertMessage({getaddr, "aeternity.com"}, 50),
    ?assertMatch([], lists:sort(aec_peers:connected_peers())),
    ?assertMatch([], lists:sort(aec_peers:get_random(all))),
    ?assertMatch({error, _}, aec_peers:get_connection(Id1)),

    ?assertMatch(1, aec_peers:count(hostnames)),
    ?assertEqual(0, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(0, aec_peers:count(outbound)),
    ?assertEqual(0, aec_peers:count(peers)),
    ?assertEqual(0, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    % First retry
    ?assertMessage({getaddr, "aeternity.com"}, 400),
    ?assertMatch(1, aec_peers:count(hostnames)),

    aec_peers:add_trusted(Peer2),
    ?assertMessage({getaddr, "aeternity.com"}, 100),

    % Second retry
    ?assertMessage({getaddr, "aeternity.com"}, 400),
    ?assertMatch(1, aec_peers:count(hostnames)),

    % Third retry
    ?assertMessage({getaddr, "aeternity.com"}, 500),
    ?assertMatch(1, aec_peers:count(hostnames)),

    % Trusted peers keep retrying forever, but normal peers are removed.
    ?assertMessage({getaddr, "aeternity.com"}, 500),
    ?assertMatch(1, aec_peers:count(hostnames)),

    ?assertMessage({getaddr, "aeternity.com"}, 500),
    ?assertMatch(1, aec_peers:count(hostnames)),

    % Make the hostname resolvable again
    mock_getaddr({ok, {192, 168, 0, 10}}),

    % Only the trusted peer is added, the other one was removed.
    ?assertMessage({getaddr, "aeternity.com"}, 500),
    {ok, Conn1} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey2 }], {ok, _}, 200),

    ?assertMatch(0, aec_peers:count(hostnames)),
    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(1, aec_peers:count(outbound)),
    ?assertEqual(1, aec_peers:count(peers)),
    ?assertEqual(1, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    mock_getaddr({error, nxdomain}),

    % Delete peer2 and add back both.
    aec_peers:del_peer(peer_id(Peer2)),
    ?assertCalled(disconnect, [Conn1], ok, 100),
    aec_peers:add_peers(Source, Peer1),
    ?assertMessage({getaddr, "aeternity.com"}, 100),
    aec_peers:add_trusted(Peer2),
    ?assertMessage({getaddr, "aeternity.com"}, 100),

    ?assertMatch(1, aec_peers:count(hostnames)),
    ?assertEqual(0, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(0, aec_peers:count(outbound)),
    ?assertEqual(0, aec_peers:count(peers)),
    ?assertEqual(0, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    % First retry
    ?assertMessage({getaddr, "aeternity.com"}, 200),

    mock_getaddr({ok, {192, 168, 0, 10}}),

    % Both peers hould get added and get connected to.
    ?assertMessage({getaddr, "aeternity.com"}, 350),
    ?assertCalled(connect, [#{ r_pubkey := PubKey2 }], {ok, _}, 200),
    ?assertCalled(connect, [#{ r_pubkey := PubKey1 }], {ok, _}, 1200),

    ok.

%% Test that adding multiple peers with the same hostname works.
test_multiple_peers_same_hostname() ->
    test_mgr_set_recipient(self()),
    setup_mock_getaddr(),

    Source = {192, 168, 0, 1},
    PubKey1 = <<"ef42d46eace742cd0000000000000000">>,
    Peer1 = peer(PubKey1, <<"aeternity.com">>, 4000),
    PubKey2 = <<"52af7b3a5d2c460b0000000000000000">>,
    Peer2 = peer(PubKey2, <<"aeternity.com">>, 4001),
    PubKey3 = <<"d59e8323509a49da0000000000000000">>,
    Peer3 = peer(PubKey3, <<"aeternity.com">>, 4002),

    mock_getaddr({ok, {192, 168, 0, 10}}),

    aec_peers:add_peers(Source, Peer1),
    aec_peers:add_peers(Source, Peer2),
    aec_peers:add_peers(Source, Peer3),

    ?assertMessage({getaddr, "aeternity.com"}, 50),
    ?assertMessage({getaddr, "aeternity.com"}, 50),
    ?assertMessage({getaddr, "aeternity.com"}, 50),

    {ok, Conn1} = ?assertCalled(connect, _, {ok, _}, 300),
    ok = conn_peer_connected(Conn1),
    {ok, Conn2} = ?assertCalled(connect, _, {ok, _}, 1200),
    ok = conn_peer_connected(Conn2),
    {ok, Conn3} = ?assertCalled(connect, _, {ok, _}, 2200),
    ok = conn_peer_connected(Conn3),

    ?assertMatch(0, aec_peers:count(hostnames)),
    ?assertEqual(3, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(3, aec_peers:count(outbound)),
    ?assertEqual(3, aec_peers:count(peers)),
    ?assertEqual(3, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),
    ok.


test_address_group_selection() ->
    test_mgr_set_recipient(self()),

    Source = {192, 168, 0, 1},
    PubKey1 = <<"ef42d46eace742cd0000000000000000">>,
    Id1 = peer_id(PubKey1),
    Peer1 = peer(PubKey1, "10.1.0.1", 4000),
    PubKey2 = <<"854a8e1f93f94b950000000000000000">>,
    Id2 = peer_id(PubKey2),
    Peer2 = peer(PubKey2, "10.1.0.2", 4000),
    PubKey3 = <<"eb56e9292dda481c0000000000000000">>,
    Id3 = peer_id(PubKey3),
    Peer3 = peer(PubKey3, "10.2.0.1", 4000),
    PubKey4 = <<"9d9e1c43de304d810000000000000000">>,
    Id4 = peer_id(PubKey4),
    Peer4 = peer(PubKey4, "10.2.0.2", 4000),
    PubKey5 = <<"75640b5ffaac40480000000000000000">>,
    Id5 = peer_id(PubKey5),
    Peer5 = peer(PubKey5, "10.3.0.1", 4000),

    aec_peers:add_peers(Source, Peer1),
    {ok, Conn1} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey1 }], {ok, _}, 200),
    ok = conn_peer_connected(Conn1),

    aec_peers:add_peers(Source, Peer2),
    ?assertNotCalled(connect, _, 1100),

    ?assertEqual([Peer1], aec_peers:connected_peers()),
    ?assertEqual([Peer1], aec_peers:get_random(all)),
    ?assertMatch({ok, Conn1}, aec_peers:get_connection(Id1)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id2)),

    aec_peers:add_peers(Source, Peer3),
    {ok, Conn3} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey3 }], {ok, _}, 1200),
    ok = conn_peer_connected(Conn3),

    ?assertEqual(lists:sort([Peer1, Peer3]),
                 lists:sort(aec_peers:connected_peers())),
    ?assertEqual(lists:sort([Peer1, Peer3]),
                 lists:sort(aec_peers:get_random(all))),
    ?assertMatch({ok, Conn1}, aec_peers:get_connection(Id1)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id2)),
    ?assertMatch({ok, Conn3}, aec_peers:get_connection(Id3)),

    aec_peers:add_peers(Source, Peer4),
    ?assertNotCalled(connect, _, 2100),

    ?assertEqual(lists:sort([Peer1, Peer3]),
                 lists:sort(aec_peers:connected_peers())),
    ?assertEqual(lists:sort([Peer1, Peer3]),
                 lists:sort(aec_peers:get_random(all))),
    ?assertMatch({ok, Conn1}, aec_peers:get_connection(Id1)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id2)),
    ?assertMatch({ok, Conn3}, aec_peers:get_connection(Id3)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id4)),

    aec_peers:add_peers(Source, Peer5),
    {ok, Conn5} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey5 }], {ok, _}, 2200),
    ok = conn_peer_connected(Conn5),

    ?assertEqual(lists:sort([Peer1, Peer3, Peer5]),
                 lists:sort(aec_peers:connected_peers())),
    ?assertEqual(lists:sort([Peer1, Peer3, Peer5]),
                 lists:sort(aec_peers:get_random(all))),
    ?assertMatch({ok, Conn1}, aec_peers:get_connection(Id1)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id2)),
    ?assertMatch({ok, Conn3}, aec_peers:get_connection(Id3)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id4)),
    ?assertMatch({ok, Conn5}, aec_peers:get_connection(Id5)),

    ?assertEqual(3, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(3, aec_peers:count(outbound)),
    ?assertEqual(5, aec_peers:count(peers)),
    ?assertEqual(3, aec_peers:count(verified)),
    ?assertEqual(2, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),
    ok.

test_selection_without_address_group() ->
    test_mgr_set_recipient(self()),

    Source = {192, 168, 0, 1},
    PubKey1 = <<"ef42d46eace742cd0000000000000000">>,
    Id1 = peer_id(PubKey1),
    Peer1 = peer(PubKey1, "10.1.0.1", 4000),
    PubKey2 = <<"854a8e1f93f94b950000000000000000">>,
    Id2 = peer_id(PubKey2),
    Peer2 = peer(PubKey2, "10.1.0.2", 4000),
    PubKey3 = <<"eb56e9292dda481c0000000000000000">>,
    Id3 = peer_id(PubKey3),
    Peer3 = peer(PubKey3, "10.1.0.3", 4000),

    aec_peers:add_peers(Source, Peer1),
    {ok, Conn1} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey1 }], {ok, _}, 200),
    ok = conn_peer_connected(Conn1),

    aec_peers:add_peers(Source, Peer2),
    {ok, Conn2} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey2 }], {ok, _}, 1200),
    ok = conn_peer_connected(Conn2),

    aec_peers:add_peers(Source, Peer3),
    {ok, Conn3} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey3 }], {ok, _}, 2200),
    ok = conn_peer_connected(Conn3),

    ?assertEqual(lists:sort([Peer1, Peer2, Peer3]),
                 lists:sort(aec_peers:connected_peers())),
    ?assertEqual(lists:sort([Peer1, Peer2, Peer3]),
                 lists:sort(aec_peers:get_random(all))),
    ?assertMatch({ok, Conn1}, aec_peers:get_connection(Id1)),
    ?assertMatch({ok, Conn2}, aec_peers:get_connection(Id2)),
    ?assertMatch({ok, Conn3}, aec_peers:get_connection(Id3)),
    ok.

test_connection_closed() ->
    test_mgr_set_recipient(self()),

    Source = {192, 168, 0, 1},
    PubKey = <<"ef42d46eace742cd0000000000000000">>,
    Id = peer_id(PubKey),
    Peer = peer(PubKey, "10.1.0.1", 4000),

    aec_peers:add_peers(Source, Peer),
    {ok, Conn1} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey }], {ok, _}, 200),
    ok = conn_peer_connected(Conn1),

    ?assertMatch([Peer], aec_peers:connected_peers()),
    ?assertMatch([Peer], aec_peers:get_random(all)),
    ?assertMatch({ok, Conn1}, aec_peers:get_connection(Id)),

    ok = conn_connection_closed(Conn1),

    ?assertMatch([], aec_peers:connected_peers()),
    ?assertMatch([Peer], aec_peers:get_random(all)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id)),

    ?assertEqual(0, aec_peers:count(connections)),
    ?assertEqual(1, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    % Check it is reconnects
    {ok, Conn2} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey }], {ok, _}, 1200),
    ok = conn_peer_connected(Conn2),

    ?assertMatch([Peer], aec_peers:connected_peers()),
    ?assertMatch([Peer], aec_peers:get_random(all)),
    ?assertMatch({ok, Conn2}, aec_peers:get_connection(Id)),

    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(1, aec_peers:count(outbound)),
    ?assertEqual(1, aec_peers:count(peers)),
    ?assertEqual(1, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),
    ok.

test_connection_failure() ->
    test_mgr_set_recipient(self()),

    Source = {192, 168, 0, 1},
    PubKey = <<"ef42d46eace742cd0000000000000000">>,
    Id = peer_id(PubKey),
    Peer = peer(PubKey, "10.1.0.1", 4000),

    aec_peers:add_peers(Source, Peer),
    {ok, Conn1} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey }], {ok, _}, 200),
    ok = conn_peer_connected(Conn1),

    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(1, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    ok = conn_connection_closed(Conn1),

    ?assertEqual(0, aec_peers:count(connections)),
    ?assertEqual(1, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    {ok, Conn2} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey }], {ok, _}, 300),
    ok = conn_connection_failed(Conn2),

    ?assertMatch([], aec_peers:connected_peers()),
    ?assertMatch([Peer], aec_peers:get_random(all)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id)),

    ?assertEqual(0, aec_peers:count(connections)),
    ?assertEqual(1, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    % Check it is retried after 200 milliseconds
    {ok, Conn3} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey }], {ok, _}, 300),
    ok = conn_peer_connected(Conn3),

    ?assertMatch([Peer], aec_peers:connected_peers()),
    ?assertMatch([Peer], aec_peers:get_random(all)),
    ?assertMatch({ok, Conn3}, aec_peers:get_connection(Id)),

    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(1, aec_peers:count(outbound)),
    ?assertEqual(1, aec_peers:count(peers)),
    ?assertEqual(1, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),
    ok.

test_connection_down() ->
    test_mgr_set_recipient(self()),

    Source = {192, 168, 0, 1},
    PubKey = <<"ef42d46eace742cd0000000000000000">>,
    Id = peer_id(PubKey),
    Peer = peer(PubKey, "10.1.0.1", 4000),

    aec_peers:add_peers(Source, Peer),
    {ok, Conn1} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey }], {ok, _}, 200),
    conn_kill(Conn1),

    ?assertMatch([], aec_peers:connected_peers()),
    ?assertMatch([], aec_peers:get_random(all)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id)),

    ?assertEqual(0, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(verified)),
    ?assertEqual(1, aec_peers:count(unverified)),
    ?assertEqual(1, aec_peers:count(standby)),

    % Peer is downgraded to the unverified pool, and retried.
    {ok, Conn2} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey }], {ok, _}, 200),

    ?assertMatch([], aec_peers:connected_peers()),
    ?assertMatch([], aec_peers:get_random(all)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id)),

    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(verified)),
    ?assertEqual(1, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    % Killing the connection before calling peer_connected
    % so the peer stay unverified.

    conn_kill(Conn2),

    ?assertMatch([], aec_peers:connected_peers()),
    ?assertMatch([], aec_peers:get_random(all)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id)),

    ?assertEqual(0, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(verified)),
    ?assertEqual(1, aec_peers:count(unverified)),
    ?assertEqual(1, aec_peers:count(standby)),


    {ok, Conn3} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey }], {ok, _}, 400),
    conn_kill(Conn3),
    ?assertMatch([], aec_peers:connected_peers()),
    ?assertMatch([], aec_peers:get_random(all)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id)),

    ?assertEqual(0, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(verified)),
    ?assertEqual(1, aec_peers:count(unverified)),
    ?assertEqual(1, aec_peers:count(standby)),


    % Last retry of unverified peer, should be removed.
    % This depends on max_rejections = 3 in the setup
    {ok, Conn4} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey }], {ok, _}, 500),
    conn_kill(Conn4),

    ?assertMatch([], aec_peers:connected_peers()),
    ?assertMatch([], aec_peers:get_random(all)),
    ?assertMatch({error, _}, aec_peers:get_connection(Id)),

    ?assertEqual(0, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),
    ok.

test_inbound_connections() ->
    test_mgr_set_recipient(self()),

    Peers = [ peer(I, "10.1.0.1", I) || I <- lists:seq(1, 10) ],

    lists:foldl(fun(Peer, Acc) ->
        {ok, Conn} = test_mgr_start_inbound(Peer),
        ?assertEqual(permanent, conn_peer_accepted(Conn, maps:get(host, Peer))),
        PeerId = peer_id(Peer),
        Acc#{ PeerId => Conn }
    end, #{}, Peers),

    ?assertEqual(lists:sort(Peers), lists:sort(aec_peers:connected_peers())),
    ?assertEqual(lists:sort(Peers), lists:sort(aec_peers:get_random(all))),

    ?assertEqual(10, aec_peers:count(connections)),
    ?assertEqual(10, aec_peers:count(inbound)),
    ?assertEqual(0, aec_peers:count(outbound)),
    ?assertEqual(10, aec_peers:count(peers)),
    ?assertEqual(10, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),

    % Check we are not trying to connect to any peers already connected.
    ?assertNotCalled(connect, _, 1000),

    % Check the inbound connection limit.
    Peer11 = peer(11, "10.1.0.1", 11),
    {ok, Conn11a} = test_mgr_start_inbound(Peer11),
    ?assertEqual(temporary, conn_peer_accepted(Conn11a, maps:get(host, Peer11))),
    ?assertEqual(11, aec_peers:count(connections)),
    conn_connection_closed(Conn11a),

    ?assertEqual(lists:sort(Peers), lists:sort(aec_peers:connected_peers())),
    ?assertEqual(lists:sort([Peer11 | Peers]), lists:sort(aec_peers:get_random(all))),

    % The extra inbound that was closed is used for outbound.
    #{ pubkey := PubKey11} = Peer11,
    {ok, Conn11b} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey11 }], {ok, _}, 1100),
    conn_peer_connected(Conn11b),

    ?assertEqual(11, aec_peers:count(connections)),
    ?assertEqual(10, aec_peers:count(inbound)),
    ?assertEqual(1, aec_peers:count(outbound)),
    ?assertEqual(11, aec_peers:count(peers)),
    ?assertEqual(11, aec_peers:count(verified)),
    ?assertEqual(0, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),
    ok.

%% Tests tat trusted peers are connected to even though they are from the same
%% address group; tests the outbound connection limit; tests updating the same
%% peers multiple times.
test_outbound_connections() ->
    test_mgr_set_recipient(self()),

    Source = {192, 168, 0, 1},
    Peer1 = peer(1, "10.1.0.1", 1),
    #{ pubkey := PubKey1 } = Peer1,
    Peer2 = peer(2, "10.1.0.2", 2),
    #{ pubkey := PubKey2 } = Peer2,
    Peer3 = peer(3, "10.2.0.1", 3),
    #{ pubkey := PubKey3 } = Peer3,
    Peer4 = peer(4, "10.3.0.1", 4),
    Peer5 = peer(5, "10.4.0.1", 5),
    Peers = [Peer1, Peer2, Peer3],

    ok = aec_peers:add_trusted(Peers),
    {ok, Conn1} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey1 }], {ok, _}, 1100),
    ok = conn_peer_connected(Conn1),
    {ok, Conn2} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey2 }], {ok, _}, 1100),
    ok = conn_peer_connected(Conn2),
    {ok, Conn3} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey3 }], {ok, _}, 1100),
    ok = conn_peer_connected(Conn3),

    ok = aec_peers:add_trusted(Peers),
    ok = aec_peers:add_peers(Source, Peers),

    ok = aec_peers:add_trusted(Peer4),
    ok = aec_peers:add_peers(Source, Peer5),

    % Check we are not trying to connect to more peers.
    ?assertNotCalled(connect, _, 1000),

    ?assertEqual(lists:sort(Peers), lists:sort(aec_peers:connected_peers())),
    ?assertEqual(lists:sort([Peer4 | Peers]), lists:sort(aec_peers:get_random(all))),

    ?assertEqual(3, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(3, aec_peers:count(outbound)),
    ?assertEqual(5, aec_peers:count(peers)),
    ?assertEqual(4, aec_peers:count(verified)),
    ?assertEqual(1, aec_peers:count(unverified)),
    ?assertEqual(0, aec_peers:count(standby)),
    ok.

test_ping() ->
    test_mgr_set_recipient(self()),

    Source = {192, 168, 0, 1},
    PubKey = <<"ef42d46eace742cd0000000000000000">>,
    Id = peer_id(PubKey),
    Peer = peer(PubKey, "10.1.0.1", 4000),

    ok = aec_peers:add_peers(Source, Peer),
    {ok, Conn} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey }], {ok, _}, 200),
    ok = conn_peer_connected(Conn),

    % Check we are sheduling a ping right away.
    ?assertCalled(schedule_ping, [Id], ok, 200),
    ok = conn_log_ping(Conn, ok),

    % Check we continue pinging
    ?assertCalled(schedule_ping, [Id], ok, 600),
    ok = conn_log_ping(Conn, error),

    % Even when ping failed.
    ?assertCalled(schedule_ping, [Id], ok, 600),
    ok = conn_log_ping(Conn, ok),

    ok.

test_connection_conflict() ->
    test_mgr_set_recipient(self()),

    Source = {192, 168, 0, 1},
    Peer1 = peer(1, "10.1.0.1", 4000),
    #{ pubkey := PubKey1 } = Peer1,
    Id1 = peer_id(PubKey1),
    Peer2 = peer(9, "10.1.0.2", 4000),
    #{ pubkey := PubKey2 } = Peer2,
    Id2 = peer_id(PubKey2),

    ok = aec_peers:add_peers(Source, Peer1),
    {ok, Conn1} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey1 }], {ok, _}, 200),
    ok = conn_peer_connected(Conn1),

    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(1, aec_peers:count(outbound)),
    ?assertEqual(1, aec_peers:count(peers)),

    {ok, Conn2} = test_mgr_start_inbound(Peer1),
    ?assertEqual({error, already_connected}, conn_peer_accepted(Conn2, maps:get(host, Peer1))),

    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(1, aec_peers:count(outbound)),
    ?assertEqual(1, aec_peers:count(peers)),

    ok = aec_peers:del_peer(Id1),
    ?assertCalled(disconnect, [Conn1], ok, 100),

    ?assertEqual(0, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(0, aec_peers:count(outbound)),
    ?assertEqual(0, aec_peers:count(peers)),

    ok = aec_peers:add_peers(Source, Peer2),
    {ok, Conn3} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey2 }], {ok, _}, 1200),
    ok = conn_peer_connected(Conn3),

    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(0, aec_peers:count(inbound)),
    ?assertEqual(1, aec_peers:count(outbound)),
    ?assertEqual(1, aec_peers:count(peers)),

    {ok, Conn4} = test_mgr_start_inbound(Peer2),
    ?assertEqual(permanent, conn_peer_accepted(Conn4, maps:get(host, Peer2))),
    ?assertCalled(disconnect, [Conn3], ok, 100),

    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(1, aec_peers:count(inbound)),
    ?assertEqual(0, aec_peers:count(outbound)),
    ?assertEqual(1, aec_peers:count(peers)),

    ok = aec_peers:del_peer(Id2),
    ?assertCalled(disconnect, [Conn4], ok, 100),

    % Check that an outbound connection not yet connected is canceled.

    ok = aec_peers:add_peers(Source, Peer1),
    {ok, Conn5} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey1 }], {ok, _}, 1200),

    {ok, Conn6} = test_mgr_start_inbound(Peer1),
    ?assertEqual(permanent, conn_peer_accepted(Conn6, maps:get(host, Peer1))),
    ?assertCalled(disconnect, [Conn5], ok, 100),

    ?assertEqual(1, aec_peers:count(connections)),
    ?assertEqual(1, aec_peers:count(inbound)),
    ?assertEqual(0, aec_peers:count(outbound)),
    ?assertEqual(1, aec_peers:count(peers)),
    ok.

test_blocking() ->
    test_mgr_set_recipient(self()),

    Source = {192, 168, 0, 1},
    Peer1 = peer(1, "10.1.0.1", 1),
    Id1 = peer_id(Peer1),
    #{ pubkey := PubKey1 } = Peer1,
    Peer2 = peer(2, "10.1.0.2", 2),
    #{ pubkey := PubKey2 } = Peer2,
    Id2 = peer_id(Peer2),
    Peer3 = peer(3, "10.1.0.3", 3),
    #{ pubkey := PubKey3 } = Peer3,
    Id3 = peer_id(Peer3),
    Peer4 = peer(4, "10.1.0.4", 4),
    Id4 = peer_id(Peer4),

    A = erlang:system_time(millisecond),

    aec_peers:block_peer(Peer1),
    ?assertEqual(1, aec_peers:count(blocked)),
    ?assert(aec_peers:is_blocked(Id1)),
    ?assertEqual([Peer1], aec_peers:blocked_peers()),

    ok = aec_peers:add_peers(Source, Peer1),
    ?assertNotCalled(connect, _, 1100),
    ?assertEqual(0, aec_peers:count(peers)),
    ?assertEqual(0, aec_peers:count(connections)),

    {ok, Conn1} = test_mgr_start_inbound(Peer1),
    ?assertEqual({error, blocked}, conn_peer_accepted(Conn1, maps:get(host, Peer1))),
    ?assertEqual(0, aec_peers:count(peers)),
    ?assertEqual(0, aec_peers:count(connections)),

    ok = aec_peers:add_peers(Source, Peer2),
    {ok, Conn2} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey2 }], {ok, _}, 1200),
    ?assertEqual(1, aec_peers:count(peers)),
    ?assertEqual(1, aec_peers:count(connections)),

    aec_peers:block_peer(Peer2),
    ?assertCalled(disconnect, [Conn2], ok, 100),
    ?assertEqual(2, aec_peers:count(blocked)),
    ?assert(aec_peers:is_blocked(Id2)),
    ?assertEqual(lists:sort([Peer1, Peer2]),
                 lists:sort(aec_peers:blocked_peers())),
    ?assertEqual(0, aec_peers:count(peers)),
    ?assertEqual(0, aec_peers:count(connections)),

    ok = aec_peers:add_peers(Source, Peer3),
    {ok, Conn3} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey3 }], {ok, _}, 1200),
    ok = conn_peer_connected(Conn3),
    ?assertEqual(1, aec_peers:count(peers)),
    ?assertEqual(1, aec_peers:count(connections)),

    aec_peers:block_peer(Peer3),
    ?assertCalled(disconnect, [Conn3], ok, 100),
    ?assertEqual(3, aec_peers:count(blocked)),
    ?assert(aec_peers:is_blocked(Id3)),
    ?assertEqual(lists:sort([Peer1, Peer2, Peer3]),
                 lists:sort(aec_peers:blocked_peers())),
    ?assertEqual(0, aec_peers:count(peers)),
    ?assertEqual(0, aec_peers:count(connections)),

    {ok, Conn4} = test_mgr_start_inbound(Peer4),
    ?assertEqual(permanent, conn_peer_accepted(Conn4, maps:get(host, Peer4))),
    ?assertEqual(1, aec_peers:count(peers)),
    ?assertEqual(1, aec_peers:count(connections)),

    aec_peers:block_peer(Peer4),
    ?assertCalled(disconnect, [Conn4], ok, 100),
    ?assertEqual(4, aec_peers:count(blocked)),
    ?assert(aec_peers:is_blocked(Id4)),
    ?assertEqual(lists:sort([Peer1, Peer2, Peer3, Peer4]),
                 lists:sort(aec_peers:blocked_peers())),
    ?assertEqual(0, aec_peers:count(peers)),
    ?assertEqual(0, aec_peers:count(connections)),

    aec_peers:unblock_peer(Id2),
    ?assertEqual(3, aec_peers:count(blocked)),
    ?assertEqual(lists:sort([Peer1, Peer3, Peer4]),
                 lists:sort(aec_peers:blocked_peers())),

    ok = aec_peers:add_peers(Source, Peer2),
    {ok, Conn5} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey2 }], {ok, _}, 1200),
    ok = conn_peer_connected(Conn5),
    ?assertEqual(1, aec_peers:count(peers)),
    ?assertEqual(1, aec_peers:count(connections)),

    ?assert(aec_peers:is_blocked(Id1)),
    ?assertEqual(lists:sort([Peer1, Peer3, Peer4]),
                 lists:sort(aec_peers:blocked_peers())),

    % Test peers are unblocked when calling `is_blocked/1` after interval.

    B = erlang:system_time(millisecond),

    timer:sleep(2200 - (B - A)),

    ?assertNot(aec_peers:is_blocked(Id1)),
    ?assertNot(aec_peers:is_blocked(Id2)),
    ?assertNot(aec_peers:is_blocked(Id3)),
    ?assertNot(aec_peers:is_blocked(Id4)),
    ?assertEqual([], aec_peers:blocked_peers()),

    % Test peers are unblocked when accepting peers after interval.

    aec_peers:block_peer(Peer1),
    {ok, Conn6} = test_mgr_start_inbound(Peer1),
    ?assertEqual({error, blocked}, conn_peer_accepted(Conn6, maps:get(host, Peer1))),
    timer:sleep(1000),
    {ok, Conn7} = test_mgr_start_inbound(Peer1),
    ?assertEqual({error, blocked}, conn_peer_accepted(Conn7, maps:get(host, Peer1))),
    timer:sleep(1100),
    {ok, Conn8} = test_mgr_start_inbound(Peer1),
    ?assertEqual(permanent, conn_peer_accepted(Conn8, maps:get(host, Peer1))),

    % Test peers are unblocked when connecting to a peer after interval.

    aec_peers:del_peer(Id1),
    aec_peers:del_peer(Id2),
    aec_peers:del_peer(Id3),
    aec_peers:del_peer(Id4),

    aec_peers:block_peer(Peer1),

    aec_peers:add_peers(Source, Peer1),
    ?assertEqual(0, aec_peers:count(peers)),
    timer:sleep(1000),
    aec_peers:add_peers(Source, Peer1),
    ?assertEqual(0, aec_peers:count(peers)),
    timer:sleep(1100),
    aec_peers:add_peers(Source, Peer1),
    {ok, Conn9} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey1 }], {ok, _}, 1200),
    ok = conn_peer_connected(Conn9),
    ?assertEqual(1, aec_peers:count(peers)),
    ?assertEqual(1, aec_peers:count(connections)),

    % Check trusted peers cannot be blocked.

    aec_peers:add_trusted(Peer2),
    {ok, Conn10} = ?assertCalled(connect, [#{ conn_type := noise, r_pubkey := PubKey2 }], {ok, _}, 1200),
    ok = conn_peer_connected(Conn10),
    ?assertEqual(2, aec_peers:count(peers)),
    ?assertEqual(2, aec_peers:count(connections)),
    ?assertEqual([], aec_peers:blocked_peers()),
    aec_peers:block_peer(Peer2),
    ?assertEqual([], aec_peers:blocked_peers()),

    ok.

roundtrip_test() ->
    P1 = peer(<<"ef42d46eace742cd0000000000000000">>, "aeternity.com", 1990),
    {ok, ParsedP1} = aec_peers:parse_peer_address(aec_peers:encode_peer_address(P1)),
    ?assertEqual(P1#{host => <<"aeternity.com">>}, ParsedP1),
    P2 =  peer(<<"ef42d46eace742cd0000000000000000">>, <<"192.168.1.2">>, 16200),
    {ok, ParsedP2} = aec_peers:parse_peer_address(aec_peers:encode_peer_address(P2)),
    ?assertEqual(P2, ParsedP2).



%=== INTERNAL FUNCTIONS ========================================================

global_setup() ->
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(gproc),

    meck:new(aec_keys, [passthrough]),
    meck:expect(aec_keys, peer_privkey,
                fun() -> {ok, <<"7000000000000000000000000000000070000000000000000000000000000000">>} end),
    meck:expect(aec_keys, peer_pubkey,
                fun() -> {ok, <<"70000000000000000000000000000000">>} end),
    Persist = application:get_env(aecore, persist),
    application:set_env(aecore, persist, false),
    aec_db:check_db(),
    aec_db:prepare_mnesia_bypass(),
    aec_db:clear_db(),
    Persist.

global_teardown(Persist) ->
    meck:unload(aec_keys),
    application:stop(gproc),
    crypto:stop(),
    application:stop(mnesia),
    application:set_env(aecore, persist, Persist),
    ok = mnesia:delete_schema([node()]).

setup() ->
    setup([]).

setup(Overrides) ->
    Overridden = lists:foldl(fun({K, V, D}, Acc) ->
        OldValue = application:get_env(aecore, K, D),
        application:set_env(aecore, K, V),
        [{K, OldValue} | Acc]
    end, [], Overrides),

    TestMgr = test_mgr_start(),

    meck:new(aec_peer_connection, []),
    meck:expect(aec_peer_connection, connect,
        fun(PeerInfo) -> call(TestMgr, connect, [PeerInfo]) end),
    meck:expect(aec_peer_connection, disconnect,
        fun(Pid) when is_pid(Pid) -> call(Pid, disconnect, []) end),

    meck:new(aec_sync, []),
    meck:expect(aec_sync, schedule_ping,
        fun(PeerId) -> call(TestMgr, schedule_ping, [PeerId]) end),

    aec_peers:start_link(),
    Overridden.

teardown(Overridden) ->
    test_mgr_stop(),
    gen_server:stop(aec_peers),
    meck:unload(aec_peer_connection),
    meck:unload(aec_sync),
    cleanup_mock_getaddr(), % only for test_invalid_hostname/0 test
    lists:foreach(fun({K, V}) ->
        application:set_env(aecore, K, V)
    end, Overridden),
    ok.

peer(PubKey, Host, Port) when is_binary(PubKey) ->
    #{ pubkey => PubKey, host => Host, port => Port };
peer(Idx, Host, Port) when is_integer(Idx), Idx >= 0, Idx < 16 ->
    [Char] = io_lib:format("~.16b", [Idx]),
    PubKey = list_to_binary(lists:duplicate(32, Char)),
    #{ pubkey => PubKey, host => Host, port => Port }.

dump_messages() -> dump_messages([]).

dump_messages(Acc) ->
    receive
        Msg -> dump_messages([Msg | Acc])
    after 0 ->
        case Acc of
            [] -> ok;
            _ ->
                io:format("Message box:~n", []),
                lists:foreach(fun(M) ->
                    io:format("    ~p~n", [M])
                end, lists:reverse(Acc))
        end
    end.

setup_mock_getaddr() ->
    cleanup_mock_getaddr(),
    ok = meck:new(inet, [unstick, passthrough]).

cleanup_mock_getaddr() ->
    catch meck:unload(inet).

mock_getaddr(Result) ->
    Self = self(),
    ok = meck:expect(inet, getaddr, fun(Hostname, _) ->
        Self ! {getaddr, Hostname},
        Result
    end).

%--- TESTING PROCESSES ---------------------------------------------------------

cast(PidOrName, Label, Args) ->
    PidOrName ! {Label, Args}.

call(PidOrName, Label, Args) ->
    MRef = erlang:monitor(process, PidOrName),
    PidOrName ! {{self(), MRef}, Label, Args},
    receive
        {MRef, Result} ->
            erlang:demonitor(MRef, [flush]),
            Result;
        {'DOWN', MRef, _, _, _Reason} ->
            {error, noproc}
    after
        5000 ->
            {error, timeout}
    end.

reply({Sender, MRef}, Result) ->
    Sender ! {MRef, Result},
    ok.

test_mgr_start() ->
    erlang:spawn_link(fun() -> proc_test_mgr_init() end).

test_mgr_set_recipient(Pid) ->
    call(test_conn_mgr, set_recipient, [Pid]).

test_mgr_stop() ->
    call(test_conn_mgr, stop, []).

test_mgr_start_inbound(PeerInfo) ->
    call(test_conn_mgr, start_inbound, [PeerInfo]).

proc_test_mgr_init() ->
    S = #{ procs => [], recipient => undefined },
    register(test_conn_mgr, self()),
    proc_test_mgr_loop(S).

proc_test_mgr_loop(S) ->
    receive
        {From, set_recipient, [Pid]} ->
            reply(From, ok),
            proc_test_mgr_loop(S#{ recipient := Pid });
        {From, start_inbound, [PeerInfo]} ->
            #{ procs := Procs, recipient := Recipient } = S,
            Pid = conn_start(self(), Recipient, {inbound, PeerInfo}),
            S2 = S#{ procs := [Pid | Procs] },
            reply(From, {ok, Pid}),
            proc_test_mgr_loop(S2);
        {From, connect, [PeerInfo]} ->
            #{ procs := Procs, recipient := Recipient } = S,
            Pid = conn_start(self(), Recipient, {outbound, PeerInfo}),
            S2 = S#{ procs := [Pid | Procs] },
            reply(From, {ok, Pid}),
            Recipient ! {connect, [PeerInfo], {ok, Pid}},
            proc_test_mgr_loop(S2);
        {From, schedule_ping, [PeerId]} ->
            #{ recipient := Recipient } = S,
            reply(From, ok),
            Recipient ! {schedule_ping, [PeerId], ok},
            proc_test_mgr_loop(S);
        {stopped, [Child]} ->
            #{ procs := Procs } = S,
            S2 = S#{ procs := lists:delete(Child, Procs) },
            proc_test_mgr_loop(S2);
        {From, stop, []} ->
            #{ procs := Procs } = S,
            lists:foreach(fun conn_kill/1, Procs),
            reply(From, ok)
    end.


conn_start(Parent, Recipient, PeerInfo) ->
    erlang:spawn_link(fun() -> proc_conn_init(Parent, Recipient, PeerInfo) end).

conn_kill(Pid) ->
    Ref = erlang:monitor(process, Pid),
    call(Pid, kill, []),
    receive
        {'DOWN', Ref, _Type, Pid, _Info} -> ok
    after
        5000 -> ?fail("timout waiting for connection to get killed", [])
    end.

conn_peer_connected(Pid) ->
    call(Pid, peer_connected, []).

conn_peer_accepted(Pid, Host) ->
    call(Pid, peer_accepted, [Host]).

conn_peer_alive(Pid) ->
    call(Pid, peer_alive, []).

conn_connection_failed(Pid) ->
    call(Pid, connection_failed, []).

conn_connection_closed(Pid) ->
    call(Pid, connection_closed, []).

conn_peer_dead(Pid) ->
    call(Pid, peer_dead, []).

conn_log_ping(Pid, Outcome) ->
    call(Pid, log_ping, [Outcome]).

proc_conn_init(Parent, Recipient, {outbound, Opts}) ->
    #{ r_pubkey := PeerId } = Opts,
    proc_conn_loop(#{
        connected => false,
        parent => Parent,
        recipient => Recipient,
        id => PeerId
    });
proc_conn_init(Parent, Recipient, {inbound, PeerInfo}) ->
    PeerId = peer_id(PeerInfo),
    proc_conn_loop(#{
        connected => false,
        parent => Parent,
        recipient => Recipient,
        id => PeerId,
        info => PeerInfo
    }).

proc_conn_loop(S) ->
    receive
        {From, peer_connected, []} ->
            #{ parent := Parent, id := PeerId } = S,
            case aec_peers:peer_connected(PeerId, self()) of
                ok = Result ->
                    S2 = S#{ connected := true },
                    reply(From, Result),
                    proc_conn_loop(S2);
                {error, _} = Result ->
                    cast(Parent, stopped, self()),
                    reply(From, Result)
            end;
        {From, peer_accepted, [Host]} ->
            #{ parent := Parent, info := Info } = S,
            {ok, Addr} = inet:parse_address(Host),
            case aec_peers:peer_accepted(Info, Addr, self()) of
                R when R =:= permanent; R =:= temporary ->
                    S2 = S#{ connected := true },
                    reply(From, R),
                    proc_conn_loop(S2);
                {error, _} = Result ->
                    cast(Parent, stopped, self()),
                    reply(From, Result)
            end;
        {From, peer_alive, []} ->
            #{ parent := Parent, id := PeerId } = S,
            case aec_peers:peer_alive(PeerId, self()) of
                ok ->
                    reply(From, ok),
                    proc_conn_loop(S);
                {error, _} = Result ->
                    cast(Parent, stopped, self()),
                    reply(From, Result)
            end;
        {From, connection_failed, []} ->
            #{ parent := Parent, id := PeerId } = S,
            Result = aec_peers:connection_failed(PeerId, self()),
            cast(Parent, stopped, self()),
            reply(From, Result);
        {From, connection_closed, []} ->
            #{ parent := Parent, id := PeerId } = S,
            Result = aec_peers:connection_closed(PeerId, self()),
            cast(Parent, stopped, self()),
            reply(From, Result);
        {From, peer_dead, []} ->
            #{ id := PeerId } = S,
            ok = aec_peers:peer_dead(PeerId, self()),
            reply(From, ok),
            proc_conn_loop(S);
        {From, log_ping, [Outcome]} ->
            #{ id := PeerId } = S,
            Result = aec_peers:log_ping(PeerId, Outcome),
            reply(From, Result),
            proc_conn_loop(S);
        {From, disconnect, []} ->
            #{ recipient := Recipient } = S,
            Recipient ! {disconnect, [  self()], ok},
            reply(From, ok);
        {From, stop, []} ->
            #{ connected := IsConnected, parent := Parent, id := PeerId } = S,
            case IsConnected of
                true ->  aec_peers:connection_closed(PeerId, self());
                false -> ok
            end,
            cast(Parent, stopped, self()),
            reply(From, ok);
        {From, kill, []} ->
            #{ parent := Parent } = S,
            cast(Parent, stopped, self()),
            reply(From, ok)
    end.

    peer_id(Bin) when is_binary(Bin) -> Bin;
    peer_id(InfoOrPeer) -> aec_peer:id(InfoOrPeer).
-endif.
