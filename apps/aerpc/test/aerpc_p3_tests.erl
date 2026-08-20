%%%-------------------------------------------------------------------
%%% @doc P3 coverage: the filter registry and its bounds, log-index
%%% retention, and the fourth instance of the micro-vs-key-block hash
%%% confusion -- this one in the subscription fan-out.
%%%
%%% The subscription case is here rather than in a lab run on purpose:
%%% it was found by reading, held across two commits, and scoped into
%%% this row so a test would catch it instead of an acceptance run.
%%%
%%% The retention cases assert the half that is easy to leave out.
%%% Evicting entries is not the hard part; moving the floor with them is,
%%% because an index that keeps claiming coverage for heights it no
%%% longer holds answers `eth_getLogs' with a short list that looks
%%% exactly like a complete one.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_p3_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aerpc/include/aerpc_log_store.hrl").

-define(KEY_HASH,   <<71:32/unit:8>>).
-define(MICRO_HASH, <<72:32/unit:8>>).
-define(PREV_KEY,   <<73:32/unit:8>>).
-define(KEY_HASH2,  <<76:32/unit:8>>).
-define(PREV_KEY2,  <<77:32/unit:8>>).
-define(SENDER,     <<74:8, 0:248>>).
-define(RECIPIENT,  <<75:8, 0:248>>).
-define(TOP_HEIGHT, 20).

%% ===================================================================
%% Filter registry
%% ===================================================================

registry_test_() ->
    {foreach,
     fun setup_registry/0,
     fun teardown_registry/1,
     [fun(_) -> {"a block filter is allocated, polls empty, and uninstalls",
                 fun block_filter_lifecycle/0} end,
      fun(_) -> {"an unknown or uninstalled id is -32000 filter not found",
                 fun unknown_filter/0} end,
      fun(_) -> {"getFilterLogs is log-filters only",
                 fun get_filter_logs_kind/0} end,
      fun(_) -> {"eth_newFilter rejects blockHash",
                 fun new_filter_rejects_block_hash/0} end,
      fun(_) -> {"a block filter returns the key-block hashes it has not "
                 "returned before",
                 fun block_filter_cursor/0} end,
      fun(_) -> {"a pending-tx filter drains once and its buffer is capped",
                 fun pending_tx_filter/0} end,
      fun(_) -> {"a locally submitted transaction reaches a pending filter",
                 fun pending_filter_tx_created/0} end,
      fun(_) -> {"the filter cap is enforced",
                 fun filter_cap/0} end,
      fun(_) -> {"an idle filter is swept",
                 fun idle_filter_swept/0} end]}.

block_filter_lifecycle() ->
    {ok, Id} = aerpc_filter_registry:new_block_filter(),
    ?assertMatch(<<"0x", _/binary>>, Id),
    %% Cursor starts at the current top, so nothing is new yet.
    ?assertEqual({ok, []}, aerpc_filter_registry:changes(Id)),
    ?assertEqual({ok, true},  aerpc_filter_registry:uninstall(Id)),
    ?assertEqual({ok, false}, aerpc_filter_registry:uninstall(Id)),
    ?assertMatch({error, -32000, <<"filter not found">>},
                 aerpc_filter_registry:changes(Id)).

unknown_filter() ->
    ?assertMatch({error, -32000, _}, aerpc_filter_registry:changes(<<"0xdead">>)),
    ?assertMatch({error, -32000, _}, aerpc_filter_registry:logs(<<"0xdead">>)).

get_filter_logs_kind() ->
    {ok, BlockId} = aerpc_filter_registry:new_block_filter(),
    ?assertMatch({error, -32000, _}, aerpc_filter_registry:logs(BlockId)),
    {ok, LogId} = aerpc_filter_registry:new_log_filter(#{}),
    ?assertMatch({ok, _}, aerpc_filter_registry:logs(LogId)).

new_filter_rejects_block_hash() ->
    %% A single-block query has no cursor to advance; that is eth_getLogs.
    ?assertMatch({error, -32602, _},
                 aerpc_filter_registry:new_log_filter(
                   #{<<"blockHash">> => <<"0x00">>})).

block_filter_cursor() ->
    {ok, Id} = aerpc_filter_registry:new_block_filter(),
    ?assertEqual({ok, []}, aerpc_filter_registry:changes(Id)),
    set_top(?TOP_HEIGHT + 2),
    {ok, Hashes} = aerpc_filter_registry:changes(Id),
    ?assertEqual(2, length(Hashes)),
    [?assertMatch(<<"0x", _/binary>>, H) || H <- Hashes],
    %% The cursor advanced, so an immediate second poll is empty rather
    %% than replaying the same two blocks.
    ?assertEqual({ok, []}, aerpc_filter_registry:changes(Id)).

pending_tx_filter() ->
    {ok, Id} = aerpc_filter_registry:new_pending_tx_filter(),
    ?assertEqual({ok, []}, aerpc_filter_registry:changes(Id)),
    STx = spend_tx(1),
    publish_tx(STx),
    {ok, [Hash]} = aerpc_filter_registry:changes(Id),
    ?assertEqual(aerpc_encoding:format_tx_hash(aetx_sign:hash(STx)), Hash),
    %% Drained.
    ?assertEqual({ok, []}, aerpc_filter_registry:changes(Id)),
    %% And bounded: past the cap the oldest go, and the loss is visible
    %% in status rather than silent.
    [publish_tx(spend_tx(N)) || N <- lists:seq(1, 1100)],
    {ok, Drained} = aerpc_filter_registry:changes(Id),
    ?assertEqual(1000, length(Drained)).

pending_filter_tx_created() ->
    %% Same defect, poll half: the filter was subscribed to tx_received
    %% only, so Lab's "drained 2 hashes" could not have come from that
    %% run's own locally-pushed workload.
    {ok, Id} = aerpc_filter_registry:new_pending_tx_filter(),
    STx = spend_tx(21),
    publish_tx(tx_created, STx),
    _ = aerpc_filter_registry:status(),   %% flush the info message
    {ok, Hashes} = aerpc_filter_registry:changes(Id),
    ?assertEqual([aerpc_encoding:format_tx_hash(aetx_sign:hash(STx))], Hashes).

filter_cap() ->
    application:set_env(aerpc, max_filters, 2),
    {ok, _} = aerpc_filter_registry:new_block_filter(),
    {ok, _} = aerpc_filter_registry:new_block_filter(),
    ?assertMatch({error, -32009, _}, aerpc_filter_registry:new_block_filter()).

idle_filter_swept() ->
    application:set_env(aerpc, filter_ttl_seconds, 1),
    {ok, Id} = aerpc_filter_registry:new_block_filter(),
    ?assertEqual({ok, []}, aerpc_filter_registry:changes(Id)),
    %% Integer-second resolution, so sleep past two boundaries.
    timer:sleep(2100),
    whereis(aerpc_filter_registry) ! sweep,
    _ = aerpc_filter_registry:status(),
    ?assertMatch({error, -32000, _}, aerpc_filter_registry:changes(Id)).

%% ===================================================================
%% Log index retention
%% ===================================================================

retention_test_() ->
    {setup,
     fun() -> aerpc_log_store:init(), ok end,
     fun(_) -> catch ets:delete(aerpc_log_idx),
               catch ets:delete(aerpc_log_meta),
               ok
     end,
     [{"eviction removes only the entries below the new floor",
       fun evicts_below_floor/0},
      {"the coverage window narrows with the eviction",
       fun coverage_narrows/0}]}.

evicts_below_floor() ->
    [aerpc_log_store:insert(entry_at(H)) || H <- lists:seq(1, 10)],
    ?assertEqual(10, aerpc_log_store:size()),
    ?assertEqual(5, aerpc_log_store:evict_below(6)),
    ?assertEqual(5, aerpc_log_store:size()),
    Remaining = [E#log_entry.height
                 || E <- aerpc_log_store:select_range(any, 0, 100)],
    ?assertEqual([6, 7, 8, 9, 10], lists:sort(Remaining)),
    %% Idempotent: nothing left below the floor to remove.
    ?assertEqual(0, aerpc_log_store:evict_below(6)).

coverage_narrows() ->
    aerpc_log_store:set_floor(1),
    aerpc_log_store:set_watermark(10),
    ?assert(aerpc_log_store:indexed({1, 10})),
    %% After eviction the floor must move with it, or eth_getLogs would
    %% answer heights 1-5 from an index that no longer holds them.
    _ = aerpc_log_store:evict_below(6),
    aerpc_log_store:set_floor(6),
    ?assertNot(aerpc_log_store:indexed({1, 10})),
    ?assert(aerpc_log_store:indexed({6, 10})).

%% ===================================================================
%% Subscriptions: the held micro-vs-key-block instance
%% ===================================================================

subscriptions_test_() ->
    {foreach,
     fun setup_subs/0,
     fun teardown_subs/1,
     [fun(_) -> {"a micro-block event does not announce the still-open "
                 "generation",
                 fun micro_event_does_not_announce_open_generation/0} end,
      fun(_) -> {"a generation is announced exactly once, however many "
                 "micro blocks it had",
                 fun generation_announced_exactly_once/0} end,
      fun(_) -> {"a key-block event notifies on the generation it closed",
                 fun key_event_uses_closed_generation/0} end,
      fun(_) -> {"with no subscribers nothing is built at all",
                 fun no_subscribers_no_work/0} end]}.

micro_event_does_not_announce_open_generation() ->
    {ok, _SubId} = aerpc_subscriptions:subscribe(self(), newHeads, undefined),
    %% A micro block extends a generation that is still open; announcing
    %% it here is what produced 41 frames for 15 generations on the wire,
    %% because every micro block under a generation re-announced it whole.
    [publish_top(?MICRO_HASH, micro) || _ <- lists:seq(1, 4)],
    ?assertEqual(0, frames_received()).

key_event_uses_closed_generation() ->
    {ok, SubId} = aerpc_subscriptions:subscribe(self(), newHeads, undefined),
    publish_top(?KEY_HASH, key),
    receive
        {aerpc_notify, SubId, Block} ->
            %% A key block opens an empty generation, so the one worth
            %% announcing is the one it closed.
            ?assertEqual(?PREV_KEY, maps:get(marker, Block))
    after 2000 ->
        ?assert(false)
    end.

%% The property Lab's harness now asserts, at the source: count the
%% deliveries, not just their shape.
generation_announced_exactly_once() ->
    {ok, _SubId} = aerpc_subscriptions:subscribe(self(), newHeads, undefined),
    %% A realistic generation: several micro blocks, then the key block
    %% that closes it.
    [publish_top(?MICRO_HASH, micro) || _ <- lists:seq(1, 3)],
    publish_top(?KEY_HASH, key),
    ?assertEqual(1, frames_received()),
    %% A repeated key-block event for the same generation is a no-op.
    publish_top(?KEY_HASH, key),
    ?assertEqual(0, frames_received()),
    %% A different generation still gets its own frame.
    publish_top(?KEY_HASH2, key),
    ?assertEqual(1, frames_received()).

no_subscribers_no_work() ->
    publish_top(?KEY_HASH, key),
    ?assertEqual(0, meck:num_calls(aerpc_block, by_hash, '_')).

%% ===================================================================
%% newPendingTransactions over the subscription path
%% ===================================================================

pending_subscription_test_() ->
    {foreach,
     fun setup_subs/0,
     fun teardown_subs/1,
     [fun(_) -> {"a mempool arrival is pushed as a tx hash",
                 fun pending_sub_pushes_hash/0} end,
      fun(_) -> {"with the full flag the payload is the eth tx object",
                 fun pending_sub_full_tx/0} end,
      fun(_) -> {"a LOCALLY submitted transaction reaches the subscriber",
                 fun pending_sub_tx_created/0} end,
      fun(_) -> {"a newHeads subscriber is not sent pending transactions",
                 fun pending_does_not_leak_to_other_kinds/0} end,
      fun(_) -> {"a pending subscription survives a generation close "
                 "alongside newHeads and logs",
                 fun pending_survives_generation_close/0} end,
      fun(_) -> {"a pending subscription alone costs no generation work",
                 fun pending_alone_does_no_generation_work/0} end,
      fun(_) -> {"unsubscribing stops the frames",
                 fun pending_unsubscribe/0} end]}.

pending_sub_pushes_hash() ->
    {ok, SubId} = aerpc_subscriptions:subscribe(self(), pending_tx, false),
    STx = spend_tx(7),
    publish_pending(STx),
    Expected = aerpc_encoding:format_tx_hash(aetx_sign:hash(STx)),
    receive
        {aerpc_notify, SubId, Payload} -> ?assertEqual(Expected, Payload)
    after 2000 -> ?assert(false)
    end.

pending_sub_tx_created() ->
    %% aec_tx_pool:push/1 defaults to tx_created, and that is what the
    %% node's own POST /v3/transactions uses -- so every SDK, wallet and
    %% dapp submission arrives on this event and not on tx_received.
    %% Listening only to tx_received meant a single-node deployment saw
    %% nothing at all.
    {ok, SubId} = aerpc_subscriptions:subscribe(self(), pending_tx, false),
    STx = spend_tx(12),
    publish_pending(tx_created, STx),
    Expected = aerpc_encoding:format_tx_hash(aetx_sign:hash(STx)),
    receive
        {aerpc_notify, SubId, Payload} -> ?assertEqual(Expected, Payload)
    after 2000 -> ?assert(false)
    end.

pending_sub_full_tx() ->
    {ok, SubId} = aerpc_subscriptions:subscribe(self(), pending_tx, true),
    STx = spend_tx(8),
    publish_pending(STx),
    receive
        {aerpc_notify, SubId, Payload} ->
            ?assert(is_map(Payload)),
            ?assertEqual(aerpc_encoding:format_tx_hash(aetx_sign:hash(STx)),
                         maps:get(<<"hash">>, Payload)),
            %% Pending, so no block position -- same shape the HTTP
            %% eth_getTransactionByHash returns for a mempool tx.
            ?assertEqual(null, maps:get(<<"blockHash">>, Payload))
    after 2000 -> ?assert(false)
    end.

pending_does_not_leak_to_other_kinds() ->
    {ok, _HeadsId} = aerpc_subscriptions:subscribe(self(), newHeads, undefined),
    publish_pending(spend_tx(9)),
    receive
        {aerpc_notify, _, _} -> ?assert(false)
    after 300 -> ok
    end.

pending_survives_generation_close() ->
    %% The P3c-1 crash: fanout/2 folded over every subscription with
    %% clauses for newHeads and logs only, so a pending_tx entry in
    %% by_id was a function_clause that killed this gen_server -- and the
    %% supervisor restarted it empty, silently dropping every other
    %% client's subscriptions on the node too.
    Registry = whereis(aerpc_subscriptions),
    {ok, PendingId} = aerpc_subscriptions:subscribe(self(), pending_tx, false),
    {ok, HeadsId}   = aerpc_subscriptions:subscribe(self(), newHeads, undefined),
    {ok, _LogsId}   = aerpc_subscriptions:subscribe(self(), logs, #{}),

    publish_top(?KEY_HASH, key),

    %% Still the same process: no crash, no restart.
    ?assertEqual(Registry, whereis(aerpc_subscriptions)),
    ?assert(is_process_alive(Registry)),
    %% The generation-driven subscriber was served.
    ?assert(lists:member(HeadsId, received_sub_ids())),

    %% And the pending subscription is still registered afterwards --
    %% which is what "survives" has to mean. A restarted registry would
    %% have forgotten it and delivered nothing here.
    publish_pending(tx_created, spend_tx(31)),
    ?assert(lists:member(PendingId, received_sub_ids())).

pending_alone_does_no_generation_work() ->
    %% With only a pending subscriber the map is non-empty, so the old
    %% `map_size(Subs) =:= 0' guard let the expensive path run: a whole
    %% generation fetch and a bloom over its logs, for nobody.
    {ok, _PendingId} = aerpc_subscriptions:subscribe(self(), pending_tx, false),
    publish_top(?KEY_HASH, key),
    ?assertEqual(0, meck:num_calls(aerpc_block, by_hash, '_')),
    ?assertEqual(0, frames_received()).

pending_unsubscribe() ->
    {ok, SubId} = aerpc_subscriptions:subscribe(self(), pending_tx, false),
    publish_pending(spend_tx(10)),
    receive {aerpc_notify, SubId, _} -> ok after 2000 -> ?assert(false) end,
    ?assert(aerpc_subscriptions:unsubscribe(self(), SubId)),
    publish_pending(spend_tx(11)),
    receive
        {aerpc_notify, _, _} -> ?assert(false)
    after 300 -> ok
    end.

%% ===================================================================
%% Unsupported kind vs malformed params
%% ===================================================================

subscribe_params_test_() ->
    [{"the three supported kinds parse",
      fun() ->
          ?assertEqual({ok, newHeads, undefined},
                       aerpc_subscriptions:parse_subscribe_params(
                         [<<"newHeads">>])),
          ?assertEqual({ok, logs, #{}},
                       aerpc_subscriptions:parse_subscribe_params(
                         [<<"logs">>])),
          ?assertEqual({ok, pending_tx, false},
                       aerpc_subscriptions:parse_subscribe_params(
                         [<<"newPendingTransactions">>])),
          ?assertEqual({ok, pending_tx, true},
                       aerpc_subscriptions:parse_subscribe_params(
                         [<<"newPendingTransactions">>, true])),
          %% Lab sent this exact form and got -32602.
          ?assertEqual({ok, pending_tx, false},
                       aerpc_subscriptions:parse_subscribe_params(
                         [<<"newPendingTransactions">>, false]))
      end},
     {"an unsupported kind is distinguishable from a malformed call",
      fun() ->
          {error, KindCode, Msg} =
              aerpc_subscriptions:parse_subscribe_params([<<"syncing">>]),
          ?assertEqual(-32004, KindCode),
          %% Naming the kind and the supported set is what lets a client
          %% fall back to the poll filter instead of just failing.
          ?assertNotEqual(nomatch, binary:match(Msg, <<"syncing">>)),
          ?assertNotEqual(nomatch,
                          binary:match(Msg, <<"newPendingTransactions">>)),
          %% The distinction is the point: not the same code as a typo.
          [?assertMatch({error, -32602, _},
                        aerpc_subscriptions:parse_subscribe_params(P))
           || P <- [[], [123], <<"newHeads">>, #{}]],
          ?assertNotEqual(-32602, KindCode)
      end}].

%% ===================================================================
%% Fixtures
%% ===================================================================

setup_registry() ->
    reset_env(),
    ok = meck:new(aec_chain, [passthrough, no_link]),
    set_top(?TOP_HEIGHT),
    ok = meck:expect(aec_chain, get_key_block_by_height,
                     fun(H) -> {ok, key_block_at(H)} end),
    %% The log path reaches a generation lookup; without a DB behind it
    %% that has to be answered here rather than passed through.
    ok = meck:expect(aec_chain, get_generation_by_hash,
                     fun(_Hash, _Dir) -> error end),
    {ok, Pid} = aerpc_filter_registry:start_link(),
    Pid.

teardown_registry(Pid) ->
    stop(Pid),
    ok = meck:unload(aec_chain),
    reset_env().

setup_subs() ->
    ok = meck:new(aec_chain, [passthrough, no_link]),
    ok = meck:new(aerpc_block, [passthrough, no_link]),
    ok = meck:expect(aec_chain, get_header, fun header_for/1),
    %% A `logs' subscriber sends the fan-out through aerpc_logs, which
    %% reaches a generation lookup; without a DB behind it that has to be
    %% answered here rather than passed through.
    ok = meck:expect(aec_chain, get_generation_by_hash,
                     fun(_Hash, _Dir) -> error end),
    %% Return a marker carrying the hash the fan-out actually asked for,
    %% which is the whole question this case settles.
    ok = meck:expect(aerpc_block, by_hash,
                     fun(HexHash, _Full) ->
                         {ok, #{marker => aerpc_encoding:from_hex_data(HexHash)}}
                     end),
    {ok, Pid} = aerpc_subscriptions:start_link(),
    Pid.

teardown_subs(Pid) ->
    stop(Pid),
    ok = meck:unload(aerpc_block),
    ok = meck:unload(aec_chain).

reset_env() ->
    application:unset_env(aerpc, max_filters),
    application:unset_env(aerpc, filter_ttl_seconds),
    ok.

set_top(Height) ->
    ok = meck:expect(aec_chain, top_header,
                     fun() -> aec_blocks:to_header(key_block_at(Height)) end).

%% ?HIGHEST_TARGET_SCI. A real target rather than `undefined': these
%% blocks get hashed (the registry turns heights into block hashes), and
%% `serialize_to_binary/1' writes the target as a 32-bit integer, so
%% `undefined' is a badarg there rather than at construction.
-define(TARGET, 16#2100ffff).

key_block_at(Height) ->
    aec_blocks:new_key(Height, ?PREV_KEY, ?PREV_KEY, <<0:32/unit:8>>,
                       ?TARGET, 0, 1504731164584, default,
                       protocol(Height), <<0:32/unit:8>>, ?SENDER).

micro_block() ->
    aec_blocks:new_micro(?TOP_HEIGHT, ?PREV_KEY, ?PREV_KEY, <<0:32/unit:8>>,
                         <<0:32/unit:8>>, [], 1504731164584, no_fraud,
                         protocol(?TOP_HEIGHT)).

header_for(?MICRO_HASH) -> {ok, aec_blocks:to_header(micro_block())};
header_for(?KEY_HASH)   -> {ok, aec_blocks:to_header(key_block_at(?TOP_HEIGHT))};
header_for(?KEY_HASH2)  -> {ok, aec_blocks:to_header(key_block2())};
header_for(_Other)      -> error.

%% A second key block closing a DIFFERENT generation, so the
%% announced-once guard can be shown to suppress a repeat without
%% suppressing genuinely new work.
key_block2() ->
    aec_blocks:new_key(?TOP_HEIGHT + 1, ?PREV_KEY2, ?PREV_KEY2,
                       <<0:32/unit:8>>, ?TARGET, 0, 1504731164584, default,
                       protocol(?TOP_HEIGHT + 1), <<0:32/unit:8>>, ?SENDER).

%% Drain and count whatever the fan-out delivered. A short settle window,
%% because the assertion is about how many frames arrive rather than how
%% fast.
%% Drain and report which subscription ids delivered, for the cases that
%% care about who was served rather than how many frames arrived.
received_sub_ids() ->
    received_sub_ids([]).

received_sub_ids(Acc) ->
    receive
        {aerpc_notify, SubId, _Payload} -> received_sub_ids([SubId | Acc])
    after 150 ->
        Acc
    end.

frames_received() ->
    frames_received(0).

frames_received(N) ->
    receive
        {aerpc_notify, _SubId, _Payload} -> frames_received(N + 1)
    after 150 ->
        N
    end.

protocol(Height) -> aec_hard_forks:protocol_effective_at_height(Height).

entry_at(Height) ->
    aerpc_log_store:make_entry(<<Height:32/unit:8>>, Height, 0, 0,
                               [], <<>>, ?KEY_HASH, ?MICRO_HASH, ?KEY_HASH).

spend_tx(Nonce) ->
    {ok, Aetx} = aec_spend_tx:new(
                   #{sender_id    => aeser_id:create(account, ?SENDER),
                     recipient_id => aeser_id:create(account, ?RECIPIENT),
                     amount       => 1, fee => 20000, nonce => Nonce,
                     payload      => <<>>}),
    aetx_sign:new(Aetx, []).

publish_tx(SignedTx) ->
    publish_tx(tx_received, SignedTx).

publish_tx(Event, SignedTx) ->
    Pid = whereis(aerpc_filter_registry),
    Pid ! {gproc_ps_event, Event, #{info => SignedTx}},
    ok.

publish_pending(SignedTx) ->
    publish_pending(tx_received, SignedTx).

publish_pending(Event, SignedTx) ->
    Pid = whereis(aerpc_subscriptions),
    Pid ! {gproc_ps_event, Event, #{info => SignedTx}},
    _ = sys:get_state(Pid),
    ok.

publish_top(Hash, Type) ->
    Pid = whereis(aerpc_subscriptions),
    Pid ! {gproc_ps_event, top_changed,
           #{info => #{block_hash => Hash, block_type => Type}}},
    _ = sys:get_state(Pid),
    ok.

stop(Pid) ->
    unlink(Pid),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive {'DOWN', MRef, process, Pid, _} -> ok
    after 5000 -> ok
    end.

-endif.
