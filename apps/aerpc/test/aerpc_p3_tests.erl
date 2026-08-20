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
     [fun(_) -> {"a micro-block event notifies on the GENERATION key "
                 "block, not the micro-block hash",
                 fun micro_event_uses_generation_hash/0} end,
      fun(_) -> {"a key-block event notifies on the generation it closed",
                 fun key_event_uses_closed_generation/0} end,
      fun(_) -> {"with no subscribers nothing is built at all",
                 fun no_subscribers_no_work/0} end]}.

micro_event_uses_generation_hash() ->
    {ok, SubId} = aerpc_subscriptions:subscribe(self(), newHeads, undefined),
    publish_top(?MICRO_HASH, micro),
    %% Before the fix this arrived as `#{}': aerpc_block:by_hash/2 was
    %% handed the micro-block hash, and a generation lookup on that
    %% returns nothing.
    receive
        {aerpc_notify, SubId, Block} ->
            ?assertEqual(?PREV_KEY, maps:get(marker, Block))
    after 2000 ->
        ?assert(false)
    end.

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

no_subscribers_no_work() ->
    publish_top(?MICRO_HASH, micro),
    ?assertEqual(0, meck:num_calls(aerpc_block, by_hash, '_')).

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
header_for(_Other)      -> error.

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
    Pid = whereis(aerpc_filter_registry),
    Pid ! {gproc_ps_event, tx_received, #{info => SignedTx}},
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
