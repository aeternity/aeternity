%%%-------------------------------------------------------------------
%%% @doc Regression coverage for the two defects Lab's second acceptance
%%% run found on the 20-byte address contract.
%%%
%%% R1 -- incremental address-index maintenance never indexed anything.
%%% `top_changed' carries the new TOP block hash, and the handler fed it
%%% to a key-block-keyed generation lookup: `error' for a micro block,
%%% an empty generation for a key block. Measured effect: a contract
%%% deployed during uptime answered `eth_getCode' with "0x" and
%%% `eth_getBalance' with "0x0" -- well-formed wrong answers, which is
%%% exactly what the index exists to prevent. These cases drive the real
%%% `handle_info' clause, because the helpers under it were never the
%%% broken part.
%%%
%%% R2 -- the dry-run nonce came from `aec_next_nonce:pick_for_account/1',
%%% which is pool-aware, while `aec_dry_run' applies against chain state.
%%% Twelve of twelve `eth_estimateGas' calls failed with
%%% tx_nonce_too_high_for_account while two txs sat in the pool. The test
%%% pins the nonce actually handed to `aec_dry_run' and mocks the
%%% pool-aware source to a deliberately different value, so wiring it
%%% back in fails here rather than in a lab run.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_r1r2_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% Distinct in their FIRST 20 bytes, which is the part that becomes the
%% address. `<<11:32/unit:8>>' and friends are big-endian, so they share
%% an all-zero 20-byte prefix and every one of them would collide -- the
%% index would then correctly keep the first mapping and refuse the rest,
%% and the test would be measuring collision handling rather than
%% incremental maintenance.
-define(SENDER,     <<11:8, 0:248>>).
-define(RECIPIENT,  <<22:8, 0:248>>).
-define(BENE,       <<33:8, 0:248>>).
-define(CONTRACT,   <<77:8, 0:248>>).
-define(MICRO_HASH, <<44:32/unit:8>>).
-define(KEY_HASH,   <<55:32/unit:8>>).
-define(PREV_KEY,   <<66:32/unit:8>>).
-define(HEIGHT,     10).
-define(CAPTURE,    aerpc_r1r2_capture).

%% ===================================================================
%% R1 -- incremental maintenance
%% ===================================================================

r1_test_() ->
    {foreach,
     fun setup_index/0,
     fun teardown_index/1,
     [fun(_Pid) -> {"a mined micro block indexes its transactions",
                    fun micro_block_indexes_txs/0} end,
      fun(_Pid) -> {"an address first seen after backfill stops being "
                    "answered as empty and starts resolving",
                    fun post_backfill_address_resolves/0} end,
      fun(_Pid) -> {"a key block indexes its beneficiary and sweeps the "
                    "generation it closed",
                    fun key_block_sweeps_closed_generation/0} end,
      fun(_Pid) -> {"an event for a block the node does not have changes "
                    "nothing",
                    fun unknown_block_is_a_noop/0} end]}.

micro_block_indexes_txs() ->
    %% Guard the fixtures themselves: three pubkeys that shared an
    %% addr20 would make every assertion below meaningless.
    ?assertEqual(3, length(lists:usort(
                             [aerpc_addr_index:to_addr20(PK)
                              || PK <- [?SENDER, ?RECIPIENT, ?BENE]]))),
    ?assertEqual(unknown, resolve(?SENDER)),
    ?assertEqual(unknown, resolve(?RECIPIENT)),
    publish_top(?MICRO_HASH, micro),
    ?assertEqual({ok, ?SENDER},    resolve(?SENDER)),
    ?assertEqual({ok, ?RECIPIENT}, resolve(?RECIPIENT)).

post_backfill_address_resolves() ->
    %% The R1 symptom at the method boundary: before the event the
    %% address is `unknown' and eth's empty default applies, which is the
    %% wrong answer for an account that does exist on chain.
    Addr = aerpc_encoding:format_account(?RECIPIENT),
    ?assertEqual({unknown, aerpc_addr_index:to_addr20(?RECIPIENT)},
                 aerpc_account:decode_address(Addr)),
    ?assertEqual({ok, <<"0x">>}, aerpc_account:code(Addr)),
    publish_top(?MICRO_HASH, micro),
    ?assertEqual({ok, ?RECIPIENT}, aerpc_account:decode_address(Addr)).

key_block_sweeps_closed_generation() ->
    ?assertEqual(unknown, resolve(?BENE)),
    publish_top(?KEY_HASH, key),
    ?assertEqual({ok, ?BENE}, resolve(?BENE)),
    %% The sweep is the catch-up net for micro blocks whose own
    %% top_changed never arrived, which is what a reorg or a sync burst
    %% produces: the top advances in one event.
    ?assertEqual({ok, ?SENDER},    resolve(?SENDER)),
    ?assertEqual({ok, ?RECIPIENT}, resolve(?RECIPIENT)).

unknown_block_is_a_noop() ->
    Before = maps:get(indexed, aerpc_addr_index:status()),
    publish_top(<<99:32/unit:8>>, micro),
    ?assertEqual(Before, maps:get(indexed, aerpc_addr_index:status())).

%% ===================================================================
%% R2 -- the dry-run nonce
%% ===================================================================

r2_test_() ->
    {foreach,
     fun setup_call/0,
     fun teardown_call/1,
     [fun(_) -> {"the nonce is the on-chain account nonce + 1, never the "
                 "pool-aware one",
                 fun nonce_from_chain_state/0} end,
      fun(_) -> {"an account with no on-chain entry gets nonce 1",
                 fun nonce_for_absent_account/0} end,
      fun(_) -> {"no 'from' keeps the magic caller's fixed nonce",
                 fun nonce_for_magic_caller/0} end]}.

nonce_from_chain_state() ->
    %% Chain state says 447, the pool-aware source says 449. dry_run
    %% applies against chain state, so 448 is the only nonce it accepts.
    set_account_nonce(447),
    _ = estimate_with_from(?SENDER),
    ?assertEqual(448, captured_nonce()),
    %% And prove the pool-aware source really would have disagreed, so
    %% this case fails if anyone wires it back in.
    ?assertEqual({ok, 449}, aec_next_nonce:pick_for_account(?SENDER)).

nonce_for_absent_account() ->
    set_account_absent(),
    _ = estimate_with_from(?SENDER),
    ?assertEqual(1, captured_nonce()).

nonce_for_magic_caller() ->
    set_account_nonce(447),
    _ = aerpc_call:estimate_gas(#{<<"to">>    => hex(?CONTRACT),
                                  <<"input">> => <<"0x">>}, <<"latest">>),
    ?assertEqual(1, captured_nonce()).

%% ===================================================================
%% R1 fixtures
%% ===================================================================

setup_index() ->
    ok = meck:new(aec_chain, [passthrough, no_link]),
    ok = meck:expect(aec_chain, top_block_hash, fun() -> <<0:32/unit:8>> end),
    ok = meck:expect(aec_chain, get_block_state_partial,
                     fun(_H, _E) -> {ok, aec_trees:new_without_backend()} end),
    ok = meck:expect(aec_chain, get_header, fun header_for/1),
    ok = meck:expect(aec_chain, get_block, fun block_for/1),
    ok = meck:expect(aec_chain, get_generation_by_hash,
                     fun(?PREV_KEY, forward) ->
                             {ok, #{key_block    => key_block(),
                                    micro_blocks => [micro_block()]}};
                        (_Other, _Dir) ->
                             error
                     end),
    {ok, Pid} = aerpc_addr_index:start_link(),
    wait_for_backfill(complete, 100),
    Pid.

teardown_index(Pid) ->
    stop(Pid),
    ok = meck:unload(aec_chain).

%% Go in through the event: the handler clause is what regressed.
publish_top(Hash, Type) ->
    Pid = whereis(aerpc_addr_index),
    Pid ! {gproc_ps_event, top_changed,
           #{info => #{block_hash => Hash, block_type => Type}}},
    %% sys:get_state/1 is an ordinary message, so it lands behind the one
    %% above and returning proves that one has been handled.
    _ = sys:get_state(Pid),
    ok.

resolve(Pubkey) ->
    aerpc_addr_index:resolve(aerpc_addr_index:to_addr20(Pubkey)).

header_for(?MICRO_HASH) -> {ok, aec_blocks:to_header(micro_block())};
header_for(?KEY_HASH)   -> {ok, aec_blocks:to_header(key_block())};
header_for(_Other)      -> error.

block_for(?MICRO_HASH) -> {ok, micro_block()};
block_for(?KEY_HASH)   -> {ok, key_block()};
block_for(_Other)      -> error.

micro_block() ->
    aec_blocks:new_micro(?HEIGHT, ?PREV_KEY, ?PREV_KEY, <<0:32/unit:8>>,
                         <<0:32/unit:8>>, [spend_tx()], timestamp(), no_fraud,
                         protocol()).

key_block() ->
    aec_blocks:new_key(?HEIGHT, ?PREV_KEY, ?PREV_KEY, <<0:32/unit:8>>,
                       undefined, 0, timestamp(), default, protocol(),
                       <<0:32/unit:8>>, ?BENE).

protocol()  -> aec_hard_forks:protocol_effective_at_height(?HEIGHT).
timestamp() -> 1504731164584.

spend_tx() ->
    {ok, Aetx} = aec_spend_tx:new(
                   #{sender_id    => aeser_id:create(account, ?SENDER),
                     recipient_id => aeser_id:create(account, ?RECIPIENT),
                     amount       => 1, fee => 20000, nonce => 1,
                     payload      => <<>>}),
    aetx_sign:new(Aetx, []).

%% ===================================================================
%% R2 fixtures
%% ===================================================================

setup_call() ->
    ?CAPTURE = ets:new(?CAPTURE, [set, public, named_table]),
    ok = meck:new(aec_chain, [passthrough, no_link]),
    ok = meck:new(aec_next_nonce, [passthrough, no_link]),
    ok = meck:new(aec_dry_run, [passthrough, no_link]),
    ok = meck:new(aect_contracts, [passthrough, no_link]),
    %% A contract has to be found or do_dry_run/2 short-circuits to
    %% no_contract and never builds a tx at all.
    ok = meck:expect(aec_chain, get_contract, fun(_PK) -> {ok, a_contract} end),
    ok = meck:expect(aect_contracts, abi_version, fun(a_contract) -> 3 end),
    %% The pool-aware source, deliberately disagreeing with chain state.
    ok = meck:expect(aec_next_nonce, pick_for_account,
                     fun(_PK) -> {ok, 449} end),
    ok = meck:expect(aec_dry_run, dry_run,
                     fun(_Top, _Accounts, [{tx, Tx}], _Opts) ->
                         ets:insert(?CAPTURE, {nonce, aetx:nonce(Tx)}),
                         {error, captured}
                     end),
    ok.

teardown_call(_) ->
    ets:delete(?CAPTURE),
    ok = meck:unload(aec_dry_run),
    ok = meck:unload(aec_next_nonce),
    ok = meck:unload(aect_contracts),
    ok = meck:unload(aec_chain).

set_account_nonce(N) ->
    Account = aec_accounts:set_nonce(aec_accounts:new(?SENDER, 1000000), N),
    ok = meck:expect(aec_chain, get_account, fun(_PK) -> {value, Account} end).

set_account_absent() ->
    ok = meck:expect(aec_chain, get_account, fun(_PK) -> none end).

estimate_with_from(Pubkey) ->
    aerpc_call:estimate_gas(#{<<"to">>    => hex(?CONTRACT),
                              <<"from">>  => hex(Pubkey),
                              <<"input">> => <<"0x">>},
                            <<"latest">>).

captured_nonce() ->
    case ets:lookup(?CAPTURE, nonce) of
        [{nonce, N}] -> N;
        []           -> undefined
    end.

%% ===================================================================
%% Shared helpers
%% ===================================================================

%% 32-byte form, so these cases exercise the nonce and not the index.
hex(Pubkey) -> aerpc_encoding:to_hex_data(Pubkey).

stop(Pid) ->
    unlink(Pid),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive {'DOWN', MRef, process, Pid, _} -> ok
    after 5000 -> ok
    end.

wait_for_backfill(_Want, 0) ->
    ?assert(false);
wait_for_backfill(Want, N) ->
    case maps:get(backfill, aerpc_addr_index:status()) of
        Want   -> ok;
        _Other -> timer:sleep(20), wait_for_backfill(Want, N - 1)
    end.

-endif.
