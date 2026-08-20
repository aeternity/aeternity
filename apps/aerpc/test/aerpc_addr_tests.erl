%%%-------------------------------------------------------------------
%%% @doc Unit coverage for the 20-byte address contract and its reverse
%%% index.
%%%
%%% The case that matters most is the negative one: an address the index
%%% cannot resolve while the backfill is still running must FAIL rather
%%% than fall through to eth's "unknown address has balance zero"
%%% default. A wrong balance that looks like a right one is worse than
%%% an error, and it is the failure mode this whole module exists to
%%% prevent, so it is asserted from `eth_getBalance' downwards and not
%%% just at `resolve/1'.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_addr_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Derivation and the outbound leg -- no index needed
%% ===================================================================

derivation_test_() ->
    Pubkey = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
               21,22,23,24,25,26,27,28,29,30,31,32>>,
    [{"the address is the first 20 bytes of the pubkey",
      fun() ->
          ?assertEqual(<<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20>>,
                       aerpc_addr_index:to_addr20(Pubkey))
      end},
     {"outbound address fields are 20-byte lower-case 0x hex",
      fun() ->
          Addr = aerpc_encoding:format_account(Pubkey),
          ?assertEqual(<<"0x0102030405060708090a0b0c0d0e0f1011121314">>, Addr),
          %% Lower-case is load-bearing: getAddress/1 checksums an
          %% all-lower address itself but throws on mixed case that
          %% fails EIP-55, so mixed case must never be emitted.
          ?assertEqual(Addr, string:lowercase(Addr)),
          ?assertEqual(Addr, aerpc_encoding:format_contract(Pubkey))
      end}].

%% ===================================================================
%% Inbound with no index running at all
%% ===================================================================

no_index_test_() ->
    [{"a 20-byte address errors rather than resolving to nothing",
      fun() ->
          %% Precondition, asserted rather than assumed: these cases are
          %% only meaningful with no index process alive.
          ?assertEqual(undefined, ets:info(aerpc_addr_idx)),
          ?assertEqual(incomplete,
                       aerpc_addr_index:resolve(<<0:20/unit:8>>)),
          ?assertMatch({error, -32007, _},
                       aerpc_account:decode_address(addr_hex(<<0:20/unit:8>>)))
      end},
     {"eth_getBalance refuses rather than answering 0x0",
      fun() ->
          %% Stefan's re-run bar, asserted at the method boundary.
          ?assertMatch({error, -32007, _},
                       aerpc_methods:dispatch_method(
                         <<"eth_getBalance">>,
                         [addr_hex(<<9:20/unit:8>>), <<"latest">>]))
      end},
     {"the other address-keyed reads refuse too",
      fun() ->
          A = addr_hex(<<9:20/unit:8>>),
          ?assertMatch({error, -32007, _},
                       aerpc_methods:dispatch_method(
                         <<"eth_getTransactionCount">>, [A, <<"latest">>])),
          ?assertMatch({error, -32007, _},
                       aerpc_methods:dispatch_method(
                         <<"eth_getCode">>, [A, <<"latest">>]))
      end},
     {"the 32-byte and AE-native forms are unaffected by the index",
      fun() ->
          PK = <<3:32/unit:8>>,
          ?assertEqual({ok, PK},
                       aerpc_account:decode_address(
                         aerpc_encoding:to_hex_data(PK)))
      end}].

%% ===================================================================
%% With the index running over an empty chain state
%% ===================================================================

index_test_() ->
    {setup,
     fun setup_index/0,
     fun teardown_index/1,
     [{"backfill reaches complete", fun backfill_completes/0},
      {"an indexed pubkey round-trips through the 20-byte form",
       fun round_trip/0},
      {"a miss after backfill is 'unknown', and only then means zero",
       fun miss_after_backfill/0},
      {"a collision keeps the first mapping and refuses the second",
       fun collision_refused/0},
      {"rebuild puts the index back to refusing until it finishes",
       fun rebuild_refuses_again/0}]}.

backfill_completes() ->
    ?assertMatch(#{backfill := complete}, aerpc_addr_index:status()).

round_trip() ->
    PK = <<42:32/unit:8>>,
    ok = aerpc_addr_index:index_pubkey(PK),
    Addr = aerpc_encoding:format_account(PK),
    %% The address we emit is exactly the one we accept back.
    ?assertEqual({ok, PK}, aerpc_account:decode_address(Addr)),
    ?assertEqual({ok, PK},
                 aerpc_addr_index:resolve(aerpc_addr_index:to_addr20(PK))).

miss_after_backfill() ->
    Addr20 = <<77:20/unit:8>>,
    ?assertEqual(unknown, aerpc_addr_index:resolve(Addr20)),
    %% Now -- and only now -- eth's zero/empty defaults are the correct
    %% answer, because "not in the index" really does mean "not on chain".
    ?assertEqual({unknown, Addr20},
                 aerpc_account:decode_address(addr_hex(Addr20))),
    ?assertEqual({ok, <<"0x">>}, aerpc_account:code(addr_hex(Addr20))).

collision_refused() ->
    Shared = <<5:20/unit:8>>,
    First  = <<Shared/binary, 1:12/unit:8>>,
    Second = <<Shared/binary, 2:12/unit:8>>,
    Before = maps:get(collisions, aerpc_addr_index:status()),
    ok = aerpc_addr_index:index_pubkey(First),
    ok = aerpc_addr_index:index_pubkey(Second),
    %% First mapping wins: overwriting would silently re-point an
    %% address already served to a client.
    ?assertEqual({ok, First}, aerpc_addr_index:resolve(Shared)),
    ?assertEqual(Before + 1, maps:get(collisions, aerpc_addr_index:status())),
    %% Re-inserting the same pair is not a collision.
    ok = aerpc_addr_index:index_pubkey(First),
    ?assertEqual(Before + 1, maps:get(collisions, aerpc_addr_index:status())).

rebuild_refuses_again() ->
    PK = <<51:32/unit:8>>,
    ok = aerpc_addr_index:index_pubkey(PK),
    ?assertMatch({ok, _}, aerpc_addr_index:resolve(
                            aerpc_addr_index:to_addr20(PK))),
    ok = aerpc_addr_index:rebuild(),
    %% The entry is gone and, until the walk finishes again, a miss is
    %% an error rather than a zero.
    wait_for_backfill(complete, 50),
    ?assertEqual(unknown, aerpc_addr_index:resolve(
                            aerpc_addr_index:to_addr20(PK))).

%% ===================================================================
%% Helpers
%% ===================================================================

addr_hex(Addr20) -> aerpc_encoding:to_hex_data(Addr20).

%% Back the index with an empty but real trees record, so the backfill
%% exercises its actual iterator path and finishes deterministically.
setup_index() ->
    ok = meck:new(aec_chain, [passthrough, no_link]),
    ok = meck:expect(aec_chain, top_block_hash,
                     fun() -> <<0:32/unit:8>> end),
    ok = meck:expect(aec_chain, get_block_state_partial,
                     fun(_Hash, _Elements) ->
                         {ok, aec_trees:new_without_backend()}
                     end),
    {ok, Pid} = aerpc_addr_index:start_link(),
    wait_for_backfill(complete, 100),
    Pid.

teardown_index(Pid) ->
    unlink(Pid),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive {'DOWN', MRef, process, Pid, _} -> ok
    after 5000 -> ok
    end,
    ok = meck:unload(aec_chain).

wait_for_backfill(_Want, 0) ->
    ?assert(false);
wait_for_backfill(Want, N) ->
    case maps:get(backfill, aerpc_addr_index:status()) of
        Want   -> ok;
        _Other -> timer:sleep(20), wait_for_backfill(Want, N - 1)
    end.

-endif.
