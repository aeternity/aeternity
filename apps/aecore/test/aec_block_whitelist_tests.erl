%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the the emergency block whitelist
%%% @end
%%%=============================================================================
-module(aec_block_whitelist_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

setup_minimal() ->
    ok = application:ensure_started(gproc),
    ok = aec_test_utils:start_chain_db(),
    aec_block_generator:start_link(),

    meck:new(aec_governance, [passthrough]),
    meck:expect(aec_governance, expected_block_mine_rate,
                fun() ->
                        meck:passthrough([]) div 2560
                end),
    TmpKeysDir = aec_test_utils:aec_keys_setup(),
    {ok, PubKey} = aec_keys:pubkey(),
    ok = application:set_env(aecore, beneficiary, aeser_api_encoder:encode(account_pubkey, PubKey)),
    aec_test_utils:mock_genesis_and_forks(preset_accounts(PubKey), #{}),
    aec_test_utils:mock_time(),
    {ok, _} = aec_tx_pool_gc:start_link(),
    {ok, _} = aec_tx_pool:start_link(),
    TmpKeysDir.

teardown_minimal(TmpKeysDir) ->
    ok = application:unset_env(aecore, beneficiary),
    ok = aec_tx_pool:stop(),
    ok = aec_tx_pool_gc:stop(),
    aec_block_generator:stop(),
    ok = application:stop(gproc),
    _  = flush_gproc(),
    ?assert(meck:validate(aec_governance)),
    meck:unload(aec_governance),
    aec_test_utils:unmock_genesis_and_forks(),
    aec_test_utils:unmock_time(),
    ok = aec_test_utils:stop_chain_db(),
    aec_test_utils:aec_keys_cleanup(TmpKeysDir),
    ok.

setup_cuckoo_pow() ->
    aec_test_utils:mock_fast_cuckoo_pow().

teardown_cuckoo_pow(_) ->
    ok.

setup_common() ->
    setup_cuckoo_pow(),
    _TmpKeysDir = setup_minimal().

teardown_common(TmpKeysDir) ->
    ok = aec_conductor:stop(),
    teardown_minimal(TmpKeysDir),
    teardown_cuckoo_pow(unused_argument),
    ok.

%%%===================================================================
%%% Tests for mining infrastructure
%%%===================================================================


chain_test_() ->
    {foreach,
     fun() ->
             TmpKeysDir = setup_common(),
             {ok, _} = aec_conductor:start_link([{autostart, false}]),
             meck:new(aec_headers, [passthrough]),
             meck:new(aec_blocks, [passthrough]),
             meck:expect(aec_headers, validate_key_block_header, fun(_, _) -> ok end),
             meck:expect(aec_headers, validate_micro_block_header, fun(_, _) -> ok end),
             meck:expect(aec_blocks, validate_key_block, fun(_, _) -> ok end),
             meck:expect(aec_blocks, validate_micro_block, fun(_, _) -> ok end),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             teardown_common(TmpKeysDir),
             meck:unload(aec_headers),
             meck:unload(aec_blocks),
             ok
     end,
     [
      {"Start mining add a block.", fun test_start_mining_add_block/0}
     ]}.

test_start_mining_add_block() ->
    Keys = beneficiary_keys(),
    %% Assert preconditions
    assert_stopped_and_genesis_at_top(),

    aec_conductor:start_mining(),
    [_GB, B1, B2] = aec_test_utils:gen_blocks_only_chain(3, preset_accounts(Keys)),
    BH2 = aec_blocks:to_header(B2),
    meck:expect(aec_fork_block_settings, block_whitelist, 0, #{2 => <<"AAA">>}),
    aec_consensus_bitcoin_ng:start(#{}),
    ?assertEqual(ok, aec_conductor:post_block(B1)),
    %% Does not go pass conductor checks
    ?assertEqual({error,blocked_by_whitelist}, aec_conductor:post_block(B2)),
    %% Does not go pass chain checks
    ?assertEqual({error,blocked_by_whitelist}, aec_chain_state:insert_block(B2)),

    %% Whitelisted blocks make it to the DB
    meck:expect(aec_fork_block_settings, block_whitelist, 0, #{2 => header_hash(BH2)}),
    aec_consensus_bitcoin_ng:start(#{}),
    ?assertEqual(ok, aec_conductor:post_block(B2)),

    aec_test_utils:wait_for_it(
      fun () -> aec_chain:top_header() end,
      BH2).

assert_stopped_and_genesis_at_top() ->
    assert_stopped(),
    Keys = beneficiary_keys(),
    Preset = preset_accounts(Keys),
    {Genesis, _} = aec_test_utils:genesis_block_with_state(Preset),
    ?assertEqual(aec_chain:top_block_hash(),
                 header_hash( aec_blocks:to_header( Genesis ))).

beneficiary_keys() ->
    {ok, Pub} = aec_keys:pubkey(),
    {ok, Priv} = aec_keys:sign_privkey(),
    {Pub, Priv}.

preset_accounts({Pub, _}) -> preset_accounts(Pub);
preset_accounts(Pub) -> [{Pub, 50000 * aec_test_utils:min_gas_price()}].

flush_gproc() ->
    flush_gproc([]).

flush_gproc(Acc) ->
    receive
        {gproc_ps_event, _, _} = E -> flush_gproc([E|Acc])
    after 0 -> Acc
    end.

assert_stopped() ->
    ?assertEqual(stopped, aec_conductor:get_mining_state()).

header_hash(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    Hash.

-endif.
