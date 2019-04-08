%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_miner
%%% @end
%%%=============================================================================
-module(aec_conductor_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

-define(TEST_MODULE, aec_conductor).

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
    aec_test_utils:mock_genesis_and_forks(preset_accounts(PubKey)),
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
    ok = meck:new(aeu_env, [passthrough]),
    aec_test_utils:mock_fast_cuckoo_pow().

teardown_cuckoo_pow(_) ->
    ok = meck:unload(aeu_env).

setup_common() ->
    setup_cuckoo_pow(),
    _TmpKeysDir = setup_minimal().

teardown_common(TmpKeysDir) ->
    ok = ?TEST_MODULE:stop(),
    teardown_minimal(TmpKeysDir),
    teardown_cuckoo_pow(unused_argument),
    ok.

%%%===================================================================
%%% Tests for mining infrastructure
%%%===================================================================

miner_test_() ->
    {foreach,
     fun() ->
             TmpKeysDir = setup_common(),
             {ok, _} = ?TEST_MODULE:start_link([{autostart, true}]),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             teardown_common(TmpKeysDir)
     end,
     [{"Stop and restart miner", fun test_stop_restart/0},
      {"Test consecutive start/stop ", fun test_stop_restart_seq/0},
      {"Test block generator state after stop", fun test_block_generator_state_after_stop/0}
     ]}.

test_stop_restart() ->
    ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
    wait_for_stopped(),
    ?assertEqual(ok, ?TEST_MODULE:start_mining()),
    wait_for_running(),
    ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
    wait_for_stopped(),
    ?assertEqual(ok, ?TEST_MODULE:start_mining()),
    wait_for_running(),
    ok.

test_stop_restart_seq() ->
    ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
    wait_for_stopped(),
    ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
    wait_for_stopped(),
    ?assertEqual(ok, ?TEST_MODULE:start_mining()),
    wait_for_running(),
    ?assertEqual(ok, ?TEST_MODULE:start_mining()),
    wait_for_running(),
    ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
    wait_for_stopped(),
    ok.

test_block_generator_state_after_stop() ->
    ?assertEqual(running, ?TEST_MODULE:get_mining_state()),
    ?assertEqual(stopped, aec_block_generator:get_generation_state()),

    aec_events:subscribe(block_created),
    ?assertEqual(ok, ?TEST_MODULE:start_mining()),
    wait_for_block_created(),
    ?assertEqual(running, ?TEST_MODULE:get_mining_state()),
    ?assertEqual(running, aec_block_generator:get_generation_state()),

    ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
    wait_for_stopped(),
    ?assertEqual(stopped, ?TEST_MODULE:get_mining_state()),
    ?assertEqual(stopped, aec_block_generator:get_generation_state()),
    ok.

miner_no_beneficiary_test_() ->
    {foreach,
     fun() ->
             TmpKeysDir = setup_common(),
             ok = application:unset_env(aecore, beneficiary),
             {ok, _} = ?TEST_MODULE:start_link([]),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             teardown_common(TmpKeysDir)
     end,
     [{"Test start/stop miner", fun test_start_stop_no_beneficiary/0}]}.

test_start_stop_no_beneficiary() ->
    %% Assert mining is stopped by default.
    ?assertEqual(stopped, ?TEST_MODULE:get_mining_state()),
    ?assertEqual(ok, ?TEST_MODULE:stop_mining()),

    %% Assert mining cannot be started when beneficiary is not set.
    ?assertEqual({error, beneficiary_not_configured}, ?TEST_MODULE:start_mining()),
    ?assertEqual(stopped, ?TEST_MODULE:get_mining_state()),
    ok.

miner_timeout_test_() ->
    {foreach,
     fun() ->
             ok = meck:new(aec_mining, [passthrough]),
             ok = meck:new(aeu_env, [passthrough]),
             ok = meck:expect(aeu_env, get_env, 3,
                              fun
                                  (aecore, mining_attempt_timeout, _) ->
                                      500;
                                  (App, Key, Def) ->
                                      meck:passthrough([App, Key, Def])
                              end),
             TmpKeysDir = setup_minimal(),
             {ok, _} = ?TEST_MODULE:start_link([{autostart, false}]),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = ?TEST_MODULE:stop(),
             teardown_minimal(TmpKeysDir),
             ok = meck:unload(aeu_env),
             ok = meck:unload(aec_mining)
     end,
     [{"Time out miner that does not return", fun test_time_out_miner/0}
     ]}.

test_time_out_miner() ->
    TestPid = self(),
    ok = meck:expect(
           aec_mining, generate,
           fun(_, _, _, _, _) ->
                   TestPid ! {self(), called},
                   receive after infinity -> never_reached end
           end),

    %% Assert preconditions
    assert_stopped(),
    ok = receive {_, called} -> never_reached after 0 -> ok end,

    ?TEST_MODULE:start_mining(),

    %% First mining worker spawned hangs.
    receive {_, called} -> ok end,
    %% Retrieve mining worker pid before aec_conductor timer stops it.
    %% TODO Make retrieval of mining worker pid not time-dependent
    %% hence making test less fragile and removing need for large
    %% timeout that slows test down.
    [PowPid] = ?TEST_MODULE:get_mining_workers(),
    PowMonRef = monitor(process, PowPid),
    PowExitReason =
        receive {'DOWN', PowMonRef, process, _, Info} -> Info end,
    ?assertEqual(shutdown, PowExitReason),

    %% A second distinct mining worker is spawned.
    receive {_, called} -> ok end,

    ok.

%%%===================================================================
%%% Chain tests
%%%===================================================================

chain_test_() ->
    {foreach,
     fun() ->
             TmpKeysDir = setup_common(),
             {ok, _} = ?TEST_MODULE:start_link([{autostart, false}]),
             meck:new(aec_headers, [passthrough]),
             meck:new(aec_blocks, [passthrough]),
             meck:expect(aec_headers, validate_key_block_header, fun(_) -> ok end),
             meck:expect(aec_headers, validate_micro_block_header, fun(_) -> ok end),
             meck:expect(aec_blocks, validate_key_block, fun(_) -> ok end),
             meck:expect(aec_blocks, validate_micro_block, fun(_) -> ok end),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             teardown_common(TmpKeysDir),
             meck:unload(aec_headers),
             meck:unload(aec_blocks),
             ok
     end,
     [
      {"Start mining add a block.", fun test_start_mining_add_block/0},
      {"Test preemption of mining by block pushed by a network peer", fun test_preemption_pushed/0},
      {"Test preemption of mining by block pulled from a network peer", fun test_preemption_pulled/0},
      {"Test chain genesis state" , fun test_chain_genesis_state/0},
      {timeout, 20, {"Test block publishing", fun test_block_publishing/0}}
     ]}.

test_start_mining_add_block() ->
    Keys = beneficiary_keys(),
    %% Assert preconditions
    assert_stopped_and_genesis_at_top(),

    ?TEST_MODULE:start_mining(),
    [_GB, B1, B2] = aec_test_utils:gen_blocks_only_chain(3, preset_accounts(Keys)),
    BH2 = aec_blocks:to_header(B2),
    ?assertEqual(ok, ?TEST_MODULE:post_block(B1)),
    ?assertEqual(ok, ?TEST_MODULE:post_block(B2)),
    aec_test_utils:wait_for_it(
      fun () -> aec_chain:top_header() end,
      BH2).

test_preemption_pushed() ->
    Keys = beneficiary_keys(),
    %% Assert preconditions
    assert_stopped_and_genesis_at_top(),

    %% Generate a chain
    Chain = aec_test_utils:gen_blocks_only_chain(7, preset_accounts(Keys)),
    {Chain1, Chain2} = lists:split(3, Chain),
    Top1 = lists:last(Chain1),
    Top2 = lists:last(Chain2),
    Hash1 = block_hash(Top1),
    Hash2 = block_hash(Top2),

    %% Seed the server with the first part of the chain
    [ok = ?TEST_MODULE:post_block(B) || B <- Chain1],
    wait_for_top_block_hash(Hash1),

    %% Start mining and make sure we are starting
    %% from the correct hash.
    true = aec_events:subscribe(start_mining),
    aec_conductor:start_mining(),

    wait_for_start_mining(Hash1),

    %% Post the rest of the chain, which will take over.
    [?TEST_MODULE:post_block(B) || B <- Chain2],
    wait_for_top_block_hash(Hash2),

    %% The mining should now have been preempted
    %% and started over with the new top block hash
    wait_for_start_mining(Hash2),

    %% TODO: check the transaction pool
    ok.

test_preemption_pulled() ->
    Keys = beneficiary_keys(),
    %% Assert preconditions
    assert_stopped_and_genesis_at_top(),

    %% Generate a chain
    Chain = aec_test_utils:gen_blocks_only_chain(7, preset_accounts(Keys)),
    {Chain1, Chain2} = lists:split(3, Chain),
    Top1 = lists:last(Chain1),
    Top2 = lists:last(Chain2),
    Hash1 = block_hash(Top1),
    Hash2 = block_hash(Top2),

    %% Seed the server with the first part of the chain
    [ok = ?TEST_MODULE:post_block(B) || B <- Chain1],
    wait_for_top_block_hash(Hash1),

    %% Start mining and make sure we are starting
    %% from the correct hash.
    true = aec_events:subscribe(start_mining),
    aec_conductor:start_mining(),

    wait_for_start_mining(Hash1),

    %% Sync the rest of the chain, which will take over.
    [?TEST_MODULE:post_block(B) || B <- Chain2],
    wait_for_top_block_hash(Hash2),

    %% The mining should now have been preempted
    %% and started over with the new top block hash
    wait_for_start_mining(Hash2),

    ok.

-define(error_atom, {error, A} when is_atom(A)).

test_chain_genesis_state() ->
    Keys = beneficiary_keys(),
    %% Assert preconditions
    assert_stopped_and_genesis_at_top(),

    {GB, GBS} = aec_test_utils:genesis_block_with_state(preset_accounts(Keys)),
    GH = aec_blocks:to_header(GB),
    GHH = header_hash(GH),

    %% Check genesis block in chain, including state
    ?assertEqual(GHH, aec_chain:genesis_hash()),
    ?assertEqual(GH, aec_chain:genesis_header()),
    ?assertEqual(GB, aec_chain:genesis_block()),

    {ok, GBS1} = aec_chain:get_block_state(GHH),
    ?assertEqual(aec_trees:hash(GBS1), aec_trees:hash(GBS)),

    %% Check that genesis is top
    ?assertEqual(GHH, aec_chain:top_block_hash()),

    %% Check chain state functions
    GenesisAccountsBalances = aec_test_utils:genesis_accounts_balances(preset_accounts(Keys)),
    ?assertEqual({ok, GenesisAccountsBalances},
                 aec_chain:all_accounts_balances_at_hash(GHH)),
    [{PK, Balance} | _] = GenesisAccountsBalances,
    GenAccount = aec_accounts:new(PK, Balance),
    ?assertMatch({value, GenAccount},
                 aec_chain:get_account(PK)),
    ?assertEqual(none, aec_chain:get_account(<<"I am a fake public key">>)),
    ok.

test_block_publishing() ->
    Keys = beneficiary_keys(),
    %% Assert preconditions
    assert_stopped_and_genesis_at_top(),

    %% Generate a chain
    [_B0, B1, B2, B3, B4, B5] = Chain = aec_test_utils:gen_blocks_only_chain(6, preset_accounts(Keys)),
    [_H0, H1, H2, H3, H4, H5] = [block_hash(B) || B <- Chain],

    aec_events:subscribe(block_to_publish),
    aec_events:subscribe(top_changed),
    aec_events:subscribe(block_created),

    %% Seed the server with the first part of the chain
    ok = ?TEST_MODULE:post_block(B1),
    wait_for_top_block_hash(H1),
    expect_publish_event_hashes([H1]),
    expect_top_event_hashes([H1]),
    ok = ?TEST_MODULE:post_block(B2),
    wait_for_top_block_hash(H2),
    expect_publish_event_hashes([H2]),
    expect_top_event_hashes([H2]),

    %% Make sure there are no other messages waiting for us
    ?assertEqual([], flush_gproc()),

    %% Start mining and wait for two blocks.
    aec_conductor:start_mining(),
    MinedH1 = block_hash(wait_for_block_created()),
    MinedH2 = block_hash(wait_for_block_created()),
    aec_conductor:stop_mining(),
    expect_publish_event_hashes([MinedH1, MinedH2]),
    expect_top_event_hashes([MinedH1, MinedH2]),

    %% We should not have other events
    ?assertEqual([], flush_gproc()),

    %% Post the rest of the chain, which will take over eventually
    [?TEST_MODULE:post_block(B) || B <- [B2, B3, B4, B5]],
    wait_for_top_block_hash(H5),

    %% The first block cannot have taken over, so it should have no event
    %% We should have block_to_publish and top_changed events for at least the last block.
    expect_publish_event_hashes([H3, H4, H5], [H3, H4]),
    expect_top_event_hashes([H3, H4, H5], [H3, H4]),

    %% And no other events should have been emitted.
    ?assertEqual([], flush_gproc()),
    ok.

%%%===================================================================
%%% Micro block signing tests
%%%===================================================================

generation_test_() ->
    {foreach,
     fun() ->
             TmpKeysDir = setup_common(),
             {ok, _} = ?TEST_MODULE:start_link([{autostart, false}]),
             meck:new(aec_headers, [passthrough]),
             meck:new(aec_blocks, [passthrough]),
             meck:new(aec_mining, [passthrough]),
             meck:expect(aec_headers, validate_key_block_header, fun(_) -> ok end),
             meck:expect(aec_headers, validate_micro_block_header, fun(_) -> ok end),
             meck:expect(aec_blocks, validate_key_block, fun(_) -> ok end),
             meck:expect(aec_blocks, validate_micro_block, fun(_) -> ok end),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             teardown_common(TmpKeysDir),
             meck:unload(aec_mining),
             meck:unload(aec_blocks),
             meck:unload(aec_headers),
             ok
     end,
     [
        {timeout, 10, {"Start signing after mined block", fun test_mined_block_signing/0}},
        {timeout, 10, {"Start signing after two mined block", fun test_two_mined_block_signing/0}},
        {timeout, 10, {"Start signing after received block", fun test_received_block_signing/0}}
     ]}.

test_mined_block_signing() ->
    Keys = beneficiary_keys(),
    true = aec_events:subscribe(block_created),
    true = aec_events:subscribe(micro_block_created),

    ?TEST_MODULE:start_mining(),
    assert_leader(false),
    assert_generation_state(stopped),
    KeyBlock = wait_for_block_created(),
    assert_leader(true),
    assert_generation_state(running),
    wait_for_top_block_hash(block_hash(KeyBlock)),

    ok = aec_tx_pool:push(tx(Keys)),
    MicroBlock = wait_for_micro_block_created(),
    wait_for_top_block_hash(block_hash(MicroBlock)),

    ok = prev_on_chain(MicroBlock, KeyBlock),
    ok.

test_two_mined_block_signing() ->
    Keys = beneficiary_keys(),
    true = aec_events:subscribe(block_created),
    true = aec_events:subscribe(micro_block_created),

    ?TEST_MODULE:start_mining(),
    assert_leader(false),
    assert_generation_state(stopped),
    KeyBlock1 = wait_for_block_created(),
    assert_leader(true),
    assert_generation_state(running),
    wait_for_top_block_hash(block_hash(KeyBlock1)),
    KeyBlock2 = wait_for_block_created(),
    assert_leader(true),
    assert_generation_state(running),
    wait_for_top_block_hash(block_hash(KeyBlock2)),

    ok = aec_tx_pool:push(tx(Keys)),
    MicroBlock = wait_for_micro_block_created(),
    wait_for_top_block_hash(block_hash(MicroBlock)),

    ok = prev_on_chain(MicroBlock, KeyBlock2),
    ok = prev_on_chain(KeyBlock2, KeyBlock1),
    ok.

test_received_block_signing() ->
    Keys = beneficiary_keys(),
    meck:expect(aec_mining, generate,
                fun(_, _, _, _, _) -> timer:sleep(1000), {error, no_solution} end),
    true = aec_events:subscribe(block_to_publish),

    ?TEST_MODULE:start_mining(),
    assert_leader(false),
    assert_generation_state(stopped),

    [_GB, KB1] = aec_test_utils:gen_blocks_only_chain(2, preset_accounts(Keys)),
    ?assertEqual(ok, ?TEST_MODULE:post_block(KB1)),

    {_, KB1} = wait_for_block_to_publish(),
    assert_leader(true),
    assert_generation_state(running),
    wait_for_top_block_hash(block_hash(KB1)),
    ?assertEqual(true, aec_blocks:is_key_block(KB1)),

    %% Single tx should trigger micro block
    ok = aec_tx_pool:push(tx(Keys)),

    {_, NewBlock} = wait_for_block_to_publish(),
    wait_for_top_block_hash(block_hash(NewBlock)),
    ?assertEqual(false, aec_blocks:is_key_block(NewBlock)),

    ok = prev_on_chain(NewBlock, KB1),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

beneficiary_keys() ->
    {ok, Pub} = aec_keys:pubkey(),
    {ok, Priv} = aec_keys:sign_privkey(),
    {Pub, Priv}.

preset_accounts({Pub, _}) -> preset_accounts(Pub);
preset_accounts(Pub) -> [{Pub, 50000 * aec_test_utils:min_gas_price()}].

tx({Pub, Priv}) ->
    #{ public := RPub } = enacl:sign_keypair(),
    {ok, Tx} = aec_spend_tx:new(#{sender_id => aeser_id:create(account, Pub),
                                  recipient_id => aeser_id:create(account, RPub),
                                  amount => 1,
                                  nonce => 1,
                                  fee => 20000 * aec_test_utils:min_gas_price(),
                                  ttl => 0,
                                  payload => <<"">>}),
    aec_test_utils:sign_tx(Tx, Priv).

prev_on_chain(Block, Target) ->
    prev_on_chain(Block, block_hash(Block), block_hash(Target)).

prev_on_chain(_, Hash, Hash) -> ok;
prev_on_chain(Block, _, Target) ->
    NewHash = aec_blocks:prev_hash(Block),
    case aec_chain:get_block(NewHash) of
        error -> {error, not_found};
        {ok, NewBlock} -> prev_on_chain(NewBlock, block_hash(Block), Target)
    end.


assert_stopped() ->
    ?assertEqual(stopped, ?TEST_MODULE:get_mining_state()).

assert_stopped_and_genesis_at_top() ->
    assert_stopped(),
    Keys = beneficiary_keys(),
    Preset = preset_accounts(Keys),
    {Genesis, _} = aec_test_utils:genesis_block_with_state(Preset),
    ?assertEqual(aec_chain:top_block_hash(),
                 header_hash( aec_blocks:to_header( Genesis ))).

assert_leader(Value) ->
    ?assertEqual(Value, ?TEST_MODULE:is_leader()).

assert_generation_state(Value) ->
    ?assertEqual(Value, aec_block_generator:get_generation_state()).

block_hash(Block) ->
    {ok, Hash} = aec_blocks:hash_internal_representation(Block),
    Hash.

header_hash(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    Hash.

wait_for_stopped() ->
    aec_test_utils:wait_for_it(fun ?TEST_MODULE:get_mining_state/0, stopped).

wait_for_running() ->
    aec_test_utils:wait_for_it(fun ?TEST_MODULE:get_mining_state/0, running).

expect_publish_event_hashes(Expected) ->
    expect_publish_event_hashes(Expected, []).

expect_publish_event_hashes([],_AllowMissing) ->
    ok;
expect_publish_event_hashes(Expected, AllowMissing) ->
    receive
        {gproc_ps_event, block_to_publish, #{info := Info}} ->
            {Type, Block} = Info,
            ?assertEqual(true, lists:member(Type, [received, created])),
            Hash = block_hash(Block),
            NewExpected = Expected -- [Hash],
            case lists:member(Hash, Expected) of
                true  -> expect_publish_event_hashes(NewExpected, AllowMissing);
                false -> error({unexpected, Hash})
            end
    after 1000 ->
        case Expected -- AllowMissing of
            [] -> ok;
            Other -> error({missing, Other})
        end
    end.


expect_top_event_hashes(Expected) ->
    expect_top_event_hashes(Expected, []).

expect_top_event_hashes([],_AllowMissing) ->
    ok;
expect_top_event_hashes(Expected, AllowMissing) ->
    receive
        {gproc_ps_event, top_changed, #{info := #{ block_hash := Hash
                                                 , block_type := _
                                                 , prev_hash  := _
                                                 , height     := _ }}} ->
            NewExpected = Expected -- [Hash],
            case lists:member(Hash, Expected) of
                true  -> expect_top_event_hashes(NewExpected, AllowMissing);
                false -> error({unexpected, Hash})
            end
    after 1000 ->
            case Expected -- AllowMissing of
                [] -> ok;
                Other -> error({missing, Other})
            end
    end.

wait_for_top_block_hash(Hash) ->
    aec_test_utils:wait_for_it(
      fun () -> aec_chain:top_block_hash() end,
      Hash).

wait_for_block_to_publish() ->
    wait_for_gproc(block_to_publish, 30000).

wait_for_block_created() ->
    wait_for_gproc(block_created, 30000).

wait_for_micro_block_created() ->
    wait_for_gproc(micro_block_created, 30000).

wait_for_start_mining() ->
    wait_for_gproc(start_mining, 1000).

wait_for_start_mining(Hash) ->
    Info = wait_for_start_mining(),
    case proplists:get_value(top_block_hash, Info) of
        Hash -> ok;
        _Other -> wait_for_start_mining(Hash)
    end.

wait_for_gproc(Event, Timeout) ->
    receive
        {gproc_ps_event, Event, #{info := Info}} -> Info
    after Timeout -> error({timeout, Event})
    end.

flush_gproc() ->
    flush_gproc([]).

flush_gproc(Acc) ->
    receive
        {gproc_ps_event, _, _} = E -> flush_gproc([E|Acc])
    after 0 -> Acc
    end.

-endif.
