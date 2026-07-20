#!/usr/bin/env escript
%%! -noshell

-mode(compile).

-define(TIMEOUT_MS, 10000).

main(Args) ->
    add_test_paths(),
    Scenario = parse_scenario(Args),
    try
        run(Scenario)
    catch
        Class:Reason:Stack ->
            io:format("SCENARIO=~p~n", [Scenario]),
            io:format("PROOF_STATUS=failed~n"),
            io:format("ERROR=~p:~p~nSTACK=~p~n", [Class, Reason, Stack]),
            halt(1)
    end.

parse_scenario([Arg]) ->
    list_to_existing_atom(Arg);
parse_scenario([]) ->
    initial_tx.

run(Scenario) ->
    ok = aec_test_utils:ensure_system_init(),
    ProofTab = ets:new(proof_state, [named_table, public]),
    true = ets:insert(ProofTab, {mining_calls, 0}),
    ok = meck:new(aeu_env, [passthrough]),
    ok = meck:new(aec_governance, [passthrough]),
    ok = meck:new(aeu_info, [passthrough]),
    ok = meck:new(aec_mining, [passthrough]),
    ok = meck:new(aec_headers, [passthrough]),
    ok = meck:new(aec_blocks, [passthrough]),
    ok = meck:new(aec_block_micro_candidate, [passthrough]),
    ok = meck:expect(aec_governance, expected_block_mine_rate,
                     fun() -> meck:passthrough([]) div 2560 end),
    ok = meck:expect(aeu_info, block_info, fun() -> 0 end),
    ok = meck:expect(aec_mining, generate,
                     fun(_HeaderBin, _Target, Nonce, _MinerConfig, _AddressedInstance) ->
                             MiningCall = ets:update_counter(proof_state, mining_calls, 1),
                             case MiningCall of
                                 1 ->
                                     {ok, {Nonce, []}};
                                 _ ->
                                     timer:sleep(5000),
                                     {error, no_solution}
                             end
                     end),
    ok = meck:expect(aec_mining, verify,
                     fun(_HeaderBin, _Nonce, _Seal, _Target) -> true end),
    ok = meck:expect(aec_headers, validate_key_block_header, fun(_, _) -> ok end),
    ok = meck:expect(aec_headers, validate_micro_block_header, fun(_, _) -> ok end),
    ok = meck:expect(aec_blocks, validate_key_block, fun(_, _) -> ok end),
    ok = meck:expect(aec_blocks, validate_micro_block, fun(_, _) -> ok end),
    setup_candidate_mock(Scenario),
    aec_test_utils:mock_fast_and_deterministic_cuckoo_pow(),
    try
        ok = application:ensure_started(gproc),
        ok = aec_test_utils:start_chain_db(),
        {ok, _} = aec_block_generator:start_link(),
        TmpKeysDir = aec_test_utils:aec_keys_setup(),
        try
            {ok, PubKey} = aec_keys:get_pubkey(),
            {ok, PrivKey} = aec_keys:sign_privkey(),
            Beneficiary = aeser_api_encoder:encode(account_pubkey, PubKey),
            ok = application:set_env(aecore, beneficiary, Beneficiary),
            PresetAccounts = [{PubKey, 50000 * aec_test_utils:min_gas_price()}],
            ok = aec_test_utils:mock_genesis_and_forks(PresetAccounts),
            ok = aec_test_utils:mock_time(),
            {ok, _} = aec_tx_pool_gc:start_link(),
            {ok, _} = aec_tx_pool:start_link(),
            try
                InitialTx = make_tx(PubKey, PrivKey),
                LateTx = make_tx(PubKey, PrivKey),
                true = aec_events:subscribe(start_mining),
                true = aec_events:subscribe(block_created),
                true = aec_events:subscribe(micro_block_created),
                true = aec_events:subscribe(candidate_block),
                {ok, _} = aec_conductor:start_link([{autostart, false}]),
                maybe_push_initial_tx(Scenario, InitialTx),
                log_pool_state(),
                ok = aec_conductor:start_mining(),
                State = observe_events(Scenario, LateTx),
                print_summary(Scenario, State),
                assert_fix_holds(Scenario, State)
            after
                catch aec_conductor:stop(),
                catch aec_tx_pool:stop(),
                catch aec_tx_pool_gc:stop(),
                catch aec_test_utils:unmock_time(),
                catch aec_test_utils:unmock_genesis_and_forks()
            end
        after
            catch application:unset_env(aecore, beneficiary),
            catch aec_test_utils:aec_keys_cleanup(TmpKeysDir),
            catch aec_block_generator:stop(),
            catch aec_test_utils:stop_chain_db(),
            catch application:stop(gproc)
        end
    after
        catch ets:delete(proof_state),
        catch meck:unload(aec_block_micro_candidate),
        catch meck:unload(aec_blocks),
        catch meck:unload(aec_headers),
        catch meck:unload(aec_mining),
        catch meck:unload(aeu_info),
        catch meck:unload(aec_governance),
        catch meck:unload(aeu_env)
    end.

setup_candidate_mock(empty_candidate) ->
    ok = meck:expect(aec_block_micro_candidate, create,
                     fun(BlockInfo) ->
                             timer:sleep(250),
                             empty_candidate_result(BlockInfo)
                     end);
setup_candidate_mock(_Scenario) ->
    ok = meck:expect(aec_block_micro_candidate, create,
                     fun(BlockInfo) ->
                             timer:sleep(250),
                             meck:passthrough([BlockInfo])
                     end).

add_test_paths() ->
    [code:add_patha(Path) || Path <- filelib:wildcard("_build/test/lib/*/ebin")],
    [code:add_patha(Path) || Path <- filelib:wildcard("_build/test/lib/*/test")],
    ok.

maybe_push_initial_tx(initial_tx, Tx) ->
    ok = aec_tx_pool:push(Tx);
maybe_push_initial_tx(empty_candidate, Tx) ->
    ok = aec_tx_pool:push(Tx);
maybe_push_initial_tx(late_tx, _Tx) ->
    ok.

log_pool_state() ->
    {ok, PendingTxs} = aec_tx_pool:peek(infinity),
    io:format("POOL_SIZE_AFTER_SETUP=~p~n", [aec_tx_pool:size()]),
    io:format("POOL_PEEK_AFTER_SETUP=~p~n", [length(PendingTxs)]).

make_tx(PubKey, PrivKey) ->
    #{public := RecipientPubKey} = enacl:sign_keypair(),
    {ok, Tx} = aec_spend_tx:new(#{sender_id => aeser_id:create(account, PubKey),
                                  recipient_id => aeser_id:create(account, RecipientPubKey),
                                  amount => 1,
                                  nonce => 1,
                                  fee => 20000 * aec_test_utils:min_gas_price(),
                                  ttl => 0,
                                  payload => <<>>}),
    aec_test_utils:sign_tx(Tx, PrivKey).

observe_events(Scenario, LateTx) ->
    Deadline = erlang:monotonic_time(millisecond) + ?TIMEOUT_MS,
    loop(Deadline, Scenario,
         #{late_tx => LateTx,
           late_tx_pushed => false,
           key_hash => undefined,
           micro_hash => undefined,
           candidate_seen => false,
           candidate_infos => [],
           empty_candidate_seen => false,
           restart_before_micro => false,
           restart_on_micro => false,
           restart_before_empty => false,
           restart_after_empty => false,
           raw_events => []}).

loop(Deadline, Scenario, State) ->
    Timeout = max(0, Deadline - erlang:monotonic_time(millisecond)),
    case done(Scenario, State) orelse Timeout =:= 0 of
        true ->
            State;
        false ->
            receive
                {gproc_ps_event, Event, #{info := Info}} ->
                    State1 = handle_event(Scenario, Event, Info, State),
                    loop(Deadline, Scenario, State1)
            after Timeout ->
                State
            end
    end.

done(_Scenario, #{restart_before_micro := true}) ->
    true;
done(initial_tx, #{key_hash := KeyHash, micro_hash := MicroHash, restart_on_micro := true})
  when KeyHash =/= undefined, MicroHash =/= undefined ->
    true;
done(empty_candidate, #{key_hash := KeyHash, empty_candidate_seen := true, restart_after_empty := true})
  when KeyHash =/= undefined ->
    true;
done(late_tx, #{key_hash := KeyHash, micro_hash := MicroHash, restart_on_micro := true})
  when KeyHash =/= undefined, MicroHash =/= undefined ->
    true;
done(_, _) ->
    false.

handle_event(Scenario, start_mining, Info, State0) ->
    TopHash = proplists:get_value(top_block_hash, Info),
    PoolSize = aec_tx_pool:size(),
    Raw = maps:get(raw_events, State0),
    State1 = State0#{raw_events => Raw ++ [{start_mining, TopHash, PoolSize}]},
    KeyHash = maps:get(key_hash, State1),
    MicroHash = maps:get(micro_hash, State1),
    EmptySeen = maps:get(empty_candidate_seen, State1),
    case {KeyHash, MicroHash, TopHash, EmptySeen, Scenario} of
        {undefined, _, _, _, _} ->
            State1;
        {_, undefined, KeyHash, false, empty_candidate} ->
            State1#{restart_before_empty => true,
                    restart_before_micro => true};
        {_, undefined, KeyHash, true, empty_candidate} ->
            State1#{restart_after_empty => true};
        {_, undefined, _, _, _} ->
            State1#{restart_before_micro => true};
        {_, MicroHash, MicroHash, _, _} ->
            State1#{restart_on_micro => true};
        _ ->
            State1
    end;
handle_event(Scenario, block_created, Block, State0) ->
    Hash = block_hash(Block),
    PoolSize = aec_tx_pool:size(),
    Raw = maps:get(raw_events, State0),
    State1 = State0#{key_hash => Hash,
                     raw_events => Raw ++ [{block_created, Hash, PoolSize, aec_conductor:is_leader()}]},
    maybe_push_late_tx(Scenario, State1);
handle_event(_Scenario, micro_block_created, Block, State) ->
    Hash = block_hash(Block),
    PoolSize = aec_tx_pool:size(),
    Raw = maps:get(raw_events, State),
    State#{micro_hash => Hash,
           raw_events => Raw ++ [{micro_block_created, Hash, PoolSize}]};
handle_event(_Scenario, candidate_block, Info, State) ->
    PoolSize = aec_tx_pool:size(),
    Raw = maps:get(raw_events, State),
    CandidateInfos = maps:get(candidate_infos, State),
    State#{candidate_seen => true,
           candidate_infos => CandidateInfos ++ [Info],
           empty_candidate_seen => maps:get(empty_candidate_seen, State) orelse Info =:= empty_candidate,
           raw_events => Raw ++ [{candidate_block, Info, PoolSize}]};
handle_event(_Scenario, _Event, _Info, State) ->
    State.

maybe_push_late_tx(late_tx, State = #{late_tx_pushed := false, late_tx := Tx, raw_events := Raw}) ->
    ok = aec_tx_pool:push(Tx),
    PoolSize = aec_tx_pool:size(),
    State#{late_tx_pushed => true,
           raw_events => Raw ++ [{late_tx_pushed, PoolSize}]};
maybe_push_late_tx(_Scenario, State) ->
    State.

empty_candidate_result(BlockInfo) ->
    Block = resolve_block(BlockInfo),
    Hash = block_hash(Block),
    PrevKeyBlock =
        case aec_blocks:is_key_block(Block) of
            true ->
                Block;
            false ->
                {ok, KeyBlock} = aec_chain:get_block(aec_blocks:prev_key_hash(Block)),
                KeyBlock
        end,
    {ok, Trees} = aec_chain:get_block_state(Hash),
    {EmptyBlock, _Trees1} = aec_block_micro_candidate:create_with_state(Block, PrevKeyBlock, [], Trees),
    {ok, EmptyBlock, undefined}.

resolve_block(BlockHash) when is_binary(BlockHash) ->
    {ok, Block} = aec_chain:get_block(BlockHash),
    Block;
resolve_block(Block) ->
    Block.

block_hash(Block) ->
    {ok, Hash} = aec_blocks:hash_internal_representation(Block),
    Hash.

print_summary(Scenario, State) ->
    io:format("SCENARIO=~p~n", [Scenario]),
    io:format("EVENT_TRACE=~p~n", [maps:get(raw_events, State)]),
    io:format("KEY_HASH=~p~n", [maps:get(key_hash, State)]),
    io:format("MICRO_HASH=~p~n", [maps:get(micro_hash, State)]),
    io:format("CANDIDATE_INFOS=~p~n", [maps:get(candidate_infos, State)]),
    io:format("LATE_TX_PUSHED=~p~n", [maps:get(late_tx_pushed, State)]),
    io:format("RESTART_BEFORE_MICRO=~p~n", [maps:get(restart_before_micro, State)]),
    io:format("RESTART_ON_MICRO=~p~n", [maps:get(restart_on_micro, State)]),
    io:format("RESTART_BEFORE_EMPTY=~p~n", [maps:get(restart_before_empty, State)]),
    io:format("RESTART_AFTER_EMPTY=~p~n", [maps:get(restart_after_empty, State)]).

assert_fix_holds(initial_tx,
                 #{key_hash := KeyHash,
                   micro_hash := MicroHash,
                   restart_before_micro := RestartBeforeMicro,
                   restart_on_micro := RestartOnMicro}) ->
    true = (KeyHash =/= undefined),
    true = (MicroHash =/= undefined),
    false = RestartBeforeMicro,
    true = RestartOnMicro,
    io:format("PROOF_STATUS=passed~n"),
    halt(0);
assert_fix_holds(empty_candidate,
                 #{key_hash := KeyHash,
                   candidate_infos := CandidateInfos,
                   restart_before_empty := RestartBeforeEmpty,
                   restart_after_empty := RestartAfterEmpty}) ->
    true = (KeyHash =/= undefined),
    true = lists:member(empty_candidate, CandidateInfos),
    false = RestartBeforeEmpty,
    true = RestartAfterEmpty,
    io:format("PROOF_STATUS=passed~n"),
    halt(0);
assert_fix_holds(late_tx,
                 #{late_tx_pushed := LateTxPushed,
                   key_hash := KeyHash,
                   micro_hash := MicroHash,
                   restart_before_micro := RestartBeforeMicro,
                   restart_on_micro := RestartOnMicro}) ->
    true = LateTxPushed,
    true = (KeyHash =/= undefined),
    true = (MicroHash =/= undefined),
    false = RestartBeforeMicro,
    true = RestartOnMicro,
    io:format("PROOF_STATUS=passed~n"),
    halt(0).
