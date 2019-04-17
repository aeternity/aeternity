%%%=============================================================================
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%     Tests for aec_tx_gc objects Garbage Collection.
%%% @end
%%%=============================================================================

-module(aec_tx_gc_tests).

-include_lib("eunit/include/eunit.hrl").

tx_gc_test_() ->
    {foreach,
     fun() ->
             ok = application:ensure_started(gproc),
             TmpKeysDir = aec_test_utils:aec_keys_setup(),
             aec_test_utils:start_chain_db(),
             aec_test_utils:mock_genesis_and_forks(),
             GB = aec_test_utils:genesis_block(),
             aec_chain_state:insert_block(GB),
             {ok, _} = aec_tx_pool_gc:start_link(),
             {ok, _} = aec_tx_pool:start_link(),
             {ok, _} = aec_tx_gc:start_link(),
             meck:new(aec_governance, [passthrough]),
             meck:expect(aec_governance, minimum_gas_price, 1, 1),
             meck:new(aec_tx_pool, [passthrough]),
             meck:expect(aec_tx_pool, minimum_miner_gas_price, 0, 1),
             ok = application:set_env(aecore, mempool_nonce_baseline, 100),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = application:unset_env(aecore, mempool_nonce_baseline),
             aec_test_utils:stop_chain_db(),
             aec_test_utils:unmock_genesis_and_forks(),
             ok = aec_tx_gc:stop(),
             ok = aec_tx_pool:stop(),
             ok = aec_tx_pool_gc:stop(),
             meck:unload(aec_tx_pool),
             meck:unload(aec_governance),
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir),
             ok = application:stop(gproc),
             ok
     end,
     [{"Test that GCed transactions are removed from the DB",
       fun() ->
               %% Create 3 transactions and push them into the mempool
               Tx1 = signed_tx(1, 1), %% Scheduled for mempool GC at height = 1
               Tx2 = signed_tx(2, 2), %% Scheduled for mempool GC at height = 2
               Tx3 = signed_tx(3, 3), %% Scheduled for mempool GC at height = 3
               ?assertEqual(ok, aec_tx_pool:push(Tx1)),
               ?assertEqual(ok, aec_tx_pool:push(Tx2)),
               ?assertEqual(ok, aec_tx_pool:push(Tx3)),
               Tx1Hash = aetx_sign:hash(Tx1),
               Tx2Hash = aetx_sign:hash(Tx2),
               Tx3Hash = aetx_sign:hash(Tx3),

               ?assertMatch({ok, [_, _, _]}, aec_tx_pool:peek(infinity)),

               %% Check mempool transactions are stored as aec_signed_tx objects in the DB
               ?assertMatch({value, _}, aec_db:find_signed_tx(Tx1Hash)),
               ?assertMatch({value, _}, aec_db:find_signed_tx(Tx2Hash)),
               ?assertMatch({value, _}, aec_db:find_signed_tx(Tx3Hash)),

               %% Check aec_tx_gc DB objects are not present for transactions in the mempool
               ?assertEqual([], read_aec_tx_gc_object(Tx1Hash)),
               ?assertEqual([], read_aec_tx_gc_object(Tx2Hash)),
               ?assertEqual([], read_aec_tx_gc_object(Tx3Hash)),

               %% GC at height = 2 (Tx1 and Tx2 are GCed)
               ok = aec_tx_pool:sync_garbage_collect(2),

               %% Check Tx3 is still in the mempool
               ?assertMatch({ok, [Tx3]}, aec_tx_pool:peek(infinity)),

               %% Check aec_signed_tx object is kept only for Tx3 after mempool GC at height = 2
               ?assertMatch(none, aec_db:find_signed_tx(aetx_sign:hash(Tx1))),
               ?assertMatch(none, aec_db:find_signed_tx(aetx_sign:hash(Tx2))),
               ?assertMatch({value, _}, aec_db:find_signed_tx(aetx_sign:hash(Tx3))),

               %% Check aec_tx_gc objects are created for GCed Tx1 and Tx2
               ?assertMatch([_], read_aec_tx_gc_object(Tx1Hash)),
               ?assertMatch([_], read_aec_tx_gc_object(Tx2Hash)),
               ?assertMatch([], read_aec_tx_gc_object(Tx3Hash)),

               %% aec_tx_gc objects will be scheduled for removal by aec_tx_gc GC at [Mempool GC Height] + ?TTL.
               %% For test ?TTL = 4 and [Mempool GC Height] was 2, so aec_tx_gc objects will be removed at height = 6.

               %% Check that nothing changes in the DB for aec_tx_gc GC at height = 5
               ok = aec_tx_gc:sync_gc(5),

               ?assertMatch(none, aec_db:find_signed_tx(aetx_sign:hash(Tx1))),
               ?assertMatch(none, aec_db:find_signed_tx(aetx_sign:hash(Tx2))),
               ?assertMatch({value, _}, aec_db:find_signed_tx(aetx_sign:hash(Tx3))),

               ?assertMatch([_], read_aec_tx_gc_object(Tx1Hash)),
               ?assertMatch([_], read_aec_tx_gc_object(Tx2Hash)),
               ?assertMatch([], read_aec_tx_gc_object(Tx3Hash)),

               %% Check that aec_tx_gc objects are removed by aec_tx_gc GC at height = 6
               ok = aec_tx_gc:sync_gc(6),

               ?assertMatch(none, aec_db:find_signed_tx(aetx_sign:hash(Tx1))),
               ?assertMatch(none, aec_db:find_signed_tx(aetx_sign:hash(Tx2))),
               ?assertMatch({value, _}, aec_db:find_signed_tx(aetx_sign:hash(Tx3))),

               ?assertMatch([], read_aec_tx_gc_object(Tx1Hash)),
               ?assertMatch([], read_aec_tx_gc_object(Tx2Hash)),
               ?assertMatch([], read_aec_tx_gc_object(Tx3Hash)),
               ok
       end},
      {"Test GC does not interfere with fork changes",
       fun() ->
               %% Set genesis block with some funds
               aec_test_utils:stop_chain_db(),
               {Pub, Priv} = keypair(),
               PresetAccounts = [{Pub, 1000000 * aec_test_utils:min_gas_price()}],
               meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
               {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
               aec_test_utils:start_chain_db(),
               {ok,_} = aec_chain_state:insert_block(GenesisBlock),

               %% Create first key block in the chain (which will be a fork point)
               {ok, ForkPointBlock} = aec_block_key_candidate:create(aec_chain:top_block(), Priv),
               {ok, ForkPointBlockHash} = aec_blocks:hash_internal_representation(ForkPointBlock),
               {ok, _} = aec_chain_state:insert_block(ForkPointBlock),
               ?assertEqual(ForkPointBlockHash, aec_chain:top_block_hash()),
               ok = aec_keys:promote_candidate(aec_blocks:miner(ForkPointBlock)),
               ForkPointBlockHash = aec_chain:top_block_hash(),
               ForkPointBlock = aec_chain:top_block(),

               %% Prepare transactions
               [Tx2, Tx1] = signed_txs(1, 2, Pub, Priv, 2),
               Tx1Hash = aetx_sign:hash(Tx1),
               Tx2Hash = aetx_sign:hash(Tx2),

               %% Push transactions to the mempool
               ?assertEqual(ok, aec_tx_pool:push(Tx1)),
               ?assertEqual(ok, aec_tx_pool:push(Tx2)),
               ?assertMatch({ok, [Tx1, Tx2]}, aec_tx_pool:peek(infinity)),

               %% Include transaction into the chain (call the chain Fork1 = F1)
               {ok, F1MicroCandidate0, _} = aec_block_micro_candidate:create(ForkPointBlock),
               {ok, F1MicroCandidate} = aec_keys:sign_micro_block(F1MicroCandidate0),
               {ok, F1MicroHash} = aec_blocks:hash_internal_representation(F1MicroCandidate),
               {ok,_} = aec_chain_state:insert_block(F1MicroCandidate),
               ok = aec_tx_pool:top_change(micro, ForkPointBlockHash, F1MicroHash),

               %% Check that transactions are no longer in mempool
               ?assertMatch({ok, []}, aec_tx_pool:peek(infinity)),

               %% Mine a key block on top of the micro block (chain Fork1 = F1)
               {ok, F1KeyBlock1} = aec_block_key_candidate:create(F1MicroCandidate, Priv),
               {ok, F1KeyBlock1Hash} = aec_blocks:hash_internal_representation(F1KeyBlock1),
               {ok, _} = aec_chain_state:insert_block(F1KeyBlock1),
               ok = aec_tx_pool:top_change(micro, F1MicroHash, F1KeyBlock1Hash),

               %% Check that F1KeyBlock1 is now the top
               ?assertEqual(F1KeyBlock1Hash, aec_chain:top_block_hash()),

               %% Create a fork that will take over top (call the chain Fork2 = F2)

               %% Insert one key block...
               {ok, F2KeyBlock1} = aec_block_key_candidate:create(ForkPointBlock, Priv),
               {ok, F2KeyBlock1Hash} = aec_blocks:hash_internal_representation(F2KeyBlock1),
               {ok, _} = aec_chain_state:insert_block(F2KeyBlock1),

               %% ...one key block does not take over the top...
               ?assertNotEqual(F2KeyBlock1Hash, aec_chain:top_block_hash()),
               ?assertEqual(F1KeyBlock1Hash, aec_chain:top_block_hash()),

               %% ...insert second key block...
               {ok, F2KeyBlock2} = aec_block_key_candidate:create(F2KeyBlock1, Priv),
               {ok, F2KeyBlock2Hash} = aec_blocks:hash_internal_representation(F2KeyBlock2),
               {ok, _} = aec_chain_state:insert_block(F2KeyBlock2),

               %% ...and the second key block takes over.
               ?assertNotEqual(F1KeyBlock1Hash, aec_chain:top_block_hash()),
               ?assertEqual(F2KeyBlock2Hash, aec_chain:top_block_hash()),
               ok = aec_tx_pool:top_change(key, F1KeyBlock1Hash, F2KeyBlock2Hash),

               %% Transaction from micro block F1MicroCandidate on Fork1 are back in the mempool.
               ?assertMatch({ok, [Tx1, Tx2]}, aec_tx_pool:peek(infinity)),
               ?assertMatch({value, _}, aec_db:find_signed_tx(Tx1Hash)),
               ?assertMatch({value, _}, aec_db:find_signed_tx(Tx2Hash)),

               %% Garbage collect these transactions from the database
               %% Mempool GC kicks in at height = 2 (TTL defined in TX)
               %% Transactions GC kicks in at height = 6 (2 + 4)

%% UNCOMMENTING BELOW SHOULD NOT MAKE TEST FAIL!
%%               ok = aec_tx_pool:sync_garbage_collect(2),
%%               ok = aec_tx_gc:sync_gc(6),
%%
%%               ?assertMatch(none, aec_db:find_signed_tx(Tx1Hash)),
%%               ?assertMatch(none, aec_db:find_signed_tx(Tx2Hash)),
%%               ?assertMatch([], read_aec_tx_gc_object(Tx1Hash)),
%%               ?assertMatch([], read_aec_tx_gc_object(Tx1Hash)),

               %% Make Fork1 take over again (add two key blocks to it)
               {ok, F1KeyBlock2} = aec_block_key_candidate:create(F1KeyBlock1, Priv),
               {ok, _} = aec_chain_state:insert_block(F1KeyBlock2),
               {ok, F1KeyBlock3} = aec_block_key_candidate:create(F1KeyBlock2, Priv),
               {ok, F1KeyBlock3Hash} = aec_blocks:hash_internal_representation(F1KeyBlock3),
               {ok, _} = aec_chain_state:insert_block(F1KeyBlock3),

               %% Check top changed
               ?assertEqual(F1KeyBlock3Hash, aec_chain:top_block_hash()),
               ok = aec_tx_pool:top_change(key, F2KeyBlock2Hash, F1KeyBlock3Hash),

               %% Check transactions are in the database
               ?assertMatch({value, _}, aec_db:find_signed_tx(Tx1Hash)),
               ?assertMatch({value, _}, aec_db:find_signed_tx(Tx2Hash)),

               ok
       end}]
    }.

signed_txs(FirstNonce, TTL, Pub, Priv, Count) ->
    signed_txs(FirstNonce, TTL, Pub, Priv, Count, []).

signed_txs(_Nonce, _TTL, _Pub, _Priv, 0, Acc) ->
    Acc;
signed_txs(Nonce, TTL, Pub, Priv, Count, Acc) when Count > 0 ->
    SignedTx = signed_tx(Nonce, TTL, Pub, Priv),
    signed_txs(Nonce + 1, TTL, Pub, Priv, Count - 1, [SignedTx | Acc]).

signed_tx(Nonce, TTL) ->
    {Pub, Priv} = keypair(),
    signed_tx(Nonce, TTL, Pub, Priv).

signed_tx(Nonce, TTL, Pub, Priv) ->
    {ok, Tx} = aec_spend_tx:new(
                 #{sender_id => aeser_id:create(account, Pub),
                   recipient_id => aeser_id:create(account, Pub),
                   amount => 1,
                   nonce => Nonce,
                   fee => 20000,
                   ttl => TTL,
                   payload => <<"">>}),
    aec_test_utils:sign_tx(Tx, Priv).

keypair() ->
    #{ public := Pub, secret := Priv } = enacl:sign_keypair(),
    {Pub, Priv}.

read_aec_tx_gc_object(Hash) ->
    aec_db:transaction(
      fun() ->
              mnesia:read(aec_tx_gc, Hash)
      end).
