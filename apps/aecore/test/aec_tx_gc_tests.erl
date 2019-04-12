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
             ok = application:ensure_started(crypto),
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
             ok
     end,
     fun(_) ->
             ok = application:unset_env(aecore, mempool_nonce_baseline),
             ok = application:stop(crypto),
             ok = application:stop(gproc),
             aec_test_utils:stop_chain_db(),
             aec_test_utils:unmock_genesis_and_forks(),
             ok = aec_tx_gc:stop(),
             ok = aec_tx_pool:stop(),
             ok = aec_tx_pool_gc:stop(),
             meck:unload(aec_tx_pool),
             meck:unload(aec_governance),
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
       end}]}.

signed_tx(Nonce, TTL) ->
    {Pub, Priv} = keypair(),
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
