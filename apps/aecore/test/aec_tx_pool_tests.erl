%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_tx_pool_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TAB, aec_tx_pool_test_keys).

-define(BENEFICIARY, <<"ak$tjnw1KcmnwfqXvhtGa9GRjanbHM3t6PmEWEWtNMM3ouvNKRu5">>).

tx_pool_test_() ->
    {foreach,
     fun() ->
             ok = application:set_env(aecore, beneficiary, ?BENEFICIARY),
             application:ensure_started(gproc),
             ok = application:ensure_started(crypto),
             TmpKeysDir = aec_test_utils:aec_keys_setup(),
             aec_test_utils:start_chain_db(),
             aec_test_utils:mock_genesis(),
             GB = aec_test_utils:genesis_block(),
             aec_chain_state:insert_block(GB),
             aec_test_utils:mock_block_target_validation(), %% Mocks aec_governance.
             {ok, _} = aec_tx_pool:start_link(),
             %% Start `aec_keys` merely for generating realistic test
             %% signed txs - as a node would do.
             ets:new(?TAB, [public, ordered_set, named_table]),
             meck:new(aeu_time, [passthrough]),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = application:unset_env(aecore, beneficiary),
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir),
             ok = application:stop(gproc),
             ets:delete(?TAB),
             aec_test_utils:stop_chain_db(),
             aec_test_utils:unmock_genesis(),
             aec_test_utils:unmock_block_target_validation(), %% Unloads aec_governance mock.
             ok = aec_tx_pool:stop(),
             meck:unload(aeu_time),
             ok
     end,
     [{"No txs in mempool",
       fun() ->
               ?assertEqual({ok, []}, aec_tx_pool:peek(1)),
               ?assertEqual({ok, []}, aec_tx_pool:peek(3)),
               ?assertEqual(0, aec_tx_pool:size())
       end},
      {"As a healthy network peer, the node stores in mempool txs received from"
       " peers and serves txs in mempool to peers",
       fun() ->
               %% No txs to serve to peers.
               ?assertEqual({ok, []}, aec_tx_pool:peek(1)),

               %% Tx received from a peer.
               STx1 = a_signed_tx(new_pubkey(), me, 1, 1),
               ?assertEqual(ok, aec_tx_pool:push(STx1, tx_received)),

               %% One tx to serve to peers.
               ?assertEqual({ok, [STx1]}, aec_tx_pool:peek(1)),

               %% Add it again and see that it is not added twice
               ?assertEqual(ok, aec_tx_pool:push(STx1, tx_received)),
               ?assertEqual({ok, [STx1]}, aec_tx_pool:peek(1)),

               %% Other tx received from a peer.
               STx2 = a_signed_tx(new_pubkey(), me, 1, 2),
               ?assertEqual(ok, aec_tx_pool:push(STx2, tx_received)),

               %% Two tx2 to serve to peers.
               {ok, PoolTxs} = aec_tx_pool:peek(infinity),
               ?assertEqual(lists:sort([STx1, STx2]), lists:sort(PoolTxs))
       end},
      {"Mempool follows chain insertions and forks",
       fun() ->
               aec_test_utils:stop_chain_db(),
               %% Prepare a chain with specific genesis block with some funds
               PubKey1 = new_pubkey(),
               PubKey2 = new_pubkey(),
               meck:expect(aec_genesis_block_settings, preset_accounts, 0,
                  [{PubKey1, 100}, {PubKey2, 100}]),
               {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
               aec_test_utils:start_chain_db(),
               ok = aec_chain_state:insert_block(GenesisBlock),

               %% The first block needs to be a key-block
               {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PubKey1),
               {ok, KeyHash1} = aec_blocks:hash_internal_representation(KeyBlock1),
               ok = aec_chain_state:insert_block(KeyBlock1),
               ?assertEqual(KeyHash1, aec_chain:top_block_hash()),

               TopBlock = aec_chain:top_block(),
               TopBlockHash = aec_chain:top_block_hash(),

               %% Prepare a few txs.
               STx1 = a_signed_tx(PubKey1, new_pubkey(), 1, 1),
               STx2 = a_signed_tx(PubKey1, new_pubkey(), 2, 1),
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               ?assertEqual(ok, aec_tx_pool:push(STx2)),
               {ok, PoolTxs} = aec_tx_pool:peek(infinity),
               ?assertEqual(lists:sort([STx1, STx2]), lists:sort(PoolTxs)),

               %% Insert a block in chain.
               {ok, USCandidate1, _} = aec_block_micro_candidate:create(TopBlock),
               {ok, Candidate1} = aec_keys:sign_micro_block(USCandidate1),

               {ok, CHash1} = aec_blocks:hash_internal_representation(Candidate1),
               ok = aec_chain_state:insert_block(Candidate1),
               ?assertEqual(CHash1, aec_chain:top_block_hash()),

               %% Check that we uses all the txs in mempool
               Included = aec_blocks:txs(Candidate1),
               ?assertEqual(lists:sort(Included), lists:sort([STx1, STx2])),

               %% Ping tx_pool for top change
               aec_tx_pool:top_change(TopBlockHash, CHash1),

               %% The mempool should now be empty
               ?assertEqual({ok, []}, aec_tx_pool:peek(infinity)),

               %% Create a fork
               %% First add a chain of two micro blocks with key blocks
               %% on top of each of them
               %% Ensure micro_block_cycle time
               meck:expect(aeu_time, now_in_msecs, fun() -> meck:passthrough([]) + 3000 end),
               STx3 = a_signed_tx(PubKey2, new_pubkey(), 1, 1),
               ?assertEqual(ok, aec_tx_pool:push(STx3)),
               {ok, USCandidate3, _} = aec_block_micro_candidate:create(aec_chain:top_block()),
               {ok, Candidate3} = aec_keys:sign_micro_block(USCandidate3),

               ok = aec_chain_state:insert_block(Candidate3),
               TopBlockFork1 = aec_chain:top_block(),
               {ok, KeyBlock2} = aec_block_key_candidate:create(TopBlockFork1, PubKey1),
               {ok, CHashFork1} = aec_blocks:hash_internal_representation(KeyBlock2),

               meck:expect(aeu_time, now_in_msecs, fun() -> meck:passthrough([]) + 6000 end),
               STx4 = a_signed_tx(PubKey2, new_pubkey(), 2, 1),
               ?assertEqual(ok, aec_tx_pool:push(STx4)),
               {ok, USCandidate4, _} = aec_block_micro_candidate:create(aec_chain:top_block()),
               {ok, Candidate4} = aec_keys:sign_micro_block(USCandidate4),

               ok = aec_chain_state:insert_block(Candidate4),
               TopBlockFork2 = aec_chain:top_block(),
               {ok, KeyBlock3} = aec_block_key_candidate:create(TopBlockFork2, PubKey1),
               {ok, CHashFork2} = aec_blocks:hash_internal_representation(KeyBlock3),

               %% Push the keyblock with the longest chain of micro blocks
               ok = aec_chain_state:insert_block(KeyBlock3),
               ?assertEqual(CHashFork2, aec_chain:top_block_hash()),
               aec_tx_pool:top_change(CHash1, CHashFork2),
               %% The mempool should now be empty
               ?assertEqual({ok, []}, aec_tx_pool:peek(infinity)),

               %% Ensure that the shorter fork takes over by
               %% increasing the difficulty
               meck:new(aec_blocks, [passthrough]),
               meck:expect(aec_headers, difficulty,
                           fun(B) -> meck:passthrough([B]) * 2 end),

               %% Push the keyblock with the shorter chain of micro blocks
               %% and check that it takes over.
               ok = aec_chain_state:insert_block(KeyBlock2),
               ?assertEqual(CHashFork1, aec_chain:top_block_hash()),

               %% Ping tx_pool for top change
               aec_tx_pool:top_change(CHashFork2, CHashFork1),

               %% The not included transaction should now be back in the pool
               ?assertEqual({ok, [STx4]}, aec_tx_pool:peek(infinity)),

               meck:unload(aec_headers),
               ok
       end},
      {"Ensure ordering",
       fun() ->
                 %% We should sort by fee, but preserve the order of nonces for each sender
                 PK1 = new_pubkey(),
                 PK2 = new_pubkey(),
                 PK3 = new_pubkey(),
                 PK4 = new_pubkey(),
                 STxs =
                   [ a_signed_tx        (_Sender=PK1, me,_Nonce=1,_Fee=1)
                   , a_signed_tx        (        PK1, me,       2,     2)
                   , a_signed_tx        (        PK1, me,       3,     3)
                   , a_signed_tx        (        PK2, me,       2,     5)
                   , a_signed_tx        (        PK2, me,       1,     6)
                   , signed_ct_create_tx(        PK4,           1,     1,_GasPrice=1)
                   , signed_ct_call_tx  (        PK4,           2,     4,          9)
                   , signed_ct_call_tx  (        PK4,           3,     7,          0)
                   ],

                 [?assertEqual(ok, aec_tx_pool:push(Tx)) || Tx <- STxs],
                 {ok, CurrentMempoolSigned} = aec_tx_pool:peek(10),
                 %% extract transactions without verification
                 CurrentMempool = [ aetx_sign:tx(STx) || STx <- CurrentMempoolSigned ],

                 MempoolOrder = [{aetx:origin(Tx), aetx:nonce(Tx)} || Tx <- CurrentMempool],
                 %% this is not-optimal order: transactions for PK1 and PK4 are invalid in that order
                 CorrectOrder = [{PK4,3},{PK2,1},{PK2,2},{PK4,2},{PK1,3},{PK1,2},{PK4,1},{PK1,1}],

                 ?assertEqual(CorrectOrder, MempoolOrder),

                 %% check if we track nonces correctly
                 MaxNonce = aec_tx_pool:get_max_nonce(PK1),
                 ?assertEqual({ok,3}, MaxNonce),

                 NotExistingSender = aec_tx_pool:get_max_nonce(PK3),
                 ?assertEqual(undefined, NotExistingSender)
             end},
      {"Ensure candidate ordering",
       fun() ->
               PK = new_pubkey(),

               %% Only one tx in pool
               STx1 = a_signed_tx(PK, me, Nonce1=1,_Fee1=1),
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               ?assertEqual({ok, [STx1]}, aec_tx_pool:get_candidate(10, aec_chain:top_block_hash())),

               %% Order by nonce even if fee is higher
               STx2 = a_signed_tx(PK, me, Nonce2=2, Fee2=5),
               ?assertEqual(ok, aec_tx_pool:push(STx2)),
               ?assertEqual({ok, [STx1, STx2]}, aec_tx_pool:get_candidate(10, aec_chain:top_block_hash())),

               %% Replace same nonce with the higher fee
               STx3 = a_signed_tx(PK, me, Nonce1=1, 2),
               ?assertEqual(ok, aec_tx_pool:push(STx3)),
               ?assertEqual({ok, [STx3, STx2]}, aec_tx_pool:get_candidate(10, aec_chain:top_block_hash())),

               %% Replace same nonce with same fee but positive gas price (gas price of transaction without gas price is considered zero)
               STx4 = signed_ct_create_tx(PK, Nonce2=2, Fee2=5,_GasPrice4=1),
               ?assertEqual(ok, aec_tx_pool:push(STx4)),
               ?assertEqual({ok, [STx3, STx4]}, aec_tx_pool:get_candidate(10, aec_chain:top_block_hash())),

               %% Replace same nonce with same fee but higher gas price
               STx5 = signed_ct_create_tx(PK, Nonce2=2, Fee2=5, 2),
               ?assertEqual(ok, aec_tx_pool:push(STx5)),
               ?assertEqual({ok, [STx3, STx5]}, aec_tx_pool:get_candidate(10, aec_chain:top_block_hash())),

               %% Order by nonce even if fee and gas price are higher
               STx6 = signed_ct_call_tx(PK, _Nonce6=3,_Fee6=9,_GasPrice6=9),
               ?assertEqual(ok, aec_tx_pool:push(STx6)),
               ?assertEqual({ok, [STx3, STx5, STx6]}, aec_tx_pool:get_candidate(10, aec_chain:top_block_hash())),

               ok
       end},
      {"Ensure persistence",
       fun() ->
               %% Prepare a few txs.
               STx1 = a_signed_tx(me, new_pubkey(), 1, 1),
               STx2 = a_signed_tx(me, new_pubkey(), 2, 1),
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               ?assertEqual(ok, aec_tx_pool:push(STx2)),
               {ok, PoolTxs} = aec_tx_pool:peek(infinity),
               ?assertEqual(lists:sort([STx1, STx2]), lists:sort(PoolTxs)),

               %% Stop the mempool and start it again to see that it reinits
               ok        = aec_tx_pool:stop(),
               {ok, Pid} = aec_tx_pool:start_link(),
               {ok, PoolTxs2} = aec_tx_pool:peek(infinity),
               ?assertEqual(lists:sort([STx1, STx2]), lists:sort(PoolTxs2)),
               unlink(Pid), %% Leave it for the cleanup
               ok
       end},
      {"Test rejection of transactions",
       fun() ->
               aec_test_utils:stop_chain_db(),
               %% Prepare a chain with specific genesis block with some funds
               PubKey1 = new_pubkey(),
               PubKey2 = new_pubkey(),
               meck:expect(aec_genesis_block_settings, preset_accounts, 0,
                  [{PubKey1, 100}, {PubKey2, 100}]),
               {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
               aec_test_utils:start_chain_db(),
               ok = aec_chain_state:insert_block(GenesisBlock),

               %% The first block needs to be a key-block
               {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PubKey1),
               {ok, KeyHash1} = aec_blocks:hash_internal_representation(KeyBlock1),
               ok = aec_chain_state:insert_block(KeyBlock1),
               ?assertEqual(KeyHash1, aec_chain:top_block_hash()),

               TopBlock = aec_chain:top_block(),

               %% Add a transaction to the chain
               STx1 = a_signed_tx(PubKey1, new_pubkey(), 1, 1),
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               {ok, USCandidate1, _} = aec_block_micro_candidate:create(TopBlock),
               {ok, Candidate1} = aec_keys:sign_micro_block(USCandidate1),
               {ok, Top} = aec_blocks:hash_internal_representation(Candidate1),
               ok = aec_chain_state:insert_block(Candidate1),
               ?assertEqual(Top, aec_chain:top_block_hash()),

               %% Now we should reject the same transaction since it
               %% is already in the chain
               ?assertEqual({error, already_accepted},
                            aec_tx_pool:push(STx1)),

               %% A transaction with too low nonce should be rejected
               STx2 = a_signed_tx(PubKey1, new_pubkey(), 1, 1),
               ?assertEqual({error, account_nonce_too_high},
                            aec_tx_pool:push(STx2)),

               %% A transaction with too high nonce should _NOT_ be rejected
               STx3 = a_signed_tx(PubKey1, new_pubkey(), 5, 1),
               ?assertEqual(ok, aec_tx_pool:push(STx3)),

               %% A transaction with too low fee should be rejected
               STx4 = a_signed_tx(PubKey1, new_pubkey(), 6,
                                  aec_governance:minimum_tx_fee() - 1),
               ?assertEqual({error, too_low_fee}, aec_tx_pool:push(STx4)),

               %% A transaction with too low gas price should be rejected
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_create_tx(PubKey1, 10, 100, 0))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_call_tx  (PubKey1, 20, 100, 0))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_create_tx(PubKey1, 11, 100, 1))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_call_tx  (PubKey1, 21, 100, 1))),
               meck:expect(aec_governance, minimum_gas_price, 0, 2),
               ?assertEqual({error, too_low_gas_price}, aec_tx_pool:push(signed_ct_create_tx(PubKey1, 12, 100, 0))),
               ?assertEqual({error, too_low_gas_price}, aec_tx_pool:push(signed_ct_call_tx  (PubKey1, 22, 100, 0))),
               ?assertEqual({error, too_low_gas_price}, aec_tx_pool:push(signed_ct_create_tx(PubKey1, 13, 100, 1))),
               ?assertEqual({error, too_low_gas_price}, aec_tx_pool:push(signed_ct_call_tx  (PubKey1, 23, 100, 1))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_create_tx(PubKey1, 14, 100, 2))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_call_tx  (PubKey1, 24, 100, 2))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_create_tx(PubKey1, 15, 100, 3))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_call_tx  (PubKey1, 25, 100, 3))),

               %% A transaction with too low ttl should be rejected
               %% First add another block to make the chain high enough to
               %% fail on TTL
               {ok, Candidate2} = aec_block_key_candidate:create(aec_chain:top_block(), PubKey1),
               {ok, Top2} = aec_blocks:hash_internal_representation(Candidate2),
               ok = aec_chain_state:insert_block(Candidate2),
               ?assertEqual(Top2, aec_chain:top_block_hash()),

               STx5 = a_signed_tx(PubKey1, new_pubkey(), 6, 1, 1),
               ?assertEqual({error, ttl_expired}, aec_tx_pool:push(STx5)),

               ok
       end},
       {"Test GC",
        fun() ->
            %% initialize chain
            {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
            ok = aec_chain_state:insert_block(GenesisBlock),

            %% Prepare three transactions
            PubKey = new_pubkey(),
            STx1 = a_signed_tx(PubKey, PubKey, 1, 1),
            STx2 = a_signed_tx(PubKey, PubKey, 2, 1),
            STx3 = a_signed_tx(PubKey, PubKey, 3, 1),

            %% Post them
            ?assertEqual(ok, aec_tx_pool:push(STx1)),
            ?assertEqual(ok, aec_tx_pool:push(STx2)),
            ?assertEqual(ok, aec_tx_pool:push(STx3)),

            ?assertMatch({ok, [_, _, _]}, aec_tx_pool:peek(infinity)),

            %% Txs will be scheduled for
            %% removal at Height + ?TX_TTL
            %% For test ?TX_TTL = 8

            %% Doing a garbage collect at height 0 shouldn't affect
            aec_tx_pool:garbage_collect(0),
            ?assertMatch({ok, [_, _, _]}, aec_tx_pool:peek(infinity)),

            %% At 4 still GC should not kick in.
            aec_tx_pool:garbage_collect(4),
            ?assertMatch({ok, [_, _, _]}, aec_tx_pool:peek(infinity)),

            %% At 8, now TXs should be dropped.
            aec_tx_pool:garbage_collect(8),
            ?assertMatch({ok, []}, aec_tx_pool:peek(infinity))

        end}
     ]}.

a_signed_tx(Sender, Recipient, Nonce, Fee) ->
    a_signed_tx(Sender, Recipient, Nonce, Fee,0).

a_signed_tx(Sender, Recipient, Nonce, Fee, TTL) ->
    {ok, Tx} = aec_spend_tx:new(#{sender => acct(Sender),
                                  recipient => acct(Recipient),
                                  amount => 1,
                                  nonce => Nonce,
                                  fee => Fee,
                                  ttl => TTL,
                                  payload => <<"">>}),
    {ok, STx} = sign(Sender, Tx),
    STx.

signed_ct_create_tx(Sender, Nonce, Fee, GasPrice) ->
    Spec =
        #{ fee        => Fee
         , owner      => aec_id:create(account, Sender)
         , nonce      => Nonce
         , code       => <<"NOT PROPER BYTE CODE">>
         , vm_version => 1
         , deposit    => 10
         , amount     => 200
         , gas        => 10
         , gas_price  => GasPrice
         , call_data  => <<"NOT ENCODED ACCORDING TO ABI">>
         , ttl        => 0
         },
    {ok, Tx} = aect_create_tx:new(Spec),
    {ok, STx} = sign(Sender, Tx),
    STx.

signed_ct_call_tx(Sender, Nonce, Fee, GasPrice) ->
    Contract = aec_id:create(contract, <<"contract_address......(32 bytes)">>),
    Spec =
        #{ fee         => Fee
         , contract    => Contract
         , caller      => aec_id:create(account, Sender)
         , nonce       => Nonce
         , vm_version  => 1
         , amount      => 100
         , gas         => 10000
         , gas_price   => GasPrice
         , call_data   => <<"CALL DATA">>
         , ttl         => 0
         },
    {ok, Tx} = aect_call_tx:new(Spec),
    {ok, STx} = sign(Sender, Tx),
    STx.

sign(me, Tx) ->
    aec_keys:sign_tx(Tx);  %% why via keys here?
sign(PubKey, Tx) ->
    try
        [{_, PrivKey}] = ets:lookup(?TAB, PubKey),
        {ok, Trees} = aec_chain:get_top_state(),
        {ok, Signers} = aetx:signers(Tx, Trees),
        true = lists:member(PubKey, Signers),
        {ok, aec_test_utils:sign_tx(Tx, PrivKey)}
    catch
        error:Err ->
            erlang:error({Err, erlang:get_stacktrace()})
    end.

acct(me) ->
    {ok, Key} = aec_keys:pubkey(),
    aec_id:create(account, Key);
acct(A) when is_binary(A) ->
    aec_id:create(account, A).

new_pubkey() ->
    {Pub, Priv} = keypair(),
    ets:insert(?TAB, {Pub, Priv}),
    Pub.

keypair() ->
    #{ public := Pub, secret := Priv } = enacl:sign_keypair(),
    {Pub, Priv}.
