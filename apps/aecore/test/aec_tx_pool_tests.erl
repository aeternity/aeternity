%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_tx_pool_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include("../../aecontract/include/hard_forks.hrl").

-define(TAB, aec_tx_pool_test_keys).

-define(BENEFICIARY, <<"ak_tjnw1KcmnwfqXvhtGa9GRjanbHM3t6PmEWEWtNMM3ouvNKRu5">>).

tx_pool_test_() ->
    {foreach,
     fun() ->
             ok = application:set_env(aecore, beneficiary, ?BENEFICIARY),
             application:ensure_started(gproc),
             ok = application:ensure_started(crypto),
             TmpKeysDir = aec_test_utils:aec_keys_setup(),
             aec_test_utils:start_chain_db(),
             aec_test_utils:mock_genesis_and_forks(),
             GB = aec_test_utils:genesis_block(),
             aec_chain_state:insert_block(GB),
             aec_test_utils:mock_governance(), %% Mocks aec_governance.
             {ok, _} = aec_tx_pool_gc:start_link(),
             {ok, _} = aec_tx_pool:start_link(),
             %% Start `aec_keys` merely for generating realistic test
             %% signed txs - as a node would do.
             ets:new(?TAB, [public, ordered_set, named_table]),
             meck:new(aeu_time, [passthrough]),
             meck:new(aec_accounts, [passthrough]),
             meck:new(aec_jobs_queues),
             meck:expect(aec_jobs_queues, run, fun(_, F) -> F() end),
             meck:expect(aec_governance, minimum_gas_price, 1, 1),
             meck:new(aec_tx_pool, [passthrough]),
             meck:expect(aec_tx_pool, minimum_miner_gas_price, 0, 1),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = application:unset_env(aecore, mempool_nonce_offset),
             ok = application:unset_env(aecore, mempool_nonce_baseline),
             ok = application:unset_env(aecore, beneficiary),
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir),
             ok = application:stop(gproc),
             ets:delete(?TAB),
             aec_test_utils:stop_chain_db(),
             aec_test_utils:unmock_genesis_and_forks(),
             aec_test_utils:unmock_governance(), %% Unloads aec_governance mock.
             ok = aec_tx_pool:stop(),
             ok = aec_tx_pool_gc:stop(),
             meck:unload(aec_accounts),
             meck:unload(aeu_time),
             meck:unload(aec_jobs_queues),
             meck:unload(aec_tx_pool),
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
               STx1 = a_signed_tx(new_pubkey(), me, 1, 20000),
               ?assertEqual(ok, aec_tx_pool:push(STx1, tx_received)),

               %% One tx to serve to peers.
               ?assertEqual({ok, [STx1]}, aec_tx_pool:peek(1)),

               %% Add it again and see that it is not added twice
               ?assertEqual(ok, aec_tx_pool:push(STx1, tx_received)),
               ?assertEqual({ok, [STx1]}, aec_tx_pool:peek(2)),

               %% Other tx received from a peer.
               STx2 = a_signed_tx(new_pubkey(), me, 1, 20000),
               ?assertEqual(ok, aec_tx_pool:push(STx2, tx_received)),

               %% Two tx2 to serve to peers.
               {ok, PoolTxs} = aec_tx_pool:peek(infinity),
               ?assertEqual(lists:sort([STx1, STx2]), lists:sort(PoolTxs))
       end},
      {"ensure nonce limit for sender without account in state",
       fun() ->
            PK0 = new_pubkey(),
            ?assertEqual(none,                   aec_chain:get_account(PK0)),
            ?assertEqual(ok,                     aec_tx_pool:push( a_signed_tx(PK0, me, 1, 20000) )),
            ?assertEqual({error,nonce_too_high}, aec_tx_pool:push( a_signed_tx(PK0, me, 2, 20000) )),

            aec_test_utils:stop_chain_db(),
            PK1 = new_pubkey(),
            meck:expect(aec_fork_block_settings, genesis_accounts, 0, [{PK1, 100000}]),
            aec_consensus:set_genesis_hash(),
            {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
            aec_test_utils:start_chain_db(),
            {ok,_} = aec_chain_state:insert_block(GenesisBlock),
            ?assertMatch({value, _}, aec_chain:get_account(PK1)),
            ?assertEqual(ok, aec_tx_pool:push( a_signed_tx(PK1, me, 1, 20000) )),
            ?assertEqual(ok, aec_tx_pool:push( a_signed_tx(PK1, me, 2, 20000) )),
            ok
       end},
      {"ensure nonce limit",
       fun() ->
            aec_test_utils:stop_chain_db(),
            PK = new_pubkey(),
            meck:expect(aec_fork_block_settings, genesis_accounts, 0, [{PK, 100000}]),
            aec_consensus:set_genesis_hash(),
            {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
            aec_test_utils:start_chain_db(),
            {ok,_} = aec_chain_state:insert_block(GenesisBlock),
            ?assertMatch({value, _}, aec_chain:get_account(PK)),

            ?assertEqual(ok, aec_tx_pool:push( a_signed_tx(PK, me, 1, 20000) )),
            ?assertEqual(ok, aec_tx_pool:push( a_signed_tx(PK, me, 2, 20000) )),
            ?assertEqual(ok, aec_tx_pool:push( a_signed_tx(PK, me, 5, 20000) )),
            ?assertEqual({error, nonce_too_high}, aec_tx_pool:push( a_signed_tx(PK, me, 6, 20000) )),
            ?assertMatch({ok, [_, _, _]}, aec_tx_pool:peek(infinity)),

            %% The first block needs to be a key-block
            {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PK),
            {ok, KeyHash1} = aec_blocks:hash_internal_representation(KeyBlock1),
            {ok,_} = aec_chain_state:insert_block(KeyBlock1),
            ?assertEqual(KeyHash1, aec_chain:top_block_hash()),
            ok = aec_keys:promote_candidate(aec_blocks:miner(KeyBlock1)),

            TopBlock = aec_chain:top_block(),
            TopBlockHash = aec_chain:top_block_hash(),

            {ok, USCandidate1, _} = aec_block_micro_candidate:create(TopBlock),
            {ok, Candidate1} = aec_keys:sign_micro_block(USCandidate1),
            {ok, CHash1} = aec_blocks:hash_internal_representation(Candidate1),
            {ok,_} = aec_chain_state:insert_block(Candidate1),
            aec_tx_pool:top_change(micro, TopBlockHash, CHash1),

            ?assertMatch({ok, [_]}, aec_tx_pool:peek(infinity)), %% nonoce=5 still in mempool

            ?assertEqual({error, nonce_too_low}, aec_tx_pool:push( a_signed_tx(PK, me, 1, 20000) )),
            ?assertEqual(ok, aec_tx_pool:push( a_signed_tx(PK, me, 6, 20000) )),
            ?assertEqual(ok, aec_tx_pool:push( a_signed_tx(PK, me, 7, 20000) )),
            ?assertEqual({error, nonce_too_high}, aec_tx_pool:push( a_signed_tx(PK, me, 8, 20000) )),
            ok
       end},
      {"ensure nonce is not checked when syncing",
       fun() ->
            PK0 = new_pubkey(),
            ?assertEqual(none, aec_chain:get_account(PK0)),
            ?assertEqual(ok,                     aec_tx_pool:push( a_signed_tx(PK0, me, 1, 20000), tx_received )),
            ?assertEqual({error,nonce_too_high}, aec_tx_pool:push( a_signed_tx(PK0, me, 2, 20000) )),
            ?assertEqual(ok,                     aec_tx_pool:push( a_signed_tx(PK0, me, 2, 20000), tx_received )),

            aec_test_utils:stop_chain_db(),
            PK1 = new_pubkey(),
            meck:expect(aec_fork_block_settings, genesis_accounts, 0, [{PK1, 100000}]),
            aec_consensus:set_genesis_hash(),
            {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
            aec_test_utils:start_chain_db(),
            {ok,_} = aec_chain_state:insert_block(GenesisBlock),
            ?assertMatch({value, _}, aec_chain:get_account(PK1)),
            ?assertEqual(ok, aec_tx_pool:push( a_signed_tx(PK1, me, 1, 20000) )),
            ?assertEqual(ok, aec_tx_pool:push( a_signed_tx(PK1, me, 2, 20000) )),
            ?assertEqual(ok, aec_tx_pool:push( a_signed_tx(PK1, me, 5, 20000) )),
            ?assertEqual({error, nonce_too_high}, aec_tx_pool:push( a_signed_tx(PK1, me, 6, 20000) )),
            ?assertEqual(ok, aec_tx_pool:push( a_signed_tx(PK1, me, 6, 20000), tx_received )),

            ?assertMatch({ok, [_, _, _, _, _, _]}, aec_tx_pool:peek(infinity)),

            %% The first block needs to be a key-block
            {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PK1),
            {ok, KeyHash1} = aec_blocks:hash_internal_representation(KeyBlock1),
            {ok,_} = aec_chain_state:insert_block(KeyBlock1),
            ?assertEqual(KeyHash1, aec_chain:top_block_hash()),
            ok = aec_keys:promote_candidate(aec_blocks:miner(KeyBlock1)),

            TopBlock = aec_chain:top_block(),
            TopBlockHash = aec_chain:top_block_hash(),

            {ok, USCandidate1, _} = aec_block_micro_candidate:create(TopBlock),
            {ok, Candidate1} = aec_keys:sign_micro_block(USCandidate1),
            {ok, CHash1} = aec_blocks:hash_internal_representation(Candidate1),
            {ok,_} = aec_chain_state:insert_block(Candidate1),
            aec_tx_pool:top_change(micro, TopBlockHash, CHash1),

            ?assertMatch({ok, [_, _, _, _]}, aec_tx_pool:peek(infinity)),

            ?assertEqual({error, nonce_too_low}, aec_tx_pool:push( a_signed_tx(PK1, me, 1, 20000) )),
            ?assertEqual({error, nonce_too_low}, aec_tx_pool:push( a_signed_tx(PK1, me, 1, 20000), tx_received )),
            ?assertEqual(ok, aec_tx_pool:push( a_signed_tx(PK1, me, 7, 20000) )),
            ?assertEqual({error,nonce_too_high}, aec_tx_pool:push( a_signed_tx(PK1, me, 8, 20000) )),
            ?assertEqual(ok, aec_tx_pool:push( a_signed_tx(PK1, me, 8, 20000), tx_received )),
            ok
       end},
      {"fill micro block with transactions",
       {timeout, 10, fun() ->
               MaxNonce = 400,
               %% setup nonce offset for pubkey without account present
               ok = application:set_env(aecore, mempool_nonce_baseline, MaxNonce),

               %% No txs to serve to peers.
               ?assertEqual({ok, []}, aec_tx_pool:peek(1)),

               %% Tx received from a peer.
               PubKey = new_pubkey(),
               STxs = [ a_signed_tx(PubKey, me, Nonce, 20000, 10) || Nonce <- lists:seq(1,MaxNonce) ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- STxs ],

               GenesisHeight = aec_block_genesis:height(),
               GenesisProtocol = aec_block_genesis:version(),
               {ok, Hash} = aec_headers:hash_header(aec_block_genesis:genesis_header()),
               MaxGas = aec_governance:block_gas_limit(),
               {ok, STxs2} = aec_tx_pool:get_candidate(MaxGas, Hash),
               TotalGas = lists:sum([aetx:gas_limit(aetx_sign:tx(T), GenesisHeight, GenesisProtocol) || T <- STxs2 ]),
               MinGas = aetx:gas_limit(aetx_sign:tx(hd(STxs)), GenesisHeight, GenesisProtocol),

               %% No single tx would have fitted on top of this
               ?assert(MinGas > MaxGas - TotalGas),
               %% No txs further to the microblock limit were included
               ?assertMatch(X when X =< MaxGas, TotalGas)
       end}},
      {"fill micro block with and without previously rejected tx",
       {timeout, 10, fun() ->
               ok = application:set_env(aecore, mempool_nonce_offset, 600),
               aec_test_utils:stop_chain_db(),
               {ok, MinerPubKey} = aec_keys:pubkey(),
               PubKey1 = new_pubkey(),
               PubKey2 = new_pubkey(),
               meck:expect(aec_fork_block_settings, genesis_accounts, 0,
                           [{PubKey1, 20001}, {PubKey2, 20000000}]),
               aec_consensus:set_genesis_hash(),
               {Block0, _} = aec_block_genesis:genesis_block_with_state(),
               aec_test_utils:start_chain_db(),
               {ok,_} = aec_chain_state:insert_block(Block0),

               %% The first block needs to be a key-block
               {ok, KeyBlock} = aec_block_key_candidate:create(aec_chain:top_block(), MinerPubKey),
               {ok,_} = aec_chain_state:insert_block(KeyBlock),
               ok = aec_keys:promote_candidate(aec_blocks:miner(KeyBlock)),
               {ok, KeyHash} = aec_blocks:hash_internal_representation(KeyBlock),
               ?assertEqual(KeyHash, aec_chain:top_block_hash()),

               %% PubKey1 creates two transaction but has enough fund only for one
               %% PubKey2 has enough fund for all it's transactions
               Txs1 = [Tx1_1, Tx1_2]  = [ a_signed_tx(PubKey1, me, Nonce, 20000, 10) || Nonce <- lists:seq(1, 2) ],
               Txs2 = [Tx2_1, Tx2_2]  = [ a_signed_tx(PubKey2, me, Nonce, 20000, 10) || Nonce <- lists:seq(1, 2) ],
               [ ok = aec_tx_pool:push(Tx, tx_created) || Tx <- Txs1++Txs2 ],
               ?assertMatch([_,_,_,_], aec_tx_pool:peek_db()),
               ?assertEqual([], aec_tx_pool:peek_visited()),

               %% Micro block candidate contains all txs because
               %% total gas for them is smaller then aec_governance:block_gas_limit()
               {ok, CTxs1} = aec_tx_pool:get_candidate(aec_governance:block_gas_limit(), KeyHash),
               ?assertEqual(lists:sort(Txs1++Txs2), lists:sort(CTxs1)),
               ?assertEqual([], aec_tx_pool:peek_db()),
               ?assertMatch([_,_,_,_], aec_tx_pool:peek_visited()),

               {ok, MicroCandidate, _} = aec_block_micro_candidate:create(KeyBlock),
               {ok, Micro} = aec_keys:sign_micro_block(MicroCandidate),
               {ok,_} = aec_chain_state:insert_block(Micro),
               {ok, MicroHash} = aec_blocks:hash_internal_representation(Micro),
               ?assertEqual(MicroHash, aec_chain:top_block_hash()),

               %% Micro block contains only valid transacions
               %% Tx1_2 is excluded because PubKey1 account has not enough funds
               ?assert(lists:member(Tx1_1, aec_blocks:txs(Micro))),
               ?assert(not lists:member(Tx1_2, aec_blocks:txs(Micro))),
               ?assert(lists:member(Tx2_1, aec_blocks:txs(Micro))),
               ?assert(lists:member(Tx2_2, aec_blocks:txs(Micro))),

               ?assertEqual([], aec_tx_pool:peek_db()),
               ?assertMatch([_,_,_,_], aec_tx_pool:peek_visited()),

               aec_tx_pool:top_change(micro, KeyHash, MicroHash),

               %% Invalid Txs1_2 tx is still in the pool
               ?assertEqual([], aec_tx_pool:peek_db()),
               ?assertMatch([Tx1_2], aec_tx_pool:peek_visited()),

               %% No new transaction: retry previously invalid tx
               ?assertEqual([], aec_tx_pool:peek_db()),
               {ok, CTxs2} = aec_tx_pool:get_candidate(aec_governance:block_gas_limit(), MicroHash),
               ?assertEqual([Tx1_2], CTxs2),

               %% Some new transacions (new tx gas < gas limit - invalid tx gas): use new + retry previously invalid tx
               ?assertEqual([], aec_tx_pool:peek_db()),
               Txs3 = [ a_signed_tx(PubKey2, me, Nonce, 20000, 10) || Nonce <- lists:seq(3, 103) ],
               [ ok = aec_tx_pool:push(Tx) || Tx <- Txs3 ],
               GenesisHeight = aec_block_genesis:height(),
               GenesisProtocol = aec_block_genesis:version(),
               TotalGas3 = lists:sum([ aetx:gas_limit(aetx_sign:tx(T), GenesisHeight, GenesisProtocol) || T <- [ Tx1_2 | Txs3 ] ]),
               ?assert(TotalGas3 =< aec_governance:block_gas_limit()),
               {ok, CTxs3} = aec_tx_pool:get_candidate(aec_governance:block_gas_limit(), MicroHash),
               ?assertEqual(lists:sort([ Tx1_2 | Txs3]), lists:sort(CTxs3)),

               %% New transacions only (new tx gas > gas limit): do not use invalid tx
               ?assertEqual([], aec_tx_pool:peek_db()),
               Txs4 = [ a_signed_tx(PubKey2, me, Nonce, 20000, 10) || Nonce <- lists:seq(104, 504) ],
               [ ok = aec_tx_pool:push(Tx) || Tx <- Txs4 ],
               TotalGas4 = lists:sum([ aetx:gas_limit(aetx_sign:tx(T), GenesisHeight, GenesisProtocol) || T <- Txs4 ]),
               ?assert(TotalGas4 > aec_governance:block_gas_limit()),
               {ok, CTxs4} = aec_tx_pool:get_candidate(aec_governance:block_gas_limit(), MicroHash),
               ?assert(not lists:member(Tx1_2, CTxs4)),

               ok
       end}},
      {"Mempool follows chain insertions and forks",
       fun() ->
               aec_test_utils:stop_chain_db(),
               %% Prepare a chain with specific genesis block with some funds
               PubKey1 = new_pubkey(),
               PubKey2 = new_pubkey(),
               meck:expect(aec_fork_block_settings, genesis_accounts, 0,
                  [{PubKey1, 100000}, {PubKey2, 100000}]),
               aec_consensus:set_genesis_hash(),
               {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
               aec_test_utils:start_chain_db(),
               {ok,_} = aec_chain_state:insert_block(GenesisBlock),

               %% The first block needs to be a key-block
               {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PubKey1),
               {ok, KeyHash1} = aec_blocks:hash_internal_representation(KeyBlock1),
               {ok,_} = aec_chain_state:insert_block(KeyBlock1),
               ?assertEqual(KeyHash1, aec_chain:top_block_hash()),
               ok = aec_keys:promote_candidate(aec_blocks:miner(KeyBlock1)),

               TopBlock = aec_chain:top_block(),
               TopBlockHash = aec_chain:top_block_hash(),

               %% Prepare a few txs.
               STx1 = a_signed_tx(PubKey1, new_pubkey(), 1, 20000),
               STx2 = a_signed_tx(PubKey1, new_pubkey(), 2, 20000),
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               ?assertEqual(ok, aec_tx_pool:push(STx2)),
               {ok, PoolTxs} = aec_tx_pool:peek(infinity),
               ?assertEqual(lists:sort([STx1, STx2]), lists:sort(PoolTxs)),

               %% Insert a block in chain.
               {ok, USCandidate1, _} = aec_block_micro_candidate:create(TopBlock),
               {ok, Candidate1} = aec_keys:sign_micro_block(USCandidate1),

               {ok, CHash1} = aec_blocks:hash_internal_representation(Candidate1),
               {ok,_} = aec_chain_state:insert_block(Candidate1),
               ?assertEqual(CHash1, aec_chain:top_block_hash()),

               %% Check that we uses all the txs in mempool
               Included = aec_blocks:txs(Candidate1),
               ?assertEqual(lists:sort(Included), lists:sort([STx1, STx2])),

               %% Ping tx_pool for top change
               aec_tx_pool:top_change(micro, TopBlockHash, CHash1),

               %% The mempool should now be empty
               ?assertEqual({ok, []}, aec_tx_pool:peek(infinity)),

               %% Create a fork
               %% First add a chain of two micro blocks with key blocks
               %% on top of each of them
               %% Ensure micro_block_cycle time
               meck:expect(aeu_time, now_in_msecs, fun() -> meck:passthrough([]) + 3000 end),
               STx3 = a_signed_tx(PubKey2, new_pubkey(), 1, 20000),
               ?assertEqual(ok, aec_tx_pool:push(STx3)),
               {ok, USCandidate3, _} = aec_block_micro_candidate:create(aec_chain:top_block()),
               {ok, Candidate3} = aec_keys:sign_micro_block(USCandidate3),

               {ok,_} = aec_chain_state:insert_block(Candidate3),
               TopBlockFork1 = aec_chain:top_block(),
               {ok, KeyBlock2} = aec_block_key_candidate:create(TopBlockFork1, PubKey1),
               {ok, CHashFork1} = aec_blocks:hash_internal_representation(KeyBlock2),

               meck:expect(aeu_time, now_in_msecs, fun() -> meck:passthrough([]) + 6000 end),
               STx4 = a_signed_tx(PubKey2, new_pubkey(), 2, 40000),
               ?assertEqual(ok, aec_tx_pool:push(STx4)),
               {ok, USCandidate4, _} = aec_block_micro_candidate:create(aec_chain:top_block()),
               {ok, Candidate4} = aec_keys:sign_micro_block(USCandidate4),

               {ok,_} = aec_chain_state:insert_block(Candidate4),
               TopBlockFork2 = aec_chain:top_block(),
               {ok, KeyBlock3} = aec_block_key_candidate:create(TopBlockFork2, PubKey1),
               {ok, CHashFork2} = aec_blocks:hash_internal_representation(KeyBlock3),

               %% Push the keyblock with the longest chain of micro blocks
               {ok,_} = aec_chain_state:insert_block(KeyBlock3),
               ?assertEqual(CHashFork2, aec_chain:top_block_hash()),
               aec_tx_pool:top_change(key, CHash1, CHashFork2),
               %% The mempool should now be empty
               ?assertEqual({ok, []}, aec_tx_pool:peek(infinity)),

               %% Ensure that the shorter fork takes over by
               %% increasing the difficulty
               meck:new(aec_blocks, [passthrough]),
               meck:expect(aec_headers, difficulty,
                           fun(B) -> meck:passthrough([B]) * 2 end),

               %% Push the keyblock with the shorter chain of micro blocks
               %% and check that it takes over.
               {ok,_} = aec_chain_state:insert_block(KeyBlock2),
               ?assertEqual(CHashFork1, aec_chain:top_block_hash()),

               %% Ping tx_pool for top change
               aec_tx_pool:top_change(key, CHashFork2, CHashFork1),

               %% The not included transaction should now be back in the pool
               ?assertEqual({ok, [STx4]}, aec_tx_pool:peek(infinity)),

               meck:unload(aec_headers),
               ok
       end},
      {"Ensure ordering",
       fun() ->
                 aec_test_utils:stop_chain_db(),
                 %% We should sort by fee, but preserve the order of nonces for each sender
                 PK1 = new_pubkey(),
                 PK2 = new_pubkey(),
                 PK3 = new_pubkey(),
                 PK4 = new_pubkey(),
                 PK5 = new_pubkey(),

                 meck:expect(aec_fork_block_settings, genesis_accounts, 0,
                             [{PK1, 100000}, {PK2, 100000}, {PK3, 100000}, {PK4, 100000},
                              {PK5, 10000000000000000000000}]),
                 aec_consensus:set_genesis_hash(),
                 GeneralizedAccounts = [PK5],
                 meck:expect(aec_accounts, type,
                             fun(Account) ->
                                 Pubkey = aec_accounts:pubkey(Account),
                                 case lists:member(Pubkey,
                                                   GeneralizedAccounts) of
                                    true -> generalized;
                                    false -> basic
                                 end
                             end),
                 {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
                 aec_test_utils:start_chain_db(),
                 {ok,_} = aec_chain_state:insert_block(GenesisBlock),

                 %% Bring the chain to height 1
                 {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PK1),
                 {ok,_} = aec_chain_state:insert_block(KeyBlock1),
                 WithMetaTx = aec_hard_forks:protocol_effective_at_height(1) >= ?FORTUNA_PROTOCOL_VSN,

                 STxs =
                   [ a_signed_tx        (_Sender=PK1, me,_Nonce=1,_Fee=300000)
                   , a_signed_tx        (        PK1, me,       2,     400000)
                   , a_signed_tx        (        PK1, me,       3,     500000)
                   , a_signed_tx        (        PK2, me,       2,     700000)
                   , a_signed_tx        (        PK2, me,       1,     800000)
                   , signed_ct_create_tx(        PK4,           1,     300000,_GasPrice=1100000000)
                   , signed_ct_call_tx  (        PK4,           2,     600000,          9000000000)
                   , signed_ct_call_tx  (        PK4,           3,     900000,          1)
                   ] ++
                   [ a_meta_tx          (        PK5,                  350000,          1,      1) || WithMetaTx ] ++
                   [ a_meta_tx          (        PK5,                  350000,          1,  51000) || WithMetaTx ] ++
                   [ a_meta_tx          (        PK5,                  299999, 1010000000,      1) || WithMetaTx ],

                 [?assertEqual(ok, aec_tx_pool:push(Tx)) || Tx <- STxs],
                 {ok, CurrentMempoolSigned} = aec_tx_pool:peek(20),
                 %% extract transactions without verification
                 CurrentMempool = [ aetx_sign:tx(STx) || STx <- CurrentMempoolSigned ],

                 MempoolOrder = [{aetx:origin(Tx), aetx:nonce(Tx)} || Tx <- CurrentMempool],
                 %% this is not-optimal order: transactions for PK1 and PK4 are invalid in that order
                 CorrectOrder0 = [{PK4,3},{PK2,1},{PK2,2},{PK4,2},{PK1,3},{PK5,0},{PK1,2},{PK5,0},{PK4,1},{PK5,0},{PK1,1}],
                 CorrectOrder  = [{PK, N} || {PK, N} <- CorrectOrder0, PK /= PK5 orelse WithMetaTx ],

                 ?assertEqual(CorrectOrder, MempoolOrder),

                 %% check if we track nonces correctly
                 MaxNonce = aec_tx_pool:get_max_nonce(PK1),
                 ?assertEqual({ok,3}, MaxNonce),

                 NotExistingSender = aec_tx_pool:get_max_nonce(PK3),
                 ?assertEqual(undefined, NotExistingSender)
             end},
      {"Mempool consistency",
       fun() ->
               PK = new_pubkey(),
               MaxGas = aec_governance:block_gas_limit(),
               TopBlockHash = aec_chain:top_block_hash(),
               STx1 = a_signed_tx(PK, me, Nonce1=1, _Fee1=20000),
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               ?assertEqual([], aec_tx_pool:peek_visited()),
               [{PK, Nonce1, _}] = aec_tx_pool:peek_nonces(),
               Size = aec_tx_pool:size(),
               ?assertEqual({ok, [STx1]},
                            aec_tx_pool:get_candidate(MaxGas, TopBlockHash)),
               ?assertEqual([STx1], aec_tx_pool:peek_visited()),
               ?assertEqual([], aec_tx_pool:peek_db()),
               ?assertEqual(Size, aec_tx_pool:size()),
               %% a 'key' top_change should restore visited to the mempool
               aec_tx_pool:top_change(key, TopBlockHash, TopBlockHash),
               ?assertEqual([], aec_tx_pool:peek_visited()),
               ?assertEqual([STx1], aec_tx_pool:peek_db()),
               ?assertEqual(Size, aec_tx_pool:size())
       end},
      {"Ensure candidate ordering",
       fun() ->
               aec_test_utils:stop_chain_db(),
               PK = new_pubkey(),
               PK2 = new_pubkey(),
               meck:expect(aec_fork_block_settings, genesis_accounts, 0,
                           [{PK, 100000000}, {PK2, 10000000000}]),
               aec_consensus:set_genesis_hash(),
               {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
               aec_test_utils:start_chain_db(),
               {ok,_} = aec_chain_state:insert_block(GenesisBlock),

               %% Bring the chain to height 1
               {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PK),
               {ok,_} = aec_chain_state:insert_block(KeyBlock1),

               MaxGas = aec_governance:block_gas_limit(),

               %% Only one tx in pool
               STx1 = a_signed_tx(PK, me, Nonce1=1,_Fee1=20000),
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               ?assertEqual({ok, [STx1]}, aec_tx_pool:get_candidate(MaxGas, aec_chain:top_block_hash())),

               aec_tx_pool:restore_mempool(),
               %% Order by nonce even if fee is higher
               STx2 = a_signed_tx(PK, me, Nonce2=2, Fee2=200000),
               ?assertEqual(ok, aec_tx_pool:push(STx2)),

               ?assertEqual({ok, [STx1, STx2]}, aec_tx_pool:get_candidate(MaxGas, aec_chain:top_block_hash())),

               aec_tx_pool:restore_mempool(),
               %% Replace same nonce with the higher fee
               STx3 = a_signed_tx(PK, me, Nonce1=1, 20000),
               ?assertEqual(ok, aec_tx_pool:push(STx3)),
               ?assertEqual({ok, [STx3, STx2]}, aec_tx_pool:get_candidate(MaxGas, aec_chain:top_block_hash())),

               aec_tx_pool:restore_mempool(),
               %% Replace same nonce with same fee but positive gas price (gas price of transaction without gas price is considered zero)
               STx4 = signed_ct_create_tx(PK, Nonce2=2, Fee2=200000,_GasPrice4=1100000000),
               ?assertEqual(ok, aec_tx_pool:push(STx4)),
               ?assertEqual({ok, [STx3, STx4]}, aec_tx_pool:get_candidate(MaxGas, aec_chain:top_block_hash())),

               aec_tx_pool:restore_mempool(),
               %% Replace same nonce with same fee but higher gas price
               STx5 = signed_ct_create_tx(PK, Nonce2=2, Fee2=200000, 2000000000),
               ?assertEqual(ok, aec_tx_pool:push(STx5)),
               ?assertEqual({ok, [STx3, STx5]}, aec_tx_pool:get_candidate(MaxGas, aec_chain:top_block_hash())),

               aec_tx_pool:restore_mempool(),
               %% Order by nonce even if fee and gas price are higher
               STx6 = signed_ct_call_tx(PK, _Nonce6=3,_Fee6=1000000,_GasPrice6=9000000000),
               ?assertEqual(ok, aec_tx_pool:push(STx6)),
               ?assertEqual({ok, [STx3, STx5, STx6]}, aec_tx_pool:get_candidate(MaxGas, aec_chain:top_block_hash())),

               %% If applicable, add a MetaTx
               case aec_hard_forks:protocol_effective_at_height(1) >= ?FORTUNA_PROTOCOL_VSN of
                   true ->
                       aec_tx_pool:restore_mempool(),
                       meck:expect(aec_accounts, type,
                                   fun(Account) ->
                                       Pubkey = aec_accounts:pubkey(Account),
                                       case Pubkey =:= PK2 of
                                           true -> generalized;
                                           false -> basic
                                       end
                                   end),
                       STx7 = a_meta_tx(PK2, 200000, 1, 1),
                       ?assertEqual(ok, aec_tx_pool:push(STx7)),
                       ?assertEqual({ok, [STx3, STx5, STx6, STx7]},
                                    aec_tx_pool:get_candidate(MaxGas, aec_chain:top_block_hash()));
                    false ->
                       ok
               end,

               ok
       end},
      {"Ensure block gas limit",
       fun() ->
               PK1 = new_pubkey(),
               PK2 = new_pubkey(),
               PK3 = new_pubkey(),

               %% Prepare 3 txs:
               %% 1st tx has the lowest gas
               %% Depends on aec_geovernance settings whether 2nd or 3rd is largest
               STx1 = a_signed_tx(        me,   PK1,      1, 20000),
               STx2 = signed_ct_create_tx(PK2,    1, 800000,  1000),
               STx3 = signed_ct_call_tx(  PK3,    1, 800000,  1000),

               Height = 0,
               Protocol = aec_hard_forks:protocol_effective_at_height(0),
               GasTx1 = aetx:gas_limit(aetx_sign:tx(STx1), Height, Protocol),
               GasTx2 = aetx:gas_limit(aetx_sign:tx(STx2), Height, Protocol),
               GasTx3 = aetx:gas_limit(aetx_sign:tx(STx3), Height, Protocol),

               ?assert(GasTx2 > GasTx1),
               ?assert(GasTx3 > GasTx1),
               {MinGasTx, MaxGasTx} =
                   case GasTx2 > GasTx3 of
                       true -> { {STx3, GasTx3}, {STx2, GasTx2} };
                       false ->  { {STx2, GasTx2}, {STx3, GasTx3} }
                     end,

               %% Push all txs to the pool.
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               ?assertEqual(ok, aec_tx_pool:push(STx2)),
               ?assertEqual(ok, aec_tx_pool:push(STx3)),

               %% Do not get any tx - not enough gas.
               ?assertEqual({ok, []},
                            aec_tx_pool:get_candidate(GasTx1 - 1, aec_chain:top_block_hash())),

               %% Get 1st tx only.
               ?assertEqual({ok, [STx1]},
                            aec_tx_pool:get_candidate(GasTx1, aec_chain:top_block_hash())),

               aec_tx_pool:restore_mempool(),

               %% Get only 2 txs, the 1st + 2nd or 1st + 3rd.
               {_, Max} = MaxGasTx,
               {ok, STxs1} = aec_tx_pool:get_candidate(GasTx1 + Max, aec_chain:top_block_hash()),
               ?assert(lists:member(STx1, STxs1) and (lists:member(STx2, STxs1) or lists:member(STx3, STxs1))),

               aec_tx_pool:restore_mempool(),
               %% Get all 3 txs by providing exactly the gas the txs need.
               {ok, STxs2} = aec_tx_pool:get_candidate(GasTx1 + GasTx2 + GasTx3, aec_chain:top_block_hash()),
               ?assert(lists:member(STx1, STxs2)),
               ?assert(lists:member(STx2, STxs2)),
               ?assert(lists:member(STx3, STxs2)),

               aec_tx_pool:restore_mempool(),

               %% Get 1st and 3rd tx, skip 2nd tx.
               {MinSTx, Min} = MinGasTx,
               {ok, STxs3} = aec_tx_pool:get_candidate(GasTx1 + Min, aec_chain:top_block_hash()),
               ?assert(lists:member(STx1, STxs3)),
               ?assert(lists:member(MinSTx, STxs3)),

               ok
       end},
      {"Ensure persistence",
       fun() ->
               aec_test_utils:stop_chain_db(),
               PK = new_pubkey(),
               meck:expect(aec_fork_block_settings, genesis_accounts, 0,
                           [{PK, 100000}]),
               aec_consensus:set_genesis_hash(),
               {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
               aec_test_utils:start_chain_db(),

               {ok,_} = aec_chain_state:insert_block(GenesisBlock),
               %% Prepare a few txs.
               STx1 = a_signed_tx(PK, new_pubkey(), 1, 20000),
               STx2 = a_signed_tx(PK, new_pubkey(), 2, 20000),
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
               %% setup nonce offset
               ok = application:set_env(aecore, mempool_nonce_offset, 100),

               aec_test_utils:stop_chain_db(),
               %% Prepare a chain with specific genesis block with some funds
               PubKey1 = new_pubkey(),
               PubKey2 = new_pubkey(),
               meck:expect(aec_fork_block_settings, genesis_accounts, 0,
                  [{PubKey1, 100000}, {PubKey2, 100000}]),
               aec_consensus:set_genesis_hash(),
               {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
               aec_test_utils:start_chain_db(),
               {ok,_} = aec_chain_state:insert_block(GenesisBlock),

               %% The first block needs to be a key-block
               {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PubKey1),
               {ok, KeyHash1} = aec_blocks:hash_internal_representation(KeyBlock1),
               {ok,_} = aec_chain_state:insert_block(KeyBlock1),
               ?assertEqual(KeyHash1, aec_chain:top_block_hash()),
               ok = aec_keys:promote_candidate(aec_blocks:miner(KeyBlock1)),

               TopBlock = aec_chain:top_block(),

               %% Add a transaction to the chain
               STx1 = a_signed_tx(PubKey1, new_pubkey(), 1, 20000),
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               {ok, USCandidate1, _} = aec_block_micro_candidate:create(TopBlock),
               {ok, Candidate1} = aec_keys:sign_micro_block(USCandidate1),
               {ok, Top} = aec_blocks:hash_internal_representation(Candidate1),
               {ok,_} = aec_chain_state:insert_block(Candidate1),
               ?assertEqual(Top, aec_chain:top_block_hash()),

               %% Now we should reject the same transaction since it
               %% is already in the chain
               ?assertEqual({error, already_accepted},
                            aec_tx_pool:push(STx1)),

               %% A transaction with too low nonce should be rejected
               STx2 = a_signed_tx(PubKey1, new_pubkey(), 1, 20000),
               ?assertEqual({error, nonce_too_low},
                            aec_tx_pool:push(STx2)),

               %% A transaction with too high nonce should _NOT_ be rejected
               STx3 = a_signed_tx(PubKey1, new_pubkey(), 5, 20000),
               ?assertEqual(ok, aec_tx_pool:push(STx3)),

               %% A transaction with too low fee should be rejected
               STx4 = a_signed_tx(PubKey1, new_pubkey(), 6, 0),
               ?assertEqual({error, too_low_fee}, aec_tx_pool:push(STx4)),

               %% A transaction with too low gas price should be rejected
               meck:expect(aec_governance, minimum_gas_price, 1, 1),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_create_tx(PubKey1, 10, 1000000, 1))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_call_tx  (PubKey1, 20, 1000000, 1))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_create_tx(PubKey1, 11, 2000000, 2))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_call_tx  (PubKey1, 21, 2000000, 2))),
               meck:expect(aec_governance, minimum_gas_price, 1, 2),
               ?assertEqual({error, too_low_gas_price}, aec_tx_pool:push(signed_ct_create_tx(PubKey1, 12, 2000000, 0))),
               ?assertEqual({error, too_low_gas_price}, aec_tx_pool:push(signed_ct_call_tx  (PubKey1, 22, 2000000, 0))),
               ?assertEqual({error, too_low_gas_price}, aec_tx_pool:push(signed_ct_create_tx(PubKey1, 13, 2000000, 1))),
               ?assertEqual({error, too_low_gas_price}, aec_tx_pool:push(signed_ct_call_tx  (PubKey1, 23, 2000000, 1))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_create_tx(PubKey1, 14, 2000000, 2))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_call_tx  (PubKey1, 24, 2000000, 2))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_create_tx(PubKey1, 15, 4000000, 3))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_call_tx  (PubKey1, 25, 4000000, 3))),
               meck:expect(aec_tx_pool, minimum_miner_gas_price, 0, 3),
               ?assertEqual({error, too_low_gas_price_for_miner}, aec_tx_pool:push(signed_ct_create_tx(PubKey1, 16, 2000000, 2))),
               ?assertEqual({error, too_low_gas_price_for_miner}, aec_tx_pool:push(signed_ct_call_tx  (PubKey1, 26, 2000000, 2))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_create_tx(PubKey1, 17, 4000000, 3))),
               ?assertEqual(ok, aec_tx_pool:push(signed_ct_call_tx  (PubKey1, 27, 4000000, 3))),

               %% A transaction with too low ttl should be rejected
               %% First add another block to make the chain high enough to
               %% fail on TTL
               {ok, Candidate2} = aec_block_key_candidate:create(aec_chain:top_block(), PubKey1),
               {ok, Top2} = aec_blocks:hash_internal_representation(Candidate2),
               {ok,_} = aec_chain_state:insert_block(Candidate2),
               ?assertEqual(Top2, aec_chain:top_block_hash()),

               STx5 = a_signed_tx(PubKey1, new_pubkey(), 6, 40000, 1),
               ?assertEqual({error, ttl_expired}, aec_tx_pool:push(STx5)),

               ok
       end},
       {"Test GC",
        fun() ->
            %% initialize chain
            aec_test_utils:stop_chain_db(),

            PubKey = new_pubkey(),
            meck:expect(aec_fork_block_settings, genesis_accounts, 0,
                        [{PubKey, 100000}]),
            aec_consensus:set_genesis_hash(),
            {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
            aec_test_utils:start_chain_db(),
            {ok,_} = aec_chain_state:insert_block(GenesisBlock),

            %% Prepare three transactions
            STx1 = a_signed_tx(PubKey, PubKey, 1, 20000),
            STx2 = a_signed_tx(PubKey, PubKey, 2, 20000),
            STx3 = a_signed_tx(PubKey, PubKey, 3, 20000),

            %% Post them
            ?assertEqual(ok, aec_tx_pool:push(STx1)),
            ?assertEqual(ok, aec_tx_pool:push(STx2)),
            ?assertEqual(ok, aec_tx_pool:push(STx3)),

            ?assertMatch({ok, [_, _, _]}, aec_tx_pool:peek(infinity)),

            %% Txs will be scheduled for
            %% removal at Height + ?TX_TTL
            %% For test ?TX_TTL = 8

            %% Doing a garbage collect at height 0 shouldn't affect
            tx_pool_gc(0),
            ?assertMatch({ok, [_, _, _]}, aec_tx_pool:peek(infinity)),

            %% At 4 still GC should not kick in.
            tx_pool_gc(4),
            ?assertMatch({ok, [_, _, _]}, aec_tx_pool:peek(infinity)),

            %% At 8, now TXs should be dropped.
            tx_pool_gc(8),
            ?assertMatch({ok, []}, aec_tx_pool:peek(infinity))

        end},
      {"Test Origins cache GC",
       fun() ->
               %% Initialize chain
               aec_test_utils:stop_chain_db(),

               PubKey = new_pubkey(),
               meck:expect(aec_fork_block_settings, genesis_accounts, 0,
                           [{PubKey, 100000}]),
               aec_consensus:set_genesis_hash(),
               {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
               aec_test_utils:start_chain_db(),
               {ok,_} = aec_chain_state:insert_block(GenesisBlock),

               %% The first block needs to be a key-block
               {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PubKey),
               {ok, KeyHash1} = aec_blocks:hash_internal_representation(KeyBlock1),
               {ok,_} = aec_chain_state:insert_block(KeyBlock1),
               ?assertEqual(KeyHash1, aec_chain:top_block_hash()),
               ok = aec_keys:promote_candidate(aec_blocks:miner(KeyBlock1)),

               %% Prepare transactions
               STx11 = a_signed_tx(PubKey, PubKey, 1, 20000),
               STx12 = a_signed_tx(PubKey, PubKey, 1, 30000),
               STx21 = a_signed_tx(PubKey, PubKey, 2, 20000),
               STx22 = a_signed_tx(PubKey, PubKey, 2, 30000),
               STx31 = a_signed_tx(PubKey, PubKey, 3, 20000),
               STx32 = a_signed_tx(PubKey, PubKey, 3, 30000),

               %% Post transactions
               ?assertEqual(ok, aec_tx_pool:push(STx11)),
               ?assertEqual(ok, aec_tx_pool:push(STx12)),
               ?assertEqual(ok, aec_tx_pool:push(STx21)),
               ?assertEqual(ok, aec_tx_pool:push(STx22)),
               ?assertEqual(ok, aec_tx_pool:push(STx31)),
               ?assertEqual(ok, aec_tx_pool:push(STx32)),

               ?assertMatch({ok, [_, _, _, _, _, _]}, aec_tx_pool:peek(infinity)),

               %% Add transactions to the chain
               TopBlock = aec_chain:top_block(),
               {ok, USCandidate1, _} = aec_block_micro_candidate:create(TopBlock),
               {ok, Candidate1} = aec_keys:sign_micro_block(USCandidate1),
               {ok, Top} = aec_blocks:hash_internal_representation(Candidate1),
               {ok,_} = aec_chain_state:insert_block(Candidate1),
               ?assertEqual(Top, aec_chain:top_block_hash()),
               aec_tx_pool:top_change(micro, KeyHash1, Top),

               %% Post more transactions from the same origin
               STx41 = a_signed_tx(PubKey, PubKey, 4, 20000),
               STx42 = a_signed_tx(PubKey, PubKey, 4, 30000),
               ?assertEqual(ok, aec_tx_pool:push(STx41)),
               ?assertEqual(ok, aec_tx_pool:push(STx42)),

               %% Transactions with higher fee made it into the chain
               ?assertEqual({error, already_accepted}, aec_tx_pool:push(STx12)),
               ?assertEqual({error, already_accepted}, aec_tx_pool:push(STx22)),
               ?assertEqual({error, already_accepted}, aec_tx_pool:push(STx32)),

               %% The rest is in the mempool
               ?assertMatch({ok, [_, _, _, _, _]}, aec_tx_pool:peek(infinity)),

               %% GC removes stale transactions with nonce lower than 4
               ok = aec_tx_pool_gc:origins_cache_gc(),

               %% Only transactions with nonce=4 are not GCed
               ?assertMatch({ok, [STx42, STx41]}, aec_tx_pool:peek(infinity))
       end}
     ]}.

tx_pool_gc(Height) ->
    aec_tx_pool:sync_garbage_collect(Height).

a_signed_tx(Sender, Recipient, Nonce, Fee) ->
    a_signed_tx(Sender, Recipient, Nonce, Fee,0).

a_signed_tx(Sender, Recipient, Nonce, Fee, TTL) ->
    {ok, Tx} = a_spend_tx(Sender, Recipient, Nonce, Fee, TTL),
    {ok, STx} = sign(Sender, Tx),
    STx.

a_spend_tx(Sender, Recipient, Nonce, Fee, TTL) ->
    aec_spend_tx:new(#{sender_id => acct(Sender),
                       recipient_id => acct(Recipient),
                       amount => 1,
                       nonce => Nonce,
                       fee => Fee,
                       ttl => TTL,
                       payload => <<"">>}).

signed_ct_create_tx(Sender, Nonce, Fee, GasPrice) ->
    Spec =
        #{ fee         => Fee
         , owner_id    => aeser_id:create(account, Sender)
         , nonce       => Nonce
         , code        => <<"NOT PROPER BYTE CODE">>
         , vm_version  => ?VM_AEVM_SOPHIA_2
         , abi_version => ?ABI_AEVM_SOPHIA_1
         , deposit     => 10
         , amount      => 200
         , gas         => 100000
         , gas_price   => GasPrice
         , call_data   => <<"NOT ENCODED ACCORDING TO ABI">>
         , ttl         => 0
         },
    {ok, Tx} = aect_create_tx:new(Spec),
    {ok, STx} = sign(Sender, Tx),
    STx.

signed_ct_call_tx(Sender, Nonce, Fee, GasPrice) ->
    ContractId = aeser_id:create(contract, <<"contract_address......(32 bytes)">>),
    Spec =
        #{ fee         => Fee
         , contract_id => ContractId
         , caller_id   => aeser_id:create(account, Sender)
         , nonce       => Nonce
         , abi_version => 1
         , amount      => 100
         , gas         => 50000
         , gas_price   => GasPrice
         , call_data   => <<"CALL DATA">>
         , ttl         => 0
         },
    {ok, Tx} = aect_call_tx:new(Spec),
    {ok, STx} = sign(Sender, Tx),
    STx.

a_meta_tx(Sender, OuterFee, GasPrice, InnerFee) ->
    {ok, Tx} = a_spend_tx(Sender, Sender, 0, InnerFee, 0),
    STx = aetx_sign:new(Tx, []),
    Opts0 =
        #{ga_id       => aeser_id:create(account, Sender),
          auth_data   => <<"">>,
          abi_version => 1,
          gas         => 20000,
          gas_price   => GasPrice,
          fee         => OuterFee,
          tx          => STx},
    Opts =
        case aecore_suite_utils:latest_protocol_version() >= ?IRIS_PROTOCOL_VSN of
            true -> Opts0;
            false -> Opts0#{ttl => 0}
        end,
    {ok, MTx} = aega_meta_tx:new(Opts),
    aetx_sign:new(MTx, []).

sign(me, Tx) ->
    {ok, PrivKey} = aec_keys:sign_privkey(),
    {ok, aec_test_utils:sign_tx(Tx, PrivKey)};
sign(PubKey, Tx) ->
    try
        [{_, PrivKey}] = ets:lookup(?TAB, PubKey),
        {ok, Trees} = aec_chain:get_top_state(),
        {ok, Signers} = aetx:signers(Tx, Trees),
        true = lists:member(PubKey, Signers),
        {ok, aec_test_utils:sign_tx(Tx, PrivKey)}
    catch error:Err:StackTrace ->
        erlang:error({Err, StackTrace})
    end.

acct(me) ->
    {ok, Key} = aec_keys:pubkey(),
    aeser_id:create(account, Key);
acct(A) when is_binary(A) ->
    aeser_id:create(account, A).

new_pubkey() ->
    {Pub, Priv} = keypair(),
    ets:insert(?TAB, {Pub, Priv}),
    Pub.

keypair() ->
    #{ public := Pub, secret := Priv } = enacl:sign_keypair(),
    {Pub, Priv}.
