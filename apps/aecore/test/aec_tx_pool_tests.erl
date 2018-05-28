%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_tx_pool_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").

-define(TAB, aec_tx_pool_test_keys).

tx_pool_test_() ->
    {foreach,
     fun() ->
             application:ensure_started(gproc),
             ok = application:ensure_started(crypto),
             TmpKeysDir = aec_test_utils:aec_keys_setup(),
             aec_test_utils:start_chain_db(),
             aec_test_utils:mock_genesis(),
             GB = aec_test_utils:genesis_block(),
             aec_chain_state:insert_block(GB),
             aec_test_utils:mock_block_target_validation(),
             {ok, _} = aec_tx_pool:start_link(),
             %% Start `aec_keys` merely for generating realistic test
             %% signed txs - as a node would do.
             ets:new(?TAB, [public, ordered_set, named_table]),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir),
             ok = application:stop(gproc),
             ets:delete(?TAB),
             aec_test_utils:stop_chain_db(),
             aec_test_utils:unmock_genesis(),
             aec_test_utils:unmock_block_target_validation(),
             ok = aec_tx_pool:stop(),
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
               {ok, Candidate1, _} = aec_block_candidate:create(TopBlock),
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
               STx3 = a_signed_tx(PubKey2, new_pubkey(), 1, 1),
               STx4 = a_signed_tx(PubKey2, new_pubkey(), 2, 1),
               ?assertEqual(ok, aec_tx_pool:push(STx3)),
               ?assertEqual(ok, aec_tx_pool:push(STx4)),
               {ok, Candidate2, _} = aec_block_candidate:create(TopBlock),
               {ok, CHash2} = aec_blocks:hash_internal_representation(Candidate2),

               %% Ensure that the new fork takes over by
               %% increasing the difficulty
               meck:new(aec_blocks, [passthrough]),
               meck:expect(aec_headers, difficulty,
                           fun(B) -> meck:passthrough([B]) * 2 end),

               ok = aec_chain_state:insert_block(Candidate2),

               %% The new fork took over
               ?assertEqual(CHash2, aec_chain:top_block_hash()),

               %% Ping tx_pool for top change
               aec_tx_pool:top_change(CHash1, CHash2),

               %% The old transactions should now be back in the pool
               {ok, PoolTxs2} = aec_tx_pool:peek(infinity),
               Sorted2 = lists:sort(PoolTxs2),
               ?assertEqual(lists:sort([STx1, STx2]), Sorted2),


               meck:unload(aec_headers),
               ok
       end},
      {"Ensure ordering",
       fun() ->
                 %% We should sort by fee, but preserve the order of nonces for each sender
                 PK1 = new_pubkey(),
                 PK2 = new_pubkey(),
                 PK3 = new_pubkey(),
                 STx1 = a_signed_tx(PK1, me, 1, 1),
                 STx2 = a_signed_tx(PK1, me, 2, 2),
                 STx3 = a_signed_tx(PK1, me, 3, 3),
                 STx4 = a_signed_tx(PK2, me, 2, 5),
                 STx5 = a_signed_tx(PK2, me, 1, 6),

                 [?assertEqual(ok, aec_tx_pool:push(Tx)) || Tx <- [STx1, STx2, STx3, STx4, STx5]],
                 {ok, CurrentMempoolSigned} = aec_tx_pool:peek(10),
                 %% extract transactions without verification
                 CurrentMempool = [ aetx_sign:tx(STx) || STx <- CurrentMempoolSigned ],

                 MempoolOrder = [{aetx:origin(Tx), aetx:nonce(Tx)} || Tx <- CurrentMempool],
                 %% this is not-optimal order: transactions for PK1 are invalid in that order
                 CorrectOrder = [{PK2,1},{PK2,2},{PK1,3},{PK1,2},{PK1,1}],

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
               STx1 = a_signed_tx(PK, me, 1, 1),
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               ?assertEqual({ok, [STx1]}, aec_tx_pool:get_candidate(10, <<>>)),

               %% Order by nonce even if fee is higher
               STx2 = a_signed_tx(PK, me, 2, 5),
               ?assertEqual(ok, aec_tx_pool:push(STx2)),
               ?assertEqual({ok, [STx1, STx2]}, aec_tx_pool:get_candidate(10, <<>>)),

               %% Replace same nonce with the higher fee
               STx3 = a_signed_tx(PK, me, 1, 2),
               ?assertEqual(ok, aec_tx_pool:push(STx3)),
               ?assertEqual({ok, [STx3, STx2]}, aec_tx_pool:get_candidate(10, <<>>)),

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
               TopBlock = aec_chain:top_block(),

               %% Add a transaction to the chain
               STx1 = a_signed_tx(PubKey1, new_pubkey(), 1, 1),
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               {ok, Candidate1, _} = aec_block_candidate:create(TopBlock),
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

               STx4 = a_signed_tx(PubKey1, new_pubkey(), 6,
                                  aec_governance:minimum_tx_fee() - 1),
               ?assertEqual({error, too_low_fee}, aec_tx_pool:push(STx4)),
               ok
       end}
     ]}.

a_signed_tx(Sender, Recipient, Nonce, Fee) ->
    {ok, Tx} = aec_spend_tx:new(#{sender => acct(Sender),
                                  recipient => acct(Recipient),
                                  amount => 1,
                                  nonce => Nonce, fee => Fee,
                                  payload => <<"">>}),
    {ok, STx} = sign(Sender, Tx),
    STx.

sign(me, Tx) ->
    aec_keys:sign(Tx);  %% why via keys here?
sign(PubKey, Tx) ->
    try
        [{_, PrivKey}] = ets:lookup(?TAB, PubKey),
        {ok, Trees} = aec_chain:get_top_state(),
        {ok, Signers} = aetx:signers(Tx, Trees),
        true = lists:member(PubKey, Signers),
        {ok, aetx_sign:sign(Tx, PrivKey)}
    catch
        error:Err ->
            erlang:error({Err, erlang:get_stacktrace()})
    end.

acct(me) ->
    {ok, Key} = aec_keys:pubkey(),
    Key;
acct(A) when is_binary(A) ->
    A.

new_pubkey() ->
    {Pub, Priv} = keypair(),
    ets:insert(?TAB, {Pub, Priv}),
    Pub.

keypair() ->
    #{ public := Pub, secret := Priv } = enacl:sign_keypair(),
    {Pub, Priv}.
