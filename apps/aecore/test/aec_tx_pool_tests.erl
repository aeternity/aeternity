%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_tx_pool_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

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
             meck:new(aec_accounts_trees, [passthrough]),
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
             ok = application:unset_env(aecore, mempool_future_nonce_tx_ttl),
             ok = application:unset_env(aecore, mempool_allow_reentry),
             meck:unload(aec_tx_pool),
             meck:unload(aec_jobs_queues),
             meck:unload(aec_accounts_trees),
             catch meck:unload(aec_chain), %% only mocked by some tests
             meck:unload(aec_accounts),
             meck:unload(aeu_time),
             ets:delete(?TAB),
             ok = aec_tx_pool:stop(),
             ok = aec_tx_pool_gc:stop(),
             aec_test_utils:unmock_governance(), %% Unloads aec_governance mock.
             aec_test_utils:unmock_genesis_and_forks(),
             aec_test_utils:stop_chain_db(),
             ok = aec_test_utils:aec_keys_cleanup(TmpKeysDir),
             ok = application:stop(gproc),
             ok = application:unset_env(aecore, beneficiary),
             ok
     end,
     [{"No txs in mempool",
       fun() ->
               ?assertEqual({ok, []}, aec_tx_pool:peek(1)),
               ?assertEqual({ok, []}, aec_tx_pool:peek(3)),
               ?assertEqual(0, aec_tx_pool:size())
       end},
      {"Push on empty chain db falls back to the genesis env",
       fun() ->
               %% Restart the chain db without inserting the genesis block:
               %% aec_chain has no top block node and get_onchain_env/0 must
               %% fall back to the genesis header and state.
               aec_test_utils:stop_chain_db(),
               aec_test_utils:start_chain_db(),
               ?assertEqual(undefined, aec_chain:top_header_hash_and_state()),

               Short = 2,
               ok = application:set_env(aecore, mempool_future_nonce_tx_ttl, Short),
               ?assert(Short < aec_tx_pool:tx_ttl()),
               {GCHeight, Dbs} = aec_tx_pool:gc_height_and_dbs(),

               STx = a_signed_tx(me, new_pubkey(), aec_tx_pool:nonce_offset() + 1, 20000),
               ?assertEqual(ok, aec_tx_pool:push(STx, tx_received)),
               ?assertEqual({ok, [STx]}, aec_tx_pool:peek(infinity)),
               %% The sender cannot be looked up either, so nothing about it can
               %% be judged too far ahead and the stay is the full one.
               ?assertEqual({ok, GCHeight + aec_tx_pool:tx_ttl()}, gc_ttl_of(STx, Dbs))
       end},
      {"Push rejects a tx with an invalid signature",
       fun() ->
               {ok, Tx} = a_spend_tx(new_pubkey(), new_pubkey(), 1, 20000, 0),
               {_WrongPub, WrongPriv} = keypair(),
               STx = aec_test_utils:sign_tx(Tx, WrongPriv),
               ?assertEqual({error, signature_check_failed},
                            aec_tx_pool:push(STx, tx_received)),
               ?assertEqual({ok, []}, aec_tx_pool:peek(infinity))
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
               ?assertEqual({error, already_known}, aec_tx_pool:push(STx1, tx_received)),
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
            persistent_term:put({aec_consensus_bitcoin_ng, whitelist}, #{}),
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
            {ok, Miner} = aec_keys:candidate_pubkey(),
            {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PK, Miner),
            {ok, KeyHash1} = aec_blocks:hash_internal_representation(KeyBlock1),
            {ok,_} = aec_chain_state:insert_block(KeyBlock1),
            ?assertEqual(KeyHash1, aec_chain:top_block_hash()),
            ?assertEqual(Miner, aec_blocks:miner(KeyBlock1)),
            ok = aec_keys:promote_candidate(aec_blocks:miner(KeyBlock1)),

            TopBlock = aec_chain:top_block(),
            TopBlockHash = aec_chain:top_block_hash(),

            {ok, USCandidate1, _} = aec_block_micro_candidate:create(TopBlock),
            {ok, Candidate1} = aec_keys:sign_micro_block(USCandidate1),
            {ok, CHash1} = aec_blocks:hash_internal_representation(Candidate1),
            {ok,_} = aec_chain_state:insert_block(Candidate1),
            aec_tx_pool:top_change(#{type => micro, old_hash => TopBlockHash,
                                     new_hash => CHash1}),

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
            {ok, Miner} = aec_keys:candidate_pubkey(),
            {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PK1, Miner),
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
            aec_tx_pool:top_change(#{type => micro, old_hash => TopBlockHash,
                                     new_hash => CHash1}),

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
               {ok, []} = aec_tx_pool:get_candidate(0, Hash),         % regression bug check
               {ok, STxs2} = aec_tx_pool:get_candidate(MaxGas, Hash),
               TotalGas = lists:sum([aetx:gas_limit(aetx_sign:tx(T), GenesisHeight, GenesisProtocol) || T <- STxs2 ]),
               MinGas = aetx:gas_limit(aetx_sign:tx(hd(STxs)), GenesisHeight, GenesisProtocol),

               %% No single tx would have fitted on top of this
               ?assert(MinGas > MaxGas - TotalGas),
               %% No txs further to the microblock limit were included
               ?assertMatch(X when X =< MaxGas, TotalGas)
       end}},
      {"Candidate selection stops walking the pool at its deadline",
       {timeout, 10, fun() ->
               NumTxs = 200,
               ok = application:set_env(aecore, mempool_nonce_baseline, NumTxs),
               PubKey = new_pubkey(),
               STxs = [ a_signed_tx(PubKey, me, Nonce, 20000, 10)
                        || Nonce <- lists:seq(1, NumTxs) ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- STxs ],

               {ok, Hash} = aec_headers:hash_header(aec_block_genesis:genesis_header()),
               MaxGas = aec_governance:block_gas_limit(),

               %% A deadline that has already passed. Selection tests it
               %% between ets:select chunks, so the first chunk still runs -
               %% what must not happen is a walk of the whole pool.
               Past = erlang:monotonic_time(millisecond) - 1,
               {ok, Truncated, TruncatedInfo} =
                   aec_tx_pool:get_candidate(MaxGas, #{}, Hash, #{deadline => Past}),
               ?assertMatch([_|_], Truncated),
               ?assert(length(Truncated) < NumTxs),
               %% And it says so, which is the only way the caller can tell a
               %% cut-short walk from a pool that had nothing more to offer.
               ?assertEqual(#{expired => true}, TruncatedInfo),

               %% The same pool, unbounded, offers everything - so the pool was
               %% not short of transactions, the deadline cut the walk short.
               {ok, All, AllInfo} =
                   aec_tx_pool:get_candidate(MaxGas, #{}, Hash, #{deadline => infinity}),
               ?assertEqual(NumTxs, length(All)),
               ?assertEqual(#{expired => false}, AllInfo)
       end}},
      {"An absent deadline selects exactly as infinity does",
       {timeout, 10, fun() ->
               NumTxs = 30,
               ok = application:set_env(aecore, mempool_nonce_baseline, NumTxs),
               PubKey = new_pubkey(),
               STxs = [ a_signed_tx(PubKey, me, Nonce, 20000, 10)
                        || Nonce <- lists:seq(1, NumTxs) ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- STxs ],

               {ok, Hash} = aec_headers:hash_header(aec_block_genesis:genesis_header()),
               MaxGas = aec_governance:block_gas_limit(),

               %% All three spellings must behave identically: the legacy
               %% arities are the unbounded case.
               {ok, Infinity, #{expired := false}} =
                   aec_tx_pool:get_candidate(MaxGas, #{}, Hash, #{deadline => infinity}),
               {ok, NoOpts, #{expired := false}} =
                   aec_tx_pool:get_candidate(MaxGas, #{}, Hash, #{}),
               {ok, Legacy}   = aec_tx_pool:get_candidate(MaxGas, Hash),
               ?assertEqual(NumTxs, length(Infinity)),
               ?assertEqual(lists:sort(Infinity), lists:sort(NoOpts)),
               ?assertEqual(lists:sort(Infinity), lists:sort(Legacy))
       end}},
      {"Candidate selection skips the ignored transactions",
       fun() ->
               ok = application:set_env(aecore, mempool_nonce_baseline, 10),
               PubKey = new_pubkey(),
               [STx1, STx2, STx3] = STxs =
                   [ a_signed_tx(PubKey, me, Nonce, 20000, 10) || Nonce <- lists:seq(1, 3) ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- STxs ],

               {ok, Hash} = aec_headers:hash_header(aec_block_genesis:genesis_header()),
               MaxGas = aec_governance:block_gas_limit(),
               Ignored = aetx_sign:hash(STx2),

               %% get_candidate/3 takes the list its callers have always
               %% passed; the selection fold consults the set as a map.
               {ok, FromList} = aec_tx_pool:get_candidate(MaxGas, [Ignored], Hash),
               {ok, FromMap, _} =
                   aec_tx_pool:get_candidate(MaxGas, #{Ignored => []}, Hash, #{}),
               ?assertEqual(lists:sort([STx1, STx3]), lists:sort(FromList)),
               ?assertEqual(lists:sort(FromList), lists:sort(FromMap))
       end},
      {"Candidate selection resolves each origin account once",
       {timeout, 10, fun() ->
               %% Several senders with many transactions each: without the
               %% per-pass memoisation the fold resolves an account for every
               %% entry it visits, not for every distinct origin.
               PerAccount = 50,
               ok = application:set_env(aecore, mempool_nonce_baseline, PerAccount),
               PubKeys = [new_pubkey(), new_pubkey(), new_pubkey()],
               STxs = [ a_signed_tx(PK, me, Nonce, 20000, 10)
                        || PK <- PubKeys, Nonce <- lists:seq(1, PerAccount) ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- STxs ],

               {ok, Hash} = aec_headers:hash_header(aec_block_genesis:genesis_header()),
               MaxGas = aec_governance:block_gas_limit(),

               meck:reset(aec_accounts_trees),
               {ok, Selected} = aec_tx_pool:get_candidate(MaxGas, Hash),
               ?assertEqual(length(STxs), length(Selected)),
               ?assertEqual(length(PubKeys),
                            meck:num_calls(aec_accounts_trees, lookup, ['_', '_']))
       end}},
      {"Candidate selection judges nonces by the account state it memoises",
       {timeout, 10, fun() ->
               %% Funded senders, so the memoised lookup answers with an
               %% account rather than falling through to the baseline check
               %% every sender without one gets.
               aec_test_utils:stop_chain_db(),
               Stale = new_pubkey(),
               Fresh = new_pubkey(),
               meck:expect(aec_fork_block_settings, genesis_accounts, 0,
                           [{Stale, 20000000}, {Fresh, 20000000}]),
               aec_consensus:set_genesis_hash(),
               {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
               aec_test_utils:start_chain_db(),
               {ok,_} = aec_chain_state:insert_block(GenesisBlock),

               [StaleTx1, StaleTx2, StaleTx3] = StaleTxs =
                   [ a_signed_tx(Stale, me, Nonce, 20000, 10) || Nonce <- lists:seq(1, 3) ],
               FreshTxs =
                   [ a_signed_tx(Fresh, me, Nonce, 20000, 10) || Nonce <- lists:seq(1, 3) ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- StaleTxs ++ FreshTxs ],

               %% The chain moves on underneath the pool: one sender's account
               %% nonce is now 2, so its first two entries can never be applied
               %% again. Selection has to see that through the memoised lookup,
               %% and per sender - the other one is still at nonce 0.
               {value, Account} = aec_chain:get_account(Stale),
               Advanced = aec_accounts:set_nonce(Account, 2),
               meck:expect(aec_accounts_trees, lookup,
                           fun(PubKey, _) when PubKey =:= Stale -> {value, Advanced};
                              (PubKey, Tree) -> meck:passthrough([PubKey, Tree])
                           end),

               Hash = aec_chain:top_block_hash(),
               MaxGas = aec_governance:block_gas_limit(),
               meck:reset(aec_accounts_trees),
               {ok, Selected} = aec_tx_pool:get_candidate(MaxGas, Hash),

               %% All six come back - the pool hands over what it rejected too -
               %% but the two stale ones come last, as rejects rather than as
               %% nonce-ordered candidates. A cache that answered `none`, or one
               %% sender's account for another, would order them differently.
               ?assertEqual([StaleTx1, StaleTx2], lists:nthtail(4, Selected)),
               ?assertEqual(lists:sort([StaleTx3 | FreshTxs]),
                            lists:sort(lists:sublist(Selected, 4))),
               %% Still one lookup per distinct origin: cache hits for the rest.
               ?assertEqual(2, meck:num_calls(aec_accounts_trees, lookup, ['_', '_']))
       end}},
      {"Selection takes an account's nonces in order, not in fee order",
       {timeout, 10, fun() ->
               %% One sender, whose queue has a hole in it, and whose txs past
               %% the hole pay better than the ones before it. Fee order alone
               %% reaches for those first - and none of them can be applied on
               %% top of this block, however much they pay, because the nonces
               %% ahead of them have not been applied yet.
               [Sender] = funded_accounts(1),
               %% Far enough ahead that pushing past the hole is allowed: what
               %% is under test is selection, not what the pool accepts.
               ok = application:set_env(aecore, mempool_nonce_offset, 100),

               Ready  = [ a_signed_tx(Sender, me, Nonce, 20000) || Nonce <- lists:seq(1, 3) ],
               Behind = [ a_signed_tx(Sender, me, Nonce, 40000) || Nonce <- lists:seq(5, 7) ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- Ready ++ Behind ],

               MaxGas = aec_governance:block_gas_limit(),
               {ok, Selected} = aec_tx_pool:get_candidate(MaxGas, aec_chain:top_block_hash()),

               %% Everything up to the hole, and nothing past it.
               ?assertEqual(Ready, Selected),

               %% The ones past the hole are left exactly as they were: not
               %% selected, and not retired either - a nonce the chain is not
               %% ready for is not a transaction that is wrong, and charging it
               %% for an apply it never had is what empties a mempool of txs
               %% that would have been perfectly good a block later.
               ?assertEqual(length(Ready) + length(Behind), aec_tx_pool:size()),
               ?assertEqual({ok, 7}, aec_tx_pool:get_max_nonce(Sender))
       end}},
      {"A cheaper tail does not take the room another account pays more for",
       {timeout, 10, fun() ->
               %% Following one account's nonce sequence must not mean emptying
               %% its whole queue before anyone else is looked at: the run stops
               %% where the account stops paying what the walk is being offered
               %% elsewhere, and picks up again when the walk reaches its price.
               [Deep, Rich] = funded_accounts(2),
               ok = application:set_env(aecore, mempool_nonce_offset, 100),

               %% Deep leads with the best fee in the pool and follows it with a
               %% long, cheap tail. Rich pays more than that tail, for all of it.
               DeepHead = a_signed_tx(Deep, me, 1, 80000),
               DeepTail = [ a_signed_tx(Deep, me, Nonce, 20000) || Nonce <- lists:seq(2, 9) ],
               RichTxs  = [ a_signed_tx(Rich, me, Nonce, 40000) || Nonce <- lists:seq(1, 4) ],
               [ ok = aec_tx_pool:push(STx, tx_created)
                 || STx <- [DeepHead | DeepTail] ++ RichTxs ],

               %% Room for the head, all of Rich, and one of the cheap tail.
               Header   = aec_chain:top_header(),
               Height   = aec_headers:height(Header),
               Protocol = aec_headers:version(Header),
               GasOf    = fun(STx) -> aetx:gas_limit(aetx_sign:tx(STx), Height, Protocol) end,
               MaxGas   = lists:sum([ GasOf(STx) || STx <- [DeepHead | RichTxs] ])
                              + GasOf(hd(DeepTail)),

               {ok, Selected} = aec_tx_pool:get_candidate(MaxGas, aec_chain:top_block_hash()),

               %% Everything Rich offered is in, ahead of the cheap tail.
               [ ?assert(lists:member(STx, Selected)) || STx <- RichTxs ],
               ?assert(lists:member(DeepHead, Selected)),
               %% And the tail took only the room that was left over.
               ?assertEqual([hd(DeepTail)],
                            [ STx || STx <- DeepTail, lists:member(STx, Selected) ])
       end}},
      {"An expired deadline stops a walk holding something, not one holding nothing",
       {timeout, 10, fun() ->
               %% The pool is walked in fee order, so the account that can offer
               %% nothing is walked first, and fills the first select chunk on
               %% its own. Stopping there hands back an empty candidate - and an
               %% empty micro block is never published, so the bound would have
               %% cost the whole block rather than shortened it.
               Chunk = 20, %% ?POOL_WALK_CHUNK in aec_tx_pool
               [Behind, Ready] = funded_accounts(2),
               ok = application:set_env(aecore, mempool_nonce_offset, 200),

               %% Nothing here can be applied: the account is at nonce 0.
               BehindTxs = [ a_signed_tx(Behind, me, Nonce, 40000)
                             || Nonce <- lists:seq(101, 100 + Chunk) ],
               ReadyTxs  = [ a_signed_tx(Ready, me, Nonce, 20000)
                             || Nonce <- lists:seq(1, 5) ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- BehindTxs ++ ReadyTxs ],

               Hash   = aec_chain:top_block_hash(),
               MaxGas = aec_governance:block_gas_limit(),
               Past   = erlang:monotonic_time(millisecond) - 1,

               %% Nothing packed yet, so the walk carries on past the deadline
               %% rather than come back empty-handed. It still reports itself cut
               %% short - it was - but it has a block to be cut short of.
               {ok, Selected, _} =
                   aec_tx_pool:get_candidate(MaxGas, #{}, Hash, #{deadline => Past}),
               ?assertEqual(ReadyTxs, Selected),

               aec_tx_pool:restore_mempool(),

               %% Told the caller already holds a block's worth, the same walk
               %% stops on the same deadline - there is a block to publish, so
               %% the bound does what it is for.
               {ok, [], #{expired := true}} =
                   aec_tx_pool:get_candidate(MaxGas, #{}, Hash,
                                             #{deadline => Past, packed => true})
       end}},
      {"Selection that ran out of time does not start on the visited table",
       {timeout, 10, fun() ->
               Waiting = 60,
               ok = application:set_env(aecore, mempool_nonce_baseline, Waiting),
               VisitedKey = new_pubkey(),
               WaitingKey = new_pubkey(),
               VisitedTxs = [ a_signed_tx(VisitedKey, me, Nonce, 20000, 10)
                              || Nonce <- lists:seq(1, 20) ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- VisitedTxs ],

               {ok, Hash} = aec_headers:hash_header(aec_block_genesis:genesis_header()),
               MaxGas = aec_governance:block_gas_limit(),

               %% Selecting them once is what moves them to the visited table.
               {ok, Selected0} = aec_tx_pool:get_candidate(MaxGas, Hash),
               ?assertEqual(length(VisitedTxs), length(Selected0)),
               ?assertEqual(length(VisitedTxs), aec_tx_pool:size(visited)),

               %% Enough in the mempool proper that walking it cannot finish
               %% inside the first select chunk.
               WaitingTxs = [ a_signed_tx(WaitingKey, me, Nonce, 20000, 10)
                              || Nonce <- lists:seq(1, Waiting) ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- WaitingTxs ],

               Past = erlang:monotonic_time(millisecond) - 1,
               {ok, Truncated, #{expired := true}} =
                   aec_tx_pool:get_candidate(MaxGas, #{}, Hash, #{deadline => Past}),

               %% The visited table is a second walk, taken only once the
               %% mempool proper is exhausted - which an expired walk never is.
               ?assert(length(Truncated) < Waiting),
               ?assertEqual([], [ STx || STx <- Truncated,
                                         lists:member(STx, VisitedTxs) ])
       end}},
      {"The best paying of many transactions competing for one nonce is selected",
       {timeout, 10, fun() ->
               %% The walk reads one bounded chunk of the nonce and takes the
               %% first entry of it, so the dearest of many competitors is
               %% selected only because the index is ordered by fee.
               [PubKey] = funded_accounts(1),
               Fees  = [ 20000 + 1000 * N || N <- lists:seq(0, 39) ],
               STxs  = [ a_signed_tx(PubKey, me, 1, Fee) || Fee <- Fees ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- STxs ],
               ?assertEqual(length(STxs), aec_tx_pool:size()),

               Hash   = aec_chain:top_block_hash(),
               MaxGas = aec_governance:block_gas_limit(),
               {ok, Selected, #{expired := false}} =
                   aec_tx_pool:get_candidate(MaxGas, #{}, Hash, #{}),
               %% One nonce can be filled once, and the highest fee fills it.
               ?assertEqual([lists:last(STxs)], Selected)
       end}},
      {"A cheaper competitor does not inherit the price of the one it replaces",
       {timeout, 10, fun() ->
               %% A transaction the pool accepts but selection can never take -
               %% here one whose ttl has just passed - is free to sit at the front
               %% of the fee order. Whatever is behind it at the same nonce still
               %% has to pay its own way, or a sender could park a minimum-fee
               %% transaction where a top-fee one appeared to be and have it taken
               %% ahead of everyone the walk still owes.
               [Attacker, Honest] = funded_accounts(2),

               %% Height 1, so that a ttl of 1 is expired for selection while
               %% still being accepted by the pool.
               {ok, KeyBlock1} =
                   aec_block_key_candidate:create(aec_chain:top_block(), Attacker, Attacker),
               {ok, _} = aec_chain_state:insert_block(KeyBlock1),

               Decoy = a_signed_tx(Attacker, me, 1, 80000, _TTL = 1),
               Cheap = a_signed_tx(Attacker, me, 1, 20000),
               Mid   = a_signed_tx(Honest,   me, 1, 40000),
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- [Decoy, Cheap, Mid] ],

               Header   = aec_chain:top_header(),
               Height   = aec_headers:height(Header),
               Protocol = aec_headers:version(Header),
               MaxGas   = aetx:gas_limit(aetx_sign:tx(Mid), Height, Protocol),

               %% Room for one of them. It goes to the transaction that paid the
               %% most for it, not to the one sheltering behind the decoy. The
               %% decoy itself comes back too, retired rather than selected.
               {ok, Selected} =
                   aec_tx_pool:get_candidate(MaxGas, aec_chain:top_block_hash()),
               ?assert(lists:member(Mid, Selected)),
               ?assertNot(lists:member(Cheap, Selected))
       end}},
      {"A run steps over a nonce an earlier pass already took",
       {timeout, 10, fun() ->
               %% The nonce is spoken for, so the run carries on from the next one
               %% rather than filling it twice - and rather than stopping there,
               %% which would strand the rest of the queue for this build.
               [PubKey] = funded_accounts(1),
               [T1, T2, T3] = [ a_signed_tx(PubKey, me, N, 20000) || N <- lists:seq(1, 3) ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- [T1, T2, T3] ],

               Hash   = aec_chain:top_block_hash(),
               MaxGas = aec_governance:block_gas_limit(),
               Ignore = #{aetx_sign:hash(T2) => []},
               {ok, Selected, _} = aec_tx_pool:get_candidate(MaxGas, Ignore, Hash, #{}),
               ?assertEqual([T1, T3], Selected)
       end}},
      {"A nonce whose dearest transaction does not fit yields to a smaller one",
       {timeout, 10, fun() ->
               %% Only one transaction can ever fill a nonce, but when the dearest
               %% of them does not fit the room left, a smaller competitor still
               %% can - once the walk reaches what that one pays.
               [Sender] = funded_accounts(1),
               Dear  = signed_ct_call_tx(Sender, _Nonce = 1, _Fee = 800000, _GasPrice = 1),
               Cheap = a_signed_tx(Sender, me, 1, 20000),
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- [Dear, Cheap] ],

               Header   = aec_chain:top_header(),
               Height   = aec_headers:height(Header),
               Protocol = aec_headers:version(Header),
               GasOf    = fun(STx) -> aetx:gas_limit(aetx_sign:tx(STx), Height, Protocol) end,
               MaxGas   = GasOf(Cheap),
               ?assert(GasOf(Dear) > MaxGas),

               ?assertEqual({ok, [Cheap]},
                            aec_tx_pool:get_candidate(MaxGas, aec_chain:top_block_hash()))
       end}},
      {"A nonce the walk keeps coming back to still yields the same transaction",
       {timeout, 10, fun() ->
               %% None of these fits the room left, so the nonce is handed back
               %% to the walk once per competitor it holds. What the walk finally
               %% selects may not change for that.
               [Sender] = funded_accounts(1),
               Dear  = [ signed_ct_call_tx(Sender, _Nonce = 1, Fee, _GasPrice = 1)
                         || Fee <- [ 800000 + 1000 * N || N <- lists:seq(0, 29) ] ],
               Cheap = a_signed_tx(Sender, me, 1, 20000),
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- [Cheap | Dear] ],

               Header   = aec_chain:top_header(),
               Height   = aec_headers:height(Header),
               Protocol = aec_headers:version(Header),
               GasOf    = fun(STx) -> aetx:gas_limit(aetx_sign:tx(STx), Height, Protocol) end,
               MaxGas   = GasOf(Cheap),
               [ ?assert(GasOf(STx) > MaxGas) || STx <- Dear ],

               {ok, Selected, #{expired := false}} =
                   aec_tx_pool:get_candidate(MaxGas, #{}, aec_chain:top_block_hash(), #{}),

               ?assertEqual([Cheap], Selected),
               %% The rest are left where they were, neither retired nor offered.
               ?assertEqual([Cheap], aec_tx_pool:peek_visited()),
               ?assertEqual(length(Dear), length(aec_tx_pool:peek_db()))
       end}},
      {"A nonce is resumed where the walk left it, visited entries included",
       {timeout, 10, fun() ->
               %% The nonce index spans both tables, so a competitor the fee walk
               %% has not reached can be sitting in the visited one. Resuming a
               %% nonce has to keep it in view, not seek past it to something
               %% cheaper.
               [Sender] = funded_accounts(1),

               %% Selecting it once is what puts it in the visited table.
               Visited = a_signed_tx(Sender, me, 1, 40000),
               ok = aec_tx_pool:push(Visited, tx_created),
               {ok, [Visited]} =
                   aec_tx_pool:get_candidate(aec_governance:block_gas_limit(),
                                             aec_chain:top_block_hash()),
               ?assertEqual([Visited], aec_tx_pool:peek_visited()),

               %% Dearer than Visited and too big to fit; cheaper and small enough.
               Dear  = signed_ct_call_tx(Sender, _Nonce = 1, _Fee = 800000, _GasPrice = 1),
               Cheap = a_signed_tx(Sender, me, 1, 20000),
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- [Dear, Cheap] ],

               Header   = aec_chain:top_header(),
               Height   = aec_headers:height(Header),
               Protocol = aec_headers:version(Header),
               GasOf    = fun(STx) -> aetx:gas_limit(aetx_sign:tx(STx), Height, Protocol) end,
               MaxGas   = GasOf(Visited),
               ?assert(GasOf(Dear) > MaxGas),

               %% Dear gives the nonce back at Visited; Cheap brings the walk down
               %% to a price Visited is worth, so Visited fills the nonce.
               {ok, Selected, _} =
                   aec_tx_pool:get_candidate(MaxGas, #{}, aec_chain:top_block_hash(), #{}),
               ?assertEqual([Visited], Selected)
       end}},
      {"A stale transaction below a finished account's run is left where it is",
       {timeout, 10, fun() ->
               %% The run ends above nonce 3, which takes the sender out of the
               %% walk for the rest of this candidate. Its stale entry pays less,
               %% so the walk only reaches it afterwards - and finds a sender it
               %% has already finished with. Nothing judges it here; the origins
               %% cache retires it on the garbage collector's own sweep.
               [Sender] = funded_accounts(1),
               ok = application:set_env(aecore, mempool_nonce_offset, 100),
               Stale = a_signed_tx(Sender, me, 1, 20000),
               Due   = a_signed_tx(Sender, me, 3, 40000),
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- [Stale, Due] ],

               %% The chain moves on underneath the pool: nonce 1 can never be
               %% applied again, and nonce 3 is the one the sender is now due.
               {value, Account} = aec_chain:get_account(Sender),
               Advanced = aec_accounts:set_nonce(Account, 2),
               meck:expect(aec_accounts_trees, lookup,
                           fun(PubKey, _) when PubKey =:= Sender -> {value, Advanced};
                              (PubKey, Tree) -> meck:passthrough([PubKey, Tree])
                           end),

               MaxGas = aec_governance:block_gas_limit(),
               {ok, Selected} =
                   aec_tx_pool:get_candidate(MaxGas, aec_chain:top_block_hash()),

               %% Only the due one is offered, and the stale one is not handed to
               %% the builder alongside it just to fail there.
               ?assertEqual([Due], Selected),
               ?assertEqual([Due], aec_tx_pool:peek_visited()),
               ?assertEqual([Stale], aec_tx_pool:peek_db())
       end}},
      {"A deadline does not cut a run of nonces short - the gas limit does",
       {timeout, 10, fun() ->
               %% Each nonce a run takes either spends gas or fills one an earlier
               %% pass offered, so gas and the ignore set bound it - not the clock.
               Run = 100,
               [PubKey] = funded_accounts(1),
               ok = application:set_env(aecore, mempool_nonce_offset, Run),
               STxs = [ a_signed_tx(PubKey, me, Nonce, 20000)
                        || Nonce <- lists:seq(1, Run) ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- STxs ],

               Header   = aec_chain:top_header(),
               Height   = aec_headers:height(Header),
               Protocol = aec_headers:version(Header),
               GasOf    = fun(STx) -> aetx:gas_limit(aetx_sign:tx(STx), Height, Protocol) end,
               Hash     = aec_chain:top_block_hash(),
               MaxGas   = aec_governance:block_gas_limit(),
               Past     = erlang:monotonic_time(millisecond) - 1,

               %% Already past the deadline and told a block is held, so the walk
               %% stops - at the end of the chunk, with the run whole.
               {ok, Selected, #{expired := true}} =
                   aec_tx_pool:get_candidate(MaxGas, #{}, Hash,
                                             #{deadline => Past, packed => true}),
               ?assertEqual(STxs, Selected),

               aec_tx_pool:restore_mempool(),

               %% Room for ten of them is what stops the same run instead.
               Ten = lists:sublist(STxs, 10),
               {ok, Selected1, #{expired := false}} =
                   aec_tx_pool:get_candidate(lists:sum([ GasOf(STx) || STx <- Ten ]),
                                             #{}, Hash, #{deadline => infinity}),
               ?assertEqual(Ten, Selected1)
       end}},
      {"A second pass resumes after the nonces the first one packed",
       {timeout, 20, fun() ->
               %% Where an account's run resumes is read from the nonce index, one
               %% packed nonce at a time. The nonce it stops at is read whole -
               %% here that is more competitors than one index read returns.
               Flood = 150,
               [PubKey] = funded_accounts(1),
               ok = application:set_env(aecore, mempool_nonce_offset, 200),
               Front = [ a_signed_tx(PubKey, me, Nonce, 20000)
                         || Nonce <- lists:seq(1, 5) ],
               Rest  = [ a_signed_tx(PubKey, me, 6, 20000 + 100 * N)
                         || N <- lists:seq(1, Flood) ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- Front ++ Rest ],

               Header   = aec_chain:top_header(),
               Height   = aec_headers:height(Header),
               Protocol = aec_headers:version(Header),
               GasOf    = fun(STx) -> aetx:gas_limit(aetx_sign:tx(STx), Height, Protocol) end,
               Hash     = aec_chain:top_block_hash(),
               MaxGas   = aec_governance:block_gas_limit(),

               %% Room for the front of the queue only.
               {ok, First, _} =
                   aec_tx_pool:get_candidate(lists:sum([ GasOf(STx) || STx <- Front ]),
                                             #{}, Hash, #{}),
               ?assertEqual(Front, First),

               %% The packed transactions are themselves ignored, so nothing in
               %% the pool can lead the walk into this account's run: resuming at
               %% nonce 6 is the only way the dearest of its competitors is
               %% reached at all.
               Ignore = maps:from_list([ {aetx_sign:hash(STx), []} || STx <- First ]),
               {ok, Second, #{expired := false}} =
                   aec_tx_pool:get_candidate(MaxGas, Ignore, Hash, #{}),
               ?assertEqual([lists:last(Rest)], Second),

               aec_tx_pool:restore_mempool(),

               %% Past the deadline with a block already held, the resume is
               %% abandoned rather than reported half-read: the account is left
               %% alone, so no nonce of its run can be filled twice.
               Past = erlang:monotonic_time(millisecond) - 1,
               ?assertEqual({ok, [], #{expired => true}},
                            aec_tx_pool:get_candidate(MaxGas, Ignore, Hash,
                                                      #{deadline => Past,
                                                        packed => true}))
       end}},
      {"An expired deadline stops between the competitors for one nonce",
       {timeout, 10, fun() ->
               %% Retiring the invalid transactions competing for a single nonce
               %% is a loop of its own, below both the chunked walk and the run
               %% of nonces - so it has to honour the deadline itself.
               Competitors = 40,
               [PubKey] = funded_accounts(1),

               %% Height 1, so that a ttl of 1 is expired for selection while
               %% still being accepted by the pool.
               {ok, KeyBlock1} =
                   aec_block_key_candidate:create(aec_chain:top_block(), PubKey, PubKey),
               {ok, _} = aec_chain_state:insert_block(KeyBlock1),

               STxs = [ a_signed_tx(PubKey, me, 1, 20000 + 1000 * N, _TTL = 1)
                        || N <- lists:seq(0, Competitors - 1) ],
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- STxs ],
               ?assertEqual(Competitors, aec_tx_pool:size()),
               ?assertEqual(0, aec_tx_pool:size(visited)),

               Hash   = aec_chain:top_block_hash(),
               MaxGas = aec_governance:block_gas_limit(),
               Past   = erlang:monotonic_time(millisecond) - 1,

               %% Every competitor is invalid, so none can fill the nonce. With
               %% a block already packed the walk may stop, and examining one
               %% competitor is enough for it to notice that it should.
               {ok, _, #{expired := true}} =
                   aec_tx_pool:get_candidate(MaxGas, #{}, Hash,
                                             #{deadline => Past, packed => true}),
               %% Retiring a tx is what moves it to the visited table, so this
               %% counts how many of the pile the walk actually looked at.
               ?assertEqual(1, aec_tx_pool:size(visited)),

               aec_tx_pool:restore_mempool(),

               %% Unbounded, the walk works through the whole pile - so it was
               %% the deadline that stopped it, not a shortage of competitors.
               {ok, _, #{expired := false}} =
                   aec_tx_pool:get_candidate(MaxGas, #{}, Hash, #{deadline => infinity}),
               ?assertEqual(Competitors, aec_tx_pool:size(visited))
       end}},
      {"An expired deadline stops reading a flooded nonce for the resume point",
       {timeout, 20, fun() ->
               %% Proving that no competitor of a nonce was packed means reading
               %% them all, a chunk at a time - another loop below the run of
               %% nonces, so it too has to honour the deadline.
               Flood = 150,
               [PubKey, Other] = funded_accounts(2),
               STxs    = [ a_signed_tx(PubKey, me, 1, 20000 + 100 * N)
                           || N <- lists:seq(1, Flood) ],
               OtherTx = a_signed_tx(Other, me, 1, 20000),
               [ ok = aec_tx_pool:push(STx, tx_created) || STx <- STxs ++ [OtherTx] ],

               Hash   = aec_chain:top_block_hash(),
               MaxGas = aec_governance:block_gas_limit(),
               Past   = erlang:monotonic_time(millisecond) - 1,
               %% Another account's transaction, so the resume is consulted at all
               %% while leaving every competitor of the flooded nonce unpacked.
               Ignore = #{aetx_sign:hash(OtherTx) => []},

               ?assertEqual({ok, [], #{expired => true}},
                            aec_tx_pool:get_candidate(MaxGas, Ignore, Hash,
                                                      #{deadline => Past,
                                                        packed => true})),

               %% Unbounded, the same read reaches the end of the nonce and the run
               %% takes the dearest of it - so it was the deadline that stopped it.
               ?assertEqual({ok, [lists:last(STxs)], #{expired => false}},
                            aec_tx_pool:get_candidate(MaxGas, Ignore, Hash,
                                                      #{deadline => infinity}))
       end}},
      {"fill micro block with and without previously rejected tx",
       {timeout, 10, fun() ->
               ok = application:set_env(aecore, mempool_nonce_offset, 600),
               aec_test_utils:stop_chain_db(),
               %% At equal fee and gas price the walk visits accounts in pubkey
               %% order; fix it, or which txs fill the last candidate is random.
               [PubKey1, PubKey2] = lists:sort([new_pubkey(), new_pubkey()]),
               meck:expect(aec_fork_block_settings, genesis_accounts, 0,
                           [{PubKey1, 20001}, {PubKey2, 20000000}]),
               aec_consensus:set_genesis_hash(),
               {Block0, _} = aec_block_genesis:genesis_block_with_state(),
               aec_test_utils:start_chain_db(),
               {ok,_} = aec_chain_state:insert_block(Block0),

               %% The first block needs to be a key-block
               {ok, Miner} = aec_keys:candidate_pubkey(),
               {ok, KeyBlock} = aec_block_key_candidate:create(aec_chain:top_block(), Miner, Miner),
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

               aec_tx_pool:top_change(#{type => micro, old_hash => KeyHash,
                                        new_hash => MicroHash}),

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

               %% More new transacions than one block holds: the rejected tx is
               %% retried at PubKey1's due nonce, and PubKey2's queue is followed
               %% from the visited table into the new txs until the gas runs out.
               ?assertEqual([], aec_tx_pool:peek_db()),
               Txs4 = [ a_signed_tx(PubKey2, me, Nonce, 20000, 10) || Nonce <- lists:seq(104, 504) ],
               [ ok = aec_tx_pool:push(Tx) || Tx <- Txs4 ],
               TotalGas4 = lists:sum([ aetx:gas_limit(aetx_sign:tx(T), GenesisHeight, GenesisProtocol) || T <- Txs4 ]),
               ?assert(TotalGas4 > aec_governance:block_gas_limit()),
               {ok, CTxs4} = aec_tx_pool:get_candidate(aec_governance:block_gas_limit(), MicroHash),
               ?assert(lists:member(Tx1_2, CTxs4)),
               ?assert(lists:member(hd(Txs4), CTxs4)),
               ?assert(not lists:member(lists:last(Txs4), CTxs4)),

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
               {ok, Miner} = aec_keys:candidate_pubkey(),
               {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PubKey1, Miner),
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
               aec_tx_pool:top_change(#{type => micro, old_hash => TopBlockHash,
                                        new_hash => CHash1}),

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
               {ok, KeyBlock2} = aec_block_key_candidate:create(TopBlockFork1, PubKey1, PubKey1),
               {ok, CHashFork1} = aec_blocks:hash_internal_representation(KeyBlock2),

               meck:expect(aeu_time, now_in_msecs, fun() -> meck:passthrough([]) + 6000 end),
               %% A TTL of its own, so that the replay below has to combine it
               %% with the TTL read once for the whole replay.
               STx4TTL = 3,
               STx4 = a_signed_tx(PubKey2, new_pubkey(), 2, 40000, STx4TTL),
               ?assertEqual(ok, aec_tx_pool:push(STx4)),
               {ok, USCandidate4, _} = aec_block_micro_candidate:create(aec_chain:top_block()),
               {ok, Candidate4} = aec_keys:sign_micro_block(USCandidate4),

               {ok,_} = aec_chain_state:insert_block(Candidate4),
               TopBlockFork2 = aec_chain:top_block(),
               {ok, KeyBlock3} = aec_block_key_candidate:create(TopBlockFork2, PubKey1, PubKey1),
               {ok, CHashFork2} = aec_blocks:hash_internal_representation(KeyBlock3),

               %% Push the keyblock with the longest chain of micro blocks
               {ok,_} = aec_chain_state:insert_block(KeyBlock3),
               ?assertEqual(CHashFork2, aec_chain:top_block_hash()),
               aec_tx_pool:top_change(#{type => key, old_hash => CHash1,
                                        new_hash => CHashFork2}),
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
               aec_tx_pool:top_change(#{type => key, old_hash => CHashFork2,
                                        new_hash => CHashFork1}),

               %% The not included transaction should now be back in the pool
               ?assertEqual({ok, [STx4]}, aec_tx_pool:peek(infinity)),

               %% ... and the TTL read once for the whole replay must have been
               %% combined with the tx's own TTL when it was re-added.
               {GCHeight, Dbs} = aec_tx_pool:gc_height_and_dbs(),
               ?assertEqual({ok, min(GCHeight + aec_tx_pool:tx_ttl(), STx4TTL)},
                            gc_ttl_of(STx4, Dbs)),

               meck:unload(aec_headers),
               ok
       end},
      {"A fork replay judges a returning transaction against the new top",
       fun() ->
               Short = 2,
               ok = application:set_env(aecore, mempool_future_nonce_tx_ttl, Short),
               aec_test_utils:stop_chain_db(),
               PubKey1 = new_pubkey(),
               PubKey2 = new_pubkey(),
               meck:expect(aec_fork_block_settings, genesis_accounts, 0,
                           [{PubKey1, 100000}, {PubKey2, 100000}]),
               aec_consensus:set_genesis_hash(),
               {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
               aec_test_utils:start_chain_db(),
               {ok,_} = aec_chain_state:insert_block(GenesisBlock),

               %% The first block needs to be a key-block
               {ok, Miner} = aec_keys:candidate_pubkey(),
               {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PubKey1, Miner),
               {ok, KeyHash1} = aec_blocks:hash_internal_representation(KeyBlock1),
               {ok,_} = aec_chain_state:insert_block(KeyBlock1),
               ok = aec_keys:promote_candidate(aec_blocks:miner(KeyBlock1)),

               %% Three transactions of one sender, mined into a micro block.
               [STx1, STx2, STx3] =
                   [ a_signed_tx(PubKey2, new_pubkey(), N, 20000) || N <- [1, 2, 3] ],
               [ ?assertEqual(ok, aec_tx_pool:push(T)) || T <- [STx1, STx2, STx3] ],
               {ok, USCandidate, _} = aec_block_micro_candidate:create(aec_chain:top_block()),
               {ok, Candidate} = aec_keys:sign_micro_block(USCandidate),
               {ok, CHash} = aec_blocks:hash_internal_representation(Candidate),
               {ok,_} = aec_chain_state:insert_block(Candidate),
               ?assertEqual(lists:sort([STx1, STx2, STx3]),
                            lists:sort(aec_blocks:txs(Candidate))),
               aec_tx_pool:top_change(#{type => micro, old_hash => KeyHash1,
                                        new_hash => CHash}),
               ?assertEqual({ok, []}, aec_tx_pool:peek(infinity)),

               %% A key block on the block before them takes the top over - two
               %% key blocks outweigh one with a micro block - which replays all
               %% three back into the pool.
               {ok, KeyBlock2} = aec_block_key_candidate:create(KeyBlock1, PubKey1, PubKey1),
               {ok, KeyHash2} = aec_blocks:hash_internal_representation(KeyBlock2),
               {ok,_} = aec_chain_state:insert_block(KeyBlock2),
               ?assertEqual(KeyHash2, aec_chain:top_block_hash()),

               %% On the new top their sender is back at nonce 0, so this offset
               %% leaves only the last of them too far ahead. Judged against the
               %% top they are leaving, where it stands at nonce 3, none is.
               ok = application:set_env(aecore, mempool_nonce_offset, 2),
               aec_tx_pool:top_change(#{type => key, old_hash => CHash,
                                        new_hash => KeyHash2}),
               {ok, Back} = aec_tx_pool:peek(infinity),
               ?assertEqual(lists:sort([STx1, STx2, STx3]), lists:sort(Back)),

               {GCHeight, Dbs} = aec_tx_pool:gc_height_and_dbs(),
               Horizon = GCHeight + aec_tx_pool:tx_ttl(),
               ?assert(GCHeight + Short < Horizon),
               ?assertEqual({ok, Horizon},          gc_ttl_of(STx1, Dbs)),
               ?assertEqual({ok, Horizon},          gc_ttl_of(STx2, Dbs)),
               ?assertEqual({ok, GCHeight + Short}, gc_ttl_of(STx3, Dbs))
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
                 {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PK1, PK1),
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
               %% The index key carries the whole mempool key. Its fee positions
               %% are what delete_nonce/2 has to reconstruct, so pin them.
               ?assertEqual([{PK, Nonce1, -aetx:deep_fee(aetx_sign:tx(STx1)), 0,
                              aetx_sign:hash(STx1)}],
                            aec_tx_pool:peek_nonces()),
               Size = aec_tx_pool:size(),
               ?assertEqual({ok, [STx1]},
                            aec_tx_pool:get_candidate(MaxGas, TopBlockHash)),
               ?assertEqual([STx1], aec_tx_pool:peek_visited()),
               ?assertEqual([], aec_tx_pool:peek_db()),
               ?assertEqual(Size, aec_tx_pool:size()),
               %% a 'key' top_change should restore visited to the mempool
               aec_tx_pool:top_change(#{type => key, old_hash => TopBlockHash,
                                        new_hash => TopBlockHash}),
               ?assertEqual([], aec_tx_pool:peek_visited()),
               ?assertEqual([STx1], aec_tx_pool:peek_db()),
               ?assertEqual(Size, aec_tx_pool:size())
       end},
      {"Peek by account",
       fun() ->
               ok = application:set_env(aecore, mempool_nonce_baseline, 100),
               PK1 = new_pubkey(),
               PK2 = new_pubkey(),
               MaxGas = aec_governance:block_gas_limit(),
               TopBlockHash = aec_chain:top_block_hash(),
               %% Fee grows with the nonce, so the fee order of the mempool is
               %% the reverse of the nonce order asserted on below.
               STxs1 = [a_signed_tx(PK1, me, N, 20000 + N * 1000)
                        || N <- lists:seq(1, 5)],
               STxs2 = [a_signed_tx(PK2, me, N, 20000) || N <- lists:seq(1, 3)],
               [?assertEqual(ok, aec_tx_pool:push(STx)) || STx <- STxs1 ++ STxs2],

               %% Only the requested account's txs, in nonce order
               ?assertEqual({ok, STxs1}, aec_tx_pool:peek(infinity, PK1)),
               ?assertEqual({ok, STxs2}, aec_tx_pool:peek(infinity, PK2)),
               ?assertEqual({ok, []}, aec_tx_pool:peek(infinity, new_pubkey())),

               %% The max nonce is respected ...
               ?assertEqual({ok, lists:sublist(STxs1, 3)},
                            aec_tx_pool:peek(infinity, PK1, 3)),
               ?assertEqual({ok, []}, aec_tx_pool:peek(infinity, PK1, 0)),
               %% ... and truncating to Max keeps the lowest nonces
               ?assertEqual({ok, lists:sublist(STxs1, 2)},
                            aec_tx_pool:peek(2, PK1)),
               ?assertEqual({ok, []}, aec_tx_pool:peek(0, PK1)),

               %% Txs moved to the visited mempool are still found
               {ok, _} = aec_tx_pool:get_candidate(MaxGas, TopBlockHash),
               ?assertEqual([], aec_tx_pool:peek_db()),
               ?assertEqual({ok, STxs1}, aec_tx_pool:peek(infinity, PK1)),
               ?assertEqual({ok, STxs2}, aec_tx_pool:peek(infinity, PK2)),

               %% ... and deleted ones are not
               [STxDel | STxsLeft] = STxs1,
               ?assertEqual(ok, aec_tx_pool:delete(aetx_sign:hash(STxDel))),
               ?assertEqual({ok, STxsLeft}, aec_tx_pool:peek(infinity, PK1)),

               %% Deleting the rest empties the nonce index too - an index entry
               %% deleted under a key other than the one it was written with
               %% would linger here.
               [ ?assertEqual(ok, aec_tx_pool:delete(aetx_sign:hash(STx)))
                 || STx <- STxsLeft ++ STxs2 ],
               ?assertEqual([], aec_tx_pool:peek_nonces())
       end},
      {"Peek by account orders the transactions competing for one nonce by fee",
       fun() ->
               ok = application:set_env(aecore, mempool_nonce_baseline, 100),
               PK = new_pubkey(),
               STxs = [a_signed_tx(PK, me, 1, 20000 + N * 1000) || N <- lists:seq(1, 3)],
               [?assertEqual(ok, aec_tx_pool:push(STx)) || STx <- STxs],
               ?assertEqual({ok, lists:reverse(STxs)}, aec_tx_pool:peek(infinity, PK)),
               %% Truncating to Max therefore keeps the dearest.
               ?assertEqual({ok, [lists:last(STxs)]}, aec_tx_pool:peek(1, PK))
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
               {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PK, PK),
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
               STx3 = a_signed_tx(PK, me, Nonce1=1, 20000000),
               ?assertNotEqual(STx1, STx3),
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
      {"A transaction far ahead of its sender's nonce gets the shorter stay",
       {timeout, 10, fun() ->
               %% Gossip does not apply the nonce offset, and selection does not
               %% offer a transaction the chain is not ready for - so it is never
               %% charged an apply failure either. future_nonce_tx_ttl is the only
               %% thing left to bound its stay.
               Short = 2,
               ok = application:set_env(aecore, mempool_future_nonce_tx_ttl, Short),
               [PubKey] = funded_accounts(1),
               ok = aec_tx_pool:stop(),
               {ok, Pid} = aec_tx_pool:start_link(),
               unlink(Pid), %% Leave it for the cleanup
               {GCHeight, Dbs} = aec_tx_pool:gc_height_and_dbs(),
               Offset  = aec_tx_pool:nonce_offset(),
               Horizon = GCHeight + aec_tx_pool:tx_ttl(),
               ?assert(GCHeight + Short < Horizon),

               %% The last nonce the offset allows keeps the full stay.
               Near = a_signed_tx(PubKey, me, Offset, 20000),
               ?assertEqual(ok, aec_tx_pool:push(Near, tx_received)),
               ?assertEqual({ok, Horizon}, gc_ttl_of(Near, Dbs)),

               %% The first one beyond it does not.
               Far = a_signed_tx(PubKey, me, Offset + 1, 20000),
               ?assertEqual(ok, aec_tx_pool:push(Far, tx_received)),
               ?assertEqual({ok, GCHeight + Short}, gc_ttl_of(Far, Dbs)),

               %% Locally created transactions are still turned away at ingress, so
               %% the shorter stay is not a way in for them.
               ?assertEqual({error, nonce_too_high},
                            aec_tx_pool:push(a_signed_tx(PubKey, me, Offset + 2, 20000))),

               %% A restart reads the persisted mempool into a fresh GC table, so
               %% the shorter stay has to be worked out again there - otherwise it
               %% would be renewed to the full one on every restart.
               ok = aec_tx_pool:stop(),
               {ok, Pid2} = aec_tx_pool:start_link(),
               unlink(Pid2), %% Leave it for the cleanup
               ?assertEqual({ok, Horizon},          gc_ttl_of(Near, Dbs)),
               ?assertEqual({ok, GCHeight + Short}, gc_ttl_of(Far, Dbs)),

               %% And the shorter stay is what collects it, index included.
               tx_pool_gc(GCHeight + Short),
               ?assertEqual({ok, [Near]}, aec_tx_pool:peek(infinity)),
               ?assertEqual([{PubKey, Offset, -aetx:deep_fee(aetx_sign:tx(Near)), 0,
                              aetx_sign:hash(Near)}],
                            aec_tx_pool:peek_nonces())
       end}},
      {"Without a readable accounts tree every transaction keeps the full stay",
       {timeout, 10, fun() ->
               %% A node that cannot read the state of its top block cannot tell
               %% a transaction far ahead of its sender from any other, so it
               %% must not shorten any stay on that basis.
               Short = 2,
               ok = application:set_env(aecore, mempool_future_nonce_tx_ttl, Short),
               [PubKey] = funded_accounts(1),
               ok = aec_tx_pool:stop(),
               {ok, Pid} = aec_tx_pool:start_link(),
               unlink(Pid), %% Leave it for the cleanup
               {GCHeight, Dbs} = aec_tx_pool:gc_height_and_dbs(),
               Horizon = GCHeight + aec_tx_pool:tx_ttl(),
               ?assert(GCHeight + Short < Horizon),
               Far = a_signed_tx(PubKey, me, aec_tx_pool:nonce_offset() + 1, 20000),
               ?assertEqual(ok, aec_tx_pool:push(Far, tx_received)),
               %% Restored against a readable tree it is judged short...
               ?assertEqual({ok, GCHeight + Short}, gc_ttl_of(Far, Dbs)),

               meck:new(aec_chain, [passthrough]),
               try
                   %% ...but with no top block at all there is nothing to judge
                   %% against, and the full stay is what it keeps.
                   meck:expect(aec_chain, top_block_hash, 0, undefined),
                   ok = aec_tx_pool:stop(),
                   {ok, Pid2} = aec_tx_pool:start_link(),
                   unlink(Pid2), %% Leave it for the cleanup
                   ?assertEqual({ok, Horizon}, gc_ttl_of(Far, Dbs)),

                   %% Same when the top is there but its state is not.
                   meck:delete(aec_chain, top_block_hash, 0),
                   meck:expect(aec_chain, get_block_state_partial, 2, error),
                   ok = aec_tx_pool:stop(),
                   {ok, Pid3} = aec_tx_pool:start_link(),
                   unlink(Pid3), %% Leave it for the cleanup
                   ?assertEqual({ok, Horizon}, gc_ttl_of(Far, Dbs))
               after
                   %% The fixture sweeps this too: eunit kills the process on a
                   %% {timeout, _} expiry and this clause would not run.
                   meck:unload(aec_chain)
               end
       end}},
      {"A batch judges each sender against its own nonce",
       {timeout, 10, fun() ->
               %% The batch memoises the ceiling per sender, so one sender's
               %% answer must never be handed to another.
               Short = 2,
               ok = application:set_env(aecore, mempool_future_nonce_tx_ttl, Short),
               [Known] = funded_accounts(1),
               Unknown = new_pubkey(),
               ok = aec_tx_pool:stop(),
               {ok, Pid} = aec_tx_pool:start_link(),
               unlink(Pid), %% Leave it for the cleanup
               {GCHeight, Dbs} = aec_tx_pool:gc_height_and_dbs(),
               Horizon = GCHeight + aec_tx_pool:tx_ttl(),
               ?assert(GCHeight + Short < Horizon),

               %% The same nonce: too far ahead for the sender the chain knows,
               %% and not judged at all for the one it does not.
               Nonce     = aec_tx_pool:nonce_offset() + 1,
               KnownTx   = a_signed_tx(Known, me, Nonce, 20000),
               UnknownTx = a_signed_tx(Unknown, me, Nonce, 20000),
               [ ?assertEqual(ok, aec_tx_pool:push(T, tx_received))
                 || T <- [KnownTx, UnknownTx] ],
               ?assertEqual({ok, GCHeight + Short}, gc_ttl_of(KnownTx, Dbs)),
               ?assertEqual({ok, Horizon},          gc_ttl_of(UnknownTx, Dbs)),

               %% Restored in one batch, each still keeps its own sender's stay.
               ok = aec_tx_pool:stop(),
               {ok, Pid2} = aec_tx_pool:start_link(),
               unlink(Pid2), %% Leave it for the cleanup
               ?assertEqual({ok, GCHeight + Short}, gc_ttl_of(KnownTx, Dbs)),
               ?assertEqual({ok, Horizon},          gc_ttl_of(UnknownTx, Dbs))
       end}},
      {"Equal mempool stays hold a future nonce as long as any other transaction",
       fun() ->
               %% The way back to the behaviour of holding every transaction for
               %% mempool.tx_ttl, whatever its nonce.
               TxTTL = aec_tx_pool:tx_ttl(),
               ok = application:set_env(aecore, mempool_future_nonce_tx_ttl, TxTTL),
               [PubKey] = funded_accounts(1),
               {GCHeight, Dbs} = aec_tx_pool:gc_height_and_dbs(),
               Far = a_signed_tx(PubKey, me, aec_tx_pool:nonce_offset() + 1, 20000),
               ?assertEqual(ok, aec_tx_pool:push(Far, tx_received)),
               ?assertEqual({ok, GCHeight + TxTTL}, gc_ttl_of(Far, Dbs))
       end},
      {"A returning transaction's stay is judged like a fresh one's",
       {timeout, 10, fun() ->
               %% A collected transaction may come back still further ahead than
               %% the offset allows, so reentry judges it rather than waving it
               %% through on the full stay.
               Short = 2,
               ok = application:set_env(aecore, mempool_future_nonce_tx_ttl, Short),
               ok = application:set_env(aecore, mempool_allow_reentry, true),
               [PubKey] = funded_accounts(1),
               {GCHeight, Dbs} = aec_tx_pool:gc_height_and_dbs(),
               Far = a_signed_tx(PubKey, me, aec_tx_pool:nonce_offset() + 1, 20000),
               ?assertEqual(ok, aec_tx_pool:push(Far, tx_received)),
               ?assertEqual({ok, GCHeight + Short}, gc_ttl_of(Far, Dbs)),

               tx_pool_gc(GCHeight + Short),
               ?assertEqual({ok, []}, aec_tx_pool:peek(infinity)),
               %% Its garbage-collection entry went with it, so what follows is a
               %% fresh judgment and not the old one read back - the collector
               %% only ever lowers an existing entry, never raises it.
               ?assertEqual({error, not_found}, gc_ttl_of(Far, Dbs)),
               ?assertEqual(ok, aec_tx_pool:push(Far, tx_received)),
               ?assertEqual({ok, GCHeight + Short}, gc_ttl_of(Far, Dbs))
       end}},
      {"A mempool stay longer than tx_ttl is capped to it",
       fun() ->
               %% A transaction may shorten its stay, never extend it.
               TxTTL = aec_tx_pool:tx_ttl(),
               ok = application:set_env(aecore, mempool_future_nonce_tx_ttl, TxTTL + 100),
               [PubKey] = funded_accounts(1),
               {GCHeight, Dbs} = aec_tx_pool:gc_height_and_dbs(),
               Far = a_signed_tx(PubKey, me, aec_tx_pool:nonce_offset() + 1, 20000),
               ?assertEqual(ok, aec_tx_pool:push(Far, tx_received)),
               ?assertEqual({ok, GCHeight + TxTTL}, gc_ttl_of(Far, Dbs))
       end},
      {"A sender the chain does not know keeps the full stay",
       fun() ->
               %% Its transactions are offered and retired the way they always
               %% were, so the shorter stay would only take that away.
               ok = application:set_env(aecore, mempool_future_nonce_tx_ttl, 2),
               ok = application:set_env(aecore, mempool_nonce_baseline, 100),
               {GCHeight, Dbs} = aec_tx_pool:gc_height_and_dbs(),
               STx = a_signed_tx(new_pubkey(), me, 50, 20000),
               ?assertEqual(ok, aec_tx_pool:push(STx, tx_received)),
               ?assertEqual({ok, GCHeight + aec_tx_pool:tx_ttl()}, gc_ttl_of(STx, Dbs))
       end},
      {"A generalized-account sender keeps the full stay",
       fun() ->
               %% It carries no nonce of its own, so nothing of its can be too
               %% far ahead of it. Ingress turns its spends away, leaving the
               %% restore of an already accepted one to reach the judgment.
               Short = 2,
               ok = application:set_env(aecore, mempool_future_nonce_tx_ttl, Short),
               [PubKey] = funded_accounts(1),
               {GCHeight, Dbs} = aec_tx_pool:gc_height_and_dbs(),
               Far = a_signed_tx(PubKey, me, aec_tx_pool:nonce_offset() + 1, 20000),
               ?assertEqual(ok, aec_tx_pool:push(Far, tx_received)),
               ?assertEqual({ok, GCHeight + Short}, gc_ttl_of(Far, Dbs)),

               %% Control: while the sender is still basic a restore judges it
               %% short, so the full stay below is the account type talking and
               %% not the batch failing to judge at all.
               ok = aec_tx_pool:stop(),
               {ok, Basic} = aec_tx_pool:start_link(),
               unlink(Basic), %% Leave it for the cleanup
               ?assertEqual({ok, GCHeight + Short}, gc_ttl_of(Far, Dbs)),

               meck:expect(aec_accounts, type,
                           fun(Account) ->
                                   case aec_accounts:pubkey(Account) =:= PubKey of
                                       true  -> generalized;
                                       false -> meck:passthrough([Account])
                                   end
                           end),
               ok = aec_tx_pool:stop(),
               {ok, Pid} = aec_tx_pool:start_link(),
               unlink(Pid), %% Leave it for the cleanup
               ?assertEqual({ok, GCHeight + aec_tx_pool:tx_ttl()}, gc_ttl_of(Far, Dbs))
       end},
      {"A forced push keeps the full stay however far ahead its nonce is",
       fun() ->
               ok = application:set_env(aecore, mempool_future_nonce_tx_ttl, 2),
               [PubKey] = funded_accounts(1),
               {GCHeight, Dbs} = aec_tx_pool:gc_height_and_dbs(),
               %% An ordinary push of this nonce is turned away, so reaching the
               %% pool at all is the forced path, and the stay is what it keeps.
               Far = a_signed_tx(PubKey, me, aec_tx_pool:nonce_offset() + 1, 20000),
               ?assertEqual(ok, aec_tx_pool:force_push(Far, 5000)),
               ?assertEqual({ok, GCHeight + aec_tx_pool:tx_ttl()}, gc_ttl_of(Far, Dbs))
       end},
      {"GC height combines the configured tx_ttl with the tx's own TTL",
       fun() ->
               aec_test_utils:stop_chain_db(),
               PK = new_pubkey(),
               meck:expect(aec_fork_block_settings, genesis_accounts, 0,
                           [{PK, 100000000}]),
               aec_consensus:set_genesis_hash(),
               {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
               aec_test_utils:start_chain_db(),
               {ok,_} = aec_chain_state:insert_block(GenesisBlock),

               %% Bring the chain to height 1 and restart the pool, so that its
               %% GC height is a known, non-zero value. At GC height 0 the two
               %% operands of the min/2 in pool_db_raw_put/6 commute, which
               %% would hide a swap of the configured TTL and the tx's own TTL.
               {ok, KeyBlock1} =
                   aec_block_key_candidate:create(aec_chain:top_block(), PK, PK),
               {ok,_} = aec_chain_state:insert_block(KeyBlock1),
               ok = aec_tx_pool:stop(),
               {ok, Pid} = aec_tx_pool:start_link(),
               unlink(Pid), %% Leave it for the cleanup
               {GCHeight, Dbs} = aec_tx_pool:gc_height_and_dbs(),
               ?assertEqual(1, GCHeight),

               Horizon = GCHeight + aec_tx_pool:tx_ttl(),

               %% No TTL of its own: collected at the configured horizon.
               STx1 = a_signed_tx(PK, me, 1, 20000, 0),
               ?assertEqual(ok, aec_tx_pool:push(STx1)),
               ?assertEqual({ok, Horizon}, gc_ttl_of(STx1, Dbs)),

               %% A TTL below the horizon shortens the tx's stay ...
               TTL2 = GCHeight + 1,
               STx2 = a_signed_tx(PK, me, 2, 20000, TTL2),
               ?assertEqual(ok, aec_tx_pool:push(STx2)),
               ?assertEqual({ok, TTL2}, gc_ttl_of(STx2, Dbs)),

               %% ... while a TTL beyond it must not extend it.
               TTL3 = Horizon + 100,
               STx3 = a_signed_tx(PK, me, 3, 20000, TTL3),
               ?assertEqual(ok, aec_tx_pool:push(STx3)),
               ?assertEqual({ok, Horizon}, gc_ttl_of(STx3, Dbs)),

               %% Restoring the persisted mempool at startup reads the
               %% configured TTL once for the whole restore - the heights it
               %% computes must be the same ones.
               ok = aec_tx_pool:stop(),
               {ok, Pid2} = aec_tx_pool:start_link(),
               unlink(Pid2), %% Leave it for the cleanup
               ?assertEqual({ok, Horizon}, gc_ttl_of(STx1, Dbs)),
               ?assertEqual({ok, TTL2},    gc_ttl_of(STx2, Dbs)),
               ?assertEqual({ok, Horizon}, gc_ttl_of(STx3, Dbs))
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
               {ok, Miner} = aec_keys:candidate_pubkey(),
               {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PubKey1, Miner),
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
               {ok, Candidate2} = aec_block_key_candidate:create(aec_chain:top_block(), PubKey1, PubKey1),
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
               %% Disable the failure mechanism it is too efficient for this test to work...
               meck:expect(aec_tx_pool, failed_txs, 1, ok),

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
               {ok, Miner} = aec_keys:candidate_pubkey(),
               {ok, KeyBlock1} = aec_block_key_candidate:create(aec_chain:top_block(), PubKey, Miner),
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
               aec_tx_pool:top_change(#{type => micro, old_hash => KeyHash1,
                                        new_hash => Top}),

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

%% int_check_nonce/4 in isolation - the offset is an argument, so only the nonce
%% baseline still comes from config; the pool paths above reach only some branches.
int_check_nonce_test_() ->
    Offset = 5,
    Baseline = 3,
    {foreach,
     fun() ->
             ok = application:set_env(aecore, mempool_nonce_baseline, Baseline),
             ok
     end,
     fun(_) ->
             ok = application:unset_env(aecore, mempool_nonce_baseline)
     end,
     [{"An unknown account falls back to the nonce baseline",
       fun() ->
               Check = fun(Lookup, Nonce, CheckNonce) ->
                               aec_tx_pool:int_check_nonce(Lookup, Nonce, CheckNonce, Offset)
                       end,
               %% A missing state tree is treated exactly like a missing account.
               [ begin
                     ?assertEqual(ok, Check(Lookup, Baseline, true)),
                     ?assertEqual({error, nonce_too_high}, Check(Lookup, Baseline + 1, true)),
                     %% Gossiped and candidate checks do not apply the baseline.
                     ?assertEqual(ok, Check(Lookup, Baseline + 1, false))
                 end || Lookup <- [none, {error, no_state_trees}] ]
       end},
      {"A basic account accepts nonces within the offset above its own",
       fun() ->
               Acc = {value, basic_account(7)},
               ?assertEqual({error, nonce_too_low},
                            aec_tx_pool:int_check_nonce(Acc, 6, true, Offset)),
               ?assertEqual({error, nonce_too_low},
                            aec_tx_pool:int_check_nonce(Acc, 7, true, Offset)),
               ?assertEqual(ok, aec_tx_pool:int_check_nonce(Acc, 8, true, Offset)),
               ?assertEqual(ok, aec_tx_pool:int_check_nonce(Acc, 7 + Offset, true, Offset)),
               ?assertEqual({error, nonce_too_high},
                            aec_tx_pool:int_check_nonce(Acc, 8 + Offset, true, Offset)),
               %% Without the offset check an arbitrarily high nonce is allowed,
               %% but nonce_too_low still is not.
               ?assertEqual(ok, aec_tx_pool:int_check_nonce(Acc, 8 + Offset, false, Offset)),
               ?assertEqual({error, nonce_too_low},
                            aec_tx_pool:int_check_nonce(Acc, 7, false, Offset))
       end},
      {"A generalized account may only sign with nonce 0",
       fun() ->
               GA = {value, generalized_account(7)},
               ?assertEqual(ok, aec_tx_pool:int_check_nonce(GA, 0, true, Offset)),
               ?assertEqual(ok, aec_tx_pool:int_check_nonce(GA, 0, false, Offset)),
               %% Any other nonce is rejected regardless of the offset check,
               %% and regardless of how it compares to the account's own nonce.
               ?assertEqual({error, generalized_account_cant_sign_non_meta_tx},
                            aec_tx_pool:int_check_nonce(GA, 1, true, Offset)),
               ?assertEqual({error, generalized_account_cant_sign_non_meta_tx},
                            aec_tx_pool:int_check_nonce(GA, 8, false, Offset))
       end}
     ]}.

basic_account(Nonce) ->
    aec_accounts:set_nonce(aec_accounts:new(<<1:32/unit:8>>, 1000000), Nonce).

generalized_account(Nonce) ->
    {ok, GA} = aec_accounts:attach_ga_contract(
                 basic_account(Nonce),
                 aeser_id:create(contract, <<2:32/unit:8>>),
                 <<0:32/unit:8>>),
    GA.

tx_pool_gc(Height) ->
    aec_tx_pool_gc:sync_gc(Height).

%% The height at which the GC will collect the tx, as recorded in the GC db.
gc_ttl_of(STx, Dbs) ->
    aec_tx_pool_gc:ttl(aetx_sign:hash(STx), Dbs).

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
    {ok, Key} = aec_keys:get_pubkey(),
    aeser_id:create(account, Key);
acct(A) when is_binary(A) ->
    aeser_id:create(account, A).

new_pubkey() ->
    {Pub, Priv} = keypair(),
    ets:insert(?TAB, {Pub, Priv}),
    Pub.

%% N new accounts, funded in a genesis block of their own. Selection sequences an
%% account's transactions by the nonce its state carries, so a test about that
%% ordering needs origins the block state actually knows - an unfunded one falls
%% through to the baseline check instead, and is judged a transaction at a time.
funded_accounts(N) ->
    aec_test_utils:stop_chain_db(),
    PubKeys = [ new_pubkey() || _ <- lists:seq(1, N) ],
    meck:expect(aec_fork_block_settings, genesis_accounts, 0,
                [ {PubKey, 20000000} || PubKey <- PubKeys ]),
    aec_consensus:set_genesis_hash(),
    {GenesisBlock, _} = aec_block_genesis:genesis_block_with_state(),
    aec_test_utils:start_chain_db(),
    {ok, _} = aec_chain_state:insert_block(GenesisBlock),
    PubKeys.

keypair() ->
    #{ public := Pub, secret := Priv } = enacl:sign_keypair(),
    {Pub, Priv}.
