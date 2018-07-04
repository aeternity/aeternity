-module(aec_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blocks.hrl").

-define(compareBlockResults(B1, B2),
        ?assertEqual(aec_blocks:serialize_to_map(B1),
                     aec_blocks:serialize_to_map(B2))).

block_hash(B) ->
    {ok, Hash} = aec_headers:hash_header(aec_blocks:to_header(B)),
    Hash.

kill_and_restart_conductor() ->
    %% Stop server
    ok = aec_conductor:stop(),
    %% check that it is dead.
    dead =
        try sys:get_status(aec_conductor)
        catch exit:{noproc, _} -> dead
        end,
    %% Restart server
    {ok, _} = aec_conductor:start_link([{autostart, false}]),
    server_up = wait_for_conductor(),
    ok.

wait_for_conductor() ->
    try sys:get_status(aec_conductor),
        server_up
    catch exit:{noproc, _} ->
        timer:sleep(10),
        wait_for_conductor()
    end.

write_chain_test_() ->
    {foreach,
     fun() ->
             ok = application:ensure_started(gproc),
             aec_test_utils:start_chain_db(),
             meck:new(aec_pow_cuckoo, [passthrough]),
             meck:expect(aec_pow_cuckoo, verify, fun(_, _, _, _) -> true end),
             meck:new(aec_events, [passthrough]),
             meck:expect(aec_events, publish, fun(_, _) -> ok end),
             aec_test_utils:mock_genesis(),
             TmpDir = aec_test_utils:aec_keys_setup(),
             {ok, PubKey} = aec_keys:pubkey(),
             ok = application:set_env(aecore, beneficiary, aec_base58c:encode(account_pubkey, PubKey)),
             {ok, _} = aec_tx_pool:start_link(),
             {ok, _} = aec_conductor:start_link([{autostart, false}]),
             TmpDir
     end,
     fun(TmpDir) ->
             ok = application:unset_env(aecore, beneficiary),
             ok = aec_conductor:stop(),
             ok = aec_tx_pool:stop(),
             ok = application:stop(gproc),
             meck:unload(aec_pow_cuckoo),
             meck:unload(aec_events),
             aec_test_utils:unmock_genesis(),
             aec_test_utils:stop_chain_db(),
             aec_test_utils:aec_keys_cleanup(TmpDir)
     end,
     [{"Write a block to chain and read it back.",
       fun() ->
               GB = aec_test_utils:genesis_block(),
               ok = aec_conductor:post_block(GB),

               Hash = block_hash(GB),

               Block = aec_db:get_block(Hash),

               %% Check block apart from state trees.
               ?compareBlockResults(GB, Block),

               %% Genesis should be top block
               TopBlockHash = aec_db:get_top_block_hash(),
               ?assertEqual(Hash, TopBlockHash),

               ok
       end},
      {"Build chain with genesis block plus 2 blocks",
       fun() ->
               [GB, B1, B2] = aec_test_utils:gen_blocks_only_chain(3),

               GHash = block_hash(GB),
               Block = aec_db:get_block(GHash),

               %% Check block apart from state trees.
               ?compareBlockResults(GB, Block),

               %% Genesis should be top block
               TopBlockHash = aec_db:get_top_block_hash(),
               ?assertEqual(GHash, TopBlockHash),

               %% Add one block
               ?assertEqual(ok, aec_conductor:post_block(B2)),

               %% GB should still be top block
               NewTopBlockHash = aec_db:get_top_block_hash(),
               ?assertEqual(GHash, NewTopBlockHash),

               %% Add missing block
               ?assertEqual(ok, aec_conductor:post_block(B1)),

               %% Now B2 should be the top block
               LastTopBlockHash = aec_db:get_top_block_hash(),
               B2Hash = block_hash(B2),
               ?assertEqual(B2Hash, LastTopBlockHash),

               ok
       end}
     ]}.


restart_test_() ->
    {foreach,
     fun() ->
             ok = application:ensure_started(gproc),
             TmpDir = aec_test_utils:aec_keys_setup(),
             {ok, PubKey} = aec_keys:pubkey(),
             ok = application:set_env(aecore, beneficiary, aec_base58c:encode(account_pubkey, PubKey)),
             aec_test_utils:start_chain_db(),
             meck:new(aec_events, [passthrough]),
             meck:expect(aec_events, publish, fun(_, _) -> ok end),
             meck:new(aec_pow_cuckoo, [passthrough]),
             meck:expect(aec_pow_cuckoo, verify, fun(_, _, _, _) -> true end),
             aec_test_utils:mock_genesis(),
             {ok, _} = aec_tx_pool:start_link(),
             {ok, _} = aec_conductor:start_link([{autostart, false}]),
             TmpDir
     end,
     fun(TmpDir) ->
             ok = application:unset_env(aecore, beneficiary),
             ok = aec_tx_pool:stop(),
             meck:unload(aec_pow_cuckoo),
             meck:unload(aec_events),
             ok = application:stop(gproc),
             aec_test_utils:unmock_genesis(),
             aec_test_utils:stop_chain_db(),
             aec_test_utils:aec_keys_cleanup(TmpDir)
     end,
     [{"Build chain, then kill server, check that chain is read back.",
       fun() ->
               [GB, B1, B2] = aec_test_utils:gen_blocks_only_chain(3),
               ?assertEqual({ok, GB}, aec_chain:get_key_block_by_height(0)),
               ?assertEqual(ok, aec_conductor:post_block(B1)),
               ?assertEqual(ok, aec_conductor:post_block(B2)),
               %% Now B2 should be the top block
               TopBlockHash = aec_db:get_top_block_hash(),
               B2Hash = block_hash(B2),
               ?assertEqual(B2Hash, TopBlockHash),
               ChainTop1 = aec_chain:top_block(),
               ?compareBlockResults(B2, ChainTop1),

               %% Check the state trees from persistence
               {ok, ChainTop1Hash} =
                   aec_blocks:hash_internal_representation(ChainTop1),
               {ok, ChainTop1State} =
                   aec_chain:get_block_state(ChainTop1Hash),
               ?assertEqual(ChainTop1State,
                            aec_db:get_block_state(TopBlockHash)),

               %% Kill chain server

               kill_and_restart_conductor(),
               NewTopBlockHash = aec_db:get_top_block_hash(),
               ?assertEqual(B2Hash, NewTopBlockHash),

               ChainTop2 = aec_chain:top_block(),
               ?compareBlockResults(B2, ChainTop2),

               %% Compare the trees after restart
               %% Check the state trees from persistence
               {ok, ChainTop2Hash} =
                   aec_blocks:hash_internal_representation(ChainTop2),
               {ok, ChainTop2State} =
                   aec_chain:get_block_state(ChainTop2Hash),
               ?assertEqual(ChainTop1State,
                            ChainTop2State),

               ok
       end}
     ]}.

persisted_valid_gen_block_test_() ->
    {foreach,
     fun() ->
             TmpDir = aec_test_utils:aec_keys_setup(),
             aec_test_utils:mock_genesis(),
             meck:new(aec_db, [passthrough]),
             TmpDir
     end,
     fun(TmpDir) ->
             aec_test_utils:unmock_genesis(),
             aec_test_utils:aec_keys_cleanup(TmpDir),
             meck:unload(aec_db),
             application:set_env(aecore, persist, false)
     end,
     [{"Check persisted validation of genesis block, persistence OFF",
       fun() ->
            application:set_env(aecore, persist, false),
            ?assertEqual(true, aec_db:persisted_valid_genesis_block())
       end},
      {"Check persisted validation of genesis block, persistence ON",
       fun() ->
            application:set_env(aecore, persist, true),

            meck:expect(aec_db, get_genesis_hash, 0, undefined),
            ?assertEqual(true, aec_db:persisted_valid_genesis_block()),

            {ok, ExpectedGH} = aec_headers:hash_header(aec_block_genesis:genesis_header()),

            meck:expect(aec_db, get_genesis_hash, 0, ExpectedGH),
            ?assertEqual(true, aec_db:persisted_valid_genesis_block()),

            meck:expect(aec_db, get_genesis_hash, 0, <<"invalid genesis block hash">>),
            ?assertEqual(false, aec_db:persisted_valid_genesis_block()),
            ok
       end}
     ]}.

