-module(aec_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blocks.hrl").

-define(compareBlockResults(B1, B2),
        ?assertEqual(aec_blocks:serialize_to_binary(B1),
                     aec_blocks:serialize_to_binary(B2))).

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
             {ok, _} = aec_db_error_store:start_link(),
             aec_test_utils:start_chain_db(),
             meck:new(aec_mining, [passthrough]),
             meck:expect(aec_mining, verify, fun(_, _, _, _) -> true end),
             meck:new(aec_events, [passthrough]),
             meck:expect(aec_events, publish, fun(_, _) -> ok end),
             aec_test_utils:mock_genesis_and_forks(),
             TmpDir = aec_test_utils:aec_keys_setup(),
             {ok, PubKey} = aec_keys:pubkey(),
             ok = application:set_env(aecore, beneficiary, aeser_api_encoder:encode(account_pubkey, PubKey)),
             {ok, _} = aec_tx_pool:start_link(),
             {ok, _} = aec_conductor:start_link([{autostart, false}]),
             TmpDir
     end,
     fun(TmpDir) ->
             ok = application:unset_env(aecore, beneficiary),
             ok = aec_conductor:stop(),
             ok = aec_tx_pool:stop(),
             ok = application:stop(gproc),
             meck:unload(aec_mining),
             meck:unload(aec_events),
             aec_test_utils:unmock_genesis_and_forks(),
             aec_test_utils:stop_chain_db(),
             ok = aec_db_error_store:stop(),
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
               ?assertEqual(ok, aec_conductor:post_block(B1)),

               %% B1 should be the top block
               NewTopBlockHash = aec_db:get_top_block_hash(),
               ?assertEqual(block_hash(B1), NewTopBlockHash),

               %% Add another block
               ?assertEqual(ok, aec_conductor:post_block(B2)),

               %% Now B2 should be the top block
               LastTopBlockHash = aec_db:get_top_block_hash(),
               ?assertEqual(block_hash(B2), LastTopBlockHash),

               ok
       end},
      {"Throughput test building chain with 100 blocks in ram",
       fun() ->
               %% Setup
               TotalBlockCount = 100,
               TestFun = fun(B) -> ok = aec_db:write_block(B) end,
               [_GB | Blocks] = aec_test_utils:gen_blocks_only_chain(TotalBlockCount + 1),
               Opts = #{db_mode => ram, test_fun => {aec_db, write_block},
                        block_type => key},
               aec_test_utils:run_throughput_test(TestFun, Blocks, Opts),

               ok
       end}
     ]}.

restart_test_() ->
    {foreach,
     fun() ->
             ok = application:ensure_started(gproc),
             TmpDir = aec_test_utils:aec_keys_setup(),
             {ok, PubKey} = aec_keys:pubkey(),
             ok = application:set_env(aecore, beneficiary, aeser_api_encoder:encode(account_pubkey, PubKey)),
             aec_test_utils:start_chain_db(),
             meck:new(aec_events, [passthrough]),
             meck:expect(aec_events, publish, fun(_, _) -> ok end),
             meck:new(aec_mining, [passthrough]),
             meck:expect(aec_mining, verify, fun(_, _, _, _) -> true end),
             aec_test_utils:mock_genesis_and_forks(),
             {ok, _} = aec_tx_pool:start_link(),
             {ok, _} = aec_conductor:start_link([{autostart, false}]),
             TmpDir
     end,
     fun(TmpDir) ->
             ok = application:unset_env(aecore, beneficiary),
             ok = aec_tx_pool:stop(),
             meck:unload(aec_mining),
             meck:unload(aec_events),
             ok = application:stop(gproc),
             aec_test_utils:unmock_genesis_and_forks(),
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
             aec_test_utils:mock_genesis_and_forks(),
             meck:new(aec_db, [passthrough]),
             TmpDir
     end,
     fun(TmpDir) ->
             aec_test_utils:unmock_genesis_and_forks(),
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

persisted_database_write_error_test_() ->
    {foreach,
     fun() ->
             Persist = application:get_env(aecore, persist),
             application:set_env(aecore, persist, true),
             {ok, _} = aec_db_error_store:start_link(),
             aec_db:check_db(),
             aec_db:clear_db(),
             TmpDir = aec_test_utils:aec_keys_setup(),
             ok = meck:new(mnesia_rocksdb_lib, [passthrough]),
             aec_test_utils:mock_genesis_and_forks(),
             {TmpDir, Persist}
     end,
     fun({TmpDir, Persist}) ->
             application:stop(mnesia),
             aec_test_utils:unmock_genesis_and_forks(),
             aec_test_utils:aec_keys_cleanup(TmpDir),
             application:set_env(aecore, persist, Persist),
             ok = aec_db_error_store:stop(),
             ok = meck:unload(mnesia_rocksdb_lib),
             ok = mnesia:delete_schema([node()])
     end,
     [{"Failed database write will be reported to caller",
       fun() ->
               %% Setup
               Res = {error, bad_thing_happened},
               ok = meck:expect(mnesia_rocksdb_lib, put, fun(_, _, _, _) -> Res end),
               [_GB, B1] = aec_test_utils:gen_blocks_only_chain(2),

               %% Try writing a block
               ?assertExit(Res, aec_db:write_block(B1)),

               %% Cleanup
               ok
       end},
      {"Throughput test building chain with 10 blocks on disc",
       fun() ->
               %% Setup
               TotalBlockCount = 10,
               TestFun = fun(B) -> ok = aec_db:write_block(B) end,
               [_GB | Blocks] = aec_test_utils:gen_blocks_only_chain(TotalBlockCount + 1),
               Opts = #{db_mode => disc, test_fun => {aec_db, write_block},
                        block_type => key},
               aec_test_utils:run_throughput_test(TestFun, Blocks, Opts),

               ok
       end}
     ]}.
