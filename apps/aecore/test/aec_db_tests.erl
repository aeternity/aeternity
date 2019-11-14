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
      {"Throughput test building chain with 1000 blocks",
       fun() ->
               TotalBlockCount = 1000,

               %% Setup
               [GB | Blocks] = aec_test_utils:gen_blocks_only_chain(TotalBlockCount + 1),
               GHash = block_hash(GB),
               Block = aec_db:get_block(GHash),
               ?compareBlockResults(GB, Block),
               TopBlockHash = aec_db:get_top_block_hash(),
               ?assertEqual(GHash, TopBlockHash),

               InsertBlockFun = fun(B0, AccTime) ->
                                        T0 = erlang:system_time(microsecond),
                                        ok = aec_db:write_block(B0),
                                        T1 = erlang:system_time(microsecond),
                                        [T1 - T0 | AccTime]
                                end,

               %% Run timings
               Timings = lists:foldl(InsertBlockFun, [], Blocks),

               %% Prepare and print data
               TotalRuntime = lists:sum(Timings),
               [Min | _] = TimingsSorted = lists:sort(Timings),
               Max = lists:last(TimingsSorted),
               Range = Max - Min,
               Mean = TotalRuntime div length(Timings),
               Median = lists:nth(ceil(length(Timings) / 2), TimingsSorted),
               Counts = lists:foldl(
                          fun(N, Acc) ->
                                  case lists:keyfind(N, 1, Acc) of
                                      false ->
                                          [{N, 1} | Acc];
                                      {N, Count} ->
                                          lists:keyreplace(N, 1, Acc, {N, Count + 1})
                                  end
                          end, [], Timings),
               [{Mode, _} | _] = lists:keysort(2, Counts),

               io:format(user,
                         "~nThroughput testing results (in microseconds):~n~n"
                         "# of blocks inserted\t= ~p~n"
                         "Total runtime\t\t= ~p~n~n"
                         "Min\t= ~p~n"
                         "Max\t= ~p~n"
                         "Mean\t= ~p~n"
                         "Mode\t= ~p~n"
                         "Range\t= ~p~n"
                         "Median\t= ~p~n",
                         [TotalBlockCount, TotalRuntime, Min, Max, Mean, Mode, Range, Median]
                        ),

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

