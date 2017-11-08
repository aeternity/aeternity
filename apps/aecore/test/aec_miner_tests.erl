%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_miner
%%% @end
%%%=============================================================================
-module(aec_miner_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").

-define(TEST_MODULE, aec_miner).
%%-define(DEBUG, true).

-ifdef(DEBUG).
-define(show_miner_state(), ?debugFmt("State ~p~n",[element(1,sys:get_state(?TEST_MODULE))])).
-else.
-define(show_miner_state(), ok).
-endif.

miner_test_() ->
    {foreach,
     fun() ->
             meck:new(application, [unstick, passthrough]),
             aec_test_utils:mock_fast_cuckoo_pow(),
             meck:new(aec_governance, [passthrough]),
             meck:expect(aec_governance, expected_block_mine_rate,
                         fun() ->
                                 meck:passthrough([]) div 2560
                         end),
             aec_test_utils:mock_time(),
             ok = application:ensure_started(gproc),
             ok = application:ensure_started(erlexec),
             TmpKeysDir = aec_test_utils:aec_keys_setup(),
             {ok, _} = aec_tx_pool:start_link(),
             {ok, _} = aec_persistence:start_link(),
             {ok, _} = aec_chain:start_link(aec_block_genesis:genesis_block()),
             {ok, _} = ?TEST_MODULE:start_link([{autostart, true}]),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = ?TEST_MODULE:stop(),
             ok = aec_chain:stop(),
             ok = aec_persistence:stop_and_clean(),
             ok = aec_tx_pool:stop(),
             ok = application:stop(gproc),
             ?assert(meck:validate(aec_governance)),
             meck:unload(aec_governance),
             aec_test_utils:unmock_time(),
             aec_test_utils:aec_keys_cleanup(TmpKeysDir),
             meck:unload(application),
             ok = application:stop(erlexec)
     end,
     [fun(_) ->
              {"Suspend and resume",
               fun() ->
                       ?show_miner_state(),
                       ?assertEqual(ok, ?TEST_MODULE:suspend()),
                       ?show_miner_state(),
                       ?assertEqual(ok, ?TEST_MODULE:resume()),
                       ?show_miner_state(),
                       wait_for_running()
               end} 
      end,
      fun(_) ->
              {"Resume twice",
               fun() ->
                       ?show_miner_state(),
                       ?assertEqual(ok, ?TEST_MODULE:resume()),
                       ?show_miner_state(),
                       ?assertEqual(ok, ?TEST_MODULE:suspend()),
                       ?show_miner_state(),
                       aec_test_utils:wait_for_it(
                         fun() ->
                                 {State, _} = sys:get_state(?TEST_MODULE),
                                 (State =:= idle)
                         end, true),
                       ?assertEqual(ok, ?TEST_MODULE:resume()),
                       ?show_miner_state(),
                       ?TEST_MODULE:resume(),
                       ?show_miner_state(),
                       wait_for_running(),
                       ?assertEqual(ok, ?TEST_MODULE:suspend())
               end}
      end,
      fun(_) ->
              {"Suspend twice",
               fun() ->
                       ?show_miner_state(),
                       ?assertEqual(ok, ?TEST_MODULE:resume()),
                       ?assertEqual(ok, ?TEST_MODULE:suspend()),
                       ?assertEqual(ok, ?TEST_MODULE:suspend()),
                       ?show_miner_state()
               end}
      end,
      fun(_) ->
              {"Suspend in idle",
               fun() ->
                       ?show_miner_state(),
                       ?assertEqual(ok, ?TEST_MODULE:resume()),
                       ?assertEqual(ok, ?TEST_MODULE:suspend()),
                       ?show_miner_state(),
                       aec_test_utils:wait_for_it(
                         fun() ->
                                 {State, _} = sys:get_state(?TEST_MODULE),
                                 State
                         end, idle)
                       %% ?show_miner_state(),

                       %% ?assertEqual(ok, ?TEST_MODULE:suspend())
               end}
      end,
      fun(_) ->
              {timeout, 80,
               {"Run miner for a while",
                fun() ->
                        ?assertEqual(ok, ?TEST_MODULE:suspend()),
                        aec_test_utils:wait_for_it(
                          fun() ->
                                  {State, _} = sys:get_state(?TEST_MODULE),
                                  (State =:= idle)
                          end, true),

                        meck:new(aec_chain, [passthrough]),
                        TestPid = self(),
                        meck:expect(
                          aec_chain, write_block,
                          fun(B) ->
                                  Result = meck:passthrough([B]),
                                  TestPid ! block_written_in_chain,
                                  Result
                          end),
                        ?show_miner_state(),
                        ?assertEqual(ok, ?TEST_MODULE:resume()),
                        ?show_miner_state(),
                        wait_for_running(),
                        ?show_miner_state(),
                        receive block_written_in_chain -> ok end,
                        ?show_miner_state(),
                        aec_test_utils:wait_for_it(fun() ->
                                            {ok, TopBlock} = aec_chain:top(),
                                            aec_blocks:height(TopBlock) > 0
                                    end,
                                    true),
                        ?assertEqual(ok, ?TEST_MODULE:suspend()),
                        ?show_miner_state(),
                        {ok, TopBlock} = aec_chain:top(),
                        ?assertMatch(
                           Txs when is_list(Txs) andalso length(Txs) > 0,
                           aec_blocks:txs(TopBlock)),
                        ?assertMatch(<<H:?TXS_HASH_BYTES/unit:8>> when H > 0,
                                     TopBlock#block.txs_hash),
                        ?assert(meck:validate(aec_chain)),
                        meck:unload(aec_chain)
                end}
              }
      end,
      fun(_) ->
              {timeout, 60,
               {"Remove keys while miner runs",
                fun() ->
                        ?show_miner_state(),
                        ?assertEqual(ok, ?TEST_MODULE:resume()),
                        ?show_miner_state(),
                        wait_for_running(),
                        ?show_miner_state(),
                        ?assertEqual(ok, aec_keys:delete()),
                        ?show_miner_state(),
                        aec_test_utils:wait_for_it(
                         fun () ->
                                 ?show_miner_state(),
                                 {MinerState, _Data1}
                                     = sys:get_state(aec_miner),
                                 MinerState
                         end, waiting_for_keys),
                        ?assertNotEqual(error, aec_keys:new(<<"mynewpassword">>)),
                        ?show_miner_state(),
                        wait_for_running(),
                        ?show_miner_state(),
                        ok
                end}
              }
      end
     ]}.


chain_test_() ->
    {foreach,
     fun() ->
             meck:new(application, [unstick, passthrough]),
             aec_test_utils:mock_fast_cuckoo_pow(),
             ok = application:ensure_started(erlexec),
             application:ensure_started(gproc),
             meck:new(aec_governance, [passthrough]),
             meck:expect(aec_governance, expected_block_mine_rate,
                         fun() ->
                                 meck:passthrough([]) div 2560
                         end),
             TmpKeysDir = aec_test_utils:aec_keys_setup(),
             aec_test_utils:mock_time(),
             {ok, _} = aec_tx_pool:start_link(),
             {ok, _} = aec_persistence:start_link(),
             {ok, _} = aec_chain:start_link(aec_block_genesis:genesis_block()),
             {ok, _} = ?TEST_MODULE:start_link(),
             meck:new(aec_headers, [passthrough]),
             meck:new(aec_blocks, [passthrough]),
             meck:expect(aec_headers, validate, fun(_) -> ok end),
             meck:expect(aec_blocks, validate, fun(_) -> ok end),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = ?TEST_MODULE:stop(),
             ok = aec_chain:stop(),
             ok = aec_persistence:stop_and_clean(),
             ok = aec_tx_pool:stop(),
             aec_test_utils:aec_keys_cleanup(TmpKeysDir),
             ok = application:stop(gproc),
             aec_test_utils:unmock_time(),
             ?assert(meck:validate(aec_governance)),
             meck:unload(aec_governance),
             meck:unload(aec_headers),
             meck:unload(aec_blocks),
             meck:unload(application),
             ok = application:stop(erlexec)
     end,
     [
      fun(_) ->
              {"Start mining add a block.",
               fun() ->
                       %% Add a couple of blocks to the chain.
                       [_GB, B1, B2] = aec_test_utils:gen_block_chain(3),
                       BH2 = aec_blocks:to_header(B2),
                       ?assertEqual(ok, ?TEST_MODULE:post_block(B1)),
                       ?assertEqual(ok, ?TEST_MODULE:post_block(B2)),
                       aec_test_utils:wait_for_it(
                         fun () -> aec_chain:top_header() end,
                         {ok, BH2})
               end}
      end,
      fun(_) ->
              {"Switch to an alternative chain while mining.",
               fun() ->
                       Chain1 = aec_test_utils:gen_block_chain(5),
                       [?TEST_MODULE:post_block(B)
                        || B <- Chain1],

                       [_,_,_,_,B5] = Chain1,
                       BH5 = aec_blocks:to_header(B5),
                       aec_test_utils:wait_for_it(
                         fun () -> aec_chain:top_header() end,
                         {ok, BH5}),

                       %% TODO: Add some transactions to the pool
                       %% Let the miner mine one block

                       Chain2 = aec_test_utils:gen_block_chain(7),
                       [?TEST_MODULE:post_block(B)
                        || B <- Chain2],

                       [_,_,_,_,_,_,B7] = Chain2,
                       BH7 = aec_blocks:to_header(B7),
                       aec_test_utils:wait_for_it(
                         fun () -> aec_chain:top_header() end,
                         {ok, BH7}),

                       %% TODO: check the transaction pool
                       ok
               end
              }
      end
     ]}.

wait_for_running() ->
    aec_test_utils:wait_for_it(
      fun() ->
              {State, _} = sys:get_state(?TEST_MODULE),
              (State =:= running)
                  orelse
                    (State =:= configure)
      end, true).

-endif.
