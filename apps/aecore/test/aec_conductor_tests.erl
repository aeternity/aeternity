%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_miner
%%% @end
%%%=============================================================================
-module(aec_conductor_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").

-define(TEST_MODULE, aec_conductor).

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
             ok = flush_gproc(),
             ?assert(meck:validate(aec_governance)),
             meck:unload(aec_governance),
             aec_test_utils:unmock_time(),
             aec_test_utils:aec_keys_cleanup(TmpKeysDir),
             meck:unload(application)
     end,
     [fun(_) ->
              {"Stop and restart miner",
               fun() ->
                       ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
                       wait_for_stopped(),
                       ?assertEqual(ok, ?TEST_MODULE:start_mining()),
                       wait_for_running(),
                       ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
                       wait_for_stopped(),
                       ?assertEqual(ok, ?TEST_MODULE:start_mining()),
                       wait_for_running(),
                       ok
               end}
      end,
      fun(_) ->
              {"Test consequtive start/stop ",
               fun() ->
                       ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
                       wait_for_stopped(),
                       ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
                       wait_for_stopped(),
                       ?assertEqual(ok, ?TEST_MODULE:start_mining()),
                       wait_for_running(),
                       ?assertEqual(ok, ?TEST_MODULE:start_mining()),
                       wait_for_running(),
                       ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
                       wait_for_stopped()
               end}
      end,
      fun(_) ->
              {timeout, 80,
               {"Run miner for a while",
                fun() ->
                        ?assertEqual(0, get_top_height()),
                        ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
                        wait_for_stopped(),
                        true = aec_events:subscribe(block_created),
                        ?assertEqual(ok, ?TEST_MODULE:start_mining()),
                        wait_for_block_created(),
                        ?assert(0 < get_top_height()),
                        ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
                        ok
                end}
              }
      end
      %% fun(_) ->
      %%         {timeout, 60,
      %%          {"Remove keys while miner runs",
      %%           fun() ->
      %%                   ?assertEqual(ok, ?TEST_MODULE:start_mining()),
      %%                   wait_for_running(),
      %%                   ?assertEqual(ok, aec_keys:delete()),
      %%                   aec_test_utils:wait_for_it(
      %%                    fun () ->

      %%                            waiting_for_keys
      %%                    end, waiting_for_keys),
      %%                   ?assertNotEqual(error, aec_keys:new(<<"mynewpassword">>)),
      %%                   wait_for_running(),
      %%                   ok
      %%           end}
      %%         }
      %% end
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
             meck:unload(application)
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
              {"Test preemption of mining",
               fun() ->
                       %% Stop the miner to be in a controlled environment
                       aec_conductor:stop_mining(),
                       wait_for_stopped(),

                       %% Generate a chain
                       Chain = aec_test_utils:gen_block_chain(7),
                       {Chain1, Chain2} = lists:split(3, Chain),
                       Top1 = lists:last(Chain1),
                       Top2 = lists:last(Chain2),
                       Hash1 = block_hash(Top1),
                       Hash2 = block_hash(Top2),

                       %% Seed the server with the first part of the chain
                       [ok = ?TEST_MODULE:post_block(B) || B <- Chain1],
                       wait_for_top_block_hash(Hash1),

                       %% Start mining and make sure we are starting
                       %% from the correct hash.
                       true = aec_events:subscribe(start_mining),
                       aec_conductor:start_mining(),

                       wait_for_start_mining(Hash1),

                       %% Post the rest of the chain, which will take over.
                       [?TEST_MODULE:post_block(B) || B <- Chain2],
                       wait_for_top_block_hash(Hash2),

                       %% The mining should now have been preempted
                       %% and started over with the new top block hash
                       wait_for_start_mining(Hash2),

                       %% TODO: check the transaction pool
                       ok
               end
              }
      end

     ]}.

block_hash(Block) ->
    {ok, Hash} = aec_blocks:hash_internal_representation(Block),
    Hash.

get_top_height() ->
    {ok, TopBlock} = aec_chain:top(),
    aec_blocks:height(TopBlock).

wait_for_stopped() ->
    aec_test_utils:wait_for_it(fun ?TEST_MODULE:get_mining_state/0, stopped).

wait_for_running() ->
    aec_test_utils:wait_for_it(fun ?TEST_MODULE:get_mining_state/0, running).

wait_for_top_block_hash(Hash) ->
    aec_test_utils:wait_for_it(
      fun () -> aec_chain:top_block_hash() end,
      Hash).

wait_for_block_created() ->
    _ = wait_for_gproc(block_created, 30000),
    ok.

wait_for_start_mining(Hash) ->
    ?debugFmt("Waiting for: ~p", [Hash]),
    Info = wait_for_gproc(start_mining, 1000),
    case proplists:get_value(top_block_hash, Info) of
        Hash -> ok;
        Other ->
            ?debugFmt("Other: ~p", [Other]),
            wait_for_start_mining(Hash)
    end.

wait_for_gproc(Event, Timeout) ->
    receive
        {gproc_ps_event, Event, Info} -> Info
    after Timeout -> error({timeout, block_created})
    end.

flush_gproc() ->
    receive
        {gproc_ps_event, _, _} -> flush_gproc()
    after 0 -> ok
    end.

-endif.
