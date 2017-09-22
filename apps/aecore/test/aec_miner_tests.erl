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

-define(TEST_MODULE, aec_miner).

miner_test_() ->
    {foreach,
     fun() ->
             meck:new(aeu_time, [passthrough]),
             meck:new(aec_governance, [passthrough]),
             meck:expect(aeu_time, msecs_to_secs, fun(Ms) -> Ms end),
             meck:expect(aec_governance, expected_block_mine_rate,
                         fun() ->
                                 meck:passthrough([])
                         end),
             {ok, _} = aec_chain:start_link(aec_block_genesis:genesis_block()),
             {ok, _} = aec_state:start_link(),
             TmpKeysDir = mktempd(),
             ok = application:ensure_started(crypto),
             {ok, _} = aec_keys:start_link(["mypassword", TmpKeysDir]),
             {ok, _} = ?TEST_MODULE:start_link(),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = ?TEST_MODULE:stop(),
             ok = aec_keys:stop(),
             ok = aec_state:stop(),
             ok = aec_chain:stop(),
             ok = application:stop(crypto),
             {ok, KeyFiles} = file:list_dir(TmpKeysDir),
             %% Expect two filenames - private and public keys.
             [_KF1, _KF2] = KeyFiles,
             lists:foreach(
               fun(F) ->
                       AbsF = filename:absname_join(TmpKeysDir, F),
                       {ok, _} = {file:delete(AbsF), {F, AbsF}}
               end,
               KeyFiles),
             ok = file:del_dir(TmpKeysDir),
             ?assert(meck:validate(aec_governance)),
             ?assert(meck:validate(aeu_time)),
             meck:unload(aec_governance),
             meck:unload(aeu_time),
             file:delete(TmpKeysDir)
     end,
     [fun(_) ->
              {"Suspend and resume",
               fun() ->
                       ?assertEqual(ok, ?TEST_MODULE:suspend()),
                       ?assertEqual(ok, ?TEST_MODULE:resume()),
                       ?assertEqual(ok, ?TEST_MODULE:suspend())
               end}
      end,
      fun(_) ->
              {"Resume twice",
               fun() ->
                       ?assertEqual(ok, ?TEST_MODULE:suspend()),
                       ?assertEqual(ok, ?TEST_MODULE:resume()),
                       ?assertEqual({error,already_started}, ?TEST_MODULE:resume()),
                       ?assertEqual(ok, ?TEST_MODULE:suspend())
               end}
      end,
      fun(_) ->
              {"Suspend twice",
               fun() ->
                       ?assertEqual(ok, ?TEST_MODULE:suspend()),
                       ?assertEqual({error,not_started}, ?TEST_MODULE:suspend())
               end}
      end,
      fun(_) ->
              {"Suspend in idle",
               fun() ->
                       ?assertEqual(ok, ?TEST_MODULE:suspend()),
                       ?assertEqual({error,not_started}, ?TEST_MODULE:suspend())
               end}
      end,
      fun(_) ->
              {timeout, 60,
               {"Run miner for a while",
                fun() ->
                        timer:sleep(5000),    %% TODO: make it event-driven
                        ?assertEqual(ok, ?TEST_MODULE:suspend()),
                        {ok, TopBlock} = aec_chain:top(),
                        ?debugFmt("reached height ~p~n", [aec_blocks:height(TopBlock)]),
                        ?assert(aec_blocks:height(TopBlock) > 0),
                        ?debugFmt("reached balance ~p~n", [?TEST_MODULE:get_balance()]),
                        ?assertEqual(1, length(TopBlock#block.txs)),
                        ?assertMatch(<<H:?TXS_HASH_BYTES/unit:8>> when H > 0, TopBlock#block.txs_hash)
                end}
              }
      end,
      fun(_) ->
              {"Remove keys while miner runs",
               fun() ->
                       ?assertEqual({running, {state}}, sys:get_state(aec_miner)),
                       ?assertEqual(ok, aec_keys:delete()),
                       timer:sleep(100),
                       ?assertEqual({waiting_for_keys, {state}}, sys:get_state(aec_miner)),
                       ?assertNotEqual(error, aec_keys:new("mynewpassword")),
                       timer:sleep(100),
                       ?assertEqual({running, {state}}, sys:get_state(aec_miner))
               end}
      end
     ]}.

mktempd() ->
    mktempd(os:type()).

mktempd({unix, _}) ->
    lib:nonl(?cmd("mktemp -d")).

-endif.
