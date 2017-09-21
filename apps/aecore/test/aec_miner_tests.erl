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
             meck:new(aec_chain, [passthrough]),
             meck:new(aeu_time, [passthrough]),
             meck:new(aec_governance, [passthrough]),
             meck:expect(
               aec_chain, top,
               fun() ->
                       %% TODO Remove this workaround once
                       %% `aec_chain:top/0` returns state trees.
                       {ok, B} = meck:passthrough([]),
                       {ok, B#block{trees = (aec_block_genesis:genesis_block())#block.trees}}
               end),
             %% As unit test run with speeded-up PoW, difficulty calculation may fail as
             %% block timestamps are rounded to seconds. Use millisecs here.
             meck:expect(aeu_time, msecs_to_secs, fun(Ms) -> Ms end),
             meck:expect(aec_governance, expected_block_mine_rate,
                         fun() ->
                                 meck:passthrough([])
                         end),
             TmpKeysDir = mktempd(),
             ok = application:ensure_started(crypto),
             {ok, _} = aec_keys:start_link(["mypassword", TmpKeysDir]),
             {ok, _} = ?TEST_MODULE:start_link(),
             TmpKeysDir
     end,
     fun(TmpKeysDir) ->
             ok = ?TEST_MODULE:stop(),
             ok = aec_keys:stop(),
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
             ?assert(meck:validate(aec_chain)),
             ?assert(meck:validate(aec_governance)),
             ?assert(meck:validate(aeu_time)),
             meck:unload(aec_governance),
             meck:unload(aeu_time),
             meck:unload(aec_chain),
             file:delete(TmpKeysDir)
     end,
     [fun(_) ->
             [{"Start and stop",
               fun() ->
                       ?assertEqual(ok, ?TEST_MODULE:start_mining()),
                       ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
                       ?assertEqual(ok, ?TEST_MODULE:start_mining()),
                       ?assertEqual(ok, ?TEST_MODULE:stop_mining())
               end}
             ]
      end,
      fun(_) ->
              [{"Start twice",
               fun() ->
                       ?assertEqual(ok, ?TEST_MODULE:start_mining()),
                       ?assertEqual({error,already_started}, ?TEST_MODULE:start_mining()),
                       ?assertEqual(ok, ?TEST_MODULE:stop_mining())
               end},
              {"Stop twice",
               fun() ->
                       ?assertEqual(ok, ?TEST_MODULE:start_mining()),
                       ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
                       ?assertEqual({error,not_started}, ?TEST_MODULE:stop_mining())
               end},
              {"Stop in idle",
               fun() ->
                       ?assertEqual({error,not_started}, ?TEST_MODULE:stop_mining())
               end},
              {timeout, 60,
               {"Run miner for a while",
                fun() ->
                        ?assertEqual(ok, ?TEST_MODULE:start_mining()),
                        timer:sleep(20000),
                        ?assertEqual(ok, ?TEST_MODULE:stop_mining()),
                        {ok, TopBlock} = aec_chain:top(),
                        ?debugFmt("reached height ~p~n", [aec_blocks:height(TopBlock)]),
                        ?assert(aec_blocks:height(TopBlock) > 0),
                        ?debugFmt("reached balance ~p~n", [?TEST_MODULE:get_balance()]),
                        ?assertEqual(1, length(TopBlock#block.txs)),
                        ?assertMatch(<<H:?TXS_HASH_BYTES/unit:8>> when H > 0,
                                                                       TopBlock#block.txs_hash)
                end}
              }
             ]
      end
     ]}.

mktempd() ->
    mktempd(os:type()).

mktempd({unix, _}) ->
    lib:nonl(?cmd("mktemp -d")).

-endif.
