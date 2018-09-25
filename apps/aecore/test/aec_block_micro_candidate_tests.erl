%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_block_{key,micro}_candidate module
%%% @end
%%%=============================================================================
-module(aec_block_micro_candidate_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").
-define(TEST_PUB, <<181,14,1,149,98,104,64,190,242,52,152,159,190,216,30,49,
                    94,251,20,75,9,85,29,82,35,178,98,75,188,72,242,141>>).
-define(TEST_PRIV, <<220,220,95,208,13,103,52,54,220,60,93,149,153,25,95,67,
                     178,143,191,176,251,107,170,15,223,140,13,57,96,171,79,
                     43,181,14,1,149,98,104,64,190,242,52,152,159,190,216,30,
                     49,94,251,20,75,9,85,29,82,35,178,98,75,188,72,242,141>>).
-define(TEST_ID, aec_id:create(account, ?TEST_PUB)).

block_extension_test_() ->
    {foreach,
      fun() ->
        meck:new(aeu_time, [passthrough]),
        meck:new(aec_chain, [passthrough]),
        meck:new(aec_db, [passthrough]),
        meck:new(aec_keys, [passthrough]),
        meck:new(aec_tx_pool, [passthrough]),
        meck:new(aec_trees, [passthrough])
      end,
      fun(_) ->
        meck:unload(aec_trees),
        meck:unload(aec_tx_pool),
        meck:unload(aec_keys),
        meck:unload(aec_chain),
        meck:unload(aec_db),
        meck:unload(aeu_time)
      end,
      [{"Generate a block in one step, compared with two steps, with a spend tx",
        fun() ->
          {ok, Tx} = aec_spend_tx:new(#{ sender_id => ?TEST_ID, recipient_id => ?TEST_ID
                                       , amount => 10, fee => 1, ttl => 100, nonce => 1, payload => <<>> }),
          STx = aec_test_utils:sign_tx(Tx, ?TEST_PRIV),

          AccMap = #{ preset_accounts => [{?TEST_PUB, 1000}] },
          {Block0, Trees0} = aec_block_genesis:genesis_block_with_state(AccMap),

          meck:expect(aeu_time, now_in_msecs, 0, 1234567890),
          meck:expect(aec_chain, get_block_state, 1, {ok, Trees0}),
          meck:expect(aec_tx_pool, get_candidate, 2, {ok, [STx]}),
          meck:expect(aec_keys, pubkey, 0, {ok, ?TEST_PUB}),
          meck:expect(aec_db, find_discovered_pof, 1, none),
          {ok, Block1A, #{ trees := _Trees1A }} = aec_block_micro_candidate:create(Block0),

          meck:expect(aec_tx_pool, get_candidate, 2, {ok, []}),
          {ok, Block1B0, BInfo} = aec_block_micro_candidate:create(Block0),

          {ok, Block1B, #{ trees := _Trees1B }} =
                aec_block_micro_candidate:update(Block1B0, [STx], BInfo),

          ?assertEqual(Block1A, Block1B)
        end},
       {"Generate a block in one step, compared with two steps, with contract calls (and a spend tx)",
        fun() ->
          GasPrice = 3,
          GasUsed = 7,
          Call = aect_call:set_gas_used(
                   GasUsed,
                   aect_call:new(
                     aec_id:create(account, <<"caller_address........(32 bytes)">>),
                     _CallerNonce = 1,
                     aec_id:create(contract, <<"contract_address......(32 bytes)">>),
                     _BlockHeight = 42,
                     GasPrice)),

          {ok, Tx} = aec_spend_tx:new(#{ sender_id => ?TEST_ID, recipient_id => ?TEST_ID
                                       , amount => 10, fee => 1, ttl => 100, nonce => 1, payload => <<>> }),
          STx = aec_test_utils:sign_tx(Tx, ?TEST_PRIV),

          AccMap = #{ preset_accounts => [{?TEST_PUB, 1000}] },
          {Block0, Trees0} = aec_block_genesis:genesis_block_with_state(AccMap),

          meck:expect(aeu_time, now_in_msecs, 0, 1234567890),
          meck:expect(aec_chain, get_block_state, 1, {ok, Trees0}),
          meck:expect(aec_tx_pool, get_candidate, 2, {ok, [STx]}),
          meck:expect(aec_keys, pubkey, 0, {ok, ?TEST_PUB}),
          meck:expect(aec_db, find_discovered_pof, 1, none),

          {ok,_Block1A, #{ trees := Trees1A }} = aec_block_micro_candidate:create(Block0),

          %% Amend call state tree, in order not to require calling
          %% actual contract that would make this unit test
          %% unnecessary complex.
          meck:expect(aec_trees, apply_txs_on_state_trees,
                      fun(STxs, Trees, Env) ->
                              {ok, STxs, [], NewTrees} =
                                  meck:passthrough([STxs, Trees, Env]),
                              case lists:member(STx, STxs) of
                                  false -> {ok, [], STxs, NewTrees};
                                  true ->
                                      NewTreesWithCall =
                                          aec_trees:set_calls(
                                            NewTrees,
                                            aect_call_state_tree:insert_call(
                                              Call,
                                              aec_trees:calls(NewTrees))),
                                      {ok, STxs, [], NewTreesWithCall}
                              end
                      end),

          {ok, Block1B, #{ trees := Trees1B }} = aec_block_micro_candidate:create(Block0),

          ?assertEqual(get_miner_account_balance(Trees1A), %% NG: Not yet + GasUsed * GasPrice,
                       get_miner_account_balance(Trees1B)),

          meck:expect(aec_tx_pool, get_candidate, 2, {ok, []}),
          {ok, Block1C0, BInfo} = aec_block_micro_candidate:create(Block0),

          {ok, Block1C, #{ trees := _Trees1B }} =
                aec_block_micro_candidate:update(Block1C0, [STx], BInfo),

          ?assertEqual(Block1B, Block1C)
        end}
      ]}.

get_miner_account_balance(State) ->
    {ok, Miner} = aec_keys:pubkey(),
    aec_accounts:balance(aec_accounts_trees:get(Miner,
                                                aec_trees:accounts(State))).

-endif.
