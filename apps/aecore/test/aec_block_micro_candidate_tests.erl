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
-define(TEST_ID, aeser_id:create(account, ?TEST_PUB)).

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
          STx = aec_test_utils:sign_tx(spend_tx(#{}), ?TEST_PRIV),

          AccMap = #{ preset_accounts => [{?TEST_PUB, 100000}] },
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
                     aeser_id:create(account, <<"caller_address........(32 bytes)">>),
                     _CallerNonce = 1,
                     aeser_id:create(contract, <<"contract_address......(32 bytes)">>),
                     _BlockHeight = 42,
                     GasPrice)),

          STx = aec_test_utils:sign_tx(spend_tx(#{}), ?TEST_PRIV),

          AccMap = #{ preset_accounts => [{?TEST_PUB, 100000}] },
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
                              {ok, STxs, [], NewTrees, Events} =
                                  meck:passthrough([STxs, Trees, Env]),
                              case lists:member(STx, STxs) of
                                  false -> {ok, [], STxs, NewTrees, Events};
                                  true ->
                                      NewTreesWithCall =
                                          aec_trees:set_calls(
                                            NewTrees,
                                            aect_call_state_tree:insert_call(
                                              Call,
                                              aec_trees:calls(NewTrees))),
                                      {ok, STxs, [], NewTreesWithCall, Events}
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
        end},
       {"Updating a block does not exceed microblock gas limit",
        fun() ->
            Height = 10,
            Protocol = aec_hard_forks:protocol_effective_at_height(Height),
            SpendTx = fun(Data) -> aec_test_utils:sign_tx(spend_tx(Data), ?TEST_PRIV) end,
            Gas = fun(Txs) -> lists:sum(
                                lists:map(
                                  fun(T) -> aetx:gas_limit(aetx_sign:tx(T), Height, Protocol) end, Txs))
                  end,

            %% Reduce number of spend txs necessary for filling up block
            %% (hence reduce time necessary for running test)
            %% by computing spend tx payload roughly as expensive as 10 spend txs.
            SmallSpendTx = SpendTx(#{nonce => 0, payload => <<>>}),
            PayloadSize = 10 * trunc(Gas([SmallSpendTx]) / aec_governance:byte_gas()),
            Payload = << <<0>> || _ <- lists:seq(1, PayloadSize) >>,

            Tx = fun(N) -> SpendTx(#{nonce   => N,
                                     payload => Payload,
                                     fee     => 20 * aetx:min_fee(aetx_sign:tx(SmallSpendTx), Height, Protocol)}) end,

            %% Compute txs for filling up block.
            MaxGas = aec_governance:block_gas_limit(),
            {ExceedingTx, FillingTxs = [_|_]} =
                (fun F(Nonce, TxsAccIn) ->
                    {true, _} = {Gas(TxsAccIn) =< MaxGas, {TxsAccIn, MaxGas}}, %% Hardcoded expectation - for readability.
                    ThisTx = Tx(Nonce),
                    NextNonce = 1 + Nonce,
                    TxsAccOutTmp = TxsAccIn ++ [ThisTx],
                    case Gas(TxsAccOutTmp) =< MaxGas of
                        false ->
                            {ThisTx, TxsAccIn};
                        true ->
                            F(NextNonce, TxsAccOutTmp)
                    end
                 end)(1, []),
            ?assertMatch(X when X =< MaxGas, Gas(FillingTxs)), %% Hardcoded expectation - for readability.
            ?assertMatch(X when X  > MaxGas, Gas(FillingTxs ++ [ExceedingTx])), %% Hardcoded expectation - for readability.

            AccMap = #{ preset_accounts => [{?TEST_PUB, 100000000000000000000000000000000}] },
            {Block0, Trees0} = aec_block_genesis:genesis_block_with_state(AccMap),
            meck:expect(aec_chain, get_block_state, 1, {ok, Trees0}),
            meck:expect(aec_keys, pubkey, 0, {ok, ?TEST_PUB}),
            meck:expect(aec_db, find_discovered_pof, 1, none),

            %% Create full block, then check that attempting to add one tx fails.
            meck:expect(aec_tx_pool, get_candidate, 2, {ok, FillingTxs}),
            {ok, FullBlock, FullBlockInfo} = aec_block_micro_candidate:create(Block0),
            ?assertEqual(FillingTxs, aec_blocks:txs(FullBlock)), %% Hardcoded expectation - in case any txs discarded for e.g. insufficient funds.
            ?assertEqual(
                %% The total gas of the filling txs is deterministic.
                %% If it equals block gas limit,
                %% this assertion will fail with other error.
                %% If it happens, tune test
                %% in order to misalign total gas of txs with block gas limit
                %% e.g. put additional payload to all spend txs.
                {error, no_update_to_block_candidate},
                aec_block_micro_candidate:update(FullBlock, [ExceedingTx], FullBlockInfo)),
            ok
        end}
      ]}.

get_miner_account_balance(State) ->
    {ok, Miner} = aec_keys:pubkey(),
    aec_accounts:balance(aec_accounts_trees:get(Miner,
                                                aec_trees:accounts(State))).

spend_tx(Data) ->
    DefaultData =
        #{ sender_id => ?TEST_ID, recipient_id => ?TEST_ID
         , amount => 10, fee => 20000, ttl => 100, nonce => 1, payload => <<>> },
    {ok, Tx} = aec_spend_tx:new(maps:merge(DefaultData, Data)),
    Tx.

-endif.
