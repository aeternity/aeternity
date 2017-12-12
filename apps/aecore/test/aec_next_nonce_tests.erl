-module(aec_next_nonce_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_next_nonce).

-define(PUBKEY, <<"BAAggMEhrC3ODBqlYeQ6dk00F87AKMkV6kkyhgfJ/luOzGUC+4APxFkVgAYPai3TjSyLRObv0GeDACg1ZxwnfHY=">>).

pick_for_account_test_() ->
    {foreach,
     fun() ->
             meck:new(aec_conductor),
             meck:new(aec_tx_pool)
     end,
     fun(_) ->
             meck:unload(aec_conductor),
             meck:unload(aec_tx_pool)
     end,
     [{"Return account_not_found when both top block state tree and mempool are empty",
       fun() ->
               StateTrees = aec_test_utils:create_state_tree(),
               meck:expect(aec_tx_pool, get_max_nonce, fun(?PUBKEY) -> undefined end),
               TopBlock = #block{trees = StateTrees},
               ?assertEqual({error, account_not_found},
                            ?TEST_MODULE:pick_for_account(?PUBKEY, TopBlock))
       end},
      {"Return incremented state tree nonce for account existing in state tree and empty mempool",
       fun() ->
               Account = #account{pubkey = ?PUBKEY, nonce = 8},
               StateTrees = aec_test_utils:create_state_tree_with_account(Account),
               meck:expect(aec_tx_pool, get_max_nonce, fun(?PUBKEY) -> undefined end),
               TopBlock = #block{trees = StateTrees},
               ?assertEqual({ok, 9},
                            ?TEST_MODULE:pick_for_account(?PUBKEY, TopBlock))
       end},
      {"Return [max(mempool account nonce, state tree account nonce) + 1] for account present in state and having txs in mempool",
       fun() ->
               Account = #account{pubkey = ?PUBKEY, nonce = 8},
               StateTrees = aec_test_utils:create_state_tree_with_account(Account),
               TopBlock = #block{trees = StateTrees},
               meck:expect(aec_tx_pool, get_max_nonce, fun(?PUBKEY) -> {ok, 12} end),

               ?assertEqual({ok, 13},
                            ?TEST_MODULE:pick_for_account(?PUBKEY, TopBlock))
       end}]}.
