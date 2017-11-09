-module(aec_tx_common_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("trees.hrl").

-define(TEST_MODULE, aec_tx_common).

ensure_account_at_height_test_() ->
    [{"Not existing account is created with 0 balance",
      fun() ->
              Trees0 = aec_test_utils:create_state_tree(),
              AccountPubkey = <<"account_pubkey">>,
              BlockHeight = 23,

              {ok, Trees} = ?TEST_MODULE:ensure_account_at_height(AccountPubkey, Trees0, BlockHeight),

              AccountsTree = aec_trees:accounts(Trees),
              ExpectedAccount = #account{pubkey = AccountPubkey,
                                         balance = 0,
                                         height = BlockHeight},
              ?assertEqual({ok, ExpectedAccount}, aec_accounts:get(AccountPubkey, AccountsTree))
      end},
     {"account_height_too_bit is returned on block height lower than current account height",
      fun() ->
              AccountPubkey = <<"account_pubkey">>,
              AccountHeight = 25,
              BlockHeight = 23,
              Account = #account{pubkey = AccountPubkey,
                                 balance = 777,
                                 height = AccountHeight},
              Trees = aec_test_utils:create_state_tree_with_account(Account),

              ?assertEqual({error, account_height_too_big},
                           ?TEST_MODULE:ensure_account_at_height(AccountPubkey, Trees, BlockHeight))
      end},
     {"Same unmodified state tree is returned when account is present",
      fun() ->
              AccountPubkey = <<"account_pubkey">>,
              AccountHeight = 21,
              BlockHeight = 23,
              Account = #account{pubkey = AccountPubkey,
                                 balance = 777,
                                 height = AccountHeight},
              Trees = aec_test_utils:create_state_tree_with_account(Account),

              ?assertEqual({ok, Trees},
                           ?TEST_MODULE:ensure_account_at_height(AccountPubkey, Trees, BlockHeight))
      end}].

