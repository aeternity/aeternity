-module(aec_tx_test_utils).

-export([create_state_tree/0,
         create_state_tree_with_account/1,
         create_state_tree_with_accounts/1]).

-include("common.hrl").
-include("blocks.hrl").

create_state_tree() ->
    {ok, AccountsTree} = aec_accounts:empty(),
    StateTrees0 = #trees{},
    aec_trees:set_accounts(StateTrees0, AccountsTree).

create_state_tree_with_account(Account) ->
    create_state_tree_with_accounts([Account]).

create_state_tree_with_accounts(Accounts) ->
    {ok, AccountsTree0} = aec_accounts:empty(),
    AccountsTree1 = lists:foldl(
                      fun(Account, Tree0) ->
                              {ok, Tree} = aec_accounts:put(Account, Tree0),
                              Tree
                      end, AccountsTree0, Accounts),
    StateTrees0 = #trees{},
    aec_trees:set_accounts(StateTrees0, AccountsTree1).
