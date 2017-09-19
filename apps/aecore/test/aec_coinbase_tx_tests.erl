-module(aec_coinbase_tx_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("trees.hrl").

-define(TEST_MODULE, aec_coinbase_tx).


create_coinbase_tx_existing_account_test() ->
    PubKey = <<"my_pubkey">>,
    Account0 = #account{pubkey = PubKey,
                        balance = 23,
                        height = 6},
    Trees0 = create_state_tree_with_account(Account0),

    {ok, CoinbaseTx} = aec_coinbase_tx:new(#{account => PubKey}, Trees0),
    {ok, Trees} = aec_coinbase_tx:process(CoinbaseTx, Trees0, 9),

    AccountsTree = aec_trees:accounts(Trees),
    {ok, Account} = aec_accounts:get(PubKey, AccountsTree),
    ?assertEqual(PubKey, Account#account.pubkey),
    ?assertEqual(23 + 10, Account#account.balance), %% block reward = 10
    ?assertEqual(9, Account#account.height).

create_coinbase_tx_no_account_test() ->
    PubKey = <<"my_pubkey">>,
    Account0 = #account{pubkey = PubKey},
    Trees0 = create_state_tree(),

    {ok, CoinbaseTx} = aec_coinbase_tx:new(#{account => PubKey}, Trees0),
    ?assertEqual({error, account_not_found}, aec_coinbase_tx:process(CoinbaseTx, Trees0, 9)).


create_state_tree() ->
    {ok, AccountsTree} = aec_accounts:empty(),
    StateTrees0 = #trees{},
    aec_trees:set_accounts(StateTrees0, AccountsTree).

create_state_tree_with_account(Account) ->
    StateTrees0 = create_state_tree(),
    AccountsTree0 = aec_trees:accounts(StateTrees0),
    {ok, AccountsTree} = aec_accounts:put(Account, AccountsTree0),
    aec_trees:set_accounts(StateTrees0, AccountsTree).

-endif.
