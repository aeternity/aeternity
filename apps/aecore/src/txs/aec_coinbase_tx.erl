-module(aec_coinbase_tx).

%% API
-export([new/2,
         initiating_account/1,
         initiating_account_nonce/1,
         check/3,
         process/3]).

-behavior(aec_tx).

-include("common.hrl").
-include("trees.hrl").
-include("txs.hrl").


-spec new(map(), trees()) -> {ok, coinbase_tx()}.
new(#{account := AccountPubkey}, _Trees) ->
    %% TODO Populate nonce in coinbase tx with the nonce value that
    %% succeeds the nonce in the account as per specified state
    %% account tree.
    {ok, #coinbase_tx{account = AccountPubkey}}.

initiating_account(Tx) ->
    Tx#coinbase_tx.account.

initiating_account_nonce(Tx) ->
    Tx#coinbase_tx.nonce.

-spec check(coinbase_tx(), trees(), non_neg_integer()) -> {ok, trees()} | {error, term()}.
check(#coinbase_tx{account = AccountPubkey}, Trees0, Height) ->
    AccountsTrees0 = aec_trees:accounts(Trees0),
    case aec_accounts:get(AccountPubkey, AccountsTrees0) of
        {ok, _Account} ->
            {ok, Trees0};
        {error, notfound} ->
            %% Add newly referenced account (w/0 amount) to the state
            Account = aec_accounts:new(AccountPubkey, 0, Height),
            {ok, AccountsTrees} = aec_accounts:put(Account, AccountsTrees0),
            Trees = aec_trees:set_accounts(Trees0, AccountsTrees),
            {ok, Trees}
    end.

-spec process(coinbase_tx(), trees(), non_neg_integer()) -> {ok, trees()} | {error, term()}.
process(#coinbase_tx{account = AccountPubkey}, Trees0, Height) ->
    AccountsTrees0 = aec_trees:accounts(Trees0),
    case aec_accounts:get(AccountPubkey, AccountsTrees0) of
        {ok, Account0} ->
            %% TODO Check that nonce in coinbase tx is strictly
            %% greater than nonce in account.  This may need to be
            %% checked in the `check` function.
            Reward = aec_governance:block_mine_reward(),
            {ok, Account} = aec_accounts:earn(Account0, Reward, Height),
            {ok, AccountsTrees} = aec_accounts:put(Account, AccountsTrees0),
            Trees = aec_trees:set_accounts(Trees0, AccountsTrees),
            {ok, Trees};
        {error, notfound} ->
            {error, account_not_found}
    end.
