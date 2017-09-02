-module(aec_coinbase_tx).

%% API
-export([new/2, run/3]).

-behavior(aec_tx).

-include("txs.hrl").

-define(BLOCK_MINE_REWARD, 10). %% To be set in governance


new(#{account := AccountPubkey}, _Trees) ->
    {ok, {#coinbase_tx{account = AccountPubkey}}}.

run(#coinbase_tx{account = AccountPubkey}, Trees0, Height) ->
    AccountsTrees0 = aec_trees:accounts(Trees0),
    {ok, Account0} = aec_accounts:get(AccountPubkey, AccountsTrees0),

    {ok, Account} = aec_accounts:earn(Account0, ?BLOCK_MINE_REWARD, Height),

    {ok, AccountsTrees} = aec_accounts:put(Account, AccountsTrees0),
    Trees = aec_trees:with_accounts(Trees0, AccountsTrees),
    {ok, Trees}.
