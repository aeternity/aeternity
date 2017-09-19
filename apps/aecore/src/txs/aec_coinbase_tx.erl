-module(aec_coinbase_tx).

%% API
-export([new/2, process/3]).

-behavior(aec_tx).

-include("common.hrl").
-include("trees.hrl").
-include("txs.hrl").

-define(BLOCK_MINE_REWARD, 10). %% To be set in governance

-spec new(map(), trees()) -> {ok, coinbase_tx()}.
new(#{account := AccountPubkey}, _Trees) ->
    {ok, #coinbase_tx{account = AccountPubkey}}.

-spec process(coinbase_tx(), trees(), non_neg_integer()) -> {ok, trees()} | {error, term()}.
process(#coinbase_tx{account = AccountPubkey}, Trees0, Height) ->
    AccountsTrees0 = aec_trees:accounts(Trees0),
    case aec_accounts:get(AccountPubkey, AccountsTrees0) of
        {ok, Account0} ->
            %% Update account
            {ok, Account} = aec_accounts:earn(Account0, ?BLOCK_MINE_REWARD, Height),
            {ok, AccountsTrees} = aec_accounts:put(Account, AccountsTrees0),
            Trees = aec_trees:set_accounts(Trees0, AccountsTrees),
            {ok, Trees};
        {error, notfound} ->
            {error, account_not_found}
    end.
