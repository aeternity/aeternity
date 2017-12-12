-module(aec_accounts).

%% API
-export([empty/0,
         new/3,
         get/2,
         get_with_proof/2,
         get_all_accounts_balances/1,
         put/2,
         balance/1,
         nonce/1,
         height/1,
         earn/3,
         spend/4,
         root_hash/1,
         verify_proof/3]).

-include("common.hrl").
-include("trees.hrl").

%% TODO: remove? We should not need such API (init via trees)
empty() ->
    {ok, _AccountsTree} = aec_trees:new_merkle_tree().

new(Pubkey, Balance, Height) ->
    #account{pubkey = Pubkey, balance = Balance, height = Height}.

get(Pubkey, AccountsTree) ->
    case aec_trees:get(Pubkey, AccountsTree) of
        {ok, SerializedAccount} when is_binary(SerializedAccount) ->
            Account =
                #account{pubkey = Pubkey} = %% Hardcoded expectation.
                deserialize(SerializedAccount),
            {ok, Account};
        {error, notfound} = E ->
            E
    end.

get_with_proof(Pubkey, AccountsTree) ->
    case aec_trees:get_with_proof(Pubkey, AccountsTree) of
        {ok, {SerializedAccount, Proof}} when is_binary(SerializedAccount) ->
            Account =
                #account{pubkey = Pubkey} = %% Hardcoded expectation.
                deserialize(SerializedAccount),
            {ok, {Account, Proof}};
        {error, notfound} = E ->
            E
    end.

-spec get_all_accounts_balances(aec_trees:trees()) -> list({pubkey(), non_neg_integer()}).
get_all_accounts_balances(AccountsTree) ->
    AccountsDump = aec_trees:to_orddict(AccountsTree),
    lists:foldl(
      fun({Pubkey, SerializedAccount}, Acc) ->
              #account{balance = B} = deserialize(SerializedAccount),
              [{Pubkey, B} | Acc]
      end, [], AccountsDump).

put(Account, AccountsTree) ->
    {ok, _NewAccountsTree} =
        aec_trees:put(Account#account.pubkey, serialize(Account), AccountsTree).

balance(#account{balance = Balance}) ->
    Balance.

nonce(#account{nonce = Nonce}) ->
    Nonce.

height(#account{height = Height}) ->
    Height.

earn(#account{balance = Balance0} = Account0, Amount, Height) ->
    {ok, Account0#account{balance = Balance0 + Amount,
                          height = Height}}.

spend(#account{balance = Balance0} = Account0, Amount, Nonce, Height) ->
    {ok, Account0#account{balance = Balance0 - Amount,
                          nonce = Nonce,
                          height = Height}}.

root_hash(AccountsTree) ->
    aec_trees:root_hash(AccountsTree).

verify_proof(Account, RootHash, Proof) ->
    aec_trees:verify_proof(
      Account#account.pubkey, serialize(Account), RootHash, Proof).

serialize(Account) ->
    term_to_binary(Account).

deserialize(SerializedAccount) ->
    binary_to_term(SerializedAccount).
