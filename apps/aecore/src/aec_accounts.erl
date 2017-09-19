-module(aec_accounts).

%% API
-export([empty/0,
         new/3,
         get/2,
         get_with_proof/2,
         put/2,
         earn/3,
         root_hash/1,
         verify_proof/3]).

-include("common.hrl").
-include("trees.hrl").

empty() ->
    {ok, _AccountsTree} = aec_trees:new().

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

put(Account, AccountsTree) ->
    {ok, _NewAccountsTree} =
        aec_trees:put(Account#account.pubkey, serialize(Account), AccountsTree).

earn(#account{balance = Balance0} = Account0, Amount, Height) ->
    {ok, Account0#account{balance = Balance0 + Amount,
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
