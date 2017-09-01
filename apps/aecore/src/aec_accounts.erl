-module(aec_accounts).

%% API
-export([empty/0,
         get/2,
         get_with_proof/2,
         put/2,
         earn/3,
         root_hash/1]).

-include("trees.hrl").

empty() ->
    {ok, gb_merkle_trees:empty()}.

get(Pubkey, AccountsTree) ->
    case gb_merkle_trees:lookup(Pubkey, AccountsTree) of
        none ->
            {error, notfound};
        SerializedAccount when is_binary(SerializedAccount) ->
            Account = deserialize(SerializedAccount),
            {ok, Account}
    end.

get_with_proof(Pubkey, AccountsTree) ->
    case get(Pubkey, AccountsTree) of
        {error, notfound} = E ->
            E;
        {ok, Account} ->
            Proof = gb_merkle_trees:merkle_proof(Account#account.pubkey,
                                                 AccountsTree),
            {ok, {Account, Proof}}
    end.

put(Account, AccountsTree) ->
    NewAccountsTree =
        gb_merkle_trees:enter(Account#account.pubkey, serialize(Account),
                              AccountsTree),
    {ok, NewAccountsTree}.

earn(#account{balance = Balance0}, Amount, Height) ->
    {ok, #account{balance = Balance0 + Amount,
                  height = Height}}.

root_hash(AccountsTree) ->
    case gb_merkle_trees:root_hash(AccountsTree) of
        undefined ->
            {error, empty};
        Hash when is_binary(Hash) ->
            {ok, Hash}
    end.

serialize(Account) ->
    term_to_binary(Account).

deserialize(SerializedAccount) ->
    binary_to_term(SerializedAccount).
