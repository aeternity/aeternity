%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Accounts, and Merkle trees of accounts.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_accounts).

%% API - account
-export([new/3,
         balance/1,
         nonce/1,
         height/1,
         earn/3,
         spend/4]).

%% API - accounts tree
-export([empty/0,
         get/2,
         get_with_proof/2,
         get_all_accounts_balances/1,
         put/2,
         root_hash/1,
         verify_proof/3]).

-export_type([tree/0]).

-include("common.hrl").
-include("trees.hrl").

-type deterministic_account_binary_with_pubkey() :: binary().

-type key() :: pubkey().
-type value() :: deterministic_account_binary_with_pubkey().
-type tree() :: aeu_mtrees:mtree(key(), value()).

-spec empty() -> {ok, tree()}.
empty() ->
    {ok, _AccountsTree} = aeu_mtrees:new().

new(Pubkey, Balance, Height) ->
    #account{pubkey = Pubkey, balance = Balance, height = Height}.

get(Pubkey, AccountsTree) ->
    case aeu_mtrees:get(Pubkey, AccountsTree) of
        {ok, SerializedAccount} when is_binary(SerializedAccount) ->
            Account =
                #account{pubkey = Pubkey} = %% Hardcoded expectation.
                deserialize(SerializedAccount),
            {ok, Account};
        {error, notfound} = E ->
            E
    end.

get_with_proof(Pubkey, AccountsTree) ->
    case aeu_mtrees:get_with_proof(Pubkey, AccountsTree) of
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
    AccountsDump = aeu_mtrees:to_orddict(AccountsTree),
    lists:foldl(
      fun({Pubkey, SerializedAccount}, Acc) ->
              #account{balance = B} = deserialize(SerializedAccount),
              [{Pubkey, B} | Acc]
      end, [], AccountsDump).

put(Account, AccountsTree) ->
    {ok, _NewAccountsTree} =
        aeu_mtrees:put(Account#account.pubkey, serialize(Account), AccountsTree).

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
    aeu_mtrees:root_hash(AccountsTree).

verify_proof(Account, RootHash, Proof) ->
    aeu_mtrees:verify_proof(
      Account#account.pubkey, serialize(Account), RootHash, Proof).

-spec serialize(account()) -> deterministic_account_binary_with_pubkey().
serialize(Account) ->
    term_to_binary(Account).

deserialize(SerializedAccount) ->
    binary_to_term(SerializedAccount).
