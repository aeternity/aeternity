%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Merkle trees of accounts.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_accounts_trees).

%% API - similar to OTP `gb_trees` module
-export([empty/0,
         empty_with_backend/0,
         get/2,
         lookup/2,
         enter/2]).

%% API - Merkle tree
-export([root_hash/1,
         lookup_with_proof/2,
         verify_proof/3,
         commit_to_db/1
        ]).

%% API - misc
-export([get_all_accounts_balances/1]).

-export_type([tree/0]).

-include("common.hrl").

-type key() :: pubkey().
-type value() :: aec_accounts:deterministic_account_binary_with_pubkey().
-opaque tree() :: aeu_mtrees:mtree(key(), value()).

%%%===================================================================
%%% API - similar to OTP `gb_trees` module
%%%===================================================================
-spec empty() -> tree().
empty() ->
    aeu_mtrees:empty().

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    aeu_mtrees:empty_with_backend(aec_db_backends:accounts_backend()).

-spec get(pubkey(), tree()) -> aec_accounts:account().
get(Pubkey, Tree) ->
    Account = aec_accounts:deserialize(aeu_mtrees:get(Pubkey, Tree)),
    Pubkey  = aec_accounts:pubkey(Account), %% Hardcoded expectation.
    Account.

-spec lookup(pubkey(), tree()) -> none | {value, aec_accounts:account()}.
lookup(Pubkey, Tree) ->
    case aeu_mtrees:lookup(Pubkey, Tree) of
        none ->
            none;
        {value, SerializedAccount} ->
            Account = aec_accounts:deserialize(SerializedAccount),
            Pubkey  = aec_accounts:pubkey(Account), %% Hardcoded expectation.
            {value, Account}
    end.

-spec enter(aec_accounts:account(), tree()) -> tree().
enter(Account, Tree) ->
    aeu_mtrees:enter(key(Account), value(Account), Tree).

%%%===================================================================
%%% API - Merkle tree
%%%===================================================================

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(Tree) ->
    aeu_mtrees:root_hash(Tree).

-spec lookup_with_proof(pubkey(), tree()) ->
                               none |
                               {value_and_proof, aec_accounts:account(), aeu_mtrees:proof()}.
lookup_with_proof(Pubkey, Tree) ->
    case aeu_mtrees:lookup_with_proof(Pubkey, Tree) of
        none ->
            none;
        {value_and_proof, SerializedAccount, Proof} ->
            Account = aec_accounts:deserialize(SerializedAccount),
            Pubkey  = aec_accounts:pubkey(Account), %% Hardcoded expectation.
            {value_and_proof, Account, Proof}
    end.

-spec verify_proof(aec_accounts:account(), aeu_mtrees:root_hash(), aeu_mtrees:proof()) ->
                          {ok, verified} | {error, term()}.
verify_proof(Account, RootHash, Proof) ->
    aeu_mtrees:verify_proof(key(Account), value(Account), RootHash, Proof).

-spec commit_to_db(tree()) -> tree().
commit_to_db(Tree) ->
    aeu_mtrees:commit_to_db(Tree).

%%%===================================================================
%%% API - misc
%%%===================================================================

-spec get_all_accounts_balances(tree()) -> [{pubkey(), non_neg_integer()}].
get_all_accounts_balances(AccountsTree) ->
    AccountsDump = aeu_mtrees:to_list(AccountsTree),
    lists:foldl(
      fun({Pubkey, SerializedAccount}, Acc) ->
              Account = aec_accounts:deserialize(SerializedAccount),
              [{Pubkey, aec_accounts:balance(Account)} | Acc]
      end, [], AccountsDump).

%%%===================================================================
%%% Internal functions
%%%===================================================================

key(A) ->
    aec_accounts:pubkey(A).

value(A) ->
    aec_accounts:serialize(A).
