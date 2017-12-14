%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Merkle trees of accounts.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_accounts_trees).

%% API - similar to OTP `gb_trees` module
-export([empty/0,
         lookup/2,
         enter/2]).

%% API - Merkle tree
-export([root_hash/1,
         lookup_with_proof/2,
         verify_proof/3]).

%% API - misc
-export([get_all_accounts_balances/1]).

-export_type([tree/0]).

-include("common.hrl").
-include("trees.hrl").

-type key() :: pubkey().
-type value() :: aec_accounts:deterministic_account_binary_with_pubkey().
-opaque tree() :: aeu_mtrees:mtree(key(), value()).

%%%===================================================================
%%% API - similar to OTP `gb_trees` module
%%%===================================================================

-spec empty() -> tree().
empty() ->
    aeu_mtrees:empty().

-spec lookup(pubkey(), tree()) -> none | {value, account()}.
lookup(Pubkey, Tree) ->
    case aeu_mtrees:lookup(Pubkey, Tree) of
        none ->
            none;
        {value, SerializedAccount} ->
            Account =
                #account{pubkey = Pubkey} = %% Hardcoded expectation.
                aec_accounts:deserialize(SerializedAccount),
            {value, Account}
    end.

-spec enter(account(), tree()) -> tree().
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
                               {value_and_proof, account(), aeu_mtrees:proof()}.
lookup_with_proof(Pubkey, Tree) ->
    case aeu_mtrees:lookup_with_proof(Pubkey, Tree) of
        none ->
            none;
        {value_and_proof, SerializedAccount, Proof} ->
            Account =
                #account{pubkey = Pubkey} = %% Hardcoded expectation.
                aec_accounts:deserialize(SerializedAccount),
            {value_and_proof, Account, Proof}
    end.

-spec verify_proof(account(), aeu_mtrees:root_hash(), aeu_mtrees:proof()) ->
                          {ok, verified} | {error, term()}.
verify_proof(Account, RootHash, Proof) ->
    aeu_mtrees:verify_proof(key(Account), value(Account), RootHash, Proof).

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
