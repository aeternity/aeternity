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
         commit_to_db/1
        ]).

%% API - Proof of inclusion
-export([ add_poi/3
        , verify_poi/3
        ]).

%% API - misc
-export([get_all_accounts_balances/1]).

-export_type([tree/0]).

-type key() :: aec_keys:pubkey().
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

-spec get(aec_keys:pubkey(), tree()) -> aec_accounts:account().
get(Pubkey, Tree) ->
    Account = aec_accounts:deserialize(Pubkey, aeu_mtrees:get(Pubkey, Tree)),
    Pubkey  = aec_accounts:pubkey(Account), %% Hardcoded expectation.
    Account.

-spec lookup(aec_keys:pubkey(), tree()) -> none | {value, aec_accounts:account()}.
lookup(Pubkey, Tree) ->
    case aeu_mtrees:lookup(Pubkey, Tree) of
        none ->
            none;
        {value, SerializedAccount} ->
            Account = aec_accounts:deserialize(Pubkey, SerializedAccount),
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

-spec add_poi(aec_keys:pubkey(), tree(), aec_poi:poi()) ->
                     {'ok', binary(), aec_poi:poi()}
                         | {'error', 'not_present' | 'wrong_root_hash'}.
add_poi(Pubkey, Tree, Poi) ->
    aec_poi:add_poi(Pubkey, Tree, Poi).

-spec verify_poi(aec_keys:pubkey(), binary(), aec_poi:poi()) ->
                        'ok' | {'error', term()}.
verify_poi(AccountKey, SerializedAccount, Poi) ->
    aec_poi:verify(AccountKey, SerializedAccount, Poi).

-spec commit_to_db(tree()) -> tree().
commit_to_db(Tree) ->
    aeu_mtrees:commit_to_db(Tree).

%%%===================================================================
%%% API - misc
%%%===================================================================

-spec get_all_accounts_balances(tree()) -> [{aec_keys:pubkey(), non_neg_integer()}].
get_all_accounts_balances(AccountsTree) ->
    AccountsDump = aeu_mtrees:to_list(AccountsTree),
    lists:foldl(
      fun({Pubkey, SerializedAccount}, Acc) ->
              Account = aec_accounts:deserialize(Pubkey, SerializedAccount),
              [{Pubkey, aec_accounts:balance(Account)} | Acc]
      end, [], AccountsDump).

%%%===================================================================
%%% Internal functions
%%%===================================================================

key(A) ->
    aec_accounts:pubkey(A).

value(A) ->
    aec_accounts:serialize(A).
