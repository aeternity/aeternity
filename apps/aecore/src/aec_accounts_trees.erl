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
         new_with_backend/1,
         enter/2]).

%% API - Merkle tree
-export([root_hash/1,
         commit_to_db/1
        ]).

%% API - Proof of inclusion
-export([ add_poi/3
        , verify_poi/3
        , lookup_poi/2
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

-spec new_with_backend(aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_backend(Hash) ->
    aeu_mtrees:new_with_backend(Hash, aec_db_backends:accounts_backend()).

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
                     {'ok', aec_poi:poi()}
                         | {'error', 'not_present' | 'wrong_root_hash'}.
add_poi(Pubkey, Tree, Poi) ->
    aec_poi:add_poi(Pubkey, Tree, Poi).

-spec verify_poi(aec_keys:pubkey(), aec_accounts:account(), aec_poi:poi()) ->
                        'ok' | {'error', term()}.
verify_poi(AccountKey, Account, Poi) ->
    %% Hardcode expectation on specified account object key being
    %% equal to key in internal representation of account.  The key is
    %% not part of the account serialization so this shall never
    %% happen.
    AccountKey = aec_accounts:pubkey(Account),
    aec_poi:verify(AccountKey, aec_accounts:serialize(Account), Poi).

-spec lookup_poi(aec_keys:pubkey(), aec_poi:poi()) ->
                        {'ok', aec_accounts:account()} | {'error', not_found}.
lookup_poi(AccountKey, Poi) ->
    case aec_poi:lookup(AccountKey, Poi) of
        {ok, SerializedAccount} -> {ok, aec_accounts:deserialize(AccountKey, SerializedAccount)};
        Err -> Err
    end.

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
