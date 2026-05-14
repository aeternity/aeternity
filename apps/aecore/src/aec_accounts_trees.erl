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
         to_list/1,
         new_with_backend/1,
         new_with_dirty_backend/1,
         gc_cache/1,
         enter/2]).

%% API - Merkle tree
-export([root_hash/1,
         db/1,
         commit_to_db/1,
         mtree_iterator/1
        ]).

-export([ from_binary_without_backend/1
        , to_binary_without_backend/1
        ]).

%% API - Proof of inclusion
-export([ add_poi/3
        , verify_poi/3
        , lookup_poi/2
        ]).

-export([delete/2]).

%% Backwards compatibility conversion
-export([from_db_format/1]).

%% API - misc
-export([ get_all_accounts_balances/1
        , lock_coins/2]).

%% Batch management
-export([ flush_account_batch/1 ]).

-export_type([tree/0]).

-type key() :: aec_keys:pubkey().
-type value() :: aec_accounts:deterministic_account_binary_with_pubkey().

%% Per-microblock deferred account writes.  Accumulates all `enter` calls for
%% the block; written to the MPT in one pass at flush time.
-record(accounts_tree, {
    accounts      :: aeu_mtrees:mtree(key(), value()),
    account_batch :: #{aec_keys:pubkey() => aec_accounts:account()}
}).

-opaque tree() :: #accounts_tree{}.

-define(VSN, 1).

%%%===================================================================
%%% API - similar to OTP `gb_trees` module
%%%===================================================================

-spec empty() -> tree().
empty() ->
    #accounts_tree{accounts = aeu_mtrees:empty(), account_batch = #{}}.

-spec from_db_format(tree()) -> tree().
from_db_format(Tree = #accounts_tree{accounts = AT}) ->
    Tree#accounts_tree{accounts = aeu_mtrees:from_db_format(AT)}.

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    #accounts_tree{accounts      = aeu_mtrees:empty_with_backend(aec_db_backends:accounts_backend()),
                   account_batch = #{}}.

-spec new_with_backend(aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_backend(Hash) ->
    #accounts_tree{accounts      = aeu_mtrees:new_with_backend(Hash, aec_db_backends:accounts_backend()),
                   account_batch = #{}}.

-spec new_with_dirty_backend(aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_dirty_backend(Hash) ->
    #accounts_tree{accounts      = aeu_mtrees:new_with_backend(Hash, aec_db_backends:dirty_accounts_backend()),
                   account_batch = #{}}.

-spec gc_cache(tree()) -> tree().
gc_cache(Tree) ->
    #accounts_tree{accounts = AT} = flush_account_batch(Tree),
    Tree#accounts_tree{accounts = aeu_mtrees:gc_cache(AT), account_batch = #{}}.

-spec to_list(tree()) -> [{binary(), binary()}].
to_list(Tree) ->
    #accounts_tree{accounts = AT} = flush_account_batch(Tree),
    aeu_mtrees:to_list(AT).

-spec get(aec_keys:pubkey(), tree()) -> aec_accounts:account().
get(Pubkey, Tree) ->
    case lookup(Pubkey, Tree) of
        {value, Account} ->
            Pubkey = aec_accounts:pubkey(Account), %% Hardcoded expectation.
            Account;
        none ->
            error({not_present, Pubkey})
    end.

-spec lookup(aec_keys:pubkey(), tree()) -> none | {value, aec_accounts:account()}.
lookup(Pubkey, #accounts_tree{accounts = AT, account_batch = Batch}) ->
    case maps:find(Pubkey, Batch) of
        {ok, Account} ->
            {value, Account};
        error ->
            case aeu_mtrees:lookup(Pubkey, AT) of
                none ->
                    none;
                {value, SerializedAccount} ->
                    Account = aec_accounts:deserialize(Pubkey, SerializedAccount),
                    Pubkey  = aec_accounts:pubkey(Account), %% Hardcoded expectation.
                    {value, Account}
            end
    end.

%% Defer the MPT write; accumulate in the microblock-level batch.
-spec enter(aec_accounts:account(), tree()) -> tree().
enter(Account, #accounts_tree{account_batch = Batch} = Tree) ->
    Pubkey = aec_accounts:pubkey(Account),
    Tree#accounts_tree{account_batch = Batch#{Pubkey => Account}}.

-spec delete(aec_keys:pubkey(), tree()) -> tree().
delete(Pubkey, #accounts_tree{accounts = AT, account_batch = Batch} = Tree) ->
    AT1 = aeu_mtrees:delete(Pubkey, AT),
    Tree#accounts_tree{accounts = AT1, account_batch = maps:remove(Pubkey, Batch)}.

-spec to_binary_without_backend(tree()) -> binary().
to_binary_without_backend(Tree) ->
    #accounts_tree{accounts = AT} = flush_account_batch(Tree),
    Bin = aeu_mtrees:serialize(AT),
    aeser_chain_objects:serialize(
        accounts_mtree,
        ?VSN,
        serialization_template(?VSN),
        [{accounts, Bin}]).

-spec from_binary_without_backend(binary()) -> tree().
from_binary_without_backend(Bin) ->
    [{accounts, AccountsBin}] =
        aeser_chain_objects:deserialize(accounts_mtree, ?VSN,
                                             serialization_template(?VSN), Bin),
    #accounts_tree{accounts = aeu_mtrees:deserialize(AccountsBin), account_batch = #{}}.

serialization_template(?VSN) ->
    [{accounts, binary}].

-spec mtree_iterator(tree()) -> aeu_mtrees:iterator().
mtree_iterator(Tree) ->
    #accounts_tree{accounts = AT} = flush_account_batch(Tree),
    aeu_mtrees:iterator(AT).

%%%===================================================================
%%% API - Merkle tree
%%%===================================================================

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(Tree) ->
    #accounts_tree{accounts = AT} = flush_account_batch(Tree),
    aeu_mtrees:root_hash(AT).

-spec db(tree()) -> {ok, aeu_mtrees:db()}.
db(#accounts_tree{accounts = AT}) ->
    aeu_mtrees:db(AT).

-spec add_poi(aec_keys:pubkey(), tree(), aec_poi:poi()) ->
                     {'ok', aec_poi:poi()}
                         | {'error', 'not_present' | 'wrong_root_hash'}.
add_poi(Pubkey, Tree, Poi) ->
    %% Flush this account's pending batch entry before building the proof so the
    %% MPT reflects the latest state.  Mirrors flush_contract_batch in aect_state_tree.
    Tree1 = flush_account_for(Pubkey, Tree),
    #accounts_tree{accounts = AT} = Tree1,
    aec_poi:add_poi(Pubkey, AT, Poi).

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
    #accounts_tree{accounts = AT} = FT = flush_account_batch(Tree),
    FT#accounts_tree{accounts = aeu_mtrees:commit_to_db(AT)}.

%%%===================================================================
%%% API - Batch management
%%%===================================================================

%% Flush all pending account writes to the MPT.  Called at microblock end via
%% aec_trees:flush_contract_store_batch/1.  O(1) fast-path when batch is empty.
-spec flush_account_batch(tree()) -> tree().
flush_account_batch(#accounts_tree{account_batch = Batch} = Tree)
  when map_size(Batch) =:= 0 ->
    Tree;
flush_account_batch(#accounts_tree{accounts = AT, account_batch = Batch} = Tree) ->
    AT1 = maps:fold(fun(_Pubkey, Account, Acc) ->
                        aeu_mtrees:enter(key(Account), value(Account), Acc)
                    end, AT, Batch),
    Tree#accounts_tree{accounts = AT1, account_batch = #{}}.

%%%===================================================================
%%% API - misc
%%%===================================================================

-spec get_all_accounts_balances(tree()) -> [{aec_keys:pubkey(), non_neg_integer()}].
get_all_accounts_balances(AccountsTree) ->
    AccountsDump = to_list(AccountsTree),  %% to_list/1 flushes the batch first.
    lists:foldl(
      fun({Pubkey, SerializedAccount}, Acc) ->
              Account = aec_accounts:deserialize(Pubkey, SerializedAccount),
              [{Pubkey, aec_accounts:balance(Account)} | Acc]
      end, [], AccountsDump).

-spec lock_coins(integer(), tree()) -> tree().
lock_coins(0     , AccountsTree) -> AccountsTree;
lock_coins(Amount, AccountsTree) ->
    HolderPubKey = aec_governance:locked_coins_holder_account(),
    HolderAccount0 = case lookup(HolderPubKey, AccountsTree) of
                        none             -> aec_accounts:new(HolderPubKey, 0);
                        {value, Account} -> Account
                    end,
    {ok, HolderAccount} =
        case Amount > 0 of
            true -> aec_accounts:earn(HolderAccount0, Amount);
            false ->
                AbsAmount = abs(Amount),
                true = 0 =< aec_accounts:balance(HolderAccount0) - AbsAmount,
                Nonce = aec_accounts:nonce(HolderAccount0),
                aec_accounts:spend(HolderAccount0, AbsAmount, Nonce)
        end,
    enter(HolderAccount, AccountsTree).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Flush a single account's batch entry to the MPT (used before PoI construction).
%% Analogous to aect_state_tree:flush_contract_batch/2 for the contract store batch.
flush_account_for(Pubkey, #accounts_tree{accounts = AT, account_batch = Batch} = Tree) ->
    case maps:take(Pubkey, Batch) of
        {Account, Batch1} ->
            AT1 = aeu_mtrees:enter(key(Account), value(Account), AT),
            Tree#accounts_tree{accounts = AT1, account_batch = Batch1};
        error ->
            Tree
    end.

key(A) ->
    aec_accounts:pubkey(A).

value(A) ->
    aec_accounts:serialize(A).
