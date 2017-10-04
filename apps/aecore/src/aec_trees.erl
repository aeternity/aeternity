-module(aec_trees).

-include("common.hrl").
-include("trees.hrl").

%% API

-export([all_trees_new/0,
         all_trees_hash/1,
         accounts/1,
         set_accounts/2,
         new/0,
         get/2,
         get_with_proof/2,
         put/3,
         root_hash/1,
         verify_proof/4]).

-type tree() :: gb_merkle_trees:tree().

-spec all_trees_new() -> {ok, trees()}.
all_trees_new() ->
    {ok, A} = new(),
    {ok, #trees{accounts = A}}.

all_trees_hash(Trees) ->
    %% TODO Consider all state trees - not only accounts.
    {ok, H} = aec_accounts:root_hash(accounts(Trees)),
    H.

-spec accounts(trees()) -> tree().
accounts(Trees) ->
    Trees#trees.accounts.

-spec set_accounts(trees(), tree()) -> trees().
set_accounts(Trees, Accounts) ->
    Trees#trees{accounts = Accounts}.

-spec new() -> {ok, tree()}.
new() ->
    {ok, gb_merkle_trees:empty()}.

get(Key, Tree) when is_binary(Key) ->
    case gb_merkle_trees:lookup(Key, Tree) of
        none ->
            {error, notfound};
        Value when is_binary(Value) ->
            {ok, Value}
    end.

get_with_proof(Key, Tree) when is_binary(Key) ->
    case get(Key, Tree) of
        {ok, Value} when is_binary(Value) ->
            Proof = gb_merkle_trees:merkle_proof(Key, Tree),
            {ok, {Value, Proof}};
        {error, notfound} = E ->
            E
    end.

put(Key, Value, Tree) when is_binary(Key), is_binary(Value) ->
    NewTree = gb_merkle_trees:enter(Key, Value, Tree),
    {ok, NewTree}.

root_hash(Tree) ->
    case gb_merkle_trees:root_hash(Tree) of
        undefined ->
            {error, empty};
        Hash when is_binary(Hash) ->
            {ok, Hash}
    end.

verify_proof(Key, Value, RootHash, Proof) ->
    gb_merkle_trees:verify_merkle_proof(Key, Value, RootHash, Proof).
