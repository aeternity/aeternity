-module(aec_trees).

-include("trees.hrl").

%% API
-export([accounts/1,
         with_accounts/2]).
-export([new/0,
         get/2,
         get_with_proof/2,
         put/3,
         root_hash/1,
         verify_proof/4]).

accounts(Trees) ->
    Trees#trees.accounts.

with_accounts(Trees, Accounts) ->
    Trees#trees{accounts = Accounts}.

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
