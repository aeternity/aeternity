%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc In-memory Merkle trees.
%%%
%%% The root hash depends on the order of the operations performed on
%%% the tree.
%%%
%%% This implementation is a wrapper for `gb_merkle_trees` with
%%% stricter checks on arguments, less ambiguous return values and
%%% enabling better type specifications.
%%%
%%% @see gb_merkle_trees
%%% @end
%%%-------------------------------------------------------------------
-module(aeu_mtrees).

%% API
-export([new/0,
         get/2,
         get_with_proof/2,
         put/3,
         root_hash/1,
         verify_proof/4,
         to_orddict/1]).

-export_type([mtree/0,
              mtree/2,
              root_hash/0]).

-define(HASH_BYTES, 32).
-define(IS_KEY(K), is_binary(K)).
-define(IS_VALUE(V), is_binary(V)).

-type key() :: binary().
-type value() :: binary().
-type mtree() :: mtree(key(), value()).

%% Enable specification of types of key and value for enabling code
%% using this module to document types for readability.
%% Both key and value must be binaries.
-type mtree(_K, _V) :: gb_merkle_trees:tree().

-type root_hash() :: <<_:(?HASH_BYTES*8)>>.

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> mtree().
new() ->
    {ok, gb_merkle_trees:empty()}.

get(Key, Tree) when ?IS_KEY(Key) ->
    case gb_merkle_trees:lookup(Key, Tree) of
        none ->
            {error, notfound};
        Value when is_binary(Value) ->
            {ok, Value}
    end.

get_with_proof(Key, Tree) when ?IS_KEY(Key) ->
    case get(Key, Tree) of
        {ok, Value} when ?IS_VALUE(Value) ->
            Proof = gb_merkle_trees:merkle_proof(Key, Tree),
            {ok, {Value, Proof}};
        {error, notfound} = E ->
            E
    end.

put(Key, Value, Tree) when ?IS_KEY(Key), ?IS_VALUE(Value) ->
    NewTree = gb_merkle_trees:enter(Key, Value, Tree),
    {ok, NewTree}.

%% Return root hash of specified non-empty Merkle tree.
-spec root_hash(mtree()) -> {ok, root_hash()} | {error, empty}.
root_hash(Tree) ->
    case gb_merkle_trees:root_hash(Tree) of
        undefined ->
            {error, empty};
        Hash = <<_:?HASH_BYTES/unit:8>> ->
            {ok, Hash}
    end.

verify_proof(Key, Value, RootHash, Proof) ->
    gb_merkle_trees:verify_merkle_proof(Key, Value, RootHash, Proof).

-spec to_orddict(mtree()) -> [{key(), value()}].
to_orddict(Tree) ->
    gb_merkle_trees:to_orddict(Tree).

%%%===================================================================
%%% Internal functions
%%%===================================================================
