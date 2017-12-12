%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc In-memory Merkle trees.
%%%
%%% The root hash depends on the order of the operations performed on
%%% the tree.
%%%
%%% This module is a wrapper for `gb_merkle_trees`, implementing the
%%% following enhancements:
%%% * Stricter checks on arguments;
%%% * Some less ambiguous return values;
%%% * API compatible with the OTP `gb_trees` module;
%%% * It enables better type specifications in code using this module.
%%%
%%% @see gb_merkle_trees
%%% @see gb_trees
%%% @end
%%%-------------------------------------------------------------------
-module(aeu_mtrees).

%% API - subset of OTP `gb_trees` module
-export([empty/0,
         lookup/2,
         enter/3,
         to_list/1]).

%% API - Merkle tree
-export([root_hash/1,
         lookup_with_proof/2,
         verify_proof/4]).

-export_type([mtree/0,
              mtree/2,
              root_hash/0,
              proof/0]).

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

-type proof() :: gb_merkle_trees:merkle_proof().

%%%===================================================================
%%% API - subset of OTP `gb_trees` module
%%%===================================================================

-spec empty() -> mtree().
empty() ->
    gb_merkle_trees:empty().

lookup(Key, Tree) when ?IS_KEY(Key) ->
    case gb_merkle_trees:lookup(Key, Tree) of
        none ->
            none;
        Value when ?IS_VALUE(Value) ->
            {value, Value}
    end.

enter(Key, Value, Tree) when ?IS_KEY(Key), ?IS_VALUE(Value) ->
    gb_merkle_trees:enter(Key, Value, Tree).

-spec to_list(mtree()) -> [{key(), value()}].
to_list(Tree) ->
    gb_merkle_trees:to_orddict(Tree).

%%%===================================================================
%%% API - Merkle tree
%%%===================================================================

%% Return root hash of specified non-empty Merkle tree.
-spec root_hash(mtree()) -> {ok, root_hash()} | {error, empty}.
root_hash(Tree) ->
    case gb_merkle_trees:root_hash(Tree) of
        undefined ->
            {error, empty};
        Hash = <<_:?HASH_BYTES/unit:8>> ->
            {ok, Hash}
    end.

-spec lookup_with_proof(key(), mtree()) -> none |
                                           {value_and_proof, value(), proof()}.
lookup_with_proof(Key, Tree) when ?IS_KEY(Key) ->
    case lookup(Key, Tree) of
        none ->
            none;
        {value, Value} ->
            Proof = gb_merkle_trees:merkle_proof(Key, Tree),
            {value_and_proof, Value, Proof}
    end.

-spec verify_proof(key(), value(), root_hash(), proof()) -> {ok, verified} |
                                                            {error, term()}.
verify_proof(Key, Value, RootHash, Proof) ->
    gb_merkle_trees:verify_merkle_proof(Key, Value, RootHash, Proof).

%%%===================================================================
%%% Internal functions
%%%===================================================================
