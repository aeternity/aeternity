%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc In-memory Merkle Patricia trees.
%%%
%%% This module is a wrapper for 'aeu_mp_trees', implementing the
%%% following enhancements:
%%% * API compatible with the OTP 'gb_trees' module;
%%% * It enables better type specifications in code using this module.
%%%
%%% @see gb_trees
%%% @end
%%%-------------------------------------------------------------------
-module(aeu_mtrees).

%% API - subset of OTP `gb_trees` module
-export([empty/0,
         delete/2,
         get/2,
         insert/3,
         iterator/1,
         iterator/2,
         iterator_from/2,
         iterator_from/3,
         iterator_next/1,
         lookup/2,
         enter/3,
         to_list/1]).

%% API - Merkle tree
-export([root_hash/1,
         lookup_with_proof/2,
         verify_proof/4,
         commit_to_db/1,
         empty_with_backend/1
        ]).

%% For internal functional db
-export([ proof_db_commit/2
        , proof_db_get/2
        , proof_db_put/3
        ]).

-export_type([iterator/0,
              iterator_opts/0,
              mtree/0,
              mtree/2,
              root_hash/0,
              proof/0]).

-define(HASH_BYTES, 32).
-define(IS_KEY(K), is_binary(K)).
-define(IS_VALUE(V), is_binary(V)).

-type key() :: binary().
-type value() :: binary().
-type mtree() :: mtree(key(), value()).

-opaque iterator() :: aeu_mp_trees:iterator().
-type iterator_opts() :: aeu_mp_trees:iterator_opts().

%% Enable specification of types of key and value for enabling code
%% using this module to document types for readability.
%% Both key and value must be binaries.
-type mtree(_K, _V) :: aeu_mp_trees:tree().

%% 256 bits as of ?HASH_BYTES * 8
-type root_hash() :: <<_:256>>.

-type proof() :: aeu_mp_trees_db:db().

%%%===================================================================
%%% API - subset of OTP `gb_trees` module
%%%===================================================================

-spec empty() -> mtree().
empty() ->
    aeu_mp_trees:new().

-spec empty_with_backend(aeu_mp_trees_db:db()) -> mtree().
empty_with_backend(DB) ->
    aeu_mp_trees:new(DB).

delete(Key, Tree) when ?IS_KEY(Key) ->
    aeu_mp_trees:delete(Key, Tree).

get(Key, Tree) when ?IS_KEY(Key) ->
    case aeu_mp_trees:get(Key, Tree) of
        <<>> -> error({not_present, Key});
        Val -> Val
    end.

lookup(Key, Tree) when ?IS_KEY(Key) ->
    case aeu_mp_trees:get(Key, Tree) of
        <<>> ->
            none;
        Value when ?IS_VALUE(Value) ->
            {value, Value}
    end.

enter(Key, Value, Tree) when ?IS_KEY(Key), ?IS_VALUE(Value) ->
    aeu_mp_trees:put(Key, Value, Tree).

insert(Key, Value, Tree) when ?IS_KEY(Key), ?IS_VALUE(Value) ->
    case lookup(Key, Tree) of
        none -> aeu_mp_trees:put(Key, Value, Tree);
        {value, _} -> error({already_present, Key})
    end.

-spec iterator(mtree()) -> iterator().
iterator(Tree) ->
    aeu_mp_trees:iterator(Tree).

-spec iterator(mtree(), iterator_opts()) -> iterator().
iterator(Tree, Opts) ->
    aeu_mp_trees:iterator(Tree, Opts).

-spec iterator_from(key(), mtree()) -> iterator().
iterator_from(Key, Tree) ->
    aeu_mp_trees:iterator_from(Key, Tree).

-spec iterator_from(key(), mtree(), iterator_opts()) -> iterator().
iterator_from(Key, Tree, Opts) ->
    aeu_mp_trees:iterator_from(Key, Tree, Opts).

-spec iterator_next(iterator()) ->
                           {key(), value(), iterator()} | '$end_of_table'.
iterator_next(Iter) ->
    aeu_mp_trees:iterator_next(Iter).

-spec to_list(mtree()) -> [{key(), value()}].
to_list(Tree) ->
    Iterator = aeu_mp_trees:iterator(Tree),
    to_list(aeu_mp_trees:iterator_next(Iterator), []).

to_list('$end_of_table', Acc) -> Acc;
to_list({Key, Val, Iter}, Acc) ->
    to_list(aeu_mp_trees:iterator_next(Iter), [{Key, Val}|Acc]).

%%%===================================================================
%%% API - Merkle tree
%%%===================================================================

%% Return root hash of specified non-empty Merkle tree.
-spec root_hash(mtree()) -> {ok, root_hash()} | {error, empty}.
root_hash(Tree) ->
    case aeu_mp_trees:root_hash(Tree) of
        <<>> ->
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
            ProofDB = new_proof_db(),
            {Value, Proof} = aeu_mp_trees:construct_proof(Key, ProofDB, Tree),
            {value_and_proof, Value, Proof}
    end.

-spec verify_proof(key(), value(), root_hash(), proof()) -> {ok, verified} |
                                                            {error, term()}.
verify_proof(Key, Value, RootHash, Proof) ->
    case aeu_mp_trees:verify_proof(Key, Value, RootHash, Proof) of
        ok -> {ok, verified};
        Other -> {error, Other}
    end.

-spec commit_to_db(mtree()) -> mtree().
commit_to_db(Tree) ->
    case aeu_mp_trees:commit_to_db(Tree) of
        {ok, Tree1}   -> Tree1;
        {error, What} -> error({failed_commit, What})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

new_proof_db() ->
    aeu_mp_trees_db:new(proof_db_spec()).

proof_db_spec() ->
    #{ handle => dict:new()
     , cache  => dict:new()
     , get    => {?MODULE, proof_db_get}
     , put    => {?MODULE, proof_db_put}
     , commit => {?MODULE, proof_db_commit}
     }.

proof_db_get(Key, Proof) ->
    {value, dict:fetch(Key, Proof)}.

proof_db_put(Key, Val, Proof) ->
    dict:store(Key, Val, Proof).

proof_db_commit(_Cache,_DB) ->
    error(no_commits_in_proof).
