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
         read_only_subtree/2,
         to_list/1]).

%% API - utils outside OTP `gb_trees` module
-export([fold/3]).

%% API - Merkle tree
-export([root_hash/1,
         lookup_with_proof/2,
         lookup_with_proof/3,
         verify_proof/4,
         lookup_proof/3,
         commit_to_db/1,
         new_with_backend/2,
         gc_old_nodes/1,
         empty_with_backend/1
        ]).

%% For internal functional db
-export([ proof_db_drop_cache/1
        , proof_db_get/2
        , proof_db_put/3
        ]).

-export([ serialize/1
        , deserialize/1
        , deserialize_with_backend/2]).

-export_type([iterator/0,
              iterator_opts/0,
              mtree/0,
              mtree/2,
              root_hash/0,
              proof/0]).


-define(HASH_BYTES, 32).
-define(IS_KEY(K), is_binary(K)).
-define(IS_VALUE(V), is_binary(V)).
-define(STATE_HASH_BYTES, 32). %% TODO NG move to proper hrl

-define(VSN, 1).

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

-spec new_with_backend(root_hash() | 'empty', aeu_mp_trees_db:db()) -> mtree().
new_with_backend(empty, DB) ->
    empty_with_backend(DB);
new_with_backend(<<_:256>> = Hash, DB) ->
    aeu_mp_trees:new(Hash, DB).

-spec gc_old_nodes(mtree()) -> mtree().
gc_old_nodes(Tree) ->
    %% TODO: this is a workarond and not a proper GC
    Foldl =
        fun F('$end_of_table', Accum) -> Accum;
            F({Key, Value, Iter}, Accum) ->
                F(aeu_mp_trees:iterator_next(Iter),
                  insert(Key, Value, Accum))
        end,
    Iterator = aeu_mp_trees:iterator(Tree),
    Foldl(aeu_mp_trees:iterator_next(Iterator), empty()).

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

%% NOTE: The key needs to have a value in the tree for this to succeed.
-spec read_only_subtree(key(), mtree()) -> {ok, mtree()} | {error, no_such_subtree}.
read_only_subtree(Key, Tree) when ?IS_KEY(Key) ->
    aeu_mp_trees:read_only_subtree(Key, Tree).

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
    map(fun(K, V) -> {K, V} end, Tree).

-spec map(fun((key(), value()) -> term()), mtree()) -> list(term()).
map(Fun, Tree) ->
    MapThroughValues =
        fun Map('$end_of_table', Accum) -> Accum;
            Map({Key, Value, Iter}, Accum) ->
                Map(aeu_mp_trees:iterator_next(Iter),
                    [Fun(Key, Value) | Accum])
        end,
    Iterator = aeu_mp_trees:iterator(Tree),
    MapThroughValues(aeu_mp_trees:iterator_next(Iterator), []).
%%%===================================================================
%%% API - utils outside OTP `gb_trees` module
%%%===================================================================

%% Similar to `dict:fold/3`.
-spec fold(Fun, Acc0, iterator()) -> Acc1 when
      Fun :: fun((key(), value(), AccIn) -> AccOut),
      Acc0 :: Acc,
      Acc1 :: Acc,
      AccIn :: Acc,
      AccOut :: Acc.
fold(Fun, Acc0, Iter) ->
    case iterator_next(Iter) of
        '$end_of_table' -> Acc0;
        {Key, Val, NextIter} ->
            fold(Fun, Fun(Key, Val, Acc0), NextIter)
    end.

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

%% Will use the built in dict proof db.
lookup_with_proof(Key, Tree) when ?IS_KEY(Key) ->
    lookup_with_proof(Key, Tree, new_proof_db()).

-spec lookup_with_proof(key(), mtree(), proof()) -> none |
                                                    {value_and_proof, value(), proof()}.
lookup_with_proof(Key, Tree, ProofDB) when ?IS_KEY(Key) ->
    case lookup(Key, Tree) of
        none ->
            none;
        {value, Value} ->
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

-spec lookup_proof(key(), root_hash(), proof()) -> {ok, value()} |
                                                   {error, term()}.
lookup_proof(Key, RootHash, Proof) ->
    case aeu_mp_trees:lookup_in_proof(Key, RootHash, Proof) of
        {value, Val} -> {ok, Val};
        none -> {error, not_found}
    end.

-spec commit_to_db(mtree()) -> mtree().
commit_to_db(Tree) ->
    aeu_mp_trees:commit_reachable_to_db(Tree).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Note that this is only the default proof db, if the caller did not
%% provide a proof db in the call.

new_proof_db() ->
    aeu_mp_trees_db:new(proof_db_spec()).

proof_db_spec() ->
    #{ handle => dict:new()
     , cache  => dict:new()
     , get    => {?MODULE, proof_db_get}
     , put    => {?MODULE, proof_db_put}
     , drop_cache => {?MODULE, proof_db_drop_cache}
     }.

proof_db_get(Key, Proof) ->
    {value, dict:fetch(Key, Proof)}.

proof_db_put(Key, Val, Proof) ->
    dict:store(Key, Val, Proof).

proof_db_drop_cache(_Cache) ->
    dict:new().

%%%===================================================================
%%% serialization
%%%===================================================================
-spec serialize(mtree()) -> binary().
serialize(Tree) ->
    ValuesBins =
        map(fun(Key, Value) ->
                aec_object_serialization:serialize(
                    mtree_value,
                    ?VSN,
                    value_serialization_template(?VSN),
                    [ {key, Key}
                    , {val, Value}
                    ])
            end,
            Tree),
    aec_object_serialization:serialize(
        mtree,
        ?VSN,
        serialization_template(?VSN),
        [{values, ValuesBins}]).

-spec deserialize(binary()) -> mtree().
deserialize(Bin) ->
    deserialize_(Bin, empty()).

-spec deserialize_with_backend(binary(), aeu_mp_trees_db:db()) -> mtree().
deserialize_with_backend(Bin, DB) ->
    deserialize_(Bin, empty_with_backend(DB)).

-spec deserialize_(binary(), mtree()) -> mtree().
deserialize_(Bin, EmptyMTree) ->
    [{values, ValuesBin}] =
        aec_object_serialization:deserialize(mtree, ?VSN,
                                             serialization_template(?VSN), Bin),
    lists:foldl(
        fun(ValBin, Accum) ->
            [ {key, Key}
            , {val, Value}] =
                aec_object_serialization:deserialize(mtree_value, ?VSN,
                                                     value_serialization_template(?VSN),
                                                     ValBin),
            insert(Key, Value, Accum)
        end,
        EmptyMTree,
        ValuesBin).

serialization_template(?VSN) ->
   [{values, [binary]}]. 

value_serialization_template(?VSN) ->
   [ {key, binary}
   , {val, binary}
   ]. 

