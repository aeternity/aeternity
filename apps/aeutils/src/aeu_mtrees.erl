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
         db/1,
         lookup_with_proof/2,
         lookup_with_proof/3,
         verify_proof/4,
         lookup_proof/3,
         commit_to_db/1,
         new_with_backend/2,
         proxy_tree/2,
         gc_cache/1,
         empty_with_backend/1
        ]).

-include("aeu_proxy.hrl").

%% For internal functional db
-behavior(aeu_mp_trees_db).
-export([ mpt_db_drop_cache/1
        , mpt_db_list_cache/1
        , mpt_db_get/2
        , mpt_db_put/3
        ]).

-export([ serialize/1
        , deserialize/1
        , deserialize_with_backend/2]).

-export_type([iterator/0,
              iterator_opts/0,
              mtree/0,
              mtree/2,
              root_hash/0,
              db/0,
              proof/0]).

-export([ pp_term/1 ]).


-define(HASH_BYTES, 32).
-define(IS_KEY(K), is_binary(K)).
-define(IS_VALUE(V), is_binary(V)).
-define(STATE_HASH_BYTES, 32). %% TODO NG move to proper hrl

-define(VSN, 1).

-type key() :: binary().
-type value() :: binary().
-type mtree() :: mtree(key(), value()).

-opaque iterator() :: aeu_mp_trees:iterator() | #proxy_mp_tree_iter{}.
-type iterator_opts() :: aeu_mp_trees:iterator_opts().

%% Enable specification of types of key and value for enabling code
%% using this module to document types for readability.
%% Both key and value must be binaries.
-type mtree(_K, _V) :: aeu_mp_trees:tree() | #proxy_mp_tree{}.

%% 256 bits as of ?HASH_BYTES * 8
-type root_hash() :: <<_:256>>.
-type db() :: aeu_mp_trees:db().

-type proof() :: aeu_mp_trees_db:db().

pp_term(Term) ->
    aeu_mp_trees:pp_term(Term).

%%%===================================================================
%%% API - subset of OTP `gb_trees` module
%%%===================================================================

-spec empty() -> mtree().
empty() ->
    aeu_mp_trees:new().

-spec empty_with_backend(aeu_mp_trees_db:db()) -> mtree().
empty_with_backend(DB) ->
    aeu_mp_trees:new(DB).

-spec new_with_backend( root_hash()
                      | 'empty'
                      | {proxy, root_hash()}, aeu_mp_trees_db:db()) -> mtree().
new_with_backend(empty, DB) ->
    empty_with_backend(DB);
new_with_backend({proxy, _} = Proxy, DB) ->
    aeu_mp_trees:new(Proxy, DB);
new_with_backend(<<_:256>> = Hash, DB) ->
    aeu_mp_trees:new(Hash, DB).

proxy_tree(Mod, Arg) ->
    Mod:proxy_init(Arg).

-spec gc_cache(mtree()) -> mtree().
?PROXY_GET(gc_cache, Mod, P);
gc_cache(Tree) ->
    aeu_mp_trees:gc_cache(Tree).

?PROXY_PUT(delete, Key, Mod, P);
delete(Key, Tree) when ?IS_KEY(Key) ->
    aeu_mp_trees:delete(Key, Tree).

?PROXY_GET(get, Key, Mod, P);
get(Key, Tree) when ?IS_KEY(Key) ->
    case aeu_mp_trees:get(Key, Tree) of
        <<>> -> error({not_present, Key});
        Val -> Val
    end.

?PROXY_GET(lookup, Key, Mod, P);
lookup(Key, Tree) when ?IS_KEY(Key) ->
    case aeu_mp_trees:get(Key, Tree) of
        <<>> ->
            none;
        Value when ?IS_VALUE(Value) ->
            {value, Value}
    end.

?PROXY_PUT(enter, Key, Value, Mod, P); 
enter(Key, Value, Tree) when ?IS_KEY(Key), ?IS_VALUE(Value) ->
    aeu_mp_trees:put(Key, Value, Tree).

?PROXY_PUT(insert, Key, Value, Mod, P);
insert(Key, Value, Tree) when ?IS_KEY(Key), ?IS_VALUE(Value) ->
    case lookup(Key, Tree) of
        none -> enter(Key, Value, Tree);
        {value, _} -> error({already_present, Key})
    end.

%% NOTE: The key needs to have a value in the tree for this to succeed.
-spec read_only_subtree(key(), mtree()) -> {ok, mtree()} | {error, no_such_subtree}.
?PROXY_GET(read_only_subtree, Key, Mod, P);
read_only_subtree(Key, Tree) when ?IS_KEY(Key) ->
    aeu_mp_trees:read_only_subtree(Key, Tree).

-spec iterator(mtree()) -> iterator().
?PROXY_GET(iterator, Mod, P);
iterator(Tree) ->
    aeu_mp_trees:iterator(Tree).

-spec iterator(mtree(), iterator_opts()) -> iterator().
?PROXY_GET(iterator, Opts, Mod, P);
iterator(Tree, Opts) ->
    aeu_mp_trees:iterator(Tree, Opts).

-spec iterator_from(key(), mtree()) -> iterator().
?PROXY_GET(iterator_from, Key, Mod, P);
iterator_from(Key, Tree) ->
    aeu_mp_trees:iterator_from(Key, Tree).

-spec iterator_from(key(), mtree(), iterator_opts()) -> iterator().
?PROXY_GET(iterator_from, Key, Opts, Mod, P);
iterator_from(Key, Tree, Opts) ->
    aeu_mp_trees:iterator_from(Key, Tree, Opts).

-spec iterator_next(iterator()) ->
                           {key(), value(), iterator()} | '$end_of_table'.
?PROXY_ITER(iterator_next, Mod, I);
iterator_next(Iter) ->
    aeu_mp_trees:iterator_next(Iter).

-spec to_list(mtree()) -> [{key(), value()}].
?PROXY_GET(to_list, Mod, P);
to_list(Tree) ->
    map(fun(K, V) -> {K, V} end, Tree).

-spec map(fun((key(), value()) -> term()), mtree()) -> list(term()).
?PROXY_GET(map, Fun, Mod, P);
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
?PROXY_ITER(fold, Fun, Acc0, Mod, I);
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
?PROXY_GET(root_hash, Mod, P);
root_hash(Tree) ->
    case aeu_mp_trees:root_hash(Tree) of
        <<>> ->
            {error, empty};
        Hash = <<_:?HASH_BYTES/unit:8>> ->
            {ok, Hash}
    end.

-spec db(mtree()) -> {ok, db()}.
db(Tree) ->
    {ok, aeu_mp_trees:db(Tree)}.

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
     , module => ?MODULE
     }.

mpt_db_get(Key, Proof) ->
    {value, dict:fetch(Key, Proof)}.

mpt_db_put(Key, Val, Proof) ->
    dict:store(Key, Val, Proof).

mpt_db_drop_cache(_Cache) ->
    dict:new().

mpt_db_list_cache(Cache) ->
    dict:to_list(Cache).

%%%===================================================================
%%% serialization
%%%===================================================================
-spec serialize(mtree()) -> binary().
serialize(Tree) ->
    ValuesBins =
        map(fun(Key, Value) ->
                aeser_chain_objects:serialize(
                    mtree_value,
                    ?VSN,
                    value_serialization_template(?VSN),
                    [ {key, Key}
                    , {val, Value}
                    ])
            end,
            Tree),
    aeser_chain_objects:serialize(
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
        aeser_chain_objects:deserialize(mtree, ?VSN,
                                             serialization_template(?VSN), Bin),
    lists:foldl(
        fun(ValBin, Accum) ->
            [ {key, Key}
            , {val, Value}] =
                aeser_chain_objects:deserialize(mtree_value, ?VSN,
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
