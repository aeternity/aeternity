%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of Naming System
%%% @end
%%%-------------------------------------------------------------------

-module(aens_state_tree).

%% API
-export([delete_commitment/2,
         empty/0,
         enter_commitment/2,
         enter_name/2,
         get_name/2,
         lookup_commitment/2,
         lookup_name/2,
         root_hash/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type mkey() :: aens_commitments:id() | aens_names:id().
-type mvalue() :: aens_commitments:serialized() | aens_names:serialized().
-type mtree() :: aeu_mtrees:tree(mkey(), mvalue()).
-type commitment() :: aens_commitments:commitment().
-type name() :: aens_names:name().

-record(ns_tree, {mtree = gb_merkle_trees:empty() :: mtree()}).

-opaque tree() :: #ns_tree{}.

-export_type([tree/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec delete_commitment(binary(), tree()) -> tree().
delete_commitment(Id, Tree) ->
    aeu_mtrees:delete(Id, Tree#ns_tree.mtree).

-spec empty() -> tree().
empty() ->
    MTree = aeu_mtrees:empty(),
    #ns_tree{mtree = MTree}.

-spec enter_commitment(commitment(), tree()) -> tree().
enter_commitment(C, Tree) ->
    Id = aens_commitments:id(C),
    Serialized = aens_commitments:serialize(C),
    aeu_mtrees:insert(Id, Serialized, Tree#ns_tree.mtree).

-spec enter_name(name(), tree()) -> tree().
enter_name(N, T) ->
    Id = aens_names:id(N),
    Serialized = aens_names:serialize(N),
    MTree1 = aeu_mtrees:enter(Id, Serialized, T#ns_tree.mtree),
    T#ns_tree{mtree = MTree1}.

-spec get_name(binary(), tree()) -> name().
get_name(Id, Tree) ->
    aens_names:deserialize(aeu_mtrees:get(Id, Tree#ns_tree.mtree)).

-spec lookup_commitment(commitment(), tree()) -> tree().
lookup_commitment(Id, Tree) ->
    case aeu_mtrees:get(Id, Tree#ns_tree.mtree) of
        {value, Val} -> {value, aens_commitments:deserialize(Val)};
        none -> none
    end.

-spec lookup_name(binary(), tree()) -> {value, name()} | none.
lookup_name(Id, Tree) ->
    case aeu_mtrees:get(Id, Tree#ns_tree.mtree) of
        {value, Val} -> {value, aens_names:deserialize(Val)};
        none -> none
    end.

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#ns_tree{mtree = MTree}) ->
    aeu_mtrees:root_hash(MTree).
