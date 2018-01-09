%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for keeping the state of Naming System
%%% @end
%%%=============================================================================

-module(aens_state_tree).

-include_lib("apps/aecore/include/common.hrl").
-include("aens.hrl").

%% API
-export([delete_commitment/2,
         delete_name/2,
         empty/0,
         enter_commitment/2,
         enter_name/2,
         get_name/2,
         prune/2,
         lookup_commitment/2,
         lookup_name/2,
         root_hash/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type mkey() :: aens_commitments:id() | aens_names:id().
-type mvalue() :: aens_commitments:serialized() | aens_names:serialized().
-type nstree() :: aeu_mtrees:tree(mkey(), mvalue()).
-type commitment() :: aens_commitments:commitment().
-type name() :: aens_names:name().
-type cache() :: gb_sets:set({integer(), binary()}).
-type block_height() :: non_neg_integer().

-record(ns_tree, { mtree = aeu_mtrees:empty() :: nstree()
                 , cache = cache_new() :: cache()
                 }).

-opaque tree() :: #ns_tree{}.

-export_type([tree/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec delete_commitment(binary(), tree()) -> tree().
delete_commitment(Id, Tree) ->
    MTree1 = aeu_mtrees:delete(Id, Tree#ns_tree.mtree),
    Tree#ns_tree{mtree = MTree1}.

-spec delete_name(binary(), tree()) -> tree().
delete_name(Id, Tree) ->
    MTree1 = aeu_mtrees:delete(Id, Tree#ns_tree.mtree),
    Tree#ns_tree{mtree = MTree1}.

-spec empty() -> tree().
empty() ->
    MTree = aeu_mtrees:empty(),
    #ns_tree{mtree = MTree}.

-spec prune(block_height(), tree()) -> tree().
prune(NextBlockHeight, #ns_tree{} = Tree) ->
    {Tree1, ExpiredActions} = int_prune(NextBlockHeight - 1, Tree),
    run_elapsed(ExpiredActions, Tree1, NextBlockHeight).

run_elapsed([], Tree, _) ->
    Tree;
run_elapsed([{Mod, Serialized}|Expired], Tree, Height) ->
    Entity = Mod:deserialize(Serialized),
    {ok, Tree1} = do_run_elapsed(Entity, Tree, Height),
    run_elapsed(Expired, Tree1, Height).

-spec enter_commitment(commitment(), tree()) -> tree().
enter_commitment(Commitment, Tree) ->
    Id = aens_commitments:id(Commitment),
    Serialized = aens_commitments:serialize(Commitment),
    Expiration = aens_commitments:expires(Commitment),
    %% TODO: consider two trees (names vs pre-claims/commitments)
    Cache1 = cache_push(Expiration, Id, aens_commitments, Tree#ns_tree.cache),
    MTree1 = aeu_mtrees:insert(Id, Serialized, Tree#ns_tree.mtree),
    Tree#ns_tree{cache = Cache1, mtree = MTree1}.

-spec enter_name(name(), tree()) -> tree().
enter_name(Name, Tree) ->
    Id = aens_names:id(Name),
    Serialized = aens_names:serialize(Name),
    Expiration = aens_names:expires(Name),
    Cache1 = cache_push(Expiration, Id, aens_names, Tree#ns_tree.cache),
    MTree1 = aeu_mtrees:enter(Id, Serialized, Tree#ns_tree.mtree),
    Tree#ns_tree{cache = Cache1, mtree = MTree1}.

-spec get_name(binary(), tree()) -> name().
get_name(Id, Tree) ->
    aens_names:deserialize(aeu_mtrees:get(Id, Tree#ns_tree.mtree)).

-spec lookup_commitment(binary(), tree()) -> {value, commitment()} | none.
lookup_commitment(Id, Tree) ->
    case aeu_mtrees:lookup(Id, Tree#ns_tree.mtree) of
        {value, Val} -> {value, aens_commitments:deserialize(Val)};
        none -> none
    end.

-spec lookup_name(binary(), tree()) -> {value, name()} | none.
lookup_name(Id, Tree) ->
    case aeu_mtrees:lookup(Id, Tree#ns_tree.mtree) of
        {value, Val} -> {value, aens_names:deserialize(Val)};
        none -> none
    end.

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#ns_tree{mtree = MTree}) ->
    aeu_mtrees:root_hash(MTree).

%%%===================================================================
%%% Internal functions
%%%===================================================================

int_prune(NextBlockHeight, #ns_tree{cache = Cache, mtree = MTree} = Tree) ->
    {Cache1, Mtree1, ExpiredActions} = int_prune(cache_safe_peek(Cache), NextBlockHeight, Cache, MTree, []),
    {Tree#ns_tree{ cache = Cache1, mtree = Mtree1}, ExpiredActions}.

int_prune(none, _NextBlockHeight, Cache, MTree, ExpiredAcc) ->
    {Cache, MTree, lists:reverse(ExpiredAcc)};
int_prune({Height1,_Id,_Mod}, NextBlockHeight, Cache, MTree, ExpiredAcc) when NextBlockHeight < Height1 ->
    {Cache, MTree, lists:reverse(ExpiredAcc)};
int_prune({HeightLower, Id, Mod}, NextBlockHeight, Cache, MTree, ExpiredAcc) ->
    {{HeightLower, Id, Mod}, Cache1} = cache_pop(Cache),
    case aeu_mtrees:lookup(Id, MTree) of
        {value, ExpiredAction} ->
            int_prune(cache_safe_peek(Cache1), NextBlockHeight, Cache1, MTree, [{Mod, ExpiredAction}|ExpiredAcc]);
        none ->
            int_prune(cache_safe_peek(Cache1), NextBlockHeight, Cache1, MTree, ExpiredAcc)
    end.

%% INFO: do_run_elapsed/3 implements 'expire' driven transitions:
%%
%%                   expire
%%       unclaimed <-------- revoked
%%           | ^              ^^
%%           | |              || expire
%%           | | expire       ||
%% pre-claim | |              ||  _
%%           | |       revoke || | | transfer
%%           v |              || | v
%%      pre-claimed -------> claimed
%%                   claim    | ^
%%                            | |
%%                             -
%%                           update
%%

do_run_elapsed(#name{expires = ExpirationBlockHeight}, NamesTree0, NextBlockHeight)
    when ExpirationBlockHeight /= NextBlockHeight-1 ->
    %% INFO: Do nothing. Name was updated and we triggered old cache event.
    {ok, NamesTree0};
do_run_elapsed(#name{hash = NameHash, status = claimed, expires = ExpirationBlockHeight},
               NamesTree0, NextBlockHeight) when ExpirationBlockHeight == NextBlockHeight-1 ->
    Name0 = aens_state_tree:get_name(NameHash, NamesTree0),
    TTL = aec_governance:name_protection_period(),
    Name1 = aens_names:revoke(Name0, TTL, ExpirationBlockHeight),
    NamesTree1 = aens_state_tree:enter_name(Name1, NamesTree0),
    {ok, NamesTree1};
do_run_elapsed(#name{hash = NameHash, status = revoked, expires = ExpirationBlockHeight},
    NamesTree0, NextBlockHeight) when ExpirationBlockHeight == NextBlockHeight-1  ->
    NamesTree1 = aens_state_tree:delete_name(NameHash, NamesTree0),
    {ok, NamesTree1};
do_run_elapsed(#commitment{hash = Hash}, NamesTree0, _Height) ->
    %% INFO: We delete in both cases when name is claimed or not claimed
    %%       when it expires
    NamesTree1 = aens_state_tree:delete_commitment(Hash, NamesTree0),
    {ok, NamesTree1}.

%%%===================================================================
%%% TTL Cache
%%%===================================================================

cache_new() ->
    gb_sets:empty().

cache_push(Expires, Id, Mod, C) ->
    gb_sets:add({Expires, Id, Mod}, C).

cache_safe_peek(C) ->
    case gb_sets:is_empty(C) of
        true  -> none;
        false -> gb_sets:smallest(C)
    end.

cache_pop(C) ->
    gb_sets:take_smallest(C).
