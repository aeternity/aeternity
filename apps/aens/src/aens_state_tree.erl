%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for keeping the state of Naming System
%%% @end
%%%=============================================================================

-module(aens_state_tree).

-include("aens.hrl").

%% API
-export([commit_to_db/1,
         cache_root_hash/1,
         delete_commitment/2,
         delete_name/2,
         empty/0,
         empty_with_backend/0,
         enter_commitment/2,
         enter_name/2,
         enter_subname/2,
         get_name/2,
         get_subname/2,
         prune/2,
         prune_subnames/2,
         lookup_commitment/2,
         lookup_name/2,
         new_with_backend/2,
         root_hash/1]).

-export([ from_binary_without_backend/1
        , to_binary_without_backend/1
        ]).

-export([record_fields/1]).

%% Export for test
-ifdef(TEST).
-export([ name_list/1
        , subname_list/1
        , commitment_list/1
        , subnames_hashes/3
        ]).
-endif.

%%%===================================================================
%%% Types
%%%===================================================================

-type mkey() :: aens_hash:commitment_hash() | aens_hash:name_hash().
-type mvalue() :: aens_commitments:serialized() | aens_names:serialized().
-type nstree() :: aeu_mtrees:mtree(mkey(), mvalue()).
-type commitment() :: aens_commitments:commitment().
-type name() :: aens_names:name().
-type subname() :: aens_subnames:subname().
-type cache() :: aeu_mtrees:mtree(cache_key(), cache_value()).
-type cache_key() :: binary(). %% Sext encoded
-type cache_value() :: binary(). %% ?DUMMY_VAL
-type block_height() :: non_neg_integer().

-record(ns_tree, { mtree = aeu_mtrees:empty() :: nstree()
                 , cache = aeu_mtrees:empty() :: cache()
                 }).

-opaque tree() :: #ns_tree{}.

-export_type([tree/0]).

-define(VSN, 1).

%% ==================================================================
%% Tracing support
record_fields(ns_tree) -> record_info(fields, ns_tree);
record_fields(_      ) -> no.
%% ==================================================================

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
    #ns_tree{}.

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    MTree = aeu_mtrees:empty_with_backend(aec_db_backends:ns_backend()),
    Cache = aeu_mtrees:empty_with_backend(aec_db_backends:ns_cache_backend()),
    #ns_tree{mtree = MTree, cache = Cache}.

-spec new_with_backend(aeu_mtrees:root_hash() | 'empty',
                       aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_backend(RootHash, CacheRootHash) ->
    MTree = aeu_mtrees:new_with_backend(RootHash, aec_db_backends:ns_backend()),
    Cache = aeu_mtrees:new_with_backend(CacheRootHash, aec_db_backends:ns_cache_backend()),
    #ns_tree{mtree = MTree, cache = Cache}.

-spec prune(block_height(), tree()) -> tree().
prune(NextBlockHeight, #ns_tree{} = Tree) ->
    {Tree1, ExpiredActions} = int_prune(NextBlockHeight - 1, Tree),
    {Tree2, DeletedNames} = run_elapsed(ExpiredActions, Tree1, NextBlockHeight, []),
    prune_subnames(DeletedNames, Tree2).


run_elapsed([], Tree, _, Acc) ->
    {Tree, Acc};
run_elapsed([{aens_names, Id, Serialized}|Expired], Tree, Height, Acc) ->
    Name = aens_names:deserialize(Id, Serialized),
    {Action, Tree1} = run_elapsed_name(Name, Tree, Height),
    Acc1 = if Action == deleted -> [aens_names:hash(Name) | Acc]; true -> Acc end,
    run_elapsed(Expired, Tree1, Height, Acc1);
run_elapsed([{aens_commitments, Id, Serialized}|Expired], Tree, Height, Acc) ->
    Commitment = aens_commitments:deserialize(Id, Serialized),
    {deleted, Tree1} = run_elapsed_commitment(Commitment, Tree),
    run_elapsed(Expired, Tree1, Height, Acc).

-spec enter_commitment(commitment(), tree()) -> tree().
enter_commitment(Commitment, Tree) ->
    CommitmentHash = aens_commitments:hash(Commitment),
    Serialized = aens_commitments:serialize(Commitment),
    TTL = aens_commitments:ttl(Commitment),
    %% TODO: consider two trees (names vs pre-claims/commitments)
    Cache1 = cache_push(TTL, CommitmentHash, aens_commitments, Tree#ns_tree.cache),
    MTree1 = aeu_mtrees:insert(CommitmentHash, Serialized, Tree#ns_tree.mtree),
    Tree#ns_tree{cache = Cache1, mtree = MTree1}.

-spec enter_name(name(), tree()) -> tree().
enter_name(Name, Tree) ->
    NameHash = aens_names:hash(Name),
    Serialized = aens_names:serialize(Name),
    TTL = aens_names:ttl(Name),
    Cache1 = cache_push(TTL, NameHash, aens_names, Tree#ns_tree.cache),
    MTree1 = aeu_mtrees:enter(NameHash, Serialized, Tree#ns_tree.mtree),
    Tree#ns_tree{cache = Cache1, mtree = MTree1}.

-spec enter_subname(subname(), tree()) -> tree().
enter_subname(Subname, Tree) ->
    NameHash = aens_subnames:hash(Subname),
    Serialized = aens_subnames:serialize(Subname),
    MTree1 = aeu_mtrees:enter(NameHash, Serialized, Tree#ns_tree.mtree),
    Tree#ns_tree{mtree = MTree1}.

-spec get_name(binary(), tree()) -> name().
get_name(Id, Tree) ->
    aens_names:deserialize(Id, aeu_mtrees:get(Id, Tree#ns_tree.mtree)).

-spec get_subname(binary(), tree()) -> subname().
get_subname(Id, Tree) ->
    aens_subnames:deserialize(Id, aeu_mtrees:get(Id, Tree#ns_tree.mtree)).

-spec lookup_commitment(binary(), tree()) -> {value, commitment()} | none.
lookup_commitment(Id, Tree) ->
    case aeu_mtrees:lookup(Id, Tree#ns_tree.mtree) of
        {value, Val} -> {value, aens_commitments:deserialize(Id, Val)};
        none -> none
    end.

-spec lookup_name(binary(), tree()) -> {value, name() | subname()} | none.
lookup_name(Id, Tree) ->
    case {aeu_mtrees:lookup(Id, Tree#ns_tree.mtree), size(Id)} of
        {{value, Val}, ?NAME_HASH_BYTES} -> {value, aens_names:deserialize(Id, Val)};
        {{value, Val}, ?SUBNAME_HASH_BYTES} -> {value, aens_subnames:deserialize(Id, Val)};
        {none, _} -> none
    end.

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#ns_tree{mtree = MTree}) ->
    aeu_mtrees:root_hash(MTree).

-spec cache_root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
cache_root_hash(#ns_tree{cache = MTree}) ->
    aeu_mtrees:root_hash(MTree).

-spec commit_to_db(tree()) -> tree().
commit_to_db(#ns_tree{mtree = MTree, cache = Cache} = Tree) ->
    Tree#ns_tree{mtree = aeu_mtrees:commit_to_db(MTree),
                 cache = aeu_mtrees:commit_to_db(Cache)
                }.

-ifdef(TEST).
-spec commitment_list(tree()) -> list(commitment()).
commitment_list(#ns_tree{mtree = Tree}) ->
    IsCommitment = fun(Id, MaybeC) ->
                       try [aens_commitments:deserialize(Id, MaybeC)]
                       catch _:_ -> [] end
                   end,
    [ C || {Id, Val} <- aeu_mtrees:to_list(Tree),
           C <- IsCommitment(Id, Val) ].

-spec name_list(tree()) -> list(name()).
name_list(#ns_tree{mtree = Tree}) ->
    IsName = fun(Id, MaybeC) ->
                 try [aens_names:deserialize(Id, MaybeC)]
                 catch _:_ -> [] end
             end,
    [ C || {Id, Val} <- aeu_mtrees:to_list(Tree),
           C <- IsName(Id, Val) ].

-spec subname_list(tree()) -> list(subname()).
subname_list(#ns_tree{mtree = Tree}) ->
    IsName = fun(Id, MaybeC) ->
                 try [aens_subnames:deserialize(Id, MaybeC)]
                 catch _:_ -> [] end
             end,
    [ C || {Id, Val} <- aeu_mtrees:to_list(Tree),
           C <- IsName(Id, Val) ].
-endif.


-spec to_binary_without_backend(tree()) -> binary().
to_binary_without_backend(#ns_tree{mtree = MTree}) ->
    MTBin = aeu_mtrees:serialize(MTree),
    aeser_chain_objects:serialize(
        nameservice_mtree,
        ?VSN,
        serialization_template(?VSN),
        [{mtree, MTBin}]).

-spec from_binary_without_backend(binary()) -> tree().
from_binary_without_backend(Bin) ->
    [{mtree, MTBin}] =
        aeser_chain_objects:deserialize(nameservice_mtree, ?VSN,
                                             serialization_template(?VSN), Bin),
    MTree = aeu_mtrees:deserialize(MTBin),
    Cache = create_cache_from_mtree(MTree, aeu_mtrees:empty()),
    #ns_tree{mtree = MTree,
             cache = Cache}.

serialization_template(?VSN) ->
    [{mtree, binary}].

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
            int_prune(cache_safe_peek(Cache1), NextBlockHeight, Cache1, MTree, [{Mod, Id, ExpiredAction}|ExpiredAcc]);
        none ->
            int_prune(cache_safe_peek(Cache1), NextBlockHeight, Cache1, MTree, ExpiredAcc)
    end.

%% INFO: run_elapsed_name/3 and run_elapsed_commitment/2 implements 'expire' driven transitions:
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

run_elapsed_name(Name, NamesTree0, NextBlockHeight) ->
    ExpirationBlockHeight = aens_names:ttl(Name),
    Status = aens_names:status(Name),
    case ExpirationBlockHeight =:= (NextBlockHeight - 1) of
        false ->
            %% INFO: Do nothing.
            %%       Name was updated and we triggered old cache event.
            {nop, NamesTree0};
        true when Status =:= claimed ->
            NameHash = aens_names:hash(Name),
            Name0    = aens_state_tree:get_name(NameHash, NamesTree0),
            TTL      = aec_governance:name_protection_period(),
            Name1    = aens_names:revoke(Name0, TTL, ExpirationBlockHeight),
            {revoked, aens_state_tree:enter_name(Name1, NamesTree0)};
        true when Status =:= revoked ->
            NameHash = aens_names:hash(Name),
            {deleted, aens_state_tree:delete_name(NameHash, NamesTree0)}
    end.

run_elapsed_commitment(Commitment, NamesTree0) ->
    %% INFO: We delete in both cases when name is claimed or not claimed
    %%       when it expires
    CommitmentHash = aens_commitments:hash(Commitment),
    NamesTree1 = aens_state_tree:delete_commitment(CommitmentHash, NamesTree0),
    {deleted, NamesTree1}.


-define(SUBNAMES_PRUNE_BATCH_SIZE, 2).

-spec prune_subnames([binary()], tree()) -> tree().
prune_subnames([], #ns_tree{} = Tree) ->
    Tree;
prune_subnames([NameHash | NameHashes], #ns_tree{} = Tree) ->
    {SubnamesHashes, HasNext} =
        subnames_hashes(NameHash, Tree, ?SUBNAMES_PRUNE_BATCH_SIZE),
    Tree1 = lists:foldl(fun delete_name/2, Tree, SubnamesHashes),
    prune_subnames(if HasNext == true  -> [NameHash | NameHashes];
                      HasNext == false -> NameHashes
                   end,
                   Tree1).


subnames_hashes(NameHash, #ns_tree{mtree = MTree}, BatchSize)
  when (is_integer(BatchSize) andalso BatchSize > 1) orelse BatchSize == all ->
    Iter = aeu_mtrees:iterator_from(NameHash, MTree, [{with_prefix, NameHash}]),
    iter_subnames_hashes(Iter, 0, BatchSize, []).

iter_subnames_hashes(_Iter, Max, Max, Acc) ->
    {Acc, true};
iter_subnames_hashes(Iter, Count, Max, Acc) ->
    case aeu_mtrees:iterator_next(Iter) of
        '$end_of_table' ->
            {Acc, false};
        {Key, _Val, Next} ->
            iter_subnames_hashes(Next, Count + 1, Max, [Key | Acc])
    end.

%%%===================================================================
%%% TTL Cache
%%%===================================================================
-define(DUMMY_VAL, <<0>>).

cache_push(TTL, Hash, Mod, C) ->
    SExt = sext:encode({TTL, Hash, Mod}),
    aeu_mtrees:enter(SExt, ?DUMMY_VAL, C).

cache_safe_peek(C) ->
    case aeu_mtrees:iterator_next(aeu_mtrees:iterator(C)) of
        '$end_of_table' -> none;
        {Next, ?DUMMY_VAL, _Iter} -> sext:decode(Next)
    end.

cache_pop(C) ->
    case aeu_mtrees:iterator_next(aeu_mtrees:iterator(C)) of
        '$end_of_table' -> none;
        {Next,?DUMMY_VAL,_Iter} ->
            {sext:decode(Next), aeu_mtrees:delete(Next, C)}
    end.

create_cache_from_mtree(MTree, EmptyCache) ->
    create_cache_from_mtree_(aeu_mtrees:iterator_next(
                               aeu_mtrees:iterator(MTree)), EmptyCache).

create_cache_from_mtree_('$end_of_table', Cache) -> Cache;
create_cache_from_mtree_({Key, Val, Iter}, Cache0) ->
    {Type, Vsn, RawFields} = aeser_chain_objects:deserialize_type_and_vsn(Val),
    [SubType, NameType, CommType] =
        [M:serialization_type() || M <- [aens_subnames, aens_names, aens_commitments]],
    if Type == SubType ->
            Cache0;
       true ->
            Mod = if Type == NameType -> aens_names;
                     Type == CommType -> aens_commitments
                  end,
            Template = Mod:serialization_template(Vsn),
            Fields = aeserialization:decode_fields(Template, RawFields),
            Obj = Mod:deserialize_from_fields(Vsn, Key, Fields),
            TTL = Mod:ttl(Obj),
            Cache = cache_push(TTL, Key, Mod, Cache0),
            create_cache_from_mtree_(aeu_mtrees:iterator_next(Iter), Cache)
    end.
