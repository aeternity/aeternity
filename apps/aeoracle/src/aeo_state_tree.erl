%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of oracles
%%% @end
%%%-------------------------------------------------------------------

-module(aeo_state_tree).

%% API
-export([ get_query/3
        , get_oracle/2
        , empty/0
        , enter_query/2
        , insert_query/2
        , insert_oracle/2
        , lookup_query/3
        , lookup_oracle/2
        , prune/2
        , root_hash/1
        ]).

-ifdef(TEST).
-export([ query_list/1
        , oracle_list/1
        ]).
-endif.

%% The oracle state tree keep track of oracles and its associated queries
%% (query objects). The naive approach, storing the queries directly
%% in the oracle field in the state tree, does not work well. Since the state
%% tree has to be a Merkle tree all nodes are serialized. This would mean
%% deserialize/serialize of all queries when adding or updating a single
%% query. Instead we have a forest of Merkle trees (on Merkle tree per
%% oracle) in a separate structure, and only store the root hash of that
%% queries tree in the oracle field.

%%%===================================================================
%%% Types
%%%===================================================================

-type otree() :: aeu_mtrees:tree(aeo_oracles:id(), aeo_oracles:serialized()).
-type itree() :: aeu_mtrees:tree(aeo_query:id(), aeo_query:serialized()).
-type query() :: aeo_query:query().
-type oracle() :: aeo_oracles:oracle().
-type itrees() :: gb_trees:tree(aeo_oracles:id(), itree()).
-type cache_item() :: {oracle, aeo_oracles:id()}
                    | {query, aeo_oracles:id(), aeo_query:id()}.
-type cache() :: gb_sets:set({integer(), cache_item()}).
-type block_height() :: non_neg_integer().

-record(oracle_tree, { otree  = aeu_mtrees:empty() :: otree()
                     , itrees = gb_trees:empty()   :: itrees()
                     , cache  = cache_new()        :: cache()
                     }).

-opaque tree() :: #oracle_tree{}.

-export_type([ tree/0
             ]).

-define(HASH_SIZE, 32).

%%%===================================================================
%%% API
%%%===================================================================

-spec empty() -> tree().
empty() ->
    OTree  = aeu_mtrees:empty(),
    ITrees = gb_trees:empty(),
    #oracle_tree{ otree  = OTree
                , itrees = ITrees
                , cache  = cache_new()
                }.

-spec prune(block_height(), tree()) -> tree().
prune(Height, #oracle_tree{} = Tree) ->
    %% TODO: We need to know what we pruned as well
    %% Oracle information should be around for the expiry block
    %% since we prune before the block, use Height - 1 for pruning.
    int_prune(Height - 1, Tree).

-spec enter_query(query(), tree()) -> tree().
enter_query(I, Tree) ->
    add_query(enter, I, Tree).

-spec insert_query(query(), tree()) -> tree().
insert_query(I, Tree) ->
    add_query(insert, I, Tree).

-spec get_query(aeo_oracles:id(), aeo_query:id(), tree()) -> query().
get_query(OracleId, Id, Tree) ->
    ITree = gb_trees:get(OracleId, Tree#oracle_tree.itrees),
    aeo_query:deserialize(aeu_mtrees:get(Id, ITree)).

-spec lookup_query(aeo_oracles:id(), aeo_query:id(), tree()) ->
                                                {'value', query()} | none.
lookup_query(OracleId, Id, Tree) ->
    case gb_trees:lookup(OracleId, Tree#oracle_tree.itrees) of
        {value, ITree} ->
            case aeu_mtrees:lookup(Id, ITree) of
                {value, Val} -> {value, aeo_query:deserialize(Val)};
                none -> none
            end;
        none -> none
    end.

-spec insert_oracle(oracle(), tree()) -> tree().
insert_oracle(O, Tree) ->
    Id = aeo_oracles:id(O),
    Serialized = aeo_oracles:serialize(O),
    Expires = aeo_oracles:expires(O),

    OTree  = aeu_mtrees:insert(Id, Serialized, Tree#oracle_tree.otree),
    ITrees = gb_trees:insert(Id, aeu_mtrees:empty(), Tree#oracle_tree.itrees),
    Cache  = cache_push({oracle, Id}, Expires, Tree#oracle_tree.cache),
    Tree#oracle_tree{ otree  = OTree
                    , itrees = ITrees
                    , cache  = Cache
                    }.

-spec get_oracle(binary(), tree()) -> oracle().
get_oracle(Id, Tree) ->
    aeo_oracles:deserialize(aeu_mtrees:get(Id, Tree#oracle_tree.otree)).

-spec lookup_oracle(binary(), tree()) -> {'value', oracle()} | 'none'.
lookup_oracle(Id, Tree) ->
    case aeu_mtrees:lookup(Id, Tree#oracle_tree.otree) of
        {value, Val}  -> {value, aeo_oracles:deserialize(Val)};
        none -> none
    end.

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#oracle_tree{otree = OTree}) ->
    aeu_mtrees:root_hash(OTree).

-ifdef(TEST).
-spec oracle_list(tree()) -> list(oracle()).
oracle_list(#oracle_tree{otree = OTree}) ->
    [ aeo_oracles:deserialize(Val) || {_, Val} <- aeu_mtrees:to_list(OTree) ].

-spec query_list(tree()) -> list(query()).
query_list(#oracle_tree{itrees = ITrees}) ->
    [ aeo_query:deserialize(Val) || {_, ITree} <- gb_trees:to_list(ITrees),
                                    {_, Val} <- aeu_mtrees:to_list(ITree) ].
-endif.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_query(How, I, Tree) ->
    OracleId    = aeo_query:oracle_address(I),
    Oracle      = get_oracle(OracleId, Tree),
    ITree       = gb_trees:get(OracleId, Tree#oracle_tree.itrees),
    Id          = aeo_query:id(I),
    SerializedI = aeo_query:serialize(I),
    Expires     = aeo_query:expires(I),
    ITree1      = case How of
                      enter  -> aeu_mtrees:enter(Id, SerializedI, ITree);
                      insert -> aeu_mtrees:insert(Id, SerializedI, ITree)
                  end,

    IRoot       = safe_root_hash(ITree1),
    SerializedO = aeo_oracles:serialize(
                      aeo_oracles:set_queries_hash(IRoot, Oracle)),

    OTree  = aeu_mtrees:enter(OracleId, SerializedO, Tree#oracle_tree.otree),
    ITrees = gb_trees:update(OracleId, ITree1, Tree#oracle_tree.itrees),
    Cache  = cache_push({query, OracleId, Id}, Expires, Tree#oracle_tree.cache),
    Tree#oracle_tree{ otree  = OTree
                    , itrees = ITrees
                    , cache  = Cache
                    }.

int_prune(Height, #oracle_tree{ cache = Cache } = Tree) ->
    int_prune(cache_safe_peek(Cache), Height, Tree).

int_prune(none, _Height, Tree) ->
    Tree;
int_prune({Height, Id}, Height, #oracle_tree{ cache = Cache } = Tree) ->
    {{Height, Id}, Cache1} = cache_pop(Cache),
    Tree1 = delete(Id, Tree#oracle_tree{ cache = Cache1 }),
    int_prune(cache_safe_peek(Cache1), Height, Tree1);
int_prune({Height1,_Id}, Height2, Tree) when Height2 < Height1 ->
    Tree.

delete({oracle, Id}, Tree) ->
    OTree  = aeu_mtrees:delete(Id, Tree#oracle_tree.otree),
    ITrees = gb_trees:delete(Id, Tree#oracle_tree.itrees),
    Tree#oracle_tree{ otree = OTree, itrees = ITrees };
delete({query, OracleId, Id}, Tree) ->
    %% It is possible that the oracle expired first.
    %% Since {X, {oracle, Hash}} sorts before {X, {query, H1, H2}} the
    %% inverse cannot happen.
    case gb_trees:lookup(OracleId, Tree#oracle_tree.itrees) of
        {value, ITree} ->
            ITree1 = aeu_mtrees:delete(Id, ITree),
            ITrees = gb_trees:update(OracleId, ITree1, Tree#oracle_tree.itrees),
            Oracle = get_oracle(OracleId, Tree),
            IRoot  = safe_root_hash(ITree1),
            SerializedO = aeo_oracles:serialize(
                              aeo_oracles:set_queries_hash(IRoot, Oracle)),

            OTree  = aeu_mtrees:enter(OracleId, SerializedO, Tree#oracle_tree.otree),
            Tree#oracle_tree{ otree = OTree, itrees = ITrees };
        none ->
            Tree
    end.

%%%===================================================================
%%% TTL Cache
%%%===================================================================

cache_new() ->
    gb_sets:empty().

cache_push(Id, Expires, C) ->
    gb_sets:add({Expires, Id}, C).

cache_safe_peek(C) ->
    case gb_sets:is_empty(C) of
        true  -> none;
        false -> gb_sets:smallest(C)
    end.

cache_pop(C) ->
    gb_sets:take_smallest(C).

safe_root_hash(ITree) ->
    case aeu_mtrees:root_hash(ITree) of
        {ok, RootHash} -> RootHash;
        {error, empty} -> <<0:(?HASH_SIZE*8)>>
    end.
