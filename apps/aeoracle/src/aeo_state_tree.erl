%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of oracles
%%% @end
%%%-------------------------------------------------------------------

-module(aeo_state_tree).

%% API
-export([ cache_root_hash/1
        , commit_to_db/1
        , get_query/3
        , get_oracle/2
        , get_oracle_query_ids/2
        , get_oracle_queries/5
        , get_oracles/3
        , empty/0
        , empty_with_backend/0
        , enter_oracle/2
        , enter_query/2
        , insert_query/2
        , insert_oracle/2
        , is_oracle/2
        , lookup_query/3
        , lookup_oracle/2
        , new_with_backend/2
        , new_with_dirty_backend/2
        , prune/2
        , prune/3
        , root_hash/1
        , flush_oracle_batch/1
        , oracles_db/1
        , cache_db/1
        ]).

-export([ from_binary_without_backend/1
        , to_binary_without_backend/1
        , from_db_format/1
        ]).

-export([record_fields/1]).

-ifdef(TEST).
-export([ query_list/1
        , oracle_list/1
        ]).
-endif.

-define(PUB_SIZE, 32).

%% The oracle state tree keep track of oracles and its associated queries
%% (query objects). The naive approach, storing the queries directly
%% in the oracle field in the state tree, does not work well. Since the state
%% tree has to be a Merkle tree all nodes are serialized. This would mean
%% deserialize/serialize of all queries when adding or updating a single
%% query. Instead we store the queries prefixed with the oracle id in
%% the same tree as the oracles. This is to enable iteration over a single
%% oracle's queries.

%%%===================================================================
%%% Types
%%%===================================================================

-type otree() :: aeu_mtrees:mtree().
-type query() :: aeo_query:query().
-type oracle() :: aeo_oracles:oracle().
-type cache() :: aeu_mtrees:mtree(cache_key(), cache_value()).
-type cache_key() :: binary(). %% Sext encoded
-type cache_value() :: binary(). %% ?DUMMY_VAL
-type block_height() :: non_neg_integer().

%% Per-microblock deferred oracle/query writes, keyed by otree key
%% (oracle pubkey, or <<OraclePubkey, QueryId>> for a query). The flush
%% replays through the unchanged writer (do_add_oracle/do_add_query),
%% maintaining both the otree and the secondary TTL cache. The cache is
%% not part of the consensus root and prune re-validates expiry from the
%% real object, so collapsing repeated same-key writes does not change
%% consensus.
-type oracle_batch_entry() :: {oracle | query, insert | enter,
                         aeo_oracles:oracle() | aeo_query:query()}.
-type oracle_batch() :: #{binary() => oracle_batch_entry()}.

-record(oracle_tree, { otree  = aeu_mtrees:empty() :: otree()
                     , cache  = aeu_mtrees:empty() :: cache()
                     , oracle_batch = #{}               :: oracle_batch()
                     }).

-opaque tree() :: #oracle_tree{}.

-export_type([ tree/0
             ]).

-define(HASH_SIZE, 32).

-define(VSN, 1).

%% ==================================================================
%% Tracing support
record_fields(oracle_tree) -> record_info(fields, oracle_tree);
record_fields(_          ) -> no.
%% ==================================================================

%%%===================================================================
%%% API
%%%===================================================================

-spec empty() -> tree().
empty() ->
    #oracle_tree{ otree  = aeu_mtrees:empty()
                , cache  = aeu_mtrees:empty()
                }.

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    OTree  = aeu_mtrees:empty_with_backend(aec_db_backends:oracles_backend()),
    Cache  = aeu_mtrees:empty_with_backend(aec_db_backends:oracles_cache_backend()),
    #oracle_tree{ otree  = OTree
                , cache  = Cache
                }.

-spec from_db_format(tree()) -> tree().
from_db_format(Tree = #oracle_tree{ otree = OTree, cache = Cache }) ->
    Tree#oracle_tree{ otree = aeu_mtrees:from_db_format(OTree),
                      cache = aeu_mtrees:from_db_format(Cache) }.

-spec new_with_backend(aeu_mtrees:root_hash() | 'empty',
                       aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_backend(RootHash, CacheRootHash) ->
    OTree  = aeu_mtrees:new_with_backend(RootHash, aec_db_backends:oracles_backend()),
    Cache  = aeu_mtrees:new_with_backend(CacheRootHash, aec_db_backends:oracles_cache_backend()),
    #oracle_tree{ otree  = OTree
                , cache  = Cache
                }.

-spec new_with_dirty_backend(aeu_mtrees:root_hash() | 'empty',
                             aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_dirty_backend(RootHash, CacheRootHash) ->
    OTree  = aeu_mtrees:new_with_backend(RootHash, aec_db_backends:dirty_oracles_backend()),
    Cache  = aeu_mtrees:new_with_backend(CacheRootHash, aec_db_backends:dirty_oracles_cache_backend()),
    #oracle_tree{ otree  = OTree
                , cache  = Cache
                }.

-spec prune(block_height(), aec_trees:trees()) -> aec_trees:trees().
prune(Height, Trees0) ->
    %% Oracle information should be around for the expiry block
    %% since we prune before the block, use Height - 1 for pruning.
    %% Flush first: prune walks the otree+cache directly (defensive —
    %% at generation boundaries the batch is already empty).
    Trees = flush_oracles_in(Trees0),
    {Trees1, _} = int_prune(Height - 1, Trees, undefined),
    Trees1.

flush_oracles_in(Trees) ->
    aec_trees:set_oracles(Trees, flush_oracle_batch(aec_trees:oracles(Trees))).

-spec prune(block_height(), aec_trees:trees(), aetx_env:env()) -> {aec_trees:trees(), aetx_env:env()}.
prune(Height, Trees0, TxEnv) ->
    %% Oracle information should be around for the expiry block
    %% since we prune before the block, use Height - 1 for pruning.
    Trees = flush_oracles_in(Trees0),
    int_prune(Height - 1, Trees, TxEnv).

-spec enter_query(query(), tree()) -> tree().
enter_query(I, Tree) ->
    add_query(enter, I, Tree).

-spec insert_query(query(), tree()) -> tree().
insert_query(I, Tree) ->
    add_query(insert, I, Tree).

-spec get_query(aeo_oracles:pubkey(), aeo_query:id(), tree()) -> query().
get_query(OracleId, QId, #oracle_tree{otree = OTree, oracle_batch = B}) ->
    TreeId = <<OracleId/binary, QId/binary>>,
    case maps:find(TreeId, B) of
        {ok, {query, _How, Q}} -> Q;
        _ ->
            Serialized = aeu_mtrees:get(TreeId, OTree),
            aeo_query:deserialize(QId, Serialized)
    end.

-spec lookup_query(aeo_oracles:pubkey(), aeo_query:id(), tree()) ->
    {'value', query()} | none.
lookup_query(OracleId, QId, #oracle_tree{otree = OTree, oracle_batch = B}) ->
    TreeId = <<OracleId/binary, QId/binary>>,
    case maps:find(TreeId, B) of
        {ok, {query, _How, Q}} -> {value, Q};
        _ ->
            case aeu_mtrees:lookup(TreeId, OTree) of
                {value, Val} -> {value, aeo_query:deserialize(QId, Val)};
                none -> none
            end
    end.

-spec enter_oracle(oracle(), tree()) -> tree().
enter_oracle(O, Tree) ->
    add_oracle(enter, O, Tree).

-spec insert_oracle(oracle(), tree()) -> tree().
insert_oracle(O, Tree) ->
    add_oracle(insert, O, Tree).

-spec get_oracle(binary(), tree()) -> oracle().
get_oracle(Id, #oracle_tree{otree = OTree, oracle_batch = B}) ->
    case maps:find(Id, B) of
        {ok, {oracle, _How, O}} -> O;
        _ -> aeo_oracles:deserialize(Id, aeu_mtrees:get(Id, OTree))
    end.

%% Enumeration entry points read the whole otree via an iterator —
%% flush the batch first so they see all pending writes (no non-funnel
%% path may observe a stale otree).
-spec get_oracle_query_ids(binary(), tree()) -> [aeo_query:id()].
get_oracle_query_ids(Id, Tree) ->
    find_oracle_query_ids(Id, flush_oracle_batch(Tree)).

-spec get_oracle_queries(aeo_oracles:pubkey(), binary() | '$first', open | closed | all,
                         non_neg_integer(), tree()) -> list(query()).
get_oracle_queries(OracleId, From, QueryType, Max, Tree) ->
    find_oracle_queries(OracleId, From, QueryType, Max, flush_oracle_batch(Tree)).

-spec get_oracles(binary() | '$first', non_neg_integer(), tree()) -> list(oracle()).
get_oracles(From, Max, Tree) ->
    find_oracles(From, Max, flush_oracle_batch(Tree)).

-spec lookup_oracle(binary(), tree()) -> {'value', oracle()} | 'none'.
lookup_oracle(Id, #oracle_tree{otree = OTree, oracle_batch = B}) ->
    case maps:find(Id, B) of
        {ok, {oracle, _How, O}} -> {value, O};
        _ ->
            case aeu_mtrees:lookup(Id, OTree) of
                {value, Val}  -> {value, aeo_oracles:deserialize(Id, Val)};
                none -> none
            end
    end.

-spec is_oracle(aeo_oracles:pubkey(), tree()) -> boolean().
is_oracle(Pubkey, #oracle_tree{ otree = OTree, oracle_batch = B }) ->
    case maps:find(Pubkey, B) of
        {ok, {oracle, _How, _}} -> true;
        _ ->
            case aeu_mtrees:lookup(Pubkey, OTree) of
                none -> false;
                {value, _} -> true
            end
    end.

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(Tree) ->
    #oracle_tree{otree = OTree} = flush_oracle_batch(Tree),
    aeu_mtrees:root_hash(OTree).

%% WARNING: backing MPT db of the *materialised* otree only — does not
%% reflect entries pending in `oracle_batch'.  Backend identity only.
-spec oracles_db(tree()) -> {'ok', aeu_mp_trees:db()}.
oracles_db(#oracle_tree{otree = OTree}) ->
    aeu_mtrees:db(OTree).

-spec cache_db(tree()) -> {'ok', aeu_mp_trees:db()}.
cache_db(#oracle_tree{cache = CTree}) ->
    aeu_mtrees:db(CTree).

-spec cache_root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
cache_root_hash(Tree) ->
    #oracle_tree{cache = CTree} = flush_oracle_batch(Tree),
    aeu_mtrees:root_hash(CTree).

-ifdef(TEST).
-spec oracle_list(tree()) -> list(oracle()).
oracle_list(Tree) ->
    #oracle_tree{otree = OTree} = flush_oracle_batch(Tree),
    [ aeo_oracles:deserialize(Key, Val)
      || {Key, Val} <- aeu_mtrees:to_list(OTree),
         byte_size(Key) =:= ?PUB_SIZE
    ].

-spec query_list(tree()) -> list(query()).
query_list(Tree) ->
    #oracle_tree{otree = OTree} = flush_oracle_batch(Tree),
    [ aeo_query:deserialize(QId, Val)
      || {Key = <<_:?PUB_SIZE/unit:8, QId/binary>>, Val} <- aeu_mtrees:to_list(OTree),
         byte_size(Key) > ?PUB_SIZE
    ].
-endif.

-spec commit_to_db(tree()) -> tree().
commit_to_db(Tree) ->
    #oracle_tree{otree = OTree, cache = Cache} = FT = flush_oracle_batch(Tree),
    FT#oracle_tree{otree = aeu_mtrees:commit_to_db(OTree),
                   cache = aeu_mtrees:commit_to_db(Cache)
                  }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% Public add_*: defer into the per-microblock batch (no otree/cache
%% write here). Last-writer-wins per key; flush replays the final object
%% through the unchanged writer, so the otree is byte-identical to
%% writing per-tx and the TTL cache is built the same way as before.
add_oracle(How, O, #oracle_tree{oracle_batch = B} = Tree) ->
    Pubkey = aeo_oracles:pubkey(O),
    Tree#oracle_tree{oracle_batch = B#{Pubkey => {oracle, How, O}}}.

add_query(How, I, #oracle_tree{oracle_batch = B} = Tree) ->
    OraclePubkey = aeo_query:oracle_pubkey(I),
    QueryId      = aeo_query:id(I),
    TreeId       = <<OraclePubkey/binary, QueryId/binary>>,
    Tree#oracle_tree{oracle_batch = B#{TreeId => {query, How, I}}}.

%% The original (now flush-time) writers: otree + secondary TTL cache.
do_add_oracle(How, O, #oracle_tree{ otree = OTree } = Tree) ->
    Pubkey = aeo_oracles:pubkey(O),
    Serialized = aeo_oracles:serialize(O),
    TTL = aeo_oracles:ttl(O),
    OTree1 = case How of
                enter  -> aeu_mtrees:enter(Pubkey, Serialized, OTree);
                insert -> aeu_mtrees:insert(Pubkey, Serialized, OTree)
            end,
    Cache  = cache_push({oracle, Pubkey}, TTL, Tree#oracle_tree.cache),
    Tree#oracle_tree{ otree  = OTree1
                    , cache  = Cache
                    }.

do_add_query(How, I, #oracle_tree{otree = OTree} = Tree) ->
    OraclePubkey = aeo_query:oracle_pubkey(I),
    QueryId      = aeo_query:id(I),
    TreeId       = <<OraclePubkey/binary, QueryId/binary>>,
    SerializedI  = aeo_query:serialize(I),
    TTL          = aeo_query:ttl(I),
    OTree1       = case How of
                       enter  -> aeu_mtrees:enter(TreeId, SerializedI, OTree);
                       insert -> aeu_mtrees:insert(TreeId, SerializedI, OTree)
                  end,
    Cache  = cache_push({query, OraclePubkey, QueryId}, TTL, Tree#oracle_tree.cache),
    Tree#oracle_tree{ otree  = OTree1
                    , cache  = Cache
                    }.

%% Flush all pending oracle/query writes.  Called at microblock end via
%% aec_trees:flush_state_batches/1 and defensively by every whole-tree
%% reader/iterator/prune entry point (so no non-funnel path can observe
%% a stale otree).  O(1) fast-path when empty.
-spec flush_oracle_batch(tree()) -> tree().
flush_oracle_batch(#oracle_tree{oracle_batch = B} = Tree) when map_size(B) =:= 0 ->
    Tree;
flush_oracle_batch(#oracle_tree{oracle_batch = B} = Tree) ->
    Tree1 = Tree#oracle_tree{oracle_batch = #{}},
    maps:fold(fun(_K, {oracle, How, O}, Acc) -> do_add_oracle(How, O, Acc);
                 (_K, {query,  How, I}, Acc) -> do_add_query(How, I, Acc)
              end, Tree1, B).

int_prune(Height, Trees, TxEnv) ->
    OTree = #oracle_tree{ cache = Cache } = aec_trees:oracles(Trees),
    ATree = aec_trees:accounts(Trees),
    {OTree1, ATree1, TxEnv1} = int_prune(cache_safe_peek(Cache), Height, OTree, ATree, TxEnv),
    {aec_trees:set_accounts(aec_trees:set_oracles(Trees, OTree1), ATree1), TxEnv1}.

int_prune(none, _Height, OTree, ATree, TxEnv) ->
    {OTree, ATree, TxEnv};
int_prune({Height, Id}, Height, #oracle_tree{ cache = Cache } = OTree, ATree, TxEnv) ->
    {{Height, Id}, Cache1} = cache_pop(Cache),
    {OTree1, ATree1, TxEnv1} = delete(Id, Height, OTree#oracle_tree{ cache = Cache1 }, ATree, TxEnv),
    int_prune(cache_safe_peek(Cache1), Height, OTree1, ATree1, TxEnv1);
int_prune({Height1,_Id}, Height2, OTree, ATree, TxEnv) when Height2 < Height1 ->
    {OTree, ATree, TxEnv}.

delete({oracle, Id}, H, OTree, ATree, TxEnv) ->
    {OTree1, ATree1} =
        %% If oracle was extended the cache might be stale
        case oracle_expired(Id, H, OTree) of
            true ->
                TreeIds = find_oracle_query_tree_ids(Id, OTree),
                OTree0 = aeu_mtrees:delete(Id, OTree#oracle_tree.otree),
                int_delete(TreeIds, {OTree0, ATree});
            false ->
                {OTree#oracle_tree.otree, ATree}
        end,
    {OTree#oracle_tree{otree = OTree1}, ATree1, TxEnv};
delete({query, OracleId, Id}, H, OTree, ATree, TxEnv) ->
    {OTree1, ATree1, TxEnv1} =
        %% When responded to we get a stale cache entry
        case oracle_query_expired(OracleId, Id, H, OTree) of
            true ->
                TreeId = <<OracleId/binary, Id/binary>>,
                int_delete_query(TreeId, {OTree#oracle_tree.otree, ATree}, TxEnv);
            false ->
                {OTree#oracle_tree.otree, ATree, TxEnv}
        end,
    {OTree#oracle_tree{otree = OTree1}, ATree1, TxEnv1}.

oracle_expired(Id, H, OTree) ->
    case lookup_oracle(Id, OTree) of
        {value, O} -> H >= aeo_oracles:ttl(O);
        none       -> false
    end.

oracle_query_expired(OracleId, Id, H, OTree) ->
    case lookup_query(OracleId, Id, OTree) of
        {value, Q} -> H >= aeo_query:ttl(Q);
        none       -> false
    end.

int_delete([Id|Left], Trees) ->
    int_delete(Left, int_delete_query(Id, Trees));
int_delete([], Trees) ->
    Trees.

int_delete_query(Id, Trees) ->
    {OTree, ATree, _} = int_delete_query(Id, Trees, undefined),
    {OTree, ATree}.

int_delete_query(Id, {OTree, ATree}, TxEnv) ->
    {ATree1, TxEnv1} =
        case aeu_mtrees:lookup(Id, OTree) of
            {value, Val} ->
                <<_:?PUB_SIZE/unit:8, QId/binary>> = Id,
                Q = aeo_query:deserialize(QId, Val),
                oracle_refund(Q, ATree, TxEnv);
            none ->
                {ATree, TxEnv}
        end,
    {aeu_mtrees:delete(Id, OTree), ATree1, TxEnv1}.

oracle_refund(Q, ATree, TxEnv) ->
    case aeo_query:is_closed(Q) of
        false ->
            PubKey = aeo_query:sender_pubkey(Q),
            case aec_accounts_trees:lookup(PubKey, ATree) of
                {value, Account} ->
                    Fee = aeo_query:fee(Q),
                    {ok, Account1} = aec_accounts:earn(Account, Fee),
                    TxEnv1 = oracle_refund_tx_event(PubKey, Fee, TxEnv),
                    {aec_accounts_trees:enter(Account1, ATree), TxEnv1};
                none ->
                    lager:error("Account ~p could not be found for refunding oracle query ~p",
                                [aeo_query:sender_pubkey(Q), aeo_query:id(Q)]),
                    error({account_disappeared, aeo_query:sender_pubkey(Q)})
            end;
        true ->
            {ATree, TxEnv}
    end.

oracle_refund_tx_event(_PubKey, _Fee, undefined) ->
    undefined;
oracle_refund_tx_event(PubKey, Fee, TxEnv) ->
    aetx_env:tx_event(delta, {PubKey, Fee}, <<"Oracle.refund">>, TxEnv).

%%%===================================================================
%%% Iterator for finding all oracle queries
%%%===================================================================

find_oracle_query_tree_ids(OracleId, Tree) ->
    find_oracle_query_ids(OracleId, Tree, tree).

find_oracle_query_ids(OracleId, Tree) ->
    find_oracle_query_ids(OracleId, Tree, id).

find_oracle_queries(OracleId, FromQueryId, QueryType, Max, #oracle_tree{otree = T}) ->
    IteratorKey = case FromQueryId of
                      '$first' -> OracleId;
                      _        -> <<OracleId/binary, FromQueryId/binary>>
                  end,
    Iterator = aeu_mtrees:iterator_from(IteratorKey, T),
    find_oracle_queries(Iterator, QueryType, Max, []).

find_oracle_queries(Iterator, QueryType, N, Acc) when N > 0 ->
    case aeu_mtrees:iterator_next(Iterator) of
        {Key, Value, NextIterator} when byte_size(Key) > ?PUB_SIZE ->
            <<_:?PUB_SIZE/unit:8, QId/binary>> = Key,
            Query = aeo_query:deserialize(QId, Value),
            case {QueryType, aeo_query:is_open(Query)} of
                {open, true} ->
                    find_oracle_queries(NextIterator, QueryType, N - 1, [Query | Acc]);
                {open, false} ->
                    find_oracle_queries(NextIterator, QueryType, N, Acc);
                {closed, true} ->
                    find_oracle_queries(NextIterator, QueryType, N, Acc);
                {closed, false} ->
                    find_oracle_queries(NextIterator, QueryType, N - 1, [Query | Acc]);
                {all, _} ->
                    find_oracle_queries(NextIterator, QueryType, N - 1, [Query | Acc])
            end;
        %% Either end_of_table or next Oracle
        _Other ->
            lists:reverse(Acc)
    end;
find_oracle_queries(_Iterator, _QueryType, 0, Acc) ->
    lists:reverse(Acc).

find_oracles(FromOracleId, Max, #oracle_tree{otree = T}) ->
    %% Only allow paths that match the size of an OracleId - Queries have
    %% a longer path.
    IterOpts = [{max_path_length, ?PUB_SIZE*2}],
    Iterator =
        case FromOracleId of
            '$first' -> aeu_mtrees:iterator(T, IterOpts);
            _        -> aeu_mtrees:iterator_from(FromOracleId, T, IterOpts)
        end,
    find_oracles(Iterator, Max).

find_oracles(_Iterator, 0) -> [];
find_oracles(Iterator, N) ->
    case aeu_mtrees:iterator_next(Iterator) of
        '$end_of_table' -> [];
        {Key, Value, NextIterator} ->
            [aeo_oracles:deserialize(Key, Value) | find_oracles(NextIterator, N-1)]
    end.

find_oracle_query_ids(OracleId, #oracle_tree{otree = T}, Type) ->
    Iterator = aeu_mtrees:iterator_from(OracleId, T),
    Next = aeu_mtrees:iterator_next(Iterator),
    find_oracle_query_ids(OracleId, Next, Type, []).

find_oracle_query_ids(_OracleId, '$end_of_table',_Type, Acc) ->
    Acc;
find_oracle_query_ids(OracleId, {Key,_Val, Iter}, Type, Acc) ->
    S = byte_size(OracleId),
    case Key of
        <<OracleId:S/binary, Id/binary>> ->
            NewAcc = case Type of
                         tree -> [Key|Acc];
                         id   -> [Id|Acc]
                     end,
            Next = aeu_mtrees:iterator_next(Iter),
            find_oracle_query_ids(OracleId, Next, Type, NewAcc);
        _ ->
            Acc
    end.

%%%===================================================================
%%% TTL Cache
%%%===================================================================
-define(DUMMY_VAL, <<0>>).

cache_push(Id, TTL, C) ->
    SExt = sext:encode({TTL, Id}),
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

-spec to_binary_without_backend(tree()) -> binary().
to_binary_without_backend(Tree) ->
    #oracle_tree{otree = OTree} = flush_oracle_batch(Tree),
    OTBin = aeu_mtrees:serialize(OTree),
    aeser_chain_objects:serialize(
        oracles_mtree,
        ?VSN,
        serialization_template(?VSN),
        [{otree, OTBin}]).

-spec from_binary_without_backend(binary()) -> tree().
from_binary_without_backend(Bin) ->
    [{otree, OTBin}] =
        aeser_chain_objects:deserialize(oracles_mtree, ?VSN,
                                             serialization_template(?VSN), Bin),
    OTree = aeu_mtrees:deserialize(OTBin),
    Cache = create_cache_from_mtree(OTree, aeu_mtrees:empty()),
    #oracle_tree{otree = OTree,
                 cache = Cache}.

serialization_template(?VSN) ->
    [{otree, binary}].

create_cache_from_mtree(MTree, EmptyCache) ->
    create_cache_from_mtree_(aeu_mtrees:iterator_next(
                               aeu_mtrees:iterator(MTree)), EmptyCache).

create_cache_from_mtree_('$end_of_table', Cache) -> Cache;
create_cache_from_mtree_({Key, Val, Iter}, Cache0) ->
    {Module, Obj} = deserialize_value(Key, Val),
    TTL = Module:ttl(Obj),
    Cache = cache_push(TTL, Key, Cache0),
    create_cache_from_mtree_(aeu_mtrees:iterator_next(Iter), Cache).

deserialize_value(Hash, Bin) ->
    {Type, Vsn, RawFields} =
        aeser_chain_objects:deserialize_type_and_vsn(Bin),
    Oracle = aeo_oracles:serialization_type(),
    Query = aeo_query:serialization_type(),
    Module =
        case Type of
            Oracle -> aeo_oracles;
            Query  -> aeo_query
        end,
    Template = Module:serialization_template(Vsn),
    Fields = aeserialization:decode_fields(Template, RawFields),
    Obj = Module:deserialize_from_fields(Vsn, Hash, Fields),
    {Module, Obj}.
