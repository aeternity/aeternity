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
        , lookup_query/3
        , lookup_oracle/2
        , new_with_backend/2
        , prune/2
        , root_hash/1
        ]).

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

-record(oracle_tree, { otree  = aeu_mtrees:empty() :: otree()
                     , cache  = aeu_mtrees:empty() :: cache()
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

-spec new_with_backend(aeu_mtrees:root_hash() | 'empty',
                       aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_backend(RootHash, CacheRootHash) ->
    OTree  = aeu_mtrees:new_with_backend(RootHash, aec_db_backends:oracles_backend()),
    Cache  = aeu_mtrees:new_with_backend(CacheRootHash, aec_db_backends:oracles_cache_backend()),
    #oracle_tree{ otree  = OTree
                , cache  = Cache
                }.

-spec prune(block_height(), aec_trees:trees()) -> aec_trees:trees().
prune(Height, Trees) ->
    %% Oracle information should be around for the expiry block
    %% since we prune before the block, use Height - 1 for pruning.
    int_prune(Height - 1, Trees).

-spec enter_query(query(), tree()) -> tree().
enter_query(I, Tree) ->
    add_query(enter, I, Tree).

-spec insert_query(query(), tree()) -> tree().
insert_query(I, Tree) ->
    add_query(insert, I, Tree).

-spec get_query(aeo_oracles:pubkey(), aeo_query:id(), tree()) -> query().
get_query(OracleId, Id, Tree) ->
    TreeId = <<OracleId/binary, Id/binary>>,
    Serialized = aeu_mtrees:get(TreeId, Tree#oracle_tree.otree),
    aeo_query:deserialize(Serialized).

-spec lookup_query(aeo_oracles:pubkey(), aeo_query:id(), tree()) ->
    {'value', query()} | none.
lookup_query(OracleId, Id, Tree) ->
    TreeId = <<OracleId/binary, Id/binary>>,
    case aeu_mtrees:lookup(TreeId, Tree#oracle_tree.otree) of
        {value, Val} -> {value, aeo_query:deserialize(Val)};
        none -> none
    end.

-spec enter_oracle(oracle(), tree()) -> tree().
enter_oracle(O, Tree) ->
    add_oracle(enter, O, Tree).

-spec insert_oracle(oracle(), tree()) -> tree().
insert_oracle(O, Tree) ->
    add_oracle(insert, O, Tree).

-spec get_oracle(binary(), tree()) -> oracle().
get_oracle(Id, Tree) ->
    aeo_oracles:deserialize(Id, aeu_mtrees:get(Id, Tree#oracle_tree.otree)).

-spec get_oracle_query_ids(binary(), tree()) -> [aeo_query:id()].
get_oracle_query_ids(Id, Tree) ->
    find_oracle_query_ids(Id, Tree).

-spec get_oracle_queries(aeo_oracles:pubkey(), binary() | '$first', open | closed | all,
                         non_neg_integer(), tree()) -> list(query()).
get_oracle_queries(OracleId, From, QueryType, Max, Tree) ->
    find_oracle_queries(OracleId, From, QueryType, Max, Tree).

-spec get_oracles(binary() | '$first', non_neg_integer(), tree()) -> list(oracle()).
get_oracles(From, Max, Tree) ->
    find_oracles(From, Max, Tree).

-spec lookup_oracle(binary(), tree()) -> {'value', oracle()} | 'none'.
lookup_oracle(Id, Tree) ->
    case aeu_mtrees:lookup(Id, Tree#oracle_tree.otree) of
        {value, Val}  -> {value, aeo_oracles:deserialize(Id, Val)};
        none -> none
    end.

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#oracle_tree{otree = OTree}) ->
    aeu_mtrees:root_hash(OTree).

-spec cache_root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
cache_root_hash(#oracle_tree{cache = CTree}) ->
    aeu_mtrees:root_hash(CTree).

-ifdef(TEST).
-spec oracle_list(tree()) -> list(oracle()).
oracle_list(#oracle_tree{otree = OTree}) ->
    [ aeo_oracles:deserialize(Key, Val)
      || {Key, Val} <- aeu_mtrees:to_list(OTree),
         byte_size(Key) =:= ?PUB_SIZE
    ].

-spec query_list(tree()) -> list(query()).
query_list(#oracle_tree{otree = OTree}) ->
    [ aeo_query:deserialize(Val)
      || {Key, Val} <- aeu_mtrees:to_list(OTree),
         byte_size(Key) > ?PUB_SIZE
    ].
-endif.

-spec commit_to_db(tree()) -> tree().
commit_to_db(#oracle_tree{otree = OTree, cache = Cache} = Tree) ->
    Tree#oracle_tree{otree = aeu_mtrees:commit_to_db(OTree),
                     cache = aeu_mtrees:commit_to_db(Cache)
                    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_oracle(How, O, #oracle_tree{ otree = OTree } = Tree) ->
    Pubkey = aeo_oracles:pubkey(O),
    Serialized = aeo_oracles:serialize(O),
    Expires = aeo_oracles:expires(O),

    OTree1 = case How of
                enter  -> aeu_mtrees:enter(Pubkey, Serialized, OTree);
                insert -> aeu_mtrees:insert(Pubkey, Serialized, OTree)
            end,
    Cache  = cache_push({oracle, Pubkey}, Expires, Tree#oracle_tree.cache),
    Tree#oracle_tree{ otree  = OTree1
                    , cache  = Cache
                    }.

add_query(How, I, #oracle_tree{otree = OTree} = Tree) ->
    OraclePubkey = aeo_query:oracle_pubkey(I),
    QueryId      = aeo_query:id(I),
    TreeId       = <<OraclePubkey/binary, QueryId/binary>>,
    SerializedI  = aeo_query:serialize(I),
    Expires      = aeo_query:expires(I),
    OTree1       = case How of
                       enter  -> aeu_mtrees:enter(TreeId, SerializedI, OTree);
                       insert -> aeu_mtrees:insert(TreeId, SerializedI, OTree)
                  end,
    Cache  = cache_push({query, OraclePubkey, QueryId}, Expires, Tree#oracle_tree.cache),
    Tree#oracle_tree{ otree  = OTree1
                    , cache  = Cache
                    }.

int_prune(Height, Trees) ->
    OTree = #oracle_tree{ cache = Cache } = aec_trees:oracles(Trees),
    ATree = aec_trees:accounts(Trees),
    {OTree1, ATree1} = int_prune(cache_safe_peek(Cache), Height, OTree, ATree),
    aec_trees:set_accounts(aec_trees:set_oracles(Trees, OTree1), ATree1).

int_prune(none, _Height, OTree, ATree) ->
    {OTree, ATree};
int_prune({Height, Id}, Height, #oracle_tree{ cache = Cache } = OTree, ATree) ->
    {{Height, Id}, Cache1} = cache_pop(Cache),
    {OTree1, ATree1} = delete(Id, Height, OTree#oracle_tree{ cache = Cache1 }, ATree),
    int_prune(cache_safe_peek(Cache1), Height, OTree1, ATree1);
int_prune({Height1,_Id}, Height2, OTree, ATree) when Height2 < Height1 ->
    {OTree, ATree}.

delete({oracle, Id}, H, OTree, ATree) ->
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
    {OTree#oracle_tree{otree = OTree1}, ATree1};
delete({query, OracleId, Id}, H, OTree, ATree) ->
    {OTree1, ATree1} =
        %% When responded to we get a stale cache entry
        case oracle_query_expired(OracleId, Id, H, OTree) of
            true ->
                TreeId = <<OracleId/binary, Id/binary>>,
                int_delete_query(TreeId, {OTree#oracle_tree.otree, ATree});
            false ->
                {OTree#oracle_tree.otree, ATree}
        end,
    {OTree#oracle_tree{otree = OTree1}, ATree1}.

oracle_expired(Id, H, OTree) ->
    case lookup_oracle(Id, OTree) of
        {value, O} -> H >= aeo_oracles:expires(O);
        none       -> false
    end.

oracle_query_expired(OracleId, Id, H, OTree) ->
    case lookup_query(OracleId, Id, OTree) of
        {value, Q} -> H >= aeo_query:expires(Q);
        none       -> false
    end.

int_delete([Id|Left], Trees) ->
    int_delete(Left, int_delete_query(Id, Trees));
int_delete([], Trees) ->
    Trees.

int_delete_query(Id, {OTree, ATree}) ->
    ATree1 =
        case aeu_mtrees:lookup(Id, OTree) of
            {value, Val} ->
                Q = aeo_query:deserialize(Val),
                oracle_refund(Q, ATree);
            none ->
                ATree
        end,
    {aeu_mtrees:delete(Id, OTree), ATree1}.

oracle_refund(Q, ATree) ->
    case aeo_query:is_closed(Q) of
        false ->
            case aec_accounts_trees:lookup(aeo_query:sender_pubkey(Q), ATree) of
                {value, Account} ->
                    {ok, Account1} = aec_accounts:earn(Account, aeo_query:fee(Q)),
                    aec_accounts_trees:enter(Account1, ATree);
                none ->
                    lager:error("Account ~p could not be found for refunding oracle query ~p",
                                [aeo_query:sender_pubkey(Q), aeo_query:id(Q)]),
                    error({account_disappeared, aeo_query:sender_pubkey(Q)})
            end;
        true ->
            ATree
    end.

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
            Query = aeo_query:deserialize(Value),
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

cache_push(Id, Expires, C) ->
    SExt = sext:encode({Expires, Id}),
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
