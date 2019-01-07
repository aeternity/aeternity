%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Merkle Patricia Trees
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(aeu_mp_trees).

-export([ new/0
        , new/1
        , new/2
        , commit_reachable_to_db/1
        , construct_proof/3
        , db/1
        , delete/2
        , get/2
        , has_node/3
        , iterator/1
        , iterator/2
        , iterator_from/2
        , iterator_from/3
        , iterator_next/1
        , lookup_in_proof/3
        , pp/1
        , put/3
        , root_hash/1
        , read_only_subtree/2
        , unfold/3
        , verify_proof/4
        , visit_reachable_hashes/3
        , visit_reachable_hashes/4
        ]).

%% For internal functional db
-export([ dict_db_drop_cache/1
        , dict_db_get/2
        , dict_db_put/3
        ]).

-export_type([ tree/0
             , iterator/0
             , iterator_opts/0
             , key/0
             , path/0
             , value/0
             , unfold_leaf/0
             , unfold_node/0
             ]).

-record(mpt, { hash = <<>>          :: <<>> | hash() %% <<>> for the empty tree
             , db   = new_dict_db() :: aeu_mp_trees_db:db()
             }).

-record(iter, { key  = <<>>          :: <<>> | key()
              , root = <<>>          :: <<>> | hash()
              , max_length           :: pos_integer() | 'undefined'
              , with_prefix = <<>>   :: <<>> | key()
              , db   = new_dict_db() :: aeu_mp_trees_db:db()
              }).

-opaque tree() :: #mpt{}.
-opaque iterator() :: #iter{}.

-type iterator_opts() :: [ {'max_path_length', pos_integer()}
                         | {'with_prefix', key()}].

-type tree_node() :: null() | leaf() | extension() | branch().

-type null()         :: <<>>.
-type leaf()         :: {'leaf', path(), value()}.
-type extension()    :: {'ext', path(), hash()}.
-type branch()       :: {'branch', branch_tuple()}.
-type branch_tuple() :: {enc_node(), enc_node(), enc_node(), enc_node(),
                         enc_node(), enc_node(), enc_node(), enc_node(),
                         enc_node(), enc_node(), enc_node(), enc_node(),
                         enc_node(), enc_node(), enc_node(), enc_node(),
                         value()}.

-type enc_node()       :: null() | short_rlp_node() | hash().
-type short_rlp_node() :: aeu_rlp:encoded(). %% size `< 32 bytes'

-type raw_node()      :: null() | raw_leaf() | raw_extension() | raw_branch().
-type raw_leaf()      :: [encoded_path()|value()]. %% [encoded_path(), value()].
-type raw_extension() :: [encoded_path()|hash()].  %% [encoded_path(), hash()].
-type raw_branch()    :: [enc_node()|value()].     %% [16x enc_node() + value()]

-type path()         :: <<>> | key(). %% Hex string.
-type encoded_path() :: binary().  %% Compact encoding of hex sequence.

-type key()          :: <<_:4,_:_*4>>. %% Non-empty hexstring
-type value()        :: aeu_rlp:encodable().
-type hash()         :: <<_:256>>.

-type db()           :: aeu_mp_trees_db:db().

-type unfold_leaf() :: {leaf, path()}.
-type unfold_node() :: {node, path(), enc_node()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> tree().
new() ->
    #mpt{}.

-spec new(db()) -> tree().
new(DB) ->
    #mpt{db = DB}.

-spec new(hash(), db()) -> tree().
new(RootHash, DB) ->
    %% Assert that at least the root hash is present in the db.
    _ = db_get(RootHash, DB),
    #mpt{ hash = RootHash
        , db   = DB
        }.

-spec db(tree()) -> db().
db(#mpt{ db = DB}) ->
    DB.

-spec read_only_subtree(key() | <<>>, tree()) -> {ok, tree()} | {error, no_such_subtree}.
%% @doc Returns the subtree of a given key. Note that the key needs to be
%%      stored in the tree, i.e., this will fail even if there are other
%%      keys that have the given key as prefix if there isn't a node
%%      for the given key.
%%      Only use this for lookups, not for storing values.
read_only_subtree(Key, #mpt{hash = Hash, db = DB} = MPT) ->
    case int_get_subtree(Key, decode_node(Hash, DB), DB) of
        {ok, {<<>>, _}} ->
            {ok, new()};
        {ok, {Node, DB1}} ->
            {NewHash, DB2} = force_encoded_node_to_hash(Node, DB1),
            {ok, MPT#mpt{hash = NewHash, db = DB2}};
        {error, no_such_subtree} = Err ->
            Err
    end.

-spec get(bitstring(), tree()) -> value() | <<>>.
get(Key, #mpt{hash = Hash, db = DB}) ->
    int_get(Key, decode_node(Hash, DB), DB).

-spec put(key(), value() | <<>>, tree()) -> tree().
%% @doc Note that putting `<<>>' as value is equivalent to deleting the value.
put(Key, <<>>, #mpt{} = Mpt) when is_bitstring(Key), Key =/= <<>> ->
    delete(Key, Mpt);
put(Key, Val, #mpt{} = Mpt) when is_bitstring(Key), Key =/= <<>> ->
    #mpt{hash = Hash, db = DB} = Mpt,
    assert_val(Val),
    try int_put(Key, Val, decode_node(Hash, DB), DB) of
        {NewTree, DB1} ->
            {NewHash, DB2} = force_encoded_node_to_hash(NewTree, DB1),
            Mpt#mpt{ hash = NewHash, db = DB2}
    catch throw:unchanged -> Mpt
    end.

-spec delete(key(), tree()) -> tree().
delete(Key, #mpt{} = Mpt) when is_bitstring(Key) ->
    #mpt{hash = Hash, db = DB} = Mpt,
    try int_delete(Key, decode_node(Hash, DB), DB) of
        {<<>>, DB1} ->
            Mpt#mpt{hash = <<>>, db   = DB1};
        {NewTree, DB1} ->
            {NewHash, DB2} = force_encoded_node_to_hash(NewTree, DB1),
            Mpt#mpt{ hash = NewHash, db = DB2}
    catch throw:unchanged -> Mpt
    end.

-spec root_hash(tree()) -> <<>> | hash().
root_hash(#mpt{hash = H}) -> H.

-spec commit_reachable_to_db(tree()) -> tree().
commit_reachable_to_db(#mpt{db = DB, hash = Hash} = MPT) ->
    VisitFun = fun(Key, Val, AccDB) ->
                       {continue, db_commit_from_cache(Key, Val, AccDB)}
               end,
    DB1 = int_visit_reachable_hashes_in_cache([Hash], DB, DB, VisitFun),
    MPT#mpt{db = db_drop_cache(DB1)}.

-spec construct_proof(key(), db(), tree()) -> {value(), db()}.
construct_proof(Key, ProofDB, #mpt{db = DB, hash = Hash}) ->
    ProofDB1 = proof_db_insert(Hash, DB, ProofDB),
    int_c_proof(Key, decode_node(Hash, DB), DB, ProofDB1).

-type verify_error() :: 'bad_proof'
                      | {'bad_value', term()}
                      | {'bad_hash', hash()}.

-spec verify_proof(key(), value(), hash(), db()) -> 'ok'
                                                  | verify_error().
verify_proof(Key, Val, Hash, ProofDB) ->
    try decode_node_and_check_hash(Hash, ProofDB) of
        {ok, Node} -> int_verify_proof(Key, Node, Val, ProofDB);
        {bad_hash, H} -> {bad_hash, H}
    catch
        _:_ -> bad_proof
    end.

-spec lookup_in_proof(key(), hash(), db()) -> 'none' | {'value', value()}.
lookup_in_proof(Key, Hash, ProofDB) ->
    try decode_node_and_check_hash(Hash, ProofDB) of
        {ok, Node} ->
            case int_verify_proof(Key, Node, lookup, ProofDB) of
                bad_proof         -> none;
                {bad_hash, _}     -> none;
                {bad_value, <<>>} -> none;
                {bad_value, Val}  -> {value, Val}
            end;
        {bad_hash,_H} -> none
    catch
        _:_ -> none
    end.

-spec iterator(tree()) -> iterator().
iterator(#mpt{hash = Hash, db = DB}) ->
    #iter{key = <<>>, root = Hash, db = DB}.

-spec iterator(tree(), iterator_opts()) -> iterator().
iterator(#mpt{hash = Hash, db = DB}, Opts) ->
    process_iterator_opts(#iter{key = <<>>, root = Hash, db = DB}, Opts).

%%% @doc Iterator from a key. Key doesn't need to exist. Calling
%%% iterator_next/1 gives the next value after Key.
-spec iterator_from(key(), tree()) -> iterator().
iterator_from(Key, #mpt{hash = Hash, db = DB}) ->
    #iter{key = Key, root = Hash, db = DB}.

-spec iterator_from(key(), tree(), iterator_opts()) -> iterator().
iterator_from(Key, #mpt{hash = Hash, db = DB}, Opts) ->
    process_iterator_opts(#iter{key = Key, root = Hash, db = DB}, Opts).

-spec iterator_next(iterator()) ->
                           {key(), value(), iterator()} | '$end_of_table'.
iterator_next(#iter{key = Key, root = Hash, db = DB,
                    max_length = M, with_prefix = Prefix} = Iter) ->
    Res =
        case Key =:= <<>> of
            true  -> pick_first(decode_node(Hash, DB), M, Prefix, DB);
            false -> int_iter_next(Key, decode_node(Hash, DB), M, Prefix, DB)
        end,
    case Res of
        '$end_of_table' -> '$end_of_table';
        {Key1, Val} -> {Key1, Val, Iter#iter{key = Key1}}
    end.

-spec unfold(path(), enc_node(), tree()) -> [unfold_node() | unfold_leaf()].
unfold(<<>>, NodeHash, #mpt{ hash = NodeHash }) ->
    [];
unfold(<<>>, _, #mpt{ hash = Hash, db = DB }) ->
    int_unfold(<<>>, decode_node(Hash, DB), DB);
unfold(PrefixPath, Node, #mpt{ db = DB }) ->
    int_unfold(PrefixPath, decode_node(Node, DB), DB).

-spec has_node(path(), enc_node(), tree()) -> yes | no | maybe.
has_node(Path, Node, T = #mpt{ hash = Root, db = DB }) ->
    try decode_node(Node, DB) of
        {'leaf', NodePath, Val} ->
            case get(<<Path/bits, NodePath/bits>>, T) of
                Val -> yes;
                _   -> no
            end;
        _ ->
            int_has_node(Path, Node, decode_node(Root, DB), DB)
    catch _:_ ->
        int_has_node(Path, Node, decode_node(Root, DB), DB)
    end.

-type visit_fun() :: fun((hash(), SerializedNode :: binary(), Acc :: term()) ->
                                'stop'
                             | {'continue', NewAcc :: term()}).

-spec visit_reachable_hashes(tree(), InitAcc :: term(), visit_fun()) ->
                                    Acc :: term().
%% @doc Equivalent to visit_reachable_hashes/4 from the current root hash.
%% @end
visit_reachable_hashes(#mpt{hash = Hash} = Tree, InitAcc, VisitFun) ->
    visit_reachable_hashes(Tree, [Hash], InitAcc, VisitFun).

-spec visit_reachable_hashes(tree(), [hash()], InitAcc :: term(), visit_fun())->
                                    Acc :: term().
%% @doc Visits all hashes reachable from a list of root hashes, using
%%      the VisitFun and the initial accumulator InitAcc.
%%      Note that not all nodes are visited, only those stored in the DB
%%      as hashes. Useful for implementing a GC.
%% @end
visit_reachable_hashes(#mpt{db = DB}, [_|_] = RootHashes, InitAcc, VisitFun) ->
    int_visit_reachable_hashes_both(RootHashes, DB, InitAcc, VisitFun).

-spec pp(tree()) -> 'ok'.
pp(#mpt{hash = Hash, db = DB}) ->
    io:format("Root: ~s\n", [hexstring(Hash)]),
    [io:format("~s\n", [S])
     || S <- lists:flatten([pp_tree(decode_node(Hash, DB), DB)])],
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_val(Val) ->
    aeu_rlp:encode(Val),
    ok.

%%%===================================================================
%%% Get

-spec int_get(path(), tree_node(), db()) -> value() | <<>>.

int_get(_Path, <<>>, _DB) ->
    <<>>;
int_get(<<>>, {branch, Branch},_DB) when tuple_size(Branch) =:= 17 ->
    branch_value(Branch);
int_get(Path, {branch, Branch}, DB) when tuple_size(Branch) =:= 17 ->
    <<Next:4, Rest/bits>> = Path,
    NextNode = decode_node(branch_next(Next, Branch), DB),
    int_get(Rest, NextNode, DB);
int_get(Path, {Type, NodePath, NodeVal}, DB) when Type =:= ext; Type =:= leaf ->
    S = bit_size(NodePath),
    case Path of
        NodePath when Type =:= leaf ->
            NodeVal;
        <<NodePath:S/bits, _/bits>> when Type =:= leaf ->
            <<>>;
        <<NodePath:S/bits, Rest/bits>> when Type =:= ext ->
            int_get(Rest, decode_node(NodeVal, DB), DB);
        _ ->
            <<>>
    end.

int_get_subtree(<<>>, <<>>, DB) ->
    {ok, {<<>>, DB}};
int_get_subtree(_Path, <<>>,_DB) ->
    {error, no_such_subtree};
int_get_subtree(<<>>, {branch, Branch}, DB) when tuple_size(Branch) =:= 17 ->
    {ok, branch(set_branch_value(Branch, <<>>), DB)};
int_get_subtree(Path, {branch, Branch}, DB) when tuple_size(Branch) =:= 17 ->
    <<Next:4, Rest/bits>> = Path,
    NextNode = decode_node(branch_next(Next, Branch), DB),
    int_get_subtree(Rest, NextNode, DB);
int_get_subtree(Path, {Type, NodePath, NodeVal}, DB) when Type =:= ext; Type =:= leaf ->
    S = bit_size(NodePath),
    case Path of
        NodePath when Type =:= leaf ->
            {ok, {<<>>, DB}};
        <<NodePath:S/bits, _/bits>> when Type =:= leaf ->
            {error, no_such_subtree};
        <<NodePath:S/bits, Rest/bits>> when Type =:= ext ->
            int_get_subtree(Rest, decode_node(NodeVal, DB), DB);
        _ ->
            {error, no_such_subtree}
    end.


%%%===================================================================
%%% Put

-spec int_put(path(), value(), tree_node(), db()) -> {enc_node(), db()}.
%% throws 'unchanged' if no action is required
int_put(Key, Val, <<>>, DB) ->
    leaf(Key, Val, DB);
int_put(Key1, Val1, {leaf, Key2, Val2}, DB) ->
    {Common, K1, K2} = find_common_path(Key1, Key2),
    case {K1, K2} of
        {<<>>, <<>>} when Val1 =:= Val2 ->
            %% The same key and the same value.
            throw(unchanged);
        {<<>>, <<>>} ->
            %% The same key. Just update the value.
            leaf(Key1, Val1, DB);
        {<<>>, <<K:4, Rest/bits>>} ->
            %% We need a branch before the old leaf
            {L, DB1} = leaf(Rest, Val2, DB),
            {Branch, DB2} = branch([{K, L}], Val1, DB1),
            maybe_extension(Common, Branch, DB2);
        {<<K:4, Rest/bits>>, <<>>} ->
            %% We need a branch before the new leaf
            {L, DB1} = leaf(Rest, Val1, DB),
            {Branch, DB2} = branch([{K, L}], Val2, DB1),
            maybe_extension(Common, Branch, DB2);
        {<<Next1:4, Rest1/bits>>, <<Next2:4, Rest2/bits>>} ->
            %% We need a branch before both leaves
            {L1, DB1} = leaf(Rest1, Val1, DB),
            {L2, DB2} = leaf(Rest2, Val2, DB1),
            {Branch, DB3} = branch([{Next1, L1}, {Next2, L2}], <<>>, DB2),
            maybe_extension(Common, Branch, DB3)
    end;
int_put(<<>> =_Key, Val, {branch, Branch}, DB) ->
    %% Update the value of the branch
    case branch_value(Branch) =:= Val of
        true  -> throw(unchanged);
        false -> branch(set_branch_value(Branch, Val), DB)
    end;
int_put(<<Next:4, Rest/bits>>, Val, {branch, Branch}, DB) ->
    NextNode = decode_node(branch_next(Next, Branch), DB),
    {NewNode, DB1} = int_put(Rest, Val, NextNode, DB),
    Branch1 = set_branch_next(Next, Branch, NewNode),
    branch(Branch1, DB1);
int_put(Key1, Val, {ext, Key2, Hash} = Ext, DB) ->
    {Common, K1, K2} = find_common_path(Key1, Key2),
    case {K1, K2} of
        {<<>>, <<>>} ->
            %% The same preamble
            {NewNode, DB1} = int_put(<<>>, Val, decode_node(Hash, DB), DB),
            extension(Common, NewNode, DB1);
        {_, <<>>} ->
            %% The whole extension is passed
            {NewNode, DB1} = int_put(K1, Val, decode_node(Hash, DB), DB),
            extension(Common, NewNode, DB1);
        {<<>>, <<Next2:4, Rest2/bits>>} ->
            %% We need to branch out
            {NewNode2, DB1} = update_extension_path(Ext, Rest2, DB),
            {Branch, DB2} = branch([{Next2, NewNode2}], Val, DB1),
            maybe_extension(Common, Branch, DB2);
        {<<Next1:4, Rest1/bits>>, <<Next2:4, Rest2/bits>>} ->
            {NewNode1, DB1} = leaf(Rest1, Val, DB),
            {NewNode2, DB2} = update_extension_path(Ext, Rest2, DB1),
            BranchList = [{Next1, NewNode1}, {Next2, NewNode2}],
            {Branch, DB3} = branch(BranchList, <<>>, DB2),
            maybe_extension(Common, Branch, DB3)
    end.

find_common_path(B1, B2) ->
    Offset = find_common_path_1(B1, B2),
    <<Common:Offset/bits, Left1/bits>> = B1,
    <<_     :Offset/bits, Left2/bits>> = B2,
    {Common, Left1, Left2}.

find_common_path_1(<<X:4, Rest1/bits>>,
                   <<X:4, Rest2/bits>>) ->
    4 + find_common_path_1(Rest1, Rest2);
find_common_path_1(_, _) ->
    0.

%%%===================================================================
%%% Delete

-spec int_delete(path(), tree_node(), db()) -> {enc_node(), db()}.
%% throws 'unchanged' if no action is required
int_delete(_Key, <<>>,_DB) ->
    throw(unchanged);
int_delete(Key1, {leaf, Key2, _} = _Node, DB) ->
    case Key1 =:= Key2 of
        true  -> {<<>>, DB};
        false -> throw(unchanged)
    end;
int_delete(Key1, {ext, Key2, Hash} = _Node, DB) ->
    S = bit_size(Key2),
    case Key1 of
        <<Key2:S/bits, Rest/bits>> ->
            {NewNode, DB1} = int_delete(Rest, decode_node(Hash, DB), DB),
            case decode_node(NewNode, DB1) of
                {leaf, Path, Val} ->
                    leaf(<<Key2/bits, Path/bits>>, Val, DB1);
                {ext, Path, Hash1} ->
                    extension(<<Key2/bits, Path/bits>>, Hash1, DB1);
                {branch, _} ->
                    extension(Key2, NewNode, DB1)
            end;
        _ ->
            throw(unchanged)
    end;
int_delete(Key, {branch, Branch} = _Node, DB) ->
    Val = branch_value(Branch),
    case Key of
        <<>> when Val =:= <<>> -> throw(unchanged);
        <<>> -> try_reduce_branch(set_branch_value(Branch, <<>>), DB);
        <<Next:4, Rest/bits>> ->
            Node = decode_node(branch_next(Next, Branch), DB),
            case int_delete(Rest, Node, DB) of
                {<<>>, DB1} ->
                    try_reduce_branch(set_branch_next(Next, Branch, <<>>), DB1);
                {NewNode, DB1} ->
                    branch(set_branch_next(Next, Branch, NewNode), DB1)
            end
    end.

try_reduce_branch(Branch, DB) ->
    case get_singleton_branch(Branch) of
        error        -> branch(Branch, DB);
        none         -> error({empty_branch, Branch});
        {value, Val} -> leaf(<<>>, Val, DB);
        {Next, NextNode} ->
            case decode_node(NextNode, DB) of
                {leaf, Path, Val} ->
                    leaf(<<Next:4, Path/bits>>, Val, DB);
                {ext, Path, Val} ->
                    extension(<<Next:4, Path/bits>>, Val, DB);
                {branch,_B} ->
                    extension(<<Next:4>>, NextNode, DB)
            end
    end.

get_singleton_branch(Branch) ->
    InitAcc = case branch_value(Branch) of
                  <<>> -> none;
                  Val  -> {value, Val}
              end,
    get_singleton_branch(Branch, 0, InitAcc).

get_singleton_branch(_Branch, N, Res) when N > 15 ->
    Res;
get_singleton_branch(Branch, N, Acc) ->
    Node = branch_next(N, Branch),
    case Node =:= <<>> of
        true ->
            get_singleton_branch(Branch, N + 1, Acc);
        false when Acc =/= none ->
            %% More than one value
            error;
        false ->
            %% First value
            get_singleton_branch(Branch, N + 1, {N, Node})
    end.


%%%===================================================================
%%% Unfold

int_unfold(_Prefix, <<>>, _DB) ->
    [];
int_unfold(Prefix, {'leaf', Key, _}, _DB) ->
    [{leaf, <<Prefix/bitstring, Key/bitstring>>}];
int_unfold(Prefix, {'ext', P, Node}, DB) ->
    int_unfold(<<Prefix/bitstring, P/bitstring>>, decode_node(Node, DB), DB);
int_unfold(Prefix, {'branch', BranchTuple}, _DB) ->
    int_unfold_branch(Prefix, lists:droplast(tuple_to_list(BranchTuple))).

int_unfold_branch(Prefix, Bs) ->
    int_unfold_branch(Prefix, 0, Bs, []).

int_unfold_branch(Prefix, X, [<<>> | Ns], Acc) ->
    int_unfold_branch(Prefix, X + 1, Ns, Acc);
int_unfold_branch(Prefix, X, [N | Ns], Acc) ->
    Prefix1 = <<Prefix/bitstring, X:4>>,
    int_unfold_branch(Prefix, X + 1, Ns, [{node, Prefix1, N} | Acc]);
int_unfold_branch(_Prefix, 16, [], Acc) ->
    lists:reverse(Acc).

int_has_node(<<Next:4>>, Node, {branch, Branch}, _DB) ->
    case branch_next(Next, Branch) of
        <<>> -> no;
        Node -> yes;
        _    -> maybe
    end;
int_has_node(<<Next:4, Rest/bits>>, Node, {branch, Branch}, DB) ->
    NextEncoded = branch_next(Next, Branch),
    NextNode = decode_node(NextEncoded, DB),
    int_has_node(Rest, Node, NextNode, DB);
int_has_node(Path, Node, {Type, NodePath, NodeVal}, DB)
        when Type =:= ext; Type =:= leaf ->
    S = bit_size(NodePath),
    case Path of
        <<NodePath:S/bits, Rest/bits>> when Type =:= ext ->
            NextNode = decode_node(NodeVal, DB),
            int_has_node(Rest, Node, NextNode, DB);
        _ ->
            S1 = bit_size(Path),
            case NodePath of
                <<Path:S1/bits, _/bits>> ->
                    maybe;
                _ ->
                    no
            end
    end;
int_has_node(_, _, <<>>, _) ->
    no;
int_has_node(<<>>, _, {branch, _Branch}, _DB) ->
    maybe.

%%%===================================================================
%%% Reachable store nodes (useful for implementing GC).

int_visit_reachable_hashes_in_cache(Hashes, DB, Visited, VisitFun) ->
    DBGetFun = fun db_find_in_cache/2,
    int_visit_reachable_hashes(Hashes, DB, Visited, VisitFun, DBGetFun).

int_visit_reachable_hashes_both(Hashes, DB, Visited, VisitFun) ->
    DBGetFun = fun(Key, DB_) -> {value, db_get(Key, DB_)}end,
    int_visit_reachable_hashes(Hashes, DB, Visited, VisitFun, DBGetFun).

int_visit_reachable_hashes([Hash|Left], DB, Visited, VisitFun, DBGetFun) ->
    Visited1 = visit_reachable_raw(Hash, DB, Visited, VisitFun, DBGetFun),
    int_visit_reachable_hashes(Left, DB, Visited1, VisitFun, DBGetFun);
int_visit_reachable_hashes([],_DB, Visited,_VisitFun,_DBGetFun) ->
    Visited.

visit_reachable_raw(Next, DB, Visited, VisitFun, DBGetFun) ->
    case byte_size(Next) < 32 of
        true  ->
            %% Too small to contain a hash
            Visited;
        false ->
            case DBGetFun(Next, DB) of
                {value, RawNode} ->
                    case VisitFun(Next, RawNode, Visited) of
                        stop ->
                            Visited;
                        {continue, Visited1} ->
                            NextNode = decode_node(Next, DB),
                            visit_reachable_node(NextNode, DB, Visited1, VisitFun, DBGetFun)
                    end;
                none ->
                    Visited
            end
    end.

visit_reachable_node(<<>>,_DB, Visited,_VisitFun,_DBGetFun) ->
    Visited;
visit_reachable_node({branch, Branch}, DB, Visited, VisitFun, DBGetFun) ->
    visit_reachable_branch(Branch, 0, DB, Visited, VisitFun, DBGetFun);
visit_reachable_node({ext, _, Next}, DB, Visited, VisitFun, DBGetFun) ->
    visit_reachable_raw(Next, DB, Visited, VisitFun, DBGetFun);
visit_reachable_node({leaf, _, _},_DB, Visited,_VisitFun,_DBGetFun) ->
    Visited.

visit_reachable_branch(_Branch, 16,_DB, Visited,_VisitFun,_DBGetFun) ->
    Visited;
visit_reachable_branch(Branch, N, DB, Visited, VisitFun, DBGetFun) ->
    Next = branch_next(N, Branch),
    Visited1 = visit_reachable_raw(Next, DB, Visited, VisitFun, DBGetFun),
    visit_reachable_branch(Branch, N + 1, DB, Visited1, VisitFun, DBGetFun).


%%%===================================================================
%%% @doc Construct proof for a key.
%%%
%%% A proof consists of a root hash for the tree, and the parts of the
%%% database that is needed to traverse to the path of the key, or to
%%% show that the key is not present.
%%%
%%% The proof takes a proof db (aeu_mp_trees_db) as input. This db is
%%% filled with the partial original db. Serializing the proof db is
%%% the responsibility of the caller.

int_c_proof(_Key, <<>>,_DB, ProofDB) ->
    {<<>>, ProofDB};
int_c_proof(<<>>, {branch, Branch},_DB, ProofDB) ->
    {branch_value(Branch), ProofDB};
int_c_proof(Path, {branch, Branch}, DB, ProofDB) ->
    <<Next:4, Rest/bits>> = Path,
    NextEncoded = branch_next(Next, Branch),
    NextNode = decode_node(NextEncoded, DB),
    ProofDB1 = proof_db_insert(NextEncoded, DB, ProofDB),
    int_c_proof(Rest, NextNode, DB, ProofDB1);
int_c_proof(Path, {Type, NodePath, NodeVal}, DB, ProofDB) when Type =:= ext;
                                                               Type =:= leaf ->
    S = bit_size(NodePath),
    case Path of
        NodePath when Type =:= leaf ->
            {NodeVal, ProofDB};
        <<NodePath:S/bits, _/bits>> when Type =:= leaf ->
            {<<>>, ProofDB};
        <<NodePath:S/bits, Rest/bits>> when Type =:= ext ->
            NextNode = decode_node(NodeVal, DB),
            ProofDB1 = proof_db_insert(NodeVal, DB, ProofDB),
            int_c_proof(Rest, NextNode, DB, ProofDB1);
        _ ->
            {<<>>, ProofDB}
    end.

proof_db_insert(<<>>,_DB, ProofDB) ->
    ProofDB;
proof_db_insert(Rlp,_DB, ProofDB) when byte_size(Rlp) < 32 ->
    %% The node is included in the parent.
    ProofDB;
proof_db_insert(Hash, DB, ProofDB) when byte_size(Hash) =:= 32 ->
    Node = db_get(Hash, DB),
    db_put(Hash, Node, ProofDB).

%%%===================================================================
%%% @doc Verify proof for a key value pair
%%%
%%% Verifying the proof is more of less exactly the same as getting a
%%% value and comparing it. However, we need also to check that all
%%% nodes conforms to their hashes along the way.

int_verify_proof(_Path, <<>>, <<>>, _ProofDB) ->
    ok;
int_verify_proof(_Path, <<>>, _, _ProofDB) ->
    bad_proof;
int_verify_proof(<<>>, {branch, Branch}, Val, _ProofDB) ->
    case branch_value(Branch) of
        Val  -> ok;
        Val1 -> {bad_value, Val1}
    end;
int_verify_proof(Path, {branch, Branch}, Val, ProofDB) ->
    <<Next:4, Rest/bits>> = Path,
    try decode_node_and_check_hash(branch_next(Next, Branch), ProofDB) of
        {ok, NextNode} -> int_verify_proof(Rest, NextNode, Val, ProofDB);
        {bad_hash, H}  -> {bad_hash, H}
    catch
        _:_ -> bad_proof
    end;
int_verify_proof(Path, {Type, NodePath, NodeVal}, Val, ProofDB)
  when Type =:= ext; Type =:= leaf ->
    S = bit_size(NodePath),
    case Path of
        NodePath when Type =:= leaf, Val =:= NodeVal ->
            ok;
        NodePath when Type =:= leaf, Val =/= NodeVal ->
            {bad_value, NodeVal};
        <<NodePath:S/bits, Rest/bits>> when Type =:= ext ->
            try decode_node_and_check_hash(NodeVal, ProofDB) of
                {ok, Next} -> int_verify_proof(Rest, Next, Val, ProofDB);
                {bad_hash, H} -> {bad_hash, H}
            catch
                _:_ -> bad_proof
            end;
        _ ->
            case Val =:= <<>> of
                true  -> ok;
                false -> {bad_value, <<>>}
            end
    end.

%%%===================================================================
%%% Iterator traversal

process_iterator_opts(Iter, [{max_path_length, X}]) when is_integer(X), X > 0 ->
    Iter#iter{max_length = X};
process_iterator_opts(#iter{ key = Key } = Iter,
                      [{with_prefix, X}]) when is_bitstring(X) ->
    case has_prefix(X, Key) of
        true -> Iter#iter{with_prefix = X};
        false -> error({illegal_prefix, X, Key})
    end;
process_iterator_opts(Iter, []) ->
    Iter;
process_iterator_opts(_Iter, [X|_]) ->
    error({illegal_iterator_option, X}).


-spec int_iter_next(path(), tree_node(), integer() | 'undefined', path(), db()) ->
                           '$end_of_table' | {path(), value()}.

int_iter_next(_Path, <<>>,_Max,_Prefix,_DB) ->
    '$end_of_table';
int_iter_next(<<>>, {branch, Branch}, Max, Prefix, DB) ->
    case check_iter_path_length(<<0:4>>, Max) of
        {ok, NewMax} ->
            pick_first_branch(Branch, 0, NewMax, Prefix, DB);
        error ->
            '$end_of_table'
    end;
int_iter_next(<<N:4, Rest/bits>>, {branch, Branch}, Max,  Prefix, DB) ->
    case check_iter_path_length(<<N:4>>, Max) of
        {ok, NewMax} ->
            {ok, NewPrefix} = match_prefix(Prefix, <<N:4>>),
            Next = decode_node(branch_next(N, Branch), DB),
            case int_iter_next(Rest, Next, NewMax, NewPrefix, DB) of
                '$end_of_table' -> pick_first_branch(Branch, N + 1, NewMax, Prefix, DB);
                {RestPath, Val} -> {<<N:4, RestPath/bits>>, Val}
            end;
        error ->
            '$end_of_table'
    end;
int_iter_next(Path, {leaf, Path, _},_Max,_Prefix,_DB) ->
    '$end_of_table';
int_iter_next(Path, {leaf, NodePath, Val}, Max, Prefix,_DB) ->
    case check_iter_path_length(NodePath, Max) of
        {ok, _} ->
            case Path < NodePath of
                true  ->
                    case has_prefix(Prefix, NodePath) of
                        true -> {NodePath, Val};
                        false -> '$end_of_table'
                    end;
                false -> '$end_of_table'
            end;
        error   -> '$end_of_table'
    end;
int_iter_next(Path, {ext, NodePath, Hash}, Max, Prefix, DB) ->
    case check_iter_path_length(NodePath, Max) of
        {ok, NewMax} ->
            S = bit_size(NodePath),
            case Path of
                <<NodePath:S/bits, Rest/bits>> ->
                    Next = decode_node(Hash, DB),
                    {ok, NewPrefix} = match_prefix(Prefix, NodePath),
                    case int_iter_next(Rest, Next, NewMax, NewPrefix, DB) of
                        '$end_of_table' -> '$end_of_table';
                        {RestPath, Val} ->
                            KeyPath = <<NodePath/bits, RestPath/bits>>,
                            case has_prefix(Prefix, KeyPath) of
                                true -> {KeyPath, Val};
                                false -> '$end_of_table'
                            end
                    end;
                _ when Path < NodePath ->
                    case match_prefix(Prefix, NodePath) of
                        {ok, NewPrefix} ->
                            case pick_first(decode_node(Hash, DB),
                                            NewMax, NewPrefix, DB) of
                                '$end_of_table' -> '$end_of_table';
                                {RestPath, Val} ->
                                    KeyPath = <<NodePath/bits, RestPath/bits>>,
                                    {KeyPath, Val}
                            end;
                        error ->
                            '$end_of_table'
                    end;
                _ when Path >= NodePath ->
                    '$end_of_table'
            end;
        error ->
            '$end_of_table'
    end.

pick_first(<<>>,_Max, _Prefix, _DB) ->
    '$end_of_table';
pick_first({leaf, Path, Val}, Max, Prefix, _DB) ->
    case check_iter_path_length(Path, Max) of
        {ok, _} ->
            case has_prefix(Prefix, Path) of
                true -> {Path, Val};
                false -> '$end_of_table'
            end;
        error   -> '$end_of_table'
    end;
pick_first({ext, Path, Hash}, Max, Prefix, DB) ->
    case check_iter_path_length(Path, Max) of
        {ok, NewMax} ->
            case match_prefix(Prefix, Path) of
                error -> '$end_of_table';
                {ok, NewPrefix} ->
                    case pick_first(decode_node(Hash, DB), NewMax, NewPrefix, DB) of
                        '$end_of_table' ->
                            '$end_of_table';
                        {RestPath, Val} ->
                            KeyPath = <<Path/bits, RestPath/bits>>,
                            {KeyPath, Val}
                    end
            end;
        error ->
            '$end_of_table'
    end;
pick_first({branch, Branch}, Max, Prefix, DB) ->
    case is_empty_prefix(Prefix) andalso (branch_value(Branch) =/= <<>>) of
        true ->
            case check_iter_path_length(<<>>, Max) of
                {ok, _} -> {<<>>, branch_value(Branch)};
                error -> '$end_of_table'
            end;
        false ->
            case check_iter_path_length(<<0:4>>, Max) of
                {ok, NewMax} -> pick_first_branch(Branch, 0, NewMax, Prefix, DB);
                error -> '$end_of_table'
            end
    end.

pick_first_branch(_Branch, N,_MaxPath,_Prefix,_DB) when is_integer(N), N > 15 ->
    '$end_of_table';
pick_first_branch(Branch, N, MaxPath, Prefix, DB) when is_integer(N) ->
    case match_prefix(Prefix, <<N:4>>) of
        {ok, NewPrefix} ->
            case pick_first(decode_node(branch_next(N, Branch), DB),
                            MaxPath, NewPrefix, DB) of
                '$end_of_table' ->
                    pick_first_branch(Branch, N + 1, MaxPath, Prefix, DB);
                {RestPath, Val} ->
                    {<<N:4, RestPath/bits>>, Val}
            end;
        error ->
            '$end_of_table'
    end.

check_iter_path_length(_Path, undefined) -> {ok, undefined};
check_iter_path_length(Path, Remaining) ->
    Length = bit_size(Path) div 4,
    case Length =< Remaining of
        true  -> {ok, Remaining - Length};
        false -> error
    end.

is_empty_prefix(<<>>) -> true;
is_empty_prefix(_   ) -> false.

has_prefix(<<>>,_Key) ->
    true;
has_prefix(Prefix, Key) ->
    S = bit_size(Prefix),
    case Key of
        <<Prefix:S/bits, _/bits>> -> true;
        _ -> false
    end.

match_prefix(<<>>  , _Key) -> {ok, <<>>};
match_prefix(Prefix, <<>>) -> {ok, Prefix};
match_prefix(<<X:4, Left1/bits>>, <<X:4, Left2/bits>>) -> match_prefix(Left1, Left2);
match_prefix(_, _) -> error.

%%%===================================================================
%%% Prettyprinter

pp_node(Node, DB) ->
    pp_node(Node, DB, false).

pp_tree(Node, DB) ->
    pp_node(Node, DB, true).

pp_node(<<>>,_DB,_Children) -> <<"<<>>">>;
pp_node({leaf, Path, Val}, DB,_Children) ->
    {Node, _} = leaf(Path, Val, DB),
    Hash = node_hash(Node),
    S = io_lib:format("~s: {leaf, ~s, ~w}",
                      [hexstring(Hash), hexstring(Path), Val]),
    iolist_to_binary(S);
pp_node({ext, Path, ChildHash}, DB, Children) ->
    {Node, _} = extension(Path, ChildHash, DB),
    Hash = node_hash(Node),
    S = io_lib:format("~s: {ext, ~s, ~s}",
                      [hexstring(Hash), hexstring(Path), hexstring(ChildHash)]),
    [iolist_to_binary(S)
     | if Children -> pp_node(decode_node(ChildHash, DB), DB, Children);
          true -> []
       end];
pp_node({branch, Branch}, DB, Children) ->
    {Node, _} = branch(Branch, DB),
    Hash = node_hash(Node),
    BranchElements = [pp_branch_element(X)
                      || X <- lists:droplast(tuple_to_list(Branch))],
    BranchString = "[" ++ string:join(BranchElements, ",") ++ "]",
    Val = branch_value(Branch),
    S = io_lib:format("~s: {branch, ~s, ~p}",
                      [hexstring(Hash), BranchString, Val]),
    [iolist_to_binary(S)
     | if Children ->
               [pp_node(decode_node(X, DB), DB)
                || X <- lists:droplast(tuple_to_list(Branch)), X =/= <<>>];
          true -> []
       end].

pp_branch_element(<<>>) -> "<<>>";
pp_branch_element(Bin) when byte_size(Bin) =:= 32 -> [hexstring(Bin)];
pp_branch_element(Bin) -> ["*", hexstring(node_hash(Bin))].

hexstring(Bin) ->
    [hexchar(X) || <<X:4>> <= Bin].

hexchar(X) when X > -1, X < 10 -> $0 + X;
hexchar(X) when X > -1, X < 16 -> $A + X - 10.

%%%===================================================================
%%% Nodes

-spec node_hash(enc_node()) -> hash().
node_hash(<<>>) -> error(no_hash_for_null);
node_hash(Bin) when byte_size(Bin) < 32   -> aec_hash:hash(header, Bin);
node_hash(Bin) when byte_size(Bin) =:= 32 -> Bin.

-spec decode_node(enc_node(), db()) -> tree_node().
decode_node(<<>> = X,_DB) -> X;
decode_node(Bin,_DB) when byte_size(Bin) < 32 ->
    case aeu_rlp:decode(Bin) of
        [CPath, Val] ->
            {Type, Path} = decode_path(CPath),
            {Type, Path, Val};
        [_|_] = Branch ->
            {branch, list_to_tuple(Branch)}
    end;
decode_node(Hash, DB) when byte_size(Hash) =:= 32 ->
    case db_get(Hash, DB) of
        [CPath, Val] ->
            {Type, Path} = decode_path(CPath),
            {Type, Path, Val};
        [_|_] = Branch ->
            {branch, list_to_tuple(Branch)}
    end.

-spec decode_node_and_check_hash(enc_node(), db()) -> {'ok', tree_node()}
                                                    | {'bad_hash', hash()}.
decode_node_and_check_hash(Rlp, DB) when byte_size(Rlp) < 32 ->
    {ok, decode_node(Rlp, DB)};
decode_node_and_check_hash(Hash, DB) when byte_size(Hash) =:= 32 ->
    EncNode = db_get(Hash, DB),
    Actual = aec_hash:hash(header, aeu_rlp:encode(EncNode)),
    case Actual =:= Hash of
        true  -> {ok, decode_node(Hash, DB)};
        false -> {bad_hash, Hash}
    end.


-spec encode_node(raw_node(), db()) -> {enc_node(), db()}.
encode_node(Node, DB) ->
    Rlp = aeu_rlp:encode(Node),
    case byte_size(Rlp) < 32 of
        true  -> {Rlp, DB};
        false ->
            Hash = aec_hash:hash(header, Rlp),
            {Hash, db_put(Hash, Node, DB)}
    end.

%% An extension must refer to a hash rather than the rlp encoding of a node.
-spec force_encoded_node_to_hash(enc_node(), db()) -> {hash(), db()}.
force_encoded_node_to_hash(<<>>,_DB) ->
    error(cannot_store_null);
force_encoded_node_to_hash(Hash, DB) when byte_size(Hash) =:= 32 ->
    {Hash, DB};
force_encoded_node_to_hash(Rlp, DB) when byte_size(Rlp) < 32 ->
    Node = aeu_rlp:decode(Rlp),
    Hash = node_hash(Rlp),
    {Hash, db_put(Hash, Node, DB)}.

%% A hash might have been forced for being refered in an extension.
%% Resolve the hash and find out if we should use the encoded node instead.
maybe_resolve_hash(<<>> = X,_DB) -> X;
maybe_resolve_hash(Bin,_DB) when byte_size(Bin) < 32 -> Bin;
maybe_resolve_hash(Hash, DB) when byte_size(Hash) =:= 32 ->
    Node = db_get(Hash, DB),
    Rlp  = aeu_rlp:encode(Node),
    case byte_size(Rlp) < 32 of
        true  -> Rlp;
        false -> Hash
    end.

%%%===================================================================
%%% Leaf

-spec leaf(path(), value(), db()) -> {enc_node(), db()}.
leaf(_Path, <<>>,_DB) ->
    error(cannot_store_null_in_node);
leaf(Path, Val, DB) when is_binary(Val); is_list(Val) ->
    encode_node([encode_path_leaf(Path), Val], DB).

maybe_extension(<<>>, Node, DB) ->
    {Node, DB};
maybe_extension(Path, Node, DB) when Path =/= <<>> ->
    extension(Path, Node, DB).

%%%===================================================================
%%% Extension

-spec extension(path(), enc_node(), db()) -> {enc_node(), db()}.
extension(Path, Node, DB) when Path =/= <<>> ->
    %% Extensions must have a hash as value.
    {Hash, DB1} = force_encoded_node_to_hash(Node, DB),
    encode_node([encode_path_ext(Path), Hash], DB1).

update_extension_path({ext,_OldPath, Hash}, NewPath, DB) ->
    case NewPath =:= <<>> of
        true ->
            %% Fall through to the refered value
            {maybe_resolve_hash(Hash, DB), DB};
        false ->
            encode_node([encode_path_ext(NewPath), Hash], DB)
    end.

%%%===================================================================
%%% Branch

-define(branch_tuple(___VAL___),
        { <<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<>>
        , <<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<>>
        , Val}).

-spec branch(branch_tuple(), db()) -> {enc_node(), db()}.
branch(Branch, DB) ->
    encode_node(tuple_to_list(Branch), DB).

-spec branch([{0..15, enc_node()},...], value(), db()) -> {enc_node(), db()}.
branch([_|_] = List, Val, DB) ->
    {true, List} = {length(List) =:= length(lists:ukeysort(1, List)), List},
    Fun = fun({X, Node}, Acc) -> setelement(X + 1, Acc, Node) end,
    Branch = lists:foldl(Fun, ?branch_tuple(Val), List),
    branch(Branch, DB).

branch_next(N, Branch) ->
    element(N + 1, Branch).

set_branch_next(N, Branch, Node) ->
    setelement(N + 1, Branch, Node).

branch_value(Branch) ->
    element(17, Branch).

set_branch_value(Branch, Value) ->
    setelement(17, Branch, Value).

%%%===================================================================
%%% DB interface

db_get(Hash, DB) ->
    case aeu_mp_trees_db:get(Hash, DB) of
        {value, Val} -> Val;
        none -> error({hash_not_present_in_db, Hash})
    end.

db_find_in_cache(Hash, DB) ->
    aeu_mp_trees_db:cache_get(Hash, DB).

db_put(Hash, Val, DB) ->
    aeu_mp_trees_db:put(Hash, Val, DB).

db_commit_from_cache(Hash, RawNode, DB) ->
    aeu_mp_trees_db:unsafe_write_to_backend(Hash, RawNode, DB).

db_drop_cache(DB) ->
    aeu_mp_trees_db:drop_cache(DB).

%%%===================================================================
%%% Dict db backend (default if nothing else was given in new/2)

new_dict_db() ->
    aeu_mp_trees_db:new(dict_db_spec()).

dict_db_spec() ->
    #{ handle => dict:new()
     , cache  => dict:new()
     , get    => {?MODULE, dict_db_get}
     , put    => {?MODULE, dict_db_put}
     , drop_cache => {?MODULE, dict_db_drop_cache}
     }.

dict_db_get(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Val} -> {value, Val};
        error -> none
    end.

dict_db_put(Key, Val, Dict) ->
    dict:store(Key, Val, Dict).

dict_db_drop_cache(_Cache) ->
    dict:new().

%%%===================================================================
%%% Compact encoding of hex sequence with optional terminator
%%%
%%% From: https://github.com/ethereum/wiki/wiki/Patricia-Tree
%%%
%%% The flagging of both odd vs. even remaining partial path length and
%%% leaf vs. extension node as described above reside in the first nibble
%%% of the partial path of any 2-item node. They result in the following:
%%%
%%% hex char    bits    |    node type partial     path length
%%% ----------------------------------------------------------
%%%    0        0000    |       extension              even
%%%    1        0001    |       extension              odd
%%%    2        0010    |   terminating (leaf)         even
%%%    3        0011    |   terminating (leaf)         odd
%%%
%%% For even remaining path length (0 or 2), another 0 "padding"
%%% nibble will always follow.

-define(EVEN_EXT,  0).
-define(ODD_EXT,   1).
-define(EVEN_LEAF, 2).
-define(ODD_LEAF , 3).

-spec encode_path_leaf(path()) -> encoded_path().
encode_path_leaf(Path) ->
    case bit_size(Path) rem 8 of
        0 -> <<?EVEN_LEAF:4, 0:4, Path/bits>>;
        4 -> <<?ODD_LEAF:4, Path/bits>>
    end.

-spec encode_path_ext(path()) -> encoded_path().
encode_path_ext(Path) ->
    case bit_size(Path) rem 8 of
        0 -> <<?EVEN_EXT:4, 0:4, Path/bits>>;
        4 -> <<?ODD_EXT:4, Path/bits>>
    end.

decode_path(<<?EVEN_EXT :4, 0:4, Left/bits>>) -> {ext, Left};
decode_path(<<?ODD_EXT  :4,      Left/bits>>) -> {ext, Left};
decode_path(<<?EVEN_LEAF:4, 0:4, Left/bits>>) -> {leaf, Left};
decode_path(<<?ODD_LEAF :4,      Left/bits>>) -> {leaf, Left}.
