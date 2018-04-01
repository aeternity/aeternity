%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of the chain service
%%% @end
%%%-------------------------------------------------------------------

-module(aec_chain_state).

-export([ find_common_ancestor/2
        , get_hash_at_height/1
        , get_missing_block_hashes/0
        , hash_is_connected_to_genesis/1
        , insert_block/1
        , insert_header/1
        ]).

%% For tests
-export([ get_top_block_hash/1
        , get_top_header_hash/1
        , get_hash_at_height/2
        ]).


-include("common.hrl"). %% Just for types
-include("blocks.hrl"). %% Just for types

-define(internal_error(____E____), {aec_chain_state_error, ____E____}).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_hash_at_height(height()) -> {'ok', binary()} | 'error'.
get_hash_at_height(Height) when is_integer(Height), Height >= 0 ->
    get_hash_at_height(Height, new_state_from_persistence()).


-spec insert_block(#block{}) -> 'ok' | {'error', any()}.
insert_block(Block) ->
    Node = wrap_block(Block),
    try internal_insert(Node, Block, new_state_from_persistence()) of
        State1 -> persist_chain_state(State1)
    catch throw:?internal_error(What) -> {error, What}
    end.

-spec insert_header(#header{}) -> 'ok' | {'error', any()}.
insert_header(Header) ->
    Node = wrap_header(Header),
    try internal_insert(Node, Header, new_state_from_persistence()) of
        State1 -> persist_chain_state(State1)
    catch throw:?internal_error(What) -> {error, What}
    end.

-spec hash_is_connected_to_genesis(binary()) -> boolean().
hash_is_connected_to_genesis(Hash) when is_binary(Hash) ->
    case db_find_node(Hash) of
        error -> false;
        {ok, Node} ->
            State = new_state_from_persistence(),
            try determine_chain_relation(Node, State) of
                off_chain -> false;
                _         -> true
            catch throw:?internal_error(rejecting_new_genesis_block) -> false
            end
    end.

-spec find_common_ancestor(binary(), binary()) ->
                                  {'ok', binary()} | {error, atom()}.
find_common_ancestor(Hash1, Hash2) ->
    case {db_find_node(Hash1), db_find_node(Hash2)} of
        {{ok, Node1}, {ok, Node2}} ->
            case find_fork_point(Node1, Node2) of
                error          -> {error, not_found};
                {ok, ForkNode} -> {ok, hash(ForkNode)}
            end;
        _ -> {error, unknown_hash}
    end.

-spec get_missing_block_hashes() -> [binary()].
%% @doc Get hashes for missing blocks on the main chain, i.e.,
%%      hashes that we know about since they are in the header chain,
%%      but we still don't have the blocks for them.
%%      Useful for the sync protocol.
get_missing_block_hashes() ->
    State = new_state_from_persistence(),
    TopHeaderHash = get_top_header_hash(State),
    TopBlockHash  = get_top_block_hash(State),
    GenesisHash   = get_genesis_hash(State),
    get_missing_block_hashes(TopHeaderHash, TopBlockHash, GenesisHash).

%%%===================================================================
%%% Internal functions
%%%===================================================================

new_state_from_persistence() ->
    Fun = fun() ->
                  #{ type                  => ?MODULE
                   , top_header_hash       => aec_db:get_top_header_hash()
                   , top_header_difficulty => aec_db:get_top_header_difficulty()
                   , top_block_hash        => aec_db:get_top_block_hash()
                   , genesis_block_hash    => aec_db:get_genesis_hash()
                   }
          end,
    aec_db:ensure_transaction(Fun).

-spec internal_error(_) -> no_return().

internal_error(What) ->
    throw(?internal_error(What)).

get_genesis_hash(#{genesis_block_hash := GH}) -> GH.

get_top_header_hash(#{top_header_hash := H}) -> H.
set_top_header_hash(H, State) when is_binary(H) ->
    State#{top_header_hash => H}.

get_top_header_difficulty(#{top_header_difficulty := D}) -> D.
set_top_header_difficulty(Difficulty, State) ->
    State#{top_header_difficulty => Difficulty}.

get_top_block_hash(#{top_block_hash := H}) -> H.
set_top_block_hash(H, State) when is_binary(H) -> State#{top_block_hash => H}.

%%%-------------------------------------------------------------------
%%% Internal ADT for differing between blocks and headers
%%%-------------------------------------------------------------------

-record(node, { type    :: 'block' | 'header'
              , header  :: #header{}
              , hash    :: binary()
              }).

hash(#node{hash = Hash}) -> Hash.

prev_hash(#node{header = H}) -> aec_headers:prev_hash(H).

node_height(#node{header = H}) -> aec_headers:height(H).

node_difficulty(#node{header = H}) -> aec_headers:difficulty(H).

node_root_hash(#node{header = H}) -> aec_headers:root_hash(H).

maybe_add_genesis_hash(#{genesis_block_hash := undefined} = State, Node) ->
    case node_height(Node) =:= aec_block_genesis:height() of
        true  -> State#{genesis_block_hash => hash(Node)};
        false -> State
    end;
maybe_add_genesis_hash(State,_Node) ->
    State.

assert_not_new_genesis(_Node, #{genesis_block_hash := undefined}) -> ok;
assert_not_new_genesis(Node, #{genesis_block_hash := GHash}) ->
    case (node_height(Node) =:= aec_block_genesis:height()
          andalso (hash(Node) =/= GHash)) of
        true  -> internal_error(rejecting_new_genesis_block);
        false -> ok
    end.

%% this is when we insert the genesis block the first time
node_is_genesis(Node, #{genesis_block_hash := undefined}) ->
    node_height(Node) =:= aec_block_genesis:height();
node_is_genesis(Node, State) ->
    hash(Node) =:= get_genesis_hash(State).

node_is_genesis_block(Node, State) ->
    (node_is_genesis(Node, State))
        andalso (Node#node.type =:= block).

wrap_node(header, Header) -> wrap_header(Header);
wrap_node(block, Header) -> (wrap_header(Header))#node{type = block}.

wrap_block(Block) ->
    Header = aec_blocks:to_header(Block),
    {ok, Hash} = aec_headers:hash_header(Header),
    #node{ type = block
         , header = Header
         , hash = Hash
         }.

wrap_header(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    #node{ type = header
         , header = Header
         , hash = Hash
         }.

export_header(#node{header = Header}) ->
    Header.

%% NOTE: Only return nodes in the main chain.
%%       The function assumes that a node is in the main chain if
%%       there is only one node at that height, and the height is lower
%%       than the current top.
get_hash_at_height(Height, State) when is_integer(Height), Height >= 0 ->
    case get_top_header_hash(State) of
        undefined -> error;
        Hash ->
            TopHeight = node_height(db_get_node(Hash)),
            case Height > TopHeight of
                true  -> error;
                false ->
                    case db_find_nodes_at_height(Height) of
                        error -> error({broken_chain, Height});
                        {ok, [Node]} -> {ok, hash(Node)};
                        {ok, [_|_]} ->
                            %% We need to determine which of the
                            %% nodes that is in the main chain.
                            get_hash_at_height(Height, TopHeight, Hash)
                    end
            end
    end.

get_hash_at_height(Height, Height, Hash) ->
    {ok, Hash};
get_hash_at_height(Height, Current, Hash) ->
    case Current < Height of
        true  -> error;
        false ->
            Prev = db_get_prev_hash(Hash),
            get_hash_at_height(Height, Current - 1, Prev)
    end.

%%%-------------------------------------------------------------------
%%% Find missing blocks
%%%-------------------------------------------------------------------

get_missing_block_hashes(undefined, _, _) ->
    %% No top header hash
    [];
get_missing_block_hashes(_, _, undefined) ->
    %% No genesis hash
    [];
get_missing_block_hashes(TopHeaderHash, undefined, GenesisHash) ->
    %% No top block hash
    get_missing_block_hashes_1(TopHeaderHash, GenesisHash, []);
get_missing_block_hashes(TopHeaderHash, TopBlockHash,_GenesisHash) ->
    get_missing_block_hashes_1(TopHeaderHash, TopBlockHash, []).

get_missing_block_hashes_1(Current, Stop, Acc) ->
    NewAcc = case db_get_node(Current) of
                 #node{type = header} -> [Current|Acc];
                 #node{type = block}  -> Acc
             end,
    case Current =:= Stop of
        true  ->
            NewAcc;
        false ->
            get_missing_block_hashes_1(db_get_prev_hash(Current), Stop, NewAcc)
    end.

%%%-------------------------------------------------------------------
%%% Chain operations
%%%-------------------------------------------------------------------

internal_insert(Node, Original, State0) ->
    %% Keep track of which node we are actually adding to avoid giving
    %% spurious error messages.
    State = State0#{ currently_adding => hash(Node)},
    assert_not_new_genesis(Node, State),
    assert_previous_height(Node),
    StateOut = internal_insert_1(Node, Original, State),
    maps:remove(currently_adding, StateOut).

internal_insert_1(Node, Original, State) ->
    case db_find_node(hash(Node)) of
        error ->
            assert_calculated_target(Node),
            ok = db_put_node(Original, hash(Node)),
            check_update_after_insert(Node, State);
        {ok, Node} -> State;
        {ok, Old} when Node#node.type =:= block, Old#node.type =:= header ->
            assert_calculated_target(Node),
            ok = db_put_node(Original, hash(Node)),
            check_update_after_insert(Node, State);
        {ok, Old} when Node#node.type =:= Old#node.type ->
            internal_error({same_key_different_content, Node, Old});
        {ok, Old} when Node#node.type =:= header, Old#node.type =:= block ->
            case export_header(Old) =:= export_header(Node) of
                true  -> State;
                false -> internal_error({different_header_existing_block, Old, Node})
            end
    end.

check_update_after_insert(Node, State) ->
    case determine_chain_relation(Node, State) of
        off_chain -> State;
        new_top ->
            State1 = update_state_tree(Node, maybe_add_genesis_hash(State, Node)),
            case find_top_header_hash(State1) of
                not_found -> State1;
                {ok, TopHeaderHash, TopDifficulty} ->
                    State2 = set_top_header_hash(TopHeaderHash, State1),
                    set_top_header_difficulty(TopDifficulty, State2)
            end;
        in_chain ->
            %% This cannot change the top header
            update_state_tree(Node, State);
        {fork, ForkNode} ->
            State1 = update_state_tree(ForkNode, State),
            case find_top_header_hash(State1) of
                not_found -> State1;
                {ok, TopHeaderHash, TopDifficulty} ->
                    State2 = set_top_header_hash(TopHeaderHash, State1),
                    set_top_header_difficulty(TopDifficulty, State2)
            end
    end.

determine_chain_relation(Node, State) ->
    Height   = node_height(Node),
    Hash     = Node#node.hash,
    PrevHash = prev_hash(Node),
    case get_top_header_hash(State) of
        PrevHash -> new_top;
        undefined when Height > 0 ->
            off_chain; %% No proper chain yet.
        undefined when Height =:= 0 ->
            new_top; %% A genesis block. TODO: Should this be checked?
        Hash ->
            in_chain; %% This is the top header
        TopHash when is_binary(TopHash), Height =:= 0 ->
            %% This is a genesis block.
            TopBlockHash = get_top_block_hash(State),
            GenesisHash = get_genesis_hash(State),
            case Hash =:= GenesisHash of
                true when TopBlockHash =:= undefined -> in_chain;
                true -> in_chain;
                false when GenesisHash =:= undefined -> in_chain
            end;
        TopHash when is_binary(TopHash) ->
            case is_node_in_main_chain(Node, State) of
                true -> in_chain;
                false ->
                    TopNode = db_get_node(TopHash),
                    case find_fork_point(TopNode, Node) of
                        error -> off_chain;
                        {ok, ForkNode} -> {fork, ForkNode}
                    end
            end
    end.

assert_previous_height(Node) ->
    case db_find_node(prev_hash(Node)) of
        {ok, PrevNode} ->
            case node_height(PrevNode) =:= (node_height(Node) - 1) of
                true -> ok;
                false -> internal_error(height_inconsistent_with_previous_hash)
            end;
        error -> ok
    end.

find_top_header_hash(#{genesis_block_hash := undefined}) ->
    not_found;
find_top_header_hash(#{genesis_block_hash := GHash} = State) ->
    {Hash, Difficulty} = case get_top_block_hash(State) of
                             undefined ->
                                 D = node_difficulty(db_get_node(GHash)),
                                 {GHash, D};
                             TopBlockHash ->
                                 {ok, D} = db_find_difficulty(TopBlockHash),
                                 {TopBlockHash, D}
                         end,
    Node = db_get_node(Hash),
    Tops = find_tops(Node, Difficulty),
    {TopDifficulty, TopHash} = lists:last(lists:keysort(1, Tops)),
    {ok, TopHash, TopDifficulty}.

find_tops(Node, Acc) ->
    case db_children(Node) of
        [] ->
            [{Acc, hash(Node)}];
        [ChildNode] ->
            NewAcc = node_difficulty(ChildNode) + Acc,
            find_tops(ChildNode, NewAcc);
        [_|_]  = List ->
            Fun = fun(N) ->
                          NewAcc = node_difficulty(N) + Acc,
                          find_tops(N, NewAcc)
                  end,
            lists:flatmap(Fun, List)
    end.

%% To assert the target calculation we need DeltaHeight headers counted
%% backwards from the node we want to assert. If Height <= DeltaHeight
%% we will need all headers back to genesis.
assert_calculated_target(Node) ->
    case db_find_node(prev_hash(Node)) of
        error -> ok;
        {ok, PrevNode} ->
            case node_height(Node) of
                0  -> ok;
                Height ->
                    Delta = aec_governance:blocks_to_check_difficulty_count(),
                    assert_calculated_target(Node, PrevNode, Delta, Height)
            end
    end.

assert_calculated_target(Node, PrevNode, Delta, Height) when Delta >= Height ->
    %% We only need to verify that the target is equal to its predecessor.
    case {node_difficulty(Node), node_difficulty(PrevNode)} of
        {X, X} -> ok;
        {X, Y} -> internal_error({target_not_equal_to_parent, Node, X, Y})
    end;
assert_calculated_target(Node, PrevNode, Delta, Height) when Delta < Height ->
    case get_n_headers_from(PrevNode, Delta) of
        {error, chain_too_short} ->
            ok;
        {ok, Headers} ->
            Header = export_header(Node),
            case aec_target:verify(Header, Headers) of
                ok -> ok;
                {error, {wrong_target, Actual, Expected}} ->
                    internal_error({wrong_target, Node, Actual, Expected})
            end
    end.

get_n_headers_from(Node, N) ->
    get_n_headers_from(Node, N-1, []).

get_n_headers_from(Node, 0, Acc) ->
    {ok, lists:reverse([export_header(Node) | Acc])};
get_n_headers_from(Node, N, Acc) ->
    case db_find_node(prev_hash(Node)) of
        {ok, PrevNode} ->
            get_n_headers_from(PrevNode, N-1, [export_header(Node) | Acc]);
        error ->
            {error, chain_too_short}
    end.

%% Transitively compute the new state trees in the main chain
%% starting from the node (which must be in the main chain).

update_state_tree(#node{type = header}, State) ->
    State;
update_state_tree(Node, State) ->
    case get_state_trees_in(Node, State) of
        error -> State;
        {ok, Trees, Difficulty} ->
            {State1,_Difficulty} = update_state_tree(Node, Trees, Difficulty, State),
            State1
    end.

update_state_tree(#node{type = header}, Difficulty, _TreesIn, State) ->
    {State, Difficulty};
update_state_tree(Node, TreesIn, Difficulty, State) ->
    case db_find_state_and_difficulty(hash(Node)) of
        {ok, Trees, DifficultyOut} ->
            update_next_state_tree(Node, Trees, DifficultyOut, State);
        error ->
            case apply_and_store_state_trees(Node, TreesIn, Difficulty, State) of
                {ok, Trees, DifficultyOut} ->
                    update_next_state_tree(Node, Trees, DifficultyOut, State);
                error ->
                    {State, Difficulty}
            end
    end.

update_next_state_tree(Node, Trees, Difficulty, State) ->
    Hash = hash(Node),
    State1 = set_top_block_hash(Hash, State),
    ChildrenNodes = db_children(Node),
    update_next_state_tree_children(ChildrenNodes, Trees,
                                    Difficulty, Difficulty, State1).

update_next_state_tree_children([],_Trees,_Difficulty, Max, State) ->
    {State, Max};
update_next_state_tree_children([Child|Left], Trees, Difficulty, Max, State) ->
    {State1, Max1} = update_state_tree(Child, Trees, Difficulty, State),
    case Max1 > Max of
        true ->
            update_next_state_tree_children(Left, Trees, Difficulty, Max1, State1);
        false ->
            State2 = set_top_block_hash(get_top_block_hash(State), State1),
            update_next_state_tree_children(Left, Trees, Difficulty, Max, State2)
    end.

get_state_trees_in(Node, State) ->
    case node_is_genesis_block(Node, State) of
        true  -> {ok, aec_block_genesis:populated_trees(), 0};
        false -> db_find_state_and_difficulty(prev_hash(Node))
    end.

apply_and_store_state_trees(Node, TreesIn, DifficultyIn, #{currently_adding := Hash}) ->
    NodeHash = hash(Node),
    try
        Trees = apply_node_transactions(Node, TreesIn),
        assert_state_hash_valid(Trees, Node),
        assert_previous_height(Node),
        assert_calculated_target(Node),
        Difficulty = DifficultyIn + node_difficulty(Node),
        ok = db_put_state(hash(Node), Trees, Difficulty),
        {ok, Trees, Difficulty}
    catch
        %% Only catch this if the current node is NOT the one added in
        %% the call. We don't want to give an error message for any
        %% other node that that. But we want to make progress in the
        %% chain state even if a successor or predecessor to the
        %% currently added node is faulty.
        throw:?internal_error(_) when NodeHash =/= Hash -> error
    end.

assert_state_hash_valid(Trees, Node) ->
    RootHash = aec_trees:hash(Trees),
    Expected = node_root_hash(Node),
    case RootHash =:= Expected of
        true -> ok;
        false ->
            internal_error({root_hash_mismatch, RootHash, Expected})
    end.

apply_node_transactions(Node, Trees) ->
    Txs = db_get_txs(hash(Node)),
    Height = node_height(Node),
    case aec_trees:apply_signed_txs_strict(Txs, Trees, Height) of
        {ok, _, NewTrees} -> NewTrees;
        {error,_What} -> internal_error(invalid_transactions_in_block)
    end.

is_node_in_main_chain(Node, State) ->
    case get_hash_at_height(node_height(Node), State) of
        error -> false;
        {ok, Hash} -> hash(Node) =:= Hash
    end.

find_fork_point(Node1, Node2) ->
    Height1  = node_height(Node1),
    Height2  = node_height(Node2),
    Hash1    = hash(Node1),
    Hash2    = hash(Node2),
    case find_fork(Hash1, Height1, Hash2, Height2) of
        {ok, Hash} -> db_find_node(Hash); %% This might still fail
        not_found -> error
    end.

find_fork(Hash, Height, Hash, Height) ->
    {ok, Hash};
find_fork(_Hash1, Height,_Hash2, Height) when Height =:= 0 ->
    not_found;
find_fork(Hash1, Height, Hash2, Height) ->
    case {db_find_prev_hash(Hash1), db_find_prev_hash(Hash2)} of
        {none, _   } -> not_found;
        {_   , none} -> not_found;
        {{value, Common}, {value, Common}} -> {ok, Common};
        {{value, Prev1},  {value, Prev2}}  ->
            NewHeight = Height - 1,
            find_fork(Prev1, NewHeight, Prev2, NewHeight)
    end;
find_fork(Hash1, Height1, Hash2, Height2) when Height1 > Height2 ->
    case db_find_prev_hash(Hash1) of
        none -> not_found;
        {value, Prev} -> find_fork(Prev, Height1-1, Hash2, Height2)
    end;
find_fork(Hash1, Height1, Hash2, Height2) when Height1 < Height2 ->
    case db_find_prev_hash(Hash2) of
        none -> not_found;
        {value, Prev} -> find_fork(Hash1, Height1, Prev, Height2-1)
    end.

%%%-------------------------------------------------------------------
%%% Persist results
%%%-------------------------------------------------------------------

persist_chain_state(State) ->
    aec_db:ensure_transaction(fun() -> persist_state(State)end),
    ok.

persist_state(State) ->
    case get_genesis_hash(State) of
        undefined -> ok;
        GenesisHash ->
            aec_db:write_genesis_hash(GenesisHash),
            case get_top_header_hash(State) of
                undefined -> ok;
                TopHeaderHash ->
                    Difficulty = get_top_header_difficulty(State),
                    aec_db:write_top_header_hash_and_difficulty(TopHeaderHash,
                                                                Difficulty),
                    case get_top_block_hash(State) of
                        undefined -> ok;
                        TopBlockHash ->
                            aec_db:write_top_block_hash(TopBlockHash)
                    end
            end
    end.

%%%-------------------------------------------------------------------
%%% Internal interface for the db
%%%-------------------------------------------------------------------

db_put_node(#block{} = Block, Hash) when is_binary(Hash) ->
    aec_db:ensure_transaction(
      fun() ->
              ok = aec_db:write_block(Block),
              #block{txs = Transactions} = Block,
              aec_db:write_txs(Transactions, Hash)
      end);
db_put_node(#header{} = Header, Hash) when is_binary(Hash) ->
    ok = aec_db:write_header(Header).

db_find_node(Hash) when is_binary(Hash) ->
    case aec_db:find_chain_node(Hash) of
        {Type, Header} -> {ok, wrap_node(Type, Header)};
        none -> error
    end.

db_get_node(Hash) when is_binary(Hash) ->
    {ok, Node} = db_find_node(Hash),
    Node.

db_find_nodes_at_height(Height) when is_integer(Height) ->
    case aec_db:find_chain_nodes_at_height(Height) of
        [_|_] = Nodes ->
            {ok, lists:map(fun({Type, Header}) -> wrap_node(Type, Header) end,
                           Nodes)};
        [] -> error
    end.


db_put_state(Hash, Trees, Difficulty) when is_binary(Hash) ->
    Trees1 = aec_trees:commit_to_db(Trees),
    ok = aec_db:write_block_state(Hash, Trees1, Difficulty).

db_find_state_and_difficulty(Hash) when is_binary(Hash) ->
    case aec_db:find_block_state_and_difficulty(Hash) of
        {value, Trees, Difficulty} -> {ok, Trees, Difficulty};
        none -> error
    end.

db_find_difficulty(Hash) when is_binary(Hash) ->
    case aec_db:find_block_difficulty(Hash) of
        {value, Difficulty} -> {ok, Difficulty};
        none -> error
    end.

db_get_txs(Hash) when is_binary(Hash) ->
    aec_blocks:txs(aec_db:get_block(Hash)).

db_get_prev_hash(Hash) when is_binary(Hash) ->
    {value, PrevHash} = db_find_prev_hash(Hash),
    PrevHash.

db_find_prev_hash(Hash) when is_binary(Hash) ->
    case db_find_node(Hash) of
        {ok, Node} -> {value, prev_hash(Node)};
        error -> none
    end.

db_children(#node{} = Node) ->
    Height = node_height(Node),
    Hash   = hash(Node),
    [wrap_node(Type, Header)
     || {Type, Header} <- aec_db:find_chain_nodes_at_height(Height + 1),
        aec_headers:prev_hash(Header) =:= Hash].
