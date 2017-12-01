%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of the chain service
%%% @end
%%%-------------------------------------------------------------------

-module(aec_chain_state).

-export([ difficulty_at_hash/2
        , difficulty_at_top_header/1
        , difficulty_at_top_block/1
        , find_common_ancestor/3
        , get_block/2
        , get_block_by_height/2
        , get_header/2
        , get_header_by_height/2
        , get_missing_block_hashes/1
        , get_state_trees_for_persistance/1
        , insert_block/2
        , insert_header/2
        , new/0
        , new/1
        , new_from_persistance/2
        , top_block/1
        , top_block_hash/1
        , top_header/1
        , top_header_hash/1
        , get_genesis_hash/1
        ]).

-include("common.hrl"). %% Just for types
-include("blocks.hrl"). %% Just for types

-opaque(state() :: #{'type' => aec_chain_state
                    , 'blocks_db' => dict:dict()
                    , 'state_db' => dict:dict()
                    , 'top_header_hash' => binary() | 'undefined'
                    , 'top_block_hash' => binary() | 'undefined'
                    , 'max_snapshot_height' => pos_integer()
                    , 'sparse_snapshots_interval' => pos_integer()
                    , 'keep_all_snapshots_height' => pos_integer()
                    , 'genesis_block_hash' => 'undefined' | binary()
                    }).

-type(opts() :: #{ 'max_snapshot_height' := pos_integer()
                 , 'sparse_snapshots_interval' := pos_integer()
                 , 'keep_all_snapshots_height' := pos_integer()
                 }).


-export_type([ state/0
             , opts/0
             ]).

-define(match_state(___S___), #{type := aec_chain_state, ___S___}).
-define(assert_state(), #{type := aec_chain_state}).
-define(internal_error(____E____), {aec_chain_state_error, ____E____}).

%% TODO: These should be configs
-define(MAX_SNAPSHOT_HEIGHT, 20).
-define(SPARSE_SNAPSHOTS_INTERVAL, 5).
-define(KEEP_ALL_SNAPSHOTS_HEIGHT, 4).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> state().
new() ->
    new(default_opts()).

-spec new(opts()) -> state().
new(OptsIn) ->
    Opts = maps:merge(default_opts(), OptsIn),
    #{ type => aec_chain_state
     , blocks_db => db_new()
     , top_header_hash => undefined
     , top_block_hash  => undefined
     , state_db => db_new()
     , max_snapshot_height => maps:get(max_snapshot_height, Opts)
     , sparse_snapshots_interval => maps:get(sparse_snapshots_interval, Opts)
     , keep_all_snapshots_height => maps:get(keep_all_snapshots_height, Opts)
     , genesis_block_hash => undefined
     }.

default_opts() ->
    #{ max_snapshot_height => ?MAX_SNAPSHOT_HEIGHT
     , sparse_snapshots_interval => ?SPARSE_SNAPSHOTS_INTERVAL
     , keep_all_snapshots_height => ?KEEP_ALL_SNAPSHOTS_HEIGHT
     }.

-spec top_header(state()) -> 'undefined' | #header{}.
top_header(?match_state(top_header_hash := undefined)) -> undefined;
top_header(?match_state(top_header_hash := X) = State) ->
    export_header(blocks_db_get(X, State)).


-spec top_header_hash(state()) -> 'undefined' | binary().
top_header_hash(?match_state(top_header_hash := X)) ->
    X.

-spec top_block_hash(state()) -> 'undefined' | binary().
top_block_hash(?match_state(top_block_hash := X)) ->
    X.

-spec top_block(state()) -> 'undefined' | #block{}.
top_block(?match_state(top_block_hash := undefined)) -> undefined;
top_block(?match_state(top_block_hash := X) = State) ->
    export_block(blocks_db_get(X, State), State).

-spec insert_block(#block{}, state()) -> {'ok', state()} | {'error', any()}.
insert_block(Block, ?assert_state() = State0) ->
    Node = wrap_block(Block),
    try internal_insert(Node, State0) of
        State1 -> {ok, maybe_add_genesis_hash(State1, Node)}
    catch throw:?internal_error(What) -> {error, What}
    end.

-spec insert_header(#header{}, state()) -> {'ok', state()} | {'error', any()}.
insert_header(Header, ?assert_state() = State) ->
    Node = wrap_header(Header),
    try internal_insert(Node, State) of
        State1 -> {ok, maybe_add_genesis_hash(State1, Node)}
    catch throw:?internal_error(What) -> {error, What}
    end.

-spec get_block(binary(), state()) -> {'ok', #block{}} | 'error'.
get_block(Hash, ?assert_state() = State) ->
    case blocks_db_find(Hash, State) of
        {ok, Node} ->
            case node_is_block(Node) of
                true  -> {ok, export_block(Node, State)};
                false -> error
            end;
        error -> error
    end.

-spec get_header(binary(), state()) -> {'ok', #header{}} | 'error'.
get_header(Hash, ?assert_state() = State) ->
    case blocks_db_find(Hash, State) of
        {ok, Internal} -> {ok, export_header(Internal)};
        error -> error
    end.

-spec get_header_by_height(non_neg_integer(), state()) ->
                                  {'ok', #header{}} | {'error', atom()}.
get_header_by_height(Height, ?assert_state() = State) when is_integer(Height),
                                                           Height >= 0 ->
    case get_top_header_hash(State) of
        undefined -> {error, no_top_header};
        Hash ->
            Node = blocks_db_get(Hash, State),
            case find_node_at_height(Height, Node, State) of
                not_found -> {error, chain_too_short};
                {ok, Internal} -> {ok, export_header(Internal)}
            end
    end.

-spec get_block_by_height(non_neg_integer(), state()) ->
                                  {'ok', #block{}} | {'error', atom()}.
get_block_by_height(Height, ?assert_state() = State) when is_integer(Height),
                                                           Height >= 0 ->
    case get_top_header_hash(State) of
        undefined -> {error, no_top_header};
        Hash ->
            Node = blocks_db_get(Hash, State),
            case find_node_at_height(Height, Node, State) of
                not_found -> {error, chain_too_short};
                {ok, ResNode} ->
                    case node_is_block(ResNode) of
                        true  -> {ok, export_block(ResNode, State)};
                        false -> {error, block_not_found}
                    end
            end
    end.


-spec difficulty_at_top_block(state()) -> {'ok', float()} | {'error', atom()}.
difficulty_at_top_block(?assert_state() = State) ->
    case get_top_block_hash(State) of
        undefined -> {error, no_top};
        Hash -> total_difficulty_at_hash(Hash, State)
    end.

-spec difficulty_at_top_header(state()) -> {'ok', float()} | {'error', atom()}.
difficulty_at_top_header(?assert_state() = State) ->
    case get_top_header_hash(State) of
        undefined -> {error, no_top};
        Hash -> total_difficulty_at_hash(Hash, State)
    end.

-spec difficulty_at_hash(binary(), state()) -> {'ok', float()} | {'error', atom()}.
difficulty_at_hash(Hash, ?assert_state() = State) ->
    total_difficulty_at_hash(Hash, State).

-spec find_common_ancestor(binary(), binary(), state()) ->
                                  {'ok', binary()} | {error, atom()}.
find_common_ancestor(Hash1, Hash2, ?assert_state() = State) ->
    case {blocks_db_find(Hash1, State), blocks_db_find(Hash2, State)} of
        {{ok, Node1}, {ok, Node2}} ->
            case find_fork_point(Node1, Node2, State) of
                not_found -> {error, not_found};
                {ok, ForkNode} -> {ok, hash(ForkNode)}
            end;
        _ -> {error, unknown_hash}
    end.

-spec get_state_trees_for_persistance(state())->[{Hash :: binary(), #trees{}}].
get_state_trees_for_persistance(?assert_state() = State) ->
    state_db_to_list(State).

-spec new_from_persistance([#block{}|#header{}], [{Hash :: binary(), #trees{}}]) -> state().
new_from_persistance(Chain, StateTreesList) ->
    State = state_db_init_from_list(StateTreesList, new()),
    NodeChain = [wrap_block_or_header(X) || X <- Chain],
    State1 = blocks_db_init_from_list(NodeChain, State),
    case find_top_hash_from_genesis_node(State1) of
        not_found -> State1;
        {ok, TopHash} ->
            State2 = set_top_header_hash(TopHash, State1),
            case find_top_block_from_top_header(State2) of
                not_found -> State2;
                {ok, Node} -> update_state_tree(Node, State2)
            end
    end.

-spec get_genesis_hash(state()) -> undefined | binary().
get_genesis_hash(?match_state(genesis_block_hash := GH)) ->
    GH.

-spec get_missing_block_hashes(state()) -> [binary()].
%% @doc Get hashes for missing blocks on the main chain, i.e.,
%%      hashes that we know about since they are in the header chain,
%%      but we still don't have the blocks for them.
%%      Useful for the sync protocol.
get_missing_block_hashes(?assert_state() = State) ->
    TopHeaderHash = get_top_header_hash(State),
    TopBlockHash  = get_top_block_hash(State),
    get_missing_block_hashes(TopHeaderHash, TopBlockHash, State).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec internal_error(_) -> no_return().

internal_error(What) ->
    throw(?internal_error(What)).

get_top_header_hash(#{top_header_hash := H}) -> H.
set_top_header_hash(H, State) when is_binary(H) -> State#{top_header_hash => H}.

get_top_block_hash(#{top_block_hash := H}) -> H.
set_top_block_hash(H, State) when is_binary(H) -> State#{top_block_hash => H}.

%%%-------------------------------------------------------------------
%%% Internal ADT for differing between blocks and headers
%%%-------------------------------------------------------------------

-record(node, { type    :: 'block' | 'header'
              , content :: any() %% aec_blocks | aec_headers
              , difficulty :: float()
              , hash    :: binary()
              }).

hash(#node{hash = Hash}) -> Hash.

prev_hash(#node{type = header, content = X}) -> aec_headers:prev_hash(X);
prev_hash(#node{type = block , content = X}) -> aec_blocks:prev_hash(X).

node_height(#node{type = header, content = X}) -> aec_headers:height(X);
node_height(#node{type = block , content = X}) -> aec_blocks:height(X).

node_difficulty(#node{difficulty = X}) -> X.

node_is_block(#node{type = Type}) -> Type =:= block.

node_root_hash(#node{type = header, content = X}) -> aec_headers:root_hash(X);
node_root_hash(#node{type = block , content = X}) -> aec_blocks:root_hash(X).

node_target(#node{type = header, content = X}) -> aec_headers:target(X);
node_target(#node{type = block , content = X}) -> aec_blocks:target(X).

maybe_add_genesis_hash(#{genesis_block_hash := undefined} = State, Node) ->
    case node_height(Node) =:= 0 of
        true  -> State#{genesis_block_hash => hash(Node)};
        false -> State
    end;
maybe_add_genesis_hash(State,_Node) ->
    State.

find_genesis_node(State) ->
    case get_block(get_genesis_hash(State), State) of
        error -> not_found;
        {ok, Genesis} -> {ok, wrap_block(Genesis)}
    end.

%% this is when we insert the genesis block the first time
node_is_genesis(Node, ?match_state(genesis_block_hash := undefined)) ->
   node_height(Node) =:= 0; 
node_is_genesis(Node, State) ->
    hash(Node) =:= get_genesis_hash(State).

node_is_genesis_block(Node, State) ->
    (node_is_genesis(Node, State))
        andalso (Node#node.type =:= block).

wrap_block_or_header(#header{} = H) -> wrap_header(H);
wrap_block_or_header(#block{}  = B) -> wrap_block(B).

wrap_block(Block) ->
    {ok, Hash} = aec_blocks:hash_internal_representation(Block),
    #node{ type = block
         , content = Block
         , difficulty = aec_blocks:difficulty(Block)
         , hash = Hash
         }.

wrap_header(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    #node{ type = header
         , content = Header
         , difficulty = aec_headers:difficulty(Header)
         , hash = Hash
         }.

export_header(#node{type = header, content = H}) -> H;
export_header(#node{type = block, content = H}) -> aec_blocks:to_header(H).

export_block(#node{type = block, hash = H, content = B}, State) ->
    case add_state_tree_to_block(B, H, State) of
        {ok, ExportBlock} -> ExportBlock;
        error -> B
    end.

%%%-------------------------------------------------------------------
%%% Find missing blocks
%%%-------------------------------------------------------------------

get_missing_block_hashes(undefined, _, _State) ->
    [];
get_missing_block_hashes(TopHash, undefined, State) ->
    case get_genesis_hash(State) of
        undefined -> [];
        Hash      -> get_missing_block_hashes(TopHash, Hash, State)
    end;
get_missing_block_hashes(FromHash, ToHash, State) ->
    Hashes = get_hashes_between(FromHash, ToHash, State),
    lists:filter(fun(H) -> not node_is_block(blocks_db_get(H, State))end,
                 Hashes).

get_hashes_between(FromHash, ToHash, State) ->
    get_hashes_between(FromHash, ToHash, State, []).

get_hashes_between(Hash, Hash,_State, Acc) -> [Hash|Acc];
get_hashes_between(Current, Stop, State, Acc) ->
    Node = blocks_db_get(Current, State),
    get_hashes_between(prev_hash(Node), Stop, State, [Current|Acc]).

%%%-------------------------------------------------------------------
%%% Handling difficulty
%%%-------------------------------------------------------------------

total_difficulty_at_hash(Hash, State) ->
    total_difficulty_at_hash(Hash, 0, State).

total_difficulty_at_hash(Hash, Acc, State) ->
    case blocks_db_find(Hash, State) of
        {ok, Node} ->
            NewAcc = Acc + node_difficulty(Node),
            PrevHash = prev_hash(Node),
            case node_is_genesis(Node, State) of
                true -> {ok, NewAcc};
                false -> total_difficulty_at_hash(PrevHash, NewAcc, State)
            end;
        error ->
            {error, not_rooted}
    end.

%%%-------------------------------------------------------------------
%%% Handling the state trees
%%%-------------------------------------------------------------------

add_state_tree_to_block(Block, Hash, State) ->
    case state_db_find(Hash, State) of
        {ok, Trees} -> {ok, aec_blocks:set_trees(Block, Trees)};
        error -> error
    end.

%%%-------------------------------------------------------------------
%%% Chain operations
%%%-------------------------------------------------------------------

internal_insert(Node, State) ->
    case blocks_db_find(Node#node.hash, State) of
        error -> check_update_after_insert(Node, blocks_db_put(Node, State));
        {ok, Node} -> State;
        {ok, Old} when Node#node.type =:= block, Old#node.type =:= header ->
            check_update_after_insert(Node, blocks_db_put(Node, State));
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
        in_chain -> update_state_tree(Node, State);
        {fork, ForkNode} ->
            NewTopHashes = find_new_header_top_from_node(ForkNode, State),
            NewTopHash = determine_new_header_top_hash(NewTopHashes, State),
            case NewTopHash =:= get_top_header_hash(State) of
                true  ->
                    %% The fork is not taking over.
                    %% Don't bother calculating the state for this chain
                    %% to save some space.
                    State;
                false ->
                    %% The fork is the new main chain. We must update state
                    %% from the fork point.
                    assert_target_of_nodes_between(hash(ForkNode), NewTopHash, State),
                    State1 = set_top_header_hash(NewTopHash, State),
                    update_state_tree(ForkNode, State1)
            end;
        new_top ->
            %% This could still be a fork if it is the connecting block
            NewTopHash =
                case find_new_header_top_from_node(Node, State) of
                    [H] -> H;
                    NewTopHashes ->
                        determine_new_header_top_hash(NewTopHashes, State)
                end,
            assert_target_of_nodes_until_current_top(NewTopHash, State),
            State1 = set_top_header_hash(NewTopHash, State),
            update_state_tree(Node, State1)
    end.

determine_chain_relation(Node, State) ->
    Height   = node_height(Node),
    Hash     = Node#node.hash,
    PrevHash = prev_hash(Node),
    assert_previous_height(Height, PrevHash, State),
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
                false when GenesisHash =:= undefined -> in_chain; 
                false -> internal_error(rejecting_new_genesis_block)
            end;
        TopHash when is_binary(TopHash) ->
            case is_node_in_main_chain(Node, State) of
                true -> in_chain;
                false ->
                    TopNode = blocks_db_get(TopHash, State),
                    case find_fork_point(TopNode, Node, State) of
                        not_found -> off_chain;
                        {ok, ForkNode} -> {fork, ForkNode}
                    end
            end
    end.

assert_previous_height(Height, PrevHash, State) ->
    case blocks_db_find(PrevHash, State) of
        {ok, Node} ->
            case node_height(Node) =:= (Height - 1) of
                true -> ok;
                false -> internal_error(height_inconsistent_with_previous_hash)
            end;
        error -> ok
    end.

find_fork_point(Node1, Node2, State) ->
    Height1 = node_height(Node1),
    Height2 = node_height(Node2),
    find_fork_point(Node1, Height1, Node2, Height2, State).

find_fork_point(Node, Height, Node, Height,_State) -> {ok, Node};
find_fork_point(_Node1, Height,_Node2, Height,_State) when Height =:= 0 ->
    not_found;
find_fork_point(Node1, Height1, Node2, Height2, State) when Height1 > Height2,
                                                            Height1 > 0 ->
    PrevHash = prev_hash(Node1),
    case blocks_db_find(PrevHash, State) of
        error -> not_found;
        {ok, NewNode} ->
            NewHeight = node_height(NewNode),
            find_fork_point(NewNode, NewHeight, Node2, Height2, State)
    end;
find_fork_point(Node1, Height1, Node2, Height2, State) when Height2 >= Height2,
                                                            Height2 > 0 ->
    PrevHash = prev_hash(Node2),
    case blocks_db_find(PrevHash, State) of
        error -> not_found;
        {ok, NewNode} ->
            NewHeight = node_height(NewNode),
            find_fork_point(Node1, Height1, NewNode, NewHeight, State)
    end.

find_top_hash_from_genesis_node(State) ->
    case find_genesis_node(State) of
        not_found -> not_found;
        {ok, Genesis} ->
            case find_new_header_top_from_node(Genesis, State) of
                [TopHash] -> {ok, TopHash};
                TopHashes ->
                    {ok, determine_new_header_top_hash(TopHashes, State)}
            end
    end.


find_new_header_top_from_node(Node, State) ->
    Height = node_height(Node),
    Hash = hash(Node),
    case children(blocks_db_find_at_height(Height + 1, State), Hash) of
        [] -> [Hash];
        [NextNode] -> find_new_header_top_from_node(NextNode, State);
        [_|_]  = List ->
            Fun = fun(N) -> find_new_header_top_from_node(N, State) end,
            lists:flatmap(Fun, List)
    end.

children(Nodes, Hash) ->
    children(Nodes, Hash, []).

children([],_Hash, Acc)  -> Acc;
children([N|Ns], ParentHash, Acc) ->
    case prev_hash(N) =:= ParentHash of
        true  -> children(Ns, ParentHash, [N|Acc]);
        false -> children(Ns, ParentHash, Acc)
    end.

determine_new_header_top_hash(Hashes, State) ->
    {_D, H}  = lists:max([{total_difficulty_at_hash(H, State), H}
                          || H <- Hashes]),
    H.

assert_target_of_nodes_until_current_top(NodeHash, State) ->
    CurrentTopHash = get_top_header_hash(State),
    assert_target_of_nodes_between(CurrentTopHash, NodeHash, State).

assert_target_of_nodes_between(ForkNodeHash, NodeHash, State) ->
    Node = blocks_db_get(NodeHash, State),
    Header = export_header(Node),
    ForkHeight = node_height_by_hash(ForkNodeHash, State),
    case aec_target:determine_delta_header_height(Header) of
        {ok, Height} ->
            {ok, DeltaVerificationNode} = find_node_at_height(Height, Node, State),
            assert_calculated_target(Node, DeltaVerificationNode, ForkHeight, State);
        {error, chain_too_short_to_recalculate_target} ->
            assert_target_equal_to_parent(Node, ForkHeight, State)
    end.

node_height_by_hash(undefined, _State) ->
    0;
node_height_by_hash(ForkNodeHash, State) ->
    ForkNode = blocks_db_get(ForkNodeHash, State),
    node_height(ForkNode).

assert_target_equal_to_parent(Node, ForkHeight, State) ->
    case node_height(Node) =:= ForkHeight of
        true ->
            ok;
        false ->
            PrevNode = blocks_db_get(prev_hash(Node), State),
            case node_target(Node) =:= node_target(PrevNode) of
                true ->
                    assert_target_equal_to_parent(PrevNode, ForkHeight, State);
                false ->
                    internal_error({target_not_equal_to_parent, Node, PrevNode})
            end
    end.

assert_calculated_target(Node, DeltaVerificationNode, ForkHeight, State) ->
    case node_height(Node) =:= ForkHeight of
        true ->
            ok;
        false ->
            case node_is_genesis(DeltaVerificationNode, State) of
                true ->
                    assert_target_equal_to_parent(Node, ForkHeight, State);
                false ->
                    do_assert_calculated_target(
                      Node, DeltaVerificationNode, ForkHeight, State)
            end
    end.

do_assert_calculated_target(Node, DeltaVerificationNode, ForkHeight, State) ->
    PrevNode = blocks_db_get(prev_hash(Node), State),

    Header = export_header(Node),
    PrevHeader = export_header(PrevNode),
    DeltaHeader = export_header(DeltaVerificationNode),
    case aec_target:verify(Header, PrevHeader, DeltaHeader) of
        ok ->
            PrevDeltaNode = blocks_db_get(prev_hash(DeltaVerificationNode), State),
            assert_calculated_target(PrevNode, PrevDeltaNode, ForkHeight, State);
        {error, target_too_high} ->
            internal_error({target_too_high, Node, DeltaVerificationNode})
    end.


genesis_state_tree(Node) ->
    %% TODO: This should be handled somewhere else.
    Trees = aec_blocks:trees(Node#node.content),
    %% Assert current assumption.
    [] = aec_blocks:txs(Node#node.content),
    case aec_trees:accounts(Trees) of
        undefined  -> % persistence
            aec_block_genesis:populated_trees();
        _ -> Trees
    end.

%% Transitively compute the new state trees in the main chain
%% starting from the node (which must be in the main chain).
update_state_tree(Node, State) ->
    case calculate_state_trees(Node, State) of
        error -> State;
        {stored,_Trees} ->
            update_next_state_tree(Node, State);
        {calculated, Trees} ->
            assert_state_hash_valid(Trees, Node),
            State1 = state_db_put(hash(Node), Trees, State),
            update_next_state_tree(Node, State1)
    end.

assert_state_hash_valid(Trees, Node) ->
    RootHash = aec_trees:all_trees_hash(Trees),
    Expected = node_root_hash(Node),
    case RootHash =:= Expected of
        true -> ok;
        false ->
            internal_error({root_hash_mismatch, RootHash, Expected})
    end.

update_next_state_tree(Node, State) ->
    case find_successor_from_top_header(Node, State) of
        {ok, NextNode} when NextNode#node.type =:= block ->
            update_state_tree(NextNode, State);
        {ok, NextNode} when NextNode#node.type =:= header ->
            update_top_block_hash(hash(Node), State);
        not_found ->
            update_top_block_hash(hash(Node), State)
    end.

%% Get the state tree if present, otherwise calculate it from the most
%% recent previous tree if possible.
calculate_state_trees(Node, State) ->
    calculate_state_trees(Node, [], State).

calculate_state_trees(#node{type = header},_Acc,_State) ->
    %% If there is at least one header in the chain, we cannot
    %% calculate the state of the block.
    error;
calculate_state_trees(Node, Acc, State) ->
    case state_db_find(hash(Node), State) of
        {ok, Trees} when Acc =:= [] -> {stored, Trees};
        {ok, Trees} -> {calculated, apply_node_transactions(Acc, Trees, State)};
        error ->
            case node_is_genesis_block(Node, State) of
                true ->
                    Trees = genesis_state_tree(Node),
                    {calculated, apply_node_transactions([Node|Acc], Trees, State)};
                false ->
                    case blocks_db_find(prev_hash(Node), State) of
                        error -> error;
                        {ok, Prev} -> calculate_state_trees(Prev, [Node|Acc], State)
                    end
            end
    end.

update_top_block_hash(Hash, State) ->
    case get_top_block_hash(State) =:= Hash of
        true -> State;
        false ->
            State1 = set_top_block_hash(Hash, State),
            garbage_collect_state_trees(State1)
    end.

apply_node_transactions([Node|Left], Trees, State) ->
    Txs = aec_blocks:txs(Node#node.content),
    Height = node_height(Node),
    {ok, NewTrees} = aec_tx:apply_signed(Txs, Trees, Height),
    assert_state_hash_valid(NewTrees, Node),
    apply_node_transactions(Left, NewTrees, State);
apply_node_transactions([], Trees,_State) ->
    Trees.

is_node_in_main_chain(Node, State) ->
    case get_top_header_hash(State) of
        undefined -> false;
        TopHash ->
            TopNode = blocks_db_get(TopHash, State),
            Height = node_height(Node),
            case find_node_at_height(Height, TopNode, State) of
                not_found -> false;
                {ok, Found} -> hash(Found) =:= hash(Node)
            end
    end.

find_successor_from_top_header(Node, State) ->
    Height = node_height(Node),
    TopHash = get_top_header_hash(State),
    TopNode = blocks_db_get(TopHash, State),
    find_node_at_height(Height + 1, TopNode, State).

find_node_at_height(AtHeight, Node, State) ->
    Height = node_height(Node),
    if
        Height <   AtHeight -> not_found;
        Height =:= AtHeight -> {ok, Node};
        Height >   AtHeight ->
            PrevNode = blocks_db_get(prev_hash(Node), State),
            find_node_at_height(AtHeight, PrevNode, State)
    end.

find_top_block_from_top_header(State) ->
    Hash = get_top_header_hash(State),
    Node = blocks_db_get(Hash, State),
    find_top_block_from_top_header(Node, State, not_found).

find_top_block_from_top_header(Node, State, Candidate) ->
    NewCandidate =
        case node_is_block(Node) of
            true when Candidate =:= not_found -> {ok, Node};
            true -> Candidate;
            false -> not_found
        end,
    case node_is_genesis(Node, State) of
        true -> NewCandidate;
        false ->
            PrevNode = blocks_db_get(prev_hash(Node), State),
            find_top_block_from_top_header(PrevNode, State, NewCandidate)
    end.

%%%-------------------------------------------------------------------
%%% Garbage collection of state trees
%%% -------------------------------------------------------------------
%%% @doc Garbage collection strategy: Keep all snapshots from the
%%% block top until ?KEEP_ALL_SNAPSHOTS_HEIGHT, then keep only
%%% snapshots spaced with ?SPARSE_SNAPSHOTS_INTERVAL until
%%% ?MAX_SNAPSHOT_HEIGHT.
%%% TODO: The parameters should be configurable from the environment.

garbage_collect_state_trees(State) ->
    case get_top_block_hash(State) of
        undefined -> State;
        Hash when is_binary(Hash) ->
            Node = blocks_db_get(Hash, State),
            Keep = gc_collect(Node, 0, State, []),
            gc_init(Keep, State)
    end.

gc_init(Nodes, State) ->
    gc_init(Nodes, State, []).

gc_init([], State, Acc) ->
    state_db_init_from_list(Acc, State);
gc_init([Node|Left], State, Acc) ->
    case calculate_state_trees(Node, State) of
        {stored, Trees} -> gc_init(Left, State, [{hash(Node), Trees}|Acc]);
        {calculated, Trees} -> gc_init(Left, State, [{hash(Node), Trees}|Acc])
    end.

gc_collect(Node, HeightFromTop, State, Acc) ->
    case node_height(Node) =:= 0 of
        true  -> [Node|Acc];%% Always keep genesis if it falls into range
        false ->
            MaxHeight = maps:get(max_snapshot_height, State),
            case HeightFromTop > MaxHeight of
                true -> Acc;
                false ->
                    PrevNode = blocks_db_get(prev_hash(Node), State),
                    Height = HeightFromTop + 1,
                    case keep_state_snapshot(HeightFromTop, State) of
                        true -> gc_collect(PrevNode, Height, State, [Node|Acc]);
                        false -> gc_collect(PrevNode, Height, State, Acc)
                    end
            end
    end.

keep_state_snapshot(HeightFromTop, State) ->
    KeepAll = maps:get(keep_all_snapshots_height, State),
    Interval = maps:get(sparse_snapshots_interval, State),
    (HeightFromTop < KeepAll)
        orelse (((HeightFromTop - KeepAll) rem Interval) == 0).

%%%-------------------------------------------------------------------
%%% Internal interface for the blocks_db
%%%-------------------------------------------------------------------

db_new() ->
    dict:new().

db_put(Key, Val, Store) ->
    dict:store(Key, Val, Store).

db_get(Key, Store) ->
    case dict:find(Key, Store) of
        {ok, Res} -> Res;
        error -> error({failed_get, Key})
    end.

db_find(Key, Store) ->
    case dict:find(Key, Store) of
        {ok, Res} -> {ok, Res};
        error -> error
    end.

db_to_list(Store) ->
    dict:to_list(Store).

blocks_db_put(#node{hash = Hash} = Node, State) ->
    DB = maps:get(blocks_db, State),
    State#{blocks_db => db_put(Hash, Node, DB)}.

blocks_db_find_at_height(Height, #{blocks_db := Store}) ->
    %% TODO: This is pretty inefficient
    Fold = fun(_Hash, Node, Acc) ->
                   case node_height(Node) =:= Height of
                       true  -> [Node|Acc];
                       false -> Acc
                   end
           end,
    dict:fold(Fold, [], Store).

blocks_db_find(Key, #{blocks_db := Store}) ->
    db_find(Key, Store).

blocks_db_get(Key, #{blocks_db := Store}) ->
    db_get(Key, Store).

blocks_db_init_from_list(List, State) ->
    [GenesisNode] = [N || N <- List, node_height(N) =:= 0],
    GenesisHash = hash(GenesisNode),
    Fun = fun(Node, Acc) -> db_put(hash(Node), Node, Acc)end,
    DB = lists:foldl(Fun, db_new(), List),

    State#{blocks_db => DB, genesis_block_hash => GenesisHash}.


state_db_put(Hash, Trees, #{state_db := DB} = State) ->
    State#{state_db => db_put(Hash, Trees, DB)}.

state_db_find(Key, #{state_db := Store}) ->
    db_find(Key, Store).

state_db_init_from_list(List, State) ->
    Fun = fun({Key, Val}, Acc) -> db_put(Key, Val, Acc)end,
    DB = lists:foldl(Fun, db_new(), List),
    State#{state_db => DB}.

state_db_to_list(#{state_db := DB} =_State) ->
    db_to_list(DB).
