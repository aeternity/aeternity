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
        , get_block/2
        , get_header/2
        , insert_block/2
        , insert_header/2
        , new/0
        , top_block/1
        , top_block_hash/1
        , top_header/1
        , top_header_hash/1
        ]).

-include("common.hrl"). %% Just for types
-include("blocks.hrl"). %% Just for types

-opaque(state() :: #{'type' => aec_chain_state
                    , 'blocks_db' => dict:dict()
                    , 'state_db' => dict:dict()
                    , 'top_header_hash' => binary() | 'undefined'
                    , 'top_block_hash' => binary() | 'undefined'
                    }).

-export_type([state/0]).

-define(match_state(___S___), #{type := aec_chain_state, ___S___}).
-define(assert_state(), #{type := aec_chain_state}).
-define(internal_error(____E____), {aec_chain_state_error, ____E____}).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> state().
new() ->
    #{ type => aec_chain_state
     , blocks_db => db_new()
     , top_header_hash => undefined
     , top_block_hash  => undefined
     , state_db => db_new()
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
insert_block(Block, ?assert_state() = State) ->
    try {ok, internal_insert(wrap_block(Block), State)}
    catch throw:?internal_error(What) -> {error, What}
    end.

-spec insert_header(#header{}, state()) -> {'ok', state()} | {'error', any()}.
insert_header(Header, ?assert_state() = State) ->
    try {ok, internal_insert(wrap_header(Header), State)}
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
              , content :: any() %% aec_block | aec_header
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

node_is_genesis(Node) ->
    node_is_genesis_block(Node)
        orelse node_is_genesis_header(Node).

node_is_genesis_header(Node) ->
    %% TODO: This is very blunt
    (Node#node.type =:= header) andalso
        (aec_block_genesis:genesis_header() =:= export_header(Node)).

node_is_genesis_block(Node) ->
    %% TODO: This is very blunt
    (Node#node.type =:= block) andalso
        (aec_block_genesis:genesis_header() =:= export_header(Node)).

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
%%% Handling difficulty
%%%-------------------------------------------------------------------

total_difficulty_at_hash(Hash, State) ->
    total_difficulty_at_hash(Hash, 0, State).

total_difficulty_at_hash(Hash, Acc, State) ->
    case blocks_db_find(Hash, State) of
        {ok, Node} ->
            NewAcc = Acc + node_difficulty(Node),
            PrevHash = prev_hash(Node),
            case node_is_genesis(Node) of
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
            NewTopNodes = find_new_header_top_from_node(ForkNode, State),
            NewTopNode = determine_new_header_top(NewTopNodes, State),
            State1 = set_top_header_hash(NewTopNode#node.hash, State),
            update_state_tree(ForkNode, State1);
        new_top ->
            [NewTopNode] = find_new_header_top_from_node(Node, State),
            State1 = set_top_header_hash(NewTopNode#node.hash, State),
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
            %% A new genesis block. TODO: This
            error(new_genesis_block_nyi);
        TopHash when is_binary(TopHash) ->
            case is_node_in_main_chain(Node, State) of
                true -> in_chain;
                false ->
                    TopNode = blocks_db_get(TopHash, State),
                    case find_fork_point(TopNode, Node, State) of
                        not_found -> off_chain;
                        ForkNode -> {fork, ForkNode}
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

find_fork_point(Node, Height, Node, Height,_State) -> Node;
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

find_new_header_top_from_node(Node, State) ->
    Height = node_height(Node),
    Hash = hash(Node),
    case children(blocks_db_find_at_height(Height + 1, State), Hash) of
        [] -> [Node];
        [NextNode] -> find_new_header_top_from_node(NextNode, State);
        [_|_]  = List ->
            lists:flatten([find_new_header_top_from_node(N, State)
                           || N <- List])
    end.

children(Nodes, Hash) ->
    children(Nodes, Hash, []).

children([],_Hash, Acc)  -> Acc;
children([N|Ns], ParentHash, Acc) ->
    case prev_hash(N) =:= ParentHash of
	true  -> children(Ns, ParentHash, [N|Acc]);
	false -> children(Ns, ParentHash, Acc)
    end.

determine_new_header_top(Nodes, State) ->
    Difficulties = [{total_difficulty_at_hash(N#node.hash, State), N}
                    || N <- Nodes],
    {_D, N}  = lists:last(lists:sort(Difficulties)),
    N.

genesis_state_tree(Node) ->
    %% TODO: This should be handled somewhere else.
    Trees = aec_blocks:trees(Node#node.content),
    %% Assert current assumption.
    [] = aec_blocks:txs(Node#node.content),
    case aec_trees:is_trees(aec_trees:accounts(Trees)) of
        false  ->
            {ok, Empty} = aec_trees:new(),
            aec_trees:set_accounts(Trees, Empty);
        true -> Trees
    end.

%% Precondition: The header chain is assumed to have been updated
%%               before this function is called, i.e., the choice
%%               of main chain must have been made already.
update_state_tree(#node{type = header} =_Node, State) ->
    State;
update_state_tree(#node{type = block} = Node, State) ->
    Hash = Node#node.hash,
    case get_prev_state_trees(Node, State) of
        error -> State;
        {ok, Tree} ->
            Txs = aec_blocks:txs(Node#node.content),
            Height = aec_blocks:height(Node#node.content),
            {ok, NewTrees} = aec_tx:apply_signed(Txs, Tree, Height),
            State1 = state_db_put(Hash, NewTrees, State),
            State2 = set_top_block_hash(Hash, State1),
            case find_node_at_height_from_top_header(Height + 1, State2) of
                {ok, NextNode} -> update_state_tree(NextNode, State2);
                not_found -> State2
            end
    end.

get_prev_state_trees(Node, State) ->
    case node_is_genesis_block(Node) of
        true -> {ok, genesis_state_tree(Node)};
        false -> state_db_find(prev_hash(Node), State)
    end.

is_node_in_main_chain(Node, State) ->
    Height = node_height(Node),
    case find_node_at_height_from_top_header(Height, State) of
        not_found -> false;
        {ok, Found} -> hash(Found) =:= hash(Node)
    end.

find_node_at_height_from_top_header(AtHeight, State) ->
    TopHash = get_top_header_hash(State),
    TopHeader = blocks_db_get(TopHash, State),
    Height = node_height(TopHeader),
    find_node_at_height(AtHeight, Height, TopHeader, State).

find_node_at_height(AtHeight, AtHeight, Node,_State) -> {ok, Node};
find_node_at_height(AtHeight, Height, Node, State) when Height > AtHeight ->
    PrevHash = prev_hash(Node),
    Node1 = blocks_db_get(PrevHash, State),
    find_node_at_height(AtHeight, Height - 1, Node1, State);
find_node_at_height(AtHeight, Height,_Node,_State) when Height < AtHeight ->
    not_found.

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

state_db_put(Hash, Trees, #{state_db := DB} = State) ->
    State#{state_db => db_put(Hash, Trees, DB)}.

state_db_find(Key, #{state_db := Store}) ->
    db_find(Key, Store).
