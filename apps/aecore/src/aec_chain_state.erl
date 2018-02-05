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
        , get_open_oracle_queries/4
        , get_oracles/3
        , get_top_N_blocks_time_summary/2
        , get_n_headers_from_top/2
        , hash_is_connected_to_genesis/2
        , has_block/2
        , has_header/2
        , insert_block/2
        , insert_header/2
        , new/0
        , new_from_persistence/0
        , top_block/1
        , get_block_state/2
        , account/2
        , all_accounts_balances/2
        , top_block_hash/1
        , top_header/1
        , top_header_hash/1
        , get_genesis_hash/1
        , name_entry/2
        , resolve_name/3
        ]).

-include("common.hrl"). %% Just for types
-include("blocks.hrl"). %% Just for types

-opaque(state() :: #{ 'type' => aec_chain_state
                    , 'top_header_hash' => binary() | 'undefined'
                    , 'top_block_hash' => binary() | 'undefined'
                    , 'genesis_block_hash' => 'undefined' | binary()
                    }).

-export_type([ state/0
             ]).

-define(match_state(___S___), #{type := aec_chain_state, ___S___}).
-define(assert_state(), #{type := aec_chain_state}).
-define(internal_error(____E____), {aec_chain_state_error, ____E____}).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> state().
new() ->
    #{ type               => ?MODULE
     , top_header_hash    => undefined
     , top_block_hash     => undefined
     , genesis_block_hash => undefined
     }.

-spec new_from_persistence() -> state().
new_from_persistence() ->
    #{ type               => ?MODULE
     , top_header_hash    => aec_db:get_top_header_hash()
     , top_block_hash     => aec_db:get_top_block_hash()
     , genesis_block_hash => aec_db:get_genesis_hash()
     }.


-spec top_header(state()) -> 'undefined' | #header{}.
top_header(?match_state(top_header_hash := undefined)) -> undefined;
top_header(?match_state(top_header_hash := X)) ->
    export_header(db_get_node(X)).


-spec top_header_hash(state()) -> 'undefined' | binary().
top_header_hash(?match_state(top_header_hash := X)) ->
    X.

-spec top_block_hash(state()) -> 'undefined' | binary().
top_block_hash(?match_state(top_block_hash := X)) ->
    X.

-spec top_block(state()) -> 'undefined' | #block{}.
top_block(?match_state(top_block_hash := undefined)) -> undefined;
top_block(?match_state(top_block_hash := X)) ->
    export_block(db_get_node(X)).

-spec get_block_state(binary(), state()) -> {'ok', trees()} | {'error', 'no_state_trees'}.
%% TODO: This can be rerouted at some point
get_block_state(Hash, ?assert_state()) ->
    case db_find_state(Hash) of
        {ok, Trees} -> {ok, Trees};
        error -> {error, no_state_trees}
    end.

-spec account(pubkey(), state()) -> 'no_top_block_hash' | 'no_state_trees' |
                                    'none' | {value, account()}.
%% TODO: This can be rerouted at some point
account(_, ?match_state(top_block_hash := undefined)) -> no_top_block_hash; %% TODO Can this ever happen?
account(Pubkey, ?match_state(top_block_hash := X) =_State) ->
    case db_find_state(X) of
        {ok, Trees} ->
            aec_accounts_trees:lookup(Pubkey, aec_trees:accounts(Trees));
        error -> no_state_trees
    end.

-spec all_accounts_balances(binary(), state()) -> {'ok', [{pubkey(), non_neg_integer()}]} |
                                                  {'error', 'no_state_trees'}.
%% TODO: This can be rerouted at some point
all_accounts_balances(BlockHeaderHash, ?assert_state() =_State) ->
    case db_find_state(BlockHeaderHash) of
        {ok, Trees} ->
            {ok, aec_accounts_trees:get_all_accounts_balances(
                   aec_trees:accounts(Trees))};
        error -> {error, no_state_trees}
    end.

-spec get_n_headers_from_top(non_neg_integer(), state()) ->
                          {'ok', list(#header{})} | {error, atom()}.
get_n_headers_from_top(_N, ?match_state(top_header_hash := undefined)) ->
    {error, chain_too_short};
get_n_headers_from_top(N, ?match_state(top_header_hash := X)) ->
    get_n_headers_from(db_get_node(X), N).

-spec insert_block(#block{}, state()) -> {'ok', state()} | {'error', any()}.
insert_block(Block, ?assert_state() = State0) ->
    Node = wrap_block(Block),
    try internal_insert(Node, Block, State0) of
        State1 -> {ok, State1}
    catch throw:?internal_error(What) -> {error, What}
    end.

-spec insert_header(#header{}, state()) -> {'ok', state()} | {'error', any()}.
insert_header(Header, ?assert_state() = State) ->
    Node = wrap_header(Header),
    try internal_insert(Node, Header, State) of
        State1 -> {ok, State1}
    catch throw:?internal_error(What) -> {error, What}
    end.

-spec get_block(binary(), state()) -> {'ok', #block{}} | 'error'.
get_block(Hash, ?assert_state()) ->
    case db_find_node(Hash) of
        {ok, Node} ->
            case node_is_block(Node) of
                true  -> {ok, export_block(Node)};
                false -> error
            end;
        error -> error
    end.

-spec has_block(binary(), state()) -> boolean().
has_block(Hash, ?assert_state()) ->
    case db_find_node(Hash) of
        {ok, Node} -> node_is_block(Node);
        error      -> false
    end.

-spec has_header(binary(), state()) -> boolean().
has_header(Hash, ?assert_state()) ->
    case db_find_node(Hash) of
        {ok,_Node} -> true;
        error      -> false
    end.

-spec get_header(binary(), state()) -> {'ok', #header{}} | 'error'.
get_header(Hash, ?assert_state()) ->
    case db_find_node(Hash) of
        {ok, Internal} -> {ok, export_header(Internal)};
        error -> error
    end.

-spec get_header_by_height(non_neg_integer(), state()) ->
                                  {'ok', #header{}} | {'error', atom()}.
get_header_by_height(Height, ?assert_state() = State) when is_integer(Height),
                                                           Height >= 0 ->
    case get_node_by_height(Height, State) of
        {ok, Node} -> {ok, export_header(Node)};
        Error -> Error
    end.

-spec get_block_by_height(non_neg_integer(), state()) ->
                                  {'ok', #block{}} | {'error', atom()}.
get_block_by_height(Height, ?assert_state() = State) when is_integer(Height),
                                                           Height >= 0 ->
    case get_node_by_height(Height, State) of
        {ok, Node} ->
            case node_is_block(Node) of
                true  -> {ok, export_block(Node)};
                false -> {error, block_not_found}
            end;
        Error -> Error
    end.

-spec hash_is_connected_to_genesis(binary(), state()) -> boolean().
hash_is_connected_to_genesis(Hash, ?assert_state() = State) when is_binary(Hash) ->
    case db_find_node(Hash) of
        error -> false;
        {ok, Node} ->
            try determine_chain_relation(Node, State) of
                off_chain -> false;
                _         -> true
            catch throw:?internal_error(rejecting_new_genesis_block) -> false
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
find_common_ancestor(Hash1, Hash2, ?assert_state()) ->
    case {db_find_node(Hash1), db_find_node(Hash2)} of
        {{ok, Node1}, {ok, Node2}} ->
            case find_fork_point(Node1, Node2) of
                error          -> {error, not_found};
                {ok, ForkNode} -> {ok, hash(ForkNode)}
            end;
        _ -> {error, unknown_hash}
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
    GenesisHash   = get_genesis_hash(State),
    get_missing_block_hashes(TopHeaderHash, TopBlockHash, GenesisHash).

get_top_N_blocks_time_summary(?assert_state() = State, N)
  when is_integer(N) andalso N > 0->
    case db_find_node(top_block_hash(State)) of
        {ok, TopNode} ->
            get_N_nodes_time_summary(TopNode, State, N);
        error ->
            []
    end.

-spec name_entry(binary(), state()) ->
                        {'ok', map()} |
                        {'error', 'no_state_trees'} |
                        {'error', 'name_not_found'}.
name_entry(Name, ?assert_state() = State) ->
    TopHash = get_top_header_hash(State),
    case db_find_state(TopHash) of
        {ok, Trees} ->
            aens:get_name_entry(Name, aec_trees:ns(Trees));
        error ->
            {error, no_state_trees}
    end.

-spec get_open_oracle_queries(pubkey(), binary() | '$first',
                              non_neg_integer(), state()) ->
    {'ok', list()} | {'error', 'no_state_trees'}.
get_open_oracle_queries(Oracle, From, Max, ?assert_state() = State) ->
    TopHash = get_top_header_hash(State),
    case db_find_state(TopHash) of
        {ok, Trees} ->
            OTrees = aec_trees:oracles(Trees),
            {ok, aeo_state_tree:get_open_oracle_queries(Oracle, From, Max, OTrees)};
        error ->
            {error, no_state_trees}
    end.

-spec get_oracles(binary() | '$first', non_neg_integer(), state()) ->
                        {'ok', list()} |
                        {'error', 'no_state_trees'}.
get_oracles(From, Max, ?assert_state() = State) ->
    TopHash = get_top_header_hash(State),
    case db_find_state(TopHash) of
        {ok, Trees} ->
            {ok, aeo_state_tree:get_oracles(From, Max, aec_trees:oracles(Trees))};
        error ->
            {error, no_state_trees}
    end.

-spec resolve_name(atom(), binary(), state()) -> {'ok', binary()} |
                                                 {error, atom()}.
resolve_name(Type, Name, ?assert_state() = State) ->
    TopHash = get_top_header_hash(State),
    case db_find_state(TopHash) of
        {ok, Trees} ->
            aens:resolve(Type, Name, aec_trees:ns(Trees));
        error ->
            {error, no_state_trees}
    end.

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
              , header  :: #header{}
              , hash    :: binary()
              }).

hash(#node{hash = Hash}) -> Hash.

prev_hash(#node{header = H}) -> aec_headers:prev_hash(H).

node_height(#node{header = H}) -> aec_headers:height(H).

node_difficulty(#node{header = H}) -> aec_headers:difficulty(H).

node_is_block(#node{type = Type}) -> Type =:= block.

node_root_hash(#node{header = H}) -> aec_headers:root_hash(H).

node_time(#node{header = H}) -> aec_headers:time_in_msecs(H).

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
node_is_genesis(Node, ?match_state(genesis_block_hash := undefined)) ->
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

export_block(#node{type = block} = Node) ->
    aec_db:get_block(hash(Node)).

get_node_by_height(Height, State)  ->
    case get_top_header_hash(State) =:= undefined of
        true -> {error, no_top_header};
        false ->
            case get_hash_at_height(Height, State) of
                error -> {error, chain_too_short};
                {ok, Hash} -> {ok, db_get_node(Hash)}
            end
    end.

get_hash_at_height(Height, State) when is_integer(Height), Height >= 0 ->
    case get_top_header_hash(State) of
        undefined -> error;
        Hash ->
            TopHeight = node_height(db_get_node(Hash)),
            get_hash_at_height(Height, TopHeight, Hash)
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
%%% Handling difficulty
%%%-------------------------------------------------------------------

total_difficulty_at_hash(Hash, State) ->
    total_difficulty_at_hash(Hash, 0, State).

total_difficulty_at_hash(Hash, Acc, State) ->
    case db_find_node(Hash) of
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
    case db_find_node(Node) of
        error ->
            assert_calculated_target(Node),
            ok = db_put_node(Original),
            check_update_after_insert(Node, State);
        {ok, Node} -> State;
        {ok, Old} when Node#node.type =:= block, Old#node.type =:= header ->
            assert_calculated_target(Node),
            ok = db_put_node(Original),
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
        new_top -> update_state_tree(Node, maybe_add_genesis_hash(State, Node));
        in_chain -> update_state_tree(Node, State);
        {fork, ForkNode} -> update_state_tree(ForkNode, State)
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
    Hash = case get_top_block_hash(State) of
               undefined -> GHash;
               TopBlockHash -> TopBlockHash
           end,
    Node = db_get_node(Hash),
    Tops = find_tops(Node, node_difficulty(Node)),
    {_Difficulty, TopHash} = lists:last(lists:keysort(1, Tops)),
    {ok, TopHash}.

find_tops(Node, Acc) ->
    NewAcc = node_difficulty(Node) + Acc,
    case db_children(Node) of
        [] ->
            [{NewAcc, hash(Node)}];
        [ChildNode] ->
            find_tops(ChildNode, NewAcc);
        [_|_]  = List ->
            Fun = fun(N) -> find_tops(N, NewAcc) end,
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

%% Transitively compute the new state trees in the main chain
%% starting from the node (which must be in the main chain).

update_state_tree(#node{type = header}, State) ->
    case find_top_header_hash(State) of
        {ok, TopHeaderHash} -> set_top_header_hash(TopHeaderHash, State);
        not_found -> State
    end;
update_state_tree(Node, State) ->
    StateOut = case get_state_trees_in(Node, State) of
                   error -> State;
                   {ok, Trees} ->
                       {State1,_Difficulty} = update_state_tree(Node, Trees, 0, State),
                       State1
               end,
    case find_top_header_hash(StateOut) of
        {ok, TopHeaderHash} -> set_top_header_hash(TopHeaderHash, StateOut);
        not_found -> StateOut
    end.

update_state_tree(#node{type = header},_TreesIn, Difficulty, State) ->
    {State, Difficulty};
update_state_tree(Node, TreesIn, Difficulty, State) ->
    case db_find_state(hash(Node)) of
        {ok, Trees} -> update_next_state_tree(Node, Trees, Difficulty, State);
        error ->
            case apply_and_store_state_trees(Node, TreesIn, State) of
                {ok, Trees} ->
                    update_next_state_tree(Node, Trees, Difficulty, State);
                error ->
                    {State, Difficulty}
            end
    end.

update_next_state_tree(Node, Trees, DifficultyIn, State) ->
    Difficulty = DifficultyIn + node_difficulty(Node),
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
        true  -> {ok, aec_block_genesis:populated_trees()};
        false -> db_find_state(prev_hash(Node))
    end.

apply_and_store_state_trees(Node, TreesIn, #{currently_adding := Hash}) ->
    NodeHash = hash(Node),
    try
        Trees = apply_node_transactions(Node, TreesIn),
        assert_state_hash_valid(Trees, Node),
        assert_previous_height(Node),
        assert_calculated_target(Node),
        ok = db_put_state(hash(Node), Trees),
        {ok, Trees}
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
    case aec_tx:apply_signed_strict(Txs, Trees, Height) of
        {ok, _, NewTrees} -> NewTrees;
        {error,_What} -> internal_error(invalid_transactions_in_block)
    end.

is_node_in_main_chain(Node, State) ->
    case get_hash_at_height(node_height(Node), State) of
        error -> false;
        {ok, Hash} -> hash(Node) =:= Hash
    end.

get_N_nodes_time_summary(TopNode, State, N) ->
    Time = node_time(TopNode),
    Difficulty = node_difficulty(TopNode),
    case node_is_genesis(TopNode, State) of
        true ->
            [{node_height(TopNode), Time, Difficulty}];
        false ->
            PrevHash = prev_hash(TopNode),
            case db_find_node(PrevHash) of
                error ->
                    [];
                {ok, PrevNode} ->
                    Summary = get_N_nodes_time_summary(PrevNode, Time,
                                                       Difficulty, State, [], N),
                    lists:reverse(Summary)
            end

    end.

get_N_nodes_time_summary(_Node, _ParentTime, _ParentDifficulty,  _State, Acc, 0) ->
    Acc;
get_N_nodes_time_summary(Node, ParentTime, ParentDifficulty, State, Acc0, N) ->
    Height = node_height(Node),
    Time = node_time(Node),
    Difficulty = node_difficulty(Node),
    Acc = [{Height + 1, ParentTime, ParentTime - Time, ParentDifficulty} | Acc0],
    case node_is_genesis(Node, State) of
        true ->
            [{Height, Time, Difficulty} | Acc];
        false ->
            PrevHash = prev_hash(Node),
            case db_find_node(PrevHash) of
                error ->
                    Acc;
                {ok, PrevNode} ->
                    get_N_nodes_time_summary(PrevNode, Time, Difficulty, State, Acc, N - 1)
            end
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
%%% Internal interface for the db
%%%-------------------------------------------------------------------

db_put_node(#block{} = Block) ->
    ok = aec_db:write_block(Block);
db_put_node(#header{} = Header) ->
    ok = aec_db:write_header(Header).

db_find_node(Hash) ->
    case aec_db:find_chain_node(Hash) of
        {Type, Header} -> {ok, wrap_node(Type, Header)};
        none -> error
    end.

db_get_node(Hash) ->
    {ok, Node} = db_find_node(Hash),
    Node.

db_put_state(Hash, Trees) ->
    Trees1 = aec_trees:commit_to_db(Trees),
    ok = aec_db:write_block_state(Hash, Trees1).

db_find_state(Hash) ->
    case aec_db:find_block_state(Hash) of
        {value, Trees} -> {ok, Trees};
        none -> error
    end.

db_get_txs(Hash) ->
    aec_blocks:txs(aec_db:get_block(Hash)).

db_get_prev_hash(Hash) ->
    {value, PrevHash} = db_find_prev_hash(Hash),
    PrevHash.

db_find_prev_hash(Hash) ->
    case db_find_node(Hash) of
        {ok, Node} -> {value, prev_hash(Node)};
        error -> none
    end.

db_children(Node) ->
    Hash = hash(Node),
    [wrap_node(Type, Header)
     || {Type, Header} <- aec_db:find_chain_node_successors(Hash)].
