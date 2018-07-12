%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of the chain service
%%% @end
%%%-------------------------------------------------------------------

-module(aec_chain_state).

-export([ calculate_state_for_new_keyblock/3
        , find_common_ancestor/2
        , get_key_block_hash_at_height/1
        , get_n_key_headers_backward_from/2
        , hash_is_connected_to_genesis/1
        , hash_is_in_main_chain/1
        , insert_block/1
        ]).

%% For tests
-export([ get_top_block_hash/1
        , get_key_block_hash_at_height/2
        , calculate_gas_fee/1
        ]).


-include("blocks.hrl").

-define(internal_error(____E____), {aec_chain_state_error, ____E____}).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_key_block_hash_at_height(aec_blocks:height()) -> {'ok', binary()} | 'error'.
get_key_block_hash_at_height(Height) when is_integer(Height), Height >= 0 ->
    get_key_block_hash_at_height(Height, new_state_from_persistence()).

-spec get_n_key_headers_backward_from(aec_headers:header(), non_neg_integer()) ->
        {ok, [#header{}]} | 'error'.
get_n_key_headers_backward_from(Header, N) ->
    Node = wrap_header(Header),
    get_n_key_headers_from(Node, N).

-spec insert_block(#block{} | map()) -> 'ok' | {'error', any()}.
insert_block(#{ key_block := KeyBlock, micro_blocks := MicroBlocks, dir := forward }) ->
    %% First insert key_block
    case insert_block(KeyBlock) of
        ok ->
            lists:foldl(fun(MB, ok) -> insert_block(MB);
                           (_MB, Err = {error, _}) -> Err
                        end, ok, MicroBlocks);
        Err = {error, _} ->
            Err
    end;
insert_block(#{ key_block := KeyBlock, micro_blocks := MicroBlocks, dir := backward }) ->
    %% First insert micro_blocks
    case lists:foldl(fun(MB, ok) -> insert_block(MB);
                        (_MB, Err = {error, _}) -> Err
                     end, ok, MicroBlocks) of
        ok ->
            insert_block(KeyBlock);
        Err = {error, _} ->
            Err
    end;
insert_block(Block) ->
    Node = wrap_block(Block),
    try internal_insert(Node, Block)
    catch throw:?internal_error(What) -> {error, What}
    end.

-spec hash_is_connected_to_genesis(binary()) -> boolean().
hash_is_connected_to_genesis(Hash) ->
    case db_find_fork_id(Hash) of
        {ok,_ForkId} -> true;
        error -> false
    end.

-spec find_common_ancestor(binary(), binary()) ->
                                  {'ok', binary()} | {error, atom()}.
find_common_ancestor(Hash1, Hash2) ->
    case {db_find_node(Hash1), db_find_node(Hash2)} of
        {{ok,_Node1}, {ok,_Node2}} ->
            case find_fork_point(Hash1, Hash2) of
                error          -> {error, not_found};
                {ok, ForkHash} -> {ok, ForkHash}
            end;
        _ -> {error, unknown_hash}
    end.

-spec hash_is_in_main_chain(binary()) -> boolean().
hash_is_in_main_chain(Hash) ->
    case db_find_node(Hash) of
        {ok,_Node} ->
            State = new_state_from_persistence(),
            case get_top_block_hash(State) of
                undefined -> false;
                TopHash -> hash_is_in_main_chain(Hash, TopHash)
            end;
        error -> false
    end.


-spec calculate_state_for_new_keyblock(binary(), aec_keys:pubkey(), aec_keys:pubkey()) ->
                                              {'ok', aec_trees:trees()}
                                            | 'error'.

calculate_state_for_new_keyblock(PrevHash, Miner, Beneficiary) ->
    case db_find_node(PrevHash) of
        error -> error;
        {ok, PrevNode} ->
            Node  = fake_key_node(PrevNode, node_height(PrevNode) + 1, Miner, Beneficiary),
            State = new_state_from_persistence(),
            case get_state_trees_in(Node, State) of
                error -> error;
                {ok, TreesIn, ForkInfoIn} ->
                    {Trees,_Fees} = apply_node_transactions(Node, TreesIn,
                                                            ForkInfoIn, State),
                    {ok, Trees}
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

new_state_from_persistence() ->
    Fun = fun() ->
                  #{ type                  => ?MODULE
                   , top_block_hash        => aec_db:get_top_block_hash()
                   , genesis_block_hash    => aec_db:get_genesis_hash()
                   }
          end,
    aec_db:ensure_transaction(Fun).

persist_state(State) ->
    case get_genesis_hash(State) of
        undefined -> ok;
        GenesisHash ->
            aec_db:write_genesis_hash(GenesisHash),
            case get_top_block_hash(State) of
                undefined -> ok;
                TopBlockHash ->
                    aec_db:write_top_block_hash(TopBlockHash)
            end
    end.

-spec internal_error(_) -> no_return().

internal_error(What) ->
    throw(?internal_error(What)).

get_genesis_hash(#{genesis_block_hash := GH}) -> GH.

get_top_block_hash(#{top_block_hash := H}) -> H.
set_top_block_hash(H, State) when is_binary(H) -> State#{top_block_hash => H}.

%%%-------------------------------------------------------------------
%%% Internal ADT for differing between blocks and headers
%%%-------------------------------------------------------------------

-record(node, { header  :: #header{}
              , hash    :: binary()
              , type    :: block_type()
              , key_hash :: binary()
              }).

hash(#node{hash = Hash}) -> Hash.

prev_hash(#node{header = H}) -> aec_headers:prev_hash(H).

node_height(#node{header = H}) -> aec_headers:height(H).

node_version(#node{header = H}) -> aec_headers:version(H).

node_difficulty(#node{type = micro}) -> 0;
node_difficulty(#node{header = H}) -> aec_headers:difficulty(H).

node_target(#node{header = H}) -> aec_headers:target(H).

node_root_hash(#node{header = H}) -> aec_headers:root_hash(H).

node_miner(#node{header = H}) -> aec_headers:miner(H).

node_beneficiary(#node{header = H}) -> aec_headers:beneficiary(H).

node_type(#node{type = T}) -> T.

node_key_hash(#node{key_hash = KeyHash}) -> KeyHash.

node_time(#node{header = H}) -> aec_headers:time_in_msecs(H).

is_key_block(N) -> node_type(N) =:= key.

is_micro_block(N) -> node_type(N) =:= micro.

maybe_add_genesis_hash(#{genesis_block_hash := undefined} = State, Node) ->
    case node_height(Node) =:= aec_block_genesis:height() of
        true  -> State#{genesis_block_hash => hash(Node)};
        false -> State
    end;
maybe_add_genesis_hash(State,_Node) ->
    State.

%% NG-INFO: microblock cannot be a genesis block
assert_not_new_genesis(#node{type = micro}, #{genesis_block_hash := undefined}) ->
    internal_error(rejecting_micro_genesis_block);
assert_not_new_genesis(#node{type = micro}, _) -> ok;
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

wrap_block(Block) ->
    Header = aec_blocks:to_header(Block),
    {ok, Hash} = aec_headers:hash_header(Header),
    #node{ header = Header
         , hash = Hash
         , type = aec_blocks:type(Block)
         , key_hash = aec_headers:key_hash(Header)
         }.

fake_key_node(PrevNode, Height, Miner, Beneficiary) ->
    Block = aec_blocks:new_key(Height,
                               hash(PrevNode),
                               <<123:?STATE_HASH_BYTES/unit:8>>,
                               node_target(PrevNode),
                               0, aeu_time:now_in_msecs(),
                               ?PROTOCOL_VERSION,
                               Miner,
                               Beneficiary),
    wrap_header(aec_blocks:to_header(Block)).

wrap_header(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    #node{ header = Header
         , hash = Hash
         , type = aec_headers:type(Header)
         , key_hash = aec_headers:key_hash(Header)
         }.

export_header(#node{header = Header}) ->
    Header.

%% NOTE: Only return nodes in the main chain.
%%       The function assumes that a node is in the main chain if
%%       there is only one node at that height, and the height is lower
%%       than the current top.
get_key_block_hash_at_height(Height, State) when is_integer(Height), Height >= 0 ->
    case get_top_block_hash(State) of
        undefined -> error;
        Hash ->
            TopNode = db_get_node(Hash),
            TopHeight = node_height(TopNode),
            case Height > TopHeight of
                true  -> error;
                false ->
                    case db_find_nodes_at_height(Height) of
                        error -> error({broken_chain, Height});
                        {ok, [Node]} -> {ok, hash(Node)};
                        {ok, [_|_] = Nodes} ->
                            keyblock_hash_in_main_chain(Nodes, Hash)
                    end
            end
    end.

%% TODO: this function relies that both key blocks and micro blocks share the
%% same index; this can change when they are indexed separately
keyblock_hash_in_main_chain([Node|Left], TopHash) ->
    case is_key_block(Node) andalso hash_is_in_main_chain(hash(Node), TopHash) of
        true  -> {ok, hash(Node)};
        false -> keyblock_hash_in_main_chain(Left, TopHash)
    end;
keyblock_hash_in_main_chain([],_TopHash) ->
    error.

hash_is_in_main_chain(Hash, TopHash) ->
    case find_fork_point(Hash, TopHash) of
        {ok, Hash} -> true;
        {ok, _} -> false;
        error -> false
    end.

%%%-------------------------------------------------------------------
%%% Chain operations
%%%-------------------------------------------------------------------

internal_insert(Node, Block) ->
    case db_find_node(hash(Node)) of
        error ->
            %% To preserve the invariants of the chain,
            %% Only add the block if we can do the whole
            %% transitive operation (i.e., calculate all the state
            %% trees, and update the pointers)
            Fun = fun() ->
                          State = new_state_from_persistence(),
                          %% Keep track of which node we are actually
                          %% adding to avoid giving spurious error
                          %% messages.
                          State1 = State#{ currently_adding => hash(Node)},
                          assert_not_new_genesis(Node, State1),
                          ok = db_put_node(Block, hash(Node)),
                          State2 = update_state_tree(Node, maybe_add_genesis_hash(State1, Node)),
                          persist_state(State2),
                          ok
                  end,
            try aec_db:ensure_transaction(Fun)
            catch exit:{aborted, {throw, ?internal_error(What)}} -> internal_error(What)
            end;
        {ok, Node} -> ok;
        {ok, Old} -> internal_error({same_key_different_content, Node, Old})
    end.

%% NG-INFO: micro blocks inherit the height from the last key block
assert_previous_height(PrevNode, Node) ->
    case is_key_block(Node) of
        true ->
            case node_height(PrevNode) =:= (node_height(Node) - 1) of
                true -> ok;
                false -> internal_error(height_inconsistent_for_keyblock_with_previous_hash)
            end;
        false ->
            case node_height(PrevNode) =:= node_height(Node) of
                true -> ok;
                false -> internal_error(height_inconsistent_for_microblock_with_previous_hash)
            end
    end.

%% To assert key block target calculation we need DeltaHeight headers counted
%% backwards from the node we want to assert.
assert_calculated_target(Node) ->
    case is_key_block(Node) of
        true  -> assert_key_block_target(Node);
        false -> ok
    end.

assert_key_block_target(Node) ->
    case db_find_node(prev_hash(Node)) of
        error -> ok;
        {ok, PrevNode} ->
            Delta         = aec_governance:key_blocks_to_check_difficulty_count(),
            Height        = node_height(Node),
            GenesisHeight = aec_headers:height(aec_block_genesis:genesis_header()),
            case Delta >= Height - GenesisHeight of
                true ->
                    %% We only need to verify that the target is equal to its predecessor.
                    assert_target_equal_to_prev(Node, PrevNode);
                false ->
                    assert_calculated_target(Node, PrevNode, Delta)
            end
    end.

assert_target_equal_to_prev(Node, PrevNode) ->
    PrevKeyNode = case is_key_block(PrevNode) of
                      true  ->
                          PrevNode;
                      false ->
                          {ok, KeyNode} = db_find_node(node_key_hash(PrevNode)),
                          KeyNode
                  end,
    case {node_target(Node), node_target(PrevKeyNode)} of
        {X, X} -> ok;
        {X, Y} -> internal_error({target_not_equal_to_parent, Node, X, Y})
    end.

assert_calculated_target(Node, PrevNode, Delta) ->
    {ok, Headers} = get_n_key_headers_from(PrevNode, Delta),
    case aec_target:verify(export_header(Node), Headers) of
        ok -> ok;
        {error, {wrong_target, Actual, Expected}} ->
            internal_error({wrong_target, Node, Actual, Expected})
    end.

get_n_key_headers_from(Node, N) ->
    get_n_key_headers_from({ok, Node}, N, []).

get_n_key_headers_from(_, 0, Acc) ->
    {ok, Acc};
get_n_key_headers_from({ok, Node}, N, Acc) ->
    case node_type(Node) of
        key ->
            PrevNode = db_find_node(prev_hash(Node)),
            get_n_key_headers_from(PrevNode, N-1, [export_header(Node) | Acc]);
        micro ->
            PrevKeyNode = db_find_node(node_key_hash(Node)),
            get_n_key_headers_from(PrevKeyNode, N, Acc)
    end;
get_n_key_headers_from(error, _N, _Acc) ->
    error.

assert_micro_block_time(PrevNode, Node) ->
    case is_micro_block(Node) of
        true ->
            case time_diff_greater_than_minimal(Node, PrevNode) of
                true  -> ok;
                false -> ok %internal_error(micro_block_time_too_low)
            end;
        false -> ok
    end.

time_diff_greater_than_minimal(Node, PrevNode) ->
    node_time(Node) >= node_time(PrevNode) + aec_governance:micro_block_cycle().

assert_micro_block_key_hash(PrevNode, Node) ->
    case is_micro_block(Node) of
        true ->
            CompareHash = case node_type(PrevNode) of
                              key   -> hash(PrevNode);
                              micro -> node_key_hash(PrevNode)
                          end,
            case node_key_hash(Node) =:= CompareHash of
                true  -> ok;
                false -> internal_error(wrong_key_hash)
            end;
        false -> ok
    end.

assert_micro_signature(PrevNode, Node) ->
    case is_micro_block(Node) of
        true ->
            {ok, KeyNode} =
                case node_type(PrevNode) of
                    key   -> {ok, PrevNode};
                    micro -> db_find_node(node_key_hash(Node))
                end,
            Bin = aec_headers:serialize_to_binary(export_header(Node)),
            Sig = db_get_signature(hash(Node)),
            case enacl:sign_verify_detached(Sig, Bin, node_miner(KeyNode)) of
                {ok, _}    -> ok;
                {error, _} -> {error, signature_verification_failed}
            end;
        false ->
            ok
    end.

%% Transitively compute new state trees iff
%%   - We can find the state trees of the previous node; and
%%   - The new node is a block.
%%
%% This should be called on the newly added node.
%% It will fail if called on a node that already has its state computed.

-record(fork_info, { fork_id
                   , difficulty
                   , fees
                   }).

update_state_tree(Node, State) ->
    case get_state_trees_in(Node, State) of
        error -> State;
        {ok, Trees, ForkInfoIn} ->
            ForkInfo =
                case node_is_genesis(Node, State) of
                    true  -> ForkInfoIn;
                    false ->
                        case db_node_has_sibling_blocks(Node) of
                            true  -> ForkInfoIn#fork_info{fork_id = hash(Node)};
                            false -> ForkInfoIn
                        end
                end,
            {State1, NewTopDifficulty} =
                update_state_tree(Node, Trees, ForkInfo, State),
            OldTopHash = get_top_block_hash(State),
            handle_top_block_change(OldTopHash, NewTopDifficulty, State1)
    end.

update_state_tree(Node, TreesIn, ForkInfo, State) ->
    case db_find_state(hash(Node)) of
        {ok,_Trees,_ForkInfo} ->
            error({found_already_calculated_state, hash(Node)});
        error ->
            case apply_and_store_state_trees(Node, TreesIn, ForkInfo, State) of
                {ok, TreesOut, ForkInfoOut} ->
                    update_next_state_tree(Node, TreesOut, ForkInfoOut, State);
                error ->
                    {State, ForkInfo#fork_info.difficulty}
            end
    end.

update_next_state_tree(Node, Trees, ForkInfo, State) ->
    Hash = hash(Node),
    State1 = set_top_block_hash(Hash, State),
    case db_children(Node) of
        [] -> {State1, ForkInfo#fork_info.difficulty};
        [Child|Left] ->
            %% If there is only one child, it inherits the fork id.
            %% For more than one child, we neeed new fork_ids, which are
            %% the first node hash of each new fork.
            Children = [{Child, ForkInfo}|
                        [{C, ForkInfo#fork_info{fork_id = hash(C)}}
                         || C <- Left]
                       ],
            Difficulty = ForkInfo#fork_info.difficulty,
            update_next_state_tree_children(Children, Trees, Difficulty, State1)
    end.

update_next_state_tree_children([],_Trees, Max, State) ->
    {State, Max};
update_next_state_tree_children([{Child, ForkInfo}|Left], Trees, Max, State) ->
    {State1, Max1} = update_state_tree(Child, Trees, ForkInfo, State),
    case Max1 > Max of
        true ->
            update_next_state_tree_children(Left, Trees, Max1, State1);
        false ->
            State2 = set_top_block_hash(get_top_block_hash(State), State1),
            update_next_state_tree_children(Left, Trees, Max, State2)
    end.

get_state_trees_in(Node, State) ->
    case node_is_genesis(Node, State) of
        true  ->
            {ok,
             aec_block_genesis:populated_trees(),
             #fork_info{ difficulty = aec_block_genesis:genesis_difficulty()
                       , fork_id = hash(Node)
                       , fees = 0
                       }
            };
        false ->
            PrevHash = prev_hash(Node),
            case db_find_state(PrevHash) of
                {ok, Trees, ForkInfo} ->
                    %% Reset accumulated fees if the previous block is a key block
                    %% to start accumulating the next generation.
                    case node_type(db_get_node(PrevHash)) of
                        key   -> {ok, Trees, ForkInfo#fork_info{fees = 0}};
                        micro -> {ok, Trees, ForkInfo}
                    end;
                error -> error
            end
    end.

apply_and_store_state_trees(#node{hash = NodeHash} = Node, TreesIn, ForkInfoIn,
                            #{currently_adding := Hash} = State) ->
    try
        case db_find_node(prev_hash(Node)) of
            error ->
                %% This must be the genesis node
                ok;
            {ok, PrevNode} ->
                assert_previous_height(PrevNode, Node),
                assert_calculated_target(Node),
                assert_micro_block_time(PrevNode, Node),
                assert_micro_block_key_hash(PrevNode, Node),
                assert_micro_signature(PrevNode, Node)
        end,
        {Trees, Fees} = apply_node_transactions(Node, TreesIn, ForkInfoIn, State),
        assert_state_hash_valid(Trees, Node),
        DifficultyOut = ForkInfoIn#fork_info.difficulty
            + node_difficulty(Node),
        FeesInNode = Fees + ForkInfoIn#fork_info.fees,
        ForkInfoInNode = ForkInfoIn#fork_info{ fees = FeesInNode
                                             , difficulty = DifficultyOut
                                             },
        ok = db_put_state(hash(Node), Trees, ForkInfoInNode),
        case node_type(Node) of
            key   -> {ok, Trees, ForkInfoInNode#fork_info{fees = 0}};
            micro -> {ok, Trees, ForkInfoInNode}
        end
    catch
        %% Only catch this if the current node is NOT the one added in
        %% the call. We don't want to give an error message for any
        %% other node that that. But we want to make progress in the
        %% chain state even if a successor or predecessor to the
        %% currently added node is faulty.
        throw:?internal_error(_) when NodeHash =/= Hash -> error
    end.

handle_top_block_change(OldTopHash, NewTopDifficulty, State) ->
    case get_top_block_hash(State) of
        OldTopHash -> State;
        NewTopHash when OldTopHash =:= undefined ->
            update_main_chain(get_genesis_hash(State), NewTopHash, State);
        NewTopHash ->
            {ok, ForkHash} = find_fork_point(OldTopHash, NewTopHash),
            case ForkHash =:= OldTopHash of
                true ->
                    %% We are extending the current chain.
                    %% Difficulty might not have changed if it is an
                    %% extension of micro blocks.
                    update_main_chain(OldTopHash, NewTopHash, ForkHash, State);
                false ->
                    %% We have a fork. Compare the difficulties.
                    {ok, OldTopDifficulty} = db_find_difficulty(OldTopHash),
                    case OldTopDifficulty >= NewTopDifficulty of
                        true -> set_top_block_hash(OldTopHash, State); %% Reset
                        false -> update_main_chain(OldTopHash, NewTopHash,
                                                   ForkHash, State)
                    end
            end
    end.

update_main_chain(OldTopHash, NewTopHash, State) ->
    {ok, ForkHash} = find_fork_point(OldTopHash, NewTopHash),
    update_main_chain(OldTopHash, NewTopHash, ForkHash, State).

update_main_chain(OldTopHash, NewTopHash, ForkHash, State) ->
    case OldTopHash =:= ForkHash of
        true ->
            add_locations(OldTopHash, NewTopHash),
            State;
        false ->
            remove_locations(ForkHash, OldTopHash),
            add_locations(ForkHash, NewTopHash),
            State
    end.

remove_locations(Hash, Hash) ->
    ok;
remove_locations(StopHash, CurrentHash) ->
    lists:foreach(fun(TxHash) ->
                          aec_db:remove_tx_location(TxHash),
                          aec_db:add_tx_hash_to_mempool(TxHash)
                  end, db_get_tx_hashes(CurrentHash)),
    remove_locations(StopHash, db_get_prev_hash(CurrentHash)).

add_locations(Hash, Hash) ->
    ok;
add_locations(StopHash, CurrentHash) ->
    lists:foreach(fun(TxHash) ->
                          aec_db:add_tx_location(TxHash, CurrentHash),
                          aec_db:remove_tx_from_mempool(TxHash)
                  end, db_get_tx_hashes(CurrentHash)),
    add_locations(StopHash, db_get_prev_hash(CurrentHash)).


assert_state_hash_valid(Trees, Node) ->
    RootHash = aec_trees:hash(Trees),
    Expected = node_root_hash(Node),
    case RootHash =:= Expected of
        true -> ok;
        false -> internal_error({root_hash_mismatch, RootHash, Expected})
    end.

apply_node_transactions(Node, Trees, #fork_info{fees = FeesIn}, State) ->
    case is_micro_block(Node) of
        true ->
            apply_micro_block_transactions(Node, FeesIn, Trees);
        false ->
            GasFees = calculate_gas_fee(aec_trees:calls(Trees)),
            TotalFees = GasFees + FeesIn,
            Trees1 = aec_trees:perform_pre_transformations(Trees, node_height(Node)),
            Delay  = aec_governance:miner_reward_delay(),
            case node_height(Node) > aec_block_genesis:height() + Delay of
                true  -> {grant_fees(Node, Trees1, Delay, TotalFees, State), TotalFees};
                false -> {Trees1, TotalFees}
            end
    end.

find_prev_key_nodes(Node, N) when N > 0 ->
    find_prev_key_nodes({ok, Node}, N, none).

find_prev_key_nodes(error,_N,_Acc) ->
    error;
find_prev_key_nodes({ok, Node}, N, Acc) ->
    case node_type(Node) of
        key when N =:= 0 ->
            {Node, Acc};
        key when N =:= 1 ->
            find_prev_key_nodes(db_find_node(prev_hash(Node)), N - 1, Node);
        key ->
            find_prev_key_nodes(db_find_node(prev_hash(Node)), N - 1, Acc);
        micro ->
            find_prev_key_nodes(db_find_node(node_key_hash(Node)), N, Acc)
    end.

grant_fees(Node, Trees, Delay, Fees, State) ->
    {KeyNode1, KeyNode2} = find_prev_key_nodes(Node, Delay + 1),
    KeyFees = case Node =:= KeyNode2 of
                  true  -> Fees;
                  false -> db_get_fees(hash(KeyNode2))
              end,
    Beneficiary1 = node_beneficiary(KeyNode1),
    Beneficiary2 = node_beneficiary(KeyNode2),
    Reward = aec_governance:block_mine_reward(),
    BeneficiaryReward1 = round(KeyFees * 0.4),
    BeneficiaryReward2 = KeyFees - BeneficiaryReward1 + Reward,
    Trees1 = aec_trees:grant_fee(Beneficiary2, Trees, BeneficiaryReward2),
    case node_is_genesis(KeyNode1, State) of
        true  -> Trees1;
        false -> aec_trees:grant_fee(Beneficiary1, Trees1, BeneficiaryReward1)
    end.

calculate_gas_fee(Calls) ->
    F = fun(_, SerCall, GasFeeIn) ->
                Call = aect_call:deserialize(SerCall),
                GasFee = aect_call:gas_used(Call) * aect_call:gas_price(Call),
                GasFee + GasFeeIn
        end,
    aeu_mtrees:fold(F, 0, aect_call_state_tree:iterator(Calls)).


apply_micro_block_transactions(Node, FeesIn, Trees) ->
    Txs = db_get_txs(hash(Node)),
    Height = node_height(Node),
    Version = node_version(Node),
    TotalFees = lists:foldl(
                  fun(SignedTx, AccFee) ->
                          Fee = aetx:fee(aetx_sign:tx(SignedTx)),
                          AccFee + Fee
                  end, FeesIn, Txs),

    case aec_block_micro_candidate:apply_block_txs_strict(Txs, Trees, Height, Version) of
        {ok, _, NewTrees} -> {NewTrees, TotalFees};
        {error,_What} -> internal_error(invalid_transactions_in_block)
    end.

find_fork_point(Hash1, Hash2) ->
    find_fork_point(Hash1, db_find_fork_id(Hash1), Hash2, db_find_fork_id(Hash2)).

find_fork_point(Hash,  {ok, FHash}, Hash,  {ok, FHash}) ->
    {ok, Hash};
find_fork_point(Hash1, {ok, FHash}, Hash2, {ok, FHash}) ->
    Height1 = node_height(db_get_node(Hash1)),
    Height2 = node_height(db_get_node(Hash2)),
    if
        Height1  >  Height2 -> {ok, Hash2};
        Height1  <  Height2 -> {ok, Hash1};
        Height1 =:= Height2 -> find_micro_fork_point(Hash1, Hash2)
    end;
find_fork_point(Hash1, {ok, FHash1}, Hash2, {ok, FHash2}) ->
    Height1 = node_height(db_get_node(FHash1)),
    Height2 = node_height(db_get_node(FHash2)),
    if
        Height1 >= Height2 ->
            PrevHash = db_get_prev_hash(FHash1),
            PrevRes = db_find_fork_id(PrevHash),
            find_fork_point(PrevHash, PrevRes, Hash2, {ok, FHash2});
        Height2 > Height1 ->
            PrevHash = db_get_prev_hash(FHash2),
            PrevRes = db_find_fork_id(PrevHash),
            find_fork_point(Hash1, {ok, FHash1}, PrevHash, PrevRes)
    end;
find_fork_point(_Hash1, _Res1,_Hash2,_Res2) ->
    error.

find_micro_fork_point(Hash1, Hash2) ->
    case do_find_micro_fork_point(Hash1, Hash2) of
        error ->
            case do_find_micro_fork_point(Hash2, Hash1) of
                {ok, _} = Res -> Res;
                error -> error(illegal_micro_fork)
            end;
        {ok, _} = Res ->
            Res
    end.

do_find_micro_fork_point(Hash, Hash)   -> {ok, Hash};
do_find_micro_fork_point(Hash1, Hash2) ->
    case is_key_block(db_get_node(Hash1)) of
        true  -> error;
        false -> do_find_micro_fork_point(db_get_prev_hash(Hash1), Hash2)
    end.

%%%-------------------------------------------------------------------
%%% Internal interface for the db
%%%-------------------------------------------------------------------

db_put_node(#block{} = Block, Hash) when is_binary(Hash) ->
    ok = aec_db:write_block(Block).

%% NG-INFO Heigh/Hash queries have sense in context of key blocks. For non-key height = 0
db_find_node(Hash) when is_binary(Hash) ->
    case aec_db:find_header(Hash) of
        {value, Header} -> {ok, wrap_header(Header)};
        none -> error
    end.

db_get_node(Hash) when is_binary(Hash) ->
    {ok, Node} = db_find_node(Hash),
    Node.

%% NG-INFO Heigh/Hash queries have sense in context of key blocks. For non-key height = 0
db_find_nodes_at_height(Height) when is_integer(Height) ->
    case aec_db:find_headers_at_height(Height) of
        [_|_] = Headers ->
            {ok, lists:map(fun(Header) -> wrap_header(Header) end, Headers)};
        [] -> error
    end.

db_put_state(Hash, Trees, ForkInfo) when is_binary(Hash) ->
    #fork_info{ difficulty = Difficulty
              , fork_id    = ForkId
              , fees       = Fees
              } = ForkInfo,
    ok = aec_db:write_block_state(Hash, Trees, Difficulty, ForkId, Fees).

db_find_state(Hash) ->
    case aec_db:find_block_state_and_data(Hash) of
        {value, Trees, Difficulty, ForkId, Fees} ->
            {ok, Trees,
             #fork_info{ difficulty = Difficulty
                       , fork_id = ForkId
                       , fees = Fees
                       }
            };
        none -> error
    end.

db_find_difficulty(Hash) when is_binary(Hash) ->
    case aec_db:find_block_difficulty(Hash) of
        {value, Difficulty} -> {ok, Difficulty};
        none -> error
    end.

db_find_fork_id(Hash) when is_binary(Hash) ->
    case aec_db:find_block_fork_id(Hash) of
        {value, ForkId} -> {ok, ForkId};
        none -> error
    end.

db_get_txs(Hash) when is_binary(Hash) ->
    aec_blocks:txs(aec_db:get_block(Hash)).

db_get_signature(Hash) when is_binary(Hash) ->
    aec_db:get_block_signature(Hash).

db_get_fees(Hash) when is_binary(Hash) ->
    {value, Fees} = aec_db:find_block_fees(Hash),
    Fees.

db_get_tx_hashes(Hash) when is_binary(Hash) ->
    aec_db:get_block_tx_hashes(Hash).

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
    %% NOTE: Micro blocks have the same height.
    [wrap_header(Header)
     || Header <- aec_db:find_headers_at_height(Height + 1)
            ++ aec_db:find_headers_at_height(Height),
        aec_headers:prev_hash(Header) =:= Hash].

db_node_has_sibling_blocks(Node) ->
    Height   = node_height(Node),
    PrevHash = prev_hash(Node),
    %% NOTE: Micro blocks have the same height.
    length([1 || Header <- aec_db:find_headers_at_height(Height)
                     ++ aec_db:find_headers_at_height(Height - 1),
                 aec_headers:prev_hash(Header) =:= PrevHash]) > 1.

