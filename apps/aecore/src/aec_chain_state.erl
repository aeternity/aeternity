%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of the chain service
%%%
%%% This module is responsible for tracking the structure of the
%%% chain. The building blocks of the chain is abstracted to Nodes. A
%%% node is either a key block or a micro block.
%%%
%%% The chain starts with the genesis block and ends with the top_block.
%%%
%%% A node is only added to the chain (and the db) if it passes full
%%% validation. The full validation of a node can only be done if the
%%% full validation of its immediate predecessor (prev_hash) has
%%% passed. By transitivity, a node can only be validated if it has a
%%% fully validated chain to the genesis node. This also means that
%%% orphan nodes cannot be added to the chain (or to the db).
%%%
%%% When a node is added, it might create a fork, extend a fork, or
%%% extend the main chain. The fork with the highest accumulated
%%% difficulty is considered the main chain. Only key blocks add to
%%% the difficulty.
%%%
%%% If a micro block is added to the top of the chain, this is
%%% considered the top block, even if it doesn't increase the
%%% difficulty. If a micro block is added to a fork with the same
%%% difficulty as the main chain, the top block will not chain.
%%%
%%% Forks in the structure are labeled by fork id. The fork id is
%%% local to each Aeternity node instance and cannot be used to reason about
%%% structure across peers. The fork id is the hash of the first node
%%% in a fork in the local system. In particular, the genesis node's
%%% hash is the first fork id. A node inherits its parent's fork id,
%%% unless there is already a sibling with this fork id. In that case,
%%% a new fork id is created with the hash of the new node.
%%%
%%% Fork ids help reasoning about the chain structure since they allow
%%% for skipping to the beginning of a fork when traversing the
%%% chain. For example, if two nodes with the same fork id, one must
%%% be the ancestor of the other. If this is not the case, we can skip
%%% to the previous fork of the higher node and compare again.
%%%
%%% Information is accumulated in the chain and is stored
%%% corresponding to a node. For example, the accumulated difficulty,
%%% the fork id, the accumulated fee for a generation (between two
%%% key blocks) and the full state trees at a node. Currently, this
%%% information is not garbage collected in any way, and it is
%%% indefinitely retrievable. This might change in the future.
%%%
%%% When adding a node, we make a difference if the node was added
%%% through gossip or sync. A gossiped node is not allowed to be
%%% further below the current top than some height delta. This to
%%% prevent malicious nodes from creating ancient forks. Sync is
%%% making a slightly more informed decision when creating forks, so
%%% it is allowed to create older forks.
%%%
%%% Proof of fraud is tracked in two ways. We can find a proof of
%%% fraud when we add a micro block for which a sibling micro block is
%%% already present, and we need to track any reported proof of fraud.
%%%
%%% When we find a fraudulent micro block, we still accept it since we
%%% do not know which fork is the winning one, but we store the proof
%%% in the db, and report it back to the caller.
%%%
%%% The existence of a proof in the chain is tracked as a Boolean in
%%% the fork_info, and it is reset at the first micro block of a
%%% generation. Thus, we can look at the fraud info of the closing key
%%% block of a generation to know if fraud was reported in the
%%% generation. As the reported PoF was validated when encountered, we
%%% can trust the Boolean flag to be correct.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(aec_chain_state).

-export([ calculate_state_for_new_keyblock/3
        , find_common_ancestor/2
        , get_key_block_hash_at_height/1
        , get_n_key_headers_backward_from/2
        , hash_is_connected_to_genesis/1
        , hash_is_in_main_chain/1
        , hash_is_in_main_chain/2
        , insert_block/1
        , insert_block/2
        , gossip_allowed_height_from_top/0
        , proof_of_fraud_report_delay/0
        ]).

%% For tests
-export([ get_top_block_hash/1
        , get_key_block_hash_at_height/2
        , calculate_gas_fee/1
        ]).

-ifdef(TEST).
-export([calc_rewards/6]).
-endif.

-include_lib("aeminer/include/aeminer.hrl").
-include("blocks.hrl").

-type events() :: aetx_env:events().

-define(internal_error(____E____), {aec_chain_state_error, ____E____}).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_key_block_hash_at_height(aec_blocks:height()) -> {'ok', binary()} | 'error'.
get_key_block_hash_at_height(Height) when is_integer(Height), Height >= 0 ->
    get_key_block_hash_at_height(Height, new_state_from_persistence()).

-spec get_n_key_headers_backward_from(aec_headers:header(), non_neg_integer()) ->
                                             {ok, [aec_headers:header()]} | 'error'.
get_n_key_headers_backward_from(Header, N) ->
    Node = wrap_header(Header),
    get_n_key_headers_from(Node, N).

-spec insert_block(aec_blocks:block()) -> {'ok', events()}
                                        | {'pof', aec_pof:pof(), events()}
                                        | {'error', any()}.
insert_block(Block) ->
    do_insert_block(Block, undefined).

%% We should not check the height distance to top for synced block, so
%% we have to keep track of the origin here.
-spec insert_block(aec_blocks:block(), atom()) ->
        {'ok', events()}
      | {'pof', aec_pof:pof(), events()}
      | {'error', any()}.
insert_block(Block, block_synced) ->
    do_insert_block(Block, sync);
insert_block(Block, _Origin) ->
    do_insert_block(Block, undefined).

do_insert_block(Block, Origin) ->
    aec_blocks:assert_block(Block),
    Node = wrap_block(Block),
    try internal_insert(Node, Block, Origin)
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
                    {Trees,_Fees,_Events} = apply_node_transactions(Node, TreesIn,
                                                                    ForkInfoIn, State),
                    {ok, Trees}
            end
    end.

-define(DEFAULT_GOSSIP_ALLOWED_HEIGHT_FROM_TOP, 5).

gossip_allowed_height_from_top() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"gossip_allowed_height_from_top">>],
                               aecore, gossip_allowed_height_from_top,
                               ?DEFAULT_GOSSIP_ALLOWED_HEIGHT_FROM_TOP).

-define(POF_REPORT_DELAY, 2).

proof_of_fraud_report_delay() ->
    ?POF_REPORT_DELAY.

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

-record(node, { header :: aec_headers:header()
              , hash   :: binary()
              , type   :: block_type()
              }).

hash(#node{hash = Hash}) -> Hash.

version(#node{header = H}) -> aec_headers:version(H).

prev_hash(#node{header = H}) -> aec_headers:prev_hash(H).

prev_key_hash(#node{header = H}) -> aec_headers:prev_key_hash(H).

node_height(#node{header = H}) -> aec_headers:height(H).

node_difficulty(#node{type = micro}) -> 0;
node_difficulty(#node{header = H}) -> aec_headers:difficulty(H).

node_target(#node{header = H}) -> aec_headers:target(H).

node_root_hash(#node{header = H}) -> aec_headers:root_hash(H).

node_miner(#node{header = H}) -> aec_headers:miner(H).

node_beneficiary(#node{header = H}) -> aec_headers:beneficiary(H).

node_type(#node{type = T}) -> T.

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

maybe_add_pof(State, Block) ->
    case aec_blocks:type(Block) of
        key   -> State#{pof => no_fraud};
        micro -> State#{pof => aec_blocks:pof(Block)}
    end.

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
    wrap_header(Header).

fake_key_node(PrevNode, Height, Miner, Beneficiary) ->
    PrevKeyHash = case node_type(PrevNode) of
                      key   -> hash(PrevNode);
                      micro -> prev_key_hash(PrevNode)
                  end,
    Vsn = aec_hard_forks:protocol_effective_at_height(Height),
    Block = aec_blocks:new_key(Height,
                               hash(PrevNode),
                               PrevKeyHash,
                               <<123:?STATE_HASH_BYTES/unit:8>>,
                               ?HIGHEST_TARGET_SCI,
                               0, aeu_time:now_in_msecs(),
                               Vsn,
                               Miner,
                               Beneficiary),
    wrap_header(aec_blocks:to_header(Block)).

wrap_header(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    wrap_header(Header, Hash).

wrap_header(Header, Hash) ->
    #node{ header = Header
         , hash = Hash
         , type = aec_headers:type(Header)
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
                    case db_find_key_nodes_at_height(Height) of
                        error -> error({broken_chain, Height});
                        {ok, [Node]} -> {ok, hash(Node)};
                        {ok, [_|_] = Nodes} ->
                            keyblock_hash_in_main_chain(Nodes, Hash)
                    end
            end
    end.

keyblock_hash_in_main_chain([Node|Left], TopHash) ->
    case hash_is_in_main_chain(hash(Node), TopHash) of
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

internal_insert(Node, Block, Origin) ->
    case db_find_node(hash(Node)) of
        error ->
            %% To preserve the invariants of the chain,
            %% Only add the block if we can do the whole
            %% transitive operation (i.e., calculate all the state
            %% trees, and update the pointers)
            Fun = fun() -> internal_insert_transaction(Node, Block, Origin)
                  end,
            try aec_db:ensure_transaction(Fun)
            catch exit:{aborted, {throw, ?internal_error(What)}} -> internal_error(What)
            end;
        {ok, Node} -> ok;
        {ok, Old} -> internal_error({same_key_different_content, Node, Old})
    end.

internal_insert_transaction(Node, Block, Origin) ->
    State1 = new_state_from_persistence(),
    assert_not_new_genesis(Node, State1),
    State2 = maybe_add_pof(maybe_add_genesis_hash(State1, Node), Block),
    case node_is_genesis(Node, State2) of
        true ->
            ok;
        false ->
            assert_not_illegal_fork_or_orphan(Node, Origin, State2),
            PrevNode = db_get_node(prev_hash(Node)),
            assert_previous_height(PrevNode, Node),
            assert_previous_key_block_hash(PrevNode, Node),
            case node_type(Node) of
                key ->
                    KeyHeaders = assert_key_block_time_return_headers(Node),
                    assert_key_block_target(Node, KeyHeaders);
                micro ->
                    assert_micro_block_time(PrevNode, Node),
                    assert_micro_signature(PrevNode, Node),
                    assert_micro_pof(PrevNode, Node, Block)
            end
    end,
    ok = db_put_node(Block, hash(Node)),
    {State3, Events} = update_state_tree(Node, State2),
    persist_state(State3),
    case maps:get(found_pof, State3) of
        no_fraud  -> {ok, Events};
        PoF       -> {pof, PoF, Events}
    end.

assert_not_illegal_fork_or_orphan(Node, Origin, State) ->
    case node_is_genesis(Node, State) of
        true -> ok;
        false ->
            assert_connection_to_chain(Node),
            case Origin of
                sync -> ok;
                undefined -> assert_height_delta(Node, State)
            end
    end.

assert_connection_to_chain(Node) ->
    case hash_is_connected_to_genesis(prev_hash(Node)) of
        true  -> ok;
        false -> internal_error({illegal_orphan, hash(Node)})
    end.

assert_height_delta(Node, State) ->
    Top       = db_get_node(get_top_block_hash(State)),
    TopHeight = node_height(Top),
    Height    = node_height(Node),
    case Height >= TopHeight - gossip_allowed_height_from_top() of
        false -> internal_error({too_far_below_top, Height, TopHeight});
        true -> ok
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

assert_previous_key_block_hash(PrevNode, Node) ->
    case is_key_block(PrevNode) of
        true ->
            case hash(PrevNode) =:= prev_key_hash(Node) of
                true -> ok;
                false -> internal_error(prev_key_hash_inconsistency)
            end;
        false ->
            case prev_key_hash(PrevNode) =:= prev_key_hash(Node) of
                true -> ok;
                false -> internal_error(prev_key_hash_inconsistency)
            end
    end.

%% To assert key block target calculation we need DeltaHeight headers counted
%% backwards from the node we want to assert.
assert_key_block_target(Node, Headers) ->
    case db_find_node(prev_hash(Node)) of
        error -> ok;
        {ok, PrevNode} ->
            Delta         = aec_governance:key_blocks_to_check_difficulty_count() + 1,
            Height        = node_height(Node),
            GenesisHeight = aec_block_genesis:height(),
            case Delta >= Height - GenesisHeight of
                true ->
                    %% We only need to verify that the target is equal to its predecessor.
                    assert_target_equal_to_prev(Node, PrevNode);
                false ->
                    assert_calculated_target(Node, PrevNode, Delta, Headers)
            end
    end.

assert_target_equal_to_prev(Node, PrevNode) ->
    PrevKeyNode = case is_key_block(PrevNode) of
                      true  ->
                          PrevNode;
                      false ->
                          KeyHash = prev_key_hash(PrevNode),
                          {ok, KeyNode} = db_find_node(KeyHash),
                          KeyNode
                  end,
    case {node_target(Node), node_target(PrevKeyNode)} of
        {X, X} -> ok;
        {X, Y} -> internal_error({target_not_equal_to_parent, Node, X, Y})
    end.

assert_calculated_target(Node, PrevNode, Delta, HeadersIn) ->
    {ok, Headers} =
        case length(HeadersIn) of
            0 ->
                get_n_key_headers_from(PrevNode, Delta);
            N when N == Delta ->
                {ok, HeadersIn};
            N when N > Delta ->
                {ok, lists:nthtail(N - Delta, HeadersIn)};
            N when N < Delta ->
                NextHash = aec_headers:prev_key_hash(hd(HeadersIn)),
                NextNode = db_get_node(NextHash),
                get_n_key_headers_from({ok, NextNode}, Delta - N, HeadersIn)
        end,
    case aec_target:verify(export_header(Node), Headers) of
        ok -> ok;
        {error, {wrong_target, Actual, Expected}} ->
            internal_error({wrong_target, Node, Actual, Expected})
    end.

get_n_key_headers_from(Node, N) ->
    case node_type(Node) of
        key   ->
            get_n_key_headers_from({ok, Node}, N, []);
        micro ->
            get_n_key_headers_from(db_find_node(prev_key_hash(Node)), N, [])
    end.

get_n_key_headers_from(_, 0, Acc) ->
    {ok, Acc};
get_n_key_headers_from({ok, Node}, N, Acc) ->
    %% Assert
    key = node_type(Node),
    MaybePrevKeyNode = db_find_node(prev_key_hash(Node)),
    get_n_key_headers_from(MaybePrevKeyNode, N-1, [export_header(Node) | Acc]);
get_n_key_headers_from(error, _N, _Acc) ->
    error.

assert_key_block_time_return_headers(Node) ->
    Time = node_time(Node),
    case median_timestamp_and_headers(Node) of
        {ok, Median, Headers} when Time > Median -> Headers;
        {ok,_Median,_Headers} -> internal_error(key_block_from_the_past)
    end.

%% Compute the median timestamp for last aec_governance:median_timestamp_key_blocks()
median_timestamp_and_headers(Node) ->
    TimeStampKeyBlocks = aec_governance:median_timestamp_key_blocks(),
    case node_height(Node) =< TimeStampKeyBlocks of
        true ->
            {ok, aec_block_genesis:time_in_msecs(), []};
        false ->
            PrevKeyNode = db_get_node(prev_key_hash(Node)),
            case get_n_key_headers_from(PrevKeyNode, TimeStampKeyBlocks) of
                {ok, Headers} ->
                    Times = [aec_headers:time_in_msecs(H) || H <- Headers],
                    {ok, median(Times), Headers};
                error ->
                    error
            end
    end.

assert_micro_block_time(PrevNode, Node) ->
    case is_micro_block(Node) of
        true ->
            case is_micro_block(PrevNode) of
                true ->
                    case time_diff_greater_than_minimal(Node, PrevNode) of
                        true  -> ok;
                        false -> internal_error(micro_block_time_too_low)
                    end;
                false ->
                    case node_time(Node) > node_time(PrevNode) of
                        true  -> ok;
                        false -> internal_error(micro_block_time_too_low)
                    end
            end;
        false -> ok
    end.

time_diff_greater_than_minimal(Node, PrevNode) ->
    node_time(Node) >= node_time(PrevNode) + aec_governance:micro_block_cycle().

assert_micro_signature(PrevNode, Node) ->
    case is_micro_block(Node) of
        true ->
            {ok, KeyNode} =
                case node_type(PrevNode) of
                    key   -> {ok, PrevNode};
                    micro -> db_find_node(prev_key_hash(Node))
                end,
            case aeu_sig:verify(export_header(Node), node_miner(KeyNode)) of
                ok         -> ok;
                {error, _} -> internal_error(signature_verification_failed)
            end;
        false ->
            ok
    end.

assert_micro_pof(PrevNode, Node, Block) ->
    case is_micro_block(Node) of
        false ->
            ok;
        true ->
            case aec_blocks:pof(Block) of
                no_fraud ->
                    ok;
                Pof ->
                    FraudNode = get_fraud_node(PrevNode, Node),
                    case aec_pof:pubkey(Pof) =:= node_miner(FraudNode) of
                        true ->
                            ok;
                        false ->
                            internal_error(wrong_pubkey_in_pof)
                    end
            end
    end.

get_fraud_node(PrevNode, Node) ->
    case node_type(PrevNode) of
        micro ->
            PrevKey = db_get_node(prev_key_hash(Node)),
            db_get_node(prev_key_hash(PrevKey));
        key ->
            db_get_node(prev_key_hash(PrevNode))
    end.

-record(fork_info, { fork_id
                   , difficulty
                   , fees
                   , fraud
                   }).

update_state_tree(Node, State) ->
    {ok, Trees, ForkInfoIn} = get_state_trees_in(Node, State),
    {ForkInfo, MicSibHeaders} = maybe_set_new_fork_id(Node, ForkInfoIn, State),
    State1 = update_found_pof(Node, MicSibHeaders, State),
    {State2, NewTopDifficulty, Events} = update_state_tree(Node, Trees, ForkInfo, State1),
    OldTopHash = get_top_block_hash(State),
    handle_top_block_change(OldTopHash, NewTopDifficulty, Events, State2).

update_state_tree(Node, TreesIn, ForkInfo, State) ->
    case db_find_state(hash(Node)) of
        {ok,_Trees,_ForkInfo} ->
            %% NOTE: This is an internal inconsistency check,
            %% so don't use internal_error
            error({found_already_calculated_state, hash(Node)});
        error ->
            {DifficultyOut, Events} = apply_and_store_state_trees(Node, TreesIn,
                                                        ForkInfo, State),
            State1 = set_top_block_hash(hash(Node), State),
            {State1, DifficultyOut, Events}
    end.

maybe_set_new_fork_id(Node, ForkInfoIn, State) ->
    case node_is_genesis(Node, State) of
        true  -> {ForkInfoIn, []};
        false ->
            case db_sibling_blocks(Node) of
                #{key_siblings   := [],
                  micro_siblings := []} ->
                    {ForkInfoIn, []};
                #{micro_siblings := MicSibs} ->
                    {ForkInfoIn#fork_info{fork_id = hash(Node)}, MicSibs}
            end
    end.

get_state_trees_in(Node, State) ->
    case node_is_genesis(Node, State) of
        true  ->
            {ok,
             aec_block_genesis:populated_trees(),
             #fork_info{ difficulty = aec_block_genesis:genesis_difficulty()
                       , fork_id = hash(Node)
                       , fees = 0
                       , fraud = false
                       }
            };
        false ->
            PrevHash = prev_hash(Node),
            case db_find_state(PrevHash) of
                {ok, Trees, ForkInfo} ->
                    %% For key blocks, reset:
                    %% 1. Fees, to accumulate new fees for generation
                    %% 2. Fraud, since a new generation starts fresh
                    case node_type(db_get_node(PrevHash)) of
                        key   -> {ok, Trees, ForkInfo#fork_info{fees = 0,
                                                                fraud = false}};
                        micro -> {ok, Trees, ForkInfo}
                    end;
                error -> error
            end
    end.

apply_and_store_state_trees(Node, TreesIn, ForkInfoIn, State) ->
    {Trees, Fees, Events} = apply_node_transactions(Node, TreesIn, ForkInfoIn, State),
    assert_state_hash_valid(Trees, Node),
    DifficultyOut = ForkInfoIn#fork_info.difficulty + node_difficulty(Node),
    Fraud = update_fraud_info(ForkInfoIn, Node, State),
    ForkInfoInNode = ForkInfoIn#fork_info{ fees = Fees
                                         , difficulty = DifficultyOut
                                         , fraud = Fraud
                                         },
    ok = db_put_state(hash(Node), Trees, ForkInfoInNode),
    ok = db_put_found_pof(Node, maps:get(found_pof, State)),
    {DifficultyOut, Events}.

update_fraud_info(ForkInfoIn, Node, State) ->
    case maps:get(pof, State) =:= no_fraud of
        true  ->
            ForkInfoIn#fork_info.fraud;
        false ->
            case ForkInfoIn#fork_info.fraud of
                true  -> internal_error({double_reported_fraud, hash(Node)});
                false -> true
            end
    end.

handle_top_block_change(OldTopHash, NewTopDifficulty, Events, State) ->
    case get_top_block_hash(State) of
        OldTopHash -> State;
        NewTopHash when OldTopHash =:= undefined ->
            State1 = update_main_chain(get_genesis_hash(State), NewTopHash, State),
            {State1, Events};
        NewTopHash ->
            {ok, ForkHash} = find_fork_point(OldTopHash, NewTopHash),
            case ForkHash =:= OldTopHash of
                true ->
                    %% We are extending the current chain.
                    %% Difficulty might not have changed if it is an
                    %% extension of micro blocks.
                    State1 = update_main_chain(OldTopHash, NewTopHash, ForkHash, State),
                    {State1, Events};
                false ->
                    %% We have a fork. Compare the difficulties.
                    {ok, OldTopDifficulty} = db_find_difficulty(OldTopHash),
                    case OldTopDifficulty >= NewTopDifficulty of
                        true ->
                            State1 =set_top_block_hash(OldTopHash, State), %% Reset
                            {State1, Events};
                        false ->
                            State1 = update_main_chain(OldTopHash, NewTopHash,
                                                       ForkHash, State),
                            {State1, Events}
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
                  end, db_safe_get_tx_hashes(CurrentHash)),
    remove_locations(StopHash, db_get_prev_hash(CurrentHash)).

add_locations(Hash, Hash) ->
    ok;
add_locations(StopHash, CurrentHash) ->
    lists:foreach(fun(TxHash) ->
                          aec_db:add_tx_location(TxHash, CurrentHash),
                          aec_db:remove_tx_from_mempool(TxHash)
                  end, db_safe_get_tx_hashes(CurrentHash)),
    add_locations(StopHash, db_get_prev_hash(CurrentHash)).


assert_state_hash_valid(Trees, Node) ->
    RootHash = aec_trees:hash(Trees),
    Expected = node_root_hash(Node),
    case RootHash =:= Expected of
        true -> ok;
        false -> internal_error({root_hash_mismatch, RootHash, Expected})
    end.

apply_node_transactions(Node, Trees, ForkInfo, State) ->
    case is_micro_block(Node) of
        true ->
            #fork_info{fees = FeesIn} = ForkInfo,
            apply_micro_block_transactions(Node, FeesIn, Trees);
        false -> %% Key block
            #fork_info{fees = FeesIn, fraud = FraudStatus} = ForkInfo,
            GasFees = calculate_gas_fee(aec_trees:calls(Trees)),
            TotalFees = GasFees + FeesIn,
            PrevVsn =
                case node_height(Node) =:= aec_block_genesis:height() of
                    true -> undefined; % genesis block has no previous block
                    false -> version(db_get_node(prev_hash(Node)))
                end,
            Trees1 =
                aec_trees:perform_pre_transformations(Trees,
                                                      PrevVsn,
                                                      version(Node),
                                                      node_height(Node)),
            Delay  = aec_governance:beneficiary_reward_delay(),
            case node_height(Node) > aec_block_genesis:height() + Delay of
                true  -> {grant_fees(Node, Trees1, Delay, FraudStatus, State), TotalFees, #{}};
                false -> {Trees1, TotalFees, #{}}
            end
    end.

find_predecessor_at_height(Node, Height) ->
    case node_height(Node) of
        Height -> Node;
        H when H < Height -> error({cannot_find_predecessor, H, Height});
        H when H =:= Height + 1 ->
            %% Special case where we already have the hash
            db_get_node(prev_key_hash(Node));
        H ->
            case db_find_key_nodes_at_height(Height) of
                {ok, [KeyNode]} -> KeyNode;
                {ok, KeyNodes} ->
                    %% Use the preceeding key node since fork info might not be known
                    %% for Node (i.e., in candidate generation).
                    %% The clause for Height + 1 above will catch the case of the
                    %% immediate previous key hash.
                    PrevKeyNode = db_get_node(prev_key_hash(Node)),
                    find_one_predecessor(KeyNodes, PrevKeyNode);
                error -> error({cannot_find_predecessor, H, Height})
            end
    end.

find_one_predecessor([N|Left], Node) ->
    Hash1 = hash(N),
    Hash2 = hash(Node),
    case find_fork_point(Hash1, Hash2) of
        {ok, Hash1} -> N;
        {ok, _} -> find_one_predecessor(Left, Node);
        error -> find_one_predecessor(Left, Node)
    end.

%% A miner is reported for fraud in the key block that closes the next
%% generation. We need to know if any of the miners rewarded by a
%% generation was reported to know how to distribute the
%% rewards. Hence, we need to look two generations forward in time.
%%
%% NOTE: If the restriction of reporting a miner in the next
%% generation is lifted, we need to do something more elaborate.

grant_fees(Node, Trees, Delay, FraudStatus, State) ->
    NewestBlockHeight = node_height(Node) - Delay + ?POF_REPORT_DELAY,
    KeyNode4 = find_predecessor_at_height(Node, NewestBlockHeight),
    KeyNode3 = db_get_node(prev_key_hash(KeyNode4)),
    KeyNode2 = db_get_node(prev_key_hash(KeyNode3)),
    KeyNode1 = db_get_node(prev_key_hash(KeyNode2)),
    FraudStatus1 = db_get_fraud_status(hash(KeyNode3)),
    FraudStatus2 = case KeyNode4 =:= Node of
                       true  -> FraudStatus;
                       false -> db_get_fraud_status(hash(KeyNode4))
                   end,
    KeyFees  = db_get_fees(hash(KeyNode2)),
    Beneficiary1 = node_beneficiary(KeyNode1),
    Beneficiary2 = node_beneficiary(KeyNode2),

    %% We give the mining reward for the closing block of the generation.
    MineReward2 = aec_governance:block_mine_reward(node_height(KeyNode2)),
    %% Fraud rewards is given for the opening block of the generation
    %% since this is the reward that was withheld.
    FraudReward1 = aec_governance:fraud_report_reward(node_height(KeyNode1)),
    {BeneficiaryReward1, BeneficiaryReward2, LockAmount} =
        calc_rewards(FraudStatus1, FraudStatus2, KeyFees, MineReward2,
                     FraudReward1, node_is_genesis(KeyNode1, State)),

    OldestBeneficiaryHeight = node_height(KeyNode1),
    {{AdjustedReward1, AdjustedReward2}, DevRewards} =
        aec_dev_reward:split(BeneficiaryReward1, BeneficiaryReward2,
                             OldestBeneficiaryHeight),

    Trees1 = lists:foldl(
               fun({K, Amt}, TreesAccum) when Amt > 0 ->
                       aec_trees:grant_fee(K, TreesAccum, Amt);
                  (_, TreesAccum) -> TreesAccum
               end,
               Trees,
               [{Beneficiary1, AdjustedReward1},
                {Beneficiary2, AdjustedReward2} | DevRewards]),
    Accounts0 = aec_trees:accounts(Trees1),
    Accounts = aec_accounts_trees:lock_coins(LockAmount, Accounts0),
    aec_trees:set_accounts(Trees1, Accounts).


calculate_gas_fee(Calls) ->
    F = fun(_, SerCall, GasFeeIn) ->
                Call = aect_call:deserialize(<<>>, SerCall),
                GasFee = aect_call:gas_used(Call) * aect_call:gas_price(Call),
                GasFee + GasFeeIn
        end,
    aeu_mtrees:fold(F, 0, aect_call_state_tree:iterator(Calls)).


apply_micro_block_transactions(Node, FeesIn, Trees) ->
    Txs = db_get_txs(hash(Node)),
    KeyHeader = db_get_header(prev_key_hash(Node)),
    Env = aetx_env:tx_env_from_key_header(KeyHeader, prev_key_hash(Node),
                                          node_time(Node), prev_hash(Node)),
    case aec_block_micro_candidate:apply_block_txs_strict(Txs, Trees, Env) of
        {ok, _, NewTrees, Events} ->
            if map_size(Events) > 0 -> lager:debug("tx_events = ~p", [Events]);
               true -> ok
            end,
            TotalFees = lists:foldl(
                          fun(SignedTx, AccFee) ->
                                  Fee = aetx:deep_fee(aetx_sign:tx(SignedTx), NewTrees),
                                  AccFee + Fee
                          end, FeesIn, Txs),
            {NewTrees, TotalFees, Events};
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
        Height1 =:= Height2 -> find_micro_fork_point(Hash1, Hash2) %% Microblock keeps height.
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

median(Xs) ->
    Sorted = lists:sort(Xs),
    Length = length(Sorted),
    Mid = Length div 2,
    Rem = Length rem 2,
    (lists:nth(Mid+Rem, Sorted) + lists:nth(Mid+1, Sorted)) div 2.

%%%-------------------------------------------------------------------
%%% Internal interface for the db
%%%-------------------------------------------------------------------

db_put_node(Block, Hash) when is_binary(Hash) ->
    ok = aec_db:write_block(Block, Hash).

db_find_node(Hash) when is_binary(Hash) ->
    case aec_db:find_header(Hash) of
        {value, Header} -> {ok, wrap_header(Header, Hash)};
        none -> error
    end.

db_get_node(Hash) when is_binary(Hash) ->
    {ok, Node} = db_find_node(Hash),
    Node.

db_get_header(Hash) when is_binary(Hash) ->
    {value, Header} =  aec_db:find_header(Hash),
    Header.

db_find_key_nodes_at_height(Height) when is_integer(Height) ->
    case aec_db:find_headers_and_hash_at_height(Height) of
        [_|_] = Headers ->
            case [wrap_header(H, Hash) || {H, Hash} <- Headers, aec_headers:type(H) =:= key] of
                [] -> error;
                List -> {ok, List}
            end;
        [] -> error
    end.

db_put_state(Hash, Trees, ForkInfo) when is_binary(Hash) ->
    #fork_info{ difficulty      = Difficulty
              , fork_id         = ForkId
              , fees            = Fees
              , fraud           = Fraud
              } = ForkInfo,
    ok = aec_db:write_block_state(Hash, Trees, Difficulty, ForkId, Fees, Fraud).

db_put_found_pof(_Node, no_fraud) ->
    ok;
db_put_found_pof(Node, PoF) ->
    %% We only store the pof if the node has not been reported yet.
    PrevKeyHash = prev_key_hash(Node),
    case aec_db:find_discovered_pof(PrevKeyHash) of
        none -> aec_db:write_discovered_pof(PrevKeyHash, PoF);
        {value, _} -> ok
    end.

db_find_state(Hash) ->
    case aec_db:find_block_state_and_data(Hash) of
        {value, Trees, Difficulty, ForkId, Fees, Fraud} ->
            {ok, Trees,
             #fork_info{ difficulty = Difficulty
                       , fork_id = ForkId
                       , fees = Fees
                       , fraud = Fraud
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

db_get_fees(Hash) when is_binary(Hash) ->
    {value, Fees} = aec_db:find_block_fees(Hash),
    Fees.


db_get_fraud_status(Hash) when is_binary(Hash) ->
    {value, FraudStatus} = aec_db:find_block_fraud_status(Hash),
    FraudStatus.

db_safe_get_tx_hashes(Hash) when is_binary(Hash) ->
    case aec_db:find_block_tx_hashes(Hash) of
        none -> [];
        {value, Hashes} -> Hashes
    end.

db_get_prev_hash(Hash) when is_binary(Hash) ->
    {value, PrevHash} = db_find_prev_hash(Hash),
    PrevHash.

db_find_prev_hash(Hash) when is_binary(Hash) ->
    case aec_db:find_header(Hash) of
        {value, Header} -> {value, aec_headers:prev_hash(Header)};
        none -> none
    end.

db_sibling_blocks(Node) ->
    Height   = node_height(Node),
    Hash     = hash(Node),
    PrevHash = prev_hash(Node),
    case node_type(Node) of
        key ->
            #{ key_siblings   => match_prev_at_height(Height    , PrevHash, Hash)
             , micro_siblings => match_prev_at_height(Height - 1, PrevHash, Hash)};
        micro ->
            #{ key_siblings   => match_prev_at_height(Height + 1, PrevHash, Hash)
             , micro_siblings => match_prev_at_height(Height    , PrevHash, Hash)}
    end.

match_prev_at_height(Height, PrevHash, Hash) ->
    [Header || {Header, H} <- aec_db:find_headers_and_hash_at_height(Height),
               H =/= Hash,
               aec_headers:prev_hash(Header) =:= PrevHash].

update_found_pof(Node, MicroSibHeaders, State) ->
    State#{found_pof => maybe_pof(Node, MicroSibHeaders)}.

maybe_pof(_Node, []) ->
    no_fraud;
maybe_pof(Node, MicroSibHeaders) ->
    case node_type(Node) of
        key -> no_fraud;
        micro ->
            PrevKeyHash = prev_key_hash(Node),
            Miner = node_miner(db_get_node(PrevKeyHash)),
            [Header| _] = MicroSibHeaders,
            aec_pof:new(export_header(Node), Header, Miner)
    end.

% if a miner is fraudulent - one does not receive a reward and it is locked
% instead.
% if a miner reports a fraudulent previous miner - the reporter receives a
% as a bonus a fraction of the previous miner's reward, the rest is locked.
% Mining reward is awared with the previous generation (K2 mining reward is
% awared with GenerationK1's fees) and thus when a miner is fraudulent we
% don't award her with mining reward but we lock the excess of coins on the
% next granting of fees. This way we can compute properly the locked amount.
calc_rewards(FraudStatus1, FraudStatus2, GenerationFees,
             K2MineReward, K1FraudReward, IsKey1Genesis) ->
    B1FeesPart = GenerationFees * 4 div 10,
    B2FeesPart = GenerationFees - B1FeesPart,
    B2FullReward = B2FeesPart + K2MineReward,
    TotalBlockAmount = GenerationFees + K2MineReward,
    {B1Amt, B2Amt} =
        case {FraudStatus1, FraudStatus2} of
            {true, true} ->
                %% The miner of KeyNode1 was reported by the miner of KeyNode2
                %% but that miner was reported by the miner of KeyNode3.
                %% No rewards at all.
                {0, 0};
            {true, false} ->
                %% The miner of KeyNode1 was reported by the miner of KeyNode2
                %% and that miner was a well behaved miner.
                FinalReward = B2FullReward + K1FraudReward,
                {0, FinalReward};
            {false, true} ->
                %% The miner of KeyNode2 was reported by the miner of KeyNode3
                %% but that reward will come later.
                %% Lock just the fees
                case IsKey1Genesis of
                    true  -> {0,          0};
                    false -> {B1FeesPart, 0}
                end;
            {false, false} ->
                %% No fraud in sight. Well done!
                case IsKey1Genesis of
                    true  -> {0,          B2FullReward};
                    false -> {B1FeesPart, B2FullReward}
                end
        end,
    LockedAmount = TotalBlockAmount - B1Amt - B2Amt,
    {B1Amt, B2Amt, LockedAmount}.
