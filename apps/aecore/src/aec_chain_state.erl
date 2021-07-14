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

-export([ calculate_state_for_new_keyblock/4
        , find_common_ancestor/2
        , get_key_block_hash_at_height/1
        , get_n_key_headers_backward_from/2
        , hash_is_connected_to_genesis/1
        , hash_is_in_main_chain/1
        , hash_is_in_main_chain/2
        , insert_block/1
        , insert_block/2
        , insert_block_conductor/2
        , gossip_allowed_height_from_top/0
        , proof_of_fraud_report_delay/0
        , get_fork_result/2
        , get_info_field/2
        , ensure_chain_ends/0
        , ensure_key_headers_height_store/0
        , grant_fees/5
        ]).

-import(aetx_env, [no_events/0]).
-import(aec_block_insertion,
                        [ node_hash/1
                        , node_prev_hash/1
                        , node_prev_key_hash/1
                        , node_height/1
                        , node_difficulty/1
                        , node_target/1
                        , node_root_hash/1
                        , node_miner/1
                        , node_beneficiary/1
                        , node_type/1
                        , node_time/1
                        , node_version/1
                        , node_is_key_block/1
                        , node_is_micro_block/1
                        , node_header/1] ).

%% For tests
-export([ get_top_block_hash/1
        , get_key_block_hash_at_height/2
        , calculate_gas_fee/1
        ]).

-ifdef(TEST).
-export([calc_rewards/6,
         wrap_block/1,
         internal_insert_transaction/4
        ]).
-endif.

-include("aec_block_insertion.hrl").
-include("blocks.hrl").
-include("aec_db.hrl").

-type events() :: aetx_env:events().

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
    aec_block_insertion:get_n_key_headers_from(Node, N).

-spec insert_block(aec_blocks:block()) -> {'ok', events()}
                                        | {'pof', aec_pof:pof(), events()}
                                        | {'error', any()}.
insert_block(Block) ->
    insert_block_strip_res(do_insert_block(Block, undefined)).

-spec insert_block_conductor(aec_blocks:block(), atom()) ->
    {'ok', boolean(), aec_headers:key_header() | undefined, events()}
    | {'pof', boolean(), aec_headers:key_header() | undefined, aec_pof:pof(), events()}
    | {'error', any()}.
insert_block_conductor(Block, block_synced) ->
    do_insert_block(Block, sync);
insert_block_conductor(Block, _Origin) ->
    do_insert_block(Block, undefined).

%% We should not check the height distance to top for synced block, so
%% we have to keep track of the origin here.
-spec insert_block(aec_blocks:block(), atom()) ->
        {'ok', events()}
      | {'pof', aec_pof:pof(), events()}
      | {'error', any()}.
insert_block(Block, block_synced) ->
    insert_block_strip_res(do_insert_block(Block, sync));
insert_block(Block, _Origin) ->
    insert_block_strip_res(do_insert_block(Block, undefined)).

insert_block_strip_res({ok, _, _, Events}) -> {ok, Events};
insert_block_strip_res({pof, _, _, PoF, Events}) -> {pof, PoF, Events};
insert_block_strip_res(Other) -> Other.

do_insert_block(Block, Origin) ->
    aec_blocks:assert_block(Block),
    Node = wrap_block(Block),
    {Time, Res} = timer:tc(fun() -> internal_insert(Node, Block, Origin) end),
    Type = node_type(Node),
    case Res of
        {error, _} ->
            aec_metrics:try_update([ae, epoch, aecore, blocks, Type, insert_execution_time, error], Time);
        _Ok ->
            aec_metrics:try_update([ae, epoch, aecore, blocks, Type, insert_execution_time, success], Time)
    end,
    Res.

-spec hash_is_connected_to_genesis(binary()) -> boolean().
hash_is_connected_to_genesis(Hash) ->
    case db_find_fork_id(Hash) of
        {ok,_ForkId} -> true;
        error -> false
    end.

-spec find_common_ancestor(binary(), binary()) ->
                                  {'ok', binary()} | {error, atom()}.
find_common_ancestor(Hash1, Hash2) ->
    find_common_ancestor(undefined, Hash1, undefined, Hash2).

find_common_ancestor(_, H, _, H) -> {ok, H};
find_common_ancestor(MaybeNode1, Hash1, MaybeNode2, Hash2) ->
    case maybe_db_find_node(MaybeNode2, Hash2) of
        {ok, Node2} ->
            case {node_prev_hash(Node2), node_prev_key_hash(Node2)} of
                {Hash1, _} ->
                    {ok, Hash1};
                {_, Hash1} ->
                    {ok, Hash1};
                _ ->
                    %% Don't fold this case to the above one! IO is expensive!
                    case maybe_db_find_node(MaybeNode1, Hash1) of
                        {ok, Node1} ->
                            case {node_prev_hash(Node1), node_prev_key_hash(Node1), node_prev_hash(Node2), node_prev_key_hash(Node2)} of
                                {Hash2, _, _, _} ->
                                    {ok, Hash2};
                                {_, Hash2, _, _} ->
                                    {ok, Hash2};
                                {H, _, H, _} ->
                                    {ok, H};
                                _ ->
                                    case find_fork_point(Node1, Hash1, Node2, Hash2) of
                                        error          -> {error, not_found};
                                        {ok, ForkHash} -> {ok, ForkHash}
                                    end
                            end;
                        _ ->
                            {error, unknown_hash}
                    end
            end;
        _ ->
            {error, unknown_hash}
    end.

-spec hash_is_in_main_chain(binary()) -> boolean().
hash_is_in_main_chain(Hash) ->
    case db_find_node(Hash) of
        {ok, Node} ->
            State = new_state_from_persistence(),
            case get_top_block_hash(State) of
                undefined -> false;
                TopHash ->
                    Height        = node_height(Node),
                    TopNode       = db_get_node(TopHash),
                    TopHeight     = node_height(TopNode),
                    {ok, {MaybeChokeNode, ChokePt}} = choke_point(Height, TopHeight, TopNode, TopHash),
                    hash_is_in_main_chain(Node, Hash, MaybeChokeNode, ChokePt)
            end;
        error -> false
    end.

-spec calculate_state_for_new_keyblock(
        binary(),
        aec_keys:pubkey(),
        aec_keys:pubkey(),
        aec_hard_forks:protocol_vsn()) -> {'ok', aec_trees:trees()} | 'error'.
calculate_state_for_new_keyblock(PrevHash, Miner, Beneficiary, Protocol) ->
    aec_db:ensure_transaction(fun() ->
        case db_find_node(PrevHash) of
            error -> error;
            {ok, PrevNode} ->
                Height = node_height(PrevNode) + 1,
                Node  = fake_key_node(PrevNode, Height, Miner, Beneficiary, Protocol),
                State = new_state_from_persistence(),
                case get_state_trees_in(Node, true) of
                    error -> error;
                    {ok, TreesIn, ForkInfoIn} ->
                        {Trees,_Fees,_Events} = apply_node_transactions(Node, TreesIn,
                                                                        ForkInfoIn, State),
                        {ok, Trees}
                end
        end
    end).

-define(DEFAULT_GOSSIP_ALLOWED_HEIGHT_FROM_TOP, 5).

gossip_allowed_height_from_top() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"gossip_allowed_height_from_top">>],
                               aecore, gossip_allowed_height_from_top,
                               ?DEFAULT_GOSSIP_ALLOWED_HEIGHT_FROM_TOP).

-define(POF_REPORT_DELAY, 2).

proof_of_fraud_report_delay() ->
    ?POF_REPORT_DELAY.

get_fork_result(Block, Fork) ->
    case is_last_signalling_block(Block, Fork) of
        true ->
            {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
            case db_find_signal_count(BlockHash) of
                {ok, Count} ->
                    {ok, fork_result(Count, Fork)};
                error ->
                    Count = count_blocks_with_signal(Block, Fork, 0),
                    db_put_signal_count(BlockHash, Count),
                    {ok, fork_result(Count, Fork)}
            end;
        false ->
            {error, not_last_signalling_block}
    end.

get_info_field(Height, Fork) when Fork =/= undefined ->
    case is_height_in_signalling_interval(Height, Fork) of
        true  -> maps:get(info_field, Fork);
        false -> default
    end;
get_info_field(_Height, undefined) ->
    default.

%%%===================================================================
%%% Internal functions
%%%===================================================================

new_state_from_persistence() ->
    Fun = fun() ->
                  #{ top_block_node        => db_get_top_block_node()
                   }
          end,
    aec_db:ensure_transaction(Fun).

persist_state(OldState, NewState) ->
    case {get_top_block_hash(OldState), get_top_block_hash(NewState)} of
        {TH, TH} -> false;
        {_, TopBlockHash} ->
            Node = get_top_block_node(NewState),
            db_write_top_block_node(Node),
            maybe_set_finalized_height(NewState),
            true
    end.

db_write_top_block_node(#chain_node{header = Header, hash = Hash}) ->
    aec_db:write_top_block_node(Hash, Header).

db_get_top_block_node() ->
    case aec_db:get_top_block_node() of
        undefined ->
            undefined;
        #{ hash := Hash
         , header := Header } ->
            wrap_header(Header, Hash)
    end.

maybe_set_finalized_height(State) ->
    Node = get_top_block_node(State),
    case node_type(Node) of
        key ->
            TopHeight = node_height(Node),
            case aec_resilience:fork_resistance_active() of
                {yes, FRHeight} ->
                    %% Set finalized height to one block below the allowed height
                    case TopHeight - FRHeight - 1 of
                        H when H > 0 ->
                            aec_db:write_finalized_height(H);
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end;
        micro ->
            ok
    end.

get_top_block_hash(#{top_block_node := #chain_node{hash = H}}) -> H;
get_top_block_hash(_) -> undefined.

get_top_block_node(#{top_block_node := N}) -> N.

set_top_block_node(#chain_node{} = N, State) -> State#{top_block_node => N}.

prev_version(Node) ->
    H = node_height(Node),
    case H =:= aec_block_genesis:height() of
        true  -> undefined;
        %% Might return different protocols for the same height due to block signaling
        false -> node_version(db_get_node(node_prev_hash(Node)))
    end.

maybe_add_pof(State, Block) ->
    case aec_blocks:type(Block) of
        key   -> State#{pof => no_fraud};
        micro -> State#{pof => aec_blocks:pof(Block)}
    end.

node_is_genesis(Node) ->
    node_hash(Node) =:= aec_consensus:get_genesis_hash().

wrap_block(Block) ->
    Header = aec_blocks:to_header(Block),
    wrap_header(Header).

fake_key_node(PrevNode, Height, Miner, Beneficiary, Protocol) ->
    PrevKeyHash = case node_type(PrevNode) of
                      key   -> node_hash(PrevNode);
                      micro -> node_prev_key_hash(PrevNode)
                  end,
    Module = aec_consensus:get_consensus_module_at_height(Height),
    Block = aec_blocks:new_key(Height,
                               node_hash(PrevNode),
                               PrevKeyHash,
                               <<123:?STATE_HASH_BYTES/unit:8>>,
                               Module:default_target(),
                               0, aeu_time:now_in_msecs(),
                               default,
                               Protocol,
                               Miner,
                               Beneficiary),
    wrap_header(aec_blocks:to_header(Block)).

wrap_header(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    wrap_header(Header, Hash).

wrap_header(Header, Hash) ->
    #chain_node{ header = Header
         , hash = Hash
         , type = aec_headers:type(Header)
         }.

%% NOTE: Only return nodes in the main chain.
%%       The function assumes that a node is in the main chain if
%%       there is only one node at that height, and the height is lower
%%       than the current top.
get_key_block_hash_at_height(Height, State) when is_integer(Height), Height >= 0 ->
    case get_top_block_hash(State) of
        undefined -> error;
        TopHash ->
            TopNode = db_get_node(TopHash),
            TopHeight = node_height(TopNode),
            case Height > TopHeight of
                true -> error;
                false ->
                    case db_find_key_nodes_at_height(Height) of
                        error        -> error({broken_chain, Height});
                        {ok, [Node]} -> {ok, node_hash(Node)};
                        {ok, [_|_] = Nodes} ->
                            {ok, {MaybeChokeNode, ChokePt}} = choke_point(Height, TopHeight, TopNode, TopHash),
                            keyblock_hash_in_main_chain(Nodes, MaybeChokeNode, ChokePt)
                    end
            end
    end.

%% The assumption is that forks are short (normally a handful of blocks) and
%% not too frequent. Or else this isn't much of an optimization.
choke_point(Height, TopHeight, MaybeTopNode, TopHash) when Height >= TopHeight -> {ok, {MaybeTopNode, TopHash}};
choke_point(Height, TopHeight, MaybeTopNode, TopHash) ->
    case db_find_key_nodes_at_height(Height + 1) of
        error        -> {ok, {MaybeTopNode, TopHash}};
        {ok, [Node]} -> {ok, {Node, node_hash(Node)}};
        {ok, _}      -> choke_point(Height + 1, TopHeight, MaybeTopNode, TopHash)
    end.

keyblock_hash_in_main_chain([Node|Left], MaybeTopNode, TopHash) ->
    case hash_is_in_main_chain(Node, node_hash(Node), MaybeTopNode, TopHash) of
        true  -> {ok, node_hash(Node)};
        false -> keyblock_hash_in_main_chain(Left, MaybeTopNode, TopHash)
    end;
keyblock_hash_in_main_chain([], _MaybeTopNode, _TopHash) ->
    error.

hash_is_in_main_chain(Hash, TopHash) ->
    hash_is_in_main_chain(undefined, Hash, undefined, TopHash).

hash_is_in_main_chain(MaybeNode, Hash, MaybeTopNode, TopHash) ->
    case find_common_ancestor(MaybeNode, Hash, MaybeTopNode, TopHash) of
        {ok, Hash} -> true;
        {ok, _} -> false;
        {error, _} -> false
    end.

%%%-------------------------------------------------------------------
%%% Chain operations
%%%-------------------------------------------------------------------

internal_insert(Node, Block, Origin) ->
    CanBeGenesis = node_height(Node) == aec_block_genesis:height(),
    IsKeyblock = node_is_key_block(Node),
    case dirty_db_find_node(node_hash(Node)) of
        error ->
            case node_is_genesis(Node) of
                true ->
                    internal_insert_genesis(Node, Block);
                false when CanBeGenesis, IsKeyblock ->
                    {error, rejecting_new_genesis_block};
                false when CanBeGenesis ->
                    {error, rejecting_micro_genesis_block};
                false ->
                    internal_insert_normal(Node, Block, Origin)
            end;
        {ok, Node} ->
            {error, already_in_db};
        {ok, Old} ->
            {error, {same_key_different_content, Node, Old}}
    end.

internal_insert_genesis(Node, Block) ->
    %% Avoid unnecessary branching and an extra DB lookup for the genesis hash in the general case
    %% by hardcoding the "genesis" case we may omit a lot of checks in the general case and simplify many preconditions
    %% Surprisingly this decreased keyblock insertion time from 300us down to 200us...
    aec_block_insertion:start_state_transition(fun() ->
        Assert = fun (X, X, _Err) -> ok;
                     (_Expected, _Actual, Err) -> aec_block_insertion:abort_state_transition(Err) end,
        Assert(undefined, aec_db:get_top_block_hash(), genesis_already_inserted),
        Assert(undefined, aec_db:get_genesis_hash(), genesis_already_inserted),
        Trees = aec_block_genesis:genesis_populated_trees(),
        Assert(node_root_hash(Node), aec_trees:hash(Trees), inconsistent_genesis_block),
        NewTopHash = node_hash(Node),
        ok = aec_db:write_block(Block, NewTopHash),
        ok = aec_db:write_block_state(
                NewTopHash,
                Trees,
                aec_block_genesis:genesis_difficulty() + node_difficulty(Node),
                NewTopHash,
                0,
                false),
        ok = aec_db:write_genesis_hash(NewTopHash),
        ok = db_write_top_block_node(Node),
        ok = aec_db:mark_chain_end_hash(NewTopHash),
        {ok, true, undefined, no_events()}
    end).

internal_insert_normal(Node, Block, Origin) ->
    %% Build the insertion context using dirty reads to the DB and possibly
    %% The ets cache, the insertion context depends on the type of block being inserted
    InsertCtx = aec_block_insertion:build_insertion_ctx(Node, Block),
    %% To preserve the invariants of the chain,
    %% Only add the block if we can do the whole
    %% transitive operation (i.e., calculate all the state
    %% trees, and update the pointers)
    Fun = fun() ->
                  internal_insert_transaction(Node, Block, Origin, InsertCtx)
          end,
    case InsertCtx of
        {error, _} = Err -> Err;
        _ ->
            case aec_block_insertion:start_state_transition(Fun) of
                {error, _} = Err -> Err;
                Res ->
                    %% Great! We inserted the block - time to update the cache
                    aec_block_insertion:update_recent_cache(Node, InsertCtx),
                    maybe_cache_new_top(Res, Node),
                    Res
            end
    end.

maybe_cache_new_top({ok, true, _PrevKeyHdr, _Events}, Node) ->
    cache_new_top(Node);
maybe_cache_new_top({pof, true, _PrevKeyHdr, _PoF, _Events}, Node) ->
    cache_new_top(Node);
maybe_cache_new_top(_, _) ->
    ok.

cache_new_top(#chain_node{} = Node) ->
    aec_block_insertion:update_top(Node).

internal_insert_transaction(Node, Block, Origin, Ctx) ->
    Consensus = aec_block_insertion:node_consensus(Node),
    State1 = new_state_from_persistence(),
    State2 = maybe_add_pof(State1, Block),
    assert_not_illegal_fork_or_orphan(Node, Origin, State2), %% TODO: move this check to aec_conductor... it doesn't really belong here
    case node_type(Node) of
        key ->
            maybe_put_signal_count(Block, node_hash(Node), aeu_env:get_env(aecore, fork, undefined));
        micro ->
            ok
    end,
    ok = db_put_node(Block, node_hash(Node)),
    {State3, Events} = update_state_tree(Node, State2, Ctx),
    TopChanged = persist_state(State1, State3),
    #chain_node{header = PrevKeyHeader} = aec_block_insertion:ctx_prev_key(Ctx),
    case maps:get(found_pogf, State3) of
        no_fraud -> ok;
        {H1, H2} ->
            %% Inform the consensus engine to act accordingly :(
            Consensus:pogf_detected(H1, H2)
    end,
    %% Update fork info
    case node_type(Node) of
        key ->
            ok = aec_db:mark_chain_end_hash(node_hash(Node)),
            ok = aec_db:unmark_chain_end_hash(node_prev_key_hash(Node));
        micro ->
            ok
    end,
    case maps:get(found_pof, State3) of
        no_fraud  -> {ok, TopChanged, PrevKeyHeader, Events};
        PoF       -> {pof, TopChanged, PrevKeyHeader, PoF, Events}
    end.

assert_not_illegal_fork_or_orphan(Node, Origin, State) ->
    Top       = get_top_block_node(State),
    TopHeight = node_height(Top),
    Height    = node_height(Node),
    case assert_height_delta(Origin, Height, TopHeight) of
        true -> ok;
        false ->
            aec_block_insertion:abort_state_transition({too_far_below_top, Height, TopHeight})
    end.

assert_height_delta(sync, Height, TopHeight) ->
    case aec_resilience:fork_resistance_active() of
        {yes, Delta} ->
            Height >= (TopHeight - Delta);
        no ->
            case aec_resilience:fork_resistance_configured() of
                {yes, _} ->
                    case aec_db:dirty_get_finalized_height() of
                        undefined ->
                            true;
                        FHeight ->
                            Height > FHeight
                    end;
                no ->
                    %% Don't enforce finalized_height if FR not configured
                    true
            end
    end;
assert_height_delta(undefined, Height, TopHeight) ->
    Height >= ( TopHeight - gossip_allowed_height_from_top() ).

update_state_tree(Node, State, Ctx) ->
    {ok, Trees, ForkInfoIn} = get_state_trees_in(Node, aec_block_insertion:ctx_prev(Ctx), true),
    {ForkInfo, MicSibHeaders, KeySibHeaders} = maybe_set_new_fork_id(Node, ForkInfoIn, State),
    State1 = update_found_pof(Node, MicSibHeaders, State, Ctx),
    State2 = update_found_pogf(Node, KeySibHeaders, State1),
    {State3, NewTopDifficulty, Events} = update_state_tree(Node, Trees, ForkInfo, State2),
    OldTopNode = get_top_block_node(State),
    handle_top_block_change(OldTopNode, NewTopDifficulty, Node, Events, State3).

update_state_tree(Node, TreesIn, ForkInfo, State) ->
     case db_find_state(node_hash(Node), true) of
         {ok, FoundTrees, FoundForkInfo} ->
            {Trees, _Fees, Events} = apply_node_transactions(Node, TreesIn,
                                                             ForkInfo, State),
            case aec_trees:hash(Trees) =:= aec_trees:hash(FoundTrees) of
                true -> %% race condition, we've already inserted this block
                    %% in case of fork, set the correct top:
                    State1 = set_top_block_node(Node, State),
                    {State1,
                     FoundForkInfo#fork_info.difficulty, %% keep correct difficulty
                     Events};
                false ->
                    %% NOTE: This is an internal inconsistency check,
                    %% so don't use aec_block_insertion:abort_state_transition
                    error({found_already_calculated_state, node_hash(Node)})
            end;
         error ->
             {DifficultyOut, Events} = apply_and_store_state_trees(Node, TreesIn,
                                                        ForkInfo, State),
            State1 = set_top_block_node(Node, State),
            {State1, DifficultyOut, Events}
    end.

maybe_set_new_fork_id(Node, ForkInfoIn, State) ->
    case {get_top_block_hash(State), node_prev_hash(Node)} of
        {H, H} ->
            %% When extending the existing top it doesn't make sense to perform a full scan
            %% of 2 generations. PrevHash(Node) =:= TopHash() can only be true if we have NO siblings
            %% This will be the case when syncing and will allow us to skip the expensive DB scan
            {ForkInfoIn, [], []};
        _ ->
            %% When benchmarking it turned out that this is very expensive to calculate
            %% Execute it only when it's possible for siblings to exist
            case db_sibling_blocks(Node) of
                #{key_siblings   := [],
                  micro_siblings := []} ->
                    {ForkInfoIn, [], []};
                #{ micro_siblings := MicSibs
                 , key_siblings := KeySibs} ->
                    {ForkInfoIn#fork_info{fork_id = node_hash(Node)}, MicSibs, KeySibs}
            end
    end.

get_state_trees_in(Node, DirtyBackend) ->
    get_state_trees_in(Node, db_get_node(node_prev_hash(Node)), DirtyBackend).

get_state_trees_in(Node, PrevNode, DirtyBackend) ->
    PrevHash = node_prev_hash(Node),
    case db_find_state(PrevHash, DirtyBackend) of
        {ok, Trees, ForkInfo} ->
            %% For key blocks, reset:
            %% 1. Fees, to accumulate new fees for generation
            %% 2. Fraud, since a new generation starts fresh
            case node_type(PrevNode) of
                key   -> {ok, Trees, ForkInfo#fork_info{fees = 0,
                                                        fraud = false}};
                micro -> {ok, Trees, ForkInfo}
            end;
        error -> error
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
    ok = db_put_state(node_hash(Node), Trees, ForkInfoInNode),
    ok = db_put_found_pof(Node, maps:get(found_pof, State)),
    {DifficultyOut, Events}.

update_fraud_info(ForkInfoIn, Node, State) ->
    case maps:get(pof, State) =:= no_fraud of
        true  ->
            ForkInfoIn#fork_info.fraud;
        false ->
            case ForkInfoIn#fork_info.fraud of
                true  -> aec_block_insertion:abort_state_transition({double_reported_fraud, node_hash(Node)});
                false -> true
            end
    end.

handle_top_block_change(OldTopNode, NewTopDifficulty, Node, Events, State) ->
    OldTopHash = node_hash(OldTopNode),
    case get_top_block_hash(State) of
        OldTopHash -> {State, no_events()};
        NewTopHash ->
            {ok, ForkHash} = find_common_ancestor(undefined, OldTopHash, Node, NewTopHash),
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
                            State1 = set_top_block_node(OldTopNode, State), %% Reset
                            {State1, Events};
                        false ->
                            State1 = update_main_chain(OldTopHash, NewTopHash,
                                                       ForkHash, State),
                            {State1, Events}
                    end
            end
    end.

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
                          aec_db:add_tx_hash_to_mempool(TxHash),
                          {ok, Tx} = aec_db:dirty_get_signed_tx(TxHash),
                          aec_events:publish(tx_received, Tx)
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
        false -> aec_block_insertion:abort_state_transition({root_hash_mismatch, RootHash, Expected})
    end.

apply_node_transactions(Node, Trees, ForkInfo, State) ->
    Consensus = aec_block_insertion:node_consensus(Node),
    case node_is_micro_block(Node) of
        true ->
            #fork_info{fees = FeesIn} = ForkInfo,
            Trees1 = Consensus:state_pre_transform_micro_node(Node, Trees),
            apply_micro_block_transactions(Node, FeesIn, Trees1);
        false ->
            #fork_info{fees = FeesIn, fraud = FraudStatus} = ForkInfo,
            Height = node_height(Node),
            PrevConsensus = aec_consensus:get_consensus_module_at_height(Height-1),
            PrevVersion = prev_version(Node),
            GasFees = calculate_gas_fee(aec_trees:calls(Trees)),
            TotalFees = GasFees + FeesIn,
            Header = node_header(Node),
            Env = aetx_env:tx_env_from_key_header(Header, node_hash(Node),
                                                  node_time(Node), node_prev_hash(Node)),

            Trees1 = aec_trees:perform_pre_transformations(Trees, Env, PrevVersion),
            Trees2 = if Consensus =:= PrevConsensus -> Trees1;
                        true -> Consensus:state_pre_transform_key_node_consensus_switch(Node, Trees1)
                     end,
            Trees3 = Consensus:state_pre_transform_key_node(Node, Trees2),
            Delay  = aec_governance:beneficiary_reward_delay(),
            case Height > aec_block_genesis:height() + Delay of
                true  -> {grant_fees(Node, Trees3, Delay, FraudStatus, State), TotalFees, no_events()};
                false -> {Trees3, TotalFees, no_events()}
            end
    end.

find_predecessor_at_height(Node, Height) ->
    case node_height(Node) of
        Height -> Node;
        H when H < Height -> error({cannot_find_predecessor, H, Height});
        H when H =:= Height + 1 ->
            %% Special case where we already have the hash
            db_get_node(node_prev_key_hash(Node));
        H ->
            case db_find_key_nodes_at_height(Height) of
                {ok, [KeyNode]} -> KeyNode;
                {ok, KeyNodes} ->
                    %% Use the preceeding key node since fork info might not be known
                    %% for Node (i.e., in candidate generation).
                    %% The clause for Height + 1 above will catch the case of the
                    %% immediate previous key hash.
                    PrevKeyNode = db_get_node(node_prev_key_hash(Node)),
                    find_one_predecessor(KeyNodes, PrevKeyNode);
                error -> error({cannot_find_predecessor, H, Height})
            end
    end.

find_one_predecessor([N|Left], Node) ->
    Hash1 = node_hash(N),
    Hash2 = node_hash(Node),
    case find_common_ancestor(N, Hash1, Node, Hash2) of
        {ok, Hash1} -> N;
        _ -> find_one_predecessor(Left, Node)
    end.

%% A miner is reported for fraud in the key block that closes the next
%% generation. We need to know if any of the miners rewarded by a
%% generation was reported to know how to distribute the
%% rewards. Hence, we need to look two generations forward in time.
%%
%% NOTE: If the restriction of reporting a miner in the next
%% generation is lifted, we need to do something more elaborate.

grant_fees(Node, Trees, Delay, FraudStatus, _State) ->
    Consensus = aec_block_insertion:node_consensus(Node),
    NewestBlockHeight = node_height(Node) - Delay + ?POF_REPORT_DELAY,
    KeyNode4 = find_predecessor_at_height(Node, NewestBlockHeight),
    KeyNode3 = db_get_node(node_prev_key_hash(KeyNode4)),
    KeyNode2 = db_get_node(node_prev_key_hash(KeyNode3)),
    KeyNode1 = db_get_node(node_prev_key_hash(KeyNode2)),
    FraudStatus1 = db_get_fraud_status(node_hash(KeyNode3)),
    FraudStatus2 = case KeyNode4 =:= Node of
                       true  -> FraudStatus;
                       false -> db_get_fraud_status(node_hash(KeyNode4))
                   end,
    KeyFees  = db_get_fees(node_hash(KeyNode2)),
    Beneficiary1 = node_beneficiary(KeyNode1),
    Beneficiary2 = node_beneficiary(KeyNode2),

    %% We give the mining reward for the closing block of the generation.
    MineReward2 = aec_governance:block_mine_reward(node_height(KeyNode2)),
    %% Fraud rewards is given for the opening block of the generation
    %% since this is the reward that was withheld.
    FraudReward1 = aec_governance:fraud_report_reward(node_height(KeyNode1)),
    {BeneficiaryReward1, BeneficiaryReward2, LockAmount} =
        calc_rewards(FraudStatus1, FraudStatus2, KeyFees, MineReward2,
                     FraudReward1, node_is_genesis(KeyNode1)),

    OldestBeneficiaryVersion = node_version(KeyNode1),
    {{AdjustedReward1, AdjustedReward2}, DevRewards} =
        aec_dev_reward:split(BeneficiaryReward1, BeneficiaryReward2,
                             OldestBeneficiaryVersion),

    Trees1 = lists:foldl(
               fun({K, Amt}, TreesAccum) when Amt > 0 ->
                       Consensus:state_grant_reward(K, TreesAccum, Amt);
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
    Txs = db_get_txs(node_hash(Node)),
    KeyHeader = db_get_header(node_prev_key_hash(Node)),
    Env = aetx_env:tx_env_from_key_header(KeyHeader, node_prev_key_hash(Node),
                                          node_time(Node), node_prev_hash(Node)),
    case timer:tc(aec_block_micro_candidate, apply_block_txs_strict, [Txs, Trees, Env]) of
        {Time, {ok, _, NewTrees, Events}} ->
            aec_metrics:try_update([ae, epoch, aecore, blocks, micro, txs_execution_time, success], Time),
            if map_size(Events) > 0 -> lager:debug("tx_events = ~p", [Events]);
               true -> ok
            end,
            TotalFees = lists:foldl(
                          fun(SignedTx, AccFee) ->
                                  Fee = aetx:deep_fee(aetx_sign:tx(SignedTx), NewTrees),
                                  AccFee + Fee
                          end, FeesIn, Txs),
            {NewTrees, TotalFees, Events};
        {Time, {error,_What}} ->
            aec_metrics:try_update([ae, epoch, aecore, blocks, micro, txs_execution_time, error], Time),
            aec_block_insertion:abort_state_transition(invalid_transactions_in_block)
    end.

find_fork_point(Node1, Hash1, Node2, Hash2) ->
    find_fork_point(Node1, Hash1, db_find_fork_id(Hash1), Node2, Hash2, db_find_fork_id(Hash2)).

find_fork_point(_, Hash,  {ok, FHash}, _, Hash,  {ok, FHash}) ->
    {ok, Hash};
find_fork_point(MaybeNode1, Hash1, {ok, FHash}, MaybeNode2, Hash2, {ok, FHash}) ->
    Node1 = maybe_db_get_node(MaybeNode1, Hash1),
    Node2 = maybe_db_get_node(MaybeNode2, Hash2),
    Height1 = node_height(Node1),
    Height2 = node_height(Node2),
    if
        Height1  >  Height2 -> {ok, Hash2};
        Height1  <  Height2 -> {ok, Hash1};
        Height1 =:= Height2 -> find_micro_fork_point(Hash1, Hash2) %% Microblock keeps height.
    end;
find_fork_point(MaybeNode1, Hash1, {ok, FHash1}, MaybeNode2, Hash2, {ok, FHash2}) ->
    FNode1 = db_get_node(FHash1),
    FNode2 = db_get_node(FHash2),
    Height1 = node_height(FNode1),
    Height2 = node_height(FNode2),
    if
        Height1 >= Height2 ->
            PrevHash = node_prev_hash(FNode1),
            PrevRes = db_find_fork_id(PrevHash),
            find_fork_point(undefined, PrevHash, PrevRes, MaybeNode2, Hash2, {ok, FHash2});
        Height2 > Height1 ->
            PrevHash = node_prev_hash(FNode2),
            PrevRes = db_find_fork_id(PrevHash),
            find_fork_point(MaybeNode1, Hash1, {ok, FHash1}, undefined, PrevHash, PrevRes)
    end;
find_fork_point(_MaybeNode1, _Hash1, _Res1, _MaybeNode2, _Hash2, _Res2) ->
    error.

maybe_db_get_node(undefined, Hash) -> db_get_node(Hash);
maybe_db_get_node(Node, _Hash) -> Node.
maybe_db_find_node(undefined, Hash) -> db_find_node(Hash);
maybe_db_find_node(Node, _Hash) -> {ok, Node}.

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
    Node = db_get_node(Hash1),
    case node_is_key_block(Node) of
        true  -> error;
        false -> do_find_micro_fork_point(node_prev_hash(Node), Hash2)
    end.

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

dirty_db_find_node(Hash) when is_binary(Hash) ->
    case aec_db:dirty_find_header(Hash) of
        {value, Header} -> {ok, wrap_header(Header, Hash)};
        none -> error
    end.

db_get_node(Hash) when is_binary(Hash) ->
    {ok, Node} = db_find_node(Hash),
    Node.

db_get_header(Hash) when is_binary(Hash) ->
    {value, Header} =  aec_db:find_header(Hash),
    Header.

%% TODO: This primitive is being used heavily(sync, block insertion, user api) and the current implementation is slowish
%% The usual result of this function call is one header, sometimes two headers. On heavy generations on a good SSD
%% using the rocksdb backend this right now takes 13-14ms which is bad in the hot path... This should take at most 100us -.-
db_find_key_nodes_at_height(Height) when is_integer(Height) ->
    case aec_db:find_key_headers_and_hash_at_height(Height) of
        [_|_] = Headers ->
            case [wrap_header(H, Hash) || {Hash, H} <- Headers] of
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
    PrevKeyHash = node_prev_key_hash(Node),
    case aec_db:find_discovered_pof(PrevKeyHash) of
        none -> aec_db:write_discovered_pof(PrevKeyHash, PoF);
        {value, _} -> ok
    end.

db_find_state(Hash, DirtyBackend) ->
    case aec_db:find_block_state_and_data(Hash, DirtyBackend) of
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
    Hash     = node_hash(Node),
    PrevHash = node_prev_hash(Node),
    case node_type(Node) of
        key ->
            #{ key_siblings   => match_prev_at_height(Height    , PrevHash, Hash)
             , micro_siblings => match_prev_at_height(Height - 1, PrevHash, Hash)};
        micro ->
            #{ key_siblings   => match_prev_at_height(Height + 1, PrevHash, Hash)
             , micro_siblings => match_prev_at_height(Height    , PrevHash, Hash)}
    end.

db_find_signal_count(Hash) ->
    case aec_db:find_signal_count(Hash) of
        {value, Count} -> {ok, Count};
        none           -> error
    end.

db_put_signal_count(Hash, Count) ->
    aec_db:write_signal_count(Hash, Count).

match_prev_at_height(Height, PrevHash, Hash) ->
    [Header || {H, Header} <- aec_db:find_headers_and_hash_at_height(Height),
               H =/= Hash,
               aec_headers:prev_hash(Header) =:= PrevHash].

update_found_pof(Node, MicroSibHeaders, State, Ctx) ->
    State#{found_pof => maybe_pof(Node, MicroSibHeaders, Ctx)}.

update_found_pogf(Node, KeySibHeaders, State) ->
    State#{found_pogf => maybe_pogf(Node, KeySibHeaders)}.

maybe_pof(_Node, [], _Ctx) ->
    no_fraud;
maybe_pof(Node, MicroSibHeaders, Ctx) ->
    case node_type(Node) of
        key -> no_fraud;
        micro ->
            Miner = node_miner(aec_block_insertion:ctx_prev_key(Ctx)),
            [Header| _] = MicroSibHeaders,
            aec_pof:new(node_header(Node), Header, Miner)
    end.

maybe_pogf(_Node, []) -> no_fraud;
maybe_pogf(Node, [Sibling|T]) ->
    case node_type(Node) of
        micro -> no_fraud;
        key ->
            case node_miner(Node) =:= aec_headers:miner(Sibling) of
                true ->
                    {node_header(Node), Sibling};
                false ->
                    maybe_pogf(Node, T)
            end
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

%%%-------------------------------------------------------------------
%%% Fork signalling related functions
%%%-------------------------------------------------------------------
maybe_put_signal_count(Block, Hash, Fork) when Fork =/= undefined ->
    case count_blocks_with_signal(Block, Fork) of
        {ok, Count}   -> db_put_signal_count(Hash, Count);
        {error, _Rsn} -> ok
    end;
maybe_put_signal_count(_Block, _Hash, undefined) ->
    ok.

count_blocks_with_signal(Block, Fork) ->
    BlockHeight = aec_blocks:height(Block),
    case is_height_in_signalling_interval(BlockHeight, Fork) of
        true ->
            PrevKeyBlockHash = aec_blocks:prev_key_hash(Block),
            case db_find_signal_count(PrevKeyBlockHash) of
                {ok, Count} ->
                    %% There is count stored for the previous block.
                    Count1 = Count + count_inc(is_matching_info_present(Block, Fork)),
                    {ok, Count1};
                error ->
                    %% No count stored for the previus block, it must be
                    %% computed by traversing to the first signalling block.
                    {ok, count_blocks_with_signal(Block, Fork, 0)}
            end;
        false ->
            {error, block_not_in_signalling_interval}
    end.

%% This function may take long time. It's called in the conductor's context, so
%% it blocks the conductor process.
count_blocks_with_signal(Block, Fork, Count) ->
    Count1 = Count + count_inc(is_matching_info_present(Block, Fork)),
    case is_first_signalling_block(Block, Fork) of
        true ->
            Count1;
        false ->
            {value, Block1} = aec_db:find_block(aec_blocks:prev_key_hash(Block)),
            count_blocks_with_signal(Block1, Fork, Count1)
    end.

is_height_in_signalling_interval(Height, #{signalling_start_height := StartHeight,
                                           signalling_end_height := EndHeight}) ->
    %% Block at EndHeight doesn't belong to the signalling interval! EndHeight
    %% is fork height and the signalling result must be known at this height.
    (Height >= StartHeight) andalso (Height < EndHeight).

is_first_signalling_block(Block, #{signalling_start_height := StartHeight}) ->
    aec_blocks:height(Block) =:= StartHeight.

is_last_signalling_block(Block, #{signalling_end_height := EndHeight}) ->
    aec_blocks:height(Block) =:= (EndHeight - 1).

count_inc(true)  -> 1;
count_inc(false) -> 0.

is_matching_info_present(Block, #{info_field := InfoField}) ->
    Info = aec_headers:info(aec_blocks:to_header(Block)),
    Info =:= InfoField.

fork_result(Count, #{signalling_block_count := SigCount}) when Count >= SigCount ->
    true;
fork_result(_Count, _Fork) ->
    false.

%% Ok this is a DB migration which is pretty heavy to execute
%% as it involves a DFS search of the ENTIRE header chain starting from genesis
%% If the migration is required then starts a background task which does this + uses aec_chain_state as a temporary store
%% We essentially walk the entire header chain and at the end insert the results to the DB in an atomic transaction
%% comparing to what already was inserted
-spec ensure_chain_ends() -> ok | pid().
ensure_chain_ends() ->
    case aec_db:get_top_block_hash() of
        undefined -> ok; %% Fresh DB
        _ -> %% Ok this is not a fresh DB
            case aec_db:chain_migration_status(chain_ends) of
                done -> %% No migration is in progress
                    case aec_db:find_chain_end_hashes() of
                        [] -> %% Ok this is an old DB - migration required
                            aec_db:start_chain_migration(chain_ends),
                            start_chain_ends_migration();
                        _ -> %% No migration required
                            ok
                    end;
                in_progress -> %% The migration was interrupted and not finished yet
                    lager:info("[Orphan key blocks scan] Resuming interrupted scan"),
                    start_chain_ends_migration()
            end
    end.

start_chain_ends_migration() ->
    lager:info("[Orphan key blocks scan] Scanning the DB for orphan keyblocks in the background"),
    spawn(fun() -> %% Don't use spawn_link here - we can't be killed by the setup process
        %% An error here should bring down the entire node with it!
        try
            %% On a cloud ssd it executed in 158s compared to 1.5h by doing it without hacks
            %% The reason why it's so fast is that mnesia_rocksdb creates an iterator object and while iterating
            %% rocksdb aggressively prefetches the data in increasingly bigger chunks
            %% Retrieves tuples {hash, prev_key_hash, height} and should work for legacy header formats
            %% Takes up at most a few MB of RAM
            {Time, R0} = timer:tc(fun() ->
                mnesia:dirty_select(aec_headers, [{ {aec_headers, '$1', '$2', '$3'}
                                                  , [{'=:=', {element, 1, '$2'}, key_header}]
                                                  , [{{'$1', {element, 4, '$2'}, '$3'}}]
                                                  }])
              end),
            lager:info("[Orphan key blocks scan] Retrieved the key header chain in ~p microseconds", [Time]),
            R1 = lists:keysort(3, R0),
            S = lists:foldl(fun({Hash, PrevKeyHash, _}, S0) ->
                S1 = sets:del_element(PrevKeyHash, S0),
                sets:add_element(Hash, S1)
              end, sets:new(), R1),
            chain_ends_finish_migration(S)
        catch
            E:R:Stack ->
                (catch io:format(user, "[Orphan key blocks scan] Terminating node: ~p ~p ~p", [E, R, Stack])),
                (catch lager:error("[Orphan key blocks scan] Terminating node: ~p ~p ~p", [E, R, Stack])),
                init:stop("[Orphan key blocks scan] Encountered a fatal error during the migration. Terminating the node")
        end
      end).

%% Finishes the DB migration - this is kind of tricky as the migration was done while the node was running
%% We insert a orphan top to the DB only when we don't have already a newer orphan which is a predecessor
%% Let's do it in O(N*M*O_is_predecesor) where N is the number of orphans found while the migration was in progress and M
%% is the numbers of orphans found by the migration, N should be relatively small
chain_ends_finish_migration(OldTops0) ->
    lager:info("[Orphan key blocks scan] Finished - applying the scan result"),
    OldTops = sets:to_list(OldTops0),
    lager:debug("[Orphan key blocks scan] Finished - found ~p orphans", [length(OldTops)]),
    %% This needs to respect ACID!
    ok = aec_db:ensure_transaction(fun() ->
        NewTops = aec_db:find_chain_end_hashes(),
        chain_ends_maybe_apply(OldTops, NewTops, NewTops),
        aec_db:finish_chain_migration(chain_ends),
        ok
      end).

chain_ends_maybe_apply([], _, _) -> ok; %% Done
chain_ends_maybe_apply([H|T], [], NewTops) -> %% No objections
    aec_db:mark_chain_end_hash(H),
    chain_ends_maybe_apply(T, NewTops, NewTops);
chain_ends_maybe_apply([H|T], [H|_], NewTops) -> %% Already found
    chain_ends_maybe_apply(T, NewTops, NewTops);
chain_ends_maybe_apply([H1|T1] = OldTops, [H2|T2], NewTops) -> %% Check if H1 is a direct predecessor of H2
    case find_common_ancestor(H1, H2) of
        {ok, H1} -> chain_ends_maybe_apply(T1, NewTops, NewTops); %% H2 is more recent than H1
        {ok, _} -> chain_ends_maybe_apply(OldTops, T2, NewTops); %% H1 might be new
        {error, unknown_hash} -> %% Community fork migration
            EH1 = aeser_api_encoder:encode(key_block_hash, H1),
            EH2 = aeser_api_encoder:encode(key_block_hash, H2),
            lager:info("[Orphan key blocks scan] ignoring deleted header: find_common_ancestor(~p, ~p)", [EH1, EH2]),
            chain_ends_maybe_apply(T1, NewTops, NewTops)
    end.


% We use aec_chain_state to store all key block headers with heights.
% This allows for cheap retrieval of key header by height.
-spec ensure_key_headers_height_store() -> ok | pid().
ensure_key_headers_height_store() ->
    case aec_db:get_top_block_hash() of
        undefined -> ok; %% Fresh DB
        _ -> %% Ok this is not a fresh DB
            case aec_db:chain_migration_status(key_headers) of
                done -> %% No migration is in progress
                    case aec_db:find_key_headers_and_hash_at_height(1) of
                        [] -> %% Ok this is an old DB - migration required
                            aec_db:start_chain_migration(key_headers),
                            start_key_headers_height_store_migration();
                        _ -> %% No migration required
                            ok
                    end;
                in_progress -> %% The migration was interrupted and not finished yet
                    lager:info("[Key headers migration scan] Resuming interrupted scan"),
                    start_key_headers_height_store_migration()
            end
    end.

start_key_headers_height_store_migration() ->
    lager:info("[Key headers migrations scan] Retriving all key headers"),
    spawn(fun() -> %% Don't use spawn_link here - we can't be killed by the setup process
        %% An error here should bring down the entire node with it!
        try
            mnesia:activity(async_dirty, fun () ->
                Res = timer:tc(fun() ->
                    mnesia:select(aec_headers,
                                  [{ {aec_headers, '$1', '$2', '$3'}
                                  , [{'=:=', {element, 1, '$2'}, key_header}]
                                  , [{{'$1', '$2', '$3'}}]
                                  }],
                                  10000,
                                  read)
                end),
                {TotalTime, TotalCount} = key_headers_height_store_migration_step(Res),
                lager:info("[Key headers migrations scan] DONE: In total migrated ~p headers in ~p microseconds", [TotalCount, TotalTime])
            end),
            aec_db:finish_chain_migration(key_headers)
        catch
            E:R:Stack ->
                (catch io:format(user, "[Key headers migration scan] Terminating node: ~p ~p ~p", [E, R, Stack])),
                (catch lager:error("[Key headers migration scan] Terminating node: ~p ~p ~p", [E, R, Stack])),
                init:stop("[Key headers migration scan] Encountered a fatal error during the migration. Terminating the node")
        end
    end).

key_headers_height_store_migration_step(First) ->
    key_headers_height_store_migration_step(0, 0, First).
key_headers_height_store_migration_step(Time, N, {TimeAdd, '$end_of_table'}) ->
    {Time + TimeAdd, N};
key_headers_height_store_migration_step(Time, N, {TimeRead, {Headers, Cont}}) ->
    lager:info("[Key headers migrations scan] Read headers in ~p microseconds", [TimeRead]),
    {TimeWrite, Count} = timer:tc(fun() ->
        aec_db:ensure_transaction(fun() ->
            lists:foldl(
                fun({Hash, Header, Height}, Count) ->
                    mnesia:write(#aec_chain_state{key = {key_header, Height, Hash}, value = Header}),
                    Count+1
                end,
                0,
                Headers
            )
        end)
    end),
    lager:info("[Key headers migrations scan] Wrote ~p headers in ~p microseconds", [Count, TimeWrite]),
    key_headers_height_store_migration_step(
        Time + TimeRead + TimeWrite,
        N + Count,
        timer:tc(fun() -> mnesia:select(Cont) end)
    ).
