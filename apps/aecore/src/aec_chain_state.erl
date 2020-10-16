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

%% Let's not add another entry to the supervision tree
-on_load(setup_etc_cache/0).

-export([ calculate_state_for_new_keyblock/4
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
        , get_fork_result/2
        , get_info_field/2
        ]).

-import(aetx_env, [no_events/0]).

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
        {ok, Node} ->
            State = new_state_from_persistence(),
            case get_top_block_hash(State) of
                undefined -> false;
                TopHash ->
                    Height        = node_height(Node),
                    TopHeight     = node_height(db_get_node(TopHash)),
                    {ok, ChokePt} = choke_point(Height, TopHeight, TopHash),
                    hash_is_in_main_chain(Hash, ChokePt)
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
                case get_state_trees_in(Node, State, true) of
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
                  #{ type                  => ?MODULE
                   , top_block_hash        => aec_db:get_top_block_hash()
                   , genesis_block_hash    => aec_db:get_genesis_hash()
                   }
          end,
    aec_db:ensure_transaction(Fun).

persist_state(OldState, NewState) ->
    case {get_genesis_hash(OldState), get_genesis_hash(NewState)} of
        {_, undefined} -> ok;
        {GH, GH} -> ok;
        {_, GenesisHash} ->
            aec_db:write_genesis_hash(GenesisHash)
    end,
    case {get_top_block_hash(OldState), get_top_block_hash(NewState)} of
        {_, undefined} -> ok;
        {TH, TH} -> ok;
        {_, TopBlockHash} ->
            aec_db:write_top_block_hash(TopBlockHash)
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

%% Cache for block insertion
%% For performance reasons an valid cache entry MUST be connected to genesis
%% Ensuring this removes an additional read query to the db
%% When the window reaches max capacity(as defined by the consensus engine)
%% then the cache for the oldest block in the window gets evicted - this ensures that the
%% amount of memory consumed by the ?RECENT_CACHE is bounded
%% When the window size is N then we cache N^2 headers
%% Orphaned key blocks will leak memory - fortunately leaking memory will require PoW
%% TODO: periodically remove orphans from ram
-define(RECENT_CACHE, aec_chain_state_cache).
-record(recent_blocks, { key :: binary()
                       %% window of last N keyheaders newest first
                       %% All but the first header in this list is stripped
                       %% Invariant: {ok, key} = calulate_hash(hd(recent_key_headers))
                       , recent_key_headers :: [aec_headers:header() | term()]
                       %% current length of the header window
                       , len :: non_neg_integer()
                       }).

recent_cache_n() ->
    max(aec_governance:key_blocks_to_check_difficulty_count() + 1, aec_governance:median_timestamp_key_blocks()).

%% Slims down the given key header for caching
%% In case of BitcoinNG we should only care about the time, height and difficulty
%% What get's cached depends on the currently active consensus engine
recent_cache_trim_header(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    {Hash, aec_headers:target(Header), aec_headers:time_in_msecs(Header)}.

%% Insertion context - cached data used during block insertion
-record(insertion_ctx, {
        %% window of last N keyheaders newest first
        window_len = undefined :: non_neg_integer() | undefined,
        %% Recent key headers -> ALWAYS stripped
        recent_key_headers = undefined :: [term()] | undefined,
        prev_node = undefined :: #node{} | undefined,
        prev_key_node = undefined :: #node{} | undefined
    }).

%% Should hard crash when not enough headers are present
ctx_get_n_key_headers(#insertion_ctx{window_len = L, recent_key_headers = Headers}, N) when L >= N ->
    ctx_get_n_key_headers(Headers, N, []).

ctx_get_n_key_headers(_, 0, Acc) -> Acc;
ctx_get_n_key_headers([H|T], N, Acc) -> ctx_get_n_key_headers(T, N-1, [H|Acc]).

ctx_prev(#insertion_ctx{prev_node = PrevNode}) -> PrevNode.
ctx_prev_key(#insertion_ctx{prev_key_node = PrevKeyNode}) -> PrevKeyNode.

hash(#node{hash = Hash}) -> Hash.

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

version(#node{header = H}) -> aec_headers:version(H).

is_key_block(N) -> node_type(N) =:= key.

is_micro_block(N) -> node_type(N) =:= micro.

node_header(#node{header = H}) -> H.

prev_version(Node) ->
    case node_height(Node) =:= aec_block_genesis:height() of
        true  -> undefined;
        false -> version(db_get_node(prev_hash(Node)))
    end.

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

fake_key_node(PrevNode, Height, Miner, Beneficiary, Protocol) ->
    PrevKeyHash = case node_type(PrevNode) of
                      key   -> hash(PrevNode);
                      micro -> prev_key_hash(PrevNode)
                  end,
    Block = aec_blocks:new_key(Height,
                               hash(PrevNode),
                               PrevKeyHash,
                               <<123:?STATE_HASH_BYTES/unit:8>>,
                               ?HIGHEST_TARGET_SCI,
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
    #node{ header = Header
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
        Hash ->
            TopNode = db_get_node(Hash),
            TopHeight = node_height(TopNode),
            case Height > TopHeight of
                true -> error;
                false ->
                    case db_find_key_nodes_at_height(Height) of
                        error        -> error({broken_chain, Height});
                        {ok, [Node]} -> {ok, hash(Node)};
                        {ok, [_|_] = Nodes} ->
                            {ok, ChokePt} = choke_point(Height, TopHeight, Hash),
                            keyblock_hash_in_main_chain(Nodes, ChokePt)
                    end
            end
    end.

%% The assumption is that forks are short (normally a handful of blocks) and
%% not too frequent. Or else this isn't much of an optimization.
choke_point(Height, TopHeight, TopHash) when Height >= TopHeight -> {ok, TopHash};
choke_point(Height, TopHeight, TopHash) ->
    case db_find_key_nodes_at_height(Height + 1) of
        error        -> {ok, TopHash};
        {ok, [Node]} -> {ok, hash(Node)};
        {ok, _}      -> choke_point(Height + 1, TopHeight, TopHash)
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
    case dirty_db_find_node(hash(Node)) of
        error ->
            %% Build the insertion context using dirty reads to the DB and possibly
            %% The ets cache, the insertion context depends on the type of block being inserted
            InsertCtx = case ets:lookup(?RECENT_CACHE, prev_key_hash(Node)) of
                            [C] -> build_insertion_ctx(Node, node_type(Node), C);
                            [] -> build_insertion_ctx(Node, node_type(Node), undefined)
                        end,
            %% To preserve the invariants of the chain,
            %% Only add the block if we can do the whole
            %% transitive operation (i.e., calculate all the state
            %% trees, and update the pointers)
            Fun = fun() ->
                          internal_insert_transaction(Node, Block, Origin, InsertCtx)
                  end,
            try
                case InsertCtx of
                    {error, _} = Err -> Err;
                    _ ->
                        Res = aec_db:ensure_transaction(Fun),
                        %% Great! We inserted the block - time to update the cache
                        update_recent_cache(Node, InsertCtx),
                        Res
                end
            catch
                exit:{aborted, {throw, ?internal_error(What)}} ->
                    {error, What}
            end;
        {ok, Node} ->
            {error, already_in_db};
        {ok, Old} ->
            {error, {same_key_different_content, Node, Old}}
    end.

%% Builds the insertion context from cached data and the node to insert
%% Please note that this is called in dirty context without a try
%% clause - don't hard crash here.
%% Performs basic checks to ensure the chain consistency
build_insertion_ctx(Node, micro, undefined) ->
    % Microblocks only require the prev node and prev_key_node for validation
    case build_insertion_ctx_prev(Node, []) of
        {ok, PrevNode, PrevKeyNode} ->
            #insertion_ctx{ prev_key_node = PrevKeyNode
                          , prev_node = PrevNode };
        {error, _} = Err ->
            Err
    end;
build_insertion_ctx(Node, micro, #recent_blocks{recent_key_headers = [H|_]}) ->
    case build_insertion_ctx_prev(Node, [H]) of
        {ok, PrevNode, PrevKeyNode} ->
            #insertion_ctx{ prev_key_node = PrevKeyNode
                          , prev_node = PrevNode };
        {error, _} = Err ->
            Err
    end;
build_insertion_ctx(Node, key, undefined) ->
    Height        = node_height(Node),
    GenesisHeight = aec_block_genesis:height(),
    N = min(Height-GenesisHeight, recent_cache_n()),
    case build_insertion_ctx_prev(Node, []) of
        {ok, undefined, undefined} ->
            #insertion_ctx{ window_len = 0
                          , recent_key_headers = [] };
        {ok, PrevNode, PrevKeyNode} ->
            case get_n_key_headers_from(PrevKeyNode, N) of
                {ok, Headers} ->
                    RecentKeyHeaders = [recent_cache_trim_header(H) || H <- lists:reverse(Headers)],
                    #insertion_ctx{ prev_key_node = PrevKeyNode
                                  , prev_node = PrevNode
                                  , window_len = N
                                  , recent_key_headers = RecentKeyHeaders };
                _ ->
                    %% This may only happen if this is an orphan block
                    {error, {illegal_orphan, hash(Node)}}
            end;
        {error, _} = Err ->
            Err
    end;
build_insertion_ctx(Node, key, #recent_blocks{recent_key_headers = [H|T], len = N}) ->
    case build_insertion_ctx_prev(Node, [H]) of
        {ok, PrevNode, PrevKeyNode} ->
            #insertion_ctx{ prev_key_node = PrevKeyNode
                          , prev_node = PrevNode
                          , window_len = N
                          , recent_key_headers = [recent_cache_trim_header(H)|T] };
        {error, _} = Err ->
            Err
    end.

%% Retrieves the prev and prev_key node - performs basic consistency checks
build_insertion_ctx_prev(Node, []) ->
    build_insertion_ctx_prev(Node, [ctx_db_get_node(prev_key_hash(Node))]);
build_insertion_ctx_prev(Node, [undefined]) ->
    %% Ok we don't have the prev key hash in our db - this means that this MUST be a genesis
    %% block - if this is not the case then reject this block
    case node_height(Node) =:= aec_block_genesis:height() of
        true ->
            {ok, undefined, undefined};
        _ ->
            {error, {illegal_orphan, hash(Node)}}
    end;
build_insertion_ctx_prev(Node, [#node{header = H}]) ->
    build_insertion_ctx_prev(Node, [H]);
build_insertion_ctx_prev(Node, [PrevKeyHeader]) ->
    PrevKeyHash = prev_key_hash(Node),
    PrevKeyNode = #node{hash = PrevKeyHash, header = PrevKeyHeader, type = key},
    case prev_hash(Node) of
        PrevKeyHash ->
            build_insertion_ctx_check_prev_height(Node, PrevKeyNode, PrevKeyNode);
        H ->
            case ctx_db_get_node(H) of
                undefined ->
                    %% Ok so the prev keyblock is present but not the prev block?
                    %% this shouldn't be the case even for the genesis block
                    {error, {illegal_orphan, hash(Node)}};
                #node{type = key} ->
                    {error, prev_key_hash_inconsistency};
                PrevNode ->
                    case prev_key_hash(PrevNode) =:= PrevKeyHash of
                        true ->
                            %% Now assert heights
                            build_insertion_ctx_check_prev_height(Node, PrevNode, PrevKeyNode);
                        false ->
                            {error, prev_key_hash_inconsistency}
                    end
            end
   end.

build_insertion_ctx_check_prev_height(#node{type = key} = Node, PrevNode, PrevKeyNode) ->
    case node_height(PrevNode) =:= (node_height(Node) - 1) of
        true -> {ok, PrevNode, PrevKeyNode};
        false -> {error, height_inconsistent_for_keyblock_with_previous_hash}
    end;
build_insertion_ctx_check_prev_height(#node{type = micro} = Node, PrevNode, PrevKeyNode) ->
    case node_height(PrevNode) =:= node_height(Node) of
        true -> {ok, PrevNode, PrevKeyNode};
        false -> {error, height_inconsistent_for_microblock_with_previous_hash}
    end.

ctx_db_get_node(H) ->
    case dirty_db_find_node(H) of
        error -> undefined;
        {ok, R} -> R
    end.

update_recent_cache(#node{type = micro}, _InsertCtx) -> ok;
update_recent_cache(#node{type = key, header = Header, hash = H}, #insertion_ctx{window_len = N, recent_key_headers = Recents}) ->
    Entry =
        case N < recent_cache_n() of
            true ->
                #recent_blocks{key = H, len = N+1, recent_key_headers = [Header|Recents]};
            false ->
                %% Evict the cache for the oldest entry to ensure an upper bound on the used memory
                {ToEvict, _, _} = lists:last(Recents),
                ets:delete(?RECENT_CACHE, ToEvict),
                #recent_blocks{key = H, len = N, recent_key_headers = [Header|lists:droplast(Recents)]}
        end,
    ets:insert(?RECENT_CACHE, Entry).

internal_insert_transaction(Node, Block, Origin, Ctx) ->
    State1 = new_state_from_persistence(),
    assert_not_new_genesis(Node, State1),
    State2 = maybe_add_pof(maybe_add_genesis_hash(State1, Node), Block),
    case node_is_genesis(Node, State2) of
        true ->
            ok;
        false ->
            assert_not_illegal_fork_or_orphan(Node, Origin, State2),
            case node_type(Node) of
                key ->
                    assert_key_block_time(Node, Ctx),
                    assert_key_block_target(Node, Ctx),
                    maybe_put_signal_count(Block, hash(Node), aeu_env:get_env(aecore, fork, undefined));
                micro ->
                    assert_micro_block_time(Node, Ctx),
                    assert_micro_signature(Node, Ctx),
                    assert_micro_pof(Block, Ctx)
            end
    end,
    ok = db_put_node(Block, hash(Node)),
    {State3, Events} = update_state_tree(Node, State2, Ctx),
    persist_state(State1, State3),
    case maps:get(found_pof, State3) of
        no_fraud  -> {ok, Events};
        PoF       -> {pof, PoF, Events}
    end.

assert_not_illegal_fork_or_orphan(Node, Origin, State) ->
    case Origin of
        sync -> ok;
        undefined -> assert_height_delta(Node, State)
    end.

assert_height_delta(Node, State) ->
    Top       = db_get_node(get_top_block_hash(State)),
    TopHeight = node_height(Top),
    Height    = node_height(Node),
    case Height >= TopHeight - gossip_allowed_height_from_top() of
        false -> internal_error({too_far_below_top, Height, TopHeight});
        true -> ok
    end.

%% To assert key block target calculation we need DeltaHeight headers counted
%% backwards from the node we want to assert.
assert_key_block_target(Node, Ctx) ->
        Delta         = aec_governance:key_blocks_to_check_difficulty_count() + 1,
        Height        = node_height(Node),
        GenesisHeight = aec_block_genesis:height(),
        case Delta >= Height - GenesisHeight of
            true ->
                %% We only need to verify that the target is equal to its predecessor.
                assert_target_equal_to_prev(Node, Ctx);
            false ->
                assert_calculated_target(Node, Delta, Ctx)
        end.

assert_target_equal_to_prev(Node, Ctx) ->
    PrevKeyNode = ctx_prev_key(Ctx),
    case {node_target(Node), node_target(PrevKeyNode)} of
        {X, X} -> ok;
        {X, Y} -> internal_error({target_not_equal_to_parent, Node, X, Y})
    end.

assert_calculated_target(Node, Delta, Ctx) ->
    Headers = ctx_get_n_key_headers(Ctx, Delta),
    case aec_target:verify(node_header(Node), Headers) of
        ok -> ok;
        {error, {wrong_target, Actual, Expected}} ->
            internal_error({wrong_target, Node, Actual, Expected})
    end.

get_n_key_headers_from(Node, N) ->
    case node_type(Node) of
        key   ->
            get_n_key_headers_from({ok, Node}, N, []);
        micro ->
            get_n_key_headers_from(dirty_db_find_node(prev_key_hash(Node)), N, [])
    end.

get_n_key_headers_from(_, 0, Acc) ->
    {ok, Acc};
get_n_key_headers_from({ok, Node}, N, Acc) ->
    %% Assert
    key = node_type(Node),
    MaybePrevKeyNode = dirty_db_find_node(prev_key_hash(Node)),
    get_n_key_headers_from(MaybePrevKeyNode, N-1, [node_header(Node) | Acc]);
get_n_key_headers_from(error, _N, _Acc) ->
    error.

assert_key_block_time(Node, Ctx) ->
    Time = node_time(Node),
    case median_timestamp(Node, Ctx) of
        {ok, Median} when Time > Median -> ok;
        {ok,_Median} -> internal_error(key_block_from_the_past)
    end.

%% Compute the median timestamp for last aec_governance:median_timestamp_key_blocks()
median_timestamp(Node, Ctx) ->
    TimeStampKeyBlocks = aec_governance:median_timestamp_key_blocks(),
    case node_height(Node) =< TimeStampKeyBlocks of
        true ->
            {ok, aec_block_genesis:time_in_msecs()};
        false ->
            Headers = ctx_get_n_key_headers(Ctx, TimeStampKeyBlocks),
            Times = [T || {_, _, T} <- Headers],
            {ok, median(Times)}
    end.

assert_micro_block_time(Node, Ctx) ->
    PrevNode = ctx_prev(Ctx),
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

assert_micro_signature(Node, Ctx) ->
    case aeu_sig:verify(node_header(Node), node_miner(ctx_prev_key(Ctx))) of
        ok         -> ok;
        {error, _} -> internal_error(signature_verification_failed)
    end.

assert_micro_pof(Block, Ctx) ->
    case aec_blocks:pof(Block) of
        no_fraud ->
            ok;
        Pof ->
            FraudNode = get_fraud_node(Ctx),
            case aec_pof:pubkey(Pof) =:= node_miner(FraudNode) of
                true ->
                    ok;
                false ->
                    internal_error(wrong_pubkey_in_pof)
            end
    end.

get_fraud_node(Ctx) ->
    PrevNode = ctx_prev(Ctx),
    case node_type(PrevNode) of
        micro ->
            ctx_prev_key(Ctx);
        key ->
            db_get_node(prev_key_hash(PrevNode))
    end.

-record(fork_info, { fork_id
                   , difficulty
                   , fees
                   , fraud
                   }).

update_state_tree(Node, State, Ctx) ->
    {ok, Trees, ForkInfoIn} = get_state_trees_in(Node, State, true),
    {ForkInfo, MicSibHeaders} = maybe_set_new_fork_id(Node, ForkInfoIn, State),
    State1 = update_found_pof(Node, MicSibHeaders, State, Ctx),
    {State2, NewTopDifficulty, Events} = update_state_tree(Node, Trees, ForkInfo, State1),
    OldTopHash = get_top_block_hash(State),
    handle_top_block_change(OldTopHash, NewTopDifficulty, Events, State2).

update_state_tree(Node, TreesIn, ForkInfo, State) ->
    case db_find_state(hash(Node), true) of
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

get_state_trees_in(Node, State, DirtyBackend) ->
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
            case db_find_state(PrevHash, DirtyBackend) of
                {ok, Trees, ForkInfo} ->
                    %% For key blocks, reset:
                    %% 1. Fees, to accumulate new fees for generation
                    %% 2. Fraud, since a new generation starts fresh
                    case node_type(db_get_node(prev_hash(Node))) of
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
                            State1 = set_top_block_hash(OldTopHash, State), %% Reset
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
        false ->
            #fork_info{fees = FeesIn, fraud = FraudStatus} = ForkInfo,
            Height = node_height(Node),
            PrevVersion = prev_version(Node),
            GasFees = calculate_gas_fee(aec_trees:calls(Trees)),
            TotalFees = GasFees + FeesIn,
            Header = node_header(Node),
            Env = aetx_env:tx_env_from_key_header(Header, hash(Node),
                                                  node_time(Node), prev_hash(Node)),

            Trees1 = aec_trees:perform_pre_transformations(Trees, Env, PrevVersion),
            Delay  = aec_governance:beneficiary_reward_delay(),
            case Height > aec_block_genesis:height() + Delay of
                true  -> {grant_fees(Node, Trees1, Delay, FraudStatus, State), TotalFees, no_events()};
                false -> {Trees1, TotalFees, no_events()}
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

    OldestBeneficiaryVersion = version(KeyNode1),
    {{AdjustedReward1, AdjustedReward2}, DevRewards} =
        aec_dev_reward:split(BeneficiaryReward1, BeneficiaryReward2,
                             OldestBeneficiaryVersion),

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
            internal_error(invalid_transactions_in_block)
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

db_find_signal_count(Hash) ->
    case aec_db:find_signal_count(Hash) of
        {value, Count} -> {ok, Count};
        none           -> error
    end.

db_put_signal_count(Hash, Count) ->
    aec_db:write_signal_count(Hash, Count).

match_prev_at_height(Height, PrevHash, Hash) ->
    [Header || {Header, H} <- aec_db:find_headers_and_hash_at_height(Height),
               H =/= Hash,
               aec_headers:prev_hash(Header) =:= PrevHash].

update_found_pof(Node, MicroSibHeaders, State, Ctx) ->
    State#{found_pof => maybe_pof(Node, MicroSibHeaders, Ctx)}.

maybe_pof(_Node, [], _Ctx) ->
    no_fraud;
maybe_pof(Node, MicroSibHeaders, Ctx) ->
    case node_type(Node) of
        key -> no_fraud;
        micro ->
            Miner = node_miner(ctx_prev_key(Ctx)),
            [Header| _] = MicroSibHeaders,
            aec_pof:new(node_header(Node), Header, Miner)
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

setup_etc_cache() ->
    %% Lager might not be up so don't bother with logging
    Self = self(),
    Tab = ?RECENT_CACHE,
    Keypos = #recent_blocks.key,
    case ets:info(Tab, name) of
        undefined ->
            spawn(fun() ->
                ets:new(Tab, [set, public, named_table, {keypos, Keypos}]),
                Self ! cache_ready,
                timer:sleep(infinity)
            end),
            receive
                cache_ready ->
                    ok
            after
                3000 ->
                    exit(timeout)
            end;
        _ ->
            ok
    end,
    ok.
