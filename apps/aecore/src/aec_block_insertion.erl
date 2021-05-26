%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc Helpers for ADT's used during block insertion, those helpers are meant
%%%      mainly for aec_chain_state and consensus modules
%%%
-module(aec_block_insertion).

-export([ start_state_transition/1
        , abort_state_transition/1
        %% Wrapped header helpers
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
        , node_hash/1
        , node_header/1
        , node_consensus/1
        %% Insertion context
        , update_recent_cache/2
        , update_top/1
        , build_insertion_ctx/2
        , ctx_prev_key/1
        , ctx_prev/1
        , ctx_get_recent_n/2
        %% DB helpers
        , get_n_key_headers_from/2
        , get_top_hash/0
        , get_top_header/0
        , get_top_height/0]).

-export_type([insertion_ctx/0]).

-include("blocks.hrl").

-type chain_node() :: aec_chain_node:chain_node().

%% Cache entry for recently inserted blocks
-record(recent_blocks, {
    key :: binary()
    %% Window of recent statistics of the chain
    %% The head of this list is always the node corresponding to the key
    %% Invariant: key =:= node_hash(hd(recents))
    %% The remaining entries are {hash, term()}
    %% The first element of the tuple is used for maintaining the cache
    %% The second element is defined by the currently active consensus engine
    %% Please keep in mind that the cache is never filled by 2 engines at the same time
    %% When a consensus change occurs the cache is regenerated
    , recents :: [aec_headers:header() | term()]
    %% current length of the header window
    , len :: non_neg_integer()
}).
-type recent_blocks() :: #recent_blocks{}.
-type maybe_recent_blocks() :: recent_blocks() | undefined.

%% Insertion context - cached data used during block insertion
%% The data present in the context is sufficient for fully validating
%% the block headers outside a DB transaction.
-record(insertion_ctx, {
    %% window of last N keyheaders newest first
    window_len = 0 :: non_neg_integer(),
    %% Recent key headers -> ALWAYS stripped
    recent_key_headers = [] :: [term()],
    prev_node = aec_chain_node:new() :: chain_node(),
    prev_key_node = aec_chain_node:new() :: chain_node()
}).
-type insertion_ctx() :: #insertion_ctx{}.

-type height() :: aec_blocks:height().
-type difficulty() :: non_neg_integer().

%% Let's not add another entry to the supervision tree
-on_load(setup_ets_cache/0).

-define(internal_error(____E____), {aec_chain_state_error, ____E____}).

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
-define(REGISTRY_CACHE, aec_chain_state_registry_cache).

-spec start_state_transition(fun(() -> T)) -> T | {error, term()}.
start_state_transition(Fun) ->
    try
        aec_db:ensure_transaction(Fun)
    catch
        exit:{aborted, {throw, ?internal_error(What)}} ->
            {error, What}
    end.

-spec abort_state_transition(term()) -> no_return().
%% Aborts the block insertion - this may be only called inside the callback passed to
%% start_state_transition. Rollbacks all changes made by start_state_transition.
abort_state_transition(Reason) ->
    throw(?internal_error(Reason)).

-spec node_prev_hash(chain_node()) -> block_header_hash().
node_prev_hash(N) -> aec_headers:prev_hash(aec_chain_node:header(N)).

-spec node_prev_key_hash(chain_node()) -> block_header_hash().
node_prev_key_hash(N) -> aec_headers:prev_key_hash(aec_chain_node:header(N)).

-spec node_height(chain_node()) -> height().
node_height(N) -> aec_headers:height(aec_chain_node:header(N)).

-spec node_difficulty(chain_node()) -> difficulty().
node_difficulty(N) ->
    case aec_chain_node:type(N) of
        micro -> 0;
        _ -> aec_headers:difficulty(aec_chain_node:header(N))
    end.

-spec node_target(chain_node()) -> aec_consensus:key_target().
node_target(ChainNode) -> aec_headers:target(aec_chain_node:header(ChainNode)).

-spec node_root_hash(chain_node()) -> state_hash().
node_root_hash(ChainNode) -> aec_headers:root_hash(aec_chain_node:header(ChainNode)).

node_miner(ChainNode) -> aec_headers:miner(aec_chain_node:header(ChainNode)).

node_beneficiary(ChainNode) ->
    aec_headers:beneficiary(aec_chain_node:header(ChainNode)).

node_type(ChainNode) -> aec_chain_node:type(ChainNode).

node_time(ChainNode) -> aec_headers:time_in_msecs(aec_chain_node:header(ChainNode)).

node_version(ChainNode) -> aec_headers:version(aec_chain_node:header(ChainNode)).

node_is_key_block(N) -> node_type(N) =:= key.

node_is_micro_block(N) -> node_type(N) =:= micro.

node_hash(ChainNode) -> aec_chain_node:hash(ChainNode).

node_header(ChainNode) -> aec_chain_node:header(ChainNode).

node_consensus(ChainNode) ->
    aec_headers:consensus_module(aec_chain_node:header(ChainNode)).

ctx_prev(#insertion_ctx{prev_node = PrevNode}) -> PrevNode.
ctx_prev_key(#insertion_ctx{prev_key_node = PrevKeyNode}) -> PrevKeyNode.

%% Should hard crash when not enough headers are present
ctx_get_recent_n(#insertion_ctx{window_len = L, recent_key_headers = Headers}, N) when L >= N ->
    ctx_get_recent_n(Headers, N, []).

ctx_get_recent_n(_, 0, Acc) -> Acc;
ctx_get_recent_n([{_,H}|T], N, Acc) -> ctx_get_recent_n(T, N-1, [H|Acc]).

%% Slims down the given key header for caching
%% In case of BitcoinNG we should only care about the time, height and difficulty
%% What get's cached depends on the currently active consensus engine
recent_cache_trim_node(ActiveConsensus, RecentNode) ->
    {node_hash(RecentNode), ActiveConsensus:recent_cache_trim_key_header(node_header(RecentNode))}.

build_insertion_ctx(Node, Block) ->
    Consensus = node_consensus(Node),
    IsMicroblock = node_is_micro_block(Node),
    Cache = case ets:lookup(?RECENT_CACHE, node_prev_key_hash(Node)) of
                [] -> undefined;
                [#recent_blocks{recents = [N|_]}=C] ->
                    PrevConsensus = node_consensus(N),
                    if PrevConsensus =:= Consensus -> C;
                       IsMicroblock -> C; %% When handling microblocks we can get the prev_key_block from the cache
                       true ->
                            %% Oh no, a consensus change is currently underway
                            %% Consensus engines might store different data in the cache
                            %% Consensus changes are very rare - removing everything in the
                            %% ETS cache is OK
                            lager:debug("Invalidating block insertion ETS cache due to consensus change", []),
                            ets:delete_all_objects(?RECENT_CACHE),
                            undefined
                    end
            end,
    case build_insertion_ctx(Consensus, Node, node_type(Node), Cache) of
        {error, _} = Err -> Err;
        Ctx ->
            R = if IsMicroblock -> Consensus:dirty_validate_micro_node_with_ctx(Node, Block, Ctx);
                   true         -> Consensus:dirty_validate_key_node_with_ctx(Node, Block, Ctx)
            end,
            case R of
                {error, _} = Err -> Err;
                ok -> Ctx
            end
    end.

%% Builds the insertion context from cached data and the node to insert
%% Please note that this is called in dirty context without a try
%% clause - don't hard crash here.
%% Performs basic checks to ensure the chain consistency
-spec build_insertion_ctx(atom(), chain_node(), block_type(),
    maybe_recent_blocks()) -> insertion_ctx() | {error, term()}.
build_insertion_ctx(_Consensus, Node, micro, undefined) ->
    % Microblocks only require the prev node and prev_key_node for validation
    case build_insertion_ctx_prev(Node, undefined) of
        {ok, PrevNode, PrevKeyNode} ->
            #insertion_ctx{ prev_key_node = PrevKeyNode
                          , prev_node = PrevNode };
        {error, _} = Err ->
            Err
    end;
build_insertion_ctx(_Consensus, Node, micro, #recent_blocks{recents = [H|_]}) ->
    case build_insertion_ctx_prev(Node, H) of
        {ok, PrevNode, PrevKeyNode} ->
            #insertion_ctx{ prev_key_node = PrevKeyNode
                          , prev_node = PrevNode };
        {error, _} = Err ->
            Err
    end;
build_insertion_ctx(Consensus, Node, key, undefined) ->
    Height        = node_height(Node),
    GenesisHeight = aec_block_genesis:height(),
    N = min(Height-GenesisHeight, Consensus:recent_cache_n()),
    case build_insertion_ctx_prev(Node, undefined) of
        {ok, PrevNode, PrevKeyNode} ->
            case get_n_key_nodes_from(PrevKeyNode, N) of
                {ok, Nodes} ->
                    RecentKeyHeaders = [recent_cache_trim_node(Consensus, N1)
                                        || N1 <- lists:reverse(Nodes)],
                    #insertion_ctx{ prev_key_node = PrevKeyNode
                                  , prev_node = PrevNode
                                  , window_len = N
                                  , recent_key_headers = RecentKeyHeaders };
                _ ->
                    %% This may only happen if this is an orphan block
                    {error, {illegal_orphan, node_hash(Node)}}
            end;
        {error, _} = Err ->
            Err
    end;
build_insertion_ctx(Consensus, Node, key, #recent_blocks{recents = [H|T], len = N}) ->
    case build_insertion_ctx_prev(Node, H) of
        {ok, PrevNode, PrevKeyNode} ->
            #insertion_ctx{ prev_key_node = PrevKeyNode
                          , prev_node = PrevNode
                          , window_len = N
                          , recent_key_headers = [recent_cache_trim_node(Consensus, H)|T] };
        {error, _} = Err ->
            Err
    end.

%% Retrieves the prev and prev_key node - performs basic consistency checks
build_insertion_ctx_prev(Node, undefined) ->
    case dirty_db_find_node(node_prev_key_hash(Node)) of
        error ->
            {error, {illegal_orphan, node_hash(Node)}};
        {ok, N} ->
            build_insertion_ctx_prev(Node, N)
    end;
build_insertion_ctx_prev(Node, PrevKeyNode) ->
    PrevKeyHash = node_prev_key_hash(Node),
    case node_prev_hash(Node) of
        PrevKeyHash ->
            build_insertion_ctx_check_prev_height(Node, PrevKeyNode, PrevKeyNode);
        H ->
            case dirty_db_find_node(H) of
                error ->
                    %% Ok so the prev keyblock is present but not the prev block?
                    %% this shouldn't be the case even for the genesis block
                    {error, {illegal_orphan, node_hash(Node)}};
                {ok, PrevNode} ->
                    case {aec_chain_node:type(PrevNode), node_prev_key_hash(PrevNode) =:= PrevKeyHash} of
                        {key, _} -> {error, prev_key_hash_inconsistency};
                        {_, false} -> {error, prev_key_hash_inconsistency};
                        {_, true} ->
                            %% Now assert heights
                            build_insertion_ctx_check_prev_height(Node, PrevNode, PrevKeyNode)
                    end
            end
   end.

-spec build_insertion_ctx_check_prev_height(
    chain_node(), chain_node(), chain_node()) ->
    {ok, chain_node(), chain_node()} | {error, atom()}.
build_insertion_ctx_check_prev_height(Node, PrevNode, PrevKeyNode) ->
    case aec_chain_node:type(Node) of
        key ->
            build_insertion_ctx_check_prev_height_key(Node, PrevNode, PrevKeyNode);
        micro ->
            build_insertion_ctx_check_prev_height_micro(Node, PrevNode, PrevKeyNode)
    end.

build_insertion_ctx_check_prev_height_key(Node, PrevNode, PrevKeyNode) ->
    case node_height(PrevNode) =:= (node_height(Node) - 1) of
        true -> {ok, PrevNode, PrevKeyNode};
        false -> {error, height_inconsistent_for_keyblock_with_previous_hash}
    end.

build_insertion_ctx_check_prev_height_micro(Node, PrevNode, PrevKeyNode) ->
    case node_height(PrevNode) =:= node_height(Node) of
        true -> {ok, PrevNode, PrevKeyNode};
        false -> {error, height_inconsistent_for_microblock_with_previous_hash}
    end.

dirty_db_find_node(Hash) when is_binary(Hash) ->
    case aec_db:dirty_find_header(Hash) of
        {value, Header} -> {ok, wrap_header(Header, Hash)};
        none -> error
    end.

-spec wrap_header(aec_headers:key_header(), aec_chain_node:hash()) -> chain_node().
wrap_header(Header, Hash) ->
    aec_chain_node:new(Header, Hash, aec_headers:type(Header)).

-spec update_recent_cache(chain_node(), insertion_ctx()) -> ok | true.
update_recent_cache(Node,
    #insertion_ctx{window_len = N, recent_key_headers = Recents}) ->
    case aec_chain_node:type(Node) of
        micro -> ok;
        key ->
            Consensus = node_consensus(Node),
            NodeHash = aec_chain_node:hash(Node),
            Entry =
                case N < Consensus:recent_cache_n() of
                    true ->
                        #recent_blocks{
                            key = NodeHash,
                            len = N+1,
                            recents = [Node | Recents]
                        };
                    false ->
                        %% Evict the cache for the oldest entry to ensure an upper bound on the used memory
                        {ToEvict, _} = lists:last(Recents),
                        ets:delete(?RECENT_CACHE, ToEvict),
                        #recent_blocks{
                            key = NodeHash,
                            len = N,
                            recents = [Node | lists:droplast(Recents)]
                        }
                end,
            ets:insert(?RECENT_CACHE, Entry)
    end.

-spec update_top(chain_node()) -> true.
update_top(ChainNode) ->
    ets:insert(?REGISTRY_CACHE, {top_node, ChainNode}).

get_top_node() ->
    ets:lookup_element(?REGISTRY_CACHE, top_node, 2).

get_top_header() ->
    node_header(get_top_node()).

get_top_hash() ->
    node_hash(get_top_node()).

get_top_height() ->
    node_height(get_top_node()).

get_n_key_headers_from(Node, N) ->
    case get_n_key_nodes_from(Node, N) of
        error ->
            error;
        {ok, Headers} ->
            {ok, [node_header(H) || H <- Headers]}
    end.

get_n_key_nodes_from(Node, N) ->
    case node_type(Node) of
        key   ->
            get_n_key_nodes_from({ok, Node}, N, []);
        micro ->
            get_n_key_nodes_from(dirty_db_find_node(node_prev_key_hash(Node)), N, [])
    end.

get_n_key_nodes_from(_, 0, Acc) ->
    {ok, Acc};
get_n_key_nodes_from({ok, Node}, N, Acc) ->
    %% Assert
    key = node_type(Node),
    MaybePrevKeyNode = dirty_db_find_node(node_prev_key_hash(Node)),
    get_n_key_nodes_from(MaybePrevKeyNode, N-1, [Node | Acc]);
get_n_key_nodes_from(error, _N, _Acc) ->
    error.

setup_ets_cache() ->
    %% Lager might not be up so don't bother with logging
    Self = self(),
    Tab = ?RECENT_CACHE,
    Registry = ?REGISTRY_CACHE,
    Keypos = #recent_blocks.key,
    case ets:info(Tab, name) of
        undefined ->
            spawn(fun() ->
                ets:new(Tab, [set, public, named_table, {keypos, Keypos}]),
                ets:new(Registry, [set, public, named_table]),
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
