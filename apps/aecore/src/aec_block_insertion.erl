%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc Helpers for ADT's used during block insertion, those helpers are meant
%%%      mainly for aec_chain_state and consensus modules
%%% @end
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
        , get_top_height/0
        ]).

-include("aec_block_insertion.hrl").

-opaque chain_node() :: #chain_node{}. % type 'node' is Erlang internal!
-export_type([chain_node/0]).

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

node_prev_hash(#chain_node{header = H}) -> aec_headers:prev_hash(H).

node_prev_key_hash(#chain_node{header = H}) -> aec_headers:prev_key_hash(H).

node_height(#chain_node{header = H}) -> aec_headers:height(H).

node_difficulty(#chain_node{type = micro}) -> 0;
node_difficulty(#chain_node{header = H}) -> aec_headers:difficulty(H).

node_target(#chain_node{header = H}) -> aec_headers:target(H).

node_root_hash(#chain_node{header = H}) -> aec_headers:root_hash(H).

node_miner(#chain_node{header = H}) -> aec_headers:miner(H).

node_beneficiary(#chain_node{header = H}) -> aec_headers:beneficiary(H).

node_type(#chain_node{type = T}) -> T.

node_time(#chain_node{header = H}) -> aec_headers:time_in_msecs(H).

node_version(#chain_node{header = H}) -> aec_headers:version(H).

node_is_key_block(N) -> node_type(N) =:= key.

node_is_micro_block(N) -> node_type(N) =:= micro.

node_hash(#chain_node{hash = Hash}) -> Hash.

node_header(#chain_node{header = H}) -> H.

node_consensus(#chain_node{header = H}) -> aec_headers:consensus_module(H).

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
                {ok, #chain_node{type = key}} ->
                    {error, prev_key_hash_inconsistency};
                {ok, PrevNode} ->
                    case node_prev_key_hash(PrevNode) =:= PrevKeyHash of
                        true ->
                            %% Now assert heights
                            build_insertion_ctx_check_prev_height(Node, PrevNode, PrevKeyNode);
                        false ->
                            {error, prev_key_hash_inconsistency}
                    end
            end
   end.

build_insertion_ctx_check_prev_height(#chain_node{type = key} = Node, PrevNode, PrevKeyNode) ->
    case node_height(PrevNode) =:= (node_height(Node) - 1) of
        true -> {ok, PrevNode, PrevKeyNode};
        false -> {error, height_inconsistent_for_keyblock_with_previous_hash}
    end;
build_insertion_ctx_check_prev_height(#chain_node{type = micro} = Node, PrevNode, PrevKeyNode) ->
    case node_height(PrevNode) =:= node_height(Node) of
        true -> {ok, PrevNode, PrevKeyNode};
        false -> {error, height_inconsistent_for_microblock_with_previous_hash}
    end.

dirty_db_find_node(Hash) when is_binary(Hash) ->
    case aec_db:dirty_find_header(Hash) of
        {value, Header} -> {ok, wrap_header(Header, Hash)};
        none -> error
    end.

wrap_header(Header, Hash) ->
    #chain_node{
        header = Header
        , hash = Hash
        , type = aec_headers:type(Header)
    }.

update_recent_cache(#chain_node{type = micro}, _InsertCtx) -> ok;
update_recent_cache(#chain_node{type = key, hash = H} = Node, #insertion_ctx{window_len = N, recent_key_headers = Recents}) ->
    Consensus = node_consensus(Node),
    Entry =
        case N < Consensus:recent_cache_n() of
            true ->
                #recent_blocks{key = H, len = N+1, recents = [Node|Recents]};
            false ->
                %% Evict the cache for the oldest entry to ensure an upper bound on the used memory
                {ToEvict, _} = lists:last(Recents),
                ets:delete(?RECENT_CACHE, ToEvict),
                #recent_blocks{key = H, len = N, recents = [Node|lists:droplast(Recents)]}
        end,
    ets:insert(?RECENT_CACHE, Entry).

update_top(#chain_node{} = Node) ->
    ets:insert(?REGISTRY_CACHE, {top_node, Node}).

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
