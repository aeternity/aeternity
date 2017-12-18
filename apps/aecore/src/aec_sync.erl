%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module storing peers list and providing funtionc for peers interaction
%%% @end
%%%=============================================================================
-module(aec_sync).

-behaviour(gen_server).

-import(aeu_debug, [pp/1]).

%% API
-export([connect_peer/1,
         start_link/0
        ]).

-export([subset_size/0,
         set_subset_size/1]).

-export([schedule_ping/2]).
-export([local_ping_object/0,
         compare_ping_objects/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2,  terminate/2, code_change/3]).

%% Callback for jobs producer queue
-export([sync_worker/0]).

-type ping_obj() :: map().

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec connect_peer(http_uri:uri()) -> ok.
connect_peer(Uri) ->
    gen_server:cast(?MODULE, {connect, Uri}).

-spec subset_size() -> non_neg_integer().
subset_size() ->
    gen_server:call(?MODULE, subset_size).

-spec set_subset_size(Sz :: non_neg_integer()) ->
                             PrevSz when PrevSz :: non_neg_integer().
set_subset_size(Sz) when is_integer(Sz), Sz > 0 ->
    gen_server:call(?MODULE, {set_subset_size, Sz}).

%% Builds a 'Ping' object for the initial ping.
%% The 'Ping' object contains the following data:
%% - source: our own peer uri
%% - genesis_hash: base64-encoded hash of our genesis block
%% - best_hash: base64-encoded hash of our top block
%% - difficulty: (Ethereum: total_difficulty) our top_work
%% - share: how many random peers we'd like the other node to share
%% - peers: a random subset (size `Share`) that we know of
%%
%% Note that the peers element is not populated by this function.
%% The pinger should insert a random list, excluding the pinged peer.
%% The pinged should insert a random list, excluding the pinging peer,
%%  as well as any peers that it shared.

-type ping_object() :: map().
-spec local_ping_object() -> ping_object().
local_ping_object() ->
    GHash = aec_conductor:genesis_hash(),
    TopHash = aec_conductor:top_header_hash(),
    Source = source_uri(),
    {ok, Difficulty} = aec_conductor:get_total_difficulty(),
    #{<<"genesis_hash">> => base64:encode(GHash),
      <<"best_hash">>    => base64:encode(TopHash),
      <<"difficulty">>   => Difficulty,
      <<"source">>       => Source,
      <<"share">>        => 32,  % TODO: make this configurable
      <<"peers">>        => []}.

%% Compare local and remote 'Ping' objects.
%% 1. Compare genesis blocks. If they differ, return error
%% 2. Compare best_hash. If they are the same, we're in sync
%% 3. Compare difficulty: if ours is greater, we don't initiate sync
%%    - Otherwise, trigger a sync, return 'ok'.
-spec compare_ping_objects(ping_obj(), ping_obj()) -> ok | {error, any()}.
compare_ping_objects(Local, Remote) ->
    lager:debug("Compare, Local: ~p; Remote: ~p", [pp(Local), pp(Remote)]),
    Src = maps:get(<<"source">>, Remote),
    Res = case {maps:get(<<"genesis_hash">>, Local),
                maps:get(<<"genesis_hash">>, Remote)} of
              {G, G} ->
                  lager:debug("genesis blocks match", []),
                  %% same genesis block - continue
                  case {maps:get(<<"best_hash">>, Local),
                        maps:get(<<"best_hash">>, Remote)} of
                      {T, T} ->
                          lager:debug("same top blocks", []),
                          %% headers in sync; check missing blocks
                          %% Note that in this case, both will publish
                          %% events as if they're the server (basically
                          %% meaning that they were tied for server position).
                          server_get_missing_blocks(Src);
                      _ ->
                          Dl = maps:get(<<"difficulty">>, Local),
                          Dr = maps:get(<<"difficulty">>, Remote),
                          if Dl > Dr ->
                                  lager:debug("Our difficulty is higher", []),
                                  server_get_missing_blocks(Src),
                                  ok;
                             true ->
                                  start_sync(Src)
                          end
                  end,
                  ok;
              _ ->
                  {error, different_genesis_blocks}
          end,
    fetch_mempool(Src),
    Res.

start_sync(PeerUri) ->
    gen_server:cast(?MODULE, {start_sync, PeerUri}).

server_get_missing_blocks(PeerUri) ->
    gen_server:cast(?MODULE, {server_get_missing, PeerUri}).

fetch_mempool(PeerUri) ->
    gen_server:cast(?MODULE, {fetch_mempool, PeerUri}).

schedule_ping(PeerUri, PingF) when is_function(PingF, 1) ->
    gen_server:cast(?MODULE, {schedule_ping, PeerUri, PingF}).

%%%=============================================================================
%%% gen_server functions
%%%=============================================================================

%% The 'subset_size' attribute is meant to denote the number of peers
%% to use (in random order) for broadcasting blocks. It's not clear that
%% using anything less than 'all' peers would be a good idea.
-record(state, {subset_size = all         :: all | non_neg_integer(),
                peers = gb_trees:empty(),
                pings = gb_trees:empty()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    aec_events:subscribe(block_created),
    aec_events:subscribe(top_changed),
    aec_events:subscribe(tx_created),
    Peers = application:get_env(aecore, peers, []),
    BlockedPeers = application:get_env(aecore, blocked_peers, []),
    [aec_peers:block_peer(P) || P <- BlockedPeers],
    aec_peers:add_and_ping_peers(Peers),
    {ok, #state{}}.

handle_call(subset_size, _From, #state{subset_size = Sz} = State) ->
    {reply, Sz, State};
handle_call({set_subset_size, Sz}, _From, #state{subset_size = PrevSz} = State)
  when is_integer(Sz), Sz > 0 ->
    {reply, PrevSz, State#state{subset_size = Sz}};
handle_call(_, _From, State) ->
    {reply, error, State}.

handle_cast({connect, Uri}, State) ->
    aec_peers:add(Uri, _Connect = true),
    {noreply, State};
handle_cast({start_sync, PeerUri}, State) ->
    jobs:enqueue(sync_jobs, {start_sync, PeerUri}),
    {noreply, State};
handle_cast({server_get_missing, PeerUri}, State) ->
    jobs:enqueue(sync_jobs, {server_get_missing, PeerUri}),
    {noreply, State};
handle_cast({fetch_mempool, PeerUri}, State) ->
    jobs:enqueue(sync_jobs, {fetch_mempool, PeerUri}),
    {noreply, State};
handle_cast({schedule_ping, PeerUri, PingF}, State) ->
    jobs:enqueue(sync_jobs, {ping, PeerUri, PingF}),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, Event, #{info := Info}}, State) ->
    case Event of
        block_created   -> enqueue(forward, #{status => created,
                                              block => Info}, State);
        top_changed     -> enqueue(forward, #{status => top_changed,
                                              block => Info}, State);
        tx_created      -> enqueue(forward, #{status => created,
                                              tx => Info}, State);
        tx_received     -> enqueue(forward, #{status => received,
                                              tx => Info}, State);
        _ -> ignore
    end,
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Jobs worker
%%%=============================================================================

sync_worker() ->
    Res = jobs:dequeue(sync_jobs, 1),
    lager:debug("dequed job ~p", [pp(Res)]),
    process_job(Res).

%% Note: we always dequeue exactly ONE job
process_job([{T, Job}]) ->
    reg_worker(Job, T),
    case Job of
        {forward, #{block := Block}, Peer} ->
            do_forward_block(Block, Peer);
        {forward, #{tx := Tx}, Peer} ->
            do_forward_tx(Tx, Peer);
        {start_sync, PeerUri} ->
            do_start_sync(PeerUri);
        {server_get_missing, PeerUri} ->
            do_server_get_missing(PeerUri);
        {fetch_mempool, PeerUri} ->
            do_fetch_mempool(PeerUri);
        {ping, PeerUri, PingF} ->
            PingF(PeerUri);
        _Other ->
            lager:debug("unknown job", [])
    end.

enqueue(Op, Msg, State) ->
    case aec_peers:get_random(State#state.subset_size) of
        [] ->
            ok;
        [_|_] = Peers ->
            [enqueue_recv({Op, Msg, Peer}) || Peer <- Peers]
    end.

enqueue_recv(Job) ->
    jobs:enqueue(sync_jobs, Job).

do_forward_block(Block, Peer) ->
    Res = aeu_requests:send_block(Peer, Block),
    lager:debug("send_block Res (~p): ~p", [Peer, Res]).

do_forward_tx(Tx, Peer) ->
    Res = aeu_requests:send_tx(Peer, Tx),
    lager:debug("send_tx Res (~p): ~p", [Peer, Res]).

do_start_sync(PeerUri) ->
    aec_events:publish(chain_sync, {client_start, PeerUri}),
    fetch_headers(PeerUri, genesis_hash()).

do_server_get_missing(PeerUri) ->
    aec_events:publish(chain_sync, {server_start, PeerUri}),
    do_get_missing_blocks(PeerUri),
    aec_events:publish(chain_sync, {server_done, PeerUri}).

genesis_hash() ->
    aec_conductor:genesis_hash().


fetch_headers(PeerUri, GHash) ->
    case aeu_requests:top(PeerUri) of
        {ok, Hdr} ->
            lager:debug("Top hdr (~p): ~p", [PeerUri, pp(Hdr)]),
            {ok, HdrHash} = aec_headers:hash_header(Hdr),
            case HdrHash of
                GHash ->
                    %% already the genesis hash
                    ok;
                _ ->
                    fetch_headers_1(HdrHash, Hdr, PeerUri, GHash, [])
            end;
        {error, Reason} ->
            lager:debug("fetching top block (~p) failed: ~p", [PeerUri, Reason])
    end.

fetch_headers_1(HdrHash, Hdr, PeerUri, GHash, Acc) ->
    {ResTag, _} = aec_conductor:get_header_by_hash(HdrHash),
    case ((ResTag =:= ok)
          andalso aec_conductor:hash_is_connected_to_genesis(HdrHash)) of
        true ->
            %% we have a continuous header chain
            headers_fetched(PeerUri, Acc);
        false ->
            lager:debug("we don't have the top block header,"
                        " fetching block (~p)", [pp(HdrHash)]),
            Acc1 = fetch_block(HdrHash, PeerUri, Acc),
            fetch_next_hdr(aec_headers:prev_hash(Hdr), PeerUri, GHash, Acc1)
    end.

fetch_block(Hash, PeerUri, Acc) ->
    case do_fetch_block(Hash, PeerUri) of
        {ok, Block} ->
            lager:debug("Block (~p|~p) fetched", [PeerUri, pp(Hash)]),
            [{Hash, Block}|Acc];
        {error,_} = Err ->
            %% What is the right course of action here? Continue? Abort?
            lager:error("Error fetching block (~p|~p): ~p",
                        [PeerUri, Hash, Err]),
            Acc
    end.

do_fetch_block(Hash, PeerUri) ->
    case aeu_requests:block(PeerUri, Hash) of
        {ok, Block} ->
            case header_hash(Block) =:= Hash of
                true ->
                    lager:debug("block fetched from ~p (~p); ~p",
                                [PeerUri, pp(Hash), pp(Block)]),
                    {ok, Block};
                false ->
                    {error, hash_mismatch}
            end;
        {error, _} = Error ->
            lager:debug("failed to fetch block from ~p; Hash = ~p; Error = ~p",
                        [PeerUri, pp(Hash), Error]),
            Error
    end.

fetch_next_hdr(GHash, PeerUri, GHash, Acc) ->
    %% No need to fetch the genesis block
    headers_fetched(PeerUri, Acc);
fetch_next_hdr(Hash, PeerUri, GHash, Acc) ->
    case do_fetch_block(Hash, PeerUri) of
        {ok, Block} ->
            fetch_hdrs_block_recvd(
              Hash, Block, PeerUri, GHash, Acc);
        {error, _} ->
            %% Unexpected. Try to write the blocks we have
            headers_fetched(PeerUri, Acc)
    end.

fetch_hdrs_block_recvd(HdrHash, Block, PeerUri, GHash, Acc) ->
    {ResTag, _} = aec_conductor:get_block_by_hash(HdrHash),
    case ((ResTag =:= ok)
          andalso aec_conductor:hash_is_connected_to_genesis(HdrHash)) of
        true ->
            lager:debug(
              "we have this block, go to next phase (~p)",
              [pp(HdrHash)]),
            %% we're done
            headers_fetched(PeerUri, Acc);
        false ->
            lager:debug("new block, continue (~p)", [pp(HdrHash)]),
            Hdr = aec_blocks:to_header(Block),
            fetch_next_hdr(
              aec_headers:prev_hash(Hdr), PeerUri, GHash,
              [{HdrHash, Block}|Acc])
    end.

headers_fetched(PeerUri, Acc) ->
    try_write_blocks(Acc),
    do_get_missing_blocks(PeerUri),
    aec_events:publish(chain_sync, {client_done, PeerUri}).

do_get_missing_blocks(PeerUri) ->
    Missing = aec_conductor:get_missing_block_hashes(),
    lager:debug("Missing block hashes: ~p", [pp(Missing)]),
    fetch_missing_blocks(Missing, PeerUri).

try_write_blocks(Blocks) ->
    lists:foreach(fun sync_post_block/1, Blocks).

sync_post_block({_Hash, Block}) ->
    %% No event publication for now (esp. not block_received!)
    lager:debug("Calling post_block(~p)", [pp(Block)]),
    aec_conductor:add_synced_block(Block).

fetch_missing_blocks(Hashes, PeerUri) ->
    Blocks =
        lists:foldl(
          fun(Hash, Acc) ->
                  fetch_block(Hash, PeerUri, Acc)
          end, [], Hashes),
    try_write_blocks(Blocks).

do_fetch_mempool(PeerUri) ->
    case aeu_requests:transactions(PeerUri) of
        {ok, Txs} ->
            lager:debug("Mempool (~p) received, size: ~p",
                        [PeerUri, length(Txs)]),
            aec_tx_pool:push(Txs),
            aec_events:publish(mempool_sync, {fetched, PeerUri});
        Other ->
            lager:debug("Error fetching mempool from ~p: ~p",
                        [PeerUri, Other]),
            Other
    end.

reg_worker(Job, T) ->
    gproc:reg(worker_prop(Job), T).

worker_prop(Job) ->
    {p, l, {?MODULE, worker, Job}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

header_hash(Block) ->
    Header = aec_blocks:to_header(Block),
    {ok, HeaderHash} = aec_headers:hash_header(Header),
    HeaderHash.

source_uri() ->
    list_to_binary(aec_peers:get_local_peer_uri()).
