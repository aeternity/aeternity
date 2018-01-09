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

%% API called from strongly connected component aec_peers
-export([schedule_ping/2]).

%% API called from both aehttp_dispatch_ext and aeu_requests
-export([local_ping_object/0,
         compare_ping_objects/3]).

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

%% Builds a 'Ping' object for the initial ping.
%% The 'Ping' object contains the following data:
%% - source: our own peer uri
%% - genesis_hash: base58Check-encoded hash of our genesis block
%% - best_hash: base58Check-encoded hash of our top block
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
    Source = aec_peers:get_local_peer_uri(),
    {ok, Difficulty} = aec_conductor:get_total_difficulty(),
    #{<<"genesis_hash">> => aec_base58c:encode(block_hash, GHash),
      <<"best_hash">>    => aec_base58c:encode(block_hash, TopHash),
      <<"difficulty">>   => Difficulty,
      <<"source">>       => Source,
      <<"share">>        => 32,  % TODO: make this configurable
      <<"peers">>        => []}.

%% Compare local and remote 'Ping' objects.
%% 1. Compare genesis blocks. If they differ, return error
%% 2. Compare best_hash. If they are the same, we're in sync
%% 3. Compare difficulty: if ours is greater, we don't initiate sync
%%    - Otherwise, trigger a sync, return 'ok'.
-spec compare_ping_objects(http_uri:uri(), ping_obj(), ping_obj()) -> ok | {error, any()}.
compare_ping_objects(RemoteUri, Local, Remote) ->
    lager:debug("Compare (~p): Local: ~p; Remote: ~p", [RemoteUri, pp(Local), pp(Remote)]),
    case {maps:get(<<"genesis_hash">>, Local),
          maps:get(<<"genesis_hash">>, Remote)} of
        {G, G} ->
            lager:debug("genesis blocks match", []),
            %% same genesis block - continue
            compare_genesis_blocks_match(RemoteUri, Local, Remote);
        _ ->
            {error, different_genesis_blocks}
    end.

compare_genesis_blocks_match(RemoteUri, Local, Remote) ->
    case compare_top_blocks(Local, Remote) of
        {error, _} = Error ->
            Error;
        {ok, Next} ->
            case Next of
                get_missing_blocks ->
                    server_get_missing_blocks(RemoteUri);
                start_sync ->
                    start_sync(RemoteUri)
            end,
            fetch_mempool(RemoteUri),
            ok
    end.

compare_top_blocks(Local, Remote) ->
    case {maps:get(<<"best_hash">>, Local),
          maps:get(<<"best_hash">>, Remote)} of
        {T, T} ->
            lager:debug("same top blocks", []),
            %% headers in sync; check missing blocks
            %% Note that in this case, both will publish
            %% events as if they're the server (basically
            %% meaning that they were tied for server position).
            {ok, get_missing_blocks};
        {_, RemHash} ->
            case validate_remote_hash(RemHash) of
                true ->
                    Dl = maps:get(<<"difficulty">>, Local),
                    Dr = maps:get(<<"difficulty">>, Remote),
                    if Dl > Dr ->
                            lager:debug("Our difficulty is higher", []),
                            {ok, get_missing_blocks};
                       true ->
                            {ok, start_sync}
                    end;
                false ->
                    {error, invalid_block_hash}
            end
    end.


validate_remote_hash(Hash) ->
    try aec_base58c:decode(Hash) of
        {block_hash, _} ->
            true;
        _ ->
            false
    catch
        error:_ ->
            false
    end.

start_sync(Uri) ->
    gen_server:cast(?MODULE, {start_sync, Uri}).

server_get_missing_blocks(Uri) ->
    gen_server:cast(?MODULE, {server_get_missing, Uri}).

fetch_mempool(Uri) ->
    gen_server:cast(?MODULE, {fetch_mempool, Uri}).

schedule_ping(Peer, PingF) when is_function(PingF, 1) ->
    gen_server:cast(?MODULE, {schedule_ping, Peer, PingF}).

%%%=============================================================================
%%% gen_server functions
%%%=============================================================================

-record(state, {}).

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

handle_call(_, _From, State) ->
    {reply, error, State}.

handle_cast({connect, Uri}, State) ->
    aec_peers:add(Uri, _Connect = true),
    {noreply, State};
handle_cast({start_sync, Uri}, State) ->
    jobs:enqueue(sync_jobs, {start_sync, Uri}),
    {noreply, State};
handle_cast({server_get_missing, Uri}, State) ->
    jobs:enqueue(sync_jobs, {server_get_missing, Uri}),
    {noreply, State};
handle_cast({fetch_mempool, Uri}, State) ->
    jobs:enqueue(sync_jobs, {fetch_mempool, Uri}),
    {noreply, State};
handle_cast({schedule_ping, Peer, PingF}, State) ->
    jobs:enqueue(sync_jobs, {ping, Peer, PingF}),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, Event, #{info := Info}}, State) ->
    case Event of
        block_created   -> enqueue(forward, #{status => created,
                                              block => Info});
        top_changed     -> enqueue(forward, #{status => top_changed,
                                              block => Info});
        tx_created      -> enqueue(forward, #{status => created,
                                              tx => Info});
        tx_received     -> enqueue(forward, #{status => received,
                                              tx => Info});
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
process_job([{_T, Job}]) ->
    case Job of
        {forward, #{block := Block}, Uri} ->
            do_forward_block(Block, Uri);
        {forward, #{tx := Tx}, Uri} ->
            do_forward_tx(Tx, Uri);
        {start_sync, Uri} ->
            do_start_sync(Uri);
        {server_get_missing, Uri} ->
            do_server_get_missing(Uri);
        {fetch_mempool, Uri} ->
            do_fetch_mempool(Uri);
        {ping, Peer, PingF} ->
            PingF(Peer);
        _Other ->
            lager:debug("unknown job", [])
    end.

enqueue(Op, Msg) ->
    [ jobs:enqueue(sync_jobs, {Op, Msg, Uri}) || Uri <- aec_peers:get_random(all) ].

do_forward_block(Block, Uri) ->
    Res = aeu_requests:send_block(Uri, Block),
    lager:debug("send_block Res (~p): ~p", [Uri, Res]).

do_forward_tx(Tx, Uri) ->
    Res = aeu_requests:send_tx(Uri, Tx),
    lager:debug("send_tx Res (~p): ~p", [Uri, Res]).

do_start_sync(Uri) ->
    aec_events:publish(chain_sync, {client_start, Uri}),
    fetch_headers(Uri, genesis_hash()).

do_server_get_missing(Uri) ->
    aec_events:publish(chain_sync, {server_start, Uri}),
    do_get_missing_blocks(Uri),
    aec_events:publish(chain_sync, {server_done, Uri}).

genesis_hash() ->
    aec_conductor:genesis_hash().


fetch_headers(Uri, GHash) ->
    case aeu_requests:top(Uri) of
        {ok, Hdr} ->
            lager:debug("Top hdr (~p): ~p", [Uri, pp(Hdr)]),
            {ok, HdrHash} = aec_headers:hash_header(Hdr),
            case HdrHash of
                GHash ->
                    %% already the genesis hash
                    ok;
                _ ->
                    fetch_headers_1(HdrHash, Hdr, Uri, GHash, [])
            end;
        {error, Reason} ->
            lager:debug("fetching top block (~p) failed: ~p", [Uri, Reason])
    end.

fetch_headers_1(HdrHash, Hdr, Uri, GHash, Acc) ->
    {ResTag, _} = aec_conductor:get_header_by_hash(HdrHash),
    case ((ResTag =:= ok)
          andalso aec_conductor:hash_is_connected_to_genesis(HdrHash)) of
        true ->
            %% we have a continuous header chain
            headers_fetched(Uri, Acc);
        false ->
            lager:debug("we don't have the top block header,"
                        " fetching block (~p)", [pp(HdrHash)]),
            Acc1 = fetch_block(HdrHash, Uri, Acc),
            fetch_next_hdr(aec_headers:prev_hash(Hdr), Uri, GHash, Acc1)
    end.

fetch_block(Hash, Uri, Acc) ->
    case do_fetch_block(Hash, Uri) of
        {ok, Block} ->
            lager:debug("Block (~p|~p) fetched", [Uri, pp(Hash)]),
            [{Hash, Block}|Acc];
        {error,_} = Err ->
            %% What is the right course of action here? Continue? Abort?
            lager:error("Error fetching block (~p|~p): ~p",
                        [Uri, Hash, Err]),
            Acc
    end.

do_fetch_block(Hash, Uri) ->
    case aeu_requests:get_block(Uri, Hash) of
        {ok, Block} ->
            case header_hash(Block) =:= Hash of
                true ->
                    lager:debug("block fetched from ~p (~p); ~p",
                                [Uri, pp(Hash), pp(Block)]),
                    {ok, Block};
                false ->
                    {error, hash_mismatch}
            end;
        {error, _} = Error ->
            lager:debug("failed to fetch block from ~p; Hash = ~p; Error = ~p",
                        [Uri, pp(Hash), Error]),
            Error
    end.

fetch_next_hdr(GHash, Uri, GHash, Acc) ->
    %% No need to fetch the genesis block
    headers_fetched(Uri, Acc);
fetch_next_hdr(Hash, Uri, GHash, Acc) ->
    case do_fetch_block(Hash, Uri) of
        {ok, Block} ->
            fetch_hdrs_block_recvd(
              Hash, Block, Uri, GHash, Acc);
        {error, _} ->
            %% Unexpected. Try to write the blocks we have
            headers_fetched(Uri, Acc)
    end.

fetch_hdrs_block_recvd(HdrHash, Block, Uri, GHash, Acc) ->
    {ResTag, _} = aec_conductor:get_block_by_hash(HdrHash),
    case ((ResTag =:= ok)
          andalso aec_conductor:hash_is_connected_to_genesis(HdrHash)) of
        true ->
            lager:debug(
              "we have this block, go to next phase (~p)",
              [pp(HdrHash)]),
            %% we're done
            headers_fetched(Uri, Acc);
        false ->
            lager:debug("new block, continue (~p)", [pp(HdrHash)]),
            Hdr = aec_blocks:to_header(Block),
            fetch_next_hdr(
              aec_headers:prev_hash(Hdr), Uri, GHash,
              [{HdrHash, Block}|Acc])
    end.

headers_fetched(Uri, Acc) ->
    try_write_blocks(Acc),
    do_get_missing_blocks(Uri),
    aec_events:publish(chain_sync, {client_done, Uri}).

do_get_missing_blocks(Uri) ->
    Missing = aec_conductor:get_missing_block_hashes(),
    lager:debug("Missing block hashes: ~p", [pp(Missing)]),
    fetch_missing_blocks(Missing, Uri).

try_write_blocks(Blocks) ->
    lists:foreach(fun sync_post_block/1, Blocks).

sync_post_block({_Hash, Block}) ->
    %% No event publication for now (esp. not block_received!)
    lager:debug("Calling post_block(~p)", [pp(Block)]),
    aec_conductor:add_synced_block(Block).

fetch_missing_blocks(Hashes, Uri) ->
    Blocks =
        lists:foldl(
          fun(Hash, Acc) ->
                  fetch_block(Hash, Uri, Acc)
          end, [], Hashes),
    try_write_blocks(Blocks).

do_fetch_mempool(Uri) ->
    case aeu_requests:transactions(Uri) of
        {ok, Txs} ->
            lager:debug("Mempool (~p) received, size: ~p",
                        [Uri, length(Txs)]),
            aec_tx_pool:push(Txs),
            aec_events:publish(mempool_sync, {fetched, Uri});
        Other ->
            lager:debug("Error fetching mempool from ~p: ~p",
                        [Uri, Other]),
            Other
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

header_hash(Block) ->
    Header = aec_blocks:to_header(Block),
    {ok, HeaderHash} = aec_headers:hash_header(Header),
    HeaderHash.

