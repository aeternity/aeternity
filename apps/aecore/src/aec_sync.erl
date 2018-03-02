%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module storing peers list and providing funtions for peer interaction
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
-export([schedule_ping/1]).

%% API called from both aehttp_dispatch_ext and aeu_requests
-export([local_ping_object/0,
         compare_ping_objects/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2,  terminate/2, code_change/3]).

%% Callback for jobs producer queue
-export([sync_worker/0]).

-type http_uri_uri() :: string() | unicode:unicode_binary(). %% From https://github.com/erlang/otp/blob/OTP-20.2.3/lib/inets/doc/src/http_uri.xml#L57

-type ping_obj() :: map().

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec connect_peer(http_uri_uri()) -> ok.
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
    GHash = aec_chain:genesis_hash(),
    TopHash = aec_chain:top_header_hash(),
    Source = aec_peers:get_local_peer_uri(),
    {ok, Difficulty} = aec_chain:difficulty_at_top_block(),
    {ok, PubKey} = aec_keys:pubkey(),
    #{<<"genesis_hash">> => GHash,
      <<"best_hash">>    => TopHash,
      <<"difficulty">>   => Difficulty,
      <<"source">>       => Source,
      <<"source_id">>    => PubKey,
      <<"share">>        => 32,  % TODO: make this configurable
      <<"peers">>        => []}.

%% Compare local and remote 'Ping' objects.
%% 1. Compare genesis blocks. If they differ, return error
%% 2. Compare best_hash. If they are the same, we're in sync
%% 3. Compare difficulty: if ours is greater, we don't initiate sync
%%    - Otherwise, trigger a sync, return 'ok'.
%% Note that the caller ensures the Uri to be a valid Uri
-spec compare_ping_objects(http_uri_uri(), ping_obj(), ping_obj()) -> ok | {error, any()}.
compare_ping_objects(RemoteUri, Local, Remote) ->
    lager:debug("Compare (~p): Local: ~p; Remote: ~p", [RemoteUri, Local, Remote]),
    ok = aec_peers:add(RemoteUri, false),  %% in case aec_peers has restarted inbetween
    case {maps:get(<<"genesis_hash">>, Local),
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
                    server_get_missing_blocks(RemoteUri);
                _ ->
                    Dl = maps:get(<<"difficulty">>, Local),
                    Dr = maps:get(<<"difficulty">>, Remote),
                    if Dl > Dr ->
                         lager:debug("Our difficulty is higher", []),
                         server_get_missing_blocks(RemoteUri);
                       true ->
                         start_sync(RemoteUri)
                    end
            end,
            fetch_mempool(RemoteUri),
            ok;
        _ ->
            {error, different_genesis_blocks}
    end.


start_sync(Uri) ->
    gen_server:cast(?MODULE, {start_sync, Uri}).

server_get_missing_blocks(Uri) ->
    gen_server:cast(?MODULE, {server_get_missing, Uri}).

fetch_mempool(Uri) ->
    gen_server:cast(?MODULE, {fetch_mempool, Uri}).

schedule_ping(Uri) ->
    gen_server:cast(?MODULE, {schedule_ping, Uri}).

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
    aec_peers:add_and_ping_peers(Peers, true),
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
handle_cast({schedule_ping, Uri}, State) ->
    jobs:enqueue(sync_jobs, {ping, Uri}),
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
        {ping, Uri} ->
            await_aehttp_and_ping_peer(Uri);
        _Other ->
            lager:debug("unknown job", [])
    end.

enqueue(Op, Msg) ->
    [ jobs:enqueue(sync_jobs, {Op, Msg, Uri}) || Uri <- aec_peers:get_random(all) ].

await_aehttp_and_ping_peer(Uri) ->
    %% Don't ping until our own HTTP endpoint is up. This is not strictly
    %% needed for the ping itself, but given that a ping can quickly
    %% lead to a greater discovery, we should be prepared to handle pings
    %% ourselves at this point.
    %% The URI comes from aec_peers and is a valid http url.
    case await_aehttp() of
        ok ->
            ping_peer(Uri);
        {error, timeout} ->
            lager:debug("timeout waiting for aehttp - no ping (~p) will retry", [Uri]),
            await_aehttp_and_ping_peer(Uri)
    end.

ping_peer(Uri) ->
    LocalPingObj = local_ping_object(),
    Res = aeu_requests:ping(Uri, LocalPingObj),
    lager:debug("ping result (~p): ~p", [Uri, Res]),
    case Res of
        {ok, RemotePingObj, RemotePeers} ->
            case compare_ping_objects(Uri, LocalPingObj, RemotePingObj) of
                ok ->
                    %% log_ping adds peer as side-effect
                    %% (in case aec_peers has restarted inbetween)
                    %% If it has been user removed before it was scheduled
                    %% it is also added again.
                    aec_peers:log_ping(Uri, good),
                    aec_peers:add_and_ping_peers(RemotePeers);
                {error, different_genesis_blocks} ->
                    aec_peers:block_peer(Uri)
            end;
        {error, protocol_violation} ->
            aec_peers:block_peer(Uri);
        {error, _} ->
            %% If we ping a peer with wrong API version, time out in aue_request
            %% and Peer is errored until it upgrades/downgrades to the same version.
            aec_peers:log_ping(Uri, error)
  end.

%% The gproc name below is registered in the start function of
%% aehttp_app, and serves as a synch point. The timeout is hopefully
%% large enough to reflect only error conditions. Expected wait time
%% should be a fraction of a second, if any.
await_aehttp() ->
    try
      gproc:await({n,l,{epoch,app,aehttp}}, 10000), % should (almost) never timeout
      ok
    catch error:{_Reason, _Args} ->
      {error, timeout}
    end.

do_forward_block(Block, Uri) ->
    Res = aeu_requests:send_block(Uri, Block),
    lager:debug("send_block Res (~p): ~p", [Uri, Res]).

do_forward_tx(Tx, Uri) ->
    Res = aeu_requests:send_tx(Uri, Tx),
    lager:debug("send_tx Res (~p): ~p", [Uri, Res]).

do_start_sync(Uri) ->
    aec_events:publish(chain_sync, {client_start, Uri}),
    fetch_chain(Uri, aec_chain:genesis_hash()).

do_server_get_missing(Uri) ->
    aec_events:publish(chain_sync, {server_start, Uri}),
    do_get_missing_blocks(Uri),
    aec_events:publish(chain_sync, {server_done, Uri}).

fetch_chain(Uri, GHash) ->
    case aeu_requests:top(Uri) of
        {ok, Hdr} ->
            lager:debug("Top hdr (~p): ~p", [Uri, pp(Hdr)]),
            {ok, Hash} = aec_headers:hash_header(Hdr),
            fetch_chain(Hash, Uri, GHash, true, []);
        {error, Reason} ->
            lager:debug("fetching top block (~p) failed: ~p", [Uri, Reason])
    end.

%% Fetch the chain, keep track of if we can be connected to genesis.
%% Initially we can be connected, later we can only be connected
%% after we have actually fetched a block from another peer, and then
%% found a block locally (in the extreme case that local block will
%% be the genesis).
fetch_chain(GHash, Uri, GHash, _MaybeConnected, Acc) ->
    chain_fetched(Uri, Acc);
fetch_chain(Hash, Uri, GHash, MaybeConnected, Acc) ->
    case do_fetch_block(Hash, Uri) of
        {ok, false, Block} -> %% We already have this block
            case MaybeConnected andalso is_connected_to_genesis(Hash) of
                true ->
                    %% We are done
                    chain_fetched(Uri, Acc);
                false ->
                    Hdr = aec_blocks:to_header(Block),
                    fetch_chain(aec_headers:prev_hash(Hdr), Uri, GHash, false, Acc)
            end;
        {ok, true, Block} -> %% We fetched this block
            Hdr = aec_blocks:to_header(Block),
            fetch_chain(aec_headers:prev_hash(Hdr), Uri, GHash, true, [{Hash, Block}|Acc]);
        {error, _} ->
            %% Post the bit we have
            chain_fetched(Uri, Acc)
    end.

is_connected_to_genesis(Hash) ->
    lager:debug("Checking if ~p is connected to genesis", [pp(Hash)]),
    aec_chain:hash_is_connected_to_genesis(Hash).

chain_fetched(Uri, Acc) ->
    try_write_blocks(Acc),
    do_get_missing_blocks(Uri),
    aec_events:publish(chain_sync, {client_done, Uri}).

fetch_block(Hash, Uri) ->
    case do_fetch_block_ext(Hash, Uri) of
        {ok, _, Block} ->
            lager:debug("Block (~p) fetched from ~p", [pp(Hash), Uri]),
            [{Hash, Block}];
        {error, _} = Err ->
            lager:error("Error fetching block (~p) from ~p: ~p",
                        [pp(Hash), Uri, Err]),
            []
    end.

do_fetch_block(Hash, Uri) ->
    case aec_chain:get_block(Hash) of
        {ok, Block} ->
            lager:debug("block ~p already fetched, using local copy", [pp(Hash)]),
            {ok, false, Block};
        error ->
            do_fetch_block_ext(Hash, Uri)
    end.

do_fetch_block_ext(Hash, Uri) ->
    lager:debug("we don't have the block -fetching (~p)", [pp(Hash)]),
    case aeu_requests:get_block(Uri, Hash) of
        {ok, Block} ->
            case header_hash(Block) =:= Hash of
                true ->
                    lager:debug("block fetched from ~p (~p); ~p",
                                [Uri, pp(Hash), pp(Block)]),
                    {ok, true, Block};
                false ->
                    {error, hash_mismatch}
            end;
        {error, _} = Error ->
            lager:debug("failed to fetch block from ~p; Hash = ~p; Error = ~p",
                        [Uri, pp(Hash), Error]),
            Error
    end.

do_get_missing_blocks(Uri) ->
    Missing = aec_chain:get_missing_block_hashes(),
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
                  fetch_block(Hash, Uri) ++ Acc
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

