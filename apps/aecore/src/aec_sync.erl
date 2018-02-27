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

-record(sync_peer, {difficulty, from, to, hash, uri}).

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
    #{<<"genesis_hash">> => GHash,
      <<"best_hash">>    => TopHash,
      <<"difficulty">>   => Difficulty,
      <<"source">>       => Source,
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
                         start_sync(RemoteUri, maps:get(<<"best_hash">>, Remote), Dr)
                    end
            end,
            fetch_mempool(RemoteUri),
            ok;
        _ ->
            {error, different_genesis_blocks}
    end.


start_sync(Uri, RemoteHash, RemoteDifficulty) ->
    gen_server:cast(?MODULE, {start_sync, Uri, RemoteHash, RemoteDifficulty}).

server_get_missing_blocks(Uri) ->
    gen_server:cast(?MODULE, {server_get_missing, Uri}).

fetch_mempool(Uri) ->
    gen_server:cast(?MODULE, {fetch_mempool, Uri}).

schedule_ping(Uri) ->
    gen_server:cast(?MODULE, {schedule_ping, Uri}).

fetch_next(Uri, HeightIn, HashIn) ->
    gen_server:call(?MODULE, {fetch_chain, Uri, HeightIn, HashIn}).

delete_from_pool(Uri) ->
    gen_server:cast(?MODULE, {delete_from_pool, Uri}).

%% Check whether this header is better than previous best
new_header(Uri, Header, AgreedHeight, Hash) ->
    gen_server:call(?MODULE, {new_header, Uri, Header, AgreedHeight, Hash}).

%%%=============================================================================
%%% gen_server functions
%%%=============================================================================

%% When we Ping a node with at least as much difficulty as we have, 
%% then we are going to sync with it.
%% We already agree upon the genesis block and need to find the highest common
%% block we agree upon. We use a binary search to find out.
%% From the height we agree upon, we start asking for blocks to add to the chain.
%% 
%% When an additional Ping arrives for which we agree upon genesis, we have
%% the following possibilities:
%% 1. It has worst top hash than our node, do not include in sync
%% 2. It has better top hash than our node
%%    We binary search for a node that we agree upon (could be genesis) 
%% We add new node to sync pool to sync agreed node upto top of new
%% 1. If we are already synchronizing, we only update From and To,
%%    the ongoing sync will pick that up
%% 2. If we are not already synchronizing, we start doing so.
%% 
%% We sync with several nodes at the same time and should add a strategy 
%% to decide for each node what they should be fetching. This is prepared
%% in the handle call of fetch_chain

-record(state, {sync_pool = []}).

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
    %% When we start the top_header may still be unknown... 
    %% it takes some time to build the chain.
    {ok, #state{}}.

handle_call({new_header, Uri, Header, AgreedHeight, AgreedHash}, _, State) ->
    Height = aec_headers:height(Header),
    Difficulty = aec_headers:difficulty(Header),
    %% We have talked to the Uri, so Uri can be trusted
    {IsNew, NewPool} = insert(#sync_peer{difficulty = Difficulty, 
                                         from = AgreedHeight, 
                                         to = Height, 
                                         hash = AgreedHash, 
                                         uri = Uri}, 
                              State#state.sync_pool),
    {reply, IsNew, State#state{sync_pool = NewPool}};
handle_call({fetch_chain, Uri, HeightIn, HashIn}, _, State) ->
    %% Tell me what to fetch now
    case lists:keyfind(Uri, #sync_peer.uri, State#state.sync_pool) of
         false ->
             %% this sync should be canceled
             {reply, {error, sync_stopped}, State};
         Sync when Sync#sync_peer.from >= HeightIn ->
             {reply, {fetch, Sync#sync_peer.from, Sync#sync_peer.to, Sync#sync_peer.hash}, State};
         Sync when Sync#sync_peer.to < HeightIn ->
             {reply, {error, sync_stopped}, 
              State#state{sync_pool = lists:keydelete(Uri, #sync_peer.uri, State#state.sync_pool)}};
         #sync_peer{to = To} = Sync ->
             {reply, {fetch, HeightIn, To, HashIn}, 
              State#state{sync_pool = 
                            lists:keyreplace(Uri, #sync_peer.uri, 
                                             State#state.sync_pool, 
                                             Sync#sync_peer{from = HeightIn, hash = HashIn})}}
    end;
handle_call(_, _From, State) ->
    {reply, error, State}.

handle_cast({connect, Uri}, State) ->
    aec_peers:add(Uri, _Connect = true),
    {noreply, State};
handle_cast({start_sync, Uri, RemoteHash, _RemoteDifficulty}, State) ->
    %% We could decide not to sync if we are already syncing, but that
    %% opens up for an attack in which someone fakes to have higher difficulty
    jobs:enqueue(sync_jobs, {start_sync, Uri, RemoteHash}),
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
handle_cast({delete_from_pool, Uri}, State) ->
    %% This Uri misbehaved, even if we got a new Ping inbetween asking for deletion and
    %% actual deletion, we remove it. A new ping will arrive in the future.
    {noreply, ok, State#state{sync_pool = lists:keydelete(Uri, #sync_peer.uri, State#state.sync_pool)}};
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

insert(#sync_peer{difficulty = Difficulty,
                  from = From,
                  to = To,
                  uri = Uri} = Sync, Pool) ->
  {NewUri, NewPool} = 
      case lists:keyfind(Sync#sync_peer.uri, #sync_peer.uri, Pool) of
          false ->
             {true, [Sync | Pool]};
          RSync when RSync#sync_peer.from > From ->
             {false, lists:keyreplace(Uri, #sync_peer.uri, Pool, 
                                      RSync#sync_peer{difficulty = 
                                                        max(Difficulty, RSync#sync_peer.difficulty), 
                                                      to = max(To, RSync#sync_peer.to)})};
          RSync ->
             {false, lists:keyreplace(Uri, #sync_peer.uri, Pool, 
                                      Sync#sync_peer{difficulty = max(Difficulty, RSync#sync_peer.difficulty), 
                                                     to = max(To, RSync#sync_peer.to)})}
      end,
  {NewUri, lists:keysort(#sync_peer.difficulty, NewPool)}.

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
        {start_sync, Uri, RemoteHash} ->
            do_start_sync(Uri, RemoteHash);
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

do_start_sync(Uri, RemoteHash) ->
    aec_events:publish(chain_sync, {client_start, Uri}),
    case aeu_requests:get_header_by_hash(Uri, RemoteHash) of
        {ok, Hdr} ->
            lager:debug("New header received (~p): ~p", [Uri, pp(Hdr)]),
            %% We assume that the top of the chain is a moving target
            %% Get a hash first and work with that.
            LocalHeader = aec_chain:top_header(),
            LocalHeight = aec_headers:height(LocalHeader),
            RemoteHeight = aec_headers:height(Hdr),
            AgreedHeight = 
               case LocalHeight == 0 of
                    true -> 0;
                    false ->
                      agree_on_height(Uri, Hdr, RemoteHeight, LocalHeader, LocalHeight, LocalHeight, 0)
               end,
            %% The prev_hash of next block should be hash_header of block it builds upon
            lager:debug("Agreed upon height (~p): ~p", [Uri, AgreedHeight]),
            {ok, LocalAtHeight} = aec_chain:get_header_by_height(AgreedHeight),
            {ok, AgreedHash} = aec_headers:hash_header(LocalAtHeight), 
            case new_header(Uri, Hdr, AgreedHeight, AgreedHash) of
                false ->
                    lager:debug("Already syncing with peer ~p", [Uri]),
                    %% Already a sync in progress with this peer
                    ok;
                true ->
                    fetch_more(Uri, AgreedHeight, AgreedHash)
            end;
        {error, Reason} ->
            lager:debug("fetching top block (~p) failed: ~p", [Uri, Reason])
    end.

%% Ping logic makes sure they always agree on genesis header (height 0)
agree_on_height(Uri, RHeader, RH, LHeader, LH, Max, Min) when RH == LH ->
    case RHeader == LHeader of
        true ->
             %% We agree on a block
             Middle = (Max + LH) div 2,
             case Min < Middle andalso Middle < Max of
               true ->
                   {ok, LocalAtHeight} = aec_chain:get_header_by_height(Middle),
                   agree_on_height(Uri, RHeader, RH, LocalAtHeight, Middle, Max, LH);
               false ->
                   LH
             end;
        false -> 
             %% We disagree. Local on a fork compared to remote. Check half-way
             Middle = (Min + LH) div 2,
             case Min < Middle andalso Middle < Max of
                 true ->
                     {ok, LocalAtHeight} = aec_chain:get_header_by_height(Middle),
                     agree_on_height(Uri, RHeader, RH, LocalAtHeight, Middle, LH, Min);
                false ->
                    Min
             end
    end;
agree_on_height(Uri, _, RH, LHeader, LH, Max, Min) when RH =/= LH -> 
    case aeu_requests:get_header_by_height(Uri, LH) of
         {ok, RemoteAtHeight} ->
             lager:debug("New header received (~p): ~p", [Uri, pp(RemoteAtHeight)]),         
             agree_on_height(Uri, RemoteAtHeight, LH, LHeader, LH, Max, Min);
         {error, Reason} ->
             lager:debug("Fetching header ~p from ~p failed: ~p", [LH, Uri, Reason]),
             Min
    end.   

do_server_get_missing(Uri) ->
    aec_events:publish(chain_sync, {server_start, Uri}),
    do_get_missing_blocks(Uri),
    aec_events:publish(chain_sync, {server_done, Uri}).


fetch_chain(Uri, FromHeight, ToHeight, _) when FromHeight == ToHeight ->
    aec_events:publish(chain_sync, {client_done, Uri});
fetch_chain(Uri, FromH, ToH, HashFromH) when FromH < ToH ->
    case aeu_requests:get_header_by_height(Uri, FromH + 1) of
        {ok, Header} ->
          lager:debug("Header fetched (~p): ~p", [Uri, pp(Header)]),
          case aec_headers:prev_hash(Header) == HashFromH of
            true ->
              {ok, HeaderHash} = aec_headers:hash_header(Header),
              fetch_chain_from_header(Uri, FromH + 1, HeaderHash); 
            false ->
              lager:info("Abort sync due to non-fitting header ~p =/= ", [Header]),
              delete_from_pool(Uri),
              {error, sync_abort}
          end;
      {error, Reason} ->
          delete_from_pool(Uri),
          {error, Reason}
    end.

fetch_chain_from_header(Uri, Height, HeaderHash) ->
  case do_fetch_block(HeaderHash, Uri) of
    {ok, true, Block} ->
      lager:debug("Calling post_block(~p) --> hash of header ~p", [pp(Block), HeaderHash]),
      case aec_conductor:add_synced_block(Block) of
        ok ->
          fetch_more(Uri, Height, HeaderHash);
        {error, _} = Error ->
          delete_from_pool(Uri),
          Error
      end;
    {ok, false, _} ->
      %% More than one sync in progress, the block is already there
      fetch_more(Uri, Height, HeaderHash);
    {error, _} = Error ->
      lager:info("Abort sync due to non-fitting block ~p =/= ", [HeaderHash]),
      delete_from_pool(Uri),
      Error
  end.

fetch_more(Uri, LastHeight, HeaderHash) ->
  %% We need to supply the Hash, because locally we might have a shorter, 
  %% but locally more difficult fork
  case fetch_next(Uri, LastHeight, HeaderHash) of
    {fetch, NewFrom, NewTo, Hash} -> 
      fetch_chain(Uri, NewFrom, NewTo, Hash);
    Error ->
      lager:info("Abort sync at height ~p Error ~p ", [LastHeight, Error]),
      delete_from_pool(Uri),
      Error
  end.

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

