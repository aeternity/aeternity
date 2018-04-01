%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module storing peers list and providing funtions for peer interaction
%%% @end
%%%=============================================================================
-module(aec_sync).

-behaviour(gen_server).

-import(aeu_debug, [pp/1]).
-import(aec_peers, [ppp/1]).

%% API
-export([connect_peer/1,
         start_link/0
        ]).

%% API called from strongly connected component aec_peers
-export([schedule_ping/1]).

%% API called from both aehttp_dispatch_ext and aeu_requests
-export([local_ping_object/0]).

-export([server_get_missing_blocks/1, start_sync/3, fetch_mempool/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2,  terminate/2, code_change/3]).

%% Callback for jobs producer queue
-export([sync_worker/0]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-define(MAX_HEADERS_PER_CHUNK, 100).
-define(MAX_DIFF_FOR_SYNC, 50).

-record(sync_peer, {difficulty, from, to, hash, peer, pid}).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec connect_peer(aec_peers:peer_info()) -> ok.
connect_peer(PeerInfo) ->
    gen_server:cast(?MODULE, {connect, PeerInfo}).

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
    {ok, Difficulty} = aec_chain:difficulty_at_top_block(),
    #{<<"genesis_hash">> => GHash,
      <<"best_hash">>    => TopHash,
      <<"difficulty">>   => Difficulty,
      <<"share">>        => 32,  % TODO: make this configurable
      <<"peers">>        => []}.

start_sync(PeerId, RemoteHash, RemoteDifficulty) ->
    gen_server:cast(?MODULE, {start_sync, PeerId, RemoteHash, RemoteDifficulty}).

server_get_missing_blocks(PeerId) ->
    gen_server:cast(?MODULE, {server_get_missing, PeerId}).

fetch_mempool(PeerId) ->
    gen_server:cast(?MODULE, {fetch_mempool, PeerId}).

schedule_ping(PeerId) ->
    gen_server:cast(?MODULE, {schedule_ping, PeerId}).

fetch_next(PeerId, HeightIn, HashIn, Result) ->
    gen_server:call(?MODULE, {fetch_next, PeerId, HeightIn, HashIn, Result}, 30000).

delete_from_pool(PeerId) ->
    gen_server:cast(?MODULE, {delete_from_pool, PeerId}).

%% Check whether this header is better than previous best
new_header(PeerId, Header, AgreedHeight, Hash) ->
    gen_server:call(?MODULE, {new_header, self(), PeerId, Header, AgreedHeight, Hash}).

update_hash_pool(Hashes) ->
  gen_server:call(?MODULE, {update_hash_pool, Hashes}).

sync_in_progress(PeerId) ->
   gen_server:call(?MODULE, {sync_in_progress, PeerId}).

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
%%    We binary search for a block that we agree upon (could be genesis)
%% We add new node to sync pool to sync agreed block upto top of new
%% 1. If we are already synchronizing, we ignore it,
%%    the ongoing sync will pick that up later
%% 2. If we are not already synchronizing, we start doing so.
%%
%% We sync with several nodes at the same time and use as strategy
%% to pick a random hash from the hashes in the pool.

-record(state, {sync_pool = [], hash_pool = []}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    aec_events:subscribe(block_created),
    aec_events:subscribe(top_changed),
    aec_events:subscribe(tx_created),

    Peers = parse_peers(aeu_env:user_map_or_env(<<"peers">>, aecore, peers, [])),
    BlockedPeers = parse_peers(aeu_env:user_map_or_env(<<"blocked_peers">>, aecore, blocked_peers, [])),

    [aec_peers:block_peer(aec_peers:peer_id(P)) || P <- BlockedPeers],
    aec_peers:add_and_ping_peers(Peers, true),
    {ok, #state{}}.

handle_call({new_header, Pid, PeerId, Header, AgreedHeight, AgreedHash}, _, State) ->
    Height = aec_headers:height(Header),
    Difficulty = aec_headers:difficulty(Header),
    %% We have talked to the PeerId, so PeerId can be trusted
    {IsNew, NewPool} = insert(#sync_peer{difficulty = Difficulty,
                                         from = AgreedHeight,
                                         to = Height,
                                         hash = AgreedHash,
                                         pid = Pid,
                                         peer = PeerId},
                              State#state.sync_pool),
    case IsNew of
        true -> erlang:monitor(process, Pid);
        false -> ok
    end,
    {reply, IsNew, State#state{sync_pool = NewPool}};
handle_call({update_hash_pool, Hashes}, _From, State) ->
    HashPool = merge(State#state.hash_pool, Hashes),
    lager:debug("Hash pool now contains ~p hashes", [length(HashPool)]),
    {reply, ok, State#state{hash_pool = HashPool}};
handle_call({fetch_next, PeerId, HeightIn, HashIn, Result}, _, State) ->
    HashPool =
       case Result of
         {ok, Block} ->
           BlockH = aec_blocks:height(Block),
           BlockHash = header_hash(Block),
           %% If the hash of this block does not fit wanted hash, it is discarded
           %% (In case we ask for block with hash X and we get a block with hash Y)
           lists:keyreplace({BlockH, BlockHash}, 1,
                            State#state.hash_pool, {{BlockH, BlockHash}, #{block => Block}});
         _ ->
           State#state.hash_pool
       end,
    lager:debug("fetch next from Hashpool ~p", [ [ {H, maps:is_key(block, Map)} || {{H,_}, Map} <- HashPool] ]),
    case update_chain_from_pool(HeightIn, HashIn, HashPool) of
        {error, Reason} ->
            lager:error("chain update failed ~p", [Reason]),
            {reply, {error, sync_stopped}, State#state{hash_pool = HashPool}};
        {ok, NewHeight, NewHash, []} ->
            lager:debug("Got all the blocks in hash pool"),
            %% we might be done, check for more:
            case lists:keyfind(PeerId, #sync_peer.peer, State#state.sync_pool) of
                false ->
                    %% this sync should be canceled
                    {reply, {error, sync_stopped}, State#state{hash_pool = []}};
                #sync_peer{to = To} when To =< NewHeight ->
                   NewSyncPool = lists:keydelete(PeerId, #sync_peer.peer, State#state.sync_pool),
                   {reply, done, State#state{hash_pool = [], sync_pool = NewSyncPool}};
                #sync_peer{} ->
                   {reply, {fill_pool, NewHeight, NewHash}, State#state{hash_pool = []}}
            end;
        {ok, NewHeight, NewHash, NewHashPool} ->
            lager:debug("Updated Hashpool ~p", [ [ {H, maps:is_key(block, Map)} || {{H,_}, Map} <- NewHashPool] ]),
            case [ {H, Hash} || {{H, Hash}, Map} <- NewHashPool, not maps:is_key(block, Map) ] of
                [] ->
                    %% We have all blocks, just insertion left
                    lager:info("Got all blocks insertion to be done from height ~p", [NewHeight]),
                    {reply, {insert, NewHeight, NewHash}, State#state{hash_pool = NewHashPool}};
                PickAHash ->
                    {PickH, PickHash} = lists:nth(rand:uniform(length(PickAHash)), PickAHash),
                    lager:debug("Get block at height ~p", [PickH]),
                    {reply, {fetch, NewHeight, NewHash, PickHash}, State#state{hash_pool = NewHashPool}}
            end
    end;
handle_call({sync_in_progress, PeerId}, _, State) ->
    {reply, lists:keyfind(PeerId, #sync_peer.peer, State#state.sync_pool), State};
handle_call(_, _From, State) ->
    {reply, error, State}.

handle_cast({connect, PeerInfo}, State) ->
    aec_peers:add(PeerInfo#{ ping => true }),
    {noreply, State};
handle_cast({start_sync, PeerId, RemoteHash, _RemoteDifficulty}, State) ->
    %% We could decide not to sync if we are already syncing, but that
    %% opens up for an attack in which someone fakes to have higher difficulty
    jobs:enqueue(sync_jobs, {start_sync, PeerId, RemoteHash}),
    {noreply, State};
handle_cast({server_get_missing, PeerId}, State) ->
    jobs:enqueue(sync_jobs, {server_get_missing, PeerId}),
    {noreply, State};
handle_cast({fetch_mempool, PeerId}, State) ->
    jobs:enqueue(sync_jobs, {fetch_mempool, PeerId}),
    {noreply, State};
handle_cast({schedule_ping, PeerId}, State) ->
    jobs:enqueue(sync_jobs, {ping, PeerId}),
    {noreply, State};
handle_cast({delete_from_pool, PeerId}, State) ->
    %% This PeerId misbehaved, even if we got a new Ping in-between asking for deletion and
    %% actual deletion, we remove it. A new ping will arrive in the future.
    %% If the reference was kept, demonitor could be used here... but just ignore DOWN for these
    {noreply, State#state{sync_pool = lists:keydelete(PeerId, #sync_peer.peer, State#state.sync_pool)}};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, Event, #{info := Info}}, State) ->
    case Event of
        block_created -> enqueue(forward, #{status => created,
                                            block => Info});
        top_changed   -> enqueue(forward, #{status => top_changed,
                                            block => Info});
        tx_created    -> enqueue(forward, #{status => created,
                                            tx => Info});
        tx_received   -> enqueue(forward, #{status => received,
                                            tx => Info});
        _             -> ignore
    end,
    {noreply, State};
handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    %% It might be one of our syncing workers that crashed
    if Reason =/= normal -> lager:info("worker stopped with reason: ~p", [Reason]);
       true -> ok
    end,
    {noreply, State#state{sync_pool = lists:keydelete(Pid, #sync_peer.pid, State#state.sync_pool)}};
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.

insert(#sync_peer{difficulty = Difficulty,
                  from = From,
                  to = To,
                  peer = PeerId} = NewSync, Pool) ->
    {NewPeerId, NewPool} =
        case lists:keyfind(PeerId, #sync_peer.peer, Pool) of
            false ->
                {true, [NewSync | Pool]};
            OldSync ->
                NewSync1 = case OldSync#sync_peer.from > From of
                              true  -> OldSync;
                              false -> NewSync
                           end,
                MaxDiff = max(Difficulty, OldSync#sync_peer.difficulty),
                MaxTo   = max(To, OldSync#sync_peer.to),
                NewSync2 = NewSync1#sync_peer{difficulty = MaxDiff, to = MaxTo},
                {false, lists:keyreplace(PeerId, #sync_peer.peer, Pool, NewSync2)}
        end,
    {NewPeerId, lists:keysort(#sync_peer.difficulty, NewPool)}.

%% both lists are sorted on block height, same height may occur more than once
%% with different hash
merge([], NewHashes) ->
  NewHashes;
merge(OldHashes, []) ->
  OldHashes;
merge([{{H1, _Hash1}, _} | OldHashes], [{{H2, Hash2}, Map2} | NewHashes]) when H1 < H2 ->
  %% This is unlikely to happen, it would mean that we agree upon a height that we still
  %% try to obtain on the chain. Drop it.
  merge(OldHashes, [{{H2, Hash2}, Map2} | NewHashes]);
merge([{{H1, Hash1}, Map1} | OldHashes], [{{H2, _Hash2}, _} | NewHashes]) when H1 > H2 ->
  %% drop Hash2, we have already passed that height.
  %% In the very unlikely event that we are on the wrong chain, we will ping with
  %% heigher difficulty when this fork has been read.
  merge([{{H1, Hash1}, Map1} | OldHashes], NewHashes);
merge(OldHashes, [{{H2, Hash2}, Map2} | NewHashes]) ->
  %% We are on same height, but pool may have different hashes on that height
  pick_same({{H2, Hash2}, Map2}, OldHashes, NewHashes).


pick_same({{H, Hash2}, Map2}, [{{H, Hash1}, Map1} | OldHashes], NewHashes) ->
  case Hash1 == Hash2 of
    true ->
      %% Same branch of the chain (possibly overwrite uri with newer one)
      [{{H, Hash1}, maps:merge(Map1, Map2)} | pick_same({{H, Hash2}, Map2}, OldHashes, NewHashes)];
    false ->
      [{{H, Hash1}, Map1} | pick_same({{H, Hash2}, Map2}, OldHashes, NewHashes)]
  end;
pick_same(_, OldHashes, NewHashes) ->
  merge(OldHashes, NewHashes).


%% TODO: This design is sub-par... We should split the concept of adding blocks to the chain from
%% the fetching logic loop. For now don't add more that 20 blocks in one step here as a temporary
%% workaround, since adding blocks is done on the gen_server-loop.
-define(MAX_ADDS, 20).
update_chain_from_pool(AgreedHeight, AgreedHash, HashPool) ->
    lager:debug("splitting at height ~p with prev hash ~p", [AgreedHeight + 1 , AgreedHash]),
    case split_hash_pool(AgreedHeight + 1, AgreedHash, HashPool, [], 0) of
      {_, _, [], Rest, NAdded} when Rest =/= [] andalso NAdded < ?MAX_ADDS ->
         lager:error("Cannot split hash pool ~p at ~p: ~p", [Rest, AgreedHeight, AgreedHash]),
         %% This is weird, we cannot get our next block
        {error, {stuck_at, AgreedHeight + 1}};
      {NewAgreedHeight, NewAgreedHash, Same, Rest, _} ->
        {ok, NewAgreedHeight, NewAgreedHash, Same ++ Rest}
    end.

split_hash_pool(Height, PrevHash, [{{H, _},_} | HashPool], Same, NAdded) when H < Height ->
    split_hash_pool(Height, PrevHash, HashPool, Same, NAdded);
split_hash_pool(Height, PrevHash, [{{H, Hash}, Map} = Item | HashPool], Same, NAdded) when H == Height, NAdded < ?MAX_ADDS ->
    case maps:get(block, Map, undefined) of
        undefined ->
            split_hash_pool(Height, PrevHash, HashPool, [Item | Same], NAdded);
        Block ->
            %% We are guaranteed that hash and height of block are correct
            Hash = header_hash(Block),
            case aec_blocks:prev_hash(Block) of
              PrevHash ->
                  lager:debug("Calling post_block(~p) --> hash of header ~p", [pp(Block), Hash]),
                  case aec_conductor:add_synced_block(Block) of
                    ok ->
                      split_hash_pool(H + 1, Hash, HashPool, [], NAdded + 1);
                    {error, _} = Error ->
                      lager:error("Could not insert ~p: ~p", [Block, Error]),
                      split_hash_pool(Height, PrevHash, HashPool, Same, NAdded)
                  end;
              _ ->
                split_hash_pool(Height, PrevHash, HashPool, [Item | Same], NAdded)
            end
    end;
split_hash_pool(Height, PrevHash, HashPool, Same, NAdded) ->
    {Height - 1, PrevHash, Same, HashPool, NAdded}.

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
        {forward, #{block := Block}, PeerId} ->
            do_forward_block(Block, PeerId);
        {forward, #{tx := Tx}, PeerId} ->
            do_forward_tx(Tx, PeerId);
        {start_sync, PeerId, RemoteHash} ->
            case sync_in_progress(PeerId) of
              false -> do_start_sync(PeerId, RemoteHash);
              _     -> lager:info("sync already in progress ~p", [ppp(PeerId)])
            end;
        {server_get_missing, PeerId} ->
            do_server_get_missing(PeerId);
        {fetch_mempool, PeerId} ->
            do_fetch_mempool(PeerId);
        {ping, PeerId} ->
            ping_peer(PeerId);
        _Other ->
            lager:debug("unknown job", [])
    end.

enqueue(Op, Msg) ->
    Peers = aec_peers:get_random(all),
    [ jobs:enqueue(sync_jobs, {Op, Msg, aec_peers:peer_id(PeerInfo)})
      || PeerInfo <- Peers ].

ping_peer(PeerId) ->
    Res = aec_peer_connection:ping(PeerId),
    lager:debug("Ping (~p): ~p", [ppp(PeerId), Res]),
    case Res of
        ok ->
            aec_peers:log_ping(PeerId, ok);
        {error, _} ->
            aec_peers:log_ping(PeerId, error)
  end.

do_forward_block(Block, PeerId) ->
    %% If we sync against the PeerId and it has far more blocks, ignore sending this one
    H = aec_blocks:height(Block),
    case sync_in_progress(PeerId) of
        #sync_peer{to = To} when To > H + ?MAX_DIFF_FOR_SYNC ->
            lager:debug("not forwarding to (~p): too far ahead", [ppp(PeerId)]);
         _ ->
            Res = aec_peer_connection:send_block(PeerId, Block),
            lager:debug("send_block Res (~p): ~p", [ppp(PeerId), Res])
    end.

do_forward_tx(Tx, PeerId) ->
    Res = aec_peer_connection:send_tx(PeerId, Tx),
    lager:debug("send_tx (~p): ~p", [ppp(PeerId), Res]).

do_start_sync(PeerId, RemoteHash) ->
    aec_events:publish(chain_sync, {client_start, PeerId}),
    case aec_peer_connection:get_header_by_hash(PeerId, RemoteHash) of
        {ok, Hdr} ->
            lager:debug("New header received (~p): ~p", [ppp(PeerId), pp(Hdr)]),
            %% We assume that the top of the chain is a moving target
            %% Get a hash first and work with that.
            LocalHeader = aec_chain:top_header(),
            LocalHeight = aec_headers:height(LocalHeader),
            RemoteHeight = aec_headers:height(Hdr),
            AgreedHeight =
               case LocalHeight == 0 of
                    true -> 0;
                    false ->
                      agree_on_height(PeerId, Hdr, RemoteHeight,
                                      LocalHeader, LocalHeight, LocalHeight, 0)
               end,
            lager:debug("Agreed upon height (~p): ~p", [ppp(PeerId), AgreedHeight]),
            {ok, LocalAtHeight} = aec_chain:get_header_by_height(AgreedHeight),
            {ok, AgreedHash} = aec_headers:hash_header(LocalAtHeight),
            %% The prev_hash of next block should be AgreedHash
            case new_header(PeerId, Hdr, AgreedHeight, AgreedHash) of
                false ->
                    lager:debug("Already syncing with peer ~p", [ppp(PeerId)]),
                    %% Already a sync in progress with this peer
                    ok;
                true ->
                    PoolResult = fill_pool(PeerId, AgreedHash),
                    fetch_more(PeerId, AgreedHeight, AgreedHash, PoolResult)
            end;
        {error, Reason} ->
            lager:debug("fetching top block (~p) failed: ~p", [ppp(PeerId), Reason])
    end.

%% Ping logic makes sure they always agree on genesis header (height 0)
agree_on_height(PeerId, RHeader, RH, LHeader, LH, Max, Min) when RH == LH ->
    case RHeader == LHeader of
        true ->
             %% We agree on a block
             Middle = (Max + LH) div 2,
             case Min < Middle andalso Middle < Max of
               true ->
                   {ok, LocalAtHeight} = aec_chain:get_header_by_height(Middle),
                   agree_on_height(PeerId, RHeader, RH, LocalAtHeight, Middle, Max, LH);
               false ->
                   LH
             end;
        false ->
             %% We disagree. Local on a fork compared to remote. Check half-way
             Middle = (Min + LH) div 2,
             case Min < Middle andalso Middle < Max of
                 true ->
                     {ok, LocalAtHeight} = aec_chain:get_header_by_height(Middle),
                     agree_on_height(PeerId, RHeader, RH, LocalAtHeight, Middle, LH, Min);
                false ->
                    Min
             end
    end;
agree_on_height(PeerId, _, RH, LHeader, LH, Max, Min) when RH =/= LH ->
    case aec_peer_connection:get_header_by_height(PeerId, LH) of
         {ok, RemoteAtHeight} ->
             lager:debug("New header received (~p): ~p", [ppp(PeerId), pp(RemoteAtHeight)]),
             agree_on_height(PeerId, RemoteAtHeight, LH, LHeader, LH, Max, Min);
         {error, Reason} ->
             lager:debug("Fetching header ~p from ~p failed: ~p", [LH, ppp(PeerId), Reason]),
             Min
    end.

do_server_get_missing(PeerId) ->
    aec_events:publish(chain_sync, {server_start, PeerId}),
    do_get_missing_blocks(PeerId),
    aec_events:publish(chain_sync, {server_done, PeerId}).

fill_pool(PeerId, AgreedHash) ->
    case aec_peer_connection:get_n_successors(PeerId, AgreedHash, ?MAX_HEADERS_PER_CHUNK) of
        {ok, []} ->
            delete_from_pool(PeerId),
            aec_events:publish(chain_sync, {client_done, PeerId}),
            done;
        {ok, ChunkHashes} ->
            HashPool = [ {K, #{peer => PeerId}} || K <- ChunkHashes ],
            lager:debug("We need to get the following hashes: ~p", [HashPool]),
            update_hash_pool(HashPool),
            {filled_pool, length(ChunkHashes)-1};
        {error, _} = Error ->
            lager:info("Abort sync with ~p (~p) ", [ppp(PeerId), Error]),
            delete_from_pool(PeerId),
            {error, sync_abort}
    end.

fetch_more(PeerId, _, _, done) ->
    delete_from_pool(PeerId),
    aec_events:publish(chain_sync, {client_done, PeerId});
fetch_more(PeerId, LastHeight, _, {error, Error}) ->
    lager:info("Abort sync at height ~p Error ~p ", [LastHeight, Error]),
    delete_from_pool(PeerId);
fetch_more(PeerId, LastHeight, HeaderHash, Result) ->
    %% We need to supply the Hash, because locally we might have a shorter,
    %% but locally more difficult fork
    case fetch_next(PeerId, LastHeight, HeaderHash, Result) of
        {fetch, NewHeight, NewHash, Hash} ->
            case do_fetch_block(Hash, PeerId) of
                {ok, _, NewBlock} ->
                    fetch_more(PeerId, NewHeight, NewHash, {ok, NewBlock});
                {error, _} = Error ->
                    fetch_more(PeerId, NewHeight, NewHash, Error)
            end;
        {insert, NewHeight, NewHash} ->
            fetch_more(PeerId, NewHeight, NewHash, no_result);
        {fill_pool, AgreedHeight, AgreedHash} ->
            PoolResult = fill_pool(PeerId, AgreedHash),
            fetch_more(PeerId, AgreedHeight, AgreedHash, PoolResult);
        Other ->
            fetch_more(PeerId, LastHeight, HeaderHash, Other)
  end.

fetch_block(Hash, PeerId) ->
    case do_fetch_block_ext(Hash, PeerId) of
        {ok, _, Block} ->
            lager:debug("Block (~p) fetched from ~p", [pp(Hash), ppp(PeerId)]),
            [{Hash, Block}];
        {error, _} = Err ->
            lager:error("Error fetching block (~p) from ~p: ~p",
                        [pp(Hash), ppp(PeerId), Err]),
            []
    end.

do_fetch_block(Hash, PeerId) ->
    case aec_chain:get_block(Hash) of
        {ok, Block} ->
            lager:debug("block ~p already fetched, using local copy", [pp(Hash)]),
            {ok, false, Block};
        error ->
            do_fetch_block_ext(Hash, PeerId)
    end.

do_fetch_block_ext(Hash, PeerId) ->
    lager:debug("we don't have the block -fetching (~p)", [pp(Hash)]),
    case aec_peer_connection:get_block(PeerId, Hash) of
        {ok, Block} ->
            case header_hash(Block) =:= Hash of
                true ->
                    lager:debug("block fetched from ~p (~p); ~p",
                                [ppp(PeerId), pp(Hash), pp(Block)]),
                    {ok, true, Block};
                false ->
                    {error, hash_mismatch}
            end;
        {error, _} = Error ->
            lager:debug("failed to fetch block from ~p; Hash = ~p; Error = ~p",
                        [ppp(PeerId), pp(Hash), Error]),
            Error
    end.

do_get_missing_blocks(PeerId) ->
    Missing = aec_chain:get_missing_block_hashes(),
    lager:debug("Missing block hashes: ~p", [pp(Missing)]),
    fetch_missing_blocks(Missing, PeerId).

try_write_blocks(Blocks) ->
    lists:foreach(fun sync_post_block/1, Blocks).

sync_post_block({_Hash, Block}) ->
    %% No event publication for now (esp. not block_received!)
    lager:debug("Calling post_block(~p)", [pp(Block)]),
    aec_conductor:add_synced_block(Block).

fetch_missing_blocks(Hashes, PeerId) ->
    Blocks =
        lists:foldl(
          fun(Hash, Acc) ->
                  fetch_block(Hash, PeerId) ++ Acc
          end, [], Hashes),
    try_write_blocks(Blocks).

do_fetch_mempool(PeerId) ->
    case aec_peer_connection:get_mempool(PeerId) of
        {ok, Txs} ->
            lager:debug("Mempool (~p) received, size: ~p",
                        [ppp(PeerId), length(Txs)]),
            aec_tx_pool:push(Txs),
            aec_events:publish(mempool_sync, {fetched, PeerId});
        Other ->
            lager:debug("Error fetching mempool from ~p: ~p",
                        [ppp(PeerId), Other]),
            Other
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

header_hash(Block) ->
    Header = aec_blocks:to_header(Block),
    {ok, HeaderHash} = aec_headers:hash_header(Header),
    HeaderHash.

parse_peers(Ps) ->
    lists:append([ parse_peer(P) || P <- Ps ]).

parse_peer(P) ->
    case aec_peers:parse_peer_address(P) of
        {ok, PInfo} ->
            [PInfo];
        {error, _} ->
            []
    end.
