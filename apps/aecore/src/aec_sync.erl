%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module storing peers list and providing funtionc for peers interaction
%%% @end
%%%=============================================================================
-module(aec_sync).

-behaviour(gen_server).
-compile({parse_transform, lager_transform}).
-include("peers.hrl").

%% API
-export([connect_peer/1,
         received_block/1,
         block_created/1
        ]).

-export([subset_size/0,
         set_subset_size/1]).

-export([local_ping_object/0,
         compare_ping_objects/2]).

-export([subscribe/1,
         unsubscribe/1]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
	 handle_info/2,  terminate/2, code_change/3]).

%% Callback for jobs producer queue
-export([sync_worker/0]).

-define(RECV_POOL, aec_sync_recv_pool).

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

-spec received_block(aec_blocks:block()) -> ok.
received_block(Block) ->
    gen_server:cast(?MODULE, {received_block, Block}).

block_created(Block) ->
    gen_server:cast(?MODULE, {block_created, Block}).

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
    GHdr = aec_block_genesis:genesis_header(),
    {ok, GHash} = aec_headers:hash_header(GHdr),
    {ok, TopHdr} = aec_chain:top_header(),
    {ok, TopHash} = aec_headers:hash_header(TopHdr),
    Source = source_uri(),
    {ok, {Difficulty, _}} = aec_chain:get_total_difficulty(),
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
    lager:debug("Compare, Local: ~p~nRemote: ~p", [Local, Remote]),
    case {maps:get(<<"genesis_hash">>, Local),
          maps:get(<<"genesis_hash">>, Remote)} of
        {G, G} ->
            lager:debug("genesis blocks match", []),
            %% same genesis block - continue
            case {maps:get(<<"best_hash">>, Local),
                  maps:get(<<"best_hash">>, Remote)} of
                {T, T} ->
                    lager:debug("same top blocks", []),
                    %% we're in sync!
                    ok;
                _ ->
                    Dl = maps:get(<<"difficulty">>, Local),
                    Dr = maps:get(<<"difficulty">>, Remote),
                    if Dl > Dr ->
                            lager:debug("Our difficulty is higher", []),
                            ok;
                       true ->
                            Src = maps:get(<<"source">>, Remote),
                            start_sync(Src)
                    end
            end,
            ok;
        _ ->
            {error, different_genesis_blocks}
    end.

start_sync(PeerUri) ->
    gen_server:cast(?MODULE, {start_sync, PeerUri}).

-spec subscribe(block_created | block_received) -> ok.
subscribe(Event) when Event == block_created; Event == block_received ->
    gproc_ps:subscribe(l, Event),
    ok.

-spec unsubscribe(block_created | block_received) -> ok.
unsubscribe(Event) ->
    gproc_ps:unsubscribe(l, Event),
    ok.

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
    ensure_tab(?RECV_POOL, [set, public, named_table]),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} ->
            ets:give_away(?RECV_POOL, Pid, []),
            {ok, Pid};
        Other ->
            Other
    end.

init([]) ->
    Peers = application:get_env(aecore, peers, []),
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
handle_cast({received_block, Block}, State) ->
    HdrHash = header_hash(Block),
    case aec_chain:get_block_by_hash(HdrHash) of
        {ok, _} ->
            %% we have the block; do not re-send
            {noreply, State};
        {error, _} ->
            cache_and_queue_block(HdrHash, Block, received, forward, State),
            {noreply, State}
    end;
handle_cast({block_created, Block}, State) ->
    HdrHash = header_hash(Block),
    cache_and_queue_block(HdrHash, Block, created, forward, State),
    publish(block_created, aec_blocks:height(Block)),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

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
    lager:debug("dequed job ~p", [Res]),
    process_job(Res).

%% Note: we always dequeue exactly ONE job
process_job([{T, Job}]) ->
    reg_worker(Job, T),
    case Job of
        {forward, HdrHash, Peer} ->
            do_forward(HdrHash, Peer);
        {{remove, _Status}, HdrHash} ->
            do_remove_received(HdrHash, T);
        {start_sync, PeerUri} ->
            do_start_sync(PeerUri);
        _Other ->
            lager:debug("unknown job", [])
    end.

cache_and_queue_block(HdrHash, Block, Status, Op, State) ->
    case Peers = aec_peers:get_random(State#state.subset_size) of
        [] ->
            ok;
        [_|_] ->
            ets:insert(?RECV_POOL, {HdrHash, #{block => Block,
                                               status => Status}}),
            [enqueue_recv({Op, HdrHash, Peer})
             || Peer <- Peers],
            jobs:enqueue(sync_jobs, {{remove, Status}, HdrHash})
    end.

enqueue_recv(Job) ->
    jobs:enqueue(sync_jobs, Job).

do_forward(HdrHash, Peer) ->
    case ets:lookup(?RECV_POOL, HdrHash) of
        [{_, #{block := Block}}] ->
            do_forward_block(Block, Peer);
        [] ->
            case aec_chain:get_block_by_hash(HdrHash) of
                {ok, Block} ->
                    do_forward_block(Block, Peer);
                {error, _} ->
                    ok
            end
    end.

do_forward_block(Block, Peer) ->
    Res = aeu_requests:send_block(Peer, Block),
    lager:debug("send_block Res (~p): ~p", [Peer, Res]).

%% A potential race is that another worker has dequeued the previous
%% job, it pertains to this block, but the worker was scheduled out
%% before having a chance to retrieve the block from the 'cache'.
%% In this case, the last worker will try to fetch the block from the
%% chain instead. If the block hasn't yet been added to the chain, the
%% worker is out of luck, and the block doesn't get forwarded.
%% For now, sleep a bit before deleting. TODO: come up with a way to
%% eliminate the race.
do_remove_received(HdrHash, T) ->
    Prods = jobs:queue_info(sync_workers, producers) -- [self()],
    %% each producer should register a property once
    %% it knows what to do.
    await_workers(HdrHash, T, [{P, process_info(P, reductions)}
                               || P <- Prods]),
    lager:debug("await_workers done, deleting ~p", [HdrHash]),
    ets:delete(?RECV_POOL, HdrHash).

await_workers(_, _, []) ->
    ok;
await_workers(Hash, T0, Prods) ->
    Regs = gproc:select({l, p}, [{ {worker_prop('$1'), '$2', '$3'},
                                   [],
                                   [{{'$2', '$1', '$3'}}] }]),
    lager:debug("Await, Regs = ~p", [Regs]),
    case lists:foldr(
           fun(Prod, Acc) ->
                   filter_producer(Prod, Regs, T0, Hash, Acc)
           end, [], Prods) of
        [] ->
            %% all Prods are either dead, working on newer
            %% things or a different hash
            ok;
        [_|_] = PrunedProds ->
            %% These might be idle, or haven't yet had time to read
            %% the hash, or are still working on the hash. Wait a while.
            timer:sleep(1000),
            await_workers(Hash, T0, prune_idle_and_dead(PrunedProds, T0, Hash))
    end.

filter_producer({P,_,Hash} = Prod, _, _, Hash, Acc) ->
    case is_process_alive(P) of
        true  -> [Prod|Acc];
        false -> Acc
    end;
filter_producer({P, R} = Prod, Regs, T0, Hash, Acc) ->
    case lists:keyfind(P, 1, Regs) of
        {_, _, T} when T > T0 ->
            %% These have newer jobs - not related
            Acc;
        {_, J, _} when element(2, J) =:= Hash ->
            %% this is related - allow to finish
            [{P, R, Hash}|Acc];
        _ ->
            case is_process_alive(P) of
                true  -> [Prod|Acc];
                false -> Acc
            end
    end.

prune_idle_and_dead(Prods, T0, Hash) ->
    lists:foldr(
      fun({P,_} = Prod, Acc) ->
              {gproc, Regs} = gproc:info(P, gproc),
              inspect_regs(Regs, T0, Hash, Prod, Acc);
         ({P,_,H} = Prod, Acc) when H =:= Hash ->
              case is_process_alive(P) of
                  true  -> [Prod|Acc];
                  false -> Acc
              end
      end, [], Prods).

inspect_regs([], _, _, {P, R} = Prod, Acc) ->
    case process_info(P, reductions) of
        R         -> Acc;
        undefined -> Acc;
        _ ->
            [Prod|Acc]
    end;
inspect_regs(Regs, T0, Hash, {P,R} = Prod, Acc) ->
    case [{J1, T1} || {{?MODULE, worker, J1}, T1} <- Regs] of
        [{_,T}] when T > T0 ->
            %% newer job - ignore
            Acc;
        [{Job,_}] when element(2, Job) =:= Hash ->
            [{P, R, Hash}|Acc];
        [] ->
            [Prod|Acc]
    end.

do_start_sync(PeerUri) ->
    fetch_headers(PeerUri, genesis_hash(), []).

genesis_hash() ->
    GHdr = aec_block_genesis:genesis_header(),
    {ok, GHash} = aec_headers:hash_header(GHdr),
    GHash.

fetch_headers(PeerUri, GHash, Acc) ->
    case aeu_requests:top(PeerUri) of
        {ok, Hdr} ->
            lager:debug("Top hdr (~p): ~p", [PeerUri, Hdr]),
            {ok, HdrHash} = aec_headers:hash_header(Hdr),
            case HdrHash of
                GHash ->
                    %% already the genesis hash
                    ok;
                _ ->
                    fetch_headers_1(HdrHash, Hdr, PeerUri, GHash, Acc)
            end;
        {error, Reason} ->
            lager:debug("fetching top block (~p) failed: ~p", [PeerUri, Reason])
    end.

fetch_headers_1(HdrHash, Hdr, PeerUri, GHash, Acc) ->
    case aec_chain:get_header_by_hash(HdrHash) of
        {ok, _} ->
            case aec_chain:get_block_by_hash(HdrHash) of
                {ok, _} ->
                    lager:debug("we have the top block - done", []),
                    %% We're done already!
                    ok;
                {error, _} ->
                    lager:debug("fetching top block (~p)", [HdrHash]),
                    Acc1 = fetch_block(HdrHash, PeerUri, Acc),
                    fetch_next(aec_headers:prev_hash(Hdr),
                               PeerUri, GHash, Acc1)
            end;
        {error, _} ->
            lager:debug("we don't have the top block header,"
                        " fetching block (~p)", [HdrHash]),
            Acc1 = fetch_block(HdrHash, PeerUri, Acc),
            fetch_next(aec_headers:prev_hash(Hdr), PeerUri, GHash, Acc1)
    end.


fetch_block(Hash, PeerUri, Acc) ->
    case aeu_requests:block(PeerUri, Hash) of
        {ok, Block} ->
            lager:debug("block fetched from ~p (~p)", [PeerUri, Hash]),
            [{Hash, Block}|Acc];
        {error, _} = Error ->
            lager:debug("failed to fetch block from ~p,~nHash = ~p~nError = ~p",
                        [PeerUri, Hash, Error]),
            %% What is the right course of action here? Continue? Abort?
            Acc
    end.

fetch_next(GHash, _, GHash, Acc) ->
    %% No need to fetch the genesis block
    try_write_blocks(Acc);
fetch_next(Hash, PeerUri, GHash, Acc) ->
    case aeu_requests:block(PeerUri, Hash) of
        {ok, Block} ->
            case aec_blocks:height(Block) of
                0 ->
                    lager:debug("fetched the genesis block", []),
                    try_write_blocks(Acc);
                _ ->
                    Hdr = aec_blocks:to_header(Block),
                    {ok, HdrHash} = aec_headers:hash_header(Hdr),
                    case aec_chain:get_block_by_hash(HdrHash) of
                        {ok, _} ->
                            lager:debug(
                              "we have this block, start writing (~p)",
                              [HdrHash]),
                            %% we're done
                            try_write_blocks(Acc);
                        {error, _} ->
                            lager:debug("new block, continue (~p)", [HdrHash]),
                            fetch_next(
                              aec_headers:prev_hash(Hdr), PeerUri, GHash,
                              [{HdrHash, Block}|Acc])
                    end
            end;
        {error, _} ->
            %% Unexpected. Try to write the blocks we have
            try_write_blocks(Acc)
    end.

try_write_blocks([{HdrHash, Block}|Blocks]) ->
    %% block list has oldest block first
    Hdr = aec_blocks:to_header(Block),
    case aec_chain:insert_header(Hdr) of
        ok ->
            lager:debug("header insert successful (~p)", [HdrHash]),
            try_write_block(Block, Blocks);
        {error, _} ->
            case aec_chain:get_header_by_hash(HdrHash) of
                {ok, _} ->
                    try_write_block(Block, Blocks);
                {error, Reason} = Error ->
                    lager:debug(
                      "insert_chain(~p) error: ~p", [HdrHash, Reason]),
                    Error
            end
    end;
try_write_blocks([]) ->
    lager:debug("no more blocks to write", []),
    ok.

try_write_block(Block, Blocks) ->
    case aec_chain:write_block(Block) of
        ok ->
            lager:debug("block write successful (~p)", [header_hash(Block)]),
            try_write_blocks(Blocks);
        {error, {block_already_stored, _}} ->
            %% this is fine, continue
            lager:debug("block already stored (~p)", [header_hash(Block)]),
            try_write_blocks(Blocks);
        {error, Reason} = Error ->
            lager:debug("write_block error (~p): ~p",
                        [header_hash(Block), Reason]),
            %% No reason to continue? We now have a top header without a
            %% corresponding block.
            Error
    end.

reg_worker(Job, T) ->
    gproc:reg(worker_prop(Job), T).

worker_prop(Job) ->
    {p, l, {?MODULE, worker, Job}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

ensure_tab(T, Opts) ->
    case ets:info(T, name) of
        undefined ->
            ets:new(T, [{heir, self(), []}|Opts]);
        _ ->
            true
    end.

header_hash(Block) ->
    Header = aec_blocks:to_header(Block),
    {ok, HeaderHash} = aec_headers:hash_header(Header),
    HeaderHash.

source_uri() ->
    list_to_binary(aec_peers:get_local_peer_uri()).

publish(Event, Msg) ->
    gproc_ps:publish(l, Event, Msg).
