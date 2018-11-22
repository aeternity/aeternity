%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module storing peers list and providing funtions for peer interaction
%%% @end
%%%=============================================================================
-module(aec_sync).

-include("blocks.hrl").

-behaviour(gen_server).

-import(aeu_debug, [pp/1]).
-import(aec_peers, [ppp/1]).

%% API
-export([start_link/0]).

%% API called from strongly connected component aec_peers
-export([schedule_ping/1]).

-export([ start_sync/3
        , get_generation/2
        , has_generation/1
        , set_last_generation_in_sync/0 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-define(MAX_HEADERS_PER_CHUNK, 100).

%%%=============================================================================
%%% API
%%%=============================================================================

start_sync(PeerId, RemoteHash, RemoteDifficulty) ->
    gen_server:cast(?MODULE, {start_sync, PeerId, RemoteHash, RemoteDifficulty}).

get_generation(PeerId, Hash) ->
    gen_server:cast(?MODULE, {get_generation, PeerId, Hash}).

set_last_generation_in_sync() ->
    gen_server:cast(?MODULE, set_last_generation_in_sync).

schedule_ping(PeerId) ->
    gen_server:cast(?MODULE, {schedule_ping, PeerId}).

-ifdef(TEST).
%% Only used by test
worker_for_peer(PeerId) ->
    gen_server:call(?MODULE, {worker_for_peer, PeerId}).

gossip_txs(GossipTxs) ->
    gen_server:call(?MODULE, {gossip_txs, GossipTxs}).
-endif.

sync_in_progress(PeerId) ->
    gen_server:call(?MODULE, {sync_in_progress, PeerId}).

known_chain(Chain, ExtraInfo) ->
    gen_server:call(?MODULE, {known_chain, Chain, ExtraInfo}).

update_sync_task(Update, Task) ->
    gen_server:call(?MODULE, {update_sync_task, Update, Task}).

next_work_item(Task, PeerId, LastResult) ->
    gen_server:call(?MODULE, {next_work_item, Task, PeerId, LastResult}).

handle_worker(Task, Action) ->
    gen_server:cast(?MODULE, {handle_worker, Task, Action}).

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

-record(sync_task, {id, chain, pool = [], agreed,
                    adding = [], pending = [], workers = []}).
-record(state, { sync_tasks = []                 :: [#sync_task{}]
               , last_generation_in_sync = false :: boolean()
               , top_target = 0                  :: aec_blocks:height()
               , gossip_txs = true               :: boolean()
               }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    aec_events:subscribe(block_to_publish),
    aec_events:subscribe(tx_created),
    aec_events:subscribe(tx_received),

    Peers = parse_peers(aeu_env:user_map_or_env(<<"peers">>, aecore, peers, [])),
    BlockedPeers = parse_peers(aeu_env:user_map_or_env(<<"blocked_peers">>, aecore, blocked_peers, [])),

    [aec_peers:block_peer(P) || P <- BlockedPeers],
    aec_peers:add_trusted(Peers),

    erlang:process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({worker_for_peer, PeerId}, _, State) ->
    {reply, get_worker_for_peer(State, PeerId), State};
handle_call({sync_in_progress, PeerId}, _, State) ->
    {reply, peer_in_sync(State, PeerId), State};
handle_call({known_chain, Chain0 = #{ chain_id := CId0 }, NewChainInfo}, _From, State0) ->
    {Chain, State} =
        case NewChainInfo of
            none ->
                {Chain0, State0};
            #{ chain_id := CId1 } when CId1 == CId0 ->
                {merge_chains(Chain0, NewChainInfo), State0};
            #{ chain_id := _CId } ->
                {Chain0, add_chain_info(NewChainInfo, State0)}
        end,
    {Res, State1} = sync_task_for_chain(Chain, State),
    State2 =
        case Res of
            {NewOrEx, SyncChain, _} when NewOrEx == new; NewOrEx == existing ->
                maybe_new_top_target(SyncChain, State1);
            {inconclusive, _, _} ->
                State1
        end,
    {reply, Res, State2};
handle_call({update_sync_task, Update, STId}, _From, State) ->
    State1 = do_update_sync_task(State, STId, Update),
    {reply, ok, State1};
handle_call({next_work_item, STId, PeerId, {error, _Reason}}, _From, State) ->
    State1 = do_update_sync_task(State, STId, {error, PeerId}),
    {reply, abort_work, State1};
handle_call({next_work_item, STId, PeerId, LastResult}, _From, State) ->
    State1 = handle_last_result(State, STId, LastResult),
    {Reply, State2} = get_next_work_item(State1, STId, PeerId),
    {reply, Reply, State2};
handle_call({gossip_txs, GossipTxs}, _From, State) ->
    {reply, ok, State#state{ gossip_txs = GossipTxs }};
handle_call(_, _From, State) ->
    {reply, error, State}.

handle_cast({start_sync, PeerId, RemoteHash, _RemoteDifficulty}, State) ->
    %% We could decide not to sync if we are already syncing, but that
    %% opens up for an attack in which someone fakes to have higher difficulty
    run_job(sync_tasks, fun() -> do_start_sync(PeerId, RemoteHash) end),
    {noreply, State};
handle_cast({get_generation, PeerId, Hash},
            State = #state{ last_generation_in_sync = false }) ->
    run_job(sync_tasks,
            fun() ->
                case do_get_generation(PeerId, Hash) of
                    ok         -> set_last_generation_in_sync();
                    {error, _} -> ok
                end
            end),
    {noreply, State};
handle_cast({get_generation, _PeerId, _Hash}, State) ->
    {noreply, State};
handle_cast(set_last_generation_in_sync, State) ->
    {noreply, State#state{ last_generation_in_sync = true }};
handle_cast({schedule_ping, PeerId}, State) ->
    case peer_in_sync(State, PeerId) of
        true ->
            ok;
        false ->
            run_job(sync_ping, fun() -> ping_peer(PeerId) end)
    end,
    {noreply, State};
handle_cast({handle_worker, STId, Action}, State) ->
    State1 =
        case get_sync_task(STId, State) of
            {ok, ST} ->
                ST1 = do_handle_worker(Action, ST),
                set_sync_task(ST1, State);
            {error, not_found} ->
                State
        end,
    {noreply, State1};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, Event, #{info := Info}},
            State = #state{ gossip_txs = GossipTxs }) ->
    %% FUTURE: Forward blocks only to outbound connections.
    %% Take a random subset (possibly empty) of peers that agree with us 
    %% on chain height to forward blocks and transactions to.
    PeerIds = [ aec_peers:peer_id(P) || P <- aec_peers:get_random(10) ],
    NonSyncingPeerIds = [ P || P <- PeerIds, not peer_in_sync(State, P) ],
    case Event of
        block_to_publish ->
            Block = Info,
            enqueue(block, Block, NonSyncingPeerIds);
        tx_created when GossipTxs ->
            %% If we allow http requests creating transactions when we are catching up
            %% with other chains, we just keep them in mempool
            enqueue(tx, Info, PeerIds);
        tx_received when GossipTxs ->
            enqueue(tx, Info, NonSyncingPeerIds);
        _             -> ignore
    end,
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    %% It might be one of our syncing workers that crashed
    if Reason =/= normal -> epoch_sync:info("worker stopped with reason: ~p", [Reason]);
       true -> ok
    end,
    {noreply, do_terminate_worker(Pid, State, Reason)};
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.

sync_task_for_chain(Chain, S = #state{ sync_tasks = STs }) ->
    case match_tasks(Chain, STs, []) of
        no_match ->
            ST = init_sync_task(Chain),
            {{new, Chain, ST#sync_task.id}, set_sync_task(ST, S)};
        {match, ST = #sync_task{ id = STId, chain = C2 }} ->
            NewChain = merge_chains(Chain#{ chain_id := STId }, C2),
            ST1 = ST#sync_task{ chain = NewChain },
            {{existing, NewChain, STId}, set_sync_task(ST1, S)};
        Res = {inconclusive, _, _} ->
            {Res, S}
    end.

handle_last_result(State, STId, LastResult) ->
    case get_sync_task(STId, State) of
        {ok, ST} ->
            ST1 = handle_last_result(ST, LastResult),
            maybe_end_sync_task(State, ST1);
        {error, not_found} ->
            State
    end.

handle_last_result(ST, none) ->
    ST;
handle_last_result(ST = #sync_task{ agreed = undefined }, {agreed_height, Agreed}) ->
    ST#sync_task{ agreed = Agreed };
handle_last_result(ST, {agreed_height, _Agreed}) ->
    ST;
handle_last_result(ST = #sync_task{ pool = [] }, {hash_pool, HashPool}) ->
    {Height, Hash, false} = lists:last(HashPool),
    ST#sync_task{ pool = HashPool, agreed = #{ height => Height, hash => Hash } };
handle_last_result(ST, {hash_pool, _HashPool}) ->
    ST;
handle_last_result(ST = #sync_task{ pool = Pool }, {get_generation, Height, Hash, PeerId, {ok, Block}}) ->
    Pool1 = lists:keyreplace(Height, 1, Pool, {Height, Hash, {PeerId, Block}}),
    ST#sync_task{ pool = Pool1 };
handle_last_result(ST, {post_blocks, ok}) ->
    ST#sync_task{ adding = [] };
handle_last_result(ST = #sync_task{ adding = Add, pending = Pends, pool = Pool, chain = Chain },
                  {post_blocks, {error, BlockFromPeerId, Height}}) ->
    %% Put back the blocks we did not manage to post, and schedule failing block
    %% for another retreival.
    [{Height, Hash, _} | PutBack] =
        lists:dropwhile(fun({H, _, _}) -> H < Height end, Add) ++ lists:append(Pends),
    NewPool = [{Height, Hash, false} | PutBack] ++ Pool,
    ST1 = ST#sync_task{ adding = [], pending = [], pool = NewPool },

    ST1#sync_task{ chain = Chain#{ peers := maps:get(peers, Chain) -- [BlockFromPeerId] }}.


split_pool(Pool) -> split_pool(Pool, []).

split_pool([{_, _, false} | _] = Pool, Acc) ->
    {lists:reverse(Acc), Pool};
split_pool([], Acc) ->
    {lists:reverse(Acc), []};
split_pool([X | Pool], Acc) ->
    split_pool(Pool, [X | Acc]).

get_next_work_item(State, STId, PeerId) ->
    case get_sync_task(STId, State) of
        {ok, ST = #sync_task{ chain = #{ peers := PeerIds } }} ->
            case lists:member(PeerId, PeerIds) of
                true ->
                    {Reply, ST1} = get_next_work_item(ST),
                    {Reply, set_sync_task(STId, ST1, State)};
                false ->
                    {abort_work, State}
            end;
        {error, not_found} ->
            {abort_work, State}
    end.

get_next_work_item(ST = #sync_task{ adding = [], pending = [ToAdd | NewPending] }) ->
    {{post_blocks, ToAdd}, ST#sync_task{ adding = ToAdd, pending = NewPending }};
get_next_work_item(ST = #sync_task{ chain = Chain, agreed = undefined }) ->
    {{agree_on_height, Chain}, ST};
get_next_work_item(ST = #sync_task{ pool = [], agreed = #{ hash := LastHash, height := H }, chain = Chain }) ->
    TargetHash = next_known_hash(maps:get(chain, Chain), H + ?MAX_HEADERS_PER_CHUNK),
    {{fill_pool, LastHash, TargetHash}, ST};
get_next_work_item(ST = #sync_task{ pool = [{_, _, {_, _}} | _] = Pool, adding = Add, pending = Pend }) ->
    {ToBeAdded, NewPool} = split_pool(Pool),
    case Add of
        [] ->
            {{post_blocks, ToBeAdded}, ST#sync_task{ pool = NewPool, adding = ToBeAdded }};
        _ when length(Pend) < 10 orelse NewPool /= [] ->
            get_next_work_item(ST#sync_task{ pool = NewPool, pending = Pend ++ [ToBeAdded] });
        _ ->
            {take_a_break, ST}
    end;
get_next_work_item(ST = #sync_task{ pool = [{_, _, false} | _] = Pool }) ->
    PickFrom = [ P || P = {_, _, false} <- Pool ],
    Random = rand:uniform(length(PickFrom)),
    {PickH, PickHash, false} = lists:nth(Random, PickFrom),
    epoch_sync:debug("Get block at height ~p", [PickH]),
    {{get_generation, PickH, PickHash}, ST};
get_next_work_item(ST) ->
    epoch_sync:info("Nothing to do: ~p", [ST]),
    {take_a_break, ST}.

maybe_end_sync_task(State, ST) ->
    case ST#sync_task.chain of
        #{ peers := [], chain := [Target | _] } ->
            epoch_sync:info("Removing/ending sync task ~p target was ~p",
                            [ST#sync_task.id, Target]),
            State1 = delete_sync_task(ST, State),
            maybe_update_top_target(State1);
        _ ->
            set_sync_task(ST, State)
    end.

maybe_new_top_target(#{ chain := [#{ height := ChainTopTarget } | _] }, State) ->
    case State#state.top_target < ChainTopTarget of
        true  -> update_top_target(ChainTopTarget, State);
        false -> State
    end.

maybe_update_top_target(State = #state{ top_target = TopTarget, sync_tasks = STs }) ->
    NewTop = lists:foldl(fun(#sync_task{ chain = Chain }, PrevMax) ->
                             case Chain of
                                 #{ chain := [#{ height := H } | _] } when H > PrevMax -> H;
                                 _ -> PrevMax
                             end
                         end, 0, STs),
    case TopTarget == NewTop of
        true  -> State;
        false -> update_top_target(NewTop, State)
    end.

update_top_target(TopTarget, State) ->
    aec_tx_pool:new_sync_top_target(TopTarget),
    State#state{ top_target = TopTarget }.

do_update_sync_task(State, STId, Update) ->
    case get_sync_task(STId, State) of
        {ok, ST = #sync_task{ chain = Chain }} ->
            Chain1 =
                case Update of
                    {done, PeerId} ->
                        Chain#{ peers := maps:get(peers, Chain) -- [PeerId] };
                    {error, PeerId} ->
                        Chain#{ peers := maps:get(peers, Chain) -- [PeerId] }
                end,
            maybe_end_sync_task(State, ST#sync_task{ chain = Chain1 });
        {error, not_found} ->
            epoch_sync:debug("SyncTask ~p not found", [STId]),
            State
    end.

get_sync_task(STId, #state{ sync_tasks = STs }) ->
    case lists:keyfind(STId, #sync_task.id, STs) of
        false -> {error, not_found};
        ST    -> {ok, ST}
    end.

set_sync_task(ST = #sync_task{ id = STId }, State) ->
    set_sync_task(STId, ST, State).

set_sync_task(STId, ST, S = #state{ sync_tasks = STs }) ->
    S#state{ sync_tasks = lists:keystore(STId, #sync_task.id, STs, ST) }.

delete_sync_task(#sync_task{ id = STId }, S) ->
    delete_sync_task(STId, S);
delete_sync_task(STId, S = #state{ sync_tasks = STs }) ->
    S#state{ sync_tasks = lists:keydelete(STId, #sync_task.id, STs) }.

add_chain_info(Chain = #{ chain_id := CId }, S) ->
    case get_sync_task(CId, S) of
        {ok, ST = #sync_task{ chain = Chain2 }} ->
            ST1 = ST#sync_task{ chain = merge_chains(Chain, Chain2) },
            set_sync_task(ST1, S);
        {error, not_found} ->
            S
    end.

init_sync_task(Chain) ->
    #sync_task{ id = maps:get(chain_id, Chain), chain = Chain }.

merge_chains(#{ chain_id := CId, peers := Ps1, chain := C1 },
             #{ chain_id := CId, peers := Ps2, chain := C2 }) ->
    %% We sort descending...
    Cmp = fun(#{ height := H1 }, #{ height := H2 }) -> H1 >= H2 end,
    #{ chain_id => CId, peers => lists:usort(Ps1 ++ Ps2),
       chain => lists:umerge(Cmp, C1, C2) }.

match_tasks(_Chain, [], []) ->
    no_match;
match_tasks(Chain, [], Acc) ->
    {N, #{ chain_id := CId, peers := Peers }} = hd(lists:reverse(Acc)),
    {inconclusive, Chain, {get_header, CId, Peers, N}};
match_tasks(Chain1, [ST = #sync_task{ chain = Chain2 } | STs], Acc) ->
    case match_chains(maps:get(chain, Chain1), maps:get(chain, Chain2)) of
        equal     -> {match, ST};
        different -> match_tasks(Chain1, STs, Acc);
        {fst, N}  -> match_tasks(Chain1, STs, [{N, Chain1} | Acc]);
        {snd, N}  -> match_tasks(Chain1, STs, [{N, Chain2} | Acc])
    end.

match_chains([#{ height := N1 } | C1], [#{ height := N2, hash := H } | _]) when N1 > N2 ->
    case find_hash_at_height(N2, C1) of
        {ok, H}   -> equal;
        {ok, _}   -> different;
        not_found -> {fst, N2}
    end;
match_chains([#{ height := N1, hash := H } | _], C2) ->
    case find_hash_at_height(N1, C2) of
        {ok, H}   -> equal;
        {ok, _}   -> different;
        not_found -> {snd, N1}
    end.

find_hash_at_height(N, [#{ height := N, hash := H } | _]) ->
    {ok, H};
find_hash_at_height(_, []) ->
    not_found;
find_hash_at_height(N, [#{ height := N1 } | _]) when N1 < N ->
    not_found;
find_hash_at_height(N, [_ | Cs]) ->
    find_hash_at_height(N, Cs).

do_handle_worker({new_worker, PeerId, Pid}, ST = #sync_task{ workers = Ws }) ->
    case lists:keyfind(PeerId, 1, Ws) of
        false    -> ok;
        {_, Old} -> epoch_sync:info("Peer ~p already has a worker (~p)", [ppp(PeerId), Old])
    end,
    erlang:link(Pid),
    epoch_sync:debug("New worker ~p for peer ~p", [Pid, ppp(PeerId)]),
    ST#sync_task{ workers = lists:keystore(PeerId, 1, Ws, {PeerId, Pid}) };
do_handle_worker({change_worker, PeerId, OldPid, NewPid}, ST = #sync_task{ workers = Ws }) ->
    case lists:keyfind(PeerId, 1, Ws) of
        false ->
            epoch_sync:info("Missing worker ~p for peer ~p", [OldPid, ppp(PeerId)]);
        {_, OldPid} ->
            ok;
        {_, AnotherPid} ->
            epoch_sync:info("Wrong worker stored for peer ~p (~p)", [ppp(PeerId), AnotherPid])
    end,
    erlang:link(NewPid),
    %% The old worker will terminate right after the call to handle_worker so we can safely
    %% unlink it.
    erlang:unlink(OldPid),
    epoch_sync:debug("Update worker ~p (was ~p) for peer ~p", [NewPid, OldPid, ppp(PeerId)]),
    ST#sync_task{ workers = lists:keystore(PeerId, 1, Ws, {PeerId, NewPid}) }.

do_terminate_worker(Pid, S = #state{ sync_tasks = STs }, Reason) ->
    case [ ST || ST <- STs, lists:keyfind(Pid, 2, ST#sync_task.workers) /= false ] of
        [ST] ->
            {Peer, ST1} = do_terminate_worker(Pid, ST),
            S1 = set_sync_task(ST1, S),
            %% If abnormal termination, update sync task accordingly
            case Reason of
                normal -> S1;
                _ -> do_update_sync_task(S1, ST#sync_task.id, {error, Peer})
            end;
        [] ->
            S
    end.

do_terminate_worker(Pid, ST = #sync_task{ workers = Ws }) ->
    {Peer, _} = lists:keyfind(Pid, 2, Ws),
    epoch_sync:debug("Terminating worker ~p for peer ~p", [Pid, ppp(Peer)]),
    {Peer, ST#sync_task{ workers = lists:keydelete(Pid, 2, Ws) }}.

%%%=============================================================================
%%% Jobs worker
%%%=============================================================================
delayed_run_job(PeerId, Task, Queue, Fun, Delay) ->
    OldWorker = self(),
    NewWorker = proc_lib:spawn(
        fun() ->
            timer:sleep(Delay),
            aec_jobs_queues:run(Queue, Fun)
        end),
    handle_worker(Task, {change_worker, PeerId, OldWorker, NewWorker}).

run_job(Queue, Fun) ->
    proc_lib:spawn(jobs, run, [Queue, Fun]).

%% Gossip Tx or Block - spawn a process and call jobs from there.
enqueue(Kind, Data, []) ->
    ok;
enqueue(Kind, Data, PeerIds) ->
    spawn(fun() ->
    case Kind of
        block ->
            %% Getting blocks to spread is a priority, bypass the gossip queue
            SerBlock = aec_peer_connection:gossip_serialize_block(Data),
            [ do_forward_block(SerBlock, PId) || PId <- PeerIds ];
        tx ->
            SerTx = aec_peer_connection:gossip_serialize_tx(Data),
            aec_jobs_queues:run(sync_gossip, fun() -> [ do_forward_tx(SerTx, PId) || PId <- PeerIds ] end)
    end end).

ping_peer(PeerId) ->
    Res = aec_peer_connection:ping(PeerId),
    epoch_sync:debug("Ping (~p): ~p", [ppp(PeerId), Res]),
    case Res of
        ok ->
            aec_peers:log_ping(PeerId, ok);
        {error, _} ->
            aec_peers:log_ping(PeerId, error)
    end.

do_forward_block(SerBlock, PeerId) ->
    Res = aec_peer_connection:send_block(PeerId, SerBlock),
    epoch_sync:debug("send_block to (~p): ~p", [ppp(PeerId), Res]).

do_forward_tx(SerTx, PeerId) ->
    Res = aec_peer_connection:send_tx(PeerId, SerTx),
    epoch_sync:debug("send_tx to (~p): ~p", [ppp(PeerId), Res]).

do_start_sync(PeerId, RemoteHash) ->
    case sync_in_progress(PeerId) of
        true ->
            epoch_sync:debug("Already syncing with ~p", [ppp(PeerId)]);
        false ->
           do_start_sync1(PeerId, RemoteHash)
    end.

do_start_sync1(PeerId, RemoteHash) ->
    case aec_peer_connection:get_header_by_hash(PeerId, RemoteHash) of
        {ok, Hdr} ->
            epoch_sync:debug("New header received (~p): ~p", [ppp(PeerId), pp(Hdr)]),

            %% We do try really hard to identify the same chain here...
            Chain = init_chain(PeerId, Hdr),
            case known_chain(Chain) of
                {ok, Task} ->
                    handle_worker(Task, {new_worker, PeerId, self()}),
                    do_work_on_sync_task(PeerId, Task);
                {error, Reason} ->
                    epoch_sync:info("Could not identify chain, aborting sync with ~p (~p)",
                                    [ppp(PeerId), Reason])
            end;
        {error, Reason} ->
            epoch_sync:debug("fetching top block (~p) failed: ~p", [ppp(PeerId), Reason])
    end.

init_chain(PeerId, Header) ->
    init_chain(make_ref(), [PeerId], Header).

init_chain(ChainId, Peers, Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    Height     = aec_headers:height(Header),
    PrevHash   = [ #{ height => Height - 1, hash => aec_headers:prev_hash(Header)} || Height > 1 ],
    #{ chain_id => ChainId, peers => Peers,
       chain => [#{ hash => Hash, height => Height }] ++ PrevHash }.

known_chain(Chain) ->
    identify_chain(known_chain(Chain, none)).

identify_chain({existing, _Chain, Task}) ->
    epoch_sync:debug("Already syncing chain ~p", [Task]),
    {ok, Task};
identify_chain({new, #{ chain := [Target | _]}, Task}) ->
    epoch_sync:info("Starting new sync task ~p target is ~p", [Task, Target]),
    {ok, Task};
identify_chain({inconclusive, Chain, {get_header, CId, Peers, N}}) ->
    %% We need another hash for this chain, make sure whoever we ask is
    %% still on this particular chain by including a known (at higher height) hash
    KnownHash = next_known_hash(maps:get(chain, Chain), N),
    case do_get_header_by_height(Peers, N, KnownHash) of
        {ok, Header} ->
            identify_chain(known_chain(Chain, init_chain(CId, Peers, Header)));
        Err = {error, _} ->
            epoch_sync:info("fetching header at height ~p from ~p failed", [N, Peers]),
            Err
    end.

%% Get the next known hash at a height bigger than N; or if no such hash
%% exist, the hash at the highest known height.
next_known_hash(Cs, N) ->
    #{ hash := Hash } =
        case lists:takewhile(fun(#{ height := H }) -> H > N end, Cs) of
            []  -> hd(Cs);
            Cs1 -> lists:last(Cs1)
        end,
    Hash.

do_get_header_by_height([], _N, _TopHash) ->
    {error, header_not_found};
do_get_header_by_height([PeerId | PeerIds], N, TopHash) ->
    case aec_peer_connection:get_header_by_height(PeerId, N, TopHash) of
        {ok, Header} ->
            {ok, Header};
        {error, Reason} ->
            epoch_sync:debug("fetching header at height ~p under ~p from ~p failed: ~p",
                             [N, pp(TopHash), ppp(PeerId), Reason]),
            do_get_header_by_height(PeerIds, N, TopHash)
    end.


do_work_on_sync_task(PeerId, Task) ->
    do_work_on_sync_task(PeerId, Task, none).

do_work_on_sync_task(PeerId, Task, LastResult) ->
    %% epoch_sync:debug("working on ~p against ~p (Last: ~p)", [Task, ppp(PeerId), LastResult]),
    case next_work_item(Task, PeerId, LastResult) of
        take_a_break ->
            delayed_run_job(PeerId, Task, sync_tasks,
                            fun() -> do_work_on_sync_task(PeerId, Task) end, 250);
        {agree_on_height, Chain} ->
            #{ chain := [#{ hash := TopHash, height := TopHeight } | _] } = Chain,
            LocalHeader = aec_chain:top_header(),
            LocalHeight = aec_headers:height(LocalHeader),
            MinAgreedHash = aec_chain:genesis_hash(),
            MaxAgree = min(LocalHeight, TopHeight),
            case  agree_on_height(PeerId, TopHash, TopHeight,
                                  MaxAgree, MaxAgree, ?GENESIS_HEIGHT, MinAgreedHash) of
                {ok, AHeight, AHash} ->
                    epoch_sync:debug("Agreed upon height (~p): ~p", [ppp(PeerId), AHeight]),
                    Agreement = {agreed_height, #{ height => AHeight, hash => AHash }},
                    do_work_on_sync_task(PeerId, Task, Agreement);
                {error, Reason} ->
                    do_work_on_sync_task(PeerId, Task, {error, {agree_on_height, Reason}})
            end;
        {fill_pool, StartHash, TargetHash} ->
            fill_pool(PeerId, StartHash, TargetHash, Task);
        {post_blocks, Blocks} ->
            Res = post_blocks(Blocks),
            do_work_on_sync_task(PeerId, Task, {post_blocks, Res});
        {get_generation, Height, Hash} ->
            Res =
                case do_fetch_generation(PeerId, Hash) of
                    {ok, local}     -> {get_generation, Height, Hash, PeerId, {ok, local}};
                    {ok, Block}     -> {get_generation, Height, Hash, PeerId, {ok, Block}};
                    {error, Reason} -> {error, {get_generation, Reason}}
                end,
            do_work_on_sync_task(PeerId, Task, Res);
        abort_work ->
            epoch_sync:info("~p aborting sync work against ~p", [self(), PeerId]),
            schedule_ping(PeerId)
    end.

post_blocks([]) -> ok;
post_blocks([{StartHeight, _, _} | _] = Blocks) ->
    post_blocks(StartHeight, StartHeight, Blocks).

post_blocks(From, To, []) ->
    epoch_sync:info("Synced blocks ~p - ~p", [From, To]),
    ok;
post_blocks(From, _To, [{Height, _Hash, {_PeerId, local}} | Blocks]) ->
    post_blocks(From, Height, Blocks);
post_blocks(From, To, [{Height, _Hash, {PeerId, Block}} | Blocks]) ->
    case add_generation(Block) of
        ok ->
            post_blocks(From, Height, Blocks);
        {error, Reason} ->
            epoch_sync:info("Failed to add synced block ~p: ~p", [Height, Reason]),
            [ epoch_sync:info("Synced blocks ~p - ~p", [From, To - 1]) || To > From ],
            {error, PeerId, Height}
    end.

%% In order not to timeout the conductor, large generations are added in
%% smaller chuncks, a few micro blocks at the time.
%% Each micro block has a fixed maximum gas, by limiting the number of micro
%% blocks we limit the total amount of work the conductor has to perform in
%% each synchronous call.
%% Map contains key dir, saying in which direction we sync
add_generation(#{micro_blocks := MBlocks} = Generation) ->
    add_micro_blocks_in_generation(Generation#{micro_blocks => []}, MBlocks).

add_micro_blocks_in_generation(Gen, []) ->
    aec_conductor:add_synced_generation_batched(Gen, true);
add_micro_blocks_in_generation(Gen, [MBlock | MBlocks]) ->
    case aec_conductor:add_synced_generation_batched(Gen#{micro_blocks => [MBlock]}, false) of
        ok -> add_micro_blocks_in_generation(Gen, MBlocks);
        Error -> Error
    end.

%% Ping logic makes sure they always agree on genesis header (height 0)
%% We look for the block that is both on remote highest chain and in our local
%% chain connected to genesis (may be on a fork, but that fork has now more
%% difficulty than our highest chain (otherwise we would not sync).
%%
%% Invariant: AgreedHash is hash at height Min.
agree_on_height(_PeerId, _RHash, _RH, _LH, Min, Min, AgreedHash) ->
    {ok, Min, AgreedHash};
agree_on_height(PeerId, RHash, RH, CheckHeight, Max, Min, AgreedHash) when RH == CheckHeight ->
    case aec_chain:hash_is_connected_to_genesis(RHash) of
        true ->
             %% We agree on a block
             Middle = (Max + RH) div 2,
             case Min < Middle andalso Middle < Max of
               true ->
                   agree_on_height(PeerId, RHash, RH, Middle, Max, RH, RHash);
               false ->
                   {ok, RH, RHash}
             end;
        false ->
             %% We disagree. Local on a fork compared to remote. Check half-way
             Middle = (Min + RH) div 2,
             case Min < Middle andalso Middle < Max of
                 true ->
                     agree_on_height(PeerId, RHash, RH, Middle, RH, Min, AgreedHash);
                 false ->
                     {ok, Min, AgreedHash}
             end
    end;
agree_on_height(PeerId, RHash0, RH, CheckHeight, Max, Min, AgreedHash) when RH =/= CheckHeight ->
    case aec_peer_connection:get_header_by_height(PeerId, CheckHeight, RHash0) of
         {ok, RemoteAtHeight} ->
             {ok, RHash} = aec_headers:hash_header(RemoteAtHeight),
             epoch_sync:debug("New header received (~p): ~p", [ppp(PeerId), pp(RemoteAtHeight)]),
             agree_on_height(PeerId, RHash, CheckHeight, CheckHeight, Max, Min, AgreedHash);
         {error, Reason} ->
             epoch_sync:debug("Fetching header ~p from ~p failed: ~p", [CheckHeight, ppp(PeerId), Reason]),
             {error, Reason}
    end.

fill_pool(PeerId, StartHash, TargetHash, ST) ->
    case aec_peer_connection:get_n_successors(PeerId, StartHash, TargetHash, ?MAX_HEADERS_PER_CHUNK) of
        {ok, []} ->
            do_get_generation(PeerId, StartHash),
            update_sync_task({done, PeerId}, ST),
            epoch_sync:info("Sync done (according to ~p)", [ppp(PeerId)]),
            aec_events:publish(chain_sync, {chain_sync_done, PeerId});
        {ok, Hashes} ->
            HashPool = [ {Height, Hash, false} || {Height, Hash} <- Hashes ],
            do_work_on_sync_task(PeerId, ST, {hash_pool, HashPool});
        {error, _} = Error ->
            epoch_sync:info("Abort sync with ~p (~p) ", [ppp(PeerId), Error]),
            update_sync_task({error, PeerId}, ST),
            {error, sync_abort}
    end.

do_get_generation(PeerId, LastHash) ->
    case aec_peer_connection:get_generation(PeerId, LastHash, forward) of
        {ok, KeyBlock, MicroBlocks, forward} ->
            Generation = #{ key_block => KeyBlock,
                            micro_blocks => MicroBlocks,
                            dir => forward },
            add_generation(Generation);
        Err = {error, _} ->
            Err
    end.

do_fetch_generation(PeerId, Hash) ->
    case has_generation(Hash) of
        true ->
            epoch_sync:debug("block ~p already fetched, using local copy", [pp(Hash)]),
            {ok, local};
        false ->
            do_fetch_generation_ext(Hash, PeerId)
    end.

do_fetch_generation_ext(Hash, PeerId) ->
    epoch_sync:debug("we don't have the block -fetching (~p)", [pp(Hash)]),
    case aec_peer_connection:get_generation(PeerId, Hash, backward) of
        {ok, KeyBlock, MicroBlocks, backward} ->
            case header_hash(KeyBlock) =:= Hash of
                true ->
                    epoch_sync:debug("block fetched from ~p (~p); ~p",
                                     [ppp(PeerId), pp(Hash), pp(KeyBlock)]),
                    {ok, #{ key_block    => KeyBlock,
                            micro_blocks => MicroBlocks,
                            dir          => backward }};
                false ->
                    {error, hash_mismatch}
            end;
        {error, _} = Error ->
            epoch_sync:debug("failed to fetch block from ~p; Hash = ~p; Error = ~p",
                             [ppp(PeerId), pp(Hash), Error]),
            Error
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

has_generation(KeyBlockHash) ->
    %% We are looking for the generation backwards from this Hash
    %% Possibly optimize by implementing aec_chain:has_generation operating on
    %% headers only
    case aec_chain:get_generation_by_hash(KeyBlockHash, backward) of
        {ok, _Generation} -> true;
        error             -> false
    end.

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

peer_in_sync(#state{ sync_tasks = STs }, PeerId) ->
    lists:member(PeerId, lists:append([ Ps || #sync_task{ chain = #{ peers := Ps } } <- STs ])).

get_worker_for_peer(#state{ sync_tasks = STs }, PeerId) ->
    case [ Pid || #sync_task{ workers = Ws } <- STs, {PeerId0, Pid} <- Ws, PeerId0 == PeerId ] of
        [] -> false;
        [Pid | _] -> {ok, Pid}
    end.
