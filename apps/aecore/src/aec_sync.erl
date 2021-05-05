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
-import(aec_peer, [ppp/1]).

%% API
-export([start_link/0]).

%% API called from strongly connected component aec_peers
-export([schedule_ping/1]).

-export([ start_sync/3
        , get_generation/2
        , set_last_generation_in_sync/0
        , ask_all_for_node_info/0
        , ask_all_for_node_info/1]).

-export([is_syncing/0,
         sync_progress/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-define(MAX_HEADERS_PER_CHUNK, 100).
-define(DEFAULT_MAX_GOSSIP, 16).

%%%=============================================================================
%%% API
%%%=============================================================================

start_sync(PeerId, RemoteHash, RemoteDifficulty) ->
    gen_server:cast(?MODULE, {start_sync, PeerId, RemoteHash, RemoteDifficulty}).

get_generation(PeerId, Hash) ->
    gen_server:cast(?MODULE, {get_generation, PeerId, Hash}).

set_last_generation_in_sync() ->
    gen_server:cast(?MODULE, set_last_generation_in_sync).

ask_all_for_node_info() ->
    ask_all_for_node_info(8500).

%% there are a bunch of processes adding their own overhead for this timer.
%% While this function will timeout in Timeout milliseconds, the acutal
%% timeout for the peer to respond is Timeout - 3000
ask_all_for_node_info(Timeout) when Timeout > 3000 ->
    gen_server:call(?MODULE, {ask_all_for_node_info, Timeout - 500}, Timeout).

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

-spec sync_progress() -> {boolean(), float()}.
sync_progress() ->
    gen_server:call(?MODULE, sync_progress).

-spec is_syncing() -> boolean().
is_syncing() ->
    gen_server:call(?MODULE, is_syncing).

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
%% We add new node to sync pool to sync agreed block up to top of new
%% 1. If we are already synchronizing, we ignore it,
%%    the ongoing sync will pick that up later
%% 2. If we are not already synchronizing, we start doing so.
%%
%% We sync with several nodes at the same time and use as strategy
%% to pick a random hash from the hashes in the pool.

-type chain_id() :: reference().
-record(chain_block, { hash :: aec_blocks:block_header_hash()
                     , height :: aec_blocks:height()
                     }).
-record(chain, { id :: chain_id()
               , peers :: [aec_peer:id()]
               , blocks :: [#chain_block{}, ...]
               }).
-record(pool_item, { height :: aec_blocks:height()
                   , hash :: aec_blocks:block_header_hash()
                   , got :: false
                          | { aec_peer:id()
                            , local | #{ key_block := aec_blocks:key_block()
                                       , micro_blocks := [aec_blocks:micro_block()]
                                       , dir := backward
                                       }
                            }
                   }).
-record(worker, { peer_id :: aec_peer:id()
                , pid :: pid()
                }).
-record(sync_task, {id :: chain_id(),
                    suspect = false :: boolean(),
                    chain :: #chain{},
                    pool = [] :: [#pool_item{}],
                    agreed :: undefined | #chain_block{},
                    adding = [] :: [#pool_item{}],
                    pending = [] :: [[#pool_item{}]],
                    workers = [] :: [#worker{}]}).
-record(state, { sync_tasks = []                 :: [#sync_task{}]
               , last_generation_in_sync = false :: boolean()
               , top_target = 0                  :: aec_blocks:height()
               , gossip_txs = true               :: boolean()
               , is_syncing = false              :: boolean()
               }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    aec_events:subscribe(block_to_publish),
    aec_events:subscribe(tx_created),
    aec_events:subscribe(tx_received),

    DefaultPeers = 
        case aeu_env:find_config([<<"include_default_peers">>], [user_config, schema_default, {value, true}]) of
            {ok, true} -> default_peers();
            {ok, false} -> [];
            undefined -> []
        end,
    ConfigPeers  = aeu_env:user_config(<<"peers">>, []),
    lager:debug("Config peers: ~p", [ConfigPeers]),
    lager:debug("Sys config peers: ~p", [DefaultPeers]),

    Peers0       = parse_peers(ConfigPeers ++ DefaultPeers),
    BlockedPeers = parse_peers(aeu_env:user_map_or_env(<<"blocked_peers">>, aecore, blocked_peers, [])),
    Peers        = Peers0 -- BlockedPeers,
    lager:debug("Peers0: ~p", [Peers0]),
    lager:debug("Blocked peers: ~p", [BlockedPeers]),
    lager:info("Trusted peers: ~p", [Peers]),

    aec_peers:add_trusted(Peers),

    lists:foreach(
        fun(Peer) ->
            Source = aec_peer:source(Peer),
            Info = aec_peer:info(Peer),
            %% add untrusted
            aec_peers:add_peers(Source, [Info])
        end,
        aec_db:read_all_peers()),

    %% block untrusted peers after they've been added
    [aec_peers:block_peer(P) || P <- BlockedPeers],

    erlang:send_after(rand:uniform(1000), self(), update_sync_progress_metric),

    erlang:process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({worker_for_peer, PeerId}, _, State) ->
    {reply, get_worker_for_peer(State, PeerId), State};
handle_call({sync_in_progress, PeerId}, _, State) ->
    {reply, peer_in_sync(State, PeerId), State};
handle_call({known_chain, Chain0 = #chain{ id = CId0 }, NewChainInfo}, _From, State0) ->
    {Chain, State} =
        case NewChainInfo of
            none ->
                {Chain0, State0};
            #chain{ id = CId1 } when CId1 == CId0 ->
                {merge_chains(Chain0, NewChainInfo), State0};
            #chain{ id = _CId } ->
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
handle_call(sync_progress, _From, State) ->
    {reply, sync_progress(State), State};
handle_call(is_syncing, _From, State) ->
    {reply, is_syncing(State), State};
handle_call({ask_all_for_node_info, Timeout}, From, State) ->
    run_job(sync_tasks,
            fun() ->
                Infos = collect_infos(Timeout),
                Response = process_infos(Infos),
                gen_server:reply(From, Response)
            end),
    {noreply, State};
handle_call(_, _From, State) ->
    {reply, error, State}.

handle_cast({start_sync, PeerId, RemoteHash, _RemoteDifficulty}, State) ->
    %% We could decide not to sync if we are already syncing, but that
    %% opens up for an attack in which someone fakes to have higher difficulty
    run_job(sync_tasks, fun() -> do_start_sync(PeerId, RemoteHash) end),
    {noreply, State};
handle_cast({get_generation, PeerId, Hash},
            %% Usually initial sync. Trust gossip afterwards.
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
            aec_peers:log_ping(PeerId, ok);
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
    MaxGossip = max_gossip(),
    PeerIds = [ aec_peer:id(P) || P <- aec_peers:get_random_connected(MaxGossip) ],
    NonSyncingPeerIds = [ P || P <- PeerIds, not peer_in_sync(State, P) ],
    case Event of
        block_to_publish ->
            case Info of
                {created, Block} ->
                    PeerIds1 = [ aec_peer:id(P) || P <- aec_peers:connected_peers(all) ],
                    enqueue(block, Block, PeerIds1);
                {received, Block} ->
                    enqueue(block, Block, NonSyncingPeerIds)
            end;
        tx_created when GossipTxs ->
            %% If we allow http requests creating transactions when we are catching up
            %% with other chains, we just keep them in mempool
            enqueue(tx, Info, PeerIds);
        tx_received when GossipTxs ->
            enqueue(tx, Info, PeerIds);
        _             -> ignore
    end,
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    %% It might be one of our syncing workers that crashed
    if Reason =/= normal -> epoch_sync:info("worker stopped with reason: ~p", [Reason]);
       true -> ok
    end,
    {noreply, do_terminate_worker(Pid, State, Reason)};
handle_info(update_sync_progress_metric, State) ->
    {_, SyncProgress} = sync_progress(State),
    aec_metrics:try_update([ae,epoch,aecore,sync,progress], SyncProgress),

    %% Next try in 30 secs.
    erlang:send_after(30 * 1000, self(), update_sync_progress_metric),
    {noreply, State};
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
            NewChain = merge_chains(Chain#chain{ id = STId }, C2),
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
    #pool_item{ height = Height, hash = Hash, got = false } = lists:last(HashPool),
    ST#sync_task{ pool = HashPool, agreed = #chain_block{ height = Height, hash = Hash } };
handle_last_result(ST, {hash_pool, _HashPool}) ->
    ST;
handle_last_result(ST, {get_generation, Height, Hash, PeerId, {ok, Block}}) ->
    Pool = ST#sync_task.pool,
    NewItem = #pool_item{ height = Height, hash = Hash, got = {PeerId, Block} },
    Pool1 = lists:keyreplace(Height, #pool_item.height, Pool, NewItem),
    ST#sync_task{ pool = Pool1 };
handle_last_result(ST, {post_blocks, ok}) ->
    ST#sync_task{ adding = [] };
handle_last_result(ST, {post_blocks, {ok, NewTop}}) ->
    inform_workers(ST, {new_top, NewTop}),
    ST#sync_task{ adding = [] };
handle_last_result(ST, {post_blocks, {rejected, BlockFromPeerId, Height}}) ->
    mark_workers_as_suspect(ST#sync_task.workers),
    sync_task_post_error(ST#sync_task{suspect = true, workers = []}, BlockFromPeerId, Height);
handle_last_result(ST, {post_blocks, {error, BlockFromPeerId, Height}}) ->
    inform_workers(ST, {new_top, Height}),
    sync_task_post_error(ST, BlockFromPeerId, Height).

sync_task_post_error(ST, BlockFromPeerId, Height) ->
    #sync_task{ adding = Add, pending = Pends, pool = Pool
              , chain = Chain
              } = ST,
    %% Put back the blocks we did not manage to post, and schedule failing block
    %% for another retrieval.
    [#pool_item{ height = Height, hash = Hash } | PutBack] =
        lists:dropwhile(fun(#pool_item{ height = H }) -> H < Height end,
                        Add) ++ lists:append(Pends),
    NewPool = [#pool_item{ height = Height, hash = Hash, got = false } | PutBack] ++ Pool,
    ST1 = ST#sync_task{ adding = [], pending = [], pool = NewPool },

    ST1#sync_task{ chain = Chain#chain{ peers = Chain#chain.peers -- [BlockFromPeerId] }}.

mark_workers_as_suspect(Ws) ->
    lists:foreach(
      fun(#worker{ peer_id = P }) ->
              aec_peer_connection:disconnect(P),
              aec_peers:peer_suspect(P),
              aec_events:publish(chain_sync, {chain_sync_done, P})
      end, Ws).

inform_workers(#sync_task{ workers = Ws }, {new_top, Height}) ->
    [ aec_peer_connection:set_sync_height(P, Height) || #worker{ peer_id = P } <- Ws ].

split_pool(Pool) ->
    lists:splitwith(fun(#pool_item{ got = X }) -> X =/= false end, Pool).

get_next_work_item(State, STId, PeerId) ->
    case get_sync_task(STId, State) of
        {ok, ST = #sync_task{ suspect = Suspect, chain = #chain{ peers = PeerIds } }} ->
            case (not Suspect) andalso lists:member(PeerId, PeerIds) of
                true ->
                    {Reply, ST1} = get_next_work_item(ST),
                    {Reply, set_sync_task(STId, ST1, State)};
                false ->
                    {abort_work, State}
            end;
        {error, not_found} ->
            {abort_work, State}
    end.

get_next_work_item(ST = #sync_task{ suspect = true }) ->
    epoch_sync:info("Sync task flagged as suspect: ~1000p", [pp_sync_task(ST)]),
    {take_a_break, ST};
get_next_work_item(ST = #sync_task{ adding = [], pending = [ToAdd | NewPending] }) ->
    {{post_blocks, ToAdd}, ST#sync_task{ adding = ToAdd, pending = NewPending }};
get_next_work_item(ST = #sync_task{ agreed = undefined }) ->
    {{agree_on_height, ST#sync_task.chain}, ST};
get_next_work_item(ST = #sync_task{ pool = [], agreed = #chain_block{} }) ->
    #chain_block{ hash = LastHash, height = H } = ST#sync_task.agreed,
    Chain = ST#sync_task.chain,
    TargetHash = next_known_hash(Chain#chain.blocks, H + ?MAX_HEADERS_PER_CHUNK),
    {{fill_pool, LastHash, TargetHash}, ST};
get_next_work_item(ST = #sync_task{ pool = [#pool_item{ got = {_, _} } | _] }) ->
    #sync_task{ pool = Pool, adding = Add, pending = Pend } = ST,
    {ToBeAdded = [_|_], NewPool} = split_pool(Pool),
    case Add of
        [] ->
            ST1 = ST#sync_task{ pool = NewPool, adding = ToBeAdded },
            {{post_blocks, ToBeAdded}, ST1};
        _ when length(Pend) < 10 orelse NewPool /= [] ->
            ST1 = ST#sync_task{ pool = NewPool, pending = Pend ++ [ToBeAdded] },
            get_next_work_item(ST1);
        _ ->
            {take_a_break, ST}
    end;
get_next_work_item(ST = #sync_task{ pool = [#pool_item{ got = false } | _] }) ->
    Pool = ST#sync_task.pool,
    PickFrom = [ P || P = #pool_item{ got = false } <- Pool ],
    Random = rand:uniform(length(PickFrom)),
    #pool_item{ height = PickH
              , hash = PickHash
              , got = false} = lists:nth(Random, PickFrom),
    epoch_sync:debug("Get block at height ~p", [PickH]),
    {{get_generation, PickH, PickHash}, ST};
get_next_work_item(ST) ->
    epoch_sync:info("Nothing to do: ~1000p", [pp_sync_task(ST)]),
    {take_a_break, ST}.

maybe_end_sync_task(State, ST) ->
    case ST#sync_task.chain of
        #chain{ peers = [], blocks = [Target | _] } ->
            case ST#sync_task.suspect of
                false ->
                    epoch_sync:info("Removing/ending sync task ~p target was ~p",
                                    [ST#sync_task.id, pp_chain_block(Target)]),
                    State1 = delete_sync_task(ST, State),
                    maybe_update_top_target(State1);
                true ->
                    epoch_sync:info("Keeping sync task ~p around; target was ~p",
                                    [ST#sync_task.id, pp_chain_block(Target)]),
                    State1 = set_sync_task(ST, State),
                    maybe_update_top_target(State1)
            end;
        _ ->
            set_sync_task(ST, State)
    end.

maybe_new_top_target(#chain{ blocks = [B | _] }, State) ->
    ChainTopTarget = B#chain_block.height,
    case State#state.top_target < ChainTopTarget of
        true  -> update_top_target(ChainTopTarget, State);
        false -> State
    end.

maybe_update_top_target(State = #state{}) ->
    STs = valid_sync_tasks(State),
    #state{ top_target = TopTarget} = State,
    NewTop = lists:foldl(fun(#sync_task{ chain = Chain }, PrevMax) ->
                             case Chain#chain.blocks of
                                 [#chain_block{ height = H } | _] when H > PrevMax -> H;
                                 _ -> PrevMax
                             end
                         end, 0, STs),
    case TopTarget == NewTop of
        true  -> State;
        false -> update_top_target(NewTop, State)
    end.

valid_sync_tasks(#state{sync_tasks = STs}) ->
    [ST || #sync_task{suspect = false} = ST <- STs].

update_top_target(TopTarget, State) ->
    aec_tx_pool:new_sync_top_target(TopTarget),
    State#state{ top_target = TopTarget }.

do_update_sync_task(State, STId, Update) ->
    case get_sync_task(STId, State) of
        {ok, ST = #sync_task{ chain = Chain }} ->
            Chain1 =
                case Update of
                    {done, PeerId} ->
                        Chain#chain{ peers = Chain#chain.peers -- [PeerId] };
                    {error, PeerId} ->
                        aec_peer_connection:set_sync_height(PeerId, none),
                        Chain#chain{ peers = Chain#chain.peers -- [PeerId] }
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
    State1 = S#state{ sync_tasks = lists:keystore(STId, #sync_task.id, STs, ST) },
    check_is_syncing(State1).

delete_sync_task(#sync_task{ id = STId }, S) ->
    delete_sync_task(STId, S);
delete_sync_task(STId, S = #state{ sync_tasks = STs }) ->
    State1 = S#state{ sync_tasks = lists:keydelete(STId, #sync_task.id, STs) },
    check_is_syncing(State1).

check_is_syncing(State) ->
    case {is_syncing(State), State#state.is_syncing} of
        {Same, Same} ->
            State;
        {New, _} ->
            lager:debug("is_syncing changed to ~p", [New]),
            aec_events:publish(chain_sync, {is_syncing, New}),
            State#state{is_syncing = New}
    end.

add_chain_info(Chain = #chain{ id = CId }, S) ->
    case get_sync_task(CId, S) of
        {ok, ST = #sync_task{ chain = Chain2 }} ->
            ST1 = ST#sync_task{ chain = merge_chains(Chain, Chain2) },
            set_sync_task(ST1, S);
        {error, not_found} ->
            S
    end.

init_sync_task(Chain) ->
    #sync_task{ id = Chain#chain.id, chain = Chain }.

merge_chains(#chain{ id = CId, peers = Ps1, blocks = C1 },
             #chain{ id = CId, peers = Ps2, blocks = C2 }) ->
    %% We sort descending...
    Cmp = fun(#chain_block{ height = H1 }, #chain_block{ height = H2 }) -> H1 >= H2 end,
    #chain{ id = CId, peers = lists:usort(Ps1 ++ Ps2),
            blocks = lists:umerge(Cmp, C1, C2) }.

match_tasks(_Chain, [], []) ->
    no_match;
match_tasks(Chain, [], Acc) ->
    {N, #chain{ id = CId, peers = Peers }} = hd(lists:reverse(Acc)),
    {inconclusive, Chain, {get_header, CId, Peers, N}};
match_tasks(Chain1, [ST = #sync_task{ chain = Chain2 } | STs], Acc) ->
    case match_chains(Chain1#chain.blocks, Chain2#chain.blocks) of
        equal     -> {match, ST};
        different -> match_tasks(Chain1, STs, Acc);
        {fst, N}  -> match_tasks(Chain1, STs, [{N, Chain1} | Acc]);
        {snd, N}  -> match_tasks(Chain1, STs, [{N, Chain2} | Acc])
    end.

match_chains([#chain_block{ height = N1 } | C1],
             [#chain_block{ height = N2, hash = H } | _]) when N1 > N2 ->
    case find_hash_at_height(N2, C1) of
        {ok, H}   -> equal;
        {ok, _}   -> different;
        not_found -> {fst, N2}
    end;
match_chains([#chain_block{ height = N1, hash = H } | _], C2) ->
    case find_hash_at_height(N1, C2) of
        {ok, H}   -> equal;
        {ok, _}   -> different;
        not_found -> {snd, N1}
    end.

find_hash_at_height(N, [#chain_block{ height = N, hash = H } | _]) ->
    {ok, H};
find_hash_at_height(_, []) ->
    not_found;
find_hash_at_height(N, [#chain_block{ height = N1 } | _]) when N1 < N ->
    not_found;
find_hash_at_height(N, [_ | Cs]) ->
    find_hash_at_height(N, Cs).

do_handle_worker({new_worker, PeerId, Pid}, ST = #sync_task{ workers = Ws }) ->
    case lists:keyfind(PeerId, #worker.peer_id, Ws) of
        false -> ok;
        #worker{ pid = Old } ->
            epoch_sync:info("Peer ~p already has a worker (~p)", [ppp(PeerId), Old])
    end,
    erlang:link(Pid),
    epoch_sync:debug("New worker ~p for peer ~p", [Pid, ppp(PeerId)]),
    NewW = #worker{ peer_id = PeerId, pid = Pid },
    ST#sync_task{ workers = lists:keystore(PeerId, #worker.peer_id, Ws, NewW) };
do_handle_worker({change_worker, PeerId, OldPid, NewPid}, ST = #sync_task{ workers = Ws }) ->
    case lists:keyfind(PeerId, #worker.peer_id, Ws) of
        false ->
            epoch_sync:info("Missing worker ~p for peer ~p", [OldPid, ppp(PeerId)]);
        #worker{ pid = OldPid } ->
            ok;
        #worker{ pid = AnotherPid } ->
            epoch_sync:info("Wrong worker stored for peer ~p (~p)", [ppp(PeerId), AnotherPid])
    end,
    erlang:link(NewPid),
    %% The old worker will terminate right after the call to handle_worker so we can safely
    %% unlink it.
    erlang:unlink(OldPid),
    epoch_sync:debug("Update worker ~p (was ~p) for peer ~p", [NewPid, OldPid, ppp(PeerId)]),
    NewW = #worker{ peer_id = PeerId, pid = NewPid },
    ST#sync_task{ workers = lists:keystore(PeerId, #worker.peer_id, Ws, NewW) }.

do_terminate_worker(Pid, S = #state{ sync_tasks = STs }, Reason) ->
    case [ ST || ST <- STs, lists:keyfind(Pid, #worker.pid, ST#sync_task.workers) /= false ] of
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
    #worker{ peer_id = Peer } = lists:keyfind(Pid, #worker.pid, Ws),
    epoch_sync:debug("Terminating worker ~p for peer ~p", [Pid, ppp(Peer)]),
    {Peer, ST#sync_task{ workers = lists:keydelete(Pid, #worker.pid, Ws) }}.

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
enqueue(_Kind, _Data, []) ->
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
    case peer_get_header_by_hash(PeerId, RemoteHash) of
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
    Height = aec_headers:height(Header),
    case aec_headers:type(Header) of
        key ->
            {ok, Hash} = aec_headers:hash_header(Header),
            Block      = #chain_block{ hash = Hash, height = Height},
            PrevBlock  = #chain_block{ hash   = aec_headers:prev_key_hash(Header),
                                       height = Height - 1 },
            %% Unless we are at height 1 add previous key-block
            #chain{ id = ChainId, peers = Peers, blocks = [Block | [PrevBlock || Height > 1]] };
        micro ->
            Block = #chain_block{ hash   = aec_headers:prev_key_hash(Header),
                                  height = Height },
            #chain{ id = ChainId, peers = Peers, blocks = [Block] }
    end.

known_chain(Chain) ->
    identify_chain(known_chain(Chain, none)).

identify_chain({existing, _Chain, Task}) ->
    epoch_sync:debug("Already syncing chain ~p", [Task]),
    {ok, Task};
identify_chain({new, #chain{ blocks = [Target | _]}, Task}) ->
    epoch_sync:info("Starting new sync task ~p target is ~1000p", [Task, pp_chain_block(Target)]),
    {ok, Task};
identify_chain({inconclusive, Chain, {get_header, CId, Peers, N}}) ->
    %% We need another hash for this chain, make sure whoever we ask is
    %% still on this particular chain by including a known (at higher height) hash
    KnownHash = next_known_hash(Chain#chain.blocks, N),
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
    #chain_block{ hash = Hash } =
        case lists:takewhile(fun(#chain_block{ height = H }) -> H > N end, Cs) of
            []  -> hd(Cs);
            Cs1 -> lists:last(Cs1)
        end,
    Hash.

do_get_header_by_height([], _N, _TopHash) ->
    {error, header_not_found};
do_get_header_by_height([PeerId | PeerIds], N, TopHash) ->
    case peer_get_header_by_height(PeerId, N, TopHash) of
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
                            fun() -> do_work_on_sync_task(PeerId, Task) end, 5000);
        {agree_on_height, Chain} ->
            case agree_on_height(PeerId, Chain) of
                {ok, AHeight, AHash} ->
                    epoch_sync:debug("Agreed upon height (~p): ~p", [ppp(PeerId), AHeight]),
                    Agreement = {agreed_height, #chain_block{ height = AHeight, hash = AHash }},
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
            epoch_sync:info("~p aborting sync work against ~p", [self(), PeerId])
    end.

agree_on_height(PeerId, #chain{ blocks = [#chain_block{ hash = TopHash, height = TopHeight } | _] }) ->
    try
        LocalHeader = aec_chain:top_header(),
        LocalHeight = aec_headers:height(LocalHeader),
        MinHeight   = min(TopHeight, LocalHeight),
        RemoteHash =
            case TopHeight == MinHeight of
                true  -> TopHash;
                false -> get_header_by_height(PeerId, LocalHeight, TopHash)
            end,
        case aec_chain:hash_is_connected_to_genesis(RemoteHash) of
            true ->
                {ok, MinHeight, RemoteHash};
            false ->
                agree_on_height(PeerId, TopHash, MinHeight, 1)
        end
    catch throw:{get_header_by_height, {error, Reason}} ->
        {error, Reason}
    end.

%% Height is a height where we disagree, so ask for Height - Step
agree_on_height(PeerId, RemoteTop, Height, Step) ->
    NewHeight = max(aec_block_genesis:height(), Height - Step),
    RHash = get_header_by_height(PeerId, NewHeight, RemoteTop),
    case aec_chain:hash_is_connected_to_genesis(RHash) of
        true ->
            agree_on_height(PeerId, RemoteTop, NewHeight, Height, RHash);
        false ->
            agree_on_height(PeerId, RemoteTop, NewHeight, Step * 2)
    end.

%% We agree on Hash at MinH and disagree at MaxH
agree_on_height(_PeerId, _RemoteTop, MinH, MaxH, Hash) when MaxH == MinH + 1 ->
    {ok, MinH, Hash};
agree_on_height(PeerId, RemoteTop, MinH, MaxH, Hash) ->
    H = (MinH + MaxH) div 2,
    RHash = get_header_by_height(PeerId, H, RemoteTop),
    case aec_chain:hash_is_connected_to_genesis(RHash) of
        true ->
            agree_on_height(PeerId, RemoteTop, H, MaxH, RHash);
        false ->
            agree_on_height(PeerId, RemoteTop, MinH, H, Hash)
    end.

get_header_by_height(PeerId, Height, RemoteTop) ->
    case Height == aec_block_genesis:height() of
        true  -> aec_chain:genesis_hash(); %% Handshake ensure we agree on genesis
        false ->
            case peer_get_header_by_height(PeerId, Height, RemoteTop) of
                {ok, RemoteAtHeight} ->
                    {ok, RHash} = aec_headers:hash_header(RemoteAtHeight),
                    RHash;
                 {error, Reason} ->
                     epoch_sync:debug("Fetching header ~p from ~p failed: ~p",
                                      [Height, ppp(PeerId), Reason]),
                     throw({get_header_by_height, {error, Reason}})
            end
    end.

post_blocks([]) -> ok;
post_blocks([#pool_item{ height = StartHeight } | _] = Blocks) ->
    post_blocks(StartHeight, StartHeight, Blocks).

post_blocks(To, To, []) ->
    epoch_sync:info("Synced block  ~p", [To]),
    {ok, To};
post_blocks(From, To, []) ->
    epoch_sync:info("Synced blocks ~p - ~p", [From, To]),
    {ok, To};
post_blocks(From, _To, [#pool_item{ height = Height, got = {_PeerId, local} } | Blocks]) ->
    post_blocks(From, Height, Blocks);
post_blocks(From, To, [#pool_item{ height = Height, got = {PeerId, Block} } | Blocks]) ->
    case add_generation(Block) of
        ok ->
            post_blocks(From, Height, Blocks);
        {error, Reason} ->
            epoch_sync:info("Failed to add synced block ~p: ~p", [Height, Reason]),
            [ epoch_sync:info("Synced blocks ~p - ~p", [From, To - 1]) || To > From ],
            case Reason of
                {too_far_below_top, _, _} ->
                    {rejected, PeerId, Height};
                blocked_by_whitelist ->
                    {rejected, PeerId, Height};
                _ ->
                    {error, PeerId, Height}
            end
    end.

%% In order not to timeout the conductor, large generations are added in
%% smaller chuncks, one micro block at the time.
%% Each micro block has a fixed maximum gas, by limiting the number of micro
%% blocks we limit the total amount of work the conductor has to perform in
%% each synchronous call.
%% Map contains key dir, saying in which direction we sync
add_generation(#{dir := forward, key_block := _KB, micro_blocks := MBs}) ->
    add_blocks(MBs);
add_generation(#{dir := backward, key_block := KB, micro_blocks := MBs}) ->
    add_blocks(MBs ++ [KB]).

add_blocks([]) ->
    ok;
add_blocks([B | Bs]) ->
    try aec_conductor:add_synced_block(B) of
        ok -> add_blocks(Bs);
        Err -> Err
    catch _:_ ->
        lager:warning("Timeout adding_synced block: ~p", [B]),
        {error, timeout}
    end.

fill_pool(PeerId, StartHash, TargetHash, ST) ->
    case peer_get_n_successors(PeerId, StartHash, TargetHash, ?MAX_HEADERS_PER_CHUNK) of
        {ok, []} ->
            aec_peer_connection:set_sync_height(PeerId, none),
            do_get_generation(PeerId, StartHash),
            update_sync_task({done, PeerId}, ST),
            epoch_sync:info("Sync done (according to ~p)", [ppp(PeerId)]),
            aec_events:publish(chain_sync, {chain_sync_done, PeerId});
        {ok, Hashes = [{FirstHeight, FirstHash} | _]} when
              %% Guaranteed by deserialization.
              is_integer(FirstHeight), FirstHeight >= 0 ->
            case FirstHash =/= StartHash of
                true ->
                    case heights_are_consecutive(Hashes) of
                        {ok, _} ->
                            HashPool = [ #pool_item{ height = Height
                                                   , hash = Hash
                                                   , got = false
                                                   } || {Height, Hash} <- Hashes ],
                            do_work_on_sync_task(PeerId, ST, {hash_pool, HashPool});
                        {error, {LastGoodHeight, FirstBadHeight}} ->
                            epoch_sync:info(
                              "Abort sync with ~p (bad successor height ~p after ~p)",
                              [ppp(PeerId), FirstBadHeight, LastGoodHeight]),
                            update_sync_task({error, PeerId}, ST),
                            {error, sync_abort}
                    end;
                false ->
                    epoch_sync:info("Abort sync with ~p (bad first hash ~p)",
                                    [ppp(PeerId), FirstHash]),
                    update_sync_task({error, PeerId}, ST),
                    {error, sync_abort}
            end;
        {error, _} = Error ->
            epoch_sync:info("Abort sync with ~p (~p) ", [ppp(PeerId), Error]),
            update_sync_task({error, PeerId}, ST),
            {error, sync_abort}
    end.

heights_are_consecutive([{FirstHeight, _} | _] = Hashes) when
      is_integer(FirstHeight), FirstHeight >= 0 ->
    lists:foldl(fun ({H, _}, {ok, AccH}) when is_integer(H) ->
                        if H =:= (1 + AccH) -> {ok, H};
                           true -> {error, {AccH, H}}
                        end;
                    (_, {error, _} = Err) ->
                        Err
                end,
                {ok, FirstHeight},
                tl(Hashes)).

do_get_generation(PeerId, LastHash) ->
    case peer_get_generation(PeerId, LastHash, forward) of
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
    case peer_get_generation(PeerId, Hash, backward) of
        {ok, KeyBlock, MicroBlocks, backward} ->
            %% Types of blocks (key vs. macro) guaranteed by deserialization.
            case header_hash(KeyBlock) =:= Hash of
                true ->
                    case gen_is_consecutive(backward, KeyBlock, MicroBlocks) of
                        {error, {B1, B2}} ->
                            epoch_sync:debug(
                              "bad generation fetched from ~p (~p); ~p, ~p",
                              [ppp(PeerId), pp(Hash), pp(B1), pp(B2)]),
                            {error, non_consecutive_generation};
                        ok ->
                            epoch_sync:debug(
                              "block fetched from ~p (~p); ~p",
                              [ppp(PeerId), pp(Hash), pp(KeyBlock)]),
                            {ok, #{ key_block    => KeyBlock,
                                    micro_blocks => MicroBlocks,
                                    dir          => backward }}
                    end;
                false ->
                    epoch_sync:debug(
                      "bad hash of key block fetched from ~p (~p); ~p",
                      [ppp(PeerId), pp(Hash), pp(KeyBlock)]),
                    {error, hash_mismatch}
            end;
        {error, _} = Error ->
            epoch_sync:debug("failed to fetch block from ~p; Hash = ~p; Error = ~p",
                             [ppp(PeerId), pp(Hash), Error]),
            Error
    end.

gen_is_consecutive(backward, _KB, []) ->
    ok;
gen_is_consecutive(backward, KB, [MB]) ->
    case
        (aec_blocks:height(KB) =:= (1 + aec_blocks:height(MB)))
        andalso (aec_blocks:prev_hash(KB) =:= header_hash(MB))
    of
        false -> {error, {MB, KB}};
        true -> ok
    end;
gen_is_consecutive(backward, KB, MBs = [MB1, MB2 | _]) ->
    case
        ((aec_blocks:height(MB2) =:= aec_blocks:height(MB1))
         andalso (aec_blocks:prev_hash(MB2) =:= header_hash(MB1)))
    of
        false -> {error, {MB1, MB2}};
        true -> gen_is_consecutive(backward, KB, tl(MBs))
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

has_generation(KeyBlockHash) ->
    case aec_chain:get_header(KeyBlockHash) of
        error ->
            false;
        {ok, Header} ->
            aec_headers:assert_key_header(Header),
            true
    end.

header_hash(Block) ->
    Header = aec_blocks:to_header(Block),
    {ok, HeaderHash} = aec_headers:hash_header(Header),
    HeaderHash.

default_peers() ->
    case aec_governance:get_network_id() of
        <<"ae_mainnet">>    -> aeu_env:get_env(aecore, mainnet_peers, []);
        <<"ae_uat">>        -> aeu_env:get_env(aecore, testnet_peers, []);
        _                   -> aeu_env:get_env(aecore, peers, [])
    end.

parse_peers(Ps) ->
    lists:append([ parse_peer(P) || P <- Ps ]).

parse_peer(P) ->
    case aec_peers:parse_peer_address(P) of
        {ok, PInfo} ->
            [PInfo];
        {error, Reason} ->
            lager:warning("Not adding peer ~p because of error ~p",
                          [P, Reason]),
            []
    end.

peer_in_sync(#state{sync_tasks = STs}, PeerId) ->
    L = lists:flatmap(fun(ST) -> ST#sync_task.chain#chain.peers end, STs),
    lists:member(PeerId, L).

get_worker_for_peer(#state{ sync_tasks = STs }, PeerId) ->
    case [ Pid || #sync_task{ workers = Ws } <- STs,
                  #worker{ peer_id = PeerId0, pid = Pid } <- Ws,
                  PeerId0 == PeerId
         ] of
        [] -> false;
        [Pid | _] -> {ok, Pid}
    end.

max_gossip() ->
    aeu_env:user_config_or_env([<<"sync">>, <<"max_gossip">>],
                               aecore, sync_max_gossip,
                               ?DEFAULT_MAX_GOSSIP).

is_syncing(#state{sync_tasks = SyncTasks}) ->
    [1 || #sync_task{suspect = false} <- SyncTasks] =/= [].

-spec sync_progress(#state{}) -> {boolean(), float()}.
sync_progress(#state{sync_tasks = SyncTasks} = State) ->
    case is_syncing(State) of
        false -> {false, 100.0};
        true ->
            TargetHeight =
                lists:foldl(
                  fun(#sync_task{suspect = true}, Acc) ->
                          Acc;
                     (SyncTask, MaxHeight) ->
                          #chain{blocks = Chain} = SyncTask#sync_task.chain,
                          [#chain_block{height = Height} | _] = Chain,
                          max(Height, MaxHeight)
                  end, 0, SyncTasks),
            TopHeight = aec_headers:height(aec_chain:top_header()),
            SyncProgress0 = round(10000000 * TopHeight / TargetHeight) / 100000,
            %% It is possible to have TopHeight already higher than Height in sync task,
            %% e.g. when a block was mined and the sync task was not yet removed.
            %% Then HTTP handler will crash, since sync_progress is defined in Swagger
            %% to be in range from 0.0 to 100.0.
            SyncProgress =
                case SyncProgress0 > 100.0 of
                    true -> 99.9;
                    false -> SyncProgress0
                end,
            {true, SyncProgress}
    end.

peer_get_header_by_hash(PeerId, RemoteHash) ->
    validate_header(aec_peer_connection:get_header_by_hash(PeerId, RemoteHash), PeerId).

peer_get_header_by_height(PeerId, Height, TopHash) ->
    validate_header(
      aec_peer_connection:get_header_by_height(PeerId, Height, TopHash), PeerId).

peer_get_n_successors(PeerId, StartHash, TargetHash, N) ->
    case aec_peer_connection:get_n_successors(PeerId, StartHash, TargetHash, N) of
        {ok, Hashes} = Ok ->
            case lists:any(fun fails_whitelist/1, Hashes) of
                true ->
                    lager:debug("Successor batch from ~p failed whitelist", [PeerId]),
                    {error, blocked_by_whitelist};
                false ->
                    Ok
            end;
        {error, _} = Error ->
            Error
    end.

peer_get_generation(PeerId, LastHash, Dir) when Dir == backward; Dir == forward ->
    case aec_peer_connection:get_generation(PeerId, LastHash, Dir) of
        {ok, KeyBlock, _MicroBlocks, Dir} = Ok ->
            try validate_block(KeyBlock) of
                ok ->
                    Ok;
                {error, _} = Error1 ->
                    lager:debug("KeyBlock fails validation: ~p", [Error1]),
                    Error1
            catch
                error:Crash:ST ->
                    lager:debug("CAUGHT KeyBlock validation error:~p/~p", [Crash, ST]),
                    {error, validation_exception}
            end;
        {error, _} = Error ->
            Error
    end.

fails_whitelist({Height, Hash}) ->
    Consensus = aec_consensus:get_consensus_module_at_height(Height),
    case Consensus:dirty_validate_key_hash_at_height(Height, Hash) of
        {error, blocked_by_whitelist} -> true;
        _ ->
            false
    end.

validate_header({ok, Header}, PeerId) ->
    try validate_header_(Header) of
        ok ->
            {ok, Header};
        {error, _} = Error ->
            lager:debug("Header from ~p failed validation: ~p", [PeerId, Header]),
            Error
    catch
        error:Crash:ST ->
            lager:debug("CAUGHT header validation error:~p/~p", [Crash, ST]),
            {error, validation_exception}
    end;
validate_header({error, _} = Error, PeerId) ->
    lager:debug("Error from ~p: ~p", [PeerId, Error]),
    Error.

validate_header_(Header) ->
    case aec_headers:height(Header) of
        0 ->
            %% the validation function won't work on genesis
            ok;
        H when H > 0 ->
            aec_validation:validate_header(Header)
    end.

validate_block(Block) ->
    Height = aec_blocks:height(Block),
    case Height of
        0 ->
            %% the validation function won't work on genesis
            ok;
        H when H > 0 ->
            Protocol = aec_hard_forks:protocol_effective_at_height(Height),
            aec_validation:validate_block(Block, Protocol)
    end.

pp_chain_block(#chain_block{hash = Hash, height = Height}) ->
    #{ hash => Hash
     , height => Height
     }.

pp_chain(C = #chain{}) ->
    #{ chain_id => C#chain.id
     , peers => C#chain.peers
     , chain => lists:map(fun pp_chain_block/1, C#chain.blocks)
     }.

pp_pool_item(X = #pool_item{}) ->
    #{ height => X#pool_item.height
     , hash => X#pool_item.hash
     , got => X#pool_item.got
     }.

pp_worker(W = #worker{}) ->
    #{ peer_id => W#worker.peer_id
     , pid => W#worker.pid
     }.

pp_sync_task(ST = #sync_task{}) ->
    PPPoolF = fun(P) -> lists:map(fun pp_pool_item/1, P) end,
    PPAgreedF = fun (undefined) -> undefined
                  ; (#chain_block{} = B) -> pp_chain_block(B)
                end,
    lists:foldl(fun({I, F}, Acc) -> setelement(I, Acc, F(element(I, Acc))) end,
                ST,
                [ {#sync_task.chain, fun pp_chain/1}
                , {#sync_task.pool, PPPoolF}
                , {#sync_task.agreed, PPAgreedF}
                , {#sync_task.adding, PPPoolF}
                , {#sync_task.pending, fun(X) -> lists:map(PPPoolF, X) end}
                , {#sync_task.workers, fun pp_worker/1}
                ]).

process_infos(Infos) ->
    {Responded, Failed0} =
        lists:foldl(
            fun({error, Err}, {RAccum, FAccum}) ->
                {RAccum, [Err | FAccum]};
               ({ok, #{} = NodeInfo }, {RAccum, FAccum}) ->
                {[NodeInfo | RAccum], FAccum}
            end,
            {[], []},
            Infos),
      Failed =
          lists:foldl(
              fun(Err, Accum) ->
                  maps:update_with(Err, fun(X) -> X + 1 end, 1, Accum)
              end,
              #{},
              Failed0),
      NoneIfEmpty =
          fun(M) ->
              case map_size(M) =:= 0 of
                  true -> none;
                  false -> M 
              end
          end,
      Aggr =
          fun(Key) ->
              Res =
                  lists:foldl(
                      fun(#{Key := NodeVsn}, Accum) ->
                          maps:update_with(NodeVsn, fun(X) -> X + 1 end, 1, Accum)
                      end,
                      #{},
                      Responded),
              NoneIfEmpty(Res)
          end,
      Peers =
          lists:map(
              fun(#{ peers := Peers }) ->
                  Peers
              end,
              Responded),
      #{ versions       => Aggr(node_version)
       , revisions      => Aggr(revision)
       , vendors        => Aggr(vendor)
       , os             => Aggr(os)
       , failed         => NoneIfEmpty(Failed)
       , peers          => Peers }.

collect_infos(Timeout) ->
    ConnectedPeers = aec_peers:connected_peers(),
    TimerOffset = 500, %% time overhead for this process' timeout
    Fun =
        fun(PeerInfo) ->
            PeerId = aec_peer:id(PeerInfo),
            %% this timeouts in Timeout milliseconds but there is a second
            %% outher timeout in the gen_server:call to the connection itself
            %% so we call the aec_peer_connection:get_node_info/2 with 
            %% Timeout - 2000 - TimerOffset
            aec_peer_connection:get_node_info(PeerId, Timeout - 2000 -
                                              TimerOffset)
        end,
    pmap(Fun, ConnectedPeers, Timeout + 2000 + TimerOffset).

pmap(Fun, L, Timeout) ->
    Workers =
        lists:map(
            fun(E) ->
                spawn_monitor(
                    fun() ->
                        {WorkerPid, WorkerMRef} =
                            spawn_monitor(
                                fun() ->
                                    Res = Fun(E),
                                    exit({ok, Res}) 
                                end),
                        Result =
                            receive
                                {'DOWN', WorkerMRef, process, WorkerPid, Res} ->
                                    case Res of
                                        {ok, R} -> {ok, R};
                                        _       -> {error, failed}
                                    end
                            after Timeout -> {error, request_timeout}
                            end,
                        exit(Result)
                    end)
            end,
            L),
    pmap_gather(Workers, []).

pmap_gather([], Acc) ->
    Acc;
pmap_gather([{Pid, MRef} | Pids], Acc) ->
    receive
        {'DOWN', MRef, process, Pid, Res} ->
            case Res of
                {ok, GoodRes} -> pmap_gather(Pids, [GoodRes | Acc]);
                {error, _} = Err -> pmap_gather(Pids, [Err | Acc])
            end
    end.

