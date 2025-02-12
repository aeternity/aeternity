%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Main conductor of the mining
%%%
%%% The aec_conductor is the main hub for adding blocks to the local chain,
%%% both when produced locally, received via sync, or posted to the HTTP
%%% API. It is event driven, running tasks in background processes to
%%% ensure responsiveness.
%%%
%%% There are three different modes:
%%% * local_pow - a PoW miner may be run locally by the node. (Block
%%%   production/mining does not have to be started, in which case only
%%%   other nodes generate new blocks.)
%%% * stratum - a Stratum implementation: PoW miners are being fed hashes to
%%%   mine on top of
%%% * pos - a proof of stake consensus with no mining involved
%%%
%%% The details of the consensus strategy are parameterized into a separate
%%% callback module such as `aec_consensus_bitcoin_ng`. The strategy is
%%% encoded on the chain itself so it is known to all validators, and can
%%% change over time. `get_active_consensus_module/0` returns the currently
%%% used module. The module has callbacks `start/2` and `stop/0` which are
%%% called when it goes in and out of use, and `client_request/1` which may
%%% be used if the strategy uses a server process; see
%%% `consensus_request/1`.
%%%
%%% The block production has two states of operation: 'running' and
%%% 'stopped'. Passing the option {autostart, bool()} to the initialization
%%% controls which mode to start in.
%%%
%%% - In the running mode, key block candidates are generated (key blocks
%%%   in Aeternity only containin metadata, no transactions) and then
%%%   sealed by a separate worker. In the context of PoW, sealing requires
%%%   producing a solution to the puzzle in order to become leader. Under
%%%   PoS, the block must be signed by the correct leader. When sealed
%%%   successfully, the new block is published and added to the chain if
%%%   the state of the chain allows that.
%%%
%%% - In the stopped mode only blocks arriving from other miners are added
%%%   to the chain.
%%%
%%% The block production can be controlled by the API functions start_mining/0
%%% and stop_mining/0. The stop_mining is preemptive (i.e., all workers
%%% involved in mining are killed).
%%%
%%% The aec_conductor operates by delegating all heavy operations to worker
%%% processes (see dispatch_worker/3) which report back via a
%%% `worker_reply' message (see handle_info/2). The control flow is
%%% directed by events:
%%%
%%% - The server init/1 function last of all sends the new server process
%%%   an initial event `init_continue', handled in `handle_info/2'. This
%%%   will then start block production (if configured to do so).
%%%
%%% - If keys are not yet present, block production will first spawn a
%%%   wait-for-keys worker, and will retry production when keys arrive.
%%%
%%% - Key block production first requires a *key block candidate* to be
%%%   created, keeping track of which top hash it is trying to build on.
%%%   If the server state has no candidate yet for the current top of the
%%%   chain, a worker will be spawned to create a new candidate, resuming
%%%   block production when ready. The details of key block candidate
%%%   creation are defined by the consensus module. Note that while the
%%%   candidate is being created, new blocks may arrive asynchronously,
%%%   changing the top hash again.
%%%
%%% - Whenever a new key block is added, any current workers that depend on
%%%   the previous key block (i.e. the previous generation) will be
%%%   aborted, e.g. mining and key block candidate workers.
%%%
%%% - Under PoW, the puzzle to be solved by the mining in order to be
%%%   allowed to build a new key block depends on the top hash on
%%%   which the key block will be added. For every new top hash,
%%%   i.e., each added (micro-)block, a corresponding key block candidate
%%%   is created and mining for that block is started; the mining
%%%   worker process runs under a timeout and will abort if it takes
%%%   too long. There can thus be several candidate blocks, each
%%%   based on a different top hash. If a solution is found, insertion
%%%   will be attempted just like if the block arrived from elsewhere.
%%%
%%% - Under PoS, a candidate is only created when the local node decides it
%%%   wants to produce a key block; for hyperchains, this depends on
%%%   computed time slots, and the candidate worker may sleep until the
%%%   right time. In this case the actual block production does not do any
%%%   mining; it just signs the new key block. If candidate creation
%%%   reports failure (e.g. due to not being leader), a fresh attempt will
%%%   be made.
%%%
%%% - The microblock server in `aec_block_generator' collects transactions
%%%   by subscribing to transaction events, and builds a candidate
%%%   microblock. When a microblock candidate is ready, this gets published
%%%   as a `candidate_block' event, telling the conductor it can fetch the
%%%   block (see `handle_info/2'). If this succeeds, the conductor will (if
%%%   it is still leader) sign the microblock and add it to the chain.
%%%
%%% - The mining configuration may specify that the system can run a number
%%%   of concurrent miner instances, for example using CUDA with multiple
%%%   GPU cards. The conductor holds a list of instances and assigns mining
%%%   workers to them, keeping track of whether they are busy or available.
%%%   By default a single instance is used. Exactly what an instance means
%%%   and does is defined by the consensus module; see the
%%%   `generate_key_header_seal/5' callback.
%%%
%%% The principle is to optimistically try to start producing, and fall
%%% back to an earlier stage if the preconditions are not met. The next
%%% stage should be triggered in the worker reply for each stage based on
%%% the postconditions of that stage. E.g. If the start_mining stage is
%%% attempted without having a block candidate, it should fall back to wait
%%% for a block candidate. When the worker returns it should either start
%%% production of a new block or retry based on the return of the effort.
%%%
%%% @end
%%% --------------------------------------------------------------------

-module(aec_conductor).

-behaviour(gen_server).

%% Mining API
-export([ get_miner_instances/0
        , get_mining_state/0
        , get_mining_workers/0
        , start_mining/0
        , start_mining/1
        , stop_mining/0
        , is_leader/0
        , get_beneficiary/0
        , get_next_beneficiary/0
        , note_rollback/1
        ]).

%% Chain API
-export([ add_synced_block/1
        , get_key_block_candidate/0
        , post_block/1
        , add_signed_block/1
        ]).

%% Consensus API
-export([ get_active_consensus_module/0
        , consensus_request/1
        , throw_error/1
        ]).

-ifdef(TEST).
-export([reinit_chain/0
        ]).
-endif.

%% Stratum mining pool API
-export([stratum_reply/2]).

%% gen_server API
-export([ start_link/0
        , start_link/1
        , stop/0 %% For testing
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

-export_type([options/0]).

-include("blocks.hrl").
-include("aec_conductor.hrl").

-define(SERVER, ?MODULE).

-define(DEFAULT_MINING_ATTEMPT_TIMEOUT, 60 * 60 * 1000). %% milliseconds

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% Gen server API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

stop() ->
    gen_server:stop(?SERVER).

%%%===================================================================
%%% Mining API

get_miner_instances() ->
    gen_server:call(?SERVER, get_instances).

-spec start_mining() -> 'ok'.
start_mining() ->
    start_mining(#{}).

-spec start_mining(mining_opts()) -> 'ok'.
start_mining(Opts) when is_map(Opts) ->
    gen_server:call(?SERVER, {start_block_production, Opts}).

-spec stop_mining() -> 'ok'.
stop_mining() ->
    gen_server:call(?SERVER, stop_block_production).

-spec get_mining_state() -> block_producing_state().
get_mining_state() ->
    gen_server:call(?SERVER, get_block_producing_state).

-spec get_mining_workers() -> [pid()].
get_mining_workers() ->
    gen_server:call(?SERVER, get_mining_workers).

-spec is_leader() -> boolean().
is_leader() ->
    gen_server:call(?SERVER, is_leader).

%%%===================================================================
%%% Chain API

%% Posting a block, typically via the HTTP API
-spec post_block(aec_blocks:block()) -> 'ok' | {'error', any()}.
post_block(Block) ->
    add_block(Block, post_block).

%% Adding a block received via synchronization with other nodes
-spec add_synced_block(aec_blocks:block()) -> 'ok' | {'error', any()}.
add_synced_block(Block) ->
    add_block(Block, add_synced_block).

%% Adding a signed microblock
-spec add_signed_block(aec_blocks:block()) -> 'ok' | {'error', any()}.
add_signed_block(Block) ->
    add_block(Block, add_signed_block).

add_block(Block, Origin) ->
    Height = aec_blocks:height(Block),
    Protocol = aec_hard_forks:protocol_effective_at_height(Height),
    case aec_validation:validate_block(Block, Protocol) of
        ok ->
            gen_server:call(?SERVER, {Origin, Block}, 30000);
        {error, {consensus, Reason}} ->
            epoch_mining:info("Consensus rejected block: ~p", [Reason]),
            {error, Reason};
        {error, {header, Reason}} ->
            epoch_mining:info("Header failed validation: ~p", [Reason]),
            {error, Reason};
        {error, {block, Reason}} ->
            epoch_mining:info("Block failed validation: ~p", [Reason]),
            {error, Reason}
    end.

%% Fetching the current key block candidate for the top of the chain
-spec get_key_block_candidate() -> {'ok', aec_blocks:block()} | {'error', atom()}.
get_key_block_candidate() ->
    gen_server:call(?SERVER, get_key_block_candidate).

-ifdef(TEST).
-spec reinit_chain() -> aec_headers:header().
reinit_chain() ->
    in_maintenance_mode(
      fun() ->
              ok = gen_server:call(?SERVER, reinit_chain),
              ok = supervisor:terminate_child(aecore_sup, aec_conductor_sup),
              {ok,_} = supervisor:restart_child(aecore_sup, aec_conductor_sup),
              ok
      end).
-endif.

%%%===================================================================
%%% Consensus API
get_active_consensus_module() ->
    gen_server:call(?SERVER, get_active_consensus_module).

%% Shortcut for performing a request to the currently active consensus module
%% (Note that not all consensus modules support requests)
consensus_request(Request) ->
    %% When changing the active consensus module or the configuration
    %% The conductor ensures that the given "consensus" was fully started
    try
        M = get_active_consensus_module(),
        M:client_request(Request)
    catch
        Error:Reason:Stack ->
            lager:debug("consensus_request(~p) Failed: ~p ~p ~p\n", [Request, Error, Reason, Stack]),
            error(Reason)
    end.

%%%===================================================================
%%% Stratum mining pool API

-spec stratum_reply({aec_consensus:key_nonce(), aec_consensus:key_seal()}, candidate_hash()) -> term().
stratum_reply({Nonce, Evd}, ForSealing) ->
    ?SERVER ! {stratum_reply, {{continue_mining, {ok, {Nonce, Evd}}}, ForSealing}}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Options) ->
    lager:debug("Options = ~p", [Options]),
    process_flag(trap_exit, true),
    ok     = init_chain_state(),
    IsProducingBlocks =
        case get_option(autostart, Options) of
            undefined   -> false;
            {ok, R}  -> R
        end,
    State1 = acquire_top_and_consensus(IsProducingBlocks),
    State2 = set_option(autostart, Options, State1),
    State3 = set_option(strictly_follow_top, Options, State2),
    State4 = set_mode(State3),
    State5 = init_instances(State4),

    aec_metrics:try_update([ae,epoch,aecore,chain,height],
                           aec_blocks:height(aec_chain:top_block())),
    epoch_mining:info("Miner process initialized ~p", [State5]),
    %% subscribe to new microblock candidates
    aec_events:subscribe(candidate_block),
    %% NOTE: The init continues at handle_info(init_continue, State).
    self() ! init_continue,
    {ok, State5}.

acquire_top_and_consensus(BlockProducing) ->
    TopBlockHash0 = aec_chain:top_block_hash(),
    {ok, TopHeader0} = aec_chain:get_header(TopBlockHash0),
    ConsensusModule = aec_headers:consensus_module(TopHeader0),
    ConsensusConfig = aec_consensus:get_consensus_config_at_height(aec_headers:height(TopHeader0)),

    %% Might mutate the DB in some cases
    ConsensusModule:start(ConsensusConfig, #{block_production => BlockProducing}), %% Might do nothing or it might spawn a genserver :P

    Consensus = #consensus{ micro_block_cycle = aec_governance:micro_block_cycle()
                          , leader = false
                          , consensus_module = ConsensusModule },

    {TopBlockHash, TopKeyBlockHash, TopHeight} =
        aec_db:ensure_transaction(
          fun() ->
                  {aec_chain:top_block_hash(),
                   aec_chain:top_key_block_hash(),
                   aec_chain:top_height()}
          end),
    #state{ top_block_hash     = TopBlockHash,
	    top_key_block_hash = TopKeyBlockHash,
	    top_height         = TopHeight,
	    consensus          = Consensus}.

init_chain_state() ->
    case aec_chain:genesis_hash() of
        undefined ->
            {GB, _GBState} = aec_block_genesis:genesis_block_with_state(),
            {ok, _} = aec_chain_state:insert_block(GB),
            ok;
        Hash when is_binary(Hash) ->
            ok
    end.

-ifdef(TEST).
reinit_chain_state() ->
    %% NOTE: ONLY FOR TEST
    %% Because we are using dirty reads to optimise the DB in production
    %% let's clear the db in a separate db tx
    %% this is ok as anyway this is only a test endpoint used in one place:
    %% apps/aehttp/test/aehttp_integration_SUITE.erl
    in_maintenance_mode(fun reinit_chain_state_/0).

reinit_chain_state_() ->
    ok = supervisor:terminate_child(aec_worker_sup, aec_tx_pool),
    ok = supervisor:terminate_child(aec_worker_sup, aec_tx_pool_gc),
    aec_db:ensure_transaction(fun aec_db:clear_db/0),
    aec_db:ensure_transaction(fun init_chain_state/0),
    {ok, _} = supervisor:restart_child(aec_worker_sup, aec_tx_pool_gc),
    {ok, _} = supervisor:restart_child(aec_worker_sup, aec_tx_pool),
    ok.

in_maintenance_mode(F) when is_function(F,0) ->
    case app_ctrl:get_mode() of
        normal ->
            app_ctrl:set_and_await_mode(maintenance, 10000),
            Res = F(),
            app_ctrl:set_and_await_mode(normal, 5000),
            Res;
        _ ->
            F()
    end.

reinit_chain_impl(State1 = #state{ consensus = #consensus{consensus_module = ActiveConsensus} = Cons }) ->
    lager:info("Reinitializing chain"),
    %% NOTE: ONLY FOR TEST
    ActiveConsensus:stop(),
    aec_consensus:set_consensus(), %% It's time to commit env changes
    aec_consensus:set_genesis_hash(),
    ok = reinit_chain_state(),
    TopBlockHash = aec_chain:top_block_hash(),
    TopKeyBlockHash = aec_chain:top_key_block_hash(),
    {ok, TopHeader} = aec_chain:get_header(TopBlockHash),
    TopHeight = aec_headers:height(TopHeader),
    ConsensusModule = aec_headers:consensus_module(TopHeader),
    ConsensusConfig = aec_consensus:get_consensus_config_at_height(aec_headers:height(TopHeader)),
    BlockProducing = is_mining(State1),
    ConsensusModule:start(ConsensusConfig, #{block_production => BlockProducing}), %% Might do nothing or it might spawn a genserver :P
    State2 = State1#state{top_block_hash = TopBlockHash,
                          top_key_block_hash = TopKeyBlockHash,
                          top_height = TopHeight},
    Cons1 = Cons#consensus{consensus_module = ConsensusModule},
    epoch_mining:info("Mining stopped"),
    State3 = kill_all_workers(State2),
    %% Not sure if this code is actually needed, since the conductor will be restarted
    %% anyway, but we'll keep it around for now.
    State =
        case State2#state.block_producing_state of
            stopped  ->
                State3#state{consensus = Cons1};
            running ->
                epoch_mining:info("Mining started" ++ print_opts(State3)),
                start_block_production_(State3#state{block_producing_state = running,
                                           micro_block_candidate = undefined,
                                           key_block_candidates = undefined,
                                           consensus = Cons1#consensus{leader = false}})
        end,
    lager:info("Reinitialized chain"),
    {reply, ok, State}.
-else.
reinit_chain_impl(State) ->
    epoch_mining:error("Received unknown request: ~p", [reinit_chain]),
    Reply = ok,
    {reply, Reply, State}.
-endif.

handle_call({add_synced_block, Block},_From, State) ->
    {Reply, State1} = handle_synced_block(Block, State),
    {reply, Reply, State1};
handle_call({add_signed_block, Block},_From, State) ->
    {Reply, State1} = handle_signed_block(Block, State),
    {reply, Reply, State1};
handle_call(get_key_block_candidate,_From, State) ->
    %% if there is no pending key block candidate or if it is not up to date,
    %% then create a new candidate, otherwise return the current one
    {Res, State1} =
        case State#state.pending_key_block of
            undefined ->
                get_pending_key_block(aec_chain:top_block_hash(), State);
            Block ->
                case {aec_chain:top_block_hash(), aec_blocks:prev_hash(State#state.pending_key_block)} of
                    {OldHash, OldHash} ->
                        {{ok, Block}, State};
                    {NewHash, _OldHash} ->
                        get_pending_key_block(NewHash, State)
                end
        end,
    {reply, Res, State1};
handle_call(get_instances, _From, State) ->
    {reply, State#state.instances, State};
handle_call(get_active_consensus_module, _From, State) ->
    {reply, (State#state.consensus)#consensus.consensus_module, State};
handle_call({post_block, Block},_From, State) ->
    {Reply, State1} = handle_post_block(Block, State),
    {reply, Reply, State1};
handle_call(stop_block_production,_From, State = #state{ consensus = Cons }) ->
    epoch_mining:info("Mining stopped"),
    aec_block_generator:stop_generation(),
    aec_events:publish(stop_mining, []),
    State1 = kill_all_workers(State),
    State2 = State1#state{block_producing_state = 'stopped',
                          consensus = Cons#consensus{leader = false},
                          key_block_candidates  = undefined},
    {reply, ok, create_key_block_candidate(State2)};
handle_call({start_block_production, _Opts},_From, #state{block_producing_state = 'running'} = State) ->
    epoch_mining:info("Mining running" ++ print_opts(State)),
    {reply, ok, State};
handle_call({start_block_production, _Opts},_From, #state{mode = pos, has_beneficiary = false} = State) ->
    epoch_mining:info("Cannot start block production - node is not a block producer"),
    {reply, {error, node_not_block_producer}, State};
handle_call({start_block_production, _Opts},_From, #state{has_beneficiary = false} = State) ->
    epoch_mining:error("Cannot start mining - beneficiary not configured"),
    {reply, {error, beneficiary_not_configured}, State};
handle_call({start_block_production, Opts},_From, #state{consensus = Cons} = State) ->
    epoch_mining:info("Mining started" ++ print_opts(State)),
    State1 = start_block_production_(State#state{block_producing_state = 'running', consensus = Cons#consensus{leader = false},
                                      mining_opts = Opts}),
    {reply, ok, State1};
handle_call(get_block_producing_state,_From, State) ->
    {reply, State#state.block_producing_state, State};
handle_call(get_mining_workers, _From, State) ->
    {reply, worker_pids_by_tag(mining, State), State};
handle_call(is_leader, _From, State = #state{ consensus = Cons }) ->
    {reply, Cons#consensus.leader, State};
handle_call(reinit_chain, _From, State) ->
    reinit_chain_impl(State);
handle_call({note_rollback, Info}, _From, State) ->
    #state{ top_block_hash = TBHash
	  , top_key_block_hash = TopKeyBlockHash
	  , top_height = TopHeight
	  , consensus = Consensus } = acquire_top_and_consensus(is_mining(State)),
    State1 = State#state{ top_block_hash = TBHash
			, top_key_block_hash = TopKeyBlockHash
			, top_height = TopHeight
			, consensus = Consensus },
    aec_tx_pool:note_rollback(Info),
    {reply, ok, State1};
handle_call(Request, _From, State) ->
    epoch_mining:error("Received unknown request: ~p", [Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Other, State) ->
    epoch_mining:error("Received unknown cast: ~p", [Other]),
    {noreply, State}.

handle_info({gproc_ps_event, candidate_block, _}, State = #state{consensus = #consensus{leader = false}}) ->
    %% ignore new microblock candidates if we are not a leader any more.
    {noreply, State};
handle_info({gproc_ps_event, candidate_block, #{info := new_candidate}}, State) ->
    %% a new microblock candidate is ready
    case try_fetch_and_make_candidate() of
        {ok, Candidate} ->
            State1 = State#state{ micro_block_candidate = Candidate },
            {noreply, start_micro_signing(State1)};
        {error, no_candidate} ->
            {noreply, State#state{ micro_block_candidate = undefined }}
    end;
handle_info(init_continue, State) ->
    %% This is triggered by init/1 at the time of creating the gen_server
    %% and should thus normally be the first thing to be executed
    {noreply, start_block_production_(State)};
handle_info({worker_reply, Pid, Res}, State) ->
    State1 = handle_worker_reply(Pid, Res, State),
    {noreply, State1};
handle_info({stratum_reply, Res}, State) ->
    State1 = handle_stratum_reply(Res, State),
    {noreply, State1};
handle_info({'DOWN', Ref, process, Pid, Why}, State) when Why =/= normal->
    State1 = handle_monitor_message(Ref, Pid, Why, State),
    {noreply, State1};
handle_info(Other, State) ->
    %% TODO: Handle monitoring messages
    epoch_mining:error("Received unknown info message: ~p", [Other]),
    {noreply, State}.

terminate(Reason, State) ->
    lager:debug("Reason = ~p", [Reason]),
    kill_all_workers(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

print_opts(#state{ mining_opts = Opts }) ->
    case map_size(Opts) of
        0 ->
            "";
        _ ->
            lists:flatten(io_lib:fwrite(": ~w", [Opts]))
    end.

%% Fetching a new microblock when it has been published
try_fetch_and_make_candidate() ->
    case aec_block_generator:get_candidate() of
        {ok, Block} ->
            Candidate = make_micro_candidate(Block),
            {ok, Candidate};
        {error, no_candidate} = Err ->
            Err
    end.

make_key_candidate(Block) ->
    Consensus   = aec_blocks:consensus_module(Block),
    BlockHeader = aec_blocks:to_header(Block),
    ForSealing  = Consensus:key_header_for_sealing(BlockHeader),
    Nonce       = Consensus:nonce_for_sealing(BlockHeader),
    {ForSealing, #candidate{ block    = Block,
                             nonce    = Nonce,
                             top_hash = aec_blocks:prev_hash(Block) }}.

make_micro_candidate(Block) ->
    #candidate{ block    = Block,
                top_hash = aec_blocks:prev_hash(Block) }.

%%%===================================================================
%%% Handle init options

set_option(autostart, Options, State) ->
    case get_option(autostart, Options) of
        undefined   -> State;
        {ok, true}  -> State#state{block_producing_state = running};
        {ok, false} -> State#state{block_producing_state = stopped}
    end;
set_option(strictly_follow_top, Options, State) ->
    case get_option(strictly_follow_top, Options) of
        undefined   -> State;
        {ok, V} when is_boolean(V) -> State#state{mining_opts = #{strictly_follow_top => V}}
    end.

get_option(Opt, Options) ->
    case proplists:lookup(Opt, Options) of
        none -> application:get_env(aecore, Opt);
        {_, Val} -> {ok, Val}
    end.

get_beneficiary(Consensus) ->
    Consensus:beneficiary().

get_next_beneficiary(Consensus) ->
    TopHeader = aec_chain:top_header(),
    get_next_beneficiary(Consensus, TopHeader).

get_next_beneficiary(Consensus, _TopHeader) ->
    case Consensus:next_beneficiary() of
        {ok, _L} = OK -> OK;
        {error, not_leader} = NotLeader ->
            NotLeader;
        {error, _} = Err ->
            Err
    end.

get_beneficiary() ->
    TopHeader = aec_chain:top_header(),
    Consensus = aec_consensus:get_consensus_module_at_height(aec_headers:height(TopHeader)),
    get_beneficiary(Consensus).

get_next_beneficiary() ->
    TopHeader = aec_chain:top_header(),
    Consensus = aec_consensus:get_consensus_module_at_height(aec_headers:height(TopHeader) + 1),
    get_next_beneficiary(Consensus, TopHeader).

note_rollback(Info) ->
    gen_server:call(?SERVER, {note_rollback, Info}).

set_mode(State) ->
    ConsensusModule = consensus_module(State),
    case ConsensusModule:get_type() of
        pow ->
            case aeu_env:user_config_or_env([<<"stratum">>, <<"enabled">>], aecore, stratum_enabled) of
                {ok, true} ->
                    set_stratum_mode(State);
                _ ->
                    set_beneficiary_configured(State#state{mode = local_pow}, ConsensusModule)
            end;
        pos ->
            IsBlockProducer = ConsensusModule:is_block_producer(),
            State#state{mode = pos, has_beneficiary = IsBlockProducer}
    end.

set_stratum_mode(State) ->
    Key = [<<"stratum">>, <<"reward">>, <<"keys">>, <<"dir">>],
    {ok, Dir} = aeu_env:user_config_or_env(Key, aecore, stratum_reward_keys_dir),
    {PubKey, _PrivKey} = aestratum_config:read_keys(Dir, create),
    State#state{mode = stratum, stratum_beneficiary = PubKey, has_beneficiary = true}.

set_beneficiary_configured(State, ConsensusModule) ->
    HasBeneficiaryConfigured =
        case get_beneficiary(ConsensusModule) of
            {error, beneficiary_not_configured} -> false;
            {ok, _} -> true
        end,
    State#state{has_beneficiary = HasBeneficiaryConfigured}.

%%%===================================================================
%%% Handle monitor messages

log_worker_crash_reason(create_key_block_candidate = Tag, Pid,
    {aborted, {{leader_validation_failed, invalid_leader}, _Stack}}
) ->
    %% For hyperchains this happens more often than for PoW: log it shorter and less severe
    epoch_mining:warning("Worker ~w (~w) died: ~0p", [Tag, Pid, leader_validation_failed]);
log_worker_crash_reason(Tag, Pid, Why) ->
    epoch_mining:error("Worker ~w (~w) died: ~0p", [Tag, Pid, Why]).

handle_monitor_message(Ref, Pid, Why, State) ->
    case lookup_worker(Ref, Pid, State) of
        not_found ->
            epoch_mining:info("Got unknown monitor DOWN message: ~0p",
                              [{Ref, Pid, Why}]),
            State;
        {ok, Tag} ->
            log_worker_crash_reason(Tag, Pid, Why),
            State1 = state_cleanup_after_worker(State, Tag, Pid),
            State2 = maybe_unblock_tag(Tag, State1),
            State3 = erase_worker(Pid, State2),
            start_block_production_(State3)
    end.

%%%===================================================================
%%% Worker handling
%%% @private
%%% @doc
%%%
%%% Worker functions are funs of arity 0 with a tag to determine the
%%% type of worker. Tags are enforced to be 'singleton' (only one worker
%%% allowed) or 'concurrent' (allow concurrent processes).
%%%
%%% The worker processes are monitored. Some types of worker are
%%% killed after a timeout.
%%%
%%% The worker processes provide return values through message
%%% passing. Return values are passed as messages, and the reply is
%%% handled based on the tag. The worker fun does not need to handle
%%% the message passing itself. This is taken care of by the
%%% dispatcher.
%%%
%%% Note that when the reply is handled, the state is the current
%%% server state, not the state in which the worker was
%%% dispatched. Any consistency checks for staleness must be handled
%%% in the reply handler.
%%%
%%% Workers can be killed (e.g., on preemption because of a changed
%%% chain) based on tag. Note that since the worker might have sent an
%%% answer before it is killed, it is good to check answers for
%%% staleness. TODO: This could be done by the framework.

lookup_worker(Ref, Pid, State) ->
    case orddict:find(Pid, State#state.workers) of
        {ok, #worker_info{mon = Ref} = Info} -> {ok, Info#worker_info.tag};
        error -> not_found
    end.

worker_pids_by_tag(Tag, State) ->
    orddict:fetch_keys(
      orddict:filter(
        fun(_, Info) -> Info#worker_info.tag == Tag end,
        State#state.workers)).

dispatch_worker(Tag, Fun, State) ->
    case is_tag_blocked(Tag, State) of
        true ->
            epoch_mining:error("Disallowing dispatch of additional ~p worker",
                               [Tag]),
            {State, undefined};
        false ->
            {Pid, Info} = spawn_worker(Tag, Fun),
            State1 = maybe_block_tag(Tag, State),
            Workers = orddict:store(Pid, Info, State1#state.workers),
            State2 = State1#state{workers = Workers},
            {State2, Pid}
    end.

is_tag_blocked(Tag, State) ->
    ordsets:is_element(Tag, State#state.blocked_tags).

maybe_block_tag(Tag, #state{blocked_tags = B} = State) ->
    case tag_concurrency(Tag) of
        concurrent -> State;
        singleton  -> State#state{blocked_tags = ordsets:add_element(Tag, B)}
    end.

maybe_unblock_tag(Tag, #state{blocked_tags = B} = State) ->
    case tag_concurrency(Tag) of
        concurrent -> State;
        singleton  -> State#state{blocked_tags = ordsets:del_element(Tag, B)}
    end.

tag_concurrency(mining)                     -> concurrent;
tag_concurrency(create_key_block_candidate) -> singleton;
tag_concurrency(micro_sleep)                -> singleton;
tag_concurrency(wait_for_keys)              -> singleton.

spawn_worker(Tag, Fun) ->
    Timeout = worker_timeout(Tag),
    spawn_worker(Tag, Fun, Timeout).

worker_timeout(create_key_block_candidate) ->
    infinity;
worker_timeout(micro_sleep) ->
    infinity; %% TODO NG: pull from governance and add buffer
worker_timeout(mining) ->
    aeu_env:get_env(aecore, mining_attempt_timeout, ?DEFAULT_MINING_ATTEMPT_TIMEOUT);
worker_timeout(wait_for_keys) ->
    infinity.

spawn_worker(Tag, Fun, Timeout) ->
    Wrapper = wrap_worker_fun(Fun),
    {Pid, Ref} = spawn_monitor(Wrapper),
    Timer = case Timeout of
                infinity ->
                    no_timer;
                TimeMs when is_integer(TimeMs), TimeMs > 0 ->
                    {ok, TRef} = timer:exit_after(TimeMs, Pid, shutdown),
                    {t, TRef}
            end,
    {Pid, #worker_info{tag = Tag, mon = Ref, timer = Timer}}.

wrap_worker_fun(Fun) ->
    Server = self(),
    fun() ->
            Server ! {worker_reply, self(), Fun()}
    end.

handle_stratum_reply(Reply, State) ->
    worker_reply(mining, Reply, State).

handle_worker_reply(Pid, Reply, State) ->
    Workers = State#state.workers,
    case orddict:find(Pid, Workers) of
        {ok, Info} ->
            Tag = Info#worker_info.tag,
            State1 = state_cleanup_after_worker(State, Tag, Pid),
            ok     = cleanup_after_worker(Info),
            State2 = maybe_unblock_tag(Tag, State1),
            State3 = erase_worker(Pid, State2),
            worker_reply(Tag, Reply, State3);
        error ->
            epoch_mining:info("Got unsolicited worker reply: ~p",
                              [{Pid, Reply}]),
            State
    end.

worker_reply(create_key_block_candidate, Res, State) ->
    handle_key_block_candidate_reply(Res, State);
worker_reply(mining, Res, State) ->
    handle_mining_reply(Res, State);
worker_reply(micro_sleep, Res, State) ->
    handle_micro_sleep_reply(Res, State);
worker_reply(wait_for_keys, Res, State) ->
    handle_wait_for_keys_reply(Res, State).

state_cleanup_after_worker(State, mining, Pid) ->
    deregister_instance(Pid, State);
state_cleanup_after_worker(State, _Tag, _Pid) ->
    State.

cleanup_after_worker(Info) ->
    case Info#worker_info.timer of
        no_timer -> ok;
        {t, TRef} -> timer:cancel(TRef)
    end,
    demonitor(Info#worker_info.mon, [flush]),
    ok.

kill_worker(Pid, Info, State) ->
    State1 = state_cleanup_after_worker(State, Info#worker_info.tag, Pid),
    ok     = cleanup_after_worker(Info),
    exit(Pid, shutdown),
    %% Flush messages from this worker.
    receive {worker_reply, Pid, _} -> ok
    after 0 -> ok end,
    State2 = maybe_unblock_tag(Info#worker_info.tag, State1),
    erase_worker(Pid, State2).

kill_all_workers(#state{workers = Workers} = State) ->
    lists:foldl(
      fun({Pid, Info}, S) ->
              kill_worker(Pid, Info, S)
      end,
      State, Workers).

kill_all_workers_with_tag(Tag, #state{workers = Workers} = State) ->
    lists:foldl(
      fun({Pid, Info}, S) ->
              case Tag =:= Info#worker_info.tag of
                  true  -> kill_worker(Pid, Info, S);
                  false -> S
              end
      end, State, Workers).

erase_worker(Pid, #state{workers = Workers} = State) ->
    State#state{workers = orddict:erase(Pid, Workers)}.

%%%===================================================================
%%% Miner instances handling

init_instances(#state{mode = pos} = State) ->
    ConsensusModule = consensus_module(State),
    [{Instance, Config}] = ConsensusModule:get_block_producer_configs(),
    State#state{instances = [create_instance(0, Instance, Config)]}; %% one instance shall be enough
init_instances(State) -> %% local_pow and stratum
    ConsensusModule = consensus_module(State),
    MinerConfigs   = ConsensusModule:get_block_producer_configs(),
    MinerInstances = create_miner_instances(MinerConfigs),
    State#state{instances = MinerInstances}.

%% Turns miner configs into miner instances, each with a unique Id
create_miner_instances(MinerConfigs) when is_list(MinerConfigs) ->
    {MinerInstances, _} =
        lists:foldl(
          fun(MinerConfig, {Acc, Id}) ->
                  {Instances, NextId} = create_miner_instances(MinerConfig, Id),
                  {Instances ++ Acc, NextId}
          end, {[], 0}, MinerConfigs),
    MinerInstances.

%% A single miner config can yield 0 or more instances
create_miner_instances(MinerConfig, FirstId) ->
    case aeminer_pow_cuckoo:addressed_instances(MinerConfig) of
        undefined ->
            {[create_instance(FirstId, undefined, MinerConfig)], FirstId + 1};
        AddressedInstances when is_list(AddressedInstances) ->
            lists:foldl(
              fun(AddressedInstance, {Acc, Id}) ->
                      {[create_instance(Id, AddressedInstance, MinerConfig) | Acc], Id + 1}
              end, {[], FirstId}, AddressedInstances)
    end.

create_instance(Id, Instance, Config) ->
    #instance{id       = Id,
              instance = Instance,
              config   = Config,
              state    = available}.

available_instance(#state{instances = MinerInstances}) ->
    get_first_available_instance(MinerInstances).

get_first_available_instance([]) ->
    none;
get_first_available_instance([#instance{state = available} = Instance | _Instances]) ->
    Instance;
get_first_available_instance([_Instance | Instances]) ->
    get_first_available_instance(Instances).

%% Update the list of instances in the state record, setting the `state`
%% field of the given instance to `Pid`
register_instance(Instance, Pid, #state{instances = MinerInstances0} = State) ->
    UpdatedInstance = Instance#instance{state = Pid},
    MinerInstances  = lists:keyreplace(Instance#instance.id, #instance.id, MinerInstances0, UpdatedInstance),
    State#state{instances = MinerInstances}.

%% Update the list of instances in the state record, setting the `state`
%% field of the given instance to `available`
deregister_instance(Pid, #state{instances = MinerInstances0} = State) ->
    Instance0 = lists:keyfind(Pid, #instance.state, MinerInstances0),
    Instance  = Instance0#instance{state = available},
    MinerInstances = lists:keyreplace(Pid, #instance.state, MinerInstances0, Instance),
    State#state{instances = MinerInstances}.

%%%===================================================================
%%% Preemption of workers if the top of the chain changes.

preempt_on_new_top(#state{ top_block_hash = OldHash,
                           top_key_block_hash = OldKeyHash,
                           top_height = OldHeight,
                           consensus = Consensus,
                           mode = Mode
                         } = State, NewBlock, NewHash, Origin) ->
    #consensus{ consensus_module = ConsensusModule } = Consensus,
    BlockType = aec_blocks:type(NewBlock),
    PrevNewHash = aec_blocks:prev_hash(NewBlock),
    Hdr = aec_blocks:to_header(NewBlock),
    Height = aec_headers:height(Hdr),
    maybe_gc_tx_pool(BlockType, Height, OldHeight),
    aec_tx_pool:top_change(#{type => BlockType,
                             old_hash => OldHash,
                             new_hash => NewHash,
                             old_height => OldHeight,
                             new_height => Height,
                             prev_new_hash => PrevNewHash}),
    aec_events:publish(top_changed, #{ block_hash => NewHash
                                     , block_type => BlockType
                                     , prev_hash  => aec_headers:prev_hash(Hdr)
                                     , height     => Height }),
    maybe_publish_top(Origin, NewBlock),
    aec_metrics:try_update([ae,epoch,aecore,chain,height], Height),
    State1 = State#state{top_block_hash = NewHash,
                         top_height = Height},
    KeyHash = aec_blocks:prev_key_hash(NewBlock),
    %% A new micro block from the same generation should:
    %%  * Not cause a pre-emption or full re-generation of key-block in PoW
    %%    context. The keyblock is being regenerated on every attempt to solve
    %%    the puzzle. There are a lot of attempts during the 3 seconds between
    %%    microblocks. It is not an optimisation for the miner to throw away a
    %%    valid solution that would discard the last microblock.

    case Mode of
        pos -> {pos, State1};
        _pow ->
            ResetWorkers = BlockType /= micro orelse OldKeyHash =/= KeyHash,
            case ResetWorkers of
                false ->
                    {micro_changed, State1};
                true ->
                    SignModule = ConsensusModule:get_sign_module(),
                    State2 = kill_all_workers_with_tag(mining, State1),
                    State3 = kill_all_workers_with_tag(create_key_block_candidate, State2),
                    State4 = kill_all_workers_with_tag(micro_sleep, State3), %% in case we are the leader
                    NewTopKey = case BlockType of
                                    micro -> KeyHash;
                                    key   -> NewHash
                                end,
                    State5 = State4#state{ top_key_block_hash = NewTopKey,
                                           key_block_candidates = undefined },

                    [ SignModule:promote_candidate(aec_blocks:miner(NewBlock)) || BlockType == key ],

                    {changed, BlockType, NewBlock, create_key_block_candidate(State5)}
            end
    end.

%% GH3283: If we start storing more kinds of tx_events - we have to expand this
%% function. I don't think we necessarily wants to publish the internal
%% contract txs in the same way as channel events...
%%
%% Exactly how we present the internal contract txs is an open question, but
%% some way to associate ContractCallTxHash with [InternalTxs]... And also
%% making the storage of this optional?!
maybe_publish_events(BlockType, Events, Hash, Origin) when Origin =/= block_synced,
                                                           Events =/= [] ->
    case BlockType of
        micro -> publish_tx_events(Events, Hash, Origin);
        key   -> publish_events(Events)
    end;
maybe_publish_events(_BlockType, _Events, _Hash, _Origin) ->
    ok.

publish_tx_events(Events, Hash, Origin) ->
    %% For performance reasons, the Events list is LIFO. We want to publish
    %% events in the order in which they were generated, so we reverse it.
    lager:debug("publish tx_events: ~p", [Events]),
    [aec_events:publish({tx_event, Event}, Info#{ block_hash => Hash
                                                , block_origin => Origin})
     || {Event, Info} <- lists:reverse(Events)].

publish_events(Events) ->
    lager:debug("publish key-block events: ~p", [Events]),
    [aec_events:publish(Type, Info) || {Type, Info} <- Events].


maybe_publish_top(block_created,_TopBlock) ->
    %% A new block we created is published unconditionally below.
    ok;
maybe_publish_top(micro_block_created,_TopBlock) ->
    %% A new block we created is published unconditionally below.
    ok;
maybe_publish_top(block_synced,_TopBlock) ->
    %% We don't publish blocks pulled from network. Otherwise on
    %% bootstrap the node would publish old blocks.
    ok;
maybe_publish_top(block_received, TopBlock) ->
    %% The received block pushed by a network peer changed the
    %% top. Publish the new top.
    aec_events:publish(block_to_publish, {received, TopBlock});
maybe_publish_top(micro_block_received, TopBlock) ->
    %% The received micro block pushed by a network peer changed the
    %% top. Publish the new top.
    aec_events:publish(block_to_publish, {received, TopBlock}).

maybe_publish_block(block_synced,_Block) ->
    %% We don't publish blocks pulled from network. Otherwise on
    %% bootstrap the node would publish old blocks.
    ok;
maybe_publish_block(BlockReceived,_Block)
  when BlockReceived =:= block_received
       orelse BlockReceived =:= micro_block_received ->
    %% We don't publish all blocks pushed by network peers, only if it
    %% changes the top.
    ok;
maybe_publish_block(block_created = T, Block) ->
    aec_events:publish(T, Block),
    %% This is a block we created ourselves. Always publish.
    aec_events:publish(block_to_publish, {created, Block});
maybe_publish_block(micro_block_created = T, Block) ->
    aec_events:publish(T, Block),
    %% This is a block we created ourselves. Always publish.
    aec_events:publish(block_to_publish, {created, Block}).

%%%===================================================================
%%% Worker: Wait for keys to appear

-define(WAIT_FOR_KEYS_RETRIES, 10).

wait_for_keys(State) ->
    ConsensusModule = consensus_module(State),
    Fun = fun () -> wait_for_keys_worker(ConsensusModule) end,
    {State1, _Pid} = dispatch_worker(wait_for_keys, Fun, State),
    State1.

wait_for_keys_worker(ConsensusModule) ->
    wait_for_keys_worker(ConsensusModule, ?WAIT_FOR_KEYS_RETRIES).

wait_for_keys_worker(_ConsensusModule, 0) ->
    timeout;
wait_for_keys_worker(ConsensusModule, N) ->
    SignModule = ConsensusModule:get_sign_module(),
    case SignModule:is_ready() of
        true -> keys_ready;
        false ->
            timer:sleep(500),
            wait_for_keys_worker(ConsensusModule, N - 1)
    end.

handle_wait_for_keys_reply(keys_ready, State) ->
    start_block_production_(State#state{keys_ready = true});
handle_wait_for_keys_reply(timeout, State) ->
    %% TODO: We should probably die hard at some point instead of retrying.
    epoch_mining:error("Timed out waiting for keys. Retrying."),
    wait_for_keys(State#state{keys_ready = false}).

%%%===================================================================
%%% Worker: Start mining
%%%
%%% - First, a key block candidate is created if none exists yet. Candidate
%%% creation happens in a separate worker, retrying block production when
%%% done - see create_key_block_candidate/1. If block production is
%%% disabled, nothing else is done here unless when running in Stratum
%%% mode.
%%% - If the latest created block candidate does not match the current top
%%% hash, a new candidate will be created.

start_block_production_(#state{keys_ready = false} = State) ->
    %% We need to get the keys first
    wait_for_keys(State);
start_block_production_(#state{key_block_candidates = undefined,
                               has_beneficiary      = true} = State) ->
    %% If the mining is turned off and beneficiary is configured,
    %% the key block candidate is still created, but not mined.
    %% The candidate can be retrieved via the API and other nodes can mine it.
    State1 = kill_all_workers_with_tag(create_key_block_candidate, State),
    create_key_block_candidate(State1);
start_block_production_(#state{mode = NotStratum, block_producing_state = stopped} = State)
    when NotStratum =:= local_pow orelse NotStratum =:= pos ->
    State;
start_block_production_(#state{key_block_candidates = [{_, #candidate{top_hash = OldHash}} | _],
                               top_block_hash = TopHash } = State) when OldHash =/= TopHash ->
    %% Candidate generated with stale top hash.
    %% Regenerate the candidate.
    epoch_mining:info("Key block candidate for old top hash; regenerating"),
    create_key_block_candidate(State);
start_block_production_(#state{mode = local_pow, key_block_candidates = [{ForSealing, Candidate} | Candidates]} = State) ->
    case available_instance(State) of
        none -> State;
        Instance ->
            epoch_mining:info("Starting PoW miner on top of ~p", [State#state.top_block_hash]),
            Consensus         = aec_blocks:consensus_module(Candidate#candidate.block),
            Header            = aec_blocks:to_header(Candidate#candidate.block),
            MinerConfig       = Instance#instance.config,
            AddressedInstance = Instance#instance.instance,
            Nonce             = Consensus:trim_sealing_nonce(Candidate#candidate.nonce, MinerConfig),
            Info              = [{top_block_hash, State#state.top_block_hash}],
            aec_events:publish(start_mining, Info),
            Fun = fun() ->
                          { Consensus:generate_key_header_seal(
                              ForSealing,
                              Header,
                              Nonce,
                              MinerConfig,
                              AddressedInstance)
                          , ForSealing}
                  end,
            Candidate1 = register_miner(Candidate, Consensus, Nonce, MinerConfig),
            State1 = State#state{key_block_candidates = [{ForSealing, Candidate1} | Candidates]},
            {State2, Pid} = dispatch_worker(mining, Fun, State1),
            State3 = register_instance(Instance, Pid, State2),
            epoch_mining:info("Block generator worker ~p started", [Pid]),
            start_block_production_(State3)
    end;
start_block_production_(#state{ mode = stratum,
                                key_block_candidates = [{ForSealing, #candidate{refs = StratumRefs} = Candidate} | Candidates]} = State)
  when StratumRefs =:= 0  ->
    epoch_mining:info("Stratum dispatch ~p", [ForSealing]),
    Target            = aec_blocks:target(Candidate#candidate.block),
    Info              = [{top_block_hash, State#state.top_block_hash}],
    Server            = self(),
    aec_events:publish(start_mining, Info),
    aec_events:publish(stratum_new_candidate, [{ForSealing, Candidate, Target, Server}]),
    Candidate1 = register_stratum(Candidate),
    State1 = State#state{key_block_candidates = [{ForSealing, Candidate1} | Candidates]},
    State1;
start_block_production_(#state{mode = pos, key_block_candidates = [{ForSealing, Candidate} | Candidates]} = State) ->
    case available_instance(State) of
        none ->
            epoch_mining:debug("No available instance", []),
            State;
        Instance ->
            epoch_mining:info("Starting PoS block sealer on top of ~p", [State#state.top_block_hash]),
            Consensus         = aec_blocks:consensus_module(Candidate#candidate.block),
            Header            = aec_blocks:to_header(Candidate#candidate.block),
            StakerConfig      = Instance#instance.config,
            AddressedInstance = Instance#instance.instance,
            Nonce             = Consensus:trim_sealing_nonce(Candidate#candidate.nonce, StakerConfig),
            Info              = [{top_block_hash, State#state.top_block_hash}],
            epoch_mining:info("Sealing candidate ~p", [Candidate]),
            aec_events:publish(start_mining, Info),
            Fun = fun() ->
                          { Consensus:generate_key_header_seal(
                              ForSealing,
                              Header,
                              Nonce,
                              StakerConfig,
                              AddressedInstance)
                          , ForSealing}
                  end,
            Candidate1 = register_miner(Candidate, Consensus, Nonce, StakerConfig),
            State1 = State#state{key_block_candidates = [{ForSealing, Candidate1} | Candidates]},
            {State2, Pid} = dispatch_worker(mining, Fun, State1),
            State3 = register_instance(Instance, Pid, State2),
            epoch_mining:info("Mining started ~p started", [Pid]),
            start_block_production_(State3)
    end.

register_stratum(Candidate = #candidate{refs  = Refs}) ->
    Candidate#candidate{refs  = Refs + 1}.

register_miner(Candidate = #candidate{refs  = Refs}, Consensus, Nonce, MinerConfig) ->
    NextNonce = Consensus:next_nonce_for_sealing(Nonce, MinerConfig),
    Candidate#candidate{refs  = Refs + 1,
                        nonce = NextNonce}.

handle_mining_reply({{continue_mining, MaybeSeal}, ForSealing}, State) ->
    handle_mining_reply_({MaybeSeal, ForSealing}, State);
handle_mining_reply({{stop_block_production, MaybeSeal}, ForSealing}, State) ->
    handle_mining_reply_({MaybeSeal, ForSealing}, State#state{block_producing_state = 'stopped'}).

handle_mining_reply_(Reply, #state{key_block_candidates = undefined} = State) ->
    %% Something invalidated the block candidates already.
    epoch_mining:debug("Candidate invalidated in conductor ~p", [Reply]),
    start_block_production_(State);
handle_mining_reply_({{ok, Seal}, ForSealing}, #state{} = State) ->
    Candidates = State#state.key_block_candidates,
    %% Check that the solution is for one of the valid candidates.
    case proplists:get_value(ForSealing, Candidates) of
        #candidate{block = CandidateBlock} ->
            aec_metrics:try_update([ae,epoch,aecore,mining,blocks_mined], 1),
            State1 = State#state{key_block_candidates = undefined},
            Consensus = aec_blocks:consensus_module(CandidateBlock),
            Block = Consensus:set_key_block_seal(CandidateBlock, Seal),
            case handle_mined_block(Block, State1) of
                {ok, State2} ->
                    State2;
                {{error, Reason}, State2} ->
                    epoch_mining:error("Block insertion failed: ~p.", [Reason]),
                    start_block_production_(State2)
            end;
        undefined ->
            %% This mining effort was for an earlier block candidate.
            epoch_mining:error("Found solution for old block", []),
            start_block_production_(State)
    end;
handle_mining_reply_({{error, no_solution}, ForSealing}, State) ->
    aec_metrics:try_update([ae,epoch,aecore,mining,retries], 1),
    epoch_mining:debug("Failed to mine block, no solution; retrying."),
    retry_mining(State, ForSealing);
handle_mining_reply_({{error, {runtime, Reason}}, ForSealing}, State) ->
    aec_metrics:try_update([ae,epoch,aecore,mining,retries], 1),
    epoch_mining:error("Failed to mine block, runtime error; "
                       "retrying with different nonce. "
                       "Error: ~p", [Reason]),
    retry_mining(State, ForSealing).

%%%===================================================================
%%% Retry mining when we failed to find a solution.

retry_mining(S = #state{key_block_candidates = [{ForSealing, Candidate} | Candidates]}, ForSealing) ->
    Candidate1 = Candidate#candidate{refs = Candidate#candidate.refs - 1},
    start_block_production_(S#state{key_block_candidates = [{ForSealing, Candidate1} | Candidates]});
retry_mining(S = #state{key_block_candidates = Candidates}, ForSealing) when is_list(Candidates) ->
    case proplists:get_value(ForSealing, Candidates) of
        undefined ->
            create_key_block_candidate(S);
        #candidate{refs = 1} ->
            create_key_block_candidate(S#state{key_block_candidates = proplists:delete(ForSealing, Candidates)});
        #candidate{refs = N} = C when N > 1 ->
            Candidates1 = lists:keyreplace(ForSealing, 1, Candidates, {ForSealing, C#candidate{refs = N - 1}}),
            create_key_block_candidate(S#state{key_block_candidates = Candidates1})
    end.

%%%===================================================================
%%% Worker: Start signing microblocks

start_micro_signing(#state{keys_ready = false} = State) ->
    %% We need to get the keys first
    wait_for_keys(State);
start_micro_signing(#state{consensus = #consensus{leader = true}, micro_block_candidate = undefined} = State) ->
    %% We have to wait for a new block candidate first.
    State;
start_micro_signing(#state{consensus = #consensus{leader = true},
                           micro_block_candidate = #candidate{top_hash = MicroBlockHash},
                           top_block_hash = TopHash} = State) when MicroBlockHash =/= TopHash ->
    %% Candidate generated with stale top hash.
    %% Regenerate the candidate.
    State#state{micro_block_candidate = undefined};
start_micro_signing(#state{consensus = #consensus{leader = true},
                           micro_block_candidate = #candidate{block = MicroBlock}} = State) ->
    ConsensusModule = consensus_module(State),
    case is_tag_blocked(micro_sleep, State) of
        true ->
            epoch_mining:debug("Too early to sign micro block, wait a bit longer"),
            State;
        false ->
            epoch_mining:info("Signing microblock"),
            SignModule = ConsensusModule:get_sign_module(),
            {ok, SignedMicroBlock} = SignModule:sign_micro_block(MicroBlock),
            State1 = State#state{micro_block_candidate = undefined},
            case handle_signed_block(SignedMicroBlock, State1) of
                {ok, State2} ->
                    State2;
                {{error, Reason}, State2} ->
                    epoch_mining:error("Block insertion failed: ~p.", [Reason]),
                    start_micro_signing(State2)
            end
    end;
start_micro_signing(#state{consensus = Consensus,
                           micro_block_candidate = MicroCandidate,
                           key_block_candidates = KeyBlockCandidate,
                           top_block_hash = SeenHash
                          } = State) ->
    %% Probably no longer the leader
    epoch_mining:debug("Fallback clause, candidate conditions not met. micro: ~p, key: ~p, seen top: ~p, consensus: ~p",
                       [MicroCandidate, KeyBlockCandidate, SeenHash, Consensus]),
    State.

%%%===================================================================
%%% Worker: Timer for sleep between micro blocks

start_micro_sleep(#state{consensus = #consensus{leader = true, micro_block_cycle = Timeout}} = State) ->
    epoch_mining:debug("Starting sleep in between microblocks"),
    Info      = [{start_micro_sleep, State#state.top_block_hash}],
    aec_events:publish(start_micro_sleep, Info),
    Fun = fun() ->
                  timer:sleep(Timeout)
          end,
    {State1, _Pid} = dispatch_worker(micro_sleep, Fun, State),
    State1;
start_micro_sleep(State) ->
    State.

handle_micro_sleep_reply(ok, State) ->
    start_micro_signing(State).

%%%===================================================================
%%% Worker: Generate new block candidates

create_key_block_candidate(#state{keys_ready = false} = State) ->
    %% Keys are needed for creating a candidate
    wait_for_keys(State);
create_key_block_candidate(#state{has_beneficiary = false} = State) ->
    %% Without a configured beneficiary, nothing is done here and we don't
    %% go back to block production.
    State;
create_key_block_candidate(#state{key_block_candidates = [{_, #candidate{top_hash = TopHash}} | _],
                                  top_block_hash       = TopHash} = State) ->
    %% We have the most recent candidate already. Just start mining.
    start_block_production_(State);
create_key_block_candidate(#state{block_producing_state = stopped, mode = pos} = State) ->
    State;
create_key_block_candidate(#state{top_block_hash = TopHash, mode = pos} = State) ->
    epoch_mining:info("HC: check and maybe create micro + key block at the top of the chain"),
    Fun = hc_create_block_fun(TopHash),
    {State1, _Pid} = dispatch_worker(create_key_block_candidate, Fun, State),
    State1;
create_key_block_candidate(#state{top_block_hash      = TopHash,
                                  mode                = Mode,
                                  stratum_beneficiary = StratumBeneficiary} = State) ->
    ConsensusModule = consensus_module(State),
    epoch_mining:info("Creating key block candidate on the top"),
    BeneficiaryFun =
        fun() ->
            case Mode of
                stratum -> {ok, StratumBeneficiary};
                _ ->
                    get_next_beneficiary(ConsensusModule)
            end
        end,
    Fun = fun() ->
                SignModule = ConsensusModule:get_sign_module(),
                case BeneficiaryFun() of
                    {ok, Beneficiary} ->
                        {ok, Miner} = SignModule:candidate_pubkey(),
                        {aec_block_key_candidate:create(TopHash, Beneficiary, Miner), TopHash};
                    {error, _} = Err -> {Err, TopHash}
                end
          end,
    {State1, _Pid} = dispatch_worker(create_key_block_candidate, Fun, State),
    State1.

handle_key_block_candidate_reply({{ok, KeyBlockCandidate}, TopHash},
                                 #state{top_block_hash = TopHash,
                                        key_block_candidates = Candidates0} = State) ->
    epoch_mining:info("Created key block candidate "
                      "Its target is ~p (= difficulty ~p).",
                      [aec_blocks:target(KeyBlockCandidate),
                       aec_blocks:difficulty(KeyBlockCandidate)]),
    {ForSealing, Candidate} = make_key_candidate(KeyBlockCandidate),

    Candidates = case Candidates0 of
                     undefined -> [{ForSealing, Candidate}];
                     _         -> [{ForSealing, Candidate} | Candidates0]
                 end,
    State1 = State#state{key_block_candidates = Candidates},
    start_block_production_(State1);
handle_key_block_candidate_reply({{ok, KeyBlockCandidate}, NewTopHash},
                                 #state{top_block_hash = TopHash,
                                        mode = pos} = State) ->
    epoch_mining:info("Created PoS key block candidate: ~p with micro-block: ~p",
                      [aec_blocks:height(KeyBlockCandidate), NewTopHash /= TopHash]),
    {ForSealing, Candidate} = make_key_candidate(KeyBlockCandidate),

    Candidates = [{ForSealing, Candidate}],
    State1 = State#state{key_block_candidates = Candidates},
    start_block_production_(State1);
handle_key_block_candidate_reply({{ok, _KeyBlockCandidate}, _OldTopHash},
                                 #state{top_block_hash = _TopHash} = State) ->
    epoch_mining:debug("Created key block candidate is already stale, create a new one", []),
    create_key_block_candidate(State);
handle_key_block_candidate_reply({{error, {no_action, _Rsn} = Reason}, _}, State) ->
    epoch_mining:debug("Creation of key block candidate cancelled: ~w", [Reason]), % not an error, will try again
    create_key_block_candidate(State);
handle_key_block_candidate_reply({{error, key_not_found}, _}, State) ->
    start_block_production_(State#state{keys_ready = false});
handle_key_block_candidate_reply({{error, Reason}, _}, #state{top_height = Height} = State)
        when Reason =:= not_in_cache;
             Reason =:= not_leader ->
    epoch_mining:debug("Creation of key block candidate (~p) failed: ~p", [Height + 1, Reason]),
    create_key_block_candidate(State);
handle_key_block_candidate_reply({{error, Reason}, _}, State) ->
    epoch_mining:error("Creation of key block candidate failed: ~p", [Reason]),
    create_key_block_candidate(State).

%% The result is sent via a message to wrap_worker_fun/1, and then is handled here by worker_reply/3
%% (and from there handled by handle_key_block_candidate_reply/2)
hc_create_block_fun(TopHash) ->
    fun() ->
        hc_next_producer(TopHash)
    end.

hc_next_producer(TopHash) ->
    case aec_consensus_hc:next_producer() of
        {wait, Reason, WaitT} ->
            lager:debug("Producer should wait (~p) for ~p ms", [Reason, WaitT]),
            maybe_sleep(WaitT),
            {{error, {no_action, Reason}}, TopHash};
        {ok, {leader_hole, _Leader, Holes}, WaitT} when WaitT > 0 ->
            %% We are the leader, but need 1+ hole blocks before we can produce
            lager:debug("Is producer, and chain is currently too short: missing=~p wait ~p ms", [Holes, WaitT]),
            maybe_sleep(WaitT),
            hc_next_producer(aec_chain:top_block_hash());
        {ok, {leader_hole, Leader, Holes}, _} ->
            %% Create one hole and it will need to be sealed by an asynchronous worker
            lager:debug("Is producer, and chain is currently too short: missing=~p", [Holes]),
            {hc_create_hole(TopHash, Holes, Leader), TopHash};
        {ok, {leader, Leader}, WaitT} ->
            lager:debug("Is producer, calling hc_create_block in ~p ms", [WaitT]),
            maybe_sleep(WaitT),
            {hc_create_block(TopHash, Leader), TopHash};
        {error, Reason, RetryT} ->
            lager:debug("Producer error (~p) retry in ~p ms", [Reason, RetryT]),
            maybe_sleep(RetryT),
            {{error, Reason}, TopHash}
    end.

maybe_sleep(T) when T =< 0 -> ok;
maybe_sleep(T)             -> timer:sleep(T).

%% For as long as chain length is shorter than currentHeight-1, create holes. Send holes async to conductor for writing
hc_create_hole(TopHash, MissingBlocksCount, Producer) when MissingBlocksCount > 0 ->
    aec_block_hole_candidate:create(TopHash, Producer, Producer, true).

hc_create_block(TopHash0, Producer) ->
    VoteResult = aec_consensus_hc:vote_result(),
    TopHash = hc_create_microblock(TopHash0, Producer, VoteResult),
    aec_block_hole_candidate:create(TopHash, Producer, Producer, false).

hc_create_microblock(TopHash, Leader, VoteResult) ->
    CreateResult = case VoteResult of
                        {ok, VoteTx} ->
                            Tx = aetx_sign:tx(VoteTx),
                                case aec_chain:get_block(TopHash) of
                                    {ok, Block} ->
                                        Gas = aetx:gas_limit(Tx, aec_blocks:height(Block), aec_blocks:version(Block)),
                                        aec_block_micro_candidate:create(TopHash, Gas);
                                    Error ->
                                        lager:warning("Couldn't calculate gas for vote transaction ~p", [Error]),
                                        aec_block_micro_candidate:create(TopHash)
                                end;
                        _ ->
                            aec_block_micro_candidate:create(TopHash)
                    end,
    case CreateResult of
        {ok, MBlock, MBlockInfo} ->
            MBlock1 = hc_apply_vote(VoteResult, MBlock, MBlockInfo),
            case aec_blocks:txs(MBlock1) of
                [] ->
                    epoch_mining:debug("Empty micro-block, top is still: ~p", [TopHash]),
                    TopHash;
                [_ | _ ] ->
                    SignModule = aec_consensus_hc:get_sign_module(),
                    {ok, SignedMBlock} = SignModule:sign_micro_block(MBlock1, Leader),
                    add_signed_block(SignedMBlock),
                    {ok, MBHash} = aec_blocks:hash_internal_representation(SignedMBlock),
                    epoch_mining:debug("New micro-block added, top is: ~p", [MBHash]),
                    MBHash
            end;
        _ ->
            epoch_mining:info("Failed micro-block, top is still: ~p", [TopHash]),
            TopHash
    end.

hc_apply_vote(VoteResult, MBlock, MBlockInfo) ->
    hc_apply_vote(VoteResult, MBlock, MBlockInfo, false).

hc_apply_vote(VoteResult, MBlock, MBlockInfo, TriedWithTrees) ->
    case VoteResult of
        {ok, VoteTransaction} ->
            case aec_block_micro_candidate:update(MBlock, [VoteTransaction], MBlockInfo) of
                {ok, UpdatedMBlock, _MBlockInfo} ->
                    UpdatedMBlock;
                Error ->
                    case TriedWithTrees of
                        false ->
                            %% Retry with trees to correct nonce
                            {ok, Trees} = aec_block_micro_candidate:trees(MBlockInfo),
                            hc_apply_vote(aec_consensus_hc:vote_result(Trees), MBlock, MBlockInfo, true);
                        true ->
                            lager:warning("Error adding vote transaction ~p", [Error]),
                            MBlock
                    end
            end;
        _ ->
            MBlock
    end.

%%%===================================================================
%%% In server context: A block was given to us from the outside world

%% A block was received via synchronization with other nodes
handle_synced_block(Block, State) ->
    epoch_mining:debug("synced_block: ~p", [Block]),
    handle_add_block(Block, State, block_synced).

%% A block was posted via the HTTP API or similar
handle_post_block(Block, State) ->
    case aec_blocks:is_key_block(Block) of
        true ->
            epoch_mining:info("post_block: ~p", [Block]),
            handle_add_block(Block, State, block_received);
        false ->
            epoch_mining:info("post_micro_block: ~p", [Block]),
            handle_add_block(Block, State, micro_block_received)
    end.

handle_mined_block(Block, State) ->
    epoch_mining:info("Block mined: Height = ~p; Hash = ~p",
                      [aec_blocks:height(Block),
                       ok(aec_blocks:hash_internal_representation(Block))]),
    handle_add_block(Block, State, block_created).

%% Adding a locally signed microblock
handle_signed_block(Block, State) ->
    epoch_mining:info("Block signed: Height = ~p; Hash = ~p",
                      [aec_blocks:height(Block),
                       ok(aec_blocks:hash_internal_representation(Block))]),
    handle_add_block(Block, State, micro_block_created).

ok({ok, Value}) ->
    Value.


handle_add_block(Block, State, Origin) ->
    ActiveConsensusModule = consensus_module(State),
    try
    Header = aec_blocks:to_header(Block),
    case aec_headers:consensus_module(Header) of
        ActiveConsensusModule ->
            handle_add_block(Header, Block, State, Origin);
        _ ->
            case ActiveConsensusModule:can_be_turned_off() of
                true ->
                    %% This is OK only when we deal with ordinary consensus algorithms
                    %% This is expected when switching between PoA, PoW, Hyperchains
                    handle_add_block(Header, Block, State, Origin);
                false ->
                    %% "Dev mode" consensus should have killed the peer pool and other unnecessary components by now
                    %% Some pending blocks might still be present in the message queue - ignore them
                    {{error, special_consensus_active}, State}
            end
    end
    catch
        exit:{aborted, {{handled_abort, Err}, _}} ->
            lager:debug("Block was not accepted because of ~p", [Err]),
            {{error, Err}, State}
    end.


throw_error(Error) ->
    error({handled_abort, Error}).

handle_add_block(Header, Block, State, Origin) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    Prev = aec_headers:prev_hash(Header),
    StrictlyFollow = maps:get(strictly_follow_top, State#state.mining_opts, false),
    case {StrictlyFollow, Prev =:= State#state.top_block_hash} of
        {true, false} when Origin =:= block_created;
                           Origin =:= micro_block_created ->
            epoch_mining:info("Mined block has old top - discarding (~w)", [Hash]),
            {{error, obsolete}, State};
        _ ->
            handle_add_block(Block, Hash, Prev, State, Origin)
    end.

handle_add_block(Block, Hash, Prev, #state{top_block_hash = TopBlockHash, consensus = Consensus} = State, Origin) ->
    epoch_mining:debug("trying to add block (hash=~w, prev=~w)", [Hash, Prev]),
    %% Block validation is performed in the caller's context for
    %% external (gossip/sync) blocks and we trust the ones we
    %% produce ourselves.
    ConsensusModule = Consensus#consensus.consensus_module,
    case aec_chain_state:insert_block_conductor(Block, Origin) of
        {ok, TopChanged, PrevKeyHeader, Events} = OkResult  ->
            case ConsensusModule of
                aec_consensus_hc ->
                    Header = aec_blocks:to_header(Block),
                    lager:debug("insert_block ~s ~p -> ~p", [
                        case aec_headers:is_hole(Header) of true -> "HOLE"; false -> "non-hole" end,
                        Header, OkResult]);
                _ ->
                    lager:debug("insert_block ~p -> ~p", [aec_blocks:to_header(Block), OkResult])
            end,
            handle_successfully_added_block(Block, Hash, TopChanged, PrevKeyHeader, Events, State, Origin);
        {pof, TopChanged, PrevKeyHeader, _PoF, Events} ->
            %% TODO: should we really publish tx_events in this case?
            lager:info("PoF found in ~p", [Hash]),
            handle_successfully_added_block(Block, Hash, TopChanged, PrevKeyHeader, Events, State, Origin);
        {error, already_in_db} ->
            epoch_mining:debug("Block (~p) already in chain when top is (~p) [conductor]",
                               [Hash, TopBlockHash]),
            {ok, State};
        {error, Reason} when Origin == block_created; Origin == micro_block_created ->
            lager:error("Couldn't insert created block (~p)", [Reason]),
            {{error, Reason}, State};
        {error, Reason = {illegal_orphan, H}} ->
            lager:info("Couldn't insert received block ({not_attached_to_chain, ~s})", [aeu_debug:pp(H)]),
            {{error, Reason}, State};
        {error, Reason} ->
            lager:info("Couldn't insert received block (~p)", [Reason]),
            {{error, Reason}, State}
    end.

handle_successfully_added_block(Block, Hash, false, _PrevKeyHeader, Events, State, Origin) ->
    %% when the top hash did not change
    BlockType = aec_blocks:type(Block),
    maybe_publish_events(BlockType, Events, Hash, Origin),
    maybe_publish_block(Origin, Block),
    State1 = maybe_consensus_change(State, Block),
    [ maybe_garbage_collect(Block, Hash, false) || BlockType == key ],
    {ok, State1};
handle_successfully_added_block(Block, Hash, true, PrevKeyHeader, Events, State, Origin) ->
    %% when the top hash has changed
    maybe_publish_events(aec_blocks:type(Block), Events, Hash, Origin),
    maybe_publish_block(Origin, Block),
    State1 = maybe_consensus_change(State, Block),
    ConsensusModule = consensus_module(State1),
    %% abort running workers if necessary
    case preempt_on_new_top(State1, Block, Hash, Origin) of
        {pos, State2} ->
            case Origin of
                block_created -> {ok, create_key_block_candidate(State2)};
                _             -> {ok, State2}
            end;
        {micro_changed, State2 = #state{ consensus = Cons }} ->
            {ok, setup_loop(State2, false, Cons#consensus.leader, Origin)};
        {changed, BlockType, NewTopBlock, State2} ->
            (BlockType == key) andalso
                aec_metrics:try_update(
                  [ae,epoch,aecore,blocks,key,info], info_value(NewTopBlock)),
            [ maybe_garbage_collect(NewTopBlock, Hash, true)
              || BlockType == key ],
            IsLeader = is_leader(NewTopBlock, PrevKeyHeader, ConsensusModule),
            {ok, setup_loop(State2, true, IsLeader, Origin)}
    end.

maybe_gc_tx_pool(key, Height, OldHeight) when Height > OldHeight ->
    _ = aec_tx_pool_gc:sync_gc(Height),
    ok;
maybe_gc_tx_pool(_, _, _) ->
    ok.

maybe_consensus_change(State, Block) ->
    %% When a new block got successfully inserted we need to check whether the next block
    %% would use different consensus algorithm - This is the point where
    %% dev mode should wreck havoc on the entire system and redirect everything
    %% to a chain simulator ;)
    ActiveConsensusModule = consensus_module(State),
    H = aec_blocks:height(Block),
    case aec_consensus:get_consensus_module_at_height(H + 1) of
        ActiveConsensusModule ->
            State;
        NewConsensus ->
            %% It looks like dev mode needs to be activated :)
            true = ActiveConsensusModule:can_be_turned_off(),
            ActiveConsensusModule:stop(),
            NewConfig = aec_consensus:get_consensus_config_at_height(H+1),
            Mining = is_mining(State),
            NewConsensus:start(NewConfig, #{block_production => Mining}),
            #state{consensus = Consensus} = State,
            State#state{ consensus = Consensus#consensus{ consensus_module = NewConsensus } }
    end.

info_value(Block) ->
    case aec_headers:info(aec_blocks:to_key_header(Block)) of
        I when is_integer(I) ->
            I;
        undefined ->
            0
    end.

is_leader(NewTopBlock, PrevKeyHeader, ConsensusModule) ->
    LeaderKey =
        case aec_blocks:type(NewTopBlock) of
            key   ->
                aec_blocks:miner(NewTopBlock);
            micro ->
                aec_headers:miner(PrevKeyHeader)
        end,
    SignModule = ConsensusModule:get_sign_module(),
    case SignModule:get_pubkey() of
        {ok, MinerKey} -> LeaderKey =:= MinerKey;
        {error, _}     -> false
    end.

%% Runs after a block has been added
setup_loop(State = #state{ consensus = Cons }, RestartMining, IsLeader, Origin) ->
    State1 = State#state{ consensus = Cons#consensus{ leader = IsLeader } },
    State2 =
        case Origin of
            Origin when IsLeader, Origin =:= block_created
                        orelse Origin =:= block_received ->
                aec_block_generator:start_generation(),
                start_micro_signing(State1);
            block_received when not IsLeader ->
                aec_block_generator:stop_generation(),
                State1;
            micro_block_created when IsLeader ->
                start_micro_sleep(State1);
            Origin when Origin =:= block_created; Origin =:= micro_block_created;
                        Origin =:= block_received; Origin =:= micro_block_received;
                        Origin =:= block_synced ->
                State1
        end,
    case RestartMining of
        true  -> start_block_production_(State2);
        false -> State2
    end.

get_pending_key_block(undefined, State) ->
    {{error, not_found}, State};
get_pending_key_block(_TopHash, #state{has_beneficiary = false} = State) ->
    {{error, beneficiary_not_configured}, State};
get_pending_key_block(TopHash, State) ->
    ConsensusModule = consensus_module(State),
    SignModule = ConsensusModule:get_sign_module(),
    Beneficiary =
        case State#state.mode of
            stratum -> State#state.stratum_beneficiary;
            _ ->
                {ok, B} = get_next_beneficiary(ConsensusModule),
                B
        end,
    {ok, Miner} = SignModule:candidate_pubkey(),
    case aec_block_key_candidate:create(TopHash, Beneficiary, Miner) of
        {ok, Block} -> {{ok, Block}, State#state{ pending_key_block = Block }};
        {error, _}  -> {{error, not_found}, State}
    end.


%% Called only from the conductor.
%% Putting it here avoids us to clutter aec_db_gc's public API
%%
%% To avoid starting of the GC process just for EUNIT
-ifdef(EUNIT).
maybe_garbage_collect(_, _, _) -> nop.
-else.

%% This should be called when there are no processes modifying the block state
%% (e.g. aec_conductor on specific places)
maybe_garbage_collect(Block, Hash, TopChange) ->
    T0 = erlang:system_time(microsecond),
    Header = aec_blocks:to_header(Block),
    Res = aec_db_gc:maybe_garbage_collect(Header, Hash, TopChange),
    T1 = erlang:system_time(microsecond),
    lager:debug("Result -> ~p (time: ~p us)", [Res, T1-T0]),
    Res.
-endif.

consensus_module(#state{ consensus = #consensus{consensus_module =
                                                ConsensusModule}}) ->
    ConsensusModule.

is_mining(#state{block_producing_state = running}) -> true;
is_mining(#state{block_producing_state = stopped}) -> false.
