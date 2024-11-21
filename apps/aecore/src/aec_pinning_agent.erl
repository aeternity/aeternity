%%%-----------------------------------------------------------------------------
%%% @doc
%%% Default pinning agent for hyperchains.
%%% Supported by the Aeternity Foundation
%%% @author mans.af.klercker@happihacking.se
%%% @end
%%%-----------------------------------------------------------------------------

-module(aec_pinning_agent).
-author("mans.af.klercker@happihacking.se").
-behaviour(gen_server).

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    start_link/2,
    stop/0
]).

%% Callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    spawn_for_epoch/3,
    start/3
]).

-define(SERVER, ?MODULE).
-define(WORKER, worker_name()).

%% Loop state
-record(state, {
    contract
}).
-type state() :: state.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link(term(), atom()) -> {ok, pid()} | {error, {already_started, pid()}} | ignore | {error, Reason::any()}.
start_link(Contract, PinningBehavior) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Contract, PinningBehavior], []).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

init([Contract, PinningBehavior]) ->
    case PinningBehavior of
        true ->
            State = #state{contract = Contract},
            lager:debug("started pinning agent"),
            aec_events:subscribe(new_epoch),
            {ok, State};
        false ->
            lager:debug("default pinning off - no agent started", []),
            ignore
    end.


handle_call(_Request, _From, LoopState) ->
    Reply = ok,
    {reply, Reply, LoopState}.

handle_cast(_Msg, LoopState) ->
    {noreply, LoopState}.

handle_info({gproc_ps_event, new_epoch, #{info := EpochInfo}}, #state{contract = Contract} = State) ->
    Reply = spawn_for_epoch(EpochInfo, Contract),
    {Reply, State};
handle_info(_Info, LoopState) ->
    {noreply, LoopState}.

terminate(_Reason, _LoopState) ->
    ok.

code_change(_OldVsn, LoopState, _Extra) ->
    {ok, LoopState}.

    % case aec_parent_connector:has_parent_account(LastLeader) of
    %     true -> aec_pinning_agent:spawn_for_epoch(NextEpochInfo, get_contract_pubkey(?ELECTION_CONTRACT), LastLeader);
    %     false -> lager:debug("AGENT: No parent chain account found for ~p", [LastLeader])
    % end
stop() ->
    lager:debug("STOPPED"),
    gen_server:stop(?SERVER).
%%%=============================================================================
%%% API
%%%=============================================================================


%%%=============================================================================
%%% Pinning Worker Process
%%%=============================================================================

spawn_for_epoch(#{last := Last} = EpochInfo, Contract) ->
    {ok, LastLeader} = aec_consensus_hc:leader_for_height(Last),
    case aec_parent_connector:has_parent_account(LastLeader) of
        true -> spawn_for_epoch(EpochInfo, Contract, LastLeader);
        false -> lager:debug("no pin in epoch, no parent account for ~p", [LastLeader]), noreply
    end.

spawn_for_epoch(EpochInfo, Contract, LastLeader) ->
    try
    case whereis(my_unique_process) of
        undefined ->
            Pid = spawn(aec_pinning_agent, start, [EpochInfo, Contract, LastLeader]),
            register(?WORKER, Pid),
            noreply;
        Pid when is_pid(Pid) ->
            lager:debug("pinning worker already started", []),
            noreply
    end
    catch
        T:E -> lager:debug("Pinning agent worker failed: ~p:~p", [T,E]),  noreply
    end.

start(EpochInfo, Contract, LastLeader) ->
    #{ epoch      := _Epoch
     , last       := Last
     , validators := _Validators} = EpochInfo,
    subscribe(),
    lager:debug("pinning worker started ~p", [self()]),
    wait_for_top_changed(Last - 1, none, false, Contract, LastLeader).


%%%=============================================================================
%%% FSM
%%%=============================================================================

subscribe() ->
    aec_events:subscribe(top_changed).

post_pin_to_pc(LastLeader, Height) ->
    PCPinTx = aec_parent_connector:pin_to_pc(LastLeader, 1, 1000000 * min_gas_price()),
    lager:debug("(~p) Pinned to PC ~p at height ~p", [self(), PCPinTx, Height]),
    PCPinTx.

post_pin_pctx_to_cc(PinTx, LastLeader, Height) ->
    try
        aec_parent_connector:pin_tx_to_cc(PinTx, LastLeader, 1, 1000000 * min_gas_price()),
        lager:debug("(~p) noting on CC @~p", [self(), Height])
    catch
        T:E -> lager:debug("Pin to CC failed: ~p:~p", [T,E]), ok
    end.

post_pin_proof(ContractPubkey, PinTx, LastLeader, Height) ->
    lager:debug("(~p) pin proof @~p ~p", [self(), Height, PinTx]),
    aec_parent_connector:pin_contract_call(ContractPubkey, PinTx, LastLeader, 0, 1000000 * min_gas_price()).

wait_for_top_changed(Last, PCPinTx, CCPosted, Contract, LastLeader) ->
    receive
        {gproc_ps_event, top_changed, #{info := #{height := Height} = Info}} ->
            NewPCPinTx = maybe_post_pin_to_pc(PCPinTx, LastLeader, Height),
            NewCCPosted = maybe_post_pin_to_cc(NewPCPinTx, CCPosted, LastLeader, Height),

            case {Height, NewCCPosted} of
                {Last, true} -> post_pin_proof(Contract, NewPCPinTx, LastLeader, Last);
                {Last, false} -> ok;
                _ -> wait_for_top_changed(Last, NewPCPinTx, NewCCPosted, Contract, LastLeader)
            end
    end.

maybe_post_pin_to_pc(none, LastLeader, Height) ->
    post_pin_to_pc(LastLeader, Height);
maybe_post_pin_to_pc(PCPinTx, _, _) ->
    PCPinTx.

maybe_post_pin_to_cc(PCPinTx, false, LastLeader, Height) ->
    case aec_parent_connector:get_pin_by_tx_hash(PCPinTx) of
        {ok, #{pc_height := -1}} -> false;
        {ok, _} -> post_pin_pctx_to_cc(PCPinTx, LastLeader, Height), true;
        _ -> false
    end;
maybe_post_pin_to_cc(_, CCPosted, _, _) ->
    CCPosted.


%%%=============================================================================
%%% Helpers, communication
%%%=============================================================================

worker_name() ->
    list_to_atom(?MODULE_STRING "_worker").

min_gas_price() ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    max(aec_governance:minimum_gas_price(Protocol),
        aec_tx_pool:minimum_miner_gas_price()).
