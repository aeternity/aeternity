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

-define(SERVER, ?MODULE).

%% Loop state
-record(state, {
    contract,
    pinning_mode,
    next_last,
    pc_pin,
    cc_note,
    last_leader
}).
-type state() :: state.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link(term(), atom()) -> {ok, pid()} | {error, {already_started, pid()}} | ignore | {error, Reason::any()}.
start_link(Contract, PinningBehavior) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Contract, PinningBehavior], []).

stop() ->
    gen_server:stop(?SERVER).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

init([Contract, PinningBehavior]) ->
    case PinningBehavior of
        true ->
            State = #state{contract = Contract, pinning_mode = false},
            lager:debug("started pinning agent"),
            aec_events:subscribe(new_epoch),
            aec_events:subscribe(top_changed),
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

handle_info({gproc_ps_event, new_epoch, #{info := #{last := Last, first := First, epoch := Epoch}}}, State) ->
    {ok, LastLeader} = aec_consensus_hc:leader_for_height(Last),
    case aec_parent_connector:has_parent_account(LastLeader) of
        true ->
            PCPinTx = post_pin_to_pc(LastLeader, First),
            ProcState = State#state{pinning_mode = true,
                                    next_last = Last-1,
                                    pc_pin = PCPinTx,
                                    cc_note = false,
                                    last_leader = LastLeader },
            {noreply, ProcState};
        false ->
            lager:debug("no pin in epoch ~p, no parent account for ~p", [Epoch, LastLeader]),
            {noreply, State#state{pinning_mode = false}}
    end;
handle_info({gproc_ps_event, top_changed, #{info := #{height := Height}}},
            #state{pinning_mode = true,
                   contract = Contract,
                   next_last = Last,
                   pc_pin = PCPinTx,
                   cc_note = CCPosted,
                   last_leader = LastLeader } = State) ->
    NewCCPosted = maybe_post_pin_to_cc(PCPinTx, CCPosted, LastLeader, Height),
    PinningModeCont =
        case {Height, NewCCPosted} of
            {Last, true} -> post_pin_proof(Contract, PCPinTx, LastLeader, Last), false;
            {Last, false} -> false; % we're on last, pc pin tx not finalized, we bow out.
            _ -> true % not on last block, we continue triggering on top_changed
        end,
    {noreply, State#state{pinning_mode = PinningModeCont, cc_note = NewCCPosted}};
handle_info(_Info, LoopState) ->
    {noreply, LoopState}.

terminate(_Reason, _LoopState) ->
    ok.

code_change(_OldVsn, LoopState, _Extra) ->
    {ok, LoopState}.


%%%=============================================================================
%%% INTERNALS
%%%=============================================================================

post_pin_to_pc(LastLeader, Height) ->
    PCPinTx = aec_parent_connector:pin_to_pc(LastLeader, 1, 1000000 * min_gas_price()),
    lager:debug("Pinned to PC @~p: ~p", [Height, PCPinTx]),
    PCPinTx.

post_pin_pctx_to_cc(PinTx, LastLeader, Height) ->
    try
        aec_parent_connector:pin_tx_to_cc(PinTx, LastLeader, 1, 1000000 * min_gas_price()),
        lager:debug("noting on CC @~p", [Height])
    catch
        T:E -> lager:debug("Pin to CC failed: ~p:~p", [T,E]), ok
    end.

post_pin_proof(ContractPubkey, PinTx, LastLeader, Height) ->
    lager:debug("pin proof @~p ~p", [Height, PinTx]),
    aec_parent_connector:pin_contract_call(ContractPubkey, PinTx, LastLeader, 0, 1000000 * min_gas_price()).

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

min_gas_price() ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    max(aec_governance:minimum_gas_price(Protocol),
        aec_tx_pool:minimum_miner_gas_price()).
