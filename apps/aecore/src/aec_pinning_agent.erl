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

-include_lib("aecontract/include/aecontract.hrl").


%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    start_link/3,
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
    sign_module,
    next_last,
    pc_pin,
    cc_note,
    last_leader
}).
-type state() :: state.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link(term(), atom(), term()) -> {ok, pid()} | {error, {already_started, pid()}} | ignore | {error, Reason::any()}.
start_link(Contract, PinningBehavior, SignModule) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Contract, PinningBehavior, SignModule], []).

stop() ->
    gen_server:stop(?SERVER).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

init([Contract, PinningBehavior, SignModule]) ->
    case PinningBehavior of
        true ->
            State = #state{contract = Contract, pinning_mode = false, sign_module = SignModule},
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
                   sign_module = SignModule,
                   next_last = Last,
                   pc_pin = PCPinTx,
                   cc_note = CCPosted,
                   last_leader = LastLeader } = State) ->
    NewCCPosted = maybe_post_pin_to_cc(PCPinTx, CCPosted, LastLeader, Height, SignModule),
    PinningModeCont =
        case {Height, NewCCPosted} of
            {Last, true} -> post_pin_proof(Contract, PCPinTx, LastLeader, Last, SignModule), false;
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

post_pin_pctx_to_cc(PinTx, LastLeader, Height, SignModule) ->
    try
        pin_tx_to_cc(PinTx, LastLeader, 1, 1000000 * min_gas_price(), SignModule),
        lager:debug("noting on CC @~p", [Height])
    catch
        T:E -> lager:debug("Pin to CC failed: ~p:~p", [T,E]), ok
    end.

post_pin_proof(ContractPubkey, PinTx, LastLeader, Height, SignModule) ->
    lager:debug("pin proof @~p ~p", [Height, PinTx]),
    pin_contract_call(ContractPubkey, PinTx, LastLeader, 0, 1000000 * min_gas_price(), SignModule).

maybe_post_pin_to_cc(PCPinTx, false, LastLeader, Height, SignModule) ->
    case aec_parent_connector:get_pin_by_tx_hash(PCPinTx) of
        {ok, #{pc_height := -1}} -> false;
        {ok, _} -> post_pin_pctx_to_cc(PCPinTx, LastLeader, Height, SignModule), true;
        _ -> false
    end;
maybe_post_pin_to_cc(_, CCPosted, _, _, _) ->
    CCPosted.


%%%=============================================================================
%%% Helpers, communication
%%%=============================================================================

pin_tx_to_cc(PinTxHash, Who, Amount, Fee, SignModule) ->
    Nonce = get_local_nonce(Who),
    SpendTx = create_pin_tx_({Who, Who, Nonce, Amount, Fee, PinTxHash}),
    NetworkId = aec_governance:get_network_id(),
    SignedSpendTx = sign_tx(SpendTx, NetworkId, Who, SignModule),
    aec_tx_pool:push(SignedSpendTx, tx_received).

get_local_nonce(Who) ->
    case aec_next_nonce:pick_for_account(Who, max) of
        {ok, NextNonce} -> NextNonce;
        {error, account_not_found} -> 1
    end.

create_pin_tx_({SenderPubkey, ReceiverPubkey, Nonce, Amount, Fee, PinPayload}) ->
    TxArgs = #{ sender_id    => aeser_id:create(account, SenderPubkey),
                recipient_id => aeser_id:create(account, ReceiverPubkey),
                amount       => Amount,
                fee          => Fee,
                nonce        => Nonce,
                payload      => aeser_hc:encode_child_pin_payload(PinPayload)},
    {ok, SpendTx} = aec_spend_tx:new(TxArgs),
    SpendTx.

sign_tx(Tx, NetworkId, Signer, SignModule) when is_binary(Signer) ->
    Bin0 = aetx:serialize_to_binary(Tx),
    BinForNetwork = aec_governance:add_custom_network_id(NetworkId, Bin0),
    {ok, Signature} = SignModule:sign_binary(BinForNetwork, Signer),
    aetx_sign:new(Tx, [Signature]).

pin_contract_call(ContractPubkey, PinTx, Who, Amount, _Fee, SignModule) ->
    Nonce = get_local_nonce(Who),
    {ok, CallData} = aeb_fate_abi:create_calldata("pin", [{bytes, PinTx}]),
    ABI = ?ABI_FATE_SOPHIA_1,
    TxSpec =
        #{ caller_id   => aeser_id:create(account, Who),
            nonce       => Nonce,
            contract_id => aeser_id:create(contract, ContractPubkey),
            abi_version => ABI,
            fee         => 1000000 * min_gas_price(),
            amount      => Amount,
            gas         => 1000000,
            gas_price   => min_gas_price(),
            call_data   => CallData },
    {ok, Tx} = aect_call_tx:new(TxSpec),
    NetworkId = aec_governance:get_network_id(),
    SignedCallTx = sign_tx(Tx, NetworkId, Who, SignModule),
    aec_tx_pool:push(SignedCallTx, tx_received).

%% below code will be enabled in later PR
% find_spends_to(Account) ->
%     {ok, #{last := Last, first := First}} = aec_chain_hc:epoch_info(),
%     Blocks = aec_chain_hc:get_micro_blocks_between(First, Last-1),
%     lists:flatten([ pick_pin_spends_to(Account, aec_blocks:txs(B)) || B <- Blocks ]).


% pick_pin_spends_to(Account, Txs) ->
%     BareTxs = [ aetx_sign:tx(T) || T <- Txs],
%     InnerTxs = [ aetx:specialize_type(Tx) || Tx <- BareTxs,  spend_tx == aetx:tx_type(Tx)],
%     [ aec_spend_tx:payload(T) || {_, T} <- InnerTxs, aeser_id:specialize(aec_spend_tx:recipient_id(T)) == {account,Account}, is_pin(aec_spend_tx:payload(T))].


% is_pin(Pin) ->
%     case aeser_hc:decode_child_pin_payload(Pin) of
%         {error,_} -> false;
%         _ -> true
%     end.


min_gas_price() ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    max(aec_governance:minimum_gas_price(Protocol),
        aec_tx_pool:minimum_miner_gas_price()).
