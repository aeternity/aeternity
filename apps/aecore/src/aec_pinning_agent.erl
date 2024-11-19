%%%-----------------------------------------------------------------------------
%%% @doc
%%% Default pinning agent for hyperchains.
%%% Supported by the Aeternity Foundation
%%% @author mans.af.klercker@happihacking.se
%%% @end
%%%-----------------------------------------------------------------------------

-module(aec_pinning_agent).
-author("mans.af.klercker@happihacking.se").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    spawn_for_epoch/3,
    start/3
    ]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================


spawn_for_epoch(EpochInfo, Contract, LastLeader) ->
    lager:debug("AGENT: Trying to spawn pinning agent...", []),
    try
    case whereis(my_unique_process) of
        undefined ->
            Pid = spawn(aec_pinning_agent, start, [EpochInfo, Contract, LastLeader]),
            register(?MODULE, Pid),
            Pid;
        Pid when is_pid(Pid) ->
            lager:debug("AGENT: already started", []),
            ok
    end
    catch
        T:E -> lager:debug("AGENT: Broke!! ~p:~p", [T,E]), ok
    end.

start(EpochInfo, Contract, LastLeader) ->
    {ok,
    #{ first      := First
     , epoch      := _Epoch
     , length     := Length
     , validators := _Validators}} = EpochInfo,
    subscribe(),
    lager:debug("AGENT: started ~p", [self()]),
    wait_for_top_changed(First + Length - 2, none, false, Contract, LastLeader).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% FSM
%%%=============================================================================


subscribe() ->
    aec_events:subscribe(top_changed).

post_pin_to_pc(LastLeader, Height) ->
    lager:debug("AGENT: (~p) Time to send to PC ~p", [self(), Height]),
    PCPinTx = aec_parent_connector:pin_to_pc(LastLeader, 1, 1000000 * min_gas_price()),
    lager:debug("AGENT: (~p) pc-pinned ~p", [self(), PCPinTx]),
    PCPinTx.

post_pin_pctx_to_cc(PinTx, LastLeader, Height) ->
    lager:debug("AGENT: (~p) noting on CC @~p", [self(), Height]),
    try
        aec_parent_connector:pin_tx_to_cc(PinTx, LastLeader, 1, 1000000 * min_gas_price())
    catch
        T:E -> lager:debug("CRASHHHH: ~p:~p", [T,E]), ok
    end.

post_pin_proof(ContractPubkey, PinTx, LastLeader, Height) ->
    lager:debug("AGENT: (~p) pin proof @~p ~p", [self(), Height, PinTx]),
    aec_parent_connector:pin_contract_call(ContractPubkey, PinTx, LastLeader, 0, 1000000 * min_gas_price()).

wait_for_top_changed(Last, none, _, Contract, LastLeader) -> % no pin on PC yet, then we post one once the next CC block is done
    receive
        {gproc_ps_event, top_changed, #{info := #{ height := Height }}} ->
            PCPinTx = post_pin_to_pc(LastLeader, Height),
            wait_for_top_changed(Last, PCPinTx, false, Contract, LastLeader)
    end;
wait_for_top_changed(Last, PCPinTx, false, Contract, LastLeader) -> %% if PC pin tx done, post to CC if it's on PC
    receive
        {gproc_ps_event, top_changed, #{info := Info}} ->
            CCPosted = % always try to post to CC if we haven't
                case aec_parent_connector:get_pin_by_tx_hash(PCPinTx) of
                    {ok, #{pc_height := -1}} -> % Not on PC yet, let's wait for next PC generation
                        false;
                    {ok,_} -> % it's on PC
                        post_pin_pctx_to_cc(PCPinTx, LastLeader, maps:get(height, Info)), true;
                    _ -> false
                end,
            case Info of
                #{ height := Last } when CCPosted == true -> % if we posted to CC and we're on last, post proof and die
                    post_pin_proof(Contract, PCPinTx, LastLeader, Last);
                #{ height := Last } when CCPosted == false ->
                    ok; % just exit
                _ -> % otherwise, we just carry on...
                    wait_for_top_changed(Last, PCPinTx, CCPosted, Contract, LastLeader)
            end
    end;
wait_for_top_changed(Last, PCPinTx, true, Contract, LastLeader) ->
    receive
        {gproc_ps_event, top_changed, #{info := Info}} ->
            case Info of
                #{ height := Last } -> % we're on last, post proof and die
                    post_pin_proof(Contract, PCPinTx, LastLeader, Last);
                _ -> % something else, we just carry on...
                    wait_for_top_changed(Last, PCPinTx, true, Contract, LastLeader)
            end
    end.

%%%=============================================================================
%%% Helpers, communication
%%%=============================================================================

min_gas_price() ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    max(aec_governance:minimum_gas_price(Protocol),
        aec_tx_pool:minimum_miner_gas_price()).
