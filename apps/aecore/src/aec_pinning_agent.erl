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
    spawn_for_epoch/1,
    start/1
    ]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================


spawn_for_epoch(Epoch) ->
    try
    case whereis(my_unique_process) of
        undefined ->
            Pid = spawn(aec_pinning_agent, start, [Epoch]),
            register(my_unique_process, Pid),
            Pid;
        Pid when is_pid(Pid) ->
            ok
    end
    catch
        T:E -> lager:debug("AGENT: Broke!! ~p:~p", [T,E]), ok
    end.

start(EpochInfo) ->
    {ok,
    #{ first      := First
     , epoch      := _Epoch
     , length     := Length
     , validators := _Validators}} = EpochInfo,
    subscribe(),
    lager:debug("AGENT: started ~p", [self()]),
    wait_for_top_changed(First + Length - 2, none, false).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

subscribe() ->
    aec_events:subscribe(top_changed).

post_pin_to_pc() ->
    <<"mock">>. %% the tx

post_pin_pctx_to_cc(_PinProof) ->
    ok.

post_pin_proof(_Height) ->
    ok.

wait_for_top_changed(Last, none, _) -> % no pin on PC yet, then we post one once the next CC block is done
    receive
        {gproc_ps_event, top_changed, #{info := #{ height := Height }}} ->
            lager:debug("AGENT (~p): Time to send to PC ~p", [self(), Height]),
            PcPinTx = post_pin_to_pc(),
            wait_for_top_changed(Last, PcPinTx, false)
    end;
wait_for_top_changed(Last, PCPinTx, false) -> %% if PC pin tx done, post to CC if it's on PC
    receive
        {gproc_ps_event, top_changed, #{info := Info}} ->
            CCPosted = % always try to post to CC if we haven't
                %case parent_connector:get_pin_by_tx_hash(PCPinTx) of
                case mock_pc() of
                    {ok,_} -> % it's on PC
                        lager:debug("AGENT (~p): noting on CC", [self()]),
                        post_pin_pctx_to_cc(PCPinTx), true;
                    {error, _} -> % Not on PC yet, let's wait for next PC generation
                        false
                end,
            case Info of
                #{ height := Last } when CCPosted -> % if we posted to CC and we're on last, post proof and die
                    post_pin_proof(PCPinTx);
                _ -> % sotherwise, we just carry on...
                    wait_for_top_changed(Last, PCPinTx, true)
            end
    end;
wait_for_top_changed(Last, PCPinTx, true) ->
    receive
        {gproc_ps_event, top_changed, #{info := Info}} ->
            case Info of
                #{ height := Last } -> % we're on last, post proof and die
                    lager:debug("AGENT (~p): pin proof ~p", [self(), Last]),
                    post_pin_proof(PCPinTx);
                _ -> % something else, we just carry on...
                    wait_for_top_changed(Last, PCPinTx, true)
            end
    end.

mock_pc() -> {ok, ok}.
