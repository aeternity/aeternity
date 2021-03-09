%% @doc This modules implements the State Channel FSM and its external API. The
%% FSM is used for both state channel roles, 'responder' and 'initiator',
%% because most logic is shared with the initialization being specific to the
%% role.
%%
%% The presumption of the fsm is that only the client possesses the signing
%% capability. Therefore, anytime a request needs co-signing, a request is
%% sent to the client. This complicates the state machine significantly, but
%% the hope is that the client SDK can be greatly simplified in turn.
%%
%% If the client (presumably Websocket client) disconnects, the fsm will keep
%% running, but will not be able to request co-signing. If, however, the peer
%% presents requests that are already co-signed, the fsm will fake the signing
%% reply and proceed as normal. How the 'out-of-band' co-signing is achieved is
%% beyond the scope of this description, but it could e.g. happen similar to
%% air-gapped signing. The checks for this happen in the request_signing() function.
%%
%% @reference See the design of
%% <a href="https://github.com/aeternity/protocol/tree/master/channels">State Channels</a>
%% for further information.
%% @reference See the
%% <a href="https://github.com/aeternity/aeternity/wiki/State-Channels#fsm">state
%% diagrams</a> for a conceptual representation of this implementation.
-module(aesc_fsm).

-behaviour(gen_statem).

-compile({inline,[get_state_implementation/2]}).

%% API
-export([ start_link/1
        , attach_responder/2      %% (fsm(), map())
        , close_solo/2            %% (fsm(), map())
        , snapshot_solo/2         %% (fsm(), map())
        , reconnect_client/2      %% (fsm(), signed_tx())
        , reconnect_client/3      %% (fsm(), Client :: pid(), signed_tx())
        , connection_died/1       %% (fsm())
        , get_contract/2
        , get_contract_call/4     %% (fsm(), contract_id(), caller(), round())
        , get_offchain_state/1
        , get_poi/2
        , inband_msg/3
        , leave/1
        , settle/2                %% (fsm(), map())
        , shutdown/2              %% (fsm(), map())
        , slash/2                 %% (fsm(), map())
        , force_progress/2        %% (fsm(), map())
        , upd_call_contract/2     %% (fsm(), map())
        , upd_create_contract/2   %% (fsm(), map())
        , upd_deposit/2           %% (fsm() , map())
        , upd_transfer/4          %% (fsm() , from(), to(), amount())
        , upd_transfer/5          %% (fsm() , from(), to(), amount(), #{})
        , upd_withdraw/2          %% (fsm() , map())
        , where/2
        ]).

%% Inspection and configuration functions
-export([ change_config/3      %% (fsm(), key(), value()) -> ok | {error,_}
        , dry_run_contract/2   %% (fsm(), map()) -> {ok, call()} | {error, _}
        , get_balances/2       %% (fsm(), {ok, [key()]) -> [{key(), amount()}]} | {error, _}
        , get_history/1        %% (fsm()) -> [Event]
        , get_history/2        %% (fsm(), Opts) -> [Event]
        , get_round/1          %% (fsm()) -> {ok, round()} | {error, _}
        , patterns/0
        , prune_local_calls/1  %% (fsm()) -> {ok, round()} | {error, _}
        , record_fields/1
        , report_tags/0
        , timeouts/0
        , bh_deltas/0
        ]).

%% Used by noise session
-export([ message/2
        , noise_connected/1]).

%% Used by client
-export([ signing_response/3          %% (Fsm, Tag, Obj)
        , error_code_to_msg/1         %% (Code)
        , stop/1 ]).                  %% (Fsm)

%% Used by min-depth watcher
-export([ minimum_depth_achieved/4      %% (Fsm, OnChainId, Type, TxHash)
        , channel_changed_on_chain/2    %% (Fsm, Info)
        , channel_closing_on_chain/2
        , channel_closed_on_chain/2     %% (Fsm, Info)
        , channel_unlocked/2
        ]). %% (Fsm, Info)

%% gen_statem callbacks
-export([ init/1
        , callback_mode/0
        , code_change/4
        , terminate/3
        ]).

%% FSM states (as per gen_statem callback callback_mode/0)
-export([ initialized/3
        , accepted/3
        , awaiting_leave_ack/3
        , awaiting_locked/3
        , awaiting_connect/3
        , awaiting_open/3
        , awaiting_reestablish/3
        , awaiting_signature/3
        , awaiting_update_ack/3
        , channel_closed/3
        , channel_closing/3   %% on-chain closing has been detected
        , dep_half_signed/3
        , dep_signed/3
        , half_signed/3
        , mutual_closing/3
        , mutual_closed/3
        , open/3
        , reestablish_init/3
        , signed/3
        , wdraw_half_signed/3
        , wdraw_signed/3
        ]).

-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aeutils/include/aeu_stacktrace.hrl").
-include("aesc_codec.hrl").
-include("aechannel.hrl").
-include("aesc_fsm.hrl").

%% mainly to avoid warnings during build
-export_type([ opts/0
             , state_name/0
             , data/0
             ]).

%% -include_lib("trace_runner/include/trace_runner.hrl").

-ifdef(TEST).
-export([ strict_checks/2
        , get_state/1
        , pr_stacktrace/1
        ]).
-endif.

%% ==================================================================
%% API

start_link(#{} = Arg) ->
    gen_statem:start_link(?MODULE, Arg, ?GEN_STATEM_OPTS).

attach_responder(Fsm, AttachInfo) when is_map(AttachInfo) ->
    case has_gproc_key(Fsm, AttachInfo) of
        true ->
            gen_statem:call(Fsm, {attach_responder, AttachInfo});
        false ->
            lager:debug("responder ~p has unregistered - taken", [Fsm]),
            {error, taken}
    end.

-spec close_solo(pid(), #{fee := integer()}) -> ok | {error, atom()}.
close_solo(Fsm, XOpts) ->
    lager:debug("close_solo(~p, ~p)", [Fsm, XOpts]),
    gen_statem:call(Fsm, {close_solo, XOpts}).

-spec snapshot_solo(pid(), #{fee := integer()}) -> ok | {error, atom()}.
snapshot_solo(Fsm, XOpts) ->
    lager:debug("snapshot_solo(~p, ~p)", [Fsm, XOpts]),
    gen_statem:call(Fsm, {snapshot_solo, XOpts}).

-spec reconnect_client(pid(), aesc_fsm_id:wrapper()) -> ok | {error, any()}.
reconnect_client(Fsm, FsmIdWrapper) ->
    reconnect_client(Fsm, self(), FsmIdWrapper).

-spec reconnect_client(pid(), pid(), aesc_fsm_id:wrapper()) -> ok | {error, any()}.
reconnect_client(Fsm, Pid, FsmIdWrapper) when is_pid(Pid) ->
    gen_statem:call(Fsm, {?RECONNECT_CLIENT, Pid, FsmIdWrapper}).

connection_died(Fsm) ->
    %TODO: possibility for reconnect
    lager:debug("connection to participant died(~p)", [Fsm]),
    gen_statem:cast(Fsm, ?DISCONNECT).

-spec get_contract(pid(), aect_contracts:pubkey()) -> {ok, aect_contracts:contract()} | {error, not_found}.
get_contract(Fsm, Pubkey) ->
    gen_statem:call(Fsm, {get_contract, Pubkey}).

get_contract_call(Fsm, Contract, Caller, Round) when is_integer(Round), Round > 0 ->
    lager:debug("get_contract_call(~p, ~p, ~p)", [Contract, Caller, Round]),
    gen_statem:call(Fsm, {get_contract_call, Contract, Caller, Round}).

-spec get_offchain_state(pid()) -> {ok, aesc_offchain_state:state()}.
get_offchain_state(Fsm) ->
    gen_statem:call(Fsm, get_offchain_state).

-spec get_poi(pid(), list()) -> {ok, aec_trees:poi()} | {error, not_found}.
get_poi(Fsm, Filter) ->
    gen_statem:call(Fsm, {get_poi, Filter}).

-ifdef(TEST).
-spec get_state(pid()) -> {ok, #{}}.
get_state(Fsm) ->
    gen_statem:call(Fsm, get_state).
-endif.

inband_msg(Fsm, To, Msg) ->
    MaxSz = aesc_codec:max_inband_msg_size(),
    try iolist_to_binary(Msg) of
        MsgBin when byte_size(MsgBin) =< MaxSz ->
            lager:debug("inband_msg(~p, ~p, ~p)", [Fsm, To, Msg]),
            gen_statem:call(Fsm, {inband_msg, To, MsgBin});
        MsgBin ->
            lager:debug("INVALID inband_msg(~p): ~p", [Fsm, byte_size(MsgBin)]),
            {error, invalid_request}
    ?CATCH_LOG(_E)
        {error, invalid_request}
  end.

leave(Fsm) ->
    lager:debug("leave(~p)", [Fsm]),
    gen_statem:call(Fsm, leave).

-spec settle(pid(), #{fee := integer()}) -> ok | {error, atom()}.
settle(Fsm, XOpts) ->
    lager:debug("settle(~p, ~p)", [Fsm, XOpts]),
    gen_statem:call(Fsm, {settle, XOpts}).

-spec shutdown(pid(), #{fee := integer()}) -> ok | {error, atom()}.
shutdown(Fsm, XOpts) ->
    lager:debug("shutdown(~p, ~p)", [Fsm, XOpts]),
    gen_statem:call(Fsm, {shutdown, XOpts}).

-spec slash(pid(), #{fee := integer()}) -> ok | {error, atom()}.
slash(Fsm, XOpts) ->
    lager:debug("slash(~p, ~p)", [Fsm, XOpts]),
    gen_statem:call(Fsm, {slash, XOpts}).

upd_call_contract(Fsm, #{contract    := _,
                         abi_version := _,
                         amount      := Amt,
                         call_data   := _} = Opts) when is_integer(Amt), Amt >= 0 ->
    lager:debug("upd_call_contract(~p)", [Opts]),
    CallStack = maps:get(call_stack, Opts, []),
    gen_statem:call(Fsm, {upd_call_contract, Opts#{call_stack => CallStack},
                          execute}).

force_progress(Fsm, #{ contract    := _
                     , abi_version := _
                     , amount      := Amt
                     , call_data   := _
                     , gas         := Gas
                     , gas_price   := GasPrice } = Opts)
    when is_integer(Amt), Amt >= 0, is_integer(Gas), Gas >= 0,
         is_integer(GasPrice), GasPrice >= 0 ->
    lager:debug("force_progress(~p)", [Opts]),
    CallStack = maps:get(call_stack, Opts, []),
    gen_statem:call(Fsm, {force_progress, Opts#{call_stack => CallStack}}).

upd_create_contract(Fsm, #{vm_version  := _,
                           abi_version := _,
                           deposit     := Amt,
                           code        := _,
                           call_data   := _} = Opts) when is_integer(Amt), Amt >= 0 ->
    lager:debug("upd_create_contract(~p)", [Opts]),
    gen_statem:call(Fsm, {upd_create_contract, Opts}).

upd_deposit(_Fsm, #{amount := Amt}) when Amt < 0 ->
    {error, negative_amount};
upd_deposit(Fsm, #{amount := Amt} = Opts) when is_integer(Amt) ->
    lager:debug("upd_deposit(~p)", [Opts]),
    gen_statem:call(Fsm, {upd_deposit, Opts}).

upd_transfer(Fsm, From, To, Amount) ->
    upd_transfer(Fsm, From, To, Amount, #{}).

upd_transfer(_Fsm, _From, _To, Amount, _Opts) when Amount < 0 ->
    {error, negative_amount};
upd_transfer(Fsm, From, To, Amount, Opts) when is_integer(Amount), is_map(Opts) ->
    lager:debug("upd_transfer(~p, ~p, ~p, ~p, ~p)", [Fsm, From, To, Amount, Opts]),
    gen_statem:call(Fsm, {upd_transfer, From, To, Amount, Opts}).

upd_withdraw(_Fsm, #{amount := Amt}) when Amt < 0 ->
    {error, negative_amount};
upd_withdraw(Fsm, #{amount := Amt} = Opts) when is_integer(Amt) ->
    lager:debug("upd_withdraw(~p)", [Opts]),
    gen_statem:call(Fsm, {upd_withdraw, Opts}).

where(ChanId, Role) when Role == initiator; Role == responder ->
    GprocKey = gproc_name_by_role(ChanId, Role),
    case gproc:where(GprocKey) of
        Pid when is_pid(Pid) ->
            Pubkey = gproc:lookup_value(GprocKey),
            #{ fsm_pid => Pid, role => Role, pub_key => Pubkey };
        undefined ->
            undefined
    end.

%% ==================================================================
%% Inspection and configuration functions

patterns() ->
    [{gen_tcp, F, A, []} || {F,A} <- gen_tcp:module_info(exports),
                            F =/= module_info] ++
    [{?MODULE, F, A, []} || {F,A} <- ?MODULE:module_info(exports),
                            not lists:member(F, [module_info, patterns])].

record_fields(data ) -> record_info(fields, data);
record_fields(w    ) -> aesc_window:record_fields(w);
record_fields(Other) -> aesc_offchain_state:record_fields(Other).

report_tags() ->
    maps:keys(?DEFAULT_REPORT_FLAGS).

bh_deltas() ->
    record_info(fields, bh_delta).

timeouts() ->
    maps:keys(?DEFAULT_TIMEOUTS).

%% @doc Fetch the list of recent fsm events (sliding window)
get_history(Fsm) ->
    get_history(Fsm, #{}).

get_history(Fsm, Opts) when is_map(Opts) ->
    lager:debug("get_history(~p)", [Fsm]),
    gen_statem:call(Fsm, {get_history, Opts}).

change_config(Fsm, Key, Value) ->
    case check_change_config(Key, Value) of
        {ok, Key1, Val1} ->
            gen_statem:call(Fsm, {change_config, Key1, Val1});
        {error, _} = Error ->
            Error
    end.

%% @doc Returns a list of [{Pubkey, Balance}], where keys are ordered the same
%% way as in Accounts. If a key doesn't correspond to an existing account, it
%% doesn't show up in the result. Thus, unknown accounts are recognized by
%% their absense in the response.
get_balances(Fsm, Accounts) ->
    gen_statem:call(Fsm, {get_balances, Accounts}).

get_round(Fsm) ->
    gen_statem:call(Fsm, get_round).

prune_local_calls(Fsm) ->
    gen_statem:call(Fsm, prune_local_calls).

dry_run_contract(Fsm, #{ contract    := _
                       , abi_version := _
                       , amount      := Amt
                       , call_data   := _ } = Opts) when is_integer(Amt), Amt >= 0 ->
    gen_statem:call(Fsm, {upd_call_contract, Opts, dry_run}).

%% ==================================================================
%% Used by noise session

message(Fsm, {T, _} = Msg) when ?KNOWN_MSG_TYPE(T) ->
    lager:debug("message(~p, ~p)", [Fsm, Msg]),
    gen_statem:cast(Fsm, Msg).

noise_connected(Fsm) ->
    gen_statem:cast(Fsm, {noise_connected, self()}).

%% ==================================================================
%% Used by client

%% @doc Signing requests are sent as plain messages to the Client (normally,
%% the process starting the fsm.) Messages are on the form
%%
%% {aesc_fsm, Fsm :: pid(), ChanId :: binary(), Msg}
%%   where Msg :: {sign, Tag :: sign_tag(), Obj :: any()}}.
%%
%% Perform the appropriate signing
%% ( corresponding to aetx_sign:sign(Obj, [PrivKey]), but likely involving a
%%   remote client. )
%% and reply by calling aesc_fsm:signing_response(Fsm, Tag, SignedObj)
%%
-spec signing_response(pid(), sign_tag(), aetx_sign:signed_tx()
                                        | {error, integer()}) -> ok | {error, atom()}.
signing_response(Fsm, Tag, {error, Code}) ->
    gen_statem:call(Fsm, {abort_update, Code, Tag});
signing_response(Fsm, Tag, SignedTx) ->
    gen_statem:cast(Fsm, {?SIGNED, Tag, SignedTx}).

error_code_to_msg(?ERR_VALIDATION       ) -> <<"validation error">>;
error_code_to_msg(?ERR_CONFLICT         ) -> <<"conflict">>;
error_code_to_msg(?ERR_TIMEOUT          ) -> <<"timeout">>;
error_code_to_msg(?ERR_ABORT            ) -> <<"abort">>;
error_code_to_msg(?ERR_ONCHAIN_REJECTED ) -> <<"rejected on-chain">>;
error_code_to_msg(Code) when Code >= ?ERR_USER ->
    <<"user-defined">>;
error_code_to_msg(_) ->
    <<"unknown">>.

-spec stop(pid()) -> ok.
stop(Fsm) ->
    lager:debug("Ungracefully stopping the FSM ~p", [Fsm]),
    MRef = erlang:monitor(process, Fsm),
    try_stop(Fsm, 3, MRef).

try_stop(Fsm, Retries, MRef) ->
    BT = process_info(Fsm, backtrace),
    stop_ok(catch gen_statem:stop(Fsm, shutdown, 5000), Fsm, Retries, MRef, BT).

%% TODO
%% This function is currently exported in the WS API, and used by the aehtt_sc_SUITE.
%% This should be addressed. We really don't want this kind of stop method exported.
stop_ok(ok, _, _, MRef, _) ->
    erlang:demonitor(MRef),
    ok;
stop_ok({'EXIT', noproc}, _, _, MRef, _) ->
    erlang:demonitor(MRef),
    ok;
stop_ok({'EXIT', {normal,{sys,terminate,_}}}, _, _, MRef, _) ->
    erlang:demonitor(MRef),
    ok;
stop_ok({'EXIT', {disconnect,{sys,terminate,_}}}, _, _, MRef, _) ->
    erlang:demonitor(MRef),
    ok;
stop_ok({'EXIT', timeout}, Fsm, Retries, MRef, BT) when Retries > 0 ->
    BT1 = process_info(Fsm, backtrace),
    lager:debug("Timed out stopping fsm (~p): ~p", [Fsm, process_info(Fsm)]),
    lager:debug("Trace0 = ~s", [element(2, BT)]),
    lager:debug("Trace1 = ~s", [element(2, BT1)]),
    timer:sleep(100),
    try_stop(Fsm, Retries-1, MRef);
stop_ok({'EXIT', timeout}, Fsm, _, MRef, BT) ->
    lager:debug("Killing fsm (~p) ...", [Fsm]),
    lager:debug("Trace = ~s", [element(2, BT)]),
    exit(Fsm, kill),
    receive
       {'DOWN', MRef, _, _, _} ->
            ok
    end.

%% ==================================================================
%% Used by min-depth watcher

minimum_depth_achieved(Fsm, ChanId, Type, TxHash) ->
    gen_statem:cast(Fsm, {?MIN_DEPTH_ACHIEVED, ChanId, Type, TxHash}).

channel_changed_on_chain(Fsm, Info) ->
    gen_statem:cast(Fsm, {?CHANNEL_CHANGED, Info}).

channel_closing_on_chain(Fsm, Info) ->
    gen_statem:cast(Fsm, {?CHANNEL_CLOSING, Info}).

channel_closed_on_chain(Fsm, Info) ->
    gen_statem:cast(Fsm, {?CHANNEL_CLOSED, Info}).

channel_unlocked(Fsm, Info) ->
    lager:debug("sending unlocked", []),
    gen_statem:cast(Fsm, {?CHANNEL_UNLOCKED, Info}).

%% NOTE: we currently double-book Role in both the #data{} record and
%% the Opts map. This is a bit annoying, but simplifies the pattern-matching
%% in this module (the #data{} part); also, the aesc_offchain_state module
%% needs Role for the cache interaction. Future refactoring can resolve this
%% slight code smell.

-ifdef(TEST).
%% This is supposed to be used in tests only. If Alice stops checking
%% her own signatures, Bob will still check hers and will reject any wrongly
%% signed txs. There is a risk for Alice corrupting her own state if using
%% this function
-spec strict_checks(pid(), boolean()) -> ok.
strict_checks(Fsm, Strict) when is_boolean(Strict) ->
    gen_statem:call(Fsm, {strict_checks, Strict}).
-endif.

%% ======================================================================
%% FSM states

initialized(enter, _OldSt, _D) -> keep_state_and_data;
initialized(cast, {?CH_ACCEPT, Msg}, #data{role = initiator} = D) ->
    case check_accept_msg(Msg, D) of
        {ok, D1} ->
            lager:debug("Valid channel_accept: ~p", [Msg]),
            gproc_register(D1),
            D2 = report(info, channel_accept, D1),
            {ok, CTx, Updates, BlockHash} = create_tx_for_signing(D2),
            case request_signing(create_tx, CTx, Updates, BlockHash, D2) of
                {ok, D3, Actions} ->
                    next_state(awaiting_signature, D3, Actions);
                {error, _} = Error ->
                    close(Error, D2)
            end;
        {error, _} = Error ->
            close(Error, D)
    end;
initialized(Type, Msg, D) ->
    handle_common_event(Type, Msg, postpone_or_error(Type), D).

accepted(enter, _OldSt, _D) -> keep_state_and_data;
accepted(cast, {?FND_CREATED, Msg}, #data{role = responder} = D) ->
    case check_funding_created_msg(Msg, D) of
        {ok, SignedTx, Updates, BlockHash, D1} ->
            try
            D2 = report(info, funding_created, D1),
            lager:debug("funding_created: ~p", [SignedTx]),
            case request_signing_(?FND_CREATED, SignedTx, Updates, BlockHash, D2) of
                {ok, D3, Actions} ->
                    next_state(awaiting_signature, D3, Actions);
                {error, _} = Error ->
                    close(Error, D2)
            end
            ?CATCH_LOG(_E)
              error(_E)
            end;
        {error, Error} ->
             close(Error, D)
    end;
accepted(Type, Msg, D) ->
    handle_common_event(Type, Msg, postpone_or_error(Type), D).

awaiting_leave_ack(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_leave_ack(cast, {?LEAVE_ACK, Msg}, D) ->
    lager:debug("received leave_ack", []),
    case check_leave_ack_msg(Msg, D) of
        {ok, D1} ->
            D2 = report_leave(D1),
            close(leave, D2);
        {error, _} = Err ->
            close(Err, D)
    end;
awaiting_leave_ack(cast, {?LEAVE, Msg}, D) ->
    case check_leave_msg(Msg, D) of
        {ok, D1} ->
            D2 = send_leave_ack_msg(D1),
            D3 = report_leave(D2),
            close(leave, D3);
        {error, _} = Err ->
            close(Err, D)
    end;
awaiting_leave_ack(Type, Msg, D) ->
    handle_common_event(Type, Msg, postpone, D).

awaiting_locked(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_locked(cast, {?MIN_DEPTH_ACHIEVED, ChainId, ?WATCH_FND, TxHash},
                #data{op = #op_min_depth{ tag = ?WATCH_FND
                                        , tx_hash = TxHash % same tx hash
                                        , data = OpData}} = D) ->
    D1 = report(info, own_funding_locked, D),
    next_state(
      signed, send_funding_locked_msg(D1#data{ on_chain_id = ChainId
                                             , op = #op_lock{ tag = create
                                                            , data = OpData}}));
awaiting_locked(cast, {?MIN_DEPTH_ACHIEVED, ChainId, ?WATCH_DEP, TxHash},
                #data{ on_chain_id = ChainId
                     , op = #op_min_depth{ tag = ?WATCH_DEP
                                         , tx_hash = TxHash % same tx hash
                                         , data = OpData}} = D) ->
    D1 = report(info, own_deposit_locked, D),
    next_state(dep_signed,
        send_deposit_locked_msg(TxHash, D1#data{op = #op_lock{ tag = deposit
                                                             , data = OpData}}));
awaiting_locked(cast, {?MIN_DEPTH_ACHIEVED, ChainId, ?WATCH_WDRAW, TxHash},
                #data{ on_chain_id = ChainId
                     , op = #op_min_depth{ tag = ?WATCH_WDRAW
                                         , tx_hash = TxHash % same tx hash
                                         , data = OpData}} = D) ->
    D1 = report(info, own_withdraw_locked, D),
    next_state(wdraw_signed,
        send_withdraw_locked_msg(TxHash, D1#data{ op = #op_lock{ tag = withdraw
                                                               , data = OpData}}));
awaiting_locked(cast, {?FND_LOCKED, _Msg}, D) ->
    postpone(D);
awaiting_locked(cast, {?DEP_LOCKED, _Msg}, D) ->
    postpone(D);
awaiting_locked(cast, {?WDRAW_LOCKED, _Msg}, D) ->
    postpone(D);
awaiting_locked(cast, {?DEP_ERR, #{error_code := ?ERR_ONCHAIN_REJECTED} = _Msg},
                #data{op = #op_min_depth{tag = ?WATCH_DEP}} = D) ->
    %% TODO: try posting the co-signed tx
    {ok, D1} = fall_back_to_stable_state(D),
    handle_recoverable_error(#{ code => ?ERR_ONCHAIN_REJECTED
                              , msg_type => ?DEP_SIGNED
                              , respond => false}, D1);
awaiting_locked(cast, {?WDRAW_ERR, #{error_code := ?ERR_ONCHAIN_REJECTED} = _Msg},
                #data{op = #op_min_depth{tag = ?WATCH_WDRAW}} = D) ->
    %% TODO: try posting the co-signed tx
    {ok, D1} = fall_back_to_stable_state(D),
    handle_recoverable_error(#{ code => ?ERR_ONCHAIN_REJECTED
                              , msg_type => ?WDRAW_SIGNED
                              , respond => false}, D1);
%% Our FSM is in cheating  mode but the other party rightfully refused the
%% update. Rollback to correct state
awaiting_locked(cast, {?DEP_ERR, _}, #data{strict_checks = false} = D) ->
    handle_recoverable_error(#{ code => ?ERR_CONFLICT
                              , respond => false
                              , msg_type => ?DEP_SIGNED }, D, []);
%% Our FSM is in cheating  mode but the other party rightfully refused the
%% update. Rollback to correct state
awaiting_locked(cast, {?WDRAW_ERR, _}, #data{strict_checks = false} = D) ->
    handle_recoverable_error(#{ code => ?ERR_CONFLICT
                              , respond => false
                              , msg_type => ?WDRAW_SIGNED }, D, []);
awaiting_locked(cast, {Error, _} = Msg, #data{ state = State} = D)
    when Error =:= ?DEP_ERR;
         Error =:= ?WDRAW_ERR ->
    lager:warning("Discarding ~p in state: ~p", [Msg, State]),
    keep_state(log(drop, msg_type(Msg), Msg, D));
awaiting_locked(cast, {?CHANNEL_CHANGED, _Info} = Msg, D) ->
    handle_common_event(cast, Msg, error, D);
awaiting_locked(Type, Msg, D) ->
    lager:debug("Unexpected ~p: Msg = ~p, Op = ~p", [Type, Msg, D#data.op]),
    handle_common_event(Type, Msg, error, D).

awaiting_connect(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_connect(cast, {noise_connected, Pid}, #data{ peer_connected = false
                                                    , role    = initiator
                                                    , session = Pid
                                                    , state   = State
                                                    , op      = Op } = D) ->
    lager:debug("Op = ~p", [Op]),
    D1 = D#data{op = ?NO_OP, peer_connected = true},
    case Op of
        #op_reestablish{mode = {connect, ReestablishOpts}} ->
            {_Round, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
            next_state(reestablish_init,
                       send_reestablish_msg(ReestablishOpts#{offchain_tx => SignedTx},
                                            D1));
        ?NO_OP ->
            next_state(initialized, send_open_msg(D1))
    end;
awaiting_connect(Type, Msg, D) ->
    handle_common_event(Type, Msg, postpone, D).

awaiting_open(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_open(cast, {?CH_OPEN, Msg}, #data{role = responder} = D) ->
    case check_open_msg(Msg, D) of
        {ok, D1} ->
            D2 = report(info, channel_open, D1),
            gproc_register(D2),
            next_state(accepted, send_channel_accept(D2));
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_open({call, From}, {attach_responder, _Info}, #data{channel_status = attached} = D) ->
    keep_state(D, [{reply, From, {error, taken}}]);
awaiting_open({call, From}, {attach_responder, Info}, #data{opts = Opts} = D) ->
    lager:debug("Attach request: ~p", [Info]),
    #{initiator := I, responder := R} = Opts,
    case check_attach_info(Info, I, R, D) of
        ok ->
            #{ initiator := I1
             , port      := _Port
             , gproc_key := K } = Info,
            gproc:unreg(K),
            Opts1 = Opts#{initiator => I1},
            {Pid, _} = From,
            keep_state(maybe_initialize_offchain_state(
                         I, I1, D#data{ session = Pid
                                      , opts = Opts1, channel_status = attached}),
                       [{reply, From, ok}]);
        {error, Err} ->
            lager:debug("Bad attach info: ~p", [Err]),
            lager:debug("My regs: ~p", [gproc:info(self(), gproc)]),
            keep_state(D, [{reply, From, {error, invalid_attach}}])
    end;
awaiting_open(Type, Msg, D) ->
    handle_common_event(Type, Msg, error_all, D).

awaiting_reestablish(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_reestablish(cast, {?CH_REESTABL, Msg}, #data{ role = responder
                                                     , op = Op} = D) ->
    case check_reestablish_msg(Msg, D) of
        {ok, D1} ->
            D2 = report(info, channel_reestablished, D1),
            lager:debug("Op = ~p", [Op]),
            D3 = case Op#op_reestablish.mode of
                     restart ->
                         lager:debug("re-register and restart watcher", []),
                         gproc_register(D2),
                         watcher_reg(D2#data.on_chain_id),
                         D2;
                     remain ->
                         lager:debug("Already running - don't re-register", []),
                         D2
                 end,
            next_state(open, send_reestablish_ack_msg(D3));
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_reestablish({call, From},
                     {attach_responder, #{ reestablish := true
                                         , gproc_key   := K } = Info},
                     #data{ role        = responder } = D) ->
    lager:debug("Attach request: ~p", [Info]),
    case check_attach_info(Info, D) of
        ok ->
            gproc:unreg(K),
            {Pid, _} = From,
            keep_state(D#data{ session = Pid }, [{reply, From, ok}]);
        {error, Error} ->
            lager:debug("Attach failed with ~p", [Error]),
            keep_state(D, [{reply, From, {error, invalid_attach}}])
    end;
awaiting_reestablish(Type, Msg, D) ->
    handle_common_event(Type, Msg, postpone_or_error(Type), D).

awaiting_signature(enter, _OldSt, _D) ->
    keep_state_and_data;
awaiting_signature(cast, {?SIGNED, Tag, {error, Code}} = Msg,
                   #data{op = #op_sign{ tag = Tag }} = D) ->
    lager:debug("Error code as signing reply (Tag = ~p): ~p", [Tag, Code]),
    D1 = log(rcv, msg_type(Msg), Msg, D),
    handle_recoverable_error(#{code => Code, msg_type => Tag}, D1);
awaiting_signature(cast, {Req, _} = Msg, #data{ ongoing_update = true
                                              , op = Op} = D)
  when ?UPDATE_CAST(Req) ->
    case is_solo_tx_from_op(Op) of
        false ->
            %% Race detection!
            lager:debug("race detected: ~p", [Msg]),
            handle_update_conflict(Req, D);
        true ->
            postpone(D)
    end;
awaiting_signature(cast, {?SIGNED, slash_tx, SignedTx} = Msg,
                   #data{op = #op_sign{tag = slash_tx}} = D) ->
    lager:debug("slash_tx signed", []),
    %% TODO: Would be prudent to check the SignedTx before pushing
    try_to_push_to_mempool(SignedTx, D,
                           _NextStateIfSuccess = channel_closing,
                           fun(D0) ->
                             log(rcv, ?SIGNED, Msg, D0#data{op = ?NO_OP})
                           end,
                           _NextStateIfFailure = channel_closing);
awaiting_signature(cast, {?SIGNED, create_tx, SignedTx} = Msg,
                   #data{ role = initiator
                        , op = #op_sign{ tag = create_tx
                                       , data = OpData0}} = D) ->
    #op_data{updates = Updates} = OpData0,
    maybe_check_sigs_create(SignedTx, Updates, initiator,
        fun() ->
            %% At this point, the initiator can glean the on_chain_id
            {_, D1} = on_chain_id(D, SignedTx),
            OpData = OpData0#op_data{signed_tx = SignedTx},
            next_state(half_signed, send_funding_created_msg(
                SignedTx, log(rcv, ?SIGNED, Msg,
                              D1#data{ op = #op_ack{ tag = create_tx
                                                   , data = OpData}
                                     , client_may_disconnect = true })))
        end, D);
awaiting_signature(cast, {?SIGNED, deposit_tx, SignedTx} = Msg,
                   #data{op = #op_sign{ tag = deposit_tx
                                      , data = OpData0} = OpSign} = D) ->
    #op_data{updates = Updates} = OpData0,
    maybe_check_auth(SignedTx, OpSign, not_deposit_tx, me,
        fun() ->
            OpData = OpData0#op_data{signed_tx = SignedTx},
            next_state(dep_half_signed,
                  send_deposit_created_msg(SignedTx, Updates,
                      log(rcv, ?SIGNED, Msg,
                              D#data{op = #op_ack{ tag = deposit_tx
                                                 , data = OpData}})))
        end, D);
awaiting_signature(cast, {?SIGNED, withdraw_tx, SignedTx} = Msg,
                   #data{op = #op_sign{ tag = withdraw_tx
                                      , data = OpData0} = OpSign} = D) ->
    #op_data{updates = Updates} = OpData0,
    maybe_check_auth(SignedTx, OpSign, not_withdraw_tx, me,
        fun() ->
            OpData = OpData0#op_data{signed_tx = SignedTx},
            next_state(wdraw_half_signed,
                    send_withdraw_created_msg(SignedTx, Updates,
                        log(rcv, ?SIGNED, Msg,
                              D#data{op = #op_ack{ tag = withdraw_tx
                                                 , data = OpData}})))
        end, D);
awaiting_signature(cast, {?SIGNED, ?FND_CREATED = OpTag, SignedTx} = Msg,
                   #data{ role = responder
                        , op = #op_sign{ tag = OpTag
                                       , data = OpData0 } } = D) ->
    #op_data{updates = Updates} = OpData0,
    maybe_check_sigs_create(SignedTx, Updates, both,
        fun() ->
            OpData = OpData0#op_data{signed_tx = SignedTx},
            D1 = D#data{ create_tx = SignedTx
                       , op = #op_ack{ tag = OpTag
                                     , data = OpData}
                       , client_may_disconnect = true },
            ActionFun = fun(Tx, Data) -> send_funding_signed_msg(Tx, Data) end,
            D2 = safe_watched_action_report(SignedTx, Msg, D1, Updates, ActionFun, ?WATCH_FND, OpTag, ?SIGNED),
            gproc_register_on_chain_id(D2),
            next_state(awaiting_locked, D2)
        end, D);
awaiting_signature(cast, {?SIGNED, ?DEP_CREATED = OpTag, SignedTx} = Msg,
                   #data{op = #op_sign{ tag = OpTag
                                      , data = OpData0 } = OpSign} = D) ->
    #op_data{updates = Updates} = OpData0,
    maybe_check_auth(SignedTx, OpSign, not_deposit_tx, both,
        fun() ->
            OpData = OpData0#op_data{signed_tx = SignedTx},
            D1 = D#data{op = #op_ack{ tag = OpTag
                                    , data = OpData }},
            ActionFun = fun(Tx, Data) -> send_deposit_signed_msg(Tx, Data) end,
            D2 = safe_watched_action_report(SignedTx, Msg, D1, Updates, ActionFun, ?WATCH_DEP, OpTag, ?SIGNED),
            next_state(awaiting_locked, D2)
        end, D);
awaiting_signature(cast, {?SIGNED, ?WDRAW_CREATED = OpTag, SignedTx} = Msg,
                   #data{op = #op_sign{ tag = OpTag
                                      , data = OpData0 } = OpSign} = D) ->
    #op_data{updates = Updates} = OpData0,
    maybe_check_auth(SignedTx, OpSign, not_withdraw_tx, both,
        fun() ->
            OpData = OpData0#op_data{signed_tx = SignedTx},
            D1 = D#data{op = #op_ack{ tag = OpTag
                                    , data = OpData }},
            ActionFun = fun(Tx, Data) -> send_withdraw_signed_msg(Tx, Data) end,
            D2 = safe_watched_action_report(SignedTx, Msg, D1, Updates, ActionFun, ?WATCH_WDRAW, OpTag, ?SIGNED),
            next_state(awaiting_locked, D2)
        end, D);
awaiting_signature(cast, {?SIGNED, ?UPDATE = OpTag, SignedTx} = Msg,
                   #data{ state = State
                        , op = #op_sign{ tag = OpTag
                                       , data = OpData0 } = OpSign} = D) ->
    #op_data{updates = Updates} = OpData0,
    lager:debug("?UPDATE signed: ~p", [Updates]),
    maybe_check_auth(SignedTx, OpSign, not_offchain_tx, me,
        fun() ->
            OpData = OpData0#op_data{signed_tx = SignedTx},
            State1 = aesc_offchain_state:set_half_signed_tx(SignedTx, State),
            D1 = D#data{ state = State1
                       , op = #op_ack{ tag = OpTag
                                     , data = OpData }},
            D2 = log(rcv, ?SIGNED, Msg, D1),
            D3 = send_update_msg(SignedTx, Updates, D2),
            next_state(awaiting_update_ack, D3)
        end, D);
awaiting_signature(cast, {?SIGNED, ?UPDATE_ACK = OpTag, SignedTx} = Msg,
                   #data{ op = #op_sign{ tag = OpTag
                                       , data = OpData0 } = OpSign
                        , opts = Opts
                        , state = State } = D) ->
    #op_data{ updates = Updates
            , block_hash = BlockHash } = OpData0,
    lager:debug("?UPDATE_ACK signed: ~p", [Updates]),
    maybe_check_auth(SignedTx, OpSign, not_offchain_tx, both,
        fun() ->
            D1 = log(rcv, ?SIGNED, Msg, D),
            D2 = send_update_ack_msg(SignedTx, D1),
            {OnChainEnv, OnChainTrees} = load_pinned_env(BlockHash),
            State1 = aesc_offchain_state:set_signed_tx(SignedTx, Updates, State,
                                                       OnChainTrees, OnChainEnv, Opts),
            D3 = D2#data{ state = State1
                        , op    = ?NO_OP },
            next_state(open, D3)
        end, D);
awaiting_signature(cast, {?SIGNED, ?SHUTDOWN = OpTag, SignedTx} = Msg,
                   #data{op = #op_sign{ tag = OpTag
                                      , data = OpData0 } = OpSign} = D) ->
    lager:debug("SHUTDOWN signed", []),
    maybe_check_auth(SignedTx, OpSign, not_close_mutual_tx, me,
        fun() ->
            D1 = log(rcv, ?SIGNED, Msg, D),
            D2 = send_shutdown_msg(SignedTx, D1),
            OpData = OpData0#op_data{signed_tx = SignedTx},
            D3 = D2#data{ op = #op_ack{ tag = OpTag
                                      , data = OpData }},
            next_state(mutual_closing, D3)
        end, D);
awaiting_signature(cast, {?SIGNED, ?SHUTDOWN_ACK = OpTag, SignedTx} = Msg,
                   #data{op = #op_sign{ tag = OpTag
                                      , data = OpData0 } = OpSign} = D) ->
    maybe_check_auth(SignedTx, OpSign, not_close_mutual_tx, both,
        fun() ->
            D1 = log(rcv, ?SIGNED, Msg, D),
            D2 = send_shutdown_ack_msg(SignedTx, D1),
            D3 = D2#data{op = ?NO_OP},
            D4 = report_on_chain_tx(close_mutual, SignedTx, D3),
            D5 = D4#data{op = #op_close{data = OpData0#op_data{signed_tx = SignedTx}}},
            next_state(mutual_closed, D5)
        end, D);
awaiting_signature(cast, {?SIGNED, force_progress_tx = OpTag, SignedTx} = Msg,
                   #data{op = #op_sign{ tag = OpTag }} = D) ->
    lager:debug("force_progress_tx signed", []),
    D1 = log(rcv, ?SIGNED, Msg, D),
    D2 = D1#data{op = ?NO_OP},
    case verify_signatures_onchain_check(pubkeys(me, D2, SignedTx), SignedTx) of
        ok ->
            force_progress_signed(SignedTx, D2);
        {error,_} = Error ->
            lager:debug("Failed signature check: ~p", [Error]),
            next_state(open, D2)
    end;
awaiting_signature(cast, {?SIGNED, snapshot_solo_tx = OpTag, SignedTx} = Msg,
                   #data{op = #op_sign{ tag = OpTag
                                      , data = OpData }} = D) ->
    lager:debug("snapshot_solo_tx signed", []),
    #op_data{updates = Updates} = OpData,
    D1 = log(rcv, ?SIGNED, Msg, D),
    D2 = D1#data{op = ?NO_OP},
    case verify_signatures_onchain_check(pubkeys(me, D2, SignedTx), SignedTx) of
        ok ->
            snapshot_solo_signed(SignedTx, Updates, D2);
        {error,_} = Error ->
            lager:debug("Failed signature check: ~p", [Error]),
            next_state(open, D2)
    end;
awaiting_signature(cast, {?SIGNED, close_solo_tx = OpTag, SignedTx} = Msg,
                   #data{op = #op_sign{ tag = OpTag
                                      , data = OpData }} = D) ->
    #op_data{updates = Updates} = OpData,
    D1 = log(rcv, ?SIGNED, Msg, D),
    close_solo_signed(SignedTx, Updates, D1);
awaiting_signature(cast, {?SIGNED, settle_tx = OpTag, SignedTx} = Msg,
                   #data{op = #op_sign{ tag = OpTag
                                      , data = OpData } = OpSign} = D) ->
    #op_data{updates = Updates} = OpData,
    maybe_check_auth(SignedTx, OpSign, not_settle_tx, me,
        fun() ->
                D1 = log(rcv, ?SIGNED, Msg, D),
                D2 = D1#data{op = ?NO_OP},
                settle_signed(SignedTx, Updates, D2)
        end, D);

%% Timeouts
awaiting_signature(timeout, _Msg,
                   #data{ channel_status = closing
                        , op = #op_sign{tag = Tag}} = D) when
                    Tag =:= ?SHUTDOWN;
                    Tag =:= ?SHUTDOWN_ACK ->
    lager:debug("Timeout while waiting for signature on mutual close"),
    next_state(channel_closing, D#data{op = ?NO_OP});

%% Other
awaiting_signature(Type, Msg, D) ->
    handle_common_event(Type, Msg, postpone, D).

awaiting_update_ack(enter, _OldSt, _D) ->
    keep_state_and_data;
awaiting_update_ack(cast, {Req, _} = Msg, #data{op = Op} = D) when ?UPDATE_CAST(Req) ->
    %% This might happen if a request is sent before our signed ?UPDATE msg
    %% arrived.
    case is_solo_tx_from_op(Op) of
        false ->
            %% Race detection!
            lager:debug("race detected: ~p", [Msg]),
            handle_update_conflict(Req, D);
        true ->
            postpone(D)
    end;
awaiting_update_ack(cast, {?UPDATE_ACK, Msg}, #data{} = D) ->
    case check_update_ack_msg(Msg, D) of
        {ok, D1} ->
            next_state(open, D1);
        {error, _Error} ->
            handle_update_conflict(?UPDATE_ACK, D)
    end;
awaiting_update_ack(cast, {?UPDATE_ERR, Msg}, D) ->
    lager:debug("received update_err: ~p", [Msg]),
    case check_update_err_msg(Msg, D) of
        {ok, ?ERR_CONFLICT, D1} ->
            D2 = report(conflict, Msg, D1),
            next_state(open, D2);
        {ok, _ErrorCode, D1} ->
            %% TODO: send a different kind of report (e.g. validation error)
            D2 = report(conflict, Msg, D1),
            next_state(open, D2);
        {error, fallback_state_mismatch} = Error ->
            %% falling back to an inconsistent state
            %% this is possible if we are a malicious actor only
            D1 = report(conflict, Msg, D),
            close(Error, D1);
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_update_ack(Type, Msg, D) ->
    handle_common_event(Type, Msg, postpone, D).

channel_closed(enter, _OldSt, D) ->
    keep_state(clear_ongoing(D));
channel_closed(cast, {?MIN_DEPTH_ACHIEVED, ChainId, ?WATCH_CLOSED, TxHash},
               #data{ on_chain_id = ChainId
                    , op = #op_min_depth{ tag = ?WATCH_CLOSED
                                        , tx_hash = TxHash % same tx hash
                                        } } = D) ->
    close(closed_confirmed, D);
channel_closed(cast, {?CHANNEL_CHANGED, #{ tx_hash := TxHash
                                         , chan_id := ChannelId } },
               #data{ on_chain_id = ChannelId %% same channel id
                    , op = #op_min_depth{ tag = ?WATCH_CLOSED
                                        , tx_hash = TxHash % same tx hash
                                        } } = D) ->
    %% The close mutual transaction ended up in a micro fork but was included
    %% again. This is an expected scenario
    keep_state(D#data{last_channel_change = TxHash});
channel_closed(cast, {?CHANNEL_CHANGED, #{tx_hash := TxHash} = _Info} = Msg, D) ->
    %% This is a weird case. The channel was closed, but now isn't.
    %% This would indicate a fork switch. TODO: detect channel status
    %% and respond accordingly. For now. Panic and terminate
    D1 = report(info, zombie_channel, D#data{last_channel_change = TxHash}),
    close(zombie_channel, log(rcv, msg_type(Msg), Msg, D1));
channel_closed(Type, Msg, D) ->
    handle_common_event(Type, Msg, discard, D).

mutual_closed(enter, _, D) ->
    keep_state(enter_closing(D));
mutual_closed(cast, {?SHUTDOWN_ERR, Msg}, D) ->
    mutual_close_error(Msg, D);
mutual_closed(timeout, _Msg, D) ->
    close(timeout, D);
mutual_closed(Type, Msg, D) ->
    handle_common_event(Type, Msg, postpone, D).

channel_closing(enter, _OldSt, D) ->
    D1 = enter_closing(D),
    keep_state(clear_ongoing(D1));
channel_closing(cast, {?CHANNEL_UNLOCKED, #{chan_id := ChId}} = Msg,
                #data{ on_chain_id = ChId
                     , op = #op_watch{ type =  unlock
                                     , data = #op_data{signed_tx = SignedTx}}
                     } = D) ->
    lager:debug("channel unlocked", []),
    D1 = log(rcv, ?CHANNEL_UNLOCKED, Msg, D),
    {Type, _Tx} = aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
    case Type of
        channel_settle_tx ->
            lager:debug("pushing settle tx", []),
            push_to_mempool(SignedTx);
        _ ->
            lager:debug("SignedTx of Type ~p - ignoring", [Type]),
            ok
    end,
    keep_state(D1);
channel_closing(cast, {?SHUTDOWN, Msg}, D) ->
    case {was_fork_activated(?LIMA_PROTOCOL_VSN), check_shutdown_msg(Msg, D)} of
        {false, _} ->
            %% TODO: send an ?UPDATE_ERR (which isn't yet defined)
            lager:debug("Shutdown while channel_closing not allowed before the Lima fork"),
            keep_state(D);
        {true, {ok, SignedTx, Updates, BlockHash, D1}} ->
            shutdown_msg_received(SignedTx, Updates, BlockHash, D1);
        {true, {error, E}} ->
            %% TODO: send an ?UPDATE_ERR (which isn't yet defined)
            %% For now, log and ignore
            lager:debug("Bad shutdown_msg in channel_closing: ~p", [E]),
            keep_state(D)
    end;
channel_closing(cast, {?CH_REESTABL, Msg}, #data{ role = responder
                                               , op = Op} = D) ->
    case check_reestablish_msg(Msg, D) of
        {ok, D1} ->
            D2 = report(info, channel_reestablished, D1),
            lager:debug("Op = ~p", [Op]),
            D3 = case Op of
                     #op_reestablish{mode = restart} ->
                         lager:debug("re-register and restart watcher", []),
                         gproc_register(D2),
                         watcher_reg(D2#data.on_chain_id),
                         D2;
                     _ ->
                         lager:debug("Already running - don't re-register", []),
                         D2
                 end,
            next_state(channel_closing, send_reestablish_ack_msg(D3));
        {error, _} = Error ->
            close(Error, D)
    end;
channel_closing(Type, Msg, D) ->
    handle_common_event(Type, Msg, discard, D).

enter_closing(D) ->
    if D#data.channel_status =/= closing ->
            D1 = report(info, closing, D),
            report_update(D1#data{channel_status = closing});
       true ->
            report_update(D)
    end.


dep_half_signed(enter, _OldSt, _D) ->
    keep_state_and_data;
dep_half_signed(cast, {Req, _} = Msg, D) when ?UPDATE_CAST(Req) ->
    %% This might happen if a request is sent before our ?DEP_CREATED msg
    %% arrived.
    lager:debug("race detected: ~p", [Msg]),
    handle_update_conflict(Req, D);
dep_half_signed(cast, {?DEP_SIGNED = OpTag, Msg},
                #data{op = #op_ack{tag = deposit_tx, data = OpData}} = D) ->
    case check_deposit_signed_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            try_to_push_to_mempool(SignedTx, D1, awaiting_locked,
                fun(D0) ->
                    #op_data{updates = Updates} = OpData,
                    _D = safe_watched_action_report(SignedTx, Msg, D0, Updates,
                                                    undefined, ?WATCH_DEP, OpTag)
                end);
        {error, _Error} ->
            handle_update_conflict(?DEP_SIGNED, D)
    end;
dep_half_signed(cast, {?DEP_ERR, Msg}, D) ->
    lager:debug("received deposit_error: ~p", [Msg]),
    case check_deposit_error_msg(Msg, D) of
        {ok, ?ERR_CONFLICT, D1} ->
            D2 = report(conflict, Msg, D1),
            next_state(open, D2);
        {ok, _ErrorCode, D1} ->
            %% TODO: send a different kind of report (e.g. validation error)
            D2 = report(conflict, Msg, D1),
            next_state(open, D2);
        {error, _} = Error ->
            close(Error, D)
    end;
dep_half_signed(Type, Msg, D) ->
    handle_common_event(Type, Msg, error, D).

dep_signed(enter, _OldSt, _D) -> keep_state_and_data;
dep_signed(cast, {?DEP_LOCKED, Msg},
           #data{op = #op_lock{ tag = deposit
                              , data = OpData}} = D) ->
    #op_data{signed_tx  = SignedTx} = OpData,
    case check_deposit_locked_msg(Msg, SignedTx, D) of
        {ok, D1} ->
            D2 = report(info, deposit_locked, D1),
            deposit_locked_complete(OpData, D2#data{op = ?NO_OP});
        {error, _} = Error ->
            close(Error, D)
    end;
dep_signed(cast, {?SHUTDOWN, Msg}, D) ->
    shutdown_msg_received(Msg, D);
dep_signed(Type, Msg, D) ->
    handle_common_event(Type, Msg, error, D).

half_signed(enter, _OldSt, _D) -> keep_state_and_data;
half_signed(cast, {?FND_SIGNED = OpTag, Msg},
            #data{ role = initiator
                 , op = #op_ack{tag = create_tx} = Op } = D) ->
    case check_funding_signed_msg(Msg, D) of
        {ok, SignedTx, _BlockHash, D1} ->
            try_to_push_to_mempool(SignedTx, D1, awaiting_locked,
                fun(D0) ->
                    lager:debug("funding_signed ok", []),
                    D01 = report(info, funding_signed, D0),
                    D02 = D01#data{create_tx = SignedTx},
                    #op_ack{ tag = create_tx
                           , data = #op_data{updates = Updates}} = Op,
                    D03 = safe_watched_action_report(SignedTx, Msg, D02,
                                                     Updates, undefined,
                                                     ?WATCH_FND, OpTag),
                    gproc_register_on_chain_id(D03),
                    D03
                end);
        {error, Error} ->
            close(Error, D)
    end;
half_signed(Type, Msg, D) ->
    handle_common_event(Type, Msg, postpone_or_error(Type), D).

mutual_closing(enter, _OldSt, _D) -> keep_state_and_data;
mutual_closing(cast, {?SHUTDOWN_ERR, #{error_code := ?ERR_ONCHAIN_REJECTED} = _Msg},
                #data{op = #op_ack{ tag = ?SHUTDOWN} } = D) ->
    %% TODO: try posting the co-signed tx
    {ok, D1} = fall_back_to_stable_state(D),
    handle_recoverable_error(#{ code => ?ERR_ONCHAIN_REJECTED
                              , msg_type => ?DEP_SIGNED
                              , respond => false}, D1);
mutual_closing(cast, {?SHUTDOWN_ERR, Msg}, D) ->
    lager:debug("received ?SHUTDOWN_ERR in mutual_closing: ~p", [Msg]),
    mutual_close_error(Msg, D);
mutual_closing(cast, {?SHUTDOWN_ACK, Msg}, D) ->
    case check_shutdown_ack_msg(Msg, D) of
        {ok, SignedTx, Updates, BlockHash, D1} ->
            lager:debug("shutdown_ack ok", []),
            try_to_push_to_mempool(SignedTx, D1, mutual_closed,
                fun(D0) ->
                    D01 = report_on_chain_tx(close_mutual, SignedTx, D0),
                    OpData = #op_data{ signed_tx  = SignedTx
                                     , updates    = Updates
                                     , block_hash = BlockHash },
                    D01#data{op = #op_close{data = OpData}}
                end);
        {error, _} = Error ->
            lager:debug("Validation of SHUTDOWN_ACK failed: ~p", [Error]),
            handle_recoverable_error(#{ code => ?ERR_VALIDATION
                                      , respond => true
                                      , msg_type => ?SHUTDOWN_ACK }, D)
    end;
mutual_closing(cast, {closing_signed, _Msg}, D) ->  %% TODO: not using this, right?
    close(closing_signed, D);
mutual_closing(timeout, _Msg, #data{channel_status = closing} = D) ->
    lager:debug("Timeout while waiting for peer signature on mutual close"),
    next_state(channel_closing, D);
mutual_closing(Type, Msg, D) ->
    handle_common_event(Type, Msg, error, D).

open(enter, _OldSt, D) ->
    D2 = if D#data.channel_status =/= open ->
                 D1 = report(info, open, D),
                 report_update(D1#data{channel_status = open});
            true ->
                 report_update(D)
         end,
    keep_state(clear_ongoing(D2));
open(cast, {?UPDATE, Msg}, D) ->
    case check_update_msg(Msg, D) of
        {ok, SignedTx, Updates, BlockHash, D1} ->
            D2 = report(info, update, D1),
            case request_signing_(?UPDATE_ACK, SignedTx, Updates, BlockHash, D2) of
                {ok, D3, Actions} ->
                    next_state(awaiting_signature, set_ongoing(?UPDATE, D3), Actions);
                {error, _Error} ->
                    lager:debug("Rejected incoming off-chain update because of ~p", [_Error]),
                    %% TODO: This is not strictly a conflict
                    %% Should be a different response
                    handle_update_conflict(?UPDATE, D2)
            end;
        {error, _Error} ->
            lager:debug("Rejected incoming off-chain update because of ~p", [_Error]),
            handle_update_conflict(?UPDATE, D)
    end;
open(cast, {?DEP_CREATED, Msg}, D) ->
    case check_deposit_created_msg(Msg, D) of
        {ok, SignedTx, Updates, BlockHash, D1} ->
            D2 = report(info, deposit_created, D1),
            lager:debug("deposit_created: ~p", [SignedTx]),
            case request_signing_(?DEP_CREATED, SignedTx, Updates, BlockHash, D2) of
                {ok, D3, Actions} ->
                    next_state(awaiting_signature, set_ongoing(?DEP_CREATED, D3), Actions);
                {error, _Error} ->
                    lager:debug("Rejected incoming deposit because of ~p", [_Error]),
                    %% TODO: should be a different error response
                    handle_update_conflict(?DEP_CREATED, D2)
            end;
        {error, _Error} ->
            lager:debug("Rejected incoming deposit because of ~p", [_Error]),
            handle_update_conflict(?DEP_CREATED, D)
    end;
open(cast, {?WDRAW_CREATED, Msg}, D) ->
    case check_withdraw_created_msg(Msg, D) of
        {ok, SignedTx, Updates, BlockHash, D1} ->
            D2 = report(info, withdraw_created, D1),
            lager:debug("withdraw_created: ~p", [SignedTx]),
            case request_signing_(?WDRAW_CREATED, SignedTx, Updates,
                                  BlockHash, D2) of
                {ok, D3, Actions} ->
                    next_state(awaiting_signature, set_ongoing(?WDRAW_CREATED, D3), Actions);
                {error, _Error} ->
                    lager:debug("Rejected incoming withdrawal because of ~p", [_Error]),
                    %% TODO: Should be a different error response
                    handle_update_conflict(?WDRAW_CREATED, D2)
            end;
        {error, _Error} ->
            lager:debug("Rejected incoming withdrawal because of ~p", [_Error]),
            handle_update_conflict(?WDRAW_CREATED, D)
    end;
open(cast, {?SIGNED, _, _} = Msg, D) ->
    lager:debug("Received signing reply in 'open' - ignore: ~p", [Msg]),
    keep_state(log(drop, ?SIGNED, Msg, D));
open({call, From}, {attach_responder, #{reestablish := true} = Info} = _Req,
     #data{ peer_connected = false, opts = Opts } = D) ->
    lager:debug("call: ~p; D = ~p", [_Req, pr_data(D)]),
    #{ initiator := I, responder := R } = Opts,
    case check_attach_info(Info, I, R, D) of
        ok ->
            gproc:unreg(maps:get(gproc_key, Info)),
            {Pid, _} = From,
            next_state(awaiting_reestablish,
                       D#data{ session = Pid
                             , op = #op_reestablish{mode = remain}
                             , peer_connected = true
                             , channel_status = attached },
                       [{reply, From, ok}]);
        {error, Err} ->
            lager:debug("Bad attach info: ~p", [Err]),
            keep_state(D, [{reply, From, {error, invalid_attach}}])
    end;
open({call, From}, Request, D) ->
    handle_call(open, Request, From, D);
open(cast, {?LEAVE, Msg}, #data{ role = Role
                               , opts = Opts } = D) ->
    case check_leave_msg(Msg, D) of
        {ok, D1} ->
            lager:debug("received leave msg", []),
            send_leave_ack_msg(D1),
            D2 = report_leave(D1),
            case {Role, maps:get(keep_running, Opts, false)} of
                {responder, true} ->
                    lager:debug("Keep running after peer leave", []),
                    keep_state(D2);
                _ ->
                    lager:debug("Closing due to peer leave", []),
                    close(leave, D2)
            end;
        {error, E} ->
            lager:debug("leave msg error: ~p", [E]),
            keep_state(D)
    end;
open(cast, {?SHUTDOWN, Msg}, D) ->
    shutdown_msg_received(Msg, D);
open(Type, Msg, D) ->
    handle_common_event(Type, Msg, discard, D).

reestablish_init(enter, _OldSt, _D) -> keep_state_and_data;
reestablish_init(cast, {?CH_REEST_ACK, Msg}, D) ->
    case check_reestablish_ack_msg(Msg, D) of
        {ok, D1} ->
            D2 = report(info, channel_reestablished, D1),
            ChId = D2#data.on_chain_id,
            ok = watcher_reg(ChId),
            {ok, Channel} = aec_chain:get_channel(ChId),
            NextState =
                case aesc_channels:is_solo_closing(Channel) of
                    false -> open;
                    true -> channel_closing %% solo closing sequence
                end,
            next_state(NextState, D2);
        {error, _} = Err ->
            close(Err, D)
    end;
reestablish_init(Type, Msg, D) ->
    handle_common_event(Type, Msg, postpone_or_error(Type), D).

signed(enter, _OldSt, _D) -> keep_state_and_data;
signed(cast, {?FND_LOCKED, Msg}, D) ->
    case check_funding_locked_msg(Msg, D) of
        {ok, D1} ->
            D2 = report(info, funding_locked, D1),
            funding_locked_complete(D2);
        {error, _} = Error ->
            close(Error, D)
    end;
signed(cast, {?SHUTDOWN, Msg}, D) ->
    shutdown_msg_received(Msg, D);
signed(Type, Msg, D) ->
    handle_common_event(Type, Msg, postpone_or_error(Type), D).

wdraw_half_signed(enter, _OldSt, _D) -> keep_state_and_data;
wdraw_half_signed(cast, {Req,_} = Msg, D) when ?UPDATE_CAST(Req) ->
    %% This might happen if a request is sent before our ?WDRAW_CREATED msg
    %% arrived.
    lager:debug("race detected: ~p", [Msg]),
    handle_update_conflict(Req, D);
wdraw_half_signed(cast, {?WDRAW_SIGNED = OpTag, Msg},
                  #data{op = #op_ack{tag = withdraw_tx, data = OpData}} = D) ->
    case check_withdraw_signed_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            try_to_push_to_mempool(SignedTx, D1, awaiting_locked,
                fun(D0) ->
                    #op_data{updates = Updates} = OpData,
                    _D = safe_watched_action_report(SignedTx, Msg, D0, Updates,
                                                    undefined, ?WATCH_WDRAW, OpTag)
                end);
        {error, _Error} ->
            handle_update_conflict(?WDRAW_SIGNED, D)
    end;
wdraw_half_signed(cast, {?WDRAW_ERR, Msg}, D) ->
    lager:debug("received withdraw_error: ~p", [Msg]),
    case check_withdraw_error_msg(Msg, D) of
        {ok, ?ERR_CONFLICT, D1} ->
            D2 = report(conflict, Msg, D1),
            next_state(open, D2);
        {ok, _ErrorCode, D1} ->
            %% TODO: send a different kind of report (e.g. validation error)
            D2 = report(conflict, Msg, D1),
            next_state(open, D2);
        {error, _} = Error ->
            close(Error, D)
    end;
wdraw_half_signed(Type, Msg, D) ->
    handle_common_event(Type, Msg, error, D).

%% Don't flag for update conflicts once we've pushed to the chain, and
%% wait for confirmation; postpone instead.
%%
wdraw_signed(enter, _OldSt, _D) -> keep_state_and_data;
wdraw_signed(cast, {?WDRAW_LOCKED, Msg},
             #data{op = #op_lock{ tag = withdraw
                                , data = OpData}} = D) ->
    #op_data{signed_tx  = SignedTx} = OpData,
    case check_withdraw_locked_msg(Msg, SignedTx, D) of
        {ok, D1} ->
            D2 = report(info, withdraw_locked, D1),
            withdraw_locked_complete(OpData, D2#data{op = ?NO_OP});
        {error, _} = Error ->
            close(Error, D)
    end;
wdraw_signed(cast, {?SHUTDOWN, Msg}, D) ->
    shutdown_msg_received(Msg, D);
wdraw_signed(Type, Msg, D) ->
    handle_common_event(Type, Msg, error, D).

%% ======================================================================
%% Action logic

send_open_msg(#data{ opts    = Opts
                   , session = Sn
                   , log     = Log } = Data) ->
    #{ lock_period        := LockPeriod
     , initiator          := Initiator
     , responder          := Responder
     , push_amount        := PushAmount
     , initiator_amount   := InitiatorAmount
     , responder_amount   := ResponderAmount
     , channel_reserve    := ChannelReserve } = Opts,
    ChainHash = aec_chain:genesis_hash(),
    %% Generate a temporary channel id
    ChannelPubKey = aesc_channels:pubkey(Initiator, erlang:unique_integer(), Responder),
    Msg = #{ chain_hash           => ChainHash
           , temporary_channel_id => ChannelPubKey
           , lock_period          => LockPeriod
           , push_amount          => PushAmount
           , initiator_amount     => InitiatorAmount
           , responder_amount     => ResponderAmount
           , channel_reserve      => ChannelReserve
           , initiator            => Initiator
           , responder            => Responder
           },
    aesc_session_noise:channel_open(Sn, Msg),
    Data#data{ channel_id   = ChannelPubKey
             , log = log_msg(snd, ?CH_OPEN, Msg, Log) }.

check_open_msg(#{ chain_hash           := ChainHash
                , temporary_channel_id := ChanId
                , lock_period          := LockPeriod
                , push_amount          := PushAmt
                , initiator_amount     := InitiatorAmt
                , responder_amount     := ResponderAmt
                , channel_reserve      := ChanReserve
                , initiator            := InitiatorPubkey
                , responder            := ResponderPubkey } = Msg,
               #data{opts = Opts, log = Log} = Data) ->
    %% TODO: Implement more checks
    case aec_chain:genesis_hash() of
        ChainHash ->
            Opts1 =
                Opts#{ lock_period        => LockPeriod
                     , initiator          => InitiatorPubkey
                     , responder          => ResponderPubkey
                     , push_amount        => PushAmt
                     , initiator_amount   => InitiatorAmt
                     , responder_amount   => ResponderAmt
                     , channel_reserve    => ChanReserve},
            {ok, Data#data{ channel_id     = ChanId
                          , opts           = Opts1
                          , peer_connected = true
                          , log            = log_msg(rcv, ?CH_OPEN, Msg, Log) }};
        _ ->
            {error, chain_hash_mismatch}
    end.

send_reestablish_msg(#{ existing_channel_id := ChId
                      , offchain_tx := OffChainTx },
                     #data{session = Sn, log = Log} = Data) ->
    ChainHash = aec_chain:genesis_hash(),
    TxBin = aetx_sign:serialize_to_binary(OffChainTx),
    Msg = #{ chain_hash => ChainHash
           , channel_id => ChId
           , data       => #{tx => TxBin} },
    aesc_session_noise:channel_reestablish(Sn, Msg),
    Data#data{ channel_id  = ChId
             , on_chain_id = ChId
             , op          = #op_reestablish{mode = restart, offchain_tx = OffChainTx}
             , log         = log_msg(snd, ?CH_REESTABL, Msg, Log)}.

check_reestablish_msg(#{ chain_hash := ChainHash
                       , channel_id := ChId
                       , data       := #{tx := TxBin} } = Msg,
                      #data{state = State, log = Log} = Data) ->
    ChannelRes = get_channel(ChainHash, ChId),
    case ChannelRes of
        {ok, _Channel} ->
            try SignedTx = aetx_sign:deserialize_from_binary(TxBin),
                 Check = aesc_offchain_state:check_reestablish_tx(SignedTx, State),
                 case Check of
                     {ok, NewState} ->
                         Log1 = log_msg(rcv, ?CH_REESTABL, Msg, Log),
                         {ok, Data#data{ channel_id     = ChId
                                       , on_chain_id    = ChId
                                       , peer_connected = true
                                       , state          = NewState
                                       , log            = Log1 }};
                     {error, _} = TxErr ->
                         TxErr
                 end
            ?CATCH_LOG(_E)
                {error, invalid_reestablish}
            end;
        {error, _} = ChErr ->
            ChErr
    end.

send_reestablish_ack_msg(#data{ state       = State
                              , on_chain_id = ChId
                              , session     = Sn } = Data) ->
    ChainHash = aec_chain:genesis_hash(),
    {_, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ chain_hash  => ChainHash
           , channel_id  => ChId
           , data        => #{tx => TxBin} },
    aesc_session_noise:channel_reestablish_ack(Sn, Msg),
    Data#data{ op = ?NO_OP
             , log = log_msg(snd, ?CH_REEST_ACK, Msg, Data#data.log) }.

check_reestablish_ack_msg(#{data := #{tx := TxBin}} = Msg, Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    Funs = [ fun chk_chain_hash/3
           , fun chk_channel_id/3
           , fun chk_dual_sigs/3
           , fun chk_same_tx/3
           , fun chk_mindepth_opts/3
           , fun log_reestabl_ack_msg/3
           ],
    case run_checks(Funs, Msg, SignedTx, Data) of
        {ok, D1} ->
            {ok, D1#data{peer_connected = true}};
        Other ->
            Other
    end.

run_checks([], _, _, Data) ->
    {ok, Data};
run_checks([F|Funs], Msg, SignedTx, Data) ->
    case F(Msg, SignedTx, Data) of
        {true, _} ->
            run_checks(Funs, Msg, SignedTx, Data);
        {false, Err} ->
            {error, Err};
        {data, Data1} ->
            run_checks(Funs, Msg, SignedTx, Data1)
    end.

chk_chain_hash(#{ chain_hash := CH }, _, _) ->
    {CH == aec_chain:genesis_hash(), chain_hash_mismatch}.

chk_channel_id(#{ channel_id := ChId }, _, #data{ on_chain_id = OCId }) ->
    {ChId =/= undefined andalso ChId == OCId, channel_id_mismatch}.

chk_dual_sigs(_, SignedTx, #data{on_chain_id = ChannelPubkey}) ->
    {Mod, _Tx} = aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    {ok, Channel} = aec_chain:get_channel(ChannelPubkey),
    InitiatorPubkey = aesc_channels:initiator_pubkey(Channel),
    ResponderPubkey = aesc_channels:responder_pubkey(Channel),
    Pubkeys = [InitiatorPubkey, ResponderPubkey],
    case Mod of
        aesc_offchain_tx ->
            Res = verify_signatures_offchain(ChannelPubkey, Pubkeys, SignedTx),
            {ok == Res, signatures_invalid};
        _ ->
            TxLoc = aec_chain:find_tx_location(aetx_sign:hash(SignedTx)),
            case TxLoc of
                %% The last state we are aware of is on-chain so it could be
                %% used in disputes
                %% TODO: check for Forced Progress while the FSM had been
                %% offline
                BH when is_binary(BH) ->
                    {true, this_not_used};
                NotIncluded when NotIncluded =:= not_found;
                                 NotIncluded =:= none ->
                    {false, on_chain_transaction_rejected};
                %% a bit more special case is when the transaction had
                %% been removed from the chain but it is still in the
                %% mempool; We can get here if it had been accepted on-chain
                %% and enough confirmations had been reached according to the
                %% user's setup, then they had closed the connection and now
                %% are trying to reestablish using this old state. Since the
                %% tx in the pool can never be accepted on-chain, abort and
                %% let the users figure it out
                mempool ->
                    {false, on_chain_transaction_in_mempool}
            end
    end.

chk_same_tx(_, SignedTx, #data{state = State}) ->
    {_, MySignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    SignedTxBin = aetx_sign:serialize_to_binary(SignedTx),
    MySignedTxBin = aetx_sign:serialize_to_binary(MySignedTx),
    {SignedTxBin == MySignedTxBin, offchain_state_mismatch}.

chk_mindepth_opts(Msg, _, #data{opts = Opts} = Data) ->
    #{ minimum_depth := MinDepth
     , minimum_depth_strategy := MinDepthStrategy }
        = Opts1 = maybe_use_minimum_depth_params(Msg, Opts),
    lager:debug("Checked minimum_depth parameters: ~p / ~p",
                [MinDepthStrategy, MinDepth]),
    Data1 = Data#data{opts = Opts1},
    {data, Data1}.

log_reestabl_ack_msg(Msg, _, #data{log = L} = D) ->
    {data, D#data{log = log_msg(rcv, ?CH_REEST_ACK, Msg, L)}}.

send_channel_accept(#data{ opts = Opts
                         , session = Sn
                         , channel_id = ChanId } = Data) ->
    #{ minimum_depth          := MinDepth
     , minimum_depth_strategy := MinDepthStrategy
     , initiator              := Initiator
     , responder              := Responder
     , initiator_amount       := InitiatorAmt
     , responder_amount       := ResponderAmt
     , channel_reserve        := ChanReserve } = Opts,
    ChainHash = aec_chain:genesis_hash(),
    Msg = #{ chain_hash             => ChainHash
           , temporary_channel_id   => ChanId
           , minimum_depth          => MinDepth
           , minimum_depth_strategy => MinDepthStrategy
           , initiator_amount       => InitiatorAmt
           , responder_amount       => ResponderAmt
           , channel_reserve        => ChanReserve
           , initiator              => Initiator
           , responder              => Responder
           },
    aesc_session_noise:channel_accept(Sn, Msg),
    Data#data{log = log_msg(snd, ?CH_ACCEPT, Msg, Data#data.log)}.

check_accept_msg(#{ chain_hash             := ChainHash
                  , temporary_channel_id   := ChanId } = Msg,
                 #data{ channel_id = ChanId
                      , opts = Opts
                      , log = Log } = Data) ->
    %% TODO: implement more checks
    case aec_chain:genesis_hash() of
        ChainHash ->
            Log1 = log_msg(rcv, ?CH_ACCEPT, Msg, Log),
            Data1 = Data#data{ opts = Opts
                             , log = Log1
                             , peer_connected = true },
            {data, Data2} = chk_mindepth_opts(Msg, undefined, Data1),
            {ok, Data2};
        _ ->
            {error, chain_hash_mismatch}
    end.

new_onchain_tx_for_signing(Type, Opts, D) ->
    new_onchain_tx_for_signing(Type, Opts, fail, D).

new_onchain_tx_for_signing(Type, Opts, OnErr, D) when OnErr == fail;
                                                      OnErr == return ->
    #data{opts = ChannelOpts} = D,
    try new_onchain_tx_for_signing_(Type, Opts, OnErr, D, ChannelOpts)
    ?CATCH_LOG(E, "maybe_block_hash_error")
        error(E)
    end.

new_onchain_tx_for_signing_(Type, Opts, OnErr, D, ChannelOpts) ->
    Defaults = tx_defaults(Type, Opts, D),
    Opts1 = maps:merge(Defaults, Opts),
    {BlockHash, OnChainEnv, OnChainTrees} = pick_onchain_env(Opts, D),
    PinnedHeight = aetx_env:height(OnChainEnv),
    PinnedProtocol = aetx_env:consensus_version(OnChainEnv),
    TxRes = new_onchain_tx(Type, Opts1, D, BlockHash, OnChainEnv,
                           OnChainTrees, ChannelOpts),
    case {TxRes, OnErr} of
        {{ok, Tx, Updates}, _} ->
            case {aetx:min_fee(Tx, PinnedHeight, PinnedProtocol), aetx:fee(Tx)} of
                {MinFee, Fee} when MinFee =< Fee ->
                    {ok, Tx, Updates, BlockHash};
                {MinFee, Fee} ->
                    lager:debug("Fee (~p) is too low for ~p (Min: ~p)",
                                [Fee, Type, MinFee]),
                    error(too_low_fee)
            end;
        {{error, Reason}, fail} ->
            error(Reason);
        {{error, Reason}, return} ->
            {error, Reason}
    end.

-spec new_onchain_tx( channel_create_tx
                    | channel_close_mutual_tx
                    | channel_deposit_tx
                    | channel_withdraw_tx
                    | channel_force_progress_tx
                    | channel_snapshot_solo_tx
                    | channel_close_solo_tx
                    | channel_slash_tx
                    | channel_settle_tx,
                    map(), #data{}, aec_blocks:block_header_hash(),
                    aetx_env:env(), aec_trees:trees(), map()) ->
    {ok, aetx:tx(), [aesc_offchain_update:update()]} | {error, atom()}.
new_onchain_tx(channel_close_mutual_tx, #{ acct := From } = Opts,
               #data{opts = DOpts, on_chain_id = Chan, state = State} = D, _,
               OnChainEnv, _, _ChannelOpts) ->
    #{initiator := Initiator,
      responder := Responder} = DOpts,
    ChanId = aeser_id:create(channel, Chan),
    FromId = aeser_id:create(account, From),
    {ok, IAmt} = aesc_offchain_state:balance(Initiator, State),
    {ok, RAmt} = aesc_offchain_state:balance(Responder, State),
    TTL = maps:get(ttl, Opts, 0), %% 0 means no TTL limit
    Presets = maps:with([nonce, gas_price, fee], Opts),
    InitialOpts0 = maps:merge(Presets,
                              #{ channel_id             => ChanId
                               , from_id                => FromId
                               % both amounts are only going to get lower due to the tx fee
                               , initiator_amount_final => IAmt
                               , responder_amount_final => RAmt
                               , ttl                    => TTL}),
    Height = aetx_env:height(OnChainEnv),
    {ok, InitCloseTx} = new_onchain_tx_(aesc_close_mutual_tx, InitialOpts0,
                                        Height, D),
    Fee = aetx:fee(InitCloseTx),
    case pay_close_mutual_fee(Fee, IAmt, RAmt) of
        {ok, IAmt1, RAmt1} ->
            {ok, CloseMutualTx} =
                aesc_close_mutual_tx:new(InitialOpts0#{ initiator_amount_final => IAmt1
                                                      , responder_amount_final => RAmt1
                                                      , fee => Fee}),
            {ok, CloseMutualTx, []};
        {error, _} = Err ->
            Err
    end;
new_onchain_tx(channel_deposit_tx, #{acct := FromId,
                                     amount := Amount} = Opts,
               #data{on_chain_id = ChanId, state=State} = D,
               _BlockHash, OnChainEnv, OnChainTrees, ChannelOpts) ->
    Updates = [ aesc_offchain_update:op_deposit(aeser_id:create(account, FromId), Amount)
              | meta_updates(Opts) ],
    ActiveProtocol = aetx_env:consensus_version(OnChainEnv),
    UpdatedStateTx = aesc_offchain_state:make_update_tx(Updates, State,
                                                        ChanId,
                                                        ActiveProtocol,
                                                        OnChainTrees,
                                                        OnChainEnv,
                                                        channel_reserve(ChannelOpts)),
    {channel_offchain_tx, UpdatedOffchainTx} = aetx:specialize_type(UpdatedStateTx),
    StateHash = aesc_offchain_tx:state_hash(UpdatedOffchainTx),

    {LastRound, _} = aesc_offchain_state:get_latest_signed_tx(State),
    Opts1 = maps:merge(Opts, #{ state_hash => StateHash
                              , round      => increment_round(LastRound)
                              , channel_id => aeser_id:create(channel, ChanId)
                              , from_id    => aeser_id:create(account, FromId)
                              }),
    lager:debug("deposit_tx Opts = ~p", [Opts1]),
    PinnedHeight = aetx_env:height(OnChainEnv),
    {ok, DepositTx} = new_onchain_tx_(aesc_deposit_tx, Opts1, PinnedHeight, D),
    {ok, DepositTx, Updates};
new_onchain_tx(channel_withdraw_tx, #{acct := ToId,
                                      amount := Amount} = Opts,
               #data{on_chain_id = ChanId, state=State} = D,
               _BlockHash, OnChainEnv, OnChainTrees, ChannelOpts) ->
    Updates = [ aesc_offchain_update:op_withdraw(aeser_id:create(account, ToId), Amount)
              | meta_updates(Opts) ],
    ActiveProtocol = aetx_env:consensus_version(OnChainEnv),
    UpdatedStateTx = aesc_offchain_state:make_update_tx(Updates, State,
                                                        ChanId,
                                                        ActiveProtocol,
                                                        OnChainTrees,
                                                        OnChainEnv,
                                                        channel_reserve(ChannelOpts)),
    {channel_offchain_tx, UpdatedOffchainTx} = aetx:specialize_type(UpdatedStateTx),
    StateHash = aesc_offchain_tx:state_hash(UpdatedOffchainTx),

    {LastRound, _} = aesc_offchain_state:get_latest_signed_tx(State),
    Opts1 = maps:merge(Opts, #{ state_hash => StateHash
                              , round      => increment_round(LastRound)
                              , channel_id => aeser_id:create(channel, ChanId)
                              , to_id      => aeser_id:create(account, ToId)
                              }),
    lager:debug("withdraw_tx Opts = ~p", [Opts1]),
    PinnedHeight = aetx_env:height(OnChainEnv),
    {ok, WithdrawTx} = new_onchain_tx_(aesc_withdraw_tx, Opts1, PinnedHeight, D),
    {ok, WithdrawTx, Updates};
new_onchain_tx(channel_force_progress_tx, #{acct := _Acct} = Opts,
               D, _BlockHash, OnChainEnv, _OnChainTrees, _ChannelOpts) ->
    lager:debug("force_progress_tx Opts = ~p", [maps:without([offchain_trees],
                                                             Opts)]),
    PinnedHeight = aetx_env:height(OnChainEnv),
    {ok, FPTx} = new_onchain_tx_(aesc_force_progress_tx, Opts, PinnedHeight, D),
    {channel_force_progress_tx, FP} = aetx:specialize_type(FPTx),
    Updates = aesc_force_progress_tx:updates(FP),
    {ok, FPTx, Updates};
new_onchain_tx(channel_create_tx, Opts,
               #data{opts = #{initiator := Initiator,
                              responder := Responder},
                     state=State} = D,
               _BlockHash, OnChainEnv, _OnChainTrees, _ChannelOpts) ->
    StateHash = aesc_offchain_state:hash(State),
    Opts1 = Opts#{ state_hash    => StateHash
                 , initiator_id  => aeser_id:create(account, Initiator)
                 , responder_id  => aeser_id:create(account, Responder)
                 },
    lager:debug("create_tx Opts = ~p", [Opts1]),
    PinnedHeight = aetx_env:height(OnChainEnv),
    {ok, CreateTx} = new_onchain_tx_(aesc_create_tx, Opts1, PinnedHeight, D),
    {ok, CreateTx, []}; %% no updates
new_onchain_tx(channel_settle_tx, Opts,
               #data{ opts  = #{initiator := I,
                                responder := R}
                    , state = State } = D,
               _BlockHash, OnChainEnv, _OnChainTrees, _ChannelOpts) ->
    Account = my_account(D),
    Def = tx_defaults(channel_settle_tx, #{acct => Account}, D),
    Opts1 = maps:merge(Def, Opts),
    ChanId = maps:get(channel_id, Opts1),
    {ok, IAmt} = aesc_offchain_state:balance(I, State),
    {ok, RAmt} = aesc_offchain_state:balance(R, State),
    TTL = adjust_ttl(maps:get(ttl, Opts1, 0)),
    SlashOpts = Opts1#{ channel_id => aeser_id:create(channel, ChanId)
                      , from_id    => aeser_id:create(account, Account)
                      , initiator_amount_final => IAmt
                      , responder_amount_final => RAmt
                      , ttl        => TTL},
    PinnedHeight = aetx_env:height(OnChainEnv),
    {ok, Tx} = new_onchain_tx_(aesc_settle_tx, SlashOpts, PinnedHeight, D),
    {ok, Tx, []};
new_onchain_tx(channel_close_solo_tx, Opts,
               #data{ on_chain_id = ChanId
                    , opts = #{initiator := Initiator,
                               responder := Responder}
                    , state       = State } = D,
               _BlockHash, OnChainEnv, _OnChainTrees, _ChannelOpts) ->
    Account = my_account(D),
    TTL = adjust_ttl(maps:get(ttl, Opts, 0)),
    {Round, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    {ok, Channel} = aec_chain:get_channel(ChanId),
    OnChainRound = aesc_channels:round(Channel),
    Payload = case Round =:= OnChainRound of
                  false ->
                      aetx_sign:serialize_to_binary(SignedTx);
                  true ->
                      %% on-chain tx, we assume included in the chain
                      <<>>
              end,
    {ok, Poi} = aesc_offchain_state:poi([{account, Initiator},
                                         {account, Responder}], State),
    Opts1 =
          Opts#{ channel_id => aeser_id:create(channel, ChanId)
               , from_id    => aeser_id:create(account, Account)
               , payload    => Payload
               , poi        => Poi
               , ttl        => TTL},
    PinnedHeight = aetx_env:height(OnChainEnv),
    {ok, Tx} = new_onchain_tx_(aesc_close_solo_tx, Opts1, PinnedHeight, D),
    {ok, Tx, []};
new_onchain_tx(channel_snapshot_solo_tx, Opts,
               #data{ on_chain_id = ChanId
                    , state       = State } = D,
               _BlockHash, OnChainEnv, _OnChainTrees, _ChannelOpts) ->
    PinnedHeight = aetx_env:height(OnChainEnv),
    Account = my_account(D),
    TTL = adjust_ttl(maps:get(ttl, Opts, 0)),
    {Round, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    case aesc_utils:is_offchain_tx(SignedTx) of
        true ->
            case later_round_than_onchain(ChanId, Round) of
                true ->
                    Payload = aetx_sign:serialize_to_binary(SignedTx),
                    Opts1 =
                        Opts#{ channel_id => aeser_id:create(channel, ChanId)
                             , from_id    => aeser_id:create(account, Account)
                             , payload    => Payload
                             , ttl        => TTL },
                    {ok, Tx} = new_onchain_tx_(aesc_snapshot_solo_tx, Opts1,
                                               PinnedHeight, D),
                    {ok, Tx, []};
                false ->
                    {error, already_onchain}
            end;
        false ->
            {error, not_offchain_tx}
    end;
new_onchain_tx(channel_slash_tx, Opts,
               #data{ on_chain_id = ChanId
                    , opts = #{initiator := Initiator,
                               responder := Responder}
                    , state       = State} = D,
               _BlockHash, OnChainEnv, _OnChainTrees, _ChannelOpts) ->
    Account = my_account(D),
    Def = tx_defaults(channel_slash_tx, #{acct => Account}, D),
    Opts1 = maps:merge(Def, Opts),
    TTL = adjust_ttl(maps:get(ttl, Opts1, 0)),
    {ok, Poi} = aesc_offchain_state:poi([{account, Initiator},
                                         {account, Responder}], State),
    Opts2 =
        Opts1#{ channel_id => aeser_id:create(channel, ChanId)
              , from_id    => aeser_id:create(account, Account)
              , poi        => Poi
              , ttl        => TTL },
    PinnedHeight = aetx_env:height(OnChainEnv),
    {ok, Tx} = new_onchain_tx_(aesc_slash_tx, Opts2, PinnedHeight, D),
    {ok, Tx, []}.

-spec new_onchain_tx_(aesc_create_tx | aesc_withdraw_tx | aesc_deposit_tx |
                      aesc_force_progress_tx |
                      aesc_close_solo_tx | aesc_snapshot_solo_tx |
                      aesc_slash_tx | aesc_settle_tx | aesc_close_mutual_tx,
                      map(), non_neg_integer(),
                      #data{}) -> {ok, aetx:tx()}.
new_onchain_tx_(Mod, Opts, CurrHeight, D) when Mod =:= aesc_create_tx;
                                               Mod =:= aesc_withdraw_tx;
                                               Mod =:= aesc_deposit_tx;
                                               Mod =:= aesc_force_progress_tx;
                                               Mod =:= aesc_close_solo_tx;
                                               Mod =:= aesc_snapshot_solo_tx;
                                               Mod =:= aesc_slash_tx;
                                               Mod =:= aesc_settle_tx;
                                               Mod =:= aesc_close_mutual_tx ->
    FeeSpecified = maps:is_key(fee, Opts),
    GasPriceSpecified = maps:is_key(gas_price, Opts),
    case FeeSpecified of
        true when not GasPriceSpecified -> %% use preset fee
            call_callback_new(Mod, Opts, D);
        true when GasPriceSpecified -> %% both fee and gas_price are specified
            {ok, Tx1} = call_callback_new(Mod, Opts, D),
            {ok, Tx2} = create_with_minimum_fee(Mod, Opts#{fee => 0}, % gas_price is already in Opts
                                                CurrHeight, D),
            %% use the tx with the bigger fee
            case aetx:fee(Tx1) > aetx:fee(Tx2) of
                true -> {ok, Tx1};
                false -> {ok, Tx2}
            end;
        false ->
            create_with_minimum_fee(Mod, Opts#{fee => 0}, % if gas_price is provided in Opts, it will be used
                                    CurrHeight, D)
    end.

%% @doc A valid transaction fee is a function on gas required and gas price used
%% the following function uses the gas price the node would be using if it
%% were mining gas required is a funcion of:
%%
%%   * transaction type (base_gas)
%%   * transaction size (size_gas)
%%   * gas needed for contract execution in a channel_force_progress
%%
%% Since the fee is part of the serialization of the transaction,
%% modifying the fee, might change the serialized transaction size and thus
%% changing the size_gas required for the transaction. Increasing the number
%% for the fee might result in the transaction requiring even more fee.
%% That's why we make 5 attempts for computing the minimal fee
create_with_minimum_fee(Mod, Opts, CurrHeight, D) ->
    create_with_minimum_fee(Mod, Opts, CurrHeight, D, 5).

create_with_minimum_fee(_, _, _, _, Attempts) when Attempts < 1 ->
    error(could_not_compute_fee);
create_with_minimum_fee(Mod, Opts, CurrHeight, D, Attempts) ->
    {ok, Tx} = call_callback_new(Mod, Opts, D),
    MinFee = min_tx_fee(Tx, Opts),
    TxFee = aetx:fee(Tx),
    {_CurrHeight, CurrProtocol} = curr_height_and_protocol(),
    FeeGas = aetx:fee_gas(Tx, CurrHeight, CurrProtocol),
    GasPrice = MinFee div FeeGas,
    case MinFee =< TxFee of
        true ->
            case Mod of
                aesc_force_progress_tx ->
                    {aesc_force_progress_tx, FPTx} =
                        aetx:specialize_callback(Tx),
                    [Update] = aesc_force_progress_tx:updates(FPTx),                    
                    case aesc_offchain_update:get_gas_price(Update) of
                        {ok, UpdateGasPrice} when UpdateGasPrice >= GasPrice -> %% ok with this gas price
                            {ok, Tx};
                        {ok, _OtherGasPrice} ->
                            Opts1 =
                                Opts#{update => aesc_offchain_update:set_gas_price(GasPrice,
                                                                  Update)},
                            create_with_minimum_fee(Mod, Opts1, CurrHeight, D, Attempts - 1)
                    end;
                _ ->
                    {ok, Tx}
            end;
        false ->
            Opts1 = Opts#{fee => MinFee},
            Opts2 =
                case maps:find(update, Opts1) of
                    error -> Opts1; %% not FP
                    {ok, Update} -> %% force progress tx, bump gas_price in
                                     %% the update as well
                        Opts1#{update =>
                               aesc_offchain_update:set_gas_price(GasPrice,
                                                                  Update)}
                end,
            create_with_minimum_fee(Mod, Opts2, CurrHeight, D, Attempts - 1)
    end.

create_tx_for_signing(#data{opts = #{ initiator        := Initiator
                                    , initiator_amount := IAmt
                                    , responder_amount := RAmt
                                    , channel_reserve  := ChannelReserve
                                    , lock_period      := LockPeriod } = Opts } = D) ->
    NoDelegates =
        case curr_protocol() of
            Protocol when Protocol < ?IRIS_PROTOCOL_VSN -> [];
            _PostIris -> {[], []}
        end,
    Obligatory =
        #{ acct => Initiator
         , initiator_amount => IAmt
         , responder_amount => RAmt
         , channel_reserve  => ChannelReserve
         , lock_period      => LockPeriod
         , delegate_ids     => NoDelegates},
    %% nonce is not exposed to the client via WebSocket but is used in tests
    %% shall we expose it to the client as well?
    Optional = maps:with([nonce, fee, gas_price], Opts),
    new_onchain_tx_for_signing(channel_create_tx,
                               maps:merge(Obligatory, Optional), D).

dep_tx_for_signing(Opts, D) ->
    new_onchain_tx_for_signing(channel_deposit_tx, Opts, D).

wdraw_tx_for_signing(Opts, D) ->
    new_onchain_tx_for_signing(channel_withdraw_tx, Opts, D).

close_mutual_tx_for_signing(Opts, D) ->
    Account = my_account(D),
    new_close_mutual_tx(Opts#{acct => Account}, D).

%% @doc The responding side creates a 'throwaway' close_mutual_tx, in order to
%% validate the tx received from the initiating side. The critical parts to
%% validate are the state-related ones. Nonce, fee and origin are copied from
%% the original. Once validated, the 'fake' tx is discarded.
fake_close_mutual_tx(Mod, Tx, D) ->
    OtherAccount = other_account(D),
    Nonce  = Mod:nonce(Tx),
    Fee    = Mod:fee(Tx),
    From   = Mod:origin(Tx),
    OtherAccount = From,  %% assertion
    new_close_mutual_tx(#{ nonce => Nonce
                         , fee => Fee
                         , acct => From }, D).

new_close_mutual_tx(Opts, D) ->
    new_onchain_tx_for_signing(channel_close_mutual_tx, Opts, return, D).

slash_tx_for_signing(Opts, #data{state = St} = D) ->
    {Round, SignedTx} = aesc_offchain_state:get_latest_signed_tx(St),
    slash_tx_for_signing(Round, SignedTx, Opts, D).

slash_tx_for_signing(_Round, SignedTx, Opts, D) ->
    Account = my_account(D),
    new_onchain_tx_for_signing(channel_slash_tx,
                               Opts#{ acct => Account
                                    , payload => aetx_sign:serialize_to_binary(SignedTx)
                                    }, D).

settle_tx_for_signing(Opts, D) ->
    Account = my_account(D),
    new_onchain_tx_for_signing(channel_settle_tx, Opts#{acct => Account}, D).

close_solo_tx_for_signing(Opts, D) ->
    Account = my_account(D),
    new_onchain_tx_for_signing(channel_close_solo_tx, Opts#{acct => Account}, D).

snapshot_solo_tx_for_signing(Opts, D) ->
    Account = my_account(D),
    new_onchain_tx_for_signing(channel_snapshot_solo_tx, Opts#{acct => Account},
                               _OnErr = return, D).

force_progress_tx_for_signing(Opts, D) ->
    Account = my_account(D),
    new_onchain_tx_for_signing(channel_force_progress_tx, Opts#{acct => Account}, D).

tx_defaults(Type, Opts, #data{ on_chain_id = ChanId } = D) ->
    Default = tx_defaults_(Type, Opts, D),
    Default#{ channel_id => ChanId
            , nonce => default_nonce(Opts) }.

tx_defaults_(channel_create_tx, _Opts, _D) ->
    #{};
tx_defaults_(channel_deposit_tx = Tx, Opts, D) ->
    Opts1 = tx_defaults_(channel_create_tx, Opts, D),
    default_ttl(Tx, Opts1, D);
tx_defaults_(channel_withdraw_tx, Opts, D) ->
    tx_defaults_(channel_deposit_tx, Opts, D); %% same as deposit defaults
tx_defaults_(channel_force_progress_tx = Tx, Opts, D) ->
    default_ttl(Tx, Opts, D);
tx_defaults_(channel_slash_tx = Tx, Opts, D) ->
    default_ttl(Tx, Opts, D);
tx_defaults_(channel_snapshot_solo_tx = Tx, Opts, D) ->
    default_ttl(Tx, Opts, D);
tx_defaults_(channel_close_solo_tx = Tx, Opts, D) ->
    default_ttl(Tx, Opts, D);
tx_defaults_(channel_settle_tx = Tx, Opts, D) ->
    default_ttl(Tx, Opts, D);
tx_defaults_(channel_close_mutual_tx = Tx, Opts, D) ->
    default_ttl(Tx, Opts, D).

default_nonce(Opts) ->
    case maps:find(nonce, Opts) of
        {ok, Nonce} ->
            Nonce;
        error ->
            Pubkey = maps:get(acct, Opts),
            get_nonce(Pubkey)
    end.

get_nonce(Pubkey) ->
    {value, Account} = aec_chain:get_account(Pubkey),
    case aec_accounts:type(Account) of
        basic ->
            {ok, N} = aec_next_nonce:pick_for_account(Pubkey),
            N;
        generalized ->
            0
    end.

default_ttl(_Type, Opts, #data{opts = DOpts}) ->
    TTL = maps:get(ttl, Opts, maps:get(ttl, DOpts, 0)),
    Opts#{ttl => adjust_ttl(TTL)}.

adjust_ttl(undefined) ->
    CurrHeight = aec_headers:height(aec_chain:top_header()),
    CurrHeight + ?DEFAULT_FSM_TX_TTL_DELTA;
adjust_ttl(TTL) when is_integer(TTL), TTL >= 0 ->
    TTL.

-spec curr_hash_and_height() -> {aec_blocks:block_header_hash(),
                                 aec_blocks:height()}.
curr_hash_and_height() ->
    TopHeader = aec_chain:top_header(),
    {ok, Hash} = aec_headers:hash_header(TopHeader),
    Height = aec_headers:height(TopHeader),
    {Hash, Height}.

curr_height_and_protocol() ->
    TopHeader = aec_chain:top_header(),
    {aec_headers:height(TopHeader), aec_headers:version(TopHeader)}.

-spec curr_height() -> aec_blocks:height().
curr_height() ->
    {_, Height} = curr_hash_and_height(),
    Height.

curr_protocol() ->
    TopHeader = aec_chain:top_header(),
    aec_headers:version(TopHeader).

-spec protocol_at_height(aec_blocks:height()) -> aec_hard_forks:protocol_vsn().
protocol_at_height(Height) ->
    {ok, Header} = aec_chain:get_key_header_by_height(Height),
    aec_headers:version(Header).

pick_hash(#data{block_hash_delta = #bh_delta{ not_newer_than = NNT
                                            , not_older_than = NOT
                                            , pick           = PickDelta }}) ->
    %% Using the boundary of the range is a bit too risky with regard of
    %% synking. That's why we use an offset
    Offset = min(NOT, NNT + PickDelta),
    TopHeader = aec_chain:top_header(),
    Height = aec_headers:height(TopHeader),
    %% use upper limit
    {ok, Header} = aec_chain:get_key_header_by_height(max(Height - NNT -
                                                          Offset, 0)),
    {ok, Hash} = aec_headers:hash_header(Header),
    Hash.

load_pinned_env(?NOT_SET_BLOCK_HASH) ->
    {_OnChainEnv, _OnChainTrees} =
        aetx_env:tx_env_and_trees_from_top(aetx_contract);
load_pinned_env(BlockHash) ->
    try aetx_env:tx_env_and_trees_from_hash(aetx_contract, BlockHash) of
        {_OnChainEnv, _OnChainTrees} = Pinned ->
            Pinned
    ?CATCH_LOG(Reason)
        error(unknown_block_hash)
    end.

-spec pick_onchain_env(map(), #data{}) ->
    {aec_blocks:block_header_hash(), aetx_env:env(), aec_trees:trees()}.
pick_onchain_env(#{block_hash := ?NOT_SET_BLOCK_HASH}, _D) ->
    {OnChainEnv, OnChainTrees} =
        aetx_env:tx_env_and_trees_from_top(aetx_contract),
    {?NOT_SET_BLOCK_HASH, OnChainEnv, OnChainTrees};
pick_onchain_env(Opts, D) ->
    BlockHash =
        case maps:find(block_hash, Opts) of
            error ->
                _BH = pick_hash(D);
            {ok, BH} when is_binary(BH) -> %% ?NOT_SET_BLOCK_HASH is handled
                BH
        end,
    {OnChainEnv, OnChainTrees} = load_pinned_env(BlockHash),
    {BlockHash, OnChainEnv, OnChainTrees}.

new_contract_tx_for_signing(Opts, From, #data{ state = State
                                             , opts = ChannelOpts
                                             , on_chain_id = ChannelId } = D) ->
    #{ owner       := Owner
     , vm_version  := VmVersion
     , abi_version := ABIVersion
     , code        := Code
     , deposit     := Deposit
     , call_data   := CallData
     } = Opts,
    Id = aeser_id:create(account, Owner),
    Updates = [aesc_offchain_update:op_new_contract(Id, VmVersion, ABIVersion, Code,
                                                    Deposit, CallData)
              | meta_updates(Opts)],
    {BlockHash, OnChainEnv, OnChainTrees} = pick_onchain_env(Opts, D),
    ActiveProtocol = aetx_env:consensus_version(OnChainEnv),
    try
        Tx1 = aesc_offchain_state:make_update_tx(Updates, State, ChannelId, ActiveProtocol,
                                                 OnChainTrees, OnChainEnv,
                                                 channel_reserve(ChannelOpts)),
        case request_signing(?UPDATE, Tx1, Updates, BlockHash, D, defer) of
            {ok, Send, D1, Actions} ->
                %% reply before sending sig request
                gen_statem:reply(From, ok),
                Send(),
                next_state(awaiting_signature, set_ongoing(D1), Actions);
            {error, _} = Error ->
                gen_statem:reply(From, Error),
                keep_state(D)
        end
    ?CATCH_LOG(E)
        process_update_error(E, From, D)
    end.

pay_close_mutual_fee(Fee, IAmt, RAmt) ->
    Ceil  = trunc(math:ceil(Fee/2)),
    Floor = trunc(math:floor(Fee/2)),
    if (IAmt + RAmt) < Fee                    -> {error, insufficient_balance};
       (IAmt >= Ceil) andalso (RAmt >= Floor) -> {ok, IAmt - Ceil, RAmt - Floor};
       (RAmt >= Ceil) andalso (IAmt >= Floor) -> {ok, IAmt - Floor, RAmt - Ceil};
       (IAmt > RAmt)                          -> {ok, IAmt - Fee + RAmt, 0};
       true                                   -> {ok, 0, RAmt - Fee + IAmt}
    end.

my_account(#data{role = initiator, opts = #{initiator := I}}) -> I;
my_account(#data{role = responder, opts = #{responder := R}}) -> R.

other_account(#data{role = initiator, opts = #{responder := R}}) -> R;
other_account(#data{role = responder, opts = #{initiator := I}}) -> I.

both_accounts(Data) ->
    [other_account(Data),
     my_account(Data)].

send_funding_created_msg(SignedTx, #data{ channel_id = Ch
                                        , session    = Sn
                                        , op = #op_ack{ tag  = create_tx
                                                      , data = OpData}} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    #op_data{block_hash = BlockHash} = OpData,
    Msg = #{ temporary_channel_id => Ch
           , block_hash           => BlockHash
           , data                 => #{tx      => TxBin,
                                       updates => []}},
    aesc_session_noise:funding_created(Sn, Msg),
    log(snd, ?FND_CREATED, Msg, Data).

check_funding_created_msg(#{ temporary_channel_id := ChanId
                           , block_hash           := BlockHash
                           , data                 := #{ tx      := TxBin
                                                      , updates := UpdatesBin }} = Msg
                           , #data{ state = State
                                  , channel_id = ChanId } = Data) ->
    Updates = [aesc_offchain_update:deserialize(U) || U <- UpdatesBin],
    try SignedTx = aetx_sign:deserialize_from_binary(TxBin),
        Checks =
            [ fun() -> verify_signatures_channel_create(SignedTx, initiator) end,
              fun() -> check_block_hash(BlockHash, Data) end,
              fun() -> check_update_tx_initial(SignedTx, Updates, State) end
            ],
        case aeu_validation:run(Checks) of
            ok ->
                {_, Data1} = on_chain_id(Data, SignedTx),
                Data2 = log(rcv, ?FND_CREATED, Msg, Data1),
                {ok, SignedTx, Updates, BlockHash, Data2};
            {error, _} = Error ->
                Error
        end
    ?CATCH_LOG(_E)
        {error, invalid_funding}
    end.

send_funding_signed_msg(SignedTx, #data{ channel_id = Ch
                                       , session    = Sn
                                       , op = #op_min_depth{ tag = ?WATCH_FND
                                                           , data = OpData}} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    #op_data{block_hash = BlockHash} = OpData,
    Msg = #{ temporary_channel_id  => Ch
           , block_hash            => BlockHash
           , data                  => #{tx => TxBin}},
    aesc_session_noise:funding_signed(Sn, Msg),
    log(snd, ?FND_SIGNED, Msg,
        set_ongoing(?FND_SIGNED, Data)).

check_funding_signed_msg(#{ temporary_channel_id := ChanId
                          %% since it is co-authenticated already, we don't
                          %% check the block hash being reported
                          , block_hash           := BlockHash
                          , data                 := #{tx := TxBin}} = Msg,
                          #data{ channel_id = ChanId } = Data) ->
    try SignedTx = aetx_sign:deserialize_from_binary(TxBin),
        Checks =
            [ fun() -> verify_signatures_channel_create(SignedTx, both) end
            ],
        case aeu_validation:run(Checks) of
            ok ->
                Data1 = log(rcv, ?FND_SIGNED, Msg, Data),
                {ok, SignedTx, BlockHash, Data1};
            {error, _} = Err ->
                Err
        end
    ?CATCH_LOG(_E)
        {error, invalid_funding_ack}
    end.

send_funding_locked_msg(#data{channel_id  = TmpChanId,
                              on_chain_id = OnChainId,
                              session     = Sn} = Data) ->
    Msg = #{ temporary_channel_id => TmpChanId
           , channel_id           => OnChainId },
    aesc_session_noise:funding_locked(Sn, Msg),
    log(snd, ?FND_LOCKED, Msg, Data).

check_funding_locked_msg(#{ temporary_channel_id := TmpChanId
                          , channel_id           := OnChainId } = Msg,
                         #data{channel_id  = MyTmpChanId,
                               on_chain_id = MyOnChainId} = Data) ->
    case TmpChanId == MyTmpChanId of
        true ->
            case OnChainId == MyOnChainId of
                true ->
                    {ok, log(rcv, ?FND_LOCKED, Msg, Data)};
                false ->
                    {error, channel_id_mismatch}
            end;
        false ->
            {error, temporary_channel_id_mismatch}
    end.

send_deposit_created_msg(SignedTx, Updates,
                         #data{ on_chain_id = Ch
                              , session = Sn
                              , op = #op_ack{ tag  = deposit_tx
                                            , data = OpData}} = Data) ->
    UBins = [aesc_offchain_update:serialize(U) || U <- Updates],
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    #op_data{block_hash = BlockHash} = OpData,
    Msg = #{ channel_id => Ch
           , block_hash => BlockHash
           , data       => #{tx => TxBin,
                             updates => UBins}},
    aesc_session_noise:deposit_created(Sn, Msg),
    log(snd, ?DEP_CREATED, Msg,
        set_ongoing(?DEP_CREATED, Data)).

check_deposit_created_msg(#{ channel_id := ChanId
                           , block_hash := BlockHash
                           , data       := #{tx      := TxBin,
                                             updates := UpdatesBin}} = Msg,
                          #data{on_chain_id = ChanId} = Data) ->
    Updates = [aesc_offchain_update:deserialize(U) || U <- UpdatesBin],
    try SignedTx = aetx_sign:deserialize_from_binary(TxBin),
        Checks =
            [ fun() -> check_block_hash(BlockHash, Data) end,
              fun() ->
                  check_tx(SignedTx, Updates, BlockHash, aesc_deposit_tx,
                           Data, not_deposit_tx)
              end,
              fun() ->
                  verify_signatures(SignedTx, Data,
                                    pubkeys(other_participant, Data, SignedTx))
              end
            ],
        case aeu_validation:run(Checks) of
            ok ->
                {ok, SignedTx, Updates, BlockHash, log(rcv, ?DEP_CREATED, Msg, Data)};
            {error, _} = Err -> Err
        end
    ?CATCH_LOG(_E)
        {error, invalid_deposit}
    end.

send_deposit_signed_msg(SignedTx, #data{ on_chain_id = Ch
                                       , session     = Sn
                                       , op = #op_min_depth{ tag  = ?WATCH_DEP
                                                           , data = OpData}} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    #op_data{block_hash = BlockHash} = OpData,
    Msg = #{ channel_id  => Ch
           , block_hash  => BlockHash
           , data        => #{tx => TxBin}},
    aesc_session_noise:deposit_signed(Sn, Msg),
    log(snd, ?DEP_SIGNED, Msg, Data).

check_deposit_signed_msg(#{ channel_id := ChanId
                          , block_hash := _BlockHash
                          , data       := #{tx := TxBin}} = Msg
                          , #data{ on_chain_id = ChanId
                                 , op = #op_ack{ tag = deposit_tx
                                               , data = OpData}} = Data) ->
    #op_data{signed_tx = ExpectedTx} = OpData,
    try SignedTx = aetx_sign:deserialize_from_binary(TxBin),
        Checks =
            [ fun() ->
                  check_tx_if_expected(SignedTx, ExpectedTx, not_deposit_tx)
              end,
              fun() ->
                  maybe_verify_signatures(SignedTx, Data)
              end
            ],
        case aeu_validation:run(Checks) of
            ok ->
                Data1 = log(rcv, ?DEP_SIGNED, Msg, Data),
                {ok, SignedTx, Data1};
            {error, _} = Err ->
                Err
        end
    ?CATCH_LOG(_E)
        {error, invalid_deposit_ack}
    end.

send_deposit_locked_msg(TxHash, #data{on_chain_id = ChanId,
                                      session     = Sn} = Data) ->
    Msg = #{ channel_id => ChanId
           , data       => #{tx_hash => TxHash} },
    aesc_session_noise:deposit_locked(Sn, Msg),
    log(snd, ?DEP_LOCKED, Msg, Data).

check_deposit_locked_msg(#{ channel_id := ChanId
                          , data       := #{tx_hash := TxHash} } = Msg,
                         SignedTx,
                         #data{on_chain_id = ChanId} = Data) ->
    case aetx_sign:hash(SignedTx) of
        TxHash ->
            {ok, log(rcv, ?DEP_LOCKED, Msg, Data)};
        _ ->
            {error, deposit_tx_hash_mismatch}
    end;
check_deposit_locked_msg(_, _, _) ->
    {error, channel_id_mismatch}.

check_deposit_error_msg(Msg, D) ->
    check_op_error_msg(?DEP_ERR, Msg, D).

check_withdraw_error_msg(Msg, D) ->
    check_op_error_msg(?WDRAW_ERR, Msg, D).

check_update_err_msg(Msg, D) ->
    check_op_error_msg(?UPDATE_ERR, Msg, D).

check_shutdown_err_msg(#{ channel_id := ChanId } = Msg,
                       #data{ on_chain_id = ChanId
                            , op = Op } = D) ->
    Process =
        fun(SignedTx) ->
            TxHash = aetx_sign:hash(SignedTx),
            case aec_chain:find_tx_location(TxHash) of
                Block when is_binary(Block) ->
                    lager:debug("Tx already on-chain. Ignore error msg", []),
                    {error, tx_already_on_chain};
                mempool ->
                    lager:debug("Tx in mempool. Treat as if already on-chain. Ignore error", []),
                    {error, tx_already_on_chain};
                not_found->
                    lager:debug("Tx not found. Assume it may later appear on-chain. Ignore error", []),
                    {error, tx_not_found};
                none ->
                    lager:debug("Tx location: 'none', i.e. it has been rejected. Accept error msg", []),
                    check_op_error_msg(?SHUTDOWN_ERR, Msg, D)
            end
        end,
    case Op of
        #op_close{data = #op_data{signed_tx = SignedTx}} ->
            Process(SignedTx);
        #op_ack{tag  = shutdown, data = #op_data{signed_tx = SignedTx}} ->
            Process(SignedTx);
        _ ->
            check_op_error_msg(?SHUTDOWN_ERR, Msg, D)
    end;
check_shutdown_err_msg(_, _) ->
    {error, chain_id_mismatch}.

check_op_error_msg(Op, #{ channel_id := ChanId
                        , error_code := ErrCode } = Msg,
                   #data{ on_chain_id = ChanId } = D) ->
    case fall_back_to_stable_state(D) of
        {ok, D1} ->
            {ok, ErrCode, log(rcv, Op, Msg, D1)};
        _Other ->
            {error, fallback_state_mismatch}
    end;
check_op_error_msg(_, _, _) ->
    {error, chain_id_mismatch}.

fall_back_to_stable_state(#data{ state = State } = D) ->
    {LastRound, _} = aesc_offchain_state:get_latest_signed_tx(State),
    case aesc_offchain_state:get_fallback_state(State) of
        {LastRound, State1} -> %% same round
            {ok, clear_ongoing(D#data{ state = State1
                                     , op    = ?NO_OP })};
        _Other ->
            lager:debug("Fallback state mismatch: ~p/~p",
                        [LastRound, _Other]),
            {error, fallback_state_mismatch}
    end.

send_withdraw_created_msg(SignedTx, Updates,
                          #data{ on_chain_id = Ch
                               , session = Sn
                               , op = #op_ack{ tag  = withdraw_tx
                                             , data = OpData}} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    UBins = [aesc_offchain_update:serialize(U) || U <- Updates],
    #op_data{block_hash = BlockHash} = OpData,
    Msg = #{ channel_id => Ch
           , block_hash => BlockHash
           , data       => #{tx      => TxBin,
                             updates => UBins}},
    aesc_session_noise:wdraw_created(Sn, Msg),
    log(snd, ?WDRAW_CREATED, Msg,
        set_ongoing(?WDRAW_CREATED, Data)).

check_withdraw_created_msg(#{ channel_id := ChanId
                            , block_hash := BlockHash
                            , data       := #{ tx      := TxBin
                                             , updates := UpdatesBin }} = Msg,
                  #data{on_chain_id = ChanId} = Data) ->
    Updates = [aesc_offchain_update:deserialize(U) || U <- UpdatesBin],
    try SignedTx = aetx_sign:deserialize_from_binary(TxBin),
        Checks =
            [ fun() -> check_block_hash(BlockHash, Data) end,
              fun() ->
                  check_tx(SignedTx, Updates, BlockHash, aesc_withdraw_tx,
                           Data, not_withdraw_tx)
              end,
              fun() ->
                  verify_signatures(SignedTx, Data,
                                    pubkeys(other_participant, Data,
                                            SignedTx))
              end
            ],
        case aeu_validation:run(Checks) of
            ok ->
                {ok, SignedTx, Updates, BlockHash, log(rcv, ?WDRAW_CREATED, Msg, Data)};
            {error, _} = Err ->
                Err
        end
    ?CATCH_LOG(_E)
        {error, invalid_withdrawal}
    end;
check_withdraw_created_msg(_, _) ->
    {error, channel_id_mismatch}.

send_withdraw_signed_msg(SignedTx, #data{ on_chain_id = Ch
                                        , session = Sn
                                        , op = #op_min_depth{ tag  = ?WATCH_WDRAW
                                                            , data = OpData}} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    #op_data{block_hash = BlockHash} = OpData,
    Msg = #{ channel_id  => Ch
           , block_hash  => BlockHash
           , data        => #{tx => TxBin}},
    aesc_session_noise:wdraw_signed(Sn, Msg),
    log(snd, ?WDRAW_SIGNED, Msg, Data).

check_withdraw_signed_msg(#{ channel_id := ChanId
                           %% since it is co-authenticated already, we ignore
                           %% the block hash being reported
                           , block_hash := _BlockHash
                           , data       := #{tx := TxBin}} = Msg,
                          #data{ on_chain_id = ChanId
                               , op = #op_ack{tag = withdraw_tx} = Op } = Data) ->
    #op_ack{data = #op_data{signed_tx = ExpectedTx}} = Op,
    try SignedTx = aetx_sign:deserialize_from_binary(TxBin),
        Checks =
            [ fun() ->
                  check_tx_if_expected(SignedTx, ExpectedTx, not_withdraw_tx)
              end,
              fun() ->
                  maybe_verify_signatures(SignedTx, Data)
              end
            ],
        case aeu_validation:run(Checks) of
            ok ->
                {ok, SignedTx, log(rcv, ?WDRAW_SIGNED, Msg, Data)};
            {error, _} = Err ->
                Err
        end
    ?CATCH_LOG(_E)
        {error, invalid_withdrawal_ack}
    end;
check_withdraw_signed_msg(_, _) ->
    {error, channel_id_mismatch}.

send_withdraw_locked_msg(TxHash, #data{on_chain_id = ChanId, session = Sn} = Data) ->
    Msg = #{ channel_id => ChanId
           , data       => #{tx_hash => TxHash} },
    aesc_session_noise:wdraw_locked(Sn, Msg),
    log(snd, ?WDRAW_LOCKED, Msg, Data).

check_withdraw_locked_msg(#{ channel_id := ChanId
                           , data       := #{tx_hash := TxHash} } = Msg,
                          SignedTx,
                          #data{on_chain_id = ChanId} = Data) ->
    case aetx_sign:hash(SignedTx) of
        TxHash ->
            {ok, log(rcv, ?WDRAW_LOCKED, Msg, Data)};
        _ ->
            {error, withdraw_tx_hash_mismatch}
    end;
check_withdraw_locked_msg(_, _, _) ->
    {error, channel_id_mismatch}.

send_update_msg(SignedTx, Updates,
                #data{ on_chain_id = OnChainId
                     , session = Sn
                     , op = #op_ack{ tag  = ?UPDATE
                                   , data = OpData}} = Data) ->
    UBins = [aesc_offchain_update:serialize(U) || U <- Updates],
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    #op_data{block_hash = BlockHash} = OpData,
    Msg = #{ channel_id => OnChainId
           , block_hash => BlockHash
           , data       => #{ tx      => TxBin
                            , updates => UBins }},
    aesc_session_noise:update(Sn, Msg),
    log(snd, ?UPDATE, Msg,
        set_ongoing(?UPDATE, Data)).

check_update_msg(Msg, D) ->
    lager:debug("check_update_msg(~p)", [Msg]),
    try check_update_msg_(Msg, D)
    ?CATCH_LOG(E)
        {error, E}
    end.

check_update_msg_(#{ channel_id := ChanId
                   , block_hash := BlockHash
                   , data       := #{ tx      := TxBin
                                    , updates := UpdatesBin }} = Msg,
                  #data{ on_chain_id = ChanId } = D) ->
    Updates = [aesc_offchain_update:deserialize(U) || U <- UpdatesBin],
    try aetx_sign:deserialize_from_binary(TxBin) of
        SignedTx ->
            case check_signed_update_tx(SignedTx, Updates, BlockHash, D) of
                ok ->
                    {ok, SignedTx, Updates, BlockHash, log(rcv, ?UPDATE, Msg, D)};
                {error, _} = Err ->
                    Err
            end
    ?CATCH_LOG(E)
        {error, {deserialize, E}}
    end.

check_signed_update_tx(SignedTx, Updates, BlockHash, #data{} = D) ->
    lager:debug("check_signed_update_tx(~p)", [SignedTx]),
    Checks =
        [ fun() -> check_block_hash(BlockHash, D) end,
          fun() ->
              check_tx(SignedTx, Updates, BlockHash, aesc_offchain_tx,
                       D, not_offchain_tx)
          end,
          fun() ->
              verify_signatures(SignedTx, D,
                                pubkeys(other_participant, D, SignedTx))
          end
        ],
    try aeu_validation:run(Checks) of
        ok -> ok;
        {error, _} = Error ->
            Error
    ?CATCH_LOG(_E)
        {error, invalid_update_ack}
    end.

check_update_tx_initial(SignedTx, Updates, State) ->
    aesc_offchain_state:check_initial_update_tx(SignedTx, Updates, State).

check_update_tx(SignedTx, Updates, BlockHash, State, Opts, ChannelPubkey) ->
    {OnChainEnv, OnChainTrees} = load_pinned_env(BlockHash),
    ActiveProtocol = aetx_env:consensus_version(OnChainEnv),
    aesc_offchain_state:check_update_tx(SignedTx, Updates, State,
                                        ChannelPubkey,
                                        ActiveProtocol,
                                        OnChainTrees, OnChainEnv,
                                        channel_reserve(Opts)).

check_update_ack_msg(Msg, D) ->
    lager:debug("check_update_ack_msg(~p)", [Msg]),
    try check_update_ack_msg_(Msg, D)
    ?CATCH_LOG(E)
        {error, E}
    end.

check_update_ack_msg_(#{ channel_id := ChanId
                       , data       := #{tx := TxBin} } = Msg,
                      #data{on_chain_id = ChanId} = D) ->
    try aetx_sign:deserialize_from_binary(TxBin) of
        SignedTx ->
            check_signed_update_ack_tx(SignedTx, Msg, D)
    ?CATCH_LOG(E)
        {error, {deserialize, E}}
    end.

check_signed_update_ack_tx(SignedTx, Msg,
                           #data{ state = State
                                , opts = Opts
                                , op = #op_ack{tag = ?UPDATE} = Op} = D) ->
    #op_ack{ tag = ?UPDATE
           , data = #op_data{updates    = Updates,
                             block_hash = BlockHash}} = Op,
    HalfSignedTx = aesc_offchain_state:get_latest_half_signed_tx(State),
    %% since it is co-authenticated already, we ignore
    %% the block hash being reported
    Checks =
        [ fun() -> check_update_ack_(SignedTx, HalfSignedTx) end,
          fun() ->
              verify_signatures(SignedTx, D,
                                pubkeys(both, D, SignedTx))
          end
        ],
    try aeu_validation:run(Checks) of
        ok ->
            {OnChainEnv, OnChainTrees} = load_pinned_env(BlockHash),
            {ok, D#data{ state = aesc_offchain_state:set_signed_tx(
                                    SignedTx, Updates, State, OnChainTrees, OnChainEnv, Opts)
                        , log = log_msg(rcv, ?UPDATE_ACK, Msg, D#data.log)}};
        {error, _} = Err -> Err
    ?CATCH_LOG(_E)
        {error, invalid_update_ack}
    end.

check_update_ack_(SignedTx, HalfSignedTx) ->
    HalfSigs = aetx_sign:signatures(HalfSignedTx),
    lager:debug("HalfSigs = ~p", [HalfSigs]),
    Sigs = aetx_sign:signatures(SignedTx),
    lager:debug("Sigs = ~p", [Sigs]),
    Remainder = Sigs -- HalfSigs,
    lager:debug("Remainder = ~p", [Remainder]),
    ExpectedTx =
        aetx_sign:innermost_tx(SignedTx) == aetx_sign:innermost_tx(HalfSignedTx),
    case ExpectedTx of
        true ->
            lager:debug("Txes are the same", []);
        false ->
            error(different_tx)
    end,
    ok.

handle_upd_transfer(FromPub, ToPub, Amount, From, UOpts, #data{ state = State
                                                              , opts = Opts
                                                              , on_chain_id = ChannelId
                                                              } = D) ->
    {BlockHash, OnChainEnv, OnChainTrees} = pick_onchain_env(UOpts, D),
    %% off-chain transfers do not need to be pinned as their execution does not
    %% depend on a specific environment
    ActiveProtocol = aetx_env:consensus_version(OnChainEnv),
    try
        Updates = [aesc_offchain_update:op_transfer(aeser_id:create(account, FromPub),
                                                    aeser_id:create(account, ToPub), Amount)
                   | meta_updates(UOpts)],
        Tx1 = aesc_offchain_state:make_update_tx(Updates, State, ChannelId, ActiveProtocol,
                                                 OnChainTrees, OnChainEnv,
                                                 channel_reserve(Opts)),
        case request_signing(?UPDATE, Tx1, Updates, BlockHash, D, defer) of
            {ok, Send, D1, Actions} ->
                %% reply before sending sig request
                gen_statem:reply(From, ok),
                Send(),
                next_state(awaiting_signature, D1, Actions);
            {error, _} = Error ->
                gen_statem:reply(From, Error),
                keep_state(D)
        end
    ?CATCH_LOG(E)
        process_update_error(E, From, D)
    end.

meta_updates(Opts) when is_map(Opts) ->
    L = maps:get(meta, Opts, []),
    [aesc_offchain_update:op_meta(D) || D <- L,
                                        is_binary(D)].

send_leave_msg(#data{ on_chain_id = ChId
                    , session     = Session} = Data) ->
    Msg = #{ channel_id => ChId },
    aesc_session_noise:leave(Session, Msg),
    log(snd, ?LEAVE, Msg, Data).

check_leave_msg(#{ channel_id := ChId } = Msg,
                #data{on_chain_id = ChId0} = Data) ->
    case ChId == ChId0 of
        true ->
            {ok, log(rcv, ?LEAVE, Msg, Data)};
        false ->
            {error, channel_id_mismatch}
    end.

send_leave_ack_msg(#data{ on_chain_id = ChId
                        , session     = Session} = Data) ->
    Msg = #{ channel_id => ChId },
    aesc_session_noise:leave_ack(Session, Msg),
    log(snd, ?LEAVE_ACK, Msg, Data).

check_leave_ack_msg(#{channel_id := ChId} = Msg,
                    #data{on_chain_id = ChId0} = Data) ->
    case ChId == ChId0 of
        true ->
            {ok, log(rcv, ?LEAVE_ACK, Msg, Data)};
        false ->
            {error, channel_id_mismatch}
    end.

shutdown_msg_received(Msg, D) ->
    case check_shutdown_msg(Msg, D) of
        {ok, SignedTx, Updates, BlockHash, D1} ->
            shutdown_msg_received(SignedTx, Updates, BlockHash, D1);
        {error, _E} ->
            handle_recoverable_error(#{ code => ?ERR_VALIDATION
                                      , respond => true
                                      , msg_type => ?SHUTDOWN}, D)
    end.

shutdown_msg_received(SignedTx, Updates, BlockHash, D) ->
    D1 = report(info, shutdown, D),
    case request_signing_(?SHUTDOWN_ACK, SignedTx, Updates, BlockHash, D1) of
        {ok, D2, Actions} ->
            next_state(awaiting_signature, D2, Actions);
        {error, E} ->
            lager:debug("Couldn't co-sign shutdown req: ~p", [E]),
            keep_state(D1)
    end.

send_shutdown_msg(SignedTx, #data{session = Session} = Data) ->
    Msg = shutdown_msg(SignedTx, Data),
    aesc_session_noise:shutdown(Session, Msg),
    log(snd, ?SHUTDOWN, Msg, Data).

send_shutdown_ack_msg(SignedTx, #data{session = Session} = Data) ->
    Msg = shutdown_msg(SignedTx, Data),
    aesc_session_noise:shutdown_ack(Session, Msg),
    log(snd, ?SHUTDOWN_ACK, Msg, Data).

shutdown_msg(SignedTx, #data{ on_chain_id = OnChainId
                            , op = #op_sign{ tag  = Shutdown
                                           , data = OpData}})
    when Shutdown =:= ?SHUTDOWN;
         Shutdown =:= ?SHUTDOWN_ACK ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    #op_data{block_hash = BlockHash} = OpData,
    #{ channel_id => OnChainId
     , block_hash => BlockHash
     , data       => #{tx => TxBin} }.

check_shutdown_msg(#{ channel_id := ChanId
                    , block_hash := BlockHash
                    , data := #{tx := TxBin}} = Msg
                    , #data{on_chain_id = ChanId} = D) ->
    Updates = [],
    try SignedTx = aetx_sign:deserialize_from_binary(TxBin),
        {aesc_close_mutual_tx, RealTxI} =
            aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
        {ok, FakeCloseTx, [], _BlockHash} = fake_close_mutual_tx(aesc_close_mutual_tx,
                                                                  RealTxI, D),
        {channel_close_mutual_tx, FakeTxI} = aetx:specialize_type(FakeCloseTx),
        Checks =
            shutdown_tx_checks(SignedTx, BlockHash, other_participant, D) ++
            [ fun() ->
                  case (serialize_close_mutual_tx(FakeTxI) =:=
                            serialize_close_mutual_tx(RealTxI)) of
                      true ->
                          ok;
                      false ->
                          {error, shutdown_tx_validation}
                  end
              end
            ],
        case aeu_validation:run(Checks) of
            ok ->
                {ok, SignedTx, Updates, BlockHash, log(rcv, ?SHUTDOWN, Msg, D)};
            {error, _} = Error ->
                Error
        end
    ?CATCH_LOG(_E)
        {error, invalid_shutdown}
    end.

shutdown_tx_checks(SignedTx, BlockHash, WhoSignedIt,
                   #data{strict_checks = StrictChecks} = D) ->
    shutdown_tx_checks(SignedTx, BlockHash, WhoSignedIt, D, StrictChecks).

shutdown_tx_checks(SignedTx, BlockHash, WhoSignedIt, D, StrictChecks) ->
    Updates = [],
    [ fun() -> check_block_hash(BlockHash, D) end,
      fun() ->
          check_tx(SignedTx, Updates, BlockHash, aesc_close_mutual_tx,
                  D, not_close_mutual_tx)
      end,
      fun() ->
          case StrictChecks of
              false -> ok;
              true ->
                  verify_signatures(SignedTx, D,
                                    pubkeys(WhoSignedIt, D, SignedTx))
          end
      end].

serialize_close_mutual_tx(Tx) ->
    {_, Elems} = aesc_close_mutual_tx:serialize(Tx),
    lists:keydelete(nonce, 1, Elems).

check_shutdown_ack_msg(#{ data       := #{tx := TxBin}
                        %% since it is co-authenticated already, we ignore
                        %% the block hash being reported
                        , block_hash := _BlockHash} = Msg,
                       #data{ op = #op_ack{tag = shutdown} = Op } = D) ->
    #op_ack{data = #op_data{ signed_tx  = MySignedTx
                           , block_hash = BlockHash
                           , updates    = Updates } } = Op,
    try SignedTx = aetx_sign:deserialize_from_binary(TxBin),
        Checks =
            [ fun() ->
                  check_tx_if_expected(SignedTx, MySignedTx,
                                       not_close_mutual_tx)
              end,
              fun() ->
                  maybe_verify_signatures(SignedTx, D)
              end],
        case aeu_validation:run(Checks) of
            ok ->
                {ok, SignedTx, Updates, BlockHash, log(rcv, ?SHUTDOWN_ACK, Msg, D)};
            {error, _} = Error ->
                Error
        end
    ?CATCH_LOG(_E)
        {error, invalid_shutdown}
    end.

send_inband_msg(To, Info, #data{session = Session} = D) ->
    ChanId = cur_channel_id(D),
    From = my_account(D),
    M = #{ channel_id => ChanId
         , from       => From
         , to         => To
         , info       => Info },
    aesc_session_noise:inband_msg(Session, M),
    log(snd, ?INBAND_MSG, M, D).

check_inband_msg(_, #data{ client_connected = false }) ->
    %% Inband messages don't require co-signing, but there is no
    %% client to forward to.
    {error, client_disconnected};
check_inband_msg(#{ channel_id := ChanId
                  , from       := From
                  , to         := To } = Msg,
                 #data{on_chain_id = ChanId} = D) ->
    case {my_account(D), other_account(D)} of
        {To, From}     ->  {ok, log(rcv, ?INBAND_MSG, Msg, D)};
        {To, _Other}   ->  {error, invalid_sender};
        {_Other, From} ->  {error, invalid_recipient};
        _ ->
            {error, invalid_addresses}
    end;
check_inband_msg(_, _) ->
    {error, chain_id_mismatch}.

authenticate_reconnect(FsmIdWrapper, #data{on_chain_id = ChId, fsm_id_wrapper = undefined} = D) ->
    %% If we don't store the fsm_id then we have a ChId and the cache was initialized
    aesc_state_cache:authenticate_user(ChId, my_account(D), FsmIdWrapper);
authenticate_reconnect(ProvidedFsmIdWrapper, #data{fsm_id_wrapper = ExpectedFsmIdWrapper}) ->
    %% The channel opening sequence is still in progress
    aesc_fsm_id:compare(ProvidedFsmIdWrapper, ExpectedFsmIdWrapper).

fallback_to_stable_state(#data{state = State} = D) ->
    clear_ongoing(D#data{ state = aesc_offchain_state:fallback_to_stable_state(State)
                        , op    = ?NO_OP }).

tx_round(Tx) -> call_cb(Tx, round, []).

-spec call_cb(aetx:tx(), atom(), list()) -> any().
call_cb(Tx, F, Args) ->
    {Mod, TxI} = specialize_cb(Tx),
    do_apply(Mod, F, [TxI|Args]).

specialize_cb(Tx) ->
    aetx:specialize_callback(Tx).

do_apply(M, F, A) ->
    apply(M, F, A).

send_update_ack_msg(SignedTx, #data{ on_chain_id = OnChainId
                                   , session     = Sn } = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id => OnChainId
           , data       => #{tx => TxBin} },
    aesc_session_noise:update_ack(Sn, Msg),
    log(snd, ?UPDATE_ACK, Msg, Data).

handle_update_conflict(Op, D) ->
    handle_update_conflict(Op, D, []).

%% @doc casts and calls have different expectations regarding responses:
%% not providing a {reply, ClientPid, Response} in the options will result
%% in dead-locking the WebSocket handler process
handle_update_conflict(Op, D, Opts) ->
    handle_recoverable_error(#{ code => ?ERR_CONFLICT
                              , respond => true
                              , msg_type => Op }, D, Opts).

handle_recoverable_error(ErrorInfo, D) ->
    handle_recoverable_error(ErrorInfo, D, []).

handle_recoverable_error(ErrorInfo, D, Opts) ->
    handle_recoverable_error(ErrorInfo, D, Opts, true).

-spec handle_recoverable_error( #{ code := _
                                 , msg_type => atom()
                                 , respond => boolean()}
                               , #data{}, list(), boolean()) -> next_fsm_state().
handle_recoverable_error(ErrorInfo, D, Opts, ReportConflict) ->
    handle_recoverable_error(open, ErrorInfo, D, Opts, ReportConflict).

handle_recoverable_error(NextState, ErrorInfo, D, Opts, ReportConflict) ->
    #data{role = Role} = D,
    lager:debug("~p ErrorInfo = ~p", [Role, ErrorInfo]),
    try
        D1 = send_recoverable_error_msg_(ErrorInfo,
                                         fallback_to_stable_state(D),
                                         ReportConflict),
        next_state(NextState, D1, Opts)
    ?CATCH_LOG(E)
        error(E)
    end.

make_recoverable_error_msg(#{code := ErrorCode}, #data{ state = State
                                                      , on_chain_id = ChanId}) ->
    {_, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    Round = tx_round(aetx_sign:innermost_tx(SignedTx)),
    #{ channel_id => ChanId
     , round      => Round
     , error_code => ErrorCode }.

-spec send_recoverable_error_msg_(map(), #data{}, boolean()) -> #data{}.
send_recoverable_error_msg_(Info, #data{session = Sn } = Data, ReportConflict) ->
    Msg = make_recoverable_error_msg(Info, Data),
    maybe_send_error_msg(Info, Sn, Msg, Data),
    Data1 = case ReportConflict of
                true ->
                    report(conflict, Msg, Data);
                false ->
                    Data
            end,
    log(snd, error_msg_type(Info, Data1), Msg, Data1).

maybe_send_error_msg(#{ respond := true } = Info, Sn, Msg, D) ->
    send_recoverable_error_msg(error_msg_type(Info, D), Sn, Msg);
maybe_send_error_msg(#{ respond := false }, _Sn, _Msg, _D) ->
    ok;
maybe_send_error_msg(#{ code := Code, msg_type := MsgType }, Sn, Msg, _) ->
    case starting_msg(MsgType) of
        true when Code =/= ?ERR_CONFLICT, Code =/= ?ERR_ONCHAIN_REJECTED ->
            %% Don't send; this hasn't touched the other fsm yet
            lager:debug("Not sending error msg to peer", []),
            ok;
        _ ->
            lager:debug("sending error msg (type: ~p) to peer: ~p", [MsgType, Msg]),
            send_recoverable_error_msg(error_msg_type(MsgType), Sn, Msg)
    end;
maybe_send_error_msg(_, Sn, Msg, #data{error_msg_type = Type}) ->
    send_recoverable_error_msg(Type, Sn, Msg).

error_msg_type(#{msg_type := Type}, _)          -> error_msg_type(Type);
error_msg_type(_, #data{error_msg_type = Type}) -> Type.

error_msg_type(?UPDATE)         -> ?UPDATE_ERR;
error_msg_type(?UPDATE_ACK)     -> ?UPDATE_ERR;
error_msg_type(?DEP_CREATED)    -> ?DEP_ERR;
error_msg_type(?DEP_SIGNED)     -> ?DEP_ERR;
error_msg_type(deposit_tx)      -> ?DEP_ERR;
error_msg_type(?WDRAW_CREATED)  -> ?WDRAW_ERR;
error_msg_type(?WDRAW_SIGNED)   -> ?WDRAW_ERR;
error_msg_type(withdraw_tx)     -> ?WDRAW_ERR;
error_msg_type(?SHUTDOWN)       -> ?SHUTDOWN_ERR;
error_msg_type(?SHUTDOWN_ACK)   -> ?SHUTDOWN_ERR;
error_msg_type(A) when is_atom(A) ->
    lager:debug("No error_msg_type(~p); reusing ~p", [A, ?UPDATE_ERR]),
    ?UPDATE_ERR.  % TODO: error msg types for other sequences?

%% These msg types indicate the start of a sequence. Unless otherwise specified,
%% this would lead us to guess that the error should be reported only locally.
starting_msg(?UPDATE    ) -> true;
starting_msg(deposit_tx ) -> true;
starting_msg(withdraw_tx) -> true;
starting_msg(?SHUTDOWN  ) -> true;
starting_msg(_) -> false.

send_recoverable_error_msg(undefined, _, _) ->
    lager:debug("no msg_type given - not sending error msg", []),
    ok;
send_recoverable_error_msg(?UPDATE_ERR, Sn, Msg) ->
    lager:debug("send update_error: ~p", [Msg]),
    aesc_session_noise:update_error(Sn, Msg);
send_recoverable_error_msg(?DEP_ERR, Sn, Msg) ->
    lager:debug("send deposit_error: ~p", [Msg]),
    aesc_session_noise:dep_error(Sn, Msg);
send_recoverable_error_msg(?WDRAW_ERR, Sn, Msg) ->
    lager:debug("send withdraw_error: ~p", [Msg]),
    aesc_session_noise:wdraw_error(Sn,Msg);
send_recoverable_error_msg(?SHUTDOWN_ERR, Sn, Msg) ->
    lager:debug("send shutdown_error: ~p", [Msg]),
    aesc_session_noise:shutdown_error(Sn, Msg).

request_signing(Tag, Aetx, Updates, BlockHash, #data{} = D) ->
    request_signing(Tag, Aetx, Updates, BlockHash, D, send).

request_signing(_, _, _, _, #data{ client_connected = false }, _) ->
    %% If we have to create the aetx_sign object, it's not yet signed,
    %% and with no connected client, we can't sign it.
    {error, client_disconnected};
request_signing(Tag, Aetx, Updates, BlockHash, #data{} = D, SendAction) ->
    request_signing_(Tag, aetx_sign:new(Aetx, []), Updates, BlockHash, D, SendAction).

-spec request_signing_(sign_tag(),
                       aetx_sign:signed_tx(),
                       [aesc_offchain_update:update()],
                       aec_blocks:block_header_hash(),
                       #data{}) -> {ok, #data{}, [gen_statem:action()]}
                                 | {error, any()}.
request_signing_(Tag, SignedTx, Updates, BlockHash, D) ->
    request_signing_(Tag, SignedTx, Updates, BlockHash, D, send).

-spec request_signing_(sign_tag(),
                       aetx_sign:signed_tx(),
                       [aesc_offchain_update:update()],
                       aec_blocks:block_header_hash(),
                       #data{}, send | defer) ->
                              {ok, #data{}, [gen_statem:action()]}
                            | {ok, fun(() -> ok), #data{}, [gen_statem:action()]}
                            | {error, client_disconnected}.
request_signing_(Tag, SignedTx, Updates, BlockHash, #data{client = Client} = D, SendAction) ->
    Info = #{ signed_tx => SignedTx
            , updates => Updates },
    Msg = rpt_message(#{ type => sign
                       , tag  => Tag
                       , info => Info }, D),
    D1 = D#data{ op = #op_sign{ tag = Tag
                              , data = #op_data{ signed_tx = SignedTx
                                               , block_hash = BlockHash
                                               , updates = Updates }}},
    D2 = log(req, sign, Msg, D1),
    SendF = sig_request_f(Client, Tag, Msg),
    %% If we have a client pid: send the request
    %% If not: proceed if our sig is already on the tx
    %% Note that has_my_signature/3 is a bit costly, so we don't
    %% check unless the client really is disconnected
    IsOk = is_pid(Client) orelse
        has_my_signature(my_account(D), SignedTx),
    case IsOk of
        true ->
            if is_pid(Client) ->
                    req_signing_ok(SendAction, SendF, D2, []);
               true ->
                    req_signing_ok(SendAction, SendF, D2,
                                   [{next_event, cast, {?SIGNED, Tag, SignedTx}}])
            end;
        false ->
            {error, client_disconnected}
    end.

%% @doc When in a handle_call(), we want to reply to the caller before sending
%% it some other message. In Erlang, using selective message reception, this
%% is irrelevant, but in other environments (at the other end of a websocket)
%% FIFO message handling can get confusing if we mix up the order. So in this
%% case, pass the send fun and let the calling function decide.
req_signing_ok(send, SendF, D, Actions) ->
    SendF(),
    {ok, D, Actions};
req_signing_ok(defer, SendF, D, Actions) ->
    {ok, SendF, D, Actions}.

sig_request_f(undefined, _, _) ->
    fun() -> ok end;
sig_request_f(Client, Tag, Msg) when is_pid(Client) ->
    fun() ->
            Client ! {?MODULE, self(), Msg},
            lager:debug("signing(~p) requested", [Tag])
    end.

%% @doc Checks if a user had provided authentication but doesn't check the
%% authentication itself
has_my_signature(Me, SignedTx) ->
    case aetx:specialize_callback(aetx_sign:tx(SignedTx)) of
        {aega_meta_tx, Tx} ->
            case aega_meta_tx:ga_pubkey(Tx) of
                Me -> true;
                _Other -> has_my_signature(Me, aega_meta_tx:tx(Tx)) %% go deeper
            end;
        {_NotGA, _} -> %% innermost transaction
            Protocol = curr_protocol(),
            ok =:= aetx_sign:verify_one_pubkey(Me, SignedTx, Protocol)
    end.

watcher_request(Type, SignedTx, Updates, #data{ op = Op
                                              , opts = Opts } = D) ->
    lager:debug("Type = ~p, SignedTx = ~p, Updates = ~p, Op = ~p", [Type, SignedTx, Updates, Op]),
    #{ minimum_depth := MinDepthFactor
     , minimum_depth_strategy := MinDepthStrategy } = Opts,
    MinDepth = min_depth(MinDepthStrategy, MinDepthFactor, SignedTx),
    BlockHash = block_hash_from_op(Op),
    {Mod, Tx} = aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    TxHash = aetx_sign:hash(SignedTx),
    evt({tx_hash, TxHash}),
    Nonce = Mod:nonce(Tx),
    evt({nonce, Nonce}),
    {OnChainId, D1} = on_chain_id(D, SignedTx),
    OpData = #op_data{ signed_tx = SignedTx
                     , block_hash = BlockHash
                     , updates = Updates },
    case Type of
        {?MIN_DEPTH, ?WATCH_FND = Sub} ->
            Reqs = [ min_depth_req(TxHash, MinDepth, Sub)
                   , watch_req() ],
            ok = watcher_reg(OnChainId, Reqs),
            Op1 = #op_min_depth{ tag = Sub
                               , tx_hash = TxHash
                               , data = OpData },
            D2 = D1#data{op = Op1},
            {ok, D2};
        {?MIN_DEPTH, Sub} ->
            ok = ask_watcher(OnChainId, min_depth_req(TxHash, MinDepth, Sub)),
            Op1 = #op_min_depth{ tag = Sub
                               , tx_hash = TxHash
                               , data = OpData },
            lager:debug("New Op = ~p", [Op1]),
            D2 = D1#data{op = Op1},
            {ok, D2};
        unlock ->
            ok = ask_watcher(OnChainId, unlock_req()),
            Op1 = #op_watch{ type = unlock
                           , tx_hash = TxHash
                           , data = OpData },
            D2 = D1#data{op = Op1},
            {ok, D2};
        close ->
            ok = ask_watcher(OnChainId, close_req(MinDepth)),
            Op1 = #op_watch{ type = close
                           , tx_hash = TxHash
                           , data = OpData },
            D2 = D1#data{op = Op1},
            {ok, D2}
        %% _ ->
        %%     lager:debug("Unknown Type = ~p", [Type]),
        %%     ok = ask_watcher(OnChainId, min_depth_req(TxHash, MinDepth, Type)),
        %%     Op1 = #op_watch{ type = Type
        %%                    , tx_hash = TxHash
        %%                    , data = OpData },
        %%     D2 = D1#data{op = Op1},
        %%     {ok, D2}
    end.

%% Chain watcher wrappers =====================================
%%
watcher_reg(ChId) ->
    watcher_reg(ChId, [watch_req()]).

watcher_reg(ChId, Reqs) ->
    ok = aesc_chain_watcher:register(ChId, ?MODULE, Reqs).

ask_watcher(ChId, Req) ->
    aesc_chain_watcher:request(ChId, Req).

min_depth_req(TxHash, MinDepth, Type) ->
    aesc_chain_watcher:min_depth_req(TxHash, MinDepth, Type).

close_req(MinDepth) ->
    aesc_chain_watcher:close_req(MinDepth).

unlock_req() ->
    aesc_chain_watcher:unlock_req().

watch_req() ->
    aesc_chain_watcher:watch_req().
%%
%% ============================================================

on_chain_id(#data{on_chain_id = ID} = D, _) when ID =/= undefined ->
    {ID, D};
on_chain_id(D, SignedTx) ->
    try aesc_utils:channel_pubkey(SignedTx) of
        {ok, PubKey} ->
            {PubKey, D#data{on_chain_id = PubKey}};
        {error, _} when D#data.strict_checks =:= false ->
            %% We can't derive a channel ID, but this is presumably a testing
            %% scenario. The intention of non-strict checks is that we shouldn't
            %% crash on validation errors, so leave things alone.
            {D#data.on_chain_id, D}
    ?CATCH_LOG(_E)
        error(_E)
    end.

initialize_cache(#data{ on_chain_id    = ChId
                      , state          = State
                      , opts           = Opts0
                      , fsm_id_wrapper = FsmIdWrapper} = D) ->
    Opts = maps:with(opts_to_cache(), Opts0),
    aesc_state_cache:new(ChId, my_account(D), State, Opts, FsmIdWrapper),
    D#data{fsm_id_wrapper = undefined}.

opts_to_cache() ->
    [ role
    , initiator
    , responder
    , connection
    , minimum_depth
    , lock_period
    , timeouts
    , report
    , log_keep
    , keep_running ].

cache_state(#data{ on_chain_id = ChId
                 , last_channel_change = LastTxHash
                 , state       = State } = D) ->
    aesc_state_cache:update(ChId, my_account(D), State, #{last_channel_change => LastTxHash}).

-spec handle_change_config(atom(), any(), #data{}) ->
    {ok, #data{}}
    | {error, invalid_config}.
handle_change_config(log_keep, Keep, #data{log = L} = D)
  when is_integer(Keep), Keep >= 0 ->
    {ok, D#data{log = aesc_window:change_keep(Keep, L)}};
handle_change_config(_, _, _) ->
    {error, invalid_config}.

report_update(#data{state = State, last_reported_update = Last} = D) ->
    case aesc_offchain_state:get_latest_signed_tx(State) of
        {Last, _} ->
            D;
        {New, SignedTx} ->
            D1 = report(update, SignedTx, D),
            cache_state(D1),
            D1#data{last_reported_update = New}
    end.

report_leave(#data{role = Role, opts = Opts, state = State} = D) ->
    {_, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    D1 = case {Role, maps:get(keep_running, Opts, false)} of
             {responder, true} ->
                 report_with_notice(leave, SignedTx, keep_running, D);
             _ ->
                 report(leave, SignedTx, D)
         end,
    cache_state(D1),
    D1.

report_on_chain_tx(Info, SignedTx, D) ->
    report_on_chain_tx(Info, signed_tx_type(SignedTx), SignedTx, D).

report_on_chain_tx(Info, Type, SignedTx, D) ->
    report(on_chain_tx, #{ tx => SignedTx
                         , type => Type
                         , info => Info}, D).

maybe_act_on_tx(channel_snapshot_solo_tx, SignedTx, D) ->
    MyPubkey = my_account(D),
    case call_cb(aetx_sign:innermost_tx(SignedTx), origin, []) of
        MyPubkey ->
            lager:debug("snapshot_solo_tx from my client", []),
            #data{op = OrigOp} = D,
            %% We want to order a local min_depth watch, but not log it as a wait state op.
            {ok, D1} = watcher_request({?MIN_DEPTH, ?WATCH_SNAPSHOT_SOLO}, SignedTx, [], D),
            D1#data{op = OrigOp};
        OtherPubkey ->
            lager:debug("snapshot_solo_tx from other client (~p)", [OtherPubkey]),
            D
    end;
maybe_act_on_tx(_, _, D) ->
    D.

signed_tx_type(SignedTx) ->
    {Type, _} = aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
    Type.

report_with_notice(Tag, Info, Notice, D) ->
    report_info(do_rpt(Tag, D), #{ type   => report
                                 , tag    => Tag
                                 , notice => Notice
                                 , info   => Info }, D).

report(Tag, Info, D) ->
    report_info(do_rpt(Tag, D), #{ type => report
                                 , tag  => Tag
                                 , info => Info }, D).

report_info(DoRpt, Msg0, #data{ role = Role
                              , client_connected = Connected
                              , client = Client} = D) ->
    if DoRpt ->
            Msg = rpt_message(Msg0, D),
            lager:debug("~p report_info(true, Client = ~p, Msg = ~p)",
                        [Role, Client, Msg]),
            maybe_send_rpt(Connected, Client, Msg),
            log(rpt, maps:get(tag, Msg), Msg, D);
       true  ->
            lager:debug("~p report_info(~p, ~p)", [Role, DoRpt, Msg0]),
            D
    end.

maybe_send_rpt(true, Client, Msg) ->
    Client ! {?MODULE, self(), Msg},
    ok;
maybe_send_rpt(false, _, Msg) ->
    lager:debug("No client. Msg = ~p", [Msg]),
    ok.

rpt_message(Msg, #data{on_chain_id = undefined}) ->
    Msg;
rpt_message(Msg, #data{on_chain_id = OnChainId} = D) ->
    maybe_add_fsm_id(Msg#{channel_id => OnChainId}, D).

maybe_add_fsm_id(#{info := funding_signed} = Msg, #data{fsm_id_wrapper = FsmIdW}) ->
    Msg#{fsm_id => FsmIdW};
maybe_add_fsm_id(#{info := funding_created} = Msg, #data{fsm_id_wrapper = FsmIdW}) ->
    Msg#{fsm_id => FsmIdW};
maybe_add_fsm_id(Msg, _) ->
    Msg.

do_rpt(Tag, #data{opts = #{report := Rpt}}) ->
    try maps:get(Tag, Rpt, false)
    ?CATCH_LOG(_E)
        false
    end.

log(Op, Type, M, #data{log = Log0} = D) ->
    D#data{log = log_msg(Op, Type, M, Log0)}.

log_msg(Op, Type, M, Log) ->
    aesc_window:add_new({Op, Type, os:timestamp(), M}, Log).

win_to_list(Log) ->
    aesc_window:to_list(Log).

filter_log(Log, Opts) ->
    N = maps:get(n, Opts, 10),
    {_, Res} = aesc_window:match(
                 fun(Msg, Acc) ->
                         match_f(Msg, Acc, Opts)
                 end, {N, []}, Log),
    lists:reverse(Res).

match_f({Type, Tag, _TS, _M} = Entry, {N, Found} = Acc, Opts) ->
    case match_filter(type, Type, Opts)
        andalso match_filter(tag, Tag, Opts) of
        true ->
            Found1 = [Entry|Found],
            case N - 1 of
                N1 when N1 =< 0 ->
                    {done, {N1, Found1}};
                N1 ->
                    {cont, {N1, Found1}}
            end;
        false ->
            {cont, Acc}
    end.

match_filter(Key, Val, Opts) ->
    case Opts of
        #{Key := L} ->
            lists:member(Val, L);
        _ ->
            %% allow if no filter on Key
            true
    end.


process_update_error({off_chain_update_error, Reason}, From, D) ->
    keep_state(D, [{reply, From, {error, Reason}}]);
process_update_error(Reason, From, D) ->
    keep_state(D, [{reply, From, {error, Reason}}]).

check_closing_event(St, #{tx := SignedTx} = Info, D) ->
    {Type, _Tx} = aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
    case Type =:= channel_force_progress_tx of
        true ->
            process_incoming_forced_progress(St, Info, D);
        false ->
            aec_db:ensure_activity(async_dirty,
                                  fun() ->
                                          try
                                              check_closing_event_(Info, D)
                                          ?CATCH_LOG(E)
                                              error(E)
                                          end
                                  end)
    end.

-spec check_closing_event_(map(), #data{}) ->
    {can_slash, LastRound :: non_neg_integer(), LatestCoSignedTx :: aetx_sign:signed_tx()} |
    {ok, proper_solo_closing, LatestCoSignedTx :: aetx_sign:signed_tx()} |
    {error, not_solo_closing}.
check_closing_event_(#{ tx := SignedTx
                      , channel := Ch
                      , block_hash := _BlockHash}, #data{state = St}) ->
    %% Get the latest channel object from the chain
    case aesc_channels:is_solo_closing(Ch) of
        true ->
            {MyLastRound, LastSignedTx} =
                aesc_offchain_state:get_latest_signed_tx(St),
            case aesc_utils:check_round_greater_than_last(
                   Ch, MyLastRound, slash) of
                ok ->
                    %% We have a later valid channel state
                    {can_slash, MyLastRound, LastSignedTx};
                {error, same_round} ->
                    {ok, proper_solo_closing, SignedTx};
                {error, old_round} ->
                    %% This is weird: presumably, our state is out-of-date
                    {ok, proper_solo_closing, SignedTx}
            end;
        false ->
            {error, not_solo_closing}
    end.

-spec verify_signatures_channel_create(aetx_sign:signed_tx(),
                                       initiator | both) -> ok | {error, atom()}.
verify_signatures_channel_create(SignedTx, Who) ->
    case aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)) of
        {aesc_create_tx, CreateTx} ->
            Initiator = aesc_create_tx:initiator_pubkey(CreateTx),
            Responder = aesc_create_tx:responder_pubkey(CreateTx),
            Both = [Initiator, Responder],
            PubkeysToCheck =
                case Who of
                    initiator -> [Initiator];
                    both -> Both
            end,
            SkipKeys = Both -- PubkeysToCheck,
            verify_signatures_onchain_skip(SkipKeys, SignedTx);
        _ ->
            {error, not_create_tx}
    end.

verify_signatures_onchain_check(Pubkeys, SignedTx) ->
    {Mod, Tx} = aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    {OnChainEnv, OnChainTrees} = authentication_env(),
    {ok, Participants} = Mod:signers(Tx, OnChainTrees),
    SkipKeys = Participants -- Pubkeys,
    case aesc_utils:verify_signatures_onchain(SignedTx, OnChainTrees, OnChainEnv,
                                              SkipKeys) of
        ok -> ok;
        {error, _E} -> {error, bad_signature}
    end.

verify_signatures_onchain_skip(SkipKeys, SignedTx) ->
    {OnChainEnv, OnChainTrees} = authentication_env(),
    case aesc_utils:verify_signatures_onchain(SignedTx, OnChainTrees, OnChainEnv,
                                              SkipKeys) of
        ok -> ok;
        {error, _E} -> {error, bad_signature}
    end.

verify_signatures_offchain(ChannelPubkey, Pubkeys, SignedTx) ->
    {OnChainEnv, OnChainTrees} = authentication_env(),
    {ok, Channel} = aec_chain:get_channel(ChannelPubkey),
    InitiatorPubkey = aesc_channels:initiator_pubkey(Channel),
    ResponderPubkey = aesc_channels:responder_pubkey(Channel),
    Participants = [InitiatorPubkey, ResponderPubkey],
    SkipKeys = Participants -- Pubkeys,
    Check = aesc_utils:verify_signatures_offchain(Channel, SignedTx, OnChainTrees,
                                                  OnChainEnv, SkipKeys),
    case Check of
        ok -> ok;
        _ -> {error, bad_signature}
    end.

later_round_than_onchain(ChannelPubkey, Round) ->
    {ok, Channel} = aec_chain:get_channel(ChannelPubkey),
    OnChainRound = aesc_channels:round(Channel),
    Round > OnChainRound.

%% @doc check authentication according latest top - although for fork safety's
%% sake updates can be pinned to older and safer blocks, validation of
%% authentication must be performed according the latest top
authentication_env() ->
    tx_env_and_trees_from_top(aetx_contract).

maybe_verify_signatures(SignedTx, #data{strict_checks = StrictChecks} = Data) ->
    case StrictChecks of
        false -> ok;
        true ->
            verify_signatures(SignedTx, Data, both_accounts(Data))
    end.

-spec verify_signatures(aetx_sign:signed_tx(), #data{}, [aec_keys:pubkey()]) ->
    ok | {error, atom()}.
verify_signatures(SignedTx, Data, Pubkeys) ->
    ChannelPubkey = cur_channel_id(Data),
    case aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)) of
        {OffChainMod, _Tx} when OffChainMod =:= aesc_offchain_tx ->
            verify_signatures_offchain(ChannelPubkey, Pubkeys, SignedTx);
        {_Mod, _Tx} ->
            verify_signatures_onchain_check(Pubkeys, SignedTx)
    end.

check_tx(SignedTx, Updates, BlockHash, Mod, Data, ErrTypeMsg) ->
    ChannelPubkey = cur_channel_id(Data),
    ExpectedRound = next_round(Data),
    #data{state = State, opts = Opts, on_chain_id = ChannelPubkey} = Data,
    case aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)) of
        {aesc_settle_tx, _Tx} ->
            %% TODO: conduct more relevant checks
            ok;
        {Mod, Tx} -> %% same callback module
            Checks =
                [ fun() -> check_channel_id(Mod, Tx, ChannelPubkey) end,
                  fun() -> check_round(Mod, Tx, ExpectedRound) end,
                  fun() ->
                      check_update_tx(SignedTx, Updates, BlockHash,
                                      State, Opts, ChannelPubkey)
                  end
                ],
            aeu_validation:run(Checks);
        E ->
            lager:debug("_E = ~p", [E]),
            {error, ErrTypeMsg}
    end.

-spec check_tx_if_expected(aetx_sign:signed_tx(), aetx_sign:signed_tx(),
                           atom()) -> ok | {error, atom()}.
check_tx_if_expected(NewSignedTx, OldSignedTx, WrongMod) ->
    {Mod, Tx} =
        aetx:specialize_callback(aetx_sign:innermost_tx(NewSignedTx)),
    {OldMod, OldTx} =
        aetx:specialize_callback(aetx_sign:innermost_tx(OldSignedTx)),
    ExpectedRound = get_round(OldMod, OldTx),
    ExpectedHash = get_state_hash(OldMod, OldTx),
    Checks =
        [ fun() -> check_channel_id(Mod, Tx, OldMod:channel_pubkey(OldTx)) end,
          fun() -> check_round(Mod, Tx, ExpectedRound) end,
          fun() -> check_state_hash(Mod, Tx, ExpectedHash) end,
          fun() ->
              case Mod =:= OldMod of
                  true -> ok;
                  false -> {error, WrongMod}
              end
          end
        ],
    aeu_validation:run(Checks).

get_round(aesc_close_mutual_tx, _)  -> no_round;
get_round(aesc_settle_tx, _)        -> no_round;
get_round(Mod, Tx)                  -> Mod:round(Tx).

get_state_hash(aesc_close_mutual_tx, _) -> no_state_hash;
get_state_hash(aesc_settle_tx, _)       -> no_state_hash;
get_state_hash(Mod, Tx)                 -> Mod:state_hash(Tx).

check_round(aesc_close_mutual_tx, _, _) -> ok; %% no round here
check_round(aesc_settle_tx, _, _)       -> ok; %% no round here
check_round(Mod, Tx, ExpectedRound) ->
    case Mod:round(Tx) of
        ExpectedRound ->
            ok;
        _OtherRound ->
            {error, wrong_round}
    end.

check_channel_id(Mod, Tx, ChannelPubkey) ->
    case Mod:channel_pubkey(Tx) of
        ChannelPubkey -> %% expected pubkey
            ok;
        _ -> %% unexpected pubkey
            {error, different_channel_id}
    end.

check_state_hash(aesc_close_mutual_tx, _, _) -> ok; %% no hash here
check_state_hash(aesc_settle_tx, _, _)       -> ok; %% no hash here
check_state_hash(Mod, Tx, StateHash) ->
    case Mod:state_hash(Tx) of
        StateHash -> %% expected hash
            ok;
        _ -> %% unexpected state hash
            {error, bad_state_hash}
    end.

maybe_check_sigs_create(_, _, _, NextState, #data{strict_checks = false}) ->
    lager:debug("strict_checks = false", []),
    NextState();
maybe_check_sigs_create(Tx, Updates, Who, NextState, #data{state = State} = D) ->
    CheckSigs =
        case verify_signatures_channel_create(Tx, Who) of
            ok ->
                case check_update_tx_initial(Tx, Updates, State) of
                    ok -> true;
                    {error, E} -> {false, E}
                end;
            {error, E1} ->
                {false, E1}
        end,
    case CheckSigs of
        true ->
            NextState();
        {false, E2} ->
            D1 = report(error, E2, D),
            keep_state(D1)
    end.

%% @doc this is a function to be called after this FSM's client had
%% authenticated a transaction. It uses latest stored transaction that was
%% sent to the client in order to validate signed transaction without
%% executing updates
maybe_check_auth(_, _, _, _, NextState, #data{strict_checks = false}) ->
    lager:debug("strict_checks = false", []),
    NextState();
maybe_check_auth(SignedTx, OpSign, WrongTxModMsg, Who, NextState, D)
        when Who =:= me
      orelse Who =:= other_participant
      orelse Who =:= both ->
    #op_sign{data = #op_data{signed_tx = OldTx},
             tag = OpTag} = OpSign,
    Pubkeys = pubkeys(Who, D, SignedTx),
    Checks =
        [ fun() ->
              check_tx_if_expected(SignedTx, OldTx, WrongTxModMsg)
          end,
          fun() ->
              verify_signatures(SignedTx, D, Pubkeys)
          end],
    case aeu_validation:run(Checks) of
        ok ->
            NextState();
        {error, E} ->
            D1 = report(error, E, D),
            handle_recoverable_error(#{ code => ?ERR_VALIDATION
                                      , msg_type => OpTag}, D1)
    end.

pubkeys(both, D, _) ->
    both_accounts(D);
pubkeys(Who, D, SignedTx) ->
    case aesc_utils:count_authentications(SignedTx) of
        N when N < 2 ->
            case Who of
                me -> [my_account(D)];
                other_participant -> [other_account(D)]
            end;
        _ ->
            both_accounts(D)
    end.

next_round(#data{state = State}) ->
    {Round, _} = aesc_offchain_state:get_latest_signed_tx(State),
    increment_round(Round).

increment_round(Round) ->
    Round + 1.

check_attach_info(Info, #data{opts = Opts} = D) ->
    #{initiator := I, responder := R} = Opts,
    check_attach_info(Info, I, R, D).

check_attach_info(#{reestablish := true} = Info, _I, R, #data{on_chain_id = ChannelId}) ->
    ChainHash = aec_chain:genesis_hash(),
    lager:debug("Info = ~p, ChainHash = ~p, ChannelId = ~p", [Info, ChainHash, ChannelId]),
    case Info of
        #{ chain_hash := ChainHash
         , channel_id := ChannelId
         , responder  := R
         , port       := _Port
         , gproc_key  := _K } ->
            ok;
        _ ->
            case maps:find(responder, Info) of
                {ok, R1} when R1 =/= R ->
                    {error, responder_key_mismatch};
                _Other ->
                    {error, unrecognized_attach_info}
            end
    end;
check_attach_info(#{ initiator := I1
                   , responder := R1
                   , port      := _P
                   , gproc_key := _K} = Info, I, R, _D) ->
    lager:debug("Info = ~p, I = ~p, R = ~p", [Info, I, R]),
    if I =:= any, R1 =:= R ->
            aesc_checks:account(I1, initiator_not_found);
       I1 =:= I, R1 =:= R ->
            ok;
       R1 =/= R ->   %% shouldn't happen
            {error, responder_key_mismatch};
       I1 =/= I ->
            {error, initiator_key_mismatch};
       true ->
            {error, unrecognized_attach_info}
    end;
check_attach_info(_Info, _I, _R, _D) ->
    lager:debug("Unrecognized: Info = ~p, I = ~p, R = ~p", [_Info, _I, _R]),
    {error, unrecognized_attach_info}.

%% ==================================================================
%% gen_statem callbacks

callback_mode() ->
    [ state_functions %% "... Module:StateName/3, is used."
    , state_enter     %% "... at every **state change**, call the state callback
                      %% with arguments `(enter, OldState, Data)`. ... a state
                      %% enter call will be done right before entering the initial
                      %% state ..."
    ].

-spec init(map()) -> {ok, InitialState, data(), [{timeout, Time::pos_integer(),
                                                   InitialState}, ...]}
                          when InitialState :: state_name().
init(#{opts := Opts} = Arg) ->
    case check_limits(Opts) of
        ok ->
            init_(Arg);
        {error, Reason} ->
            {stop, Reason}
    end.

check_limits(Opts) ->
    case maps:is_key(existing_channel_id, Opts) of
        true ->
            aesc_limits:register_returning();
        false ->
            aesc_limits:allow_new()
    end.

init_(#{opts := Opts0} = Arg) ->
    
    {ReestablishOpts, ConnectOpts, Opts1} =
        { maps:with(?REESTABLISH_OPTS_KEYS, Opts0)
        , connection_opts(Arg)
        , maps:without(?REESTABLISH_OPTS_KEYS ++ ?CONNECT_OPTS_KEYS, Opts0)
        },
    %% Verify parameters
    Opts2 = check_opts(
              [ fun check_minimum_depth_opt/1
              , fun check_timeout_opt/1
              , fun check_rpt_opt/1
              , fun check_log_opt/1
              , fun check_block_hash_deltas/1
              , fun check_keep_running_opt/1
              ], Opts1),
    Initiator = maps:get(initiator, Opts2),
    Opts3 = Opts2#{connection => ConnectOpts},
    SessionRes = start_noise_session(ReestablishOpts, Opts3),
    case {SessionRes, Initiator} of
        {{error, E}, _} ->
            lager:error("Failed to start noise session: Error = ~p", [E]),
            {stop, failed_noise_session_start};
        {{ok, SessionPid}, any} ->
            StateFun = fun(NewInitiator) ->
                init_state(NewInitiator, Opts3, ReestablishOpts)
            end,
            prepare_initial_state(Opts3, #{state => StateFun}, SessionPid, ReestablishOpts);
        {{ok, SessionPid}, _} ->
            case init_state(Initiator, Opts3, ReestablishOpts) of
                %% TODO: Handle missing state trees - the client should be able to
                %%       bootstrap the channel by providing a cosigned state
                {error, E} ->
                    {stop, E};
                {ok, StateRecovery} ->
                    prepare_initial_state(Opts3, StateRecovery, SessionPid, ReestablishOpts)
            end
    end.

maybe_save_session(initiator, Session) ->
    Session;
maybe_save_session(_, _) ->
    undefined.

connection_opts(#{opts := Opts} = Arg) ->
    maps:merge(maps:with([host,port], Arg),
               maps:with(?CONNECT_OPTS_KEYS, Opts)).

terminate(Reason, _State, #data{session = Sn} = Data) ->
    lager:debug("terminate(~p, ~p, _)", [Reason, _State]),
    Data1 = report(info, {died, Reason}, Data),
    _Data2 = report(debug, {log, win_to_list(Data#data.log)}, Data1),
    try aesc_session_noise:close(Sn)
    ?CATCH_LOG(_E)
        ok
    end,
    ok.

code_change(_OldVsn, OldState, OldData, _Extra) ->
    {ok, OldState, OldData}.

%% ======================================================================
%% FSM transitions

%% We set timers here to ensure that they are always set.
%% gen_statem cancels event timers each time an event arrives
%%
-spec next_state(atom(), #data{}) -> next_fsm_state().
next_state(St, D) -> next_state(St, D, []).

-spec next_state(atom(), #data{}, list()) -> next_fsm_state().
next_state(St, D, Opts) ->
    {next_state, St, cur_st(St, D), [timer_for_state(St, D)|Opts]}.

-spec keep_state(#data{}) -> keep_fsm_state().
keep_state(D) -> keep_state(D, []).

-spec keep_state(#data{}, list()) -> keep_fsm_state().
keep_state(D, Opts) ->
    {keep_state, D, [timer_for_state(D)|Opts]}.

-spec postpone(#data{}) -> keep_fsm_state().
postpone(D) ->
    keep_state(D, [postpone]).

cur_st(St, D) ->
    D#data{cur_statem_state = St}.

%% ==================================================================
%% Internal functions

-spec init_state(aec_keys:pubkey(), map(), map()) ->
    {ok, map()} | {error, atom()}.
init_state(Initiator, Opts, ReestablishOpts) ->
    CheckedOpts = maps:merge(Opts, ReestablishOpts),
    CheckedOpts1 = CheckedOpts#{initiator => Initiator},
    aesc_offchain_state:new(CheckedOpts1).

prepare_initial_state(Opts, StateRecovery, SessionPid, ReestablishOpts) ->
    #{state := State} = StateRecovery,
    FsmIdWrapper = aesc_fsm_id:new(),
    #{ client := Client
     , role := Role
     , block_hash_delta := BlockHashDelta
     , log_keep := LogKeep } = Opts,
    Reestablish = maps:is_key(existing_channel_id, ReestablishOpts),
    MaybeFsmIdWrapper = case Reestablish of
                            true ->
                                undefined;
                            false ->
                                FsmIdWrapper
                        end,
    Data = #data{ role             = Role
                , client           = Client
                , client_mref      = erlang:monitor(process, Client)
                , client_connected = true
                , block_hash_delta = BlockHashDelta
                , fsm_id_wrapper   = MaybeFsmIdWrapper
                , session          = maybe_save_session(Role, SessionPid)
                , opts             = Opts
                , state            = State
                , log              = aesc_window:new(LogKeep)
                },
    lager:debug("FSM started, Data = ~p", [pr_data(Data)]),
    %% TODO: Amend the fsm above to include this step. We have transport-level
    %% connectivity, but not yet agreement on the channel parameters. We will next send
    %% a channel_open() message and await a channel_accept().
    ChannelReserve =
        case Reestablish of
            true ->
                onchain_persisted_channel_reserve(maps:get(existing_channel_id,
                                                           ReestablishOpts));
            false -> not_used
        end,
    NextState = case {Role, Reestablish} of
       {_, true} ->
            OpMode =
                case Role of
                    initiator -> {connect, ReestablishOpts};
                    responder -> restart
                end,
            ChanId = maps:get(existing_channel_id, ReestablishOpts),
            Data1 = maybe_pick_up_last_change(
                      maps:get(cached_opts, StateRecovery, #{}),
                      Data#data{ channel_id  = ChanId
                               , on_chain_id = ChanId
                               , opts = Opts#{channel_reserve => ChannelReserve}
                               , op = ?NO_OP }),
            {ok, Next, Data2} = check_chain_before_reestablish(StateRecovery,
                                                               Data1),
            ok_next(Next, Data2#data{op = #op_reestablish{mode = OpMode}});
        {initiator, false} ->
            ok_next(awaiting_connect, Data);
        {responder, false} ->
            ok_next(awaiting_open, Data)
    end,
    %% The FSM is up - send the FSM ID to the user
    {ok, Next1, D, StOpts} = NextState,
    D1 = report(info, {fsm_up, FsmIdWrapper}, D),
    NextState1 = {ok, Next1, D1, StOpts},
    %% In case we are reestablishing a channel we change the token to a new one
    case Reestablish of
        true ->
            ChId = maps:get(existing_channel_id, ReestablishOpts),
            %% Shouldn't fail
            ok = aesc_state_cache:change_fsm_id(ChId, my_account(Data), FsmIdWrapper),
            NextState1;
        false ->
            NextState1
    end.

-spec check_change_config(atom(), any()) -> {ok, atom(), any()} | {error, invalid_config}.
check_change_config(log_keep, Keep) when is_integer(Keep), Keep >= 0 ->
    {ok, log_keep, Keep};
check_change_config(_, _) ->
    {error, invalid_config}.

%% @doc Returns the minimum depth to watch for the given transaction and
%% options. The calculation depends on the chosen strategy.
%%
%% Supported strategies: txfee
%%
%% txfee: A minimum_depth factor of 0 results in a static min_depth of 1 for all
%% transactions. For factors greater than 0 then the min_depth is calculated as
%% the nth root of the transaction fee divided by the minimum gas, where n is
%% the given minimum_depth factor.
-spec min_depth(minimum_depth_strategy(), minimum_depth_factor(), aetx_sign:signed_tx()) -> non_neg_integer().
min_depth(txfee, MinDepthFactor, SignedTx) when
      MinDepthFactor =/= undefined ->
    Tx = aetx_sign:tx(SignedTx),
    MinGas = aec_tx_pool:minimum_miner_gas_price(),
    TxFee = aetx:fee(Tx),
    FeeCoefficient = TxFee / MinGas,
    MinDepth = if
        MinDepthFactor > 0  ->
            ceil(math:pow(FeeCoefficient, 1 / MinDepthFactor));
        true ->
            1
    end,
    lager:debug("Calculated txfee-based MinDepth = ~p with FeeCoefficient = ~p, MinDepthFactor = ~p, "
                "TxFee = ~p, MinGas = ~p",
                [MinDepth, FeeCoefficient, MinDepthFactor, TxFee, MinGas]),
    MinDepth.

%% @doc Set default minimum depth parameters. If the role is initiator, no
%% change is made because the responder might provide these defaults when the
%% channel is accepted.
-spec check_minimum_depth_opt(opts()) -> opts().
check_minimum_depth_opt(#{role := initiator} = Opts) ->
    Opts;
check_minimum_depth_opt(Opts) ->
    MinDepthStrategy = maps:get(minimum_depth_strategy, Opts, ?DEFAULT_MINIMUM_DEPTH_STRATEGY),
    MinDepthFactor = maps:get(minimum_depth, Opts, default_minimum_depth(MinDepthStrategy)),
    lager:debug("Final minimum_depth parameters for responder: ~p / ~p",
                [MinDepthStrategy, MinDepthFactor]),
    Opts#{ minimum_depth          => MinDepthFactor
         , minimum_depth_strategy => MinDepthStrategy }.

check_timeout_opt(#{timeouts := TOs} = Opts) ->
    TOs1 = maps:merge(?DEFAULT_TIMEOUTS, TOs),
    Opts1 = Opts#{timeouts => TOs1},
    lager:debug("Timeouts: ~p", [Opts1]),
    Opts1;
check_timeout_opt(Opts) ->
    check_timeout_opt(Opts#{timeouts => #{}}).

check_rpt_opt(#{report := R} = Opts) when is_map(R) ->
    L = [{K,V} || {K,V} <- maps:to_list(R),
                  lists:member(K, report_tags()) andalso is_boolean(V)],
    ROpts = maps:from_list(L),
    lager:debug("Report opts = ~p", [ROpts]),
    Opts#{report => maps:merge(?DEFAULT_REPORT_FLAGS, ROpts)};
check_rpt_opt(#{report := R} = Opts) ->
    lager:error("Unknown report opts: ~p", [R]),
    Opts#{report => ?DEFAULT_REPORT_FLAGS};
check_rpt_opt(#{report_info := Rinfo} = Opts) when is_boolean(Rinfo)->
    ROpts = #{info => Rinfo},
    lager:debug("Report opts = ~p", [ROpts]),
    Opts#{report => maps:merge(?DEFAULT_REPORT_FLAGS, ROpts)};
check_rpt_opt(Opts) ->
    Opts#{report => ?DEFAULT_REPORT_FLAGS}.

check_log_opt(#{log_keep := Keep} = Opts) when is_integer(Keep), Keep >= 0 ->
    Opts;
check_log_opt(#{log_keep := Keep} = Opts) ->
    lager:error("Invalid 'log_keep' option: ~p", [Keep]),
    Opts#{log_keep => ?KEEP};
check_log_opt(Opts) ->
    Opts#{log_keep => ?KEEP}.

check_block_hash_deltas(#{block_hash_delta := #{ not_older_than := NOT
                                               , not_newer_than := NNT
                                               , pick           := Pick }} = Opts)
    when is_integer(NOT), is_integer(NNT), NOT >= 0, NNT >= 0, NOT >= NNT + Pick ->
    lager:debug("block hash is set, not_newer_than ~p, not_older_than ~p, pick ~p",
                 [NNT, NOT, Pick]),
    Delta = #bh_delta{ not_newer_than = NNT
                     , not_older_than = NOT
                     , pick           = Pick },
    Opts#{block_hash_delta => Delta};
check_block_hash_deltas(#{block_hash_delta := InvalidBHDelta} = Opts) ->
    lager:error("Invalid 'block_hash_delta' option: ~p", [InvalidBHDelta]),
    Opts1 = maps:remove(block_hash_delta, Opts),
    check_block_hash_deltas(Opts1);
check_block_hash_deltas(Opts) ->
    lager:debug("block hash not set, fallback mode", []),
    Delta = #bh_delta{ not_newer_than = 0 %% backwards compatibility
                     , not_older_than = 10
                     , pick           = 0 }, %% backwards compatibility
    Opts#{block_hash_delta => Delta}.

check_keep_running_opt(#{role := Role} = Opts) ->
    case {maps:find(keep_running, Opts), Role} of
        {{ok, Bool}, responder} when is_boolean(Bool) ->
            Opts;
        {{ok, _}, initiator} ->
            lager:debug("The 'keep_running' option not valid for initiator - ignoring", []),
            maps:remove(keep_running, Opts);
        {{ok, Other}, _} ->
            lager:debug("Invalid 'keep_running' option (~p) - ignoring", [Other]),
            maps:remove(keep_running, Opts);
        {error, _} ->
            Opts
    end.

check_opts([], Opts) ->
    Opts;
check_opts([H|T], Opts) ->
    check_opts(T, H(Opts)).

prepare_for_reestablish(#data{ opts = Opts
                             , on_chain_id = ChanId } = D) ->
    try
        {ok, _SessionPid} = start_noise_session(#{existing_channel_id => ChanId},
                                                Opts#{role => responder}),
        %% We don't save the session pid here
        D
    ?CATCH_LOG(_E)
            D
    end.

%% @doc As per CHANNELS.md, the responder is regarded as the one typically
%% providing the service, and the initiator connects.
start_noise_session(#{existing_channel_id := ChId},
                    #{ role       := responder
                     , responder  := Responder
                     , connection := #{port := Port} = COpts }) ->
    NoiseOpts = maps:get(noise, COpts, []),
    lager:debug("NoiseOpts = ~p", [NoiseOpts]),
    SessionOpts = #{ reestablish => true
                   , port        => Port
                   , chain_hash  => aec_chain:genesis_hash()
                   , channel_id  => ChId
                   , responder   => Responder },
    noise_accept(SessionOpts, NoiseOpts, _Attempts = 3, COpts);
start_noise_session(_,
                    #{ role       := responder
                     , responder  := Responder
                     , initiator  := Initiator
                     , connection := #{port := Port} = COpts }) ->
    NoiseOpts = maps:get(noise, COpts, []),
    lager:debug("NoiseOpts = ~p", [NoiseOpts]),
    SessionOpts = #{ initiator => Initiator
                   , responder => Responder
                   , port      => Port },
    noise_accept(SessionOpts, NoiseOpts, _Attempts = 3, COpts);
start_noise_session(_,
                    #{ role       := initiator
                     , connection := #{ host := Host
                                      , port := Port } = COpts }) ->
    NoiseOpts = maps:get(noise, COpts, []),
    lager:debug("COpts = ~p", [COpts]),
    aesc_session_noise:connect(Host, Port, NoiseOpts).

noise_accept(_SessionOpts, _NoiseOpts, Attempts, #{ responder := Responder
                                                  , initiator := Initiator})
    when Attempts < 1 ->
    Error = failed_spawning_noise,
    lager:error("Failed with ~p, Initiator: ~p, Responder: ~p",
                [Error, Initiator, Responder]),
    {error, Error};
noise_accept(SessionOpts, NoiseOpts, Attempts, COpts) ->
    case aesc_session_noise:accept(SessionOpts, NoiseOpts) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Err} ->
            lager:warning("Noise accept failed with ~p", [Err]),
            noise_accept(SessionOpts, NoiseOpts, Attempts - 1, COpts)
    end.


maybe_initialize_offchain_state(any, NewI, #data{state = F} = D) when is_function(F, 1) ->
    {ok, #{state := State} = Recovered} = F(NewI), %% Should not fail
    maybe_pick_up_last_change(Recovered, D#data{state = State});
maybe_initialize_offchain_state(_, _, D) ->
    D.

maybe_pick_up_last_change(#{last_channel_change := LastTxHash}, D) ->
    D#data{last_channel_change = LastTxHash};
maybe_pick_up_last_change(_, D) ->
    D.

ok_next(Next, Data) ->
    {ok, Next, cur_st(Next, Data), [timer_for_state(Next, Data)]}.

timer(Name, Msg, #data{opts = #{timeouts := TOs}}) ->
    timeout_(maps:get(Name, TOs), Msg).

timeout_(Timeout, Msg) when is_integer(Timeout); Timeout == infinity ->
    {timeout, Timeout, Msg}.

timer_for_state(#data{cur_statem_state = St} = D) when St =/= undefined ->
    timer_for_state(St, D).

timer_for_state(St, #data{opts = #{timeouts := TOs}} = D) ->
    case maps:find(St, TOs) of
        {ok, T} -> timeout_(T, St);
        error ->
            Alias = ?TIMER_SUBST(St),
            timer(Alias, St, D)
    end.

set_ongoing(D) ->
    D#data{ongoing_update = true}.

set_ongoing(Msg, D) ->
    D#data{ ongoing_update = true
          , error_msg_type = error_msg_type(Msg) }.

clear_ongoing(D) ->
    D#data{ ongoing_update = false
          , error_msg_type = undefined }.

error_binary(E) when is_atom(E) ->
    atom_to_binary(E, latin1).

send_error_msg(Reason, #data{session = Sn} = D) ->
    case cur_channel_id(D) of
        undefined ->
            %% no msg
            D;
        ChId ->
            case Reason of
                {error, E} ->
                    Eb = error_binary(E),
                    Msg = #{ channel_id => ChId
                           , data       => Eb },
                    D1 = report(error, Eb, D),
                    aesc_session_noise:generic_error(Sn, Msg),
                    D1;
                _ ->
                    %% no msg
                    D
            end
    end.

msg_type(Msg) when is_tuple(Msg) ->
    T = element(1, Msg),
    if ?KNOWN_MSG_TYPE(T) -> T;
       ?WATCHER_EVENT(T) -> T;
       true -> unknown
    end;
msg_type(T) -> T.

cur_channel_id(#data{on_chain_id = undefined, channel_id = ChId}) -> ChId;
cur_channel_id(#data{on_chain_id = ChId}) -> ChId.

gproc_register(#data{role = Role, channel_id = ChanId} = D) ->
    gproc_register_(ChanId, Role, D).

gproc_register_on_chain_id(#data{role = Role, on_chain_id = Id} = D)
  when Id =/= undefined ->
    gproc_register_(Id, Role, D).

gproc_register_(ChanId, Role, D) ->
    Pubkey = my_account(D),
    Nbr = gproc_name_by_role(ChanId, Role),
    Nbp = gproc_name_by_pubkey(ChanId, Pubkey),
    lager:debug("gproc:reg() Nbr = ~p; Nbp = ~p; prev = ~p",
                [Nbr, Nbp, gproc:info(self(), gproc)]),
    try_gproc_reg(Nbr, Pubkey),
    try_gproc_reg(Nbp).

try_gproc_reg(Key) ->
    try_gproc_reg(Key, undefined).

try_gproc_reg(Key, Value) ->
    try gproc:reg(Key, Value)
    ?CATCH_LOG(badarg)
        Prev = gproc:where(Key),
        lager:error("Couldn't register channel, K=~p, V=~p, Prev=~p",
                    [Key, Value, Prev]),
        error(badarg)
    end.

gproc_name_by_role(Id, Role) ->
    {n, l, {aesc_channel, {Id, role, Role}}}.

gproc_name_by_pubkey(Id, Pubkey) ->
    {n, l, {aesc_channel, {Id, key, Pubkey}}}.

evt(_Msg) ->
    ok.

has_gproc_key(Fsm, #{gproc_key := K}) ->
    try _ = gproc:get_value(K, Fsm),
          true
    ?CATCH_LOG(badarg)
        false
    end.

is_channel_locked(0) -> false;
is_channel_locked(LockedUntil) ->
    LockedUntil >= curr_height().

-spec withdraw_locked_complete(#op_data{}, #data{}) -> next_fsm_state().
withdraw_locked_complete(OpData, #data{state = State, opts = Opts} = D) ->
    #op_data{ signed_tx  = SignedTx
            , updates    = Updates
            , block_hash = BlockHash} = OpData,
    {OnChainEnv, OnChainTrees} = load_pinned_env(BlockHash),
    State1 = aesc_offchain_state:set_signed_tx(SignedTx, Updates, State,
                                               OnChainTrees, OnChainEnv, Opts),
    D1 = D#data{state = State1},
    next_state(open, D1).

-spec deposit_locked_complete(#op_data{}, #data{}) -> next_fsm_state().
deposit_locked_complete(OpData, #data{state = State, opts = Opts} = D) ->
    #op_data{ signed_tx  = SignedTx
            , updates    = Updates
            , block_hash = BlockHash} = OpData,
    {OnChainEnv, OnChainTrees} = load_pinned_env(BlockHash),
    lager:debug("Applying updates: ~p", [Updates]),
    State1 = aesc_offchain_state:set_signed_tx(SignedTx, Updates, State,
                                               OnChainTrees, OnChainEnv, Opts),
    D1 = D#data{state = State1},
    next_state(open, D1).

settle_signed(SignedTx, Updates, #data{ on_chain_id = ChId} = D) ->
    ?LOG_DEBUG(settle_signed, 3, [SignedTx]),
    case aec_chain:get_channel(ChId) of
        {ok, Ch} ->
            LockedUntil = aesc_channels:locked_until(Ch),
            case is_channel_locked(LockedUntil) of
                true ->
                    lager:debug("channel is locked", []),
                    {ok, D1} = watcher_request(unlock, SignedTx, Updates, D),
                    next_state(channel_closing, D1);
                false ->
                    lager:debug("channel not locked, pushing settle", []),
                    %% {ok, D1} = watcher_request(close, SignedTx, Updates, D),
                    try_to_push_to_mempool(SignedTx, D, channel_closing,
                                           fun(Dx) ->
                                               {ok, Dx1} =
                                                   watcher_request(close,
                                                                   SignedTx,
                                                                   Updates,
                                                                   Dx),
                                               Dx1
                                          end,
                                          channel_closing)
            end;
        {error,_} = Error ->
            close(Error, D)
    end.

force_progress_signed(SignedTx, #data{ on_chain_id = ChId } = D) ->
    lager:debug("SignedTx = ~p", [SignedTx]),
    case aec_chain:get_channel(ChId) of
        {ok, Channel} ->
            NextState =
                case aesc_channels:is_solo_closing(Channel) of
                    false -> open;
                    true -> channel_closing %% solo closing sequence
                end,
            try_to_push_to_mempool(SignedTx, D, NextState);
        {error,_} = Error ->
            close(Error, D)
    end.

snapshot_solo_signed(SignedTx, _Updates, #data{ on_chain_id = ChId } = D) ->
    lager:debug("SignedTx = ~p", [SignedTx]),
    case aec_chain:get_channel(ChId) of
        {ok, Ch} ->
            case aesc_channels:is_active(Ch) of
                true ->
                    try_to_push_to_mempool(SignedTx, D, open);
                false ->
                    %% TODO: do we want to fail siilently?
                    lager:debug("Snapshot_solo tx failed: channel not active", []),
                    next_state(open, D)
            end;
        {error,_} = Error ->
            close(Error, D)
    end.

close_solo_signed(SignedTx, _Updates, #data{ on_chain_id = ChId } = D) ->
    lager:debug("SignedTx = ~p", [SignedTx]),
    case aec_chain:get_channel(ChId) of
        {ok, Ch} ->
            LockedUntil = aesc_channels:locked_until(Ch),
            case is_channel_locked(LockedUntil) of
                false ->
                    lager:debug("channel not locked: solo-closing", []),
                    %% Let the watcher pick up the tx once it makes it onto the
                    %% chain
                    try_to_push_to_mempool(SignedTx, D, open,
                                           fun(D0) -> D0#data{op = ?NO_OP} end);
                true ->
                    lager:debug("channel is locked: cannot solo-close", []),
                    next_state(open, D)
            end;
        {error,_} = Error ->
            close(Error, D)
    end.

funding_locked_complete(#data{ op = #op_lock{ tag = create
                                            , data = OpData}
                             , create_tx = CreateTx
                             , state = State
                             , opts = Opts} = D) ->
    #op_data{ updates    = Updates
            , block_hash = BlockHash} = OpData,
    {OnChainEnv, OnChainTrees} = load_pinned_env(BlockHash),
    State1 = aesc_offchain_state:set_signed_tx(CreateTx, Updates, State, OnChainTrees,
                                               OnChainEnv, Opts),
    %% Garbage collecting here won't do anything as the old fsm state still
    %% contains the state_password_wrapper - the password still is in use
    %% TODO: Add a new state which would force garbage collection of the password
    next_state(open, initialize_cache(D#data{state = State1})).

close(Reason, D) ->
    close_(Reason, log(evt, close, Reason, D)).

close_(close_mutual, D) ->
    D1 = report(info, close_mutual, D),
    {stop, normal, D1};
close_(closed_confirmed, D) ->
    D1 = report(info, closed_confirmed, D),
    {stop, normal, D1};
close_(leave, D) ->
    {stop, normal, D};
close_(bad_signature, D) ->
    D1 = report(info, bad_signature, D),
    {stop, normal, D1};
close_(bad_state_hash, D) ->
    D1 = report(info, bad_state_hash, D),
    {stop, normal, D1};
close_(not_create_tx, D) ->
    D1 = report(info, not_create_tx, D),
    {stop, normal, D1};
close_({timeout, _}, D) ->
    D1 = report(info, timeout, D),
    {stop, normal, D1};
close_(Reason, D) ->
    D1 = try send_error_msg(Reason, D)
                  ?CATCH_LOG(_E)
                  D
         end,
    {stop, Reason, D1}.

handle_call(_, {?RECONNECT_CLIENT, Pid, FsmIdWrapper} = Msg, From,
            #data{ client_connected = false
                 , client_mref      = undefined
                 , client           = undefined } = D0) ->
    lager:debug("Client reconnect request with no client", []),
    D = log(rcv, msg_type(Msg), Msg, D0),
    case authenticate_reconnect(FsmIdWrapper, D) of
        true ->
            lager:debug("Client authentication successful; Client = ~p", [Pid]),
            MRef = erlang:monitor(process, Pid),
            D1 = D#data{ client           = Pid
                       , client_mref      = MRef
                       , client_connected = true },
            D2 = report(info, {fsm_up, FsmIdWrapper}, D1),
            keep_state(D2, [{reply, From, ok}]);
        false ->
            lager:debug("Client authentication failed", []),
            keep_state(D, [{reply, From, {error, invalid_fsm_id}}])
    end;
handle_call(_, {?RECONNECT_CLIENT, _, FsmIdWrapper}, From, #data{ client = Client } = D) when is_pid(Client) ->
    lager:debug("Client reconnect request with existing client", []),
    case authenticate_reconnect(FsmIdWrapper, D) of
        true ->
            lager:debug("Client authentication successful", []),
            keep_state(D, [{reply, From, {error, {existing_client, Client}}}]);
        false ->
            lager:debug("Client authentication failed", []),
            keep_state(D, [{reply, From, {error, invalid_fsm_id}}])
    end;
handle_call(St, Req, From, #data{} = D) ->
    lager:debug("handle_call(~p, ~p, ~p, ~p)", [St, Req, From, pr_data(D)]),
    try handle_call_(St, Req, From, D)
    ?CATCH_LOG(E, "maybe_block_hash_error")
        keep_state(D, [{reply, From, {error, E}}])
    end;
handle_call(_St, _Req, From, D) ->
    keep_state(D, [{reply, From, {error, unknown_request}}]).

handle_call_(awaiting_signature, {abort_update, Code, Tag}, From,
             #data{ op = #op_sign{tag = Tag}
                  , on_chain_id = ChannelId} = D) ->
    case { lists:member(Tag, ?CANCEL_SIGN_TAGS)
         , lists:member(Tag, ?CANCEL_ACK_TAGS )} of
        {SoloAction, AckAction} when SoloAction orelse AckAction ->
            D1 = report(info, aborted_update, D),
            lager:debug("update aborted", []),
            {ok, Channel} = aec_chain:get_channel(ChannelId),
            NextState =
                case aesc_channels:is_solo_closing(Channel) of
                    false -> open;
                    true -> channel_closing %% solo closing sequence
                end,
            case SoloAction of
                true ->
                    next_state(NextState, clear_ongoing(D1#data{op = ?NO_OP}),
                               [{reply, From, ok}]);
                false ->
                    handle_recoverable_error(NextState,
                                             #{ code => Code
                                              , respond => true
                                              , msg_type => Tag }, D1, [{reply, From, ok}],
                                             _ReportConflictToClient = false)
            end;
        {false, false} ->
            lager:debug("update can not be aborted for ~p", [Tag]),
            keep_state(D, [{reply, From, {error, not_allowed_now}}])
    end;
handle_call_(_NonSigningState, {abort_update, _, _Tag}, From, #data{} = D) ->
    lager:debug("update can not be aborted while ~p with tag ~p",
                [_NonSigningState, _Tag]),
    keep_state(D, [{reply, From, {error, not_allowed_now}}]);
handle_call_(_AnyState, {inband_msg, ToPub, Msg}, From, #data{} = D) ->
    case {ToPub, other_account(D)} of
        {X, X} ->
            keep_state(send_inband_msg(ToPub, Msg, D), [{reply, From, ok}]);
        _ ->
            keep_state(D, [{reply, From, {error, unknown_recipient}}])
    end;
handle_call_(open, {upd_transfer, FromPub, ToPub, Amount, Opts}, From,
            #data{opts = #{initiator := I, responder := R}} = D) ->
    case FromPub =/= ToPub andalso ([] == [FromPub, ToPub] -- [I, R]) of
        true ->
            handle_upd_transfer(FromPub, ToPub, Amount, From, Opts, set_ongoing(D));
        false ->
            keep_state(D, [{reply, From, {error, invalid_pubkeys}}])
    end;
handle_call_(open, {upd_deposit, #{amount := Amt} = Opts}, From,
             #data{} = D) when is_integer(Amt), Amt > 0 ->
    FromPub = my_account(D),
    case maps:find(from, Opts) of
        {ok, FromPub} -> ok;
        {ok, _Other}  -> error(conflicting_accounts);
        error         -> ok
    end,
    {ok, DepTx, Updates, BlockHash} = dep_tx_for_signing(Opts#{acct => FromPub}, D),
    case request_signing(deposit_tx, DepTx, Updates, BlockHash, D, defer) of
        {ok, Send, D1, Actions} ->
            %% reply before sending sig request
            gen_statem:reply(From, ok),
            Send(),
            next_state(awaiting_signature, set_ongoing(D1), Actions);
        {error, _} = Error ->
            gen_statem:reply(From, Error),
            keep_state(D)
    end;
handle_call_(open, {upd_withdraw, #{amount := Amt} = Opts}, From,
             #data{} = D) when is_integer(Amt), Amt > 0 ->
    ToPub = my_account(D),
    case maps:find(to, Opts) of
        {ok, ToPub} -> ok;
        {ok, _Other}  -> error(conflicting_accounts);
        error         -> ok
    end,
    {ok, DepTx, Updates, BlockHash} = wdraw_tx_for_signing(Opts#{acct => ToPub}, D),
    case request_signing(withdraw_tx, DepTx, Updates, BlockHash, D, defer) of
        {ok, Send, D1, Actions} ->
            %% reply before sending sig request
            gen_statem:reply(From, ok),
            Send(),
            next_state(awaiting_signature, set_ongoing(D1), Actions);
        {error, _} = Error ->
            gen_statem:reply(From, Error),
            keep_state(D)
    end;
handle_call_(open, leave, From, #data{} = D) ->
    D1 = send_leave_msg(D),
    gen_statem:reply(From, ok),
    next_state(awaiting_leave_ack, D1);
handle_call_(open, {upd_create_contract, Opts}, From, #data{} = D) ->
    FromPub = my_account(D),
    case maps:find(from, Opts) of
        {ok, FromPub} -> ok;
        {ok, _Other}  -> error(conflicting_accounts);
        error         -> ok
    end,
    new_contract_tx_for_signing(Opts#{owner => FromPub}, From, D);
handle_call_(FSMState, {force_progress, Opts}, From,
             #data{on_chain_id = ChannelId} = D)
    when ChannelId =/= undefined andalso (FSMState =:= open orelse
                                          FSMState =:= channel_closing) ->
    FromPub = my_account(D),
    case maps:find(from, Opts) of
        {ok, FromPub} -> ok;
        {ok, _Other}  -> error(conflicting_accounts);
        error         -> ok
    end,
    try {ok, FPTx, Updates, BlockHash} = force_progress_tx_for_signing(Opts, D),
        case request_signing(force_progress_tx, FPTx, Updates, BlockHash, D, defer) of
            {ok, Send, D1, Actions} ->
                %% reply before sending sig request
                gen_statem:reply(From, ok),
                Send(),
                next_state(awaiting_signature, set_ongoing(D1), Actions);
            {error, _} = Error ->
                gen_statem:reply(From, Error),
                keep_state(D)
        end
    ?CATCH_LOG(E)
        process_update_error(E, From, D)
    end;
handle_call_(open, {upd_call_contract, Opts, ExecType}, From,
             #data{state=State, opts = ChannelOpts,
                   on_chain_id = ChannelId} = D) when ChannelId =/= undefined ->
    FromPub = my_account(D),
    case maps:find(from, Opts) of
        {ok, FromPub} -> ok;
        {ok, _Other}  -> error(conflicting_accounts);
        error         -> ok
    end,
    CallStack = maps:get(call_stack, Opts, []),
    #{contract    := ContractPubKey,
      abi_version := ABIVersion,
      amount      := Amount,
      call_data   := CallData} = Opts,
    Update = aesc_offchain_update:op_call_contract(aeser_id:create(account, FromPub),
                                                   aeser_id:create(contract, ContractPubKey),
                                                   ABIVersion, Amount,
                                                   CallData, CallStack),
    Updates = [Update | meta_updates(Opts)],
    {BlockHash, OnChainEnv, OnChainTrees} = pick_onchain_env(Opts, D),
    ActiveProtocol = aetx_env:consensus_version(OnChainEnv),
    try  Tx1 = aesc_offchain_state:make_update_tx(Updates, State,
                                                  ChannelId,
                                                  ActiveProtocol,
                                                  OnChainTrees, OnChainEnv,
                                                  channel_reserve(ChannelOpts)),
         case ExecType of
            dry_run ->
                UpdState = aesc_offchain_state:set_signed_tx(
                             aetx_sign:new(Tx1, []), %% fake signed tx, no signatures will be checked
                             Updates,
                             State, OnChainTrees, OnChainEnv, ChannelOpts,
                             _CheckSigs = false),
                {Round, _} = aesc_offchain_state:get_latest_signed_tx(UpdState),
                {ok, Call} = aesc_offchain_state:get_contract_call(ContractPubKey,
                                                                   FromPub, Round,
                                                                   UpdState),
                keep_state(D, [{reply, From, {ok, Call}}]);
            execute ->
                case request_signing(?UPDATE, Tx1, Updates, BlockHash, D, defer) of
                    {ok, Send, D1, Actions} ->
                        %% reply before sending sig request
                        gen_statem:reply(From, ok),
                        Send(),
                        next_state(awaiting_signature, set_ongoing(D1), Actions);
                    {error, _} = Error ->
                        gen_statem:reply(From, Error),
                        keep_state(D)
                end
         end
    ?CATCH_LOG(E)
        process_update_error(E, From, D)
    end;
handle_call_(awaiting_signature, Req, From,
             #data{ongoing_update = true, op = Op} = D)
  when ?UPDATE_REQ(element(1, Req)) ->
    %% Race detection!
    lager:debug("race detected: ~p", [Req]),
    #op_sign{tag = OngoingOp} = Op,
    case is_solo_tx_from_op(Op) of
        false ->
            lager:debug("calling handle_update_conflict", []),
            gen_statem:reply(From, ok),
            handle_update_conflict(OngoingOp, D#data{op = ?NO_OP});
        true ->
            postpone(D)
    end;
handle_call_(open, {get_contract_call, Contract, Caller, Round}, From,
             #data{state = State} = D) ->
    Response =
        case aesc_offchain_state:get_contract_call(Contract, Caller, Round, State) of
            {error, call_not_found} -> {error, call_not_found};
            {ok, Call} -> {ok, Call}
        end,
    keep_state(D, [{reply, From, Response}]);
handle_call_(open, {shutdown, XOpts}, From, D) ->
    case close_mutual_tx_for_signing(XOpts, D) of
        {ok, CloseTx, Updates, BlockHash} ->
            case request_signing(?SHUTDOWN, CloseTx, Updates, BlockHash, D, defer) of
                {ok, Send, D1, Actions} ->
                    %% reply before sending sig request
                    gen_statem:reply(From, ok),
                    Send(),
                    next_state(awaiting_signature, set_ongoing(D1), Actions);
                {error, _} = Error ->
                    gen_statem:reply(From, Error),
                    keep_state(D)
            end;
        {error, _} = Error ->
            gen_statem:reply(From, Error),
            keep_state(D)
    end;
handle_call_(channel_closing, {shutdown, XOpts}, From, #data{strict_checks = Strict} = D) ->
    case (not Strict) or was_fork_activated(?LIMA_PROTOCOL_VSN) of
        true ->
            %% Initiate mutual close
            {ok, CloseTx, Updates, BlockHash} = close_mutual_tx_for_signing(XOpts, D),
            case request_signing(?SHUTDOWN, CloseTx, Updates, BlockHash, D, defer) of
                {ok, Send, D1, Actions} ->
                    %% reply before sending sig request
                    gen_statem:reply(From, ok),
                    Send(),
                    next_state(awaiting_signature, set_ongoing(D1), Actions);
                {error, _} = Error ->
                    gen_statem:reply(From, Error),
                    keep_state(D)
            end;
        false ->
            %% Backwards compatibility
            keep_state(D, [{reply, From, {error, unknown_request}}])
    end;
handle_call_(_, get_state, From, Data) ->
    get_state_implementation(From, Data);
handle_call_(_, get_offchain_state, From, #data{state = State} = D) ->
    keep_state(D, [{reply, From, {ok, State}}]);
handle_call_(_, {get_contract, Pubkey}, From, #data{state = State} = D) ->
    Contracts = aec_trees:contracts(aesc_offchain_state:get_latest_trees(State)),
    Response =
        case aect_state_tree:lookup_contract(Pubkey, Contracts) of
            {value, Contract} -> {ok, Contract};
            none -> {error, not_found}
        end,
    keep_state(D, [{reply, From, Response}]);
handle_call_(_, {get_poi, Filter}, From, #data{state = State} = D) ->
    Response =
        case aesc_offchain_state:poi(Filter, State) of
            {ok, PoI} -> {ok, PoI};
            {error, _} -> {error, not_found}
        end,
    keep_state(D, [{reply, From, Response}]);
handle_call_(_St, {get_history, Opts}, From, #data{log = Log} = D) ->
    keep_state(D, [{reply, From, filter_log(Log, Opts)}]);
handle_call_(_St, {change_config, Key, Value}, From, D) ->
    case handle_change_config(Key, Value, D) of
        {ok, D1} ->
            keep_state(D1, [{reply, From, ok}]);
        {error, _} = Error ->
            keep_state(D, [{reply, From, Error}])
    end;
handle_call_(_, {get_balances, Accounts}, From, #data{ state = State } = D) ->
    Result =
        try {ok, lists:foldr(
                   fun(Acct, Acc) ->
                           case aesc_offchain_state:balance(Acct, State) of
                               {ok, Amt} ->
                                   [{Acct, Amt}|Acc];
                               {error, not_found} ->
                                   Acc
                           end
                   end, [], Accounts)}
        ?CATCH_LOG(_E)
            {error, invalid_arguments}
        end,
    lager:debug("get_balances(~p) -> ~p", [Accounts, Result]),
    keep_state(D, [{reply, From, Result}]);
handle_call_(_, get_round, From, #data{ state = State } = D) ->
    Res = try {Round, _} = aesc_offchain_state:get_latest_signed_tx(State),
              {ok, Round}
          ?CATCH_LOG(_E)
              {error, no_state}
          end,
    keep_state(D, [{reply, From, Res}]);
handle_call_(_, prune_local_calls, From, #data{ state = State0 } = D) ->
    State = aesc_offchain_state:prune_calls(State0),
    keep_state(D#data{state = State}, [{reply, From, ok}]);
handle_call_(St, {close_solo, XOpts}, From, D) ->
    case St of
        channel_closing ->
            keep_state(D, [{reply, From, {error, channel_closing}}]);
        _ ->
            {ok, CloseSoloTx, Updates, BlockHash} = close_solo_tx_for_signing(XOpts, D),
            case request_signing(close_solo_tx, CloseSoloTx, Updates, BlockHash, D, defer) of
                {ok, Send, D1, Actions} ->
                    %% reply before sending sig request
                    gen_statem:reply(From, ok),
                    Send(),
                    next_state(awaiting_signature, set_ongoing(D1), Actions);
                {error, _} = Error ->
                    gen_statem:reply(From, Error),
                    keep_state(D)
            end
    end;
handle_call_(St, {snapshot_solo = Req, XOpts}, From, D) ->
    case St of
        channel_closing ->
            keep_state(D, [{reply, From, {error, channel_closing}}]);
        _ ->
            D1 = log(rcv, Req, Req, D),
            case snapshot_solo_tx_for_signing(XOpts, D1) of
                {ok, SnapshotSoloTx, Updates, BlockHash} ->
                    case request_signing(
                           snapshot_solo_tx, SnapshotSoloTx, Updates, BlockHash, D1, defer) of
                        {ok, Send, D2, Actions} ->
                            %% reply before sending sig request
                            gen_statem:reply(From, ok),
                            Send(),
                            next_state(awaiting_signature, set_ongoing(D2), Actions);
                        {error, _} = Error ->
                            keep_state(D1, [{reply, From, Error}])
                    end;
                {error, _} = Error ->
                    keep_state(D1, [{reply, From, Error}])
            end
    end;
handle_call_(St, {slash, XOpts}, From, D) ->
    case St of
        channel_closing ->
            {ok, SlashTx, Updates, BlockHash} = slash_tx_for_signing(XOpts, D),
            gen_statem:reply(From, ok),
            case request_signing(slash_tx, SlashTx, Updates, BlockHash, D, defer) of
                {ok, Send, D1, Actions} ->
                    %% reply before sending sig request
                    gen_statem:reply(From, ok),
                    Send(),
                    next_state(awaiting_signature, set_ongoing(D1), Actions);
                {error, _} = Error ->
                    gen_statem:reply(From, Error),
                    keep_state(D)
            end;
        _ ->
            keep_state(D, [{reply, From, {error, channel_not_closing}}])
    end;
handle_call_(St, {settle, XOpts}, From, D) ->
    case St of
        channel_closing ->
            {ok, SettleTx, Updates, BlockHash} = settle_tx_for_signing(XOpts, D),
            gen_statem:reply(From, ok),
            case request_signing(settle_tx, SettleTx, Updates, BlockHash, D, defer) of
                {ok, Send, D1, Actions} ->
                    %% reply before sending sig request
                    gen_statem:reply(From, ok),
                    Send(),
                    next_state(awaiting_signature, set_ongoing(D1), Actions);
                {error, _} = Error ->
                    gen_statem:reply(From, Error),
                    keep_state(D)
            end;
        _ ->
            keep_state(D, [{reply, From, {error, channel_not_closed}}])
    end;
handle_call_(channel_closing, {attach_responder, #{ reestablish := true
                                                  , gproc_key   := K } = Info},
             From, #data{ role = responder } = D) ->
    lager:debug("Attach request: ~p", [Info]),
    case check_attach_info(Info, D) of
        ok ->
            gproc:unreg(K),
            {Pid, _} = From,
            keep_state(D#data{ session = Pid }, [{reply, From, ok}]);
        {error, Error} ->
            lager:debug("Attach failed with ~p", [Error]),
            keep_state(D, [{reply, From, {error, invalid_attach}}])
    end;
handle_call_(_, {strict_checks, Strict}, From, #data{} = D) when
        is_boolean(Strict) ->
    keep_state(D#data{strict_checks = Strict}, [{reply, From, ok}]);
handle_call_(St, _Req, _From, D) when ?TRANSITION_STATE(St) ->
    postpone(D);
handle_call_(_St, _Req, From, D) ->
    keep_state(D, [{reply, From, {error, unknown_request}}]).

-ifdef(TEST).
get_state_implementation(From, #data{ opts  = Opts
                                    , channel_status = Status
                                    , state = State } = D) ->
    #{initiator := Initiator, responder := Responder} = Opts,
    ChanId = cur_channel_id(D),
    {ok, IAmt} = aesc_offchain_state:balance(Initiator, State),
    {ok, RAmt} = aesc_offchain_state:balance(Responder, State),
    StateHash = aesc_offchain_state:hash(State),
    {Round, SignedTx} = try aesc_offchain_state:get_latest_signed_tx(State)
                        catch
                            error:no_tx ->
                                {0, no_tx}
                        end,
    SH = case SignedTx of
           no_tx -> no_state_hash_yet;
           _ ->
              {Mod, Tx} = aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
              Mod:state_hash(Tx)
        end,
    case SH of
        no_state_hash_yet -> pass;
        StateHash -> %% same state hash as the one in state
            pass;
        _ -> error({inconsistency, state_hash})
    end,
    Result =
        #{ channel_id     => ChanId
         , initiator      => Initiator
         , responder      => Responder
         , init_amt       => IAmt
         , resp_amt       => RAmt
         , state_hash     => StateHash
         , round          => Round
         , channel_status => Status},
    keep_state(D, [{reply, From, {ok, Result}}]).
-else.
get_state_implementation(From, D) ->
    keep_state(D, [{reply, From, {error, unknown_request}}]).
-endif.

handle_info({'DOWN', MRef, process, Client, _} = M,
            #data{ client = Client
                 , client_mref = MRef } = D) when D#data.client_may_disconnect ->
    lager:debug("Client disconnected ~p", [Client]),
    keep_state(log(disconnect, error, M,
                   D#data{ client_mref      = undefined
                         , client           = undefined
                         , client_connected = false }));
handle_info({'DOWN', MRef, process, Client, _} = M,
            #data{ client = Client
                 , client_mref = MRef } = D) ->
    close(client_disconnected, log(disconnect, error, M, D));
handle_info(Msg, #data{cur_statem_state = St} = D) ->
    lager:debug("Discarding info in ~p state: ~p", [St, Msg]),
    keep_state(log(drop, msg_type(Msg), Msg, D)).

%% @doc A few different modes are specified here:
%% * error_all - all calls and casts not explicitly handled lead to protocol
%%               error. Used mainly for the open/reestablish handshake.
%% * error     - try to handle calls, but unknown casts cause protocol error.
%%               Used during deposit/withdraw, since calls can be used to
%%               inject a competing operation (thereby rejecting the other).
%% * postpone  - postpone casts (and info), but try to handle calls.
%% * discard   - handle calls, but drop unknown casts (could be e.g. a stray
%%               signing reply in the open state).
handle_common_event(E, Msg, M, #data{cur_statem_state = St} = D) ->
    lager:debug("handle_common_event(~p, ~p, ~p, ~p, D)", [E, Msg, St, M]),
    handle_common_event_(E, Msg, St, M, D).

handle_common_event_(timeout, Info, _St, _M, D) when D#data.ongoing_update == true ->
    lager:debug("timeout (~p) - recovering (~p)", [Info, D#data.error_msg_type]),
    handle_recoverable_error(#{code => ?ERR_TIMEOUT}, D);
handle_common_event_(timeout, St = T, St, _, #data{ role = Role
                                                  , opts = Opts } = D) ->
    KeepRunning = maps:get(keep_running, Opts, false),
    case KeepRunning andalso Role =:= responder of
        true ->
            keep_state(D);
        false ->
            close({timeout, T}, D)
    end;
handle_common_event_(cast, ?DISCONNECT = Msg, _St, _, #data{ channel_status = Status
                                                           , client_may_disconnect = MayDisconnect
                                                           , role = Role
                                                           , opts = Opts } = D) ->
    D1 = log(rcv, ?DISCONNECT, Msg, D),
    case Status of
        closing ->
            keep_state(D1);
        _ ->
            case {Role, maps:get(keep_running, Opts, false), MayDisconnect} of
                {responder, true, true} ->
                    D2 = report(info, peer_disconnected, D1),
                    next_state(
                      open,
                      prepare_for_reestablish(
                        fallback_to_stable_state(D2#data{ session = undefined
                                                        , peer_connected = false })));
                _ ->
                    close(disconnect, D1)
            end
    end;
handle_common_event_(cast, {?INBAND_MSG, Msg}, _St, _, D) ->
    NewD = case check_inband_msg(Msg, D) of
               {ok, D1} ->
                   D2 = report(message, Msg, D1),
                   D2;
               {error, _} = Err ->
                   lager:warning("Error in inband_msg: ~p", [Err]),
                   D
           end,
    keep_state(NewD);
handle_common_event_(cast, {?CHANNEL_CHANGED, #{ tx_hash := TxHash
                                               , tx := SignedTx } = Info} = Msg,
                     St, _, D) ->
    lager:debug("Got ?CHANNEL_CHANGED; tx_hash := ~p", [TxHash]),
    D1 = log(rcv, msg_type(Msg), Msg, D#data{last_channel_change = TxHash}),
    {Type, _Tx} = aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
    case Type =:= channel_force_progress_tx of
        true ->
            process_incoming_forced_progress(St, Info, D1);
        false ->
            case D1#data.op of
                #op_min_depth{tx_hash = LatestTxHash} when TxHash =/= LatestTxHash ->
                    %% This is a problem: channel changed while waiting to confirm
                    %% other tx hash
                    close({error, unexpected_tx_on_chain}, D);
                _ ->
                    lager:debug("Fsm notes channel change ~p", [Info]),
                    D2 = report_on_chain_tx(channel_changed, Type, SignedTx, D1),
                    keep_state(maybe_act_on_tx(Type, SignedTx, D2))
            end
    end;
handle_common_event_(cast, {?CHANNEL_CLOSING, #{tx_hash := TxHash} = Info} = Msg, St, _, D) ->
    lager:debug("got ~p", [Msg]),
    D1 = log(rcv, ?CHANNEL_CLOSING, Msg, D#data{last_channel_change = TxHash}),
    case check_closing_event(St, Info, D1) of
        NextState when is_tuple(NextState) andalso
                       (element(1, NextState) =:= next_state orelse
                        element(1, NextState) =:= keep_state) ->
            NextState;
        {can_slash, _Round, SignedTx} ->
            D2 = report_on_chain_tx(can_slash, SignedTx, D1),
            next_state(channel_closing, D2);
        {ok, proper_solo_closing, SignedTx} ->
            lager:debug("Channel solo-closing", []),
            D2 = report_on_chain_tx(solo_closing, SignedTx, D1),
            next_state(channel_closing, D2);
        {error, not_solo_closing} ->
            %% the min depth watcher reported a channel object that is not
            %% closing, this could be due to a (micro) fork that rejected the
            %% channel_close_solo_tx that had been initally detected
            lager:debug("Received a channel closing event for not closing channel", []),
            keep_state(D1)
    end;
handle_common_event_(cast, {?MIN_DEPTH_ACHIEVED, _ChainId,
                            ?WATCH_SNAPSHOT_SOLO, TxHash}, _St, _, D) ->
    %% This min_depth notification is handled unconventionally, since only the
    %% requesting client is to get a min_depth notification, and the fsm is not
    %% locked while waiting for it.
    lager:debug("Received min_depth confirmation of snapshot_solo_tx ~p", [TxHash]),
    D1 = report(info, #{ event => min_depth_achieved
                       , type => aesc_snapshot_solo_tx:type()
                       , tx_hash => TxHash }, D),
    keep_state(D1);
handle_common_event_(cast, {?CHANNEL_CLOSED = OpTag, #{ tx_hash := TxHash
                                                      , tx := SignedTx} = _Info} = Msg, _St, _, D) ->
    %% Start a minimum-depth watch, then (if confirmed) terminate
    D1 = D#data{last_channel_change = TxHash},
    D2 = safe_watched_action_report(SignedTx, Msg, D1, [], undefined, ?WATCH_CLOSED, OpTag, msg_type(Msg)),
    next_state(channel_closed, D2);
handle_common_event_({call, From}, Req, St, Mode, D) ->
    case Mode of
        error_all ->
            keep_state(D, [{reply, From, {error, not_ready}}]);
        _ ->
            handle_call(St, Req, From, D)
    end;
handle_common_event_(info, Msg, _St, _P, D) ->
    handle_info(Msg, D);
handle_common_event_(_Type, _Msg, _St, P, D) when P == postpone ->
    postpone(D);
handle_common_event_(Type, Msg, St, discard, D) ->
    lager:warning("Discarding ~p in '~p' state: ~p", [Type, St, Msg]),
    keep_state(log(drop, msg_type(Msg), Msg, D));
handle_common_event_(Type, Msg, St, Err, D) when Err==error;
                                                 Err==error_all ->
    lager:debug("Wrong ~p in ~p: ~p", [Type, St, Msg]),
    %% should send an error msg
    close(protocol_error, D).

get_channel(ChainHash, ChId) ->
    case aec_chain:genesis_hash() of
        ChainHash ->
            aec_chain:get_channel(ChId);
        _ ->
            {error, chain_hash_mismatch}
    end.

%% @doc Enable a new fork only after FORK_MINIMUM_DEPTH generations in order to avoid fork changes.
was_fork_activated(ProtocolVSN) ->
    Height = max(curr_height() - ?FORK_MINIMUM_DEPTH, aec_block_genesis:height()),
    ProtocolVSN =< protocol_at_height(Height).

block_hash_from_op(?NO_OP) -> %% in case of unexpected close
    ?NOT_SET_BLOCK_HASH;
block_hash_from_op(Op) ->
    #op_data{block_hash = BlockHash} = op_data_from_op(Op),
    BlockHash.

is_solo_tx_from_op(?NO_OP) ->
    false;
is_solo_tx_from_op(Op) ->
    #op_data{signed_tx = SignedTx} = op_data_from_op(Op),
    {Mod, _Tx} = aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    lists:member(Mod, ?SOLO_TRANSACTIONS).



op_data_from_op(#op_sign{data = OpData}) -> OpData;
op_data_from_op(#op_ack{data = OpData}) -> OpData;
op_data_from_op(#op_lock{data = OpData}) -> OpData;
op_data_from_op(#op_min_depth{data = OpData}) -> OpData;
op_data_from_op(#op_watch{data = OpData}) -> OpData;
op_data_from_op(#op_close{data = OpData}) -> OpData.

tx_env_and_trees_from_top(Type) ->
    aesc_tx_env_cache:tx_env_and_trees_from_top(Type).

-ifdef(TEST).
%% This function is meant to prune and prettify stack traces
%% containing #data{} records, which often are so large that printouts become
%% unreadable.
pr_stacktrace(T) ->
    [{M,F,abbrev_args(A), St} || {M,F,A,St} <- T].

abbrev_args(I) when is_integer(I) ->
    I;
abbrev_args(L) when is_list(L) ->
    lists:map(
      fun(#data{} = D) ->
              pr_data(D);
         (X) -> X
      end, L).
-endif.

%% This is also used in some debug log entries
pr_data(D) ->
    lager:pr(setelement(
               #data.state,
               setelement(#data.log, D, {snip}), {snip}), ?MODULE).

-spec check_block_hash(aec_keys:pubkey(), #data{}) -> ok.
check_block_hash(?NOT_SET_BLOCK_HASH, _) -> ok;
check_block_hash(BlockHash,
                 #data{block_hash_delta = #bh_delta{ not_older_than = LowerDelta
                                                   , not_newer_than = UpperDelta}}) ->
    case aec_chain:get_header(BlockHash) of
        {ok, Header} ->
            Checks =
                [ fun() ->
                      CurrHeight = curr_height(),
                      BlockHeight = aec_headers:height(Header),
                      UpperLimit = CurrHeight - UpperDelta,
                      LowerLimit = CurrHeight - LowerDelta,
                      case BlockHeight of
                          _ when BlockHeight > UpperLimit ->
                              {error, block_hash_too_new};
                          _ when BlockHeight < LowerLimit ->
                              {error, block_hash_too_old};
                          _ -> ok
                      end
                  end,
                  fun() ->
                      case aec_chain:hash_is_in_main_chain(BlockHash) of
                          true -> ok;
                          false -> {error, block_hash_in_fork}
                      end
                  end
                ],
            aeu_validation:run(Checks);
        error ->
            {error, unknown_block_hash}
    end.

%% @doc Use the given minimum depth parameters from the message in case none
%% have been set previously in the channel options. This should only apply to
%% the initiator. In case only one of the two parameters is defined, the other
%% one will be set to the internal default value.
maybe_use_minimum_depth_params(Msg, Opts) ->
    MinDepth = maps:find(minimum_depth, Msg),
    MinDepthStrategy = maps:find(minimum_depth_strategy, Msg),
    OptMinDepth = maps:find(minimum_depth, Opts),
    OptMinDepthStrategy = maps:find(minimum_depth_strategy, Opts),

    MinDepthValid = is_valid_minimum_depth(MinDepth),
    MinDepthStrategyValid = is_valid_minimum_depth_strategy(MinDepthStrategy),
    OptMinDepthValid = is_valid_minimum_depth(OptMinDepth),
    OptMinDepthStrategyValid = is_valid_minimum_depth_strategy(OptMinDepthStrategy),

    if
        OptMinDepthValid andalso OptMinDepthStrategyValid ->
            Opts;
        OptMinDepthValid ->
            Opts#{minimum_depth_strategy => ?DEFAULT_MINIMUM_DEPTH_STRATEGY};
        OptMinDepthStrategyValid ->
            Opts#{minimum_depth => default_minimum_depth(maps:get(minimum_depth_strategy, Opts))};
        MinDepthValid andalso MinDepthStrategyValid ->
            {ok, MinDepth1} = MinDepth,
            {ok, MinDepthStrategy1} = MinDepthStrategy,
            Opts#{ minimum_depth => MinDepth1
                 , minimum_depth_strategy => MinDepthStrategy1 };
        true ->
            Opts#{ minimum_depth_strategy => ?DEFAULT_MINIMUM_DEPTH_STRATEGY
                 , minimum_depth => default_minimum_depth() }
    end.

is_valid_minimum_depth_strategy({ok, txfee}) -> true;
is_valid_minimum_depth_strategy(_)           -> false.

is_valid_minimum_depth({ok, Value}) when is_integer(Value) -> Value >= 0;
is_valid_minimum_depth(_)                                  -> false.

default_minimum_depth() ->
    default_minimum_depth(?DEFAULT_MINIMUM_DEPTH_STRATEGY).

default_minimum_depth(Strategy) ->
    case Strategy of
        txfee ->
            10;
        _ ->
            error(unknown_minimum_depth_strategy)
    end.

min_tx_fee(Tx, Opts) ->
    {CurrHeight, CurrProtocol} = curr_height_and_protocol(),
    DefaultMinGasPrice = max(aec_tx_pool:minimum_miner_gas_price(),
                             aec_governance:minimum_gas_price(CurrProtocol)),
    GasPrice = maps:get(gas_price, Opts, DefaultMinGasPrice),
    FeeGas = aetx:fee_gas(Tx, CurrHeight, CurrProtocol),
    FeeGas * GasPrice.

postpone_or_error(call) -> error_all;
postpone_or_error(_) -> postpone.

%% @doc Starts the watcher before sending the transaction to the other
%% participant, logs the message and reports the transaction.
%% This ensures the correct order of execution to prevent a race condition.
safe_watched_action_report(SignedTx, Msg, Data, Updates, ActionFun, WatchTag,
                           ReportTag) ->
    safe_watched_action_report(SignedTx, Msg, Data, Updates, ActionFun,
                               WatchTag, ReportTag, undefined).

safe_watched_action_report(SignedTx, Msg, D, Updates, ActionFun, WatchTag,
                           ReportTag, LogType) ->
    {ok, D1} = watcher_request({?MIN_DEPTH, WatchTag}, SignedTx, Updates, D),
    D2 = case LogType of
             undefined ->
                 D1;
             _ ->
                 log(rcv, LogType, Msg, D1)
         end,
    D3 = case ActionFun of
             undefined ->
                 D2;
             _ ->
                 ActionFun(SignedTx, D2)
         end,
    report_on_chain_tx(ReportTag, SignedTx, D3).

push_to_mempool(SignedTx) ->
    case aec_tx_pool:push(SignedTx) of
        ok ->
            ok;
        {error, Reason} ->
            {Type, _Tx} = aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
            lager:debug("~p tx failed: ~p", [Type, Reason]),
            {error, Reason}
    end.

try_to_push_to_mempool(SignedTx, D, NextState) ->
    try_to_push_to_mempool(SignedTx, D, NextState, fun(D0) -> D0 end).

try_to_push_to_mempool(SignedTx, D, NextState, UpdateStateFun) ->
    try_to_push_to_mempool(SignedTx, D, NextState, UpdateStateFun, open).

try_to_push_to_mempool(SignedTx, D, NextState, UpdateStateFun,
                       NextStateIfFailed) ->
    Pubkeys =
        case is_mutual_tx(SignedTx) of
            true -> both_accounts(D);
            false -> [my_account(D)]
        end,
    Result =
        case verify_signatures(SignedTx, D, Pubkeys) of
            ok -> push_to_mempool(SignedTx);
            {error, _} = Err -> Err
        end,
    case Result of
        ok ->
            next_state(NextState, UpdateStateFun(D));
        {error, _Err} ->
            #data{op = Op} = D,
            {TxType, _Tx} = aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
            Tag =
                case Op of
                    #op_ack{tag = T} -> T;
                    #op_sign{tag = T} -> T;
                    ?NO_OP when TxType =:= channel_snapshot_solo_tx ->
                        snapshot_solo_tx;
                    ?NO_OP when TxType =:= channel_force_progress_tx ->
                        force_progress_tx;
                    ?NO_OP when TxType =:= channel_settle_tx ->
                        settle_tx
                end,
            Msg = onchain_error_msg_by_next_state(SignedTx, NextState),
            D1 = log(rcv, msg_type(Msg), Msg, D),
            handle_recoverable_error(NextStateIfFailed,
                                     #{code => ?ERR_ONCHAIN_REJECTED,
                                       respond => is_mutual_tx(SignedTx),
                                       msg_type => Tag},
                                     D1,
                                     [],
                                     true)
    end.

onchain_error_msg_by_next_state(SignedTx, NextState) ->
    {Type, _Tx} = aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
    case {Type, NextState} of
        {channel_create_tx, awaiting_locked} -> ?ERROR; %% TODO
        {channel_deposit_tx, awaiting_locked} -> ?DEP_ERR;
        {channel_withdraw_tx, awaiting_locked} -> ?WDRAW_ERR;
        {channel_close_solo_tx, open} -> close_solo_error;
        {channel_slash_tx, channel_closing} -> ?ERROR; %% TODO
        {channel_settle_tx, channel_closing} -> ?ERROR; %% TODO
        {channel_close_mutual_tx, mutual_closed} -> ?SHUTDOWN_ERR;
        {channel_snapshot_solo_tx, open} -> snapshot_error;
        {channel_force_progress_tx, channel_closing} -> force_progress_error;
        {channel_force_progress_tx, open} -> force_progress_error;
        _ -> undefined
    end.

is_mutual_tx(SignedTx) ->
    {Mod, _Tx} = aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    not lists:member(Mod, ?SOLO_TRANSACTIONS).

mutual_close_error(Msg, D) ->
    case check_shutdown_err_msg(Msg, D) of
        {ok, _ErrorCode, D1} ->
            D2 = report(conflict, Msg, D1),
            next_state(open, D2);
        {error, tx_not_found} -> %% tx had not been posted on-chain
            #data{ op = Op } = D,
            {SignedTx, BlockHash} =
                case Op of
                    #op_close{data = #op_data{ signed_tx = STx
                                             , block_hash = BH}} ->
                        {STx, BH};
                    #op_ack{ data = #op_data{ signed_tx = STx
                                            , block_hash = BH}
                           , tag = shutdown} ->
                        {STx, BH}
                end,
            case aeu_validation:run(shutdown_tx_checks(SignedTx, BlockHash,
                                                       both, D, true)) of
                ok ->
                    case push_to_mempool(SignedTx) of
                        ok -> %% tx is ok, wait for it on-chain
                            keep_state(log(rcv, ?SHUTDOWN_ERR, Msg, D));
                        {error, _Err} ->
                            D1 = report(conflict, Msg, D),
                            next_state(open, D1)
                    end;
                {error, _Err} ->
                    D1 = report(conflict, Msg, D),
                    next_state(open, D1)
            end;
        {error, Error} ->
            D1 = report_with_notice(conflict, Msg, Error, D),
            keep_state(log(rcv, ?SHUTDOWN_ERR, Msg, D1))
    end.

channel_reserve(#{channel_reserve := Reserve}) ->
    Reserve.

onchain_persisted_channel_reserve(ChannelPubkey) ->
    {ok, Channel} = aec_chain:get_channel(ChannelPubkey),
    aesc_channels:channel_reserve(Channel).

mk_force_progress_tx(Opts, #data{state = State, opts = ChannelOpts,
                                 on_chain_id = ChannelPubkey}) ->
    CallStack = maps:get(call_stack, Opts, []),
    #{ contract    := ContractPubKey
     , abi_version := ABIVersion
     , amount      := Amount
     , call_data   := CallData
     , acct        := FromPub
     , gas         := Gas
     , gas_price   := GasPrice } = Opts,
    Update = aesc_offchain_update:op_call_contract(aeser_id:create(account, FromPub),
                                                   aeser_id:create(contract, ContractPubKey),
                                                   ABIVersion, Amount,
                                                   CallData, CallStack,
                                                   GasPrice, Gas),
    Updates = [Update],
    {OnChainEnv, OnChainTrees} = aetx_env:tx_env_and_trees_from_top(aetx_contract),
    ActiveProtocol = aetx_env:consensus_version(OnChainEnv),
    OffchainTx = aesc_offchain_state:make_update_tx(Updates, State,
                                                    ChannelPubkey,
                                                    ActiveProtocol,
                                                    OnChainTrees, OnChainEnv,
                                                    channel_reserve(ChannelOpts)),
    {OffChainRound, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    {ok, Channel} = aec_chain:get_channel(ChannelPubkey),
    OnChainRound = aesc_channels:round(Channel),
    Payload =
        case OnChainRound =:= OffChainRound of
            false -> %% base it on an off-chain tx
                aetx_sign:serialize_to_binary(SignedTx);
            true -> % latest on-chain tx
                <<>>
        end,
    {aesc_offchain_tx, OffTx} = aetx:specialize_callback(OffchainTx),
    FPOpts =
        maps:merge(#{ channel_id => aeser_id:create(channel, ChannelPubkey)
                    , from_id    => aeser_id:create(account, FromPub)
                    , payload    => Payload
                    , update     => Update
                    , state_hash => aesc_offchain_tx:state_hash(OffTx)
                    , round      => aesc_offchain_tx:round(OffTx)
                    , offchain_trees =>
                        aesc_offchain_state:get_latest_trees(State)
                    , fee        => 0 %% if there is a fee in Opts, it would replace this dummy value
                    , gas_price  => GasPrice
                    , gas        => Gas },
                    maps:with([nonce, fee], Opts)),
    aesc_force_progress_tx:new(FPOpts).

call_callback_new(aesc_force_progress_tx, Opts, D) ->
    mk_force_progress_tx(Opts, D);
call_callback_new(Mod, Opts, _) ->
    apply(Mod, new, [Opts]).

process_incoming_forced_progress(FSMState, #{tx := SignedTx} = Info,
                                 #data{on_chain_id = ChannelPubkey } = D) ->
    case does_force_progress_interrupt(FSMState) of
        false ->
            %% we wait to finish the current set of events before moving to
            %% the FP handling
            postpone(D);
        true ->
            {ok, Channel} = aec_chain:get_channel(ChannelPubkey),
            {ConsumedFP, CanDispute, D1} =
                process_incoming_forced_progress_(Info, Channel, D),
            case CanDispute of
                true ->
                      Report =
                          case aesc_channels:is_active(Channel) of
                              true -> can_snapshot;
                              false -> can_slash
                          end,
                      report_on_chain_tx(Report, SignedTx, D1);
                false -> pass
            end,
            case ConsumedFP of
                true ->
                    NextState =
                        case aesc_channels:is_solo_closing(Channel) of
                            false -> open;
                            true -> channel_closing %% solo closing sequence
                        end,
                    next_state(NextState, D1);
                false -> keep_state(D1)
            end
    end.

process_incoming_forced_progress_(#{ tx := SignedTx
                                   , block_hash := BlockHash },
                                  Channel,
                                  #data{ state = State
                                       , opts = Opts
                                       , on_chain_id = _ChannelPubkey } = D) ->
    %% we don't check the authentication of the incoming transaction as at
    %% this point it had already been included in a microblock and this block
    %% had been accepted as valid by our own node. Relying on those checks, we
    %% can skip validating authentication once more
    {aesc_force_progress_tx = Mod, Tx} =
        aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    try {LatestFSMRound, LatestTx} = aesc_offchain_state:get_latest_signed_tx(State),
         FPRound = get_round(Mod, Tx),
         FirstSoloRound = aesc_channels:solo_round(Channel),
         CorrectFPPayload =
             case FirstSoloRound =:= LatestFSMRound + 1 of
                 true ->
                     %% FP had been based on the lastest state known to the
                     %% FSM. This is the expected scenario
                     true;
                 false ->
                     {Type, _Tx} =
                         aetx:specialize_type(aetx_sign:innermost_tx(LatestTx)),
                     case Type of
                         channel_force_progress_tx ->
                             %% the latest transaction known to the FSM
                             %% is a FP one. Assumption is that it is
                             %% on-chain and the new incoming FP had
                             %% been based on it
                             true;
                         _ ->
                             %% the FP had been based on a state that
                             %% had been older than the one the FSM
                             %% has. This is slashable
                             false
                     end
             end,
         case CorrectFPPayload of
             %% the FP was based on a channel round that is the same as
             %% the last one the FSM has as a co-authenticated state
             %% it could be the same one or a different transaction
             %% with the same round. The latter would be a mistake on
             %% the client's side - they've signed two different
             %% transactions with the same round. There is not much we
             %% can do except accepting the FP as a valid one
             true ->
                 case FPRound - LatestFSMRound of
                     1 = _OkDiff ->
                         lager:debug("Accomodate incoming forced progress", []),
                         {OnChainEnv, OnChainTrees} = load_pinned_env(BlockHash),
                         Update = aesc_force_progress_tx:update(Tx),
                         State1 =
                             aesc_offchain_state:set_signed_tx(SignedTx,
                                                               [Update],
                                                               State,
                                                               OnChainTrees,
                                                               OnChainEnv,
                                                               Opts),
                         D1 = D#data{ state = State1
                                    , op    = ?NO_OP
                                    , last_reported_update = FPRound},
                         D2 = report_on_chain_tx(consumed_forced_progress, SignedTx, D1),
                         {true, false, D2};
                     WrongFPDiff when WrongFPDiff =< 0 ->
                         %% we shouldn't get here as it would mean that
                         %% the payload had been older than the channel state we
                         %% have in the FSM and this should have been
                         %% caught in the outer case
                         %% This clause is left for completeness
                         lager:debug("Detected a malicious forced progress", []),
                         {false, true, D};
                     MissingStateDiff when MissingStateDiff > 1 ->
                         %% channel_force_progress_tx is based on what
                         %% the FSM percieves a future state that is
                         %% authenticated by our client. We accept
                         %% it...
                         lager:info("FP is based on state that is not present", []),
                         D1 = maybe_act_on_tx(channel_force_progress_tx, SignedTx, D),
                         {false, false, D1}
                 end;
             false ->
                 lager:info("Detected cheating attempt with a force progress"
                            " based on round ~p while latest FSM round is ~p",
                            [FPRound - 1, LatestFSMRound]),
                 {false, true, D}
         end
             ?CATCH_LOG(_E)
             {false, false, D}
    end.

does_force_progress_interrupt(FSMState) ->
    NonInterruptableStates =
        [ initialized
        , accepted
        , awaiting_locked
        , awaiting_open
        , awaiting_reestablish
        , channel_closed
        , half_signed
        , mutual_closed
        , reestablish_init
        ],
    InteruptableStates =
        [ awaiting_leave_ack
        , awaiting_signature
        , awaiting_update_ack
        , channel_closing
        , dep_half_signed
        , dep_signed
        , mutual_closing
        , open
        , signed
        , wdraw_half_signed
        , wdraw_signed
        ],
    case {lists:member(FSMState, NonInterruptableStates),
          lists:member(FSMState, InteruptableStates)} of
        {true, false} -> false;
        {false, true} -> true;
        _ ->
            lager:warning("Unhandled state while force progressing ~p",
                          [FSMState]),
            false
    end.



check_chain_before_reestablish(#{state := State0} = _StateRecovery,
                               #data{ role = Role
                                    , on_chain_id = ChannelId
                                    , last_channel_change = TxHash } = Data) ->
    {ok, TxHashes} = aesc_chain_watcher:get_txs_since_tx(ChannelId, TxHash),
    Data1 = Data#data{ state = State0
                     , op    = ?NO_OP},
    {Res, UpdatedData} = apply_non_malicious_txs(TxHashes, Data1),
    case Res of
        {malicious, MaliciousTx} ->
            {ok, Channel} = aec_chain:get_channel(ChannelId),
            Report =
                case aesc_channels:is_active(Channel) of
                    true -> can_snapshot;
                    false -> can_slash
                end,
            report_on_chain_tx(Report, MaliciousTx, UpdatedData);
        ok -> pass
    end,
    NextState =
        case Role of
            initiator -> awaiting_connect;
            responder -> awaiting_reestablish
        end,
    {ok, NextState, UpdatedData}.

-spec apply_non_malicious_txs(list(binary()), #data{}) ->
    {ok, #data{}} | {{malicious, aetx_sign:signed_tx()}, #data{}}.
apply_non_malicious_txs(TxHashes, Data) ->
    TxsAndLocations = [aec_chain:find_tx_with_location(TH) || TH <- TxHashes],
    apply_non_malicious_txs_(TxsAndLocations, Data).

-spec apply_non_malicious_txs_(list(), #data{}) ->
    {ok, #data{}} | {{malicious, aetx_sign:signed_tx()}, #data{}}.
apply_non_malicious_txs_([], #data{} = Data) -> %% applied all
    {ok, Data};
apply_non_malicious_txs_([{BlockHash, SignedTx} | Rest],
                         #data{state = State0, opts = Opts} = Data) when
is_binary(BlockHash) -> 
    Aetx = aetx_sign:innermost_tx(SignedTx),
    {Mod, Tx} = aetx:specialize_callback(Aetx),
    State =
        case is_function(State0) of
            true ->
                {ok, S} = State0(maps:get(initiator, Data#data.opts)),
                S;
            false -> State0
        end,
    case is_onchain_tx_malicious(Mod, Tx, State, BlockHash) of
        true ->
            lager:debug("Detected malicious transaction ~p", [SignedTx]),
            {{malicious, SignedTx}, Data}; %% abort
        false ->
            State1 =
                case Mod of
                    aesc_close_solo_tx ->
                        lager:debug("Detected close solo transaction", []),
                        State;
                    aesc_force_progress_tx ->
                        lager:debug("Detected force progress transaction", []),
                        {OnChainEnv, OnChainTrees} = load_pinned_env(BlockHash),
                        Update = aesc_force_progress_tx:update(Tx),
                        aesc_offchain_state:set_signed_tx(SignedTx,
                                                          [Update],
                                                          State,
                                                          OnChainTrees,
                                                          OnChainEnv,
                                                          Opts);
                    _ -> State
                end,
            Data1 = Data#data{ state = State1
                             , op    = ?NO_OP},
                             %% do not set `last_reported_update`
                             %% so it can be sent in an
                             %% update message to the client
            report_on_chain_tx(channel_changed, SignedTx, Data1),
            apply_non_malicious_txs_(Rest, Data1)
    end.

-spec is_onchain_tx_malicious(atom(), aetx:tx_instance(),
                              aesc_offchain_state:state(), binary()) ->
    boolean().
is_onchain_tx_malicious(Mod, Tx, State, BlockHash) when is_binary(BlockHash) ->
    {LastValidRound, _} = aesc_offchain_state:get_latest_signed_tx(State),
    TxRound =
        case Mod of
            aesc_snapshot_solo_tx ->
                PayloadBin = aesc_snapshot_solo_tx:payload(Tx), 
                {ok, _SignedOffchainTx, OffchainTx} =
                    aesc_utils:deserialize_payload(PayloadBin),
                aesc_offchain_tx:round(OffchainTx);
            aesc_close_solo_tx ->
                ChannelPubkey = aesc_close_solo_tx:channel_pubkey(Tx),
                {ok, Channel} = aec_chain:get_channel_at_hash(ChannelPubkey,
                                                              BlockHash),
                aesc_channels:round(Channel);
            _ ->
                Mod:round(Tx)
        end,
    case {Mod, TxRound} of
        {aesc_close_solo_tx, LastValidRound} -> false;
        {aesc_close_solo_tx, UnexpectedRound}
            when UnexpectedRound < LastValidRound -> true;
        {aesc_force_progress_tx, FPRound} when FPRound =:= LastValidRound + 1 ->
            false;
        {aesc_force_progress_tx, UnexpectedRound}
            when UnexpectedRound < LastValidRound + 1 -> true;
        _ -> false
    end.

