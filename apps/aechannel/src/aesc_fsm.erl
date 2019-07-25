%% @doc This modules implements the State Channel FSM and its external API. The
%% FSM is used for both state channel roles, 'responder' and 'initiator',
%% because most logic is shared with the initialization being specific to the
%% role.
%% @reference See the design of
%% <a href="https://github.com/aeternity/protocol/tree/master/channels">State Channels</a>
%% for further information.
%% @reference See the
%% <a href="https://github.com/aeternity/aeternity/wiki/State-Channels#fsm">state
%% diagrams</a> for a conceptual representation of this implementation.
-module(aesc_fsm).

-behaviour(gen_statem).

%% API
-export([ start_link/1
        , attach_responder/2      %% (fsm(), map())
        , client_died/1           %% (fsm())
        , close_solo/1
        , connection_died/1       %% (fsm())
        , get_contract/2
        , get_contract_call/4     %% (fsm(), contract_id(), caller(), round())
        , get_offchain_state/1
        , get_poi/2
        , get_state/1
        , inband_msg/3
        , initiate/3              %% (host(), port(), Opts :: #{}
        , leave/1
        , respond/2               %% (port(), Opts :: #{})
        , settle/1
        , shutdown/1              %% (fsm())
        , slash/1
        , upd_call_contract/2     %%
        , upd_create_contract/2   %%
        , upd_deposit/2           %% (fsm() , map())
        , upd_transfer/4          %% (fsm() , from(), to(), amount())
        , upd_withdraw/2          %% (fsm() , map())
        , where/2
        ]).

%% Inspection and configuration functions
-export([ change_config/3   %% (fsm(), key(), value()) -> ok | {error,_}
        , dry_run_contract/2 %% (fsm(), map()) -> {ok, call()} | {error, _}
        , get_balances/2    %% (fsm(), {ok, [key()]) -> [{key(), amount()}]} | {error, _}
        , get_history/1     %% (fsm()) -> [Event]
        , get_round/1       %% (fsm()) -> {ok, round()} | {error, _}
        , patterns/0
        , prune_local_calls/1  %% (fsm()) -> {ok, round()} | {error, _}
        , record_fields/1
        , report_tags/0
        , timeouts/0
        ]).

%% Used by noise session
-export([message/2]).

%% Used by client
-export([signing_response/3]).    %% (Fsm, Tag, Obj)

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
        , awaiting_initial_state/3
        , awaiting_leave_ack/3
        , awaiting_locked/3
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
        , open/3
        , reestablish_init/3
        , signed/3
        , wdraw_half_signed/3
        , wdraw_signed/3
        ]).

-include_lib("aecontract/include/hard_forks.hrl").
-include("aesc_codec.hrl").
-include("aesc_fsm.hrl").

%% mainly to avoid warnings during build
-export_type([ opts/0
             , state_name/0
             , data/0
             ]).

%% -include_lib("trace_runner/include/trace_runner.hrl").

-ifdef(TEST).
-export([strict_checks/2]).
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

client_died(Fsm) ->
    %TODO: possibility for reconnect
    lager:debug("client died(~p)", [Fsm]),
    stop_ok(catch gen_statem:stop(Fsm)).

close_solo(Fsm) ->
    lager:debug("close_solo(~p)", [Fsm]),
    gen_statem:call(Fsm, close_solo).

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

-spec get_state(pid()) -> {ok, #{}}.
get_state(Fsm) ->
    gen_statem:call(Fsm, get_state).

inband_msg(Fsm, To, Msg) ->
    MaxSz = aesc_codec:max_inband_msg_size(),
    try iolist_to_binary(Msg) of
        MsgBin when byte_size(MsgBin) =< MaxSz ->
            lager:debug("inband_msg(~p, ~p, ~p)", [Fsm, To, Msg]),
            gen_statem:call(Fsm, {inband_msg, To, MsgBin});
        MsgBin ->
            lager:debug("INVALID inband_msg(~p): ~p", [Fsm, byte_size(MsgBin)]),
            {error, invalid_request}
    catch
        error:_ ->
            {error, invalid_request}
    end.

initiate(Host, Port, #{} = Opts0) ->
    lager:debug("initiate(~p, ~p, ~p)", [Host, Port, Opts0]),
    Opts = maps:merge(#{client => self(),
                        role   => initiator}, Opts0),
    case init_checks(Opts) of
        ok ->
            start_link(#{ host => Host
                        , port => Port
                        , opts => Opts });
        {error, _Reason} = Err -> Err
    end.

leave(Fsm) ->
    lager:debug("leave(~p)", [Fsm]),
    gen_statem:call(Fsm, leave).

respond(Port, #{} = Opts0) ->
    lager:debug("respond(~p, ~p)", [Port, Opts0]),
    Opts = maps:merge(#{client => self(),
                        role   => responder}, Opts0),
    case init_checks(Opts) of
        ok ->
            start_link(#{ port => Port
                        , opts => Opts });
        {error, _Reason} = Err -> Err
    end.

settle(Fsm) ->
    lager:debug("settle(~p)", [Fsm]),
    gen_statem:call(Fsm, settle).

shutdown(Fsm) ->
    lager:debug("shutdown(~p)", [Fsm]),
    gen_statem:call(Fsm, shutdown).

slash(Fsm) ->
    lager:debug("slash(~p)", [Fsm]),
    gen_statem:call(Fsm, slash).

upd_call_contract(Fsm, #{contract    := _,
                         abi_version := _,
                         amount      := Amt,
                         call_data  := _} = Opts) when is_integer(Amt), Amt >= 0 ->
    lager:debug("upd_call_contract(~p)", [Opts]),
    CallStack = maps:get(call_stack, Opts, []),
    gen_statem:call(Fsm, {upd_call_contract, Opts#{call_stack => CallStack},
                          execute}).

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

upd_transfer(_Fsm, _From, _To, Amount) when Amount < 0 ->
    {error, negative_amount};
upd_transfer(Fsm, From, To, Amount) when is_integer(Amount) ->
    lager:debug("upd_transfer(~p, ~p, ~p, ~p)", [Fsm, From, To, Amount]),
    gen_statem:call(Fsm, {upd_transfer, From, To, Amount}).

upd_withdraw(_Fsm, #{amount := Amt}) when Amt < 0 ->
    {error, negative_amount};
upd_withdraw(Fsm, #{amount := Amt} = Opts) when is_integer(Amt) ->
    lager:debug("upd_withdraw(~p)", [Opts]),
    gen_statem:call(Fsm, {upd_withdraw, Opts}).

where(ChanId, Role) when Role == initiator; Role == responder ->
    gproc:where(gproc_name_by_role(ChanId, Role));
where(ChanId, Pubkey) when is_binary(Pubkey) ->
    gproc:where(gproc_name_by_pubkey(ChanId, Pubkey)).

%% ==================================================================
%% Inspection and configuration functions

patterns() ->
    [{?MODULE, F, A, []} || {F,A} <- gen_tcp:module_info(exports),
                            F =/= module_info] ++
    [{?MODULE, F, A, []} || {F,A} <- ?MODULE:module_info(exports),
                            not lists:member(F, [module_info, patterns])].

record_fields(data ) -> record_info(fields, data);
record_fields(w    ) -> aesc_window:record_fields(w);
record_fields(Other) -> aesc_offchain_state:record_fields(Other).

report_tags() ->
    maps:keys(?DEFAULT_REPORT_FLAGS).

timeouts() ->
    maps:keys(?DEFAULT_TIMEOUTS).

%% Fetch the list of recent fsm events (sliding window)
get_history(Fsm) ->
    lager:debug("get_history(~p)", [Fsm]),
    gen_statem:call(Fsm, get_history).

change_config(Fsm, Key, Value) ->
    case check_change_config(Key, Value) of
        {ok, Key1, Val1} ->
            gen_statem:call(Fsm, {change_config, Key1, Val1});
        {error, _} = Error ->
            Error
    end.

%% Returns a list of [{Pubkey, Balance}], where keys are ordered the same
%% way as in Accounts. If a key doesn't correspond to an existing account,
%% it doesn't show up in the result. Thus, unknown accounts are recognized
%% by their absense in the response.
%%
get_balances(Fsm, Accounts) ->
    gen_statem:call(Fsm, {get_balances, Accounts}).

get_round(Fsm) ->
    gen_statem:call(Fsm, get_round).

prune_local_calls(Fsm) ->
    gen_statem:call(Fsm, prune_local_calls).

dry_run_contract(Fsm, #{contract    := _,
                        abi_version := _,
                        amount      := Amt,
                        call_data   := _} = Opts) when is_integer(Amt), Amt >= 0 ->
    gen_statem:call(Fsm, {upd_call_contract, Opts, dry_run}).

%% ==================================================================
%% Used by noise session

message(Fsm, {T, _} = Msg) when ?KNOWN_MSG_TYPE(T) ->
    lager:debug("message(~p, ~p)", [Fsm, Msg]),
    gen_statem:cast(Fsm, Msg).

%% ==================================================================
%% Used by client

%% Signing requests are sent as plain messages to the Client (normally, the
%% process starting the fsm.) Messages are on the form
%%
%% {aesc_fsm, Fsm :: pid(), ChanId :: binary(), Msg}
%%   where Msg :: {sign, Tag :: sign_tag(), Obj :: any()}}.
%%
%% Perform the appropriate signing
%% ( corresponding to aetx_sign:sign(Obj, [PrivKey]), but likely involving a
%%   remote client. )
%% and reply by calling aesc_fsm:signing_response(Fsm, Tag, SignedObj)
%%
-spec signing_response(pid(), sign_tag(), any()) -> ok.
signing_response(Fsm, Tag, Obj) ->
    gen_statem:cast(Fsm, {?SIGNED, Tag, Obj}).

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
            report(info, channel_accept, Msg, D1),
            {ok, CTx, Updates} = create_tx_for_signing(D1),
            D2 = request_signing(create_tx, CTx, Updates, D1),
            next_state(awaiting_signature, D2);
        {error, _} = Error ->
            close(Error, D)
    end;
initialized(Type, Msg, D) ->
    handle_common_event(Type, Msg, error_all, D).

accepted(enter, _OldSt, _D) -> keep_state_and_data;
accepted(cast, {?FND_CREATED, Msg}, #data{role = responder} = D) ->
    case check_funding_created_msg(Msg, D) of
        {ok, SignedTx, Updates, D1} ->
            report(info, funding_created, D1),
            lager:debug("funding_created: ~p", [SignedTx]),
            D2 = request_signing_(?FND_CREATED, SignedTx, Updates, D1),
            next_state(awaiting_signature, D2);
        {error, Error} ->
             close(Error, D)
    end;
accepted(Type, Msg, D) ->
    handle_common_event(Type, Msg, error_all, D).

awaiting_initial_state(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_initial_state(cast, {?UPDATE, Msg}, #data{role = responder} = D) ->
    lager:debug("got {update, ~p}", [Msg]),
    case check_update_msg(initial, Msg, D) of
        {ok, SignedTx, Updates, D1} ->
            lager:debug("update_msg checks out", []),
            report(info, update, D1),
            D2 = request_signing_(?UPDATE_ACK, SignedTx, Updates, D1),
            next_state(awaiting_signature, D2);
        {error,_} = Error ->
            %% TODO: do we do a dispute challenge here?
            close(Error, D)
    end;
awaiting_initial_state(Type, Msg, D) ->
    handle_common_event(Type, Msg, error, D).

awaiting_leave_ack(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_leave_ack(cast, {?LEAVE_ACK, Msg}, D) ->
    lager:debug("received leave_ack", []),
    case check_leave_ack_msg(Msg, D) of
        {ok, D1} ->
            report_leave(D1),
            close(leave, D1);
        {error, _} = Err ->
            close(Err, D)
    end;
awaiting_leave_ack(timeout, awaiting_leave_ack = T, D) ->
    close({timeout, T}, D);
awaiting_leave_ack(cast, {?LEAVE, Msg}, D) ->
    case check_leave_msg(Msg, D) of
        {ok, D1} ->
            D2 = send_leave_ack_msg(D1),
            report_leave(D2),
            close(leave, D2);
        {error, _} = Err ->
            close(Err, D)
    end;
awaiting_leave_ack(Type, Msg, D) ->
    handle_common_event(Type, Msg, postpone, D).

awaiting_locked(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_locked(cast, {?MIN_DEPTH_ACHIEVED, ChainId, ?WATCH_FND, TxHash},
                #data{latest = {?MIN_DEPTH, ?WATCH_FND, TxHash, CTx, Updates}} = D) ->
    report(info, own_funding_locked, D),
    next_state(
      signed, send_funding_locked_msg(D#data{on_chain_id = ChainId,
                                             latest = {create, CTx,
                                                       Updates}}));
awaiting_locked(cast, {?MIN_DEPTH_ACHIEVED, ChainId, ?WATCH_DEP, TxHash},
                #data{on_chain_id = ChainId,
                      latest = {?MIN_DEPTH, ?WATCH_DEP, TxHash, SignedTx, Updates}} = D) ->
    report(info, own_deposit_locked, D),
    next_state(
      dep_signed, send_deposit_locked_msg(
                    TxHash,
                    D#data{latest = {deposit, SignedTx, Updates}}));
awaiting_locked(cast, {?MIN_DEPTH_ACHIEVED, ChainId, ?WATCH_WDRAW, TxHash},
                #data{on_chain_id = ChainId,
                      latest = {?MIN_DEPTH, ?WATCH_WDRAW, TxHash, SignedTx, Updates}} = D) ->
    report(info, own_withdraw_locked, D),
    next_state(
      wdraw_signed, send_withdraw_locked_msg(
                      TxHash,
                      D#data{latest = {withdraw, SignedTx, Updates}}));
awaiting_locked(cast, {?FND_LOCKED, _Msg}, D) ->
    postpone(D);
awaiting_locked(cast, {?DEP_LOCKED, _Msg}, D) ->
    postpone(D);
awaiting_locked(cast, {?WDRAW_LOCKED, _Msg}, D) ->
    postpone(D);
awaiting_locked(cast, {?DEP_ERR, _Msg}, D) ->
    %% TODO: Stop min depth watcher!
    handle_update_conflict(?DEP_SIGNED, D);
awaiting_locked(cast, {?WDRAW_ERR, _Msg}, D) ->
    %% TODO: Stop min depth watcher!
    handle_update_conflict(?WDRAW_SIGNED, D);
awaiting_locked(Type, Msg, D) ->
    lager:debug("Unexpected ~p: Msg = ~p, Latest = ~p", [Type, Msg, D#data.latest]),
    handle_common_event(Type, Msg, error, D).

awaiting_open(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_open(cast, {?CH_OPEN, Msg}, #data{role = responder} = D) ->
    case check_open_msg(Msg, D) of
        {ok, D1} ->
            report(info, channel_open, Msg, D1),
            gproc_register(D1),
            next_state(accepted, send_channel_accept(D1));
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_open({call, From}, {attach_responder, _Info}, #data{channel_status = attached} = D) ->
    keep_state(D, [{reply, From, {error, taken}}]);
awaiting_open({call, From}, {attach_responder, Info}, #data{opts = Opts} = D) ->
    lager:debug("Attach request: ~p", [Info]),
    #{initiator := I, responder := R} = Opts,
    case check_attach_info(Info, I, R) of
        ok ->
            #{ initiator := I1
             , port      := _Port
             , gproc_key := K } = Info,
            gproc:unreg(K),
            Opts1 = Opts#{initiator => I1},
            {Pid, _} = From,
            keep_state(maybe_initialize_offchain_state(
                         I, I1, D#data{session = Pid, opts = Opts1, channel_status = attached}),
                       [{reply, From, ok}]);
        {error, Err} ->
            lager:debug("Bad attach info: ~p", [Err]),
            gen:reply(From, {error, invalid_attach}),
            keep_state(D, [{reply, From, {error, invalid_attach}}])
    end;
awaiting_open(Type, Msg, D) ->
    handle_common_event(Type, Msg, error_all, D).

awaiting_reestablish(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_reestablish(cast, {?CH_REESTABL, Msg}, #data{role = responder} = D) ->
    case check_reestablish_msg(Msg, D) of
        {ok, D1} ->
            report(info, channel_reestablished, D1),
            gproc_register(D1),
            next_state(open, send_reestablish_ack_msg(D1));
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_reestablish({call, From}, {attach_responder, Info},
                     #data{ opts        = Opts
                          , on_chain_id = ChannelId
                          , role        = responder } = D) ->
    lager:debug("Attach request: ~p", [Info]),
    #{ responder := R } = Opts,
    ChainHash = aec_chain:genesis_hash(),
    case Info of
        #{ chain_hash := ChainHash
         , channel_id := ChannelId
         , responder  := R
         , port       := _Port
         , gproc_key  := K } ->
            gproc:unreg(K),
            gen:reply(From, ok),
            {Pid, _} = From,
            keep_state(D#data{session = Pid}, [{reply, From, ok}]);
        _ ->
            keep_state(D, [{reply, From, {error, invalid_attach}}])
    end;
awaiting_reestablish(Type, Msg, D) ->
    handle_common_event(Type, Msg, error_all, D).

awaiting_signature(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_signature(cast, {Req, _} = Msg, #data{ongoing_update = true} = D)
  when ?UPDATE_CAST(Req) ->
    %% Race detection!
    lager:debug("race detected: ~p", [Msg]),
    handle_update_conflict(Req, D);
awaiting_signature(cast, {?SIGNED, slash_tx, SignedTx} = Msg,
                   #data{latest = {sign, slash_tx, _CTx, _}} = D) ->
    lager:debug("slash_tx signed", []),
    %% TODO: Would be prudent to check the SignedTx before pushing
    ok = aec_tx_pool:push(SignedTx),
    next_state(channel_closing, log(rcv, ?SIGNED, Msg, D#data{latest = undefined}));
awaiting_signature(cast, {?SIGNED, create_tx, Tx} = Msg,
                   #data{role = initiator, latest = {sign, create_tx, CTx,
                                                     Updates}} = D) ->
    maybe_check_sigs_create(Tx, Updates, initiator,
                            fun() ->
                                next_state(half_signed, send_funding_created_msg(
                                    Tx, log(rcv, ?SIGNED, Msg,
                                            D#data{latest = {ack, create_tx,
                                                             CTx, Updates}})))
                            end, D);
awaiting_signature(cast, {?SIGNED, deposit_tx, Tx} = Msg,
                   #data{latest = {sign, deposit_tx, DTx, Updates}} = D) ->
    maybe_check_sigs(Tx, Updates, aesc_deposit_tx, not_deposit_tx, me,
        fun() ->
            next_state(dep_half_signed,
                  send_deposit_created_msg(Tx, Updates,
                      log(rcv, ?SIGNED, Msg, D#data{latest = {ack,
                                                              deposit_tx, DTx,
                                                     Updates}})))
        end, D);
awaiting_signature(cast, {?SIGNED, withdraw_tx, Tx} = Msg,
                   #data{latest = {sign, withdraw_tx, WTx, Updates}} = D) ->
    maybe_check_sigs(Tx, Updates, aesc_withdraw_tx, not_withdraw_tx, me,
        fun() ->
            next_state(wdraw_half_signed,
                    send_withdraw_created_msg(Tx, Updates,
                        log(rcv, ?SIGNED, Msg, D#data{latest = {ack,
                                                                withdraw_tx,
                                                                WTx, Updates}})))
        end, D);
awaiting_signature(cast, {?SIGNED, ?FND_CREATED, SignedTx} = Msg,
                   #data{role = responder,
                         latest = {sign, ?FND_CREATED, HSCTx, Updates}} = D) ->
    maybe_check_sigs_create(SignedTx, Updates, both,
        fun() ->
            D1 = send_funding_signed_msg(
                  SignedTx,
                  log(rcv, ?SIGNED, Msg, D#data{create_tx = SignedTx,
                                                latest = {ack, ?FND_CREATED,
                                                          HSCTx, Updates}})),
            report_on_chain_tx(?FND_CREATED, SignedTx, D1),
            {ok, D2} = start_min_depth_watcher({?MIN_DEPTH, ?WATCH_FND}, SignedTx,
                                               Updates, D1),
            gproc_register_on_chain_id(D2),
            next_state(awaiting_locked, D2)
        end, D);
awaiting_signature(cast, {?SIGNED, ?DEP_CREATED, SignedTx} = Msg,
                   #data{latest = {sign, ?DEP_CREATED, HSCTx, Updates}} = D) ->
    maybe_check_sigs(SignedTx, Updates, aesc_deposit_tx, not_deposit_tx, both,
        fun() ->
            D1 = send_deposit_signed_msg(SignedTx,
                                          D#data{latest = {ack,
                                                           ?DEP_CREATED,
                                                           HSCTx, Updates}}),
            report_on_chain_tx(?DEP_CREATED, SignedTx, D1),
            {ok, D2} = start_min_depth_watcher({?MIN_DEPTH, ?WATCH_DEP}, SignedTx,
                                               Updates, D1),
            next_state(awaiting_locked,
                      log(rcv, ?SIGNED, Msg, D2))
        end, D);
awaiting_signature(cast, {?SIGNED, ?WDRAW_CREATED, SignedTx} = Msg,
                   #data{latest = {sign, ?WDRAW_CREATED, HSCTx, Updates}} = D) ->
    maybe_check_sigs(SignedTx, Updates, aesc_withdraw_tx, not_withdraw_tx, both,
        fun() ->
            D1 = send_withdraw_signed_msg(SignedTx,
                                          D#data{latest = {ack,
                                                           ?WDRAW_CREATED,
                                                           HSCTx, Updates}}),
            report_on_chain_tx(?WDRAW_CREATED, SignedTx, D1),
            {ok, D2} = start_min_depth_watcher({?MIN_DEPTH, ?WATCH_WDRAW}, SignedTx,
                                               Updates, D1),
            next_state(awaiting_locked, log(rcv, ?SIGNED, Msg, D2))
        end, D);
awaiting_signature(cast, {?SIGNED, ?UPDATE, SignedTx} = Msg,
                   #data{latest = {sign, ?UPDATE, OCTx, Updates}} = D) ->
    maybe_check_sigs(SignedTx, Updates, aesc_offchain_tx, not_offchain_tx, me,
        fun() ->
            D1 = send_update_msg(
                  SignedTx, Updates,
                  D#data{state = aesc_offchain_state:set_half_signed_tx(
                                    SignedTx, D#data.state)}),
            next_state(awaiting_update_ack,
                      log(rcv, ?SIGNED, Msg, D1#data{latest = {ack, ?UPDATE,
                                                               OCTx, Updates}}))
        end, D);
awaiting_signature(cast, {?SIGNED, ?UPDATE_ACK, SignedTx} = Msg,
                   #data{latest = {sign, ?UPDATE_ACK, _OCTx, Updates}, opts = Opts} = D) ->
    maybe_check_sigs(SignedTx, Updates, aesc_offchain_tx, not_offchain_tx, both,
        fun() ->
            D1 = send_update_ack_msg(SignedTx, D),
            {OnChainEnv, OnChainTrees} =
                aetx_env:tx_env_and_trees_from_top(aetx_contract),
            State = aesc_offchain_state:set_signed_tx(SignedTx, Updates, D1#data.state,
                                                      OnChainTrees, OnChainEnv, Opts),
            D2 = D1#data{log   = log_msg(rcv, ?SIGNED, Msg, D1#data.log),
                        state = State,
                        latest = undefined},
            next_state(open, D2)
        end, D);
awaiting_signature(cast, {?SIGNED, ?SHUTDOWN, SignedTx} = Msg,
                   #data{latest = {sign, ?SHUTDOWN, _, Updates}} = D) ->
    maybe_check_sigs(SignedTx, Updates, aesc_close_mutual_tx, not_close_mutual_tx, me,
        fun() ->
            D1 = send_shutdown_msg(SignedTx, D),
            D2 = D1#data{latest = {shutdown, SignedTx, Updates},
                        log    = log_msg(rcv, ?SIGNED, Msg, D1#data.log)},
            next_state(mutual_closing, D2)
        end, D);
awaiting_signature(cast, {?SIGNED, ?SHUTDOWN_ACK, SignedTx} = Msg,
                   #data{latest = {sign, ?SHUTDOWN_ACK, _CMTx, Updates}} = D) ->
    maybe_check_sigs(SignedTx, Updates, aesc_close_mutual_tx, not_close_mutual_tx, both,
        fun() ->
            D1 = send_shutdown_ack_msg(SignedTx, D),
            D2 = D1#data{latest = undefined,
                        log    = log_msg(rcv, ?SIGNED, Msg, D1#data.log)},
            report_on_chain_tx(close_mutual, SignedTx, D1),
            close(close_mutual, D2)
        end, D);
awaiting_signature(cast, {?SIGNED, close_solo_tx, SignedTx} = Msg,
                   #data{latest = {sign, close_solo_tx, _CSTx, Updates}} = D) ->
    D1 = D#data{log = log_msg(rcv, ?SIGNED, Msg, D#data.log),
                latest = undefined},
    case verify_signatures_onchain_check(pubkeys(me, D), SignedTx) of
        ok ->
            close_solo_signed(SignedTx, Updates, D1);
        {error,_} = Error ->
            lager:debug("Failed signature check: ~p", [Error]),
            next_state(open, D1)
    end;
awaiting_signature(cast, {?SIGNED, settle_tx, SignedTx} = Msg,
                   #data{latest = {sign, settle_tx, _STx, Updates}} = D) ->
    maybe_check_sigs(SignedTx, Updates, channel_settle_tx, not_settle_tx, me,
        fun() ->
                D1 = D#data{log = log_msg(rcv, ?SIGNED, Msg, D#data.log),
                            latest = undefined},
                settle_signed(SignedTx, Updates, D1)
        end, D);

%% Timeouts
awaiting_signature(timeout, _Msg,
                   #data{channel_status = closing, latest = {sign, Tag, _, _}} = D) when
                    Tag =:= ?SHUTDOWN;
                    Tag =:= ?SHUTDOWN_ACK ->
    lager:debug("Timeout while waiting for signature on mutual close"),
    next_state(channel_closing, D#data{latest = undefined});

%% Other
awaiting_signature(Type, Msg, D) ->
    handle_common_event(Type, Msg, postpone, D).

awaiting_update_ack(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_update_ack(cast, {Req, _} = Msg, #data{} = D) when ?UPDATE_CAST(Req) ->
    %% This might happen if a request is sent before our signed ?UPDATE msg
    %% arrived.
    lager:debug("race detected: ~p", [Msg]),
    handle_update_conflict(Req, D);
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
            report(conflict, Msg, D1),
            next_state(open, D1);
        {ok, _ErrorCode, D1} ->
            %% TODO: send a different kind of report (e.g. validation error)
            report(conflict, Msg, D1),
            next_state(open, D1);
        {error, fallback_state_mismatch} = Error ->
            %% falling back to an inconsistent state
            %% this is possible if we are a malicious actor only
            report(conflict, Msg, D),
            close(Error, D);
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_update_ack({call, _}, _Req, D) ->
    postpone(D);
awaiting_update_ack(Type, Msg, D) ->
    handle_common_event(Type, Msg, postpone, D).

channel_closed(enter, _OldSt, D) ->
    keep_state(D#data{ongoing_update = false});
channel_closed(cast, {?MIN_DEPTH_ACHIEVED, ChainId, ?WATCH_CLOSED, TxHash},
               #data{ latest = {?MIN_DEPTH, ?WATCH_CLOSED, TxHash, _, _}
                    , on_chain_id = ChainId } = D) ->
    close(closed_confirmed, D);
channel_closed(cast, {?CHANNEL_CHANGED, _Info} = Msg, D) ->
    %% This is a weird case. The channel was closed, but now isn't.
    %% This would indicate a fork switch. TODO: detect channel status
    %% and respond accordingly. For now. Panic and terminate
    report(info, zombie_channel, D),
    close(zombie_channel, log(rcv, msg_type(Msg), Msg, D));
channel_closed(Type, Msg, D) ->
    handle_common_event(Type, Msg, discard, D).

channel_closing(enter, _OldSt, D) ->
    D1 = if D#data.channel_status =/= closing ->
                 report(info, closing, D),
                 report_update(D#data{channel_status = closing});
            true ->
                 report_update(D)
         end,
    keep_state(D1#data{ongoing_update = false});
channel_closing(cast, {?CHANNEL_UNLOCKED, #{chan_id := ChId}} = Msg,
                #data{on_chain_id = ChId, latest = {watch, unlock, _TxHash, SignedTx, _}} = D) ->
    lager:debug("channel unlocked", []),
    D1 = log(rcv, ?CHANNEL_UNLOCKED, Msg, D),
    {Type, _Tx} = aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
    case Type of
        channel_settle_tx ->
            lager:debug("pushing settle tx", []),
            ok = aec_tx_pool:push(SignedTx);
        _ ->
            lager:debug("SignedTx of Type ~p - ignoring", [Type]),
            ok
    end,
    keep_state(D1);
channel_closing(cast, {?SHUTDOWN, Msg}, D) ->
    case {is_fork_active(?LIMA_PROTOCOL_VSN), check_shutdown_msg(Msg, D)} of
        {false, _} ->
            %% TODO: send an ?UPDATE_ERR (which isn't yet defined)
            lager:debug("Shutdown while channel_closing not allowed before the Lima fork"),
            keep_state(D);
        {true, {ok, SignedTx, Updates, D1}} ->
            D2 = request_signing_(?SHUTDOWN_ACK, SignedTx, Updates, D1),
            next_state(awaiting_signature, D2);
        {true, {error, E}} ->
            %% TODO: send an ?UPDATE_ERR (which isn't yet defined)
            %% For now, log and ignore
            lager:debug("Bad shutdown_msg in channel_closing: ~p", [E]),
            keep_state(D)
    end;
channel_closing(Type, Msg, D) ->
    handle_common_event(Type, Msg, discard, D).

dep_half_signed(enter, _OldSt, _D) -> keep_state_and_data;
dep_half_signed(cast, {Req, _} = Msg, D) when ?UPDATE_CAST(Req) ->
    %% This might happen if a request is sent before our ?DEP_CREATED msg
    %% arrived.
    lager:debug("race detected: ~p", [Msg]),
    handle_update_conflict(Req, D);
dep_half_signed(cast, {?DEP_SIGNED, Msg}, #data{latest = Latest} = D) ->
    case check_deposit_signed_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report_on_chain_tx(?DEP_SIGNED, SignedTx, D1),
            ok = aec_tx_pool:push(SignedTx),
            {ack, deposit_tx, _, Updates} = Latest,
            {ok, D2} = start_min_depth_watcher({?MIN_DEPTH, ?WATCH_DEP}, SignedTx, Updates, D1),
            next_state(awaiting_locked, D2);
        {error, _Error} ->
            handle_update_conflict(?DEP_SIGNED, D)
    end;
dep_half_signed(cast, {?DEP_ERR, Msg}, D) ->
    lager:debug("received deposit_error: ~p", [Msg]),
    case check_deposit_error_msg(Msg, D) of
        {ok, ?ERR_CONFLICT, D1} ->
            report(conflict, Msg, D1),
            next_state(open, D1);
        {ok, _ErrorCode, D1} ->
            %% TODO: send a different kind of report (e.g. validation error)
            report(conflict, Msg, D1),
            next_state(open, D1);
        {error, _} = Error ->
            close(Error, D)
    end;
dep_half_signed(Type, Msg, D) ->
    handle_common_event(Type, Msg, error, D).

dep_signed(enter, _OldSt, _D) -> keep_state_and_data;
dep_signed(cast, {?DEP_LOCKED, Msg}, #data{latest = {deposit, SignedTx,
                                                     Updates}} = D) ->
    case check_deposit_locked_msg(Msg, SignedTx, D) of
        {ok, D1} ->
            report(info, deposit_locked, D1),
            deposit_locked_complete(SignedTx, Updates, D1#data{latest = undefined});
        {error, _} = Error ->
            close(Error, D)
    end;
dep_signed(timeout, dep_signed = T, D) ->
    close({timeout, T}, D);
dep_signed(cast, {?SHUTDOWN, _Msg}, D) ->
    next_state(mutual_closing, D);
dep_signed(Type, Msg, D) ->
    handle_common_event(Type, Msg, error, D).

half_signed(enter, _OldSt, _D) -> keep_state_and_data;
half_signed(cast, {?FND_SIGNED, Msg}, #data{role = initiator,
                                            latest = Latest} = D) ->
    case check_funding_signed_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            lager:debug("funding_signed ok", []),
            report(info, funding_signed, D1),
            report_on_chain_tx(?FND_SIGNED, SignedTx, D1),
            ok = aec_tx_pool:push(SignedTx),
            D2 = D1#data{create_tx = SignedTx},
            {ack, create_tx, _, Updates} = Latest,
            {ok, D3} = start_min_depth_watcher({?MIN_DEPTH, ?WATCH_FND}, SignedTx, Updates, D2),
            next_state(awaiting_locked, D3);
        {error, Error} ->
            close(Error, D)
    end;
half_signed(Type, Msg, D) ->
    handle_common_event(Type, Msg, error_all, D).

mutual_closing(enter, _OldSt, _D) -> keep_state_and_data;
mutual_closing(cast, {?SHUTDOWN_ACK, Msg}, D) ->
    case check_shutdown_ack_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            lager:debug("shutdown_ack ok", []),
            ok = aec_tx_pool:push(SignedTx),
            report_on_chain_tx(close_mutual, SignedTx, D1),
            close(close_mutual, D1);
        {error, _} = Error ->
            close(Error, D)
    end;
mutual_closing(cast, {closing_signed, _Msg}, D) ->  %% TODO: not using this, right?
    close(closing_signed, D);
mutual_closing(timeout, _Msg, #data{channel_status = closing} = D) ->
    lager:debug("Timeout while waiting for peer signature on mutual close"),
    next_state(channel_closing, D);
mutual_closing(Type, Msg, D) ->
    handle_common_event(Type, Msg, error, D).

open(enter, _OldSt, D) ->
    D1 = if D#data.channel_status =/= open ->
                 report(info, open, D),
                 report_update(D#data{channel_status = open});
            true ->
                 report_update(D)
         end,
    keep_state(D1#data{ongoing_update = false});
open(cast, {?UPDATE, Msg}, D) ->
    case check_update_msg(normal, Msg, D) of
        {ok, SignedTx, Updates, D1} ->
            report(info, update, D1),
            D2 = request_signing_(?UPDATE_ACK, SignedTx, Updates, D1),
            next_state(awaiting_signature, set_ongoing(D2));
        {error, _Error} ->
            handle_update_conflict(?UPDATE, D)
    end;
open(cast, {?UPDATE_ERR, Msg}, D) ->
    %% this will happen only if we are malicious, this does cause invalid
    %% state
    report(conflict, Msg, D),
    keep_state(D);
open(cast, {?DEP_CREATED, Msg}, D) ->
    case check_deposit_created_msg(Msg, D) of
        {ok, SignedTx, Updates, D1} ->
            report(info, deposit_created, D1),
            lager:debug("deposit_created: ~p", [SignedTx]),
            D2 = request_signing_(?DEP_CREATED, SignedTx, Updates, D1),
            next_state(awaiting_signature, set_ongoing(D2));
        {error, _Error} ->
            handle_update_conflict(?DEP_CREATED, D)
    end;
open(cast, {?WDRAW_CREATED, Msg}, D) ->
    case check_withdraw_created_msg(Msg, D) of
        {ok, SignedTx, Updates, D1} ->
            report(info, withdraw_created, D1),
            lager:debug("withdraw_created: ~p", [SignedTx]),
            D2 = request_signing_(?WDRAW_CREATED, SignedTx, Updates, D1),
            next_state(awaiting_signature, set_ongoing(D2));
        {error, _Error} ->
            handle_update_conflict(?WDRAW_CREATED, D)
    end;
open(cast, {?SIGNED, _, _} = Msg, D) ->
    lager:debug("Received signing reply in 'open' - ignore: ~p", [Msg]),
    keep_state(log(ignore, ?SIGNED, Msg, D));
open({call, From}, Request, D) ->
    handle_call(open, Request, From, D);
open(cast, {?LEAVE, Msg}, D) ->
    case check_leave_msg(Msg, D) of
        {ok, D1} ->
            lager:debug("received leave msg", []),
            send_leave_ack_msg(D1),
            report_leave(D1),
            close(leave, D1);
        {error, _} = Err ->
            close(Err, D)
    end;
open(cast, {?SHUTDOWN, Msg}, D) ->
    case check_shutdown_msg(Msg, D) of
        {ok, SignedTx, Updates, D1} ->
            D2 = request_signing_(?SHUTDOWN_ACK, SignedTx, Updates, D1),
            next_state(awaiting_signature, D2);
        {error, E} ->
            close({shutdown_error, E}, D)
    end;
open(Type, Msg, D) ->
    handle_common_event(Type, Msg, discard, D).


deposit_locked_complete(SignedTx,
                        Updates,
                        #data{state = State,
                              opts = Opts} = D) ->
    {OnChainEnv, OnChainTrees} =
        aetx_env:tx_env_and_trees_from_top(aetx_contract),
    lager:debug("Applying updates: ~p", [Updates]),
    D1   = D#data{state = aesc_offchain_state:set_signed_tx(SignedTx, Updates,
                                                            State,
                                                            OnChainTrees,
                                                            OnChainEnv, Opts)},
    next_state(open, D1).

reestablish_init(enter, _OldSt, _D) -> keep_state_and_data;
reestablish_init(cast, {?CH_REEST_ACK, Msg}, D) ->
    case check_reestablish_ack_msg(Msg, D) of
        {ok, D1} ->
            report(info, channel_reestablished, D1),
            next_state(open, D1);
        {error, _} = Err ->
            close(Err, D)
    end;
reestablish_init(Type, Msg, D) ->
    handle_common_event(Type, Msg, error_all, D).

signed(enter, _OldSt, _D) -> keep_state_and_data;
signed(cast, {?FND_LOCKED, Msg}, D) ->
    case check_funding_locked_msg(Msg, D) of
        {ok, D1} ->
            report(info, funding_locked, D1),
            funding_locked_complete(D1);
        {error, _} = Error ->
            close(Error, D)
    end;
signed(cast, {?SHUTDOWN, _Msg}, D) ->
    next_state(mutual_closing, D);
signed(Type, Msg, D) ->
    handle_common_event(Type, Msg, error_all, D).

wdraw_half_signed(enter, _OldSt, _D) -> keep_state_and_data;
wdraw_half_signed(cast, {Req,_} = Msg, D) when ?UPDATE_CAST(Req) ->
    %% This might happen if a request is sent before our ?WDRAW_CREATED msg
    %% arrived.
    lager:debug("race detected: ~p", [Msg]),
    handle_update_conflict(Req, D);
wdraw_half_signed(cast, {?WDRAW_SIGNED, Msg}, #data{latest = Latest} = D) ->
    case check_withdraw_signed_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report_on_chain_tx(?WDRAW_SIGNED, SignedTx, D1),
            ok = aec_tx_pool:push(SignedTx),
            {ack, withdraw_tx, _, Updates} = Latest,
            {ok, D2} = start_min_depth_watcher(
                         {?MIN_DEPTH, ?WATCH_WDRAW}, SignedTx, Updates, D1),
            next_state(awaiting_locked, D2);
        {error, _Error} ->
            handle_update_conflict(?WDRAW_SIGNED, D)
    end;
wdraw_half_signed(cast, {?WDRAW_ERR, Msg}, D) ->
    lager:debug("received withdraw_error: ~p", [Msg]),
    case check_withdraw_error_msg(Msg, D) of
        {ok, ?ERR_CONFLICT, D1} ->
            report(conflict, Msg, D1),
            next_state(open, D1);
        {ok, _ErrorCode, D1} ->
            %% TODO: send a different kind of report (e.g. validation error)
            report(conflict, Msg, D1),
            next_state(open, D1);
        {error, _} = Error ->
            close(Error, D)
    end;
wdraw_half_signed(Type, Msg, D) ->
    handle_common_event(Type, Msg, error, D).

%% Don't flag for update conflicts once we've pushed to the chain, and
%% wait for confirmation; postpone instead.
%%
wdraw_signed(enter, _OldSt, _D) -> keep_state_and_data;
wdraw_signed(cast, {?WDRAW_LOCKED, Msg}, #data{latest = {withdraw, SignedTx,
                                                         Updates}} = D) ->
    case check_withdraw_locked_msg(Msg, SignedTx, D) of
        {ok, D1} ->
            report(info, withdraw_locked, D1),
            withdraw_locked_complete(SignedTx, Updates, D1#data{latest = undefined});
        {error, _} = Error ->
            close(Error, D)
    end;
wdraw_signed(cast, {?SHUTDOWN, _Msg}, D) ->
    next_state(mutual_closing, D);
wdraw_signed(Type, Msg, D) ->
    handle_common_event(Type, Msg, error, D).

%% ======================================================================
%% Action logic

send_open_msg(#data{opts       = Opts,
                    session    = Sn} = Data) ->
    #{ lock_period        := LockPeriod
     , initiator          := Initiator
     , responder          := Responder
     , push_amount        := PushAmount
     , initiator_amount   := InitiatorAmount
     , responder_amount   := ResponderAmount
     , channel_reserve    := ChannelReserve } = Opts,
    ChainHash = aec_chain:genesis_hash(),
    %%
    %% Generate a temporary channel id
    ChannelPubKey = aesc_channels:pubkey(Initiator,
                                         erlang:unique_integer(),
                                         Responder),
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
    Data#data{ channel_id = ChannelPubKey
             , log = log_msg(snd, ?CH_OPEN, Msg, Data#data.log) }.

check_open_msg(#{ chain_hash           := ChainHash
                , temporary_channel_id := ChanId
                , lock_period          := LockPeriod
                , push_amount          := PushAmt
                , initiator_amount     := InitiatorAmt
                , responder_amount     := ResponderAmt
                , channel_reserve      := ChanReserve
                , initiator            := InitiatorPubkey
                , responder            := ResponderPubkey } = Msg,
               #data{opts = Opts} = Data) ->
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
            {ok, Data#data{
                   channel_id = ChanId,
                   opts       = Opts1,
                   log        = log_msg(rcv, ?CH_OPEN, Msg, Data#data.log) }};
        _ ->
            {error, chain_hash_mismatch}
    end.

send_reestablish_msg(#{ existing_channel_id := ChId
                      , offchain_tx := OffChainTx },
                     #data{ session = Sn } = Data) ->
    ChainHash = aec_chain:genesis_hash(),
    TxBin = aetx_sign:serialize_to_binary(OffChainTx),
    Msg = #{ chain_hash => ChainHash
           , channel_id => ChId
           , data       => #{tx => TxBin} },
    aesc_session_noise:channel_reestablish(Sn, Msg),
    Data#data{channel_id  = ChId,
              on_chain_id = ChId,
              latest      = {reestablish, OffChainTx},
              log         = log_msg(snd, ?CH_REESTABL, Msg, Data#data.log)}.

check_reestablish_msg(#{ chain_hash := ChainHash
                       , channel_id := ChId
                       , data       := #{tx := TxBin} } = Msg,
                      #data{state = State} = Data) ->
    case get_channel(ChainHash, ChId) of
        {ok, _Channel} ->
            SignedTx = aetx_sign:deserialize_from_binary(TxBin),
            case aesc_offchain_state:check_reestablish_tx(SignedTx, State) of
                {ok, NewState} ->
                    Log1 = log_msg(rcv, ?CH_REESTABL, Msg, Data#data.log),
                    {ok, Data#data{ channel_id  = ChId
                                  , on_chain_id = ChId
                                  , state       = NewState
                                  , log         = Log1}};
                {error, _} = TxErr ->
                    TxErr
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
    Data#data{ latest = undefined
             , log = log_msg(snd, ?CH_REEST_ACK, Msg, Data#data.log) }.

check_reestablish_ack_msg(#{ data := #{tx := TxBin} } = Msg, Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    run_checks(
      [ fun chk_chain_hash/3
      , fun chk_channel_id/3
      , fun chk_dual_sigs/3
      , fun chk_same_tx/3
      , fun log_reestabl_ack_msg/3
      ], Msg, SignedTx, Data).

run_checks([F|Funs], Msg, SignedTx, Data) ->
    case F(Msg, SignedTx, Data) of
        {true, _} ->
            run_checks(Funs, Msg, SignedTx, Data);
        {false, Err} ->
            {error, Err};
        {data, Data1} ->
            run_checks(Funs, Msg, SignedTx, Data1)
    end;
run_checks([], _, _, Data) ->
    {ok, Data}.

chk_chain_hash(#{ chain_hash := CH }, _, _) ->
    {CH == aec_chain:genesis_hash(), chain_hash_mismatch}.

chk_channel_id(#{ channel_id := ChId }, _, #data{ on_chain_id = OCId }) ->
    {ChId == OCId, channel_id_mismatch}.

chk_dual_sigs(_, SignedTx, #data{on_chain_id = ChannelPubkey}) ->
    {Mod, _Tx} = aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    {ok, Channel} = aec_chain:get_channel(ChannelPubkey),
    InitiatorPubkey = aesc_channels:initiator_pubkey(Channel),
    ResponderPubkey = aesc_channels:responder_pubkey(Channel),
    Pubkeys = [InitiatorPubkey, ResponderPubkey],
    case Mod of
        aesc_offchain_tx ->
            Res =
                verify_signatures_offchain(ChannelPubkey,
                                            Pubkeys,
                                            SignedTx),
            {ok == Res, signatures_invalid};
        _ ->
            case aec_chain:find_tx_location(aetx_sign:hash(SignedTx)) of
                %% The last state we are aware of is on-chain so it could be
                %% used in disputes
                %% TODO: check for Forced Progress while the FSM had been
                %% offline
                BH when is_binary(BH) -> {true, this_not_used}; 
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

chk_same_tx(_, SignedTx, #data{ state = State }) ->
    {_, MySignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    {aetx_sign:serialize_to_binary(SignedTx)
     == aetx_sign:serialize_to_binary(MySignedTx), offchain_state_mismatch}.

log_reestabl_ack_msg(Msg, _, #data{log = L} = D) ->
    {data, D#data{log = log_msg(rcv, ?CH_REEST_ACK, Msg, L)}}.

send_channel_accept(#data{opts          = Opts,
                          session       = Sn,
                          channel_id    = ChanId} = Data) ->
    #{ minimum_depth      := MinDepth
     , initiator          := Initiator
     , responder          := Responder
     , initiator_amount   := InitiatorAmt
     , responder_amount   := ResponderAmt
     , channel_reserve    := ChanReserve } = Opts,
    ChainHash = aec_chain:genesis_hash(),
    Msg = #{ chain_hash           => ChainHash
           , temporary_channel_id => ChanId
           , minimum_depth        => MinDepth
           , initiator_amount     => InitiatorAmt
           , responder_amount     => ResponderAmt
           , channel_reserve      => ChanReserve
           , initiator            => Initiator
           , responder            => Responder
           },
    aesc_session_noise:channel_accept(Sn, Msg),
    Data#data{log = log_msg(snd, ?CH_ACCEPT, Msg, Data#data.log)}.

check_accept_msg(#{ chain_hash           := ChainHash
                  , temporary_channel_id := ChanId
                  , minimum_depth        := MinDepth
                  , initiator_amount     := _InitiatorAmt
                  , responder_amount     := _ResponderAmt
                  , channel_reserve      := _ChanReserve
                  , initiator            := Initiator
                  , responder            := Responder} = Msg,
                 #data{channel_id = ChanId,
                       opts = Opts} = Data) ->
    %% TODO: implement more checks
    case aec_chain:genesis_hash() of
        ChainHash ->
            Log1 = log_msg(rcv, ?CH_ACCEPT, Msg, Data#data.log),
            {ok, Data#data{ opts = Opts#{ minimum_depth => MinDepth
                                        , initiator     => Initiator
                                        , responder     => Responder}
                          , log = Log1 }};
        _ ->
            {error, chain_hash_mismatch}
    end.

new_onchain_tx_for_signing(Type, Opts, D) ->
    try new_onchain_tx_for_signing_(Type, Opts, D)
    catch
        error:Reason ->
            ?LOG_CAUGHT(Reason),
            error(Reason)
    end.

new_onchain_tx_for_signing_(Type, Opts, D) ->
    Defaults = tx_defaults(Type, Opts, D),
    Opts1 = maps:merge(Defaults, Opts),
    CurrHeight = curr_height(),
    {ok, Tx, Updates} = new_onchain_tx(Type, Opts1, D, CurrHeight),
    case {aetx:min_fee(Tx, CurrHeight), aetx:fee(Tx)} of
        {MinFee, Fee} when MinFee =< Fee ->
            {ok, Tx, Updates};
        {MinFee, Fee} ->
            lager:debug("Fee (~p) is too low for ~p (Min: ~p)",
                        [Fee, Type, MinFee]),
            error(too_low_fee)
    end.

-spec new_onchain_tx(channel_create_tx
                   | channel_close_mutual_tx
                   | channel_deposit_tx
                   | channel_withdraw_tx
                   | channel_close_solo_tx
                   | channel_slash_tx
                   | channel_settle_tx, map(), #data{}, aec_blocks:height()) ->
    {ok, aetx:tx(), [aesc_offchain_update:update()]}.
new_onchain_tx(channel_close_mutual_tx, #{ acct := From } = Opts,
               #data{opts = DOpts, on_chain_id = Chan, state = State}, _) ->
    #{initiator := Initiator,
      responder := Responder} = DOpts,
    ChanId = aeser_id:create(channel, Chan),
    FromId = aeser_id:create(account, From),
    {ok, IAmt} = aesc_offchain_state:balance(Initiator, State),
    {ok, RAmt} = aesc_offchain_state:balance(Responder, State),
    %% fee is preset. This is important for participant's amounts calculation
    Fee = maps:get(fee, Opts),
    Nonce = maps:get(nonce, Opts),
    TTL = maps:get(ttl, Opts, 0), %% 0 means no TTL limit
    {IAmt1, RAmt1} = pay_close_mutual_fee(Fee, IAmt, RAmt),
    {ok, CloseMutualTx} =
        aesc_close_mutual_tx:new(#{ channel_id             => ChanId
                                  , from_id                => FromId
                                  , initiator_amount_final => IAmt1
                                  , responder_amount_final => RAmt1
                                  , ttl                    => TTL
                                  , fee                    => Fee
                                  , nonce                  => Nonce }),
    {ok, CloseMutualTx, []};
new_onchain_tx(channel_deposit_tx, #{acct := FromId,
                                     amount := Amount} = Opts,
               #data{on_chain_id = ChanId, state=State}, CurrHeight) ->
    Updates = [aesc_offchain_update:op_deposit(aeser_id:create(account, FromId), Amount)],
    {OnChainEnv, OnChainTrees} =
        aetx_env:tx_env_and_trees_from_top(aetx_contract),
    Height = aetx_env:height(OnChainEnv),
    ActiveProtocol = aec_hard_forks:protocol_effective_at_height(Height),
    UpdatedStateTx = aesc_offchain_state:make_update_tx(Updates, State,
                                                        ChanId,
                                                        ActiveProtocol,
                                                        OnChainTrees,
                                                        OnChainEnv, Opts),
    {channel_offchain_tx, UpdatedOffchainTx} = aetx:specialize_type(UpdatedStateTx),
    StateHash = aesc_offchain_tx:state_hash(UpdatedOffchainTx),

    {LastRound, _} = aesc_offchain_state:get_latest_signed_tx(State),
    Opts1 = maps:merge(Opts, #{state_hash => StateHash,
                               round      => LastRound + 1,
                               channel_id => aeser_id:create(channel, ChanId),
                               from_id    => aeser_id:create(account, FromId)
                               }),
    lager:debug("deposit_tx Opts = ~p", [Opts1]),
    {ok, DepositTx} = new_onchain_tx_(aesc_deposit_tx, Opts1, CurrHeight),
    {ok, DepositTx, Updates};
new_onchain_tx(channel_withdraw_tx, #{acct := ToId,
                                      amount := Amount} = Opts,
               #data{on_chain_id = ChanId, state=State}, CurrHeight) ->
    Updates = [aesc_offchain_update:op_withdraw(aeser_id:create(account, ToId), Amount)],
    {OnChainEnv, OnChainTrees} =
        aetx_env:tx_env_and_trees_from_top(aetx_contract),
    Height = aetx_env:height(OnChainEnv),
    ActiveProtocol = aec_hard_forks:protocol_effective_at_height(Height),
    UpdatedStateTx = aesc_offchain_state:make_update_tx(Updates, State,
                                                        ChanId,
                                                        ActiveProtocol,
                                                        OnChainTrees,
                                                        OnChainEnv, Opts),
    {channel_offchain_tx, UpdatedOffchainTx} = aetx:specialize_type(UpdatedStateTx),
    StateHash = aesc_offchain_tx:state_hash(UpdatedOffchainTx),

    {LastRound, _} = aesc_offchain_state:get_latest_signed_tx(State),
    Opts1 = maps:merge(Opts, #{state_hash => StateHash,
                               round      => LastRound + 1,
                               channel_id => aeser_id:create(channel, ChanId),
                               to_id      => aeser_id:create(account, ToId)
                              }),
    lager:debug("withdraw_tx Opts = ~p", [Opts1]),
    {ok, WithdrawTx} = new_onchain_tx_(aesc_withdraw_tx, Opts1, CurrHeight),
    {ok, WithdrawTx, Updates};
new_onchain_tx(channel_create_tx, Opts,
               #data{opts = #{initiator := Initiator,
                              responder := Responder},
                     state=State}, CurrHeight) ->
    StateHash = aesc_offchain_state:hash(State),
    Opts1 = Opts#{ state_hash    => StateHash
                 , initiator_id  => aeser_id:create(account, Initiator)
                 , responder_id  => aeser_id:create(account, Responder)
                 },
    lager:debug("create_tx Opts = ~p", [Opts1]),
    {ok, CreateTx} = new_onchain_tx_(aesc_create_tx, Opts1, CurrHeight),
    {ok, CreateTx, []}; %% no updates
new_onchain_tx(channel_settle_tx, Opts,
               #data{ opts  = #{initiator := I,
                                responder := R}
                    , state = State } = D, CurrHeight) ->
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
    {ok, Tx} = new_onchain_tx_(aesc_settle_tx, SlashOpts, CurrHeight),
    {ok, Tx, []};
new_onchain_tx(channel_close_solo_tx, Opts,
               #data{ on_chain_id = ChanId
                    , opts = #{initiator := Initiator,
                               responder := Responder}
                    , state       = State } = D, CurrHeight) ->
    Account = my_account(D),
    TTL = adjust_ttl(maps:get(ttl, Opts, 0)),
    {_Round, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    Payload = case aesc_utils:is_offchain_tx(SignedTx) of
                  true ->
                      aetx_sign:serialize_to_binary(SignedTx);
                  false ->
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
    {ok, Tx} = new_onchain_tx_(aesc_close_solo_tx, Opts1, CurrHeight),
    {ok, Tx, []};
new_onchain_tx(channel_slash_tx, Opts,
               #data{ on_chain_id = ChanId
                    , opts = #{initiator := Initiator,
                               responder := Responder}
                    , state       = State} = D, CurrHeight) ->
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
              , ttl        => TTL},
    {ok, Tx} = new_onchain_tx_(aesc_slash_tx, Opts2, CurrHeight),
    {ok, Tx, []}.

new_onchain_tx_(Mod, Opts, CurrHeight) when Mod =:= aesc_create_tx;
                                            Mod =:= aesc_withdraw_tx; 
                                            Mod =:= aesc_deposit_tx;
                                            Mod =:= aesc_close_solo_tx;
                                            Mod =:= aesc_slash_tx;
                                            Mod =:= aesc_settle_tx ->
    case maps:is_key(fee, Opts) of
        true -> %% use preset fee
            apply(Mod, new, [Opts]);
        false ->
            create_with_minimum_fee(Mod, Opts#{fee => 0}, CurrHeight)
    end.

%% A valid transaction fee is a function on gas required and gas price used
%% the following function uses the gas price the node would be using if it
%% were mining
%% gas required is a funcion of:
%% * transaction type (base_gas)
%% * transaction size (size_gas)
%% * gas needed for contract execution in a channel_force_progress
%% Since the fee is part of the serialization of the transaction,
%% modifying the fee, might change the serialized transaction size and thus
%% changing the size_gas required for the transaction. Increasing the number
%% for the fee might result in the transaction requiring even more fee.
%% That's why we make 5 attempts for computing the minimal fee
create_with_minimum_fee(Mod, Opts, CurrHeight) ->
    create_with_minimum_fee(Mod, Opts, CurrHeight, 5).

create_with_minimum_fee(_, _, _, Attempts) when Attempts < 1 ->
    erlang:error(could_not_compute_fee);
create_with_minimum_fee(Mod, Opts, CurrHeight, Attempts) ->
    {ok, Tx} = apply(Mod, new, [Opts]),
    MinTxFee = aetx:min_fee(Tx, CurrHeight),
    MinGas = aetx:min_gas(Tx, CurrHeight),
    MinMinerGasPrice = aec_tx_pool:minimum_miner_gas_price(),
    MinGasRequirements = MinGas * MinMinerGasPrice,
    MinFee = max(MinTxFee, MinGasRequirements),
    TxFee = aetx:fee(Tx),
    case MinFee =< TxFee of
        true ->
            {ok, Tx};
        false ->
            create_with_minimum_fee(Mod, Opts#{fee => MinFee}, CurrHeight,
                                     Attempts - 1)
    end.

create_tx_for_signing(#data{opts = #{ initiator := Initiator
                                    , initiator_amount := IAmt
                                    , responder_amount := RAmt
                                    , channel_reserve  := ChannelReserve
                                    , lock_period      := LockPeriod }} = D) ->
    new_onchain_tx_for_signing(channel_create_tx, #{ acct => Initiator
                                                   , initiator_amount => IAmt
                                                   , responder_amount => RAmt
                                                   , channel_reserve  => ChannelReserve
                                                   , lock_period      => LockPeriod }, D).

dep_tx_for_signing(Opts, D) ->
    new_onchain_tx_for_signing(channel_deposit_tx, Opts, D).

wdraw_tx_for_signing(Opts, D) ->
    new_onchain_tx_for_signing(channel_withdraw_tx, Opts, D).

close_mutual_tx_for_signing(D) ->
    Account = my_account(D),
    new_close_mutual_tx(#{ acct => Account }, D).

%% The responding side creates a 'throwaway' close_mutual_tx, in order to
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
    new_onchain_tx_for_signing(channel_close_mutual_tx, Opts, D).


slash_tx_for_signing(#data{ state = St } = D) ->
    {Round, SignedTx} = aesc_offchain_state:get_latest_signed_tx(St),
    slash_tx_for_signing(Round, SignedTx, D).

slash_tx_for_signing(_Round, SignedTx, D) ->
    Account = my_account(D),
    new_onchain_tx_for_signing(channel_slash_tx,
                               #{acct => Account
                               , payload    => aetx_sign:serialize_to_binary(SignedTx)
                               }, D).

settle_tx_for_signing(D) ->
    Account = my_account(D),
    new_onchain_tx_for_signing(channel_settle_tx, #{acct => Account}, D).

close_solo_tx_for_signing(D) ->
    Account = my_account(D),
    new_onchain_tx_for_signing(channel_close_solo_tx, #{acct => Account}, D).

tx_defaults(Type, Opts, #data{ on_chain_id = ChanId } = D) ->
    Default = tx_defaults_(Type, Opts, D),
    Default#{ channel_id => ChanId
            , nonce => default_nonce(Opts, D) }.

tx_defaults_(channel_create_tx, _Opts, _D) ->
    #{};
tx_defaults_(channel_deposit_tx = Tx, Opts, D) ->
    Opts1 = tx_defaults_(channel_create_tx, Opts, D),
    default_ttl(Tx, Opts1, D);
tx_defaults_(channel_withdraw_tx, Opts, D) ->
    tx_defaults_(channel_deposit_tx, Opts, D); %% same as deposit defaults
tx_defaults_(channel_slash_tx = Tx, Opts, D) ->
    default_ttl(Tx, Opts, D);
tx_defaults_(channel_close_solo_tx = Tx, Opts, D) ->
    default_ttl(Tx, Opts, D);
tx_defaults_(channel_settle_tx = Tx, Opts, D) ->
    default_ttl(Tx, Opts, D);
tx_defaults_(channel_close_mutual_tx, _Opts, _D) ->
    #{fee => default_fee(channel_close_mutual_tx)}.

default_nonce(Opts, #data{opts = DOpts}) ->
    try maps:get(nonce, Opts, maps:get(nonce, DOpts))
    catch
        error:_ ->
            Pubkey = maps:get(acct, Opts),
            get_nonce(Pubkey)
    end.

get_nonce(Pubkey) ->
    {value, Account} = aec_chain:get_account(Pubkey),
    case aec_accounts:type(Account) of
        basic -> ok(aec_next_nonce:pick_for_account(Pubkey));
        generalized -> 0
    end.

%% Note that the default fee will be used as a base for adjustment, once
%% we have an actual transaction record (required by aetx:min_fee/2).
%% The default should err on the side of being too low.
default_fee(_Tx) ->
    CurrHeight = aec_headers:height(aec_chain:top_header()),
    %% this could be fragile on hard fork height if one participant's node had
    %% already forked and the other had not yet
    ?DEFAULT_FSM_TX_GAS * max(aec_governance:minimum_gas_price(CurrHeight),
                              aec_tx_pool:minimum_miner_gas_price()).

default_ttl(_Type, Opts, #data{opts = DOpts}) ->
    TTL = maps:get(ttl, Opts, maps:get(ttl, DOpts, 0)),
    Opts#{ttl => adjust_ttl(TTL)}.

adjust_ttl(undefined) ->
    CurrHeight = aec_headers:height(aec_chain:top_header()),
    CurrHeight + ?DEFAULT_FSM_TX_TTL_DELTA;
adjust_ttl(TTL) when is_integer(TTL), TTL >= 0 ->
    TTL.

curr_height() ->
    aec_headers:height(aec_chain:top_header()).


new_contract_tx_for_signing(Opts, From, #data{state = State,
                                              opts = ChannelOpts,
                                              on_chain_id = ChannelId
                                             } = D) ->
    #{owner       := Owner,
      vm_version  := VmVersion,
      abi_version := ABIVersion,
      code        := Code,
      deposit     := Deposit,
      call_data   := CallData} = Opts,
    Updates = [aesc_offchain_update:op_new_contract(aeser_id:create(account, Owner),
                                                    VmVersion, ABIVersion, Code, Deposit, CallData)],
    {OnChainEnv, OnChainTrees} =
        aetx_env:tx_env_and_trees_from_top(aetx_contract),
    Height = aetx_env:height(OnChainEnv),
    ActiveProtocol = aec_hard_forks:protocol_effective_at_height(Height),
    try  Tx1 = aesc_offchain_state:make_update_tx(Updates, State, ChannelId,
                                                  ActiveProtocol,
                                                  OnChainTrees, OnChainEnv, ChannelOpts),
         D1 = request_signing(?UPDATE, Tx1, Updates, D),
         gen_statem:reply(From, ok),
         next_state(awaiting_signature, set_ongoing(D1))
    catch
        error:Reason ->
            process_update_error(Reason, From, D)
    end.

pay_close_mutual_fee(Fee, IAmt, RAmt) ->
    Ceil  = trunc(math:ceil(Fee/2)),
    Floor = trunc(math:floor(Fee/2)),
    if (IAmt + RAmt) < Fee                    -> erlang:error(insufficient_funds);
       (IAmt >= Ceil) andalso (RAmt >= Floor) -> {IAmt - Ceil, RAmt - Floor};
       (RAmt >= Ceil) andalso (IAmt >= Floor) -> {IAmt - Floor, RAmt - Ceil};
       (IAmt > RAmt)                          -> {IAmt - Fee + RAmt, 0};
       true                                   -> {0, RAmt - Fee + IAmt}
    end.

my_account(#data{role = initiator, opts = #{initiator := I}}) -> I;
my_account(#data{role = responder, opts = #{responder := R}}) -> R.

other_account(#data{role = initiator, opts = #{responder := R}}) -> R;
other_account(#data{role = responder, opts = #{initiator := I}}) -> I.

both_accounts(Data) ->
    [other_account(Data),
     my_account(Data)].

send_funding_created_msg(SignedTx, #data{channel_id = Ch,
                                         session    = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ temporary_channel_id => Ch
           , block_hash           => ?NOT_SET_BLOCK_HASH
           , data                 => #{tx      => TxBin,
                                       updates => []}},
    aesc_session_noise:funding_created(Sn, Msg),
    log(snd, ?FND_CREATED, Msg, Data).

check_funding_created_msg(#{ temporary_channel_id := ChanId
                           , block_hash           := ?NOT_SET_BLOCK_HASH
                           , data                 := #{tx      := TxBin,
                                                       updates := UpdatesBin}} = Msg,
                          #data{ state = State, opts = Opts,
                                 channel_id = ChanId } = Data) ->
    Updates = [aesc_offchain_update:deserialize(U) || U <- UpdatesBin],
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    case verify_signatures_channel_create(SignedTx, initiator) of
        ok ->
            case check_update_tx_initial(SignedTx, Updates, State, Opts) of
                ok ->
                    {ok, SignedTx, Updates, log(rcv, ?FND_CREATED, Msg, Data)};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Err ->
            Err
    end.

send_funding_signed_msg(SignedTx, #data{channel_id = Ch,
                                        session    = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ temporary_channel_id  => Ch
           , block_hash            => ?NOT_SET_BLOCK_HASH
           , data                  => #{tx => TxBin}},
    aesc_session_noise:funding_signed(Sn, Msg),
    log(snd, ?FND_CREATED, Msg, Data).

check_funding_signed_msg(#{ temporary_channel_id := ChanId
                          , block_hash           := ?NOT_SET_BLOCK_HASH
                          , data                 := #{tx := TxBin}} = Msg,
                          #data{ channel_id = ChanId } = Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    case verify_signatures_channel_create(SignedTx, both) of
        ok ->
            {ok, SignedTx, log(rcv, ?FND_SIGNED, Msg, Data)};
        {error, _} = Err ->
            Err
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
                         #data{on_chain_id = Ch, session     = Sn} = Data) ->
    UBins = [aesc_offchain_update:serialize(U) || U <- Updates],
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id => Ch
           , block_hash => ?NOT_SET_BLOCK_HASH
           , data       => #{tx => TxBin,
                             updates => UBins}},
    aesc_session_noise:deposit_created(Sn, Msg),
    log(snd, ?DEP_CREATED, Msg, Data).

check_deposit_created_msg(#{ channel_id := ChanId
                           , block_hash := ?NOT_SET_BLOCK_HASH
                           , data       := #{tx      := TxBin,
                                             updates := UpdatesBin}} = Msg,
                          #data{on_chain_id = ChanId} = Data) ->
    Updates = [aesc_offchain_update:deserialize(U) || U <- UpdatesBin],
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    case check_tx_and_verify_signatures(SignedTx, Updates, aesc_deposit_tx,
                                        Data,
                                        [other_account(Data)],
                                        not_deposit_tx) of
        ok ->
            {ok, SignedTx, Updates, log(rcv, ?DEP_CREATED, Msg, Data)};
        {error, _} = Err -> Err
    end.

send_deposit_signed_msg(SignedTx, #data{on_chain_id = Ch,
                                        session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id  => Ch
           , block_hash  => ?NOT_SET_BLOCK_HASH
           , data        => #{tx => TxBin}},
    aesc_session_noise:deposit_signed(Sn, Msg),
    log(snd, ?DEP_SIGNED, Msg, Data).

check_deposit_signed_msg(#{ channel_id := ChanId
                          , block_hash := ?NOT_SET_BLOCK_HASH
                          , data       := #{tx := TxBin}} = Msg,
                          #data{on_chain_id = ChanId,
                                latest = {ack, deposit_tx, _, Updates}} = Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    case check_tx_and_verify_signatures(SignedTx, Updates, aesc_deposit_tx,
                                        Data,
                                        both_accounts(Data),
                                        not_deposit_tx) of
        ok ->
            {ok, SignedTx, log(rcv, ?DEP_SIGNED, Msg, Data)};
        {error, _} = Err -> Err
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
                         #data{on_chain_id = MyChanId} = Data) ->
    case ChanId == MyChanId of
        true ->
            case aetx_sign:hash(SignedTx) of
                TxHash ->
                    {ok, log(rcv, ?DEP_LOCKED, Msg, Data)};
                _ ->
                    {error, deposit_tx_hash_mismatch}
            end;
        false ->
            {error, channel_id_mismatch}
    end.

check_deposit_error_msg(Msg, D) ->
    check_op_error_msg(?DEP_ERR, Msg, D).

check_withdraw_error_msg(Msg, D) ->
    check_op_error_msg(?WDRAW_ERR, Msg, D).

check_update_err_msg(Msg, D) ->
    check_op_error_msg(?UPDATE_ERR, Msg, D).

check_op_error_msg(Op, #{ channel_id := ChanId }, D) when ChanId =/= D#data.on_chain_id ->
    {error, chain_id_mismatch};
check_op_error_msg(Op, Msg, D) ->
    #{ channel_id := ChanId , error_code := ErrCode } = Msg,
    #data{ on_chain_id = ChanId0 , state = State, log = Log } = D,
    {LastRound, _} = aesc_offchain_state:get_latest_signed_tx(State),
    case aesc_offchain_state:get_fallback_state(State) of
        {LastRound, State1} -> %% same round
            Log1 = log_msg(rcv, Op, Msg, Log),
            {ok, ErrCode, D#data{state = State1, log = Log1}};
        _Other ->
            lager:debug("Fallback state mismatch: ~p/~p",
                        [LastRound, _Other]),
            {error, fallback_state_mismatch}
    end.

send_withdraw_created_msg(SignedTx, Updates,
                          #data{on_chain_id = Ch, session = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    UBins = [aesc_offchain_update:serialize(U) || U <- Updates],
    Msg = #{ channel_id => Ch
           , block_hash => ?NOT_SET_BLOCK_HASH
           , data       => #{tx      => TxBin,
                             updates => UBins}},
    aesc_session_noise:wdraw_created(Sn, Msg),
    log(snd, ?WDRAW_CREATED, Msg, Data).

check_withdraw_created_msg(#{ channel_id := ChanId
                            , block_hash := ?NOT_SET_BLOCK_HASH
                            , data       := #{tx      := TxBin, 
                                              updates := UpdatesBin}} = Msg,
                  #data{ on_chain_id = ChanId } = Data) ->
    Updates = [aesc_offchain_update:deserialize(U) || U <- UpdatesBin],
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    case check_tx_and_verify_signatures(SignedTx, Updates, aesc_withdraw_tx,
                                        Data,
                                        pubkeys(other_participant, Data),
                                        not_withdraw_tx) of
        ok ->
            {ok, SignedTx, Updates, log(rcv, ?WDRAW_CREATED, Msg, Data)};
        {error, _} = Err -> Err
    end.

send_withdraw_signed_msg(SignedTx, #data{on_chain_id = Ch,
                                         session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id  => Ch
           , block_hash => ?NOT_SET_BLOCK_HASH
           , data        => #{tx => TxBin}},
    aesc_session_noise:wdraw_signed(Sn, Msg),
    log(snd, ?WDRAW_SIGNED, Msg, Data).

check_withdraw_signed_msg(#{ channel_id := ChanId
                           , block_hash := ?NOT_SET_BLOCK_HASH
                           , data       := #{tx := TxBin}} = Msg,
                          #data{on_chain_id = ChanId,
                                latest = Latest} = Data) ->
    {ack, withdraw_tx, _, Updates} = Latest,
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    case check_tx_and_verify_signatures(SignedTx, Updates, aesc_withdraw_tx,
                                        Data,
                                        pubkeys(both, Data),
                                        not_withdraw_tx) of
        ok ->
            {ok, SignedTx, log(rcv, ?WDRAW_SIGNED, Msg, Data)};
        {error, _} = Err -> Err
    end.

send_withdraw_locked_msg(TxHash, #data{on_chain_id = ChanId,
                                       session     = Sn} = Data) ->
    Msg = #{ channel_id => ChanId
           , data       => #{tx_hash => TxHash} },
    aesc_session_noise:wdraw_locked(Sn, Msg),
    log(snd, ?WDRAW_LOCKED, Msg, Data).

check_withdraw_locked_msg(#{ channel_id := ChanId
                           , data       := #{tx_hash := TxHash} } = Msg,
                          SignedTx,
                          #data{on_chain_id = MyChanId} = Data) ->
    case ChanId == MyChanId of
        true ->
            case aetx_sign:hash(SignedTx) of
                TxHash ->
                    {ok, log(rcv, ?WDRAW_LOCKED, Msg, Data)};
                _ ->
                    {error, withdraw_tx_hash_mismatch}
            end;
        false ->
            {error, channel_id_mismatch}
    end.

send_update_msg(SignedTx, Updates,
                #data{ on_chain_id = OnChainId, session = Sn} = Data) ->
    UBins = [aesc_offchain_update:serialize(U) || U <- Updates],
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id => OnChainId
           , block_hash => ?NOT_SET_BLOCK_HASH
           , data       => #{ tx      => TxBin
                            , updates => UBins }},
    aesc_session_noise:update(Sn, Msg),
    log(snd, ?UPDATE, Msg, Data).

check_update_msg(Type, Msg, D) ->
    lager:debug("check_update_msg(~p)", [Msg]),
    try check_update_msg_(Type, Msg, D)
    catch
        error:E ->
            ?LOG_CAUGHT(E),
            {error, E}
    end.

check_update_msg_(Type, #{ channel_id := ChanId
                         , block_hash := ?NOT_SET_BLOCK_HASH
                         , data       := #{ tx      := TxBin
                                          , updates := UpdatesBin }} = Msg,
                  #data{ on_chain_id = ChanId } = D) ->
    Updates = [aesc_offchain_update:deserialize(U) || U <- UpdatesBin],
    try aetx_sign:deserialize_from_binary(TxBin) of
        SignedTx ->
            case check_signed_update_tx(Type, SignedTx, Updates, D) of
                ok ->
                    {ok, SignedTx, Updates, log(rcv, ?UPDATE, Msg, D)};
                {error, _} = Err ->
                    Err
            end
    catch
        error:E ->
            ?LOG_CAUGHT(E),
            {error, {deserialize, E}}
    end.

check_signed_update_tx(Type, SignedTx, Updates,
                       #data{state = State, opts = Opts,
                             on_chain_id = ChannelPubkey} = D) ->
    lager:debug("check_signed_update_tx(~p)", [SignedTx]),
    case check_tx_and_verify_signatures(SignedTx, Updates, aesc_offchain_tx,
                                        D,
                                        pubkeys(other_participant, D), not_offchain_tx) of
        ok ->
            Res =
                case Type of
                    normal ->
                        check_update_tx(SignedTx, Updates, State, Opts,
                                        ChannelPubkey);
                    initial ->
                        check_update_tx_initial(SignedTx, Updates, State,
                                                Opts)
                end,
            case Res of
                ok -> ok;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Err -> Err
    end.

check_update_tx_initial(SignedTx, Updates, State, Opts) ->
    {OnChainEnv, OnChainTrees} =
        aetx_env:tx_env_and_trees_from_top(aetx_contract),
    aesc_offchain_state:check_initial_update_tx(SignedTx, Updates, State,
                                                OnChainTrees, OnChainEnv,
                                                Opts).

check_update_tx(SignedTx, Updates, State, Opts, ChannelPubkey) ->
    {OnChainEnv, OnChainTrees} =
        aetx_env:tx_env_and_trees_from_top(aetx_contract),
    Height = aetx_env:height(OnChainEnv),
    ActiveProtocol = aec_hard_forks:protocol_effective_at_height(Height),
    aesc_offchain_state:check_update_tx(SignedTx, Updates, State,
                                        ChannelPubkey,
                                        ActiveProtocol,
                                        OnChainTrees, OnChainEnv, Opts).

check_update_ack_msg(Msg, D) ->
    lager:debug("check_update_ack_msg(~p)", [Msg]),
    try check_update_ack_msg_(Msg, D)
    catch
        error:E ->
            ?LOG_CAUGHT(E),
            {error, E}
    end.

check_update_ack_msg_(#{ channel_id := ChanId
                       , data       := #{tx := TxBin} } = Msg,
                      #data{on_chain_id = ChanId} = D) ->
    try aetx_sign:deserialize_from_binary(TxBin) of
        SignedTx ->
            check_signed_update_ack_tx(SignedTx, Msg, D)
    catch
        error:E ->
            ?LOG_CAUGHT(E),
            {error, {deserialize, E}}
    end.

check_signed_update_ack_tx(SignedTx, Msg,
                           #data{state = State, opts = Opts,
                                latest = Latest} = D) ->
    {ack, ?UPDATE, _, Updates} = Latest,
    HalfSignedTx = aesc_offchain_state:get_latest_half_signed_tx(State),
    try  ok = check_update_ack_(SignedTx, HalfSignedTx),
         case check_tx_and_verify_signatures(SignedTx, Updates, aesc_offchain_tx,
                                             D,
                                             pubkeys(both, D), not_offchain_tx) of
              ok ->
                  {OnChainEnv, OnChainTrees} =
                      aetx_env:tx_env_and_trees_from_top(aetx_contract),
                  {ok, D#data{state = aesc_offchain_state:set_signed_tx(
                                        SignedTx, Updates, State, OnChainTrees, OnChainEnv, Opts),
                              log = log_msg(rcv, ?UPDATE_ACK, Msg, D#data.log)}};
              {error, _} = Err -> Err
          end
    catch
        error:E ->
            ?LOG_CAUGHT(E),
            {error, invalid_update_ack}
    end.

check_update_ack_(SignedTx, HalfSignedTx) ->
    HalfSigs = aetx_sign:signatures(HalfSignedTx),
    lager:debug("HalfSigs = ~p", [HalfSigs]),
    Sigs = aetx_sign:signatures(SignedTx),
    lager:debug("Sigs = ~p", [Sigs]),
    Remainder = Sigs -- HalfSigs,
    lager:debug("Remainder = ~p", [Remainder]),
    true = (aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx))
         == aetx:specialize_callback(aetx_sign:innermost_tx(HalfSignedTx))),
    lager:debug("Txes are the same", []),
    ok.

handle_upd_transfer(FromPub, ToPub, Amount, From, #data{ state = State
                                                       , opts = Opts
                                                       , on_chain_id = ChannelId
                                                       } = D) ->
    Updates = [aesc_offchain_update:op_transfer(aeser_id:create(account, FromPub),
                                                aeser_id:create(account, ToPub), Amount)],
    {OnChainEnv, OnChainTrees} =
        aetx_env:tx_env_and_trees_from_top(aetx_contract),
    Height = aetx_env:height(OnChainEnv),
    ActiveProtocol = aec_hard_forks:protocol_effective_at_height(Height),
    try  Tx1 = aesc_offchain_state:make_update_tx(Updates, State,
                                                  ChannelId,
                                                  ActiveProtocol,
                                                  OnChainTrees, OnChainEnv, Opts),
         D1 = request_signing(?UPDATE, Tx1, Updates, D),
         gen_statem:reply(From, ok),
         next_state(awaiting_signature, D1)
    catch
        error:Reason ->
            process_update_error(Reason, From, D)
    end.

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
    aesc_session_noise:leave(Session, Msg),
    log(snd, ?LEAVE_ACK, Msg, Data).

check_leave_ack_msg(#{channel_id := ChId} = Msg,
                    #data{on_chain_id = ChId0} = Data) ->
    case ChId == ChId0 of
        true ->
            {ok, log(rcv, ?LEAVE_ACK, Msg, Data)};
        false ->
            {error, channel_id_mismatch}
    end.

send_shutdown_msg(SignedTx, #data{session = Session} = Data) ->
    Msg = shutdown_msg(SignedTx, Data),
    aesc_session_noise:shutdown(Session, Msg),
    log(snd, ?SHUTDOWN, Msg, Data).

send_shutdown_ack_msg(SignedTx, #data{session = Session} = Data) ->
    Msg = shutdown_msg(SignedTx, Data),
    aesc_session_noise:shutdown_ack(Session, Msg),
    log(snd, ?SHUTDOWN_ACK, Msg, Data).

shutdown_msg(SignedTx, #data{ on_chain_id = OnChainId }) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    #{ channel_id => OnChainId
     , block_hash => ?NOT_SET_BLOCK_HASH
     , data       => #{tx => TxBin} }.

check_shutdown_msg(#{channel_id := ChanId,
                     block_hash := ?NOT_SET_BLOCK_HASH,
                     data := #{tx := TxBin}} = Msg,
                   #data{on_chain_id = ChanId} = D) ->
    Updates = [],
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    case check_tx_and_verify_signatures(SignedTx, Updates, aesc_close_mutual_tx,
                                        D,
                                        [other_account(D)],
                                        not_close_mutual_tx) of
        ok ->
            {aesc_close_mutual_tx, RealTxI} =
                aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
            {ok, FakeCloseTx, []} = fake_close_mutual_tx(aesc_close_mutual_tx,
                                                         RealTxI, D),
            {channel_close_mutual_tx, FakeTxI} = aetx:specialize_type(FakeCloseTx),
            case (serialize_close_mutual_tx(FakeTxI) =:=
                      serialize_close_mutual_tx(RealTxI)) of
                true ->
                    {ok, SignedTx, Updates, log(rcv, ?SHUTDOWN, Msg, D)};
                false ->
                    {error, shutdown_tx_validation}
            end;
        {error, _} = Err ->
            Err
    end.

serialize_close_mutual_tx(Tx) ->
    {_, Elems} = aesc_close_mutual_tx:serialize(Tx),
    lists:keydelete(nonce, 1, Elems).

check_shutdown_ack_msg(#{data       := #{tx := TxBin},
                         block_hash := ?NOT_SET_BLOCK_HASH} = Msg,
                       #data{latest = {shutdown, MySignedTx, _Updates}} = D) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    case check_tx_and_verify_signatures(SignedTx, [], aesc_close_mutual_tx,
                                        D,
                                        both_accounts(D),
                                        not_close_mutual_tx) of
        ok ->
            check_shutdown_msg_(SignedTx, MySignedTx, Msg, D);
        {error, _} = Err ->
            Err
    end.

check_shutdown_msg_(SignedTx, MySignedTx, Msg, D) ->
    %% TODO: More thorough checking
    case (aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx))
      =:= aetx:specialize_callback(aetx_sign:innermost_tx(MySignedTx))) of
        true ->
            {ok, SignedTx, log(rcv, ?SHUTDOWN_ACK, Msg, D)};
        false ->
            {error, shutdown_tx_mismatch}
    end.

send_inband_msg(To, Info, #data{session     = Session} = D) ->
    ChanId = cur_channel_id(D),
    From = my_account(D),
    M = #{ channel_id => ChanId
         , from       => From
         , to         => To
         , info       => Info },
    aesc_session_noise:inband_msg(Session, M),
    log(snd, ?INBAND_MSG, M, D).

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

fallback_to_stable_state(#data{state = State} = D) ->
    D#data{state = aesc_offchain_state:fallback_to_stable_state(State)}.

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
                                   , session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id => OnChainId
           , data       => #{tx => TxBin} },
    aesc_session_noise:update_ack(Sn, Msg),
    log(snd, ?UPDATE_ACK, Msg, Data).

handle_update_conflict(Req, D) ->
    lager:debug("handle_update_conflict(~p, D)", [Req]),
    try
        D1 = send_conflict_err_msg(Req, fallback_to_stable_state(D)),
        lager:debug("conflict_err_msg sent", []),
        next_state(open, D1)
    catch
        error:Err ->
            ?LOG_CAUGHT(Err),
            erlang:error(Err)
    end.

send_conflict_err_msg(Req, #data{ state = State
                                , on_chain_id = ChanId
                                , session     = Sn } = Data) ->
    {_, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    Round = tx_round(aetx_sign:innermost_tx(SignedTx)),
    Msg = #{ channel_id => ChanId
           , round      => Round
           , error_code => ?ERR_CONFLICT },
    Type = conflict_msg_type(Req),
    send_conflict_msg(Type, Sn, Msg),
    report(conflict, Msg, Data),
    log(snd, conflict_msg_type(Req), Msg, Data).

conflict_msg_type(?UPDATE)        -> ?UPDATE_ERR;
conflict_msg_type(?UPDATE_ACK)    -> ?UPDATE_ERR;
conflict_msg_type(?DEP_CREATED)   -> ?DEP_ERR;
conflict_msg_type(?DEP_SIGNED)    -> ?DEP_ERR;
conflict_msg_type(deposit_tx)     -> ?DEP_ERR;
conflict_msg_type(?WDRAW_CREATED) -> ?WDRAW_ERR;
conflict_msg_type(?WDRAW_SIGNED)  -> ?WDRAW_ERR;
conflict_msg_type(withdraw_tx)    -> ?WDRAW_ERR.

send_conflict_msg(?UPDATE_ERR, Sn, Msg) ->
    lager:debug("send update_error: ~p", [Msg]),
    aesc_session_noise:update_error(Sn, Msg);
send_conflict_msg(?DEP_ERR, Sn, Msg) ->
    lager:debug("send deposit_error: ~p", [Msg]),
    aesc_session_noise:dep_error(Sn, Msg);
send_conflict_msg(?WDRAW_ERR, Sn, Msg) ->
    lager:debug("send withdraw_error: ~p", [Msg]),
    aesc_session_noise:wdraw_error(Sn,Msg).

request_signing(Tag, Aetx, Updates, #data{} = D) ->
    request_signing_(Tag, aetx_sign:new(Aetx, []), Updates, D).

-spec request_signing_(sign_tag(),
                       aetx_sign:signed_tx(),
                       [aesc_offchain_update:update()],
                       #data{}) -> #data{}.
request_signing_(Tag, SignedTx, Updates, #data{client = Client} = D) ->
    Info = #{signed_tx => SignedTx,
             updates => Updates},
    Msg = rpt_message(#{ type => sign
                       , tag  => Tag
                       , info => Info }, D),
    Client ! {?MODULE, self(), Msg},
    lager:debug("signing(~p) requested", [Tag]),
    D#data{ latest = {sign, Tag, SignedTx, Updates}
          , log    = log_msg(req, sign, Msg, D#data.log)}.

start_min_depth_watcher(Type, SignedTx, Updates,
                        #data{watcher = Watcher0,
                              opts = #{minimum_depth := MinDepth}} = D) ->
    {Mod, Tx} = aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    TxHash = aetx_sign:hash(SignedTx),
    evt({tx_hash, TxHash}),
    Nonce = Mod:nonce(Tx),
    evt({nonce, Nonce}),
    {OnChainId, D1} = on_chain_id(D, SignedTx),
    case {Type, Watcher0} of
        {{?MIN_DEPTH, ?WATCH_FND = Sub}, undefined} ->
            {ok, Watcher1} = aesc_fsm_min_depth_watcher:start_link(
                               Sub, TxHash, OnChainId, MinDepth, ?MODULE),
            evt({watcher, Watcher1}),
            {ok, D1#data{watcher = Watcher1,
                         latest = {?MIN_DEPTH, Sub, TxHash, SignedTx, Updates}}};
        {{?MIN_DEPTH, Sub}, Pid} when is_pid(Pid) ->
            ok = aesc_fsm_min_depth_watcher:watch_for_min_depth(
                   Pid, TxHash, MinDepth, ?MODULE, Sub),
            {ok, D1#data{latest = {?MIN_DEPTH, Sub, TxHash, SignedTx, Updates}}};
        {unlock, Pid} when Pid =/= undefined ->
            ok = aesc_fsm_min_depth_watcher:watch_for_unlock(Pid, ?MODULE),
            {ok, D1#data{latest = {watch, unlock, TxHash, SignedTx, Updates}}};
        {close, Pid} when Pid =/= undefined ->
            ok = aesc_fsm_min_depth_watcher:watch_for_channel_close(Pid, MinDepth, ?MODULE),
            {ok, D1#data{latest = {watch, close, TxHash, SignedTx, Updates}}};
        {_, Pid} when Pid =/= undefined ->  %% assertion
            lager:debug("Unknown Type = ~p, Pid = ~p", [Type, Pid]),
            ok = aesc_fsm_min_depth_watcher:watch(
                   Pid, Type, TxHash, MinDepth, ?MODULE),
            {ok, D1#data{latest = {watch, Type, TxHash, SignedTx, Updates}}}
    end.



on_chain_id(#data{on_chain_id = ID} = D, _) when ID =/= undefined ->
    {ID, D};
on_chain_id(D, SignedTx) ->
    {ok, PubKey} = aesc_utils:channel_pubkey(SignedTx),
    {PubKey, D#data{on_chain_id = PubKey}}.

initialize_cache(#data{ on_chain_id = ChId
                      , state       = State } = D) ->
    aesc_state_cache:new(ChId, my_account(D), State).

cache_state(#data{ on_chain_id = ChId
                 , state       = State } = D) ->
    aesc_state_cache:update(ChId, my_account(D), State).

-spec handle_change_config(_Key::atom(), _Value::any(), #data{}) ->
                                  {ok, _Reply::any(), #data{}}
                                | {error, Reason::atom()}.

handle_change_config(log_keep, Keep, #data{log = L} = D)
  when is_integer(Keep), Keep >= 0 ->
    {ok, ok, D#data{log = aesc_window:change_keep(Keep, L)}};
handle_change_config(_, _, _) ->
    {error, invalid_config}.


report_update(#data{state = State, last_reported_update = Last} = D) ->
    case aesc_offchain_state:get_latest_signed_tx(State) of
        {Last, _} ->
            D;
        {New, SignedTx} ->
            report(update, SignedTx, D),
            cache_state(D),
            D#data{last_reported_update = New}
    end.

report_leave(#data{state = State} = D) ->
    {_, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    report(leave, SignedTx, D),
    cache_state(D).

report_on_chain_tx(Info, SignedTx, D) ->
    {Type,_} = aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
    report(on_chain_tx, #{ tx => SignedTx
                         , type => Type
                         , info => Info}, D).

report(Tag, Info, D) ->
    report_info(do_rpt(Tag, D), #{ type => report
                                 , tag  => Tag
                                 , info => Info }, D).

report(Tag, St, Msg, D) ->
    report_info(do_rpt(Tag, D), #{ type => report
                                 , tag  => Tag
                                 , info => St
                                 , data => Msg }, D).

report_info(DoRpt, Msg0, #data{client = Client} = D) ->
    if DoRpt ->
            Msg = rpt_message(Msg0, D),
            lager:debug("report_info(true, Client = ~p, Msg = ~p)", [Client, Msg]),
            Client ! {?MODULE, self(), Msg};
       true  ->
            lager:debug("report_info(~p, ~p)", [DoRpt, Msg0]),
            ok
    end,
    ok.

rpt_message(Msg, #data{ on_chain_id = undefined }) ->
    Msg;
rpt_message(Msg, #data{on_chain_id = OnChainId}) ->
    Msg#{ channel_id => OnChainId }.

do_rpt(Tag, #data{opts = #{report := Rpt}}) ->
    try maps:get(Tag, Rpt, false)
    catch
        error:_ ->
            false
    end.

log(Op, Type, M, #data{log = Log0} = D) ->
    D#data{log = log_msg(Op, Type, M, Log0)}.

log_msg(Op, Type, M, Log) ->
    aesc_window:add({Op, Type, os:timestamp(), M}, Log).

win_to_list(Log) ->
    aesc_window:to_list(Log).

check_amounts(#{ channel_reserve := ChannelReserve})
    when ChannelReserve < 0 ->
    {error, channel_reserve_too_low};
check_amounts(#{ push_amount := PushAmount})
    when PushAmount < 0 ->
    {error, push_amount_too_low};
check_amounts(#{ initiator_amount   := InitiatorAmount0
               , responder_amount   := ResponderAmount0
               , push_amount        := PushAmount
               , channel_reserve    := ChannelReserve}) ->
    InitiatorAmount = InitiatorAmount0 - PushAmount,
    ResponderAmount = ResponderAmount0 + PushAmount,
    case {InitiatorAmount >= ChannelReserve,
          ResponderAmount >= ChannelReserve} of
        {true,  true}  -> ok;
        {false, true}  -> {error, insufficient_initiator_amount};
        {true,  false} -> {error, insufficient_responder_amount};
        {false, false} -> {error, insufficient_amounts}
    end.

process_update_error({off_chain_update_error, Reason}, From, D) ->
    ?LOG_CAUGHT(Reason),
    keep_state(D, [{reply, From, {error, Reason}}]);
process_update_error(Reason, From, D) ->
    ?LOG_CAUGHT(Reason),
    keep_state(D, [{reply, From, {error, Reason}}]).

check_closing_event(Info, D) ->
    aec_db:dirty(fun() ->
                         try check_closing_event_(Info, D)
                         catch
                             error:E ->
                                 ?LOG_CAUGHT(E),
                                 error(E)
                         end
                 end).

check_closing_event_(#{ tx := SignedTx
                      , channel := Ch
                      , block_hash := BlockHash}, #data{state = St}) ->
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
            {ok, Hdr} = aec_chain:get_header(BlockHash),
            Height = aec_headers:height(Hdr),
            case aesc_channels:is_solo_closed(Ch, Height) of
                true ->
                    {ok, solo_closed, SignedTx};
                false ->
                    {error, not_solo_closing}
            end
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
        _ -> {error, not_create_tx}
    end.

verify_signatures_onchain_check(Pubkeys, SignedTx) ->
    {Mod, Tx} = aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    {OnChainEnv, OnChainTrees} =
        aetx_env:tx_env_and_trees_from_top(aetx_contract),
    {ok, Participants} = Mod:signers(Tx, OnChainTrees),
    SkipKeys = Participants -- Pubkeys,
    case aesc_utils:verify_signatures_onchain(SignedTx, OnChainTrees, OnChainEnv,
                                              SkipKeys) of
        ok -> ok;
        {error, _E} -> {error, bad_signature}
    end.

verify_signatures_onchain_skip(SkipKeys, SignedTx) ->
    {OnChainEnv, OnChainTrees} =
        aetx_env:tx_env_and_trees_from_top(aetx_contract),
    case aesc_utils:verify_signatures_onchain(SignedTx, OnChainTrees, OnChainEnv,
                                              SkipKeys) of
        ok -> ok;
        {error, _E} -> {error, bad_signature}
    end.

verify_signatures_offchain(ChannelPubkey, Pubkeys, SignedTx) ->
    {OnChainEnv, OnChainTrees} =
        aetx_env:tx_env_and_trees_from_top(aetx_contract),
    {ok, Channel} = aec_chain:get_channel(ChannelPubkey),
    InitiatorPubkey = aesc_channels:initiator_pubkey(Channel),
    ResponderPubkey = aesc_channels:responder_pubkey(Channel),
    Participants = [InitiatorPubkey, ResponderPubkey],
    SkipKeys = Participants -- Pubkeys,
    case aesc_utils:verify_signatures_offchain(Channel, SignedTx, OnChainTrees, OnChainEnv,
                                              SkipKeys) of
        ok -> ok;
        _ -> {error, bad_signature}
    end.

check_tx_and_verify_signatures(SignedTx, Updates, Mod, Data, Pubkeys, ErrTypeMsg) ->
    ChannelPubkey = cur_channel_id(Data),
    ExpectedRound = next_round(Data),
    #data{state = State, opts = Opts, on_chain_id = ChannelPubkey} = Data,
    case aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)) of
        {aesc_settle_tx, _Tx} ->
            %% TODO: conduct more relevant checks
            verify_signatures_onchain_check(Pubkeys, SignedTx);
        {Mod, Tx} -> %% same callback module
            case Mod:channel_pubkey(Tx) of
                ChannelPubkey -> %% expected pubkey
                    CorrectRound =
                        case Mod of
                            aesc_close_mutual_tx -> true; %% no round here
                            _ -> Mod:round(Tx) =:= ExpectedRound
                        end,
                    case CorrectRound of
                        false ->
                            {error, wrong_round};
                        true when is_list(Updates) ->
                            case check_update_tx(SignedTx, Updates, State,
                                                 Opts, ChannelPubkey) of
                                ok ->
                                    case Mod of
                                        aesc_offchain_tx ->
                                            verify_signatures_offchain(ChannelPubkey,
                                                                       Pubkeys,
                                                                       SignedTx);
                                        _ ->
                                            verify_signatures_onchain_check(Pubkeys,
                                                                            SignedTx)
                                    end;
                                {error, E} -> {error, E}
                            end
                    end;
                _ ->
                    {error, different_channel_id}
            end;
        _E ->
            {error, ErrTypeMsg}
    end.

maybe_check_sigs_create(_, _, _, NextState, #data{strict_checks = false}) ->
    NextState();
maybe_check_sigs_create(Tx, Updates, Who, NextState, #data{state = State, opts = Opts} = D) ->
    CheckSigs =
        case verify_signatures_channel_create(Tx, Who) of
            ok ->
                case check_update_tx_initial(Tx, Updates, State, Opts) of
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
            report(error, E2, D),
            keep_state(D)
    end.

maybe_check_sigs(_, _, _, _, _, NextState, #data{strict_checks = false}) ->
    NextState();
maybe_check_sigs(Tx, Updates, TxType, WrongTxTypeMsg, Who, NextState, D)
        when Who =:= me
      orelse Who =:= other_participant
      orelse Who =:= both ->
    Pubkeys = pubkeys(Who, D),
    case check_tx_and_verify_signatures(Tx, Updates, TxType, D, Pubkeys, WrongTxTypeMsg) of
        ok ->
            NextState();
        {error, E} ->
            report(error, E, D),
            keep_state(D)
    end.

pubkeys(Who, D) ->
    case Who of
        me -> [my_account(D)];
        other_participant -> [other_account(D)];
        both -> both_accounts(D)
    end.

next_round(#data{state = State}) ->
    {Round, _} = aesc_offchain_state:get_latest_signed_tx(State),
    Round + 1.

%% %% for tracing only
%% event(_, _, _) ->
%%     ok.
account_type(Pubkey) ->
    case aec_chain:get_account(Pubkey) of
        {value, Account} ->
            {ok, aec_accounts:type(Account)};
        _ -> not_found
    end.

check_accounts(any, Responder, responder) ->
    check_account(Responder);
check_accounts(Initiator, Responder, _Role) ->
    case {check_account(Initiator), check_account(Responder)} of
        {ok, ok}     -> ok;
        {{error, _} = E, _} -> E;
        {_, {error, _} = E} -> E
    end.

check_account(A) ->
    case account_type(A) of
        {ok, basic}       -> ok;
        {ok, generalized} -> ok;
        _                 -> {error, not_found}
    end.

check_attach_info(#{ initiator := I1
                   , responder := R1
                   , port      := _P
                   , gproc_key := _K}, I, R) ->
    if I =:= any, R1 =:= R ->
            check_account(I1);
       I1 =:= I, R1 =:= R ->
            ok;
       R1 =/= R ->   %% shouldn't happen
            {error, responder_key_mismatch};
       I1 =/= I ->
            {error, initiator_key_mismatch};
       true ->
            {error, unrecognized_attach_info}
    end;
check_attach_info(_, _I, _R) ->
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

-spec init(term()) -> {ok, InitialState, data(), [{timeout, Time::pos_integer(),
                                                   InitialState}, ...]}
                          when InitialState :: state_name().
init(#{opts := Opts0} = Arg) ->
    {Role, Opts1} = maps:take(role, Opts0),
    Client = maps:get(client, Opts1),
    {Reestablish, ReestablishOpts, Opts2} =
        { maps:is_key(existing_channel_id, Opts1)
        , maps:with(?REESTABLISH_OPTS_KEYS, Opts1)
        , maps:without(?REESTABLISH_OPTS_KEYS, Opts1)
        },
    DefMinDepth = default_minimum_depth(Role),
    Opts = check_opts(
             [
              fun(O) -> check_minimum_depth_opt(DefMinDepth, Role, O) end,
              fun check_timeout_opt/1,
              fun check_rpt_opt/1,
              fun check_log_opt/1
             ], Opts2),
    #{initiator := Initiator} = Opts,
    Session = start_session(Arg, Reestablish, Opts#{role => Role}),
    StateInitF = fun(NewI) ->
                          CheckedOpts = maps:merge(Opts#{role => Role}, ReestablishOpts),
                          {ok, State} = aesc_offchain_state:new(CheckedOpts#{initiator => NewI}),
                          State
                  end,
    State = case Initiator of
                any -> StateInitF;
                _   -> StateInitF(Initiator)
            end,
    Data = #data{role   = Role,
                client  = Client,
                session = Session,
                opts    = Opts,
                state   = State,
                log     = aesc_window:new(maps:get(log_keep, Opts))},
    lager:debug("Session started, Data = ~p", [Data]),
    %% TODO: Amend the fsm above to include this step. We have transport-level
    %% connectivity, but not yet agreement on the channel parameters. We will next send
    %% a channel_open() message and await a channel_accept().
    case Role of
        initiator ->
            if Reestablish ->
                    ok_next(reestablish_init, send_reestablish_msg(ReestablishOpts, Data));
              true ->
                    ok_next(initialized, send_open_msg(Data))
            end;
        responder ->
            if Reestablish ->
                    ChanId = maps:get(existing_channel_id, ReestablishOpts),
                    ok_next(awaiting_reestablish,
                            Data#data{ channel_id  = ChanId
                                      , on_chain_id = ChanId });
              true ->
                    ok_next(awaiting_open, Data)
            end
    end.

terminate(Reason, _State, #data{session = Sn} = Data) ->
    lager:debug("terminate(~p, ~p, _)", [Reason, _State]),
    report(info, {died, Reason}, Data),
    report(debug, {log, win_to_list(Data#data.log)}, Data),
    try aesc_session_noise:close(Sn) catch error:_ -> ok end,
    ok.

code_change(_OldVsn, OldState, OldData, _Extra) ->
    {ok, OldState, OldData}.

%% ======================================================================
%% FSM transitions

%% We set timers here to ensure that they are always set.
%% gen_statem cancels event timers each time an event arrives
%%
next_state(St, D) -> next_state(St, D, []).
next_state(St, D, Opts) ->
    {next_state, St, cur_st(St, D), [timer_for_state(St, D)|Opts]}.

keep_state(D) -> keep_state(D, []).
keep_state(D, Opts) ->
    {keep_state, D, [timer_for_state(D)|Opts]}.

postpone(D) ->
    keep_state(D, [postpone]).

cur_st(St, D) ->
    D#data{cur_statem_state = St}.

%% ==================================================================
%% Internal functions

check_change_config(log_keep, Keep) when is_integer(Keep), Keep >= 0 ->
    {ok, log_keep, Keep};
check_change_config(_, _) ->
    {error, invalid_config}.

default_minimum_depth(initiator  ) -> undefined;
default_minimum_depth(responder) -> ?MINIMUM_DEPTH.

check_minimum_depth_opt(DefMinDepth, Role, Opts) ->
    case {maps:find(minimum_depth, Opts), Role} of
        {error, responder} -> Opts#{minimum_depth => DefMinDepth};
        _                    -> Opts
    end.

check_timeout_opt(Opts) ->
    Res = case maps:find(timeouts, Opts) of
              {ok, TOs} ->
                  TOs1 = maps:merge(?DEFAULT_TIMEOUTS, TOs),
                  Opts#{timeouts => TOs1};
              error ->
                  Opts#{timeouts => ?DEFAULT_TIMEOUTS}
          end,
    lager:debug("Timeouts: ~p", [Res]),
    Res.

check_rpt_opt(Opts) ->
    ROpts = case maps:find(report, Opts) of
                {ok, R} when is_map(R) ->
                    L = [{K,V} || {K,V} <- maps:to_list(R),
                                  lists:member(K, report_tags())
                                      andalso
                                      is_boolean(V)],
                    maps:from_list(L);
                error ->
                    case maps:find(report_info, Opts) of  %% bw compatibility
                        {ok, V} when is_boolean(V) ->
                            #{info => V};
                        _ ->
                            #{}
                    end;
                Other ->
                    lager:error("Unknown report opts: ~p", [Other]),
                    #{}
          end,
    lager:debug("Report opts = ~p", [ROpts]),
    Opts#{report => maps:merge(?DEFAULT_REPORT_FLAGS, ROpts)}.

check_log_opt(Opts) ->
    case maps:find(log_keep, Opts) of
        {ok, Keep} when is_integer(Keep), Keep >= 0 ->
            Opts;
        {ok, Invalid} ->
            lager:error("Invalid 'log_keep' option: ~p", [Invalid]),
            Opts#{log_keep => ?KEEP};
        error ->
            Opts#{log_keep => ?KEEP}
    end.

check_opts([H|T], Opts) ->
    check_opts(T, H(Opts));
check_opts([], Opts) ->
    Opts.

%% As per CHANNELS.md, the responder is regarded as the one typically
%% providing the service, and the initiator connects.
start_session(#{ port := Port
               , opts := Opts0 }, Reestablish, #{ role      := responder
                                                , responder := Responder
                                                , initiator := Initiator } = Opts) ->
    NoiseOpts = maps:get(noise, Opts, []),
    SessionOpts = if Reestablish ->
                          #{ reestablish => true
                           , port        => Port
                           , chain_hash  => aec_chain:genesis_hash()
                           , channel_id  => maps:get(existing_channel_id, Opts0)
                           , responder   => Responder };
                     true ->
                          #{ initiator => Initiator
                           , responder => Responder
                           , port      => Port }
                  end,
    ok(aesc_session_noise:accept(SessionOpts, NoiseOpts));
start_session(#{host := Host, port := Port}, _, #{role := initiator} = Opts) ->
    NoiseOpts = maps:get(noise, Opts, []),
    ok(aesc_session_noise:connect(Host, Port, NoiseOpts)).

ok({ok, X}) -> X.

maybe_initialize_offchain_state(any, NewI, #data{state = F} = D) when is_function(F, 1) ->
    D#data{state = F(NewI)};
maybe_initialize_offchain_state(_, _, D) ->
    D.

ok_next(Next, Data) ->
    {ok, Next, cur_st(Next, Data), [timer_for_state(Next, Data)]}.

stop_ok(ok) ->
    ok;
stop_ok({'EXIT', noproc}) ->
    ok;
stop_ok({'EXIT', {normal,{sys,terminate,_}}}) ->
    ok.

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

error_binary(E) when is_atom(E) ->
    atom_to_binary(E, latin1).

send_error_msg(Reason, #data{session = Sn} = D) ->
    case cur_channel_id(D) of
        undefined -> no_msg;
        ChId ->
            case Reason of
                {error, E} ->
                    Eb = error_binary(E),
                    Msg = #{ channel_id => ChId
                           , data       => Eb },
                    report(error, Eb, D),
                    aesc_session_noise:error(Sn, Msg);
                _ ->
                    no_msg
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
    catch
        error:badarg ->
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
    catch
        error:badarg ->
            false
    end.

is_channel_locked(0) -> false;
is_channel_locked(LockedUntil) ->
    LockedUntil >= curr_height().

withdraw_locked_complete(SignedTx, Updates, #data{state = State, opts = Opts} = D) ->
    {OnChainEnv, OnChainTrees} =
        aetx_env:tx_env_and_trees_from_top(aetx_contract),
    D1   = D#data{state = aesc_offchain_state:set_signed_tx(SignedTx, Updates,
                                                            State,
                                                            OnChainTrees,
                                                            OnChainEnv, Opts)},
    next_state(open, D1).

settle_signed(SignedTx, Updates, #data{ on_chain_id = ChId} = D) ->
    ?LOG_DEBUG(settle_signed, 3, [SignedTx]),
    case aec_chain:get_channel(ChId) of
        {ok, Ch} ->
            LockedUntil = aesc_channels:locked_until(Ch),
            {ok, D1} =
                case is_channel_locked(LockedUntil) of
                    true ->
                        lager:debug("channel is locked", []),
                        start_min_depth_watcher(unlock, SignedTx, Updates, D);
                    false ->
                        lager:debug("channel not locked, pushing settle", []),
                        ok = aec_tx_pool:push(SignedTx),
                        start_min_depth_watcher(close, SignedTx, Updates, D)
                end,
            next_state(channel_closing, D1);
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
                    case aec_tx_pool:push(SignedTx) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            lager:debug("Close_solo tx failed: ~p", [Reason]),
                            ok
                    end;
                true ->
                    lager:debug("channel is locked: cannot solo-close", []),
                    ok
            end,
            %% Let the watcher pick up the tx once it makes it onto the chain
            next_state(open, D);
        {error,_} = Error ->
            close(Error, D)
    end.

funding_locked_complete(#data{ latest = {create, _, Updates}
                             , create_tx = CreateTx
                             , state = State
                             , opts = Opts} = D) ->
    {OnChainEnv, OnChainTrees} = aetx_env:tx_env_and_trees_from_top(aetx_contract),
    State1 = aesc_offchain_state:set_signed_tx(CreateTx, Updates, State, OnChainTrees,
                                               OnChainEnv, Opts),
    D1 = D#data{state = State1},
    initialize_cache(D1),
    next_state(open, D1).

close(Reason, D) ->
    close_(Reason, log(evt, close, Reason, D)).

close_(close_mutual, D) ->
    report(info, close_mutual, D),
    {stop, normal, D};
close_(closed_confirmed, D) ->
    report(info, closed_confirmed, D),
    {stop, normal, D};
close_(leave, D) ->
    {stop, normal, D};
close_(bad_signature, D) ->
    report(info, bad_signature, D),
    {stop, normal, D};
close_(bad_state_hash, D) ->
    report(info, bad_state_hash, D),
    {stop, normal, D};
close_(not_create_tx, D) ->
    report(info, not_create_tx, D),
    {stop, normal, D};
close_(Reason, D) ->
    try send_error_msg(Reason, D)
    catch error:_ -> ignore
    end,
    {stop, Reason, D}.

handle_call(St, Req, From, #data{} = D) ->
    lager:debug("handle_call(~p, ~p, ~p, ~p)", [St, Req, From, D]),
    try handle_call_(St, Req, From, D)
    catch
        error:Error ->
            ?LOG_CAUGHT(Error),
            keep_state(D, [{reply, From, {error, Error}}])
    end.

handle_call_(_AnyState, {inband_msg, ToPub, Msg}, From, #data{} = D) ->
    case {ToPub, other_account(D)} of
        {X, X} ->
            keep_state(send_inband_msg(ToPub, Msg, D), [{reply, From, ok}]);
        _ ->
            keep_state(D, [{reply, From, {error, unknown_recipient}}])
    end;
handle_call_(open, {upd_transfer, FromPub, ToPub, Amount}, From,
            #data{opts = #{initiator := I,
                           responder := R}} = D) ->
    case FromPub =/= ToPub andalso ([] == [FromPub, ToPub] -- [I, R]) of
        true ->
            handle_upd_transfer(FromPub, ToPub, Amount, From, set_ongoing(D));
        false ->
            keep_state(set_ongoing(D), [{reply, From, {error, invalid_pubkeys}}])
    end;
handle_call_(open, {upd_deposit, #{amount := Amt} = Opts}, From,
             #data{} = D) when is_integer(Amt), Amt > 0 ->
    FromPub = my_account(D),
    case maps:find(from, Opts) of
        {ok, FromPub} -> ok;
        {ok, _Other}  -> error(conflicting_accounts);
        error         -> ok
    end,
    {ok, DepTx, Updates} = dep_tx_for_signing(Opts#{acct => FromPub}, D),
    D1 = request_signing(deposit_tx, DepTx, Updates, D),
    gen_statem:reply(From, ok),
    next_state(awaiting_signature, set_ongoing(D1));
handle_call_(open, {upd_withdraw, #{amount := Amt} = Opts}, From,
             #data{} = D) when is_integer(Amt), Amt > 0 ->
    ToPub = my_account(D),
    case maps:find(to, Opts) of
        {ok, ToPub} -> ok;
        {ok, _Other}  -> error(conflicting_accounts);
        error         -> ok
    end,
    {ok, DepTx, Updates} = wdraw_tx_for_signing(Opts#{acct => ToPub}, D),
    D1 = request_signing(withdraw_tx, DepTx, Updates, D),
    gen_statem:reply(From, ok),
    next_state(awaiting_signature, set_ongoing(D1));
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
    Updates = [Update],
    {OnChainEnv, OnChainTrees} =
        aetx_env:tx_env_and_trees_from_top(aetx_contract),
    Height = aetx_env:height(OnChainEnv),
    ActiveProtocol = aec_hard_forks:protocol_effective_at_height(Height),
    try  Tx1 = aesc_offchain_state:make_update_tx(Updates, State,
                                                  ChannelId,
                                                  ActiveProtocol,
                                                  OnChainTrees, OnChainEnv, ChannelOpts),
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
                D1 = request_signing(?UPDATE, Tx1, Updates, D),
                gen_statem:reply(From, ok),
                next_state(awaiting_signature, set_ongoing(D1))
          end
    catch
        error:Reason ->
            process_update_error(Reason, From, D)
    end;
handle_call_(awaiting_signature, Msg, From,
             #data{ongoing_update = true} = D)
  when ?UPDATE_REQ(element(1,Msg)) ->
    %% Race detection!
    lager:debug("race detected: ~p", [Msg]),
    {sign, OngoingOp, _, _} = D#data.latest,
    gen_statem:reply(From, {error, conflict}),
    lager:debug("calling handle_update_conflict", []),
    handle_update_conflict(OngoingOp, D#data{latest = undefined});
handle_call_(open, {get_contract_call, Contract, Caller, Round}, From,
             #data{state = State} = D) ->
    Response =
        case aesc_offchain_state:get_contract_call(Contract, Caller, Round, State) of
            {error, call_not_found} -> {error, call_not_found};
            {ok, Call} -> {ok, Call}
        end,
    keep_state(D, [{reply, From, Response}]);
handle_call_(open, shutdown, From, D) ->
    {ok, CloseTx, Updates} = close_mutual_tx_for_signing(D),
    D1 = request_signing(?SHUTDOWN, CloseTx, Updates, D),
    gen_statem:reply(From, ok),
    next_state(awaiting_signature, set_ongoing(D1));
handle_call_(channel_closing, shutdown, From, #data{strict_checks = Strict} = D) ->
    case (not Strict) or is_fork_active(?LIMA_PROTOCOL_VSN) of
        true ->
            %% Initiate mutual close
            {ok, CloseTx, Updates} = close_mutual_tx_for_signing(D),
            D1 = request_signing(?SHUTDOWN, CloseTx, Updates, D),
            gen_statem:reply(From, ok),
            next_state(awaiting_signature, set_ongoing(D1));
        false ->
            %% Backwards compatibility
            keep_state(D, [{reply, From, {error, unknown_request}}])
    end;
handle_call_(_, get_state, From, #data{ on_chain_id = ChanId
                                      , opts        = Opts
                                      , state       = State} = D) ->
    #{initiator := Initiator,
      responder := Responder} = Opts,
    {ok, IAmt} = aesc_offchain_state:balance(Initiator, State),
    {ok, RAmt} = aesc_offchain_state:balance(Responder, State),
    StateHash = aesc_offchain_state:hash(State),
    {Round, _} = aesc_offchain_state:get_latest_signed_tx(State),
    Result =
        #{channel_id=> ChanId,
          initiator => Initiator,
          responder => Responder,
          init_amt  => IAmt,
          resp_amt  => RAmt,
          state_hash=> StateHash,
          round     => Round},
    keep_state(D, [{reply, From, {ok, Result}}]);
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
handle_call_(_St, get_history, From, #data{log = Log} = D) ->
    keep_state(D, [{reply, From, win_to_list(Log)}]);
handle_call_(_St, {change_config, Key, Value}, From, D) ->
    case handle_change_config(Key, Value, D) of
        {ok, Reply, D1} ->
            keep_state(D1, [{reply, From, Reply}]);
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
        catch
            error:_ ->
                {error, invalid_arguments}
        end,
    lager:debug("get_balances(~p) -> ~p", [Accounts, Result]),
    keep_state(D, [{reply, From, Result}]);
handle_call_(_, get_round, From, #data{ state = State } = D) ->
    Res = try  {Round, _} = aesc_offchain_state:get_latest_signed_tx(State),
               {ok, Round}
          catch
              error:_ ->
                  {error, no_state}
          end,
    keep_state(D, [{reply, From, Res}]);
handle_call_(_, prune_local_calls, From, #data{ state = State0 } = D) ->
    State = aesc_offchain_state:prune_calls(State0),
    keep_state(D#data{state = State}, [{reply, From, ok}]);
handle_call_(St, close_solo, From, D) ->
    case St of
        channel_closing ->
            keep_state(D, [{reply, From, {error, channel_closing}}]);
        _ ->
            {ok, CloseSoloTx, Updates} = close_solo_tx_for_signing(D),
            gen_statem:reply(From, ok),
            D1 = set_ongoing(request_signing(
                               close_solo_tx, CloseSoloTx, Updates, D)),
            next_state(awaiting_signature, D1)
    end;
handle_call_(St, slash, From, D) ->
    case St of
        channel_closing ->
            {ok, SlashTx, Updates} = slash_tx_for_signing(D),
            gen_statem:reply(From, ok),
            D1 = set_ongoing(request_signing(slash_tx, SlashTx, Updates, D)),
            next_state(awaiting_signature, D1);
        _ ->
            keep_state(D, [{reply, From, {error, channel_not_closing}}])
    end;
handle_call_(St, settle, From, D) ->
    case St of
        channel_closing ->
            {ok, SettleTx, Updates} = settle_tx_for_signing(D),
            gen_statem:reply(From, ok),
            D1 = set_ongoing(request_signing(settle_tx, SettleTx, Updates, D)),
            next_state(awaiting_signature, D1);
        _ ->
            keep_state(D, [{reply, From, {error, channel_not_closed}}])
    end;
handle_call_(_, {strict_checks, Strict}, From, #data{} = D) when
        is_boolean(Strict) ->
    keep_state(D#data{strict_checks = Strict}, [{reply, From, ok}]);
handle_call_(St, _Req, _From, D) when ?TRANSITION_STATE(St) ->
    postpone(D);
handle_call_(_St, _Req, From, D) ->
    keep_state(D, [{reply, From, {error, unknown_request}}]).

handle_info(Msg, #data{cur_statem_state = St} = D) ->
    lager:debug("Discarding info in ~p state: ~p", [St, Msg]),
    keep_state(log(drop, msg_type(Msg), Msg, D)).

%% A few different modes are specified here:
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

handle_common_event_(timeout, St = T, St, _, D) ->
    close({timeout, T}, D);
handle_common_event_(cast, {?DISCONNECT, _} = Msg, _St, _, D) ->
    D1 = log(rcv, ?DISCONNECT, Msg, D),
    case D1#data.channel_status of
        closing ->
            keep_state(D1);
        _ ->
            close(disconnect, D1)
    end;
handle_common_event_(cast, {?INBAND_MSG, Msg}, _St, _, D) ->
    NewD = case check_inband_msg(Msg, D) of
               {ok, D1} ->
                   report(message, Msg, D1),
                   D1;
               {error, _} = Err ->
                   lager:warning("Error in inband_msg: ~p", [Err]),
                   D
           end,
    keep_state(NewD);
handle_common_event_(cast, {?CHANNEL_CLOSING, Info} = Msg, _St, _, D) ->
    lager:debug("got ~p", [Msg]),
    D1 = log(rcv, ?CHANNEL_CLOSING, Msg, D),
    case check_closing_event(Info, D1) of
        {can_slash, _Round, SignedTx} ->
            %% report_on_chain_tx(can_slash, SignedTx, D1),
            %% D2 = set_ongoing(request_signing(slash_tx, SlashTx, Updates, D1)),
            %% next_state(awaiting_signature, D2);
            report_on_chain_tx(can_slash, SignedTx, D1),
            next_state(channel_closing, D1);
        {ok, proper_solo_closing, SignedTx} ->
            lager:debug("Channel solo-closing", []),
            report_on_chain_tx(solo_closing, SignedTx, D1),
            next_state(channel_closing, D1);
        {ok, solo_closed, SignedTx} ->
            lager:debug("Channel solo-closed", []),
            report_on_chain_tx(solo_closed, SignedTx, D1),
            close(channel_no_longer_active, D1);
        {error, channel_active} ->
            %% Weird, but possible e.g. due to forking (TODO: what might happen next?)
            keep_state(log(drop, msg_type(Msg), Msg, D));
        {error, _} = Error ->
            %% Couldn't find the channel object on-chain, or something weird happened
            lager:debug("Channel not found, or other: ~p", [Error]),
            close(channel_no_longer_active, D)
    end;
handle_common_event_(cast, {?CHANNEL_CHANGED, #{tx_hash := TxHash} = Info} = Msg,
                     _St, _, D) ->
    D1 = log(rcv, msg_type(Msg), Msg, D),
    case D1#data.latest of
        {?MIN_DEPTH, _, LatestTxHash, _, _} when TxHash =/= LatestTxHash ->
            %% This is a problem: channel changed while waiting to confirm
            %% other tx hash
            close({error, unexpected_tx_on_chain}, D);
        _ ->
            lager:debug("Fsm notes channel change ~p", [Info]),
            report_on_chain_tx(channel_changed, maps:get(tx, Info), D1),
            keep_state(D1)
    end;
handle_common_event_(cast, {?CHANNEL_CLOSED, #{tx := SignedTx} = _Info} = Msg, _St, _, D) ->
    %% Start a minimum-depth watch, then (if confirmed) terminate
    report_on_chain_tx(channel_closed, SignedTx, D),
    {ok, D1} = start_min_depth_watcher({?MIN_DEPTH, ?WATCH_CLOSED}, SignedTx, [], D),
    next_state(channel_closed, log(rcv, msg_type(Msg), Msg, D1));
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

init_checks(Opts) ->
    #{initiator   := Initiator,
      responder   := Responder,
      role        := Role,
      lock_period := LockPeriod} = Opts,
    Checks = [fun() -> check_amounts(Opts) end,
              fun() -> check_accounts(Initiator, Responder, Role) end,
              fun() ->
                  case LockPeriod < 0 of
                      true -> {error, lock_period_too_low};
                      false -> ok
                  end
              end
              ],
    case aeu_validation:run(Checks) of
        {error, _Reason} = Err ->
            Err;
        ok ->
            ok
    end.

is_fork_active(ProtocolVSN) ->
    %% Enable a new fork only after MINIMUM_DEPTH generations in order to avoid fork changes
    ProtocolVSN == aec_hard_forks:protocol_effective_at_height(max(curr_height() - ?MINIMUM_DEPTH, aec_block_genesis:height())).
