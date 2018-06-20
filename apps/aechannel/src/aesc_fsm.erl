-module(aesc_fsm).
-behaviour(gen_statem).

-export([start_link/1]).

%% API
-export([initiate/3,     %% (host(), port(), Opts :: #{})
         respond/2,      %% (port(), Opts :: #{})
         upd_transfer/4, %% (fsm() , from(), to(), amount())
         upd_deposit/2,  %% (fsm() , map())
         upd_withdraw/2, %% (fsm() , map())
         shutdown/1,     %% (fsm())
         client_died/1,  %% (fsm())
         inband_msg/3]).

%% Used by noise session
-export([message/2]).

%% Used by client
-export([signing_response/3]).    %% (Fsm, Tag, Obj)

%% Used by min-depth watcher
-export([minimum_depth_achieved/4]).  %% (Fsm, OnChainId, Type, TxHash)

-export([init/1,
         callback_mode/0,
         code_change/4,
         terminate/3]).

-export([where/2]).

%% FSM states
-export([initialized/3,
         accepted/3,
         awaiting_open/3,
         awaiting_signature/3,
         awaiting_locked/3,
         awaiting_initial_state/3,
         awaiting_update_ack/3,
         awaiting_deposit_update/3,
         awaiting_withdraw_update/3,
         half_signed/3,
         signed/3,
         dep_half_signed/3,
         wdraw_half_signed/3,
         dep_signed/3,
         wdraw_signed/3,
         open/3,
         closing/3,
         disconnected/3]).

-export([timeouts/0,
         report_tags/0]).

-include("aesc_codec.hrl").

-type role() :: initiator | responder.
-type sign_tag() :: create_tx
                  | funding_created.

-define(MINIMUM_DEPTH, 4). % number of blocks until an opening tx
                           % should be considered final

-define(GEN_STATEM_OPTS, []).  % Use e.g. [{debug, [trace]}] for debugging

-define(WATCH_FND, funding).
-define(WATCH_DEP, deposit).
-define(WATCH_WDRAW, withdraw).

-record(data, { role                   :: role()
              , channel_status         :: undefined | open
              , cur_statem_state       :: undefined | atom()
              , state                  :: aesc_offchain_state:state()
              , session                :: pid()
              , client                 :: pid()
              , opts                   :: map()
              , channel_id             :: undefined | binary()
              , on_chain_id            :: undefined | binary()
              , create_tx              :: undefined | any()
              , watcher                :: undefined | pid()
              , latest = undefined     :: undefined | any()
              , last_reported_update   :: undefined | non_neg_integer()
              }).

-define(TRANSITION_STATE(S),  S=:=awaiting_signature
                            ; S=:=awaiting_open
                            ; S=:=awaiting_locked
                            ; S=:=awaiting_update_ack
                            ; S=:=closing ).

callback_mode() -> [state_functions, state_enter].

%% State machine
%% +---+ channel_open                                                         +---+
%% |   | -------------> +-------------+                 error                 |   |
%% |   |                | initialized | ------------------------------------> |   |
%% |   | <------------- +-------------+                                       |   |
%% |   |   [timeout]/          | channel_accept                               |   |
%% |   |   [disconnect]        v                                              |   |
%% |   |                +-------------+                 error                 |   |
%% |   | <------------- |  accepted   | ------------------------------------> |   |
%% | c |   [timeout]/   +-------------+                                       | e |
%% | l |   [disconnect]        | funding_created                              | r |
%% | o |                       v                                              | r |
%% | s |                +-------------+                 error                 | o |
%% | e | <------------- | half_signed | ------------------------------------> | r |
%% | d |   [timeout]/   +-------------+                                       |   |
%% |   |   [disconnect]        | funding_signed                               |   |
%% |   |                       v                                              |   |
%% |   |                +-------------+  [disconnect]                         |   |
%% |   | <------------- |   signed    | -------------+  error                 |   |
%% |   |   [timeout]    +-------------+ -------------|----------------------> |   |
%% |   |                       |    |  shutdown      |                        |   |
%% |   |                       |    +------------+   |                        |   |
%% |   |                       | funding_locked  |   |                        |   |
%% |   |                       |                 |   v                        |   |
%% |   |                       v        [disconnect] +--------------+  error  |   |
%% |   |                +-------------+ ---------|-> | disconnected | ------> |   |
%% |   | <------------- |    open     |          |   +--------------+         |   |
%% |   |   [timeout]    +-------------+ <--------|------------+               |   |
%% |   |                 ^ |  ^ | |       channel_reestablish                 |   |
%% |   |        update_* | |  | | |              |                            |   |
%% |   |                 +-+  +-+ | shutdown     v                            |   |
%% |   |                          |    +-------------+ [disconnect]           |   |
%% |   |                          +--> |   closing   | ------------+          |   |
%% |   |                               +-------------+             |          |   |
%% |   |                                |  ^                       |          |   |
%% |   |                                |  | channel_reestablish   |          |   |
%% |   | <------------------------------+  |                       v          |   |
%% |   |       closing_signed              |         +--------------+  error  |   |
%% |   |                                   +-------- | disconnected | ------> |   |
%% +---+                                             +--------------+         +---+
%%   ^                                                                          |
%%   |                                                                          |
%%   +--------------------------------------------------------------------------+

message(Fsm, {T, _} = Msg) when T =:= ?CH_OPEN
                              ; T =:= ?CH_ACCEPT
                              ; T =:= ?FND_CREATED
                              ; T =:= ?FND_SIGNED
                              ; T =:= ?FND_LOCKED
                              ; T =:= ?UPDATE
                              ; T =:= ?UPDATE_ACK
                              ; T =:= ?UPDATE_ERR
                              ; T =:= ?DEP_CREATED
                              ; T =:= ?DEP_SIGNED
                              ; T =:= ?DEP_LOCKED
                              ; T =:= ?DEP_ERR
                              ; T =:= ?WDRAW_CREATED
                              ; T =:= ?WDRAW_SIGNED
                              ; T =:= ?WDRAW_LOCKED
                              ; T =:= ?WDRAW_ERR
                              ; T =:= ?INBAND_MSG
                              ; T =:= disconnect
                              ; T =:= ?SHUTDOWN
                              ; T =:= ?SHUTDOWN_ACK
                              ; T =:= ?CH_REESTABL ->
    lager:debug("message(~p, ~p)", [Fsm, Msg]),
    gen_statem:cast(Fsm, Msg).

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
    gen_statem:cast(Fsm, {signed, Tag, Obj}).

minimum_depth_achieved(Fsm, ChanId, Type, TxHash) ->
    gen_statem:cast(Fsm, {?MIN_DEPTH_ACHIEVED, ChanId, Type, TxHash}).

where(ChanId, Role) ->
    gproc:where(gproc_name(ChanId, Role)).

%% ======================================================================
%% Default timer values

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
            Alias = timer_subst(St),
            timer(Alias, St, D)
    end.

timer_subst(open                    ) -> idle;
timer_subst(awaiting_open           ) -> idle;
timer_subst(awaiting_signature      ) -> sign;
timer_subst(awaiting_locked         ) -> funding_lock;
timer_subst(awaiting_initial_state  ) -> accept;
timer_subst(awaiting_update_ack     ) -> accept;
timer_subst(awaiting_deposit_update ) -> accept;
timer_subst(awaiting_withdraw_update) -> accept;
timer_subst(accepted                ) -> funding_create;
timer_subst(half_signed             ) -> funding_sign;
timer_subst(signed                  ) -> funding_lock;
timer_subst(dep_half_signed         ) -> funding_sign;
timer_subst(wdraw_half_signed       ) -> funding_sign;
timer_subst(dep_signed              ) -> funding_lock;
timer_subst(wdraw_signed            ) -> funding_lock;
timer_subst(initialized             ) -> accept;
timer_subst(closing                 ) -> accept.

default_timeouts() ->
    #{ open           => 120000
     , accept         => 120000
     , funding_create => 120000
     , funding_sign   => 120000
     , funding_lock   => 360000
     , idle           => 600000
     , sign           => 500000
     }.

timeouts() ->
    maps:keys(default_timeouts()).

%%
%% ======================================================================

initiate(Host, Port, #{} = Opts0) ->
    lager:debug("initiate(~p, ~p, ~p)", [Host, Port, Opts0]),
    Opts = maps:merge(#{client => self()}, Opts0),
    start_link(#{role => initiator
               , host => Host
               , port => Port
               , opts => Opts}).

respond(Port, #{} = Opts0) ->
    lager:debug("respond(~p, ~p)", [Port, Opts0]),
    Opts = maps:merge(#{client => self()}, Opts0),
    start_link(#{role => responder
               , port => Port
               , opts => Opts}).

upd_transfer(_Fsm, _From, _To, Amount) when Amount < 0 ->
    {error, negative_amount};
upd_transfer(Fsm, From, To, Amount) ->
    lager:debug("upd_transfer(~p, ~p, ~p, ~p)", [Fsm, From, To, Amount]),
    gen_statem:call(Fsm, {upd_transfer, From, To, Amount}).

upd_deposit(Fsm, #{amount := Amt} = Opts) when is_integer(Amt), Amt > 0 ->
    lager:debug("upd_deposit(~p)", [Opts]),
    gen_statem:call(Fsm, {upd_deposit, Opts}).

upd_withdraw(Fsm, #{amount := Amt} = Opts) when is_integer(Amt), Amt > 0 ->
    lager:debug("upd_withdraw(~p)", [Opts]),
    gen_statem:call(Fsm, {upd_withdraw, Opts}).

inband_msg(Fsm, To, Msg) ->
    lager:debug("inband_msg(~p, ~p, ~p)", [Fsm, To, Msg]),
    gen_statem:call(Fsm, {inband_msg, To, Msg}).

shutdown(Fsm) ->
    lager:debug("shutdown(~p)", [Fsm]),
    gen_statem:call(Fsm, shutdown).

client_died(Fsm) ->
    %TODO: possibility for reconnect
    lager:debug("client died(~p)", [Fsm]),
    ok = gen_statem:stop(Fsm).

start_link(#{} = Arg) ->
    gen_statem:start_link(?MODULE, Arg, ?GEN_STATEM_OPTS).


%% ======================================================================
%% FSM initialization

init(#{role := Role} = Arg) ->
    #{client := Client} = Opts0 = maps:get(opts, Arg, #{}),
    DefMinDepth = default_minimum_depth(Role),
    Opts = check_opts(
             [
              fun(O) -> check_minimum_depth_opt(DefMinDepth, Role, O) end,
              fun check_timeout_opt/1,
              fun check_rpt_opt/1
             ], Opts0),
    Session = start_session(Arg, Opts),
    {ok, State} = aesc_offchain_state:new(Opts),
    Data = #data{role    = Role,
                 client  = Client,
                 session = Session,
                 opts    = Opts,
                 state   = State},
    lager:debug("Session started, Data = ~p", [Data]),
    %% TODO: Amend the fsm above to include this step. We have transport-level
    %% connectivity, but not yet agreement on the channel parameters. We will next send
    %% a channel_open() message and await a channel_accept().
    case Role of
        initiator ->
            {ok, initialized, send_open_msg(Data),
                        [timer_for_state(initialized, Data)]};
        responder ->
            {ok, awaiting_open, Data,
                        [timer_for_state(awaiting_open, Data)]}
    end.

check_opts([H|T], Opts) ->
    check_opts(T, H(Opts));
check_opts([], Opts) ->
    Opts.

check_minimum_depth_opt(DefMinDepth, Role, Opts) ->
    case {maps:find(minimum_depth, Opts), Role} of
        {error, responder} -> Opts#{minimum_depth => DefMinDepth};
        _                    -> Opts
    end.

check_timeout_opt(Opts) ->
    Res = case maps:find(timeouts, Opts) of
              {ok, TOs} ->
                  TOs1 = maps:merge(default_timeouts(), TOs),
                  Opts#{timeouts => TOs1};
              error ->
                  Opts#{timeouts => default_timeouts()}
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
    Opts#{report => maps:merge(default_report_flags(), ROpts)}.

%% As per CHANNELS.md, the responder is regarded as the one typically
%% providing the service, and the initiator connects.
start_session(#{role := responder, port := Port}, Opts) ->
    NoiseOpts = maps:get(noise, Opts, []),
    ok(aesc_session_noise:accept(Port, NoiseOpts));
start_session(#{role := initiator, host := Host, port := Port}, Opts) ->
    NoiseOpts = maps:get(noise, Opts, []),
    ok(aesc_session_noise:connect(Host, Port, NoiseOpts)).

ok({ok, X}) -> X.


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

%% ======================================================================
%% FSM states

awaiting_open(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_open(cast, {?CH_OPEN, Msg}, #data{role = responder} = D) ->
    case check_open_msg(Msg, D) of
        {ok, D1} ->
            report(info, channel_open, D1),
            gproc_register(D1),
            next_state(accepted, send_channel_accept(D1));
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_open(timeout, awaiting_open = T, D) ->
    close({timeout, T}, D);
awaiting_open(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D).

initialized(enter, _OldSt, _D) -> keep_state_and_data;
initialized(cast, {?CH_ACCEPT, Msg}, #data{role = initiator} = D) ->
    case check_accept_msg(Msg, D) of
        {ok, D1} ->
            gproc_register(D1),
            report(info, channel_accept, D1),
            {ok, CTx} = create_tx_for_signing(D1),
            ok = request_signing(create_tx, CTx, D1),
            D2 = D1#data{latest = {sign, create_tx, CTx}},
            next_state(awaiting_signature, D2);
        {error, _} = Error ->
            close(Error, D)
    end;
initialized(timeout, initialized = T, D) ->
    close({timeout, T}, D);
initialized(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
initialized({call, From}, Req, D) ->
    handle_call(initialized, Req, From, D).


awaiting_signature(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_signature(cast, {?SIGNED, create_tx, Tx},
                   #data{role = initiator, latest = {sign, create_tx, _CTx}} = D) ->
    next_state(half_signed,
               send_funding_created_msg(Tx, D#data{latest = undefined}));
awaiting_signature(cast, {?SIGNED, deposit_tx, Tx},
                   #data{latest = {sign, deposit_tx, _DTx}} = D) ->
    next_state(dep_half_signed,
               send_deposit_created_msg(Tx, D#data{latest = undefined}));
awaiting_signature(cast, {?SIGNED, withdraw_tx, Tx},
                   #data{latest = {sign, withdraw_tx, _DTx}} = D) ->
    next_state(wdraw_half_signed,
               send_withdraw_created_msg(Tx, D#data{latest = undefined}));
awaiting_signature(cast, {?SIGNED, ?FND_CREATED, SignedTx},
                   #data{role = responder,
                         latest = {sign, ?FND_CREATED, HSCTx}} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    HSCTx, aetx_sign:signatures(SignedTx)),
    D1 = send_funding_signed_msg(NewSignedTx, D#data{create_tx = NewSignedTx}),
    report(on_chain_tx, NewSignedTx, D1),
    {ok, D2} = start_min_depth_watcher(?WATCH_FND, NewSignedTx, D1),
    next_state(awaiting_locked, D2);
awaiting_signature(cast, {?SIGNED, ?DEP_CREATED, SignedTx},
                   #data{latest = {sign, ?DEP_CREATED, HSCTx}} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    HSCTx, aetx_sign:signatures(SignedTx)),
    D1 = send_deposit_signed_msg(NewSignedTx, D),
    report(on_chain_tx, NewSignedTx, D1),
    {ok, D2} = start_min_depth_watcher(?WATCH_DEP, NewSignedTx, D1),
    next_state(awaiting_locked, D2);
awaiting_signature(cast, {?SIGNED, ?WDRAW_CREATED, SignedTx},
                   #data{latest = {sign, ?WDRAW_CREATED, HSCTx}} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    HSCTx, aetx_sign:signatures(SignedTx)),
    D1 = send_withdraw_signed_msg(NewSignedTx, D),
    report(on_chain_tx, NewSignedTx, D1),
    {ok, D2} = start_min_depth_watcher(?WATCH_WDRAW, NewSignedTx, D1),
    next_state(awaiting_locked, D2);
awaiting_signature(cast, {?SIGNED, ?UPDATE, SignedTx}, D) ->
    D1 = send_update_msg(SignedTx,
                         D#data{state = aesc_offchain_state:add_half_signed_tx(SignedTx, D#data.state)}),
    next_state(awaiting_update_ack, D1#data{latest = undefined});
awaiting_signature(cast, {?SIGNED, ?UPDATE_ACK, SignedTx},
                   #data{latest = {sign, ?UPDATE_ACK, OCTx}, opts = Opts} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    OCTx, aetx_sign:signatures(SignedTx)),
    D1 = send_update_ack_msg(NewSignedTx, D),
    State = aesc_offchain_state:add_signed_tx(NewSignedTx, D1#data.state, Opts),
    D2 = D1#data{state = State},
    next_state(open, D2);
awaiting_signature(cast, {?UPDATE, _Msg},
                   #data{latest = {sign, Upd, _}} = D) when Upd==?UPDATE;
                                                            Upd==?UPDATE_ACK ->
    D1 = send_update_err_msg(fallback_to_stable_state(D)),
    next_state(open, D1);
awaiting_signature(cast, {?SIGNED, ?SHUTDOWN, SignedTx}, D) ->
    D1 = send_shutdown_msg(SignedTx, D),
    D2 = D1#data{latest = {shutdown, SignedTx}},
    next_state(closing, D2);
awaiting_signature(cast, {?SIGNED, ?SHUTDOWN_ACK, SignedTx},
                   #data{latest = {sign, ?SHUTDOWN_ACK, CMTx}} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    CMTx, aetx_sign:signatures(SignedTx)),
    D1 = send_shutdown_ack_msg(NewSignedTx, D),
    D2 = D1#data{latest = undefined},
    report(on_chain_tx, NewSignedTx, D1),
    close(close_mutual, D2);
awaiting_signature(timeout, awaiting_signature = T, D) ->
    close({timeout, T}, D);
awaiting_signature(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
awaiting_signature({call, From}, Req, D) ->
    handle_call(awaiting_signature, Req, From, D).


accepted(enter, _OldSt, _D) -> keep_state_and_data;
accepted(cast, {?FND_CREATED, Msg}, #data{role = responder} = D) ->
    case check_funding_created_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report(info, funding_created, D1),
            lager:debug("funding_created: ~p", [SignedTx]),
            ok = request_signing(?FND_CREATED, aetx_sign:tx(SignedTx), D1),
            D2 = D1#data{latest = {sign, ?FND_CREATED, SignedTx}},
            next_state(awaiting_signature, D2)
        %% {error, _} = Error ->
        %%     close(Error, D)
    end;
accepted(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
accepted(timeout, accepted = T, D) ->
    close({timeout, T}, D);
accepted({call, From}, Req, D) ->
    handle_call(accepted, Req, From, D).


half_signed(enter, _OldSt, _D) -> keep_state_and_data;
half_signed(cast, {?FND_SIGNED, Msg}, #data{role = initiator} = D) ->
    case check_funding_signed_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            lager:debug("funding_signed ok", []),
            report(info, funding_signed, D1),
            report(on_chain_tx, SignedTx, D1),
            ok = aec_tx_pool:push(SignedTx),
            D2 = D1#data{create_tx = SignedTx},
            {ok, D3} = start_min_depth_watcher(?WATCH_FND, SignedTx, D2),
            next_state(awaiting_locked, D3)
        %% {error, _} = Error ->
        %%     close(Error, D)
    end;
half_signed(timeout, half_signed = T, D) ->
    close({timeout, T}, D);
half_signed(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
half_signed({call, From}, Req, D) ->
    handle_call(half_signed, Req, From, D).

dep_half_signed(enter, _OldSt, _D) -> keep_state_and_data;
dep_half_signed(cast, {?DEP_SIGNED, Msg}, D) ->
    case check_deposit_signed_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report(on_chain_tx, SignedTx, D1),
            ok = aec_tx_pool:push(SignedTx),
            {ok, D2} = start_min_depth_watcher(deposit, SignedTx, D1),
            next_state(awaiting_locked, D2)
    end;
dep_half_signed(timeout, dep_half_signed = T, D) ->
    close({timeout, T}, D);
dep_half_signed(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
dep_half_signed({call, From}, Req, D) ->
    handle_call(dep_half_signed, Req, From, D).

dep_signed(enter, _OldSt, _D) -> keep_state_and_data;
dep_signed(cast, {?DEP_LOCKED, Msg}, #data{latest = {deposit, SignedTx}} = D) ->
    case check_deposit_locked_msg(Msg, SignedTx, D) of
        {ok, D1} ->
            report(info, deposit_locked, D1),
            deposit_locked_complete(SignedTx, D1#data{latest = undefined});
        {error, _} = Error ->
            close(Error, D)
    end;
dep_signed(timeout, dep_signed = T, D) ->
    close({timeout, T}, D);
dep_signed(cast, {?SHUTDOWN, _Msg}, D) ->
    next_state(closing, D);
dep_signed(cast, {?DISCONNECT, _Msg}, D) ->
    next_state(disconnected, D);
dep_signed({call, From}, Req, D) ->
    handle_call(dep_signed, Req, From, D).

deposit_locked_complete(SignedTx, #data{state = State, opts = Opts} = D) ->
    D1   = D#data{state = aesc_offchain_state:add_signed_tx(SignedTx, State, Opts)},
    next_state(open, D1).

wdraw_half_signed(enter, _OldSt, _D) -> keep_state_and_data;
wdraw_half_signed(cast, {?WDRAW_SIGNED, Msg}, D) ->
    case check_withdraw_signed_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report(on_chain_tx, SignedTx, D1),
            ok = aec_tx_pool:push(SignedTx),
            {ok, D2} = start_min_depth_watcher(withdraw, SignedTx, D1),
            next_state(awaiting_locked, D2)
    end;
wdraw_half_signed(timeout, wdraw_half_signed = T, D) ->
    close({timeout, T}, D);
wdraw_half_signed(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
wdraw_half_signed({call, From}, Req, D) ->
    handle_call(wdraw_half_signed, Req, From, D).

wdraw_signed(enter, _OldSt, _D) -> keep_state_and_data;
wdraw_signed(cast, {?WDRAW_LOCKED, Msg}, #data{latest = {withdraw, SignedTx}} = D) ->
    case check_withdraw_locked_msg(Msg, SignedTx, D) of
        {ok, D1} ->
            report(info, withdraw_locked, D1),
            withdraw_locked_complete(SignedTx, D1#data{latest = undefined});
        {error, _} = Error ->
            close(Error, D)
    end;
wdraw_signed(timeout, wdraw_signed = T, D) ->
    close({timeout, T}, D);
wdraw_signed(cast, {?SHUTDOWN, _Msg}, D) ->
    next_state(closing, D);
wdraw_signed(cast, {?DISCONNECT, _Msg}, D) ->
    next_state(disconnected, D);
wdraw_signed({call, From}, Req, D) ->
    handle_call(wdraw_signed, Req, From, D).

withdraw_locked_complete(SignedTx, #data{state = State, opts = Opts} = D) ->
    D1   = D#data{state = aesc_offchain_state:add_signed_tx(SignedTx, State, Opts)},
    next_state(open, D1).


awaiting_locked(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_locked(cast, {?MIN_DEPTH_ACHIEVED, ChainId, ?WATCH_FND, TxHash},
                #data{latest = {watch, ?WATCH_FND, TxHash, _}} = D) ->
    report(info, own_funding_locked, D),
    next_state(
      signed, send_funding_locked_msg(D#data{on_chain_id = ChainId,
                                             latest = undefined}));
awaiting_locked(cast, {?MIN_DEPTH_ACHIEVED, ChainId, ?WATCH_DEP, TxHash},
                #data{on_chain_id = ChainId,
                      latest = {watch, ?WATCH_DEP, TxHash, SignedTx}} = D) ->
    report(info, own_deposit_locked, D),
    next_state(
      dep_signed, send_deposit_locked_msg(
                    TxHash,
                    D#data{latest = {deposit, SignedTx}}));
awaiting_locked(cast, {?MIN_DEPTH_ACHIEVED, ChainId, ?WATCH_WDRAW, TxHash},
                #data{on_chain_id = ChainId,
                      latest = {watch, ?WATCH_WDRAW, TxHash, SignedTx}} = D) ->
    report(info, own_withdraw_locked, D),
    next_state(
      wdraw_signed, send_withdraw_locked_msg(
                      TxHash,
                      D#data{latest = {withdraw, SignedTx}}));
awaiting_locked(cast, {?FND_LOCKED, _Msg}, D) ->
    postpone(D);
awaiting_locked(cast, {?DEP_LOCKED, _Msg}, D) ->
    postpone(D);
awaiting_locked(cast, {?WDRAW_LOCKED, _Msg}, D) ->
    postpone(D);
awaiting_locked(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
awaiting_locked(timeout, awaiting_locked = T, D) ->
    close({timeout, T}, D).

awaiting_initial_state(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_initial_state(cast, {?UPDATE, Msg}, #data{role = responder} = D) ->
    lager:debug("got {update, ~p}", [Msg]),
    case check_update_msg(initial, Msg, D) of
        {ok, SignedTx, D1} ->
            lager:debug("update_msg checks out", []),
            report(info, update, D1),
            ok = request_signing(?UPDATE_ACK, aetx_sign:tx(SignedTx), D1),
            D2 = D1#data{latest = {sign, ?UPDATE_ACK, SignedTx}},
            next_state(awaiting_signature, D2);
        {error,_} = Error ->
            %% TODO: do we do a dispute challenge here?
            close(Error, D)
    end;
awaiting_initial_state(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
awaiting_initial_state(timeout, awaiting_initial_state = T, D) ->
    close({timeout, T}, D);
awaiting_initial_state(Evt, Msg, D) ->
    lager:debug("unexpected: awaiting_initial_state(~p, ~p, ~p)",
                [Evt, Msg, D]),
    close({unexpected, Msg}, D).

awaiting_update_ack(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_update_ack(cast, {?UPDATE_ACK, Msg}, #data{} = D) ->
    case check_update_ack_msg(Msg, D) of
        {ok, D1} ->
            next_state(open, D1);
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_update_ack(cast, {?UPDATE, _Msg}, D) ->
    D1 = send_update_err_msg(fallback_to_stable_state(D)),
    next_state(open, D1);
awaiting_update_ack(cast, {?UPDATE_ERR, Msg}, D) ->
    lager:debug("received update_err: ~p", [Msg]),
    case check_update_err_msg(Msg, D) of
        {ok, D1} ->
            report(conflict, Msg, D1),
            next_state(open, D1);
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_update_ack(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
awaiting_update_ack(timeout, awaiting_update_ack = T, D) ->
    close({timeout, T}, D);
awaiting_update_ack({call, _}, _Req, D) ->
    postpone(D).

awaiting_deposit_update(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_deposit_update(cast, {?UPDATE, Msg}, D) ->
    case check_deposit_update_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report(info, deposit_update, D1),
            ok = request_signing(?UPDATE_ACK, aetx_sign:tx(SignedTx), D1),
            D2 = D1#data{latest = {sign, ?UPDATE_ACK, SignedTx}},
            next_state(awaiting_signature, D2);
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_deposit_update(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
awaiting_deposit_update(timeout, awaiting_deposit_update = T, D) ->
    close({timeout, T}, D);
awaiting_deposit_update(Evt, Msg, D) ->
    lager:debug("unexpected: awaiting_deposit_update(~p, ~p, ~p)", [Evt, Msg, D]),
    close({unexpected, Msg}, D).

awaiting_withdraw_update(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_withdraw_update(cast, {?UPDATE, Msg}, D) ->
    case check_withdraw_update_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report(info, withdraw_update, D1),
            ok = request_signing(?UPDATE_ACK, aetx_sign:tx(SignedTx), D1),
            D2 = D1#data{latest = {sign, ?UPDATE_ACK, SignedTx}},
            next_state(awaiting_signature, D2);
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_withdraw_update(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
awaiting_withdraw_update(timeout, awaiting_withdraw_update = T, D) ->
    close({timeout, T}, D);
awaiting_withdraw_update(Evt, Msg, D) ->
    lager:debug("unexpected: awaiting_withdraw_update(~p, ~p, ~p)", [Evt, Msg, D]),
    close({unexpected, Msg}, D).

signed(enter, _OldSt, _D) -> keep_state_and_data;
signed(cast, {?FND_LOCKED, Msg}, D) ->
    case check_funding_locked_msg(Msg, D) of
        {ok, D1} ->
            report(info, funding_locked, D1),
            funding_locked_complete(D1);
        {error, _} = Error ->
            close(Error, D)
    end;
signed(timeout, signed = T, D) ->
    close({timeout, T}, D);
signed(cast, {?SHUTDOWN, _Msg}, D) ->
    next_state(closing, D);
signed(cast, {?DISCONNECT, _Msg}, D) ->
    next_state(disconnected, D);
signed({call, From}, Req, D) ->
    handle_call(signed, Req, From, D).


funding_locked_complete(D) ->
    D1   = D#data{state = aesc_offchain_state:add_signed_tx(D#data.create_tx,
                                                            D#data.state,
                                                            D#data.opts)},
    next_state(open, D1).

open(enter, _OldSt, D) ->
    D1 = if D#data.channel_status =/= open ->
                 report(info, open, D),
                 report_update(D#data{channel_status = open});
            true ->
                 report_update(D)
         end,
    keep_state(D1);
open(cast, {?UPDATE, Msg}, D) ->
    case check_update_msg(normal, Msg, D) of
        {ok, SignedTx, D1} ->
            report(info, update, D1),
            ok = request_signing(?UPDATE_ACK, aetx_sign:tx(SignedTx), D1),
            D2 = D1#data{latest = {sign, ?UPDATE_ACK, SignedTx}},
            next_state(awaiting_signature, D2);
        {error,_} = Error ->
            %% TODO: do we do a dispute challenge here?
            close(Error, D)
    end;
open(cast, {?DEP_CREATED, Msg}, D) ->
    case check_deposit_created_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report(info, deposit_created, D1),
            lager:debug("deposit_created: ~p", [SignedTx]),
            ok = request_signing(?DEP_CREATED, aetx_sign:tx(SignedTx), D1),
            D2 = D1#data{latest = {sign, ?DEP_CREATED, SignedTx}},
            next_state(awaiting_signature, D2)
    end;
open(cast, {?WDRAW_CREATED, Msg}, D) ->
    case check_withdraw_created_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report(info, withdraw_created, D1),
            lager:debug("withdraw_created: ~p", [SignedTx]),
            ok = request_signing(?WDRAW_CREATED, aetx_sign:tx(SignedTx), D1),
            D2 = D1#data{latest = {sign, ?WDRAW_CREATED, SignedTx}},
            next_state(awaiting_signature, D2)
    end;
open(cast, {?INBAND_MSG, Msg}, D) ->
    NewD = case check_inband_msg(Msg, D) of
               {ok, D1} ->
                   report(message, Msg, D1),
                   D1;
               {error, _} = Err ->
                   lager:error("Error in inband_msg: ~p", [Err]),
                   D
           end,
    keep_state(NewD);
open({call, From}, Request, D) ->
    handle_call(open, Request, From, D);
open(cast, {?SHUTDOWN, Msg}, D) ->
    case check_shutdown_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            ok = request_signing(?SHUTDOWN_ACK, aetx_sign:tx(SignedTx), D1),
            D2 = D1#data{latest = {sign, ?SHUTDOWN_ACK, SignedTx}},
            next_state(awaiting_signature, D2);
        {error, E} ->
            close({shutdown_error, E}, D)
    end;
open(cast, {?DISCONNECT, _Msg}, D) ->
    next_state(disconnected, D);
open(cast, _Msg, D) ->
    lager:error("Discarding cast in 'open' state: ~p", [_Msg]),
    keep_state(D);
open(info, Msg, D) ->
    handle_info(Msg, D);
open(timeout, open = T, D) ->
    close({timeout, T}, D).

closing(enter, _OldSt, _D) -> keep_state_and_data;
closing(cast, {?SHUTDOWN_ACK, Msg}, D) ->
    case check_shutdown_ack_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            lager:debug("shutdown_ack ok", []),
            ok = aec_tx_pool:push(SignedTx),
            report(on_chain_tx, SignedTx, D1),
            close(close_mutual, D1);
        {error, _} = Error ->
            close(Error, D)
    end;
closing(cast, {?DISCONNECT, _Msg}, D) ->
    next_state(disconnected, D);
closing(cast, {closing_signed, _Msg}, D) ->  %% TODO: not using this, right?
    close(closing_signed, D).

disconnected(cast, {?CH_REESTABL, _Msg}, D) ->
    next_state(closing, D).

close(close_mutual, D) ->
    report(info, close_mutual, D),
    {stop, normal, D};
close(Reason, D) ->
    try send_error_msg(Reason, D)
    catch error:_ -> ignore
    end,
    {stop, Reason, D}.

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

handle_call(St, Req, From, #data{} = D) ->
    lager:debug("handle_call(~p, ~p, ~p, ~p)", [St, Req, From, D]),
    handle_call_(St, Req, From, D).

handle_call_(open, {upd_transfer, FromPub, ToPub, Amount}, From,
            #data{opts = #{initiator := I,
                           responder := R}} = D) ->
    case FromPub =/= ToPub andalso ([] == [FromPub, ToPub] -- [I, R]) of
        true ->
            handle_upd_transfer(FromPub, ToPub, Amount, From, D);
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
    {ok, DepTx} = dep_tx_for_signing(Opts#{from => FromPub}, D),
    ok = request_signing(deposit_tx, DepTx, D),
    gen_statem:reply(From, ok),
    D1 = D#data{latest = {sign, deposit_tx, DepTx}},
    next_state(awaiting_signature, D1);
handle_call_(open, {upd_withdraw, #{amount := Amt} = Opts}, From,
             #data{} = D) when is_integer(Amt), Amt > 0 ->
    ToPub = my_account(D),
    case maps:find(to, Opts) of
        {ok, ToPub} -> ok;
        {ok, _Other}  -> error(conflicting_accounts);
        error         -> ok
    end,
    {ok, DepTx} = wdraw_tx_for_signing(Opts#{to => ToPub}, D),
    ok = request_signing(withdraw_tx, DepTx, D),
    gen_statem:reply(From, ok),
    D1 = D#data{latest = {sign, withdraw_tx, DepTx}},
    next_state(awaiting_signature, D1);
handle_call_(open, shutdown, From, #data{state = State} = D) ->
    try  {_Round, Latest} = aesc_offchain_state:get_latest_signed_tx(State),
         {ok, CloseTx} = close_mutual_tx(Latest, D),
         ok = request_signing(?SHUTDOWN, CloseTx, D),
         gen_statem:reply(From, ok),
         D1 = D#data{latest = {sign, ?SHUTDOWN, CloseTx}},
         next_state(awaiting_signature, D1)
    catch
        error:E ->
            keep_state(D, [{reply, From, {error, E}}])
    end;
handle_call_(open, {inband_msg, ToPub, Msg}, From, #data{} = D) ->
    case {ToPub, other_account(D)} of
        {X, X} ->
            keep_state(send_inband_msg(ToPub, Msg, D), [{reply, From, ok}]);
        _ ->
            keep_state(D, [{reply, From, {error, unknown_recipient}}])
    end;
handle_call_(St, _Req, _From, D) when ?TRANSITION_STATE(St) ->
    postpone(D);
handle_call_(_St, _Req, From, D) ->
    keep_state(D, [{reply, From, {error, unknown_request}}]).

handle_info(Msg, #data{cur_statem_state = St} = D) ->
    lager:debug("Discarding info in ~p state: ~p", [St, Msg]),
    keep_state(D).

cur_channel_id(#data{channel_id = TChId,
                     on_chain_id = PChId}) ->
    case PChId == undefined of
        true  -> TChId;
        false -> PChId
    end.

error_binary(E) when is_atom(E) ->
    atom_to_binary(E, latin1).


terminate(Reason, _State, Data) ->
    report(info, {died, Reason}, Data),
    ok.

code_change(_OldVsn, OldState, OldData, _Extra) ->
    {ok, OldState, OldData}.

%% ======================================================================
%% Action logic
%% ======================================================================

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
    ChannelId = aesc_channels:id(Initiator,
                                 erlang:unique_integer(),
                                 Responder),
    Msg = #{ chain_hash           => ChainHash
           , temporary_channel_id => ChannelId
           , lock_period          => LockPeriod
           , push_amount          => PushAmount
           , initiator_amount     => InitiatorAmount
           , responder_amount     => ResponderAmount
           , channel_reserve      => ChannelReserve
           , initiator            => Initiator
           },
    aesc_session_noise:channel_open(Sn, Msg),
    Data#data{channel_id = ChannelId}.

check_open_msg(#{ chain_hash           := ChainHash
                , temporary_channel_id := ChanId
                , lock_period          := LockPeriod
                , push_amount          := PushAmt
                , initiator_amount     := InitiatorAmt
                , responder_amount     := ResponderAmt
                , channel_reserve      := ChanReserve
                , initiator            := InitiatorPubkey},
               #data{opts = Opts} = Data) ->
    %% TODO: Implement more checks
    case aec_chain:genesis_hash() of
        ChainHash ->
            Opts1 =
                Opts#{ lock_period        => LockPeriod
                     , initiator          => InitiatorPubkey
                     , push_amount        => PushAmt
                     , initiator_amount   => InitiatorAmt
                     , responder_amount   => ResponderAmt
                     , channel_reserve    => ChanReserve},
            {ok, Data#data{channel_id = ChanId,
                           opts       = Opts1}};
        _ ->
            {error, chain_hash_mismatch}
    end.

send_channel_accept(#data{opts          = Opts,
                          session       = Sn,
                          channel_id    = ChanId} = Data) ->
    #{ minimum_depth      := MinDepth
     , responder        := Responder
     , initiator_amount   := InitiatorAmt
     , responder_amount := ResponderAmt
     , channel_reserve    := ChanReserve } = Opts,
    ChainHash = aec_chain:genesis_hash(),
    Msg = #{ chain_hash           => ChainHash
           , temporary_channel_id => ChanId
           , minimum_depth        => MinDepth
           , initiator_amount     => InitiatorAmt
           , responder_amount     => ResponderAmt
           , channel_reserve      => ChanReserve
           , responder            => Responder
           },
    aesc_session_noise:channel_accept(Sn, Msg),
    Data.

check_accept_msg(#{ chain_hash           := ChainHash
                  , temporary_channel_id := ChanId
                  , minimum_depth        := MinDepth
                  , initiator_amount     := _InitiatorAmt
                  , responder_amount     := _ResponderAmt
                  , channel_reserve      := _ChanReserve
                  , responder            := Responder},
                 #data{channel_id = ChanId,
                       opts = Opts} = Data) ->
    %% TODO: implement more checks
    case aec_chain:genesis_hash() of
        ChainHash ->
            {ok, Data#data{opts = Opts#{ minimum_depth => MinDepth
                                       , responder     => Responder}}};
        _ ->
            {error, chain_hash_mismatch}
    end.

dep_tx_for_signing(#{from := From} = Opts, #data{on_chain_id = ChanId, state=State}) ->
    StateHash = aesc_offchain_state:hash(State),
    {LastRound, _} = aesc_offchain_state:get_latest_signed_tx(State),
    Def = deposit_tx_defaults(ChanId, From, maps:get(ttl, Opts, undefined)),
    Opts1 = maps:merge(Def, Opts),
    Opts2 = maps:merge(Opts1, #{state_hash => StateHash,
                                round      => LastRound + 1}),
    lager:debug("deposit_tx Opts = ~p", [Opts2]),
    {ok, _} = Ok = aesc_deposit_tx:new(Opts2),
    Ok.

wdraw_tx_for_signing(#{to := From} = Opts, #data{on_chain_id = ChanId, state=State}) ->
    StateHash = aesc_offchain_state:hash(State),
    {LastRound, _} = aesc_offchain_state:get_latest_signed_tx(State),
    Def = withdraw_tx_defaults(ChanId, From, maps:get(ttl, Opts, undefined)),
    Opts1 = maps:merge(Def, Opts),
    Opts2 = maps:merge(Opts1, #{state_hash => StateHash,
                                round      => LastRound + 1}),
    lager:debug("withdraw_tx Opts = ~p", [Opts2]),
    {ok, _} = Ok = aesc_withdraw_tx:new(Opts2),
    Ok.

create_tx_for_signing(#data{opts = #{initiator := Initiator} = Opts, state=State}) ->
    StateHash = aesc_offchain_state:hash(State),
    Def = create_tx_defaults(Initiator),
    Opts1 = maps:merge(Def, Opts),
    lager:debug("create_tx Opts = ~p", [Opts1]),
    {ok, _} = Ok = aesc_create_tx:new(Opts1#{state_hash => StateHash}),
    Ok.

create_tx_defaults(Initiator) ->
    {ok, Nonce} = aec_next_nonce:pick_for_account(Initiator),
    Fee = aec_governance:minimum_tx_fee(),
    #{ fee   => Fee
     , nonce => Nonce }.

deposit_tx_defaults(ChanId, Acct, TTL) ->
    maps:merge(
      create_tx_defaults(Acct),
      #{ ttl        => adjust_ttl(TTL)
       , channel_id => ChanId }).

withdraw_tx_defaults(ChanId, Acct, TTL) ->
    maps:merge(
      create_tx_defaults(Acct),
      #{ ttl        => adjust_ttl(TTL)
       , channel_id => ChanId }).

adjust_ttl(undefined) ->
    CurHeight = aec_headers:height(aec_chain:top_header()),
    CurHeight + 100;
adjust_ttl(TTL) when is_integer(TTL), TTL > 0 ->
    TTL.

close_mutual_tx(LatestSignedTx, D) ->
    Account = my_account(D),
    Initiator = initiator_account(D),
    {ok, Nonce} = aec_next_nonce:pick_for_account(Initiator),
    close_mutual_tx(Account, Nonce, LatestSignedTx, D).

close_mutual_tx(Account, Nonce, _LatestSignedTx,
                #data{ on_chain_id = ChanId
                     , opts        = Opts
                     , state       = State} = D) ->
    Def = close_mutual_defaults(Account, D),
    Opts1 = maps:merge(Def, Opts),
    #{initiator := Initiator,
      responder := Responder} = Opts,
    {ok, IAmt} = aesc_offchain_state:balance(Initiator, State),
    {ok, RAmt} = aesc_offchain_state:balance(Responder, State),
    Fee = maps:get(fee, Opts1),
    TTL = maps:get(ttl, Opts1, 0), %% 0 means no TTL limit
    {IAmt1, RAmt1} = pay_close_mutual_fee(Fee, IAmt, RAmt),
    {LastRound, _} = aesc_offchain_state:get_latest_signed_tx(State),
    StateHash = aesc_offchain_state:hash(State),
    aesc_close_mutual_tx:new(#{ channel_id       => ChanId
                              , initiator_amount => IAmt1
                              , responder_amount => RAmt1
                              , ttl              => TTL
                              , fee              => Fee
                              , state_hash       => StateHash
                              , round            => LastRound + 1
                              , nonce            => Nonce }).

pay_close_mutual_fee(Fee, IAmt, RAmt) ->
    Ceil  = trunc(math:ceil(Fee/2)),
    Floor = trunc(math:floor(Fee/2)),
    if (IAmt + RAmt) < Fee                    -> erlang:error(insufficient_funds);
       (IAmt >= Ceil) andalso (RAmt >= Floor) -> {IAmt - Ceil, RAmt - Floor};
       (RAmt >= Ceil) andalso (IAmt >= Floor) -> {IAmt - Floor, RAmt - Ceil};
       (IAmt > RAmt)                          -> {IAmt - Fee + RAmt, 0};
       true                                   -> {0, RAmt - Fee + IAmt}
    end.

close_mutual_defaults(_Account, _D) ->
    Fee = aec_governance:minimum_tx_fee(),
    #{ fee   => Fee }.

my_account(#data{role = initiator, opts = #{initiator := I}}) -> I;
my_account(#data{role = responder, opts = #{responder := R}}) -> R.

other_account(#data{role = initiator, opts = #{responder := R}}) -> R;
other_account(#data{role = responder, opts = #{initiator := I}}) -> I.

initiator_account(#data{opts = #{initiator := I}}) -> I.

send_funding_created_msg(SignedTx, #data{channel_id = Ch,
                                         session    = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ temporary_channel_id => Ch
           ,  data                => TxBin},
    aesc_session_noise:funding_created(Sn, Msg),
    Data.

check_funding_created_msg(#{ temporary_channel_id := ChanId
                           , data                 := TxBin},
                          #data{channel_id = ChanId} = Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    {ok, SignedTx, Data}.

send_funding_signed_msg(SignedTx, #data{channel_id = Ch,
                                        session    = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ temporary_channel_id  => Ch
           , data                  => TxBin},
    aesc_session_noise:funding_signed(Sn, Msg),
    Data.

check_funding_signed_msg(#{ temporary_channel_id := ChanId
                          , data                 := TxBin},
                          #data{channel_id = ChanId} = Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    {ok, SignedTx, Data}.

send_funding_locked_msg(#data{channel_id  = TmpChanId,
                              on_chain_id = OnChainId,
                              session     = Sn} = Data) ->
    Msg = #{ temporary_channel_id => TmpChanId
           , channel_id           => OnChainId },
    aesc_session_noise:funding_locked(Sn, Msg),
    Data.

check_funding_locked_msg(#{ temporary_channel_id := TmpChanId
                          , channel_id           := OnChainId },
                         #data{channel_id  = MyTmpChanId,
                               on_chain_id = MyOnChainId} = Data) ->
    case TmpChanId == MyTmpChanId of
        true ->
            case OnChainId == MyOnChainId of
                true ->
                    {ok, Data};
                false ->
                    {error, channel_id_mismatch}
            end;
        false ->
            {error, temporary_channel_id_mismatch}
    end.

send_deposit_created_msg(SignedTx, #data{on_chain_id = Ch,
                                         session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id => Ch
           , data       => TxBin},
    aesc_session_noise:deposit_created(Sn, Msg),
    Data.

check_deposit_created_msg(#{ channel_id := ChanId
                           , data       := TxBin},
                          #data{on_chain_id = ChanId} = Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    {ok, SignedTx, Data}.

send_deposit_signed_msg(SignedTx, #data{on_chain_id = Ch,
                                        session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id  => Ch
           , data        => TxBin},
    aesc_session_noise:deposit_signed(Sn, Msg),
    Data.

check_deposit_signed_msg(#{ channel_id := ChanId
                          , data       := TxBin},
                          #data{on_chain_id = ChanId} = Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    {ok, SignedTx, Data}.

send_deposit_locked_msg(TxHash, #data{on_chain_id = ChanId,
                                      session     = Sn} = Data) ->
    Msg = #{ channel_id => ChanId
           , data       => TxHash },
    aesc_session_noise:deposit_locked(Sn, Msg),
    Data.

check_deposit_locked_msg(#{ channel_id := ChanId
                          , data       := TxHash },
                         SignedTx,
                         #data{on_chain_id = MyChanId} = Data) ->
    case ChanId == MyChanId of
        true ->
            case aetx_sign:hash(SignedTx) of
                TxHash ->
                    {ok, Data};
                _ ->
                    {error, deposit_tx_hash_mismatch}
            end;
        false ->
            {error, channel_id_mismatch}
    end.

check_deposit_update_msg(#{ channel_id := ChanId
                          , data       := TxBin },
                         #data{ on_chain_id = ChanId
                              , latest = {deposit, SignedDepTx}
                              , state = State
                              , opts  = Opts } = D) ->
    try aetx_sign:deserialize_from_binary(TxBin) of
        SignedUpdTx ->
            UpdTx = aetx_sign:tx(SignedUpdTx),
            {ModU, TxUI} = aetx:specialize_callback(aetx_sign:tx(SignedUpdTx)),
            PrevRound = ModU:previous_round(TxUI),
            case aesc_offchain_state:get_latest_signed_tx(State) of
                {PrevRound, _SignedStTx} ->
                    {ModD, TxDI} = aetx:specialize_callback(
                                     aetx_sign:tx(SignedDepTx)),
                    From = ModD:origin(TxDI),
                    Amount = ModD:amount(TxDI),
                    Updates = [aesc_offchain_state:op_deposit(From, Amount)],
                    NewStTx = aesc_offchain_state:make_update_tx(Updates, State, Opts),
                    case NewStTx == UpdTx of
                        true ->
                            {ok, SignedUpdTx, D};
                        false ->
                            {error, invalid_deposit_update}
                    end;
                _ ->
                    {error, invalid_previous_round}
            end
    catch
        error:E ->
            lager:error("CAUGHT ~p, trace = ~p", [E, erlang:get_stacktrace()])
    end.

send_withdraw_created_msg(SignedTx, #data{on_chain_id = Ch,
                                          session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id => Ch
           , data       => TxBin},
    aesc_session_noise:wdraw_created(Sn, Msg),
    Data.

check_withdraw_created_msg(#{ channel_id := ChanId
                            , data       := TxBin},
                           #data{on_chain_id = ChanId} = Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    {ok, SignedTx, Data}.

send_withdraw_signed_msg(SignedTx, #data{on_chain_id = Ch,
                                         session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id  => Ch
           , data        => TxBin},
    aesc_session_noise:wdraw_signed(Sn, Msg),
    Data.

check_withdraw_signed_msg(#{ channel_id := ChanId
                           , data       := TxBin},
                          #data{on_chain_id = ChanId} = Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    {ok, SignedTx, Data}.

send_withdraw_locked_msg(TxHash, #data{on_chain_id = ChanId,
                                       session     = Sn} = Data) ->
    Msg = #{ channel_id => ChanId
           , data       => TxHash },
    aesc_session_noise:wdraw_locked(Sn, Msg),
    Data.

check_withdraw_locked_msg(#{ channel_id := ChanId
                           , data       := TxHash },
                          SignedTx,
                          #data{on_chain_id = MyChanId} = Data) ->
    case ChanId == MyChanId of
        true ->
            case aetx_sign:hash(SignedTx) of
                TxHash ->
                    {ok, Data};
                _ ->
                    {error, withdraw_tx_hash_mismatch}
            end;
        false ->
            {error, channel_id_mismatch}
    end.

check_withdraw_update_msg(#{ channel_id := ChanId
                           , data       := TxBin },
                          #data{ on_chain_id = ChanId
                               , latest = {withdraw, SignedDepTx}
                               , state = State
                               , opts  = Opts } = D) ->
    try aetx_sign:deserialize_from_binary(TxBin) of
        SignedUpdTx ->
            UpdTx = aetx_sign:tx(SignedUpdTx),
            {ModU, TxUI} = aetx:specialize_callback(aetx_sign:tx(SignedUpdTx)),
            PrevRound = ModU:previous_round(TxUI),
            case aesc_offchain_state:get_latest_signed_tx(State) of
                {PrevRound, _SignedStTx} ->
                    {ModD, TxDI} = aetx:specialize_callback(
                                     aetx_sign:tx(SignedDepTx)),
                    From = ModD:origin(TxDI),
                    Amount = ModD:amount(TxDI),
                    Updates = [aesc_offchain_state:op_withdraw(From, Amount)],
                    NewStTx = aesc_offchain_state:make_update_tx(Updates, State, Opts),
                    case NewStTx == UpdTx of
                        true ->
                            {ok, SignedUpdTx, D};
                        false ->
                            {error, invalid_withdraw_update}
                    end;
                _ ->
                    {error, invalid_previous_round}
            end
    catch
        error:E ->
            lager:error("CAUGHT ~p, trace = ~p", [E, erlang:get_stacktrace()])
    end.

send_update_msg(SignedTx, #data{ on_chain_id = OnChainId
                               , session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id => OnChainId
           , data       => TxBin },
    aesc_session_noise:update(Sn, Msg),
    Data.

check_update_msg(Type, Msg, D) ->
    lager:debug("check_update_msg(~p)", [Msg]),
    try check_update_msg_(Type, Msg, D)
    catch
        error:E ->
            lager:error("CAUGHT ~p, Trace = ~p", [E, erlang:get_stacktrace()]),
            {error, E}
    end.

check_update_msg_(Type, #{ channel_id := ChanId
                         , data       := TxBin },
                  #data{ on_chain_id = ChanId } = D) ->
    try aetx_sign:deserialize_from_binary(TxBin) of
        SignedTx ->
            check_signed_update_tx(Type, SignedTx, D)
    catch
        error:E ->
            lager:error("CAUGHT ~p, trace = ~p", [E, erlang:get_stacktrace()]),
            {error, {deserialize, E}}
    end.

check_signed_update_tx(Type, SignedTx, #data{state = State, opts = Opts} = D) ->
    lager:debug("check_signed_update_tx(~p)", [SignedTx]),
    case check_update_tx(Type, SignedTx, State, Opts) of
        ok ->
            {ok, SignedTx, D};
        {error, _} = Error ->
            Error
    end.

check_update_tx(initial, SignedTx, State, Opts) ->
    aesc_offchain_state:check_initial_update_tx(SignedTx, State, Opts);
check_update_tx(normal, SignedTx, State, Opts) ->
    aesc_offchain_state:check_update_tx(SignedTx, State, Opts).


check_update_ack_msg(Msg, D) ->
    lager:debug("check_update_ack_msg(~p)", [Msg]),
    try check_update_ack_msg_(Msg, D)
    catch
        error:E ->
            lager:error("CAUGHT ~p, Trace = ~p", [E, erlang:get_stacktrace()]),
            {error, E}
    end.

check_update_ack_msg_(#{ channel_id := ChanId
                       , data       := TxBin },
                      #data{on_chain_id = ChanId} = D) ->
    try aetx_sign:deserialize_from_binary(TxBin) of
        SignedTx ->
            check_signed_update_ack_tx(SignedTx, D)
    catch
        error:E ->
            lager:error("CAUGHT ~p, trace = ~p", [E, erlang:get_stacktrace()]),
            {error, {deserialize, E}}
    end.

check_signed_update_ack_tx(SignedTx, #data{state = State, opts = Opts} = D) ->
    HalfSignedTx = aesc_offchain_state:get_latest_half_signed_tx(State),
    try  ok = check_update_ack_(SignedTx, HalfSignedTx),
         {ok, D#data{state = aesc_offchain_state:add_signed_tx(SignedTx, State, Opts)}}
    catch
        error:E ->
            lager:error("CAUGHT ~p, trace = ~p", [E, erlang:get_stacktrace()]),
            {error, invalid_update_ack}
    end.

check_update_ack_(SignedTx, HalfSignedTx) ->
    HalfSigs = aetx_sign:signatures(HalfSignedTx),
    lager:debug("HalfSigs = ~p", [HalfSigs]),
    Sigs = aetx_sign:signatures(SignedTx),
    lager:debug("Sigs = ~p", [Sigs]),
    Remainder = Sigs -- HalfSigs,
    lager:debug("Remainder = ~p", [Remainder]),
    true = (aetx_sign:tx(SignedTx) == aetx_sign:tx(HalfSignedTx)),
    lager:debug("Txes are the same", []),
    ok.

handle_upd_transfer(FromPub, ToPub, Amount, From, #data{ state = State
                                                       , opts = Opts } = D) ->
    Updates = [aesc_offchain_state:op_transfer(FromPub, ToPub, Amount)],
    try  Tx1 = aesc_offchain_state:make_update_tx(Updates, State, Opts),
         ok = request_signing(?UPDATE, Tx1, D),
         gen_statem:reply(From, ok),
         D1 = D#data{latest = {sign, ?UPDATE, Tx1}},
         next_state(awaiting_signature, D1)
    catch
        error:Reason ->
            lager:error("CAUGHT ~p, trace = ~p", [Reason, erlang:get_stacktrace()]),
            keep_state(D, [{reply, From, {error, Reason}}])
    end.

send_shutdown_msg(SignedTx, #data{session = Session} = Data) ->
    Msg = shutdown_msg(SignedTx, Data),
    aesc_session_noise:shutdown(Session, Msg),
    Data.

send_shutdown_ack_msg(SignedTx, #data{session = Session} = Data) ->
    Msg = shutdown_msg(SignedTx, Data),
    aesc_session_noise:shutdown_ack(Session, Msg),
    Data.

shutdown_msg(SignedTx, #data{ on_chain_id = OnChainId }) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    #{ channel_id => OnChainId
     , data       => TxBin }.


check_shutdown_msg(#{channel_id := ChanId, data := TxBin} = _Msg,
                   #data{on_chain_id = ChanId, state = State} = D) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    {_, LatestSignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    OtherAcct = other_account(D),
    {ok, FakeCloseTx} = close_mutual_tx(OtherAcct, 0, LatestSignedTx, D),
    RealCloseTx = aetx_sign:tx(SignedTx),
    {channel_close_mutual_tx, FakeTxI} = aetx:specialize_type(FakeCloseTx),
    {channel_close_mutual_tx, RealTxI} = aetx:specialize_type(RealCloseTx),
    case (serialize_close_mutual_tx(FakeTxI) ==
              serialize_close_mutual_tx(RealTxI)) of
        true ->
            {ok, SignedTx, D};
        false ->
            {error, shutdown_tx_validation}
    end.

serialize_close_mutual_tx(Tx) ->
    {_, Elems} = aesc_close_mutual_tx:serialize(Tx),
    lists:keydelete(nonce, 1, Elems).

check_shutdown_ack_msg(#{data := TxBin} = _Msg,
                       #data{latest = {shutdown, MySignedTx}} = D) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    case aetx_sign:signatures(SignedTx) of
        [_,_] ->
            check_shutdown_msg_(SignedTx, MySignedTx, D);
        _ ->
            {error, not_mutually_signed}
    end.

check_shutdown_msg_(SignedTx, MySignedTx, D) ->
    %% TODO: More thorough checking
    case (aetx_sign:tx(SignedTx) == aetx_sign:tx(MySignedTx)) of
        true ->
            {ok, SignedTx, D};
        false ->
            {error, shutdown_tx_mismatch}
    end.

send_inband_msg(To, Info, #data{on_chain_id = ChanId,
                                session     = Session} = D) ->
    From = my_account(D),
    M = #{ channel_id => ChanId
         , from       => From
         , to         => To
         , info       => Info },
    aesc_session_noise:inband_msg(Session, M),
    D.

check_inband_msg(#{ channel_id := ChanId
                  , from       := From
                  , to         := To }, #data{on_chain_id = ChanId} = D) ->
    case {my_account(D), other_account(D)} of
        {To, From}     ->  {ok, D};
        {To, _Other}   ->  {error, invalid_sender};
        {_Other, From} ->  {error, invalid_recipient};
        _ ->
            {error, invalid_addresses}
    end;
check_inband_msg(_, _) ->
    {error, chain_id_mismatch}.

fallback_to_stable_state(#data{state = State} = D) ->
    D#data{state = aesc_offchain_state:fallback_to_stable_state(State)}.



tx_round(Tx)            -> call_cb(Tx, round, []).

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
           , data       => TxBin },
    aesc_session_noise:update_ack(Sn, Msg),
    Data.

send_update_err_msg(#data{ state = State
                         , on_chain_id = ChanId
                         , session     = Sn } = Data) ->
    {_, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    Round = tx_round(aetx_sign:tx(SignedTx)),
    Msg = #{ channel_id => ChanId
           , round      => Round },
    aesc_session_noise:update_error(Sn, Msg),
    report(conflict, Msg, Data),
    Data.

check_update_err_msg(#{ channel_id := ChanId
                      , round      := Round },
                     #data{on_chain_id = ChanId0,
                           state = State} = D) ->
    case ChanId == ChanId0 of
        true ->
            case aesc_offchain_state:get_fallback_state(State) of
                {Round, State1} ->
                    {ok, D#data{state = State1}};
                _Other ->
                    lager:debug("Fallback state mismatch: ~p/~p",
                                [Round, _Other]),
                    {error, fallback_state_mismatch}
            end;
        false ->
            {error, chain_id_mismatch}
    end.

request_signing(Tag, Obj, #data{client = Client} = D) ->
    Msg = rpt_message(sign, Tag, Obj, D),
    Client ! {?MODULE, self(), Msg},
    lager:debug("signing(~p) requested", [Tag]),
    ok.

default_minimum_depth(initiator  ) -> undefined;
default_minimum_depth(responder) -> ?MINIMUM_DEPTH.

start_min_depth_watcher(Type, SignedTx,
                        #data{watcher = Watcher0,
                              opts = #{initiator     := Initiator,
                                       responder     := Responder,
                                       minimum_depth := MinDepth}} = D) ->
    Tx = aetx_sign:tx(SignedTx),
    TxHash = aetx_sign:hash(SignedTx),
    evt({tx_hash, TxHash}),
    Nonce = aetx:nonce(Tx),
    evt({nonce, Nonce}),
    {OnChainId, D1} = on_chain_id(D, Initiator, Nonce, Responder),
    case {Type, Watcher0} of
        {funding, undefined} ->
            {ok, Watcher1} = aesc_fsm_min_depth_watcher:start_link(
                               Type, TxHash, OnChainId, MinDepth),
            evt({watcher, Watcher1}),
            {ok, D1#data{watcher = Watcher1,
                         latest = {watch, Type, TxHash, SignedTx}}};
        {_, Pid} when Pid =/= undefined ->  % assertion
            ok = aesc_fsm_min_depth_watcher:watch(Pid, Type, TxHash, MinDepth),
            {ok, D1#data{latest = {watch, Type, TxHash, SignedTx}}}
    end.

on_chain_id(#data{on_chain_id = ID} = D, _, _, _) when ID =/= undefined ->
    {ID, D};
on_chain_id(D, Initiator, Nonce, Responder) ->
    ID = aesc_channels:id(Initiator, Nonce, Responder),
    evt({on_chain_id, ID}),
    {ID, D#data{on_chain_id = ID}}.


gproc_register(#data{role = Role, channel_id = ChanId}) ->
    gproc:reg(gproc_name(ChanId, Role)).

gproc_name(Id, Role) ->
    {n, l, {aesc_channel, {Id, Role}}}.

evt(_Msg) ->
    ok.

report_update(#data{state = State, last_reported_update = Last} = D) ->
    case aesc_offchain_state:get_latest_signed_tx(State) of
        {Last, _} ->
            D;
        {New, SignedTx} ->
            report(update, SignedTx, D),
            D#data{last_reported_update = New}
    end.

report_tags() ->
    [info, update, conflict, message, error, on_chain_tx].

default_report_flags() ->
    #{ info         => true
     , update       => true
     , conflict     => true
     , message      => true
     , error        => true
     , on_chain_tx  => true}.

report(Tag, St, D) -> report_info(do_rpt(Tag, D), Tag, St, D).

report_info(DoRpt, Tag, Info, #data{client = Client} = D) ->
    if DoRpt ->
            Msg = rpt_message(report, Tag, Info, D),
            lager:debug("report_info(true, ~p)", [Msg]),
            Client ! {?MODULE, self(), Msg};
       true  ->
            lager:debug("report_info(~p, ~p, ~p)", [DoRpt, Tag, Info]),
            ok
    end,
    ok.

rpt_message(Type, Tag, Info, #data{on_chain_id = undefined}) ->
    #{ type            => Type
     , tag             => Tag
     , info            => Info };
rpt_message(Type, Tag, Info, #data{on_chain_id = OnChainId}) ->
    #{ channel_id      => OnChainId
     , type            => Type
     , tag             => Tag
     , info            => Info }.

do_rpt(Tag, #data{opts = #{report := Rpt}}) ->
    try maps:get(Tag, Rpt, false)
    catch
        error:_ ->
            false
    end.
