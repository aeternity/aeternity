-module(aesc_fsm).
-behaviour(gen_statem).

-export([start_link/1]).

%% API
-export([initiate/3,     %% (host(), port(), Opts :: #{})
         respond/2,      %% (port(), Opts :: #{})
         upd_transfer/4, %% (fsm() , from(), to(), amount())
         shutdown/1,     %% (fsm())
         client_died/1,  %% (fsm())
         inband_msg/3]).

%% Used by noise session
-export([message/2]).

%% Used by client
-export([signing_response/3]).    %% (Fsm, Tag, Obj)

%% Used by min-depth watcher
-export([own_funding_locked/2]).  %% (Fsm, OnChainId)

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
         half_signed/3,
         signed/3,
         open/3,
         closing/3,
         disconnected/3]).

-export([timeouts/0]).

-include_lib("apps/aecore/include/common.hrl").

-type role() :: initiator | responder.
-type sign_tag() :: create_tx
                  | funding_created.

-define(MINIMUM_DEPTH, 4). % number of blocks until an opening tx
                           % should be considered final

-define(GEN_STATEM_OPTS, []).  % Use e.g. [{debug, [trace]}] for debugging

-record(data, { role                   :: role()
              , channel_status         :: undefined | open
              , cur_statem_state       :: undefined | atom()
              , state = []             :: [aetx_sign:signed_tx()]
              , session                :: pid()
              , client                 :: pid()
              , opts                   :: map()
              , channel_id             :: undefined | binary()
              , on_chain_id            :: undefined | binary()
              , create_tx              :: undefined | any()
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

message(Fsm, {T, _} = Msg) when T =:= channel_open
                              ; T =:= channel_accept
                              ; T =:= funding_created
                              ; T =:= funding_signed
                              ; T =:= funding_locked
                              ; T =:= update
                              ; T =:= update_ack
                              ; T =:= update_error
                              ; T =:= inband_msg
                              ; T =:= disconnect
                              ; T =:= shutdown
                              ; T =:= shutdown_ack
                              ; T =:= channel_reestablish ->
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

own_funding_locked(Fsm, ChanId) ->
    gen_statem:cast(Fsm, {own_funding_locked, ChanId}).

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

timer_subst(open                   ) -> idle;
timer_subst(awaiting_open          ) -> idle;
timer_subst(awaiting_signature     ) -> sign;
timer_subst(awaiting_locked        ) -> funding_lock;
timer_subst(awaiting_initial_state ) -> accept;
timer_subst(awaiting_update_ack    ) -> accept;
timer_subst(accepted               ) -> funding_create;
timer_subst(half_signed            ) -> funding_sign;
timer_subst(signed                 ) -> funding_lock;
timer_subst(initialized            ) -> accept;
timer_subst(closing                ) -> accept.

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

upd_transfer(Fsm, From, To, Amount) ->
    lager:debug("upd_transfer(~p, ~p, ~p, ~p)", [Fsm, From, To, Amount]),
    gen_statem:call(Fsm, {upd_transfer, From, To, Amount}).

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
    Data = #data{role    = Role,
                 client  = Client,
                 session = Session,
                 opts    = Opts},
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
awaiting_open(cast, {channel_open, Msg}, #data{role = responder} = D) ->
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
awaiting_open(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D).

initialized(enter, _OldSt, _D) -> keep_state_and_data;
initialized(cast, {channel_accept, Msg}, #data{role = initiator} = D) ->
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
initialized(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D);
initialized({call, From}, Req, D) ->
    handle_call(initialized, Req, From, D).


awaiting_signature(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_signature(cast, {signed, create_tx, Tx},
                   #data{role = initiator, latest = {sign, create_tx, _CTx}} = D) ->
    next_state(half_signed,
               send_funding_created_msg(Tx, D#data{latest = undefined}));
awaiting_signature(cast, {signed, funding_created, SignedTx},
                   #data{role = responder,
                         latest = {sign, funding_created, HSCTx}} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    HSCTx, aetx_sign:signatures(SignedTx)),
    D1 = send_funding_signed_msg(NewSignedTx, D#data{create_tx = NewSignedTx}),
    {ok, Watcher, D2} = start_min_depth_watcher(D1),
    next_state(awaiting_locked, D2#data{latest = {watcher, Watcher}});
awaiting_signature(cast, {signed, update, SignedTx}, D) ->
    D1 = send_update_msg(SignedTx, D#data{state = [SignedTx|D#data.state]}),
    next_state(awaiting_update_ack, D1#data{latest = undefined});
awaiting_signature(cast, {signed, update_ack, SignedTx},
                   #data{latest = {sign, update_ack, OCTx}} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    OCTx, aetx_sign:signatures(SignedTx)),
    D1 = send_update_ack_msg(NewSignedTx, D),
    D2 = D1#data{state = [NewSignedTx | clean_state(D1#data.state)]},
    next_state(open, D2);
awaiting_signature(cast, {update, _Msg},
                   #data{latest = {sign, Upd, _}} = D) when Upd==update;
                                                            Upd==update_ack ->
    D1 = send_update_err_msg(fallback_to_stable_state(D)),
    next_state(open, D1);
awaiting_signature(cast, {signed, shutdown, SignedTx}, D) ->
    D1 = send_shutdown_msg(SignedTx, D),
    D2 = D1#data{latest = {shutdown, SignedTx}},
    next_state(closing, D2);
awaiting_signature(cast, {signed, shutdown_ack, SignedTx},
                   #data{latest = {sign, shutdown_ack, CMTx}} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    CMTx, aetx_sign:signatures(SignedTx)),
    D1 = send_shutdown_ack_msg(NewSignedTx, D),
    D2 = D1#data{latest = undefined},
    close(close_mutual, D2);
awaiting_signature(timeout, awaiting_signature = T, D) ->
    close({timeout, T}, D);
awaiting_signature(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D);
awaiting_signature({call, From}, Req, D) ->
    handle_call(awaiting_signature, Req, From, D).


accepted(enter, _OldSt, _D) -> keep_state_and_data;
accepted(cast, {funding_created, Msg}, #data{role = responder} = D) ->
    case check_funding_created_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report(info, funding_created, D1),
            lager:debug("funding_created: ~p", [SignedTx]),
            ok = request_signing(funding_created, aetx_sign:tx(SignedTx), D1),
            D2 = D1#data{latest = {sign, funding_created, SignedTx}},
            next_state(awaiting_signature, D2)
        %% {error, _} = Error ->
        %%     close(Error, D)
    end;
accepted(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D);
accepted(timeout, accepted = T, D) ->
    close({timeout, T}, D);
accepted({call, From}, Req, D) ->
    handle_call(accepted, Req, From, D).


half_signed(enter, _OldSt, _D) -> keep_state_and_data;
half_signed(cast, {funding_signed, Msg}, #data{role = initiator} = D) ->
    case check_funding_signed_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report(info, funding_signed, D1),
            ok = aec_tx_pool:push(SignedTx),
            D2 = D1#data{create_tx = SignedTx},
            {ok, Watcher, D3} = start_min_depth_watcher(D2),
            next_state(awaiting_locked, D3#data{latest = {watcher, Watcher}})
        %% {error, _} = Error ->
        %%     close(Error, D)
    end;
half_signed(timeout, half_signed = T, D) ->
    close({timeout, T}, D);
half_signed(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D);
half_signed({call, From}, Req, D) ->
    handle_call(half_signed, Req, From, D).


awaiting_locked(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_locked(cast, {own_funding_locked, ChainId}, D) ->
    report(info, own_funding_locked, D),
    next_state(
      signed, send_funding_locked_msg(D#data{on_chain_id = ChainId,
                                             latest = undefined}));
awaiting_locked(cast, {funding_locked, _Msg}, D) ->
    postpone(D);
awaiting_locked(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D);
awaiting_locked(timeout, awaiting_locked = T, D) ->
    close({timeout, T}, D).

awaiting_initial_state(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_initial_state(cast, {update, Msg}, #data{role = responder} = D) ->
    lager:debug("got {update, ~p}", [Msg]),
    case check_update_msg(fun check_initial_state/2, Msg, D) of
        {ok, SignedTx, D1} ->
            lager:debug("update_msg checks out", []),
            report(info, update, D1),
            ok = request_signing(update_ack, aetx_sign:tx(SignedTx), D1),
            D2 = D1#data{latest = {sign, update_ack, SignedTx}},
            next_state(awaiting_signature, D2);
        {error,_} = Error ->
            %% TODO: do we do a dispute challenge here?
            close(Error, D)
    end;
awaiting_initial_state(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D);
awaiting_initial_state(timeout, awaiting_initial_state = T, D) ->
    close({timeout, T}, D);
awaiting_initial_state(Evt, Msg, D) ->
    lager:debug("unexpected: awaiting_initial_state(~p, ~p, ~p)",
                [Evt, Msg, D]),
    close({unexpected, Msg}, D).

awaiting_update_ack(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_update_ack(cast, {update_ack, Msg}, #data{} = D) ->
    case check_update_ack_msg(Msg, D) of
        {ok, D1} ->
            next_state(open, D1);
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_update_ack(cast, {update, _Msg}, D) ->
    D1 = send_update_err_msg(fallback_to_stable_state(D)),
    next_state(open, D1);
awaiting_update_ack(cast, {update_err, Msg}, D) ->
    lager:debug("received update_err: ~p", [Msg]),
    case check_update_err_msg(Msg, D) of
        {ok, D1} ->
            report(conflict, Msg, D1),
            next_state(open, D1);
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_update_ack(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D);
awaiting_update_ack(timeout, awaiting_update_ack = T, D) ->
    close({timeout, T}, D);
awaiting_update_ack({call, _}, _Req, D) ->
    postpone(D).

signed(enter, _OldSt, _D) -> keep_state_and_data;
signed(cast, {funding_locked, Msg}, D) ->
    case check_funding_locked_msg(Msg, D) of
        {ok, D1} ->
            report(info, funding_locked, D1),
            funding_locked_complete(D1);
        {error, _} = Error ->
            close(Error, D)
    end;
signed(timeout, signed = T, D) ->
    close({timeout, T}, D);
signed(cast, {shutdown, _Msg}, D) ->
    next_state(closing, D);
signed(cast, {disconnect, _Msg}, D) ->
    next_state(disconnected, D);
signed({call, From}, Req, D) ->
    handle_call(signed, Req, From, D).


funding_locked_complete(D) ->
    case D#data.role of
        initiator ->
            {ok, OCTx} = initial_state(D),
            ok   = request_signing(update, OCTx, D),
            D1   = D#data{latest = {sign, update, OCTx}},
            next_state(awaiting_signature, D1);
        responder ->
            next_state(awaiting_initial_state, D)
    end.



open(enter, _OldSt, D) ->
    D1 = if D#data.channel_status =/= open ->
                 report(info, open, D),
                 report_update(D#data{channel_status = open});
            true ->
                 report_update(D)
         end,
    keep_state(D1);
open(cast, {update, Msg}, D) ->
    case check_update_msg(none, Msg, D) of
        {ok, SignedTx, D1} ->
            report(info, update, D1),
            ok = request_signing(update_ack, aetx_sign:tx(SignedTx), D1),
            D2 = D1#data{latest = {sign, update_ack, SignedTx}},
            next_state(awaiting_signature, D2);
        {error,_} = Error ->
            %% TODO: do we do a dispute challenge here?
            close(Error, D)
    end;
open(cast, {inband_msg, Msg}, D) ->
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
open(cast, {shutdown, Msg}, D) ->
    case check_shutdown_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            ok = request_signing(shutdown_ack, aetx_sign:tx(SignedTx), D1),
            D2 = D1#data{latest = {sign, shutdown_ack, SignedTx}},
            next_state(awaiting_signature, D2);
        {error, E} ->
            close({shutdown_error, E}, D)
    end;
open(cast, {disconnect, _Msg}, D) ->
    next_state(disconnected, D);
open(cast, _Msg, D) ->
    lager:error("Discarding cast in 'open' state: ~p", [_Msg]),
    keep_state(D);
open(info, Msg, D) ->
    handle_info(Msg, D);
open(timeout, open = T, D) ->
    close({timeout, T}, D).

closing(enter, _OldSt, _D) -> keep_state_and_data;
closing(cast, {shutdown_ack, Msg}, D) ->
    case check_shutdown_ack_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            ok = aec_tx_pool:push(SignedTx),
            close(close_mutual, D1);
        {error, _} = Error ->
            close(Error, D)
    end;
closing(cast, {disconnect, _Msg}, D) ->
    next_state(disconnected, D);
closing(cast, {closing_signed, _Msg}, D) ->
    close(closing_signed, D).

disconnected(cast, {channel_reestablish, _Msg}, D) ->
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
handle_call_(open, shutdown, From, #data{state = State} = D) ->
    try  {_Round, Latest} = get_latest_state_tx(State),
         {ok, CloseTx} = close_mutual_tx(Latest, D),
         ok = request_signing(shutdown, CloseTx, D),
         gen_statem:reply(From, ok),
         D1 = D#data{latest = {sign, shutdown, CloseTx}},
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

create_tx_for_signing(#data{opts = #{initiator := Initiator} = Opts}) ->
    Def = create_tx_defaults(Initiator),
    Opts1 = maps:merge(Def, Opts),
    lager:debug("create_tx Opts = ~p", [Opts1]),
    {ok, _} = Ok = aesc_create_tx:new(Opts1),
    Ok.

create_tx_defaults(Initiator) ->
    {ok, Nonce} = aec_next_nonce:pick_for_account(Initiator),
    Fee = aec_governance:minimum_tx_fee(),
    #{ fee   => Fee
     , nonce => Nonce }.


close_mutual_tx(LatestSignedTx, D) ->
    Account = my_account(D),
    {ok, Nonce} = aec_next_nonce:pick_for_account(Account),
    close_mutual_tx(Account, Nonce, LatestSignedTx, D).

close_mutual_tx(Account, Nonce, LatestSignedTx,
                #data{ on_chain_id = ChanId
                     , opts        = Opts } = D) ->
    Def = close_mutual_defaults(Account, D),
    Opts1 = maps:merge(Def, Opts),
    LatestTx = aetx_sign:tx(LatestSignedTx),
    IAmt = tx_initiator_amount(LatestTx),
    RAmt = tx_responder_amount(LatestTx),
    {IAmt1, RAmt1} = pay_close_mutual_fee(maps:get(fee, Opts1), IAmt, RAmt),
    #{ttl := TTL, fee := Fee} = Opts1,
    aesc_close_mutual_tx:new(#{ channel_id       => ChanId
                              , from             => Account
                              , initiator_amount => IAmt1
                              , responder_amount => RAmt1
                              , ttl              => TTL
                              , fee              => Fee
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

send_update_msg(SignedTx, #data{ on_chain_id = OnChainId
                               , session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id => OnChainId
           , data       => TxBin },
    aesc_session_noise:update(Sn, Msg),
    Data.

check_update_msg(F, Msg, D) ->
    lager:debug("check_update_msg(~p)", [Msg]),
    try check_update_msg_(F, Msg, D)
    catch
        error:E ->
            lager:error("CAUGHT ~p, Trace = ~p", [E, erlang:get_stacktrace()]),
            {error, E}
    end.

check_update_msg_(F, #{ channel_id := ChanId
                      , data       := TxBin },
                  #data{ on_chain_id = ChanId } = D) ->
    try aetx_sign:deserialize_from_binary(TxBin) of
        SignedTx ->
            check_signed_update_tx(F, SignedTx, D)
    catch
        error:E ->
            lager:error("CAUGHT ~p, trace = ~p", [E, erlang:get_stacktrace()]),
            {error, {deserialize, E}}
    end.

check_signed_update_tx(F, SignedTx, #data{state = State, opts = Opts} = D) ->
    lager:debug("check_signed_update_tx(~p)", [SignedTx]),
    case check_update_tx(F, SignedTx, State, Opts) of
        ok ->
            {ok, SignedTx, D};
        {error, _} = Error ->
            Error
    end.

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

check_signed_update_ack_tx(SignedTx, #data{state = [HalfSignedTx|T]} = D) ->
    try  ok = check_update_ack_(SignedTx, HalfSignedTx),
         {ok, D#data{state = [SignedTx|T]}}
    catch
        error:_ ->
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
    {Round, SignedTx} = get_latest_state_tx(State),
    Tx = aetx_sign:tx(SignedTx),
    Updates = [{FromPub, ToPub, Amount}],
    try  Tx1 = apply_updates(Updates, Tx, Opts),
         Tx2 = set_tx_values([{round         , Round+1},
                              {previous_round, Round},
                              {updates       , Updates}], Tx1),
         ok = request_signing(update, Tx2, D),
         gen_statem:reply(From, ok),
         D1 = D#data{latest = {sign, update, Tx2}},
         next_state(awaiting_signature, D1)
    catch
        error:Reason ->
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
    {_, LatestSignedTx} = get_latest_state_tx(State),
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


get_latest_state_tx(State) ->
    [SignedTx|_] = drop_until_mutually_signed(State),
    {tx_round(aetx_sign:tx(SignedTx)), SignedTx}.

get_fallback_state(State) ->
    [SignedTx|_] = L = drop_until_mutually_signed(State),
    {tx_round(aetx_sign:tx(SignedTx)), L}.

fallback_to_stable_state(#data{state = State} = D) ->
    [_|_] = NewState = drop_until_mutually_signed(State),
    D#data{state = NewState}.

drop_until_mutually_signed([SignedTx|T] = State) ->
    case aetx_sign:signatures(SignedTx) of
        [_, _] ->
            %% mutually signed
            State;
        _ ->
            drop_until_mutually_signed(T)
    end;
drop_until_mutually_signed([]) ->
    error(no_latest_state).

clean_state([]) ->
    [];
clean_state([SignedTx|T]) ->
    case aetx_sign:signatures(SignedTx) of
        [_,_] -> [SignedTx|clean_state(T)];
        [_]   -> clean_state(T);
        []    -> clean_state(T)
    end.

check_update_tx(F, SignedTx, State, Opts) ->
    lager:debug("check_update_tx(State = ~p)", [State]),
    Tx = aetx_sign:tx(SignedTx),
    lager:debug("Tx = ~p", [Tx]),
    case tx_previous_round(Tx) of
        0 when State == [] ->
            lager:debug("previous round = 0", []),
            check_update_tx_(F, Tx, SignedTx, Opts);
        PrevRound ->
            lager:debug("PrevRound = ~p", [PrevRound]),
            {LastRound, LastSignedTx} = get_latest_state_tx(State),
            lager:debug("LastRound = ~p", [LastRound]),
            case PrevRound == LastRound of
                true ->
                    lager:debug("PrevRound == LastRound", []),
                    check_update_tx_(F, Tx, LastSignedTx, Opts);
                false -> {error, invalid_previous_round}
            end
    end.

check_update_tx_(F, Tx, SignedTx, Opts) ->
    LastTx = aetx_sign:tx(SignedTx),
    Updates = tx_updates(Tx),
    try  CheckTx = apply_updates(Updates, LastTx, Opts),
         case {{tx_initiator_amount(CheckTx), tx_responder_amount(CheckTx)},
               {tx_initiator_amount(Tx)     , tx_responder_amount(Tx)}} of
             {X, X} ->
                 run_extra_checks(F, Tx);
             Other ->
                 {error, {amount_mismatch, Other}}
         end
    catch
        error:Reason ->
            {error, Reason}
    end.


tx_round(Tx)            -> call_cb(Tx, round, []).
tx_previous_round(Tx)   -> call_cb(Tx, previous_round, []).
tx_initiator(Tx)        -> call_cb(Tx, initiator, []).
tx_responder(Tx)        -> call_cb(Tx, responder, []).
tx_initiator_amount(Tx) -> call_cb(Tx, initiator_amount, []).
tx_responder_amount(Tx) -> call_cb(Tx, responder_amount, []).
tx_serialize(Tx)        -> call_cb(Tx, serialize, []).
tx_updates(Tx)          -> call_cb(Tx, updates, []).

-spec call_cb(aetx:tx(), atom(), list()) -> any().
call_cb(Tx, F, Args) ->
    {Mod, TxI} = specialize_cb(Tx),
    do_apply(Mod, F, [TxI|Args]).

specialize_cb(Tx) ->
    aetx:specialize_callback(Tx).

do_apply(M, F, A) ->
    apply(M, F, A).


set_tx_values(Values, Tx) ->
    {Mod, TxI} = aetx:specialize_callback(Tx),
    NewTxI = set_tx_values_(Values, Mod, TxI),
    aetx:update_tx(Tx, NewTxI).

set_tx_values_([{K, V}|T], Mod, Tx) ->
    set_tx_values_(T, Mod, Mod:set_value(Tx, K, V));
set_tx_values_([], _, Tx) ->
    Tx.

apply_updates([], Tx, _Opts) ->
    Tx;
apply_updates([{From, To, Amount}|Ds], Tx, Opts) ->
    Initiator = tx_initiator(Tx),
    Responder = tx_responder(Tx),
    IAmt = tx_initiator_amount(Tx),
    RAmt = tx_responder_amount(Tx),
    Reserve = maps:get(channel_reserve, Opts, 0),
    {FA, FB, A, B} =
        case {From, To} of
            {Initiator, Responder} ->
                {initiator_amount, responder_amount, IAmt, RAmt};
            {Responder, Initiator} ->
                {responder_amount, initiator_amount, RAmt, IAmt};
            _Other ->
                %% TODO: If multi-party channel, this could be valid
                error(unknown_pubkeys)
        end,
    {A1, B1} = {A - Amount, B + Amount},
    Tx1 = if A1 < Reserve ->
                  %% TODO: consider minimum balance
                  error(insufficient_balance);
             true ->
                  set_tx_values([{FA, A1},
                                 {FB, B1}], Tx)
          end,
    apply_updates(Ds, Tx1, Opts).

run_extra_checks(none, _) -> ok;
run_extra_checks(F, Tx) when is_function(F, 2) ->
    {_Vsn, Vals} = tx_serialize(Tx),
    case [Err ||
             {error,_} = Err <- [F(E, Tx) || E <- Vals]] of
        [] ->
            ok;
        [_|_] = Errors ->
            {error, Errors}
    end.

%% update_tx checks
check_initial_state({previous_round, N}, _) ->
    assert(N =:= 0, {error, not_initial_round});
check_initial_state({round, N}, _) ->
    assert(N =:= 1, {error, invalid_round});
check_initial_state({updates, Ds}, _) ->
    assert(Ds == [], {error, updates_in_initial_round});
check_initial_state(_, _) -> ok.

assert(true ,  _   ) -> ok;
assert(false, Error) -> Error.


send_update_ack_msg(SignedTx, #data{ on_chain_id = OnChainId
                                   , session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id => OnChainId
           , data       => TxBin },
    aesc_session_noise:update_ack(Sn, Msg),
    Data.

send_update_err_msg(#data{ state = [SignedTx|_]
                         , on_chain_id = ChanId
                         , session     = Sn } = Data) ->
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
            case get_fallback_state(State) of
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

request_signing(Tag, Obj, #data{client    = Client,
                                channel_id = ChanId}) ->
    Msg = {sign, Tag, Obj},
    Client ! {?MODULE, self(), ChanId, Msg},
    lager:debug("signing(~p) requested", [Tag]),
    ok.

default_minimum_depth(initiator  ) -> undefined;
default_minimum_depth(responder) -> ?MINIMUM_DEPTH.

start_min_depth_watcher(#data{create_tx = SignedTx,
                              opts = #{initiator     := Initiator,
                                       responder     := Responder,
                                       minimum_depth := MinDepth}} = Data) ->
    Tx = aetx_sign:tx(SignedTx),
    TxHash = aetx_sign:hash(SignedTx),
    evt({tx_hash, TxHash}),
    Nonce = aetx:nonce(Tx),
    evt({nonce, Nonce}),
    OnChainId = aesc_channels:id(Initiator, Nonce, Responder),
    evt({on_chain_id, OnChainId}),
    {ok, Watcher} = aesc_fsm_min_depth_watcher:start_link(
                      TxHash, OnChainId, MinDepth),
    evt({watcher, Watcher}),
    {ok, Watcher, Data#data{on_chain_id = OnChainId}}.

initial_state(#data{ create_tx    = SignedTx
                   ,  on_chain_id = ChanId }) ->
    Tx = aetx_sign:tx(SignedTx),
    Initiator = tx_initiator(Tx),
    Responder = tx_responder(Tx),
    InitiatorAmount = tx_initiator_amount(Tx),
    ResponderAmount = tx_responder_amount(Tx),
    NewOpts = #{ channel_id       => ChanId
               , initiator        => Initiator
               , responder        => Responder
               , initiator_amount => InitiatorAmount
               , responder_amount => ResponderAmount
               , updates          => []
               , state            => <<>>
               , previous_round   => 0
               , round            => 1 },
    lager:debug("offchain_tx:new(~p)", [NewOpts]),
    aesc_offchain_tx:new(NewOpts).


gproc_register(#data{role = Role, channel_id = ChanId}) ->
    gproc:reg(gproc_name(ChanId, Role)).

gproc_name(Id, Role) ->
    {n, l, {aesc_channel, {Id, Role}}}.

evt(_Msg) ->
    ok.

report_update(#data{state = State, last_reported_update = Last} = D) ->
    case get_latest_state_tx(State) of
        {Last, _} ->
            D;
        {New, SignedTx} ->
            report(update, SignedTx, D),
            D#data{last_reported_update = New}
    end.

report_tags() ->
    [info, update, conflict, message, error].

default_report_flags() ->
    #{ error    => true
     , message  => true
     , conflict => true }.

report(Tag, St, D) -> report_info(do_rpt(Tag, D), Tag, St, D).

report_info(DoRpt, Tag, St, #data{client = Client, channel_id = ChanId}) ->
    lager:debug("report_info(~p, ~p, ~p, ~p)", [DoRpt, Tag, ChanId, St]),
    if DoRpt -> Client ! {?MODULE, self(), ChanId, {Tag, St}};
       true  -> ok
    end,
    ok.

do_rpt(Tag, #data{opts = #{report := Rpt}}) ->
    try maps:get(Tag, Rpt, false)
    catch
        error:_ ->
            false
    end.
