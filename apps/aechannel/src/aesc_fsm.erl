-module(aesc_fsm).
-behaviour(gen_statem).

-export([start_link/1]).

%% API

-export([initiate/3,              %% (host(), port(), Opts :: #{}
         respond/2,               %% (port(), Opts :: #{})
         upd_transfer/4,          %% (fsm() , from(), to(), amount())
         upd_deposit/2,           %% (fsm() , map())
         upd_withdraw/2,          %% (fsm() , map())
         upd_create_contract/2,   %%
         upd_call_contract/2,     %%
         get_contract_call/4,     %% (fsm(), contract_id(), caller(), round())
         leave/1,
         shutdown/1,              %% (fsm())
         client_died/1,           %% (fsm())
         inband_msg/3,
         get_state/1,
         get_poi/2]).

%% Inspection and configuration functions
-export([ get_history/1     %% (fsm()) -> [Event]
        , change_config/3   %% (fsm(), key(), value()) -> ok | {error,_}
        , get_balances/2    %% (fsm(), {ok, [key()]) -> [{key(), amount()}]} | {error, _}
        , get_round/1       %% (fsm()) -> {ok, round()} | {error, _}
        , prune_local_calls/1  %% (fsm()) -> {ok, round()} | {error, _}
        ]).

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
         reestablish_init/3,
         accepted/3,
         awaiting_open/3,
         awaiting_reestablish/3,
         awaiting_signature/3,
         awaiting_locked/3,
         awaiting_initial_state/3,
         awaiting_update_ack/3,
         awaiting_leave_ack/3,
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

-define(UPDATE_REQ(R), R==?UPDATE; R==?DEP_CREATED; R==?WDRAW_CREATED).


-define(KEEP, 10).
-record(w, { n = 0         :: non_neg_integer()
           , keep = ?KEEP  :: non_neg_integer()
           , a = []        :: list()
           , b = []        :: list()
           }).

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
              , ongoing_update = false :: boolean()
              , last_reported_update   :: undefined | non_neg_integer()
              , log = #w{}             :: #w{}
              }).

-define(TRANSITION_STATE(S),  S=:=awaiting_signature
                            ; S=:=awaiting_open
                            ; S=:=awaiting_locked
                            ; S=:=awaiting_update_ack
                            ; S=:=awaiting_leave_ack
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
                              ; T =:= ?LEAVE
                              ; T =:= ?LEAVE_ACK
                              ; T =:= ?SHUTDOWN
                              ; T =:= ?SHUTDOWN_ACK
                              ; T =:= ?CH_REESTABL
                              ; T =:= ?CH_REEST_ACK ->
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

where(ChanId, Role) when Role == initiator; Role == responder ->
    gproc:where(gproc_name_by_role(ChanId, Role));
where(ChanId, Pubkey) when is_binary(Pubkey) ->
    gproc:where(gproc_name_by_pubkey(ChanId, Pubkey)).

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
timer_subst(awaiting_reestablish    ) -> idle;
timer_subst(reestablish_init        ) -> accept;
timer_subst(awaiting_signature      ) -> sign;
timer_subst(awaiting_locked         ) -> funding_lock;
timer_subst(awaiting_initial_state  ) -> accept;
timer_subst(awaiting_update_ack     ) -> accept;
timer_subst(awaiting_leave_ack      ) -> accept;
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

set_ongoing(D) ->
    D#data{ongoing_update = true}.
%%
%% ======================================================================

%% NOTE: we currently double-book Role in both the #data{} record and
%% the Opts map. This is a bit annoying, but simplifies the pattern-matching
%% in this module (the #data{} part); also, the aesc_offchain_state module
%% needs Role for the cache interaction. Future refactoring can resolve this
%% slight code smell.

initiate(Host, Port, #{} = Opts0) ->
    lager:debug("initiate(~p, ~p, ~p)", [Host, Port, Opts0]),
    Opts = maps:merge(#{client => self()}, Opts0),
    start_link(#{ host => Host
                , port => Port
                , opts => Opts#{role => initiator} }).

respond(Port, #{} = Opts0) ->
    lager:debug("respond(~p, ~p)", [Port, Opts0]),
    Opts = maps:merge(#{client => self()}, Opts0),
    start_link(#{ port => Port
                , opts => Opts#{role => responder} }).

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

upd_create_contract(Fsm, #{vm_version := _,
                           deposit    := Amt,
                           code       := _,
                           call_data  := _} = Opts) when is_integer(Amt), Amt >= 0 ->
    lager:debug("upd_create_contract(~p)", [Opts]),
    gen_statem:call(Fsm, {upd_create_contract, Opts}).


upd_call_contract(Fsm, #{contract    := _,
                         vm_version  := _,
                         amount      := Amt,
                         call_data  := _} = Opts) when is_integer(Amt), Amt >= 0 ->
    lager:debug("upd_call_contract(~p)", [Opts]),
    CallStack = maps:get(call_stack, Opts, []),
    gen_statem:call(Fsm, {upd_call_contract, Opts#{call_stack => CallStack}}).

get_contract_call(Fsm, Contract, Caller, Round) when is_integer(Round), Round > 0 ->
    lager:debug("get_contract_call(~p, ~p, ~p)", [Contract, Caller, Round]),
    gen_statem:call(Fsm, {get_contract_call, Contract, Caller, Round}).

inband_msg(Fsm, To, Msg) ->
    lager:debug("inband_msg(~p, ~p, ~p)", [Fsm, To, Msg]),
    gen_statem:call(Fsm, {inband_msg, To, Msg}).


-spec get_state(pid()) -> {ok, #{}}.
get_state(Fsm) ->
    gen_statem:call(Fsm, get_state).

-spec get_poi(pid(), list()) -> {ok, aec_trees:poi()} | {error, not_found}.
get_poi(Fsm, Filter) ->
    gen_statem:call(Fsm, {get_poi, Filter}).

leave(Fsm) ->
    lager:debug("leave(~p)", [Fsm]),
    gen_statem:call(Fsm, leave).


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

check_change_config(log_keep, Keep) when is_integer(Keep), Keep >= 0 ->
    {ok, log_keep, Keep};
check_change_config(_, _) ->
    {error, invalid_config}.

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

%% ======================================================================
%% FSM initialization

init(#{opts := Opts0} = Arg) ->
    #{role := Role, client := Client} = Opts0,
    DefMinDepth = default_minimum_depth(Role),
    Opts = check_opts(
             [
              fun(O) -> check_minimum_depth_opt(DefMinDepth, Role, O) end,
              fun check_timeout_opt/1,
              fun check_rpt_opt/1,
              fun check_log_opt/1
             ], Opts0),
    Session = start_session(Arg, Opts),
    {ok, State} = aesc_offchain_state:new(Opts),
    Data = #data{role    = Role,
                 client  = Client,
                 session = Session,
                 opts    = Opts,
                 state   = State,
                 log     = #w{keep = maps:get(log_keep, Opts)}},
    lager:debug("Session started, Data = ~p", [Data]),
    %% TODO: Amend the fsm above to include this step. We have transport-level
    %% connectivity, but not yet agreement on the channel parameters. We will next send
    %% a channel_open() message and await a channel_accept().
    Reestablish = maps:is_key(existing_channel_id, Opts),
    case Role of
        initiator ->
            if Reestablish ->
                    {ok, reestablish_init, send_reestablish_msg(Data),
                     [timer_for_state(reestablish_init, Data)]};
               true ->
                    {ok, initialized, send_open_msg(Data),
                     [timer_for_state(initialized, Data)]}
            end;
        responder ->
            if Reestablish ->
                    {ok, awaiting_reestablish, Data,
                     [timer_for_state(awaiting_reestablish, Data)]};
               true ->
                    {ok, awaiting_open, Data,
                     [timer_for_state(awaiting_open, Data)]}
            end
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


%% As per CHANNELS.md, the responder is regarded as the one typically
%% providing the service, and the initiator connects.
start_session(#{port := Port}, #{role := responder} = Opts) ->
    NoiseOpts = maps:get(noise, Opts, []),
    ok(aesc_session_noise:accept(Port, NoiseOpts));
start_session(#{host := Host, port := Port}, #{role := initiator} = Opts) ->
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
    close(disconnect, D);
awaiting_open(cast, {_, _} = Msg, D) ->
    lager:debug("Wrong msg in awaiting_open: ~p", [Msg]),
    %% should send an error msg
    close(protocol_error, D);
awaiting_open({call, From}, Req, D) ->
    handle_call(awaiting_open, Req, From, D).

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
awaiting_reestablish(timeout, awaiting_reestablish = T, D) ->
    close({timeout, T}, D);
awaiting_reestablish(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
awaiting_reestablish(cast, {_, _} = Msg, D) ->
    lager:debug("Wrong msg in awaiting_reestablish: ~p", [Msg]),
    %% should send and error msg
    close(protocol_error, D).

initialized(enter, _OldSt, _D) -> keep_state_and_data;
initialized(cast, {?CH_ACCEPT, Msg}, #data{role = initiator} = D) ->
    case check_accept_msg(Msg, D) of
        {ok, D1} ->
            gproc_register(D1),
            report(info, channel_accept, D1),
            {ok, CTx} = create_tx_for_signing(D1),
            D2 = request_signing(create_tx, CTx, D1),
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

reestablish_init(enter, _OldSt, _D) -> keep_state_and_data;
reestablish_init(cast, {?CH_REEST_ACK, Msg}, D) ->
    case check_reestablish_ack_msg(Msg, D) of
        {ok, D1} ->
            report(info, channel_reestablished, D1),
            next_state(open, D1);
        {error, _} = Err ->
            close(Err, D)
    end;
reestablish_init(timeout, reestablish_init = T, D) ->
    close({timeout, T}, D);
reestablish_init(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
reestablish_init({call, From}, Req, D) ->
    handle_call(reestablish_init, Req, From, D).

awaiting_signature(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_signature(cast, {Req, _} = Msg, #data{ongoing_update = true} = D)
  when ?UPDATE_REQ(Req) ->
    %% Race detection!
    lager:debug("race detected: ~p", [Msg]),
    handle_update_conflict(Req, D);
awaiting_signature(cast, {?SIGNED, create_tx, Tx} = Msg,
                   #data{role = initiator, latest = {sign, create_tx, _CTx}} = D) ->
    next_state(half_signed,
               send_funding_created_msg(
                 Tx, log(rcv, ?SIGNED, Msg, D#data{latest = undefined})));
awaiting_signature(cast, {?SIGNED, deposit_tx, Tx} = Msg,
                   #data{latest = {sign, deposit_tx, _DTx}} = D) ->
    next_state(dep_half_signed,
               send_deposit_created_msg(
                 Tx, log(rcv, ?SIGNED, Msg, D#data{latest = undefined})));
awaiting_signature(cast, {?SIGNED, withdraw_tx, Tx} = Msg,
                   #data{latest = {sign, withdraw_tx, _DTx}} = D) ->
    next_state(wdraw_half_signed,
               send_withdraw_created_msg(
                 Tx, log(rcv, ?SIGNED, Msg, D#data{latest = undefined})));
awaiting_signature(cast, {?SIGNED, ?FND_CREATED, SignedTx} = Msg,
                   #data{role = responder,
                         latest = {sign, ?FND_CREATED, HSCTx}} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    HSCTx, aetx_sign:signatures(SignedTx)),
    D1 = send_funding_signed_msg(
           NewSignedTx,
           log(rcv, ?SIGNED, Msg, D#data{create_tx = NewSignedTx})),
    report(on_chain_tx, NewSignedTx, D1),
    {ok, D2} = start_min_depth_watcher(?WATCH_FND, NewSignedTx, D1),
    gproc_register_on_chain_id(D2),
    next_state(awaiting_locked, D2);
awaiting_signature(cast, {?SIGNED, ?DEP_CREATED, SignedTx} = Msg,
                   #data{latest = {sign, ?DEP_CREATED, HSCTx}} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    HSCTx, aetx_sign:signatures(SignedTx)),
    D1 = send_deposit_signed_msg(NewSignedTx, D),
    report(on_chain_tx, NewSignedTx, D1),
    {ok, D2} = start_min_depth_watcher(?WATCH_DEP, NewSignedTx, D1),
    next_state(awaiting_locked,
               log(rcv, ?SIGNED, Msg, D2));
awaiting_signature(cast, {?SIGNED, ?WDRAW_CREATED, SignedTx} = Msg,
                   #data{latest = {sign, ?WDRAW_CREATED, HSCTx}} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    HSCTx, aetx_sign:signatures(SignedTx)),
    D1 = send_withdraw_signed_msg(NewSignedTx, D),
    report(on_chain_tx, NewSignedTx, D1),
    {ok, D2} = start_min_depth_watcher(?WATCH_WDRAW, NewSignedTx, D1),
    next_state(awaiting_locked, log(rcv, ?SIGNED, Msg, D2));
awaiting_signature(cast, {?SIGNED, ?UPDATE, SignedTx} = Msg, D) ->
    D1 = send_update_msg(
           SignedTx,
           D#data{state = aesc_offchain_state:add_half_signed_tx(
                            SignedTx, D#data.state)}),
    next_state(awaiting_update_ack,
               log(rcv, ?SIGNED, Msg, D1#data{latest = undefined}));
awaiting_signature(cast, {?SIGNED, ?UPDATE_ACK, SignedTx} = Msg,
                   #data{latest = {sign, ?UPDATE_ACK, OCTx}, opts = Opts} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    OCTx, aetx_sign:signatures(SignedTx)),
    D1 = send_update_ack_msg(NewSignedTx, D),
    State = aesc_offchain_state:add_signed_tx(NewSignedTx, D1#data.state, Opts),
    D2 = D1#data{log   = log_msg(rcv, ?SIGNED, Msg, D1#data.log),
                 state = State},
    next_state(open, D2);
awaiting_signature(cast, {?SIGNED, ?SHUTDOWN, SignedTx} = Msg, D) ->
    D1 = send_shutdown_msg(SignedTx, D),
    D2 = D1#data{latest = {shutdown, SignedTx},
                 log    = log_msg(rcv, ?SIGNED, Msg, D1#data.log)},
    next_state(closing, D2);
awaiting_signature(cast, {?SIGNED, ?SHUTDOWN_ACK, SignedTx} = Msg,
                   #data{latest = {sign, ?SHUTDOWN_ACK, CMTx}} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    CMTx, aetx_sign:signatures(SignedTx)),
    D1 = send_shutdown_ack_msg(NewSignedTx, D),
    D2 = D1#data{latest = undefined,
                 log    = log_msg(rcv, ?SIGNED, Msg, D1#data.log)},
    report(on_chain_tx, NewSignedTx, D1),
    close(close_mutual, D2);
%% Other
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
            D2 = request_signing(
                   ?FND_CREATED, aetx_sign:tx(SignedTx), SignedTx, D1),
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
dep_half_signed(cast, {Req, _} = Msg, D) when ?UPDATE_REQ(Req) ->
    %% This might happen if a request is sent before our ?DEP_CREATED msg
    %% arrived.
    lager:debug("race detected: ~p", [Msg]),
    handle_update_conflict(Req, D);
dep_half_signed(cast, {?DEP_SIGNED, Msg}, D) ->
    case check_deposit_signed_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report(on_chain_tx, SignedTx, D1),
            ok = aec_tx_pool:push(SignedTx),
            {ok, D2} = start_min_depth_watcher(deposit, SignedTx, D1),
            next_state(awaiting_locked, D2)
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
wdraw_half_signed(cast, {Req,_} = Msg, D) when ?UPDATE_REQ(Req) ->
    %% This might happen if a request is sent before our ?WDRAW_CREATED msg
    %% arrived.
    lager:debug("race detected: ~p", [Msg]),
    handle_update_conflict(Req, D);
wdraw_half_signed(cast, {?WDRAW_SIGNED, Msg}, D) ->
    case check_withdraw_signed_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report(on_chain_tx, SignedTx, D1),
            ok = aec_tx_pool:push(SignedTx),
            {ok, D2} = start_min_depth_watcher(withdraw, SignedTx, D1),
            next_state(awaiting_locked, D2)
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
wdraw_half_signed(timeout, wdraw_half_signed = T, D) ->
    close({timeout, T}, D);
wdraw_half_signed(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
wdraw_half_signed({call, From}, Req, D) ->
    handle_call(wdraw_half_signed, Req, From, D).

%% Don't flag for update conflicts once we've pushed to the chain, and
%% wait for confirmation; postpone instead.
%%
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
    close({timeout, T}, D);
awaiting_locked({call, From}, Req, D) ->
    handle_call(awaiting_locked, Req, From, D).

awaiting_initial_state(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_initial_state(cast, {?UPDATE, Msg}, #data{role = responder} = D) ->
    lager:debug("got {update, ~p}", [Msg]),
    case check_update_msg(initial, Msg, D) of
        {ok, SignedTx, D1} ->
            lager:debug("update_msg checks out", []),
            report(info, update, D1),
            D2 = request_signing(
                   ?UPDATE_ACK, aetx_sign:tx(SignedTx), SignedTx, D1),
            next_state(awaiting_signature, D2);
        {error,_} = Error ->
            %% TODO: do we do a dispute challenge here?
            close(Error, D)
    end;
awaiting_initial_state(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
awaiting_initial_state(timeout, awaiting_initial_state = T, D) ->
    close({timeout, T}, D);
awaiting_initial_state({call, From}, Req, D) ->
    handle_call(awaiting_initial_state, Req, From, D);
awaiting_initial_state(Evt, Msg, D) ->
    lager:debug("unexpected: awaiting_initial_state(~p, ~p, ~p)",
                [Evt, Msg, D]),
    close({unexpected, Msg}, D).

awaiting_update_ack(enter, _OldSt, _D) -> keep_state_and_data;
awaiting_update_ack(cast, {Req,_} = Msg, #data{} = D) when ?UPDATE_REQ(Req) ->
    %% This might happen if a request is sent before our signed ?UPDATE msg
    %% arrived.
    lager:debug("race detected: ~p", [Msg]),
    handle_update_conflict(Req, D);
awaiting_update_ack(cast, {?UPDATE_ACK, Msg}, #data{} = D) ->
    case check_update_ack_msg(Msg, D) of
        {ok, D1} ->
            next_state(open, D1);
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_update_ack(cast, {?UPDATE, _} = Msg, D) ->
    lager:debug("race detected: ~p", [Msg]),
    handle_update_conflict(?UPDATE, D);
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
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_update_ack(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
awaiting_update_ack(timeout, awaiting_update_ack = T, D) ->
    close({timeout, T}, D);
awaiting_update_ack({call, _}, _Req, D) ->
    postpone(D).

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
awaiting_leave_ack(cast, {?DISCONNECT, _Msg}, D) ->
    close(disconnect, D);
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
awaiting_leave_ack(cast, _Msg, D) ->
    postpone(D);
awaiting_leave_ack({call, _From}, _Req, D) ->
    postpone(D).

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
    initialize_cache(D1),
    next_state(open, D1).

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
        {ok, SignedTx, D1} ->
            report(info, update, D1),
            D2 = request_signing(
                   ?UPDATE_ACK, aetx_sign:tx(SignedTx), SignedTx, D1),
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
            D2 = request_signing(
                   ?DEP_CREATED, aetx_sign:tx(SignedTx), SignedTx, D1),
            next_state(awaiting_signature, D2)
    end;
open(cast, {?WDRAW_CREATED, Msg}, D) ->
    case check_withdraw_created_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report(info, withdraw_created, D1),
            lager:debug("withdraw_created: ~p", [SignedTx]),
            D2 = request_signing(
                   ?WDRAW_CREATED, aetx_sign:tx(SignedTx), SignedTx, D1),
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
        {ok, SignedTx, D1} ->
            D2 = request_signing(
                   ?SHUTDOWN_ACK, aetx_sign:tx(SignedTx), SignedTx, D1),
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

close(Reason, D) ->
    close_(Reason, log(evt, close, Reason, D)).

close_(close_mutual, D) ->
    report(info, close_mutual, D),
    {stop, normal, D};
close_(leave, D) ->
    {stop, normal, D};
close_(Reason, D) ->
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
    try handle_call_(St, Req, From, D)
    catch
        error:Error ->
            keep_state(D, [{reply, From, {error, Error}}])
    end.

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
    {ok, DepTx} = dep_tx_for_signing(Opts#{from => FromPub}, D),
    D1 = request_signing(deposit_tx, DepTx, D),
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
    {ok, DepTx} = wdraw_tx_for_signing(Opts#{to => ToPub}, D),
    D1 = request_signing(withdraw_tx, DepTx, D),
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
handle_call_(open, {upd_call_contract, Opts}, From, #data{} = D) ->
    FromPub = my_account(D),
    case maps:find(from, Opts) of
        {ok, FromPub} -> ok;
        {ok, _Other}  -> error(conflicting_accounts);
        error         -> ok
    end,
    call_contract_tx_for_signing(Opts#{caller => FromPub}, From, D);
handle_call_(open, {get_contract_call, Contract, Caller, Round}, From,
             #data{state = State} = D) ->
    Response =
        case aesc_offchain_state:get_contract_call(Contract, Caller, Round, State) of
            {error, call_not_found} -> {error, call_not_found};
            {ok, Call} -> {ok, Call}
        end,
    keep_state(D, [{reply, From, Response}]);
handle_call_(open, shutdown, From, #data{state = State} = D) ->
    try  {_Round, Latest} = aesc_offchain_state:get_latest_signed_tx(State),
         {ok, CloseTx} = close_mutual_tx(Latest, D),
         D1 = request_signing(?SHUTDOWN, CloseTx, D),
         gen_statem:reply(From, ok),
         next_state(awaiting_signature, set_ongoing(D1))
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
    report(debug, {log, win_to_list(Data#data.log)}, Data),
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
    Data#data{ channel_id = ChannelId
             , log = log_msg(snd, ?CH_OPEN, Msg, Data#data.log) }.

check_open_msg(#{ chain_hash           := ChainHash
                , temporary_channel_id := ChanId
                , lock_period          := LockPeriod
                , push_amount          := PushAmt
                , initiator_amount     := InitiatorAmt
                , responder_amount     := ResponderAmt
                , channel_reserve      := ChanReserve
                , initiator            := InitiatorPubkey} = Msg,
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
            {ok, Data#data{
                   channel_id = ChanId,
                   opts       = Opts1,
                   log        = log_msg(rcv, ?CH_OPEN, Msg, Data#data.log) }};
        _ ->
            {error, chain_hash_mismatch}
    end.

send_reestablish_msg(#data{ opts = #{ existing_channel_id  := ChId
                                    , offchain_tx := OffChainTx }
                          , session = Sn } = Data) ->
    ChainHash = aec_chain:genesis_hash(),
    TxBin = aetx_sign:serialize_to_binary(OffChainTx),
    Msg = #{ chain_hash => ChainHash
           , channel_id => ChId
           , data       => TxBin },
    aesc_session_noise:channel_reestablish(Sn, Msg),
    Data#data{channel_id  = ChId,
              on_chain_id = ChId,
              latest      = {reestablish, OffChainTx},
              log         = log_msg(snd, ?CH_REESTABL, Msg, Data#data.log)}.

check_reestablish_msg(#{ chain_hash := ChainHash
                       , channel_id := ChId
                       , data       := TxBin } = Msg,
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
           , data        => TxBin },
    aesc_session_noise:channel_reestablish_ack(Sn, Msg),
    Data#data{ latest = undefined
             , log = log_msg(snd, ?CH_REEST_ACK, Msg, Data#data.log) }.

check_reestablish_ack_msg(#{ data := TxBin } = Msg, Data) ->
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

chk_dual_sigs(_, SignedTx, #data{ state = State }) ->
    [_,_] = aetx_sign:signatures(SignedTx),
    {ok == aesc_offchain_state:verify_signatures(SignedTx, State),
     signatures_invalid}.

chk_same_tx(_, SignedTx, #data{ state = State }) ->
    {_, MySignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    {aetx_sign:serialize_to_binary(SignedTx)
     == aetx_sign:serialize_to_binary(MySignedTx), offchain_state_mismatch}.

log_reestabl_ack_msg(Msg, _, #data{log = L} = D) ->
    {data, D#data{log = log_msg(rcv, ?CH_REEST_ACK, Msg, L)}}.

get_channel(ChainHash, ChId) ->
    case aec_chain:genesis_hash() of
        ChainHash ->
            aec_chain:get_channel(ChId);
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
    Data#data{log = log_msg(snd, ?CH_ACCEPT, Msg, Data#data.log)}.

check_accept_msg(#{ chain_hash           := ChainHash
                  , temporary_channel_id := ChanId
                  , minimum_depth        := MinDepth
                  , initiator_amount     := _InitiatorAmt
                  , responder_amount     := _ResponderAmt
                  , channel_reserve      := _ChanReserve
                  , responder            := Responder} = Msg,
                 #data{channel_id = ChanId,
                       opts = Opts} = Data) ->
    %% TODO: implement more checks
    case aec_chain:genesis_hash() of
        ChainHash ->
            Log1 = log_msg(rcv, ?CH_ACCEPT, Msg, Data#data.log),
            {ok, Data#data{ opts = Opts#{ minimum_depth => MinDepth
                                        , responder     => Responder}
                          , log = Log1 }};
        _ ->
            {error, chain_hash_mismatch}
    end.

dep_tx_for_signing(#{from := From, amount := Amount} = Opts,
                   #data{on_chain_id = ChanId, state=State}) ->
    Updates = [aesc_offchain_update:op_deposit(aec_id:create(account, From), Amount)],
    UpdatedStateTx = aesc_offchain_state:make_update_tx(Updates, State, Opts),
    {channel_offchain_tx, UpdatedOffchainTx} = aetx:specialize_type(UpdatedStateTx),
    StateHash = aesc_offchain_tx:state_hash(UpdatedOffchainTx),

    {LastRound, _} = aesc_offchain_state:get_latest_signed_tx(State),
    Def = deposit_tx_defaults(ChanId, From, maps:get(ttl, Opts, undefined)),
    Opts1 = maps:merge(Def, Opts),
    Opts2 = maps:merge(Opts1, #{state_hash => StateHash,
                                round      => LastRound + 1,
                                channel_id => aec_id:create(channel, ChanId),
                                from       => aec_id:create(account, From)
                               }),
    lager:debug("deposit_tx Opts = ~p", [Opts2]),
    {ok, _} = Ok = aesc_deposit_tx:new(Opts2),
    Ok.

wdraw_tx_for_signing(#{to := To, amount := Amount} = Opts,
                     #data{on_chain_id = ChanId, state=State}) ->
    Updates = [aesc_offchain_update:op_withdraw(aec_id:create(account, To), Amount)],
    UpdatedStateTx = aesc_offchain_state:make_update_tx(Updates, State, Opts),
    {channel_offchain_tx, UpdatedOffchainTx} = aetx:specialize_type(UpdatedStateTx),
    StateHash = aesc_offchain_tx:state_hash(UpdatedOffchainTx),

    {LastRound, _} = aesc_offchain_state:get_latest_signed_tx(State),
    Def = withdraw_tx_defaults(ChanId, To, maps:get(ttl, Opts, undefined)),
    Opts1 = maps:merge(Def, Opts),
    Opts2 = maps:merge(Opts1, #{state_hash => StateHash,
                                round      => LastRound + 1,
                                channel_id => aec_id:create(channel, ChanId),
                                to         => aec_id:create(account, To)
                               }),
    lager:debug("withdraw_tx Opts = ~p", [Opts2]),
    {ok, _} = Ok = aesc_withdraw_tx:new(Opts2),
    Ok.

new_contract_tx_for_signing(Opts, From, #data{state = State, opts = ChannelOpts } = D) ->
    #{owner       := Owner,
      vm_version  := VmVersion,
      code        := Code,
      deposit     := Deposit,
      call_data   := CallData} = Opts,
    Updates = [aesc_offchain_update:op_new_contract(aec_id:create(account, Owner),
                                                    VmVersion, Code, Deposit, CallData)],
    try  Tx1 = aesc_offchain_state:make_update_tx(Updates, State, ChannelOpts),
         D1 = request_signing(?UPDATE, Tx1, D),
         gen_statem:reply(From, ok),
         next_state(awaiting_signature, D1)
    catch
        error:Reason ->
            lager:error("CAUGHT ~p, trace = ~p", [Reason, erlang:get_stacktrace()]),
            keep_state(D, [{reply, From, {error, Reason}}])
    end.

call_contract_tx_for_signing(Opts, From, #data{state = State, opts = ChannelOpts } = D) ->
    #{caller      := Caller,
      contract    := ContractPubKey,
      vm_version  := VmVersion,
      amount      := Amount,
      call_data   := CallData,
      call_stack  := CallStack} = Opts,
    Updates = [aesc_offchain_update:op_call_contract(aec_id:create(account, Caller),
                                                     aec_id:create(contract, ContractPubKey),
                                                     VmVersion, Amount, CallData, CallStack)],
    try  Tx1 = aesc_offchain_state:make_update_tx(Updates, State, ChannelOpts),
         D1 = request_signing(?UPDATE, Tx1, D),
         gen_statem:reply(From, ok),
         next_state(awaiting_signature, D1)
    catch
        error:Reason ->
            lager:error("CAUGHT ~p, trace = ~p", [Reason, erlang:get_stacktrace()]),
            keep_state(D, [{reply, From, {error, Reason}}])
    end.

create_tx_for_signing(#data{opts = #{initiator := Initiator,
                                     responder := Responder} = Opts,
                            state=State}) ->
    StateHash = aesc_offchain_state:hash(State),
    Def = create_tx_defaults(Initiator),
    Opts1 = maps:merge(Def, Opts),
    lager:debug("create_tx Opts = ~p", [Opts1]),
    Opts2 = Opts1#{state_hash => StateHash,
                   initiator  => aec_id:create(account, Initiator),
                   responder  => aec_id:create(account, Responder)
                  },
    {ok, _} = Ok = aesc_create_tx:new(Opts2),
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
    aesc_close_mutual_tx:new(#{ channel_id             => aec_id:create(channel, ChanId)
                              , initiator_amount_final => IAmt1
                              , responder_amount_final => RAmt1
                              , ttl                    => TTL
                              , fee                    => Fee
                              , state_hash             => StateHash
                              , round                  => LastRound + 1
                              , nonce                  => Nonce }).

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
    log(snd, ?FND_CREATED, Msg, Data).

check_funding_created_msg(#{ temporary_channel_id := ChanId
                           , data                 := TxBin } = Msg,
                          #data{ channel_id = ChanId} = Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    {ok, SignedTx, log(rcv, ?FND_CREATED, Msg, Data)}.

send_funding_signed_msg(SignedTx, #data{channel_id = Ch,
                                        session    = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ temporary_channel_id  => Ch
           , data                  => TxBin},
    aesc_session_noise:funding_signed(Sn, Msg),
    log(snd, ?FND_CREATED, Msg, Data).

check_funding_signed_msg(#{ temporary_channel_id := ChanId
                          , data                 := TxBin} = Msg,
                          #data{ channel_id = ChanId } = Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    {ok, SignedTx, log(rcv, ?FND_SIGNED, Msg, Data)}.

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

send_deposit_created_msg(SignedTx, #data{on_chain_id = Ch,
                                         session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id => Ch
           , data       => TxBin},
    aesc_session_noise:deposit_created(Sn, Msg),
    log(snd, ?DEP_CREATED, Msg, Data).

check_deposit_created_msg(#{ channel_id := ChanId
                           , data       := TxBin} = Msg,
                          #data{on_chain_id = ChanId} = Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    {ok, SignedTx, log(rcv, ?DEP_CREATED, Msg, Data)}.

send_deposit_signed_msg(SignedTx, #data{on_chain_id = Ch,
                                        session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id  => Ch
           , data        => TxBin},
    aesc_session_noise:deposit_signed(Sn, Msg),
    log(snd, ?DEP_SIGNED, Msg, Data).

check_deposit_signed_msg(#{ channel_id := ChanId
                          , data       := TxBin} = Msg,
                          #data{on_chain_id = ChanId} = Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    {ok, SignedTx, log(rcv, ?DEP_SIGNED, Msg, Data)}.

send_deposit_locked_msg(TxHash, #data{on_chain_id = ChanId,
                                      session     = Sn} = Data) ->
    Msg = #{ channel_id => ChanId
           , data       => TxHash },
    aesc_session_noise:deposit_locked(Sn, Msg),
    log(snd, ?DEP_LOCKED, Msg, Data).

check_deposit_locked_msg(#{ channel_id := ChanId
                          , data       := TxHash } = Msg,
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

check_op_error_msg(Op, #{ channel_id := ChanId
                        , round      := Round
                        , error_code := ErrCode } = Msg,
                        #data{on_chain_id = ChanId0,
                              state = State} = D) ->
    case ChanId == ChanId0 of
        true ->
            case aesc_offchain_state:get_fallback_state(State) of
                {Round, State1} ->
                    {ok, ErrCode,
                     D#data{state = State1,
                            log = log_msg(
                                    rcv, Op, Msg, D#data.log)}};
                _Other ->
                    lager:debug("Fallback state mismatch: ~p/~p",
                                [Round, _Other]),
                    {error, fallback_state_mismatch}
            end;
        false ->
            {error, chain_id_mismatch}
    end.

send_withdraw_created_msg(SignedTx, #data{on_chain_id = Ch,
                                          session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id => Ch
           , data       => TxBin},
    aesc_session_noise:wdraw_created(Sn, Msg),
    log(snd, ?WDRAW_CREATED, Msg, Data).

check_withdraw_created_msg(#{ channel_id := ChanId
                            , data       := TxBin} = Msg,
                           #data{on_chain_id = ChanId} = Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    {ok, SignedTx, log(rcv, ?WDRAW_CREATED, Msg, Data)}.

send_withdraw_signed_msg(SignedTx, #data{on_chain_id = Ch,
                                         session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id  => Ch
           , data        => TxBin},
    aesc_session_noise:wdraw_signed(Sn, Msg),
    log(snd, ?WDRAW_SIGNED, Msg, Data).

check_withdraw_signed_msg(#{ channel_id := ChanId
                           , data       := TxBin} = Msg,
                          #data{on_chain_id = ChanId} = Data) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    {ok, SignedTx, log(rcv, ?WDRAW_SIGNED, Msg, Data)}.

send_withdraw_locked_msg(TxHash, #data{on_chain_id = ChanId,
                                       session     = Sn} = Data) ->
    Msg = #{ channel_id => ChanId
           , data       => TxHash },
    aesc_session_noise:wdraw_locked(Sn, Msg),
    log(snd, ?WDRAW_LOCKED, Msg, Data).

check_withdraw_locked_msg(#{ channel_id := ChanId
                           , data       := TxHash } = Msg,
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

send_update_msg(SignedTx, #data{ on_chain_id = OnChainId
                               , session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id => OnChainId
           , data       => TxBin },
    aesc_session_noise:update(Sn, Msg),
    log(snd, ?UPDATE, Msg, Data).

check_update_msg(Type, Msg, D) ->
    lager:debug("check_update_msg(~p)", [Msg]),
    try check_update_msg_(Type, Msg, D)
    catch
        error:E ->
            lager:error("CAUGHT ~p, Trace = ~p", [E, erlang:get_stacktrace()]),
            {error, E}
    end.

check_update_msg_(Type, #{ channel_id := ChanId
                         , data       := TxBin } = Msg,
                  #data{ on_chain_id = ChanId } = D) ->
    try aetx_sign:deserialize_from_binary(TxBin) of
        SignedTx ->
            check_signed_update_tx(Type, SignedTx, Msg, D)
    catch
        error:E ->
            lager:error("CAUGHT ~p, trace = ~p", [E, erlang:get_stacktrace()]),
            {error, {deserialize, E}}
    end.

check_signed_update_tx(Type, SignedTx, Msg,
                       #data{state = State, opts = Opts} = D) ->
    lager:debug("check_signed_update_tx(~p)", [SignedTx]),
    case check_update_tx(Type, SignedTx, State, Opts) of
        ok ->
            {ok, SignedTx, log(rcv, ?UPDATE, Msg, D)};
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
                       , data       := TxBin } = Msg,
                      #data{on_chain_id = ChanId} = D) ->
    try aetx_sign:deserialize_from_binary(TxBin) of
        SignedTx ->
            check_signed_update_ack_tx(SignedTx, Msg, D)
    catch
        error:E ->
            lager:error("CAUGHT ~p, trace = ~p", [E, erlang:get_stacktrace()]),
            {error, {deserialize, E}}
    end.

check_signed_update_ack_tx(SignedTx, Msg,
                           #data{state = State, opts = Opts} = D) ->
    HalfSignedTx = aesc_offchain_state:get_latest_half_signed_tx(State),
    try  ok = check_update_ack_(SignedTx, HalfSignedTx),
         {ok, D#data{state = aesc_offchain_state:add_signed_tx(
                               SignedTx, State, Opts),
                     log = log_msg(rcv, ?UPDATE_ACK, Msg, D#data.log)}}
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
    Updates = [aesc_offchain_update:op_transfer(aec_id:create(account, FromPub),
                                                aec_id:create(account, ToPub), Amount)],
    try  Tx1 = aesc_offchain_state:make_update_tx(Updates, State, Opts),
         D1 = request_signing(?UPDATE, Tx1, D),
         gen_statem:reply(From, ok),
         next_state(awaiting_signature, D1)
    catch
        error:Reason ->
            lager:error("CAUGHT ~p, trace = ~p", [Reason, erlang:get_stacktrace()]),
            keep_state(D, [{reply, From, {error, Reason}}])
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
     , data       => TxBin }.


check_shutdown_msg(#{channel_id := ChanId, data := TxBin} = Msg,
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
            {ok, SignedTx, log(rcv, ?SHUTDOWN, Msg, D)};
        false ->
            {error, shutdown_tx_validation}
    end.

serialize_close_mutual_tx(Tx) ->
    {_, Elems} = aesc_close_mutual_tx:serialize(Tx),
    lists:keydelete(nonce, 1, Elems).

check_shutdown_ack_msg(#{data := TxBin} = Msg,
                       #data{latest = {shutdown, MySignedTx}} = D) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    case aetx_sign:signatures(SignedTx) of
        [_,_] ->
            check_shutdown_msg_(SignedTx, MySignedTx, Msg, D);
        _ ->
            {error, not_mutually_signed}
    end.

check_shutdown_msg_(SignedTx, MySignedTx, Msg, D) ->
    %% TODO: More thorough checking
    case (aetx_sign:tx(SignedTx) == aetx_sign:tx(MySignedTx)) of
        true ->
            {ok, SignedTx, log(rcv, ?SHUTDOWN_ACK, Msg, D)};
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
           , data       => TxBin },
    aesc_session_noise:update_ack(Sn, Msg),
    log(snd, ?UPDATE_ACK, Msg, Data).

handle_update_conflict(Req, D) when ?UPDATE_REQ(Req) ->
    D1 = send_conflict_err_msg(Req, fallback_to_stable_state(D)),
    next_state(open, D1).

send_conflict_err_msg(Req, #data{ state = State
                                , on_chain_id = ChanId
                                , session     = Sn } = Data) ->
    {_, SignedTx} = aesc_offchain_state:get_latest_signed_tx(State),
    Round = tx_round(aetx_sign:tx(SignedTx)),
    Msg = #{ channel_id => ChanId
           , round      => Round
           , error_code => ?ERR_CONFLICT },
    send_conflict_msg(Req, Sn, Msg),
    report(conflict, Msg, Data),
    log(snd, conflict_msg_type(Req), Msg, Data).

conflict_msg_type(?UPDATE)        -> ?UPDATE_ERR;
conflict_msg_type(?DEP_CREATED)   -> ?DEP_ERR;
conflict_msg_type(?WDRAW_CREATED) -> ?WDRAW_ERR.

send_conflict_msg(?UPDATE, Sn, Msg) ->
    aesc_session_noise:update_error(Sn, Msg);
send_conflict_msg(?DEP_CREATED, Sn, Msg) ->
    aesc_session_noise:dep_error(Sn, Msg);
send_conflict_msg(?WDRAW_CREATED, Sn, Msg) ->
    aesc_session_noise:wdraw_error(Sn,Msg).


check_update_err_msg(Msg, D) ->
    check_op_error_msg(?UPDATE_ERR, Msg, D).

request_signing(Tag, Obj, #data{} = D) ->
    request_signing(Tag, Obj, Obj, D).

request_signing(Tag, Obj, Ref, #data{client = Client} = D) ->
    Msg = rpt_message(sign, Tag, Obj, D),
    Client ! {?MODULE, self(), Msg},
    lager:debug("signing(~p) requested", [Tag]),
    D#data{ latest = {sign, Tag, Ref}
          , log    = log_msg(req, sign, Msg, D#data.log)}.

default_minimum_depth(initiator  ) -> undefined;
default_minimum_depth(responder) -> ?MINIMUM_DEPTH.

start_min_depth_watcher(Type, SignedTx,
                        #data{watcher = Watcher0,
                              opts = #{minimum_depth := MinDepth}} = D) ->
    Tx = aetx_sign:tx(SignedTx),
    TxHash = aetx_sign:hash(SignedTx),
    evt({tx_hash, TxHash}),
    Nonce = aetx:nonce(Tx),
    evt({nonce, Nonce}),
    {OnChainId, D1} = on_chain_id(D, Tx),
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


on_chain_id(#data{on_chain_id = ID} = D, _) when ID =/= undefined ->
    {ID, D};
on_chain_id(D, Tx) ->
    {Mod, Txi} = aetx:specialize_callback(Tx),
    ID = Mod:channel_id(Txi),
    {ID, D#data{on_chain_id = ID}}.

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
    gproc:reg(Nbr, Pubkey),
    gproc:reg(Nbp).


gproc_name_by_role(Id, Role) ->
    {n, l, {aesc_channel, {Id, role, Role}}}.

gproc_name_by_pubkey(Id, Pubkey) ->
    {n, l, {aesc_channel, {Id, key, Pubkey}}}.

evt(_Msg) ->
    ok.

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
    {ok, ok, D#data{log = L#w{keep = Keep}}};
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

report_tags() ->
    maps:keys(default_report_flags()).

%% See `report_tags()` above: This map needs to contain all recognized tags.
default_report_flags() ->
    #{ info         => true
     , update       => true
     , leave        => true
     , conflict     => true
     , message      => true
     , error        => true
     , debug        => false
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

log(Op, Type, M, #data{log = Log0} = D) ->
    D#data{log = log_msg(Op, Type, M, Log0)}.

log_msg(Op, Type, M, #w{n = N, a = A, keep = Keep} = W) when N < Keep ->
    W#w{n = N+1, a = [{Op, Type, os:timestamp(), M}|A]};
log_msg(Op, Type, M, #w{a = A} = W) ->
    W#w{n = 1, a = [{Op, Type, os:timestamp(), M}], b = A}.

win_to_list(#w{a = A, b = B}) ->
    A ++ B.
