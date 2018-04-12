-module(aesc_fsm).
-behaviour(gen_statem).

-export([start_link/1]).

%% API
-export([initiate/3,        %% (host(), port(), Opts :: #{})
         participate/2]).   %% (port(), Opts :: #{})

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
         half_signed/3,
         signed/3,
         open/3,
         closing/3,
         disconnected/3]).

-include_lib("apps/aecore/include/common.hrl").

-type role() :: initiator | responder.
-type sign_tag() :: create_tx
                  | funding_created.

-define(MINIMUM_DEPTH, 4). % number of blocks until an opening tx
                           % should be considered final

-record(state, { sequence_number    :: non_neg_integer()
               , data               :: binary()
               , initiator_amount   :: aesc_channels:amount()
               , responder_amount   :: aesc_channels:amount()
               }).

-record(data, { role                   :: role()
              , state                  :: undefined | #state{}
              , session                :: pid()
              , client                 :: pid()
              , opts                   :: map()
              , channel_id             :: undefined | binary()
              , on_chain_id            :: undefined | binary()
              , create_tx              :: undefined | any()
              , latest = undefined     :: undefined | any()
              }).

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
                              ; T =:= update_signed
                              ; T =:= disconnect
                              ; T =:= shutdown
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

timer(Name, #data{opts = #{timeouts := TOs}}) ->
    {timeout, maps:get(Name, TOs), Name}.

default_timeouts() ->
    #{ open           => 5000
     , accept         => 2000
     , funding_create => 2000
     , funding_sign   => 2000
     , funding_lock   => 30000
     , idle           => 60000
     , sign           => 5000
     }.

%%
%% ======================================================================

initiate(Host, Port, #{} = Opts0) ->
    lager:debug("initiate(~p, ~p, ~p)", [Host, Port, Opts0]),
    Opts = maps:merge(#{client => self()}, Opts0),
    start_link(#{role => initiator
               , host => Host
               , port => Port
               , opts => Opts}).

participate(Port, #{} = Opts0) ->
    lager:debug("participate(~p, ~p)", [Port, Opts0]),
    Opts = maps:merge(#{client => self()}, Opts0),
    start_link(#{role => responder
               , port => Port
               , opts => Opts}).

start_link(#{} = Arg) ->
    gen_statem:start_link(?MODULE, Arg, [{debug, [trace]}]).


init(#{role := Role} = Arg) ->
    #{client := Client} = Opts0 = maps:get(opts, Arg, #{}),
    DefMinDepth = default_minimum_depth(Role),
    Opts = check_opts(
             [
              fun(O) -> check_minimum_depth_opt(DefMinDepth, Role, O) end,
              fun check_timeout_opt/1
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
            {ok, initialized, send_open_msg(Data)};
        responder ->
            {ok, awaiting_open, Data}
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
    case maps:find(timeouts, Opts) of
        {ok, TOs} ->
            TOs1 = maps:merge(default_timeouts(), TOs),
            Opts#{timeouts => TOs1};
        error ->
            Opts#{timeouts => default_timeouts()}
    end.

%% As per CHANNELS.md, the responder is regarded as the one typically
%% providing the service, and the initiator connects.
start_session(#{role := responder, port := Port}, Opts) ->
    NoiseOpts = maps:get(noise, Opts, []),
    ok(aesc_session_noise:accept(Port, NoiseOpts));
start_session(#{role := initiator, host := Host, port := Port}, Opts) ->
    NoiseOpts = maps:get(noise, Opts, []),
    ok(aesc_session_noise:connect(Host, Port, NoiseOpts)).

ok({ok, X}) -> X.

awaiting_open(enter, _OldSt, D) ->
    {keep_state, D, [timer(open, D)]};
awaiting_open(cast, {channel_open, Msg}, #data{role = responder} = D) ->
    case check_open_msg(Msg, D) of
        {ok, D1} ->
            report_info(channel_open, D1),
            gproc_register(D1),
            {next_state, accepted, send_channel_accept(D1)};
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_open(timeout, open = T, D) ->
    close({timeout, T}, D);
awaiting_open(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D).

initialized(enter, _OldSt, D) ->
    {keep_state, D, [timer(accept, D)]};
initialized(cast, {channel_accept, Msg}, #data{role = initiator} = D) ->
    case check_accept_msg(Msg, D) of
        {ok, D1} ->
            gproc_register(D1),
            report_info(channel_accept, D1),
            {ok, CTx} = create_tx_for_signing(D1),
            ok = request_signing(create_tx, CTx, D1),
            D2 = D1#data{latest = {sign, create_tx, CTx}},
            {next_state, awaiting_signature, D2};
        {error, _} = Error ->
            close(Error, D)
    end;
initialized(timeout, accept = T, D) ->
    close({timeout, T}, D);
initialized(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D).

awaiting_signature(enter, _OldSt, D) ->
    {keep_state, D, [timer(sign, D)]};
awaiting_signature(cast, {signed, create_tx, Tx},
                   #data{role = initiator, latest = {sign, create_tx, _CTx}} = D) ->
    {next_state, half_signed,
     send_funding_created_msg(Tx, D#data{latest = undefined})};
awaiting_signature(cast, {signed, funding_created, SignedTx},
                   #data{role = responder,
                         latest = {sign, funding_created, HSCTx}} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    HSCTx, aetx_sign:signatures(SignedTx)),
    D1 = send_funding_signed_msg(NewSignedTx, D#data{create_tx = NewSignedTx}),
    {ok, Watcher, D2} = start_min_depth_watcher(D1),
    {next_state, awaiting_locked, D2#data{latest = {watcher, Watcher}}};
awaiting_signature(cast, {signed, update, SignedTx}, D) ->
    D1 = send_update_msg(SignedTx, D#data{state = [SignedTx|D#data.state]}),
    {next_state, awaiting_update_ack, D1#data{latest = undefined}};
awaiting_signature(cast, {signed, update_ack, SignedTx},
                   #data{latest = {sign, update_ack, OCTx}} = D) ->
    NewSignedTx = aetx_sign:add_signatures(
                    OCTx, aetx_sign:signatures(SignedTx)),
    D1 = send_update_ack_msg(NewSignedTx, D),
    D2 = D1#data{state = [NewSignedTx | clean_state(D1#data.state)]},
    {next_state, open, D2};
awaiting_signature(timeout, sign = T, D) ->
    close({timeout, T}, D);
awaiting_signature(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D).

accepted(enter, _OldSt, D) ->
    {keep_state, D, [timer(funding_create, D)]};
accepted(cast, {funding_created, Msg}, #data{role = responder} = D) ->
    case check_funding_created_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report_info(funding_created, D1),
            lager:debug("funding_created: ~p", [SignedTx]),
            ok = request_signing(funding_created, aetx_sign:tx(SignedTx), D1),
            D2 = D1#data{latest = {sign, funding_created, SignedTx}},
            {next_state, awaiting_signature, D2}
        %% {error, _} = Error ->
        %%     close(Error, D)
    end;
accepted(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D);
accepted(timeout, funding_create = T, D) ->
    close({timeout, T}, D).

half_signed(enter, _OldSt, D) ->
    {keep_state, D, [timer(funding_sign, D)]};
half_signed(cast, {funding_signed, Msg}, #data{role = initiator} = D) ->
    case check_funding_signed_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report_info(funding_signed, D1),
            ok = aec_tx_pool:push(SignedTx),
            D2 = D1#data{create_tx = SignedTx},
            {ok, Watcher, D3} = start_min_depth_watcher(D2),
            {next_state, awaiting_locked, D3#data{latest = {watcher, Watcher}}}
        %% {error, _} = Error ->
        %%     close(Error, D)
    end;
half_signed(timeout, funding_sign = T, D) ->
    close({timeout, T}, D);
half_signed(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D).

awaiting_locked(enter, _OldSt, D) ->
    {keep_state, D, [timer(funding_lock, D)]};
awaiting_locked(cast, {own_funding_locked, ChainId}, D) ->
    report_info(own_funding_locked, D),
    {next_state, signed,
     send_funding_locked_msg(D#data{on_chain_id = ChainId,
                                    latest = undefined})};
awaiting_locked(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D);
awaiting_locked(timeout, funding_lock = T, D) ->
    close({timeout, T}, D).

awaiting_initial_state(enter, _OldSt, D) ->
    {keep_state, D, [timer(accept, D)]};  % reusing accept timer (TODO ?)
awaiting_initial_state(cast, {update, Msg}, #data{role = responder} = D) ->
    case check_update_msg(Msg, D) of
        {ok, SignedTx, D1} ->
            report_info(update, D1),
            ok = request_signing(update_ack, aetx_sign:tx(SignedTx), D1),
            D2 = D1#data{latest = {sign, update_ack, SignedTx}},
            {next_state, awaiting_signature, D2};
        {error,_} = _Error ->
            %% TODO: do we do a dispute challenge here?
            close(Error, D)
    end;
awaiting_initial_state(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D);
awaiting_initial_state(timeout, accept, D) ->
    close({timeout, awaiting_initial_state}, D).

awaiting_update_ack(enter, _OldSt, D) ->
    {keep_state, D, [timer(accept)]};
awaiting_update_ack(cast, {update_ack, SignedTx}, #data{role = initiator} = D) ->
    case check_update_ack(SignedTx, D) of
        {ok, D1} ->
            {next_state, open, D1};
        {error, _} = Error ->
            close(Error)
    end;
awaiting_update_ack(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D);
awaiting_update_ack(timeout, accept, D) ->
    close({timeout, awaiting_update_ack}, D).

signed(enter, _OldSt, D) ->
    {keep_state, D, [timer(funding_lock, D)]};
signed(cast, {funding_locked, Msg}, D) ->
    case check_funding_locked_msg(Msg, D) of
        {ok, D1} ->
            report_info(funding_locked, D1),
            case D1#data.role of
                initiator ->
                    OCTx = initial_state(D1),
                    ok   = request_signing(state, OCTx, D1),
                    D2   = D1#data{latest = {sign, state_update, OCTx}},
                    {next_state, awaiting_signature, D2};
                responder ->
                    {next_state, awaiting_initial_state, D1}
            end;
        {error, _} = Error ->
            close(Error, D)
    end;
signed(timeout, funding_lock = T, D) ->
    close({timeout, T}, D);
signed(cast, {shutdown, _Msg}, D) ->
    {next_state, closing, D};
signed(cast, {disconnect, _Msg}, D) ->
    {next_state, disconnected, D}.

open(enter, _OldSt, D) ->
    {keep_state, D, [timer(idle, D)]};
open(cast, {Upd, _Msg}, D) when Upd =:= update_deposit;
                                Upd =:= update_withdrawal ->
    {keep_state, D};
open(cast, {shutdown, _Msg}, D) ->
    {next_state, closing, D};
open(cast, {disconnect, _Msg}, D) ->
    {next_state, disconnected, D};
open(timeout, idle = T, D) ->
    close({timeout, T}, D).

closing(cast, {disconnect, _Msg}, D) ->
    {next_state, disconnected, D};
closing(cast, {closing_signed, _Msg}, D) ->
    close(closing_signed, D).

disconnected(cast, {channel_reestablish, _Msg}, D) ->
    {next_state, closing, D}.

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
                    Msg = #{ channel_id => ChId
                           , data       => error_binary(E) },
                    aesc_session_noise:error(Sn, Msg);
                _ ->
                    no_msg
            end
    end.

cur_channel_id(#data{channel_id = TChId,
                     on_chain_id = PChId}) ->
    case PChId == undefined of
        true  -> TChId;
        false -> PChId
    end.

error_binary(E) when is_atom(E) ->
    atom_to_binary(E, latin1).


terminate(_Reason, _State, _Data) ->
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

send_update_msg(SignedTx, #data{ on_chain_id = OnChainId,
                               , session     = Sn} = Data) ->
    TxBin = aetx_sign:serialize_to_binary(SignedTx),
    Msg = #{ channel_id => ChanId
           , data       => TxBin },
    aesc_session_noise:update(Sn, Msg),
    Data.

check_update_msg(#{ channel_id := ChanId
                  , data       := TxBin },
                 #data{ on_chain_id = ChanId
                      , state       = State } = D) ->
    SignedTx = aetx_sign:deserialize_from_binary(TxBin),
    case State of
        [LastSignedTx|_] ->
            foo
    end.


request_signing(Tag, Obj, #data{client    = Client,
                                channel_id = ChanId}) ->
    Msg = {sign, Tag, Obj},
    Client ! {?MODULE, self(), ChanId, Msg},
    ok.

default_minimum_depth(initiator  ) -> undefined;
default_minimum_depth(responder) -> ?MINIMUM_DEPTH.

start_min_depth_watcher(#data{create_tx = SignedTx,
                              opts = #{initiator     := Initiator,
                                       responder     := Responder,
                                       minimum_depth := MinDepth}} = Data) ->
    Tx = aetx_sign:tx(SignedTx),
    TxHash = aetx:hash(Tx),
    evt({tx_hash, TxHash}),
    {_Type, CTx} = Sp = aetx:specialize_type(Tx),
    evt({specialize_type, Sp}),
    Nonce = aesc_create_tx:nonce(CTx),
    evt({nonce, Nonce}),
    OnChainId = aesc_channels:id(Initiator, Nonce, Responder),
    evt({on_chain_id, OnChainId}),
    {ok, Watcher} = aesc_fsm_min_depth_watcher:start_link(TxHash, OnChainId, MinDepth),
    evt({watcher, Watcher}),
    {ok, Watcher, Data#data{on_chain_id = OnChainId}}.

initial_state(#data{create_tx = SignedTx,
                    on_chain_id =>  ChanId } = D) ->
    Tx = aetx_sign:tx(SignedTx),
    {Mod, CTx} = aetx:specialize_type(Tx),
    Initiator = Mod:initiator(CTx),
    Responder = Mod:responder(CTx),
    InitiatorAmount = Mod:initiator_amount(CTx),
    ResponderAmount = Mod:responder_amount(CTx),
    aesc_offchain_tx:new(#{ channel_id  => ChanId
                          , initiator   => Initiator
                          , responder   => Responder
                          , initiator_amount => InitiatorAmount
                          , responder_amount => ResponderAmount
                          , state       => <<>>
                          , sequence_number => 0 }).


gproc_register(#data{role = Role, channel_id = ChanId}) ->
    gproc:reg(gproc_name(ChanId, Role)).

gproc_name(Id, Role) ->
    {n, l, {aesc_channel, {Id, Role}}}.


%% start_trace() ->
%%     dbg:tracer(),
%%     dbg:tpl(?MODULE, x),
%%     dbg:tp(aetx_sign, add_signatures, x),
%%     dbg:tp(aesc_channels, id, 3, x),
%%     dbg:tp(aesc_fsm_min_depth_watcher, x),
%%     dbg:p(all,[c]).

%% stop_trace() ->
%%     dbg:ctpl(?MODULE),
%%     dbg:stop().

evt(_Msg) ->
    ok.

report_info(St, #data{client = Client, channel_id = ChanId,
                       opts = #{report_info := DoRpt}}) ->
    lager:debug("report_info(~p, ~p, ~p)", [DoRpt, ChanId, St]),
    if DoRpt -> Client ! {?MODULE, self(), ChanId, {info, St}};
       true  -> ok
    end.
