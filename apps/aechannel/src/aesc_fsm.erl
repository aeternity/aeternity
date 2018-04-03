-module(aesc_fsm).
-behaviour(gen_statem).

-export([start_link/1]).

%% API
-export([initiate/2,        %% (host(), port(), Opts :: #{})
         participate/3]).   %% (port(), Opts :: #{})

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

-export([where/1]).

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

-type role() :: initiator | participant.
-type sign_tag() :: create_tx
                  | funding_created.

-define(MINIMUM_DEPTH, 4). % number of blocks until an opening tx
                           % should be considered final

-record(state, { sequence_number    :: non_neg_integer()
               , data               :: binary()
               , initiator_amount   :: aesc_channels:amount()
               , participant_amount :: aesc_channels:amount()
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

message(Fsm, {T, _} = Msg) when T =:= channel_accept
                              ; T =:= funding_created
                              ; T =:= funding_signed
                              ; T =:= funding_locked
                              ; T =:= update_deposit
                              ; T =:= update_withdrawal
                              ; T =:= disconnect
                              ; T =:= shutdown
                              ; T =:= channel_reestablish ->
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

where(ChanId) ->
    gproc:where(gproc_name(ChanId)).

%% ======================================================================
%% Timer values

timer(Name) ->
    {timeout, Name, timeout(Name), Name}.

timeout(open)           -> 10000;
timeout(accept)         -> 10000;
timeout(funding_create) -> 10000;
timeout(funding_sign)   -> 10000;
timeout(funding_lock)   -> 10000;
timeout(idle)           -> 10000;
timeout(sign)           -> 10000.

%%
%% ======================================================================

initiate(Port, #{} = Opts) ->
    start_link(#{role   => initiator
               , port   => Port
               , opts   => Opts}).

participate(Host, Port, #{} = Opts) ->
    start_link(#{role   => participant
               , host   => Host, port => Port
               , opts   => Opts}).

start_link(#{} = Arg0) ->
    Arg = maps:merge(Arg0, #{client => self()}),
    gen_statem:start_link(?MODULE, Arg, [{debug, [trace]}]).


init(#{role := Role, client := Client} = Arg) ->
    Opts = maps:get(opts, Arg, []),
    DefMinDepth = default_minimum_depth(Role),
    Opts1 = case {maps:find(minimum_depth, Opts), Role} of
                {error, participant} -> Opts#{minimum_depth => DefMinDepth};
                _                    -> Opts
            end,
    Session = start_session(Arg, Opts1),
    Data = #data{role = Role,
                 client = Client,
                 session = Session,
                 opts = Opts1},
    %% TODO: Amend the fsm above to include this step. We have transport-level
    %% connectivity, but not yet agreement on the channel parameters. We will next send
    %% a channel_open() message and await a channel_accept().
    case Role of
        initiator ->
            {ok, initialized, send_open_msg(Data)};
        participant ->
            {ok, awaiting_open, Data}
    end.

%% As per CHANNELS.md, the participant is regarded as the one typically
%% providing the service, and the initiator connects.
start_session(#{role := participant, port := Port}, Opts) ->
    ok(aesc_session_noise:accept(Port, Opts));
start_session(#{role := initiator, host := Host, port := Port}, Opts) ->
    ok(aesc_session_noise:connect(Host, Port, Opts)).

ok({ok, X}) -> X.

awaiting_open(enter, _OldSt, D) ->
    {keep_state, D, [timer(open)]};
awaiting_open(cast, {channel_open, Msg}, D) ->
    case check_open_msg(Msg, D) of
        {ok, D1} ->
            gproc_register(D),
            {next_state, accepted, send_channel_accept(D1)};
        {error, _} = Error ->
            close(Error, D)
    end;
awaiting_open({timeout, open} = T, _Msg, D) ->
    close(T, D);
awaiting_open(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D).

initialized(enter, _OldSt, D) ->
    {keep_state, D, [timer(accept)]};
initialized(cast, {channel_accept, Msg}, D) ->
    case check_accept_msg(Msg, D) of
        {ok, D1} ->
            gproc_register(D),
            {ok, CTx} = create_tx_for_signing(D1),
            D2 = request_signing(create_tx, CTx, D1),
            {next_state, awaiting_signature, D2};
        {error, _} = Error ->
            close(Error, D)
    end;
initialized({timeout, accept} = T, _Msg, D) ->
    close(T, D);
initialized(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D).

awaiting_signature(enter, _OldSt, D) ->
    {keep_state, D, [timer(sign)]};
awaiting_signature(cast, {signed, create_tx, Tx},
                   #data{latest = {signed, create_tx, _CTx}} = D) ->
    {next_state, half_signed,
     send_funding_created_msg(Tx, D#data{latest = undefined})};
awaiting_signature(cast, {signed, funding_created, Tx},
                   #data{latest = {signed, funding_created, _CTx}} = D) ->
    {next_state, signed,
     send_funding_signed_msg(Tx, D#data{latest = undefined})};
awaiting_signature({timeout, sign} = T, _Msg, D) ->
    close(T, D);
awaiting_signature(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D).

accepted(enter, _OldSt, D) ->
    {keep_state, D, [timer(funding_create)]};
accepted(cast, {funding_created, Msg}, D) ->
    case check_funding_created_msg(Msg, D) of
        {ok, CTx, D1} ->
            D2 = request_signing(funding_created, CTx, D1),
            {next_state, awaiting_signature, D2}
        %% {error, _} = Error ->
        %%     close(Error, D)
    end;
accepted(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D);
accepted({timeout, funding_create} = T, _Msg, D) ->
    close(T, D).

half_signed(enter, _OldSt, D) ->
    {keep_state, D, [timer(funding_sign)]};
half_signed(cast, {funding_signed, Msg}, D) ->
    case check_funding_signed_msg(Msg, D) of
        {ok, Tx, D1} ->
            D2 = D1#data{create_tx = Tx},
            {ok, Watcher} = start_min_depth_watcher(D2),
            {next_state, awaiting_locked, D2#data{latest = {watcher, Watcher}}}
        %% {error, _} = Error ->
        %%     close(Error, D)
    end;
half_signed({timeout, funding_sign} = T, _Msg, D) ->
    close(T, D);
half_signed(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D).

awaiting_locked(enter, _OldSt, D) ->
    {keep_state, D, [timer(funding_lock)]};
awaiting_locked(cast, {own_funding_locked, ChainId}, D) ->
    {next_state, signed,
     send_funding_locked_msg(D#data{on_chain_id = ChainId,
                                    latest = undefined})};
awaiting_locked(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D);
awaiting_locked({timeout, funding_lock} = T, _Msg, D) ->
    close(T, D).

signed(enter, _OldSt, D) ->
    {keep_state, D, [timer(funding_lock)]};
signed(cast, {funding_locked, Msg}, D) ->
    case check_funding_locked_msg(Msg, D) of
        {ok, D1} ->
            {next_state, open, D1};
        {error, _} = Error ->
            close(Error, D)
    end;
signed({timeout, funding_lock} = T, _Msg, D) ->
    close(T, D);
signed(cast, {shutdown, _Msg}, D) ->
    {next_state, closing, D};
signed(cast, {disconnect, _Msg}, D) ->
    {next_state, disconnected, D}.

open(enter, _OldSt, D) ->
    {keep_state, D, [timer(idle)]};
open(cast, {Upd, _Msg}, D) when Upd =:= update_deposit;
                                Upd =:= update_withdrawal ->
    {keep_state, D};
open(cast, {shutdown, _Msg}, D) ->
    {next_state, closing, D};
open(cast, {disconnect, _Msg}, D) ->
    {next_state, disconnected, D};
open({timeout, idle} = T, _Msg, D) ->
    close(T, D).

closing(cast, {disconnect, _Msg}, D) ->
    {next_state, disconnected, D};
closing(cast, {closing_signed, _Msg}, D) ->
    close(closing_signed, D).

disconnected(cast, {channel_reestablish, _Msg}, D) ->
    {next_state, closing, D}.

close(Reason, D) ->
    {stop, Reason, D}.


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
     , participant        := Participant
     , push_amount        := PushAmount
     , initiator_amount   := InitiatorAmount
     , participant_amount := ParticipantAmount
     , channel_reserve    := ChannelReserve } = Opts,
    ChainHash = aec_chain:genesis_hash(),
    %%
    %% Generate a temporary channel id
    ChannelId = aesc_channels:id(Initiator,
                                 erlang:unique_integer(),
                                 Participant),
    Msg = #{ chain_hash           => ChainHash
           , temporary_channel_id => ChannelId
           , lock_period          => LockPeriod
           , push_amount          => PushAmount
           , initiator_amount     => InitiatorAmount
           , participant_amount   => ParticipantAmount
           , channel_reserve      => ChannelReserve
           },
    aesc_session_noise:channel_open(Sn, Msg),
    Data#data{channel_id = ChannelId}.

check_open_msg(#{ chain_hash           := ChainHash
                , temporary_channel_id := ChanId
                , lock_period          := LockPeriod
                , push_amount          := PushAmt
                , initiator_amount     := InitiatorAmt
                , participant_amount   := ParticipantAmt
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
                     , participant_amount => ParticipantAmt
                     , channel_reserve    => ChanReserve},
            {ok, Data#data{channel_id = ChanId,
                           opts       = Opts1}};
        _ ->
            {error, chain_hash_mismatch}
    end.

send_channel_accept(#data{opts          = Opts,
                          session       = Sn,
                          channel_id    = ChanId} = Data) ->
    #{ lock_period        := LockPeriod
     , push_amount        := PushAmt
     , minimum_depth      := MinDepth
     , initiator_amount   := InitiatorAmt
     , participant_amount := ParticipantAmt } = Opts,
    ChainHash = aec_chain:genesis_hash(),
    Msg = #{ chain_hash           => ChainHash
           , temporary_channel_id => ChanId
           , minimum_depth        => MinDepth
           , lock_period          => LockPeriod
           , push_amount          => PushAmt
           , initiator_amount     => InitiatorAmt
           , participant_amount   => ParticipantAmt
           },
    aesc_session_noise:channel_accept(Sn, Msg),
    Data.

check_accept_msg(#{ temporary_channel_id := ChanId
                  , chain_hash           := ChainHash
                  , minimum_depth        := MinDepth
                  , initiator_amount     := _InitiatorAmt
                  , responder_amount     := _ResponderAmt
                  , channel_reserve      := _ChanReserve
                  , initiator            := _InitiatorPubkey},
                 #data{channel_id = ChanId,
                       opts = Opts} = Data) ->
    %% TODO: implement more checks
    case aec_chain:genesis_hash() of
        ChainHash ->
            {ok, Data#data{opts = Opts#{minimum_depth => MinDepth}}};
        _ ->
            {error, chain_hash_mismatch}
    end.

create_tx_for_signing(#data{opts = Opts}) ->
    {ok, _} = Ok = aesc_create_tx:new(Opts),
    Ok.

send_funding_created_msg(SignedTx, #data{channel_id = Ch,
                                         session   = Sn} = Data) ->
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


request_signing(Tag, Obj, #data{client    = Client,
                               channel_id = ChanId} = Data) ->
    Msg = {sign, Tag, Obj},
    Client ! {?MODULE, self(), ChanId, Msg},
    Data#data{latest = Msg}.

default_minimum_depth(initiator  ) -> undefined;
default_minimum_depth(participant) -> ?MINIMUM_DEPTH.

start_min_depth_watcher(#data{create_tx = SignedTx,
                              opts = #{initiator     := Initiator,
                                       participant   := Participant,
                                       minimum_depth := MinDepth}} = Data) ->
    Tx = aetx_sign:tx(SignedTx),
    {_Type, CTx} = aetx:specialize_type(Tx),
    Nonce = aesc_create_tx:nonce(CTx),
    OnChainId = aesc_channels:id(Initiator, Nonce, Participant),
    {ok, _Watcher} = aesc_fsm_min_depth_watcher:start_link(OnChainId, MinDepth),
    {ok, Data#data{on_chain_id = OnChainId}}.

gproc_register(#data{channel_id = ChanId}) ->
    gproc:reg(gproc_name(ChanId)).

gproc_name(Id) ->
    {n, l, {aesc_channel, Id}}.
