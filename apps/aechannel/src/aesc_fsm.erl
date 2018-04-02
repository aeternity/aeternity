-module(aesc_fsm).
-behaviour(gen_statem).

-export([start_link/1]).

%% API
-export([initiate/2,        %% (#channel{}, port())
         participate/4]).   %% (#channel{}, minimum_depth(), host(), port())

-export([message/2]).

-export([init/1,
         callback_mode/0,
         code_change/4,
         terminate/3]).

%% FSM states
-export([initialized/3,
         accepted/3,
         awaiting_open/3,
         half_signed/3,
         signed/3,
         open/3,
         closing/3,
         disconnected/3]).

-include_lib("apps/aecore/include/common.hrl").

-type role() :: initiator | participant.

-define(MINIMUM_DEPTH, 4). % number of blocks until an opening tx
                           % should be considered final

-record(state, { sequence_number    :: non_neg_integer()
               , data               :: binary()
               , initiator_amount   :: aesc_channels:amount()
               , participant_amount :: aesc_channels:amount()
               }).

-record(data, { role                   :: role()
              , state                  :: #state{}
              , session                :: pid()
              , client                 :: pid()
              , channel                :: aesc_channels:channel()
              , minimum_depth          :: non_neg_integer()
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

%% ======================================================================
%% Timer values

timer(Name) ->
    {timeout, Name, timeout(Name), Name}.

timeout(accept)         -> 10000;
timeout(funding_create) -> 10000;
timeout(funding_sign)   -> 10000;
timeout(funding_lock)   -> 10000;
timeout(idle)           -> 10000.

%%
%% ======================================================================

initiate(Channel, Port) ->
    start_link(#{role => initiator
               , port => Port
               , channel => Channel}).

participate(Channel, MinDepth, Host, Port) ->
    start_link(#{role => participant
               , host => Host, port => Port
               , channel => Channel
               , minimum_depth => MinDepth}).

start_link(#{channel := Ch} = Arg) ->
    Id = aesc_channels:id(Ch),
    Name = gproc_name(Id),
    gen_statem:start_link({via, gproc, Name} ,?MODULE, Arg, [{debug, [trace]}]).


init(#{role := Role, channel := Channel} = Arg) ->
    Opts = maps:get(opts, Arg, []),
    DefaultMinDepth = default_minimum_depth(Role),
    MinDepth = maps:get(minimum_depth, Arg, DefaultMinDepth),
    Session = start_session(Arg, Opts),
    Data = #data{role = Role, session = Session, channel = Channel,
                 minimum_depth = MinDepth},
    %% TODO: Amend the fsm above to include this step. We have transport-level
    %% connectivity, but not yet agreement on the channel parameters. We will next send
    %% a channel_open() message and await a channel_accept().
    case Role of
        initiator ->
            {ok, initialized, send_open_msg(Data)};
        participant ->
            {ok, awaiting_open, Data}
    end.

start_session(#{role := initiator, port := Port}, Opts) ->
    ok(aesc_session_noise:accept(Port, Opts));
start_session(#{role := participant, host := Host, port := Port}, Opts) ->
    ok(aesc_session_noise:connect(Host, Port, Opts)).

ok({ok, X}) -> X.

awaiting_open(enter, _OldSt, D) ->
    {keep_state, D, [timer(open)]};
awaiting_open(cast, {channel_open, Msg}, D) ->
    case check_open_msg(Msg, D) of
        {ok, D1} ->
            send_channel_accept(D1),
            {next_state, initialized, D1};
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
            {next_state, accepted, D1};
        {error, _} = Error ->
            close(Error, D)
    end;
initialized({timeout, accept} = T, _Msg, D) ->
    close(T, D);
initialized(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D).

accepted(enter, _OldSt, D) ->
    {keep_state, D, [timer(funding_create)]};
accepted(cast, {funding_created, _Msg}, D) ->
    {next_state, half_signed, D};
accepted(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D);
accepted({timeout, funding_create} = T, _Msg, D) ->
    close(T, D).

half_signed(enter, _OldSt, D) ->
    {keep_state, D, [timer(funding_sign)]};
half_signed(cast, {funding_signed, _Msg}, D) ->
    {next_state, signed, D};
half_signed({timeout, funding_sign} = T, _Msg, D) ->
    close(T, D);
half_signed(cast, {disconnect, _Msg}, D) ->
    close(disconnect, D).

signed(enter, _OldSt, D) ->
    {keep_state, D, [timer(funding_lock)]};
signed(cast, {funding_locked, _Msg}, D) ->
    {next_state, signed, D};
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

send_open_msg(#data{channel = Ch,
                    session = Sn}) ->
    ChainHash = aec_chain:genesis_hash(),
    Msg = #{ chain_hash           => ChainHash
           , temporary_channel_id => aesc_channels:id(Ch)
           , lock_period          => aesc_channels:lock_period(Ch)
           , push_amount          => aesc_channels:push_amount(Ch)
           , initiator_amount     => aesc_channels:initiator_amount(Ch)
           , participant_amount   => aesc_channels:participant_amount(Ch)
           , channel_reserve      => aesc_channels:channel_reserve(Ch)
           },
    aesc_session_noise:channel_open(Sn, Msg).

check_open_msg(#{ chain_hash           := _ChainHash
                , temporary_channel_id := _ChanId
                , lock_period          := _LockPeriod
                , push_amount          := _PushAmt
                , initiator_amount     := _InitiatorAmt
                , responder_amount     := _ResponderAmt
                , channel_reserve      := _ChanReserve
                , initiator_pubkey     := _InitiatorPubkey},
               #data{} = Data) ->
    %% TODO: Implement checks
    {ok, Data}.

send_channel_accept(#data{channel = Ch,
                          session = Sn,
                          minimum_depth = MinDepth}) ->
    ChainHash = aec_chain:genesis_hash(),
    Msg = #{ chain_hash           => ChainHash
           , temporary_channel_id => aesc_channels:id(Ch)
           , minimum_depth        => MinDepth
           , lock_period          => aesc_channels:lock_period(Ch)
           , push_amount          => aesc_channels:push_amount(Ch)
           , initiator_amount     => aesc_channels:initiator_amount(Ch)
           , participant_amount   => aesc_channels:participant_amount(Ch)
           },
    aesc_session_noise:channel_accept(Sn, Msg).

check_accept_msg(#{ temporary_channel_id := _ChanId
                  , chain_hash           := _ChainHash
                  , minimum_depth        := MinDepth
                  , initiator_amount     := _InitiatorAmt
                  , responder_amount     := _ResponderAmt
                  , channel_reserve      := _ChanReserve
                  , initiator_pubkey     := _InitiatorPubkey},
                 #data{} = Data) ->
    %% TODO: implement checks
    {ok, Data#data{minimum_depth = MinDepth}}.


default_minimum_depth(initiator  ) -> undefined;
default_minimum_depth(participant) -> ?MINIMUM_DEPTH.

gproc_name(Id) ->
    {n, l, {aesc_channel, Id}}.
