-module(aesc_fsm).
-behaviour(gen_statem).

-export([start_link/1]).

%% API
-export([message/2]).

-export([init/1,
         callback_mode/0,
         code_change/4,
         terminate/3]).

%% FSM states
-export([initialized/3,
         accepted/3,
         half_signed/3,
         signed/3,
         open/3,
         closing/3,
         disconnected/3]).

-record(data, {}).

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

message(Fsm, Msg) ->
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

start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, [{debug, [trace]}]).


init(_Args) ->
    {ok, initialized, #data{}}.


initialized(enter, _OldSt, D) ->
    {keep_state, D, [timer(accept)]};
initialized(cast, {channel_accept, _Msg}, D) ->
    {next_state, accepted, D};
initialized({timeout, accept} = T, _Msg, D) ->
    close(T, D);
initialized(cast, disconnect, D) ->
    close(disconnect, D).

accepted(enter, _OldSt, D) ->
    {keep_state, D, [timer(funding_create)]};
accepted(cast, {funding_created, _Msg}, D) ->
    {next_state, half_signed, D};
accepted(cast, disconnect, D) ->
    close(disconnect, D);
accepted({timeout, funding_create} = T, _Msg, D) ->
    close(T, D).

half_signed(enter, _OldSt, D) ->
    {keep_state, D, [timer(funding_sign)]};
half_signed(cast, {funding_signed, _Msg}, D) ->
    {next_state, signed, D};
half_signed({timeout, funding_sign} = T, _Msg, D) ->
    close(T, D);
half_signed(cast, disconnect, D) ->
    close(disconnect, D).

signed(enter, _OldSt, D) ->
    {keep_state, D, [timer(funding_lock)]};
signed(cast, {funding_locked, _Msg}, D) ->
    {next_state, signed, D};
signed({timeout, funding_lock} = T, _Msg, D) ->
    close(T, D);
signed(cast, {shutdown, _Msg}, D) ->
    {next_state, closing, D};
signed(cast, disconnect, D) ->
    {next_state, disconnected, D}.

open(enter, _OldSt, D) ->
    {keep_state, D, [timer(idle)]};
open(cast, update, D) ->
    {keep_state, D};
open(cast, {Upd, _Msg}, D) when Upd =:= update_deposit;
                                Upd =:= update_withdrawal ->
    {keep_state, D};
open(cast, {shutdown, _Msg}, D) ->
    {next_state, closing, D};
open(cast, disconnect, D) ->
    {next_state, disconnected, D};
open({timeout, idle} = T, _Msg, D) ->
    close(T, D).

closing(cast, disconnect, D) ->
    {next_state, disconnected, D};
closing(cast, closing_signed, D) ->
    close(closing_signed, D).

disconnected(cast, {channel_reestablish, _Msg}, D) ->
    {next_state, closing, D}.

close(Reason, D) ->
    {stop, Reason, D}.


terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, OldState, OldData, _Extra) ->
    {ok, OldState, OldData}.
