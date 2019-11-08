%% @doc This modules provides stateless checks for various properties throughout
%% the channels application.
-module(aesc_checks).

-export([ state_password/1
        , state_password_in_opts/1
        , known_role/1
        , amounts/1
        , account/2
        , accounts/3
        , lock_period/1
        ]).

-include("aesc_codec.hrl").
-include("aechannel.hrl").

%% TODO: Make this configurable
%% No need for a stronger password policy
%% This check is only here to ensure that someone doesn't enter a 1-2 character password
-define(STATE_PASSWORD_MINIMUM_LENGTH, 6).

%% ==================================================================
%% Checks

-spec state_password_in_opts(map()) -> ok | {error, password_required_since_lima | invalid_password}.
state_password_in_opts(#{state_password := StatePassword}) ->
    state_password(StatePassword);
state_password_in_opts(Opts) when is_map(Opts)->
    ok.

-spec state_password(string()) -> ok | {error, invalid_password}.
state_password(StatePassword)
    when is_list(StatePassword), length(StatePassword) < ?STATE_PASSWORD_MINIMUM_LENGTH ->
    {error, invalid_password};
state_password(StatePassword) when is_list(StatePassword) ->
    ok.

-spec known_role(atom()) -> ok | {error, unsupported_role}.
known_role(Role) ->
    case lists:member(Role, ?KNOWN_ROLES) of
        true ->
            ok;
        false ->
            {error, unsupported_role}
    end.

-spec lock_period(integer()) -> ok | {error, lock_period_too_low}.
lock_period(V) when is_integer(V) andalso V < 0 ->
    {error, lock_period_too_low};
lock_period(V) when is_integer(V) ->
    ok.

accounts(any, Responder, responder) ->
    account(Responder, responder_not_found);
accounts(Initiator, Responder, _Role) ->
    Checks = [ fun() -> account(Initiator, initiator_not_found) end
             , fun() -> account(Responder, responder_not_found) end
             ],
    aeu_validation:run(Checks).

account(A, Error) ->
    case account_type(A) of
        {ok, basic} ->
            ok;
        {ok, generalized} ->
            ok;
        _ ->
            {error, Error}
    end.

amounts(#{channel_reserve := ChannelReserve})
  when ChannelReserve < 0 ->
    {error, channel_reserve_too_low};
amounts(#{ push_amount := PushAmount})
  when PushAmount < 0 ->
    {error, push_amount_too_low};
amounts(#{ initiator_amount   := InitiatorAmount0
         , responder_amount   := ResponderAmount0
         , push_amount        := PushAmount
         , channel_reserve    := ChannelReserve }) ->
    InitiatorAmount = InitiatorAmount0 - PushAmount,
    ResponderAmount = ResponderAmount0 + PushAmount,
    case { InitiatorAmount >= ChannelReserve
         , ResponderAmount >= ChannelReserve } of
        {true,  true}  -> ok;
        {false, true}  -> {error, insufficient_initiator_amount};
        {true,  false} -> {error, insufficient_responder_amount};
        {false, false} -> {error, insufficient_amounts}
    end.

%% ==================================================================
%% Internal functions

account_type(Pubkey) ->
    case aec_chain:get_account(Pubkey) of
        {value, Account} ->
            {ok, aec_accounts:type(Account)};
        _ ->
            not_found
    end.
