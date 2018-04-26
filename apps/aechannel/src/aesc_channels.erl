%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for channel objects
%%% @end
%%%=============================================================================
-module(aesc_channels).

-include_lib("apps/aecore/include/common.hrl").

%% API
-export([deserialize/1,
         deposit/2,
         is_active/1,
         is_solo_closed/2,
         is_solo_closing/2,
         new/1,
         peers/1,
         serialize/1,
         slash/3,
         close_solo/3,
         withdraw/2]).

%% Getters
-export([id/1,
         id/3,
         initiator/1,
         responder/1,
         total_amount/1,
         initiator_amount/1,
         responder_amount/1,
         channel_reserve/1,
         round/1,
         closes_at/1]).

-compile({no_auto_import, [round/1]}).

%%%===================================================================
%%% Types
%%%===================================================================

-type id()     :: binary().
-type amount() :: non_neg_integer().
-type seq_number() :: non_neg_integer().

-record(channel, {id               :: id(),
                  initiator        :: pubkey(),
                  responder        :: pubkey(),
                  total_amount     :: amount(),
                  initiator_amount :: amount(),
                  channel_reserve  :: amount(),
                  round            :: seq_number(),
                  lock_period      :: non_neg_integer(),
                  closes_at        :: height()}).

-opaque channel() :: #channel{}.

-type serialized() :: binary().

-export_type([id/0,
              amount/0,
              seq_number/0,
              channel/0,
              serialized/0]).

-define(CHANNEL_TYPE, <<"channel">>).
-define(CHANNEL_VSN, 1).

-define(PUB_SIZE, 65).
-define(NONCE_SIZE, 256).

%%%===================================================================
%%% API
%%%===================================================================

-spec close_solo(channel(), aesc_offchain_tx:tx(), height()) -> channel().
close_solo(#channel{lock_period = LockPeriod} = Ch, State, Height) ->
    ClosesAt = Height + LockPeriod,
    Ch#channel{initiator_amount = aesc_offchain_tx:initiator_amount(State),
               total_amount     = aesc_offchain_tx:initiator_amount(State) + aesc_offchain_tx:responder_amount(State),
               round            = aesc_offchain_tx:round(State),
               closes_at        = ClosesAt}.

-spec deposit(channel(), amount()) -> channel().
deposit(#channel{total_amount = TotalAmount} = Ch, Amount) ->
    Ch#channel{total_amount = TotalAmount + Amount}.

-spec deserialize(binary()) -> channel().
deserialize(Bin) ->
    {ok, List} = msgpack:unpack(Bin),
    [#{<<"type">>             := ?CHANNEL_TYPE},
     #{<<"vsn">>              := ?CHANNEL_VSN},
     #{<<"id">>               := Id},
     #{<<"initiator">>        := InitiatorPubKey},
     #{<<"responder">>        := ResponderPubKey},
     #{<<"total_amount">>     := TotalAmount},
     #{<<"initiator_amount">> := InitiatorAmount},
     #{<<"channel_reserve">>  := ChannelReserve},
     #{<<"round">>            := Round},
     #{<<"lock_period">>      := LockPeriod},
     #{<<"closes_at">>        := ClosesAt}] = List,
    #channel{id               = Id,
             initiator        = InitiatorPubKey,
             responder        = ResponderPubKey,
             total_amount     = TotalAmount,
             initiator_amount = InitiatorAmount,
             channel_reserve  = ChannelReserve,
             round            = Round,
             lock_period      = LockPeriod,
             closes_at        = ClosesAt}.

-spec is_active(channel()) -> boolean().
is_active(#channel{closes_at = ClosesAt}) ->
    ClosesAt =:= 0.

-spec is_solo_closed(channel(), height()) -> boolean().
is_solo_closed(#channel{closes_at = ClosesAt}, Height) ->
    ClosesAt =/= 0 andalso ClosesAt =< Height.

-spec is_solo_closing(channel(), height()) -> boolean().
is_solo_closing(#channel{closes_at = ClosesAt}, Height) ->
    ClosesAt > Height.

-spec id(pubkey(), non_neg_integer(), pubkey()) -> pubkey().
id(InitiatorPubKey, Nonce, ResponderPubKey) ->
    Bin = <<InitiatorPubKey:?PUB_SIZE/binary,
            Nonce:?NONCE_SIZE,
            ResponderPubKey:?PUB_SIZE/binary>>,
    aec_hash:hash(pubkey, Bin).

-spec new(aesc_create_tx:tx()) -> channel().
new(ChCTx) ->
    Id = id(aesc_create_tx:initiator(ChCTx),
            aesc_create_tx:nonce(ChCTx),
            aesc_create_tx:responder(ChCTx)),
    InitiatorAmount   = aesc_create_tx:initiator_amount(ChCTx),
    ResponderAmount = aesc_create_tx:responder_amount(ChCTx),
    #channel{id               = Id,
             initiator        = aesc_create_tx:initiator(ChCTx),
             responder        = aesc_create_tx:responder(ChCTx),
             total_amount     = InitiatorAmount + ResponderAmount,
             initiator_amount = InitiatorAmount,
             channel_reserve  = aesc_create_tx:channel_reserve(ChCTx),
             round            = 0,
             closes_at        = 0,
             lock_period      = aesc_create_tx:lock_period(ChCTx)}.

-spec peers(channel()) -> list(pubkey()).
peers(#channel{} = Ch) ->
    [initiator(Ch), responder(Ch)].

-spec serialize(channel()) -> binary().
serialize(#channel{} = Ch) ->
    msgpack:pack([#{<<"type">>             => ?CHANNEL_TYPE},
                  #{<<"vsn">>              => ?CHANNEL_VSN},
                  #{<<"id">>               => id(Ch)},
                  #{<<"initiator">>        => initiator(Ch)},
                  #{<<"responder">>        => responder(Ch)},
                  #{<<"total_amount">>     => total_amount(Ch)},
                  #{<<"initiator_amount">> => initiator_amount(Ch)},
                  #{<<"channel_reserve">>  => channel_reserve(Ch)},
                  #{<<"round">>            => round(Ch)},
                  #{<<"lock_period">>      => lock_period(Ch)},
                  #{<<"closes_at">>        => closes_at(Ch)}
                 ]).

-spec slash(channel(), aesc_offchain_tx:tx(), height()) -> channel().
slash(#channel{lock_period = LockPeriod} = Ch, State, Height) ->
    ClosesAt = Height + LockPeriod,
    Ch#channel{initiator_amount = aesc_offchain_tx:initiator_amount(State),
               total_amount     = aesc_offchain_tx:initiator_amount(State) + aesc_offchain_tx:responder_amount(State),
               round            = aesc_offchain_tx:round(State),
               closes_at        = ClosesAt}.

-spec withdraw(channel(), amount()) -> channel().
withdraw(#channel{total_amount = TotalAmount} = Ch, Amount) ->
    Ch#channel{total_amount = TotalAmount - Amount}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec closes_at(channel()) -> undefined | height().
closes_at(#channel{closes_at = ClosesAt}) ->
    ClosesAt.

-spec id(channel()) -> pubkey().
id(#channel{id = Id}) ->
    Id.

-spec initiator(channel()) -> pubkey().
initiator(#channel{initiator = InitiatorPubKey}) ->
    InitiatorPubKey.

-spec responder(channel()) -> pubkey().
responder(#channel{responder = ResponderPubKey}) ->
    ResponderPubKey.

-spec total_amount(channel()) -> amount().
total_amount(#channel{total_amount = TotalAmount}) ->
    TotalAmount.

-spec initiator_amount(channel()) -> amount().
initiator_amount(#channel{initiator_amount = InitiatorAmount}) ->
    InitiatorAmount.

-spec responder_amount(channel()) -> amount().
responder_amount(#channel{initiator_amount = InitiatorAmount,
                          total_amount = TotalAmount}) ->
    TotalAmount - InitiatorAmount.

-spec channel_reserve(channel()) -> amount().
channel_reserve(#channel{channel_reserve = ChannelReserve}) ->
    ChannelReserve.

-spec lock_period(channel()) -> non_neg_integer().
lock_period(#channel{lock_period = LockPeriod}) ->
    LockPeriod.

-spec round(channel()) -> non_neg_integer().
round(#channel{round = Round}) ->
    Round.
