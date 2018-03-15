%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for channel objects
%%% @end
%%%=============================================================================
-module(aesc_channels).

-include_lib("apps/aecore/include/common.hrl").

%% API
-export([deposit/3,
         deserialize/1,
         new/1,
         peers/1,
         serialize/1,
         withdraw/3]).

%% Getters
-export([id/1,
         id/3,
         initiator/1,
         initiator_amount/1,
         participant/1,
         participant_amount/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type id()     :: binary().
-type amount() :: integer().

-record(channel, {id                 :: id(),
                  initiator          :: pubkey(),
                  participant        :: pubkey(),
                  initiator_amount   :: amount(),
                  participant_amount :: amount(),
                  lock_period        :: non_neg_integer(),
                  closes_at          :: height()}).

-opaque channel() :: #channel{}.

-type serialized() :: binary().

-export_type([id/0,
              channel/0,
              serialized/0]).

-define(CHANNEL_TYPE, <<"channel">>).
-define(CHANNEL_VSN, 1).

-define(PUB_SIZE, 65).
-define(NONCE_SIZE, 256).

%%%===================================================================
%%% API
%%%===================================================================

-spec deposit(channel(), amount(), pubkey()) -> channel().
deposit(#channel{} = Channel, Amount, ToPubKey) ->
    case initiator(Channel) =:= ToPubKey of
        true ->
            update_initiator_amount(Channel, Amount);
        false ->
            ToPubKey = participant(Channel),
            update_participant_amount(Channel, Amount)
    end.

-spec deserialize(binary()) -> channel().
deserialize(Bin) ->
    {ok, List} = msgpack:unpack(Bin),
    [#{<<"type">>               := ?CHANNEL_TYPE},
     #{<<"vsn">>                := ?CHANNEL_VSN},
     #{<<"id">>                 := Id},
     #{<<"initiator">>          := InitiatorPubKey},
     #{<<"initiator_amount">>   := InitiatorAmount},
     #{<<"participant">>        := ParticipantPubKey},
     #{<<"participant_amount">> := ParticipantAmount},
     #{<<"lock_period">>        := LockPeriod},
     #{<<"closes_at">>          := ClosesAt}] = List,
    #channel{id                 = Id,
             initiator          = InitiatorPubKey,
             initiator_amount   = InitiatorAmount,
             participant        = ParticipantPubKey,
             participant_amount = ParticipantAmount,
             lock_period        = LockPeriod,
             closes_at          = ClosesAt}.

-spec new(aesc_create_tx:tx()) -> channel().
new(ChCTx) ->
    Id = id(aesc_create_tx:initiator(ChCTx),
            aesc_create_tx:nonce(ChCTx),
            aesc_create_tx:participant(ChCTx)),
    #channel{id                 = Id,
             initiator          = aesc_create_tx:initiator(ChCTx),
             initiator_amount   = aesc_create_tx:initiator_amount(ChCTx),
             participant        = aesc_create_tx:participant(ChCTx),
             participant_amount = aesc_create_tx:participant_amount(ChCTx),
             lock_period        = aesc_create_tx:lock_period(ChCTx)}.

-spec peers(channel()) -> list(pubkey()).
peers(#channel{} = Ch) ->
    [initiator(Ch), participant(Ch)].

-spec serialize(channel()) -> binary().
serialize(#channel{} = Ch) ->
    msgpack:pack([#{<<"type">>               => ?CHANNEL_TYPE},
                  #{<<"vsn">>                => ?CHANNEL_VSN},
                  #{<<"id">>                 => id(Ch)},
                  #{<<"initiator">>          => initiator(Ch)},
                  #{<<"initiator_amount">>   => initiator_amount(Ch)},
                  #{<<"participant">>        => participant(Ch)},
                  #{<<"participant_amount">> => participant_amount(Ch)},
                  #{<<"lock_period">>        => lock_period(Ch)},
                  #{<<"closes_at">>          => closes_at(Ch)}
                 ]).
-spec withdraw(channel(), amount(), pubkey()) -> channel().
withdraw(#channel{} = Channel, Amount, FromPubKey) ->
    case initiator(Channel) =:= FromPubKey of
        true ->
            update_initiator_amount(Channel, -Amount);
        false ->
            FromPubKey = participant(Channel),
            update_participant_amount(Channel, -Amount)
    end.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec closes_at(channel()) -> height().
closes_at(#channel{closes_at = ClosesAt}) ->
    ClosesAt.

-spec id(channel()) -> pubkey().
id(#channel{id = Id}) ->
    Id.

-spec initiator(channel()) -> pubkey().
initiator(#channel{initiator = InitiatorPubKey}) ->
    InitiatorPubKey.

-spec initiator_amount(channel()) -> amount().
initiator_amount(#channel{initiator_amount = InitiatorAmount}) ->
    InitiatorAmount.

-spec lock_period(channel()) -> non_neg_integer().
lock_period(#channel{lock_period = LockPeriod}) ->
    LockPeriod.

-spec participant(channel()) -> pubkey().
participant(#channel{participant = ParticipantPubKey}) ->
    ParticipantPubKey.

-spec participant_amount(channel()) -> amount().
participant_amount(#channel{participant_amount = ParticipantAmount}) ->
    ParticipantAmount.

%%%===================================================================
%%% Getters
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec id(pubkey(), non_neg_integer(), pubkey()) -> pubkey().
id(InitiatorPubKey, Nonce, ParticipantPubKey) ->
    Bin = <<InitiatorPubKey:?PUB_SIZE/binary,
            Nonce:?NONCE_SIZE,
            ParticipantPubKey:?PUB_SIZE/binary>>,
    aec_hash:hash(pubkey, Bin).

-spec update_initiator_amount(channel(), amount()) -> channel().
update_initiator_amount(Channel, Amount) ->
    InitiatorAmount = initiator_amount(Channel),
    Channel#channel{initiator_amount = InitiatorAmount + Amount}.

-spec update_participant_amount(channel(), amount()) -> channel().
update_participant_amount(Channel, Amount) ->
    ParticipantAmount = participant_amount(Channel),
    Channel#channel{participant_amount = ParticipantAmount + Amount}.
