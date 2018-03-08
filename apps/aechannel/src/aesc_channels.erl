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
         id/1,
         new/2,
         serialize/1]).

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

-spec deserialize(binary()) -> channel().
deserialize(Bin) ->
    {ok, List} = msgpack:unpack(Bin),
    [#{<<"type">>               := ?CHANNEL_TYPE},
     #{<<"vsn">>                := ?CHANNEL_VSN},
     #{<<"id">>                 := Id},
     #{<<"initiator">>          := Initiator},
     #{<<"initiator_amount">>   := InitiatorAmount},
     #{<<"participant">>        := Participant},
     #{<<"participant_amount">> := ParticipantAmount},
     #{<<"closes_at">>          := ClosesAt}] = List,
    #channel{id                 = Id,
             initiator          = Initiator,
             initiator_amount   = InitiatorAmount,
             participant        = Participant,
             participant_amount = ParticipantAmount,
             closes_at          = ClosesAt}.

-spec id(channel()) -> pubkey().
id(#channel{id = Id}) ->
    Id.

-spec new(aesc_create_tx:tx(), height()) -> channel().
new(ChCTx, BlockHeight) ->
    Id = id(aesc_create_tx:initiator(ChCTx),
            aesc_create_tx:nonce(ChCTx),
            aesc_create_tx:participant(ChCTx)),
    ClosesAt = BlockHeight + aesc_create_tx:ttl(ChCTx),
    #channel{id                 = Id,
             initiator          = aesc_create_tx:initiator(ChCTx),
             initiator_amount   = aesc_create_tx:initiator_amount(ChCTx),
             participant        = aesc_create_tx:participant(ChCTx),
             participant_amount = aesc_create_tx:participant_amount(ChCTx),
             closes_at          = ClosesAt}.

-spec serialize(channel()) -> binary().
serialize(#channel{} = Ch) ->
    msgpack:pack([#{<<"type">>               => ?CHANNEL_TYPE},
                  #{<<"vsn">>                => ?CHANNEL_VSN},
                  #{<<"id">>                 => id(Ch)},
                  #{<<"initiator">>          => initiator(Ch)},
                  #{<<"initiator_amount">>   => initiator_amount(Ch)},
                  #{<<"participant">>        => participant(Ch)},
                  #{<<"participant_amount">> => participant_amount(Ch)},
                  #{<<"closes_at">>          => closes_at(Ch)}
                 ]).

%%%===================================================================
%%% Getters
%%%===================================================================

-spec closes_at(channel()) -> height().
closes_at(#channel{closes_at = ClosesAt}) ->
    ClosesAt.

-spec initiator(channel()) -> pubkey().
initiator(#channel{initiator = Initiator}) ->
    Initiator.

-spec initiator_amount(channel()) -> amount().
initiator_amount(#channel{initiator_amount = InitiatorAmount}) ->
    InitiatorAmount.

-spec participant(channel()) -> pubkey().
participant(#channel{participant = Participant}) ->
    Participant.

-spec participant_amount(channel()) -> amount().
participant_amount(#channel{participant_amount = ParticipantAmount}) ->
    ParticipantAmount.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec id(pubkey(), non_neg_integer(), pubkey()) -> pubkey().
id(InitiatorPubKey, Nonce, ParticipantPubKey) ->
    Bin = <<InitiatorPubKey:?PUB_SIZE/binary,
            Nonce:?NONCE_SIZE,
            ParticipantPubKey:?PUB_SIZE/binary>>,
    aec_hash:hash(pubkey, Bin).
