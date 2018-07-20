%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for channel objects
%%% @end
%%%=============================================================================
-module(aesc_channels).

%% API
-export([deserialize/2,
         deposit/4,
         is_active/1,
         is_solo_closed/2,
         is_solo_closing/2,
         new/1,
         peers/1,
         serialize/1,
         serialize_for_client/1,
         close_solo/3,
         close_solo/4,
         snapshot_solo/2,
         withdraw/4]).

%% Getters
-export([id/1,
         pubkey/1,
         pubkey/3,
         initiator_id/1,
         initiator_pubkey/1,
         responder_id/1,
         responder_pubkey/1,
         delegate_ids/1,
         delegate_pubkeys/1,
         total_amount/1,
         initiator_amount/1,
         responder_amount/1,
         channel_reserve/1,
         state_hash/1,
         round/1,
         closes_at/1]).

-compile({no_auto_import, [round/1]}).

%%%===================================================================
%%% Types
%%%===================================================================

-type id() :: aec_id:id().
-type pubkey() :: aec_keys:pubkey().
-type amount() :: non_neg_integer().
-type seq_number() :: non_neg_integer().
-type payload() :: aesc_offchain_tx:tx() | <<>>.

-record(channel, {id               :: aec_id:id(),
                  initiator_id     :: aec_id:id(),
                  responder_id     :: aec_id:id(),
                  delegate_ids     :: [aec_id:id()],
                  total_amount     :: amount(),
                  initiator_amount :: amount(),
                  channel_reserve  :: amount(),
                  state_hash       :: binary(),
                  round            :: seq_number(),
                  lock_period      :: non_neg_integer(),
                  closes_at        :: aec_blocks:height()}).

-opaque channel() :: #channel{}.

-type serialized() :: binary().

-export_type([id/0,
              pubkey/0,
              amount/0,
              seq_number/0,
              channel/0,
              serialized/0,
              payload/0]).

-define(CHANNEL_TYPE, channel).
-define(CHANNEL_VSN, 1).

-define(PUB_SIZE, 32).
-define(NONCE_SIZE, 256).

%%%===================================================================
%%% API
%%%===================================================================

%% close solo with last known onchain state
-spec close_solo(channel(), aec_trees:poi(), aec_blocks:height()) -> channel().
close_solo(Ch, PoI, Height) ->
    close_solo_int(Ch, PoI, Height, round(Ch),        % keep the round
                                    state_hash(Ch)).  % keep the state hash

%% close solo with a payload
-spec close_solo(channel(), aesc_offchain_tx:tx(), aec_trees:poi(), aec_blocks:height()) -> channel().
close_solo(Ch, PayloadTx, PoI, Height) ->
    close_solo_int(Ch, PoI, Height, aesc_offchain_tx:round(PayloadTx),
                                    aesc_offchain_tx:state_hash(PayloadTx)).

close_solo_int(#channel{lock_period = LockPeriod} = Ch, PoI, Height, Round, StateHash) ->
    InitiatorPubKey = initiator_pubkey(Ch),
    ResponderPubKey = responder_pubkey(Ch),
    ClosesAt = Height + LockPeriod,
    InitiatorAmt = fetch_amount_from_poi(PoI, InitiatorPubKey),
    ResponderAmt = fetch_amount_from_poi(PoI, ResponderPubKey),
    Ch#channel{initiator_amount = InitiatorAmt,
               total_amount     = InitiatorAmt + ResponderAmt,
               round            = Round,
               state_hash       = StateHash,
               closes_at        = ClosesAt}.

-spec deposit(channel(), amount(), seq_number(), binary()) -> channel().
deposit(#channel{total_amount = TotalAmount} = Ch, Amount, Round, StateHash) ->
    Ch#channel{total_amount = TotalAmount + Amount,
               state_hash = StateHash,
               round = Round}.

-spec deserialize(aec_keys:pubkey(), binary()) -> channel().
deserialize(PubKey, Bin) ->
    [ {initiator_id     , InitiatorId}
    , {responder_id     , ResponderId}
    , {total_amount     , TotalAmount}
    , {initiator_amount , InitiatorAmount}
    , {channel_reserve  , ChannelReserve}
    , {delegate_ids     , DelegateIds}
    , {state_hash       , StateHash}
    , {round            , Round}
    , {lock_period      , LockPeriod}
    , {closes_at        , ClosesAt}
    ] = aec_object_serialization:deserialize(
          ?CHANNEL_TYPE,
          ?CHANNEL_VSN,
          serialization_template(?CHANNEL_VSN),
          Bin),
    account = aec_id:specialize_type(InitiatorId),
    account = aec_id:specialize_type(ResponderId),
    [account = aec_id:specialize_type(D) || D <- DelegateIds],
    #channel{id               = aec_id:create(channel, PubKey),
             initiator_id     = InitiatorId,
             responder_id     = ResponderId,
             delegate_ids     = DelegateIds,
             total_amount     = TotalAmount,
             initiator_amount = InitiatorAmount,
             channel_reserve  = ChannelReserve,
             state_hash       = StateHash,
             round            = Round,
             lock_period      = LockPeriod,
             closes_at        = ClosesAt}.

%% close solo with a payload
-spec snapshot_solo(channel(), aesc_offchain_tx:tx()) -> channel().
snapshot_solo(Ch, PayloadTx) ->
    Round = aesc_offchain_tx:round(PayloadTx),
    StateHash = aesc_offchain_tx:state_hash(PayloadTx),
    Ch#channel{round            = Round,
               state_hash       = StateHash}.

-spec is_active(channel()) -> boolean().
is_active(#channel{closes_at = ClosesAt}) ->
    ClosesAt =:= 0.

-spec is_solo_closed(channel(), aec_blocks:height()) -> boolean().
is_solo_closed(#channel{closes_at = ClosesAt}, Height) ->
    ClosesAt =/= 0 andalso ClosesAt =< Height.

-spec is_solo_closing(channel(), aec_blocks:height()) -> boolean().
is_solo_closing(#channel{closes_at = ClosesAt}, Height) ->
    ClosesAt > Height.

-spec pubkey(pubkey(), non_neg_integer(), pubkey()) -> pubkey().
pubkey(<<_:?PUB_SIZE/binary>> = InitiatorPubKey, Nonce,
       <<_:?PUB_SIZE/binary>> = ResponderPubKey) ->
    Bin = <<InitiatorPubKey:?PUB_SIZE/binary,
            Nonce:?NONCE_SIZE,
            ResponderPubKey:?PUB_SIZE/binary>>,
    aec_hash:hash(pubkey, Bin).

-spec new(aesc_create_tx:tx()) -> channel().
new(ChCTx) ->
    PubKey = pubkey(aesc_create_tx:initiator_pubkey(ChCTx),
                    aesc_create_tx:nonce(ChCTx),
                    aesc_create_tx:responder_pubkey(ChCTx)),
    InitiatorAmount = aesc_create_tx:initiator_amount(ChCTx),
    ResponderAmount = aesc_create_tx:responder_amount(ChCTx),
    DelegatePubkeys = aesc_create_tx:delegate_pubkeys(ChCTx),
    StateHash = aesc_create_tx:state_hash(ChCTx),
    #channel{id               = aec_id:create(channel, PubKey),
             initiator_id     = aesc_create_tx:initiator_id(ChCTx),
             responder_id     = aesc_create_tx:responder_id(ChCTx),
             total_amount     = InitiatorAmount + ResponderAmount,
             initiator_amount = InitiatorAmount,
             channel_reserve  = aesc_create_tx:channel_reserve(ChCTx),
             delegate_ids     = [aec_id:create(account, D) || D <- DelegatePubkeys],
             state_hash       = StateHash,
             round            = 0,
             closes_at        = 0,
             lock_period      = aesc_create_tx:lock_period(ChCTx)}.

-spec peers(channel()) -> list(aec_keys:pubkey()).
peers(#channel{} = Ch) ->
    [initiator_pubkey(Ch), responder_pubkey(Ch)].

-spec serialize(channel()) -> binary().
serialize(#channel{initiator_id = InitiatorId,
                   responder_id = ResponderId,
                   delegate_ids = DelegateIds} = Ch) ->
    aec_object_serialization:serialize(
      ?CHANNEL_TYPE, ?CHANNEL_VSN,
      serialization_template(?CHANNEL_VSN),
      [ {initiator_id     , InitiatorId}
      , {responder_id     , ResponderId}
      , {total_amount     , total_amount(Ch)}
      , {initiator_amount , initiator_amount(Ch)}
      , {channel_reserve  , channel_reserve(Ch)}
      , {delegate_ids     , DelegateIds}
      , {state_hash       , state_hash(Ch)}
      , {round            , round(Ch)}
      , {lock_period      , lock_period(Ch)}
      , {closes_at        , closes_at(Ch)}
      ]).

serialization_template(?CHANNEL_VSN) ->
    [ {initiator_id     , id}
    , {responder_id     , id}
    , {total_amount     , int}
    , {initiator_amount , int}
    , {channel_reserve  , int}
    , {delegate_ids     , [id]}
    , {state_hash       , binary}
    , {round            , int}
    , {lock_period      , int}
    , {closes_at        , int}
    ].

-spec serialize_for_client(channel()) -> map().
serialize_for_client(#channel{id               = Id,
                              initiator_id     = InitiatorId,
                              responder_id     = ResponderId,
                              total_amount     = TotalAmount,
                              initiator_amount = InitiatorAmount,
                              channel_reserve  = ChannelReserve,
                              delegate_ids     = Delegates,
                              state_hash       = StateHash,
                              round            = Round,
                              lock_period      = LockPeriod,
                              closes_at        = ClosesAt}) ->
    #{<<"id">>               => aec_base58c:encode(id_hash, Id),
      <<"initiator_id">>     => aec_base58c:encode(id_hash, InitiatorId),
      <<"responder_id">>     => aec_base58c:encode(id_hash, ResponderId),
      <<"total_amount">>     => TotalAmount,
      <<"initiator_amount">> => InitiatorAmount,
      <<"responder_amount">> => TotalAmount - InitiatorAmount,
      <<"channel_reserve">>  => ChannelReserve,
      <<"delegate_ids">>     => [aec_base58c:encode(id_hash, D) || D <- Delegates],
      <<"state_hash">>       => aec_base58c:encode(state, StateHash),
      <<"round">>            => Round,
      <<"lock_period">>      => LockPeriod,
      <<"closes_at">>        => ClosesAt}.

-spec withdraw(channel(), amount(), seq_number(), binary()) -> channel().
withdraw(#channel{total_amount = TotalAmount} = Ch, Amount, Round, StateHash) ->
    Ch#channel{total_amount = TotalAmount - Amount,
               state_hash = StateHash,
               round = Round}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec closes_at(channel()) -> undefined | aec_blocks:height().
closes_at(#channel{closes_at = ClosesAt}) ->
    ClosesAt.

-spec id(channel()) -> aec_id:id().
id(#channel{id = Id}) ->
    Id.

-spec pubkey(channel()) -> aec_keys:pubkey().
pubkey(#channel{id = Id}) ->
    aec_id:specialize(Id, channel).

-spec initiator_id(channel()) -> aec_id:id().
initiator_id(#channel{initiator_id = InitiatorId}) ->
    InitiatorId.

-spec initiator_pubkey(channel()) -> aec_keys:pubkey().
initiator_pubkey(#channel{initiator_id = InitiatorId}) ->
    aec_id:specialize(InitiatorId, account).

-spec responder_id(channel()) -> aec_id:id().
responder_id(#channel{responder_id = ResponderId}) ->
    ResponderId.

-spec responder_pubkey(channel()) -> aec_keys:pubkey().
responder_pubkey(#channel{responder_id = ResponderId}) ->
    aec_id:specialize(ResponderId, account).

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

-spec delegate_ids(channel()) -> list(aec_id:id()).
delegate_ids(#channel{delegate_ids = Ids}) ->
    Ids.

-spec delegate_pubkeys(channel()) -> list(aec_keys:pubkey()).
delegate_pubkeys(#channel{delegate_ids = Ids}) ->
    [aec_id:specialize(Id, account) || Id <- Ids].

-spec state_hash(channel()) -> binary().
state_hash(#channel{state_hash = StateHash}) ->
    StateHash.

-spec round(channel()) -> non_neg_integer().
round(#channel{round = Round}) ->
    Round.

-spec fetch_amount_from_poi(aec_trees:poi(), aec_keys:pubkey()) -> non_neg_integer().
fetch_amount_from_poi(PoI, Pubkey) ->
    {ok, Account} = aec_trees:lookup_poi(accounts, Pubkey, PoI),
    aec_accounts:balance(Account).

