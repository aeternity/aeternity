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
         delegates/1,
         serialize/1,
         close_solo/3,
         close_solo/4,
         withdraw/4]).

%% Getters
-export([id/1,
         id/3,
         initiator/1,
         responder/1,
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

-type id()     :: <<_:256>>.
-type amount() :: non_neg_integer().
-type seq_number() :: non_neg_integer().
-type payload() :: aesc_offchain_tx:tx() | <<>>.

-record(channel, {id               :: aec_id:id(),
                  initiator        :: aec_id:id(),
                  responder        :: aec_id:id(),
                  delegates        :: [aec_id:id()],
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
    Initiator = initiator(Ch),
    Responder = responder(Ch),
    ClosesAt = Height + LockPeriod,
    InitiatorAmt = fetch_amount_from_poi(PoI, Initiator),
    ResponderAmt = fetch_amount_from_poi(PoI, Responder),
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

-spec deserialize(id(), binary()) -> channel().
deserialize(IdBin, Bin) ->
    [ {initiator        , InitiatorId}
    , {responder        , ResponderId}
    , {total_amount     , TotalAmount}
    , {initiator_amount , InitiatorAmount}
    , {channel_reserve  , ChannelReserve}
    , {delegates        , Delegates}
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
    [account = aec_id:specialize_type(D) || D <- Delegates],
    #channel{id               = aec_id:create(channel, IdBin),
             initiator        = InitiatorId,
             responder        = ResponderId,
             delegates        = Delegates,
             total_amount     = TotalAmount,
             initiator_amount = InitiatorAmount,
             channel_reserve  = ChannelReserve,
             state_hash       = StateHash,
             round            = Round,
             lock_period      = LockPeriod,
             closes_at        = ClosesAt}.

-spec is_active(channel()) -> boolean().
is_active(#channel{closes_at = ClosesAt}) ->
    ClosesAt =:= 0.

-spec is_solo_closed(channel(), aec_blocks:height()) -> boolean().
is_solo_closed(#channel{closes_at = ClosesAt}, Height) ->
    ClosesAt =/= 0 andalso ClosesAt =< Height.

-spec is_solo_closing(channel(), aec_blocks:height()) -> boolean().
is_solo_closing(#channel{closes_at = ClosesAt}, Height) ->
    ClosesAt > Height.

-spec id(aec_keys:pubkey(), non_neg_integer(), aec_keys:pubkey()) -> aec_keys:pubkey().
id(<<_:?PUB_SIZE/binary>> = InitiatorPubKey, Nonce,
   <<_:?PUB_SIZE/binary>> = ResponderPubKey) ->
    Bin = <<InitiatorPubKey:?PUB_SIZE/binary,
            Nonce:?NONCE_SIZE,
            ResponderPubKey:?PUB_SIZE/binary>>,
    aec_hash:hash(pubkey, Bin).

-spec new(aesc_create_tx:tx()) -> channel().
new(ChCTx) ->
    Id = id(aesc_create_tx:initiator_pubkey(ChCTx),
            aesc_create_tx:nonce(ChCTx),
            aesc_create_tx:responder_pubkey(ChCTx)),
    InitiatorAmount   = aesc_create_tx:initiator_amount(ChCTx),
    ResponderAmount = aesc_create_tx:responder_amount(ChCTx),
    Delegates = aesc_create_tx:delegate_pubkeys(ChCTx),
    StateHash = aesc_create_tx:state_hash(ChCTx),
    #channel{id               = aec_id:create(channel, Id),
             initiator        = aesc_create_tx:initiator(ChCTx),
             responder        = aesc_create_tx:responder(ChCTx),
             total_amount     = InitiatorAmount + ResponderAmount,
             initiator_amount = InitiatorAmount,
             channel_reserve  = aesc_create_tx:channel_reserve(ChCTx),
             delegates        = [aec_id:create(account, D) || D <- Delegates],
             state_hash       = StateHash,
             round            = 0,
             closes_at        = 0,
             lock_period      = aesc_create_tx:lock_period(ChCTx)}.

-spec peers(channel()) -> list(aec_keys:pubkey()).
peers(#channel{} = Ch) ->
    [initiator(Ch), responder(Ch)].

-spec serialize(channel()) -> binary().
serialize(#channel{initiator = InitiatorId,
                   responder = ResponderId,
                   delegates = Delegates} = Ch) ->
    aec_object_serialization:serialize(
      ?CHANNEL_TYPE, ?CHANNEL_VSN,
      serialization_template(?CHANNEL_VSN),
      [ {initiator        , InitiatorId}
      , {responder        , ResponderId}
      , {total_amount     , total_amount(Ch)}
      , {initiator_amount , initiator_amount(Ch)}
      , {channel_reserve  , channel_reserve(Ch)}
      , {delegates        , Delegates}
      , {state_hash       , state_hash(Ch)}
      , {round            , round(Ch)}
      , {lock_period      , lock_period(Ch)}
      , {closes_at        , closes_at(Ch)}
      ]).

serialization_template(?CHANNEL_VSN) ->
    [ {initiator        , id}
    , {responder        , id}
    , {total_amount     , int}
    , {initiator_amount , int}
    , {channel_reserve  , int}
    , {delegates        , [id]}
    , {state_hash       , binary}
    , {round            , int}
    , {lock_period      , int}
    , {closes_at        , int}
    ].

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

-spec id(channel()) -> aec_keys:pubkey().
id(#channel{id = Id}) ->
    aec_id:specialize(Id, channel).

-spec initiator(channel()) -> aec_keys:pubkey().
initiator(#channel{initiator = InitiatorPubKey}) ->
    aec_id:specialize(InitiatorPubKey, account).

-spec responder(channel()) -> aec_keys:pubkey().
responder(#channel{responder = ResponderPubKey}) ->
    aec_id:specialize(ResponderPubKey, account).

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

-spec delegates(channel()) -> list(aec_keys:pubkey()).
delegates(#channel{delegates = Ds}) ->
    [aec_id:specialize(D, account) || D <- Ds].

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

    
