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
         is_last_state_forced/1,
         locked_until/1,
         new/9,
         peers/1,
         serialize/1,
         serialize_for_client/1,
         close_solo/3,
         close_solo/4,
         force_progress/6,
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
         channel_amount/1,
         initiator_amount/1,
         responder_amount/1,
         channel_reserve/1,
         state_hash/1,
         round/1,
         solo_round/1,
         lock_period/1]).

-compile({no_auto_import, [round/1]}).

%%%===================================================================
%%% Types
%%%===================================================================

-type id() :: aec_id:id().
-type pubkey() :: aec_keys:pubkey().
-type amount() :: non_neg_integer().
-type seq_number() :: non_neg_integer().
-type payload() :: aesc_offchain_tx:tx() | <<>>.

-record(channel, {id                  :: aec_id:id(),
                  initiator_id        :: aec_id:id(),
                  responder_id        :: aec_id:id(),
                  delegate_ids        :: [aec_id:id()],
                  channel_amount      :: amount(),
                  initiator_amount    :: amount(),
                  responder_amount    :: amount(),
                  channel_reserve     :: amount(),
                  state_hash          :: binary(),
                  round               :: seq_number(),
                  solo_round          :: seq_number(),
                  lock_period         :: non_neg_integer(),
                  locked_until        :: aec_blocks:height()}).

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
    LockedUntil  = Height + LockPeriod,
    InitiatorAmt = fetch_amount_from_poi(PoI, InitiatorPubKey),
    ResponderAmt = fetch_amount_from_poi(PoI, ResponderPubKey),
    Ch#channel{initiator_amount   = InitiatorAmt,
               responder_amount   = ResponderAmt,
               round              = Round,
               % do not reset solo_round
               state_hash         = StateHash,
               locked_until       = LockedUntil}.

-spec deposit(channel(), amount(), seq_number(), binary()) -> channel().
deposit(#channel{channel_amount = ChannelAmount} = Ch, Amount, Round, StateHash) ->
    Ch#channel{channel_amount     = ChannelAmount + Amount,
               state_hash         = StateHash,
               solo_round         = 0,
               round              = Round}.

-spec deserialize(pubkey(), binary()) -> channel().
deserialize(IdBin, Bin) ->
    [ {initiator_id       , InitiatorId}
    , {responder_id       , ResponderId}
    , {channel_amount     , ChannelAmount}
    , {initiator_amount   , InitiatorAmount}
    , {responder_amount   , ResponderAmount}
    , {channel_reserve    , ChannelReserve}
    , {delegate_ids       , DelegateIds}
    , {state_hash         , StateHash}
    , {round              , Round}
    , {solo_round         , SoloRound}
    , {lock_period        , LockPeriod}
    , {locked_until       , LockedUntil}
    ] = aec_object_serialization:deserialize(
          ?CHANNEL_TYPE,
          ?CHANNEL_VSN,
          serialization_template(?CHANNEL_VSN),
          Bin),
    account = aec_id:specialize_type(InitiatorId),
    account = aec_id:specialize_type(ResponderId),
    [account = aec_id:specialize_type(D) || D <- DelegateIds],
    #channel{id                 = aec_id:create(channel, IdBin),
             initiator_id       = InitiatorId,
             responder_id       = ResponderId,
             delegate_ids       = DelegateIds,
             channel_amount     = ChannelAmount,
             initiator_amount   = InitiatorAmount,
             responder_amount   = ResponderAmount,
             channel_reserve    = ChannelReserve,
             state_hash         = StateHash,
             round              = Round,
             solo_round         = SoloRound,
             lock_period        = LockPeriod,
             locked_until       = LockedUntil}.

%% set a new state
-spec snapshot_solo(channel(), aesc_offchain_tx:tx()) -> channel().
snapshot_solo(Ch, PayloadTx) ->
    Round = aesc_offchain_tx:round(PayloadTx),
    StateHash = aesc_offchain_tx:state_hash(PayloadTx),
    Ch#channel{round              = Round,
               solo_round         = 0,
               state_hash         = StateHash}.

-spec force_progress(channel(), binary(), seq_number(), 
                     amount(), amount(),
                     aec_blocks:height()) -> channel().
force_progress(Ch0, StateHash, Round, IAmt, RAmt, Height) ->
    SoloRound =
        case solo_round(Ch0) of
            0 ->
                % force progress that is based on co-signed state
                Round;
            OldSoloRound ->
                % force progress that is based on force progressed state
                % keep the first of a chain of solo forced states
                OldSoloRound
        end,
    Ch1 = Ch0#channel{state_hash          = StateHash,
                      round               = Round,
                      solo_round          = SoloRound,
                      initiator_amount    = IAmt,
                      responder_amount    = RAmt},
    case is_active(Ch1) of
        true -> Ch1;
        false -> Ch1#channel{locked_until = Height + lock_period(Ch0)}
    end.

-spec is_active(channel()) -> boolean().
is_active(#channel{locked_until = LockedUntil}) ->
    LockedUntil =:= 0.

-spec is_solo_closed(channel(), aec_blocks:height()) -> boolean().
is_solo_closed(#channel{locked_until = LockedUntil}, Height) ->
    LockedUntil =/= 0 andalso LockedUntil =< Height.

-spec is_solo_closing(channel(), aec_blocks:height()) -> boolean().
is_solo_closing(#channel{locked_until = LockedUntil}, Height) ->
    LockedUntil > Height.

-spec pubkey(pubkey(), non_neg_integer(), pubkey()) -> pubkey().
pubkey(<<_:?PUB_SIZE/binary>> = InitiatorPubKey, Nonce,
       <<_:?PUB_SIZE/binary>> = ResponderPubKey) ->
    Bin = <<InitiatorPubKey:?PUB_SIZE/binary,
            Nonce:?NONCE_SIZE,
            ResponderPubKey:?PUB_SIZE/binary>>,
    aec_hash:hash(pubkey, Bin).

-spec is_last_state_forced(channel()) -> boolean().
is_last_state_forced(#channel{solo_round = SoloRound}) ->
    SoloRound =/= 0.

-spec new(aec_keys:pubkey(), non_neg_integer(),
          aec_keys:pubkey(), non_neg_integer(),
          non_neg_integer(), [aec_keys:pubkey()],
          aec_hash:hash(), non_neg_integer(),
          non_neg_integer()) -> channel().
new(InitiatorPubKey, InitiatorAmount, ResponderPubKey, ResponderAmount,
    ReserveAmount, DelegatePubkeys, StateHash, LockPeriod, Nonce) ->
    PubKey = pubkey(InitiatorPubKey, Nonce, ResponderPubKey),
    #channel{id                   = aec_id:create(channel, PubKey),
             initiator_id         = aec_id:create(account, InitiatorPubKey),
             responder_id         = aec_id:create(account, ResponderPubKey),
             channel_amount       = InitiatorAmount + ResponderAmount,
             initiator_amount     = InitiatorAmount,
             responder_amount     = ResponderAmount,
             channel_reserve      = ReserveAmount,
             delegate_ids         = [aec_id:create(account, D) || D <- DelegatePubkeys],
             state_hash           = StateHash,
             round                = 1,
             solo_round           = 0,
             locked_until         = 0,
             lock_period          = LockPeriod}.

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
      [ {initiator_id       , InitiatorId}
      , {responder_id       , ResponderId}
      , {channel_amount     , channel_amount(Ch)}
      , {initiator_amount   , initiator_amount(Ch)}
      , {responder_amount   , responder_amount(Ch)}
      , {channel_reserve    , channel_reserve(Ch)}
      , {delegate_ids       , DelegateIds}
      , {state_hash         , state_hash(Ch)}
      , {round              , round(Ch)}
      , {solo_round         , solo_round(Ch)}
      , {lock_period        , lock_period(Ch)}
      , {locked_until       , locked_until(Ch)}
      ]).

serialization_template(?CHANNEL_VSN) ->
    [ {initiator_id       , id}
    , {responder_id       , id}
    , {channel_amount     , int}
    , {initiator_amount   , int}
    , {responder_amount   , int}
    , {channel_reserve    , int}
    , {delegate_ids       , [id]}
    , {state_hash         , binary}
    , {round              , int}
    , {solo_round         , int}
    , {lock_period        , int}
    , {locked_until       , int}
    ].

-spec serialize_for_client(channel()) -> map().
serialize_for_client(#channel{id                  = Id,
                              initiator_id        = InitiatorId,
                              responder_id        = ResponderId,
                              channel_amount      = ChannelAmount,
                              initiator_amount    = InitiatorAmount,
                              responder_amount    = ResponderAmount,
                              channel_reserve     = ChannelReserve,
                              delegate_ids        = Delegates,
                              state_hash          = StateHash,
                              round               = Round,
                              solo_round          = SoloRound,
                              lock_period         = LockPeriod,
                              locked_until        = LockedUntil}) ->
    #{<<"id">>                    => aehttp_api_encoder:encode(id_hash, Id),
      <<"initiator_id">>          => aehttp_api_encoder:encode(id_hash, InitiatorId),
      <<"responder_id">>          => aehttp_api_encoder:encode(id_hash, ResponderId),
      <<"channel_amount">>        => ChannelAmount,
      <<"initiator_amount">>      => InitiatorAmount,
      <<"responder_amount">>      => ResponderAmount,
      <<"channel_reserve">>       => ChannelReserve,
      <<"delegate_ids">>          => [aehttp_api_encoder:encode(id_hash, D) || D <- Delegates],
      <<"state_hash">>            => aehttp_api_encoder:encode(state, StateHash),
      <<"round">>                 => Round,
      <<"solo_round">>            => SoloRound,
      <<"lock_period">>           => LockPeriod,
      <<"locked_until">>          => LockedUntil}.

-spec withdraw(channel(), amount(), seq_number(), binary()) -> channel().
withdraw(#channel{channel_amount = ChannelAmount} = Ch, Amount, Round, StateHash) ->
    Ch#channel{channel_amount = ChannelAmount - Amount,
               state_hash = StateHash,
               solo_round = 0,
               round = Round}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec locked_until(channel()) -> undefined | aec_blocks:height().
locked_until(#channel{locked_until = FBU}) ->
    FBU.

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

-spec channel_amount(channel()) -> amount().
channel_amount(#channel{channel_amount = ChannelAmount}) ->
    ChannelAmount.

-spec initiator_amount(channel()) -> amount().
initiator_amount(#channel{initiator_amount = InitiatorAmount}) ->
    InitiatorAmount.

-spec responder_amount(channel()) -> amount().
responder_amount(#channel{responder_amount = ResponderAmount}) ->
    ResponderAmount.

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

-spec solo_round(channel()) -> non_neg_integer().
solo_round(#channel{solo_round = SoloRound}) ->
    SoloRound.

-spec fetch_amount_from_poi(aec_trees:poi(), aec_keys:pubkey()) -> non_neg_integer().
fetch_amount_from_poi(PoI, Pubkey) ->
    {ok, Account} = aec_trees:lookup_poi(accounts, Pubkey, PoI),
    aec_accounts:balance(Account).

