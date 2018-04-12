-module(aesc_offchain_tx).

-include("channel_txs.hrl").

-behaviour(aetx).



%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         accounts/1,
         signers/1,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%% Getters
-export([channel_id/1,
         initiator/1,
         initiator_amount/1,
         responder/1,
         responder_amount/1,
         deposits/1,
         previous_state/1,
         sequence_number/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_OFFCHAIN_TX_VSN, 1).
-define(CHANNEL_OFFCHAIN_TX_TYPE, channel_offchain_tx).
-define(CHANNEL_OFFCHAIN_TX_FEE, 0).   % off-chain

-opaque tx() :: #channel_offchain_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id         := ChannelId,
      initiator          := InitiatorPubKey,
      responder          := ResponderPubKey,
      initiator_amount   := InitiatorAmount,
      responder_amount   := ResponderAmount,
      deposits           := Deposits,
      state              := State,
      previous_state     := Prev,
      sequence_number    := SequenceNumber}) ->
    Tx = #channel_offchain_tx{
            channel_id         = ChannelId,
            initiator          = InitiatorPubKey,
            responder          = ResponderPubKey,
            initiator_amount   = InitiatorAmount,
            responder_amount   = ResponderAmount,
            deposits           = Deposits,
            state              = State,
            previous_state     = Prev,
            sequence_number    = SequenceNumber},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_OFFCHAIN_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_offchain_tx{}) ->
    %% This tx should never hit the mempool or any block
    ?CHANNEL_OFFCHAIN_TX_FEE.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_offchain_tx{sequence_number = N}) ->
    N.

-spec origin(tx()) -> pubkey().
origin(#channel_offchain_tx{initiator = Origin}) ->
    Origin.

check(#channel_offchain_tx{
         channel_id         = _ChannelId,
         initiator          = _InitiatorPubKey,
         responder          = _ResponderPubKey,
         initiator_amount   = _InitiatorAmount,
         responder_amount   = _ResponderAmount,
         deposits           = _Deposits,
         state              = _State,
         previous_state     = _Prev
         sequence_number    = _SequenceNumber}, Trees, _Height) ->
    %% TODO: implement checks relevant to off-chain
    {ok, Trees}.

process(#channel_offchain_tx{}, _Trees, _Height) ->
    error(off_chain_tx).

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_offchain_tx{
            initiator   = InitiatorPubKey,
            responder   = ResponderPubKey}) ->
    [InitiatorPubKey, ResponderPubKey].

-spec signers(tx()) -> list(pubkey()).
signers(#channel_offchain_tx{
           initiator   = InitiatorPubKey,
           responder   = ResponderPubKey}) ->
    [InitiatorPubKey, ResponderPubKey].

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_offchain_tx{
             channel_id         = ChannelId,
             initiator          = InitiatorPubKey,
             responder          = ResponderPubKey,
             initiator_amount   = InitiatorAmount,
             responder_amount   = ResponderAmount,
             deposits           = Deposits,
             state              = State,
             previous_state     = Prev,
             sequence_number    = SequenceNumber}) ->
    {version(),
     [ {channel_id        , ChannelId}
     , {previous_state    , Prev}
     , {sequence_number   , SequenceNumber}
     , {initiator         , InitiatorPubKey}
     , {responder         , ResponderPubKey}
     , {initiator_amount  , InitiatorAmount}
     , {responder_amount  , ResponderAmount}
     , {deposits          , Deposits}
     , {state             , State}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_OFFCHAIN_TX_VSN,
            [ {channel_id        , ChannelId}
            , {previous_state    , Prev}
            , {sequence_number   , SequenceNumber}
            , {initiator         , InitiatorPubKey}
            , {responder         , ResponderPubKey}
            , {initiator_amount  , InitiatorAmount}
            , {responder_amount  , ResponderAmount}
            , {deposits          , Deposits}
            , {state             , State}]) ->
    #channel_offchain_tx{
       channel_id         = ChannelId,
       initiator          = InitiatorPubKey,
       responder          = ResponderPubKey,
       initiator_amount   = InitiatorAmount,
       responder_amount   = ResponderAmount,
       deposits           = Deposits,
       state              = State,
       previous_state     = Prev,
       sequence_number    = SequenceNumber}.

-spec for_client(tx()) -> map().
for_client(#channel_offchain_tx{
              channel_id         = ChannelId,
              initiator          = InitiatorPubKey,
              responder          = ResponderPubKey,
              initiator_amount   = InitiatorAmount,
              responder_amount   = ResponderAmount,
              deposits           = Deposits,
              state              = State,
              previous_state     = Prev,
              sequence_number    = SequenceNumber}) ->
    #{<<"vsn">>                => ?CHANNEL_OFFCHAIN_TX_VSN,
      <<"channel_id">>         => aec_base58c:encode(channel, ChannelId),
      <<"previous_state">>     => Prev,
      <<"sequence_number">>    => SequenceNumber,
      <<"initiator">>          => aec_base58c:encode(
                                    account_pubkey, InitiatorPubKey),
      <<"responder">>          => aec_base58c:encode(
                                    account_pubkey, ResponderPubKey),
      <<"initiator_amount">>   => InitiatorAmount,
      <<"responder_amount">>   => ResponderAmount,
      <<"deposits">>           => [deposit_for_client(D) || D <- Deposits],
      <<"state">>              => aec_base58c:encode(state, State)}.

serialization_template(?CHANNEL_OFFCHAIN_TX_VSN) ->
    [ {channel_id        , binary}
    , {previous_state    , int}
    , {sequence_number   , int}
    , {initiator         , binary}
    , {responder         , binary}
    , {initiator_amount  , int}
    , {responder_amount  , int}
    , {deposits          , [{int,int}]}
    , {state             , binary}
    ].

%%%===================================================================
%%% Getters
%%%===================================================================

-spec channel_id(tx()) -> aesc_channels:id().
channel_id(#channel_offchain_tx{channel_id = ChannelId}) ->
    ChannelId.

-spec initiator(tx()) -> pubkey().
initiator(#channel_offchain_tx{initiator = InitiatorPubKey}) ->
    InitiatorPubKey.

-spec initiator_amount(tx()) -> aesc_channels:amount().
initiator_amount(#channel_offchain_tx{initiator_amount = InitiatorAmount}) ->
    InitiatorAmount.

-spec responder(tx()) -> pubkey().
responder(#channel_offchain_tx{responder = ResponderPubKey}) ->
    ResponderPubKey.

-spec responder_amount(tx()) -> aesc_channels:amount().
responder_amount(#channel_offchain_tx{responder_amount = ResponderAmount}) ->
    ResponderAmount.

-spec deposits(tx()) -> [offchain_deposit()].
deposits(#channel_offchain_tx{deposits = Deposits}) ->
    Deposits.

-spec previous_state(tx()) -> non_neg_integer().
previous_state(#channel_offchain_tx{previous_state = PreviousState}) ->
    PreviousState.

-spec sequence_number(tx()) -> aesc_channels:seq_number().
sequence_number(#channel_offchain_tx{sequence_number = SequenceNumber}) ->
    SequenceNumber.

%%%===================================================================
%%% Internal functions
%%%===================================================================

version() ->
    ?CHANNEL_OFFCHAIN_TX_VSN.

deposit_for_client({Code, Amount}) ->
    Str = case Code of
              ?DEPOSIT_I2P -> <<"i2p">>;
              ?DEPOSIT_P2I -> <<"p2i">>
          end,
    #{<<"op">> => Str, <<"am">> => Amount}.
