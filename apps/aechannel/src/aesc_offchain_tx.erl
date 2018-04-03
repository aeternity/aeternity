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
      participant        := ParticipantPubKey,
      initiator_amount   := InitiatorAmount,
      participant_amount := ParticipantAmount,
      state              := State,
      sequence_number    := SequenceNumber}) ->
    Tx = #channel_offchain_tx{
            channel_id         = ChannelId,
            initiator          = InitiatorPubKey,
            participant        = ParticipantPubKey,
            initiator_amount   = InitiatorAmount,
            participant_amount = ParticipantAmount,
            state              = State,
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
         participant        = _ParticipantPubKey,
         initiator_amount   = _InitiatorAmount,
         participant_amount = _ParticipantAmount,
         sequence_number    = _SequenceNumber}, Trees, _Height) ->
    %% TODO: implement checks relevant to off-chain
    {ok, Trees}.

process(#channel_offchain_tx{}, _Trees, _Height) ->
    error(off_chain_tx).

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_offchain_tx{
            initiator   = InitiatorPubKey,
            participant = ParticipantPubKey}) ->
    [InitiatorPubKey, ParticipantPubKey].

-spec signers(tx()) -> list(pubkey()).
signers(#channel_offchain_tx{
            initiator   = InitiatorPubKey,
            participant = ParticipantPubKey}) ->
    [InitiatorPubKey, ParticipantPubKey].

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_offchain_tx{
             channel_id         = ChannelId,
             initiator          = InitiatorPubKey,
             participant        = ParticipantPubKey,
             initiator_amount   = InitiatorAmount,
             participant_amount = ParticipantAmount,
             state              = State,
             sequence_number    = SequenceNumber}) ->
    {version(),
    [ {channel_id        , ChannelId}
    , {sequence_number   , SequenceNumber}
    , {initiator         , InitiatorPubKey}
    , {participant       , ParticipantPubKey}
    , {initiator_amount  , InitiatorAmount}
    , {participant_amount, ParticipantAmount}
    , {state             , State}
    ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_OFFCHAIN_TX_VSN,
            [ {channel_id        , ChannelId}
            , {sequence_number   , SequenceNumber}
            , {initiator         , InitiatorPubKey}
            , {participant       , ParticipantPubKey}
            , {initiator_amount  , InitiatorAmount}
            , {participant_amount, ParticipantAmount}
            , {state             , State}]) ->
    #channel_offchain_tx{
       channel_id         = ChannelId,
       initiator          = InitiatorPubKey,
       participant        = ParticipantPubKey,
       initiator_amount   = InitiatorAmount,
       participant_amount = ParticipantAmount,
       state              = State,
       sequence_number    = SequenceNumber}.

-spec for_client(tx()) -> map().
for_client(#channel_offchain_tx{
              channel_id         = ChannelId,
              initiator          = InitiatorPubKey,
              participant        = ParticipantPubKey,
              initiator_amount   = InitiatorAmount,
              participant_amount = ParticipantAmount,
              state              = State,
              sequence_number    = SequenceNumber}) ->
    #{<<"vsn">>                => ?CHANNEL_OFFCHAIN_TX_VSN,
      <<"channel_id">>         => aec_base58c:encode(channel, ChannelId),
      <<"sequence_number">>    => SequenceNumber,
      <<"initiator">>          => aec_base58c:encode(
                                    account_pubkey, InitiatorPubKey),
      <<"participant">>        => aec_base58c:encode(
                                    account_pubkey, ParticipantPubKey),
      <<"initiator_amount">>   => InitiatorAmount,
      <<"participant_amount">> => ParticipantAmount,
      <<"state">>              => aec_base58c:encode(state, State)}.

serialization_template(?CHANNEL_OFFCHAIN_TX_VSN) ->
    [ {channel_id        , binary}
    , {sequence_number   , int}
    , {initiator         , binary}
    , {participant       , binary}
    , {initiator_amount  , int}
    , {participant_amount, int}
    , {state             , binary}
    ].

version() ->
    ?CHANNEL_OFFCHAIN_TX_VSN.
