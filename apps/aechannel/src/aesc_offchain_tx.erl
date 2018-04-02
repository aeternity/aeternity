-module(aesc_offchain_tx).

-include("channel_txs.hrl").

-behaviour(aetx).



%% Behavior API
-export([new/1,
         fee/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         accounts/1,
         signers/1,
         serialize/1,
         deserialize/1,
         for_client/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_OFFCHAIN_TX_VSN, 1).

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

-spec fee(tx()) -> non_neg_integer().
fee(#channel_offchain_tx{}) ->
    %% This tx should never hit the mempool or any block
    0.

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

serialize(#channel_offchain_tx{
             channel_id         = ChannelId,
             initiator          = InitiatorPubKey,
             participant        = ParticipantPubKey,
             initiator_amount   = InitiatorAmount,
             participant_amount = ParticipantAmount,
             state              = State,
             sequence_number    = SequenceNumber}) ->
    [#{<<"vsn">>                => ?CHANNEL_OFFCHAIN_TX_VSN},
     #{<<"channel_id">>         => ChannelId},
     #{<<"sequence_number">>    => SequenceNumber},
     #{<<"initiator">>          => InitiatorPubKey},
     #{<<"participant">>        => ParticipantPubKey},
     #{<<"initiator_amount">>   => InitiatorAmount},
     #{<<"participant_amount">> => ParticipantAmount},
     #{<<"state">>              => State}].

-spec deserialize(list(map())) -> tx().
deserialize([#{<<"vsn">>                := ?CHANNEL_OFFCHAIN_TX_VSN},
             #{<<"channel_id">>         := ChannelId},
             #{<<"sequence_number">>    := SequenceNumber},
             #{<<"initiator">>          := InitiatorPubKey},
             #{<<"participant">>        := ParticipantPubKey},
             #{<<"initiator_amount">>   := InitiatorAmount},
             #{<<"participant_amount">> := ParticipantAmount},
             #{<<"state">>              := State}]) ->
    #channel_offchain_tx{
       channel_id         = ChannelId,
       initiator          = InitiatorPubKey,
       participant        = ParticipantPubKey,
       initiator_amount   = InitiatorAmount,
       participant_amount = ParticipantAmount,
       state              = State,
       sequence_number    = SequenceNumber}.

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
      <<"initiator">>          => aec_base58c:encode(
                                    account_pubkey, InitiatorPubKey),
      <<"participant">>        => aec_base58c:encode(
                                    account_pubkey, ParticipantPubKey),
      <<"initiator_amount">>   => InitiatorAmount,
      <<"participant_amount">> => ParticipantAmount,
      <<"state">>              => aec_base58c:encode(state, State),
      <<"sequence_number">>    => SequenceNumber}.
