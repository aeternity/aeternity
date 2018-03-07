%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel create transaction
%%% @end
%%%=============================================================================
-module(aesc_create_tx).

-include("channel_txs.hrl").

-behavior(aetx).

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

-define(CHANNEL_CREATE_TX_VSN, 1).

-opaque tx() :: #channel_create_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{initiator          := InitiatorPubKey,
      participant        := ParticipantPubKey,
      initiator_amount   := InitiatorAmount,
      participant_amount := ParticipantAmount,
      fee                := Fee,
      nonce              := Nonce}) ->
    Tx = #channel_create_tx{initiator          = InitiatorPubKey,
                            participant        = ParticipantPubKey,
                            initiator_amount   = InitiatorAmount,
                            participant_amount = ParticipantAmount,
                            fee                = Fee,
                            nonce              = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_create_tx{fee = Fee}) ->
    Fee.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_create_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#channel_create_tx{initiator = InitiatorPubKey}) ->
    InitiatorPubKey.

-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_create_tx{}, Trees, _Height) ->
    %% Implement me
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#channel_create_tx{}, Trees, _Height) ->
    %% Implement me
    {ok, Trees}.

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_create_tx{initiator   = InitiatorPubKey,
                            participant = ParticipantPubKey}) ->
    [InitiatorPubKey, ParticipantPubKey].

-spec signers(tx()) -> list(pubkey()).
signers(#channel_create_tx{initiator   = InitiatorPubKey,
                           participant = ParticipantPubKey}) ->
    [InitiatorPubKey, ParticipantPubKey].

-spec serialize(tx()) -> list(map()).
serialize(#channel_create_tx{initiator          = InitiatorPubKey,
                             participant        = ParticipantPubKey,
                             initiator_amount   = InitiatorAmount,
                             participant_amount = ParticipantAmount,
                             fee                = Fee,
                             nonce              = Nonce}) ->
    [#{<<"vsn">>                => version()},
     #{<<"initiator">>          => InitiatorPubKey},
     #{<<"participant">>        => ParticipantPubKey},
     #{<<"initiator_amount">>   => InitiatorAmount},
     #{<<"participant_amount">> => ParticipantAmount},
     #{<<"fee">>                => Fee},
     #{<<"nonce">>              => Nonce}].

-spec deserialize(list(map())) -> tx().
deserialize([#{<<"vsn">>                := ?CHANNEL_CREATE_TX_VSN},
             #{<<"initiator">>          := InitiatorPubKey},
             #{<<"participant">>        := ParticipantPubKey},
             #{<<"initiator_amount">>   := InitiatorAmount},
             #{<<"participant_amount">> := ParticipantAmount},
             #{<<"fee">>                := Fee},
             #{<<"nonce">>              := Nonce}]) ->
    #channel_create_tx{initiator          = InitiatorPubKey,
                       participant        = ParticipantPubKey,
                       initiator_amount   = InitiatorAmount,
                       participant_amount = ParticipantAmount,
                       fee                = Fee,
                       nonce              = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_create_tx{initiator          = Initiator,
                              participant        = Participant,
                              initiator_amount   = InitiatorAmount,
                              participant_amount = ParticipantAmount,
                              nonce              = Nonce,
                              fee                = Fee}) ->
    #{<<"vsn">>                => version(),
      <<"initiator">>          => aec_base58c:encode(account_pubkey, Initiator),
      <<"participant">>        => aec_base58c:encode(account_pubkey, Participant),
      <<"initiator_amount">>   => InitiatorAmount,
      <<"participant_amount">> => ParticipantAmount,
      <<"nonce">>              => Nonce,
      <<"fee">>                => Fee}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_CREATE_TX_VSN.
