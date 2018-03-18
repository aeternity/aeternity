%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel close mutual transaction
%%% @end
%%%=============================================================================
-module(aesc_close_mutual_tx).

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

-define(CHANNEL_CLOSE_MUTUAL_TX, 1).

-opaque tx() :: #channel_close_mutual_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id  := ChannelId,
      amount      := Amount,
      initiator   := InitiatorPubKey,
      participant := ParticipantPubKey,
      fee         := Fee,
      nonce       := Nonce}) ->
    Tx = #channel_close_mutual_tx{
            channel_id  = ChannelId,
            amount      = Amount,
            initiator   = InitiatorPubKey,
            participant = ParticipantPubKey,
            fee         = Fee,
            nonce       = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_close_mutual_tx{fee = Fee}) ->
    Fee.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_close_mutual_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#channel_close_mutual_tx{initiator = InitiatorPubKey}) ->
    InitiatorPubKey.

-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_close_mutual_tx{}, Trees, _Height) ->
    %% TODO: implement me
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#channel_close_mutual_tx{}, Trees, _Height) ->
    %% TODO: implement me
    {ok, Trees}.

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_close_mutual_tx{initiator   = InitiatorPubKey,
                                  participant = ParticipantPubKey}) ->
    [InitiatorPubKey, ParticipantPubKey].

-spec signers(tx()) -> list(pubkey()).
signers(#channel_close_mutual_tx{initiator   = InitiatorPubKey,
                                 participant = ParticipantPubKey}) ->
    [InitiatorPubKey, ParticipantPubKey].

-spec serialize(tx()) -> list(map()).
serialize(#channel_close_mutual_tx{channel_id  = ChannelId,
                                   amount      = Amount,
                                   initiator   = InitiatorPubKey,
                                   participant = ParticipantPubKey,
                                   fee         = Fee,
                                   nonce       = Nonce}) ->
    [#{<<"vsn">>         => version()},
     #{<<"channel_id">>  => ChannelId},
     #{<<"amount">>      => Amount},
     #{<<"initiator">>   => InitiatorPubKey},
     #{<<"participant">> => ParticipantPubKey},
     #{<<"fee">>         => Fee},
     #{<<"nonce">>       => Nonce}].

-spec deserialize(list(map())) -> tx().
deserialize([#{<<"vsn">>         := ?CHANNEL_CLOSE_MUTUAL_TX},
             #{<<"channel_id">>  := ChannelId},
             #{<<"amount">>      := Amount},
             #{<<"initiator">>   := InitiatorPubKey},
             #{<<"participant">> := ParticipantPubKey},
             #{<<"fee">>         := Fee},
             #{<<"nonce">>       := Nonce}]) ->
    #channel_close_mutual_tx{channel_id  = ChannelId,
                             amount      = Amount,
                             initiator   = InitiatorPubKey,
                             participant = ParticipantPubKey,
                             fee         = Fee,
                             nonce       = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_close_mutual_tx{channel_id  = ChannelId,
                                    amount      = Amount,
                                    initiator   = InitiatorPubKey,
                                    participant = ParticipantPubKey,
                                    fee         = Fee,
                                    nonce       = Nonce}) ->
    #{<<"vsn">>         => version(),
      <<"channel_id">>  => aec_base58c:encode(channel, ChannelId),
      <<"amount">>      => Amount,
      <<"initiator">>   => aec_base58c:encode(account_pubkey, InitiatorPubKey),
      <<"participant">> => aec_base58c:encode(account_pubkey, ParticipantPubKey),
      <<"fee">>         => Fee,
      <<"nonce">>       => Nonce}.

%%%===================================================================
%%% Getters
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_CLOSE_MUTUAL_TX.
