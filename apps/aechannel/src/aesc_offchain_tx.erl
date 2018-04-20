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
         updates/1,
         previous_round/1,
         round/1]).

-export([set_value/3]).

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
      updates            := Updates,
      state              := State,
      previous_round     := Prev,
      round              := Round}) ->
    Tx = #channel_offchain_tx{
            channel_id         = ChannelId,
            initiator          = InitiatorPubKey,
            responder          = ResponderPubKey,
            initiator_amount   = InitiatorAmount,
            responder_amount   = ResponderAmount,
            updates            = Updates,
            state              = State,
            previous_round     = Prev,
            round              = Round},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_OFFCHAIN_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_offchain_tx{}) ->
    %% This tx should never hit the mempool or any block
    ?CHANNEL_OFFCHAIN_TX_FEE.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_offchain_tx{round = N}) ->
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
         updates            = _Updates,
         state              = _State,
         previous_round     = _Prev,
         round              = _Round}, Trees, _Height) ->
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
             updates            = Updates,
             state              = State,
             previous_round     = Prev,
             round              = Round}) ->
    {version(),
     [ {channel_id        , ChannelId}
     , {previous_round    , Prev}
     , {round             , Round}
     , {initiator         , InitiatorPubKey}
     , {responder         , ResponderPubKey}
     , {initiator_amount  , InitiatorAmount}
     , {responder_amount  , ResponderAmount}
     , {updates           , Updates}
     , {state             , State}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_OFFCHAIN_TX_VSN,
            [ {channel_id        , ChannelId}
            , {previous_round    , Prev}
            , {round             , Round}
            , {initiator         , InitiatorPubKey}
            , {responder         , ResponderPubKey}
            , {initiator_amount  , InitiatorAmount}
            , {responder_amount  , ResponderAmount}
            , {updates           , Updates}
            , {state             , State}]) ->
    #channel_offchain_tx{
       channel_id         = ChannelId,
       initiator          = InitiatorPubKey,
       responder          = ResponderPubKey,
       initiator_amount   = InitiatorAmount,
       responder_amount   = ResponderAmount,
       updates            = Updates,
       state              = State,
       previous_round     = Prev,
       round              = Round}.

-spec for_client(tx()) -> map().
for_client(#channel_offchain_tx{
              channel_id         = ChannelId,
              initiator          = InitiatorPubKey,
              responder          = ResponderPubKey,
              initiator_amount   = InitiatorAmount,
              responder_amount   = ResponderAmount,
              updates            = Updates,
              state              = State,
              previous_round     = Prev,
              round              = Round}) ->
    #{<<"vsn">>                => ?CHANNEL_OFFCHAIN_TX_VSN,
      <<"channel_id">>         => aec_base58c:encode(channel, ChannelId),
      <<"previous_round">>     => Prev,
      <<"round">>              => Round,
      <<"initiator">>          => aec_base58c:encode(
                                    account_pubkey, InitiatorPubKey),
      <<"responder">>          => aec_base58c:encode(
                                    account_pubkey, ResponderPubKey),
      <<"initiator_amount">>   => InitiatorAmount,
      <<"responder_amount">>   => ResponderAmount,
      <<"updates">>            => [update_for_client(D) || D <- Updates],
      <<"state">>              => aec_base58c:encode(state, State)}.

serialization_template(?CHANNEL_OFFCHAIN_TX_VSN) ->
    [ {channel_id        , binary}
    , {previous_round    , int}
    , {round             , int}
    , {initiator         , binary}
    , {responder         , binary}
    , {initiator_amount  , int}
    , {responder_amount  , int}
    , {updates           , [{binary,binary,int}]}
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

-spec updates(tx()) -> [offchain_update()].
updates(#channel_offchain_tx{updates = Updates}) ->
    Updates.

-spec previous_round(tx()) -> non_neg_integer().
previous_round(#channel_offchain_tx{previous_round = PreviousRound}) ->
    PreviousRound.

-spec round(tx()) -> aesc_channels:seq_number().
round(#channel_offchain_tx{round = Round}) ->
    Round.

-type settable_field() :: initiator_amount
                        | responder_amount
                        | updates
                        | previous_round
                        | round.

-spec set_value(tx(), settable_field(), any()) -> tx().
set_value(#channel_offchain_tx{} = Tx, initiator_amount, Amt)
  when is_integer(Amt), Amt >= 0 ->
    Tx#channel_offchain_tx{initiator_amount = Amt};
set_value(#channel_offchain_tx{} = Tx, responder_amount, Amt)
  when is_integer(Amt), Amt >= 0 ->
    Tx#channel_offchain_tx{responder_amount = Amt};
set_value(#channel_offchain_tx{} = Tx, updates, Updates) when is_list(Updates) ->
    Tx#channel_offchain_tx{updates = Updates};
set_value(#channel_offchain_tx{round = Seq} = Tx, previous_round, N)
  when is_integer(N), N >= 0, N < Seq ->
    Tx#channel_offchain_tx{previous_round = N};
set_value(#channel_offchain_tx{round = Seq} = Tx, round, N)
  when is_integer(N), N >= 0, N > Seq ->
    Tx#channel_offchain_tx{round = N}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

version() ->
    ?CHANNEL_OFFCHAIN_TX_VSN.

update_for_client({From, To, Amount}) ->
    #{<<"from">> => From,
      <<"to">>   => To,
      <<"am">>   => Amount}.
