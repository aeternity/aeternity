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
-export([initiator/1,
         initiator_amount/1,
         channel_reserve/1,
         lock_period/1,
         participant/1,
         participant_amount/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_CREATE_TX_VSN, 1).
-define(CHANNEL_CREATE_TX_TYPE, channel_create_tx).
-define(CHANNEL_CREATE_TX_FEE, 4).

-opaque tx() :: #channel_create_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{initiator          := InitiatorPubKey,
      initiator_amount   := InitiatorAmount,
      participant        := ParticipantPubKey,
      participant_amount := ParticipantAmount,
      channel_reserve    := ChannelReserve,
      lock_period        := LockPeriod,
      fee                := Fee,
      nonce              := Nonce}) ->
    Tx = #channel_create_tx{initiator          = InitiatorPubKey,
                            participant        = ParticipantPubKey,
                            initiator_amount   = InitiatorAmount,
                            participant_amount = ParticipantAmount,
                            channel_reserve    = ChannelReserve,
                            lock_period        = LockPeriod,
                            fee                = Fee,
                            nonce              = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_CREATE_TX_TYPE.

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
check(#channel_create_tx{initiator          = InitiatorPubKey,
                         initiator_amount   = InitiatorAmount,
                         participant        = ParticipantPubKey,
                         participant_amount = ParticipantAmount,
                         channel_reserve    = ChannelReserve,
                         nonce              = Nonce,
                         fee                = Fee}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(InitiatorPubKey, Trees, Height, Nonce, InitiatorAmount + Fee) end,
         fun() -> aetx_utils:check_account(ParticipantPubKey, Trees, Height, ParticipantAmount) end,
         fun() -> check_reserve_amount(ChannelReserve, InitiatorAmount, ParticipantAmount) end,
         fun() -> check_not_channel(InitiatorPubKey, Nonce, ParticipantPubKey, Trees) end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#channel_create_tx{initiator          = InitiatorPubKey,
                           initiator_amount   = InitiatorAmount,
                           participant        = ParticipantPubKey,
                           participant_amount = ParticipantAmount,
                           fee                = Fee,
                           nonce              = Nonce} = CreateTx, Trees0, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees0),
    ChannelsTree0 = aec_trees:channels(Trees0),

    InitiatorAccount0   = aec_accounts_trees:get(InitiatorPubKey, AccountsTree0),
    ParticipantAccount0 = aec_accounts_trees:get(ParticipantPubKey, AccountsTree0),

    {ok, InitiatorAccount1}   = aec_accounts:spend(InitiatorAccount0, Fee + InitiatorAmount, Nonce, Height),
    {ok, ParticipantAccount1} = aec_accounts:spend(ParticipantAccount0, ParticipantAmount, Nonce, Height),

    AccountsTree1 = aec_accounts_trees:enter(InitiatorAccount1, AccountsTree0),
    AccountsTree2 = aec_accounts_trees:enter(ParticipantAccount1, AccountsTree1),

    Channel       = aesc_channels:new(CreateTx),
    ChannelsTree1 = aesc_state_tree:enter(Channel, ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree2),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_create_tx{initiator   = InitiatorPubKey,
                            participant = ParticipantPubKey}) ->
    [InitiatorPubKey, ParticipantPubKey].

-spec signers(tx()) -> list(pubkey()).
signers(#channel_create_tx{initiator   = InitiatorPubKey,
                           participant = ParticipantPubKey}) ->
    [InitiatorPubKey, ParticipantPubKey].

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_create_tx{initiator          = InitiatorPubKey,
                             initiator_amount   = InitiatorAmount,
                             participant        = ParticipantPubKey,
                             participant_amount = ParticipantAmount,
                             channel_reserve    = ChannelReserve,
                             lock_period        = LockPeriod,
                             fee                = Fee,
                             nonce              = Nonce}) ->
    {version(),
     [ {initiator         , InitiatorPubKey}
     , {initiator_amount  , InitiatorAmount}
     , {participant       , ParticipantPubKey}
     , {participant_amount, ParticipantAmount}
     , {channel_reserve   , ChannelReserve}
     , {lock_period       , LockPeriod}
     , {fee               , Fee}
     , {nonce             , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_CREATE_TX_VSN,
            [ {initiator         , InitiatorPubKey}
            , {initiator_amount  , InitiatorAmount}
            , {participant       , ParticipantPubKey}
            , {participant_amount, ParticipantAmount}
            , {channel_reserve   , ChannelReserve}
            , {lock_period       , LockPeriod}
            , {fee               , Fee}
            , {nonce             , Nonce}]) ->
    #channel_create_tx{initiator          = InitiatorPubKey,
                       initiator_amount   = InitiatorAmount,
                       participant        = ParticipantPubKey,
                       participant_amount = ParticipantAmount,
                       channel_reserve    = ChannelReserve,
                       lock_period        = LockPeriod,
                       fee                = Fee,
                       nonce              = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_create_tx{initiator          = Initiator,
                              initiator_amount   = InitiatorAmount,
                              participant        = Participant,
                              participant_amount = ParticipantAmount,
                              channel_reserve    = ChannelReserve,
                              lock_period        = LockPeriod,
                              nonce              = Nonce,
                              fee                = Fee}) ->
    #{<<"data_schema">>        => <<"ChannelCreateTxJSON">>, % swagger schema name
      <<"vsn">>                => version(),
      <<"initiator">>          => aec_base58c:encode(account_pubkey, Initiator),
      <<"initiator_amount">>   => InitiatorAmount,
      <<"participant">>        => aec_base58c:encode(account_pubkey, Participant),
      <<"participant_amount">> => ParticipantAmount,
      <<"channel_reserve">>    => ChannelReserve,
      <<"lock_period">>        => LockPeriod,
      <<"nonce">>              => Nonce,
      <<"fee">>                => Fee}.

serialization_template(?CHANNEL_CREATE_TX_VSN) ->
    [ {initiator         , binary}
    , {initiator_amount  , int}
    , {participant       , binary}
    , {participant_amount, int}
    , {channel_reserve   , int}
    , {lock_period       , int}
    , {fee               , int}
    , {nonce             , int}
    ].

%%%===================================================================
%%% Getters
%%%===================================================================

-spec initiator(tx()) -> pubkey().
initiator(#channel_create_tx{initiator = InitiatorPubKey}) ->
    InitiatorPubKey.

-spec initiator_amount(tx()) -> non_neg_integer().
initiator_amount(#channel_create_tx{initiator_amount = InitiatorAmount}) ->
    InitiatorAmount.

-spec channel_reserve(tx()) -> non_neg_integer().
channel_reserve(#channel_create_tx{channel_reserve = ChannelReserve}) ->
    ChannelReserve.

-spec lock_period(tx()) -> non_neg_integer().
lock_period(#channel_create_tx{lock_period = LockPeriod}) ->
    LockPeriod.

-spec participant(tx()) -> pubkey().
participant(#channel_create_tx{participant = ParticipantPubKey}) ->
    ParticipantPubKey.

-spec participant_amount(tx()) -> non_neg_integer().
participant_amount(#channel_create_tx{participant_amount = ParticipantAmount}) ->
    ParticipantAmount.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec check_not_channel(pubkey(),non_neg_integer(), pubkey(), aec_trees:trees()) ->
                               ok | {error, channel_exists}.
check_not_channel(InitiatorPubKey, Nonce, ParticipantPubKey, Trees) ->
    ChannelID     = aesc_channels:id(InitiatorPubKey, Nonce, ParticipantPubKey),
    ChannelsTrees = aec_trees:channels(Trees),
    case aesc_state_tree:lookup(ChannelID, ChannelsTrees) of
        {value, _Channel} -> {error, channel_exists};
        none              -> ok
    end.

check_push_amount(PushAmount, InitiatorAmount) ->
    case PushAmount =< InitiatorAmount of
        true  -> ok;
        false -> {error, push_amount_exceeds_initiator_amount}
    end.

check_reserve_amount(Reserve, InitiatorAmount,
                     ParticipantAmount) when is_integer(Reserve) ->
    case (Reserve =< InitiatorAmount) of
        true ->
            case (Reserve =< ParticipantAmount) of
                true  -> ok;
                false -> {error, insufficient_participant_amount}
            end;
        false ->
            {error, insufficient_initiator_amount}
    end.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_CREATE_TX_VSN.
