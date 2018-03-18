%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel deposit transaction
%%% @end
%%%=============================================================================
-module(aesc_deposit_tx).

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

-define(CHANNEL_DEPOSIT_TX_VSN, 1).

-opaque tx() :: #channel_deposit_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id   := ChannelId,
      from_account := FromPubKey,
      to_account   := ToPubKey,
      amount       := Amount,
      initiator    := InitiatorPubKey,
      participant  := ParticipantPubKey,
      fee          := Fee,
      nonce        := Nonce}) ->
    Tx = #channel_deposit_tx{
            channel_id   = ChannelId,
            from_account = FromPubKey,
            to_account   = ToPubKey,
            amount       = Amount,
            initiator    = InitiatorPubKey,
            participant  = ParticipantPubKey,
            fee          = Fee,
            nonce        = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_deposit_tx{fee = Fee}) ->
    Fee.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_deposit_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#channel_deposit_tx{from_account = FromPubKey}) ->
    FromPubKey.

-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_deposit_tx{channel_id   = ChannelId,
                          from_account = FromPubKey,
                          to_account   = ToPubKey,
                          amount       = Amount,
                          initiator    = InitiatorPubKey,
                          participant  = ParticipantPubKey,
                          fee          = Fee,
                          nonce        = Nonce}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(FromPubKey, Trees, Height, Nonce, Amount + Fee) end,
         fun() -> aesc_utils:check_active_channel_exists(ChannelId, InitiatorPubKey, ParticipantPubKey, Trees) end,
         fun() -> aesc_utils:check_are_peers([FromPubKey, ToPubKey], [InitiatorPubKey, ParticipantPubKey]) end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#channel_deposit_tx{channel_id   = ChannelId,
                            from_account = FromPubKey,
                            to_account   = ToPubKey,
                            amount       = Amount,
                            fee          = Fee,
                            nonce        = Nonce}, Trees, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees),
    ChannelsTree0 = aec_trees:channels(Trees),

    FromAccount0       = aec_accounts_trees:get(FromPubKey, AccountsTree0),
    {ok, FromAccount1} = aec_accounts:spend(FromAccount0, Amount + Fee, Nonce, Height),
    AccountsTree1      = aec_accounts_trees:enter(FromAccount1, AccountsTree0),

    Channel0      = aesc_state_tree:get(ChannelId, ChannelsTree0),
    Channel1      = aesc_channels:deposit(Channel0, Amount, ToPubKey),
    ChannelsTree1 = aesc_state_tree:enter(Channel1, ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree1),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_deposit_tx{from_account = FromPubKey,
                             to_account   = ToPubKey}) ->
    lists:usort([FromPubKey, ToPubKey]).

-spec signers(tx()) -> list(pubkey()).
signers(#channel_deposit_tx{initiator   = InitiatorPubKey,
                            participant = ParticipantPubKey}) ->
    [InitiatorPubKey, ParticipantPubKey].

-spec serialize(tx()) -> list(map()).
serialize(#channel_deposit_tx{channel_id   = ChannelId,
                              from_account = FromPubKey,
                              to_account   = ToPubKey,
                              amount       = Amount,
                              initiator    = InitiatorPubKey,
                              participant  = ParticipantPubKey,
                              fee          = Fee,
                              nonce        = Nonce}) ->
    [#{<<"vsn">>          => version()},
     #{<<"channel_id">>   => ChannelId},
     #{<<"from_account">> => FromPubKey},
     #{<<"to_account">>   => ToPubKey},
     #{<<"amount">>       => Amount},
     #{<<"initiator">>    => InitiatorPubKey},
     #{<<"participant">>  => ParticipantPubKey},
     #{<<"fee">>          => Fee},
     #{<<"nonce">>        => Nonce}].

-spec deserialize(list(map())) -> tx().
deserialize([#{<<"vsn">>          := ?CHANNEL_DEPOSIT_TX_VSN},
             #{<<"channel_id">>   := ChannelId},
             #{<<"from_account">> := FromPubKey},
             #{<<"to_account">>   := ToPubKey},
             #{<<"initiator">>    := InitiatorPubKey},
             #{<<"participant">>  := ParticipantPubKey},
             #{<<"amount">>       := Amount},
             #{<<"fee">>          := Fee},
             #{<<"nonce">>        := Nonce}]) ->
    #channel_deposit_tx{channel_id = ChannelId,
                        from_account = FromPubKey,
                        to_account   = ToPubKey,
                        amount       = Amount,
                        initiator    = InitiatorPubKey,
                        participant  = ParticipantPubKey,
                        fee          = Fee,
                        nonce        = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_deposit_tx{channel_id   = ChannelId,
                               from_account = FromPubKey,
                               to_account   = ToPubKey,
                               amount       = Amount,
                               initiator    = InitiatorPubKey,
                               participant  = ParticipantPubKey,
                               fee          = Fee,
                               nonce        = Nonce}) ->
    #{<<"vsn">>          => version(),
      <<"channel">>      => aec_base58c:encode(channel, ChannelId),
      <<"from_account">> => aec_base58c:encode(account_pubkey, FromPubKey),
      <<"to_account">>   => aec_base58c:encode(account_pubkey, ToPubKey),
      <<"amount">>       => Amount,
      <<"initiator">>    => aec_base58c:encode(account_pubkey, InitiatorPubKey),
      <<"participant">>  => aec_base58c:encode(account_pubkey, ParticipantPubKey),
      <<"fee">>          => Fee,
      <<"nonce">>        => Nonce}.

%%%===================================================================
%%% Getters
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_DEPOSIT_TX_VSN.
