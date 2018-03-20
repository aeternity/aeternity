%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel settle transaction
%%% @end
%%%=============================================================================
-module(aesc_settle_tx).

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

-define(CHANNEL_SETTLE_TX_VSN, 1).

-opaque tx() :: #channel_settle_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id := ChannelId,
      account    := AccountPubKey,
      party      := PartyPubKey,
      fee        := Fee,
      nonce      := Nonce}) ->
    Tx = #channel_settle_tx{
            channel_id = ChannelId,
            account    = AccountPubKey,
            party      = PartyPubKey,
            fee        = Fee,
            nonce      = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_settle_tx{fee = Fee}) ->
    Fee.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_settle_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#channel_settle_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_settle_tx{channel_id = ChannelId,
                         account    = AccountPubKey,
                         party      = PartyPubKey,
                         fee        = Fee,
                         nonce      = Nonce}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> check_channel(ChannelId, AccountPubKey, PartyPubKey, Trees) end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#channel_settle_tx{channel_id = ChannelId,
                           account    = AccountPubKey,
                           fee        = Fee,
                           nonce      = Nonce}, Trees, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees),
    ChannelsTree0 = aec_trees:channels(Trees),

    Channel0          = aesc_state_tree:get(ChannelId, ChannelsTree0),
    Initiator         = aesc_channels:initiator(Channel0),
    InitiatorAmount   = aesc_channels:initiator_amount(Channel0),
    Participant       = aesc_channels:participant(Channel0),
    ParticipantAmount = aesc_channels:participant_amount(Channel0),

    InitiatorAccount0         = aec_accounts_trees:get(Initiator, AccountsTree0),
    ParticipantAccount0       = aec_accounts_trees:get(Participant, AccountsTree0),
    {ok, InitiatorAccount1}   = aec_accounts:earn(InitiatorAccount0, InitiatorAmount, Height),
    {ok, ParticipantAccount1} = aec_accounts:earn(ParticipantAccount0, ParticipantAmount, Height),

    {InitiatorAccount3, ParticipantAccount3} =
        case AccountPubKey of
            Initiator ->
                {ok, InitiatorAccount2} = aec_accounts:spend(InitiatorAccount1, Fee, Nonce, Height),
                {InitiatorAccount2, ParticipantAccount1};
            Participant ->
                {ok, ParticipantAccount2} = aec_accounts:spend(ParticipantAccount1, Fee, Nonce, Height),
                {InitiatorAccount1, ParticipantAccount2}
        end,

    AccountsTree1 = aec_accounts_trees:enter(InitiatorAccount3, AccountsTree0),
    AccountsTree2 = aec_accounts_trees:enter(ParticipantAccount3, AccountsTree1),

    Channel0      = aesc_state_tree:get(ChannelId, ChannelsTree0),
    Channel1      = aesc_channels:close(Channel0),
    ChannelsTree1 = aesc_state_tree:enter(Channel1, ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree2),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_settle_tx{account = AccountPubKey,
                            party   = PartyPubKey}) ->
    [AccountPubKey, PartyPubKey].

-spec signers(tx()) -> list(pubkey()).
signers(#channel_settle_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec serialize(tx()) -> list(map()).
serialize(#channel_settle_tx{channel_id = ChannelId,
                             account    = AccountPubKey,
                             party      = PartyPubKey,
                             fee        = Fee,
                             nonce      = Nonce}) ->
    [#{<<"vsn">>        => version()},
     #{<<"channel_id">> => ChannelId},
     #{<<"account">>    => AccountPubKey},
     #{<<"party">>      => PartyPubKey},
     #{<<"fee">>        => Fee},
     #{<<"nonce">>      => Nonce}].

-spec deserialize(list(map())) -> tx().
deserialize([#{<<"vsn">>        := ?CHANNEL_SETTLE_TX_VSN},
             #{<<"channel_id">> := ChannelId},
             #{<<"account">>    := AccountPubKey},
             #{<<"party">>      := PartyPubKey},
             #{<<"fee">>        := Fee},
             #{<<"nonce">>      := Nonce}]) ->
    #channel_settle_tx{channel_id = ChannelId,
                       account    = AccountPubKey,
                       party = PartyPubKey,
                       fee        = Fee,
                       nonce      = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_settle_tx{channel_id = ChannelId,
                              account    = AccountPubKey,
                              party      = PartyPubKey,
                              fee        = Fee,
                              nonce      = Nonce}) ->
    %% TODO: add swagger schema name
    #{<<"vsn">>        => version(),
      <<"channel_id">> => aec_base58c:encode(channel, ChannelId),
      <<"account">>    => aec_base58c:encode(account_pubkey, AccountPubKey),
      <<"party">>      => aec_base58c:encode(account_pubkey, PartyPubKey),
      <<"fee">>        => Fee,
      <<"nonce">>      => Nonce}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_channel(ChannelId, AccountPubKey, PartyPubKey, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    case aesc_state_tree:lookup(ChannelId, ChannelsTree) of
        none ->
            {error, channel_does_not_exist};
        {value, Channel} ->
            Checks =
                [fun() -> aesc_utils:check_are_peers([AccountPubKey, PartyPubKey], aesc_channels:peers(Channel)) end,
                 fun() -> check_solo_closed(Channel) end],
            aeu_validation:run(Checks)
    end.

check_solo_closed(Channel) ->
    case aesc_channels:is_closed_solo(Channel) of
        true  -> ok;
        false -> {error, channel_not_closing}
    end.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_SETTLE_TX_VSN.
