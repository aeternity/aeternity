%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel withdraw transaction
%%% @end
%%%=============================================================================
-module(aesc_withdraw_tx).

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
         for_client/1,
         is_verifiable/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_WITHDRAW_TX_VSN, 1).
-define(CHANNEL_WITHDRAW_TX_TYPE, channel_withdraw_tx).
-define(CHANNEL_WITHDRAW_TX_FEE, 4).

-opaque tx() :: #channel_withdraw_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id := ChannelId,
      to         := ToPubKey,
      amount     := Amount,
      ttl        := TTL,
      fee        := Fee,
      nonce      := Nonce}) ->
    Tx = #channel_withdraw_tx{
            channel_id = ChannelId,
            to         = ToPubKey,
            amount     = Amount,
            ttl        = TTL,
            fee        = Fee,
            nonce      = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_WITHDRAW_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_withdraw_tx{fee = Fee}) ->
    Fee.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_withdraw_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#channel_withdraw_tx{to = ToPubKey}) ->
    ToPubKey.

-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_withdraw_tx{channel_id  = ChannelId,
                           to          = ToPubKey,
                           amount      = Amount,
                           ttl         = TTL,
                           fee         = Fee,
                           nonce       = Nonce}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(ToPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> aetx_utils:check_ttl(TTL, Height) end,
         fun() -> check_channel(ChannelId, Amount, ToPubKey, Trees) end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#channel_withdraw_tx{channel_id = ChannelId,
                             to         = ToPubKey,
                             amount     = Amount,
                             fee        = Fee,
                             nonce      = Nonce}, Trees, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees),
    ChannelsTree0 = aec_trees:channels(Trees),

    ToAccount0       = aec_accounts_trees:get(ToPubKey, AccountsTree0),
    {ok, ToAccount1} = aec_accounts:spend(ToAccount0, Fee, Nonce, Height),
    {ok, ToAccount2} = aec_accounts:earn(ToAccount1, Amount, Height),

    AccountsTree1 = aec_accounts_trees:enter(ToAccount2, AccountsTree0),

    Channel0      = aesc_state_tree:get(ChannelId, ChannelsTree0),
    Channel1      = aesc_channels:withdraw(Channel0, Amount),
    ChannelsTree1 = aesc_state_tree:enter(Channel1, ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree1),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_withdraw_tx{to = ToPubKey}) ->
    [ToPubKey].

-spec signers(tx()) -> list(pubkey()).
signers(#channel_withdraw_tx{channel_id = ChannelId}) ->
    case aec_chain:get_channel(ChannelId) of
        {ok, Channel} ->
            [aesc_channels:initiator(Channel), aesc_channels:participant(Channel)];
        {error, not_found} ->
            []
    end.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_withdraw_tx{channel_id = ChannelId,
                               to         = ToPubKey,
                               amount     = Amount,
                               ttl        = TTL,
                               fee        = Fee,
                               nonce      = Nonce}) ->
    {version(),
     [ {channel_id , ChannelId}
     , {to         , ToPubKey}
     , {amount     , Amount}
     , {ttl        , TTL}
     , {fee        , Fee}
     , {nonce      , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_WITHDRAW_TX_VSN,
            [ {channel_id , ChannelId}
            , {to         , ToPubKey}
            , {amount     , Amount}
            , {ttl        , TTL}
            , {fee        , Fee}
            , {nonce      , Nonce}]) ->
    #channel_withdraw_tx{channel_id = ChannelId,
                         to         = ToPubKey,
                         amount     = Amount,
                         ttl        = TTL,
                         fee        = Fee,
                         nonce      = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_withdraw_tx{channel_id   = ChannelId,
                                to           = ToPubKey,
                                amount       = Amount,
                                ttl          = TTL,
                                fee          = Fee,
                                nonce        = Nonce}) ->
    #{<<"data_schema">> => <<"ChannelWithdrawalTxJSON">>, % swagger schema name
      <<"vsn">>         => version(),
      <<"channel_id">>  => aec_base58c:encode(channel, ChannelId),
      <<"to">>          => aec_base58c:encode(account_pubkey, ToPubKey),
      <<"amount">>      => Amount,
      <<"ttl">>         => TTL,
      <<"fee">>         => Fee,
      <<"nonce">>       => Nonce}.

serialization_template(?CHANNEL_WITHDRAW_TX_VSN) ->
    [ {channel_id , binary}
    , {to         , binary}
    , {amount     , int}
    , {ttl        , int}
    , {fee        , int}
    , {nonce      , int}
    ].

-spec is_verifiable(tx()) -> boolean().
is_verifiable(#channel_withdraw_tx{channel_id = ChannelId}) ->
    case aec_chain:get_channel(ChannelId) of
        {ok, _Channel}     -> true;
        {error, not_found} -> false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec check_channel(aesc_channels:id(), aesc_channels:amount(),
                    pubkey(), aec_trees:trees()) ->
                           ok | {error, atom()}.
check_channel(ChannelId, Amount, ToPubKey, Trees) ->
    case aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees)) of
        {value, Channel} ->
            Checks =
                [fun() -> aesc_utils:check_is_active(Channel) end,
                 fun() -> aesc_utils:check_is_peer(ToPubKey, aesc_channels:peers(Channel)) end,
                 fun() -> check_amount(Channel, Amount) end],
            aeu_validation:run(Checks);
        none ->
            {error, channel_does_not_exist}
    end.

-spec check_amount(aesc_channels:channel(), aesc_channels:amount()) ->
                          ok | {error, not_enough_channel_funds}.
check_amount(Channel, Amount) ->
    case aesc_channels:total_amount(Channel) >= Amount of
        true ->
            ok;
        false ->
            {error, not_enough_channel_funds}
    end.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_WITHDRAW_TX_VSN.
