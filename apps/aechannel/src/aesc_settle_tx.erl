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

-define(CHANNEL_SETTLE_TX_VSN, 1).
-define(CHANNEL_SETTLE_TX_TYPE, channel_settle_tx).
-define(CHANNEL_SETTLE_TX_FEE, 4).

-opaque tx() :: #channel_settle_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id        := ChannelId,
      from              := FromPubKey,
      initiator_amount  := InitiatorAmount,
      responder_amount  := ResponderAmount,
      ttl               := TTL,
      fee               := Fee,
      nonce             := Nonce}) ->
    Tx = #channel_settle_tx{
            channel_id        = ChannelId,
            from              = FromPubKey,
            initiator_amount  = InitiatorAmount,
            responder_amount  = ResponderAmount,
            ttl               = TTL,
            fee               = Fee,
            nonce             = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_SETTLE_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_settle_tx{fee = Fee}) ->
    Fee.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_settle_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#channel_settle_tx{from = FromPubKey}) ->
    FromPubKey.

-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_settle_tx{channel_id       = ChannelId,
                         from             = FromPubKey,
                         initiator_amount = InitiatorAmount,
                         responder_amount = ResponderAmount,
                         ttl              = TTL,
                         fee              = Fee,
                         nonce            = Nonce}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(FromPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> aetx_utils:check_ttl(TTL, Height) end,
         fun() -> check_channel(ChannelId, FromPubKey, InitiatorAmount,
                                ResponderAmount, Height, Trees) end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#channel_settle_tx{channel_id       = ChannelId,
                           from             = FromPubKey,
                           initiator_amount = InitiatorAmount,
                           responder_amount = ResponderAmount,
                           fee              = Fee,
                           nonce            = Nonce}, Trees, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees),
    ChannelsTree0 = aec_trees:channels(Trees),

    Channel0        = aesc_state_tree:get(ChannelId, ChannelsTree0),
    Initiator       = aesc_channels:initiator(Channel0),
    InitiatorAmount = aesc_channels:initiator_amount(Channel0), % same amt
    Responder       = aesc_channels:participant(Channel0),
    ResponderAmount = aesc_channels:participant_amount(Channel0), % same amt

    InitiatorAccount0       = aec_accounts_trees:get(Initiator, AccountsTree0),
    ResponderAccount0       = aec_accounts_trees:get(Responder, AccountsTree0),
    {ok, InitiatorAccount1} = aec_accounts:earn(InitiatorAccount0, InitiatorAmount, Height),
    {ok, ResponderAccount1} = aec_accounts:earn(ResponderAccount0, ResponderAmount, Height),

    {InitiatorAccount3, ResponderAccount3} =
        case FromPubKey of
            Initiator ->
                {ok, InitiatorAccount2} = aec_accounts:spend(InitiatorAccount1, Fee, Nonce, Height),
                {InitiatorAccount2, ResponderAccount1};
            Responder ->
                {ok, ResponderAccount2} = aec_accounts:spend(ResponderAccount1, Fee, Nonce, Height),
                {InitiatorAccount1, ResponderAccount2}
        end,

    AccountsTree1 = aec_accounts_trees:enter(InitiatorAccount3, AccountsTree0),
    AccountsTree2 = aec_accounts_trees:enter(ResponderAccount3, AccountsTree1),

    ChannelsTree1 = aesc_state_tree:delete(aesc_channels:id(Channel0), ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree2),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_close_mutual_tx{channel_id = ChannelId}) ->
    case aec_chain:get_channel(ChannelId) of
        {ok, Channel} ->
            [aesc_channels:initiator(Channel), aesc_channels:participant(Channel)];
        {error, not_found} -> []
    end.

-spec signers(tx()) -> list(pubkey()).
signers(#channel_settle_tx{from = FromPubKey}) ->
    [FromPubKey].

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_settle_tx{channel_id       = ChannelId,
                             from             = FromPubKey,
                             initiator_amount = InitiatorAmount,
                             responder_amount = ResponderAmount,
                             ttl              = TTL,
                             fee              = Fee,
                             nonce            = Nonce}) ->
    {version(),
    [ {channel_id       , ChannelId}
    , {from             , FromPubKey}
    , {initiator_amount , InitiatorAmount}
    , {responder_amount , ResponderAmount}
    , {ttl              , TTL}
    , {fee              , Fee}
    , {nonce            , Nonce}
    ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_SETTLE_TX_VSN,
            [ {channel_id       , ChannelId}
            , {from             , FromPubKey}
            , {initiator_amount , InitiatorAmount}
            , {responder_amount , ResponderAmount}
            , {ttl              , TTL}
            , {fee              , Fee}
            , {nonce            , Nonce}]) ->
    #channel_settle_tx{channel_id       = ChannelId,
                       from             = FromPubKey,
                       initiator_amount = InitiatorAmount,
                       responder_amount = ResponderAmount,
                       ttl              = TTL,
                       fee              = Fee,
                       nonce            = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_settle_tx{channel_id      = ChannelId,
                              from            = FromPubKey,
                              initiator_amount= InitiatorAmount,
                              responder_amount= ResponderAmount,
                              ttl             = TTL,
                              fee             = Fee,
                              nonce           = Nonce}) ->
    #{<<"data_schema">>       => <<"ChannelSettleTxJSON">>, % swagger schema name
      <<"vsn">>               => version(),
      <<"channel_id">>        => aec_base58c:encode(channel, ChannelId),
      <<"from">>              => aec_base58c:encode(account_pubkey, FromPubKey),
      <<"initiator_amount">>  => InitiatorAmount,
      <<"responder_amount">>  => ResponderAmount,
      <<"ttl">>               => TTL,
      <<"fee">>               => Fee,
      <<"nonce">>             => Nonce}.


serialization_template(?CHANNEL_SETTLE_TX_VSN) ->
    [ {channel_id       , binary}
    , {from             , binary}
    , {initiator_amount , int}
    , {responder_amount , int}
    , {ttl              , int}
    , {fee              , int}
    , {nonce            , int}
    ].

-spec is_verifiable(tx()) -> boolean().
is_verifiable(#channel_close_mutual_tx{channel_id = ChannelId}) ->
    case aec_chain:get_channel(ChannelId) of
        {ok, _Channel} -> true;
        {error, not_found} -> false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_channel(ChannelId, FromPubKey, InitiatorAmount, ResponderAmount, Height, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    case aesc_state_tree:lookup(ChannelId, ChannelsTree) of
        none ->
            {error, channel_does_not_exist};
        {value, Channel} ->
            Checks =
                [fun() -> aesc_utils:check_is_peer(FromPubKey, aesc_channels:peers(Channel)) end,
                 %% check total amount
                 fun() -> aesc_utils:check_are_funds_in_channel(ChannelId,
                                InitiatorAmount + ResponderAmount, Trees) end,
                 %% check individual amounts are what is expected
                 fun() -> check_peer_amount(Channel,
                                            fun aesc_channels:initiator_amount/1,
                                            InitiatorAmount)
                 end,
                 fun() -> check_peer_amount(Channel,
                                            fun aesc_channels:participant_amount/1,
                                            ResponderAmount)
                 end,
                 fun() -> check_solo_closed(Channel, Height) end],
            aeu_validation:run(Checks)
    end.

check_peer_amount(Channel, ExpectedAmt, GetAmtFun) when is_function(GetAmtFun, 1) ->
    Amt = GetAmtFun(Channel),
    case Amt =:= ExpectedAmt of
        true -> ok;
        false -> {error, wrong_amt}
    end.

check_solo_closed(Channel, Height) ->
    case aesc_channels:is_solo_closed(Channel, Height) of
        true  -> ok;
        false -> {error, channel_not_closed}
    end.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_SETTLE_TX_VSN.
